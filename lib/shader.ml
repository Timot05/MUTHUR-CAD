(* The compiled program written out as a function in WGSL, the shading language of
   WebGPU, for the page (web/view.html: it marches a ray a pixel through this):

     fn model(p: vec3f, face: ptr<function, i32>, feat: ptr<function, i32>,
              gates: texture_2d<u32>, grid_lo: vec3f, cell: f32, cells: vec3i) -> f32

   It is generated from the same arrays the evaluator reads (Prog), never written by
   hand, and follows the evaluator: a select is an if, so each point computes only the
   side it takes. A node first needed inside a branch is written there; if it is needed
   again outside, it is written again.

   [face] is only a number to colour by: a made face's is mixed from its inputs'. Names
   come from the kernel (a pick is evaluated there, not in the page). [feat] is the
   id of the feature that made the face.

   Gates. The page evaluates the model millions of times a frame, nearly always where
   most of it does not matter: far from a part, the union that joins it takes the other
   side whatever the part's field is. The octree knows where (Octree.cells): cell by
   cell, which selects are decided. So the parts of the model are written inside gates,
   blocks the shader enters only where a table says to ([decisions]: a bit per gate for
   each cell of a grid over the domain).

   What is gated is a stretch of a chain. A union of a list is a chain of selects, each
   taking either what the chain has so far or one more part; so is a list of cuts. The
   shader carries what the chain has so far in one variable, and a gate holds a few
   links: their parts, their guards and their choices, all computed in full as the
   evaluator would. Where every link of a gate is decided for the chain, the gate changes
   nothing and is skipped. A decision is the kernel's own bound, the one the edges and
   the mesh are found with, so the field is the same field.

   A gate is not smaller than [least] operations. A GPU runs a skipped block for nearly
   nothing only when the block is large: measured on an Apple M1, skipping costs 45% of
   running a block of 30 operations, 7% of one of 120 and 2% of one of 240. *)

module P = Prog

type shader = {
  text : string;
  gates : (int * int) list array;  (** each gate's selects, each with the side that is its chain so far *)
}

(* A cell's row of the table is whole texels of four 32-bit words, a bit a gate; the
   table is a texture [width] wide, its rows one after another. *)
let per_texel = 128
let width = 4096
let texels gates = max 1 ((gates + per_texel - 1) / per_texel)

let lit v =
  if not (Float.is_finite v) then if v > 0. then "1e30" else "-1e30"
  else
    let s = Printf.sprintf "%.9g" v in
    let s = if String.contains s '.' || String.contains s 'e' then s else s ^ ".0" in
    if v < 0. then "(" ^ s ^ ")" else s

let model ?(least = 240) (p : P.t) root =
  let b = Buffer.create 4096 in
  let scopes = ref [ Hashtbl.create 64 ] and pad = ref "  " in
  let known n = List.exists (fun h -> Hashtbl.mem h n) !scopes and know n = Hashtbl.replace (List.hd !scopes) n () in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b !pad; Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  (* A block: what is written in it is in reach only there. *)
  let block head body =
    line "%s {" head;
    scopes := Hashtbl.create 16 :: !scopes;
    pad := !pad ^ "  ";
    body ();
    scopes := List.tl !scopes;
    pad := String.sub !pad 2 (String.length !pad - 2);
    line "}"
  in
  let gates = ref [] and count = ref 0 in
  (* A register as text: constants and coordinates in place, the rest by name. *)
  let rec reg i =
    match p.code.(i) with
    | 0 -> [| "p.x"; "p.y"; "p.z" |].(p.a.(i))
    | 1 -> lit p.k.(i)
    | 2 -> reg p.a.(i)
    | _ -> Printf.sprintf "r%d" i
  in
  let rec face n = match p.kind.(n) with 0 -> string_of_int p.fixed.(n) | 1 when p.keep.(n) >= 0 -> face p.inputs.(n).(p.keep.(n)) | 1 when p.keep.(n) = -2 -> "-1" | _ -> Printf.sprintf "f%d" n in
  let rec feat n = match p.kind.(n) with 1 when p.keep.(n) >= 0 -> feat p.inputs.(n).(p.keep.(n)) | 2 -> Printf.sprintf "q%d" n | _ -> string_of_int p.feature.(n) in
  let ops n =
    for i = p.first.(n) to p.last.(n) do
      let a = p.a.(i) and c = p.b.(i) in
      let one f = line "let r%d = %s;" i (Printf.sprintf f (reg a)) and two op = line "let r%d = %s %s %s;" i (reg a) op (reg c) in
      match p.code.(i) with
      | 4 -> one "-(%s)"
      | 5 -> one "sqrt(%s)"
      | 6 -> line "let r%d = %s * %s;" i (reg a) (reg a)
      | 7 -> one "sin(%s)"
      | 8 -> one "cos(%s)"
      | 9 -> one "exp(%s)"
      | 10 -> one "log(%s)"
      | 11 -> two "+"
      | 12 -> two "-"
      | 13 -> two "*"
      | 14 -> two "/"
      | _ -> ()
    done
  in
  (* How much is under a node. A guard is left out: it reads the sides again. *)
  let weight = Array.make p.nodes 0 in
  for n = 0 to p.nodes - 1 do
    let ins = p.inputs.(n) in
    let under = if p.kind.(n) = 2 then weight.(ins.(1)) + weight.(ins.(2)) else Array.fold_left (fun s i -> s + weight.(i)) 0 ins in
    weight.(n) <- min (1 lsl 40) (p.last.(n) - p.first.(n) + 1 + under)
  done;
  (* A link of a chain: a select whose guard reads both its sides, through arithmetic alone. *)
  let rec reads g m = g = m || (p.kind.(g) = 1 && Array.exists (fun i -> reads i m) p.inputs.(g)) in
  let link n = p.kind.(n) = 2 && reads p.inputs.(n).(0) p.inputs.(n).(1) && reads p.inputs.(n).(0) p.inputs.(n).(2) in
  (* A made face's number, mixed from its inputs': its low 16 bits, kept at every step,
     so that nothing overflows (WGSL refuses a constant that does). *)
  let made n = Array.fold_left (fun acc i -> Printf.sprintf "((%s * 31 + %s) & 65535)" acc (face i)) (string_of_int ((7919 + n) land 65535)) p.inputs.(n) in
  let rec node n =
    if not (known n) then begin
      let ins = p.inputs.(n) in
      (match p.kind.(n) with
      | 0 -> ops n
      | 1 ->
          Array.iter node ins;
          ops n;
          if p.keep.(n) = -1 then line "let f%d: i32 = %s;" n (made n)
          else if p.keep.(n) = -3 then begin
            (* joined: the inputs' face where they agree, else one made from them *)
            let same = String.concat " && " (List.map (fun i -> Printf.sprintf "%s == %s" (face ins.(0)) (face i)) (List.tl (Array.to_list ins))) in
            line "let f%d: i32 = select(%s, %s, %s);" n (made n) (face ins.(0)) (if same = "" then "true" else same)
          end
      | _ when link n -> chain n
      | _ ->
          node ins.(0);
          let r = p.last.(n) in
          line "var r%d: f32; var f%d: i32; var q%d: i32;" r n n;
          let side k () =
            node ins.(k);
            line "r%d = %s; f%d = %s; q%d = %s;" r (reg p.last.(ins.(k))) n (face ins.(k)) n (feat ins.(k))
          in
          block (Printf.sprintf "if (%s <= 0.0)" (reg p.last.(ins.(0)))) (side 1);
          block "else" (side 2));
      know n
    end
  (* The chain a link is the top of: down its heavier side while that is a link too. *)
  and chain top =
    let rec down n links =
      let ins = p.inputs.(n) in
      let so_far = if weight.(ins.(1)) >= weight.(ins.(2)) then 1 else 2 in
      let links = (n, so_far) :: links and below = ins.(so_far) in
      if link below && not (known below) then down below links else (below, links)
    in
    let bottom, links = down top [] in
    (* cut into gates from the bottom, each at least [least] operations; what is left over
       at the top joins the last, and a chain smaller than a gate has none *)
    let part (n, so_far) = weight.(p.inputs.(n).(3 - so_far)) + 4 in
    let rec cut gate size = function
      | [] -> if gate = [] then [] else [ (List.rev gate, size) ]
      | l :: rest -> if size + part l >= least then (List.rev (l :: gate), size + part l) :: cut [] 0 rest else cut (l :: gate) (size + part l) rest
    in
    let stretches = match List.rev (cut [] 0 links) with (last, size) :: (before, s) :: rest when size < least -> List.rev ((before @ last, s + size) :: rest) | all -> List.rev all in
    node bottom;
    let c = Printf.sprintf "c%d" top in
    line "var %s: f32 = %s; var %sf: i32 = %s; var %sq: i32 = %s;" c (reg p.last.(bottom)) c (face bottom) c (feat bottom);
    let one (n, so_far) =
      let ins = p.inputs.(n) in
      let below = ins.(so_far) and part = ins.(3 - so_far) in
      node part;
      (* the chain so far, under the names the guard reads it by *)
      if not (known below) then begin
        line "let r%d = %s; let f%d = %sf; let q%d = %sq;" p.last.(below) c below c below c;
        know below
      end;
      node ins.(0);
      line "if (%s(%s <= 0.0)) { %s = %s; %sf = %s; %sq = %s; }" (if so_far = 1 then "!" else "") (reg p.last.(ins.(0))) c (reg p.last.(part)) c (face part) c (feat part)
    in
    List.iter
      (fun (stretch, size) ->
        if size < least then List.iter one stretch
        else begin
          let g = !count in
          incr count;
          gates := (g, stretch) :: !gates;
          block (Printf.sprintf "if ((g%d.%c & %uu) != 0u)" (g / per_texel) "xyzw".[g mod per_texel / 32] (1 lsl (g mod 32))) (fun () -> List.iter one stretch)
        end)
      stretches;
    line "let r%d = %s; let f%d = %sf; let q%d = %sq;" p.last.(top) c top c top c
  in
  node root;
  let words = texels !count in
  let text =
    Printf.sprintf
      "fn model(p: vec3f, face: ptr<function, i32>, feat: ptr<function, i32>, gates: texture_2d<u32>, grid_lo: vec3f, cell: f32, cells: vec3i) -> f32 {\n\
      \  let at = clamp(vec3i((p - grid_lo) / cell), vec3i(0), cells - 1);\n\
      \  let row = u32(((at.z * cells.y + at.y) * cells.x + at.x) * %d);\n\
       %s%s  *face = %s;\n\
      \  *feat = %s;\n\
      \  return %s;\n\
       }\n"
      words
      (String.concat "" (List.init words (fun w -> Printf.sprintf "  let g%d = textureLoad(gates, vec2u((row + %du) %% %du, (row + %du) / %du), 0);\n" w w width w width)))
      (Buffer.contents b) (face root) (feat root) (reg p.last.(root))
  in
  { text; gates = Array.of_list (List.map snd (List.sort compare !gates)) }

(* ---- The table of decisions ---- *)

type grid = {
  lo : float array;
  cell : float;  (** the side of a cell *)
  cells : int array;  (** how many along x, y and z: rows run x first, then y, then z *)
  table : Bytes.t;  (** 32-bit words, little-endian: [width] texels across *)
}

(* Whether a gate is entered where the selects are decided as [state] says: not where
   every one of its links keeps the chain so far. *)
let entered links state = List.exists (fun (n, so_far) -> Char.code (Bytes.get state n) <> so_far) links

(* Which gates are entered in every cell of a grid over lo..hi, laid out as the shader
   reads them. A decision holds a little beyond its cell too: the shader finds a point's
   cell in 32-bit arithmetic, and takes a normal from points just off the surface. *)
let decisions ?(depth = 5) (p : P.t) c (s : shader) ~lo ~hi =
  let side = Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo) and words = texels (Array.length s.gates) in
  let cell = side /. float (1 lsl depth) in
  let cells = Array.map2 (fun l h -> max 1 (int_of_float (Float.ceil (((h -. l) /. cell) -. 1e-9)))) lo hi in
  let table = Bytes.make (16 * width * (((cells.(0) * cells.(1) * cells.(2) * words) + width - 1) / width)) '\000' and row = Bytes.create (16 * words) in
  Octree.cells ~pad:(1e-3 *. side) p c ~lo ~hi ~depth (fun l size state _ ->
      Bytes.fill row 0 (Bytes.length row) '\000';
      Array.iteri (fun g links -> if entered links state then Bytes.set_int32_le row (4 * (g / 32)) (Int32.logor (Bytes.get_int32_le row (4 * (g / 32))) (Int32.shift_left 1l (g mod 32)))) s.gates;
      (* a cell the pruning stopped at early stands for every cell of the grid in it *)
      let from a = int_of_float (Float.round ((l.(a) -. lo.(a)) /. cell)) and span = int_of_float (Float.round (size /. cell)) in
      for z = from 2 to min (cells.(2) - 1) (from 2 + span - 1) do
        for y = from 1 to min (cells.(1) - 1) (from 1 + span - 1) do
          for x = from 0 to min (cells.(0) - 1) (from 0 + span - 1) do
            Bytes.blit row 0 table (16 * words * ((((z * cells.(1)) + y) * cells.(0)) + x)) (16 * words)
          done
        done
      done);
  { lo; cell; cells; table }

(* Whether the shader enters gate [k] at x: its bit in the row of x's cell. *)
let enters (g : grid) (s : shader) x k =
  let at a = max 0 (min (g.cells.(a) - 1) (int_of_float ((x.(a) -. g.lo.(a)) /. g.cell))) in
  let row = (((at 2 * g.cells.(1)) + at 1) * g.cells.(0)) + at 0 in
  Int32.logand (Bytes.get_int32_le g.table ((16 * texels (Array.length s.gates) * row) + (4 * (k / 32)))) (Int32.shift_left 1l (k mod 32)) <> 0l
