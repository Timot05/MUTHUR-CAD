(* The model cut into regions, each with a short shader of its own (web/view.html draws
   a region as its box, and marches a pixel of the box through that shader).

   A region's shader gives the field and nothing else. Which face a point is on is not
   in it: choosing a face at every evaluation of a march took four fifths of a picture's
   time on one model, so the page asks the model's one compiled shader (Shader) for the
   face, once, where a ray has hit.

   The octree says, for a cell, which selects are decided all over it (Octree.prune). An
   evaluation in the cell then needs only what those decisions leave: of a decided
   select the side it takes, of an undecided one its guard, both sides and the choice.
   That is the cell's program, and it is written straight through: every operation once,
   a choice as a select. Nothing in it is skipped at a point, which a graphics card does
   badly; what does not matter in the region is simply not there.

   The cut is the octree's, taken as far as a number of regions allows. From the model's
   box, the cell that costs most is cut in eight, again and again; the cells that hold
   no surface are dropped, for a ray passes them without evaluating anything. What a
   cell costs is its program's length times the area it shows. Regions whose programs
   come out the same share one.

   Of the cells dropped, those inside the solid are kept as boxes and nothing more
   ([solid]): where a picture is cut by a plane (Render.kept), the solid cut open is the
   plane inside a region whose field is below zero there, or inside one of these. *)
module P = Prog

(* ---- A program under a cell's decisions ---- *)

type value = At of int  (** a register of the program, or for -1, -2, -3 a coordinate *) | Number of float

type step = Unary of int * value | Binary of int * value * value  (** by Prog's code for the operation *) | Choice of value * value * value  (** the guard, then the side for <= 0, and the other *)

type program = { steps : (int * step) array;  (** each with the register it fills, in order *) result : value }

let unary c v = match c with 4 -> -.v | 5 -> sqrt v | 6 -> v *. v | 7 -> sin v | 8 -> cos v | 9 -> exp v | _ -> log v
let binary c x y = match c with 11 -> x +. y | 12 -> x -. y | 13 -> x *. y | _ -> x /. y

let text = function At r when r < 0 -> [| "p.x"; "p.y"; "p.z" |].(-1 - r) | At r -> Printf.sprintf "r%d" r | Number k -> Shader.lit k

(* What an evaluation needs where the selects are decided as [state] says. Arithmetic
   on numbers alone is done here. *)
let program (p : P.t) state =
  let values = Hashtbl.create 64 and walked = Hashtbl.create 64 and steps = ref [] in
  let value r = Hashtbl.find values r in
  let step i s = steps := (i, s) :: !steps; Hashtbl.replace values i (At i) in
  let ops n =
    for i = p.first.(n) to p.last.(n) do
      let c = p.code.(i) in
      if c = 0 then Hashtbl.replace values i (At (-1 - p.a.(i)))
      else if c = 1 then Hashtbl.replace values i (Number p.k.(i))
      else if c = 2 then Hashtbl.replace values i (value p.a.(i))
      else if c >= 4 && c <= 10 then ( match value p.a.(i) with Number v -> Hashtbl.replace values i (Number (unary c v)) | a -> step i (Unary (c, a)))
      else if c >= 11 then
        match (value p.a.(i), value p.b.(i)) with Number x, Number y -> Hashtbl.replace values i (Number (binary c x y)) | a, b -> step i (Binary (c, a, b))
    done
  in
  let rec walk n =
    if not (Hashtbl.mem walked n) then begin
      Hashtbl.replace walked n ();
      let ins = p.inputs.(n) and of_ k = value p.last.(p.inputs.(n).(k)) in
      match p.kind.(n) with
      | 0 -> ops n
      | 1 -> Array.iter walk ins; ops n
      | _ -> (
          match Char.code (Bytes.get state n) with
          | 0 -> Array.iter walk ins; step p.last.(n) (Choice (of_ 0, of_ 1, of_ 2))
          | side -> walk ins.(side); Hashtbl.replace values p.last.(n) (of_ side))
    end
  in
  walk p.root;
  { steps = Array.of_list (List.rev !steps); result = value p.last.(p.root) }

(* The program's value at x, as the shader works it out: to hold it to the model's. *)
let run (g : program) x =
  let r = Hashtbl.create 64 in
  let get = function Number k -> k | At i when i < 0 -> x.(-1 - i) | At i -> Hashtbl.find r i in
  Array.iter (fun (i, s) -> Hashtbl.replace r i (match s with Unary (c, a) -> unary c (get a) | Binary (c, a, b) -> binary c (get a) (get b) | Choice (c, le, gt) -> if get c <= 0. then get le else get gt)) g.steps;
  get g.result

(* The program as a function in WGSL: the field at p. *)
let written (g : program) =
  let b = Buffer.create 4096 in
  Buffer.add_string b "fn model(p: vec3f) -> f32 {\n";
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b "  "; Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  Array.iter
    (fun (i, s) ->
      match s with
      | Unary (c, a) -> line "let r%d = %s;" i (match c with 4 -> "-" ^ text a | 6 -> text a ^ " * " ^ text a | _ -> Printf.sprintf "%s(%s)" (match c with 5 -> "sqrt" | 7 -> "sin" | 8 -> "cos" | 9 -> "exp" | _ -> "log") (text a))
      | Binary (c, x, y) -> line "let r%d = %s %s %s;" i (text x) (match c with 11 -> "+" | 12 -> "-" | 13 -> "*" | _ -> "/") (text y)
      | Choice (c, le, gt) -> line "let r%d = select(%s, %s, %s <= 0.0);" i (text gt) (text le) (text c))
    g.steps;
  line "return %s;" (text g.result);
  Buffer.add_string b "}\n";
  Buffer.contents b

(* ---- The cut ---- *)

type region = { lo : float array; hi : float array; shader : int  (** which of the shaders is its program *) }

type t = {
  regions : region array;
  solid : (float array * float array) array;  (** the cells that hold no surface and are inside the solid: each from, to *)
  shaders : (program * string) array;  (** each program, and it as text *)
  total : int;  (** the operations of the whole model, written the same way: what a region's are measured against *)
}

type cell = { at : float array; size : float; level : int; state : Bytes.t; made : program; cost : float }

(* The model within lo..hi as at most [most] regions (a few more, where the last cut
   made several), none smaller than the longest side over 2^depth. A cell's decisions
   are made to hold a little beyond it: the shader takes a normal from points just off
   the surface. *)
let cut ?(most = 256) ?(depth = 6) (p : P.t) c ~lo ~hi =
  let side = Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo) in
  let top at size = Array.mapi (fun a v -> Float.min hi.(a) (v +. size)) at in
  let solid = ref [] in
  let cell at size level parent =
    let state, _, surface = Octree.prune p c parent (Array.map (fun v -> v +. (0.5 *. size)) at) ((0.5 *. sqrt 3. *. size) +. (1e-3 *. side)) in
    if not surface then begin
      (* all of one side: of the solid if its middle, just evaluated, is *)
      if c.v.(p.last.(p.root)) < 0. then solid := (at, top at size) :: !solid;
      None
    end
    else
      let made = program p state in
      (* the area it shows: its two longer sides *)
      let sides = List.sort compare (Array.to_list (Array.map2 ( -. ) (top at size) at)) in
      Some { at; size; level; state; made; cost = float (Array.length made.steps) *. List.nth sides 1 *. List.nth sides 2 }
  in
  let rec go cells =
    let worst = List.fold_left (fun w k -> if k.level < depth && (match w with Some w -> k.cost > w.cost | None -> true) then Some k else w) None cells in
    match worst with
    | Some w when List.length cells < most ->
        let h = 0.5 *. w.size in
        go (List.filter_map (fun o -> Option.bind (Octree.octant w.at h hi o) (fun at -> cell at h (w.level + 1) w.state)) [ 0; 1; 2; 3; 4; 5; 6; 7 ] @ List.filter (fun k -> k != w) cells)
    | _ -> cells
  in
  let whole = P.whole p in
  let cells = go (Option.to_list (cell lo side 0 whole)) in
  let known = Hashtbl.create 64 and shaders = ref [] in
  let regions =
    List.map
      (fun k ->
        let text = written k.made in
        let shader =
          match Hashtbl.find_opt known text with
          | Some i -> i
          | None ->
              let i = Hashtbl.length known in
              Hashtbl.replace known text i;
              shaders := (k.made, text) :: !shaders;
              i
        in
        { lo = k.at; hi = top k.at k.size; shader })
      cells
  in
  { regions = Array.of_list regions; solid = Array.of_list (List.rev !solid); shaders = Array.of_list (List.rev !shaders); total = Array.length (program p whole).steps }
