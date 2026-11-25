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

   A union that joins the model's bodies (Joined) is the lesser of its sides, and reads a
   side that is a body through that body's switch: the page takes a body out of the
   picture by it, with no new program. The octree's decisions are made to hold for that.

   The cut is the octree's, taken as far as a number of regions allows. From the model's
   box, the cell that costs most is cut in eight, again and again; the cells that hold
   no surface are dropped, for a ray passes them without evaluating anything. What a
   cell costs is its program's length times the area it shows. Regions whose programs
   come out the same share one.

   Of the cells dropped, those inside the solid are kept as boxes and nothing more
   ([solid]), each with the body it is inside: where a picture is cut by a plane
   (Render.kept), the solid cut open is the plane inside a region whose field is below
   zero there, or inside one of these whose body is not switched off. A cell inside the
   solid where a body other than those it is inside may have surface is a region too:
   with those switched off, that surface is seen. *)
module P = Prog

(* ---- A program under a cell's decisions ---- *)

type value = At of int  (** a register of the program, numbered from 0 as they are filled; or for -1, -2, -3 a coordinate *) | Number of float

type step =
  | Unary of int * value
  | Binary of int * value * value  (** by Prog's code for the operation *)
  | Choice of value * value * value  (** the guard, then the side for <= 0, and the other *)
  | Least of value * value  (** a union that joins bodies *)
  | Shown of int * value  (** a body's value as the union above takes it: far outside, where the body is switched off *)

type program = { steps : (int * step) array;  (** each with the register it fills, in order *) result : value }

let unary c v = match c with 4 -> -.v | 5 -> sqrt v | 6 -> v *. v | 7 -> sin v | 8 -> cos v | 9 -> exp v | _ -> log v
let binary c x y = match c with 11 -> x +. y | 12 -> x -. y | 13 -> x *. y | _ -> x /. y

let text = function At r when r < 0 -> [| "p.x"; "p.y"; "p.z" |].(-1 - r) | At r -> Printf.sprintf "r%d" r | Number k -> Shader.lit k

(* What an evaluation needs where the selects are decided as [state] says. Arithmetic
   on numbers alone is done here. The program's registers are numbered as they are
   filled, so two cells that need the same thing have the same program, whatever else
   the model holds. *)
let program (p : P.t) (f : Joined.found) state =
  (* what each of the model's registers comes to, of those reached *)
  let values = Array.make (Array.length p.code) (Number 0.) and walked = Bytes.make p.nodes '\000' and steps = ref [] and filled = ref 0 in
  let fresh s = steps := (!filled, s) :: !steps; incr filled; At (!filled - 1) in
  let step i s = values.(i) <- fresh s in
  (* a node as a union that joins bodies takes it: a body through its switch, once *)
  let shown = Array.make (Array.length f.bodies) None in
  let taken n =
    match f.of_node.(n) with
    | -1 -> values.(p.last.(n))
    | b -> ( match shown.(b) with Some v -> v | None -> let v = fresh (Shown (b, values.(p.last.(n)))) in shown.(b) <- Some v; v)
  in
  let ops n =
    for i = p.first.(n) to p.last.(n) do
      let c = p.code.(i) in
      if c = 0 then values.(i) <- At (-1 - p.a.(i))
      else if c = 1 then values.(i) <- Number p.k.(i)
      else if c = 2 then values.(i) <- values.(p.a.(i))
      else if c >= 4 && c <= 10 then ( match values.(p.a.(i)) with Number v -> values.(i) <- Number (unary c v) | a -> step i (Unary (c, a)))
      else if c >= 11 then
        match (values.(p.a.(i)), values.(p.b.(i))) with Number x, Number y -> values.(i) <- Number (binary c x y) | a, b -> step i (Binary (c, a, b))
    done
  in
  let rec walk n =
    if Bytes.unsafe_get walked n = '\000' then begin
      Bytes.unsafe_set walked n '\001';
      let ins = p.inputs.(n) and of_ k = values.(p.last.(p.inputs.(n).(k))) in
      match p.kind.(n) with
      | 0 -> ops n
      | 1 -> Array.iter walk ins; ops n
      | _ when Joined.joins f n -> (
          match Char.code (Bytes.get state n) with
          | 0 -> walk ins.(1); walk ins.(2); step p.last.(n) (Least (taken ins.(1), taken ins.(2)))
          | side -> walk ins.(side); values.(p.last.(n)) <- taken ins.(side))
      | _ -> (
          match Char.code (Bytes.get state n) with
          | 0 -> Array.iter walk ins; step p.last.(n) (Choice (of_ 0, of_ 1, of_ 2))
          | side -> walk ins.(side); values.(p.last.(n)) <- of_ side)
    end
  in
  walk p.root;
  { steps = Array.of_list (List.rev !steps); result = taken p.root }

(* The program's value at x, as the shader works it out, with the bodies [off] says
   switched off: to hold it to the model's. *)
let run ?(off = fun _ -> false) (g : program) x =
  let r = Array.make (Array.length g.steps) 0. in
  let get = function Number k -> k | At i when i < 0 -> x.(-1 - i) | At i -> r.(i) in
  Array.iter
    (fun (i, s) ->
      r.(i) <-
        (match s with
         | Unary (c, a) -> unary c (get a)
         | Binary (c, a, b) -> binary c (get a) (get b)
         | Choice (c, le, gt) -> if get c <= 0. then get le else get gt
         | Least (a, b) -> Float.min (get a) (get b)
         | Shown (b, a) -> if off b then Joined.far else get a))
    g.steps;
  get g.result

(* The program as a function in WGSL: the field at p. *)
let written (g : program) =
  let b = Buffer.create (64 * (1 + Array.length g.steps)) in
  let add = Buffer.add_string b in
  add "fn model(p: vec3f) -> f32 {\n";
  Array.iter
    (fun (i, s) ->
      add "  let r"; add (string_of_int i); add " = ";
      (match s with
       | Unary (4, a) -> add "-"; add (text a)
       | Unary (6, a) -> add (text a); add " * "; add (text a)
       | Unary (c, a) -> add (match c with 5 -> "sqrt" | 7 -> "sin" | 8 -> "cos" | 9 -> "exp" | _ -> "log"); add "("; add (text a); add ")"
       | Binary (c, x, y) -> add (text x); add (match c with 11 -> " + " | 12 -> " - " | 13 -> " * " | _ -> " / "); add (text y)
       | Choice (c, le, gt) -> add "select("; add (text gt); add ", "; add (text le); add ", "; add (text c); add " <= 0.0)"
       | Least (x, y) -> add "min("; add (text x); add ", "; add (text y); add ")"
       | Shown (body, a) -> add "select("; add (text a); add ", "; add (Shader.lit Joined.far); add ", "; add (Joined.switch body); add ")");
      add ";\n")
    g.steps;
  add "  return "; add (text g.result); add ";\n}\n";
  Buffer.contents b

(* ---- The cut ---- *)

type region = { lo : float array; hi : float array; shader : int  (** which of the shaders is its program *) }

type t = {
  regions : region array;
  solid : (float array * float array * int) array;  (** the cells that hold no surface and are inside a body: each from, to, and the body *)
  shaders : (program * string) array;  (** each program, and it as text *)
  total : int;  (** the operations of the whole model, written the same way: what a region's are measured against *)
}

type cell = { at : float array; size : float; level : int; state : Bytes.t; made : program; cost : float }

(* What a cell of the octree comes to: a region, or with no surface in it whatever is
   switched off, air, or inside the bodies given. *)
type found = Surface of cell | Solid of int list | Air

(* The model within lo..hi as at most [most] regions (a few more, where the last cut
   made several), none smaller than the longest side over 2^depth. A cell's decisions
   are made to hold a little beyond it: the shader takes a normal from points just off
   the surface.

   The eight cells a cell is cut into are worked out on several cores at once (Par), each
   with a context of its own; so are the programs' texts. [c] is used for the first
   cell alone. *)
let cut ?(most = 256) ?(depth = 6) (p : P.t) (f : Joined.found) c ~lo ~hi =
  let side = Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo) in
  let top at size = Array.mapi (fun a v -> Float.min hi.(a) (v +. size)) at in
  let solid = ref [] in
  let cell (c : P.ctx) at size level parent =
    let r = (0.5 *. sqrt 3. *. size) +. (1e-3 *. side) in
    let state, _, surface = Octree.prune ~joins:f.joins p c parent (Array.map (fun v -> v +. (0.5 *. size)) at) r in
    let region () =
      let made = program p f state in
      (* the area it shows: its two longer sides *)
      let sides = List.sort compare (Array.to_list (Array.map2 ( -. ) (top at size) at)) in
      Surface { at; size; level; state; made; cost = float (Array.length made.steps) *. List.nth sides 1 *. List.nth sides 2 }
    in
    if surface then region ()
    else if c.v.(p.last.(p.root)) > 0. then Air (* and air whatever is switched off: that only takes away *)
    else begin
      (* Inside the solid, by its middle, just evaluated: so were the bodies the cell's
         decisions leave. One that is neither inside nor outside all over the cell may
         have surface here, seen with the others switched off. *)
      let inside = ref [] and unsure = ref false in
      let rec bodies n =
        match f.of_node.(n) with
        | -1 -> ( match Char.code (Bytes.get state n) with 0 -> bodies p.inputs.(n).(1); bodies p.inputs.(n).(2) | s -> bodies p.inputs.(n).(s))
        | b ->
            let v = c.v.(p.last.(n)) and reach = (p.lip.(n) *. r) +. p.jump.(n) in
            if v +. reach < 0. then (if not (List.mem b !inside) then inside := b :: !inside) else if not (v -. reach > 0.) then unsure := true
      in
      bodies p.root;
      if !unsure then region () else Solid !inside
    end
  in
  (* the cells of those worked out that are regions; the solid ones are kept aside *)
  let regions found =
    List.filter_map (fun (at, size, found) -> match found with Surface k -> Some k | Solid inside -> List.iter (fun b -> solid := (at, top at size, b) :: !solid) inside; None | Air -> None) found
  in
  let rec go cells =
    let worst = List.fold_left (fun w k -> if k.level < depth && (match w with Some w -> k.cost > w.cost | None -> true) then Some k else w) None cells in
    match worst with
    | Some w when List.length cells < most ->
        let h = 0.5 *. w.size in
        let eight = List.filter_map (Octree.octant w.at h hi) [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
        go (regions (Par.map (fun at -> (at, h, cell (P.ctx p) at h (w.level + 1) w.state)) eight) @ List.filter (fun k -> k != w) cells)
    | _ -> cells
  in
  let whole = P.whole p in
  let cells = go (regions [ (lo, side, cell c lo side 0 whole) ]) in
  let known = Hashtbl.create 64 and shaders = ref [] in
  let regions =
    List.map2
      (fun k text ->
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
      (Par.map (fun k -> written k.made) cells)
  in
  { regions = Array.of_list regions; solid = Array.of_list (List.rev !solid); shaders = Array.of_list (List.rev !shaders); total = Array.length (program p f whole).steps }
