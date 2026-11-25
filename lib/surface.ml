(* A triangle mesh of a model's surface, its triangles attached to the model's creases
   and corners. A first pass makes it closed and attached, in three steps; a second
   (Even, below) makes its triangles even and keeps it attached.

   - March. Each last-level cell that may hold surface (Octree.shell) is cut into six
     tetrahedra about one diagonal, the same one in every cell, so the triangles of
     neighbouring cells share their edges and the mesh is closed. A vertex is where the
     field changes sign along a tetrahedron's edge, then put on the surface by Newton.
   - Conform. A mesh edge whose two ends are in different faces crosses a boundary; the
     place is found by halving along the surface. Where the surface turns there (a
     crease: the gradients either side say so, at that very place), the edge is split
     and the new vertex is on the crease. Where it does not (a tangent seam) the edge is
     left: the surface is smooth, and a triangle may lie across.
   - Corners. The extraction's corners at which the surface turns are put in: the
     nearest vertex moved onto one, or the triangle holding it split in three.

   A mesh edge between two vertices on creases, with a different face either side, then
   runs along a crease. Nothing here needs the extraction's edges to be whole: a crease
   is found from the field wherever the mesh crosses it. *)

module P = Prog
module X = Extract

let sub = X.sub and add = X.add and scale = X.scale and dot = X.dot and norm = X.norm and dist = X.dist and cross = X.cross
let mid a b = scale 0.5 (add a b)
let unit a = scale (1. /. norm a) a

(* Where a vertex is: inside a face, on the crease between two, or on a corner. *)
type on = Face of int | Crease of int * int | Corner of int list

type t = {
  points : float array array;
  on : on array;
  triangles : (int * int * int) array;  (** anticlockwise seen from outside *)
  face : int array;  (** per triangle: the face its middle is in *)
  names : string array;  (** face names, by face *)
  size : float;  (** the side of a cell: of the smallest, if they are of several sizes *)
  largest : float;  (** the side of the largest cell *)
  worst : float;  (** how far a triangle is from the surface, at most: at its middle, and to a tolerance at more places *)
  worst_at : float array;  (** where that is *)
  corners : int * int;  (** corners put in, of those of the extraction where the surface turns *)
  creases : (int * int) array;  (** the edges that run along creases, by their two vertices *)
  took : (string * float) list;  (** what was done, in order, and the seconds each took; with how many triangles there were after the march *)
}

type env = { p : P.t; c : P.ctx; tree : Octree.t; diag : float }

let probe e x = P.eval e.p e.c (Octree.state e.tree x) x
let face e x = (probe e x).face

(* The piece of surface at x. Where the mesh is attached, surfaces are told apart by
   piece and not by face: two pieces of one name can meet in a crease (the spans of a
   sketch given one name), and a triangle must not lie across it. What a vertex is
   said to be on ([on]) is pieces too, but for a corner, whose faces the extraction
   gave. *)
let part e x = (probe e x).part

(* Newton onto the surface, no further than [within] from the start. Beside a crease
   that is not square the steps go from one face to the other and back, closing in on
   the crease itself only by a share each time: what they have closed in to after sixty
   is taken if it is within a hundred-millionth of the box. [read_on_surface] gives
   what the field was where the last step was taken from too, which is the same place
   to a million-millionth of the box: the piece of surface there is had for nothing. *)
let read_on_surface ?(within = infinity) e x0 =
  let rec go x n =
    let r = probe e x in
    let g2 = dot r.d.g r.d.g in
    if g2 = 0. || not (Float.is_finite g2) then None
    else
      let x' = sub x (scale (r.d.v /. g2) r.d.g) in
      let step = dist x' x in
      if dist x' x0 > within then None else if step < 1e-12 *. e.diag then Some (x', r) else if n > 0 then go x' (n - 1) else if step < 1e-8 *. e.diag then Some (x', r) else None
  in
  go x0 60

let to_surface ?within e x0 = Option.map fst (read_on_surface ?within e x0)

(* Newton onto the surface as above, but only across [along]: the point stays in the
   plane through x0 that [along] is square to. For the middle of an edge, which must
   come down between its two ends and not beside one of them. *)
let to_surface_across ?(within = infinity) e x0 along =
  let t = unit along in
  let rec go x n =
    let r = probe e x in
    let g = sub r.d.g (scale (dot r.d.g t) t) in
    let g2 = dot g g in
    if g2 = 0. || not (Float.is_finite g2) then None
    else
      let x' = sub x (scale (r.d.v /. g2) g) in
      let step = dist x' x in
      if dist x' x0 > within then None else if step < 1e-12 *. e.diag then Some x' else if n > 0 then go x' (n - 1) else if step < 1e-8 *. e.diag then Some x' else None
  in
  go x0 60

(* The angle between two gradients, in degrees. *)
let turn a b = Float.acos (Float.max (-1.) (Float.min 1. (dot a b /. (norm a *. norm b)))) *. 180. /. Float.pi

(* ---- March ---- *)

(* Where the field is zero on the line from a, where it is va, to b, where it is vb of
   the other sign: by the false position, kept between the two. Where the field is
   straight between them that is the first guess; where it is not (the far side of a
   wall is nearer one of the two than the side between them) it is a few more, and
   still the crossing between them, where Newton from the guess could go to the far
   side. [state] is the decisions of a cell the line is in. *)
let zero e state (va, xa) (vb, xb) =
  let along = sub xb xa in
  let small = 1e-11 *. e.diag in
  let rec go ta fa tb fb last n =
    let t = ta -. (fa *. (tb -. ta) /. (fb -. fa)) in
    let x = add xa (scale t along) in
    let v = P.value e.p e.c state x in
    if Float.abs v <= small || n = 0 || not (Float.is_finite v) then x
    else if v > 0. = (fa > 0.) then go t v tb (if last = 1 then 0.5 *. fb else fb) 1 (n - 1)
    else go ta (if last = 2 then 0.5 *. fa else fa) t v 2 (n - 1)
  in
  if va = vb then mid xa xb else go 0. va 1. vb 0 40

(* The triangles of tetrahedra, taken one at a time: where the surface cuts each. A
   tetrahedron is four nodes, each its number, the field there and where it is, in a
   cell whose decisions are [state]. A vertex is where the field changes sign along an
   edge, and is the same vertex for every tetrahedron on that edge: so where the
   tetrahedra fill space face to face, the mesh is closed. *)
let cutter e =
  (* the vertex on the edge between two nodes, one inside and one outside: its number,
     and the middle of the edge. A triangle is turned to face outward by the middles of
     its three edges, which are never in line, where the vertices themselves can be. *)
  let cuts = Hashtbl.create 4096 and points = ref [] and triangles = ref [] in
  let vertex state (ia, va, xa) (ib, vb, xb) =
    let key = (min ia ib, max ia ib) in
    let v =
      match Hashtbl.find_opt cuts key with
      | Some v -> v
      | None ->
          let v = Hashtbl.length cuts in
          Hashtbl.add cuts key v;
          points := zero e state (va, xa) (vb, xb) :: !points;
          v
    in
    (v, mid xa xb)
  in
  let middle l = scale (1. /. float_of_int (List.length l)) (List.fold_left (fun s (_, _, x) -> add s x) [| 0.; 0.; 0. |] l) in
  let tet state corners =
    let inside, outside = List.partition (fun (_, v, _) -> v <= 0.) corners in
    let triangle (a, pa) (b, pb) (c, pc) =
      triangles := (if dot (cross (sub pb pa) (sub pc pa)) (sub (middle outside) (middle inside)) >= 0. then (a, b, c) else (a, c, b)) :: !triangles
    in
    let vertex = vertex state in
    match (inside, outside) with
    | [ i ], [ a; b; c ] | [ a; b; c ], [ i ] -> triangle (vertex i a) (vertex i b) (vertex i c)
    | [ i; j ], [ k; l ] ->
        let q0 = vertex i k and q1 = vertex i l and q2 = vertex j l and q3 = vertex j k in
        triangle q0 q1 q2;
        triangle q0 q2 q3
    | _ -> ()
  in
  (tet, fun () -> (Array.of_list (List.rev !points), !triangles))

(* A cube's corners are numbered by bits (1 x, 2 y, 4 z): six tetrahedra about 0-7. *)
let tetrahedra = [ [ 0; 1; 3; 7 ]; [ 0; 3; 2; 7 ]; [ 0; 2; 6; 7 ]; [ 0; 6; 4; 7 ]; [ 0; 4; 5; 7 ]; [ 0; 5; 1; 7 ] ]

(* Cells all of one size: each last-level cell that may hold surface, cut into its six
   tetrahedra. *)
let march e ~lo ~hi ~depth =
  let size = Array.fold_left Float.max 0. (sub hi lo) /. float_of_int (1 lsl depth) in
  (* a node of the grid, by its place in it: its number, the field there, where it is *)
  let nodes = Hashtbl.create 4096 in
  let node state ((i, j, k) as key) =
    match Hashtbl.find_opt nodes key with
    | Some n -> n
    | None ->
        let x = [| lo.(0) +. (float_of_int i *. size); lo.(1) +. (float_of_int j *. size); lo.(2) +. (float_of_int k *. size) |] in
        let n = (Hashtbl.length nodes, P.value e.p e.c state x, x) in
        Hashtbl.add nodes key n;
        n
  in
  let tet, made = cutter e in
  Octree.shell e.p e.c ~lo ~hi ~depth (fun l _ state ->
      let at a = int_of_float (Float.round ((l.(a) -. lo.(a)) /. size)) in
      let corner = Array.init 8 (fun o -> node state (at 0 + (o land 1), at 1 + ((o lsr 1) land 1), at 2 + ((o lsr 2) land 1))) in
      List.iter (fun t -> tet state (List.map (fun o -> corner.(o)) t)) tetrahedra);
  let points, triangles = made () in
  (size, points, triangles)

(* ---- Cells of several sizes ---- *)

(* For a mesh to a tolerance the cells are sized to the surface: large where it is flat
   and a face is alone, small where it curves or where its faces are near each other,
   and no smaller than [least]. They are an octree's leaves, and are numbered on the
   lattice of half a smallest cell, so that the corners, the middles of the sides and
   the middle of every cell are points of it: a point is its three whole numbers in one. *)
type lattice = { from : float array; step : float; levels : int  (** the whole is 2^levels smallest cells a side *) }

let spot i j k = i lor (j lsl 20) lor (k lsl 40)
let unspot q = (q land 0xFFFFF, (q lsr 20) land 0xFFFFF, q lsr 40)
let at (l : lattice) q = let i, j, k = unspot q in [| l.from.(0) +. (float_of_int i *. l.step); l.from.(1) +. (float_of_int j *. l.step); l.from.(2) +. (float_of_int k *. l.step) |]

(* What the extraction found in and about a cell, for the cells of every level: the
   edges that pass within a side of it, and the corners in it. An edge's pieces are
   first joined end to end into lines. Of a line that passes through the cell itself
   (or within a quarter of its side) more is kept: the first and last of its points
   there and how many there are (all of them between the two, if it passes through
   once), and how far it turns on the way. *)
type strand = { edge : int; line : int; mutable inside : bool; mutable low : int; mutable high : int; mutable count : int; mutable along : float array; mutable turned : float  (** the cosine of how far it turns in the cell *) }
type known = { strands : (int, strand list ref) Hashtbl.t array; corners : (int, int list ref) Hashtbl.t array }

let joined ~within pieces =
  let last p = List.nth p (List.length p - 1) in
  let rec grow line rest =
    let rec pick seen = function
      | [] -> None
      | p :: more ->
          let others = List.rev_append seen more in
          if dist (last line) (List.hd p) <= within then Some (line @ List.tl p, others)
          else if dist (last line) (last p) <= within then Some (line @ List.tl (List.rev p), others)
          else if dist (List.hd line) (last p) <= within then Some (p @ List.tl line, others)
          else if dist (List.hd line) (List.hd p) <= within then Some (List.rev p @ List.tl line, others)
          else pick (p :: seen) more
    in
    match pick [] rest with Some (line, rest) -> grow line rest | None -> (line, rest)
  in
  let rec all = function [] -> [] | p :: rest -> let line, rest = grow p rest in line :: all rest in
  all (List.filter (fun p -> p <> []) pieces)

let known (l : lattice) ~first ~within (x : X.t) =
  let tables () = Array.init (l.levels + 1) (fun _ -> Hashtbl.create 1024) in
  let strands = tables () and corners = tables () in
  (* the cells of level d whose box, [pad] sides wider all round, holds p: each with
     whether it does a quarter of a side wider *)
  let cells d ~pad p f =
    let s = 1 lsl (l.levels + 1 - d) in
    let side = float_of_int s *. l.step in
    let t = Array.init 3 (fun a -> (p.(a) -. l.from.(a)) /. side) in
    let span a = (max 0 (int_of_float (Float.floor (t.(a) -. pad))), min ((1 lsl d) - 1) (int_of_float (Float.floor (t.(a) +. pad)))) in
    let close a i = t.(a) >= float_of_int i -. 0.25 && t.(a) <= float_of_int i +. 1.25 in
    let i0, i1 = span 0 and j0, j1 = span 1 and k0, k1 = span 2 in
    for i = i0 to i1 do for j = j0 to j1 do for k = k0 to k1 do f (spot (i * s) (j * s) (k * s)) (close 0 i && close 1 j && close 2 k) done done done
  in
  let entry table q = match Hashtbl.find_opt table q with Some r -> r | None -> let r = ref [] in Hashtbl.add table q r; r in
  List.iteri
    (fun edge (e : X.edge) ->
      List.iteri
        (fun line points ->
          let n = ref 0 in
          let put p along =
            for d = first to l.levels do
              (* for the cells of a level, points half a side apart *)
              let stride = max 1 (1 lsl (l.levels - d - 1)) in
              if !n mod stride = 0 then begin
                let at = !n / stride in
                cells d ~pad:1. p (fun q inside ->
                    let here = entry strands.(d) q in
                    let s =
                      match List.find_opt (fun s -> s.edge = edge && s.line = line) !here with
                      | Some s -> s
                      | None -> let s = { edge; line; inside = false; low = 0; high = 0; count = 0; along; turned = 1. } in here := s :: !here; s
                    in
                    if inside then begin
                      if s.inside then (s.low <- min s.low at; s.high <- max s.high at; s.turned <- Float.min s.turned (Float.abs (dot s.along along)))
                      else (s.inside <- true; s.low <- at; s.high <- at; s.along <- along);
                      s.count <- s.count + 1
                    end)
              end
            done;
            incr n
          in
          (* points along it a smallest cell apart, at most *)
          let rec walk = function
            | a :: (b :: _ as rest) ->
                let length = dist a b in
                if length > 0. then begin
                  let along = scale (1. /. length) (sub b a) in
                  let k = max 1 (int_of_float (Float.ceil (length /. (2. *. l.step)))) in
                  for i = 0 to k - 1 do put (add a (scale (float_of_int i *. length /. float_of_int k) along)) along done;
                  if List.length rest = 1 then put b along
                end;
                walk rest
            | _ -> ()
          in
          walk points)
        (joined ~within e.pieces))
    x.edges;
  List.iteri (fun v (c : X.vertex) -> for d = first to l.levels do cells d ~pad:0.25 c.at (fun q _ -> let here = entry corners.(d) q in if not (List.mem v !here) then here := v :: !here) done) x.vertices;
  { strands; corners }

(* The field at a point of the lattice, kept: a node is its number, the field there
   and where it is. *)
let node p c (l : lattice) nodes state q =
  match Hashtbl.find_opt nodes q with
  | Some n -> n
  | None ->
      let x = at l q in
      let n = (Hashtbl.length nodes, P.value p c state x, x) in
      Hashtbl.add nodes q n;
      n

(* A cell's eight corners, by bits (1 x, 2 y, 4 z), its origin i, j, k and its side s. *)
let corner i j k s o = spot (i + if o land 1 <> 0 then s else 0) (j + if o land 2 <> 0 then s else 0) (k + if o land 4 <> 0 then s else 0)

(* Is the field in a cell that one face fills as good as straight? Then the triangles of
   its tetrahedra are within [within] of the surface: the field at its middle and at the
   middles of its sides is that far, at most, from what its corners say. A plane passes
   at any size; a cylinder of radius r where the cell's side squared is 8 r [within]. *)
let straight value i j k s ~within =
  let h = s / 2 in
  let c = Array.init 8 (fun o -> value (corner i j k s o)) in
  let mean l = List.fold_left (fun a o -> a +. c.(o)) 0. l /. float_of_int (List.length l) in
  let near l di dj dk = Float.abs (value (spot (i + di) (j + dj) (k + dk)) -. mean l) <= within in
  near [ 0; 1; 2; 3; 4; 5; 6; 7 ] h h h
  && near [ 0; 2; 4; 6 ] 0 h h && near [ 1; 3; 5; 7 ] s h h && near [ 0; 1; 4; 5 ] h 0 h && near [ 2; 3; 6; 7 ] h s h && near [ 0; 1; 2; 3 ] h h 0 && near [ 4; 5; 6; 7 ] h h s

(* Is a cell with several faces one the march and Conform can take as it is? It is if
   one edge passes through it, once and all but straight, and the field at its corners
   and middle is of that edge's two faces; or if the edges in it all meet at one corner
   that is in it, and the field is of that corner's faces. If the extraction has no
   edge in it, it is if its field is [straight] all the same (two faces that lie on
   one another, or run into each other with no turn), or is of the two faces of an edge
   that passes within a side of it (the cell is beside a crease, and sees it), or is
   two straight pieces meeting ([folded], below: a crease the extraction did not find,
   as it finds none between two pieces of one name). Anything else has faces nearer
   each other than the cell is wide: a thin wall, a slot, a small hole, two edges side
   by side. *)
let plain p c (l : lattice) (k : known) (edges : (int * int * int list) array) (corners : int list array) ~straight ~within (cell : Octree.cell) i j kk s d =
  let q = spot i j kk in
  (* the field at the cell's middle and corners, with where *)
  let read = lazy (let at q = let x = at l q in (x, P.eval p c cell.state x) in let h = s / 2 in at (spot (i + h) (j + h) (kk + h)) :: List.init 8 (fun o -> at (corner i j kk s o))) in
  (* does the plane a place says its piece of surface is in come into the cell (or
     within a tenth of its side)? Told at the same nine places. *)
  let comes (x0, (r : P.sample)) =
    let slope = norm r.d.g in
    let values = List.map (fun (x, _) -> r.d.v +. dot r.d.g (sub x x0)) (Lazy.force read) in
    slope = 0. || List.exists (fun v -> Float.abs v <= 0.1 *. cell.size *. slope) values || (List.exists (fun v -> v > 0.) values && List.exists (fun v -> v < 0.) values)
  in
  (* the field is of these faces: or of others whose surface is not in the cell (the
     far side of a wall, nearer some corner than the side that is in it) *)
  let faces_are allowed = List.for_all (fun ((_, (r : P.sample)) as place) -> List.mem r.face allowed || not (comes place)) (Lazy.force read) in
  let of_edge (s : strand) = let a, b, _ = edges.(s.edge) in faces_are [ a; b ] in
  (* Two pieces of surface, each straight (what one place says of it holds at the
     others, to [within]), and the field the greater of the two throughout or the
     lesser: one crease, or none. Not if they face nearly opposite ways and both come
     into the cell, which is a wall or a gap thinner than the cell, or a blade; with
     one of them outside it, it is one side of a wall and the march takes it (its
     vertices are found between two nodes, not nearest a guess). *)
  let folded () =
    let read = Lazy.force read in
    match List.sort_uniq compare (List.map (fun (_, (r : P.sample)) -> r.part) read) with
    | [ a; b ] ->
        let first q = List.find (fun (_, (r : P.sample)) -> r.part = q) read in
        let plane (x0, (r : P.sample)) x = r.d.v +. dot r.d.g (sub x x0) in
        let la = plane (first a) and lb = plane (first b) in
        let fits f = List.for_all (fun (x, (r : P.sample)) -> Float.abs (f x -. r.d.v) <= within) read in
        (turn (snd (first a)).d.g (snd (first b)).d.g <= 150. || not (comes (first a) && comes (first b)))
        && List.for_all (fun (x, (r : P.sample)) -> Float.abs ((if r.part = a then la x else lb x) -. r.d.v) <= within) read
        && (fits (fun x -> Float.max (la x) (lb x)) || fits (fun x -> Float.min (la x) (lb x)))
    | _ -> false
  in
  let about = Option.value ~default:[] (Option.map ( ! ) (Hashtbl.find_opt k.strands.(d) q)) in
  match List.filter (fun (s : strand) -> s.inside) about with
  | [] -> (straight () || List.exists of_edge about || folded ())
  | [ one ] ->
      (* it may turn as far as leaves a chord across the cell within twice [within] of it, and no further than 37 degrees *)
      (one.turned >= Float.max 0.8 (Float.cos (Float.min 1. (16. *. within /. cell.size))) && one.count = one.high - one.low + 1) && (of_edge one)
  | several -> (
      match Option.map ( ! ) (Hashtbl.find_opt k.corners.(d) q) with
      | Some [ v ] -> (List.for_all (fun (s : strand) -> let _, _, ends = edges.(s.edge) in List.mem v ends) several) && (faces_are corners.(v))
      | Some (_ :: _ :: _) -> false
      | _ -> false)

(* The octree: a cell that may hold surface is split while [wanted] says so, down to
   the smallest. Then the leaves are made to differ by one level at most where they
   touch (side, edge or corner): a leaf that may hold surface is split if one smaller
   than half of it touches it. The march needs that. *)
let sized p c (l : lattice) ~lo ~wanted =
  let whole = 1 lsl (l.levels + 1) and count = ref 1 in
  let kid (cell : Octree.cell) o =
    incr count;
    let h = 0.5 *. cell.size in
    let k = { Octree.lo = Array.mapi (fun a v -> if o land (1 lsl a) <> 0 then v +. h else v) cell.lo; size = h; state = cell.state; live = 0; surface = true; kids = [||] } in
    let state, live, surface = Octree.prune p c cell.state (Octree.centre k) (Octree.radius k) in
    { k with state; live = List.length live; surface }
  in
  let split (cell : Octree.cell) = cell.kids <- Array.init 8 (fun o -> Some (kid cell o)) in
  let each (cell : Octree.cell) i j k s f =
    let h = s / 2 in
    Array.iteri (fun o kid -> Option.iter (fun c -> f c (i + if o land 1 <> 0 then h else 0) (j + if o land 2 <> 0 then h else 0) (k + if o land 4 <> 0 then h else 0) h) kid) cell.kids
  in
  let root =
    let r = { Octree.lo; size = float_of_int whole *. l.step; state = P.whole p; live = 0; surface = true; kids = [||] } in
    let state, live, surface = Octree.prune p c r.state (Octree.centre r) (Octree.radius r) in
    { r with state; live = List.length live; surface }
  in
  let rec grow (cell : Octree.cell) i j k s d = if cell.surface && s > 2 && wanted cell i j k s d then (split cell; each cell i j k s (fun c i j k s -> grow c i j k s (d + 1))) in
  grow root 0 0 0 whole 0;
  let rec leaf_at (cell : Octree.cell) ci cj ck s i j k =
    if Array.length cell.kids = 0 then (cell, ci, cj, ck, s)
    else
      let h = s / 2 in
      let bi = if i >= ci + h then 1 else 0 and bj = if j >= cj + h then 1 else 0 and bk = if k >= ck + h then 1 else 0 in
      match cell.kids.(bi lor (bj lsl 1) lor (bk lsl 2)) with Some kid -> leaf_at kid (ci + (bi * h)) (cj + (bj * h)) (ck + (bk * h)) h i j k | None -> (cell, ci, cj, ck, s)
  in
  let rank s = let rec go r s = if s <= 1 then r else go (r + 1) (s lsr 1) in go 0 s in
  let waiting = Array.make (l.levels + 2) [] in
  let wait c i j k s = waiting.(rank s) <- (c, i, j, k, s) :: waiting.(rank s) in
  let rec gather (cell : Octree.cell) i j k s = if Array.length cell.kids = 0 then wait cell i j k s else each cell i j k s gather in
  gather root 0 0 0 whole;
  (* the smallest first: what a leaf has split is larger than it, and waits its turn *)
  for r = 1 to l.levels + 1 do
    let rec work () =
      match waiting.(r) with
      | [] -> ()
      | ((cell : Octree.cell), i, j, k, s) :: rest ->
          waiting.(r) <- rest;
          if Array.length cell.kids = 0 then
            for di = -1 to 1 do for dj = -1 to 1 do for dk = -1 to 1 do
              let pi = i + (di * s) + 1 and pj = j + (dj * s) + 1 and pk = k + (dk * s) + 1 in
              if (di <> 0 || dj <> 0 || dk <> 0) && pi >= 0 && pj >= 0 && pk >= 0 && pi < whole && pj < whole && pk < whole then begin
                let rec even () =
                  let (m : Octree.cell), mi, mj, mk, ms = leaf_at root 0 0 0 whole pi pj pk in
                  if m.surface && ms > 2 * s then (split m; each m mi mj mk ms wait; even ())
                in
                even ()
              end
            done done done;
          work ()
    in
    work ()
  done;
  let leaves = ref [] in
  let rec gather (cell : Octree.cell) i j k s = if Array.length cell.kids = 0 then leaves := (cell, i, j, k, s) :: !leaves else each cell i j k s gather in
  gather root 0 0 0 whole;
  ({ Octree.root; whole = P.whole p; cells = !count; leaves = [] }, !leaves)

(* The march over cells of several sizes. Each leaf that may hold surface is cut into
   tetrahedra about its middle, one on each triangle of its six sides, and a side is
   cut into triangles the same way from both its cells:
   - a side whose other cell is split is four squares, each cut as a side of its own;
   - a square with a corner of a smaller cell at the middle of one of its edges is a
     fan about its own middle, through those middles;
   - any other square is cut in two across the diagonal from its lowest corner.
   The leaves differing by one level at most where they touch, there is nothing
   else, and the tetrahedra fill space face to face. *)
let march_sized e (l : lattice) nodes leaves =
  let corners = Hashtbl.create (1 lsl 16) in
  List.iter (fun (_, i, j, k, s) -> for o = 0 to 7 do Hashtbl.replace corners (corner i j k s o) () done) leaves;
  let there q = Hashtbl.mem corners q in
  let tet, made = cutter e in
  List.iter
    (fun ((cell : Octree.cell), i, j, k, s) ->
      if cell.surface then begin
        let h = s / 2 in
        let n = node e.p e.c l nodes cell.state in
        let middle = n (spot (i + h) (j + h) (k + h)) in
        let triangle a b c = tet cell.state [ middle; n a; n b; n c ] in
        let square a b c d = if min a c < min b d then (triangle a b c; triangle a c d) else (triangle b c d; triangle b d a) in
        for axis = 0 to 2 do
          for far = 0 to 1 do
            (* a point of the side: u and v along it *)
            let p u v = match axis with 0 -> spot (i + (far * s)) (j + u) (k + v) | 1 -> spot (i + u) (j + (far * s)) (k + v) | _ -> spot (i + u) (j + v) (k + (far * s)) in
            let c0 = p 0 0 and c1 = p s 0 and c2 = p s s and c3 = p 0 s and m0 = p h 0 and m1 = p s h and m2 = p h s and m3 = p 0 h and f = p h h in
            if there f then (square c0 m0 f m3; square m0 c1 m1 f; square f m1 c2 m2; square m3 f m2 c3)
            else
              match List.concat_map (fun (c, m) -> if there m then [ c; m ] else [ c ]) [ (c0, m0); (c1, m1); (c2, m2); (c3, m3) ] with
              | [ _; _; _; _ ] -> square c0 c1 c2 c3
              | round ->
                  let rec fan = function a :: (b :: _ as rest) -> triangle f a b; fan rest | _ -> () in
                  fan (round @ [ List.hd round ])
          done
        done
      end)
    leaves;
  made ()

(* ---- On every core ---- *)

(* [f] of each item of an array, the work shared among the cores (eight at most): each
   core is given something of its own to work with by [own], which for the field is a
   scratch to evaluate in. The answers come back in the items' order, the same whatever
   the cores. A short array is not worth sharing out. *)
let spread own f items =
  let n = Array.length items in
  let cores = max 1 (min 8 (Domain.recommended_domain_count ())) in
  if cores = 1 || n < 4096 then (let mine = own () in Array.map (f mine) items)
  else begin
    let out = Array.make n None and next = Atomic.make 0 in
    let work () =
      let mine = own () in
      let rec go () =
        let i = Atomic.fetch_and_add next 256 in
        if i < n then begin
          for k = i to min n (i + 256) - 1 do out.(k) <- Some (f mine items.(k)) done;
          go ()
        end
      in
      go ()
    in
    let others = List.init (cores - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join others;
    Array.map Option.get out
  end

(* The field's scratch is for one core: another core's own. *)
let own e () = { e with c = P.ctx e.p }

(* ---- Conform ---- *)

(* An end of an edge is next to a crossing within this share of the edge. *)
let next_to = 0.15

let faces_on = function Face f -> [ f ] | Crease (a, b) -> [ a; b ] | Corner l -> l

(* The two pieces an edge along a crease lies between, by what its ends are on. *)
let pieces_of ku kv = match (ku, kv) with Crease (a, b), _ when a >= 0 -> Some (a, b) | _, Crease (a, b) when a >= 0 -> Some (a, b) | _ -> None

(* Two vertices on two different creases: an edge between them runs along neither,
   whatever lies either side of it (next to a corner a triangle is too small to say). *)
let apart ku kv = match (ku, kv) with Crease (a, b), Crease (c, d) -> a >= 0 && c >= 0 && (a, b) <> (c, d) | _ -> false

let share a b = List.exists (fun f -> List.mem f (faces_on b)) (faces_on a)

(* Two points of the surface in different faces, brought together by halving: the
   middle of the two lands on one face, nearer one of them than the other, so each step
   takes a share off, not a half. *)
let pinch e a b =
  let fa = part e a in
  let rec halve a b k =
    if k = 0 || dist a b < 1e-10 *. e.diag then (a, b) else match to_surface e (mid a b) with Some m -> if part e m = fa then halve m b (k - 1) else halve a m (k - 1) | None -> (a, b)
  in
  halve a b 300

(* The crease between two points of the surface on different pieces, at its point
   nearest p, with the two pieces: none if there is none within [reach] of p, or the
   surface does not turn there. The two points' tangent planes meet in a line, and the
   point of that line nearest p is taken; two points a hair either side of it are put
   on the surface, and the same again. Where the faces are flat that is exact at once,
   and where they curve it is in two or three rounds. Failing that, the two points are
   halved together (Pinch), which always arrives but takes ten times as long. Two
   points between which the surface does not turn are first brought nearer by a few
   halvings, and if it still does not turn there, there is no crease: a seam. *)
let crease_between e ~sharp ~reach p a b =
  let rec close a b last rounds =
    let ra = probe e a and rb = probe e b in
    let ga = ra.d.g and gb = rb.d.g in
    let line = cross ga gb in
    if rounds = 0 || turn ga gb < sharp then None
    else
      match X.solve3 [| ga; gb; line |] [| dot ga a; dot gb b; dot line p |] with
      | Some x when dist x p <= reach -> (
          if (match last with Some y -> dist x y < 1e-10 *. e.diag | None -> false) then Some (x, ra.part, rb.part)
          else
            (* a hair into each face from the line, on the side its point was *)
            let into g q = let d = cross line g in read_on_surface e (add x (scale ((if dot d (sub q x) >= 0. then 1e-6 else -1e-6) *. reach /. norm d) d)) in
            match (into ga a, into gb b) with Some (a', (sa : P.sample)), Some (b', (sb : P.sample)) when sa.part <> sb.part -> close a' b' (Some x) (rounds - 1) | _ -> None)
      | _ -> None
  in
  let halved () =
    let a, b = pinch e a b in
    let ra = probe e a and rb = probe e b in
    if dist a b <= 1e-7 *. e.diag && turn ra.d.g rb.d.g >= sharp && dist (mid a b) p <= reach then Some (mid a b, ra.part, rb.part) else None
  in
  if turn (probe e a).d.g (probe e b).d.g >= sharp then match close a b None 6 with Some r -> Some r | None -> halved ()
  else
    (* no turn from one to the other: a seam, unless something narrow lies between *)
    let fa = part e a in
    let rec nearer a b k = if k = 0 then (a, b) else match read_on_surface e (mid a b) with Some (m, r) -> if r.part = fa then nearer m b (k - 1) else nearer a m (k - 1) | None -> (a, b) in
    let a, b = nearer a b 8 in
    if turn (probe e a).d.g (probe e b).d.g >= sharp then match close a b None 6 with Some r -> Some r | None -> halved () else None

(* Where a mesh edge first crosses a crease, if it does: the place, what it is on, and
   how far along the edge (0 to 1). Its two ends being in one face does not say it
   crosses none (the corner of another face can reach across it), so the pieces of
   surface are read at five places along it, each put on the surface, and a change
   between two of them is run down to the crease. A change with no turn in the surface
   is a tangent seam, and is passed over. So is one right next to an end that is itself
   on a crease: it is that crease again, the edge running off along it. *)
let crossing e ~sharp (pu, ou) (pv, ov) =
  (* a place on the surface, with the piece of surface there *)
  let at t = Option.map (fun (x, (r : P.sample)) -> (x, r.part)) (read_on_surface e (add pu (scale t (sub pv pu)))) in
  let end_ p o t = match o with Face f -> Some (p, f) | _ -> at t in
  let length = dist pu pv in
  let own o t = match o with Face _ -> false | _ -> t < next_to in
  let rec first = function
    | (a, fa) :: ((b, fb) :: _ as rest) ->
        if fa = fb then first rest
        else (
          match crease_between e ~sharp ~reach:(dist a b) (mid a b) a b with
          | Some (x, pa, pb) ->
              let t = dist pu x /. length in
              if not (own ou t || own ov (1. -. t)) then Some (x, Crease (min pa pb, max pa pb), t) else first rest
          | None -> first rest)
    | _ -> None
  in
  if length = 0. then None else first (List.filter_map Fun.id [ end_ pu ou 0.02; at 0.25; at 0.5; at 0.75; end_ pv ov 0.98 ])

(* Each triangle with its split edges: one, two or all three, the new vertices shared
   with the triangles next to it. *)
let subdivide cut triangles =
  List.concat_map
    (fun (a, b, c) ->
      match (cut a b, cut b c, cut c a) with
      | None, None, None -> [ (a, b, c) ]
      | Some m, None, None -> [ (a, m, c); (m, b, c) ]
      | None, Some m, None -> [ (b, m, a); (m, c, a) ]
      | None, None, Some m -> [ (c, m, b); (m, a, b) ]
      | Some m, Some n, None -> [ (m, b, n); (a, m, n); (a, n, c) ]
      | None, Some m, Some n -> [ (m, c, n); (b, m, n); (b, n, a) ]
      | Some n, None, Some m -> [ (m, a, n); (c, m, n); (c, n, b) ]
      | Some m, Some n, Some o -> [ (a, m, o); (m, b, n); (o, n, c); (m, n, o) ])
    triangles

(* Until no edge crosses a crease: an edge crossed near its middle is split there, and
   one crossed next to an end has that end moved onto the crease instead, which leaves
   no sliver and takes an edge lying along a crease in its stride. An edge is looked
   along if a boundary can be near it: its ends are in different faces, or one of them
   is on a crease or in a cell where a select is still undecided. An edge found to
   cross none is not asked again unless an end of it has moved: after the first round
   only the edges at a vertex moved or made in the round before are. A piece of an edge
   already split is not split again once it is shorter than a quarter of a cell: what
   crosses it then is finer than the cells (a hole a cell wide, two faces lying on one
   another), and splitting would chase it without end. *)
let conform e ~sharp ~size points triangles =
  let marched = Array.length points in
  (* [fresh]: the vertices moved or made in the round before (all of them, at first):
     only an edge with such an end is looked along *)
  let rec pass points on near fresh triangles rounds =
    let splits = Hashtbl.create 1024 and added = ref [] and snaps = Hashtbl.create 1024 in
    (* the edges to look along, each once (a closed mesh has every edge once each way
       round, so once with its lower end first), in the order the triangles give them *)
    let edges = ref [] in
    let ask u v = if u < v && (fresh.(u) || fresh.(v)) && (near.(u) || near.(v) || not (match (on.(u), on.(v)) with Face f, Face g -> f = g | a, b -> share a b)) then edges := (u, v) :: !edges in
    List.iter (fun (a, b, c) -> ask a b; ask b c; ask c a) triangles;
    let edges = Array.of_list (List.rev !edges) in
    (* looked along on every core: one edge's answer does not depend on another's *)
    let crossings =
      spread (own e)
        (fun e (u, v) ->
          let piece = max u v >= marched && dist points.(u) points.(v) < 0.25 *. size in
          if piece then None else crossing e ~sharp (points.(u), on.(u)) (points.(v), on.(v)))
        edges
    in
    (* and taken in order: an edge with an end that has just been moved waits a round *)
    Array.iteri
      (fun i (u, v) ->
        if not (Hashtbl.mem snaps u || Hashtbl.mem snaps v) then
          match crossings.(i) with
          | Some (x, what, t) when t < next_to -> Hashtbl.replace snaps u (x, what)
          | Some (x, what, t) when t > 1. -. next_to -> Hashtbl.replace snaps v (x, what)
          | Some (x, what, _) ->
              Hashtbl.add splits (u, v) (Array.length points + Hashtbl.length splits);
              added := (x, what) :: !added
          | None -> ())
      edges;
    if (Hashtbl.length splits = 0 && Hashtbl.length snaps = 0) || rounds = 0 then (points, on, triangles)
    else begin
      let added = Array.of_list (List.rev !added) in
      let grown a more = Array.append a more in
      let points = grown points (Array.map fst added) and on = grown on (Array.map snd added) and near = grown near (Array.map (fun _ -> true) added) in
      let fresh = Array.make (Array.length points) false in
      Array.iteri (fun i _ -> fresh.(Array.length points - Array.length added + i) <- true) added;
      Hashtbl.iter (fun u (x, what) -> points.(u) <- x; on.(u) <- what; near.(u) <- true; fresh.(u) <- true) snaps;
      (* a vertex at an end of a split edge: only between two of those is an edge looked up *)
      let ends = Array.make (Array.length points) false in
      Hashtbl.iter (fun (u, v) _ -> ends.(u) <- true; ends.(v) <- true) splits;
      let triangles = if Hashtbl.length splits = 0 then triangles else subdivide (fun a b -> if ends.(a) && ends.(b) then Hashtbl.find_opt splits (min a b, max a b) else None) triangles in
      pass points on near fresh triangles (rounds - 1)
    end
  in
  let read = spread (own e) (fun e x -> probe e x) points in
  pass (Array.copy points) (Array.map (fun (r : P.sample) -> Face r.part) read) (Array.map (fun (r : P.sample) -> r.reached <> []) read) (Array.make (Array.length points) true) triangles 8

(* ---- Corners ---- *)

(* Does the surface turn at x? Looked at from all round, a twentieth of a cell away. *)
let turns e ~sharp ~size x =
  let round = [ [| 1.; 0.; 0. |]; [| -1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; -1.; 0. |]; [| 0.; 0.; 1. |]; [| 0.; 0.; -1. |]; [| 1.; 1.; 1. |]; [| -1.; 1.; 1. |]; [| 1.; -1.; 1. |]; [| -1.; -1.; 1. |]; [| 1.; 1.; -1. |]; [| -1.; 1.; -1. |]; [| 1.; -1.; -1. |]; [| -1.; -1.; -1. |] ] in
  let normals = List.filter_map (fun d -> Option.map (fun y -> (probe e y).d.g) (to_surface e (add x (scale (0.05 *. size /. norm d) d)))) round in
  List.exists (fun a -> List.exists (fun b -> turn a b >= sharp) normals) normals

(* The extraction's corners where the surface turns, put into the mesh. The nearest
   vertex, if it is within a third of a cell, is moved onto the corner; otherwise the
   triangle the corner lies in is split in three about it; and if it lies in none (the
   mesh cuts a pointed corner off), the nearest vertex within a cell and a half is
   moved after all. *)
let place ?size_at ?close e ~sharp ~size (x : X.t) (points, on, triangles) =
  (* [size] is the side of the cells where they are all one size; where they are not,
     [size_at] is the side of the cell at a place, and [size] that of the smallest.
     [close]: how near a vertex must be to be moved onto a corner, a third of a cell
     unless said: among large cells that far would fold the mesh over. *)
  let size_at = Option.value size_at ~default:(fun _ -> size) and bin = match size_at with Some _ -> 4. *. size | None -> 1.5 *. size in
  let points = Array.copy points and on = Array.copy on and triangles = Array.of_list triangles in
  let cell p = (int_of_float (Float.floor (p.(0) /. bin)), int_of_float (Float.floor (p.(1) /. bin)), int_of_float (Float.floor (p.(2) /. bin))) in
  (* what a table holds within [reach] of p, to a bin *)
  let near table p reach =
    let i, j, k = cell p and r = int_of_float (Float.ceil (reach /. bin)) and found = ref [] in
    for a = -r to r do for b = -r to r do for c = -r to r do found := List.rev_append (Hashtbl.find_all table (i + a, j + b, k + c)) !found done done done;
    !found
  in
  let vertices = Hashtbl.create 4096 and holding = Hashtbl.create 4096 in
  Array.iteri (fun v p -> Hashtbl.add vertices (cell p) v) points;
  Array.iteri (fun t (a, b, c) -> Hashtbl.add holding (cell (scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))))) t) triangles;
  (* vertices added, newest first: its number, where it is, what it is on *)
  let more = ref [] and more_triangles = ref [] in
  let at v = if v < Array.length points then points.(v) else (fun (_, p, _) -> p) (List.find (fun (u, _, _) -> u = v) !more) in
  let wanted = List.filter (fun (v : X.vertex) -> turns e ~sharp ~size:(size_at v.at) v.at) x.vertices in
  let placed =
    List.filter
      (fun (v : X.vertex) ->
        let size = size_at v.at in
        let corner = Corner (List.sort_uniq compare (List.map (P.face_id e.p) v.faces)) in
        let nearest =
          List.sort compare
            (List.filter_map (fun u -> match on.(u) with Corner _ -> None | _ -> let d = dist points.(u) v.at in if d <= 1.5 *. size then Some (d, u) else None) (near vertices v.at (1.5 *. size)))
        in
        let move u = points.(u) <- v.at; on.(u) <- corner; true in
        match nearest with
        | (d, u) :: _ when d <= Option.value close ~default:(size /. 3.) -> move u
        | _ -> (
            (* the triangles it lies within, by how far it is out of their plane *)
            let within t =
              let a, b, c = triangles.(t) in
              let pa = at a and pb = at b and pc = at c in
              let n = cross (sub pb pa) (sub pc pa) in
              let n2 = dot n n in
              let weight p q = dot (cross (sub p v.at) (sub q v.at)) n /. n2 in
              if n2 > 0. && weight pb pc >= 0. && weight pc pa >= 0. && weight pa pb >= 0. then Some (Float.abs (dot (sub v.at pa) n) /. sqrt n2, t) else None
            in
            match List.sort compare (List.filter_map within (near holding v.at (1.5 *. size))) with
            | (off, t) :: _ when off <= 0.5 *. size ->
                let a, b, c = triangles.(t) and u = Array.length points + List.length !more in
                more := (u, v.at, corner) :: !more;
                triangles.(t) <- (a, b, u);
                more_triangles := (b, c, u) :: (c, a, u) :: !more_triangles;
                true
            | _ -> ( match nearest with (d, u) :: _ when d <= 1.5 *. size -> move u | _ -> false)))
      wanted
  in
  let more = List.rev !more in
  ( Array.append points (Array.of_list (List.map (fun (_, p, _) -> p) more)),
    Array.append on (Array.of_list (List.map (fun (_, _, o) -> o) more)),
    Array.append triangles (Array.of_list !more_triangles),
    (List.length placed, List.length wanted) )

(* ---- Even ---- *)

(* The second pass: the triangles made even, as Botsch and Kobbelt do it. Five times
   over: an edge longer than 4/3 of the length wanted is split at its middle, one
   shorter than 4/5 is closed up, an edge is turned over where that brings its four
   vertices nearer six edges each, and each vertex is moved to the middle of the ones
   round it. Every vertex moved or made is put back on the surface.

   What is attached stays attached. A corner does not move and is not removed. A vertex
   on a crease moves only along it (the middle of its two neighbours on the crease, put
   back on the crease) and is closed up only into one of those two. An edge along a
   crease is split on the crease and never turned over. Nothing is done that would turn
   a triangle to face the other way. *)

(* The mesh while it changes: three vertices a triangle (-1 once it is gone), the
   triangles at each vertex, and the edges along creases. *)
type work = {
  e : env;
  sharp : float;
  length : float;
  mutable pts : float array array;
  mutable kinds : on array;
  mutable vertices : int;
  mutable tri : int array;
  mutable triangles : int;
  mutable around : int list array;
  creased : (int * int, unit) Hashtbl.t;
}

let key u v = (min u v, max u v)
let corners w t = (w.tri.(3 * t), w.tri.((3 * t) + 1), w.tri.((3 * t) + 2))
let has w v t = let a, b, c = corners w t in a = v || b = v || c = v
let grown a n fill = if n < Array.length a then a else Array.append a (Array.make (max 16 (Array.length a)) fill)

let add_vertex w p kind =
  let v = w.vertices in
  w.pts <- grown w.pts v p;
  w.kinds <- grown w.kinds v kind;
  w.around <- grown w.around v [];
  w.pts.(v) <- p;
  w.kinds.(v) <- kind;
  w.around.(v) <- [];
  w.vertices <- v + 1;
  v

let add_triangle w (a, b, c) =
  let t = w.triangles in
  w.tri <- grown w.tri ((3 * t) + 2) (-1);
  w.tri.(3 * t) <- a;
  w.tri.((3 * t) + 1) <- b;
  w.tri.((3 * t) + 2) <- c;
  List.iter (fun v -> w.around.(v) <- t :: w.around.(v)) [ a; b; c ];
  w.triangles <- t + 1

let remove w t =
  let a, b, c = corners w t in
  List.iter (fun v -> w.around.(v) <- List.filter (( <> ) t) w.around.(v)) [ a; b; c ];
  w.tri.(3 * t) <- -1

(* The vertices joined to u; the triangles on the edge u-v; a triangle's third vertex. *)
let ring w u = List.sort_uniq compare (List.concat_map (fun t -> let a, b, c = corners w t in List.filter (( <> ) u) [ a; b; c ]) w.around.(u))
let wings w u v = List.filter (has w v) w.around.(u)
let third w t u v = let a, b, c = corners w t in if a <> u && a <> v then a else if b <> u && b <> v then b else c
let along_crease w u v = Hashtbl.mem w.creased (key u v)

(* A triangle's normal, twice its area long; with one vertex somewhere else. *)
let normal w (a, b, c) = cross (sub w.pts.(b) w.pts.(a)) (sub w.pts.(c) w.pts.(a))
let moved w t u p = let a, b, c = corners w t in let at v = if v = u then p else w.pts.(v) in cross (sub (at b) (at a)) (sub (at c) (at a))

(* The edges there are now, each once. *)
let edges w =
  let found = ref [] in
  for t = 0 to w.triangles - 1 do
    if w.tri.(3 * t) >= 0 then let a, b, c = corners w t in List.iter (fun (u, v) -> if u < v then found := (u, v) :: !found) [ (a, b); (b, c); (c, a) ]
  done;
  !found

(* How a vertex is held: free on the surface, sliding along a crease between two
   others, or fixed (a corner, a crease's loose end, or where creases meet). *)
type hold = Free | Slides of int * int | Fixed

let hold w u =
  match (w.kinds.(u), List.filter (along_crease w u) (ring w u)) with
  | Corner _, _ -> Fixed
  | _, [] -> Free
  | Crease _, [ a; b ] -> Slides (a, b)
  | _ -> Fixed

(* The point of a crease nearest p, where the crease runs along [along]: none if there
   is no crease within [reach]. Two points of the surface are found either side of it,
   on different pieces, and the crease between them (Conform's [crease_between]).
   [between]: only a crease between these two pieces, if they are known (not -1): near
   a corner another crease is as near. *)
let onto_crease ?between e ~sharp ~reach p along =
  let sides s =
    let across = cross (probe e s).d.g along in
    if norm across = 0. then None
    else
      List.find_map
        (fun d ->
          let side k = to_surface e (add s (scale (k *. d *. reach /. norm across) across)) in
          match (side (-1.), side 1.) with Some a, Some b when part e a <> part e b -> Some (a, b) | _ -> None)
        [ 0.05; 0.2; 0.5 ]
  in
  Option.bind (Option.bind (to_surface ~within:reach e p) sides) (fun (a, b) ->
      match (crease_between e ~sharp ~reach p a b, between) with
      | Some (x, pa, pb), Some (qa, qb) when (min pa pb, max pa pb) <> (min qa qb, max qa qb) -> ignore x; None
      | found, _ -> Option.map (fun (x, _, _) -> x) found)

(* A triangle's new normal is sound if it has some area and faces the way the old one
   did. Where the old one had no area, the new one may have none either (a knot of
   vertices in one place is undone a vertex at a time), or must face the way [by] does. *)
let sound w ~by old fresh =
  let least = 1e-8 *. w.length *. w.length in
  if norm old > least then norm fresh > least && dot fresh old > 0. else norm fresh <= least || dot fresh by > 0.

(* An edge longer than [high] is split at its middle: on the crease, if it runs along
   one, and on the surface. *)
let split_edge ?(between = false) ?to_crease w ~reach u v =
  let on_edge = wings w u v in
  let middle = mid w.pts.(u) w.pts.(v) in
  let crease = along_crease w u v in
  let on_crease = if crease then onto_crease ?between:(if between then pieces_of w.kinds.(u) w.kinds.(v) else None) w.e ~sharp:w.sharp ~reach:(Option.value to_crease ~default:reach) middle (sub w.pts.(v) w.pts.(u)) else None in
  (* an edge said to run along a crease with no crease near its middle does not: it is
     an edge like another, cutting across what it should go round *)
  let crease = crease && (on_crease <> None || not between) && not (between && apart w.kinds.(u) w.kinds.(v)) in
  let found =
    if crease then on_crease
    else if between then to_surface_across ~within:reach w.e middle (sub w.pts.(v) w.pts.(u))
    else to_surface ~within:reach w.e middle
  in
  (* [between]: only if the new vertex comes well between the two, not next to one of
     them or off to one side (where the nearest of a crease is past its end, at a corner) *)
  let well = match found with Some x -> let along = sub w.pts.(v) w.pts.(u) in Float.abs (dot (sub x middle) along) <= 0.25 *. dot along along | None -> false in
  if on_edge <> [] && (well || not between) then begin
    let x = Option.value ~default:middle found in
    let m =
      if crease then add_vertex w x (match (w.kinds.(u), w.kinds.(v)) with (Crease _ as k), _ | _, (Crease _ as k) -> k | _ -> Crease (-1, -1))
      else add_vertex w x (Face (part w.e x))
    in
    List.iter
      (fun t ->
        let a, b, c = corners w t in
        let swap x y (a, b, c) = ((if a = x then y else a), (if b = x then y else b), if c = x then y else c) in
        remove w t;
        add_triangle w (swap v m (a, b, c));
        add_triangle w (swap u m (a, b, c)))
      on_edge;
    Hashtbl.remove w.creased (key u v);
    if crease then begin
      Hashtbl.replace w.creased (key u m) ();
      Hashtbl.replace w.creased (key m v) ()
    end
  end

let split w high = List.iter (fun (u, v) -> if dist w.pts.(u) w.pts.(v) > high then split_edge w ~reach:w.length u v) (edges w)

(* An edge shorter than [low] is closed up: one end goes into the other, if it may
   ([into]; [may], given the triangles of u that stay, can say no last; with [same]
   the two are in one place, and u goes whatever it is on, v taking that on). u
   may go into v if it is free, or slides along a crease to v, or is a knot in a crease
   (not a corner, but with other than two crease edges) a tenth of the length from v
   along it; if the two share only the two vertices across the edge (or the mesh would
   fold onto itself); if no edge comes out longer than [high]; and if no triangle is
   turned over. The crease edges of u become v's. *)
let into ?(may = fun _ -> true) ?(same = false) w high u v =
    let free =
      same
      ||
      match (w.kinds.(u), hold w u) with
      | Corner _, _ -> false
      | _, Free -> true
      | _, Slides (a, b) -> (a = v && not (along_crease w v b)) || (b = v && not (along_crease w v a))
      | _, Fixed -> along_crease w u v && dist w.pts.(u) w.pts.(v) < 0.1 *. w.length
    in
    let about = ring w u in
    let shared = wings w u v and stay = List.filter (fun t -> not (has w v t)) w.around.(u) in
    let by = List.fold_left (fun s t -> add s (normal w (corners w t))) [| 0.; 0.; 0. |] (w.around.(u) @ w.around.(v)) in
    if
      free
      && List.length shared = 2
      && (let round_v = ring w v in List.length (List.filter (fun x -> List.mem x round_v) about) = 2)
      && List.for_all (fun x -> x = v || dist w.pts.(v) w.pts.(x) <= high) about
      && (same || List.for_all (fun t -> sound w ~by (normal w (corners w t)) (moved w t u w.pts.(v))) stay)
      && may stay
    then begin
      (if same then let rank = function Face _ -> 0 | Crease _ -> 1 | Corner _ -> 2 in if rank w.kinds.(u) > rank w.kinds.(v) then w.kinds.(v) <- w.kinds.(u));
      List.iter (remove w) shared;
      List.iter
        (fun t ->
          for k = 3 * t to (3 * t) + 2 do
            if w.tri.(k) = u then w.tri.(k) <- v
          done;
          w.around.(v) <- t :: w.around.(v))
        stay;
      w.around.(u) <- [];
      List.iter
        (fun x ->
          if along_crease w u x then begin
            Hashtbl.remove w.creased (key u x);
            if x <> v then Hashtbl.replace w.creased (key v x) ()
          end)
        about;
      true
    end
    else false

let close_up w low high =
  List.iter (fun (u, v) -> if w.around.(u) <> [] && w.around.(v) <> [] && wings w u v <> [] && dist w.pts.(u) w.pts.(v) < low then ignore (into w high u v || into w high v u)) (edges w)

(* The edge u-v turned over, to join the two vertices across it instead, if neither
   new triangle is turned over: said whether it was. [wanted] is asked of the two
   vertices across and the two triangles' normals first. *)
let flip ?(wanted = fun _ _ _ _ -> true) w u v =
  match wings w u v with
  | [ s; t ] ->
      (* s is the one that goes u, v, a round *)
      let s, t = let a, b, c = corners w s in if (a = u && b = v) || (b = u && c = v) || (c = u && a = v) then (s, t) else (t, s) in
      let a = third w s u v and b = third w t u v in
      let ns = normal w (corners w s) and nt = normal w (corners w t) in
      let by = add ns nt in
      let fresh x y z = cross (sub w.pts.(y) w.pts.(x)) (sub w.pts.(z) w.pts.(x)) in
      if a <> b && (not (List.mem b (ring w a))) && wanted a b ns nt && sound w ~by by (fresh u b a) && sound w ~by by (fresh b v a) then begin
        remove w s;
        remove w t;
        add_triangle w (u, b, a);
        add_triangle w (b, v, a);
        true
      end
      else false
  | _ -> false

(* An edge not along a crease is turned over where that brings its four vertices nearer
   six edges each (a fixed vertex has what it has) and the two triangles lie nearly flat. *)
let turn_over w =
  List.iter
    (fun (u, v) ->
      if not (along_crease w u v) then
        let off x by = match hold w x with Fixed -> 0 | _ -> abs (List.length w.around.(x) + by - 6) in
        let wanted a b ns nt = off u (-1) + off v (-1) + off a 1 + off b 1 < off u 0 + off v 0 + off a 0 + off b 0 && (norm ns = 0. || norm nt = 0. || dot ns nt > 0.94 *. norm ns *. norm nt) in
        ignore (flip ~wanted w u v))
    (edges w)

(* A triangle with three sides and no height: one vertex lies on the side across from
   it, as when a vertex was moved onto a crease between two already there. That side is
   turned over, and the triangle is gone. If the side ran along a crease, the crease
   now runs through the vertex that lay on it. *)
let uncap w =
  for t = 0 to w.triangles - 1 do
    if w.tri.(3 * t) >= 0 then begin
      let a, b, c = corners w t in
      let sides = List.sort (fun (_, _, l) (_, _, m) -> compare m l) (List.map (fun (x, y) -> (x, y, dist w.pts.(x) w.pts.(y))) [ (a, b); (b, c); (c, a) ]) in
      match sides with
      | [ (u, v, side); _; (_, _, least) ] when least > 0.05 *. side && norm (normal w (a, b, c)) < 0.01 *. side *. side ->
          let on_it = third w t u v and crease = along_crease w u v in
          if flip w u v && crease then begin
            Hashtbl.remove w.creased (key u v);
            Hashtbl.replace w.creased (key u on_it) ();
            Hashtbl.replace w.creased (key on_it v) ();
            match (w.kinds.(on_it), w.kinds.(u), w.kinds.(v)) with Face _, (Crease _ as k), _ | Face _, _, (Crease _ as k) -> w.kinds.(on_it) <- k | Face _, _, _ -> w.kinds.(on_it) <- Crease (-1, -1) | _ -> ()
          end
      | _ -> ()
    end
  done

(* Each vertex to the middle of those round it: a free one within the surface's plane
   there and then onto the surface, one on a crease to the middle of its two neighbours
   along it and then onto the crease. Not if that turns a triangle over. *)
let relax w =
  for u = 0 to w.vertices - 1 do
    if w.around.(u) <> [] then begin
      let by = List.fold_left (fun s t -> add s (normal w (corners w t))) [| 0.; 0.; 0. |] w.around.(u) in
      let target =
        match hold w u with
        | Fixed -> None
        | Slides (a, b) -> onto_crease w.e ~sharp:w.sharp ~reach:w.length (mid w.pts.(a) w.pts.(b)) (sub w.pts.(b) w.pts.(a))
        | Free ->
            if norm by = 0. then None
            else
              let about = ring w u in
              let middle = scale (1. /. float_of_int (List.length about)) (List.fold_left (fun s x -> add s w.pts.(x)) [| 0.; 0.; 0. |] about) in
              let n = unit by and step = sub middle w.pts.(u) in
              to_surface ~within:w.length w.e (add w.pts.(u) (sub step (scale (dot n step) n)))
      in
      match target with
      | Some p when List.for_all (fun t -> sound w ~by (normal w (corners w t)) (moved w t u p)) w.around.(u) -> w.pts.(u) <- p
      | _ -> ()
    end
  done

let work e ~sharp ~length (points, on, triangles) creased =
  let w =
    { e; sharp; length; pts = Array.copy points; kinds = Array.copy on; vertices = Array.length points; tri = [||]; triangles = 0; around = Array.make (Array.length points) []; creased = Hashtbl.create 4096 }
  in
  Array.iter (add_triangle w) triangles;
  Array.iter (fun (u, v) -> Hashtbl.replace w.creased (key u v) ()) creased;
  w

(* What is left of it, numbered afresh. *)
let pack w =
  let number = Array.make w.vertices (-1) and kept = ref 0 in
  for v = 0 to w.vertices - 1 do
    if w.around.(v) <> [] then (number.(v) <- !kept; incr kept)
  done;
  let alive = List.filter (fun v -> number.(v) >= 0) (List.init w.vertices Fun.id) in
  let live = List.filter (fun t -> w.tri.(3 * t) >= 0) (List.init w.triangles Fun.id) in
  ( ( Array.of_list (List.map (fun v -> w.pts.(v)) alive),
      Array.of_list (List.map (fun v -> w.kinds.(v)) alive),
      Array.of_list (List.map (fun t -> let a, b, c = corners w t in (number.(a), number.(b), number.(c))) live) ),
    Array.of_list (Hashtbl.fold (fun (u, v) () l -> (number.(u), number.(v)) :: l) w.creased []) )

(* The mesh with its triangles of no height turned away. *)
let uncapped e ~length mesh =
  let w = work e ~sharp:0. ~length mesh [||] in
  uncap w;
  uncap w;
  fst (pack w)

(* The mesh made even, to edges about [length] long. The edges along creases are
   given and come back, as pairs of vertices. *)
let even_out e ~sharp ~length mesh creased =
  let w = work e ~sharp ~length mesh creased in
  for _ = 1 to 5 do
    uncap w;
    split w (4. /. 3. *. length);
    close_up w (0.8 *. length) (4. /. 3. *. length);
    turn_over w;
    relax w
  done;
  pack w

(* ---- To a tolerance ---- *)

(* The other second pass, for a mesh to a tolerance: its triangles are made few, not
   even. An edge is closed up wherever that leaves the mesh within the tolerance, so a
   flat face comes down to a few large triangles and a straight crease to one edge,
   while what curves keeps what it needs. What is attached stays attached, as in Even:
   a corner stays, and a vertex on a crease goes only along it.

   How far a vertex may go is kept as planes: those of the triangles round it at the
   start, and those of every vertex since closed up into it. A plane is ten numbers,
   which for a point give the square of how far it is off the plane, and planes add:
   the sum is no less than the furthest of them, squared. *)
let plane n d = [| n.(0) *. n.(0); n.(0) *. n.(1); n.(0) *. n.(2); n.(0) *. d; n.(1) *. n.(1); n.(1) *. n.(2); n.(1) *. d; n.(2) *. n.(2); n.(2) *. d; d *. d |]

let off q x =
  (q.(0) *. x.(0) *. x.(0)) +. (2. *. q.(1) *. x.(0) *. x.(1)) +. (2. *. q.(2) *. x.(0) *. x.(2)) +. (2. *. q.(3) *. x.(0))
  +. (q.(4) *. x.(1) *. x.(1)) +. (2. *. q.(5) *. x.(1) *. x.(2)) +. (2. *. q.(6) *. x.(1))
  +. (q.(7) *. x.(2) *. x.(2)) +. (2. *. q.(8) *. x.(2)) +. q.(9)

(* Twice a triangle's area over the square of its longest side: how thin it is, 0 for
   one with no area and 0.87 for one with equal sides. *)
let thinness pa pb pc =
  let longest = Float.max (dist pa pb) (Float.max (dist pb pc) (dist pc pa)) in
  if longest = 0. then 0. else norm (cross (sub pb pa) (sub pc pa)) /. (longest *. longest)

(* Edges are closed up, the shortest first, while one in fifty still goes. One is
   closed up if the vertex that stays is within [within] of every plane of the one
   that goes, if no edge comes out longer than [longest], and if no triangle comes out
   thinner than a hundredth unless it was thinner still. Along a crease the planes say
   too little (a round hole's rim lies in the plane of the face it is cut in), and what
   is kept is how far the crease is from each edge that now stands for it. *)
let fewer w ~within ~longest =
  (* an edge along a crease: how far the crease it stands for may be from it, at most *)
  let sag = Hashtbl.create 1024 in
  let sagging u v = Option.value ~default:0. (Hashtbl.find_opt sag (key u v)) in
  let from_side p a b = let ab = sub b a in let t = if dot ab ab = 0. then 0. else Float.max 0. (Float.min 1. (dot (sub p a) ab /. dot ab ab)) in dist p (add a (scale t ab)) in
  let planes = Array.init w.vertices (fun _ -> Array.make 10 0.) in
  for t = 0 to w.triangles - 1 do
    if w.tri.(3 * t) >= 0 then begin
      let a, b, c = corners w t in
      let n = normal w (a, b, c) in
      (* a triangle with no height says nothing of where the surface is *)
      if thinness w.pts.(a) w.pts.(b) w.pts.(c) > 1e-3 then begin
        let n = unit n in
        let q = plane n (-.dot n w.pts.(a)) in
        List.iter (fun v -> Array.iteri (fun i x -> planes.(v).(i) <- planes.(v).(i) +. x) q) [ a; b; c ]
      end
    end
  done;
  let both u v = Array.init 10 (fun i -> planes.(u).(i) +. planes.(v).(i)) in
  let goes u v =
    let q = both u v and p = w.pts.(v) in
    (* u going along a crease into v leaves one edge, from v to u's other neighbour on
       the crease, for the two there were: the crease is no nearer it than u is, on
       top of how far it was from either of the two *)
    let along = match hold w u with Slides (a, b) when a = v || b = v -> let o = if a = v then b else a in Some (o, Float.max (sagging o u) (sagging u v) +. from_side w.pts.(u) w.pts.(o) p) | _ -> None in
    off q p <= within *. within
    && (match along with Some (_, s) -> s <= within | None -> true)
    && into w longest u v ~may:(fun stay ->
           let fine =
             (* two vertices in one place (a node of the march right on the surface makes
                several) are one, whatever that does to triangles that had no shape *)
             dist w.pts.(u) p < 0.05 *. w.length
             || List.for_all
               (fun t ->
                 let a, b, c = corners w t in
                 let at x = if x = u then p else w.pts.(x) in
                 let now = thinness (at a) (at b) (at c) in
                 now >= 0.01 || now >= thinness w.pts.(a) w.pts.(b) w.pts.(c))
               stay
           in
           if fine then (planes.(v) <- q; Option.iter (fun (o, s) -> Hashtbl.replace sag (key o v) s) along);
           fine)
  in
  let rec pass rounds =
    let all = Array.of_list (List.rev_map (fun (u, v) -> (dist w.pts.(u) w.pts.(v), u, v)) (edges w)) in
    Array.sort (fun (a, _, _) (b, _, _) -> Float.compare a b) all;
    let gone = ref 0 in
    Array.iter
      (fun (_, u, v) ->
        if w.around.(u) <> [] && w.around.(v) <> [] && wings w u v <> [] then
          (* the end that has less far to go, first *)
          let u, v = if off (both u v) w.pts.(v) <= off (both u v) w.pts.(u) then (u, v) else (v, u) in
          if goes u v || goes v u then incr gone)
      all;
    uncap w;
    if rounds > 1 && 50 * !gone > Array.length all then pass (rounds - 1)
  in
  pass 12

(* How far a point is from the surface, by the field there and its slope. *)
let away e x =
  let r = probe e x in
  let g = norm r.d.g in
  if g > 0. && Float.is_finite g then Float.abs r.d.v /. g else 0.

(* How far a side of a triangle is from the surface, at most, at its middle and its
   quarters; and a triangle within itself, at its middle and halfway from that to each
   corner. The furthest place is seldom the middle: a ridge crosses where it will. *)
let side_away e pu pv = List.fold_left (fun m t -> Float.max m (away e (add pu (scale t (sub pv pu))))) 0. [ 0.25; 0.5; 0.75 ]

let inner_away e pa pb pc =
  let m = scale (1. /. 3.) (add pa (add pb pc)) in
  List.fold_left (fun w p -> Float.max w (away e p)) (away e m) [ mid m pa; mid m pb; mid m pc ]

(* How far the middle of an edge along a crease is from the crease: the middle of a
   chord across a round rim is on one of the rim's two faces, and the field there says
   nothing of how far the rim is. The crease is looked for within [reach], no further,
   and beside the middle, not towards an end: anything else is another crease (two
   pieces of surface in one plane meet in an edge that has none). *)
let off_crease ?between e ~sharp ~reach crease pu pv =
  if not crease then 0.
  else
    let m = mid pu pv and along = sub pv pu in
    match onto_crease ?between e ~sharp ~reach:(Float.min reach (dist pu pv)) m along with
    | Some x when Float.abs (dot (sub x m) along) <= 0.25 *. dot along along -> dist x m
    | _ -> 0.

(* The edges of a triangle that is too far from the surface, at the middle of a side or
   at its own middle, are split until none is: the side that is off, or the longest.
   Or turned over (below). *)
let nearer w ~tol =
  let rec round rounds =
    (* every edge once, with whether the mesh turns across it along a crease *)
    let sides = Array.of_list (List.rev_map (fun (u, v) -> (u, v, along_crease w u v && (not (apart w.kinds.(u) w.kinds.(v))) && match wings w u v with [ s; t ] -> turn (normal w (corners w s)) (normal w (corners w t)) >= 5. | _ -> false)) (edges w)) in
    let far = spread (own w.e) (fun e (u, v, turned) -> off_crease ?between:(pieces_of w.kinds.(u) w.kinds.(v)) e ~sharp:w.sharp ~reach:(8. *. tol) turned w.pts.(u) w.pts.(v) > tol || side_away e w.pts.(u) w.pts.(v) > tol) sides in
    let off = Hashtbl.create 1024 in
    Array.iteri (fun i (u, v, _) -> if far.(i) then Hashtbl.replace off (key u v) ()) sides;
    (* and the triangles with no side off, at their own middles *)
    let whole = Array.of_list (List.filter (fun t -> w.tri.(3 * t) >= 0 && (let a, b, c = corners w t in not (Hashtbl.mem off (key a b) || Hashtbl.mem off (key b c) || Hashtbl.mem off (key c a)))) (List.init w.triangles Fun.id)) in
    let inner = spread (own w.e) (fun e t -> let a, b, c = corners w t in inner_away e w.pts.(a) w.pts.(b) w.pts.(c) > tol) whole in
    Array.iteri
      (fun i t ->
        if inner.(i) then begin
          let a, b, c = corners w t in
          let _, u, v = List.fold_left (fun (l, u, v) (x, y) -> let d = dist w.pts.(x) w.pts.(y) in if d > l then (d, x, y) else (l, u, v)) (-1., a, b) [ (a, b); (b, c); (c, a) ] in
          Hashtbl.replace off (key u v) ()
        end)
      whole;
    (* A side that is off may be the wrong one of two: where three faces meet, the
       march can join two creases across the third (the foot of a block's upright edge
       cut off, floor to floor). The other diagonal of its two triangles is then on the
       surface, and the side is turned over to it; it runs along a crease if the two
       triangles it now parts are on different pieces. *)
    let over u v =
      match wings w u v with
      | [ s; t ] ->
          let a = third w s u v and b = third w t u v in
          let piece t = let i, j, k = corners w t in let m = scale (1. /. 3.) (add w.pts.(i) (add w.pts.(j) w.pts.(k))) in part w.e (Option.value ~default:m (to_surface ~within:(dist w.pts.(a) w.pts.(b)) w.e m)) in
          a <> b
          && (not (List.mem b (ring w a)))
          && side_away w.e w.pts.(a) w.pts.(b) <= tol
          && flip w u v
          && begin
               Hashtbl.remove w.creased (key u v);
               (match (w.kinds.(a), w.kinds.(b), wings w a b) with
                | (Crease _ | Corner _), (Crease _ | Corner _), [ s; t ] when piece s <> piece t -> Hashtbl.replace w.creased (key a b) ()
                | _ -> ());
               true
             end
      | _ -> false
    in
    if Hashtbl.length off > 0 && rounds > 0 then begin
      List.iter
        (fun (u, v) -> if not (over u v) then split_edge ~between:true ~to_crease:(Float.min (8. *. tol) (dist w.pts.(u) w.pts.(v))) w ~reach:(dist w.pts.(u) w.pts.(v)) u v)
        (List.sort compare (Hashtbl.fold (fun k () l -> k :: l) off []));
      round (rounds - 1)
    end
  in
  round 8

(* Vertices nearer each other than [within] are made one, where they may be; those
   in one place, to a tenth of that, whatever they are on (the creases that meet at a
   corner each have a vertex put on them there, which are one vertex). *)
let weld w ~within =
  for _ = 1 to 3 do
    List.iter
      (fun (u, v) ->
        let d = dist w.pts.(u) w.pts.(v) in
        if w.around.(u) <> [] && w.around.(v) <> [] && wings w u v <> [] && d < within then
          let same = d < 0.1 *. within in
          ignore (into ~same w infinity u v || into ~same w infinity v u))
      (edges w);
    uncap w
  done

(* What is left that has no size. Where a model has edges a thousandth of a millimetre
   apart (two faces all but lying on one another, a wall that thins to nothing, three
   faces along one curve), the mesh has vertices that near, in knots that closing up an
   edge at a time cannot undo. Here:
   - the vertices joined by edges shorter than [within] are made one, the one of them
     that is attached the most;
   - the triangles that then have two corners the same are dropped;
   - of two triangles with the same three corners, one each way round, both are dropped:
     the two sides of a flap with nothing in it;
   - a triangle with three corners in a line (as written) is taken away with the edge
     its middle corner lies on, the triangle across that edge now ending at that corner;
   - a closed piece no larger than [speck] is dropped.
   Each side of a triangle still has its partner the other way round through all of
   it, and where an edge would come to have more than one pair, the vertices there
   are left as they were. *)
let tidy ~within ~speck (points, on, triangles) creases =
  let n = Array.length points in
  let written = Array.map (Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x))) points in
  (* [kept]: vertices not to be made one with another (below) *)
  let rec attempt kept rounds =
  let up = Array.init n Fun.id in
  let rec find v = if up.(v) = v then v else (let r = find up.(v) in up.(v) <- r; r) in
  let rank v = match on.(v) with Face _ -> 0 | Crease _ -> 1 | Corner _ -> 2 in
  let join u v = let a = find u and b = find v in if a <> b then if rank a >= rank b then up.(b) <- a else up.(a) <- b in
  Array.iter (fun (a, b, c) -> List.iter (fun (u, v) -> if dist points.(u) points.(v) < within && not (kept.(u) || kept.(v)) then join u v) [ (a, b); (b, c); (c, a) ]) triangles;
  let whole = List.filter (fun (a, b, c) -> a <> b && b <> c && c <> a) in
  (* the same three corners: turned to start at the lowest, a triangle is one of two *)
  let turned (a, b, c) = if a < b && a < c then (a, b, c) else if b < c && b < a then (b, c, a) else (c, a, b) in
  let flapless triangles =
    let count = Hashtbl.create 4096 in
    List.iter (fun t -> let k = turned t in Hashtbl.replace count k (1 + Option.value ~default:0 (Hashtbl.find_opt count k))) triangles;
    (* as many of each go as there are of the fewer of the two *)
    let going = Hashtbl.create 64 in
    Hashtbl.iter (fun (a, b, c) k -> match Hashtbl.find_opt count (a, c, b) with Some o -> Hashtbl.replace going (a, b, c) (min k o) | None -> ()) count;
    List.filter (fun t -> let k = turned t in match Hashtbl.find_opt going k with Some g when g > 0 -> Hashtbl.replace going k (g - 1); false | _ -> true) triangles
  in
  let flat (a, b, c) = norm (cross (sub written.(b) written.(a)) (sub written.(c) written.(a))) = 0. in
  (* a flat triangle u v w, w on the side u v: with the triangle v u d across that side,
     it becomes v w d and w u d *)
  let rec unflatten triangles rounds =
    if rounds = 0 || not (List.exists flat triangles) then triangles
    else begin
      let across = Hashtbl.create 4096 in
      List.iter (fun ((a, b, c) as t) -> List.iter (fun side -> Hashtbl.replace across side t) [ (a, b); (b, c); (c, a) ]) triangles;
      let gone = Hashtbl.create 64 and made = ref [] in
      List.iter
        (fun ((a, b, c) as t) ->
          if flat t && not (Hashtbl.mem gone t) then begin
            let longest (u, v, w) (x, y, z) = if dist written.(x) written.(y) > dist written.(u) written.(v) then (x, y, z) else (u, v, w) in
            let u, v, w = longest (longest (a, b, c) (b, c, a)) (c, a, b) in
            match Hashtbl.find_opt across (v, u) with
            | Some ((x, y, z) as s) when not (Hashtbl.mem gone s) ->
                let d = if (x, y) = (v, u) then z else if (y, z) = (v, u) then x else y in
                (* not if the two are joined already: that edge would have four triangles *)
                if d <> w && not (Hashtbl.mem across (w, d) || Hashtbl.mem across (d, w)) then begin
                  Hashtbl.replace gone t ();
                  Hashtbl.replace gone s ();
                  made := (v, w, d) :: (w, u, d) :: !made
                end
            | _ -> ()
          end)
        triangles;
      if !made = [] then triangles else unflatten (flapless (whole (!made @ List.filter (fun t -> not (Hashtbl.mem gone t)) triangles))) (rounds - 1)
    end
  in
  let tidied = unflatten (flapless (whole (List.map (fun (a, b, c) -> (find a, find b, find c)) (Array.to_list triangles)))) 8 in
  (* Making vertices one can leave an edge with four triangles, where they stood round
     a pocket. The vertices that became the ends of such an edge are then kept apart,
     and it is done again: a few needles are left, and the mesh is as closed as it
     came. If a dozen times does not settle it, the mesh is left as it came. *)
  let ways = Hashtbl.create 4096 in
  List.iter (fun (a, b, c) -> List.iter (fun (u, v) -> Hashtbl.replace ways (u, v) (1 + Option.value ~default:0 (Hashtbl.find_opt ways (u, v)))) [ (a, b); (b, c); (c, a) ]) tidied;
  let amiss = Hashtbl.fold (fun (u, v) k l -> if k > 1 || Hashtbl.find_opt ways (v, u) <> Some k then u :: v :: l else l) ways [] in
  if amiss = [] then (find, tidied)
  else if rounds = 0 then (Fun.id, Array.to_list triangles)
  else begin
    let kept = Array.copy kept in
    let ends = Hashtbl.create 64 in
    List.iter (fun v -> Hashtbl.replace ends v ()) amiss;
    Array.iteri (fun v _ -> if Hashtbl.mem ends (find v) then kept.(v) <- true) kept;
    attempt kept (rounds - 1)
  end
  in
  let find, triangles = attempt (Array.make n false) 12 in
  (* A closed piece of the mesh no larger than [speck] any way is dropped: where a gap
     in the model narrows to less than a cell, what the march makes of it can be a
     row of bubbles, each a few triangles, which are no part of anything. *)
  let triangles =
    let part = Array.init n Fun.id in
    let rec of_ v = if part.(v) = v then v else (let r = of_ part.(v) in part.(v) <- r; r) in
    List.iter (fun (a, b, c) -> part.(of_ b) <- of_ a; part.(of_ c) <- of_ a) triangles;
    let box = Hashtbl.create 64 in
    List.iter
      (fun (a, b, c) ->
        let k = of_ a in
        let lo, hi = match Hashtbl.find_opt box k with Some b -> b | None -> let b = (Array.copy points.(a), Array.copy points.(a)) in Hashtbl.add box k b; b in
        List.iter (fun v -> Array.iteri (fun i x -> lo.(i) <- Float.min lo.(i) x; hi.(i) <- Float.max hi.(i) x) points.(v)) [ a; b; c ])
      triangles;
    let small k = let lo, hi = Hashtbl.find box k in hi.(0) -. lo.(0) <= speck && hi.(1) -. lo.(1) <= speck && hi.(2) -. lo.(2) <= speck in
    if Hashtbl.length box <= 1 then triangles else List.filter (fun (a, _, _) -> not (small (of_ a))) triangles
  in
  let number = Array.make n (-1) and kept = ref 0 in
  List.iter (fun (a, b, c) -> List.iter (fun v -> if number.(v) < 0 then (number.(v) <- !kept; incr kept)) [ a; b; c ]) triangles;
  let alive = Array.make !kept 0 in
  Array.iteri (fun v k -> if k >= 0 then alive.(k) <- v) number;
  let creases = List.sort_uniq compare (List.filter_map (fun (u, v) -> let u = number.(find u) and v = number.(find v) in if u >= 0 && v >= 0 && u <> v then Some (min u v, max u v) else None) (Array.to_list creases)) in
  ( (Array.map (fun v -> points.(v)) alive, Array.map (fun v -> on.(v)) alive, Array.of_list (List.map (fun (a, b, c) -> (number.(a), number.(b), number.(c))) triangles)),
    Array.of_list creases )

(* The mesh made few, to within [tol] of the surface: half of that is for closing up,
   and what is then found further off than four fifths of it is split. *)
let to_tolerance ?(lap = ignore) e ~sharp ~least ~longest ~tol mesh creased =
  let w = work e ~sharp ~length:least mesh creased in
  weld w ~within:(0.01 *. least);
  fewer w ~within:(0.5 *. tol) ~longest;
  lap "made few";
  nearer w ~tol:(0.8 *. tol);
  lap "held to the tolerance";
  weld w ~within:(0.01 *. least);
  let mesh, creases = pack w in
  tidy ~within:(0.02 *. tol) ~speck:(8. *. tol) mesh creases

(* ---- The mesh ---- *)

(* A triangle's middle, and where that is on the surface. *)
let middles e ~size points triangles =
  Array.map (fun (a, b, c) -> let m = scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))) in (m, Option.value ~default:m (to_surface ~within:size e m))) triangles

(* The piece of surface a triangle's middle is on. *)
let part_of e ~size points triangles t =
  let a, b, c = triangles.(t) in
  let m = scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))) in
  part e (Option.value ~default:m (to_surface ~within:size e m))

(* The edges that run along creases: between two vertices on creases or corners, with a
   different piece of surface either side. *)
let creased on triangles part_of =
  let sides = Hashtbl.create 4096 in
  let attached v = match on.(v) with Face _ -> false | _ -> true in
  Array.iteri
    (fun t (a, b, c) ->
      (* a side can run along a crease only between two vertices that are on one *)
      if (attached a && attached b) || (attached b && attached c) || (attached c && attached a) then
        let f = part_of t in
        List.iter (fun (u, v) -> if attached u && attached v then Hashtbl.add sides (min u v, max u v) f) [ (a, b); (b, c); (c, a) ])
    triangles;
  let found = Hashtbl.create 4096 in
  Hashtbl.iter (fun ((u, v) as key) _ -> match (on.(u), on.(v), Hashtbl.find_all sides key) with (Crease _ | Corner _), (Crease _ | Corner _), [ f; g ] when f <> g -> Hashtbl.replace found key () | _ -> ()) sides;
  Array.of_list (Hashtbl.fold (fun key () l -> key :: l) found [])

(* [depth]: the cells are 1 / 2^depth of the box's longest side, the box drawn in onto
   the model first if [fit]. [sharp]: the turn, in degrees, from which a boundary is a
   crease. [x] is the model's extraction, for its corners. [even]: the second pass, to
   edges a cell long; without it the mesh is as marched, closed and attached but full
   of slivers.

   With [tol] the mesh is to a tolerance instead, a length: no triangle further than
   that from the surface, and no more of them than that takes. The cells are sized to
   the surface (above), the smallest as wide as the tolerance, so a gap or a wall
   thinner than that may be lost; the largest 64 times that or an eighth of the box. [x] then
   says where the edges are, too. The second pass makes the triangles few. *)
let mesh ?(fit = false) ?(depth = 6) ?tol ?(sharp = 1.) ?(even = true) ~lo ~hi g (x : X.t) =
  let p = P.compile g in
  let c = P.ctx p in
  let lo, hi = if fit then X.fitted p c ~lo ~hi else (lo, hi) in
  (* what has been done so far, latest first, with the seconds it took *)
  let took = ref [] and since = ref (Unix.gettimeofday ()) in
  let lap what = let now = Unix.gettimeofday () in took := (what, now -. !since) :: !took; since := now in
  let finish e ~size ~largest (points, on, triangles) corners creases =
    let middles = middles e ~size:largest points triangles in
    (* the furthest so far, and where *)
    let further (d, x) (d', x') = if d' > d then (d', x') else (d, x) in
    let worst = Array.fold_left (fun w (m, s) -> further w (dist m s, m)) (0., [| 0.; 0.; 0. |]) middles in
    let worst =
      match tol with
      | None -> worst
      | Some tol ->
          (* to a tolerance, more places count: along the sides, within the triangle *)
          let at x = (away e x, x) in
          let worst =
            Array.fold_left
              (fun w (a, b, c) ->
                let m = scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))) in
                let w = List.fold_left (fun w x -> further w (at x)) w [ m; mid m points.(a); mid m points.(b); mid m points.(c) ] in
                List.fold_left (fun w (u, v) -> if u < v then List.fold_left (fun w t -> further w (at (add points.(u) (scale t (sub points.(v) points.(u)))))) w [ 0.25; 0.5; 0.75 ] else w) w [ (a, b); (b, c); (c, a) ])
              worst triangles
          in
          (* and the creases, where the mesh turns across them *)
          let along = Hashtbl.create 1024 in
          Array.iter (fun (u, v) -> Hashtbl.replace along (min u v, max u v) []) creases;
          Array.iter (fun (a, b, c) -> List.iter (fun (u, v) -> let k = (min u v, max u v) in match Hashtbl.find_opt along k with Some l -> Hashtbl.replace along k (cross (sub points.(b) points.(a)) (sub points.(c) points.(a)) :: l) | None -> ()) [ (a, b); (b, c); (c, a) ]) triangles;
          Hashtbl.fold (fun (u, v) normals w -> match normals with [ m; n ] when turn m n >= 5. && not (apart on.(u) on.(v)) -> further w (off_crease ?between:(pieces_of on.(u) on.(v)) e ~sharp ~reach:(8. *. tol) true points.(u) points.(v), mid points.(u) points.(v)) | _ -> w) along worst
    in
    lap "measured";
    { points; on; triangles; face = Array.map (fun (_, s) -> face e s) middles; names = p.names; size; largest; worst = fst worst; worst_at = snd worst; corners; creases; took = List.rev !took }
  in
  match tol with
  | None ->
      let e = { p; c; tree = Octree.build p c ~lo ~hi ~depth; diag = dist lo hi } in
      let size, points, triangles = march e ~lo ~hi ~depth in
      lap (Printf.sprintf "marched (%d triangles)" (List.length triangles));
      let conformed = conform e ~sharp ~size points triangles in
      lap "attached to creases";
      let points, on, triangles, corners = place e ~sharp ~size x conformed in
      let ((points, on, triangles) as raw) = uncapped e ~length:size (points, on, triangles) in
      let creases = creased on triangles (part_of e ~size points triangles) in
      lap "corners put in";
      let mesh, creases = if even then even_out e ~sharp ~length:size raw creases else (raw, creases) in
      if even then lap "made even";
      finish e ~size ~largest:size mesh corners creases
  | Some tol ->
      let box = Array.fold_left Float.max 0. (sub hi lo) in
      let least = tol in
      let levels = max 3 (int_of_float (Float.ceil (Float.log2 (box /. least)))) in
      if levels > 18 then failwith "the tolerance is too fine for a model this size: less than a hundred-thousandth of it";
      let l = { from = lo; step = 0.5 *. least; levels } in
      let largest = Float.min (64. *. least) (box /. 8.) in
      (* the level whose cells are the largest allowed *)
      let first = levels - int_of_float (Float.floor (Float.log2 (largest /. least))) in
      let k = known l ~first ~within:(1e-5 *. dist lo hi) x in
      let corners = Array.of_list (List.map (fun (v : X.vertex) -> List.map (P.face_id p) v.faces) x.vertices) in
      let edges = Array.of_list (List.map (fun (e : X.edge) -> (P.face_id p (fst e.between), P.face_id p (snd e.between), e.ends)) x.edges) in
      let nodes = Hashtbl.create (1 lsl 16) in
      let wanted (cell : Octree.cell) i j kk s d =
        let straight () = straight (fun q -> let _, v, _ = node p c l nodes cell.state q in v) i j kk s ~within:(0.25 *. tol) in
        d < first || not (if cell.live = 0 then straight () else plain p c l k edges corners ~straight ~within:(0.25 *. tol) cell i j kk s d)
      in
      lap "edges laid in cells";
      let tree, leaves = sized p c l ~lo ~wanted in
      lap (Printf.sprintf "cells sized (%d)" (List.length leaves));
      let e = { p; c; tree; diag = dist lo hi } in
      let largest = List.fold_left (fun m ((cell : Octree.cell), _, _, _, _) -> if cell.surface then Float.max m cell.size else m) least leaves in
      let points, triangles = march_sized e l nodes leaves in
      lap (Printf.sprintf "marched (%d triangles)" (List.length triangles));
      let size_at x = match Octree.find tree x with Some cell -> cell.size | None -> least in
      let conformed = conform e ~sharp ~size:least points triangles in
      lap "attached to creases";
      let points, on, triangles, corners = place ~size_at ~close:(2. *. tol) e ~sharp ~size:least x conformed in
      let ((points, on, triangles) as raw) = uncapped e ~length:least (points, on, triangles) in
      let creases = creased on triangles (part_of e ~size:largest points triangles) in
      lap "corners put in";
      let mesh, creases = if even then to_tolerance ~lap e ~sharp ~least ~longest:(0.25 *. box) ~tol raw creases else (raw, creases) in
      finish e ~size:least ~largest mesh corners creases

(* ---- What it is like ---- *)

(* The edges along creases, as pairs of points. *)
let creases (m : t) = List.map (fun (u, v) -> (m.points.(u), m.points.(v))) (Array.to_list m.creases)

(* The points as they are written, in 32-bit numbers. Two vertices are never written as
   the same point: where they would be (they are that near, and no edge joins them, or
   Tidy would have made them one), the later is written the least such a number can
   differ further along z. A file's edges are matched by its points, and two vertices
   in one place would make two sheets that touch there into edges with four triangles. *)
let written (m : t) =
  let taken = Hashtbl.create (Array.length m.points) in
  Array.map
    (fun p ->
      let bits = Array.map Int32.bits_of_float p in
      let rec free () = if Hashtbl.mem taken (bits.(0), bits.(1), bits.(2)) then (bits.(2) <- Int32.succ bits.(2); free ()) in
      free ();
      Hashtbl.add taken (bits.(0), bits.(1), bits.(2)) ();
      Array.map Int32.float_of_bits bits)
    m.points

type quality = {
  open_edges : int;  (** edges with one triangle: 0 for a closed mesh *)
  crowded : int;  (** edges with more than two, or two the same way round *)
  angles : float array;  (** each triangle's smallest angle in degrees, sorted *)
  on_creases : int;  (** vertices on creases and corners *)
  empty : int;  (** triangles with no area once their points are written as 32-bit numbers *)
}

let quality (m : t) =
  let ways = Hashtbl.create 4096 in
  Array.iter (fun (a, b, c) -> List.iter (fun (u, v) -> Hashtbl.add ways (min u v, max u v) (u < v)) [ (a, b); (b, c); (c, a) ]) m.triangles;
  let open_edges = ref 0 and crowded = ref 0 in
  List.iter (fun key -> match Hashtbl.find_all ways key with [ _ ] -> incr open_edges | [ s; t ] when s <> t -> () | _ -> incr crowded) (List.sort_uniq compare (Hashtbl.fold (fun key _ l -> key :: l) ways []));
  let smallest (a, b, c) =
    let angle p q r = let u = sub q p and v = sub r p in if norm u = 0. || norm v = 0. then 0. else turn u v in
    let pa = m.points.(a) and pb = m.points.(b) and pc = m.points.(c) in
    Float.min (angle pa pb pc) (Float.min (angle pb pc pa) (angle pc pa pb))
  in
  let angles = Array.map smallest m.triangles in
  Array.sort compare angles;
  let points = written m in
  let empty = Array.fold_left (fun n (a, b, c) -> if norm (cross (sub points.(b) points.(a)) (sub points.(c) points.(a))) = 0. then n + 1 else n) 0 m.triangles in
  { open_edges = !open_edges; crowded = !crowded; angles; empty; on_creases = List.length (List.sort_uniq compare (List.concat_map (fun (u, v) -> [ u; v ]) (Array.to_list m.creases))) }

(* An STL file, binary: one solid, each triangle its normal and its three points as
   32-bit numbers. A vertex is written the same in every triangle it is in and no two
   are written the same ([written]), so the file is closed if the mesh is. *)
let stl (m : t) =
  let b = Buffer.create (84 + (50 * Array.length m.triangles)) in
  let head = "mk52 mesh" in
  Buffer.add_string b head;
  Buffer.add_string b (String.make (80 - String.length head) ' ');
  Buffer.add_int32_le b (Int32.of_int (Array.length m.triangles));
  let number x = Buffer.add_int32_le b (Int32.bits_of_float x) in
  let points = written m in
  Array.iter
    (fun (u, v, w) ->
      let pu = points.(u) and pv = points.(v) and pw = points.(w) in
      let n = cross (sub pv pu) (sub pw pu) in
      let n = if norm n > 0. then unit n else n in
      List.iter (Array.iter number) [ n; pu; pv; pw ];
      Buffer.add_uint16_le b 0)
    m.triangles;
  Buffer.contents b
