(* Whether solids run into each other, and how near they come.

   Two solids are held against each other by the larger of their two fields, h. Where h
   is below zero a point is inside both: they overlap. Where they do not, the least h is
   half way across the narrowest gap between them, and half that gap. So twice the least
   h is their clearance: how far apart they are at their nearest, or, below zero, how
   far into each other. It is as true as the fields are distances: a field that is less
   than the distance, as a cut solid's is away from its faces, gives a clearance that is
   less than the true one, never more.

   The least h is searched for in the box both solids are near, cell by cell, each cut
   in eight: a cell is left as soon as the fields' slope says nothing in it can be
   within the limit asked about, or better by [fine] than the best found. Then the place
   found is searched again, finely, for the number.

   A solid that is only shown, a bought part's triangles (Asset), has no field. It is
   held against a solid by the solid's field at the triangles' corners and middles: how
   deep the deepest of them is inside. That is one way only: it does not see a thin
   thing that passes between those points. *)

(* What a design says of two of its parts: that they are meant to touch, or to be in
   each other, as a screw's thread is in its hole. By the parts' names. *)
let meant : (string * string) list ref = ref []
let allow a b = meant := (a, b) :: !meant

type solid = { p : Prog.t; c : Prog.ctx; lo : float array; hi : float array; tree : Octree.t Lazy.t  (** its cells' decisions, for many points *) }

(* A solid from its graph, with the box of its surface within lo..hi. None if it has no
   surface there. *)
let solid ~lo ~hi (g : Graph.t) =
  let p = Prog.compile g in
  let c = Prog.ctx p in
  Option.map (fun (lo, hi) -> { p; c; lo; hi; tree = lazy (Octree.build p c ~lo ~hi ~depth:5) }) (Octree.bounds p c ~lo ~hi ~depth:6)

let root_value (s : solid) = s.c.v.(s.p.last.(s.p.root))

(* The least of the larger field within lo..hi, and where: no cell searched further
   than can hold a point under [under], or one better than the best by [fine]. *)
let least ~under ~fine (a : solid) (b : solid) ~lo ~hi =
  let best = ref infinity and at = ref [||] in
  let lip = Float.max a.p.lip.(a.p.root) b.p.lip.(b.p.root) in
  let x = Array.make 3 0. in
  let rec visit l size sa sb =
    for i = 0 to 2 do x.(i) <- l.(i) +. (0.5 *. size) done;
    let r = 0.5 *. sqrt 3. *. size in
    let sa, _, _ = Octree.prune a.p a.c sa x r in
    let va = root_value a in
    let sb, _, _ = Octree.prune b.p b.c sb x r in
    let h = Float.max va (root_value b) in
    if h < !best then (best := h; at := Array.copy x);
    let bound = h -. (lip *. r) in
    if bound < under && bound < !best -. fine && size > 0.5 *. fine then
      List.iter (fun o -> Option.iter (fun k -> visit k (0.5 *. size) sa sb) (Octree.octant l (0.5 *. size) hi o)) [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  in
  visit lo (Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo)) (Prog.whole a.p) (Prog.whole b.p);
  (!best, !at)

(* Two solids' clearance, and where it is: None if they are further apart than [limit]
   everywhere. *)
let clearance ?(limit = 0.3) (a : solid) (b : solid) =
  let lo = Array.init 3 (fun i -> Float.max a.lo.(i) b.lo.(i) -. limit) and hi = Array.init 3 (fun i -> Float.min a.hi.(i) b.hi.(i) +. limit) in
  if lo.(0) >= hi.(0) || lo.(1) >= hi.(1) || lo.(2) >= hi.(2) then None
  else
    let under = (0.5 *. limit) +. 1e-9 in
    let coarse, at = least ~under ~fine:0.04 a b ~lo ~hi in
    if coarse -. 0.04 > under then None
    else
      (* again about the place found, for the number *)
      let near = 0.25 in
      let h, at = least ~under:infinity ~fine:0.002 a b ~lo:(Array.map (fun v -> v -. near) at) ~hi:(Array.map (fun v -> v +. near) at) in
      let h, at = if h < coarse then (h, at) else (coarse, at) in
      if 2. *. h > limit then None else Some (2. *. h, at)

(* A shown part's points held against a solid: how far the deepest of them is inside
   (below zero), or how near the nearest is, and which point. None if all are further
   than [limit] away. *)
let against ?(limit = 0.3) (s : solid) (points : float array list) =
  let inside x = x.(0) >= s.lo.(0) -. limit && x.(0) <= s.hi.(0) +. limit && x.(1) >= s.lo.(1) -. limit && x.(1) <= s.hi.(1) +. limit && x.(2) >= s.lo.(2) -. limit && x.(2) <= s.hi.(2) +. limit in
  let best = ref infinity and at = ref [||] in
  List.iter
    (fun x ->
      if inside x then begin
        let v = Prog.value s.p s.c (Octree.state (Lazy.force s.tree) x) x in
        if v < !best then (best := v; at := x)
      end)
    points;
  if !best > limit then None else Some (!best, !at)

(* The points a mesh is held by: its triangles' corners and middles, put in [frame]. *)
let points frame (m : Asset.mesh) =
  let corner i = Frame.apply frame [| m.points.(3 * i); m.points.((3 * i) + 1); m.points.((3 * i) + 2) |] in
  let corners = List.init (Array.length m.points / 3) corner in
  let middles =
    List.init (Array.length m.triangles / 3) (fun t ->
      let a = corner m.triangles.(3 * t) and b = corner m.triangles.((3 * t) + 1) and c = corner m.triangles.((3 * t) + 2) in
      Array.init 3 (fun i -> (a.(i) +. b.(i) +. c.(i)) /. 3.))
  in
  corners @ middles
