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
  size : float;  (** the side of a cell *)
  worst : float;  (** how far the middle of a triangle is from the surface, at most *)
  corners : int * int;  (** corners put in, of those of the extraction where the surface turns *)
  creases : (int * int) array;  (** the edges that run along creases, by their two vertices *)
}

type env = { p : P.t; c : P.ctx; tree : Octree.t; diag : float }

let probe e x = P.eval e.p e.c (Octree.state e.tree x) x
let face e x = (probe e x).face

(* Newton onto the surface, no further than [within] from the start. Beside a crease
   that is not square the steps go from one face to the other and back, closing in on
   the crease itself only by a share each time: what they have closed in to after sixty
   is taken if it is within a hundred-millionth of the box. *)
let to_surface ?(within = infinity) e x0 =
  let rec go x n =
    let r = probe e x in
    let g2 = dot r.d.g r.d.g in
    if g2 = 0. || not (Float.is_finite g2) then None
    else
      let x' = sub x (scale (r.d.v /. g2) r.d.g) in
      let step = dist x' x in
      if dist x' x0 > within then None else if step < 1e-12 *. e.diag then Some x' else if n > 0 then go x' (n - 1) else if step < 1e-8 *. e.diag then Some x' else None
  in
  go x0 60

(* The angle between two gradients, in degrees. *)
let turn a b = Float.acos (Float.max (-1.) (Float.min 1. (dot a b /. (norm a *. norm b)))) *. 180. /. Float.pi

(* ---- March ---- *)

(* A cube's corners are numbered by bits (1 x, 2 y, 4 z): six tetrahedra about 0-7. *)
let tetrahedra = [ [ 0; 1; 3; 7 ]; [ 0; 3; 2; 7 ]; [ 0; 2; 6; 7 ]; [ 0; 6; 4; 7 ]; [ 0; 4; 5; 7 ]; [ 0; 5; 1; 7 ] ]

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
  (* the vertex on the edge between two nodes, one inside and one outside: its number,
     and the middle of the edge. A triangle is turned to face outward by the middles of
     its three edges, which are never in line, where the vertices themselves can be. *)
  let cuts = Hashtbl.create 4096 and points = ref [] and triangles = ref [] in
  let vertex (ia, va, xa) (ib, vb, xb) =
    let key = (min ia ib, max ia ib) in
    let v =
      match Hashtbl.find_opt cuts key with
      | Some v -> v
      | None ->
          let x = add xa (scale (va /. (va -. vb)) (sub xb xa)) in
          let v = Hashtbl.length cuts in
          Hashtbl.add cuts key v;
          points := Option.value ~default:x (to_surface ~within:size e x) :: !points;
          v
    in
    (v, mid xa xb)
  in
  let triangle outward (a, pa) (b, pb) (c, pc) = triangles := (if dot (cross (sub pb pa) (sub pc pa)) outward >= 0. then (a, b, c) else (a, c, b)) :: !triangles in
  Octree.shell e.p e.c ~lo ~hi ~depth (fun l _ state ->
      let at a = int_of_float (Float.round ((l.(a) -. lo.(a)) /. size)) in
      let corner = Array.init 8 (fun o -> node state (at 0 + (o land 1), at 1 + ((o lsr 1) land 1), at 2 + ((o lsr 2) land 1))) in
      List.iter
        (fun tet ->
          let inside, outside = List.partition (fun (_, v, _) -> v <= 0.) (List.map (fun o -> corner.(o)) tet) in
          let middle l = scale (1. /. float_of_int (List.length l)) (List.fold_left (fun s (_, _, x) -> add s x) [| 0.; 0.; 0. |] l) in
          let tri = triangle (sub (middle outside) (middle inside)) in
          match (inside, outside) with
          | [ i ], [ a; b; c ] | [ a; b; c ], [ i ] -> tri (vertex i a) (vertex i b) (vertex i c)
          | [ i; j ], [ k; l ] ->
              let q0 = vertex i k and q1 = vertex i l and q2 = vertex j l and q3 = vertex j k in
              tri q0 q1 q2;
              tri q0 q2 q3
          | _ -> ())
        tetrahedra);
  (size, Array.of_list (List.rev !points), !triangles)

(* ---- Conform ---- *)

(* An end of an edge is next to a crossing within this share of the edge. *)
let next_to = 0.15

let faces_on = function Face f -> [ f ] | Crease (a, b) -> [ a; b ] | Corner l -> l
let share a b = List.exists (fun f -> List.mem f (faces_on b)) (faces_on a)

(* Two points of the surface in different faces, brought together by halving: the
   middle of the two lands on one face, nearer one of them than the other, so each step
   takes a share off, not a half. *)
let pinch e a b =
  let fa = face e a in
  let rec halve a b k =
    if k = 0 || dist a b < 1e-10 *. e.diag then (a, b) else match to_surface e (mid a b) with Some m -> if face e m = fa then halve m b (k - 1) else halve a m (k - 1) | None -> (a, b)
  in
  halve a b 300

(* Where a mesh edge first crosses a crease, if it does: the place, what it is on, and
   how far along the edge (0 to 1). Its two ends being in one face does not say it
   crosses none (the corner of another face can reach across it), so the faces are read
   at five places along it, each put on the surface, and a change between two of them
   is run down by halving. A change with no turn in the surface is a tangent seam, and
   is passed over. So is one right next to an end that is itself on a crease: it is that
   crease again, the edge running off along it. *)
let crossing e ~sharp (pu, ou) (pv, ov) =
  let at t = to_surface e (add pu (scale t (sub pv pu))) in
  let end_ p o t = match o with Face _ -> Some p | _ -> at t in
  let length = dist pu pv in
  let own o t = match o with Face _ -> false | _ -> t < next_to in
  let rec first = function
    | a :: (b :: _ as rest) ->
        if face e a = face e b then first rest
        else
          let a', b' = pinch e a b in
          let ra = probe e a' and rb = probe e b' in
          let x = mid a' b' in
          let t = dist pu x /. length in
          if dist a' b' <= 1e-7 *. e.diag && turn ra.d.g rb.d.g >= sharp && not (own ou t || own ov (1. -. t)) then Some (x, Crease (min ra.face rb.face, max ra.face rb.face), t) else first rest
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
   cross none is not asked again unless an end of it has moved. A piece of an edge
   already split is not split again once it is shorter than a quarter of a cell: what
   crosses it then is finer than the cells (a hole a cell wide, two faces lying on one
   another), and splitting would chase it without end. *)
let conform e ~sharp ~size points triangles =
  let settled = Hashtbl.create 4096 and marched = Array.length points in
  let rec pass points on near moved triangles rounds =
    let splits = Hashtbl.create 1024 and added = ref [] and snaps = Hashtbl.create 1024 in
    let ask u v =
      let key = (min u v, max u v) in
      let fresh = Hashtbl.find_opt settled key <> Some (moved.(u) + moved.(v)) in
      if fresh && (near.(u) || near.(v) || not (share on.(u) on.(v))) && not (Hashtbl.mem splits key || Hashtbl.mem snaps u || Hashtbl.mem snaps v) then
        let piece = max u v >= marched && dist points.(u) points.(v) < 0.25 *. size in
        match if piece then None else crossing e ~sharp (points.(u), on.(u)) (points.(v), on.(v)) with
        | Some (x, what, t) when t < next_to -> Hashtbl.replace snaps u (x, what)
        | Some (x, what, t) when t > 1. -. next_to -> Hashtbl.replace snaps v (x, what)
        | Some (x, what, _) ->
            Hashtbl.add splits key (Array.length points + Hashtbl.length splits);
            added := (x, what) :: !added
        | None -> Hashtbl.replace settled key (moved.(u) + moved.(v))
    in
    List.iter (fun (a, b, c) -> ask a b; ask b c; ask c a) triangles;
    if (Hashtbl.length splits = 0 && Hashtbl.length snaps = 0) || rounds = 0 then (points, on, triangles)
    else begin
      let added = Array.of_list (List.rev !added) in
      let grown a more = Array.append a more in
      let points = grown points (Array.map fst added) and on = grown on (Array.map snd added) and near = grown near (Array.map (fun _ -> true) added) and moved = grown moved (Array.map (fun _ -> 0) added) in
      Hashtbl.iter (fun u (x, what) -> points.(u) <- x; on.(u) <- what; near.(u) <- true; moved.(u) <- moved.(u) + 1) snaps;
      pass points on near moved (subdivide (fun a b -> Hashtbl.find_opt splits (min a b, max a b)) triangles) (rounds - 1)
    end
  in
  let read = Array.map (probe e) points in
  pass (Array.copy points) (Array.map (fun (r : P.sample) -> Face r.face) read) (Array.map (fun (r : P.sample) -> r.reached <> []) read) (Array.make (Array.length points) 0) triangles 8

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
let place e ~sharp ~size (x : X.t) (points, on, triangles) =
  let points = Array.copy points and on = Array.copy on and triangles = Array.of_list triangles in
  let cell p = (int_of_float (Float.floor (p.(0) /. size)), int_of_float (Float.floor (p.(1) /. size)), int_of_float (Float.floor (p.(2) /. size))) in
  let near table p = let i, j, k = cell p in List.concat_map (fun (a, b, c) -> Hashtbl.find_all table (i + a, j + b, k + c)) (List.concat_map (fun a -> List.concat_map (fun b -> List.map (fun c -> (a, b, c)) [ -1; 0; 1 ]) [ -1; 0; 1 ]) [ -1; 0; 1 ]) in
  let vertices = Hashtbl.create 4096 and holding = Hashtbl.create 4096 in
  Array.iteri (fun v p -> Hashtbl.add vertices (cell p) v) points;
  Array.iteri (fun t (a, b, c) -> Hashtbl.add holding (cell (scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))))) t) triangles;
  (* vertices added, newest first: its number, where it is, what it is on *)
  let more = ref [] and more_triangles = ref [] in
  let at v = if v < Array.length points then points.(v) else (fun (_, p, _) -> p) (List.find (fun (u, _, _) -> u = v) !more) in
  let wanted = List.filter (fun (v : X.vertex) -> turns e ~sharp ~size v.at) x.vertices in
  let placed =
    List.filter
      (fun (v : X.vertex) ->
        let corner = Corner (List.sort_uniq compare (List.map (P.face_id e.p) v.faces)) in
        let nearest = List.sort compare (List.filter_map (fun u -> match on.(u) with Corner _ -> None | _ -> Some (dist points.(u) v.at, u)) (near vertices v.at)) in
        let move u = points.(u) <- v.at; on.(u) <- corner; true in
        match nearest with
        | (d, u) :: _ when d <= size /. 3. -> move u
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
            match List.sort compare (List.filter_map within (near holding v.at)) with
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
   in different faces. Their tangent planes meet in a line, and the point of that line
   nearest p is taken; two points a hair either side of it are put on the surface, and
   the same again. Where the faces are flat that is exact at once, and where they curve
   it is in two or three rounds. Failing that, the two points are halved together. *)
let onto_crease e ~sharp ~reach p along =
  let sides s =
    let across = cross (probe e s).d.g along in
    if norm across = 0. then None
    else
      List.find_map
        (fun d ->
          let side k = to_surface e (add s (scale (k *. d *. reach /. norm across) across)) in
          match (side (-1.), side 1.) with Some a, Some b when face e a <> face e b -> Some (a, b) | _ -> None)
        [ 0.05; 0.2; 0.5 ]
  in
  let rec close a b last rounds =
    let ga = (probe e a).d.g and gb = (probe e b).d.g in
    let line = cross ga gb in
    if rounds = 0 || turn ga gb < sharp then None
    else
      match X.solve3 [| ga; gb; line |] [| dot ga a; dot gb b; dot line p |] with
      | Some x when dist x p <= reach -> (
          if (match last with Some y -> dist x y < 1e-10 *. e.diag | None -> false) then Some x
          else
            (* a hair into each face from the line, on the side its point was *)
            let into g q = let d = cross line g in to_surface e (add x (scale ((if dot d (sub q x) >= 0. then 1e-6 else -1e-6) *. reach /. norm d) d)) in
            match (into ga a, into gb b) with Some a', Some b' when face e a' <> face e b' -> close a' b' (Some x) (rounds - 1) | _ -> None)
      | _ -> None
  in
  match Option.bind (to_surface ~within:reach e p) sides with
  | None -> None
  | Some (a, b) -> (
      match close a b None 6 with
      | Some x -> Some x
      | None ->
          let a, b = pinch e a b in
          if dist a b <= 1e-7 *. e.diag && turn (probe e a).d.g (probe e b).d.g >= sharp && dist (mid a b) p <= reach then Some (mid a b) else None)

(* A triangle's new normal is sound if it has some area and faces the way the old one
   did. Where the old one had no area, the new one may have none either (a knot of
   vertices in one place is undone a vertex at a time), or must face the way [by] does. *)
let sound w ~by old fresh =
  let least = 1e-8 *. w.length *. w.length in
  if norm old > least then norm fresh > least && dot fresh old > 0. else norm fresh <= least || dot fresh by > 0.

(* An edge longer than [high] is split at its middle: on the crease, if it runs along
   one, and on the surface. *)
let split w high =
  List.iter
    (fun (u, v) ->
      let on_edge = wings w u v in
      if on_edge <> [] && dist w.pts.(u) w.pts.(v) > high then begin
        let middle = mid w.pts.(u) w.pts.(v) in
        let crease = along_crease w u v in
        let m =
          if crease then
            add_vertex w
              (Option.value ~default:middle (onto_crease w.e ~sharp:w.sharp ~reach:w.length middle (sub w.pts.(v) w.pts.(u))))
              (match (w.kinds.(u), w.kinds.(v)) with (Crease _ as k), _ | _, (Crease _ as k) -> k | _ -> Crease (-1, -1))
          else
            let x = Option.value ~default:middle (to_surface ~within:w.length w.e middle) in
            add_vertex w x (Face (face w.e x))
        in
        List.iter
          (fun t ->
            let a, b, c = corners w t in
            let swap x y (a, b, c) = ((if a = x then y else a), (if b = x then y else b), if c = x then y else c) in
            remove w t;
            add_triangle w (swap v m (a, b, c));
            add_triangle w (swap u m (a, b, c)))
          on_edge;
        if crease then begin
          Hashtbl.remove w.creased (key u v);
          Hashtbl.replace w.creased (key u m) ();
          Hashtbl.replace w.creased (key m v) ()
        end
      end)
    (edges w)

(* An edge shorter than [low] is closed up: one end goes into the other, if it may. u
   may go into v if it is free, or slides along a crease to v, or is a knot in a crease
   (not a corner, but with other than two crease edges) a tenth of the length from v
   along it; if the two share only the two vertices across the edge (or the mesh would
   fold onto itself); if no edge comes out longer than [high]; and if no triangle is
   turned over. The crease edges of u become v's. *)
let close_up w low high =
  let into u v =
    let free =
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
      && List.length (List.filter (fun x -> List.mem x (ring w v)) about) = 2
      && List.for_all (fun x -> x = v || dist w.pts.(v) w.pts.(x) <= high) about
      && List.for_all (fun t -> sound w ~by (normal w (corners w t)) (moved w t u w.pts.(v))) stay
    then begin
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
  in
  List.iter (fun (u, v) -> if w.around.(u) <> [] && w.around.(v) <> [] && wings w u v <> [] && dist w.pts.(u) w.pts.(v) < low then ignore (into u v || into v u)) (edges w)

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

(* ---- The mesh ---- *)

(* A triangle's middle, and where that is on the surface. *)
let middles e ~size points triangles =
  Array.map (fun (a, b, c) -> let m = scale (1. /. 3.) (add points.(a) (add points.(b) points.(c))) in (m, Option.value ~default:m (to_surface ~within:size e m))) triangles

(* The edges that run along creases: between two vertices on creases or corners, with a
   different face either side. *)
let creased on triangles faces =
  let sides = Hashtbl.create 4096 in
  Array.iteri (fun t (a, b, c) -> List.iter (fun (u, v) -> Hashtbl.add sides (min u v, max u v) faces.(t)) [ (a, b); (b, c); (c, a) ]) triangles;
  let found = Hashtbl.create 4096 in
  Hashtbl.iter (fun ((u, v) as key) _ -> match (on.(u), on.(v), Hashtbl.find_all sides key) with (Crease _ | Corner _), (Crease _ | Corner _), [ f; g ] when f <> g -> Hashtbl.replace found key () | _ -> ()) sides;
  Array.of_list (Hashtbl.fold (fun key () l -> key :: l) found [])

(* [depth]: the cells are 1 / 2^depth of the box's longest side, the box drawn in onto
   the model first if [fit]. [sharp]: the turn, in degrees, from which a boundary is a
   crease. [x] is the model's extraction, for its corners. [even]: the second pass, to
   edges a cell long; without it the mesh is as marched, closed and attached but full
   of slivers. *)
let mesh ?(fit = false) ?(depth = 6) ?(sharp = 1.) ?(even = true) ~lo ~hi g (x : X.t) =
  let p = P.compile g in
  let c = P.ctx p in
  let lo, hi = if fit then X.fitted p c ~lo ~hi else (lo, hi) in
  let e = { p; c; tree = Octree.build p c ~lo ~hi ~depth; diag = dist lo hi } in
  let size, points, triangles = march e ~lo ~hi ~depth in
  let points, on, triangles, corners = place e ~sharp ~size x (conform e ~sharp ~size points triangles) in
  let ((points, on, triangles) as raw) = uncapped e ~length:size (points, on, triangles) in
  let creases = creased on triangles (Array.map (fun (_, s) -> face e s) (middles e ~size points triangles)) in
  let (points, on, triangles), creases = if even then even_out e ~sharp ~length:size raw creases else (raw, creases) in
  let middles = middles e ~size points triangles in
  { points; on; triangles; face = Array.map (fun (_, s) -> face e s) middles; names = p.names; size; worst = Array.fold_left (fun w (m, s) -> Float.max w (dist m s)) 0. middles; corners; creases }

(* ---- What it is like ---- *)

(* The edges along creases, as pairs of points. *)
let creases (m : t) = List.map (fun (u, v) -> (m.points.(u), m.points.(v))) (Array.to_list m.creases)

type quality = {
  open_edges : int;  (** edges with one triangle: 0 for a closed mesh *)
  crowded : int;  (** edges with more than two, or two the same way round *)
  angles : float array;  (** each triangle's smallest angle in degrees, sorted *)
  on_creases : int;  (** vertices on creases and corners *)
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
  { open_edges = !open_edges; crowded = !crowded; angles; on_creases = List.length (List.sort_uniq compare (List.concat_map (fun (u, v) -> [ u; v ]) (Array.to_list m.creases))) }

(* An STL file in text, a solid for each face under its name. *)
let stl (m : t) =
  let mine = Array.make (Array.length m.names) [] in
  Array.iteri (fun t f -> if f >= 0 then mine.(f) <- t :: mine.(f)) m.face;
  let b = Buffer.create (1 lsl 20) in
  Array.iteri
    (fun f triangles ->
      if triangles <> [] then begin
        let name = String.map (fun ch -> if ch = ' ' || ch = '\n' then '_' else ch) m.names.(f) in
        Printf.bprintf b "solid %s\n" name;
        List.iter
          (fun t ->
            let u, v, w = m.triangles.(t) in
            let pu = m.points.(u) and pv = m.points.(v) and pw = m.points.(w) in
            let n = cross (sub pv pu) (sub pw pu) in
            let n = if norm n > 0. then unit n else n in
            Printf.bprintf b " facet normal %g %g %g\n  outer loop\n" n.(0) n.(1) n.(2);
            List.iter (fun p -> Printf.bprintf b "   vertex %.9g %.9g %.9g\n" p.(0) p.(1) p.(2)) [ pu; pv; pw ];
            Buffer.add_string b "  endloop\n endfacet\n")
          (List.rev triangles);
        Printf.bprintf b "endsolid %s\n" name
      end)
    mine;
  Buffer.contents b
