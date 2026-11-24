(* Edges and corners by sampling: found and walked, never proved. Ported from mk36's
   lib/sampled, reading each cell's decisions from the octree.

   - Seed. From the centre of each last-level cell, Newton onto a select's curve: the
     surface (with the select on its le side) = 0 and its guard = 0.
   - Check. The point is on an edge where the select decides the surface there and its
     two sides are different faces.
   - Walk. Step along the tangent, Newton back onto the curve, the step adapted to the
     turning angle, until the curve closes, leaves the domain or is lost.
   - Cut. Where the faces either side change along the curve, the corner is solved from
     the curve's two equations and the guard that changed sign.
   - Flag. Lost walks, and cells that show two faces with no edge traced nearby. An empty
     list of flags is not a proof. *)

module P = Prog

type vertex = { at : float array; faces : string list; degree : int }
type edge = { between : string * string; pieces : float array list list; ends : int list; closed : bool }
type flag = { where : float array; why : string }

type t = {
  vertices : vertex list;
  edges : edge list;
  flags : flag list;
  size : float;  (** the side of a last-level cell *)
  cells : int;  (** octree cells built *)
  edge_cells : int;  (** last-level cells seeded *)
  traced : int;  (** curves walked *)
  samples : int;  (** point evaluations *)
  nodes : int;
  seconds : float * float;  (** pruning, then seeding and walking *)
}

(* ---- Small vector helpers ---- *)

let sub a b = Array.init 3 (fun i -> a.(i) -. b.(i))
let add a b = Array.init 3 (fun i -> a.(i) +. b.(i))
let scale s a = Array.map (fun x -> s *. x) a
let dot a b = (a.(0) *. b.(0)) +. (a.(1) *. b.(1)) +. (a.(2) *. b.(2))
let norm a = sqrt (dot a a)
let dist a b = norm (sub a b)
let cross a b = [| (a.(1) *. b.(2)) -. (a.(2) *. b.(1)); (a.(2) *. b.(0)) -. (a.(0) *. b.(2)); (a.(0) *. b.(1)) -. (a.(1) *. b.(0)) |]
let det3 r = dot r.(0) (cross r.(1) r.(2))

(* Rows r, right-hand side b: x with r x = b, by Cramer's rule. *)
let solve3 r b =
  let d = det3 r in
  if Float.abs d <= 1e-14 *. norm r.(0) *. norm r.(1) *. norm r.(2) || not (Float.is_finite d) then None
  else
    let col i = Array.init 3 (fun k -> Array.init 3 (fun j -> if j = i then b.(k) else r.(k).(j))) in
    Some (Array.init 3 (fun i -> det3 (col i) /. d))

type settings = { tol : float; hmin : float; hmax : float; max_angle : float; limit : int }
type env = { p : P.t; c : P.ctx; tree : Octree.t; lo : float array; hi : float array; st : settings; mutable last : Octree.cell }

let inside ?(pad = 0.) lo hi x =
  x.(0) >= lo.(0) -. pad && x.(0) <= hi.(0) +. pad && x.(1) >= lo.(1) -. pad && x.(1) <= hi.(1) +. pad && x.(2) >= lo.(2) -. pad && x.(2) <= hi.(2) +. pad

(* The decisions at x. Newton and the walker mostly stay in one cell: try the last first. *)
let decisions e x =
  let c = e.last in
  if Array.length c.kids = 0 && Octree.holds c x then c.state
  else match Octree.find e.tree x with Some c -> e.last <- c; c.state | None -> e.tree.whole

let probe ?force e x = P.eval ?force e.p e.c (decisions e x) x
let value e x = P.value e.p e.c (decisions e x) x
let take s side id = if id = s then side else 0
let side_of (q : P.dual) = if q.v <= 0. then 1 else 2

(* ---- One select's curve: surface = 0 and guard = 0 ---- *)

(* The curve's two equations at x: the surface with select [s] on its le side, and its
   guard. None where [s] is not on the way to the surface value. *)
let equations e s x =
  let r = probe ~force:(take s 1) e x in
  match List.assoc_opt s r.reached with Some q -> Some (r.d, q) | None -> None

let tangent e s x =
  match equations e s x with
  | None -> None
  | Some (p, q) ->
      let t = cross p.g q.g in
      let n = norm t in
      if n < 1e-12 *. norm p.g *. norm q.g || not (Float.is_finite n) then None else Some (scale (1. /. n) t)

(* Newton onto the curve (minimum-norm step: the third row keeps the step across it). *)
let project e s x0 =
  let rec go x k =
    if k = 0 then None
    else
      match equations e s x with
      | None -> None
      | Some (p, q) -> (
          match solve3 [| p.g; q.g; cross p.g q.g |] [| p.v; q.v; 0. |] with
          | None -> None
          | Some dx ->
              let x' = sub x dx in
              if not (Array.for_all Float.is_finite x') then None else if norm dx < e.st.tol then Some x' else go x' (k - 1))
  in
  go x0 40

let ordered a b = if a < b then (a, b) else (b, a)

(* Is the curve an edge at x: [s] decides the surface there, and its two sides are
   different faces? Then the two faces. *)
let state e s x =
  if not (List.mem_assoc s (probe e x).reached) then None
  else
    let a = probe ~force:(take s 1) e x and b = probe ~force:(take s 2) e x in
    if a.face = b.face then None else Some (ordered a.face b.face)

(* Is the face of [v], one side of the curve at x, really a surface beside it? A step off
   the curve, across it but along that side's own zero set, must land on the model's
   surface with that face. Where two surfaces only touch along a curve, one of them is
   on the surface nowhere but on the curve itself, and the curve is no edge. *)
let present e s x (v : P.sample) =
  match equations e s x with
  | None -> false
  | Some (p, q) ->
      let w = cross v.d.g (cross p.g q.g) in
      let n = norm w and delta = 1e-6 *. norm (sub e.hi e.lo) in
      Float.is_finite n && n > 1e-9 *. norm v.d.g *. norm p.g *. norm q.g
      && List.exists
           (fun sign ->
             let r = probe e (add x (scale (sign *. delta /. n) w)) in
             r.face = v.face && Float.abs r.d.v <= (0.1 *. delta) +. e.p.jump.(e.p.root))
           [ 1.; -1. ]

let is_edge e s x = present e s x (probe ~force:(take s 1) e x) && present e s x (probe ~force:(take s 2) e x)

(* ---- Traced samples, for de-duplication and flags ---- *)

module Seen = struct
  type t = { cell : float; table : (int * int * int, float array * (int * int)) Hashtbl.t }

  let create cell = { cell; table = Hashtbl.create 1024 }
  let key t x = (int_of_float (floor (x.(0) /. t.cell)), int_of_float (floor (x.(1) /. t.cell)), int_of_float (floor (x.(2) /. t.cell)))
  let add t x tags = Hashtbl.add t.table (key t x) (x, tags)

  (* Some traced point within [r] of x, with [tags] if given. *)
  let near t ?tags x r =
    let i, j, k = key t x and m = int_of_float (ceil (r /. t.cell)) and found = ref false in
    for a = -m to m do
      for b = -m to m do
        for c = -m to m do
          if not !found then
            List.iter (fun (y, tg) -> if dist x y <= r && (tags = None || tags = Some tg) then found := true) (Hashtbl.find_all t.table (i + a, j + b, k + c))
        done
      done
    done;
    !found

  (* Some traced point within [r] of x on an edge of [face]. *)
  let near_face t face x r =
    let i, j, k = key t x and m = int_of_float (ceil (r /. t.cell)) and found = ref false in
    for a = -m to m do
      for b = -m to m do
        for c = -m to m do
          if not !found then List.iter (fun (y, (f, g)) -> if (f = face || g = face) && dist x y <= r then found := true) (Hashtbl.find_all t.table (i + a, j + b, k + c))
        done
      done
    done;
    !found
end

(* ---- Tracing ---- *)

type stop = Closed | Wall | Ended | Stuck of float array | Singular of float array | Lost of float array | Limit of float array

(* Walk the curve of [s] from [start] along [t0]. The points reached (not [start]) and
   why the walk stopped. *)
let march e s start sign t0 =
  let st = e.st in
  let here = state e s start in
  (* [far]: the first point ahead, on this step, where [s] was no longer reached *)
  let rec go ?far x t h acc n =
    (* Back at the start: close by, heading towards it, in the start's direction. *)
    if n > 3 && dist x start <= h && dot (sub start x) t > 0. && dot t t0 > 0.5 then (List.rev acc, Closed)
    else if n > st.limit then (List.rev acc, Limit x)
    else
      let retry () = if h > st.hmin then go ?far x t (h *. 0.5) acc n else (List.rev acc, Lost x) in
      let ahead = add x (scale h t) in
      match project e s ahead with
      | None when not (List.mem_assoc s (probe e ahead).reached) ->
          (* The select is no longer reached ahead: a decision above it changed. Shorter
             steps tell whether the curve itself goes on; if none does, the walk ends
             with that first point, and the cut below finds the corner. *)
          let far = Option.value far ~default:ahead in
          if h > st.hmax /. 32. then go ~far x t (h *. 0.5) acc n else (List.rev (far :: acc), Ended)
      | None -> retry ()
      | Some x2 -> (
          match tangent e s x2 with
          | None -> retry ()
          | Some raw ->
              (* A reversed tangent means the step jumped onto another stretch. *)
              let t2 = scale sign raw in
              let ang = acos (Float.min 1. (Float.max (-1.) (dot t t2))) in
              (* A step must not jump a feature thinner than itself: halfway, the surface
                 must still be there. *)
              let jumped () = Float.abs (value e (scale 0.5 (add x x2))) > 0.1 *. h in
              (* The tangent turns right round over an ever shorter step: the curve crosses
                 itself just ahead, where its two surfaces are tangent. It ends there. *)
              if ang > 1.5 && h < st.hmax *. 1e-5 then (List.rev acc, Singular (add x (scale (0.5 *. h) t)))
              else if (ang > st.max_angle || dist x x2 > 1.5 *. h || jumped ()) && h > st.hmin then go ?far x t (h *. 0.5) acc n
              else if dist x x2 < 0.25 *. h then
                (* Newton falls back to where we are: the curve ends at a corner within
                   this step. Shorter steps come up to it. *)
                if h > st.hmax /. 64. then go ?far x t (h *. 0.5) acc n else (List.rev acc, Stuck x2)
              else if not (inside e.lo e.hi x2) then (List.rev acc, Wall)
              else if state e s x2 <> here then (List.rev (x2 :: acc), Ended)
              else go x2 t2 (if ang < st.max_angle /. 3. then Float.min st.hmax (h *. 1.5) else h) (x2 :: acc) (n + 1))
  in
  go start t0 (st.hmax /. 4.) [] 0

(* A corner: the curve's two equations and the guard of another select [o], by Newton
   from x0. Newton keeps the branches taken at x0: an iterate that lands past the corner
   would otherwise read other formulas. Then polished with the branches read at the
   corner itself, so two edges reaching it from either side end at the same point. *)
let corner e s o x0 =
  let branches x = (probe ~force:(take s 1) e x).reached in
  let path at id = if id = s then 1 else match List.assoc_opt id at with Some q -> side_of q | None -> 0 in
  let rec go force x k =
    if k = 0 then None
    else
      let r = probe ~force e x in
      match (List.assoc_opt s r.reached, List.assoc_opt o r.reached) with
      | Some q, Some w -> (
          match solve3 [| r.d.g; q.g; w.g |] [| r.d.v; q.v; w.v |] with
          | None -> None
          | Some dx ->
              let x' = sub x dx in
              if not (Array.for_all Float.is_finite x') then None else if norm dx < e.st.tol then Some x' else go force x' (k - 1))
      | _ -> None
  in
  match go (path (branches x0)) x0 40 with
  | None -> None
  | Some x -> ( match go (path (branches x)) x 10 with Some y when dist x y <= dist x x0 -> Some y | _ -> Some x)

(* Where the faces along the curve change, between [p] (state [sp]) and [q]. First as a
   corner: another select's guard changes sign from p to q. Otherwise bisect on the
   chord, then solve from there. *)
let rec event e s p sp q =
  let st = e.st in
  let rp = (probe ~force:(take s 1) e p).reached and rq = (probe ~force:(take s 1) e q).reached in
  (* A guard with the same faces either side of it is no corner. *)
  let changes_faces o x =
    let read ss so = (probe ~force:(fun id -> if id = s then ss else if id = o then so else 0) e x).face in
    read 1 1 <> read 1 2 || read 2 1 <> read 2 2
  in
  let flipped =
    List.filter_map
      (fun (o, gp) ->
        match List.assoc_opt o rq with
        | Some gq when o <> s && side_of gp <> side_of gq && (changes_faces o p || changes_faces o q) -> Some o
        | _ -> None)
      rp
  in
  let reach = (2. *. dist p q) +. st.tol in
  (* Several guards can flip over one step: the edge ends at the first corner seen from
     its own side, so solve from both ends and keep the one nearest the edge's point. *)
  let solve x0 = List.filter_map (fun o -> match corner e s o x0 with Some x when dist x x0 <= reach -> Some x | _ -> None) flipped in
  let near x l = match List.sort (fun a b -> compare (dist a x) (dist b x)) l with y :: _ -> Some y | [] -> None in
  match near (if sp = None then q else p) (solve p @ solve q) with
  | Some x -> (x, true)
  | None -> (
      let rec go p q k =
        let m = scale 0.5 (add p q) in
        if k = 0 || dist p q < st.tol then m else if state e s m = sp then go m q (k - 1) else go p m (k - 1)
      in
      let m = go p q 80 in
      let from x0 r = List.filter_map (fun (o, _) -> if o = s then None else match corner e s o x0 with Some x when dist x m <= st.hmax -> Some x | _ -> None) r in
      match near m (from p rp @ from q rq) with
      | Some x -> (x, true)
      | None ->
          (* No guard crosses the curve here, yet its faces change: a third face lies
             along the edge, or on one of its two. The place is a guess, but one on the
             curve: [m] is on the chord. *)
          let m = match project e s m with Some y when dist y m <= dist p q -> y | _ -> m in
          let x = snap e s m in
          (x, x != m))

(* A point near a corner, moved onto it: the nearest solution of the curve's equations
   with the guard of another select that is nearly 0 there. *)
and snap e s x =
  let radius = e.st.hmax in
  let found =
    List.filter_map
      (fun (o, (r : P.dual)) ->
        let gn = norm r.g in
        if o = s || gn = 0. || Float.abs r.v /. gn > radius then None else match corner e s o x with Some y when dist y x <= radius -> Some y | _ -> None)
      (probe ~force:(take s 1) e x).reached
  in
  match List.sort (fun a b -> compare (dist a x) (dist b x)) found with y :: _ -> y | [] -> x

type end_ = Event of float array | Open of stop
type piece = { tags : int * int; pts : float array list; a : end_; b : end_ }

(* The whole curve through a seed, cut into pieces where it is an edge with fixed faces.
   Also the flags, and the points past a corner where the curve may go on as another
   edge. *)
let trace e s seen x0 =
  let st = e.st in
  match tangent e s x0 with
  | None -> ([], [ { where = x0; why = "seed on a curve with no tangent (the two surfaces touch)" } ], [])
  | Some t0 ->
      let fwd, sf = march e s x0 1. t0 in
      let pts, closed, sb =
        match sf with
        | Closed -> (x0 :: fwd, true, Closed)
        | _ ->
            let bwd, sb = march e s x0 (-1.) (scale (-1.) t0) in
            (List.rev_append bwd (x0 :: fwd), false, sb)
      in
      let flags =
        List.filter_map
          (function
            | Lost x -> Some { where = x; why = "walk lost the curve (near-tangent or very curved)" }
            | Limit x -> Some { where = x; why = "walk hit its step limit" }
            | _ -> None)
          [ sb; sf ]
      in
      let pts = Array.of_list pts in
      let states = Array.map (state e s) pts in
      let n = Array.length pts in
      let beyond =
        (if (not closed) && sb = Ended && states.(0) <> None then [ pts.(0) ] else [])
        @ if (not closed) && sf = Ended && states.(n - 1) <> None then [ pts.(n - 1) ] else []
      in
      (* Runs of equal state; events between them. A run's points go from its event to
         its event: a walk that stopped at a corner stopped a little short of it. *)
      let end_of = function Stuck x -> Event (snap e s x) | Singular x -> Event x | stop -> Open stop in
      let reached = function Event x -> [ x ] | Open _ -> [] in
      let first = if closed then Open Closed else end_of sb and last = if closed then Open Closed else end_of sf in
      let unsolved = ref [] in
      let runs = ref [] and cur = ref (pts.(0) :: reached first) and cur_start = ref first in
      for i = 1 to n - 1 do
        if states.(i) = states.(i - 1) then cur := pts.(i) :: !cur
        else begin
          let ev, solved = event e s pts.(i - 1) states.(i - 1) pts.(i) in
          if not solved then unsolved := { where = ev; why = "the faces change along this edge where no corner could be solved (a third face lies along it?)" } :: !unsolved;
          runs := (states.(i - 1), !cur_start, List.rev (ev :: !cur), Event ev) :: !runs;
          cur := [ pts.(i); ev ];
          cur_start := Event ev
        end
      done;
      let runs = List.rev ((states.(n - 1), !cur_start, List.rev (reached last @ !cur), last) :: !runs) in
      (* A walk that stopped past a corner kept one point beyond it, only to find the
         corner: drop that stub (the curve beyond is walked from its own seed). *)
      let runs = if (not closed) && sb = Ended && List.length runs > 1 then List.tl runs else runs in
      let runs = if (not closed) && sf = Ended && List.length runs > 1 then List.rev (List.tl (List.rev runs)) else runs in
      let runs =
        if not closed then runs
        else
          match runs with
          | [ (stt, _, p, _) ] -> [ (stt, Open Closed, p @ [ List.hd p ], Open Closed) ]
          | (s0, _, p0, b0) :: rest -> (
              (* The walk started mid-run: join the last run onto the first. *)
              match List.rev rest with (sl, al, pl, _) :: middle when sl = s0 -> (s0, al, pl @ p0, b0) :: List.rev middle | _ -> runs)
          | [] -> runs
      in
      (* A run with no length is no edge (a walk seeded exactly at a corner). *)
      let length p =
        let rec go acc = function a :: (b :: _ as rest) -> go (acc +. dist a b) rest | _ -> acc in
        go 0. p
      in
      (* Points closer together than a thousandth of a step say nothing of the curve:
         next to a corner they are the walk creeping up to it, and the corner solved a
         hair to one side of where the walk got to. They are dropped, a run's ends kept. *)
      let thin pts =
        let near = 1e-3 *. st.hmax in
        let rec go kept = function
          | [] -> List.rev kept
          | [ last ] -> List.rev (match kept with k :: (_ :: _ as before) when dist k last <= near -> last :: before | _ -> last :: kept)
          | x :: rest -> go (match kept with k :: _ when dist k x <= near -> kept | _ -> x :: kept) rest
        in
        go [] pts
      in
      let pieces =
        List.filter_map
          (fun (stt, a, p, b) -> match stt with Some tags when length p > Float.max st.hmin (1e-3 *. st.hmax) -> Some { tags; pts = thin p; a; b } | _ -> None)
          runs
      in
      List.iter (fun p -> List.iter (fun x -> Seen.add seen x p.tags) p.pts) pieces;
      (* A piece whose two faces are not both surfaces beside it, at any of three points
         along it, is where two surfaces touch: walked (and remembered, so it is not
         walked again), but no edge. *)
      let dropped = ref [] in
      let real p =
        let pts = Array.of_list p.pts in
        let n = Array.length pts in
        List.exists (fun k -> is_edge e s pts.(min (n - 1) (max 0 (n * k / 4)))) [ 1; 2; 3 ]
        ||
        (* Touching surfaces have parallel gradients. If these cross instead, a real edge
           is being thrown away: three faces along one curve, most likely. *)
        let x = pts.(n / 2) in
        let a = (probe ~force:(take s 1) e x).d.g and b = (probe ~force:(take s 2) e x).d.g in
        if norm (cross a b) > 0.05 *. norm a *. norm b then dropped := { where = x; why = "an edge was walked here, but one of its faces is not on the surface beside it (a third face lies along it?)" } :: !dropped;
        false
      in
      let pieces = List.filter real pieces in
      (pieces, !dropped @ !unsolved @ flags, beyond)

(* ---- Assembly ---- *)

type vert = { v_at : float array; v_tags : int list; v_degree : int }
type edg = { e_tags : int * int; e_pieces : float array list list; e_ends : int list; e_closed : bool }

(* Pieces joined at their event points: two pieces with the same faces meeting alone are
   one edge (the formula changed, the edge did not); anything else is a vertex. *)
let assemble ~tol pieces =
  let pieces = Array.of_list pieces in
  let n = Array.length pieces in
  let ends = ref [] in
  Array.iteri
    (fun i p ->
      (match p.a with Event x -> ends := (x, i, 0) :: !ends | Open _ -> ());
      match p.b with Event x -> ends := (x, i, 1) :: !ends | Open _ -> ())
    pieces;
  let clusters = ref [] in
  List.iter
    (fun ((x, _, _) as e) ->
      match List.partition (fun (c, _) -> dist c x <= tol) !clusters with
      | (c, es) :: others, rest -> clusters := (c, e :: es) :: (others @ rest)
      | [], rest -> clusters := (x, [ e ]) :: rest)
    !ends;
  let parent = Array.init n Fun.id in
  let rec find i = if parent.(i) = i then i else find parent.(i) in
  let vertices = ref [] and vertex_of = Hashtbl.create 16 and joined = Hashtbl.create 16 in
  List.iter
    (fun (c, es) ->
      match es with
      | [ (_, i, ki); (_, j, kj) ] when pieces.(i).tags = pieces.(j).tags ->
          parent.(find i) <- find j;
          Hashtbl.replace joined (i, ki) ();
          Hashtbl.replace joined (j, kj) ()
      | _ ->
          let v = List.length !vertices in
          let tags = List.sort_uniq compare (List.concat_map (fun (_, i, _) -> [ fst pieces.(i).tags; snd pieces.(i).tags ]) es) in
          vertices := { v_at = c; v_tags = tags; v_degree = List.length es } :: !vertices;
          List.iter (fun (_, i, k) -> Hashtbl.replace vertex_of (i, k) v) es)
    !clusters;
  let groups = Hashtbl.create 16 in
  for i = n - 1 downto 0 do
    Hashtbl.replace groups (find i) (i :: Option.value ~default:[] (Hashtbl.find_opt groups (find i)))
  done;
  let edges =
    Hashtbl.fold
      (fun _ members acc ->
        let ends = List.concat_map (fun i -> List.filter_map (fun k -> Hashtbl.find_opt vertex_of (i, k)) [ 0; 1 ]) members in
        let loose =
          List.exists
            (fun i ->
              List.exists
                (fun (k, e) ->
                  match e with
                  | Open (Closed | Wall | Ended) -> false
                  | Open _ -> true
                  | Event _ -> not (Hashtbl.mem joined (i, k) || Hashtbl.mem vertex_of (i, k)))
                [ (0, pieces.(i).a); (1, pieces.(i).b) ])
            members
        in
        (* Closed: no vertex, no loose end, and not cut by the domain wall. *)
        let at_wall = List.exists (fun i -> pieces.(i).a = Open Wall || pieces.(i).b = Open Wall) members in
        { e_tags = pieces.(List.hd members).tags; e_pieces = List.map (fun i -> pieces.(i).pts) members; e_ends = ends; e_closed = ends = [] && (not loose) && not at_wall }
        :: acc)
      groups []
  in
  (List.rev !vertices, edges)

(* Edges much shorter than a walk step between two corners are slivers between two
   solutions of one corner: dropped, their corners merged into the first of the two
   (a point between them would be off the surface). A corner left joining two edges of
   the same faces is no corner, and they are joined. *)
let collapse ~eps (vertices, edges) =
  let length e =
    List.fold_left (fun acc ps -> fst (List.fold_left (fun (l, prev) q -> ((match prev with Some p -> l +. dist p q | None -> l), Some q)) (acc, None) ps)) 0. e.e_pieces
  in
  let verts = Array.of_list vertices in
  let alive = Array.make (Array.length verts) true in
  let edges = ref edges and changed = ref true in
  while !changed do
    changed := false;
    match List.partition (fun e -> (not e.e_closed) && List.length e.e_ends = 2 && length e < eps) !edges with
    | [], _ -> ()
    | e :: shorts, rest ->
        changed := true;
        let a, b = match e.e_ends with [ a; b ] -> (a, b) | _ -> assert false in
        let keep = min a b and gone = max a b in
        let va = verts.(a) and vb = verts.(b) in
        if a <> b then begin
          alive.(gone) <- false;
          verts.(keep) <- { v_at = verts.(keep).v_at; v_tags = List.sort_uniq compare (va.v_tags @ vb.v_tags); v_degree = va.v_degree + vb.v_degree - 2 }
        end
        else verts.(keep) <- { va with v_degree = va.v_degree - 2 };
        edges := List.map (fun e -> { e with e_ends = List.map (fun v -> if v = gone then keep else v) e.e_ends }) (shorts @ rest)
  done;
  changed := true;
  while !changed do
    changed := false;
    let found = ref None in
    Array.iteri
      (fun v vx ->
        if !found = None && alive.(v) && vx.v_degree = 2 then
          match List.filter (fun e -> List.mem v e.e_ends) !edges with
          | [ e1; e2 ] when e1 != e2 && e1.e_tags = e2.e_tags -> found := Some (v, e1, e2)
          | _ -> ())
      verts;
    match !found with
    | None -> ()
    | Some (v, e1, e2) ->
        changed := true;
        alive.(v) <- false;
        let other e = List.filter (fun u -> u <> v) e.e_ends in
        edges := { e1 with e_pieces = e1.e_pieces @ e2.e_pieces; e_ends = other e1 @ other e2; e_closed = false } :: List.filter (fun e -> e != e1 && e != e2) !edges
  done;
  let index = Array.make (Array.length verts) (-1) and kept = ref [] and n = ref 0 in
  Array.iteri (fun v vx -> if alive.(v) then (index.(v) <- !n; incr n; kept := vx :: !kept)) verts;
  (List.rev !kept, List.map (fun e -> { e with e_ends = List.map (fun v -> index.(v)) e.e_ends }) !edges)

(* An edge's line ends on its corners, to the last digit. The edges meeting at a corner
   each solved it on their own and end a hair apart, and of two corners merged one is
   gone. *)
let pin ~within (vertices, edges) =
  let verts = Array.of_list vertices in
  let at ends x =
    match List.sort compare (List.filter_map (fun v -> let d = dist verts.(v).v_at x in if d <= within then Some (d, v) else None) ends) with
    | (_, v) :: _ -> verts.(v).v_at
    | [] -> x
  in
  let ends at = function first :: rest -> ( match List.rev rest with last :: middle -> at first :: List.rev (at last :: middle) | [] -> [ at first ]) | [] -> [] in
  (vertices, List.map (fun e -> { e with e_pieces = List.map (ends (at e.e_ends)) e.e_pieces }) edges)

(* ---- The extractor ---- *)

(* The box drawn in onto the model. A model may come in a box far larger than it is (a
   feature in the box of its assembly, or a box to search for it in), and cells are
   counted across the box's longest side. Each round boxes the cells, of 32 across, that may hold surface,
   until a round takes less than a tenth off that side. A little room is left around,
   so that no wall touches the model; where the model runs out of the box, the wall
   stays where it was. A box the model already fills three quarters of is left alone. *)
let fitted p c ~lo ~hi =
  let longest lo hi = Array.fold_left Float.max 0. (sub hi lo) in
  let rec go l h rounds =
    match Octree.bounds p c ~lo:l ~hi:h ~depth:5 with
    | Some (l', h') when rounds > 0 && longest l' h' < 0.9 *. longest l h -> go l' h' (rounds - 1)
    | Some box -> box
    | None -> (l, h)
  in
  let l, h = go lo hi 4 in
  if longest l h > 0.75 *. longest lo hi then (lo, hi)
  else
    let room = 0.02 *. longest l h in
    (Array.map2 (fun wall v -> Float.max wall (v -. room)) lo l, Array.map2 (fun wall v -> Float.min wall (v +. room)) hi h)

(* Newton onto the surface only, for checking which faces a cell shows. *)
let to_surface ~tol e x0 =
  let rec go x k =
    if k = 0 then None
    else
      let r = probe e x in
      let n2 = dot r.d.g r.d.g in
      if n2 = 0. || not (Float.is_finite n2) then None
      else
        let dx = scale (r.d.v /. n2) r.d.g in
        let x' = sub x dx in
        if norm dx < tol then (let r = probe e x' in Some (x', r.face, scale (1. /. norm r.d.g) r.d.g)) else go x' (k - 1)
  in
  go x0 30

(* [depth] levels of subdivision: the last-level cells are 1 / 2^depth of the domain's
   longest side (6 is mk36's GRID=64). With [fit], the domain is first drawn in onto the
   model. *)
let extract ?(fit = false) ?(depth = 6) ~lo ~hi g =
  let t0 = Unix.gettimeofday () in
  let p = P.compile g in
  let c = P.ctx p in
  let lo, hi = if fit then fitted p c ~lo ~hi else (lo, hi) in
  let tree = Octree.build p c ~lo ~hi ~depth in
  let t1 = Unix.gettimeofday () in
  let w = Array.fold_left Float.max 0. (sub hi lo) and diag = norm (sub hi lo) in
  let e = { p; c; tree; lo; hi; last = tree.root; st = { tol = 1e-11 *. diag; hmin = diag *. 1e-7; hmax = w /. 64.; max_angle = 0.15; limit = 20_000 } } in
  let st = e.st in
  let seen = Seen.create (2. *. st.hmax) in
  let pieces = ref [] and flags = ref [] and traced = ref 0 in
  (* Walk the curve of [s] through x, unless already walked there. Several selects can
     share one dividing surface (the spans of a sketch do), and which of them is reached
     on it is down to rounding: if [s] is not, another whose guard is 0 at x may be the
     one that decides there, and its curve is the same. *)
  let rec walk ?(others = true) s x =
    match state e s x with
    | Some tags when not (Seen.near seen ~tags x st.hmax) ->
        incr traced;
        let ps, fs, beyond = trace e s seen x in
        pieces := ps @ !pieces;
        flags := fs @ !flags;
        List.iter (fun y -> Option.iter (walk s) (project e s y)) beyond
    | Some _ -> ()
    | None ->
        if others then
          List.iter
            (fun (o, (q : P.dual)) -> if o <> s && Float.abs q.v <= st.tol *. norm q.g then Option.iter (walk ~others:false o) (project e o x))
            (probe e x).reached
  in
  let box (cell : Octree.cell) = (cell.lo, Array.map (fun l -> l +. cell.size) cell.lo) in
  (* Each cell is seeded from its centre. Along the way, the faces it shows: the centre's
     own, and the one across each decision close by. *)
  let shown =
    List.map
      (fun (cell : Octree.cell) ->
        let r = Octree.radius cell and x0 = Octree.centre cell and blo, bhi = box cell in
        let at = probe e x0 in
        let faces = ref [ at.face ] in
        List.iter
          (fun (s, (q : P.dual)) ->
            let gn = norm q.g in
            if gn > 0. && Float.abs q.v /. gn < 2. *. r then begin
              (* One side's face is the centre's own. *)
              let other = (probe ~force:(take s (3 - side_of q)) e x0).face in
              faces := other :: !faces;
              (* Where the curve is to first order, from the centre's values alone: if
                 that is well outside the cell, the cell there seeds it. A decision
                 whose two sides here already have an edge walked through this cell is
                 not seeded again. *)
              match solve3 [| at.d.g; q.g; cross at.d.g q.g |] [| at.d.v; q.v; 0. |] with
              | Some dx when inside ~pad:r blo bhi (sub x0 dx) && not (Seen.near seen ~tags:(ordered at.face other) x0 r) -> (
                  match project e s x0 with Some x when inside ~pad:(0.25 *. r) blo bhi x -> walk s x | _ -> ())
              | _ -> ()
            end)
          at.reached;
        (cell, List.sort_uniq compare !faces))
      tree.leaves
  in
  (* Every face a cell shows beside another must have an edge traced nearby (not every
     pair: at a pyramid's apex opposite faces share a cell and no edge). For each cell,
     first the faces where nine points of it are, which is cheap; if some face has no
     edge, the same points moved onto the surface; then a walk from between each pair
     with such a face, and a flag if it still has none. Two faces whose normals face
     opposite ways are the two sides of a thin wall, not an edge. *)
  List.iter
    (fun ((cell : Octree.cell), faces) ->
      let r = Octree.radius cell and x0 = Octree.centre cell and blo, bhi = box cell in
      let q = 0.25 *. cell.size in
      let starts = x0 :: List.init 8 (fun o -> Array.mapi (fun a v -> if o land (1 lsl a) <> 0 then v +. q else v -. q) x0) in
      let untraced points =
        let bare f = not (Seen.near_face seen f x0 (r +. st.hmax)) in
        List.concat_map (fun (x, l, n) -> List.filter_map (fun (y, m, k) -> if l < m && dot n k > -0.5 && (bare l || bare m) then Some (x, y, l, m) else None) points) points
      in
      (* Only where no edge is near, or a face seen while seeding has none: most cells
         are settled by looking up what was traced. *)
      let reach = r +. st.hmax in
      let doubt = (not (Seen.near seen x0 reach)) || List.exists (fun f -> f >= 0 && not (Seen.near_face seen f x0 reach)) faces in
      let rough = if doubt then List.map (fun x -> let s = probe e x in (x, s.face, scale (1. /. norm s.d.g) s.d.g)) starts else [] in
      if untraced rough <> [] then begin
        let points = List.filter_map (fun x -> match to_surface ~tol:(1e-6 *. cell.size) e x with Some (y, _, _) as r when inside blo bhi y -> r | _ -> None) starts in
        List.iter
          (fun (x, y, _, _) ->
            let mid = scale 0.5 (add x y) in
            List.iter (fun (s, _) -> match project e s mid with Some z when inside ~pad:r blo bhi z -> walk s z | _ -> ()) (probe e mid).reached)
          (untraced points);
        match untraced points with
        | [] -> ()
        | left ->
            let labels = List.sort_uniq compare (List.concat_map (fun (_, _, l, m) -> [ P.face_name p l; P.face_name p m ]) left) in
            flags := { where = x0; why = "faces " ^ String.concat ", " labels ^ " meet here but no edge was traced" } :: !flags
      end)
    shown;
  let eps = st.hmax /. 8. in
  let vertices, edges = pin ~within:eps (collapse ~eps (assemble ~tol:(1e-6 *. diag) !pieces)) in
  let name = P.face_name p in
  let pair (a, b) = let a = name a and b = name b in if a < b then (a, b) else (b, a) in
  { vertices = List.map (fun v -> { at = v.v_at; faces = List.sort compare (List.map name v.v_tags); degree = v.v_degree }) vertices;
    edges = List.map (fun e -> { between = pair e.e_tags; pieces = e.e_pieces; ends = e.e_ends; closed = e.e_closed }) edges;
    flags = List.rev !flags; size = w /. float_of_int (1 lsl depth); cells = tree.cells; edge_cells = List.length tree.leaves; traced = !traced; samples = c.count; nodes = p.nodes;
    seconds = (t1 -. t0, Unix.gettimeofday () -. t1) }
