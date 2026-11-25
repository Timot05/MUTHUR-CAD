(* Copper as fields. A piece of copper (a pad, a stretch of trace, a via's ring) is the
   kernel's own kind of thing, a field that is below zero inside it, drawn in the
   board's plane: the distance to its outline, whatever z is. So everything the kernel
   does with a solid it does with copper, and nothing here is geometry of its own.

   - A trace is a sketch line given a width: each stretch of it the outline round
     that stretch, two straight sides and two half circles.
   - A pad is a circle, or a rectangle with its corners sharp or round, turned as its
     part is turned.
   - The clearance between two pieces is the kernel's between two solids (Clash).

   Lengths are millimetres. The board's plane is x and y with y up, as the kernel's is:
   a picture of it is the board seen from the front. *)
module G = Graph
module S = Sketch

(* A layer of copper: the two sides, and those inside a board of more than two. *)
type side = Front | Back | Inner of int

let said = function Front -> "front" | Back -> "back" | Inner k -> Printf.sprintf "inner layer %d" k

(* A shape as numbers too, for a page that draws it itself: a line with a width, an
   arc with a width (from an angle, counter-clockwise through a sweep, in radians), a
   rectangle with its corners rounded and turned (degrees), a circle. Or any outline
   at all, of which only its field says what it is. *)
type shape =
  | Line of (float * float) * (float * float) * float
  | Arc of (float * float) * float * float * float * float  (** centre, the arc's radius, half the width, from, sweep *)
  | Rounded of (float * float) * float * float * float * float  (** centre, half width, half height, corners' radius, turn *)
  | Any

(* A shape: its outline as it is drawn (none for a circle, which its numbers say), its
   box in the plane, and what it is. Its field is made from these when it is asked for
   ([form_field]): a board has tens of thousands, and most are never read as fields. *)
type form = { outline : S.t option; lo : float * float; hi : float * float; shape : shape }

type piece = {
  what : string;  (** for a report: "R1.1", "VCC trace 1", "GND via" *)
  net : string;  (** "" for copper that is on no net *)
  on : side list;  (** the layers it is on: one, or for a hole's pad or a via every one it goes through *)
  bare : bool;  (** no copper: a hole, which copper keeps clear of as of another net *)
  outline : S.t option;  (** as it is drawn; none for a circle *)
  lo : float * float;
  hi : float * float;  (** its box in the plane *)
  shape : shape;
}

let shares a b = List.exists (fun s -> List.mem s b.on) a.on
let turned (x, y) degrees = let a = degrees *. Float.pi /. 180. in ((x *. cos a) -. (y *. sin a), (x *. sin a) +. (y *. cos a))
let ( ++ ) (ax, ay) (bx, by) = (ax +. bx, ay +. by)

(* ---- Shapes ---- *)

(* A shape's field is made in the board's own plane, whatever frame or feature the
   board is being built in: the board is checked and drawn where it lies flat, and is
   put in its place only as a solid (Circuit_board.solid). *)
let flat build =
  let coords = !G.coords and ambient = !G.ambient and current = !G.current in
  G.coords := None;
  G.ambient := Frame.identity;
  G.current := 0;
  Fun.protect ~finally:(fun () -> G.coords := coords; G.ambient := ambient; G.current := current) build

let disc (cx, cy) r = { outline = None; lo = (cx -. r, cy -. r); hi = (cx +. r, cy +. r); shape = Line ((cx, cy), (cx, cy), r) }

(* What is within r of the line from a to b: the outline round it. *)
let stadium (ax, ay) (bx, by) r =
  let dx = bx -. ax and dy = by -. ay in
  let l = sqrt ((dx *. dx) +. (dy *. dy)) in
  if l < 1e-9 then disc (ax, ay) r
  else
    (* to the left of the way from a to b, r long *)
    let nx = -.dy /. l *. r and ny = dx /. l *. r in
    let p1 = (ax -. nx, ay -. ny) and p2 = (bx -. nx, by -. ny) and p3 = (bx +. nx, by +. ny) and p4 = (ax +. nx, ay +. ny) in
    let outline = S.(start p1 |> line "right" p2 |> arc "far end" ~centre:(bx, by) p3 |> line "left" p4 |> arc "near end" ~centre:(ax, ay) p1 |> close) in
    { outline = Some outline; lo = (Float.min ax bx -. r, Float.min ay by -. r); hi = (Float.max ax bx +. r, Float.max ay by +. r); shape = Line ((ax, ay), (bx, by), r) }

(* What is within r of the arc from a through m to b: the outline round it, two arcs
   about the arc's centre and a half circle at either end. Its box is that of points
   along it, not of the whole circle. *)
let band (ax, ay) (mx, my) (bx, by) r =
  let d = 2. *. ((ax *. (my -. by)) +. (mx *. (by -. ay)) +. (bx *. (ay -. my))) in
  if Float.abs d < 1e-9 then stadium (ax, ay) (bx, by) r
  else
    let a2 = (ax *. ax) +. (ay *. ay) and m2 = (mx *. mx) +. (my *. my) and b2 = (bx *. bx) +. (by *. by) in
    let cx = ((a2 *. (my -. by)) +. (m2 *. (by -. ay)) +. (b2 *. (ay -. my))) /. d and cy = ((a2 *. (bx -. mx)) +. (m2 *. (ax -. bx)) +. (b2 *. (mx -. ax))) /. d in
    let big = sqrt (((ax -. cx) ** 2.) +. ((ay -. cy) ** 2.)) in
    if big <= r *. 1.001 then stadium (ax, ay) (bx, by) r
    else
      (* counter-clockwise from a to b: the other way round, a and b change places *)
      let (ax, ay), (bx, by) = if ((mx -. ax) *. (by -. my)) -. ((my -. ay) *. (bx -. mx)) > 0. then ((ax, ay), (bx, by)) else ((bx, by), (ax, ay)) in
      let at (px, py) k = (cx +. ((px -. cx) *. k), cy +. ((py -. cy) *. k)) in
      let out p = at p ((big +. r) /. big) and inn p = at p ((big -. r) /. big) in
      let outline =
        S.(start (out (ax, ay)) |> arc "outer" ~centre:(cx, cy) (out (bx, by)) |> arc "far end" ~centre:(bx, by) (inn (bx, by)) |> arc ~cw:true "inner" ~centre:(cx, cy) (inn (ax, ay)) |> arc "near end" ~centre:(ax, ay) (out (ax, ay)) |> close)
      in
      let from = Float.atan2 (ay -. cy) (ax -. cx) and upto = Float.atan2 (by -. cy) (bx -. cx) in
      let sweep = let t = upto -. from in if t <= 0. then t +. (2. *. Float.pi) else t in
      let along = List.init 33 (fun i -> let t = from +. (sweep *. float_of_int i /. 32.) in (cx +. (big *. cos t), cy +. (big *. sin t))) in
      { outline = Some outline;
        lo = (List.fold_left (fun m p -> Float.min m (fst p)) infinity along -. r, List.fold_left (fun m p -> Float.min m (snd p)) infinity along -. r);
        hi = (List.fold_left (fun m p -> Float.max m (fst p)) neg_infinity along +. r, List.fold_left (fun m p -> Float.max m (snd p)) neg_infinity along +. r);
        shape = Arc ((cx, cy), big, r, from, sweep) }

(* A rectangle w by h about a centre, its corners rounded to r (0: sharp; half its
   shorter side: an oval), turned by [turn] degrees. *)
let rounded (cx, cy) w h r turn =
  let hw = 0.5 *. w and hh = 0.5 *. h in
  let r = Float.max 0. (Float.min r (Float.min hw hh)) in
  let at p = turned p turn ++ (cx, cy) in
  let reach = sqrt ((hw *. hw) +. (hh *. hh)) in
  let made outline = { outline = Some outline; lo = (cx -. reach, cy -. reach); hi = (cx +. reach, cy +. reach); shape = Rounded ((cx, cy), hw, hh, r, turn) } in
  if r >= Float.min hw hh -. 1e-9 then
    (* an oval: a line with a width *)
    if hw >= hh then stadium (at (-.(hw -. r), 0.)) (at (hw -. r, 0.)) r else stadium (at (0., -.(hh -. r))) (at (0., hh -. r)) r
  else if r <= 1e-9 then made (S.polygon [ at (-.hw, -.hh); at (hw, -.hh); at (hw, hh); at (-.hw, hh) ])
  else
    let outline =
      S.(
        start (at (-.hw +. r, -.hh))
        |> line "bottom" (at (hw -. r, -.hh))
        |> arc "corner 1" ~centre:(at (hw -. r, -.hh +. r)) (at (hw, -.hh +. r))
        |> line "right" (at (hw, hh -. r))
        |> arc "corner 2" ~centre:(at (hw -. r, hh -. r)) (at (hw -. r, hh))
        |> line "top" (at (-.hw +. r, hh))
        |> arc "corner 3" ~centre:(at (-.hw +. r, hh -. r)) (at (-.hw, hh -. r))
        |> line "left" (at (-.hw, -.hh +. r))
        |> arc "corner 4" ~centre:(at (-.hw +. r, -.hh +. r)) (at (-.hw +. r, -.hh))
        |> close)
    in
    made outline

(* Any closed outline, as it is drawn: an electrode, a pad of a shape of its own. *)
let drawn (outline : S.t) =
  let points = List.concat_map S.points (Array.to_list outline) in
  { outline = Some outline;
    lo = (List.fold_left (fun m p -> Float.min m (fst p)) infinity points, List.fold_left (fun m p -> Float.min m (snd p)) infinity points);
    hi = (List.fold_left (fun m p -> Float.max m (fst p)) neg_infinity points, List.fold_left (fun m p -> Float.max m (snd p)) neg_infinity points);
    shape = Any }

(* ---- Pieces ---- *)

let piece ?(bare = false) what ~net on (f : form) = { what; net; on; bare; outline = f.outline; lo = f.lo; hi = f.hi; shape = f.shape }

(* A shape's field, in the board's plane: of its outline, or of the circle it is. *)
let field_of outline shape =
  flat (fun () ->
      match (outline, shape) with
      | Some drawn, _ -> S.field drawn
      | None, Line ((cx, cy), _, r) -> Solid.cylinder "round" (cx, cy) r
      | None, _ -> invalid_arg "a shape with no outline that is no circle")

let field (p : piece) = field_of p.outline p.shape
let form_field (f : form) = field_of f.outline f.shape

(* A trace: a line through points, [width] wide, a piece for each stretch. They lie
   over each other where they join, which is what joins them. *)
let trace ~net ~side ~width points =
  let rec stretches k = function
    | a :: (b :: _ as rest) -> piece (Printf.sprintf "%s trace, stretch %d" (if net = "" then "a" else net) k) ~net [ side ] (stadium a b (0.5 *. width)) :: stretches (k + 1) rest
    | _ -> []
  in
  stretches 1 points

(* A via: a ring on every layer it goes through, by its outer diameter. The hole in it
   is the board's business (Board), not copper's. *)
let via ~net ~diameter ~through at = piece (Printf.sprintf "%s via" (if net = "" then "a" else net)) ~net through (disc at (0.5 *. diameter))

(* ---- How far apart two pieces are ---- *)

(* A shape is all that is within a length of its middle, which is a stretch of line, an
   arc or a rectangle. So two shapes are as far apart as their middles, less the two
   lengths: worked out here from the numbers, exactly, with no field read. A board has
   tens of thousands of pieces and every two that are near are asked. *)
type edge = Seg of (float * float) * (float * float) | Bow of (float * float) * float * float * float  (** centre, radius, from, sweep *)

type core = { edges : edge list; box : (float * float) array;  (** a rectangle's corners, counter-clockwise, or none *) at : float * float;  (** a place on it *) r : float }

let sub (ax, ay) (bx, by) = (ax -. bx, ay -. by)
let dot (ax, ay) (bx, by) = (ax *. bx) +. (ay *. by)
let cross (ax, ay) (bx, by) = (ax *. by) -. (ay *. bx)
let far a b = let d = sub a b in sqrt (dot d d)
let along (ax, ay) (bx, by) t = (ax +. (t *. (bx -. ax)), ay +. (t *. (by -. ay)))
let turn = 2. *. Float.pi

(* The nearest of some: how far, and the place on each. *)
let nearest = List.fold_left (fun (d, p, q) (d', p', q') -> if d' < d then (d', p', q') else (d, p, q)) (infinity, (0., 0.), (0., 0.))

let to_seg p (a, b) =
  let d = sub b a in
  let l2 = dot d d in
  let q = along a b (if l2 < 1e-18 then 0. else Float.max 0. (Float.min 1. (dot (sub p a) d /. l2))) in
  (far p q, q)

let on_bow ((cx, cy), big, _, _) angle = (cx +. (big *. cos angle), cy +. (big *. sin angle))
let within (_, _, from, sweep) angle = let t = angle -. from in t -. (turn *. Float.floor (t /. turn)) <= sweep +. 1e-9

let to_bow p ((c, _, from, sweep) as bow) =
  let d = sub p c in
  let angle = Float.atan2 (snd d) (fst d) in
  if dot d d > 1e-18 && within bow angle then let q = on_bow bow angle in (far p q, q)
  else
    let e1 = on_bow bow from and e2 = on_bow bow (from +. sweep) in
    if far p e1 <= far p e2 then (far p e1, e1) else (far p e2, e2)

let seg_seg (a, b) (c, d) =
  let r = sub b a and s = sub d c in
  let den = cross r s in
  (* two that lie along each other cross nowhere: their ends say how near they are *)
  let level = Float.abs den <= 1e-9 *. sqrt (dot r r *. dot s s) in
  let t = if level then -1. else cross (sub c a) s /. den and u = if level then -1. else cross (sub c a) r /. den in
  if t >= 0. && t <= 1. && u >= 0. && u <= 1. then let x = along a b t in (0., x, x)
  else
    nearest
      [ (let l, q = to_seg a (c, d) in (l, a, q)); (let l, q = to_seg b (c, d) in (l, b, q)); (let l, q = to_seg c (a, b) in (l, q, c)); (let l, q = to_seg d (a, b) in (l, q, d)) ]

(* A stretch of line and an arc: the nearest is at an end of either, where the line is
   nearest the arc's centre, where it crosses the arc's circle, or where it crosses the
   lines from the centre through the arc's ends. *)
let seg_bow (a, b) ((c, big, from, sweep) as bow) =
  let d = sub b a in
  let l2 = dot d d in
  let ts = ref [ 0.; 1. ] in
  let add t = if t >= 0. && t <= 1. then ts := t :: !ts in
  if l2 > 1e-18 then begin
    let f = sub a c in
    add (Float.max 0. (Float.min 1. (-.dot f d /. l2)));
    let half = dot f d and rest = dot f f -. (big *. big) in
    let under = (half *. half) -. (l2 *. rest) in
    if under >= 0. then (add ((-.half +. sqrt under) /. l2); add ((-.half -. sqrt under) /. l2));
    List.iter (fun angle -> let u = (cos angle, sin angle) in let den = cross d u in if Float.abs den > 1e-9 *. sqrt l2 then add (cross (sub c a) u /. den)) [ from; from +. sweep ]
  end;
  nearest
    (List.map (fun t -> let p = along a b t in let l, q = to_bow p bow in (l, p, q)) !ts
    @ List.map (fun angle -> let e = on_bow bow angle in let l, q = to_seg e (a, b) in (l, q, e)) [ from; from +. sweep ])

(* Two arcs: at an end of either, on the line through the two centres, or where the
   two circles cross. *)
let bow_bow ((c1, r1, f1, s1) as b1) ((c2, r2, f2, s2) as b2) =
  let d = sub c2 c1 in
  let l = sqrt (dot d d) in
  let a = Float.atan2 (snd d) (fst d) in
  let line = if l < 1e-9 then [] else [ a; a +. Float.pi ] in
  let crossings =
    if l < 1e-9 || l > r1 +. r2 || l < Float.abs (r1 -. r2) then []
    else let k = Float.acos (Float.max (-1.) (Float.min 1. (((r1 *. r1) +. (l *. l) -. (r2 *. r2)) /. (2. *. r1 *. l)))) in [ a +. k; a -. k ]
  in
  let on bow angles = List.filter_map (fun angle -> if within bow angle then Some (on_bow bow angle) else None) angles in
  nearest
    (List.map (fun p -> let l, q = to_bow p b2 in (l, p, q)) (on b1 ([ f1; f1 +. s1 ] @ line @ crossings))
    @ List.map (fun q -> let l, p = to_bow q b1 in (l, p, q)) (on b2 ([ f2; f2 +. s2 ] @ line)))

let edge_edge e f =
  match (e, f) with
  | Seg (a, b), Seg (c, d) -> seg_seg (a, b) (c, d)
  | Seg (a, b), Bow (c, big, from, sweep) -> seg_bow (a, b) (c, big, from, sweep)
  | Bow (c, big, from, sweep), Seg (a, b) -> let l, p, q = seg_bow (a, b) (c, big, from, sweep) in (l, q, p)
  | Bow (c, big, from, sweep), Bow (c', big', from', sweep') -> bow_bow (c, big, from, sweep) (c', big', from', sweep')

(* A shape's middle and its length; none for an outline of its own. *)
let core = function
  | Line (a, b, r) -> Some { edges = [ Seg (a, b) ]; box = [||]; at = a; r }
  | Arc (c, big, r, from, sweep) -> Some { edges = [ Bow (c, big, from, sweep) ]; box = [||]; at = on_bow (c, big, from, sweep) from; r }
  | Rounded (c, hw, hh, r, by) ->
      let w = hw -. r and h = hh -. r in
      let k = Array.map (fun p -> turned p by ++ c) [| (-.w, -.h); (w, -.h); (w, h); (-.w, h) |] in
      Some { edges = List.init 4 (fun i -> Seg (k.(i), k.((i + 1) mod 4))); box = k; at = k.(0); r }
  | Any -> None

let inside k p = Array.length k = 4 && (let all = ref true in for i = 0 to 3 do if cross (sub k.((i + 1) mod 4) k.(i)) (sub p k.(i)) < 0. then all := false done; !all)

(* How far apart two are, below zero when they are into each other (then at least
   that far), and where: between the two, at their nearest. *)
let apart (a : core) (b : core) =
  let d, x, y = if inside a.box b.at then (0., b.at, b.at) else if inside b.box a.at then (0., a.at, a.at) else nearest (List.concat_map (fun e -> List.map (edge_edge e) b.edges) a.edges) in
  let gap = d -. a.r -. b.r in
  let px, py = if d < 1e-12 then x else along x y ((a.r +. (0.5 *. gap)) /. d) in
  (gap, [| px; py; 0. |])

(* ---- The same from the fields, for any shape ---- *)

(* A field read at a place of the board's plane. *)
let reader field =
  let p = Prog.compile field in
  let c = Prog.ctx p and whole = Prog.whole p in
  fun (x, y) -> Prog.value p c whole [| x; y; 0. |]

(* The least of [g] over what is inside [a] (where a is below zero), within a box, when
   it is under [below]: how much, and where. Both are distances, so neither changes
   faster than the way gone, and a square of the plane is left as soon as its middle
   says all of it is outside a, or that nowhere in it is g under the least found so
   far. What is left is cut in four, the most promising first. It is the kernel's
   clearance between two solids, kept to the board's plane: squares for cubes, and two
   numbers to a place for three.

   Good to within twice [tol]. Where two edges run along each other at one distance
   there is no one nearest place, and every square along them is cut down to tol when
   that distance is under [below]: such a pair costs by its length. *)
let least ?(tol = 1e-3) ~below ~lo:(lx, ly) ~hi:(hx, hy) a g =
  if hx < lx || hy < ly then None
  else begin
    let best = ref below and where = ref None in
    let root2 = sqrt 2. in
    let rec visit (x, y) half va vg =
      let reach = half *. root2 in
      if va <= reach && vg -. reach < !best -. tol then begin
        if va <= 0. && vg < !best then (best := vg; where := Some (x, y));
        if reach > tol then begin
          let q = 0.5 *. half in
          let four = List.map (fun (dx, dy) -> let at = (x +. (dx *. q), y +. (dy *. q)) in (at, a at, g at)) [ (-1., -1.); (1., -1.); (-1., 1.); (1., 1.) ] in
          List.iter (fun (at, va, vg) -> visit at q va vg) (List.sort (fun (_, _, p) (_, _, q) -> compare p q) four)
        end
      end
    in
    let middle = (0.5 *. (lx +. hx), 0.5 *. (ly +. hy)) in
    visit middle (0.5 *. Float.max (hx -. lx) (hy -. ly) +. tol) (a middle) (g middle);
    Option.map (fun (x, y) -> (!best, [| x; y; 0. |])) !where
  end

(* How far apart two pieces are by their fields, when that is under [limit]: the least
   of the one's field over the inside of the other. *)
let near ~limit (p : piece) read_p (q : piece) read_q =
  (* the smaller of the two is the one gone over *)
  let size (r : piece) = Float.max (fst r.hi -. fst r.lo) (snd r.hi -. snd r.lo) in
  let (p, read_p), (q, read_q) = if size p <= size q then ((p, read_p), (q, read_q)) else ((q, read_q), (p, read_p)) in
  least ~below:limit
    ~lo:(Float.max (fst p.lo) (fst q.lo -. limit), Float.max (snd p.lo) (snd q.lo -. limit))
    ~hi:(Float.min (fst p.hi) (fst q.hi +. limit), Float.min (snd p.hi) (snd q.hi +. limit))
    read_p read_q
