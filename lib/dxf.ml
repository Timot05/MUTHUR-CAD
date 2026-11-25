(* A flat outline as a DXF file: a board's edge, for KiCad or any other program to read.

   What is given is polylines traced along the outline's edges. What is written is what
   each of them is: a straight one a LINE, one on a circle an ARC, a closed one on a
   circle a CIRCLE. One that is neither, to within [tol], is written as its own short
   lines, and counted, so that it can be told.

   The file is in millimetres, in the plane z = 0, all of it in one layer. *)

type point = float * float

type entity = Line of point * point | Arc of point * float * float * float  (** the centre, the radius, and from where to where in degrees, counter-clockwise *) | Circle of point * float

let tol = 2e-3

let dist (ax, ay) (bx, by) = Float.hypot (ax -. bx) (ay -. by)

(* How far the points are from the line through a and b, at most. *)
let off_line a b pts =
  let (ax, ay), (bx, by) = (a, b) in
  let l = dist a b in
  List.fold_left (fun m (x, y) -> Float.max m (Float.abs ((((bx -. ax) *. (y -. ay)) -. ((by -. ay) *. (x -. ax))) /. l))) 0. pts

(* The circle nearest the points, by least squares on x2 + y2 (about their middle, so
   the numbers stay small), and how far the points are from it at most. None where they
   are in a line. *)
let circle pts =
  let n = float_of_int (List.length pts) in
  let mx = List.fold_left (fun s (x, _) -> s +. x) 0. pts /. n and my = List.fold_left (fun s (_, y) -> s +. y) 0. pts /. n in
  let suu = ref 0. and suv = ref 0. and svv = ref 0. and bu = ref 0. and bv = ref 0. in
  List.iter
    (fun (x, y) ->
      let u = x -. mx and v = y -. my in
      let q = (u *. u) +. (v *. v) in
      suu := !suu +. (u *. u); suv := !suv +. (u *. v); svv := !svv +. (v *. v); bu := !bu +. (u *. q); bv := !bv +. (v *. q))
    pts;
  let det = (!suu *. !svv) -. (!suv *. !suv) in
  if Float.abs det <= 1e-12 *. (!suu +. !svv) *. (!suu +. !svv) then None
  else
    let a = ((!bu *. !svv) -. (!bv *. !suv)) /. (2. *. det) and b = ((!bv *. !suu) -. (!bu *. !suv)) /. (2. *. det) in
    let r = sqrt (((!suu +. !svv) /. n) +. (a *. a) +. (b *. b)) in
    let c = (mx +. a, my +. b) in
    Some (c, r, List.fold_left (fun m p -> Float.max m (Float.abs (dist p c -. r))) 0. pts)

let degrees (cx, cy) (x, y) =
  let d = Float.atan2 (y -. cy) (x -. cx) *. 180. /. Float.pi in
  if d < 0. then d +. 360. else d

(* ---- What a traced polyline is ---- *)

(* One thing a stretch of points can be: on a line, or on a circle, gone round it
   counter-clockwise or not. *)
type shape = Straight of point * point | Round of point * float * bool

let ends pts = (List.hd pts, List.nth pts (List.length pts - 1))

let one pts =
  let first, last = ends pts in
  if dist first last > tol && off_line first last pts <= tol then Some (Straight (first, last))
  else if List.length pts < 3 then None
  else
    match circle pts with
    | Some (((cx, cy) as c), r, off) when off <= tol ->
        (* which way it was traced: the sum of the turns about the centre *)
        let rec turn = function (ax, ay) :: ((bx, by) :: _ as rest) -> (((ax -. cx) *. (by -. cy)) -. ((ay -. cy) *. (bx -. cx))) +. turn rest | _ -> 0. in
        Some (Round (c, r, turn pts > 0.))
    | _ -> None

(* The polyline cut into stretches that are each one thing: from a point on, the longest
   that is, and then on from the point after it. Three points are always on some circle,
   so a stretch on a circle is of four or more, unless it is the whole polyline; fewer
   are taken as far as they are in a line. A last point left over is the end of a short
   line from the point before it: an edge too short to have been traced by itself. None
   if a stretch is nothing. *)
let stretches pts =
  let p = Array.of_list pts in
  let n = Array.length p in
  let sub i j = Array.to_list (Array.sub p i (j - i + 1)) in
  let rec grow keep i j best = if j >= n then best else match one (sub i j) with Some s when keep s -> grow keep i (j + 1) (Some (j, s)) | _ -> best in
  let rec from i acc =
    if i >= n then Some (List.rev acc)
    else if i = n - 1 && i > 0 then Some (List.rev ((Straight (p.(i - 1), p.(i)), p.(i)) :: acc))
    else
      let any = grow (fun _ -> true) i (i + 1) None in
      let found = match any with Some (j, Round _) when j - i < 3 && not (i = 0 && j = n - 1) -> grow (function Straight _ -> true | Round _ -> false) i (i + 1) None | _ -> any in
      match found with Some (j, s) -> from (j + 1) ((s, p.(j)) :: acc) | None -> None
  in
  from 0 []

(* Where two stretches that follow each other meet: the point both are on that is
   nearest [near], the gap between them. Two that only touch meet where they touch. *)
let meet a b near =
  let ( +| ) (ax, ay) (bx, by) = (ax +. bx, ay +. by) and ( *| ) k (x, y) = (k *. x, k *. y) in
  let pick = function [] -> near | first :: rest -> List.fold_left (fun best q -> if dist q near < dist best near then q else best) first rest in
  let on_line (p, q) = let l = dist p q in ((1. /. l) *| (q +| (-1. *| p)), p) in
  let line_circle l (c, r) =
    let (ux, uy), ((px, py) as p) = on_line l in
    let t = ((fst c -. px) *. ux) +. ((snd c -. py) *. uy) in
    let foot = p +| (t *| (ux, uy)) in
    let h = sqrt (Float.max 0. ((r *. r) -. (dist c foot ** 2.))) in
    pick [ foot +| (h *| (ux, uy)); foot +| (-.h *| (ux, uy)) ]
  in
  match (a, b) with
  | Straight (p, q), Straight (p', q') ->
      let (ux, uy), _ = on_line (p, q) and (vx, vy), _ = on_line (p', q') in
      let det = (ux *. vy) -. (uy *. vx) in
      if Float.abs det < 1e-9 then near else let t = ((((fst p') -. fst p) *. vy) -. ((snd p' -. snd p) *. vx)) /. det in p +| (t *| (ux, uy))
  | Straight (p, q), Round (c, r, _) | Round (c, r, _), Straight (p, q) -> line_circle (p, q) (c, r)
  | Round (c, r, _), Round (c', r', _) ->
      let d = dist c c' in
      if d < 1e-9 then near
      else
        let ex, ey = (1. /. d) *| (c' +| (-1. *| c)) in
        let along = (((r *. r) -. (r' *. r')) +. (d *. d)) /. (2. *. d) in
        let h = sqrt (Float.max 0. ((r *. r) -. (along *. along))) and middle = c +| (along *| (ex, ey)) in
        pick [ middle +| (h *| (-.ey, ex)); middle +| (-.h *| (-.ey, ex)) ]

(* A traced polyline as what it is: one line, arc or circle, or several that follow each
   other, each from where it meets the one before to where it meets the next. One that
   is not is given as its own short lines, and said to be ([false]). A closed one may or
   may not end on its first point. *)
let traced ~closed pts =
  let first, last = ends pts in
  let chords () =
    let rec go = function a :: (b :: _ as rest) -> Line (a, b) :: go rest | _ -> [] in
    go (if closed && dist first last > tol then pts @ [ first ] else pts)
  in
  let drawn shape from to_ = match shape with Straight _ -> Line (from, to_) | Round (c, r, ccw) -> if ccw then Arc (c, r, degrees c from, degrees c to_) else Arc (c, r, degrees c to_, degrees c from) in
  match (closed, circle pts) with
  | true, Some (c, r, off) when off <= tol -> ([ Circle (c, r) ], true)
  | _ -> (
      match stretches (if closed && dist first last > tol then pts @ [ first ] else pts) with
      | None -> (chords (), false)
      | Some found ->
          (* each with the last of its points; the next one's first point is the one after *)
          let rec go from = function
            | [ (shape, _) ] -> [ drawn shape from (if closed then first else last) ]
            | (shape, at) :: (((next, _) :: _) as rest) ->
                let joint = meet shape next at in
                drawn shape from joint :: go joint rest
            | [] -> []
          in
          (go first found, true))

(* The file. *)
let written ?(layer = "outline") entities =
  let b = Buffer.create 4096 in
  let pair code text = Buffer.add_string b (Printf.sprintf "%d\n%s\n" code text) in
  let num code v = pair code (Printf.sprintf "%.6f" v) in
  let at k (x, y) = num (10 + k) x; num (20 + k) y; num (30 + k) 0. in
  (* drawn in millimetres *)
  List.iter (fun (c, t) -> pair c t) [ (0, "SECTION"); (2, "HEADER"); (9, "$INSUNITS"); (70, "4"); (9, "$MEASUREMENT"); (70, "1"); (0, "ENDSEC"); (0, "SECTION"); (2, "ENTITIES") ];
  List.iter
    (fun e ->
      match e with
      | Line (p, q) -> pair 0 "LINE"; pair 8 layer; at 0 p; at 1 q
      | Arc (c, r, from, to_) -> pair 0 "ARC"; pair 8 layer; at 0 c; num 40 r; num 50 from; num 51 to_
      | Circle (c, r) -> pair 0 "CIRCLE"; pair 8 layer; at 0 c; num 40 r)
    entities;
  pair 0 "ENDSEC";
  pair 0 "EOF";
  Buffer.contents b
