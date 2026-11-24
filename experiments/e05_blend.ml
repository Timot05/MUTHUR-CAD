(* 5. A blend of a given radius: does asking for 4 mm give 4 mm?

   Shapes, in millimetres: two planes meeting at 90 degrees and at 60 degrees, the edge
   rounded with radius 4; then a 90 degree concave edge filled with radius 4. A fillet of
   radius r is part of a cylinder of radius r about a known line.
   Limits, fixed beforehand:
   - every point of the fillet's face within 1 % of 4 mm from that line;
   - two edges (the fillet's two borders), no corner, no flag;
   - the field keeps its declared slope bound.
   The polynomial blend carried over from mk36 is measured against the same line. *)
open Mk52
open Harness
module G = Graph

let lo = [| -31.; -29.; -27. |] and hi = [| 33.; 35.; 30. |]
let r = 4.

(* Two half-spaces through the y axis whose solid has the angle [inside] between its faces. *)
let planes inside =
  let t = (180. -. inside) *. Float.pi /. 180. in
  ((1., 0., 0.), (Float.cos t, 0., Float.sin t), Float.cos t)

(* The largest | distance to the line - r | / r among surface points on the face [face];
   the line runs along y through (cx, cz). *)
let radius_error g face (cx, cz) =
  let f = field g in
  let near = Array.of_list (List.filter (fun x -> Float.abs x.(0) < 12. && Float.abs x.(2) < 12.) (Array.to_list (points ~lo ~hi 60_000))) in
  let on = List.filter (fun x -> f.face x = face) (Array.to_list (surface_points f ~lo ~hi near)) in
  (List.fold_left (fun m x -> Float.max m (Float.abs (Float.sqrt (((x.(0) -. cx) ** 2.) +. ((x.(2) -. cz) ** 2.)) -. r) /. r)) 0. on, List.length on, f)

let run () =
  List.concat_map
    (fun (name, inside, convex) ->
      let (ax, ay, az), (bx, by, bz), c = planes inside in
      let a = Solid.plane "A" (ax, ay, az) 0. and b = Solid.plane "B" (bx, by, bz) 0. in
      let g = if convex then G.round_intersection ~radius:r ~cos:c ~name:"fillet" a b else G.round_union ~radius:r ~cos:c ~name:"fillet" a b in
      (* the fillet's axis: where both planes are r inside (convex) or outside (concave) *)
      let s = if convex then -.r else r in
      let cz = (s -. (bx *. s)) /. bz in
      let err, n, f = radius_error g "fillet[A,B]" (s, cz) in
      [ at_most ~note:(Printf.sprintf "%d points on the fillet" n) (name ^ ": radius off 4 mm, as a fraction") (if n < 100 then infinity else err) 0.01;
        at_most (name ^ ": corners, edges, flags off two edges") (fst (topology_off ~lo ~hi g (0, 2, 0))) 0.;
        at_most (name ^ ": steepest sampled gradient, against declared bound") (steepest f (points ~lo ~hi 100_000)) (f.lip *. (1. +. 1e-9)) ])
    [ ("90 degrees, convex", 90., true); ("60 degrees, convex", 60., true); ("90 degrees, concave", 90., false) ]
  @
  let (ax, ay, az), (bx, by, bz), _ = planes 90. in
  let g = G.fillet_intersection ~width:r ~name:"fillet" (Solid.plane "A" (ax, ay, az) 0.) (Solid.plane "B" (bx, by, bz) 0.) in
  let err, n, _ = radius_error g "fillet[A,B]" (-.r, -.r) in
  [ at_most ~note:(Printf.sprintf "%d points; mk36's blend is a parabola, not an arc" n) "polynomial blend of width 4, 90 degrees: radius off 4 mm, as a fraction" err 0.01 ]

let experiment = { id = 5; question = "A blend of a given radius: does asking for 4 mm give 4 mm?"; run }
