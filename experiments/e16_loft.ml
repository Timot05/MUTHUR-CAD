(* 16. Lofts: is the field between two sections the loft between them, and how far off is
   it where it is not exact?

   Shapes whose loft is known without the field, each stressing one thing:
   - a cone: a circle lofted to a smaller one about the same centre; exactly a cone;
   - a frustum: a square lofted to a smaller square, sides kept parallel; its four sides
     are exactly planes, and its four slanted edges straight lines. Drawn alike, the
     two squares are followed span by span; drawn with their sides named differently
     they are not, and are blended;
   - a ring: a circle with a hole lofted to another, both shrinking; two cones;
   - three sections: circles of radius 10, 6 and 9; two cones meeting at a crease;
   - a square to a circle: no exact answer here, but its ends are the two sections.
   Limits, fixed beforehand:
   - the field zero on the exact surface to 1e-9 mm, wherever it is meant to be exact;
   - no wrong sign further than 1e-3 mm from the exact surface;
   - the sampled slope within the declared bound;
   - corners and edges as counted, no flag: the cone 2 closed edges; the frustum 8
     corners and 12 edges; the ring 4 closed edges; three sections 2 closed edges (the
     crease is no edge: one face on both sides); the square to the circle 8 corners and
     12 edges;
   - the frustum's slanted edges within 0.05 mm of the straight ones, the tolerance a
     check holds a model to, followed and blended. The blend is not expected to meet it
     there (lib/sketch.ml): this measures by how much. *)
open Mk52
open Harness
module S = Sketch

let circle name r = S.circle name (0., 0.) r
let square half = S.polygon [ (-.half, -.half); (half, -.half); (half, half); (-.half, half) ]

(* A loft with its field, and the most the field is from 0 on points of the exact surface. *)
let on_exact f pts = worst (fun x -> Float.abs (f.value x)) pts

let run () =
  let r = Random.State.make [| 16 |] in
  let rnd a b = a +. Random.State.float r (b -. a) in
  (* the cone: radius 10 at z 0 to 4 at z 30 *)
  let cone = S.loft [ S.section 0. (circle "rim" 10.); S.section 30. (circle "rim" 4.) ] in
  let fc = field cone in
  let radius z = 10. +. ((4. -. 10.) *. z /. 30.) in
  let cone_pts = Array.init 2000 (fun _ -> let z = rnd 0. 30. and t = rnd 0. (2. *. Float.pi) in [| radius z *. cos t; radius z *. sin t; z |]) in
  let lo = [| -12.3; -12.1; -2.2 |] and hi = [| 12.1; 12.4; 32.3 |] in
  let samples = points ~lo ~hi 30_000 in
  let cone_inside x = x.(2) >= 0. && x.(2) <= 30. && Float.hypot x.(0) x.(1) <= radius x.(2) in
  let cone_far x = let z = x.(2) and rho = Float.hypot x.(0) x.(1) in Float.min (Float.min (Float.abs z) (Float.abs (z -. 30.))) (Float.abs (rho -. radius (Float.max 0. (Float.min 30. z))) *. cos (atan (6. /. 30.))) > 1e-3 in
  let cone_wrong = wrong_signs fc ~inside:cone_inside (Array.of_list (List.filter cone_far (Array.to_list samples))) in
  let cone_topo, _ = topology_off ~lo ~hi cone (0, 2, 2) in
  (* the frustum: a square of half side 10 at z 0 to one of 5 at z 10 *)
  let frustum = S.loft [ S.section 0. (square 10.); S.section 10. (square 5.) ] in
  let ff = field frustum in
  let half z = 10. -. (0.5 *. z) in
  (* points of the four slanted sides, at least 1 mm from their edges *)
  let side_pts = Array.init 2000 (fun k -> let z = rnd 0. 10. in let h = half z in let t = rnd (-.h +. 1.) (h -. 1.) in match k mod 4 with 0 -> [| h; t; z |] | 1 -> [| -.h; t; z |] | 2 -> [| t; h; z |] | _ -> [| t; -.h; z |]) in
  (* the slanted edges: how far the loft's surface is from each point of them *)
  let edge_off f =
    worst
      (fun x -> match on_surface f x with Some y -> norm (Array.map2 ( -. ) x y) | None -> infinity)
      (Array.init 400 (fun k -> let z = 10. *. float_of_int (k / 4) /. 99. in let h = half z in let sx = if k mod 2 = 0 then 1. else -1. and sy = if k mod 4 < 2 then 1. else -1. in [| sx *. h; sy *. h; z |]))
  in
  (* the same frustum, its top square's sides named otherwise: not followed, blended *)
  let blended = field (S.loft [ S.section 0. (square 10.); S.section 10. (S.polygon ~name:(Printf.sprintf "edge %d") [ (-5., -5.); (5., -5.); (5., 5.); (-5., 5.) ]) ]) in
  let flo = [| -12.3; -12.1; -2.2 |] and fhi = [| 12.1; 12.4; 12.3 |] in
  let frustum_topo, _ = topology_off ~lo:flo ~hi:fhi frustum (8, 12, 0) in
  (* the ring: outer 10 to 8, hole 4 to 5, over 15 *)
  let ring = S.loft [ S.section ~holes:[ circle "bore" 4. ] 0. (circle "rim" 10.); S.section ~holes:[ circle "bore" 5. ] 15. (circle "rim" 8.) ] in
  let fr = field ring in
  let ring_pts = Array.init 2000 (fun k -> let z = rnd 0. 15. and t = rnd 0. (2. *. Float.pi) in let rho = if k mod 2 = 0 then 10. -. (2. *. z /. 15.) else 4. +. (z /. 15.) in [| rho *. cos t; rho *. sin t; z |]) in
  let ring_topo, _ = topology_off ~lo:[| -12.3; -12.1; -2.2 |] ~hi:[| 12.1; 12.4; 17.3 |] ring (0, 4, 4) in
  (* three sections: 10, 6, 9 at 0, 10, 25 *)
  let three = S.loft [ S.section 0. (circle "rim" 10.); S.section 10. (circle "rim" 6.); S.section 25. (circle "rim" 9.) ] in
  let ft = field three in
  let r3 z = if z <= 10. then 10. -. (0.4 *. z) else 6. +. (3. *. (z -. 10.) /. 15.) in
  let three_pts = Array.init 2000 (fun _ -> let z = rnd 0. 25. and t = rnd 0. (2. *. Float.pi) in [| r3 z *. cos t; r3 z *. sin t; z |]) in
  let three_topo, _ = topology_off ~lo:[| -12.3; -12.1; -2.2 |] ~hi:[| 12.1; 12.4; 27.3 |] three (0, 2, 2) in
  (* a square to a circle: its ends are its sections *)
  let morph = S.loft [ S.section 0. (square 10.); S.section 20. (circle "rim" 6.) ] in
  let fm = field morph in
  let ends = Array.init 1000 (fun k -> if k mod 2 = 0 then let t = rnd (-10.) 10. in (match k mod 8 with 0 -> [| 10.; t; 0. |] | 2 -> [| -10.; t; 0. |] | 4 -> [| t; 10.; 0. |] | _ -> [| t; -10.; 0. |]) else let t = rnd 0. (2. *. Float.pi) in [| 6. *. cos t; 6. *. sin t; 20. |]) in
  let morph_topo, _ = topology_off ~lo:[| -12.3; -12.1; -2.2 |] ~hi:[| 12.1; 12.4; 22.3 |] morph (8, 12, 0) in
  let slope f lo hi = let pts = points ~lo ~hi 20_000 in steepest f pts -. f.lip in
  [ at_most "cone: |field| on the exact cone, mm" (on_exact fc cone_pts) 1e-9;
    at_most "cone: wrong signs further than 1 um from the surface" cone_wrong 0.;
    at_most "cone: corners, edges, flags off 2 closed" cone_topo 0.;
    at_most "frustum: |field| on the exact sides, 1 mm or more from their edges" (on_exact ff side_pts) 1e-9;
    at_most "frustum: corners, edges, flags off 8 and 12" frustum_topo 0.;
    at_most "frustum: slanted edges, furthest from the straight ones, mm" (edge_off ff) 0.05;
    at_most "frustum blended: slanted edges, furthest from the straight ones, mm" (edge_off blended) 0.05;
    at_most "ring: |field| on its two exact cones, mm" (on_exact fr ring_pts) 1e-9;
    at_most "ring: corners, edges, flags off 4 closed" ring_topo 0.;
    at_most "three sections: |field| on the two exact cones, mm" (on_exact ft three_pts) 1e-9;
    at_most "three sections: corners, edges, flags off 2 closed (no edge at the crease)" three_topo 0.;
    at_most "square to circle: |field| on the two sections, mm" (on_exact fm ends) 1e-9;
    at_most "square to circle: corners, edges, flags off 8 and 12" morph_topo 0.;
    at_most "every loft: steepest sampled gradient above the declared bound"
      (List.fold_left Float.max neg_infinity [ slope fc lo hi; slope ff flo fhi; slope fr lo [| 12.1; 12.4; 17.3 |]; slope ft lo [| 12.1; 12.4; 27.3 |]; slope fm lo [| 12.1; 12.4; 22.3 |] ]) 0. ]

let experiment = { id = 16; question = "Lofts: is the field between two sections the loft between them, and how far off is it where it is not exact?"; run }
