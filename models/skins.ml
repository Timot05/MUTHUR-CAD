(* Free-form skins as fitted pieces (Fit), and a solid under each: the skin on top, a box
   round and below it. Lengths in millimetres.

   - "torus": a patch of a torus (ring 25, tube 6), whose distance is known exactly, so
     the fitted field can be checked off the skin as well as on it.
   - "bezier": a bicubic Bezier patch, 40 x 40 with heights up to 9: free-form, as a NURBS
     face is. Only its points and normals are known. *)
open Mk52
module G = Graph

type skin = { name : string; surface : float -> float -> float array * float array; cells : (int * int) list;  (** coarse to fine *) depth : float;
              sides : (float * float) * (float * float) * float;  (** the box: x range, y range, bottom *) lo : float array; hi : float array }

let ring = 25. and tube = 6.

(* u across the ring (+-0.4 rad), v round the tube from 30 to 150 degrees: its outer top. *)
let torus_point u v =
  let phi = (u -. 0.5) *. 0.8 and theta = (30. +. (120. *. v)) *. Float.pi /. 180. in
  let rho = ring +. (tube *. cos theta) in
  ([| rho *. cos phi; rho *. sin phi; tube *. sin theta |], [| cos theta *. cos phi; cos theta *. sin phi; sin theta |])

let torus_distance x = Float.sqrt (((Float.sqrt ((x.(0) *. x.(0)) +. (x.(1) *. x.(1))) -. ring) ** 2.) +. (x.(2) *. x.(2))) -. tube

let heights = [| [| 0.; 3.; 2.; 0. |]; [| 2.; 7.; 8.; 3. |]; [| 3.; 9.; 6.; 2. |]; [| 0.; 2.; 3.; 1. |] |]

let bezier_point u v =
  let b t = [| (1. -. t) ** 3.; 3. *. t *. ((1. -. t) ** 2.); 3. *. t *. t *. (1. -. t); t ** 3. |] in
  let d t = [| -3. *. ((1. -. t) ** 2.); (3. *. ((1. -. t) ** 2.)) -. (6. *. t *. (1. -. t)); (6. *. t *. (1. -. t)) -. (3. *. t *. t); 3. *. t *. t |] in
  let sum f g = let s = ref 0. in for i = 0 to 3 do for j = 0 to 3 do s := !s +. (f.(i) *. g.(j) *. heights.(i).(j)) done done; !s in
  let z = sum (b u) (b v) and zu = sum (d u) (b v) and zv = sum (b u) (d v) in
  (* x = 40 u and y = 40 v exactly, so the normal is (-dz/dx, -dz/dy, 1), normalised *)
  let n = [| -.zu /. 40.; -.zv /. 40.; 1. |] in
  let l = Float.sqrt ((n.(0) *. n.(0)) +. (n.(1) *. n.(1)) +. 1.) in
  ([| 40. *. u; 40. *. v; z |], Array.map (fun c -> c /. l) n)

let torus =
  { name = "torus"; surface = torus_point; cells = [ (2, 4); (4, 8); (8, 16) ]; depth = 5.5; sides = ((21.5, 28.5), (-6., 6.), 1.);
    lo = [| 19.3; -8.2; -0.7 |]; hi = [| 30.9; 8.4; 7.6 |] }

let bezier =
  { name = "bezier"; surface = bezier_point; cells = [ (2, 2); (4, 4); (8, 8) ]; depth = 10.; sides = ((6., 34.), (6., 34.), -3.);
    lo = [| 3.1; 2.9; -5.2 |]; hi = [| 36.7; 37.3; 10.9 |] }

let all = [ torus; bezier ]

(* The skin alone, as a field: negative below it. *)
let skin ?(band = 1.5) s (nu, nv) = Fit.patch ~name:"skin" ~nu ~nv ~band ~depth:s.depth s.surface

(* The solid under the skin, inside the box. *)
let solid s cells =
  let (x0, x1), (y0, y1), z0 = s.sides in
  let top = G.feature __POS__ "skin" (fun () -> (skin s cells).graph) in
  let box =
    G.feature __POS__ "box" (fun () ->
        List.fold_left G.intersection
          (Solid.plane "+x" (1., 0., 0.) x1)
          [ Solid.plane "-x" (-1., 0., 0.) (-.x0); Solid.plane "+y" (0., 1., 0.) y1; Solid.plane "-y" (0., -1., 0.) (-.y0); Solid.plane "bottom" (0., 0., -1.) (-.z0) ])
  in
  G.intersection top box
