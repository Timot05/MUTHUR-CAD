(* 2. Is each primitive a distance near its surface, and does every node keep its
   declared slope bound?

   Shapes: a plane, a sphere, a cylinder and a torus, whose distances are known in closed
   form, then each operation applied to them. Lengths in millimetres, in a 100 mm box.
   Limits, fixed beforehand:
   - the sign is right at every sample (against the exact solid, where one is known);
   - the value is the exact distance to 1e-9 mm (primitives and their offsets);
   - the gradient's length is within 1 % of 1 inside +-2 mm of the surface (distances);
   - no sampled gradient, and no difference between two points 0.05 mm apart, exceeds
     the declared slope bound (every shape). The second sees a value that steps across
     a guard, which a gradient cannot.
   Last, a bound declared too low on purpose must be caught. *)
open Mk52
open Harness
module G = Graph

let lo = [| -50.; -50.; -50. |] and hi = [| 50.; 50.; 50. |]
let pts = points ~lo ~hi 200_000
let hyp a b = Float.sqrt ((a *. a) +. (b *. b))

(* name, graph, exact signed distance *)
let sphere_d x = norm [| x.(0) -. 5.; x.(1) +. 3.; x.(2) -. 2. |] -. 20.
let cylinder_d x = hyp (x.(0) +. 8.) (x.(1) -. 4.) -. 12.
let torus_d x = hyp (hyp x.(0) x.(1) -. 25.) x.(2) -. 6.
let plane_d x = (((2. *. x.(0)) -. x.(1) +. (2. *. x.(2))) /. 3.) -. 4.
let sphere () = Solid.sphere "sphere" (5., -3., 2.) 20.
let cylinder () = Solid.cylinder "cylinder" (-8., 4.) 12.
let torus () = Solid.torus "torus" ~ring:25. ~tube:6.
let plane () = Solid.plane "plane" (2., -1., 2.) 12.

let slope name (f : field) =
  [ at_most (name ^ ": steepest sampled gradient, against declared bound") (steepest f pts) (f.lip *. (1. +. 1e-9));
    at_most (name ^ ": steepest step over 0.05 mm, against declared bound") (steepest_step f ~step:0.05 pts) (f.lip *. (1. +. 1e-9)) ]

let distance name g exact =
  let f = field g in
  [ at_most (name ^ ": wrong signs") (wrong_signs f ~inside:(fun x -> exact x <= 0.) pts) 0.;
    at_most (name ^ ": value against exact distance, mm") (value_error f ~exact pts) 1e-9;
    at_most (name ^ ": | |grad| - 1 | within 2 mm of the surface") (slope_error f ~band:2. pts) 0.01 ]
  @ slope name f

(* A solid known exactly but whose field is not its distance everywhere. *)
let solid name g inside =
  let f = field g in
  at_most (name ^ ": wrong signs") (wrong_signs f ~inside pts) 0. :: slope name f

let run () =
  let s = sphere () and cy = cylinder () and t = torus () and pl = plane () in
  let fillet = G.fillet ~width:3. ~name:"fillet" s cy in
  let union_d x = Float.min (sphere_d x) (cylinder_d x) in
  (* outside its band the fillet is the union, exactly *)
  let fillet_f = field fillet in
  let off_band = worst (fun x -> if Float.abs (sphere_d x -. cylinder_d x) >= 3. then Float.abs (fillet_f.value x -. union_d x) else 0.) pts in
  (* mk36's hole, (r^2 - d^2) / 2r: not a distance. Its slope is d / r, at most 82.7 / 12
     in this box. Declared right, then declared as 1 as if it were a distance. *)
  let open Expr.Infix in
  let hole = (c 144. - (Expr.square (Expr.x + c 8.) + Expr.square (Expr.y - c 4.))) / c 24. in
  let wrong = field (G.piece ~lip:1. "hole" hole) in
  distance "plane" pl plane_d @ distance "sphere" s sphere_d @ distance "cylinder" cy cylinder_d @ distance "torus" t torus_d
  @ distance "sphere offset 2 mm" (G.offset 2. s) (fun x -> sphere_d x -. 2.)
  @ distance "sphere complement" (G.complement s) (fun x -> -.sphere_d x)
  @ solid "union" (G.union s cy) (fun x -> union_d x <= 0.)
  @ solid "intersection" (G.intersection s cy) (fun x -> Float.max (sphere_d x) (cylinder_d x) <= 0.)
  @ solid "subtract" (G.subtract s cy) (fun x -> Float.max (sphere_d x) (-.cylinder_d x) <= 0.)
  @ solid "union offset 2 mm" (G.offset 2. (G.union s cy)) (fun x -> union_d x <= 2.)
  @ solid "shell 1.5 mm" (G.shell ~thickness:1.5 s) (fun x -> sphere_d x <= 0. && sphere_d x >= -1.5)
  @ solid "plane cut" (G.intersection t pl) (fun x -> Float.max (torus_d x) (plane_d x) <= 0.)
  @ slope "fillet 3 mm" fillet_f
  @ [ at_most "fillet 3 mm: off the union outside its band, mm" off_band 1e-9 ]
  @ slope "mk36's hole, declared 6.9" (field (G.piece ~lip:6.9 "hole" hole))
  @ [ above "mk36's hole, declared 1: the wrong bound is caught" (steepest wrong pts) wrong.lip ]

let experiment = { id = 2; question = "Is each primitive a distance near its surface, and does every node keep its declared slope bound?"; run }
