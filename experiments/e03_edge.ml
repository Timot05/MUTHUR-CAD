(* 3. One edge between two pieces: is it found, and what does the field mean beside it?

   Shapes, in millimetres: two planes 37 degrees apart, then a cylinder and a plane square
   to it; each joined as an intersection (a convex edge) and as a union (a concave one).
   The edge and the exact distance to each solid are known in closed form.
   Limits, fixed beforehand:
   - one edge, no corner, no flag;
   - every traced point within 1 um of both surfaces;
   - the field equal to the true distance within 1 um, 1.5 to 2.5 mm from the surface,
     outside and inside. This is what an offset of 2 mm reads. *)
open Mk52
open Harness
module G = Graph

let lo = [| -31.; -29.; -27. |] and hi = [| 33.; 35.; 30. |]
let pts = points ~lo ~hi 200_000

(* The distance to the intersection of two solids with distances a and b whose gradients
   make the cosine c, exact for two planes and for a cylinder with a plane square to it. *)
let wedge c a b =
  if a <= 0. && b <= 0. then Float.max a b
  else if a > 0. && b -. (c *. a) <= 0. then a
  else if b > 0. && a -. (c *. b) <= 0. then b
  else Float.sqrt ((a *. a) +. (b *. b) -. (2. *. c *. a *. b)) /. Float.sqrt (1. -. (c *. c))

let cases =
  let a x = x.(2) -. 3. and b x = (0.6 *. x.(0)) +. (0.8 *. x.(2)) -. 6. in
  let cy x = Float.sqrt (((x.(0) +. 8.) ** 2.) +. ((x.(1) -. 4.) ** 2.)) -. 12. and pl x = x.(2) -. 7. in
  let planes () = (Solid.plane "A" (0., 0., 1.) 3., Solid.plane "B" (0.6, 0., 0.8) 6.) in
  let round () = (Solid.cylinder "C" (-8., 4.) 12., Solid.plane "P" (0., 0., 1.) 7.) in
  [ ("two planes, convex", (let p, q = planes () in G.intersection p q), a, b, (fun x -> wedge 0.8 (a x) (b x)), 0);
    ("two planes, concave", (let p, q = planes () in G.union p q), a, b, (fun x -> -.wedge 0.8 (-.a x) (-.b x)), 0);
    ("cylinder and plane, convex", (let p, q = round () in G.intersection p q), cy, pl, (fun x -> wedge 0. (cy x) (pl x)), 1);
    ("cylinder and plane, concave", (let p, q = round () in G.union p q), cy, pl, (fun x -> -.wedge 0. (-.cy x) (-.pl x)), 1) ]

let run () =
  List.concat_map
    (fun (name, g, a, b, exact, closed) ->
      let off, t = topology_off ~lo ~hi g (0, 1, closed) in
      let traced = List.concat_map (fun (e : Extract.edge) -> List.concat e.pieces) t.edges in
      let away = List.fold_left (fun m x -> Float.max m (Float.max (Float.abs (a x)) (Float.abs (b x)))) 0. traced in
      let f = field g in
      let near d0 = worst (fun x -> let d = exact x in if Float.abs (d -. d0) <= 0.5 then Float.abs (f.value x -. d) else 0.) pts in
      [ at_most (name ^ ": corners, edges, flags off one edge") off 0.;
        at_most ~note:(Printf.sprintf "%d traced points" (List.length traced)) (name ^ ": traced points off the two surfaces, mm") away 1e-3;
        at_most (name ^ ": field against true distance 2 mm outside, mm") (near 2.) 1e-3;
        at_most (name ^ ": field against true distance 2 mm inside, mm") (near (-2.)) 1e-3 ])
    cases

let experiment = { id = 3; question = "One edge between two pieces: is it found, and what does the field mean beside it?"; run }
