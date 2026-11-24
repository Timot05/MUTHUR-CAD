(* 6. Offsets and shells: is the wall as thick as asked?

   Shapes, in millimetres: a sphere of radius 20 and a box of 36 x 28 x 20, each moved
   out by 2, in by 2, and shelled 1.5 thick. The exact distance to both is known.
   Limits, fixed beforehand:
   - every point of an offset surface 2 mm (to 1 um) from the original, by true distance;
   - the wall, measured along the normal from the outer surface, 1.5 mm thick to 1 um
     (on the box, at points further than 3 mm from an edge, where a normal crosses one
     wall only). *)
open Mk52
open Harness
module G = Graph

let lo = [| -31.; -29.; -27. |] and hi = [| 33.; 35.; 30. |]
let pts = points ~lo ~hi 40_000
let half = [| 18.; 14.; 10. |]
let sphere_d x = norm [| x.(0) -. 2.; x.(1) +. 1.; x.(2) -. 1. |] -. 20.

(* The true distance to the box, rounded outside its edges and corners. *)
let box_d x =
  let q = Array.mapi (fun a v -> Float.abs v -. half.(a)) x in
  norm (Array.map (Float.max 0.) q) +. Float.min 0. (Float.max q.(0) (Float.max q.(1) q.(2)))

let sphere () = Solid.sphere "sphere" (2., -1., 1.) 20.

let box () =
  List.fold_left G.intersection
    (Solid.plane "+x" (1., 0., 0.) half.(0))
    [ Solid.plane "-x" (-1., 0., 0.) half.(0); Solid.plane "+y" (0., 1., 0.) half.(1); Solid.plane "-y" (0., -1., 0.) half.(1);
      Solid.plane "+z" (0., 0., 1.) half.(2); Solid.plane "-z" (0., 0., -1.) half.(2) ]

(* How far the points of the surface of [g] are from the distance [by] to the original. *)
let offset_error g exact by =
  let f = field g in
  worst (fun x -> Float.abs (exact x -. by)) (surface_points f ~lo ~hi pts)

(* The wall's thickness along the inward normal from each outer surface point kept by
   [keep]: the first place the field turns positive again. The largest error against t. *)
let wall_error g outer t keep =
  let f = field g in
  let surface = List.filter (fun x -> Float.abs (outer x) < 1e-6 && keep x) (Array.to_list (surface_points f ~lo ~hi pts)) in
  let thickness x =
    let n = (f.dual x).g in
    let l = norm n in
    let at s = f.value (Array.mapi (fun a v -> v -. (s *. n.(a) /. l)) x) in
    let rec step s = if s > 4. *. t then infinity else if at (s +. 0.01) > 0. then bisect s (s +. 0.01) 60 else step (s +. 0.01)
    and bisect a b k = if k = 0 then 0.5 *. (a +. b) else let m = 0.5 *. (a +. b) in if at m > 0. then bisect a m (k - 1) else bisect m b (k - 1) in
    step 1e-4
  in
  (List.fold_left (fun m x -> Float.max m (Float.abs (thickness x -. t))) 0. surface, List.length surface)

let run () =
  (* further than 3 mm from every edge: at most one coordinate within 3 mm of its face *)
  let clear x = Array.fold_left (fun n q -> if q > -3. then n + 1 else n) 0 (Array.mapi (fun a v -> Float.abs v -. half.(a)) x) <= 1 in
  let wall name g outer keep =
    let e, n = wall_error g outer 1.5 keep in
    at_most ~note:(Printf.sprintf "%d points" n) (name ^ ": wall off 1.5 mm along the normal, mm") (if n < 100 then infinity else e) 1e-3
  in
  [ at_most "sphere moved out 2 mm: surface off 2 mm from the sphere, mm" (offset_error (G.offset 2. (sphere ())) sphere_d 2.) 1e-3;
    at_most "sphere moved in 2 mm: surface off 2 mm from the sphere, mm" (offset_error (G.offset (-2.) (sphere ())) sphere_d (-2.)) 1e-3;
    wall "sphere shell" (G.shell ~thickness:1.5 (sphere ())) sphere_d (fun _ -> true);
    at_most ~note:"edges stay sharp: a true offset rounds them" "box moved out 2 mm: surface off 2 mm from the box, mm" (offset_error (G.offset 2. (box ())) box_d 2.) 1e-3;
    at_most "box moved in 2 mm: surface off 2 mm from the box, mm" (offset_error (G.offset (-2.) (box ())) box_d (-2.)) 1e-3;
    wall "box shell, clear of edges" (G.shell ~thickness:1.5 (box ())) box_d clear ]

let experiment = { id = 6; question = "Offsets and shells: is the wall as thick as asked?"; run }
