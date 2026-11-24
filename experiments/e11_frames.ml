(* 11. Frames: is a shape placed by a forward chain where the chain says?

   Forward chains only: each frame is given in the one before. Mates are not built, so
   nothing here is solved for.
   Shapes, in millimetres: a pin (a cylinder of radius 3) carried through a chain of two
   moves and two turns, whose axis and so whose distance are known; and the slab and post
   of experiment 4, turned off every axis.
   Limits, fixed beforehand:
   - the pin's field equal to the exact distance to 1e-9 mm, its slope bound kept;
   - a chain of frames equal to placing through each frame in turn, and to building the
     shape inside the frames nested, to 1e-9 mm;
   - the turned slab and post: 16 corners, 24 edges, no flag, each known corner within
     1 um of where the frame puts it. *)
open Mk52
open Harness

let lo = [| -74.; -71.; -68. |] and hi = [| 77.; 73.; 79. |]
let pts = points ~lo ~hi 100_000
let dist a b = norm (Array.map2 ( -. ) a b)

let run () =
  let links = [ Frame.translate (10., 5., -4.); Frame.rotate (0., 0., 1.) 30.; Frame.translate (7., 0., 0.); Frame.rotate (1., 0., 0.) 40. ] in
  let frame = Frame.chain links in
  let pin = Graph.place frame (Solid.cylinder "pin" (0., 0.) 3.) in
  let a = Frame.apply frame [| 0.; 0.; 0. |] and d = Frame.direction frame [| 0.; 0.; 1. |] in
  let exact x =
    let v = Array.map2 ( -. ) x a in
    let along = (v.(0) *. d.(0)) +. (v.(1) *. d.(1)) +. (v.(2) *. d.(2)) in
    norm (Array.mapi (fun i c -> c -. (along *. d.(i))) v) -. 3.
  in
  let f = field pin in
  let stepwise = field (List.fold_right Graph.place links (Solid.cylinder "pin" (0., 0.) 3.)) in
  (* built inside the frames, nested, instead of moved afterwards *)
  let nested = field (List.fold_right (fun link build () -> Graph.within link build) links (fun () -> Solid.cylinder "pin" (0., 0.) 3.) ()) in
  let tilt = Frame.chain [ Frame.translate (6., -4., 9.); Frame.rotate (1., 2., 3.) 37. ] in
  let block = Graph.place tilt (E04_corner.shape ()) in
  let off, t = topology_off ~lo ~hi block (16, 24, 0) in
  let miss c = List.fold_left (fun m (v : Extract.vertex) -> Float.min m (dist v.at (Frame.apply tilt c))) infinity t.vertices in
  [ at_most "pin: value against exact distance, mm" (value_error f ~exact pts) 1e-9;
    at_most "pin: wrong signs" (wrong_signs f ~inside:(fun x -> exact x <= 0.) pts) 0.;
    at_most "pin: steepest sampled gradient, against declared bound" (steepest f pts) (f.lip *. (1. +. 1e-9));
    at_most "chain against placing frame by frame, mm" (worst (fun x -> Float.abs (f.value x -. stepwise.value x)) pts) 1e-9;
    at_most "chain against building inside nested frames, mm" (worst (fun x -> Float.abs (f.value x -. nested.value x)) pts) 1e-9;
    at_most "turned slab and post: corners, edges, flags off 16 and 24" off 0.;
    at_most "turned slab and post: known corners, furthest from a found corner, mm" (List.fold_left (fun m c -> Float.max m (miss c)) 0. E04_corner.corners) 1e-3 ]

let experiment = { id = 11; question = "Frames: is a shape placed by a forward chain where the chain says?"; run }
