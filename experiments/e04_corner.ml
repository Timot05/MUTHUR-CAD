(* 4. One corner where edges of mixed convexity meet.

   Shape, in millimetres: a slab with a post standing on it (a union, as mk36's l-block).
   Where the post meets the slab's top, each of four corners joins one convex edge (the
   post's upright edge) and two concave ones. All 16 corners are known.
   Limits, fixed beforehand:
   - 16 corners, 24 edges, no flag, at three cell sizes;
   - every known corner found within 1 um, and the four mixed ones with three edges. *)
open Mk52
open Harness
module G = Graph

let lo = [| -47.; -45.; -26. |] and hi = [| 44.; 46.; 37. |]

let box p (x0, x1) (y0, y1) (z0, z1) =
  List.fold_left G.intersection
    (Solid.plane (p ^ "+x") (1., 0., 0.) x1)
    [ Solid.plane (p ^ "-x") (-1., 0., 0.) (-.x0); Solid.plane (p ^ "+y") (0., 1., 0.) y1; Solid.plane (p ^ "-y") (0., -1., 0.) (-.y0);
      Solid.plane (p ^ "+z") (0., 0., 1.) z1; Solid.plane (p ^ "-z") (0., 0., -1.) (-.z0) ]

(* The post is buried 10 mm into the slab, so no face of one lies on a face of the other. *)
let shape () = G.union (box "slab" (-40., 40.) (-40., 40.) (-20., 0.)) (box "post" (-25., 5.) (-15., 15.) (-10., 30.))

let grid xs ys zs = List.concat_map (fun x -> List.concat_map (fun y -> List.map (fun z -> [| x; y; z |]) zs) ys) xs
let mixed = grid [ -25.; 5. ] [ -15.; 15. ] [ 0. ]
let corners = grid [ -40.; 40. ] [ -40.; 40. ] [ -20.; 0. ] @ grid [ -25.; 5. ] [ -15.; 15. ] [ 30. ] @ mixed
let dist a b = norm (Array.map2 ( -. ) a b)

let run () =
  let g = shape () in
  let counts = List.map (fun depth -> at_most (Printf.sprintf "corners, edges, flags off 16 and 24, depth %d" depth) (fst (topology_off ~depth ~lo ~hi g (16, 24, 0))) 0.) [ 5; 6; 7 ] in
  let _, t = topology_off ~lo ~hi g (16, 24, 0) in
  let nearest c = List.fold_left (fun (best : Extract.vertex option) (v : Extract.vertex) -> match best with Some b when dist b.at c <= dist v.at c -> best | _ -> Some v) None t.vertices in
  let miss c = match nearest c with Some v -> dist v.at c | None -> infinity in
  let degree_off = List.fold_left (fun n c -> match nearest c with Some v when v.degree = 3 -> n | _ -> n + 1) 0 mixed in
  counts
  @ [ at_most "known corners: furthest from a found corner, mm" (List.fold_left (fun m c -> Float.max m (miss c)) 0. corners) 1e-3;
      at_most "mixed corners without exactly three edges" (float_of_int degree_off) 0. ]

let experiment = { id = 4; question = "One corner where edges of mixed convexity meet: is it found, and where?"; run }
