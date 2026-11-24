(* 7. Can a free-form face be fitted pieces behind guard planes, and how good is the
   field they give?

   Shapes, in millimetres: a patch of a torus, whose distance is known exactly, and a
   bicubic Bezier patch, free-form as a NURBS face is, known only by its points and
   normals. Each is cut into cells in (u, v), each cell fitted with one cubic (Fit), at
   three cell counts. Then the solid under each skin inside a box.
   Limits, fixed beforehand, the same at every cell count:
   - on the skin: the field within 1 um of 0 at 1,600 points of the patch;
   - beside it: within 10 um of d at d along the normal, for d up to 1 mm either way;
   - no sampled gradient within 1 mm of the skin above the declared slope bound;
   - no step across a guard, anywhere in the box, larger than the largest jump stored
     on a guard (found from pairs of points 0.05 mm apart);
   - the solid: 8 corners, 12 edges, no flag (the skin is one face: its guards are not
     edges), and pruned evaluation equal to unpruned. *)
open Mk52
open Harness

let grid n f = Array.init (n * n) (fun k -> f (0.1 +. (0.8 *. float_of_int (k mod n) /. float_of_int (n - 1))) (0.1 +. (0.8 *. float_of_int (k / n) /. float_of_int (n - 1))))

let run () =
  List.concat_map
    (fun (s : Models.Skins.skin) ->
      let on = grid 40 s.surface in
      List.concat
        (List.mapi
           (fun rank cells ->
             let nu, nv = cells in
             let name = Printf.sprintf "%s, %d x %d pieces" s.name nu nv in
             let fit = Models.Skins.skin s cells in
             let f = field fit.graph in
             let skin = worst (fun (p, _) -> Float.abs (f.value p)) on in
             let beside = worst (fun (p, n) -> List.fold_left (fun m d -> Float.max m (Float.abs (f.value (Array.mapi (fun a v -> v +. (d *. n.(a))) p) -. d))) 0. [ -1.; -0.5; -0.25; 0.25; 0.5; 1. ]) on in
             let band = Array.concat (List.map (fun d -> Array.map (fun (p, n) -> Array.mapi (fun a v -> v +. (d *. n.(a))) p) on) [ -1.; -0.5; 0.; 0.5; 1. ]) in
             (* anywhere in the box the solid fills, from its bottom to 1 mm above the skin *)
             let (x0, x1), (y0, y1), z0 = s.sides in
             let inside = Array.of_list (List.filter (fun x -> f.value x < 1.) (Array.to_list (points ~lo:[| x0; y0; z0 |] ~hi:[| x1; y1; s.hi.(2) |] 100_000))) in
             let checks =
               [ at_most (name ^ ": on the skin, mm") skin 1e-3; at_most (name ^ ": beside the skin, to 1 mm either way, mm") beside 1e-2;
                 at_most (name ^ ": steepest sampled gradient within 1 mm, against declared bound") (steepest f band) (f.lip *. (1. +. 1e-9));
                 at_most ~note:(Printf.sprintf "within 1.5 mm of the skin the guards step by at most %.2g mm" fit.skin_jump) (name ^ ": largest step across a guard in the box, against the largest stored, mm") (largest_jump f ~step:0.05 inside) (2. *. fit.worst_jump) ]
             in
             if rank <> 1 then checks
             else
               let g = Models.Skins.solid s cells in
               let p = Prog.compile g in
               let c = Prog.ctx p in
               let tree = Octree.build p c ~lo:s.lo ~hi:s.hi ~depth:6 and whole = Prog.whole p in
               let differ = worst (fun x -> Float.abs (Prog.value p c (Octree.state tree x) x -. Prog.value p c whole x)) (points ~lo:s.lo ~hi:s.hi 20_000) in
               checks
               @ [ at_most (name ^ ", solid: corners, edges, flags off 8 and 12") (fst (topology_off ~lo:s.lo ~hi:s.hi g (8, 12, 0))) 0.;
                   at_most (name ^ ", solid: pruned against unpruned value, mm") differ 0. ])
           s.cells))
    Models.Skins.all

let experiment = { id = 7; question = "Can a free-form face be fitted pieces behind guard planes, and how good is the field they give?"; run }
