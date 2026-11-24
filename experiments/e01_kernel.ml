(* 1. Do the evaluator and the pruning agree with mk36, and how fast are they?

   Shapes: the fourteen models ported from mk36, whose corners and edges mk36 has
   counted. Limits, fixed beforehand:
   - the same corners, edges and closed edges as mk36, with no flag, at three cell sizes;
   - pruned and unpruned evaluation equal at every sample;
   - extraction at mk36's cell size in a tenth of mk36's time on ten cores (mk52 runs
     on one; the ratio to mk36 on one core is noted beside it). *)
open Mk52
open Harness

let run () =
  List.concat_map
    (fun (set : Models.Sets.set) ->
      List.concat_map
        (fun (name, (v, e, closed), (ten, one)) ->
          let g = set.graph name in
          let extract depth = Extract.extract ~depth ~lo:set.lo ~hi:set.hi g in
          let counts =
            List.map
              (fun depth ->
                let t = extract depth in
                let nc = List.length (List.filter (fun (x : Extract.edge) -> x.closed) t.edges) in
                let off = abs (List.length t.vertices - v) + abs (List.length t.edges - e) + abs (nc - closed) + List.length t.flags in
                at_most (Printf.sprintf "%s: corners, edges, flags off mk36's, depth %d" name depth) (float_of_int off) 0.)
              [ 5; 6; 7 ]
          in
          let p = Prog.compile g in
          let c = Prog.ctx p in
          let tree = Octree.build p c ~lo:set.lo ~hi:set.hi ~depth:6 and whole = Prog.whole p in
          let differ = worst (fun x -> Float.abs (Prog.value p c (Octree.state tree x) x -. Prog.value p c whole x)) (points ~lo:set.lo ~hi:set.hi 20_000) in
          (* the best of five: single runs vary on a busy machine *)
          let seconds = List.fold_left (fun m _ -> let t = extract 6 in Float.min m (fst t.seconds +. snd t.seconds)) infinity [ 1; 2; 3; 4; 5 ] in
          counts
          @ [ at_most (name ^ ": pruned against unpruned value") differ 0.;
              at_most ~note:(Printf.sprintf "%.0fx mk36 on ten cores, %.0fx on one" (ten /. seconds) (one /. seconds)) (name ^ ": seconds, against a tenth of mk36's") seconds (ten /. 10.) ])
        set.expected)
    Models.Sets.all

let experiment = { id = 1; question = "Do the evaluator and the pruning agree with mk36, and how fast are they?"; run }
