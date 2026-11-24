(* dune exec bench/bench.exe -- [model ...]
   Extraction of every model (or the named sets or models), to compare with mk36's
   `GRID=64 experiments/sampled/run.exe`. DEPTH sets the levels (default 6);
   EDGES=1 prints every edge. Then the raw speed of point evaluation on the cube. *)
open Mk52

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let depth = match Sys.getenv_opt "DEPTH" with Some d -> int_of_string d | None -> 6 in
  List.iter
    (fun (set : Models.Sets.set) ->
      List.iter
        (fun (name, _, _) ->
          if args = [] || List.mem name args || List.mem set.name args then begin
            let g = set.graph name in
            let t = Extract.extract ~depth ~lo:set.lo ~hi:set.hi g in
            let closed = List.length (List.filter (fun (e : Extract.edge) -> e.closed) t.edges) in
            Printf.printf "RESULT %s V=%d E=%d (closed %d) flags=%d nodes=%d cells=%d edge-cells=%d traced=%d samples=%d seconds=%.4f (prune %.4f, walk %.4f)\n%!"
              name (List.length t.vertices) (List.length t.edges) closed (List.length t.flags) t.nodes t.cells t.edge_cells t.traced t.samples
              (fst t.seconds +. snd t.seconds) (fst t.seconds) (snd t.seconds);
            if Sys.getenv_opt "EDGES" <> None then begin
              List.iteri
                (fun k (v : Extract.vertex) -> Printf.printf "  v%d (%.4f, %.4f, %.4f) degree %d: %s\n" k v.at.(0) v.at.(1) v.at.(2) v.degree (String.concat " " v.faces))
                t.vertices;
              List.iter
                (fun (e : Extract.edge) ->
                  Printf.printf "  edge %s|%s: %s\n" (fst e.between) (snd e.between)
                    (if e.closed then "closed" else String.concat "-" (List.map (Printf.sprintf "v%d") e.ends)))
                t.edges
            end;
            List.iteri (fun i (f : Extract.flag) -> if i < 6 then Printf.printf "  flag (%.4f, %.4f, %.4f): %s\n" f.where.(0) f.where.(1) f.where.(2) f.why) t.flags
          end)
        set.expected)
    Models.Sets.all;
  if args = [] then begin
    let p = Prog.compile (Models.Corners.graph "cube") in
    let c = Prog.ctx p and whole = Prog.whole p in
    let n = 64 and sum = ref 0. and x = [| 0.; 0.; 0. |] in
    let lo = Models.Corners.lo and hi = Models.Corners.hi in
    let run label f =
      (* the best of five runs: single runs vary by a third on a busy machine *)
      let best = ref infinity in
      for _ = 1 to 5 do
        let t0 = Unix.gettimeofday () in
        for i = 0 to n do
          for j = 0 to n do
            for k = 0 to n do
              x.(0) <- lo.(0) +. ((hi.(0) -. lo.(0)) *. float_of_int i /. float_of_int n);
              x.(1) <- lo.(1) +. ((hi.(1) -. lo.(1)) *. float_of_int j /. float_of_int n);
              x.(2) <- lo.(2) +. ((hi.(2) -. lo.(2)) *. float_of_int k /. float_of_int n);
              sum := !sum +. f x
            done
          done
        done;
        best := Float.min !best (Unix.gettimeofday () -. t0)
      done;
      let m = (n + 1) * (n + 1) * (n + 1) in
      Printf.printf "EVAL cube, %s: %d points in %.4f s = %.1f million per second\n%!" label m !best (float_of_int m /. !best /. 1e6)
    in
    run "value only, whole graph" (fun x -> Prog.value p c whole x);
    run "value, gradient and face, whole graph" (fun x -> (Prog.eval p c whole x).d.v);
    let tree = Octree.build p c ~lo ~hi ~depth:6 in
    run "value only, pruned" (fun x -> Prog.value p c (Octree.state tree x) x);
    ignore !sum
  end
