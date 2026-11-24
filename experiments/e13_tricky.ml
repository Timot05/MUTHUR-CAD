(* 13. Small shapes that are hard for topology extraction: what does the kernel do?

   Shapes: models/tricky.ml. Two or three primitives each, chosen for what goes wrong:
   curves that cross themselves, surfaces that only touch, grazing crossings, features
   smaller than a cell, faces lying on cell walls or on each other. The exact solid's
   corners and edges are known for each.
   Limits, fixed beforehand:
   - each shape gives its known corners, edges and closed edges with no flag, at 64 cells
     across (depth 6);
   - a wrong answer is never given without a flag, at any of three cell sizes. The
     second is the one a sampled kernel must not fail: a flag says "look here", a silent
     wrong answer says nothing. *)
open Mk52
open Harness

type outcome = Right | Flagged | Silent

let run () =
  let silent = ref [] in
  let checks =
    List.map
      (fun (c : Models.Tricky.case) ->
        let g = c.graph () in
        let at depth =
          let t = Extract.extract ~depth ~lo:c.lo ~hi:c.hi g in
          let got = (List.length t.vertices, List.length t.edges, List.length (List.filter (fun (e : Extract.edge) -> e.closed) t.edges)) in
          let flags = List.length t.flags in
          let outcome = if got = c.expected && flags = 0 then Right else if flags > 0 then Flagged else Silent in
          if outcome = Silent then silent := Printf.sprintf "%s at depth %d" c.name depth :: !silent;
          let v, e, k = got in
          (outcome, Printf.sprintf "%d: %d/%d/%d%s" depth v e k (if flags > 0 then Printf.sprintf " +%d flags" flags else ""), fst t.seconds +. snd t.seconds)
        in
        let results = List.map at [ 5; 6; 7 ] in
        let o6, _, _ = List.nth results 1 in
        let v, e, k = c.expected in
        at_most
          ~note:(Printf.sprintf "%s; expected %d/%d/%d; got %s" c.kind v e k (String.concat ", " (List.map (fun (_, s, _) -> s) results)))
          (c.name ^ ": off its known corners and edges, or flagged, at depth 6")
          (match o6 with Right -> 0. | _ -> 1.)
          0.)
      Models.Tricky.cases
  in
  checks @ [ at_most ~note:(String.concat "; " (List.rev !silent)) "wrong answers given with no flag, over all shapes and three cell sizes" (float_of_int (List.length !silent)) 0. ]

let experiment = { id = 13; question = "Small shapes that are hard for topology extraction: what does the kernel do?"; run }
