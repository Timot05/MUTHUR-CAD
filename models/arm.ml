(* An arm of three links on a base, to show frames chained forwards. Lengths in
   millimetres.

   Every link is the same shape, built in its own coordinates: a bar from its joint at
   the origin to the next joint 30 mm along x, with a pin through that joint. A link's
   frame is given in the link before it (move to its far joint, step up so the bars
   overlap like a hinge, turn), and the next link is built inside it. So the chain is
   the nesting: change one angle and every link after it follows. Nothing is solved
   for. *)
open Mk52
module G = Graph

let reach = 30.
let all = function [] -> invalid_arg "all" | f :: rest -> List.fold_left G.intersection f rest

(* Where each link's joint is in the link before, and the turn there. *)
let joints =
  [| [ Frame.translate (0., 0., 14.); Frame.rotate (0., 0., 1.) 25. ];
     [ Frame.translate (reach, 0., 5.); Frame.rotate (0., 0., 1.) (-55.) ];
     [ Frame.translate (reach, 0., 5.); Frame.rotate (0., 0., 1.) 20.; Frame.rotate (0., 1., 0.) (-30.) ] |]

(* Link k and every link after it, in link k's frame. *)
let rec link k =
  G.feature ~frame:(Frame.chain joints.(k)) __POS__ (Printf.sprintf "link %d" (k + 1)) (fun () ->
      let bar =
        G.feature __POS__ "bar" (fun () ->
            all
              [ Solid.plane "end" (1., 0., 0.) (reach +. 5.); Solid.plane "start" (-1., 0., 0.) 5.; Solid.plane "side+" (0., 1., 0.) 4.;
                Solid.plane "side-" (0., -1., 0.) 4.; Solid.plane "top" (0., 0., 1.) 3.; Solid.plane "under" (0., 0., -1.) 3. ])
      in
      (* The pin stands 3 mm proud of the bar above and below. *)
      let pin = G.feature __POS__ "pin" (fun () -> all [ Solid.cylinder "barrel" (reach, 0.) 2.5; Solid.plane "top" (0., 0., 1.) 6.; Solid.plane "under" (0., 0., -1.) 6. ]) in
      let own = G.union bar pin in
      if k = Array.length joints - 1 then own else G.union own (link (k + 1)))

(* A plate with a post that the first link's joint sits on. *)
let base () =
  G.feature __POS__ "base" (fun () ->
      let plate =
        G.feature __POS__ "plate" (fun () ->
            all
              [ Solid.plane "+x" (1., 0., 0.) 16.; Solid.plane "-x" (-1., 0., 0.) 12.; Solid.plane "+y" (0., 1., 0.) 12.; Solid.plane "-y" (0., -1., 0.) 12.;
                Solid.plane "top" (0., 0., 1.) 0.; Solid.plane "under" (0., 0., -1.) 5. ])
      in
      let post = G.feature __POS__ "post" (fun () -> all [ Solid.cylinder "barrel" (0., 0.) 4.; Solid.plane "top" (0., 0., 1.) 12.; Solid.plane "under" (0., 0., -1.) 2. ]) in
      G.union plate post)

let graph () =
  let stand = base () in
  G.union stand (link 0)
let lo = [| -17.; -33.; -9. |] and hi = [| 103.; 47.; 57. |]
