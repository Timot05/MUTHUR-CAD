(* A bracket made of sketches: what a drawn outline gives, in one model.

   - plate: an L with rounded outer corners and a rounded inside corner, pulled 8 mm,
     with a bore and a slot cut through it (each hole a loop of its own);
   - bushing: a profile drawn in (radius, height) and turned about the bore's axis;
   - lug: an outline drawn in a plane standing on the plate (a frame turned a quarter
     about x), with an eye through it.
   The bushing and the lug start 2 mm inside the plate, and the bushing's bore is 1 mm
   narrower than the plate's, so that no two faces lie on one surface. Lengths in
   millimetres. *)
open Mk52
module G = Graph
module S = Sketch

let plate () =
  let outline =
    S.(start (0., 0.) |> line "front" (80., 0.) |> line "right" (80., 22.) |> arc "round 1" ~centre:(72., 22.) (72., 30.) |> line "ledge" (38., 30.)
       |> arc ~cw:true "inside" ~centre:(38., 40.) (28., 40.) |> line "wall" (28., 60.) |> arc "round 2" ~centre:(20., 60.) (20., 68.) |> line "back" (8., 68.)
       |> arc "round 3" ~centre:(8., 60.) (0., 60.) |> close ~name:"left")
  in
  let slot = S.(start (18., 16.) |> line "slot east" (18., 44.) |> arc "slot north" ~centre:(14., 44.) (10., 44.) |> line "slot west" (10., 16.) |> arc "slot south" ~centre:(14., 16.) (18., 16.) |> close) in
  S.extrude ~holes:[ S.circle "bore" (60., 14.) 6.; slot ] ~lo:0. ~hi:8. outline

(* Drawn in (distance from the axis, height): a seat, a skirt, a shoulder, a neck and a rounded crown. *)
let bushing () =
  S.revolve S.(start (5., -2.) |> line "seat" (13., -2.) |> line "skirt" (13., 5.) |> line "shoulder" (10., 5.) |> line "neck" (10., 12.) |> arc "crown" ~centre:(8., 12.) (8., 14.) |> line "rim" (5., 14.) |> close ~name:"bore")

let lug () =
  let outline = S.(start (-10., -2.) |> line "foot" (10., -2.) |> line "east" (10., 12.) |> arc "crown" ~centre:(0., 12.) (-10., 12.) |> close ~name:"west") in
  S.extrude ~holes:[ S.circle "eye" (0., 12.) 4. ] ~lo:(-3.) ~hi:3. outline

let graph () =
  let plate = G.feature __POS__ "plate" plate in
  let bushing = G.feature ~frame:(Frame.translate (60., 14., 8.)) __POS__ "bushing" bushing in
  let lug = G.feature ~frame:(Frame.chain [ Frame.translate (14., 56., 8.); Frame.rotate (1., 0., 0.) 90. ]) __POS__ "lug" lug in
  G.union (G.union plate bushing) lug

let lo = [| -6.3; -6.2; -6.4 |] and hi = [| 86.1; 74.3; 36.2 |]
