(* 15. Sketches: is the field of a drawn outline the distance to it, and do its corners
   and seams come out as edges?

   Outlines of lines and arcs, in millimetres, each stressing one thing: sharp convex and
   reflex corners, tangent joins of a line and an arc turning either way, arcs joined to
   arcs, half circles, a corner of 8 degrees, collinear spans, a hole.
   The answers are known without the field: the distance to the outline by going through
   its spans one by one, inside or outside by counting crossings of a ray, and for a solid
   the counts: an outline of n named spans pulled straight has 2n corners and 3n edges,
   and turned about an axis it has n edges, all closed and no corner.
   Limits, fixed beforehand:
   - the field equal to the distance to 1e-9 mm, and no wrong sign further than 1e-3 mm
     from the outline;
   - the slope at most 1 + 1e-9;
   - every solid: its corners and edges as counted, no flag;
   - a solid built in a turned frame: the same counts, each known corner within 1 um;
   - the bracket (models/bracket.ml: a plate with a bore and a slot, a turned bushing, a
     lug standing in its own frame): 36 corners and 65 edges, 11 of them closed, counted
     by hand, and no flag. *)
open Mk52
open Harness
module S = Sketch

let line = S.line and arc = S.arc

let outlines =
  [ ("rectangle", S.polygon [ (-10., -5.); (10., -5.); (10., 5.); (-10., 5.) ]);
    ("L", S.polygon [ (-10., -10.); (10., -10.); (10., -2.5); (-2.5, -2.5); (-2.5, 10.); (-10., 10.) ]);
    (* an 8 degree corner, and a short side whose line runs on over the long one *)
    ("thin wedge", S.polygon [ (0., 0.); (40., 0.); (12., 4.); (0., 4.) ]);
    ("slot", S.(start (-5., -5.) |> line "south" (5., -5.) |> arc "east" ~centre:(5., 0.) (5., 5.) |> line "north" (-5., 5.) |> arc "west" ~centre:(-5., 0.) (-5., -5.) |> close));
    (* a half circle cut into the top: a concave arc with sharp ends, and two collinear lines *)
    ( "notch",
      S.(start (-10., -5.) |> line "south" (10., -5.) |> line "east" (10., 5.) |> line "north east" (2.5, 5.) |> arc ~cw:true "notch" ~centre:(0., 5.) (-2.5, 5.) |> line "north west" (-10., 5.) |> close ~name:"west") );
    (* the L with its inside corner rounded: line, concave arc, line *)
    ( "L fillet",
      S.(start (-10., -10.) |> line "south" (10., -10.) |> line "east" (10., -2.5) |> line "ledge" (0., -2.5) |> arc ~cw:true "fillet" ~centre:(0., 0.) (-2.5, 0.) |> line "wall" (-2.5, 10.) |> line "north" (-10., 10.) |> close ~name:"west") );
    (* every corner rounded: no sharp corner anywhere *)
    ( "rounded rectangle",
      S.(start (-7., -5.) |> line "south" (7., -5.) |> arc "round 1" ~centre:(7., -2.) (10., -2.) |> line "east" (10., 2.) |> arc "round 2" ~centre:(7., 2.) (7., 5.) |> line "north" (-7., 5.)
         |> arc "round 3" ~centre:(-7., 2.) (-10., 2.) |> line "west" (-10., -2.) |> arc "round 4" ~centre:(-7., -2.) (-7., -5.) |> close) );
    (* an arc and its chord: two sharp corners between a line and a circle *)
    ("D", S.(start (6., -8.) |> arc "bow" ~centre:(0., 0.) (6., 8.) |> close ~name:"chord"));
    (* four half circles, each tangent to the next: arc to arc, turning both ways *)
    ( "wave",
      S.(start (10., 0.) |> arc "big" ~centre:(0., 0.) (-10., 0.) |> arc "small" ~centre:(-5., 0.) (0., 0.) |> arc ~cw:true "dip" ~centre:(3., 0.) (6., 0.) |> arc "tail" ~centre:(8., 0.) (10., 0.) |> close) );
    ("circle", S.circle "rim" (1., -2.) 7.) ]

let hole = S.circle "bore" (4., 0.5) 2.5

(* ---- The answers, without the field ---- *)

let dist (ax, ay) (bx, by) = Float.hypot (ax -. bx) (ay -. by)

(* The distance from p to one span, ends included. *)
let to_span (s : S.span) ((px, py) as p) =
  match s.arc with
  | None ->
      let (ax, ay), (bx, by) = (s.a, s.b) in
      let dx = bx -. ax and dy = by -. ay in
      let t = Float.max 0. (Float.min 1. ((((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. ((dx *. dx) +. (dy *. dy)))) in
      dist p (ax +. (t *. dx), ay +. (t *. dy))
  | Some (((cx, cy) as c), ccw) ->
      let angle (x, y) = Float.atan2 (y -. cy) (x -. cx) in
      let turn a b = let t = if ccw then b -. a else a -. b in t -. (2. *. Float.pi *. Float.floor (t /. (2. *. Float.pi))) in
      if turn (angle s.a) (angle p) <= S.sweep s then Float.abs (dist p c -. S.radius s) else Float.min (dist p s.a) (dist p s.b)

let distance (sk : S.t) p = Array.fold_left (fun m s -> Float.min m (to_span s p)) infinity sk

(* Inside by crossings of the ray towards +x, the arcs as 256 chords each. *)
let inside (sk : S.t) (px, py) =
  let crossings = ref 0 in
  Array.iter
    (fun (s : S.span) ->
      let k = if s.arc = None then 1 else 256 in
      for i = 0 to k - 1 do
        let ax, ay = S.along s (float_of_int i /. float_of_int k) and bx, by = S.along s (float_of_int (i + 1) /. float_of_int k) in
        if (ay <= py && by > py) || (by <= py && ay > py) then if ax +. ((py -. ay) *. (bx -. ax) /. (by -. ay)) > px then incr crossings
      done)
    sk;
  !crossings mod 2 = 1

let box (sk : S.t) pad =
  let xs = Array.to_list (Array.concat (Array.to_list (Array.map (fun s -> Array.init 9 (fun k -> S.along s (float_of_int k /. 8.))) sk))) in
  let low f = List.fold_left (fun m p -> Float.min m (f p)) infinity xs and high f = List.fold_left (fun m p -> Float.max m (f p)) neg_infinity xs in
  (* a little off centre, so that no face lies on a cell's wall *)
  ([| low fst -. pad -. 0.31; low snd -. pad -. 0.17; -.pad -. 0.23 |], [| high fst +. pad +. 0.13; high snd +. pad +. 0.29; pad +. 0.11 |])

let run () =
  let worst_value = ref 0. and wrong = ref 0. and slope = ref 0. and off_pulled = ref 0. and off_turned = ref 0. in
  let notes = ref [] in
  List.iter
    (fun (name, sk) ->
      let lo, hi = box sk 6. in
      let pts = points ~lo ~hi 30_000 in
      let f = field (S.field sk) in
      Array.iter
        (fun x ->
          let p = (x.(0), x.(1)) in
          let d = distance sk p and v = f.value x in
          worst_value := Float.max !worst_value (Float.abs (Float.abs v -. d));
          if d > 1e-3 && v <= 0. <> inside sk p then wrong := !wrong +. 1.)
        pts;
      slope := Float.max !slope (steepest f pts);
      (* pulled straight between z = -3 and 3 *)
      let n = List.length (List.sort_uniq compare (Array.to_list (Array.map (fun (s : S.span) -> s.name) sk))) in
      let counts = if n = 1 then (0, 2, 2) else (2 * Array.length sk, 3 * Array.length sk, 0) in
      let off, t = topology_off ~lo ~hi (S.extrude ~lo:(-3.) ~hi:3. sk) counts in
      if off > 0. then notes := Printf.sprintf "%s pulled: %d corners, %d edges, %d flags" name (List.length t.vertices) (List.length t.edges) (List.length t.flags) :: !notes;
      off_pulled := !off_pulled +. off;
      (* turned about z, drawn 30 mm out from the axis *)
      let moved = Array.map (fun (s : S.span) -> let m (x, y) = (x +. 30., y) in { s with a = m s.a; b = m s.b; arc = Option.map (fun (c, w) -> (m c, w)) s.arc }) sk in
      let rlo, rhi = box moved 6. in
      let r = Float.max (Float.abs rlo.(0)) rhi.(0) in
      let off, t = topology_off ~lo:[| -.r -. 0.31; -.r -. 0.17; rlo.(1) |] ~hi:[| r +. 0.13; r +. 0.29; rhi.(1) |] (S.revolve moved) ((0, n, n) |> fun c -> if n = 1 then (0, 0, 0) else c) in
      if off > 0. then notes := Printf.sprintf "%s turned: %d corners, %d edges, %d flags" name (List.length t.vertices) (List.length t.edges) (List.length t.flags) :: !notes;
      off_turned := !off_turned +. off)
    outlines;
  (* a plate with a hole: the hole's rim is one face, so two closed edges and no seam *)
  let plate = List.assoc "rectangle" outlines in
  let lo, hi = box plate 6. in
  let off_hole, _ = topology_off ~lo ~hi (S.extrude ~holes:[ hole ] ~lo:(-3.) ~hi:3. plate) (8, 14, 2) in
  let fh = field (S.profile ~holes:[ hole ] plate) in
  let wrong_hole = wrong_signs fh ~inside:(fun x -> let p = (x.(0), x.(1)) in inside plate p && not (inside hole p)) (Array.of_list (List.filter (fun x -> Float.min (distance plate (x.(0), x.(1))) (distance hole (x.(0), x.(1))) > 1e-3) (Array.to_list (points ~lo ~hi 30_000)))) in
  (* the L built in a frame turned off every axis *)
  let frame = Frame.chain [ Frame.translate (6., -4., 9.); Frame.rotate (1., 2., 3.) 37. ] in
  let ell = List.assoc "L" outlines in
  let turned = Graph.within frame (fun () -> S.extrude ~lo:(-3.) ~hi:3. ell) in
  let off_frame, t = topology_off ~lo:[| -22.; -24.; -12. |] ~hi:[| 29.; 21.; 31. |] turned (12, 18, 0) in
  let known = List.concat_map (fun (s : S.span) -> List.map (fun z -> Frame.apply frame [| fst s.a; snd s.a; z |]) [ -3.; 3. ]) (Array.to_list ell) in
  let miss c = List.fold_left (fun m (v : Extract.vertex) -> Float.min m (norm (Array.map2 ( -. ) v.at c))) infinity t.vertices in
  let off_bracket, _ = topology_off ~lo:Models.Bracket.lo ~hi:Models.Bracket.hi (Models.Bracket.graph ()) (36, 65, 11) in
  let note = String.concat "; " (List.rev !notes) in
  [ at_most "ten outlines: |field| against the distance to the outline, mm" !worst_value 1e-9;
    at_most "ten outlines: wrong signs further than 1 um from the outline" !wrong 0.;
    at_most "ten outlines: steepest sampled gradient" !slope (1. +. 1e-9);
    at_most ~note "ten outlines pulled straight: corners, edges, flags off 2n and 3n" !off_pulled 0.;
    at_most ~note "ten outlines turned about an axis: edges, flags off n closed edges" !off_turned 0.;
    at_most "plate with a hole: wrong signs" wrong_hole 0.;
    at_most "plate with a hole: corners, edges, flags off 8, 14 and 2 closed" off_hole 0.;
    at_most "L in a turned frame: corners, edges, flags off 12 and 18" off_frame 0.;
    at_most "L in a turned frame: known corners, furthest from a found corner, mm" (List.fold_left (fun m c -> Float.max m (miss c)) 0. known) 1e-3;
    at_most "bracket: corners, edges, flags off 36, 65 and 11 closed" off_bracket 0. ]

let experiment = { id = 15; question = "Sketches: is the field of a drawn outline the distance to it, and do its corners and seams come out as edges?"; run }
