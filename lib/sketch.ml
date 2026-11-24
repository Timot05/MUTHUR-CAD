(* Sketches: a closed outline of lines and arcs drawn in a plane, and its field.

   The field is the signed distance to the outline, negative inside: for every point the
   nearest part of the outline answers. The parts are the spans and the corners between
   them, and each answers with its own distance: a line or a circle's, signed by the side
   it is seen from, or at a sharp corner the distance to the corner point (positive
   outside a convex corner, negative inside a reflex one). So the slope is 1 everywhere
   and nothing steps, whatever the outline: spans may meet at any angle, arc to arc
   included.

   Which span is nearest is decided by comparing distances, a tournament of selects.
   Those comparisons are poor guards at the outline itself (two spans are both at
   distance 0 at their corner, from every side), so no edge is left to them: within a
   small disc about each corner, the two spans that meet there both answer with one
   shared node, the corner's own formula, whose guards are plain: a - b where the two
   faces cross, the normal lines through the corner, and at a tangent join the normal
   line alone (a seam). Every edge of a sketched solid is found on those.

   Each span is a face, named as drawn. A sketch is drawn in the plane of two
   coordinates, by default x and y of the frame it is built in. *)
module E = Expr
module G = Graph

type point = float * float

(* [arc]: the centre, and whether it turns counter-clockwise. An arc is at most a half turn. *)
type span = { name : string; a : point; b : point; arc : (point * bool) option }

(* A closed loop, counter-clockwise: the inside is on the left of every span. *)
type t = span array

let ( -- ) (ax, ay) (bx, by) = (ax -. bx, ay -. by)
let ( ++ ) (ax, ay) (bx, by) = (ax +. bx, ay +. by)
let dot (ax, ay) (bx, by) = (ax *. bx) +. (ay *. by)
let cross (ax, ay) (bx, by) = (ax *. by) -. (ay *. bx)
let len a = sqrt (dot a a)
let turned (x, y) phi = ((x *. cos phi) -. (y *. sin phi), (x *. sin phi) +. (y *. cos phi))
let radius s = match s.arc with Some (c, _) -> len (s.a -- c) | None -> infinity

(* The angle an arc turns through, more than 0 and at most a full turn. *)
let sweep s =
  match s.arc with
  | None -> 0.
  | Some (c, ccw) ->
      let a = s.a -- c and b = s.b -- c in
      let t = Float.atan2 (cross a b) (dot a b) in
      let t = if ccw then t else -.t in
      if t <= 1e-12 then t +. (2. *. Float.pi) else t

(* The point a fraction [f] of the way along a span, and the direction of travel at a point of it. *)
let along s f =
  match s.arc with
  | None -> s.a ++ (f *. fst (s.b -- s.a), f *. snd (s.b -- s.a))
  | Some (c, ccw) -> c ++ turned (s.a -- c) (f *. sweep s *. if ccw then 1. else -1.)

let tangent s p =
  match s.arc with
  | None -> let d = s.b -- s.a in (fst d /. len d, snd d /. len d)
  | Some (c, ccw) -> let x, y = p -- c in let l = len (x, y) in if ccw then (-.y /. l, x /. l) else (y /. l, -.x /. l)

(* ---- Drawing ---- *)

let fail fmt = Printf.ksprintf (fun s -> failwith ("sketch: " ^ s)) fmt

(* The loop checked and turned counter-clockwise. Refused: a loop that does not close, a
   span of no length, a cusp (the outline doubling back on itself) and, as far as eight
   chords a span can tell, an outline that crosses itself. *)
let make spans =
  let sk = Array.of_list spans in
  let n = Array.length sk in
  if n < 2 then fail "an outline needs at least two spans";
  let size = Array.fold_left (fun m s -> Float.max m (Float.max (len (s.a -- sk.(0).a)) (radius s |> fun r -> if Float.is_finite r then r else 0.))) 0. sk in
  Array.iteri
    (fun i s ->
      if len (s.b -- s.a) <= 1e-9 *. size then fail "%s has no length" s.name;
      if len (s.b -- sk.((i + 1) mod n).a) > 1e-9 *. size then fail "%s does not end where %s starts" s.name sk.((i + 1) mod n).name)
    sk;
  (* twice the area: the chords' polygon, and each arc's segment *)
  let area =
    Array.fold_left
      (fun acc s ->
        let t = sweep s and r = radius s in
        acc +. cross s.a s.b +. match s.arc with None -> 0. | Some (_, ccw) -> (if ccw then 1. else -1.) *. r *. r *. (t -. sin t))
      0. sk
  in
  if Float.abs area <= 1e-12 *. size *. size then fail "the outline has no area";
  let sk = if area > 0. then sk else Array.init n (fun i -> let s = sk.(n - 1 - i) in { s with a = s.b; b = s.a; arc = Option.map (fun (c, ccw) -> (c, not ccw)) s.arc }) in
  Array.iteri
    (fun i s ->
      let s' = sk.((i + 1) mod n) in
      let t = tangent s s.b and t' = tangent s' s'.a in
      if dot t t' < 0. && Float.abs (cross t t') <= 1e-9 then fail "%s and %s meet in a cusp" s.name s'.name)
    sk;
  let chords = Array.concat (Array.to_list (Array.map (fun s -> Array.init 8 (fun k -> (along s (float_of_int k /. 8.), along s (float_of_int (k + 1) /. 8.), s.name))) sk)) in
  let m = Array.length chords in
  let side a b c = cross (b -- a) (c -- a) in
  for i = 0 to m - 1 do
    for j = i + 2 to m - 1 do
      let p, p', name = chords.(i) and q, q', name' = chords.(j) in
      if (not (i = 0 && j = m - 1)) && side p p' q *. side p p' q' < 0. && side q q' p *. side q q' p' < 0. then fail "%s crosses %s" name name'
    done
  done;
  sk

(* A pen: [start p |> line "bottom" q |> arc "round" ~centre r |> ... |> close ~name:"left"].
   An arc runs counter-clockwise from the pen to its end point unless [cw]; the same
   point again is a full circle. One of more than a half turn is cut into equal spans of
   one name, so it is still one face. *)
type pen = { from : point; at : point; drawn : span list }

let start p = { from = p; at = p; drawn = [] }
let line name b pen = { pen with at = b; drawn = { name; a = pen.at; b; arc = None } :: pen.drawn }

let arc ?(cw = false) name ~centre b pen =
  let whole = { name; a = pen.at; b; arc = Some (centre, not cw) } in
  let r = radius whole in
  if Float.abs (len (b -- centre) -. r) > 1e-6 *. r then fail "%s: its ends are not at one distance from its centre" name;
  let k = max 1 (int_of_float (Float.ceil ((sweep whole /. Float.pi) -. 1e-9))) in
  let cut i = if i = 0 then whole.a else if i = k then b else along whole (float_of_int i /. float_of_int k) in
  { pen with at = b; drawn = List.rev_append (List.init k (fun i -> { whole with a = cut i; b = cut (i + 1) })) pen.drawn }

(* With [name], a last line back to the start. *)
let close ?name pen = make (List.rev (match name with Some n when pen.at <> pen.from -> line n pen.from pen | _ -> pen).drawn)

let polygon ?(name = Printf.sprintf "side %d") = function
  | [] -> fail "a polygon needs points"
  | p :: rest -> List.fold_left (fun (pen, i) q -> (line (name i) q pen, i + 1)) (start p, 0) rest |> fun (pen, i) -> close ~name:(name i) pen

let circle name (cx, cy) r = start (cx +. r, cy) |> arc name ~centre:(cx, cy) (cx +. r, cy) |> close

(* ---- The field ---- *)

(* [moving]: the same outline as it is somewhere else, and how far there it is, s (a
   formula): a loft's section between two (loft, below). Every point of the outline is
   then where it is a fraction s of the way, and every radius likewise. The spans must
   keep their directions (lines parallel, arcs over the same angles), so each corner
   and each guard stays the kind it is, and only moves. [lip] is the slope bound of
   every part; 1 for a still outline. *)
let field ?(u = E.x) ?(v = E.y) ?moving ?(lip = 1.) (sk : t) =
  let n = Array.length sk in
  let other, s = match moving with Some (o, s) -> (o, s) | None -> (sk, E.const 0.) in
  let size = Array.fold_left (fun m sp -> Float.max m (len (sp.a -- sk.(0).a))) 0. (Array.append sk other) in
  let tiny = E.const ((1e-9 *. size) ** 2.) in
  (* a point, or a number, where it is at s: still unless it moves *)
  let lerp a b = if a = b then E.const a else E.add (E.const a) (E.mul s (E.const (b -. a))) in
  let at (ax, ay) (bx, by) = (lerp ax bx, lerp ay by) in
  let start i = at sk.(i).a other.(i).a and finish i = at sk.(i).b other.(i).b and middle i = at (along sk.(i) 0.5) (along other.(i) 0.5) in
  let centre i = match (sk.(i).arc, other.(i).arc) with Some (c, _), Some (c', _) -> at c c' | _ -> fail "%s is an arc in one section and not the other" sk.(i).name in
  let from (px, py) = (E.sub u px, E.sub v py) in
  (* how far past the point p, going the way t *)
  let ahead (tx, ty) p = let dx, dy = from p in E.add (E.mul (E.const tx) dx) (E.mul (E.const ty) dy) in
  let square p = let dx, dy = from p in E.add (E.square dx) (E.square dy) in
  let piece name f = G.piece ~lip name f and expr f = G.expr ~lip f in
  let sel ?smooth guard le gt = if le == gt then le else G.select ?smooth ~guard ~le ~gt () in
  (* a span's own distance: to its whole line, with outside on the right of travel, or to its whole circle *)
  let own =
    Array.mapi
      (fun i s ->
        match s.arc with
        | None -> let tx, ty = tangent s s.a in ahead (ty, -.tx) (start i)
        | Some (_, ccw) -> let d = E.sqrt (square (centre i)) and r = lerp (radius s) (radius other.(i)) in if ccw then E.sub d r else E.sub r d)
      sk
  in
  let face = Array.mapi (fun i s -> piece s.name own.(i)) sk in
  let gap = Array.map (fun d -> expr (E.sqrt (E.add (E.square d) tiny))) own in
  (* above 0 before the span's start, past its end, and (an arc) past its middle *)
  let before_at = Array.mapi (fun i s -> E.neg (ahead (tangent s s.a) (start i))) sk and past_at = Array.mapi (fun i s -> ahead (tangent s s.b) (finish i)) sk in
  let before = Array.map expr before_at and past = Array.map expr past_at in
  let half = Array.mapi (fun i s -> Option.map (fun _ -> let m = along s 0.5 in expr (ahead (tangent s m) (middle i))) s.arc) sk in
  let next k = (k + 1) mod n in
  (* the smaller of the two: a span's length and radius change evenly, so between two places it is never less *)
  let reach k = let r (s : span) = Float.min (len (s.b -- s.a)) (radius s) in Float.min (r sk.(k)) (r other.(k)) in
  (* Corner k, where span k ends and the next starts: its distance, its disc (at most 0
     inside), its two fans and its formula over the two spans.

     A fan is the corner point's own distance, the answer where the point itself is the
     nearest part of the outline: between the two normal lines through it, outside a
     convex corner and inside a reflex one. There is one per span, each with its span's
     name. The formula is the larger of the two spans (convex) or the smaller (reflex),
     each span giving way to its fan between the normals. So the guard between the two
     faces, a - b, is read everywhere about the corner, and Newton onto the corner's
     edge has the same two equations from every side. A fan starts a hair (1e-9 of the
     sketch's size) clear of the normals, so that a point on the edge itself, which
     rounding puts on either side of them, reads the two faces and never a fan. *)
  let clear e = expr (E.sub e (E.const (1e-9 *. size))) in
  let corner =
    Array.init n (fun k ->
        let a = sk.(k) and b = sk.(next k) in
        let turn = cross (tangent a a.b) (tangent b b.a) and r2 = square (finish k) in
        let far = expr (E.sqrt (E.add r2 tiny)) in
        let disc = expr (E.sub (E.sqrt (E.add r2 tiny)) (E.const (0.25 *. Float.min (reach k) (reach (next k))))) in
        if Float.abs turn <= 1e-9 then (far, disc, face.(k), face.(next k), sel ~smooth:true past.(k) face.(k) face.(next k))
        else
          let fan (s : span) = piece s.name (if turn > 0. then E.sqrt r2 else E.neg (E.sqrt r2)) in
          let fan_a = fan a and fan_b = fan b and beyond_a = clear past_at.(k) and before_b = clear before_at.(next k) in
          let side own fan = G.select ~guard:beyond_a ~le:own ~gt:(G.select ~guard:before_b ~le:own ~gt:fan ()) () in
          let sa = side face.(k) fan_a and sb = side face.(next k) fan_b in
          let guard = G.diff face.(k) face.(next k) in
          (far, disc, fan_a, fan_b, if turn > 0. then G.select ~guard ~le:sb ~gt:sa () else G.select ~guard ~le:sa ~gt:sb ()))
  in
  (* A span's distance and its answer: its own between its ends, its corners' beyond
     them, and within a corner's disc that corner's formula. *)
  let unit i =
    let far_w, disc_w, _, start, join_w = corner.((i + n - 1) mod n) and far_k, disc_k, finish, _, join_k = corner.(i) in
    let shape own start finish =
      match half.(i) with
      | None -> sel before.(i) (sel past.(i) own finish) start
      | Some m -> sel m (sel before.(i) own start) (sel past.(i) own finish)
    in
    (shape gap.(i) far_w far_k, sel disc_k join_k (sel disc_w join_w (shape face.(i) start finish)))
  in
  let rec nearest lo hi =
    if hi - lo = 1 then unit lo
    else
      let mid = (lo + hi) / 2 in
      let da, va = nearest lo mid and db, vb = nearest mid hi in
      let guard = G.diff da db in
      (G.select ~guard ~le:da ~gt:db (), G.select ~guard ~le:va ~gt:vb ())
  in
  snd (nearest 0 n)

(* ---- Solids ---- *)

(* What was drawn, kept for showing: each solid made from a sketch, with the feature it
   was made in and that feature's frame. Pulled, the outline lies in the frame's x and y;
   turned, in its (distance from the z axis, z), shown in the plane y = 0. *)
type drawn = { feature : int; frame : Frame.t; turned : bool; loops : t list }

let drawn : drawn list ref = ref []
let keep ~turned loops = drawn := { feature = !G.current; frame = !G.ambient; turned; loops } :: !drawn

(* A span as points to draw: its two ends, or an arc every 6 degrees. *)
let points s =
  let k = max 1 (int_of_float (Float.ceil (sweep s /. (Float.pi /. 30.)))) in
  List.init (k + 1) (fun i -> along s (float_of_int i /. float_of_int k))

(* An outline with holes cut out of it, each a loop of its own. [moving]: the same
   outline and holes as they are somewhere else, and how far there (field, above). *)
let profile ?u ?v ?(holes = []) ?moving ?lip outer =
  let to_ part = Option.map (fun (outer', holes', s) -> ((match part with None -> outer' | Some k -> List.nth holes' k), s)) moving in
  List.fold_left (fun (f, k) h -> (G.subtract f (field ?u ?v ?moving:(to_ (Some k)) ?lip h), k + 1)) (field ?u ?v ?moving:(to_ None) ?lip outer, 0) holes |> fst

(* The profile between two heights along the third coordinate. The two ends are the
   faces "top" and "bottom": a span of the same name would be the same face, and the
   edge between them would be lost, so it is refused. *)
let extrude ?u ?v ?w ?(holes = []) ~lo ~hi outer =
  if u = None && v = None && w = None then keep ~turned:false (outer :: holes);
  let w = Option.value ~default:E.z w in
  List.iter (Array.iter (fun s -> if s.name = "top" || s.name = "bottom" then fail "a span named %s would be the same face as the solid's %s" s.name s.name)) (outer :: holes);
  G.intersection (G.intersection (profile ?u ?v ~holes outer) (G.piece "top" (E.sub w (E.const hi)))) (G.piece "bottom" (E.sub (E.const lo) w))

(* The profile turned about the z axis: drawn in (distance from the axis, z). The field
   is still the distance: the nearest point of a turned solid lies in the same half-plane
   through the axis. *)
let revolve ?(holes = []) outer =
  keep ~turned:true (outer :: holes);
  profile ~u:(E.sqrt (E.add (E.square E.x) (E.square E.y))) ~v:E.z ~holes outer

(* ---- Lofts ---- *)

(* A section of a loft: a profile, outline and holes, at a height along the frame's z. *)
(* Its field is [height], not [at]: a pen's [at] is in scope in the same files, and OCaml
   would take one for the other. *)
type section = { height : float; outline : t; holes : t list }

let section ?(holes = []) height outline = { height; outline; holes }

(* Two sections whose outlines are the same drawing moved: as many loops, spans and names,
   each line keeping its direction and each arc its angles (it may move and change its
   radius). What a point moves by, at most, if they are. *)
let follows (a : section) (b : section) =
  let same_dir (x, y) (x', y') = Float.abs (cross (x, y) (x', y')) <= 1e-9 && dot (x, y) (x', y') > 0. in
  let unit p = let l = len p in (fst p /. l, snd p /. l) in
  let loop (l : t) (l' : t) =
    if Array.length l <> Array.length l' then None
    else
      Array.fold_left
        (fun acc (s, s') ->
          match acc with
          | None -> None
          | Some moved ->
              if s.name <> s'.name then None
              else
                match (s.arc, s'.arc) with
                | None, None when same_dir (tangent s s.a) (tangent s' s'.a) -> Some (List.fold_left Float.max moved [ len (s'.a -- s.a); len (s'.b -- s.b) ])
                | Some (c, ccw), Some (c', ccw') when ccw = ccw' && same_dir (unit (s.a -- c)) (unit (s'.a -- c')) && same_dir (unit (s.b -- c)) (unit (s'.b -- c')) ->
                    Some (List.fold_left Float.max moved [ len (s'.a -- s.a); len (s'.b -- s.b); len (c' -- c) +. Float.abs (radius s' -. radius s) ])
                | _ -> None)
        (Some 0.)
        (Array.map2 (fun s s' -> (s, s')) l l')
  in
  if List.length a.holes <> List.length b.holes then None
  else List.fold_left2 (fun acc l l' -> match (acc, loop l l') with Some m, Some m' -> Some (Float.max m m') | _ -> None) (Some 0.) (a.outline :: a.holes) (b.outline :: b.holes)

(* A solid through sections, from the lowest to the highest, straight from one to the
   next; below the lowest and above the highest the faces "bottom" and "top" close it.

   Two neighbouring sections that are the same drawing moved (follows, above: lines
   keep their directions, arcs their angles, names their order) give exactly the
   straight loft between them: at each height the outline is where its points are,
   each a fraction of the way along a straight line between its places in the two
   sections, and the field is the distance to it across the height. A side that moves
   is a plane, a circle that grows a cone, a corner a straight edge. A span keeps its
   name, so a face does. The slope is sqrt (1 + (m / h)^2), m being the most any point
   or radius moves and h the height between the sections.

   Any other two sections are blended: between them the field is (1 - s) f0 + s f1, s
   going from 0 to 1, which is the straight loft only where the same span answers in
   both and the two are parallel lines or circles about one centre. Elsewhere it is a
   blend of the two shapes, near the CAD loft but not on it: at a convex corner of a
   tapering square the edge comes out inside the straight one (experiment 16 measures
   it). A face keeps its span's name where the same name answers in both sections, and
   is "loft[a,b]" where they differ. The slope is sqrt (1 + (D / h)^2), D being the most
   the two sections' fields differ, worked out on a grid over the outlines, with a
   margin for what lies between its points.

   Through three sections or more the loft creases at each; there is no smooth one. *)
let loft sections =
  let sections = List.sort (fun a b -> compare a.height b.height) sections in
  if List.length sections < 2 then fail "a loft needs at least two sections";
  List.iteri (fun i s -> if i > 0 && s.height -. (List.nth sections (i - 1)).height <= 0. then fail "two sections of a loft are at one height, %g" s.height) sections;
  List.iter (fun s -> List.iter (Array.iter (fun sp -> if sp.name = "top" || sp.name = "bottom" then fail "a span named %s would be the same face as the loft's %s" sp.name sp.name)) (s.outline :: s.holes)) sections;
  List.iter (fun s -> drawn := { feature = !G.current; frame = Frame.compose !G.ambient (Frame.translate (0., 0., s.height)); turned = false; loops = s.outline :: s.holes } :: !drawn) sections;
  (* how far apart two profiles' fields get, anywhere: they differ most within the box
     of their outlines (beyond it both are distances to the two shapes, which differ by
     at most their Hausdorff distance, a value taken inside), and each field's slope is
     1, so between grid points the difference grows by at most the cell's diagonal *)
  let apart (a : section) (b : section) =
    let pts = List.concat_map (fun (l : t) -> List.concat_map points (Array.to_list l)) [ a.outline; b.outline ] in
    let lo f = List.fold_left (fun m p -> Float.min m (f p)) infinity pts and hi f = List.fold_left (fun m p -> Float.max m (f p)) neg_infinity pts in
    let pad = 0.02 *. Float.max (hi fst -. lo fst) (hi snd -. lo snd) in
    let x0 = lo fst -. pad and x1 = hi fst +. pad and y0 = lo snd -. pad and y1 = hi snd +. pad in
    let plain f = let saved = !G.coords in G.coords := None; Fun.protect ~finally:(fun () -> G.coords := saved) f in
    let compiled (s : section) = let p = Prog.compile (plain (fun () -> profile ~holes:s.holes s.outline)) in (p, Prog.ctx p, Prog.whole p) in
    let pa, ca, wa = compiled a and pb, cb, wb = compiled b in
    let k = 96 in
    let worst = ref 0. in
    for i = 0 to k do
      for j = 0 to k do
        let q = [| x0 +. ((x1 -. x0) *. float_of_int i /. float_of_int k); y0 +. ((y1 -. y0) *. float_of_int j /. float_of_int k); 0. |] in
        worst := Float.max !worst (Float.abs (Prog.value pa ca wa q -. Prog.value pb cb wb q))
      done
    done;
    !worst +. Float.hypot ((x1 -. x0) /. float_of_int k) ((y1 -. y0) /. float_of_int k)
  in
  let w = E.z in
  let fields = List.map (fun s -> profile ~holes:s.holes s.outline) sections in
  let between =
    List.mapi
      (fun i (a, b) ->
        let h = b.height -. a.height in
        let s = E.div (E.sub w (E.const a.height)) (E.const h) in
        match follows a b with
        | Some moved -> profile ~holes:a.holes ~moving:(b.outline, b.holes, s) ~lip:(1.02 *. Float.sqrt (1. +. ((moved /. h) ** 2.))) a.outline
        | None ->
            let lip = 1.02 *. Float.sqrt (1. +. ((apart a b /. h) ** 2.)) in
            G.apply ~naming:(Joined "loft") ~lip (E.add (E.mul (E.sub (E.const 1.) s) G.x0) (E.mul s G.x1)) [ List.nth fields i; List.nth fields (i + 1) ])
      (List.combine (List.filteri (fun i _ -> i < List.length sections - 1) sections) (List.tl sections))
  in
  (* each loft up to its upper section and, above the last, the last profile; below the
     first, the first profile. At the first and the last heights the loft itself
     answers, not the profile beyond: those are the end faces' planes, and the faces
     beside them must be the loft's *)
  let leaves = Array.of_list (between @ [ List.nth fields (List.length fields - 1) ]) and heights = Array.of_list (List.tl (List.map (fun s -> s.height) sections)) in
  let rec tree lo hi = if hi - lo = 1 then leaves.(lo) else let mid = (lo + hi) / 2 in G.select ~guard:(G.expr (E.sub w (E.const heights.(mid - 1)))) ~le:(tree lo mid) ~gt:(tree mid hi) () in
  let first = (List.hd sections).height and last = (List.nth sections (List.length sections - 1)).height in
  let whole = G.select ~guard:(G.expr (E.sub (E.const first) w)) ~le:(tree 0 (Array.length leaves)) ~gt:(List.hd fields) () in
  G.intersection (G.intersection whole (G.piece "top" (E.sub w (E.const last)))) (G.piece "bottom" (E.sub (E.const first) w))
