(* dune test. Every model against its known counts of corners and edges, and pruning
   against no pruning. And, since counts can be right by luck, what an extraction must
   be whatever the model: its lines on the surface and ending on their corners, its
   faces closed off by their edges, and no change of face on the surface without an edge
   there. That is asked of the model's field directly, not of the extractor. And a mesh:
   closed, on the surface, and creased where the extraction says the model is. And the
   page's shader: what it skips leaves the field as it is. *)
open Mk52
module X = Extract

let failures = ref 0

let check name ok detail =
  if not ok then incr failures;
  Printf.printf "%s %s%s\n%!" (if ok then "ok  " else "FAIL") name (if ok then "" else ": " ^ detail)

(* ---- What an extraction must be ---- *)

(* A model to ask: its field unpruned, its box, and the extraction's walk step. *)
type held = { p : Prog.t; c : Prog.ctx; whole : Bytes.t; lo : float array; hi : float array; diag : float; step : float }

let hold ~lo ~hi g =
  let p = Prog.compile g in
  { p; c = Prog.ctx p; whole = Prog.whole p; lo; hi; diag = X.dist lo hi; step = Array.fold_left Float.max 0. (X.sub hi lo) /. 64. }

let probe h x = Prog.eval h.p h.c h.whole x
let off (d : Prog.dual) = Float.abs d.v /. Float.max 1e-300 (X.norm d.g)
let at x = Printf.sprintf "(%.4g, %.4g, %.4g)" x.(0) x.(1) x.(2)
let between (e : X.edge) = fst e.between ^ " | " ^ snd e.between

(* Newton onto the surface, nothing forced. *)
let to_surface h x0 =
  let rec go x n =
    let r = probe h x in
    let g2 = X.dot r.d.g r.d.g in
    if g2 = 0. || not (Float.is_finite g2) then None
    else
      let dx = X.scale (r.d.v /. g2) r.d.g in
      let x' = X.sub x dx in
      if X.norm dx < 1e-12 *. h.diag then Some x' else if n = 0 then None else go x' (n - 1)
  in
  go x0 16

(* The lines. Every point is on the surface. Every end of a line is a corner of its
   edge, to the last digit, or joins another piece of it, or is where the edge runs out
   of the box; and every corner of an edge is an end of its line. *)
let lines h (t : X.t) =
  let verts = Array.of_list t.vertices in
  let to_wall x = Array.fold_left Float.min infinity (Array.mapi (fun a v -> Float.min (v -. h.lo.(a)) (h.hi.(a) -. v)) x) in
  List.concat_map
    (fun (e : X.edge) ->
      let tips = List.concat_map (fun line -> [ List.hd line; List.nth line (List.length line - 1) ]) e.pieces in
      let on v x = X.dist verts.(v).at x = 0. in
      let joined x = List.length (List.filter (fun y -> X.dist x y <= 1e-6 *. h.diag) tips) >= 2 in
      (if List.exists (List.exists (fun x -> off (probe h x).d > 1e-6 *. h.diag)) e.pieces then [ between e ^ ": a point of its line is off the surface" ] else [])
      @ List.filter_map (fun x -> if List.exists (fun v -> on v x) e.ends || joined x || to_wall x <= h.step then None else Some (Printf.sprintf "%s: its line ends at %s, which is no corner of it" (between e) (at x))) tips
      @ List.filter_map (fun v -> if List.exists (on v) tips then None else Some (Printf.sprintf "%s: its line does not reach its corner at %s" (between e) (at verts.(v).at))) e.ends)
    t.edges

(* The corners and the faces. A corner has the edges it says, and three at least. A face
   is closed off by its edges: at every corner an even number of them end, unless one of
   them runs out of the box. *)
let closed (t : X.t) =
  let verts = Array.of_list t.vertices in
  let ends = Array.make (Array.length verts) 0 in
  List.iter (fun (e : X.edge) -> List.iter (fun v -> ends.(v) <- ends.(v) + 1) e.ends) t.edges;
  let corners =
    List.concat
      (List.mapi
         (fun v (vx : X.vertex) ->
           if vx.degree <> ends.(v) then [ Printf.sprintf "the corner at %s says %d edges and has %d" (at vx.at) vx.degree ends.(v) ]
           else if ends.(v) < 3 then [ Printf.sprintf "the corner at %s has only %d edges" (at vx.at) ends.(v) ]
           else [])
         t.vertices)
  in
  let faces = List.sort_uniq compare (List.concat_map (fun (e : X.edge) -> [ fst e.between; snd e.between ]) t.edges) in
  let unclosed =
    List.filter_map
      (fun f ->
        let mine = List.filter (fun (e : X.edge) -> fst e.between = f || snd e.between = f) t.edges in
        let leaves = List.exists (fun (e : X.edge) -> (not e.closed) && List.length e.ends < 2) mine in
        let odd = List.filter (fun v -> List.length (List.concat_map (fun (e : X.edge) -> List.filter (( = ) v) e.ends) mine) mod 2 = 1) (List.init (Array.length verts) Fun.id) in
        match odd with v :: _ when not leaves -> Some (Printf.sprintf "the face %s is not closed off by its edges (at %s)" f (at verts.(v).at)) | _ -> None)
      faces
  in
  corners @ unclosed

(* Nothing missed. From points on the surface, a short step along it each of two ways:
   where the face changes, the place is found by halving, and an edge of those two faces
   must pass there. [across]: how many points across the box. *)
let missed h (t : X.t) ~across =
  let segments = List.concat_map (fun (e : X.edge) -> List.concat_map (fun line -> List.mapi (fun i a -> (a, List.nth line (i + 1), e.between)) (List.filteri (fun i _ -> i < List.length line - 1) line)) e.pieces) t.edges in
  let to_segment x (a, b, _) =
    let ab = X.sub b a in
    let l2 = X.dot ab ab in
    X.dist x (X.add a (X.scale (if l2 = 0. then 0. else Float.max 0. (Float.min 1. (X.dot (X.sub x a) ab /. l2))) ab))
  in
  let name i = Prog.face_name h.p i and inside = X.inside h.lo h.hi in
  let cell = Array.fold_left Float.max 0. (X.sub h.hi h.lo) /. float_of_int across in
  let found = ref [] in
  let from x =
    let r = probe h x in
    let n = X.scale (1. /. X.norm r.d.g) r.d.g in
    let u = X.cross n (if Float.abs n.(0) < 0.6 then [| 1.; 0.; 0. |] else [| 0.; 1.; 0. |]) in
    let u = X.scale (1. /. X.norm u) u in
    List.iter
      (fun dir ->
        match to_surface h (X.add x (X.scale (0.5 *. cell) dir)) with
        | Some y when inside y && (probe h y).face <> r.face && X.dot (probe h y).d.g n > 0. ->
            let rec halve a b k =
              if k = 0 || X.dist a b < 1e-10 *. h.diag then (a, b)
              else match to_surface h (X.scale 0.5 (X.add a b)) with Some m -> if (probe h m).face = r.face then halve m b (k - 1) else halve a m (k - 1) | None -> (a, b)
            in
            let a, b = halve x y 60 in
            if X.dist a b < 1e-6 *. h.diag then begin
              let fa = name (probe h a).face and fb = name (probe h b).face in
              let pair = if fa < fb then (fa, fb) else (fb, fa) in
              let near = List.fold_left (fun d ((_, _, bt) as s) -> if bt = pair then Float.min d (to_segment a s) else d) infinity segments in
              if near > 0.05 *. h.step then found := Printf.sprintf "%s | %s meet at %s with no edge there" fa fb (at a) :: !found
            end
        | _ -> ())
      [ u; X.cross n u ]
  in
  for i = 0 to across - 1 do
    for j = 0 to across - 1 do
      for k = 0 to across - 1 do
        let x0 = Array.mapi (fun a n -> h.lo.(a) +. ((float_of_int n +. 0.5) *. cell)) [| i; j; k |] in
        if inside x0 && off (probe h x0).d < cell then match to_surface h x0 with Some x when inside x -> from x | _ -> ()
      done
    done
  done;
  List.sort_uniq compare !found

(* ---- The models ---- *)

(* A model at each depth: the counts it should give with no flag, and an extraction that
   holds. The surface is sampled for missed edges at 64 cells only: it is the slow part.
   [hard]: a shape known to be beyond the extractor, which must then say so with a flag. *)
let extraction ?(hard = false) name ~lo ~hi g (v, e, closed_edges) depths =
  let h = hold ~lo ~hi g in
  List.iter
    (fun depth ->
      let t = X.extract ~depth ~lo ~hi g in
      let nv = List.length t.vertices and ne = List.length t.edges in
      let nc = List.length (List.filter (fun (e : X.edge) -> e.closed) t.edges) in
      let right = nv = v && ne = e && nc = closed_edges in
      let counts = Printf.sprintf "V=%d E=%d closed=%d flags=%d, expected V=%d E=%d closed=%d flags=0" nv ne nc (List.length t.flags) v e closed_edges in
      let cells = Printf.sprintf "%s: %d cells" name (1 lsl depth) in
      if hard then check (cells ^ ", right or flagged") (t.flags <> [] || right) counts
      else begin
        check cells (right && t.flags = []) counts;
        let faults = lines h t @ closed t @ if depth = 6 then missed h t ~across:24 else [] in
        check (cells ^ ", holds") (faults = []) (Printf.sprintf "%d faults: %s" (List.length faults) (String.concat "; " (List.filteri (fun i _ -> i < 3) faults)))
      end)
    depths

(* The sets ported from mk36, with its counts: also pruned against unpruned, which must
   agree at every sample. *)
let set (s : Models.Sets.set) =
  List.iter
    (fun (name, expected, _) ->
      let g = s.graph name in
      let p = Prog.compile g in
      let c = Prog.ctx p in
      let tree = Octree.build p c ~lo:s.lo ~hi:s.hi ~depth:6 and whole = Prog.whole p in
      let worst = ref 0. and n = 23 in
      for i = 0 to n do
        for j = 0 to n do
          for k = 0 to n do
            let x = Array.mapi (fun a t -> s.lo.(a) +. ((s.hi.(a) -. s.lo.(a)) *. (float_of_int t +. 0.37) /. float_of_int (n + 1))) [| i; j; k |] in
            worst := Float.max !worst (Float.abs (Prog.value p c (Octree.state tree x) x -. Prog.value p c whole x))
          done
        done
      done;
      check (name ^ ": pruned = unpruned") (!worst = 0.) (Printf.sprintf "differ by %g" !worst);
      extraction name ~lo:s.lo ~hi:s.hi g expected [ 5; 6; 7 ])
    s.expected

(* ---- A mesh ---- *)

(* Does the surface turn across an edge? Its normals are read all round the middle of
   its line, a thousandth of a step away. *)
let sharp h (e : X.edge) =
  let line = Array.of_list (List.hd e.pieces) in
  let n = Array.length line in
  let x, along = if n = 2 then (X.scale 0.5 (X.add line.(0) line.(1)), X.sub line.(1) line.(0)) else (line.(n / 2), X.sub line.((n / 2) + 1) line.((n / 2) - 1)) in
  let up = (probe h x).d.g in
  let unit a = X.scale (1. /. X.norm a) a in
  let u = unit (X.cross up along) and v = unit up in
  let normals =
    List.filter_map
      (fun k ->
        let t = float_of_int k *. Float.pi /. 4. in
        Option.map (fun y -> (probe h y).d.g) (to_surface h (X.add x (X.scale (1e-3 *. h.step) (X.add (X.scale (cos t) u) (X.scale (sin t) v))))))
      [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  in
  List.exists (fun a -> List.exists (fun b -> Surface.turn a b >= 1.) normals) normals

(* A mesh is closed, each edge with two triangles, one each way round. Its vertices are
   on the surface. The extraction's corners where the surface turns are all in it. Its
   creases are as long as the extraction's sharp edges, to a twentieth: none lost, none
   made up. And its triangles are even: the smallest angle is 45 degrees or more in
   half of them, and under 5 in fewer than one in a hundred. *)
let mesh name depth =
  let lo, hi, g = Models.Sets.find name in
  let g = g () in
  let h = hold ~lo ~hi g in
  let x = X.extract ~depth ~lo ~hi g in
  let m = Surface.mesh ~depth ~lo ~hi g x in
  let q = Surface.quality m in
  let length lines = List.fold_left (fun l line -> fst (List.fold_left (fun (l, last) p -> ((match last with Some q -> l +. X.dist p q | None -> l), Some p)) (l, None) line)) 0. lines in
  let creased = length (List.map (fun (a, b) -> [ a; b ]) (Surface.creases m)) and edges = length (List.concat_map (fun (e : X.edge) -> if sharp h e then e.pieces else []) x.edges) in
  let astray = Array.fold_left (fun w p -> Float.max w (off (probe h p).d)) 0. m.points in
  let faults =
    (if q.open_edges = 0 && q.crowded = 0 then [] else [ Printf.sprintf "not closed: %d edges with one triangle, %d with too many" q.open_edges q.crowded ])
    @ (if astray <= 1e-6 *. h.diag then [] else [ Printf.sprintf "a vertex is %g off the surface" astray ])
    @ (if fst m.corners = snd m.corners then [] else [ Printf.sprintf "%d of %d corners put in" (fst m.corners) (snd m.corners) ])
    @ (if Float.abs (creased -. edges) <= 0.05 *. edges then [] else [ Printf.sprintf "creases %g long, the extraction's sharp edges %g" creased edges ])
    @
    let n = Array.length q.angles in
    let thin = Array.fold_left (fun k a -> if a < 5. then k + 1 else k) 0 q.angles in
    if n > 0 && q.angles.(n / 2) >= 45. && 100 * thin < n then [] else [ Printf.sprintf "uneven: the smallest angle is %.3g deg at the middle, and under 5 in %d of %d triangles" (if n = 0 then 0. else q.angles.(n / 2)) thin n ]
  in
  check (Printf.sprintf "%s: %d cells, meshed" name (1 lsl depth)) (faults = []) (String.concat "; " faults)

(* A mesh to a tolerance is closed, has no triangle without area as written, has all
   the corners where the surface turns, and no triangle of it is further from the
   surface than the tolerance: at more places than the mesher itself looks, the
   middles of its sides and its own middle and sixths. Its file is one solid, binary,
   of the length its count of triangles says. *)
let mesh_to name tol =
  let lo, hi, g = Models.Sets.find name in
  let g = g () in
  let h = hold ~lo ~hi g in
  let x = X.extract ~depth:6 ~lo ~hi g in
  let m = Surface.mesh ~tol ~lo ~hi g x in
  let q = Surface.quality m in
  let far =
    Array.fold_left
      (fun w (a, b, c) ->
        let pa = m.points.(a) and pb = m.points.(b) and pc = m.points.(c) in
        let at u v t = X.add (X.scale u pa) (X.add (X.scale v pb) (X.scale t pc)) in
        List.fold_left (fun w p -> Float.max w (off (probe h p).d)) w [ at 0.5 0.5 0.; at 0. 0.5 0.5; at 0.5 0. 0.5; at (1. /. 3.) (1. /. 3.) (1. /. 3.); at (2. /. 3.) (1. /. 6.) (1. /. 6.); at (1. /. 6.) (2. /. 3.) (1. /. 6.); at (1. /. 6.) (1. /. 6.) (2. /. 3.) ])
      0. m.triangles
  in
  let file = Surface.stl m in
  let faults =
    (if q.open_edges = 0 && q.crowded = 0 then [] else [ Printf.sprintf "not closed: %d edges with one triangle, %d with too many" q.open_edges q.crowded ])
    @ (if q.empty = 0 then [] else [ Printf.sprintf "%d triangles without area" q.empty ])
    @ (if fst m.corners = snd m.corners then [] else [ Printf.sprintf "%d of %d corners put in" (fst m.corners) (snd m.corners) ])
    @ (if m.worst <= tol then [] else [ Printf.sprintf "said to be %g from the surface" m.worst ])
    @ (if far <= tol then [] else [ Printf.sprintf "a triangle is %g from the surface" far ])
    @ if String.length file = 84 + (50 * Array.length m.triangles) && Int32.to_int (String.get_int32_le file 80) = Array.length m.triangles then [] else [ "its file is not as long as its triangles say" ]
  in
  check (Printf.sprintf "%s: meshed to within %g, in %d triangles" name tol (Array.length m.triangles)) (faults = []) (String.concat "; " faults)

(* ---- Circuit boards ---- *)

(* A board's copper is held against its nets (lib/circuit.ml). A board that is right is
   said to be; one with a via left out has its net in two pieces; a trace run 0.1 from
   another net's pad is found too near, and how near; a pour cut in two by a trace is
   two islands, a corner of it that reaches nothing is dropped, and the net is open.
   And a board made solid goes with what its model's program writes, where it was made. *)
let boards () =
  let module C = Circuit in
  let board name = Circuit_board.made := []; ignore ((List.assoc name Models.Boards.all) ()); List.hd !Circuit_board.made in
  let found name = let _, _, pours, found = C.Check.check (board name) in (pours, found) in
  check "a board: every net in one piece, none too near" (snd (found "lights") = []) "something was found on lights";
  check "a board: its pours are one island each" (List.for_all (fun p -> C.Pour.islands p = (1, 1)) (fst (found "lights"))) "a pour of lights is in pieces";
  (match snd (found "blink-broken") with
   | [ C.Check.Near (p, q, d, _); C.Check.Open ("GND", [ _; _ ]) ] -> check "a board: a trace too near a pad, and a net in two" ((p.what, q.net) = ("D1.2", "VCC") && Float.abs (d -. 0.1) < 0.005) (Printf.sprintf "%s and %s, %g apart" p.what q.what d)
   | other -> check "a board: a trace too near a pad, and a net in two" false (Printf.sprintf "%d things found" (List.length other)));
  (match found "blink-cut" with
   | [ pour ], [ C.Check.Open ("GND", [ _; _ ]) ] -> check "a board: a pour cut in two, a corner of it dropped" (C.Pour.islands pour = (2, 3)) (let k, a = C.Pour.islands pour in Printf.sprintf "%d of %d islands kept" k a)
   | _ -> check "a board: a pour cut in two, a corner of it dropped" false "not one pour and one open net");
  (* how far apart two shapes are, from their numbers: a line beside an arc, two squares
     turned corner to corner on one line, an arc that ends short of a line it would cross *)
  let apart (f : C.Copper.form) (g : C.Copper.form) = fst C.Copper.(apart (Option.get (core f.shape)) (Option.get (core g.shape))) in
  let near what got want = check ("a board: " ^ what) (Float.abs (got -. want) < 1e-9) (Printf.sprintf "%.9g, not %.9g" got want) in
  let quarter = C.Copper.band (10., 0.) (10. *. sqrt 0.5, 10. *. sqrt 0.5) (0., 10.) 0.1 in
  near "a line 2 outside an arc, each 0.2 wide" (apart quarter (C.Copper.stadium (12., 0.) (12., 5.) 0.1)) 1.8;
  near "an arc that ends before the line it would cross" (apart quarter (C.Copper.stadium (-3., 9.) (-3., 11.) 0.1)) 2.8;
  near "two squares turned corner to corner" (apart (C.Copper.rounded (0., 0.) 1. 1. 0. 45.) (C.Copper.rounded (2., 0.) 1. 1. 0. 45.)) (2. -. sqrt 2.);
  near "two squares side by side on one line" (apart (C.Copper.rounded (0., 0.) 1. 1. 0. 0.) (C.Copper.rounded (1.5, 0.) 1. 1. 0. 0.)) 0.5;
  (* an opening is an edge of the board: a place is as far inside as from the nearest of them *)
  let holed = C.Board.make "holed" (C.Board.rect 20. 10.) in
  C.Board.opening holed (Sketch.polygon [ (8., 4.); (12., 4.); (12., 6.); (8., 6.) ]);
  near "a place 1 from an opening and 3 from the edge" (C.Board.inside holed (7., 5.)) 1.;
  check "a board: inside an opening is outside the board" (C.Board.inside holed (10., 5.) < 0.) "it is not";
  C.Board.arc holed ~net:"A" ~centre:(4., 5.) ~radius:2. 0. 360.;
  check "a board: a whole circle of trace is three arcs, joined" (match C.Check.check holed with 3, [ "A" ], _, [] -> true | _ -> false) "it is not one net in one piece";
  (* The same from the two fields, searched for in the plane: for shapes thrown at
     random, as far apart as their numbers say, and none that is near missed. *)
  let searched (f : C.Copper.form) (g : C.Copper.form) =
    let p = C.Copper.piece "p" ~net:"A" [ C.Copper.Front ] f and q = C.Copper.piece "q" ~net:"B" [ C.Copper.Front ] g in
    Option.map fst (C.Copper.near ~limit:3. p (C.Copper.reader (C.Copper.field p)) q (C.Copper.reader (C.Copper.field q)))
  in
  Random.init 7;
  let some () =
    let at () = (Random.float 6., Random.float 6.) in
    match Random.int 3 with
    | 0 -> let a = at () in let b = at () in C.Copper.stadium a b (0.05 +. Random.float 0.3)
    | 1 -> let c = at () in let w = 0.4 +. Random.float 2. in let h = 0.4 +. Random.float 2. in let r = Random.float 0.15 in C.Copper.rounded c w h r (Random.float 360.)
    | _ ->
        let cx, cy = at () in
        let big = 0.5 +. Random.float 3. in
        let from = Random.float 6.28 in
        let sweep = 0.3 +. Random.float 2. in
        let on t = (cx +. (big *. cos t), cy +. (big *. sin t)) in
        C.Copper.band (on from) (on (from +. (sweep /. 2.))) (on (from +. sweep)) (0.05 +. Random.float 0.2)
  in
  let unlike = ref 0 and worst = ref 0. and asked = ref 0 in
  for _ = 1 to 300 do
    let f = some () in
    let g = some () in
    let exact = apart f g in
    match searched f g with
    | Some d ->
        incr asked;
        if exact > 0. then (worst := Float.max !worst (Float.abs (d -. exact)); if Float.abs (d -. exact) > 4e-3 then incr unlike) else if d > 2e-3 then incr unlike
    | None -> if exact < 3. -. 4e-3 then incr unlike
  done;
  check "a board: two shapes as far apart by their fields as by their numbers" (!unlike = 0 && !asked > 100) (Printf.sprintf "%d of 300 unlike, the most %.4g off; %d found near" !unlike !worst !asked);
  (* copper of an outline of its own, an L: 0.15 from a trace of another net; and with
     a trace of its own net over it, one piece with it *)
  let own net y =
    let b = C.Board.make "own" (C.Board.rect 20. 10.) in
    C.Board.copper b ~net "the L" (Sketch.polygon [ (2., 2.); (6., 2.); (6., 3.); (3., 3.); (3., 6.); (2., 6.) ]);
    C.Board.trace b ~net:"A" ~width:0.2 [ (3.5, y); (8., y) ];
    let _, _, _, found = C.Check.check b in
    found
  in
  (match own "B" 3.25 with
   | [ C.Check.Near (_, _, d, _) ] -> check "a board: copper of an outline of its own, 0.15 from a trace" (Float.abs (d -. 0.15) < 3e-3) (Printf.sprintf "%g apart" d)
   | other -> check "a board: copper of an outline of its own, 0.15 from a trace" false (Printf.sprintf "%d things found" (List.length other)));
  check "a board: and a trace of its net over it is one piece with it" (own "A" 2.9 = []) "it is not";
  (* the board's edge: a trace along it, 0.15 in, where 0.2 is asked; and one that runs out over it *)
  let edged points =
    let b = C.Board.make "edged" (C.Board.rect 20. 10.) in
    C.Board.trace b ~net:"A" ~width:0.2 points;
    let _, _, _, found = C.Check.check b in
    found
  in
  (match edged [ (5., 0.25); (15., 0.25) ] with
   | [ C.Check.Edge (_, d, _) ] -> check "a board: a trace 0.15 from the edge, where 0.2 is asked" (Float.abs (d -. 0.15) < 3e-3) (Printf.sprintf "%g from it" d)
   | other -> check "a board: a trace 0.15 from the edge, where 0.2 is asked" false (Printf.sprintf "%d things found" (List.length other)));
  (match edged [ (18., 5.); (20.5, 5.) ] with
   | [ C.Check.Edge (_, d, _) ] -> check "a board: a trace that runs out over the edge" (Float.abs (d +. 0.6) < 3e-3) (Printf.sprintf "%g from it" d)
   | other -> check "a board: a trace that runs out over the edge" false (Printf.sprintf "%d things found" (List.length other)));
  check "a board: a trace 0.3 from the edge is let be" (edged [ (5., 0.4); (15., 0.4) ] = []) "it is found";
  Circuit_board.made := [];
  let built = Emit.built ((List.assoc "blink" Models.Boards.all) ()) in
  check "a board: it goes with its model" (List.map (fun (b : C.Board.t) -> b.name) built.boards = [ "blink" ]) (string_of_int (List.length built.boards));
  (* and where it is kept in the model is where its copper is drawn: the frame it was kept in *)
  let kept = C.Board.make "kept" (C.Board.rect 10. 10.) in
  Graph.within (Frame.translate (1., 2., 3.)) (fun () -> C.Board.keep ~thickness:0.8 kept);
  let said = Json.to_string (C.Page.data kept) in
  let has part = let n = String.length part in let rec at i = i + n <= String.length said && (String.sub said i n = part || at (i + 1)) in at 0 in
  check "a board: its plane in the model is the frame it was kept in" (kept.frame.t = [| 1.; 2.; 3. |] && kept.thickness = 0.8 && has "\"o\":[1,2,3]") said;
  (* a part's body is a box on the board: on the front it stands on the face, on the back
     it hangs under it, and one of no known size has none *)
  let boxed = C.Footprint.make ~body:((-1., -0.5, 0.), (1., 0.5, 0.8)) "boxed" [ C.Footprint.pad "1" Rect (0., 0.) (0.5, 0.5) ] in
  let bodied = C.Board.make "bodied" (C.Board.rect 20. 10.) in
  C.Board.part bodied "A" ~footprint:boxed (5., 5.);
  C.Board.part bodied "B" ~footprint:boxed ~back:true ~turn:90. (10., 5.);
  C.Board.part bodied "C" ~footprint:C.Footprint.r0603 (15., 5.);
  let numbers = C.Page.bodies bodied in
  let at i = List.nth numbers i in
  check "a board: a part's body is a box on its side of the board" (List.length numbers = 30 && (at 8, at 11) = (0.8, 0.) && Float.abs (at 23 +. 0.8) < 1e-6 && Float.abs (at 26 +. 1.6) < 1e-6) (String.concat " " (List.map (Printf.sprintf "%g") numbers));
  check "a board: a footprint is written out as code with its body" (let code = C.Footprint.code boxed in let has part = let n = String.length part in let rec go i = i + n <= String.length code && (String.sub code i n = part || go (i + 1)) in go 0 in has "~body:(((-1.), (-0.5), 0.), (1., 0.5, 0.8))" && has "let boxed =") (C.Footprint.code boxed);
  Circuit_board.made := []

(* ---- The shader's gates ---- *)

(* The page's shader skips a gate where its table says every select in it keeps its
   chain so far (Shader). That must leave the field as it is: at random points the value
   and the face, with the skipped selects made to keep their chain, are the model's.
   Half the points are a hair from a cell's wall and read the cell across it, as the
   shader may: it finds a point's cell in 32-bit arithmetic. *)
let gates name least =
  let lo, hi, g = Models.Sets.find name in
  let h = hold ~lo ~hi (g ()) in
  let bodies = Joined.find h.p in
  let s = Shader.model ~least h.p bodies in
  let grid = Shader.decisions h.p bodies h.c s ~lo ~hi in
  let of_select = Hashtbl.create 64 in
  Array.iteri (fun k links -> List.iter (fun (n, so_far) -> Hashtbl.replace of_select n (k, so_far)) links) s.gates;
  Random.init 11;
  let points = 20_000 and wrong = ref 0 and skipped = ref 0 in
  for i = 1 to points do
    let x = Array.init 3 (fun a -> lo.(a) +. Random.float (hi.(a) -. lo.(a))) in
    let read = Array.copy x in
    if i mod 2 = 0 then begin
      let a = Random.int 3 and hair = 1e-5 *. grid.cell in
      let wall = lo.(a) +. (grid.cell *. Float.round ((x.(a) -. lo.(a)) /. grid.cell)) in
      x.(a) <- Float.max lo.(a) (Float.min hi.(a) (wall +. hair));
      read.(a) <- wall -. hair
    end;
    let force n = match Hashtbl.find_opt of_select n with Some (k, so_far) when not (Shader.enters grid s read k) -> incr skipped; so_far | _ -> 0 in
    let model = probe h x and shader = Prog.eval ~force h.p h.c h.whole x in
    if model.d.v <> shader.d.v || model.face <> shader.face then incr wrong
  done;
  check
    (Printf.sprintf "%s: the shader's %d gates of %d operations or more" name (Array.length s.gates) least)
    (!wrong = 0 && !skipped > 0)
    (Printf.sprintf "the field differs at %d of %d points; %d selects were skipped" !wrong points !skipped)

(* ---- A pulled sketch's chamfers ---- *)

(* A square of 20 pulled 10 high, its upper edge chamfered by 2 and its lower by 1: at
   random points the solid is the one that description gives, and between random pairs
   of points the field never changes by more than they are apart. *)
let chamfers () =
  let square = Sketch.polygon [ (-10., -10.); (10., -10.); (10., 10.); (-10., 10.) ] in
  let p = Prog.compile (Sketch.extrude ~chamfer_lo:1. ~chamfer_hi:2. ~lo:0. ~hi:10. square) in
  let c = Prog.ctx p and whole = Prog.whole p in
  Random.init 3;
  let point () = [| Random.float 26. -. 13.; Random.float 26. -. 13.; Random.float 14. -. 2. |] in
  let wrong = ref 0 and steep = ref 0 in
  for _ = 1 to 20_000 do
    let x = point () and y = point () in
    let v = Prog.value p c whole x in
    let d = Float.max (Float.abs x.(0)) (Float.abs x.(1)) -. 10. in
    let inside = d <= 0. && x.(2) >= 0. && x.(2) <= 10. && d +. x.(2) -. 8. <= 0. && d +. 1. -. x.(2) <= 0. in
    if Float.abs v > 1e-9 && inside <> (v < 0.) then incr wrong;
    let apart = sqrt (((x.(0) -. y.(0)) ** 2.) +. ((x.(1) -. y.(1)) ** 2.) +. ((x.(2) -. y.(2)) ** 2.)) in
    if Float.abs (v -. Prog.value p c whole y) > apart +. 1e-9 then incr steep
  done;
  check "chamfers: a pulled square with both edges cut" (!wrong = 0 && !steep = 0) (Printf.sprintf "the solid is wrong at %d points; the field is too steep between %d pairs" !wrong !steep)

(* ---- The regions the page draws ---- *)

(* A region's program (Region) must give the model's field anywhere in its box: at
   random points of every region, it does. The regions must also hold all the surface:
   a point where the field is nearly nothing is in one of them. *)
let regions name most =
  let lo, hi, g = Models.Sets.find name in
  let h = hold ~lo ~hi (g ()) in
  let r = Region.cut ~most h.p (Joined.find h.p) h.c ~lo ~hi in
  Random.init 5;
  let within (g : Region.region) x = x.(0) >= g.lo.(0) && x.(1) >= g.lo.(1) && x.(2) >= g.lo.(2) && x.(0) <= g.hi.(0) && x.(1) <= g.hi.(1) && x.(2) <= g.hi.(2) in
  let wrong = ref 0 and furthest = ref 0. and outside = ref 0 and hollow = ref 0 and lost = ref 0 in
  let inside (lo, hi, _) x = x.(0) >= lo.(0) && x.(1) >= lo.(1) && x.(2) >= lo.(2) && x.(0) <= hi.(0) && x.(1) <= hi.(1) && x.(2) <= hi.(2) in
  (* the cells kept as solid are solid all through *)
  Array.iter
    (fun (l, u, _) ->
      for _ = 1 to 50 do
        let x = Array.init 3 (fun a -> l.(a) +. Random.float (u.(a) -. l.(a))) in
        if not ((probe h x).d.v < 0.) then incr hollow
      done)
    r.solid;
  Array.iter
    (fun (g : Region.region) ->
      for _ = 1 to 200 do
        let x = Array.init 3 (fun a -> g.lo.(a) +. Random.float (g.hi.(a) -. g.lo.(a))) in
        let model = (probe h x).d.v in
        let off = Float.abs (Region.run (fst r.shaders.(g.shader)) x -. model) in
        furthest := Float.max !furthest off;
        if not (off <= 1e-9 *. (1. +. Float.abs model)) then incr wrong
      done)
    r.regions;
  for _ = 1 to 20_000 do
    let x = Array.init 3 (fun a -> lo.(a) +. Random.float (hi.(a) -. lo.(a))) in
    let v = (probe h x).d.v in
    if Float.abs v < 1e-3 *. h.diag && not (Array.exists (fun g -> within g x) r.regions) then incr outside;
    (* and every point of the solid is in a region or in one of those cells *)
    if v < 0. && not (Array.exists (fun g -> within g x) r.regions || Array.exists (fun b -> inside b x) r.solid) then incr lost
  done;
  check
    (Printf.sprintf "%s: cut into %d regions, %d shaders" name (Array.length r.regions) (Array.length r.shaders))
    (!wrong = 0 && !outside = 0 && !hollow = 0 && !lost = 0 && Array.length r.regions > 0)
    (Printf.sprintf "the field differs at %d points of the regions, by %g at most; %d points near the surface are in no region; %d points of its %d solid cells are not in the solid; %d points of the solid are in no region or cell"
       !wrong !furthest !outside !hollow (Array.length r.solid) !lost)

(* ---- What a model shows from outside ---- *)

(* An STL file is read the same in its two forms: one triangle, its three corners and
   its normal, the file's own or, where the file gives none, the triangle's. And an
   asset shown inside a frame is put in that frame, with the one it is given in. *)
let assets () =
  let corners = [ (0., 0., 0.); (2., 0., 0.); (0., 3., 0.) ] in
  let file name text = let path = Filename.temp_file name ".stl" in Out_channel.with_open_bin path (fun oc -> output_string oc text); path in
  let ascii normal =
    Printf.sprintf "solid one\n facet normal %s\n  outer loop\n%s  endloop\n endfacet\nendsolid one\n" normal (String.concat "" (List.map (fun (x, y, z) -> Printf.sprintf "   vertex %g %g %g\n" x y z) corners))
  in
  let binary =
    let b = Buffer.create 134 in
    Buffer.add_string b (String.make 80 ' ');
    Buffer.add_int32_le b 1l;
    List.iter (fun v -> Buffer.add_int32_le b (Int32.bits_of_float v)) ([ 0.; 0.; 1. ] @ List.concat_map (fun (x, y, z) -> [ x; y; z ]) corners);
    Buffer.add_string b "\000\000";
    Buffer.contents b
  in
  let right (m : Asset.mesh) = m.points = [| 0.; 0.; 0.; 2.; 0.; 0.; 0.; 3.; 0. |] && m.triangles = [| 0; 1; 2 |] && Array.for_all2 (fun a b -> Float.abs (a -. b) < 1e-6) m.normals [| 0.; 0.; 1.; 0.; 0.; 1.; 0.; 0.; 1. |] in
  List.iter
    (fun (name, text) ->
      let path = file name text in
      let m = Asset.read path in
      Sys.remove path;
      check ("assets: an STL file, " ^ name) (right m) (Printf.sprintf "%d points, %d corners" (Array.length m.points / 3) (Array.length m.triangles)))
    [ ("as text", ascii "0 0 1"); ("as text, no normal given", ascii "0 0 0"); ("in binary", binary) ];
  check "colours: one is read as written, and what is not one is refused" (Colour.of_string "#ff8000" = (1., 128. /. 255., 0.) && (try ignore (Colour.of_string "orange"); false with Failure _ -> true)) "";
  Asset.shown := [];
  ignore (Graph.within (Frame.translate (1., 2., 3.)) (fun () -> Asset.show ~frame:(Frame.translate (10., 0., 0.)) "there.step"; Solid.sphere "s" (0., 0., 0.) 1.));
  check "assets: one shown in a frame is put there" (match !Asset.shown with [ a ] -> a.frame.t = [| 11.; 2.; 3. |] && a.name = "there" | _ -> false) "";
  Asset.shown := []

(* The shapes the README says are still beyond the extractor at some cell size. *)
let hard = [ "villarceau"; "plane-through-edge"; "coplanar-boxes" ]

(* A traced outline written as what it is (Dxf): points along a line give the line, along
   an arc the arc with its centre, radius and ends, round a circle the circle; and an
   edge that is two arcs run into each other, as round corners do, gives the two, cut
   where they touch. *)
let outlines () =
  let near a b = Float.abs (a -. b) < 1e-3 in
  let on (cx, cy) r from to_ n = List.init (n + 1) (fun i -> let t = (from +. ((to_ -. from) *. float_of_int i /. float_of_int n)) *. Float.pi /. 180. in (cx +. (r *. cos t), cy +. (r *. sin t))) in
  let said = function Dxf.Line _ -> "a line" | Dxf.Arc (_, r, a, b) -> Printf.sprintf "an arc of %.3f from %.2f to %.2f" r a b | Dxf.Circle (_, r) -> Printf.sprintf "a circle of %.3f" r in
  let all es = String.concat ", " (List.map said es) in
  let is name ~closed pts right =
    let es, exact = Dxf.traced ~closed pts in
    check ("outlines: " ^ name) (exact && right es) (all es)
  in
  is "points in a line are the line" ~closed:false (List.init 9 (fun i -> (float_of_int i, 2. *. float_of_int i))) (function [ Dxf.Line ((0., 0.), (8., 16.)) ] -> true | _ -> false);
  is "points on an arc are the arc, whichever way it was traced" ~closed:false (List.rev (on (3., -2.) 4. 30. 120. 12))
    (function [ Dxf.Arc ((cx, cy), r, a, b) ] -> near cx 3. && near cy (-2.) && near r 4. && near a 30. && near b 120. | _ -> false);
  is "points round a circle are the circle" ~closed:true (on (1., 1.) 2.5 0. 350. 35) (function [ Dxf.Circle ((cx, cy), r) ] -> near cx 1. && near cy 1. && near r 2.5 | _ -> false);
  (* an S: a quarter about (0, 4) up to (4, 4), then one about (8, 4) the other way *)
  is "two arcs that run into each other are cut where they touch" ~closed:false (on (0., 4.) 4. (-90.) (-3.) 14 @ on (8., 4.) 4. 177. 90. 14)
    (function
      | [ Dxf.Arc ((ax, ay), r, a0, a1); Dxf.Arc ((bx, by), r', b0, b1) ] ->
          near ax 0. && near ay 4. && near r 4. && near a0 270. && Float.abs (a1 -. 360.) < 0.05 || near a1 0. && near bx 8. && near by 4. && near r' 4. && near b0 90. && Float.abs (b1 -. 180.) < 0.05
      | _ -> false);
  let text = Dxf.written [ Dxf.Line ((0., 0.), (1., 0.)); Dxf.Circle ((0., 0.), 2.) ] in
  let has part = let n = String.length part in let rec at i = i + n <= String.length text && (String.sub text i n = part || at (i + 1)) in at 0 in
  check "outlines: the file has its entities, in millimetres, and ends" (has "$INSUNITS\n70\n4\n" && has "0\nLINE\n" && has "0\nCIRCLE\n" && has "40\n2.000000\n" && has "0\nEOF\n") text

(* A cut through a model (Render.cut_cells, Render.section): a ball of radius 2 inside a
   box cut through its middle is a disc of that radius, and beside its middle a smaller
   one; what is outside the model's box is air; and the picture has the cut's own shape. *)
let cuts () =
  let ball = Graph.feature ("test", 0, 0, 0) "ball" (fun () -> Solid.sphere "ball" (0., 0., 0.) 2.) in
  let lo = [| -3.; -3.; -3. |] and hi = [| 3.; 3.; 3. |] in
  let cut at : Render.cut = { axis = 2; at; left = -4.; right = 4.; bottom = -4.; top = 4. } in
  let n = 200 in
  let solid at = Array.fold_left (fun k id -> if id >= 0 then k + 1 else k) 0 (Render.cut_cells ~cols:n ~rows:n (cut at) ~part:(fun f -> f) ~lo ~hi ball) in
  let area at = float_of_int (solid at) *. (8. /. float_of_int n) ** 2. in
  check "cuts: a ball cut through its middle is a disc of its radius" (Float.abs (area 0. -. (Float.pi *. 4.)) < 0.1) (Printf.sprintf "%.3f, not %.3f" (area 0.) (Float.pi *. 4.));
  check "cuts: beside its middle, a smaller one" (Float.abs (area 1.2 -. (Float.pi *. (4. -. 1.44))) < 0.1) (Printf.sprintf "%.3f" (area 1.2));
  check "cuts: past it, nothing" (solid 2.5 = 0 && solid 5. = 0) (Printf.sprintf "%d and %d cells" (solid 2.5) (solid 5.));
  let im, parts = Render.section ~w:200 { (cut 0.) with bottom = -2.; top = 2. } ~part:(fun f -> f) ~colour:(fun _ -> (0.2, 0.5, 0.8)) ~lo ~hi ball in
  check "cuts: the picture is as wide and as high as the cut, with room for its scale"
    (im.w > 200 && im.w < 300 && im.h > 100 && im.h < 140) (Printf.sprintf "%d by %d" im.w im.h);
  check "cuts: its one part is said, with where it is"
    (match parts with [ (_, (u0, u1), (v0, v1)) ] -> Float.abs (u0 +. 2.) < 0.05 && Float.abs (u1 -. 2.) < 0.05 && Float.abs (v0 +. 2.) < 0.05 && Float.abs (v1 -. 2.) < 0.05 | _ -> false)
    (String.concat "; " (List.map (fun (id, (u0, u1), (v0, v1)) -> Printf.sprintf "%d: %.2f..%.2f, %.2f..%.2f" id u0 u1 v0 v1) parts))

(* Two solids' clearance (Clash): balls of radius 2 whose middles are 4.5 apart are 0.5
   apart; 3.6 apart, 0.4 into each other; 4 apart, they touch; and far apart, nothing is
   said. A point of a shown part inside a solid is found, with how deep. *)
let clashes () =
  let lo = [| -8.; -8.; -8. |] and hi = [| 8.; 8.; 8. |] in
  let ball x = match Clash.solid ~lo ~hi (Solid.sphere "ball" (x, 0., 0.) 2.) with Some s -> s | None -> failwith "a ball has a surface" in
  let said = function Some (c, (at : float array)) -> Printf.sprintf "%.4f at (%.2f, %.2f, %.2f)" c at.(0) at.(1) at.(2) | None -> "nothing" in
  let is name r right = check ("clashes: " ^ name) (right r) (said r) in
  is "two balls half a millimetre apart" (Clash.clearance ~limit:1. (ball 0.) (ball 4.5)) (function Some (c, at) -> Float.abs (c -. 0.5) < 0.01 && Float.abs (at.(0) -. 2.25) < 0.05 | None -> false);
  is "two balls 0.4 into each other" (Clash.clearance (ball 0.) (ball 3.6)) (function Some (c, _) -> Float.abs (c +. 0.4) < 0.01 | None -> false);
  is "two balls that touch" (Clash.clearance (ball 0.) (ball 4.)) (function Some (c, _) -> Float.abs c < 0.01 | None -> false);
  is "two balls further apart than was asked about" (Clash.clearance ~limit:0.3 (ball 0.) (ball 4.5)) (fun r -> r = None);
  is "two balls nowhere near" (Clash.clearance (ball (-5.)) (ball 5.)) (fun r -> r = None);
  is "a shown part's point inside a solid" (Clash.against (ball 0.) [ [| 5.; 0.; 0. |]; [| 1.5; 0.; 0. |]; [| 0.; 3.; 0. |] ]) (function Some (c, at) -> Float.abs (c +. 0.5) < 1e-6 && at.(0) = 1.5 | None -> false);
  is "a shown part's points all away from a solid" (Clash.against (ball 0.) [ [| 5.; 0.; 0. |]; [| 0.; 3.; 0. |] ]) (fun r -> r = None)

(* The builder that carries the compiler (embed/embed.ml): of a project of two files, one
   referring to the other, it lists the functions that build a solid on their own and
   no others, and builds one into the solid its code says; kept running, it builds one
   after another and says which it has not; and a file that does not compile stops it,
   with the place said. *)
let builder () =
  let embed = "../embed/embed.bc.exe" and kernel = "../lib/.mk52.objs/byte" in
  let dir = Filename.temp_file "mk52" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  let project = Filename.concat dir "lamp" in
  Sys.mkdir project 0o755;
  let write name text = Out_channel.with_open_bin (Filename.concat project name) (fun oc -> output_string oc text) in
  write "size.ml" "let radius = 2.\nlet double x = 2. *. x\n";
  write "base.ml" "open Mk52\n\nlet graph () = Graph.feature __POS__ \"ball\" (fun () -> Solid.sphere \"ball\" (0., 0., 0.) Size.radius)\nlet scaled k () = Solid.sphere \"ball\" (0., 0., 0.) k\nlet not_a_solid () = 3\n";
  let run ?(input = "") args =
    let out = Filename.temp_file "mk52" ".out" and err = Filename.temp_file "mk52" ".err" and inp = Filename.temp_file "mk52" ".in" in
    Out_channel.with_open_bin inp (fun oc -> output_string oc input);
    let code = Sys.command (Printf.sprintf "%s -I %s %s < %s > %s 2> %s" embed kernel (String.concat " " (List.map Filename.quote args)) inp out err) in
    let read p = let s = In_channel.with_open_bin p In_channel.input_all in Sys.remove p; s in
    Sys.remove inp;
    (code, read out, read err)
  in
  let code, listed, err = run [ project ] in
  check "builder: a project's functions that build a solid, and no others" (code = 0 && listed = "base.graph\n") (Printf.sprintf "%d %S %S" code listed err);
  let code, built, err = run [ project; "base.graph" ] in
  let value (built : Emit.built) x = let p = Prog.compile built.graph in Prog.value p (Prog.ctx p) (Prog.whole p) x in
  check "builder: one of them built is the solid its code says"
    (code = 0 && (match (Marshal.from_string built 0 : Emit.built) with b -> Float.abs (value b [| 5.; 0.; 0. |] -. 3.) < 1e-9 && List.map (fun (f : Graph.feature) -> f.fname) b.features = [ "ball" ] | exception _ -> false))
    (Printf.sprintf "%d, %d bytes, %S" code (String.length built) err);
  let file = Filename.concat dir "built" in
  let code, said, _ = run ~input:(Printf.sprintf "list\nbuild base.graph %s\nbuild base.nothing %s\n" file file) [ project; "-serve" ] in
  let lines = List.filter (fun l -> not (String.starts_with ~prefix:"child " l)) (String.split_on_char '\n' (String.trim said)) in
  check "builder: kept running, it lists, builds one, and says it has not the other" (code = 0 && lines = [ "ready"; "base.graph"; "."; "ok"; "failed" ] && Sys.file_exists file) (String.concat " | " lines);
  write "wrong.ml" "let wrong () = Mk52.Solid.sphere \"ball\" (0., 0., 0.) \"two\"\n";
  let code, _, err = run [ project ] in
  let has part = let n = String.length part in let rec at i = i + n <= String.length err && (String.sub err i n = part || at (i + 1)) in at 0 in
  check "builder: a file that does not compile is said, with the place" (code <> 0 && has "wrong.ml\", line 1" && has "string") err

(* Faces of no thickness. A ray is marched past one to the surface behind it
   (Render.marched). A pocket cut exactly level with a plate's top leaves one across its
   mouth, which is found with its faces, and a plate alone has none (Skin). And a profile
   turned about the axis it is closed along is whole across the axis: the field there is
   well inside, not nothing (Sketch.revolve). *)
let skins () =
  (* along a ray: a skin at 5, where the field only touches zero, and a solid from 8 *)
  let value t = Float.min (Float.abs (t -. 5.)) (8. -. t) in
  let found = Render.marched ~eps:1e-3 ~step:1. ~from:0. ~until:20. value in
  check "skins: a ray goes past a skin to the surface behind it" (match found with Some t -> Float.abs (t -. 8.) < 0.01 | None -> false) (match found with Some t -> string_of_float t | None -> "nothing");
  let inside = Render.marched ~eps:1e-3 ~step:1. ~from:9. ~until:20. (fun t -> Float.max (8. -. t) (t -. 12.)) in
  check "skins: from inside a solid a ray still meets its far side" (match inside with Some t -> Float.abs (t -. 12.) < 0.01 | None -> false) (match inside with Some t -> string_of_float t | None -> "nothing");
  let plate () = Sketch.extrude ~lo:0. ~hi:6. (Sketch.circle "edge" (0., 0.) 20.) in
  let pocket = Graph.subtract (plate ()) (Sketch.extrude ~lo:3. ~hi:6. (Sketch.circle "pocket" (0., 0.) 8.)) in
  let lo = [| -21.7; -21.6; -2.1 |] and hi = [| 21.8; 21.9; 8.4 |] in
  let said l = String.concat "; " (List.map (fun (f : Skin.found) -> Printf.sprintf "%s | %s in %d cells at z = %.3f" (fst f.between) (snd f.between) f.cells f.at.(2)) l) in
  let found = Skin.find ~lo ~hi pocket in
  check "skins: a pocket cut level with the top leaves one across its mouth, and it is found"
    (found <> [] && List.for_all (fun (f : Skin.found) -> Float.abs (f.at.(2) -. 6.) < 1e-6) found && List.exists (fun (f : Skin.found) -> f.hi.(0) -. f.lo.(0) > 10.) found)
    (said found);
  check "skins: a plate alone has none" (Skin.find ~lo ~hi (plate ()) = []) (said (Skin.find ~lo ~hi (plate ())));
  let deeper = Graph.subtract (plate ()) (Sketch.extrude ~lo:3. ~hi:7. (Sketch.circle "pocket" (0., 0.) 8.)) in
  check "skins: nor has the pocket cut on past the top" (Skin.find ~lo ~hi deeper = []) (said (Skin.find ~lo ~hi deeper));
  (* a rod turned from a profile closed along the axis: x 0..5, z 0..10 *)
  let rod = Sketch.revolve Sketch.(start (0., 0.) |> line "foot" (5., 0.) |> line "side" (5., 10.) |> line "head" (0., 10.) |> close ~name:"axis") in
  let p = Prog.compile rod in
  let v x = Prog.value p (Prog.ctx p) (Prog.whole p) x in
  check "skins: a profile closed along its axis is whole across it, and its surface where it was"
    (v [| 0.; 0.; 5. |] < -1. && Float.abs (v [| 5.; 0.; 5. |]) < 1e-9 && Float.abs (v [| 2.; 0.; 5. |] +. 3.) < 1e-9 && Float.abs (v [| 0.; 7.; 5. |] -. 2.) < 1e-9 && Float.abs (v [| 1.; 0.; 10. |]) < 1e-9)
    (Printf.sprintf "on the axis %.4f, on the side %.2g, 3 inside it %.4f, 2 outside it %.4f, on the head %.2g" (v [| 0.; 0.; 5. |]) (v [| 5.; 0.; 5. |]) (v [| 2.; 0.; 5. |]) (v [| 0.; 7.; 5. |]) (v [| 1.; 0.; 10. |]))

(* ---- Bodies switched off ---- *)

(* The page takes a body out of the picture by a switch in the shaders, not by a new
   model (Joined). Seven bodies: a block, three balls inside it, a knob that sticks out
   of its side, a lid on its top face and a hollow ball beside it. With every set of them
   switched off, what the page draws must be the model made of the others: a region's
   program and the one shader under its table of gates are below zero exactly where
   that model is, with its value there, and never nearer zero than it elsewhere; and
   the regions hold all its surface, the regions and the solid cells all its inside.

   The grain is where the octree would give it up were its decisions not made to hold
   for this: in a cell 2.75 across whose middle is 3.6 inside the block, far enough for
   the block to be below the grain all over the cell, and which the grain's surface
   passes through. Without that care (Octree.prune, [joins]) this fails. *)
let bodies () =
  let square r = Sketch.polygon [ (-.r, -.r); (r, -.r); (r, r); (-.r, r) ] in
  let parts =
    [| Sketch.extrude ~lo:(-16.) ~hi:16. (square 16.); Solid.sphere "pearl" (0., 0., 0.) 3.; Solid.sphere "seed" (9., 9., 9.) 2.; Solid.sphere "grain" (10.47, 3.28, 3.28) 1.5; Solid.sphere "knob" (18., 0., 0.) 6.;
       Sketch.extrude ~lo:16. ~hi:19. (square 12.); Graph.subtract (Solid.sphere "ball" (0., -19., 0.) 2.6) (Solid.sphere "hollow" (0., -19., 0.) 1.3) |]
  in
  let join = function [] -> Graph.raw_piece "" (Expr.const Joined.far) | first :: rest -> List.fold_left Graph.union first rest in
  let lo = [| -22.; -22.; -22. |] and hi = [| 22.; 22.; 22. |] in
  let h = hold ~lo ~hi (join (Array.to_list parts)) in
  let f = Joined.find h.p in
  Random.init 23;
  let point lo hi = Array.init 3 (fun a -> lo.(a) +. Random.float (hi.(a) -. lo.(a))) in
  let value (h : held) n x = Prog.start h.p h.c ~full:false h.whole; Prog.ev h.p h.c x n; h.c.v.(h.p.last.(n)) in
  (* which part each body is: the one with its values *)
  let alone = Array.map (hold ~lo ~hi) parts and probes = List.init 8 (fun _ -> point lo hi) in
  let part_of = Array.map (fun (b : Joined.t) -> List.find_opt (fun i -> List.for_all (fun x -> value h b.node x = value alone.(i) alone.(i).p.root x) probes) (List.init (Array.length parts) Fun.id)) f.bodies in
  let found = List.sort compare (Array.to_list part_of) = List.init (Array.length parts) Option.some in
  check "bodies: what a model's unions join" found (Printf.sprintf "%d bodies" (Array.length f.bodies));
  if found then begin
    let r = Region.cut ~most:300 h.p f h.c ~lo ~hi in
    let s = Shader.model ~least:8 h.p f in
    let grid = Shader.decisions h.p f h.c s ~lo ~hi in
    let of_select = Hashtbl.create 16 in
    Array.iteri (fun k links -> List.iter (fun (n, so_far) -> Hashtbl.replace of_select n (k, so_far)) links) s.gates;
    let within l u x = x.(0) >= l.(0) && x.(1) >= l.(1) && x.(2) >= l.(2) && x.(0) <= u.(0) && x.(1) <= u.(1) && x.(2) <= u.(2) in
    let wrong = ref 0 and gated = ref 0 and skipped = ref 0 and outside = ref 0 and lost = ref 0 and hollow = ref 0 in
    for set = 0 to (1 lsl Array.length parts) - 1 do
      let off b = set land (1 lsl Option.get part_of.(b)) <> 0 in
      let left = hold ~lo ~hi (join (List.filteri (fun i _ -> set land (1 lsl i) = 0) (Array.to_list parts))) in
      let model x = Prog.value left.p left.c left.whole x in
      (* drawn right: the model's value where either is inside, and no nearer zero elsewhere *)
      let right drawn model = if Float.min drawn model <= 0. then Float.abs (drawn -. model) <= 1e-9 *. (1. +. Float.abs model) else drawn >= model -. 1e-9 in
      Array.iter (fun (g : Region.region) -> for _ = 1 to 40 do let x = point g.lo g.hi in if not (right (Region.run ~off (fst r.shaders.(g.shader)) x) (model x)) then incr wrong done) r.regions;
      Array.iter (fun (l, u, b) -> if not (off b) then for _ = 1 to 20 do if not (model (point l u) < 0.) then incr hollow done) r.solid;
      (* the one shader, as its chains run under the table: a link in a gate not entered keeps its chain so far *)
      let rec chained x n =
        match f.of_node.(n) with
        | -1 -> (
            match Hashtbl.find_opt of_select n with
            | Some (k, so_far) when not (Shader.enters grid s x k) -> incr skipped; chained x h.p.inputs.(n).(so_far)
            | _ -> Float.min (chained x h.p.inputs.(n).(1)) (chained x h.p.inputs.(n).(2)))
        | b -> if off b then Joined.far else value h n x
      in
      for _ = 1 to 3000 do
        let x = point lo hi in
        let v = model x in
        if not (right (chained x h.p.root) v) then incr gated;
        let region = Array.exists (fun (g : Region.region) -> within g.lo g.hi x) r.regions in
        if Float.abs v < 1e-3 *. h.diag && not region then incr outside;
        if v < 0. && not (region || Array.exists (fun (l, u, b) -> (not (off b)) && within l u x) r.solid) then incr lost
      done
    done;
    check
      (Printf.sprintf "bodies: any of them switched off, in %d regions and under %d gates" (Array.length r.regions) (Array.length s.gates))
      (!wrong = 0 && !gated = 0 && !outside = 0 && !lost = 0 && !hollow = 0 && !skipped > 0 && Array.length s.gates > 0)
      (Printf.sprintf "a region's field is wrong at %d points and the shader's at %d; %d points of the surface are in no region, %d of the solid in nothing, %d of a solid cell outside; %d links were skipped" !wrong !gated !outside !lost
         !hollow !skipped)
  end;
  (* what a switch may take out: a feature that is a body whole, not one inside a body *)
  let pos = ("test", 0, 0, 0) in
  let made =
    Graph.union
      (Graph.feature pos "peg" (fun () -> Solid.sphere "peg" (-3., 0., 0.) 1.))
      (Graph.feature pos "cup" (fun () -> Graph.subtract (Solid.sphere "cup" (3., 0., 0.) 2.) (Graph.feature pos "bore" (fun () -> Solid.sphere "bore" (3., 0., 0.) 1.))))
  in
  let feature name = List.find (fun (f : Graph.feature) -> f.path = name) !Graph.features in
  let by name = (feature name).fid in
  let p = Prog.compile made in
  let f = Joined.find p in
  let gone names = let ids = List.map by names in fun id -> List.mem id ids in
  let out names = List.length (List.filter (Joined.out (gone names)) (Array.to_list f.bodies)) in
  check "bodies: a feature that is a body is a switch's to take out, a cut inside one is not"
    (Array.length f.bodies = 2 && Joined.plain f (gone [ "peg" ]) && out [ "peg" ] = 1 && Joined.plain f (gone [ "cup"; "cup/bore" ]) && out [ "cup"; "cup/bore" ] = 1 && (not (Joined.plain f (gone [ "cup/bore" ]))) && out [ "cup/bore" ] = 0)
    (Printf.sprintf "%d bodies" (Array.length f.bodies));
  (* and what the switches show alone: a feature that is bodies of the model, or a union of them made for the purpose *)
  let alone g = Option.map (List.map fst) (Joined.alone p f g) in
  let of_ name = Option.get (feature name).result in
  check "bodies: a feature made of the model's bodies is shown alone by the others' switches"
    (Option.map List.length (alone (of_ "peg")) = Some 1 && alone (of_ "peg") <> alone (of_ "cup") && Option.map List.length (alone (Graph.union (of_ "cup") (of_ "peg"))) = Some 2 && alone (of_ "cup/bore") = None)
    "a feature's bodies are not those it is made of"

(* A group (Graph.group): a name around features. It may build anything; the features
   made in it are under it, named within it; alone, it is those features together. *)
let groups () =
  let pos = ("test", 0, 0, 0) in
  let ball x () = Solid.sphere "ball" (x, 0., 0.) 1. in
  let left, right = Graph.group pos "pair" (fun () -> (Graph.feature pos "left" (ball (-3.)), Graph.feature pos "right" (ball 3.))) in
  let by name = List.find (fun (f : Graph.feature) -> f.path = name) !Graph.features in
  let pair = by "pair" and l = by "pair/left" and r = by "pair/right" in
  check "groups: the features made in a group are under it, and named within it" (pair.group && (not l.group) && l.parent = pair.fid && r.parent = pair.fid) (Printf.sprintf "parents %d and %d, the group %d" l.parent r.parent pair.fid);
  let value g x = let p = Prog.compile g in Prog.value p (Prog.ctx p) (Prog.whole p) x in
  check "groups: what the function builds is handed on as it is" (value left [| -3.; 0.; 0. |] < 0. && value right [| -3.; 0.; 0. |] > 0.) "";
  let whole = Option.get pair.result in
  check "groups: alone, a group is its features together"
    (Float.abs (value whole [| -3.; 0.; 0. |] +. 1.) < 1e-9 && Float.abs (value whole [| 3.; 0.; 0. |] +. 1.) < 1e-9 && value whole [| 0.; 0.; 0. |] > 0.)
    (Printf.sprintf "%.3f, %.3f, %.3f" (value whole [| -3.; 0.; 0. |]) (value whole [| 3.; 0.; 0. |]) (value whole [| 0.; 0.; 0. |]));
  let empty = Graph.group pos "nothing in it" (fun () -> 3) in
  check "groups: one with no feature in it has no solid" (empty = 3 && (by "nothing in it").result = None) ""

(* A picture cut by a plane (Render.kept): what is beyond the plane is left out, so a ray
   is taken only where the model is kept, and it is known where it comes in through the
   plane. *)
let kept () =
  let cut = ([| 0.; 0.; 1. |], 3.) in
  let said (a, b, through) = Printf.sprintf "%g to %g%s" a b (if through then ", through the cut" else "") in
  let down = Render.kept cut [| 0.; 0.; 10. |] [| 0.; 0.; -1. |] (0., 20.) in
  check "a cut: a ray from beyond it starts at the plane, through it" (down = (7., 20., true)) (said down);
  let up = Render.kept cut [| 0.; 0.; 0. |] [| 0.; 0.; 1. |] (0., 20.) in
  check "a cut: a ray towards it ends at the plane" (up = (0., 3., false)) (said up);
  let within = Render.kept cut [| 0.; 0.; 2. |] [| 0.; 0.; -1. |] (1., 20.) in
  check "a cut: a ray from what is kept, going on into it, is left whole" (within = (1., 20., false)) (said within);
  let (a, b, _) as along = Render.kept cut [| 0.; 0.; 5. |] [| 1.; 0.; 0. |] (0., 20.) in
  check "a cut: a ray beyond it and along it is left nothing" (a >= b) (said along);
  let late = Render.kept cut [| 0.; 0.; 10. |] [| 0.; 0.; -1. |] (8., 20.) in
  check "a cut: one that is past the plane before it reaches the model does not come in through it" (late = (8., 20., false)) (said late)

let () =
  List.iter set Models.Sets.all;
  List.iter (fun (c : Models.Tricky.case) -> extraction ~hard:(List.mem c.name hard) c.name ~lo:c.lo ~hi:c.hi (c.graph ()) c.expected [ 6; 7 ]) Models.Tricky.cases;
  List.iter
    (fun (name, expected) ->
      let lo, hi, g = Models.Sets.find name in
      extraction name ~lo ~hi (g ()) expected [ 6; 7 ])
    [ ("arm", (48, 82, 10)); ("bracket", (36, 65, 11)) ];
  List.iter (fun (name, depth) -> mesh name depth) [ ("cube-three-fillets", 5); ("pyramid", 5); ("cube-hole", 5); ("bracket", 5); ("block", 6); ("arm", 6) ];
  boards ();
  List.iter (fun (name, tol) -> mesh_to name tol) [ ("cube-three-fillets", 0.01); ("pyramid", 0.01); ("cube-hole", 0.01); ("bracket", 0.05); ("block", 0.05); ("arm", 0.05) ];
  List.iter (fun (name, least) -> gates name least) [ ("bracket", 240); ("bracket", 40); ("block", 40); ("arm", 40); ("pyramid", 40) ];
  List.iter (fun name -> List.iter (regions name) [ 1; 40; 400 ]) [ "bracket"; "block"; "arm"; "pyramid"; "cube-fillet"; "torus" ];
  chamfers ();
  assets ();
  outlines ();
  cuts ();
  clashes ();
  builder ();
  skins ();
  groups ();
  bodies ();
  kept ();
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1)
