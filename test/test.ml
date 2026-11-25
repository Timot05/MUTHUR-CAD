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

(* ---- The shader's gates ---- *)

(* The page's shader skips a gate where its table says every select in it keeps its
   chain so far (Shader). That must leave the field as it is: at random points the value
   and the face, with the skipped selects made to keep their chain, are the model's.
   Half the points are a hair from a cell's wall and read the cell across it, as the
   shader may: it finds a point's cell in 32-bit arithmetic. *)
let gates name least =
  let lo, hi, g = Models.Sets.find name in
  let h = hold ~lo ~hi (g ()) in
  let s = Shader.model ~least h.p h.p.root in
  let grid = Shader.decisions h.p h.c s ~lo ~hi in
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
  let r = Region.cut ~most h.p h.c ~lo ~hi in
  Random.init 5;
  let within (g : Region.region) x = x.(0) >= g.lo.(0) && x.(1) >= g.lo.(1) && x.(2) >= g.lo.(2) && x.(0) <= g.hi.(0) && x.(1) <= g.hi.(1) && x.(2) <= g.hi.(2) in
  let wrong = ref 0 and furthest = ref 0. and outside = ref 0 in
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
    if Float.abs (probe h x).d.v < 1e-3 *. h.diag && not (Array.exists (fun g -> within g x) r.regions) then incr outside
  done;
  check
    (Printf.sprintf "%s: cut into %d regions, %d shaders" name (Array.length r.regions) (Array.length r.shaders))
    (!wrong = 0 && !outside = 0 && Array.length r.regions > 0)
    (Printf.sprintf "the field differs at %d points of the regions, by %g at most; %d points near the surface are in no region" !wrong !furthest !outside)

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

let () =
  List.iter set Models.Sets.all;
  List.iter (fun (c : Models.Tricky.case) -> extraction ~hard:(List.mem c.name hard) c.name ~lo:c.lo ~hi:c.hi (c.graph ()) c.expected [ 6; 7 ]) Models.Tricky.cases;
  List.iter
    (fun (name, expected) ->
      let lo, hi, g = Models.Sets.find name in
      extraction name ~lo ~hi (g ()) expected [ 6; 7 ])
    [ ("arm", (48, 82, 10)); ("bracket", (36, 65, 11)) ];
  List.iter (fun (name, depth) -> mesh name depth) [ ("cube-three-fillets", 5); ("pyramid", 5); ("cube-hole", 5); ("bracket", 5); ("block", 6); ("arm", 6) ];
  List.iter (fun (name, least) -> gates name least) [ ("bracket", 240); ("bracket", 40); ("block", 40); ("arm", 40); ("pyramid", 40) ];
  List.iter (fun name -> List.iter (regions name) [ 1; 40; 400 ]) [ "bracket"; "block"; "arm"; "pyramid"; "cube-fillet"; "torus" ];
  chamfers ();
  assets ();
  outlines ();
  cuts ();
  clashes ();
  builder ();
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1)
