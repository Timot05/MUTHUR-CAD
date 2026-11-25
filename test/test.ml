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

(* The shapes the README says are still beyond the extractor at some cell size. *)
let hard = [ "villarceau"; "plane-through-edge"; "coplanar-boxes" ]

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
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1)
