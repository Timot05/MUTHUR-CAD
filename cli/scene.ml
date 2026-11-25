(* What the page asks of a model that is open, answered from the file the command wrote
   for it (_mk52/open/<model>.model): how it is drawn, and what a ray through it hits.
   Each is one run of the command, printed as JSON: the page holds no model and no kernel. *)
open Mk52

let num v = Json.Num v
let int v = Json.Num (float_of_int v)
let vec a = Json.Arr (Array.to_list (Array.map num a))

(* What of a model is shown: the whole of it, or with [solo] its feature of that id alone;
   and in that, the features [hidden] taken out, with all made inside them. On the
   command line, "3" or "3:5,8" (feature 3 alone, without 5 and 8); "0" is the whole. *)
type view = { solo : int; hidden : int list }

let view_of s =
  let wrong () = failwith "a view is <feature> or <feature>:<hidden>,<hidden>" in
  let id v = match int_of_string_opt v with Some f when f >= 0 -> f | _ -> wrong () in
  let ids s = List.filter (( <> ) 0) (List.map id (List.filter (( <> ) "") (String.split_on_char ',' s))) in
  match String.split_on_char ':' s with
  | [ solo ] -> { solo = id solo; hidden = [] }
  | [ solo; hidden ] -> { solo = id solo; hidden = List.sort_uniq compare (ids hidden) }
  | _ -> wrong ()

(* Where the gates' table of what is shown is written: Place's file for the feature
   alone, its name followed by the features taken out (music.plexi.graph.0-5-8.gates). *)
let gates name view =
  let at = Place.gates name view.solo in
  if view.hidden = [] then at else Filename.remove_extension at ^ String.concat "" (List.map (Printf.sprintf "-%d") view.hidden) ^ ".gates"

(* Nodes made in this run are numbered above every node of the model read back, whose
   numbers were given in the run that built it: the compiler knows a node by its number. *)
let numbered_above (graph : Graph.t) =
  let seen = Hashtbl.create 256 and seen_e = Hashtbl.create 256 in
  let rec expr (e : Expr.t) =
    if not (Hashtbl.mem seen_e e.id) then begin
      Hashtbl.replace seen_e e.id ();
      Expr.counter := max !Expr.counter e.id;
      match e.node with Expr.U (_, a) -> expr a | B (_, a, b) -> expr a; expr b | X _ | K _ | Slot _ -> ()
    end
  in
  let rec go (g : Graph.t) =
    if not (Hashtbl.mem seen g.id) then begin
      Hashtbl.replace seen g.id ();
      Graph.counter := max !Graph.counter g.id;
      match g.node with Graph.Piece p -> expr p.field | Apply a -> expr a.formula; Array.iter go a.inputs | Select s -> go s.guard; go s.le; go s.gt
    end
  in
  go graph

(* The graph without the features [hidden]: where a node of one of them (or of a feature
   made inside one) is first reached, it is nothing, a field far outside everything, and
   what is above it is made again over that. A union then has one part fewer, a cut does
   not cut, and a body cut into is gone with its cuts. A feature shown alone is shown
   though one around it is taken out. *)
let without (m : Emit.t) ~solo hidden (graph : Graph.t) =
  if hidden = [] then graph
  else begin
    numbered_above m.built.graph;
    let parent f = match List.find_opt (fun (x : Graph.feature) -> x.fid = f) m.built.features with Some x -> x.parent | None -> 0 in
    let gone_memo = Hashtbl.create 64 in
    let rec gone f =
      match Hashtbl.find_opt gone_memo f with
      | Some v -> v
      | None ->
          let v = f <> 0 && f <> solo && (List.mem f hidden || gone (parent f)) in
          Hashtbl.replace gone_memo f v;
          v
    in
    let nothing = Graph.raw_piece "" (Expr.const 1e4) in
    let memo = Hashtbl.create 256 in
    let rec go (g : Graph.t) =
      match Hashtbl.find_opt memo g.id with
      | Some v -> v
      | None ->
          let v =
            if gone g.feature then nothing
            else
              match g.node with
              | Piece _ -> g
              | Apply a ->
                  let inputs = Array.map go a.inputs in
                  if Array.for_all2 ( == ) inputs a.inputs then g else { (Graph.raw_apply ~naming:a.naming ~lip:g.lip a.formula (Array.to_list inputs)) with feature = g.feature }
              | Select s ->
                  let guard = go s.guard and le = go s.le and gt = go s.gt in
                  if guard == s.guard && le == s.le && gt == s.gt then g else { (Graph.select ~jump:s.step ~smooth:s.smooth ~guard ~le ~gt ()) with feature = g.feature }
          in
          Hashtbl.replace memo g.id v;
          v
    in
    go graph
  end

(* The model as it was last built, and the graph to show. *)
let shown name view =
  let m = try Emit.read ~kernel:(Place.kernel ()) (Place.model name) with Sys_error _ -> failwith (Printf.sprintf "%s is not open: mk52 view %s" name name) in
  let graph = if view.solo = 0 then m.built.graph else match List.find_opt (fun (f : Graph.feature) -> f.fid = view.solo) m.built.features with Some { result = Some g; _ } -> g | _ -> failwith "no such feature" in
  (m, without m ~solo:view.solo view.hidden graph)

(* The features with a node in the graph, and their parents, in the order of the code. *)
let features_of (m : Emit.t) =
  let seen = Hashtbl.create 64 and ids = Hashtbl.create 16 in
  let rec add f = if f <> 0 && not (Hashtbl.mem ids f) then (Hashtbl.replace ids f (); match List.find_opt (fun (x : Graph.feature) -> x.fid = f) m.built.features with Some x -> add x.parent | None -> ()) in
  let rec go (g : Graph.t) =
    if not (Hashtbl.mem seen g.id) then begin
      Hashtbl.replace seen g.id ();
      add g.feature;
      match g.node with Graph.Piece _ -> () | Apply a -> Array.iter go a.inputs | Select s -> go s.guard; go s.le; go s.gt
    end
  in
  go m.built.graph;
  List.sort (fun (a : Graph.feature) (b : Graph.feature) -> compare (a.file, a.line, a.fid) (b.file, b.line, b.fid)) (List.filter (fun (f : Graph.feature) -> Hashtbl.mem ids f.fid) m.built.features)

let colour = function Some (r, g, b) -> Json.Arr [ num r; num g; num b ] | None -> Json.Null

(* A feature's colour: its own, or that of the nearest feature around it that has one. *)
let rec coloured (m : Emit.t) (f : Graph.feature) =
  match f.colour with
  | Some c -> Some c
  | None -> Option.bind (List.find_opt (fun (x : Graph.feature) -> x.fid = f.parent) m.built.features) (coloured m)

(* A feature: where it is defined, the frame it sits in if it has one (its origin and
   three axes, in the world), its colour if it has one, and the faces it made with the
   numbers the shader gives them. *)
let feature ?(faces = []) ?paint (f : Graph.feature) =
  let frame =
    match f.frame with
    | None -> Json.Null
    | Some fr -> Json.Obj [ ("o", vec fr.t); ("x", vec (Frame.direction fr [| 1.; 0.; 0. |])); ("y", vec (Frame.direction fr [| 0.; 1.; 0. |])); ("z", vec (Frame.direction fr [| 0.; 0.; 1. |])) ]
  in
  Json.Obj
    [ ("id", int f.fid); ("name", Json.Str f.fname); ("path", Json.Str f.path); ("file", Json.Str f.file); ("line", int f.line); ("parent", int f.parent); ("frame", frame); ("colour", colour paint);
      ("faces", Json.Arr (List.map (fun (name, id) -> Json.Obj [ ("name", Json.Str name); ("id", int id) ]) faces)) ]

(* Each feature's own faces: the pieces it made, by the name within the feature. *)
let faces_of (m : Emit.t) (p : Prog.t) graph =
  let seen = Hashtbl.create 64 and faces = Hashtbl.create 16 in
  let rec go (g : Graph.t) =
    if not (Hashtbl.mem seen g.id) then begin
      Hashtbl.replace seen g.id ();
      match g.node with
      | Graph.Piece piece -> (
          match (Hashtbl.find_opt p.ids piece.name, List.find_opt (fun (f : Graph.feature) -> f.fid = g.feature) m.built.features) with
          | Some id, Some f ->
              let n = String.length f.path + 1 in
              let short = if String.starts_with ~prefix:(f.path ^ "/") piece.name then String.sub piece.name n (String.length piece.name - n) else piece.name in
              let known = Option.value ~default:[] (Hashtbl.find_opt faces f.fid) in
              if not (List.mem_assoc short known) then Hashtbl.replace faces f.fid (known @ [ (short, id) ])
          | _ -> ())
      | Apply a -> Array.iter go a.inputs
      | Select s -> go s.guard; go s.le; go s.gt
    end
  in
  go graph;
  fun (f : Graph.feature) -> Option.value ~default:[] (Hashtbl.find_opt faces f.fid)

(* A sketch: the feature it was drawn in, and each loop's spans as points in the world,
   on the plane it was drawn on. *)
let sketch (d : Sketch.drawn) =
  let world (x, y) = vec (Frame.apply d.frame (if d.turned then [| x; 0.; y |] else [| x; y; 0. |])) in
  let span (s : Sketch.span) = Json.Obj [ ("name", Json.Str s.name); ("points", Json.Arr (List.map world (Sketch.points s))) ] in
  Json.Obj [ ("feature", int d.feature); ("turned", Json.Bool d.turned); ("loops", Json.Arr (List.map (fun l -> Json.Arr (Array.to_list (Array.map span l))) d.loops)) ]

(* What a model shows from outside (Asset), for the page: its name, where it is as a
   feature's frame is given, its colour if it has one, and its triangles as a file the page asks the server for,
   named by a number made of its path. The file is made the first time, and again when
   the asset's own file or the kernel has changed: a large STEP file takes seconds. One
   that cannot be read is told with why, and the rest are shown. *)
let asset (a : Asset.placed) =
  let frame = [ ("o", vec a.frame.t); ("x", vec (Frame.direction a.frame [| 1.; 0.; 0. |])); ("y", vec (Frame.direction a.frame [| 0.; 1.; 0. |])); ("z", vec (Frame.direction a.frame [| 0.; 0.; 1. |])) ] in
  let id = Digest.to_hex (Digest.string a.path) in
  let made () =
    let st = try Unix.stat a.path with Unix.Unix_error _ -> failwith "there is no such file (an asset's path is from the design folder)" in
    let stamp = Printf.sprintf "%f %d %s" st.st_mtime st.st_size (Place.kernel ()) and kept = Place.asset id in
    if (try In_channel.with_open_bin (kept ^ ".of") In_channel.input_all <> stamp with Sys_error _ -> true) || not (Sys.file_exists kept) then begin
      let mesh = Asset.written (Asset.read a.path) in
      (* written whole, then put in place: two commands may be at it at once *)
      let tmp = Printf.sprintf "%s.%d" kept (Unix.getpid ()) in
      Out_channel.with_open_bin tmp (fun oc -> output_string oc mesh);
      Sys.rename tmp kept;
      Out_channel.with_open_bin (kept ^ ".of") (fun oc -> output_string oc stamp)
    end
  in
  let mesh = try made (); [ ("mesh", Json.Str id) ] with Failure why | Sys_error why -> [ ("said", Json.Str (Printf.sprintf "%s: %s" a.path why)) ] in
  Json.Obj ([ ("name", Json.Str a.name); ("feature", int a.feature); ("colour", colour a.colour) ] @ frame @ mesh)

(* How the page draws the model: its box, its features and its sketches, and the model
   itself as a shader of its own, with where that shader's gates are entered (Shader): a
   file of 32-bit words, little-endian, which the page asks the server for. *)
let scene name view =
  let m, graph = shown name view in
  let p = Prog.compile graph in
  let s = Shader.model p p.root in
  let g = Shader.decisions p (Prog.ctx p) s ~lo:m.lo ~hi:m.hi in
  Out_channel.with_open_bin (gates name view) (fun oc -> output_bytes oc g.table);
  let faces = faces_of m p graph in
  Json.Obj
    [ ("name", Json.Str name); ("solo", int view.solo); ("hidden", Json.Arr (List.map int view.hidden)); ("lo", vec m.lo); ("hi", vec m.hi); ("step", num (1. /. graph.lip)); ("nodes", int p.nodes); ("selects", int (Prog.selects p));
      ("root", Json.Str (Sys.getcwd ()));
      ("features", Json.Arr (List.map (fun f -> feature ~faces:(faces f) ?paint:(coloured m f) f) (features_of m)));
      ("sketches", Json.Arr (List.map sketch m.built.sketches));
      ("assets", Json.Arr (List.map asset m.built.assets));
      ("shader", Json.Str s.text);
      ("gates", Json.Obj [ ("lo", vec g.lo); ("cell", num g.cell); ("cells", Json.Arr (Array.to_list (Array.map int g.cells))); ("width", int Shader.width) ]) ]

(* The model cut into regions for the page's other way of drawing (Region), made when
   that is asked for: each shader's text with how many operations it is, and each
   region as seven numbers, where its box starts and ends and which shader is its own. *)
let regions name view most =
  let m, graph = shown name view in
  let p = Prog.compile graph in
  let r = Region.cut ~most p (Prog.ctx p) ~lo:m.lo ~hi:m.hi in
  Json.Obj
    [ ("operations", int r.total);
      ("shaders", Json.Arr (Array.to_list (Array.map (fun ((made : Region.program), text) -> Json.Obj [ ("text", Json.Str text); ("operations", int (Array.length made.steps)) ]) r.shaders)));
      ("regions", Json.Arr (List.concat_map (fun (g : Region.region) -> List.map num (Array.to_list g.lo @ Array.to_list g.hi) @ [ int g.shader ]) (Array.to_list r.regions))) ]

(* What the ray from o along d hits: sphere tracing inside the model's box, as the
   shader does, then the full evaluation at the point reached. *)
let pick name view o d =
  let m, graph = shown name view in
  let p = Prog.compile graph in
  let c = Prog.ctx p and whole = Prog.whole p in
  let tn = ref 0. and tf = ref infinity in
  for a = 0 to 2 do
    let t0 = (m.lo.(a) -. o.(a)) /. d.(a) and t1 = (m.hi.(a) -. o.(a)) /. d.(a) in
    tn := Float.max !tn (Float.min t0 t1);
    tf := Float.min !tf (Float.max t0 t1)
  done;
  let diag = sqrt (Array.fold_left ( +. ) 0. (Array.map2 (fun l h -> (h -. l) *. (h -. l)) m.lo m.hi)) in
  let at t = Array.mapi (fun a v -> v +. (t *. d.(a))) o in
  let rec march t k =
    if t > !tf || k = 0 then None
    else
      (* from inside the solid too: the box's cut is open, as in the shader *)
      let v = Float.abs (Prog.value p c whole (at t)) in
      if v < 1e-6 *. diag then Some (at t) else march (t +. (v /. graph.lip)) (k - 1)
  in
  match if !tn >= !tf then None else march !tn 4000 with
  | None -> Json.Obj [ ("hit", Json.Bool false) ]
  | Some x ->
      let s = Prog.eval p c whole x in
      let made = match List.find_opt (fun (f : Graph.feature) -> f.fid = s.feature) m.built.features with Some f -> feature f | None -> Json.Null in
      Json.Obj [ ("hit", Json.Bool true); ("at", vec x); ("face", Json.Str (Prog.face_name p s.face)); ("face_id", int s.face); ("feature", made); ("value", num s.d.v) ]
