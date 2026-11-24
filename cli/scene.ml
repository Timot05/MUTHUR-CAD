(* What the page asks of a model that is open, answered from the file the command wrote
   for it (_mk52/open/<model>.model): how it is drawn, and what a ray through it hits.
   Each is one run of the command, printed as JSON: the page holds no model and no kernel. *)
open Mk52

let num v = Json.Num v
let int v = Json.Num (float_of_int v)
let vec a = Json.Arr (Array.to_list (Array.map num a))

(* The model as it was last built, and the graph to show: the whole of it, or with [solo]
   its feature of that id alone. *)
let shown name solo =
  let m = try Emit.read ~kernel:(Place.kernel ()) (Place.model name) with Sys_error _ -> failwith (Printf.sprintf "%s is not open: mk52 view %s" name name) in
  let graph = if solo = 0 then m.built.graph else match List.find_opt (fun (f : Graph.feature) -> f.fid = solo) m.built.features with Some { result = Some g; _ } -> g | _ -> failwith "no such feature" in
  (m, graph)

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

(* A feature: where it is defined, the frame it sits in if it has one (its origin and
   three axes, in the world), and the faces it made with the numbers the shader gives them. *)
let feature ?(faces = []) (f : Graph.feature) =
  let frame =
    match f.frame with
    | None -> Json.Null
    | Some fr -> Json.Obj [ ("o", vec fr.t); ("x", vec (Frame.direction fr [| 1.; 0.; 0. |])); ("y", vec (Frame.direction fr [| 0.; 1.; 0. |])); ("z", vec (Frame.direction fr [| 0.; 0.; 1. |])) ]
  in
  Json.Obj
    [ ("id", int f.fid); ("name", Json.Str f.fname); ("path", Json.Str f.path); ("file", Json.Str f.file); ("line", int f.line); ("parent", int f.parent); ("frame", frame);
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

(* How the page draws the model: its box, its shader, where the shader's gates are
   entered (a file of 32-bit words, little-endian: Shader.decisions, which the page asks
   the server for), its features and its sketches. *)
let scene name solo =
  let m, graph = shown name solo in
  let p = Prog.compile graph in
  let s = Shader.model p p.root in
  let g = Shader.decisions p (Prog.ctx p) s ~lo:m.lo ~hi:m.hi in
  let table = Place.gates name solo in
  Out_channel.with_open_bin table (fun oc -> output_bytes oc g.table);
  let faces = faces_of m p graph in
  Json.Obj
    [ ("name", Json.Str name); ("solo", int solo); ("lo", vec m.lo); ("hi", vec m.hi); ("step", num (1. /. graph.lip)); ("nodes", int p.nodes); ("selects", int (Prog.selects p));
      ("root", Json.Str (Sys.getcwd ()));
      ("features", Json.Arr (List.map (fun f -> feature ~faces:(faces f) f) (features_of m)));
      ("sketches", Json.Arr (List.map sketch m.built.sketches));
      ("shader", Json.Str s.text);
      ("gates", Json.Obj [ ("lo", vec g.lo); ("cell", num g.cell); ("cells", Json.Arr (Array.to_list (Array.map int g.cells))); ("width", int Shader.width) ]) ]

(* What the ray from o along d hits: sphere tracing inside the model's box, as the
   shader does, then the full evaluation at the point reached. *)
let pick name solo o d =
  let m, graph = shown name solo in
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
