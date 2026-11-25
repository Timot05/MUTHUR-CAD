(* Models by name, built here: a function of a project, or one of the tool's small shapes.

   A design folder holds projects. A project is a folder there, <project>/, with its
   code: .ml files, in folders if it likes.
   It is compiled once, as a whole (<project>/dune says how), against the tool's kernel,
   which the design folder reaches through its link mk52. Every function in it that
   builds a solid on its own (unit -> Graph.t) can be built alone, and is a model:
   "<project>.<file>.<function>", or "<project>.<file>" for a file's graph or its only
   function. The project's one program, entry.exe, builds any of them by name and
   writes what it built (lib/emit.ml); it is run here, with a limit on its time.
   Everything made goes into the design's _mk52/. Anything else by name is a small shape
   of the tool's own (models/). *)
open Mk52

let is_dir = Place.is_dir
let projects = Place.projects
let read_file path = In_channel.with_open_bin path In_channel.input_all

(* What makes a folder a project: one compilation of all of it, and one program that
   builds any of its functions. Written into a new project. *)
let dune_text = {dune|(* -*- tuareg -*- *)
(* A project: every .ml file here and in the folders below is compiled once, together.
   Its functions that build a solid on their own (unit -> Graph.t) are found from their
   types and gathered into one program, entry.exe, which builds any of them by name
   (mk52/tools/entries.ml). Nothing is listed by hand: a new function is found. *)
let rec sources dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun f ->
         let path = if dir = "." then f else Filename.concat dir f in
         if Sys.is_directory path then if f.[0] = '.' || f.[0] = '_' then [] else sources path
         else if Filename.check_suffix f ".ml" && f <> "entry.ml" then [ path ]
         else [])

let project = Filename.basename (Sys.getcwd ()) ^ "_project"
let compiled = Printf.sprintf "-I .%s.objs/byte -I ../mk52/lib/.mk52.objs/byte" project
let signature path = Filename.remove_extension (Filename.basename path) ^ ".sig"

let () =
  Jbuild_plugin.V1.send
    (String.concat "\n"
       ([ "(include_subdirs unqualified)";
          Printf.sprintf "(library (name %s) (wrapped false) (modules :standard \\ entry) (libraries mk52))" project;
          Printf.sprintf "(executable (name entry) (modules entry) (libraries %s mk52))" project;
          Printf.sprintf "(rule (targets entry.ml) (deps %s.cma %s) (action (with-stdout-to %%{targets} (run %%{exe:../mk52/tools/entries.exe} %s %s))))" project
            (String.concat " " (List.map signature (sources ".")))
            compiled
            (String.concat " " (List.map signature (sources "."))) ]
       @ List.map
           (fun path -> Printf.sprintf "(rule (targets %s) (deps (:source %s) %s.cma) (action (with-stdout-to %%{targets} (run ocamlc -i %s %%{source}))))" (signature path) path project compiled)
           (sources ".")))
|dune}

(* ---- A project compiled, and its functions ---- *)

(* dune on the project's program. Another build of the same folder going on (two
   commands at once) is waited for. Done once in a run of the command. *)
let compiled : (string, (unit, string) result) Hashtbl.t = Hashtbl.create 4
let program project = Printf.sprintf "%s/default/%s/entry.exe" (Filename.concat (Sys.getcwd ()) Place.build) project

let compile project =
  match Hashtbl.find_opt compiled project with
  | Some r -> r
  | None ->
      let log = Filename.temp_file "mk52" ".log" in
      let contains s sub = let n = String.length sub in let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in go 0 in
      let rec build tries =
        if Sys.command (Printf.sprintf "dune build --build-dir %s ./%s/entry.exe > %s 2>&1" (Filename.quote (Filename.concat (Sys.getcwd ()) Place.build)) (Filename.quote project) (Filename.quote log)) = 0 then Ok ()
        else
          let why = String.trim (read_file log) in
          if tries > 0 && contains why "locked the build directory" then (Unix.sleepf 0.5; build (tries - 1)) else Error why
      in
      let r = build 120 in
      (try Sys.remove log with _ -> ());
      Hashtbl.replace compiled project r;
      r

(* The project's program run: what it printed, or why it failed. It is stopped after
   [limit] seconds, 30 unless said. *)
let run ?(limit = 30.) project args =
  let log = Filename.temp_file "mk52" ".log" and out = Filename.temp_file "mk52" ".out" in
  let fd_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 and fd_log = Unix.openfile log [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 and null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process (program project) (Array.of_list ("entry" :: args)) null fd_out fd_log in
  List.iter Unix.close [ fd_out; fd_log; null ];
  let rec wait left = match Unix.waitpid [ Unix.WNOHANG ] pid with 0, _ when left > 0. -> Unix.sleepf 0.01; wait (left -. 0.01) | 0, _ -> Unix.kill pid Sys.sigkill; ignore (Unix.waitpid [] pid); None | _, st -> Some st in
  let r =
    match wait limit with
    | None -> Error (Printf.sprintf "It took more than %.0f s; it was stopped." limit)
    | Some (Unix.WEXITED 0) -> Ok (read_file out)
    | Some _ -> Error (match String.trim (read_file log) with "" -> "The project's program failed." | why -> why)
  in
  (try Sys.remove log with _ -> ());
  (try Sys.remove out with _ -> ());
  r

(* The project's functions that can be built alone, as <file>.<function>, in the files'
   order. They are written down for the page to list (_mk52/open/). *)
let functions project =
  match compile project with
  | Error why -> failwith (Printf.sprintf "%s does not build:\n%s" project why)
  | Ok () -> (
      match run project [] with
      | Error why -> failwith why
      | Ok text ->
          Out_channel.with_open_bin (Place.functions project) (fun oc -> output_string oc text);
          List.filter (( <> ) "") (String.split_on_char '\n' text))

(* "m15.end_wall.graph" is the project m15's function graph in end_wall.ml. Named
   shorter, "m15.end_wall" is that file's graph, or its only function. *)
let split name =
  match String.index_opt name '.' with
  | Some i when List.mem (String.sub name 0 i) (projects ()) ->
      let project = String.sub name 0 i and rest = String.sub name (i + 1) (String.length name - i - 1) in
      let all = functions project in
      let of_file = List.filter (fun f -> String.starts_with ~prefix:(rest ^ ".") f) all in
      if List.mem rest all then Some (project, rest)
      else if List.mem (rest ^ ".graph") all then Some (project, rest ^ ".graph")
      else (
        match of_file with
        | [ only ] -> Some (project, only)
        | [] -> failwith (Printf.sprintf "%s has no function called %s that builds a solid. It has: %s" project rest (String.concat ", " all))
        | several -> failwith (Printf.sprintf "%s.ml has several functions: name one of %s" rest (String.concat ", " (List.map (fun f -> project ^ "." ^ f) several))))
  | _ -> None

let file_of entry = String.sub entry 0 (String.index entry '.')

(* Where a model is looked at: the box of its own surface, a little wider and a little
   off centre, so that no face lies on a cell's wall. The octree finds it, from a box far
   larger than any part: 10 m each way from the origin. *)
let far = 1e4

let domain graph =
  let p = Prog.compile graph in
  let lo, hi = Extract.fitted p (Prog.ctx p) ~lo:(Array.make 3 (-.far)) ~hi:(Array.make 3 far) in
  if hi.(0) -. lo.(0) > far then failwith "it has no surface within 10 m of the origin, or one that runs on past that: there is nothing to look at";
  let d = sqrt (Array.fold_left ( +. ) 0. (Array.map2 (fun l h -> (h -. l) *. (h -. l)) lo hi)) in
  (Array.map (fun l -> l -. (0.004 *. d)) lo, Array.map (fun h -> h +. (0.006 *. d)) hi)

(* A model by name, with where it is looked at. *)
let model name : Emit.t =
  match split name with
  | Some (project, entry) -> (
      match Result.bind (run project [ entry ]) (fun text -> try Ok (Marshal.from_string text 0 : Emit.built) with _ -> Error "What it built could not be read.") with
      | Error why -> failwith (Printf.sprintf "%s.%s does not build:\n%s" project entry why)
      | Ok built ->
          let lo, hi = try domain built.graph with Failure why -> failwith (Printf.sprintf "%s.%s: %s" project entry why) in
          { name = project ^ "." ^ entry; lo; hi; built })
  | None ->
      if not (List.mem name Models.Sets.names) then failwith (Printf.sprintf "there is no model called %s (mk52 models lists them)" name);
      let lo, hi, build = Models.Sets.find name in
      let graph = build () in
      { name; lo; hi; built = Emit.built graph }

(* ---- Edges and corners ---- *)

(* A model's edges and corners, its cells counted across the model itself. *)
let topology ?depth (m : Emit.t) = Extract.extract ~fit:true ?depth ~lo:m.lo ~hi:m.hi m.built.graph

(* The outline of a flat part, for a board's edge: the edges at the highest it reaches,
   which for a plate is all round its top, holes and all, each as the line, arc or
   circle it is (Dxf). With what there is of each, and how many were none of them. *)
let outline (m : Emit.t) =
  let t = topology ~depth:7 m in
  let pieces = List.concat_map (fun (e : Extract.edge) -> List.map (fun p -> ((e.closed, e.between), p)) e.pieces) t.edges in
  let top = List.fold_left (fun h (_, p) -> List.fold_left (fun h (x : float array) -> Float.max h x.(2)) h p) neg_infinity pieces in
  let level = List.filter (fun (_, p) -> List.length p >= 2 && List.for_all (fun (x : float array) -> Float.abs (x.(2) -. top) < 1e-3) p) pieces in
  let traced = List.map (fun ((closed, between), p) -> (Dxf.traced ~closed (List.map (fun (x : float array) -> (x.(0), x.(1))) p), between)) level in
  let exact = List.concat_map (fun ((entities, exact), _) -> if exact then entities else []) traced in
  let count f = List.length (List.filter f exact) in
  let text =
    Printf.sprintf "%s at z = %.6g: %d lines, %d arcs, %d circles%s%s" m.name top
      (count (function Dxf.Line _ -> true | _ -> false)) (count (function Dxf.Arc _ -> true | _ -> false)) (count (function Dxf.Circle _ -> true | _ -> false))
      (match List.filter_map (fun ((_, exact), (a, b)) -> if exact then None else Some (Printf.sprintf "%s | %s" a b)) traced with
       | [] -> ""
       | not_exact -> Printf.sprintf ", and %d edges that are none of these, written as short lines: %s" (List.length not_exact) (String.concat "; " not_exact))
      (match List.length t.flags with 0 -> "" | n -> Printf.sprintf "; %d flags in the extraction (mk52 extract says where)" n)
  in
  (text, Dxf.written (List.concat_map (fun ((entities, _), _) -> entities) traced))

(* How far the surface turns across an edge, in degrees, at the sharpest of a few points
   along it. An edge is wherever the face's name changes, so most edges of a part are
   smooth: a fillet's border, a line running into an arc, a fillet whose input changes.
   Those turn by nothing; a crease turns by its angle. At a point of the edge the select
   whose guard is nearest zero is taken each way, as extraction does to tell an edge: if
   its two sides are the edge's two faces, the angle between their gradients there is the
   turn. Both are read at the same point, so a curved face does not add to it. *)
let sharp_from = 1.

let bend p c whole (e : Extract.edge) =
  let faces = List.sort compare [ fst e.between; snd e.between ] in
  let dot u v = (u.(0) *. v.(0)) +. (u.(1) *. v.(1)) +. (u.(2) *. v.(2)) in
  let at q =
    let reached = (Prog.eval p c whole q).reached in
    let off ((_, g) : int * Prog.dual) = Float.abs g.v /. Float.max 1e-300 (Float.sqrt (dot g.g g.g)) in
    List.find_map
      (fun (s, _) ->
        let side k = Prog.eval ~force:(fun id -> if id = s then k else 0) p c whole q in
        let l = side 1 and r = side 2 in
        if List.sort compare [ Prog.face_name p l.face; Prog.face_name p r.face ] <> faces then None
        else
          let a = l.d.g and b = r.d.g in
          let cos = dot a b /. Float.sqrt (dot a a *. dot b b) in
          if Float.is_finite cos then Some (Float.acos (Float.max (-1.) (Float.min 1. cos)) *. 180. /. Float.pi) else None)
      (List.sort (fun x y -> compare (off x) (off y)) reached)
  in
  (* up to five traced points along each piece, which lie on the edge, away from its
     ends; a piece of two points is a straight edge, read at its middle *)
  let along piece =
    let pts = Array.of_list piece in
    match Array.length pts with
    | 0 | 1 -> []
    | 2 -> Option.to_list (at (Array.map2 (fun a b -> 0.5 *. (a +. b)) pts.(0) pts.(1)))
    | n -> List.filter_map (fun i -> at pts.(i)) (List.sort_uniq compare (List.map (fun j -> max 1 (min (n - 2) (j * (n - 1) / 6))) [ 1; 2; 3; 4; 5 ]))
  in
  match List.concat_map along e.pieces with [] -> None | angles -> Some (List.fold_left Float.max 0. angles)

(* A model's extraction, as text for whoever asked and as what the page draws: each
   edge with its two faces, how far the surface turns across it and its traced polylines
   (flat x, y, z, ...), each corner with its faces, and the flags. *)
let extract ?(depth = 6) (m : Emit.t) =
  let t = topology ~depth m in
  let p = Prog.compile m.built.graph in
  let c = Prog.ctx p and whole = Prog.whole p in
  let edges = List.map (fun e -> (e, bend p c whole e)) t.edges in
  let closed = List.length (List.filter (fun (e : Extract.edge) -> e.closed) t.edges) in
  let count f = List.length (List.filter (fun (_, a) -> f a) edges) in
  let sharp = count (function Some a -> a >= sharp_from | None -> false) and smooth = count (function Some a -> a < sharp_from | None -> false) in
  let nums a = Json.Arr (List.map (fun v -> Json.Num v) a) in
  let at x = nums (Array.to_list x) in
  let kind = function Some a when a >= sharp_from -> Printf.sprintf "sharp %.0f°" a | Some _ -> "smooth" | None -> "angle not found" in
  let text =
    Printf.sprintf "%s at %d cells across, of %.3g each: %d corners, %d edges (%d closed; %d sharp, %d smooth%s), %d flags, %.0f ms.\n%s%s" m.name (1 lsl depth) t.size (List.length t.vertices) (List.length t.edges) closed sharp smooth
      (match List.length edges - sharp - smooth with 0 -> "" | n -> Printf.sprintf ", %d not measured" n)
      (List.length t.flags)
      (1000. *. (fst t.seconds +. snd t.seconds))
      (String.concat "" (List.map (fun (f : Extract.flag) -> Printf.sprintf "FLAG at (%.3f, %.3f, %.3f): %s\n" f.where.(0) f.where.(1) f.where.(2) f.why) t.flags))
      (String.concat "\n" (List.map (fun ((e : Extract.edge), a) -> Printf.sprintf "%s | %s%s, %s" (fst e.between) (snd e.between) (if e.closed then " (closed)" else "") (kind a)) edges))
  in
  let json =
    Json.Obj
      [ ("depth", Json.Num (float_of_int depth)); ("cell", Json.Num t.size); ("seconds", Json.Num (fst t.seconds +. snd t.seconds)); ("sharp_from", Json.Num sharp_from);
        ("vertices", Json.Arr (List.map (fun (v : Extract.vertex) -> Json.Obj [ ("at", at v.at); ("faces", Json.Arr (List.map (fun f -> Json.Str f) v.faces)); ("degree", Json.Num (float_of_int v.degree)) ]) t.vertices));
        ( "edges",
          Json.Arr
            (List.map
               (fun ((e : Extract.edge), a) ->
                 Json.Obj
                   [ ("a", Json.Str (fst e.between)); ("b", Json.Str (snd e.between)); ("closed", Json.Bool e.closed); ("ends", Json.Arr (List.map (fun k -> Json.Num (float_of_int k)) e.ends));
                     ("angle", match a with Some a -> Json.Num a | None -> Json.Null);
                     ("pieces", Json.Arr (List.map (fun pts -> nums (List.concat_map Array.to_list pts)) e.pieces)) ])
               edges) );
        ("flags", Json.Arr (List.map (fun (f : Extract.flag) -> Json.Obj [ ("at", at f.where); ("why", Json.Str f.why) ]) t.flags)) ]
  in
  (text, Json.to_string json)

(* ---- A picture ---- *)

(* The model as a PNG, made by the kernel (lib/render.ml), with its edges if asked for. *)
let picture ?(w = 800) ?(h = 600) ?camera ?(edges = false) ?surface (m : Emit.t) =
  let camera = match camera with Some c -> c | None -> { Render.yaw = 40.; pitch = 26.; dist = Render.fit ~w ~h; target = None } in
  let im = Render.model ~w ~h ~camera ~lo:m.lo ~hi:m.hi m.built.graph in
  if edges then Render.lines ~camera ~lo:m.lo ~hi:m.hi (List.concat_map (fun (e : Extract.edge) -> e.pieces) (topology m).edges) im;
  (* a mesh: its triangles' edges in grey, those along creases in ink, none through the model *)
  Option.iter
    (fun (s : Surface.t) ->
      let edges = List.sort_uniq compare (List.concat_map (fun (a, b, c) -> [ (min a b, max a b); (min b c, max b c); (min c a, max c a) ]) (Array.to_list s.triangles)) in
      Render.lines ~ink:(0.33, 0.37, 0.45) ~seen:0.6 ~hidden:0. ~camera ~lo:m.lo ~hi:m.hi (List.map (fun (u, v) -> [ s.points.(u); s.points.(v) ]) edges) im;
      Render.lines ~hidden:0. ~camera ~lo:m.lo ~hi:m.hi (List.map (fun (p, q) -> [ p; q ]) (Surface.creases s)) im)
    surface;
  Render.png im

(* ---- A mesh ---- *)

(* A model in the box of its mesh, a twentieth more all round, for a picture of the mesh. *)
let framed (s : Surface.t) (m : Emit.t) =
  let along f a = Array.fold_left (fun v p -> f v p.(a)) (if Array.length s.points = 0 then 0. else s.points.(0).(a)) s.points in
  let lo = Array.init 3 (along Float.min) and hi = Array.init 3 (along Float.max) in
  let room = 0.05 *. Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo) in
  if Array.length s.points = 0 then m else { m with lo = Array.map (fun v -> v -. room) lo; hi = Array.map (fun v -> v +. room) hi }

(* A model's surface as triangles attached to its creases and corners (lib/surface.ml),
   its cells counted across the model as extraction's are, and what the mesh is like.
   Without [even] it is left as marched: attached, but its triangles any shape. *)
let mesh ?(depth = 6) ?(even = true) (m : Emit.t) =
  let s = Surface.mesh ~fit:true ~depth ~even ~lo:m.lo ~hi:m.hi m.built.graph (topology ~depth m) in
  let q = Surface.quality s in
  let n = Array.length q.angles in
  let under a = 100. *. float_of_int (Array.fold_left (fun k v -> if v < a then k + 1 else k) 0 q.angles) /. float_of_int (max 1 n) in
  let faces = List.length (List.sort_uniq compare (Array.to_list s.face)) in
  ( Printf.sprintf
      "%s at %d cells across, of %.3g each: %d triangles, %d vertices (%d on creases and corners), %d faces.\n%s\nCorners where the surface turns: %d of %d put in.\nA triangle's smallest angle: %.2g deg at worst, %.3g at the middle; %.1f %% of triangles under 20 deg, %.1f %% under 5.\nA triangle's middle is at most %.3g from the surface."
      m.name (1 lsl depth) s.size n (Array.length s.points) q.on_creases faces
      (if q.open_edges = 0 && q.crowded = 0 then "Closed: every edge has two triangles, one each way round." else Printf.sprintf "NOT CLOSED: %d edges with one triangle, %d with more than two or two the same way round." q.open_edges q.crowded)
      (fst s.corners) (snd s.corners)
      (if n = 0 then 0. else q.angles.(0))
      (if n = 0 then 0. else q.angles.(n / 2))
      (under 20.) (under 5.) s.worst,
    s )

(* What an agent working on a function is told: where it is, how to work, and what there
   is to build with. *)
let brief name =
  match split name with
  | None -> failwith "only a project's function has a brief (a model named <project>.<file>.<function>)"
  | Some (project, entry) ->
      let name = project ^ "." ^ entry and file = file_of entry and fn = List.nth (String.split_on_char '.' entry) 1 in
      let path = match List.find_opt (fun p -> Filename.remove_extension (Filename.basename p) = file) (Place.sources project) with Some p -> p | None -> Filename.concat project (file ^ ".ml") in
      String.concat "\n"
        [ Printf.sprintf "%s is the function %s in %s, of the project %s: it builds one solid." name fn path project;
          "";
          Printf.sprintf "THE CODE: the project is every .ml file under %s/, compiled together. A file is the module of its name, capitalised, to the others (bore.ml is Bore), with no import. A function that takes () and returns a solid can be opened on its own, as this one is; the person sees them listed in the page." project;
          "Do not change anything under mk52/ (that is the tool: its kernel is mk52/lib/). If the kernel lacks something you need, say what and stop: that is for the person to decide.";
          "";
          "HOW TO WORK, from this folder. The person has the page open beside their editor: a model that is open is built again, by itself, each time a file is saved, so they see what you do as you do it.";
          Printf.sprintf "  mk52 picture %s  a picture of it, made by the kernel, as a PNG whose path is printed: look at it. Add yaw= pitch= distance= target=x,y,z to look from elsewhere, edges for its edges" name;
          Printf.sprintf "  mk52 extract %s  its edges and corners, with every flag: a place where the model may not be what was meant" name;
          Printf.sprintf "  mk52 outline %s  if it is a flat part, a board: its outline, holes too, as a DXF file of lines, arcs and circles" name;
          Printf.sprintf "  mk52 view %s     opens it in the person's page, in front" name;
          "What there is to model with, all under mk52/: sketches first (lib/sketch.ml: pulled, turned, or lofted through sections; models/bracket.ml is a worked example), then the primitives (lib/solid.ml), the graph's operations (lib/graph.ml), spline patches (lib/spline.ml) and fitted patches (lib/fit.ml).";
          "";
          "Say in a line what you are about to do, and look at a picture after each change; ask only when a choice is the person's to make." ]

(* A new file with one function, graph, made so that it builds and shows at once: a block
   20 mm each way, which is only somewhere to start from. A folder is made a project by
   its first file. *)
let create project name =
  let ok c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' in
  if name = "" || not (String.for_all ok name) || name.[0] < 'a' || name.[0] > 'z' then failwith "a file's name is lower-case letters, digits and _, starting with a letter";
  if name = "entry" then failwith "a file cannot be called entry: that is the project's own program";
  if project = "" || not (String.for_all ok project) then failwith "a project's name is lower-case letters, digits and _";
  if not (is_dir project) then Sys.mkdir project 0o755;
  if not (Sys.file_exists (Filename.concat project "dune")) then Out_channel.with_open_bin (Filename.concat project "dune") (fun oc -> output_string oc dune_text);
  let path = Filename.concat project (name ^ ".ml") in
  if Sys.file_exists path then failwith (path ^ " is already there");
  Out_channel.with_open_bin path (fun oc ->
      Printf.fprintf oc
        "(* %s, of %s: not modelled yet. This block is only somewhere to start from: replace it\n   with its own shape. Lengths in millimetres. *)\nopen Mk52\n\nlet graph () =\n  Graph.feature __POS__ \"%s\" (fun () ->\n      List.fold_left Graph.intersection\n        (Solid.plane \"+x\" (1., 0., 0.) 10.)\n        [ Solid.plane \"-x\" (-1., 0., 0.) 10.; Solid.plane \"+y\" (0., 1., 0.) 10.; Solid.plane \"-y\" (0., -1., 0.) 10.; Solid.plane \"+z\" (0., 0., 1.) 10.; Solid.plane \"-z\" (0., 0., -1.) 10. ])\n"
        name project name);
  path
