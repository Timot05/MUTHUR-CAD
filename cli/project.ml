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

(* The two ways a project's code becomes what it builds. By dune: compiled once into a
   program of its own, entry.exe, which needs OCaml installed and the tool's source (a
   design folder's link mk52), and is what an editor's OCaml support reads. Or by the
   builder (embed/embed.ml), which carries the compiler and needs neither. MK52_BUILDER
   says which, as dune or carried; unsaid, it is dune wherever dune can do it. *)
type way = Dune | Carried

let way =
  lazy
    (match Sys.getenv_opt "MK52_BUILDER" with
     | Some "dune" -> Dune
     | Some "carried" -> Carried
     | Some other -> failwith (Printf.sprintf "MK52_BUILDER is dune or carried, not %s" other)
     | None -> if Place.sourced () && is_dir "mk52" && Sys.command "command -v dune > /dev/null 2>&1" = 0 then Dune else Carried)

(* dune on the project's program. Another build of the same folder going on (two
   commands at once) is waited for: dune says there is one as "locked the build
   directory", and since 3.2x, where a second dune asks the first to build for it and is
   refused, as "Build requests require a watch-mode server". Done once in a run of the
   command. *)
let compiled : (string, (unit, string) result) Hashtbl.t = Hashtbl.create 4
let program project = Printf.sprintf "%s/default/%s/entry.exe" (Filename.concat (Sys.getcwd ()) Place.build) project

let compile project =
  match Hashtbl.find_opt compiled project with
  | Some r -> r
  | None when Lazy.force way = Carried -> Ok ()  (* nothing to do ahead: the builder reads the files when asked *)
  | None ->
      let log = Filename.temp_file "mk52" ".log" in
      let contains s sub = let n = String.length sub in let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in go 0 in
      let rec build tries =
        if Sys.command (Printf.sprintf "dune build --build-dir %s ./%s/entry.exe > %s 2>&1" (Filename.quote (Filename.concat (Sys.getcwd ()) Place.build)) (Filename.quote project) (Filename.quote log)) = 0 then Ok ()
        else
          let why = String.trim (read_file log) in
          if tries > 0 && (contains why "locked the build directory" || contains why "require a watch-mode server") then (Unix.sleepf 0.5; build (tries - 1)) else Error why
      in
      let r = build 120 in
      (try Sys.remove log with _ -> ());
      Hashtbl.replace compiled project r;
      r

(* The project's program run: what it printed, or why it failed. It is stopped after
   [limit] seconds, 30 unless said. *)
let run_program ?(limit = 30.) project args =
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

(* ---- The builder, kept running ---- *)

(* The builder with a project loaded (embed/embed.ml, -serve): started when the project
   is first asked about, and again whenever the project's files are not the ones it
   loaded, so that a command that stays (the agents' tools) never builds from old code.
   What it says of what went wrong goes to its log. *)
type builder = { pid : int; ask : Unix.file_descr; answer : Unix.file_descr; log : string; loaded : (string * float) list }

let builders : (string, builder) Hashtbl.t = Hashtbl.create 4
let files project = List.map (fun path -> (path, (Unix.stat path).Unix.st_mtime)) (Place.sources project)

let stop project =
  Option.iter
    (fun b ->
      Hashtbl.remove builders project;
      (try Unix.kill b.pid Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] b.pid) with Unix.Unix_error _ -> ());
      List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) [ b.ask; b.answer ];
      try Sys.remove b.log with Sys_error _ -> ())
    (Hashtbl.find_opt builders project)

let () = at_exit (fun () -> List.iter stop (List.of_seq (Hashtbl.to_seq_keys builders)))

(* A line of a builder's answer, waited for at most [limit] seconds. None if none came. *)
let line (b : builder) limit =
  let text = Buffer.create 64 and one = Bytes.create 1 and until = Unix.gettimeofday () +. limit in
  let rec go () =
    let left = until -. Unix.gettimeofday () in
    if left <= 0. then None
    else
      match Unix.select [ b.answer ] [] [] left with
      | [], _, _ -> None
      | _ -> if Unix.read b.answer one 0 1 = 0 then None else if Bytes.get one 0 = '\n' then Some (Buffer.contents text) else (Buffer.add_bytes text one; go ())
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
  in
  go ()

(* What a builder has said in its log from a place on. *)
let said (b : builder) from = let text = read_file b.log in String.trim (String.sub text from (String.length text - from))

let builder project =
  let now = files project in
  match Hashtbl.find_opt builders project with
  | Some b when b.loaded = now -> Ok b
  | _ -> (
      stop project;
      match Place.builder () with
      | None -> Error "This tool has no builder to read a project's code with (embed/ in its source, bin/mk52-build in a release)."
      | Some (program, dirs) -> (
          (* a builder that is gone is found by its answer, not by a signal that ends this command *)
          Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
          let log = Filename.temp_file "mk52" ".log" in
          let ask_r, ask_w = Unix.pipe ~cloexec:true () and answer_r, answer_w = Unix.pipe ~cloexec:true () in
          let fd_log = Unix.openfile log [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
          let pid = Unix.create_process program (Array.of_list (("mk52-build" :: List.concat_map (fun d -> [ "-I"; d ]) dirs) @ [ project; "-serve" ])) ask_r answer_w fd_log in
          List.iter Unix.close [ ask_r; answer_w; fd_log ];
          let b = { pid; ask = ask_w; answer = answer_r; log; loaded = now } in
          Hashtbl.replace builders project b;
          match line b 60. with
          | Some "ready" -> Ok b
          | _ ->
              let why = said b 0 in
              stop project;
              Error (Printf.sprintf "%s does not build:\n%s" project (if why = "" then "The builder did not start." else why))))

(* The same as the project's program run, asked of the builder: with no argument its
   functions, with one that function built. A build that goes on longer than [limit]
   seconds is stopped, and the builder is still there for the next. *)
let run_builder ?(limit = 30.) project args =
  match builder project with
  | Error why -> Error why
  | Ok b -> (
      let from = String.length (read_file b.log) in
      let failed otherwise = Error (match said b from with "" -> otherwise | why -> why) in
      let ask text = let s = text ^ "\n" in ignore (Unix.write_substring b.ask s 0 (String.length s)) in
      let gone () = stop project; Error "The builder stopped." in
      try
        match args with
        | [] ->
            ask "list";
            let rec names acc = match line b 10. with Some "." -> Ok (String.concat "" (List.rev_map (fun n -> n ^ "\n") acc)) | Some name -> names (name :: acc) | None -> gone () in
            names []
        | entry :: _ -> (
            let out = Filename.temp_file "mk52" ".out" in
            ask (Printf.sprintf "build %s %s" entry out);
            let r =
              match line b 10. with
              | Some child when String.starts_with ~prefix:"child " child -> (
                  match line b limit with
                  | Some "ok" -> Ok (read_file out)
                  | Some _ -> failed "The function failed."
                  | None ->
                      (try Unix.kill (int_of_string (String.sub child 6 (String.length child - 6))) Sys.sigkill with _ -> ());
                      ignore (line b 5.);
                      Error (Printf.sprintf "It took more than %.0f s; it was stopped." limit))
              | Some _ -> failed "The function failed."
              | None -> gone ()
            in
            (try Sys.remove out with Sys_error _ -> ());
            r)
      with Unix.Unix_error _ -> gone ())

let run ?limit project args = match Lazy.force way with Dune -> run_program ?limit project args | Carried -> run_builder ?limit project args

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

(* ---- A cut ---- *)

(* What is asked of a cut: the plane where coordinate [axis] is [at], and for the two
   other coordinates from where to where, or all of the model. *)
type cut = { axis : int; at : float; spans : (float * float) option array }

let coordinate = [| "x"; "y"; "z" |]

(* The cut to draw: for a coordinate not said, all of what is solid in the plane (found on
   a coarse grid over the model's box) and a little round it, or the box if nothing is. *)
let plane (m : Emit.t) (k : cut) : Render.cut =
  let a, b = Render.across_up k.axis in
  let whole : Render.cut = { axis = k.axis; at = k.at; left = m.lo.(a); right = m.hi.(a); bottom = m.lo.(b); top = m.hi.(b) } in
  let said i = match k.spans.(i) with Some (u, v) when u <> v -> Some (Float.min u v, Float.max u v) | _ -> None in
  let solid =
    if said a <> None && said b <> None then None
    else
      let n = 300 in
      let cells = Render.cut_cells ~cols:n ~rows:n whole ~part:(fun _ -> 0) ~lo:m.lo ~hi:m.hi m.built.graph in
      let x0 = ref n and x1 = ref (-1) and y0 = ref n and y1 = ref (-1) in
      Array.iteri (fun i id -> if id >= 0 then (x0 := min !x0 (i mod n); x1 := max !x1 (i mod n); y0 := min !y0 (i / n); y1 := max !y1 (i / n))) cells;
      if !x1 < 0 then None
      else
        let f v = float_of_int v /. float_of_int n in
        Some ((whole.left +. (f !x0 *. (whole.right -. whole.left)), whole.left +. (f (!x1 + 1) *. (whole.right -. whole.left))), (whole.top -. (f (!y1 + 1) *. (whole.top -. whole.bottom)), whole.top -. (f !y0 *. (whole.top -. whole.bottom))))
  in
  let fitted (lo, hi) other = let pad = 0.04 *. Float.max (hi -. lo) other in (lo -. pad, hi +. pad) in
  let across, up = match solid with Some (u, v) -> (fitted u (snd v -. fst v), fitted v (snd u -. fst u)) | None -> ((whole.left, whole.right), (whole.bottom, whole.top)) in
  let (left, right), (bottom, top) = (Option.value ~default:across (said a), Option.value ~default:up (said b)) in
  { axis = k.axis; at = k.at; left; right; bottom; top }

(* A model's parts are its features that are in no other: the part a feature is in. *)
let part_of (m : Emit.t) =
  let by = Hashtbl.create 64 in
  List.iter (fun (f : Graph.feature) -> Hashtbl.replace by f.fid f) m.built.features;
  let rec top fid = match Hashtbl.find_opt by fid with Some f when Hashtbl.mem by f.parent -> top f.parent | Some f -> f.fid | None -> 0 in
  (top, fun id -> match Hashtbl.find_opt by id with Some f -> f.fname | None -> "what is in no feature")

(* A part's colour in a cut: its own in the model if [paint] and it has one, else one
   by its number, a golden step round the colours from the one before, so that parts
   whose numbers are near are far apart in colour. *)
let cut_colour ?(paint = false) (m : Emit.t) id =
  let own = if paint then Option.bind (List.find_opt (fun (f : Graph.feature) -> f.fid = id) m.built.features) (Emit.coloured m.built.features) else None in
  match own with
  | Some c -> c
  | None ->
      let hue = Float.rem (float_of_int id *. 0.61803398875) 1. in
      let tone o = 0.62 +. (0.3 *. cos (6.2831853 *. (hue +. o))) in
      (tone 0., tone 0.33, tone 0.67)

let colour_name (r, g, b) =
  let hi = Float.max r (Float.max g b) and lo = Float.min r (Float.min g b) in
  if hi -. lo < 0.08 then (if hi > 0.8 then "white" else if hi > 0.35 then "grey" else "black")
  else
    let h = 60. *. (if hi = r then Float.rem ((g -. b) /. (hi -. lo)) 6. else if hi = g then ((b -. r) /. (hi -. lo)) +. 2. else ((r -. g) /. (hi -. lo)) +. 4.) in
    let h = if h < 0. then h +. 360. else h in
    if h < 18. then "red" else if h < 45. then "orange" else if h < 70. then "yellow" else if h < 160. then "green" else if h < 200. then "teal" else if h < 255. then "blue" else if h < 300. then "purple" else if h < 340. then "pink" else "red"

let seen_from = [| "+x"; "-y"; "+z"  |]

let cut_said (m : Emit.t) (k : Render.cut) =
  let a, b = Render.across_up k.axis in
  Printf.sprintf "%s cut at %s = %g, seen from %s: %s from %g to %g, left to right; %s from %g to %g, bottom to top" m.name coordinate.(k.axis) k.at seen_from.(k.axis) coordinate.(a) k.left k.right coordinate.(b) k.bottom k.top

(* The cut as a picture (Render.section), and what is in it: each part with its colour. *)
let section ?(w = 1000) ?paint (m : Emit.t) (k : cut) =
  let k = plane m k in
  let w = max 64 (min w (int_of_float (1600. *. (k.right -. k.left) /. (k.top -. k.bottom)))) in
  let part, name = part_of m in
  let im, parts = Render.section ~w k ~part ~colour:(cut_colour ?paint m) ~lo:m.lo ~hi:m.hi m.built.graph in
  let text =
    Printf.sprintf "%s; a pixel is %.3g.\n%s" (cut_said m k) ((k.right -. k.left) /. float_of_int w)
      (match parts with
       | [] -> "Nothing is solid in it."
       | _ ->
           let a, b = Render.across_up k.axis in
           String.concat "\n"
             (List.map (fun (id, (u0, u1), (v0, v1)) -> Printf.sprintf "  %s: %s, %s %.4g to %.4g, %s %.4g to %.4g" (colour_name (cut_colour ?paint m id)) (name id) coordinate.(a) u0 u1 coordinate.(b) v0 v1) parts))
  in
  (text, Render.png im)

(* The cut as text, for where a column counts more than a look: a letter for each part,
   a dot for air, a cell twice as high as wide, as a letter is. *)
let section_text ?(cols = 100) (m : Emit.t) (k : cut) =
  let k = plane m k in
  let cols = max 8 (min 200 cols) in
  let cell = (k.right -. k.left) /. float_of_int cols in
  let rows = max 1 (int_of_float (Float.round ((k.top -. k.bottom) /. (2. *. cell)))) in
  let part, name = part_of m in
  let cells = Render.cut_cells ~cols ~rows k ~part ~lo:m.lo ~hi:m.hi m.built.graph in
  let counts = Hashtbl.create 16 in
  Array.iter (fun id -> if id >= 0 then Hashtbl.replace counts id (1 + Option.value ~default:0 (Hashtbl.find_opt counts id))) cells;
  let parts = List.map fst (List.sort (fun (_, a) (_, b) -> compare b a) (Hashtbl.fold (fun id n acc -> (id, n) :: acc) counts [])) in
  let letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" in
  let letter id = let rec at i = function [] -> '?' | p :: rest -> if p = id then letters.[i mod String.length letters] else at (i + 1) rest in at 0 parts in
  let b = Buffer.create (rows * (cols + 12)) in
  let _, up = Render.across_up k.axis in
  Buffer.add_string b (Printf.sprintf "%s; a letter is %.3g wide and %.3g high. The number before a row is its %s.\n" (cut_said m k) cell (2. *. cell) coordinate.(up));
  for r = 0 to rows - 1 do
    Buffer.add_string b (Printf.sprintf "%9.3f " (k.top -. ((float_of_int r +. 0.5) /. float_of_int rows *. (k.top -. k.bottom))));
    for c = 0 to cols - 1 do Buffer.add_char b (match cells.((cols * r) + c) with -1 -> '.' | id -> letter id) done;
    Buffer.add_char b '\n'
  done;
  List.iter (fun id -> Buffer.add_string b (Printf.sprintf "  %c: %s\n" (letter id) (name id))) parts;
  if parts = [] then Buffer.add_string b "Nothing is solid in it.\n";
  Buffer.contents b

(* ---- Clashes ---- *)

(* A model's parts as solids, each with its name; and its bought parts as the points
   they are held by. Where several have one name, each is also said by where it is. *)
type thing = { said : string; called : string; solid : Clash.solid option; points : float array list; lo : float array; hi : float array }

let things (m : Emit.t) =
  let top, _ = part_of m in
  let parts =
    List.filter_map
      (fun (f : Graph.feature) ->
        match f.result with
        | Some g when top f.fid = f.fid -> Option.map (fun (s : Clash.solid) -> { said = f.fname; called = f.fname; solid = Some s; points = []; lo = s.lo; hi = s.hi }) (Clash.solid ~lo:m.lo ~hi:m.hi g)
        | _ -> None)
      m.built.features
  in
  let meshes = Hashtbl.create 8 in
  let mesh path = match Hashtbl.find_opt meshes path with Some v -> v | None -> let v = Asset.read path in Hashtbl.replace meshes path v; v in
  let bought =
    List.map
      (fun (a : Asset.placed) ->
        let points = Clash.points a.frame (mesh a.path) in
        let over f = Array.init 3 (fun i -> List.fold_left (fun v (x : float array) -> f v x.(i)) (List.hd points).(i) points) in
        { said = a.name; called = a.name; solid = None; points; lo = over Float.min; hi = over Float.max })
      m.built.assets
  in
  let all = parts @ bought in
  List.map (fun t -> if List.length (List.filter (fun o -> o.called = t.called) all) > 1 then { t with said = Printf.sprintf "%s at (%.4g, %.4g, %.4g)" t.called (0.5 *. (t.lo.(0) +. t.hi.(0))) (0.5 *. (t.lo.(1) +. t.hi.(1))) (0.5 *. (t.lo.(2) +. t.hi.(2))) } else t) all

(* What is found between two things: how far apart at their nearest (below zero: how far
   into each other), and where. *)
type found = { one : thing; other : thing; clear : float; where : float array }

let touching = 0.02

(* Every pair of a model's things, or every one of its things against every one of
   another model's: those that are into each other, that touch, or that are nearer than
   [limit]. Two bought parts are not held against each other. With how many pairs there
   were, and how many had their boxes near enough to be looked at. *)
let clashes ?(limit = 0.3) ?against (m : Emit.t) =
  let mine = things m in
  let pairs =
    match against with
    | Some (o : Emit.t) -> List.concat_map (fun a -> List.map (fun b -> (a, b)) (things o)) mine
    | None -> let rec go = function a :: rest -> List.map (fun b -> (a, b)) rest @ go rest | [] -> [] in go mine
  in
  let pairs = List.filter (fun (a, b) -> a.solid <> None || b.solid <> None) pairs in
  let near (a, b) = Array.for_all Fun.id (Array.init 3 (fun i -> a.lo.(i) -. limit <= b.hi.(i) && b.lo.(i) -. limit <= a.hi.(i))) in
  let looked = List.filter near pairs in
  let found =
    List.filter_map
      (fun (a, b) ->
        let r = match (a.solid, b.solid) with Some s, Some s' -> Clash.clearance ~limit s s' | Some s, None -> Clash.against ~limit s b.points | None, Some s -> Clash.against ~limit s a.points | None, None -> None in
        Option.map (fun (clear, where) -> { one = a; other = b; clear; where }) r)
      looked
  in
  (List.sort (fun x y -> compare x.clear y.clear) found, List.length pairs, List.length looked, List.length mine)

(* The same, said: first what is into each other, then what touches, then what is near;
   pairs of the same two names together, with the worst of them; and apart, what the
   design says is meant (Clash.allow). *)
let clash ?(limit = 0.3) ?against (m : Emit.t) =
  let found, pairs, looked, count = clashes ~limit ?against m in
  let meant = m.built.meant @ (match against with Some (o : Emit.t) -> o.built.meant | None -> []) in
  let is_meant f = List.exists (fun (a, b) -> (a = f.one.called && b = f.other.called) || (a = f.other.called && b = f.one.called)) meant in
  let said_meant, rest = List.partition is_meant found in
  let grouped fs =
    let keys = List.sort_uniq compare (List.map (fun f -> (f.one.called, f.other.called)) fs) in
    let groups = List.map (fun k -> List.filter (fun f -> (f.one.called, f.other.called) = k) fs) keys in
    List.sort (fun a b -> compare (List.hd a).clear (List.hd b).clear) groups
  in
  let line fs =
    let worst = List.hd fs in
    let amount = if worst.clear < -.touching then Printf.sprintf "%.3g into each other" (-.worst.clear) else if worst.clear <= touching then "touching" else Printf.sprintf "%.3g apart" worst.clear in
    let where = Printf.sprintf "at (%.4g, %.4g, %.4g)" worst.where.(0) worst.where.(1) worst.where.(2) in
    match fs with
    | [ f ] -> Printf.sprintf "  %s | %s: %s, %s" f.one.said f.other.said amount where
    | _ ->
        let last = List.nth fs (List.length fs - 1) in
        Printf.sprintf "  %s | %s, %d pairs: %s%s, the worst %s" worst.one.called worst.other.called (List.length fs) amount (if Float.abs (last.clear -. worst.clear) > 0.005 then Printf.sprintf " at worst, %.3g at best" last.clear else "") where
  in
  let section title fs = if fs = [] then [] else (Printf.sprintf "%s (%d):" title (List.length fs)) :: List.map line (grouped fs) in
  let into = List.filter (fun f -> f.clear < -.touching) rest and touch = List.filter (fun f -> Float.abs f.clear <= touching) rest and close = List.filter (fun f -> f.clear > touching) rest in
  String.concat "\n"
    ([ Printf.sprintf "%s%s: %d parts, %d of them bought and held by their triangles' points; %d pairs, %d near enough to look at." m.name (match against with Some o -> " against " ^ o.name | None -> "") count (List.length m.built.assets) pairs looked ]
    @ (if rest = [] then [ Printf.sprintf "Nothing is into anything else, touches it, or is nearer than %g%s." limit (if said_meant = [] then "" else ", but for what is meant") ] else [])
    @ section "Into each other" into @ section "Touching" touch @ section (Printf.sprintf "Nearer than %g" limit) close @ section "Meant by the design" said_meant)

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
          Printf.sprintf "  mk52 section %s z=0  a cut through it where z is 0 (or x=, y=), the parts in colours with a scale: for what is inside it or between its parts. The other two coordinates from:to look closer, as x=10:20; text gives it as letters" name;
          Printf.sprintf "  mk52 clash %s  if it is of several parts: which of them run into each other, touch, or are nearer than gap=0.3, its bought parts too. What is meant to touch is said in the code, as the model is built: Mk52.Clash.allow \"a part\" \"another\"" name;
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
  (* how dune compiles it, where dune can: a tool without its source compiles nothing *)
  if Place.sourced () && not (Sys.file_exists (Filename.concat project "dune")) then Out_channel.with_open_bin (Filename.concat project "dune") (fun oc -> output_string oc dune_text);
  let path = Filename.concat project (name ^ ".ml") in
  if Sys.file_exists path then failwith (path ^ " is already there");
  Out_channel.with_open_bin path (fun oc ->
      Printf.fprintf oc
        "(* %s, of %s: not modelled yet. This block is only somewhere to start from: replace it\n   with its own shape. Lengths in millimetres. *)\nopen Mk52\n\nlet graph () =\n  Graph.feature __POS__ \"%s\" (fun () ->\n      List.fold_left Graph.intersection\n        (Solid.plane \"+x\" (1., 0., 0.) 10.)\n        [ Solid.plane \"-x\" (-1., 0., 0.) 10.; Solid.plane \"+y\" (0., 1., 0.) 10.; Solid.plane \"-y\" (0., -1., 0.) 10.; Solid.plane \"+z\" (0., 0., 1.) 10.; Solid.plane \"-z\" (0., 0., -1.) 10. ])\n"
        name project name);
  path
