(* mk52: the tool's one way in. Run in a design folder (a folder of projects, made with
   mk52 init), or anywhere below it:

     mk52 init                       make this folder a design folder
     mk52 open                       open the page here, in the browser: it lists the
                                     projects' functions and shows each one opened; a
                                     project is built again whenever its code is saved
     mk52 view <model>               build a model and show it in the page, in front;
                                     "open <model>" is the same
     mk52 models                     the projects here and their functions; mk52 models
                                     shapes lists the tool's own small shapes
     mk52 close <model> ...          close those models in the page
     mk52 status                     whether the page is served here and where, and what
                                     is open in it
     mk52 stop                       stop serving the page
     mk52 extract <model> [cells]    its edges and corners (32, 64 or 128 cells across it:
                                     64 unless said), printed, and drawn in the page if
                                     it is open
     mk52 mesh <model> [cells] [raw] its surface as even triangles attached to its creases
                                     and corners: an STL file with a solid for each face,
                                     what the mesh is like, and a picture of it (raw: as
                                     marched, not made even)
     mk52 picture <model> [...]      a picture of it, made by the kernel with no page and
                                     no graphics card: a PNG, whose path is printed. After
                                     the model, any of: yaw=40 pitch=26 (degrees; pitch 90
                                     looks straight down) distance=1.5 (domain diagonals
                                     from the target) target=x,y,z edges mesh
                                     size=800x600 out=file.png
     mk52 new <project> <file>       a new file with one function, a block to start from
     mk52 brief <model>              what an agent working on that function is told
     mk52 mcp                        the kernel and the page as tools for an agent
     mk52 install                    put the command on the path (~/.local/bin/mk52)

   and four the page is served with: serve is its server (cli/serve.ml), which runs the
   others; refresh [<project> ...] builds projects again and each of their open models;
   scene <model> [<feature>] prints how a model is drawn; pick <model> <feature> ox oy oz
   dx dy dz prints what a ray hits.

   A model is a function that builds a solid on its own: <project>.<file>.<function>, or
   <project>.<file> for a file's graph or its only function (cli/project.ml); or one of
   the tool's small shapes.

   Each part has one job. This command builds, measures and draws pictures, and exits.
   The page (web/view.html) is the window: it draws on the graphics card, through WebGPU,
   with a shader the kernel wrote, and holds no model. Its server stays, and does no
   more than run this command for what the page needs worked out. They meet in the
   design's _mk52/ (lib/place.ml): a model is open for as long as its file is in
   _mk52/open/. An agent works in the person's editor, and does not go through the page
   to see: a picture is made here, by the kernel.

   The tool is where its script is (MK52_HOME). A design folder is a dune project with a
   link, mk52, to the tool: through it the design's code is compiled against the tool's
   kernel, as it is now. *)
open Mk52

let usage = "mk52 init | open | view <model> | models [shapes] | close <model> ... | status | stop | extract <model> [32 | 64 | 128] | mesh <model> [32 | 64 | 128 | 256] [raw] | picture <model> [yaw= pitch= distance= target= edges mesh size= out=] | new <project> <file> | brief <model> | mcp | install"
let home = Place.home ()

(* The design folder: here or above, the folder with a dune-project and the link to the tool. *)
let rec design dir = if Sys.file_exists (Filename.concat dir "dune-project") && Place.is_dir (Filename.concat dir "mk52") then Some dir else let up = Filename.dirname dir in if up = dir then None else design up
let () = match design (Sys.getcwd ()) with Some d -> Sys.chdir d | None -> ()

(* ---- The page ---- *)

(* The server running here, by what it wrote down when it started: its process and its port. *)
let running () =
  match List.map int_of_string_opt (String.split_on_char ' ' (String.trim (In_channel.with_open_bin Place.server In_channel.input_all))) with
  | [ Some pid; Some port ] -> ( try Unix.kill pid 0; Some (pid, port) with Unix.Unix_error _ -> None)
  | _ | (exception Sys_error _) -> None

(* The server started here if it is not running, on its own: it outlives this command.
   What it says goes to _mk52/server.log. Its port, once it has one. *)
let start () =
  match running () with
  | Some (_, port) -> port
  | None ->
      let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 and log = Unix.openfile (Filename.concat (Place.folder Place.made) "server.log") [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
      ignore (Unix.create_process Sys.executable_name [| Sys.executable_name; "serve" |] null log log);
      List.iter Unix.close [ null; log ];
      let rec wait tries = match running () with Some (_, port) -> port | None -> if tries = 0 then failwith "the page's server did not start: see _mk52/server.log" else (Unix.sleepf 0.05; wait (tries - 1)) in
      wait 100

(* The page opened in the browser ($BROWSER, if that is set, else the system's own),
   unless one is open here already and [again] is not asked: an open page says so about
   once a second (Place.seen). *)
let browse ?(again = false) port =
  let looking = try Unix.gettimeofday () -. (Unix.stat Place.seen).Unix.st_mtime < 3. with Unix.Unix_error _ -> false in
  if again || not looking then begin
    let url = Printf.sprintf "http://localhost:%d/" port in
    let browser = match Sys.getenv_opt "BROWSER" with Some b when b <> "" -> b | _ -> "open" in
    ignore (Unix.create_process browser [| browser; url |] Unix.stdin Unix.stdout Unix.stderr)
  end

(* A model written where the page takes it from, with its edges if they were extracted. A
   model written without them is one built anew: it is drawn without. *)
let hand ?edges (m : Emit.t) =
  Emit.write ~kernel:(Place.kernel ()) (Place.model m.name) m;
  match edges with
  | Some json -> Out_channel.with_open_bin (Place.edges m.name) (fun oc -> output_string oc json)
  | None -> if Sys.file_exists (Place.edges m.name) then Sys.remove (Place.edges m.name)

(* A model built and shown in the page, in front. *)
let show name =
  let m = Project.model name in
  hand m;
  Out_channel.with_open_bin Place.front (fun oc -> output_string oc m.name);
  let port = start () in
  browse port;
  Printf.printf "%s is open in the page, http://localhost:%d/\n" m.name port

(* A project built again, for the page: its functions listed, and each of its open models
   as its code now makes it. With no project named, every project. What does not build
   is said, and the rest is still done. *)
let refresh projects =
  let open_ = Place.opened () and failed = ref false in
  let attempt what f = try f () with Failure why -> failed := true; Printf.printf "%s\n" (if String.starts_with ~prefix:what why then why else what ^ ": " ^ why) in
  List.iter
    (fun project ->
      attempt project (fun () ->
          ignore (Project.functions project);
          List.iter (fun name -> if String.starts_with ~prefix:(project ^ ".") name then attempt name (fun () -> hand (Project.model name))) open_))
    (if projects = [] then Project.projects () else projects);
  if !failed then exit 1

let close name =
  let dir = Filename.concat Place.made "open" in
  let mine f = f = name ^ ".model" || f = name ^ ".edges.json" || (String.starts_with ~prefix:(name ^ ".") f && Filename.check_suffix f ".gates") in
  Array.iter (fun f -> if mine f then Sys.remove (Filename.concat dir f)) (Sys.readdir dir);
  if Sys.file_exists (Place.state name) then Sys.remove (Place.state name)

let () =
  try
    match List.tl (Array.to_list Sys.argv) with
    | "models" :: which ->
        (* a project's functions one a line, each as it is shortest named to the command;
           the tool's own shapes only on request *)
        let shapes = Models.Sets.names in
        if which = [ "shapes" ] then begin
          let wide = List.fold_left (fun m n -> max m (String.length n)) 0 shapes + 2 in
          let across = max 1 (78 / wide) in
          print_endline "The tool's own small shapes: test shapes of the kernel, opened like any model.";
          List.iteri (fun i n -> Printf.printf "%s%-*s%s" (if i mod across = 0 then "  " else "") wide n (if i mod across = across - 1 || i = List.length shapes - 1 then "\n" else "")) shapes
        end
        else begin
          let projects = Project.projects () in
          if projects = [] then print_endline "No projects here: this is not a design folder, or it is an empty one (mk52 init makes one, mk52 new <project> <file> its first project).";
          List.iter
            (fun p ->
              Printf.printf "%s/\n" p;
              (match (try Ok (Project.functions p) with Failure why -> Error why) with
              | Error why -> Printf.printf "  %s\n" why
              | Ok functions -> List.iter (fun f -> Printf.printf "  %s.%s\n" p (if Filename.extension f = ".graph" then Filename.remove_extension f else f)) functions);
              print_newline ())
            projects;
          Printf.printf "%d small shapes of the tool's own: mk52 models shapes\n" (List.length shapes)
        end
    | [ "open" ] ->
        let port = start () in
        browse ~again:true port;
        Printf.printf "The page is open: http://localhost:%d/\n" port
    | [ "serve" ] -> Serve.main ()
    | [ ("open" | "view"); name ] -> show name
    | "refresh" :: projects -> refresh projects
    | "close" :: (_ :: _ as names) ->
        let open_ = Place.opened () in
        List.iter (fun n -> if List.mem n open_ then (close n; Printf.printf "%s is closed\n" n) else Printf.printf "%s is not open\n" n) names
    | [ "status" ] ->
        print_endline (match running () with None -> "The page is not served here: mk52 open starts it." | Some (_, port) -> Printf.sprintf "The page is served here: http://localhost:%d/" port);
        (match Place.opened () with [] -> print_endline "  nothing is open" | names -> List.iter (Printf.printf "  %s\n") names)
    | [ "stop" ] -> ( match running () with None -> print_endline "The page is not served here." | Some (pid, _) -> Unix.kill pid Sys.sigterm; print_endline "The page's server is stopped.")
    | "extract" :: name :: cells ->
        let depth = match cells with [] | [ "64" ] | [ "6" ] -> 6 | [ "32" ] | [ "5" ] -> 5 | [ "128" ] | [ "7" ] -> 7 | _ -> failwith "cells across: 32, 64 or 128" in
        let m = Project.model name in
        let text, json = Project.extract ~depth m in
        (* drawn in the page, if the model is open there *)
        if List.mem m.name (Place.opened ()) then hand ~edges:json m;
        print_endline text
    | "mesh" :: name :: words ->
        let depth = match List.filter (( <> ) "raw") words with [] | [ "64" ] -> 6 | [ "32" ] -> 5 | [ "128" ] -> 7 | [ "256" ] -> 8 | _ -> failwith "cells across: 32, 64, 128 or 256; raw to leave the triangles as marched" in
        let m = Project.model name in
        let text, surface = Project.mesh ~depth ~even:(not (List.mem "raw" words)) m in
        let out = Place.surface m.name and pic = Place.picture (m.name ^ ".mesh") in
        Out_channel.with_open_bin out (fun oc -> output_string oc (Surface.stl surface));
        Out_channel.with_open_bin pic (fun oc -> output_string oc (Project.picture ~w:1200 ~h:900 ~surface (Project.framed surface m)));
        print_endline text;
        List.iter (fun f -> print_endline (Filename.concat (Sys.getcwd ()) f)) [ out; pic ]
    | "picture" :: name :: options ->
        let opt k = List.find_map (fun o -> match String.index_opt o '=' with Some i when String.sub o 0 i = k -> Some (String.sub o (i + 1) (String.length o - i - 1)) | _ -> None) options in
        let num k d = match opt k with Some v -> ( match float_of_string_opt v with Some x -> x | None -> failwith (k ^ " is a number")) | None -> d in
        let w, h = match opt "size" with Some s -> ( match List.map int_of_string_opt (String.split_on_char 'x' s) with [ Some w; Some h ] when w > 0 && h > 0 && w * h <= 4_000_000 -> (w, h) | _ -> failwith "size is like 800x600") | None -> (800, 600) in
        let target = Option.map (fun s -> match List.map float_of_string_opt (String.split_on_char ',' s) with [ Some x; Some y; Some z ] -> [| x; y; z |] | _ -> failwith "target is x,y,z") (opt "target") in
        let camera = { Render.yaw = num "yaw" 40.; pitch = num "pitch" 26.; dist = num "distance" (Render.fit ~w ~h); target } in
        let m = Project.model name in
        let surface = if List.mem "mesh" options then Some (snd (Project.mesh m)) else None in
        let png = Project.picture ~w ~h ~camera ~edges:(List.mem "edges" options) ?surface m in
        let out = match opt "out" with Some o -> o | None -> Place.picture m.name in
        Out_channel.with_open_bin out (fun oc -> output_string oc png);
        print_endline (if Filename.is_relative out then Filename.concat (Sys.getcwd ()) out else out)
    | "scene" :: name :: solo -> print_endline (Json.to_string (Scene.scene name (match solo with [ f ] -> int_of_string f | _ -> 0)))
    | "pick" :: name :: solo :: ray -> (
        match List.map float_of_string ray with
        | [ ox; oy; oz; dx; dy; dz ] -> print_endline (Json.to_string (Scene.pick name (int_of_string solo) [| ox; oy; oz |] [| dx; dy; dz |]))
        | _ -> failwith "pick <model> <feature> ox oy oz dx dy dz")
    | [ "new"; project; file ] ->
        let path = Project.create project file in
        Printf.printf "%s made: its graph is a block to start from. See it with: mk52 view %s.%s\n" path project file
    | [ "brief"; name ] -> print_endline (Project.brief name)
    | [ "init" ] ->
        (* a dune project of its own, the link through which it reaches the tool's kernel, and the tools for an agent *)
        let here = Sys.getcwd () in
        let write path text = if Sys.file_exists path then Printf.printf "%s is there already: left as it is\n" path else (Out_channel.with_open_bin path (fun oc -> output_string oc text); Printf.printf "%s made\n" path) in
        write "dune-project" "(lang dune 3.0)\n";
        if Sys.file_exists "mk52" then print_endline "mk52 is there already: left as it is" else (Unix.symlink (if Filename.dirname home = Filename.dirname here then Filename.concat ".." (Filename.basename home) else home) "mk52"; print_endline "mk52 made: a link to the tool");
        write ".gitignore" "_mk52/\n_build/\n";
        write ".mcp.json" (Printf.sprintf "{\n  \"mcpServers\": {\n    \"mk52\": {\n      \"command\": %s,\n      \"args\": [\"mcp\"]\n    }\n  }\n}\n" (Json.quote (Filename.concat home "mk52")));
        print_endline "This is a design folder now. mk52 new <project> <file> makes a project and its first file, and mk52 open opens the page."
    | [ "install" ] ->
        let bin = Filename.concat (Sys.getenv "HOME") ".local/bin" in
        if not (Project.is_dir bin) then failwith (bin ^ " is not there: make it and put it on your PATH, then try again");
        let shim = Filename.concat bin "mk52" in
        Out_channel.with_open_bin shim (fun oc -> Printf.fprintf oc "#!/bin/sh\n# mk52, the tool at %s\nexec %s \"$@\"\n" home (Filename.quote (Filename.concat home "mk52")));
        Unix.chmod shim 0o755;
        Printf.printf "%s made: mk52 now runs the tool at %s\n" shim home
    | [ "mcp" ] -> Mcp.run ()
    | _ -> prerr_endline usage; exit 2
  with Failure why -> prerr_endline why; exit 2
