(* Where things are, said once.

   The tool is where its script is (MK52_HOME, set by the mk52 script). A design folder
   holds projects: a project is a folder there with a dune file and its code, .ml files
   in folders if it likes. It keeps everything the tool makes in one folder of its own,
   _mk52/, which can be deleted at any time. That folder is also how the command and the
   page's server meet: neither asks the other anything, each reads what the other wrote.

     _mk52/open/<project>.functions   a project's functions that can be built alone
     _mk52/open/<model>.model         a model as the command last built it: it is open
                                      in the page for as long as this file is there
     _mk52/open/<model>.edges.json    its edges, if it was extracted
     _mk52/open/<model>.<f>.gates     where its shader's gates are entered, with feature
                                      f shown alone (0: the whole model)
     _mk52/assets/<file>.mesh         what a model shows from outside (Asset), as triangles:
                                      one a file, by a number made of its path
     _mk52/front                      the model the page is to bring to the front
     _mk52/state/<model>.json         what the page shows of it and has selected in it
     _mk52/server                     the server running here, if one is: its process
                                      and its port
     _mk52/seen                       written while a page is looking
     _mk52/pictures/<model>.png       the last picture made of it
     _mk52/meshes/<model>.stl         its surface as triangles, as last meshed *)
(* Where the tool is: where it is said to be (MK52_HOME, which the tool's own script
   sets), or, for a released tool, the folder that holds bin/ with the command in it and
   web/ with the page. *)
let home () =
  match Sys.getenv_opt "MK52_HOME" with
  | Some h -> h
  | None ->
      let up = Filename.dirname (Filename.dirname Sys.executable_name) in
      if Sys.file_exists (Filename.concat up "web/view.html") then up else "."

(* Whether the tool is here with its source: then a design's code can be compiled
   against the kernel by dune. A released tool is not. *)
let sourced () = Sys.file_exists (Filename.concat (home ()) "lib/dune")

(* The builder that carries the compiler (embed/), and the folders of compiled
   interfaces it is given. A released tool has them beside the command; in the tool's
   source they are where dune builds them, and the standard library's are the installed
   OCaml's own, which the builder finds by itself. None where there is no builder. *)
let builder () =
  let at p = Filename.concat (home ()) p in
  if Sys.file_exists (at "bin/mk52-build") then Some (at "bin/mk52-build", [ at "interfaces/kernel"; at "interfaces/ocaml" ])
  else if Sys.file_exists (at "_build/default/embed/embed.bc.exe") then Some (at "_build/default/embed/embed.bc.exe", [ at "_build/default/lib/.mk52.objs/byte" ])
  else None

(* What runs the tool: with its source, its script, which builds it first; released, the
   command itself. *)
let command () = if sourced () then Filename.concat (home ()) "mk52" else Sys.executable_name

(* What marks a folder as a design folder where the tool has no source to compile against. *)
let marker = "mk52.design"

(* The projects are compiled where dune compiles by default, _build/: an editor's OCaml
   support reads what it knows of a file from there, and with the projects built
   anywhere else it knows nothing and marks every name as unbound. *)
let build = "_build"

(* The design folder's projects, and a project's code: its .ml files, each by its path. *)
let is_dir p = try Sys.is_directory p with Sys_error _ -> false
(* A project has a dune file, which says how it is compiled; in a design folder made by a
   released tool, which compiles nothing, it is any folder with .ml files in it. *)
let projects () =
  let code p = Sys.file_exists (Filename.concat p "dune") || (Sys.file_exists marker && Array.exists (fun f -> Filename.check_suffix f ".ml") (Sys.readdir p)) in
  List.filter (fun p -> p <> "mk52" && p.[0] <> '_' && p.[0] <> '.' && is_dir p && code p) (List.sort compare (Array.to_list (Sys.readdir ".")))

let rec sources dir =
  List.concat_map
    (fun f ->
      let path = Filename.concat dir f in
      if is_dir path then if f.[0] = '.' || f.[0] = '_' then [] else sources path else if Filename.check_suffix f ".ml" then [ path ] else [])
    (List.sort compare (Array.to_list (Sys.readdir dir)))

let made = "_mk52"

(* A file in a folder of _mk52/, the folder made if it is not there. *)
let folder dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755; dir
let under dir file = Filename.concat (folder (Filename.concat (folder made) dir)) file

let functions project = under "open" (project ^ ".functions")
let model name = under "open" (name ^ ".model")
let edges name = under "open" (name ^ ".edges.json")
let gates name feature = under "open" (Printf.sprintf "%s.%d.gates" name feature)
let asset id = under "assets" (id ^ ".mesh")
let state name = under "state" (name ^ ".json")
let picture name = under "pictures" (name ^ ".png")
let section name = under "pictures" (name ^ ".section.png")
let surface name = under "meshes" (name ^ ".stl")
let outline name = under "outlines" (name ^ ".dxf")

(* Asked for far more often than written: nothing is made for them. *)
let front = Filename.concat made "front"
let server = Filename.concat made "server"
let seen = Filename.concat made "seen"

(* The models that are open: those with a file in _mk52/open/. *)
let opened () =
  let dir = Filename.concat made "open" in
  if not (is_dir dir) then [] else List.filter_map (fun f -> if Filename.check_suffix f ".model" then Some (Filename.chop_suffix f ".model") else None) (List.sort compare (Array.to_list (Sys.readdir dir)))

(* The page, in the tool. *)
let page () = Filename.concat (home ()) "web/view.html"

(* The kernel's source as one number, read when a program runs: a program started
   straight after it was built has the stamp of what it was built from. *)
let kernel () =
  let lib = Filename.concat (home ()) "lib" in
  if Sys.file_exists lib && Sys.is_directory lib then
    Digest.to_hex (Digest.string (String.concat "" (List.map (fun f -> Digest.file (Filename.concat lib f)) (List.sort compare (List.filter (fun f -> Filename.check_suffix f ".ml") (Array.to_list (Sys.readdir lib)))))))
  else "unknown"
