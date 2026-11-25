(* A project built with no OCaml installed: the builder that carries the compiler.

   It does what a project's own program does (entry.exe, which dune makes from the
   project's files: cli/project.ml), and is called the same way. With a project's folder
   it lists the functions there that build a solid on their own; with one of them named
   too, it builds that one and writes what was built (lib/emit.ml).

   The compiler is in this program, as a library. Each of the project's files is read,
   checked and turned into bytecode here, as a module of its name, in an order in which
   each comes after those it refers to; then the function asked for is called. The
   kernel the files build with is this program's own, so what they make is made of the
   same pieces the tool reads.

   What it needs beside itself is the interfaces to check the files against: the
   kernel's and the standard library's compiled interfaces (.cmi), in folders given
   with -I.

   embed -I dir ... project            the project's functions
   embed -I dir ... project function   that function built, written to standard output
   embed -I dir ... project -serve     the project loaded once and kept: see [serve] *)

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 2) fmt

(* A project's files: every .ml in its folder and the folders below, but entry.ml. *)
let rec sources dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun f ->
         let path = Filename.concat dir f in
         if Sys.is_directory path then if f.[0] = '.' || f.[0] = '_' then [] else sources path
         else if Filename.check_suffix f ".ml" && f <> "entry.ml" then [ path ]
         else [])

let file path = Filename.remove_extension (Filename.basename path)
let name_of path = String.capitalize_ascii (file path)

(* The files in an order to load them in: each after the files it refers to. *)
let ordered paths =
  let names = List.map name_of paths in
  let refers path =
    let ast = In_channel.with_open_bin path (fun ic -> let lb = Lexing.from_channel ic in Location.init lb path; Parse.implementation lb) in
    Depend.free_structure_names := Depend.String.Set.empty;
    Depend.add_implementation Depend.String.Map.empty ast;
    List.filter (fun n -> n <> name_of path && Depend.String.Set.mem n !Depend.free_structure_names) names
  in
  let uses = List.map (fun p -> (name_of p, (p, refers p))) paths in
  let placed = Hashtbl.create 16 and order = ref [] in
  let rec place trail n =
    if List.mem n trail then fail "%s: these files refer to each other in a ring: %s" n (String.concat ", " (List.rev (n :: trail)));
    if not (Hashtbl.mem placed n) then begin
      let path, needs = List.assoc n uses in
      List.iter (place (n :: trail)) needs;
      Hashtbl.replace placed n ();
      order := path :: !order
    end
  in
  List.iter (place []) names;
  List.rev !order

let quiet = Format.make_formatter (fun _ _ _ -> ()) ignore

(* A phrase checked and run, as if typed at OCaml's prompt. *)
let run ?(ppf = Format.err_formatter) text =
  try Toploop.execute_phrase false ppf (!Toploop.parse_toplevel_phrase (Lexing.from_string (text ^ ";;")))
  with e -> (try Location.report_exception ppf e with _ -> ()); false

(* A file loaded as the module of its name: its code as that module's body, checked and
   run. What is wrong in it is said with its place in the file. *)
let loaded path =
  let ppf = Format.err_formatter in
  try
    let body = In_channel.with_open_bin path (fun ic -> let lb = Lexing.from_channel ic in Location.init lb path; Parse.implementation lb) in
    let open Ast_helper in
    Toploop.execute_phrase false ppf (Parsetree.Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc (Some (name_of path))) (Mod.structure body)) ])
  with e -> (try Location.report_exception ppf e with _ -> ()); false

(* The compiler made ready, and the project's files loaded. The warnings and what they
   stop on are dune's for a project, so a file is refused here for what it is there. *)
let load dirs project =
  Clflags.include_dirs := dirs @ !Clflags.include_dirs;
  (* where the standard library's interfaces are given, no installed OCaml is looked for *)
  if List.exists (fun d -> Sys.file_exists (Filename.concat d "stdlib.cmi")) dirs then Clflags.no_std_include := true;
  Clflags.strict_sequence := true;
  Clflags.strict_formats := true;
  ignore (Warnings.parse_options false "@1..3@5..28@30..39@43@46..47@49..57@61..62-40");
  Topeval.init ();
  Toploop.initialize_toplevel_env ();
  let paths = sources project in
  if paths = [] then fail "%s: there are no .ml files in this project" project;
  let seen = Hashtbl.create 16 in
  List.iter (fun p -> (match Hashtbl.find_opt seen (name_of p) with Some other -> fail "%s and %s are both the module %s" other p (name_of p) | None -> ()); Hashtbl.replace seen (name_of p) p) paths;
  List.iter (fun path -> if not (loaded path) then exit 2) (ordered paths);
  paths

(* The functions that build a solid on their own: the values of each file whose type is
   unit -> Mk52.Graph.t. Those that take a unit are found in the file's signature; that
   one gives a Graph.t, however the file names that type, is put to the compiler. *)
let entries paths =
  (* an argument's type as written: the compiler keeps it as one that could be polymorphic *)
  let rec plain t = match Types.get_desc t with Types.Tpoly (t, []) -> plain t | d -> d in
  List.concat_map
    (fun path ->
      let m = name_of path in
      let values =
        let env = !Toploop.toplevel_env in
        match Mtype.scrape env (snd (Env.find_module_by_name (Longident.Lident m) env)).Types.md_type with
        | Types.Mty_signature items ->
            List.filter_map
              (function
                | Types.Sig_value (id, v, _) -> (
                    match Types.get_desc v.Types.val_type with
                    | Types.Tarrow (Asttypes.Nolabel, a, _, _) -> ( match plain a with Types.Tconstr (p, [], _) when Path.same p Predef.path_unit -> Some (Ident.name id) | _ -> None)
                    | _ -> None)
                | _ -> None)
              items
        | _ | (exception Not_found) -> []
      in
      List.filter_map (fun v -> if run ~ppf:quiet (Printf.sprintf "let (_ : unit -> Mk52.Graph.t) = %s.%s" m v) then Some (file path ^ "." ^ v, m ^ "." ^ v) else None) values)
    paths

(* A function called, and what it built written to a file. *)
let build value out =
  run (Printf.sprintf "let mk52_built_ : Mk52.Graph.t = %s ()" value)
  &&
  let graph : Mk52.Graph.t = Obj.obj (Toploop.getvalue "mk52_built_") in
  Out_channel.with_open_bin out (fun oc -> Marshal.to_channel oc (Mk52.Emit.built graph) []);
  true

(* The project kept loaded, for whoever asks one thing after another on standard input:
     list                    its functions, a line each, then a line with a dot
     build <function> <file> that function built into the file: first a line "child"
                             with the process that builds it, then "ok" or "failed"
   Loading is the slow part, and is done once. Each build is done in a copy of this
   process, so one build leaves nothing behind for the next, and one that goes on too
   long can be stopped by stopping its process. What goes wrong is said on standard
   error. *)
let serve entries =
  let say line = print_string (line ^ "\n"); flush stdout in
  say "ready";
  try
    while true do
      match String.split_on_char ' ' (input_line stdin) with
      | [ "list" ] -> List.iter (fun (name, _) -> say name) entries; say "."
      | "build" :: name :: out when out <> [] -> (
          match List.assoc_opt name entries with
          | None -> prerr_endline ("there is no function " ^ name ^ " that builds a solid on its own"); say "failed"
          | Some value -> (
              Format.pp_print_flush Format.err_formatter ();
              flush stderr;
              match Unix.fork () with
              | 0 ->
                  let ok = try build value (String.concat " " out) with e -> prerr_endline (Printexc.to_string e); false in
                  Format.pp_print_flush Format.err_formatter ();
                  flush stderr;
                  Unix._exit (if ok then 0 else 2)
              | pid ->
                  say (Printf.sprintf "child %d" pid);
                  say (match Unix.waitpid [] pid with _, Unix.WEXITED 0 -> "ok" | _ -> "failed")))
      | _ -> say "failed"
    done
  with End_of_file -> ()

let () =
  let rec split dirs rest = function "-I" :: d :: more -> split (d :: dirs) rest more | a :: more -> split dirs (a :: rest) more | [] -> (List.rev dirs, List.rev rest) in
  match split [] [] (List.tl (Array.to_list Sys.argv)) with
  | dirs, [ project ] -> List.iter (fun (name, _) -> print_endline name) (entries (load dirs project))
  | dirs, [ project; "-serve" ] -> serve (entries (load dirs project))
  | dirs, [ project; wanted ] -> (
      match List.assoc_opt wanted (entries (load dirs project)) with
      | None -> fail "%s has no function %s that builds a solid on its own" project wanted
      | Some value -> if not (build value "/dev/stdout") then exit 2)
  | _ -> fail "embed -I dir ... project [function | -serve]"
