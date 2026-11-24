(* The functions of a project that build a solid on their own, gathered into one program.

   A project (a folder of a design folder) is compiled once. Its dune file asks the
   compiler for each file's interface (ocamlc -i), and this reads them: every value
   there of type unit -> Graph.t is a function that can be built alone, named
   <file>.<function>. The interface prints a type as the file wrote it (G.t, where the
   file says module G = Graph), so whether a unit -> something is a unit -> Graph.t is
   put to the compiler: all of them at once, and one by one only if that fails.

   What is printed is the project's entry.ml: run with no argument it lists the
   functions, with one it builds that one and writes what it built (lib/emit.ml).

   entries -I dir ... a.sig b.sig ...    the folders are where the compiled files are *)
let () =
  let rec split dirs sigs = function "-I" :: d :: rest -> split (d :: dirs) sigs rest | s :: rest -> split dirs (s :: sigs) rest | [] -> (List.rev dirs, List.rev sigs) in
  let dirs, sigs = split [] [] (List.tl (Array.to_list Sys.argv)) in
  let candidates =
    List.concat_map
      (fun path ->
        let file = Filename.remove_extension (Filename.basename path) in
        (* a declaration starts at the margin and runs on over the indented lines after it *)
        let items = List.fold_left (fun acc line -> match acc with item :: rest when line <> "" && line.[0] = ' ' -> (item ^ line) :: rest | _ -> line :: acc) [] (In_channel.with_open_bin path In_channel.input_lines) in
        List.filter_map
          (fun item -> match List.filter (( <> ) "") (String.split_on_char ' ' item) with [ "val"; name; ":"; "unit"; "->"; _ ] -> Some (file ^ "." ^ name, String.capitalize_ascii file ^ "." ^ name) | _ -> None)
          (List.rev items))
      sigs
  in
  let builds values =
    let probe = Filename.temp_file "mk52_entry" ".ml" in
    Out_channel.with_open_bin probe (fun oc -> List.iter (fun v -> Printf.fprintf oc "let (_ : unit -> Mk52.Graph.t) = %s\n" v) values);
    let ok = Sys.command (Printf.sprintf "ocamlc -c %s -o %s %s 2>/dev/null" (String.concat " " (List.map (fun d -> "-I " ^ Filename.quote d) dirs)) (Filename.quote (Filename.remove_extension probe)) (Filename.quote probe)) = 0 in
    List.iter (fun ext -> try Sys.remove (Filename.remove_extension probe ^ ext) with Sys_error _ -> ()) [ ".ml"; ".cmi"; ".cmo" ];
    ok
  in
  let entries = if builds (List.map snd candidates) then candidates else List.filter (fun (_, v) -> builds [ v ]) candidates in
  Printf.printf "let entries = [ %s ]\n\n" (String.concat "; " (List.map (fun (name, value) -> Printf.sprintf "(%S, %s)" name value) entries));
  print_string
    "let () =\n\
    \  match Sys.argv with\n\
    \  | [| _ |] -> List.iter (fun (name, _) -> print_endline name) entries\n\
    \  | [| _; name |] -> ( match List.assoc_opt name entries with Some build -> Mk52.Emit.main build | None -> prerr_endline (\"no function called \" ^ name); exit 3)\n\
    \  | _ -> exit 2\n"
