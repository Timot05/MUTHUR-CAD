(* mk52 board <model> ...: the circuit board a model builds (lib/circuit.ml), checked,
   drawn, or written as a page that draws it with the graphics card.

   A board goes with the model whose code made it solid (Circuit.Board.solid): a model
   of a board alone, or an assembly with its boards in it. Where a model has several,
   the board is named after the model. *)
open Mk52
module C = Circuit

let usage = "mk52 board <model> [a board's name] [check [all] [fields] | picture [front|back|inner1|all] [width=2400] [window=x0,y0,x1,y1] | view]"

let rec run name words =
  match words with
  (* for the model's own page: every board of it, as data *)
  | [ "scene" ] -> print_endline (Json.to_string (Json.Arr (List.map C.Page.data (Project.model name).built.boards)))
  | _ -> one name words

and one name words =
  let m = Project.model name in
  let boards = m.built.boards in
  if boards = [] then failwith (Printf.sprintf "%s builds no circuit board: one goes with a model whose code makes it solid (Circuit.Board.solid)" m.name);
  let b, words =
    match words with
    | w :: rest when List.exists (fun (b : C.Board.t) -> b.name = w) boards -> (List.find (fun (b : C.Board.t) -> b.name = w) boards, rest)
    | _ when List.length boards > 1 -> failwith (Printf.sprintf "%s has %d boards: say which after it, one of %s" m.name (List.length boards) (String.concat ", " (List.map (fun (b : C.Board.t) -> b.name) boards)))
    | _ -> (List.hd boards, words)
  in
  let file = m.name ^ if List.length boards > 1 then "." ^ b.name else "" in
  let full out = if Filename.is_relative out then Filename.concat (Sys.getcwd ()) out else out in
  match words with
  | [] -> print_endline (C.Check.report b)
  | "check" :: words when List.for_all (fun w -> w = "all" || w = "fields") words -> print_endline (C.Check.report ~all:(List.mem "all" words) ~fields:(List.mem "fields" words) b)
  | "picture" :: rest ->
      let width = List.find_map (fun w -> Scanf.sscanf_opt w "width=%d" Fun.id) rest and window = List.find_map (fun w -> Scanf.sscanf_opt w "window=%f,%f,%f,%f" (fun a b c d -> (a, b, c, d))) rest in
      let view, said =
        match List.filter (fun w -> not (String.contains w '=')) rest with
        | [] | [ "all" ] -> (C.Draw.Both_sides, "all")
        | [ "front" ] -> (C.Draw.Side C.Copper.Front, "front")
        | [ "back" ] -> (C.Draw.Side C.Copper.Back, "back")
        | [ w ] when Scanf.sscanf_opt w "inner%d" Fun.id <> None -> (C.Draw.Side (C.Copper.Inner (Scanf.sscanf w "inner%d" Fun.id)), w)
        | _ -> failwith usage
      in
      let png, colours = C.Draw.picture ?width ?window b view in
      let out = Place.picture (Printf.sprintf "%s.board.%s" file said) in
      Out_channel.with_open_bin out (fun oc -> output_string oc png);
      if List.length colours <= 24 then List.iter (fun (net, (r, g, bl)) -> Printf.printf "%s is #%02x%02x%02x\n" net r g bl) colours
      else Printf.printf "%d nets, in %d colours taken in turn\n" (List.length colours) (Array.length C.Draw.palette);
      print_endline (full out)
  | [ "view" ] ->
      let out = Place.under "boards" (file ^ ".html") in
      Out_channel.with_open_bin out (fun oc -> output_string oc (C.Page.write ~template:(Filename.concat (Place.home ()) "web/board.html") b));
      print_endline (full out)
  | _ -> failwith usage

(* mk52 footprint <file.kicad_mod> ...: each footprint file as OCaml, printed, to be kept
   in a project as code. A file that cannot be read is said in a comment, and the rest
   are written all the same. *)
let footprints files =
  if files = [] then failwith "mk52 footprint <file.kicad_mod> ...";
  print_string
    "(* Footprints, each made from its maker's file once (mk52 footprint <file> ...) and code\n   since: pads by number, shape, place and size, y up, and the room the part takes. *)\nopen Mk52.Circuit.Footprint\n";
  List.iter
    (fun path ->
      match C.Footprint.read path with
      | f -> Printf.printf "\n(* %s *)\n%s" (Filename.basename path) (C.Footprint.code f)
      | exception Failure why -> Printf.printf "\n(* %s: not made. %s *)\n" (Filename.basename path) why)
    files
