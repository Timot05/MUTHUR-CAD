(* The kernel and the page as tools for an agent (Claude Code), over the Model Context
   Protocol: JSON-RPC, one message a line, on standard input and output. Claude Code
   starts it itself, as `mk52 mcp` (see the design folder's .mcp.json).

   Everything an agent needs to see or measure is worked out here, by the kernel
   (cli/project.ml): a picture, a probe, an extraction, the brief. One tool reads what
   the page wrote down: what the person has selected in a model. *)
open Mk52

(* ---- The tools ---- *)

type content = Text of string | Image of string * string  (** base64, mime type *)

let schema properties required =
  Json.Obj [ ("type", Json.Str "object"); ("properties", Json.Obj properties); ("required", Json.Arr (List.map (fun r -> Json.Str r) required)) ]

let prop kind description = Json.Obj [ ("type", Json.Str kind); ("description", Json.Str description) ]
let model_prop = ("model", prop "string" "The model's name. A project's function that builds a solid is <project>.<file>.<function>; <project>.<file> is that file's graph, or its only function. Left out: the model in front in the page.")
let read path = try Some (In_channel.with_open_bin path In_channel.input_all) with Sys_error _ -> None

(* The model a tool is about: the one named, or the one in front in the page. *)
let model_of args =
  match (Json.member "model" args, read Place.front) with
  | Json.Str m, _ -> m
  | _, Some front when List.mem (String.trim front) (Place.opened ()) -> String.trim front
  | _ -> failwith "name the model: none is in front in the page to say which is meant"

let pretty j =
  (* JSON laid out one member a line, two levels deep: enough to read *)
  let rec go indent = function
    | Json.Obj l when indent < 2 -> String.concat "\n" (List.map (fun (k, v) -> Printf.sprintf "%s%s: %s" (String.make (2 * indent) ' ') k (match v with Json.Obj _ | Json.Arr (Json.Obj _ :: _) -> "\n" ^ go (indent + 1) v | _ -> Json.to_string v)) l)
    | Json.Arr l when indent < 2 -> String.concat "\n" (List.map (go indent) l)
    | v -> String.make (2 * indent) ' ' ^ Json.to_string v
  in
  go 0 j

let tools =
  [ ( "viewer_state",
      "What the person is looking at and pointing at in a model open in the page: the feature or face highlighted, the last points they picked, the camera. Call this when they say \"this face\", \"here\", \"what I selected\". The view is the person's: nothing here changes it.",
      schema [ model_prop ] [],
      fun args ->
        let model = model_of args in
        if not (List.mem model (Place.opened ())) then [ Text (Printf.sprintf "%s is not open in the page, so nothing is selected in it. It is opened with: mk52 view %s" model model) ]
        else match Option.map Json.parse (read (Place.state model)) with Some state -> [ Text (pretty state) ] | None | (exception Failure _) -> [ Text (model ^ " is open, but has not been looked at yet: nothing is selected in it.") ] );
    ( "picture",
      "A picture of a model, made by the kernel itself (it need not be open anywhere): the model as its field says it is, each face in its own colour, and optionally its extracted edges. Look from anywhere with yaw, pitch, distance and target.",
      schema
        [ model_prop;
          ("yaw", prop "number" "Degrees about z: 0 looks along -x, 90 along -y. Default 40.");
          ("pitch", prop "number" "Degrees: 90 looks straight down, -90 straight up. Default 26.");
          ("distance", prop "number" "From the target, in domain diagonals. Default shows everything; 0.3 is close.");
          ("target", Json.Obj [ ("type", Json.Str "array"); ("items", Json.Obj [ ("type", Json.Str "number") ]); ("description", Json.Str "The point looked at, [x, y, z]. Default: the middle of the model's box.") ]);
          ("edges", prop "boolean" "Draw the extracted edges too.");
          ("width", prop "integer" "Default 800."); ("height", prop "integer" "Default 600.") ]
        [],
      fun args ->
        let num k d = Json.number ~default:d (Json.member k args) in
        let w = max 64 (min 2000 (int_of_float (num "width" 800.))) and h = max 64 (min 2000 (int_of_float (num "height" 600.))) in
        let target = match Json.list (Json.member "target" args) with [ x; y; z ] -> Some [| Json.number x; Json.number y; Json.number z |] | _ -> None in
        let camera = { Render.yaw = num "yaw" 40.; pitch = num "pitch" 26.; dist = num "distance" (Render.fit ~w ~h); target } in
        [ Image (Png.base64 (Project.picture ~w ~h ~camera ~edges:(Json.bool (Json.member "edges" args)) (Project.model (model_of args))), "image/png") ] );
    ( "feature_brief",
      "For an agent about to work on one function of a project: where it is, how to work and what there is to build with. Call this first when you are told to work on one.",
      schema [ ("name", prop "string" "The function, as <project>.<file>.<function> or <project>.<file>.") ] [ "name" ],
      fun args -> [ Text (Project.brief (Json.string (Json.member "name" args))) ] );
    ( "kernel_probe",
      "The model's field at points in space, straight from the kernel: the value (negative inside the solid, positive outside, in the model's length unit: the distance to the surface where the field is a distance), the face that is active there and the feature that made it. Use it to check a dimension or to see which feature answers somewhere. The model is built from its file as it is now.",
      schema [ model_prop; ("points", Json.Obj [ ("type", Json.Str "array"); ("description", Json.Str "Points as [x, y, z].") ; ("items", Json.Obj [ ("type", Json.Str "array"); ("items", Json.Obj [ ("type", Json.Str "number") ]) ]) ]) ] [ "points" ],
      fun args ->
        let m = Project.model (model_of args) in
        let p = Prog.compile m.built.graph in
        let c = Prog.ctx p and whole = Prog.whole p in
        [ Text
            (String.concat "\n"
               (List.map
                  (fun pt ->
                    let x = Array.of_list (List.map Json.number (Json.list pt)) in
                    let s = Prog.eval p c whole x in
                    let feature = match List.find_opt (fun (f : Graph.feature) -> f.fid = s.feature) m.built.features with Some f -> Printf.sprintf ", feature %s (%s:%d)" f.path f.file f.line | None -> "" in
                    Printf.sprintf "(%g, %g, %g): field %.6g, face %s%s" x.(0) x.(1) x.(2) s.d.v (Prog.face_name p s.face) feature)
                  (Json.list (Json.member "points" args)))) ] );
    ( "kernel_extract",
      "Edges and corners of a model, extracted now by the kernel: the counts, every flag (a place the result may be wrong), and each edge with the two faces it separates and whether it is sharp (with how far the surface turns across it, in degrees) or smooth (a tangent seam: a fillet's border, a line running into an arc). It samples, so a feature thinner than a cell can be missed; a finer depth costs more.",
      schema [ model_prop; ("depth", prop "integer" "5, 6 or 7 for 32, 64 or 128 cells across the model. Default 6.") ] [],
      fun args -> [ Text (fst (Project.extract ~depth:(int_of_float (Json.number ~default:6. (Json.member "depth" args))) (Project.model (model_of args)))) ] ) ]

(* ---- The protocol ---- *)

let reply id result = print_string (Json.to_string (Json.Obj [ ("jsonrpc", Json.Str "2.0"); ("id", id); ("result", result) ])); print_char '\n'; flush stdout
let refuse id code message = print_string (Json.to_string (Json.Obj [ ("jsonrpc", Json.Str "2.0"); ("id", id); ("error", Json.Obj [ ("code", Json.Num code); ("message", Json.Str message) ]) ])); print_char '\n'; flush stdout

let content items =
  Json.Arr
    (List.map
       (function
         | Text t -> Json.Obj [ ("type", Json.Str "text"); ("text", Json.Str t) ]
         | Image (data, mime) -> Json.Obj [ ("type", Json.Str "image"); ("data", Json.Str data); ("mimeType", Json.Str mime) ])
       items)

let handle message =
  let id = Json.member "id" message and params = Json.member "params" message in
  match Json.string (Json.member "method" message) with
  | "initialize" ->
      (* we speak whichever version the client asks for: tools work the same in all *)
      let version = Json.string ~default:"2025-06-18" (Json.member "protocolVersion" params) in
      reply id
        (Json.Obj
           [ ("protocolVersion", Json.Str version); ("capabilities", Json.Obj [ ("tools", Json.Obj []) ]);
             ("serverInfo", Json.Obj [ ("name", Json.Str "mk52"); ("version", Json.Str "0.1") ]);
             ( "instructions",
               Json.Str
                 "Tools into the mk52 kernel and its page. To see a model yourself, call picture: the kernel draws it from any side; nothing need be open. The page is the person's: a browser page beside their editor (mk52 open opens it), listing the code's functions and showing each one they have opened. An open model is built again by itself whenever a file is saved, so they see what you do as you do it. viewer_state says what they have selected or picked in a model, for when they say \"this face\" or \"here\".\n\
                  You are in a design folder: a folder of projects. A project is a folder here, <project>/, holding its code: .ml files, in folders if it likes, all compiled together. A file there is the module of its name, capitalised, to the others (bore.ml is Bore), with no import. Every function that takes () and returns a solid can be built and opened on its own, and is a model: <project>.<file>.<function>, or <project>.<file> for a file's graph. A function that needs arguments is seen where it is called. mk52/ is a link to the tool: its kernel is mk52/lib/, and it is not yours to change.\n\
                  The command: mk52 models lists the projects and their functions; mk52 picture <model> [yaw= pitch= distance= target= edges] writes a picture and prints its path; mk52 extract <model> prints its edges and corners; mk52 view <model> opens it in the person's page; mk52 new <project> <file> makes a file with a block to start from; mk52 brief <model> says how to work on one. If you are told to work on a function, call feature_brief (or run the brief) first.\n\
                  Modelling. A model is built from features, Graph.feature ?frame __POS__ \"name\" (fun () -> ...), which nest, and which may sit in a frame (mk52/lib/frame.ml: translate, rotate, chain). What there is to build with, in mk52/lib/:\n\
                  - Sketch (sketch.ml): a closed outline of lines and arcs drawn with start, line, arc and close, each span a named face, then Sketch.extrude or Sketch.revolve, with holes as loops of their own. Reach for it first when a feature is a profile pulled straight or turned about an axis; mk52/models/bracket.ml is a worked example. A sketch is drawn in the x and y of the frame it is built in.\n\
                  - Sketch.loft [ Sketch.section z0 outline0; Sketch.section ~holes z1 outline1; ... ]: a solid straight through sections drawn at heights along the frame's z, closed by \"bottom\" and \"top\". Draw the sections alike (as many spans, the same names in the same order) and it is the exact straight loft; anything else is blended.\n\
                  - Solid: plane, sphere, cylinder, cone, torus, as true distances.\n\
                  - Graph: union, intersection, subtract, offset, shell, round_intersection and round_union (fillets of a given radius).\n\
                  - Spline (spline.ml): a free-form face as one B-spline height patch; Fit.patch (fit.ml): one as fitted pieces.\n\
                  mk52/README.md says what each file holds." ) ])
  | "ping" -> reply id (Json.Obj [])
  | "tools/list" -> reply id (Json.Obj [ ("tools", Json.Arr (List.map (fun (name, description, input, _) -> Json.Obj [ ("name", Json.Str name); ("description", Json.Str description); ("inputSchema", input) ]) tools)) ])
  | "tools/call" -> (
      let name = Json.string (Json.member "name" params) in
      match List.find_opt (fun (n, _, _, _) -> n = name) tools with
      | None -> refuse id (-32602.) ("no tool called " ^ name)
      | Some (_, _, _, run) ->
          (* a tool that fails says so as its result, so the agent can read why *)
          let items, failed =
            try (run (match Json.member "arguments" params with Json.Obj _ as a -> a | _ -> Json.Obj []), false) with
            | Failure m -> ([ Text m ], true)
            | e -> ([ Text (Printexc.to_string e) ], true)
          in
          reply id (Json.Obj [ ("content", content items); ("isError", Json.Bool failed) ]))
  | m when id = Json.Null -> ignore m (* a notification: nothing to answer *)
  | m -> refuse id (-32601.) ("no method " ^ m)

let run () =
  try
    while true do
      let line = input_line stdin in
      if String.trim line <> "" then match Json.parse line with message -> handle message | exception Failure m -> refuse Json.Null (-32700.) m
    done
  with End_of_file -> ()
