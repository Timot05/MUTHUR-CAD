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
          ("paint", prop "boolean" "Each part in the colour its code gives it, not each face in a colour of its own.");
          ("width", prop "integer" "Default 800."); ("height", prop "integer" "Default 600.") ]
        [],
      fun args ->
        let num k d = Json.number ~default:d (Json.member k args) in
        let w = max 64 (min 2000 (int_of_float (num "width" 800.))) and h = max 64 (min 2000 (int_of_float (num "height" 600.))) in
        let target = match Json.list (Json.member "target" args) with [ x; y; z ] -> Some [| Json.number x; Json.number y; Json.number z |] | _ -> None in
        let camera = { Render.yaw = num "yaw" 40.; pitch = num "pitch" 26.; dist = num "distance" (Render.fit ~w ~h); target } in
        [ Image (Png.base64 (Project.picture ~w ~h ~camera ~edges:(Json.bool (Json.member "edges" args)) ~paint:(Json.bool (Json.member "paint" args)) (Project.model (model_of args))), "image/png") ] );
    ( "skins",
      "A model's faces of no thickness: where two of its faces lie on one surface with air on both sides, as when a cut ends level with another face, or a pocket's floor is the top of a hollow. Nothing is really there, and a cut or a mesh does not see it, but it is where the code meant two faces to pass one another: have one go a little further. Each is said with the two faces it lies between and where. Run it after cutting into a part.",
      schema [ model_prop ] [],
      fun args -> [ Text (Project.skins (Project.model (model_of args))) ] );
    ( "clash",
      "Whether a model's parts run into each other, touch, or come nearer than a gap: every pair of its parts (its features that are in no other), and its bought parts, which are held by the corners and middles of their triangles, against the designed ones. Said as lists: what is into each other and by how much, what touches, what is nearer than the gap, each with where; and apart from those, what the design says is meant (Mk52.Clash.allow \"a part\" \"another\", called as the model is built). With against, the model's parts are held against another model's instead: a housing against what moves in it, moved. Run it after a change to an assembly, as the tests are run after a change to code. It says where the solids are, not whether the thing can be put together. A model of many parts takes a minute.",
      schema [ model_prop; ("against", prop "string" "Another model, to hold this one's parts against."); ("gap", prop "number" "Pairs nearer than this are listed. Default 0.3.") ] [],
      fun args ->
        let against = match Json.member "against" args with Json.Str o -> Some (Project.model o) | _ -> None in
        let limit = match Json.member "gap" args with Json.Num v when v >= 0. -> Some v | _ -> None in
        [ Text (Project.clash ?limit ?against (Project.model (model_of args))) ] );
    ( "section",
      "A cut through a model, made by the kernel: what is solid in the plane where one coordinate is a number, each part in a colour of its own with a dark line round it, and a scale along two sides. For what is inside or between parts, which a picture from outside does not show: a clearance, how thick a wall is, what touches what, what is under what. Say the plane with axis and at. To look closer, give either or both of the two other coordinates as [from, to]; left out, all that is solid in the plane is shown. A cut at a z is seen from +z, x to the right and y up; at a y, from -y, x to the right and z up; at an x, from +x, y to the right and z up. With the picture comes each part cut, its colour and where in the cut it is. With text, the cut comes as letters instead, a letter a part and a dot for air: for when a column counts more than a look.",
      schema
        [ model_prop;
          ("axis", prop "string" "x, y or z: the coordinate that is the same all over the cut.");
          ("at", prop "number" "That coordinate's value.");
          ("x", prop "array" "If the cut is not at an x: how much of x to show, [from, to].");
          ("y", prop "array" "If the cut is not at a y: how much of y to show, [from, to].");
          ("z", prop "array" "If the cut is not at a z: how much of z to show, [from, to].");
          ("paint", prop "boolean" "Fill each part with its own colour in the model, where it has one, instead of colours that tell the parts apart.");
          ("text", prop "boolean" "The cut as letters, not a picture.");
          ("width", prop "integer" "The picture's width in pixels. Default 1000.");
          ("columns", prop "integer" "With text: how many letters across. Default 100.") ]
        [ "axis"; "at" ],
      fun args ->
        let axis = match Json.member "axis" args with Json.Str "x" -> 0 | Json.Str "y" -> 1 | Json.Str "z" -> 2 | _ -> failwith "axis is x, y or z" in
        let at = match Json.member "at" args with Json.Num v -> v | _ -> failwith "at is a number: where along the axis the cut is" in
        let span k = match Json.member k args with Json.Arr [ a; b ] -> Some (Json.number a, Json.number b) | Json.Null -> None | _ -> failwith (k ^ " is [from, to]") in
        let cut = { Project.axis; at; spans = Array.map span Project.coordinate } in
        let m = Project.model (model_of args) in
        let whole k = match Json.member k args with Json.Num v -> Some (int_of_float v) | _ -> None in
        if Json.bool (Json.member "text" args) then [ Text (Project.section_text ?cols:(whole "columns") m cut) ]
        else
          let text, png = Project.section ?w:(Option.map (fun w -> min 2000 w) (whole "width")) ~paint:(Json.bool (Json.member "paint" args)) m cut in
          [ Image (Png.base64 png, "image/png"); Text text ] );
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
                 "Tools into the mk52 kernel and its page. To see a model yourself, call picture: the kernel draws it from any side; nothing need be open. After cutting into a part, call skins: it finds faces of no thickness, where two faces lie on one surface, and says which. To check an assembly, call clash: which of its parts run into each other, touch or are too near, bought parts too. To see into it, call section: a cut through it at an x, a y or a z, the parts in colours with a scale, which shows what a picture from outside cannot (a clearance, a wall, what touches what). The page is the person's: a browser page beside their editor (mk52 open opens it), listing the code's functions and showing each one they have opened. An open model is built again by itself whenever a file is saved, so they see what you do as you do it. viewer_state says what they have selected or picked in a model, for when they say \"this face\" or \"here\".\n\
                  You are in a design folder: a folder of projects. A project is a folder here, <project>/, holding its code: .ml files, in folders if it likes, all compiled together. A file there is the module of its name, capitalised, to the others (bore.ml is Bore), with no import. Every function that takes () and returns a solid can be built and opened on its own, and is a model: <project>.<file>.<function>, or <project>.<file> for a file's graph. A function that needs arguments is seen where it is called. mk52/ is a link to the tool: its kernel is mk52/lib/, and it is not yours to change.\n\
                  The command: mk52 models lists the projects and their functions; mk52 picture <model> [yaw= pitch= distance= target= edges] writes a picture and prints its path; mk52 skins <model> says where two of its faces lie on one surface and leave a face of no thickness; mk52 clash <model> [another model] [gap=0.3] says which of its parts are into each other, touch or are nearer than the gap; mk52 section <model> z=<number> (or x=, y=; the other two from:to to look closer) writes a cut through it, the parts in colours with a scale, for what is inside or between parts; mk52 extract <model> prints its edges and corners; mk52 outline <model> writes a flat part's outline, a board's, as a DXF file of lines, arcs and circles; mk52 view <model> opens it in the person's page; mk52 new <project> <file> makes a file with a block to start from; mk52 brief <model> says how to work on one. If you are told to work on a function, call feature_brief (or run the brief) first.\n\
                  Modelling. A model is built from features, Graph.feature ?frame __POS__ \"name\" (fun () -> ...), which nest, and which may sit in a frame (mk52/lib/frame.ml: translate, rotate, chain). What there is to build with, in mk52/lib/:\n\
                  - Sketch (sketch.ml): a closed outline of lines and arcs drawn with start, line, arc and close, each span a named face, then Sketch.extrude or Sketch.revolve, with holes as loops of their own. Sketch.extrude ~chamfer_lo ~chamfer_hi cuts the edge round an end at 45 degrees, exactly: use it rather than a loft for a chamfered plate or shell. Reach for it first when a feature is a profile pulled straight or turned about an axis; mk52/models/bracket.ml is a worked example. A sketch is drawn in the x and y of the frame it is built in.\n\
                  - Colour: Graph.feature ~colour:\"#rrggbb\" gives a feature a colour, and what is made inside it has it too; the page shows the colours with Faces off.\n\
                  - Asset.show ~frame \"../parts/switch.step\" (asset.ml): something bought or made elsewhere, a STEP file or an STL mesh, shown with the model in a frame; the path is from the design folder. It is seen only: no part of the solid, so nothing cuts it. Call it in the function that builds the model, before the solid is returned.\n\
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
