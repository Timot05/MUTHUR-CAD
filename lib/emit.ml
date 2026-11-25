(* A model handed from one program to another, as a file.

   Each feature of a part is a file of its own (<part>/<feature>.ml, in a design folder) with
   [let graph () = ...]. It is compiled into a small program of its own, which builds the
   graph and writes it out ([main]): the graph, the features made on the way, the
   sketches drawn and the assets shown. The command (cli/main.ml) runs that program, adds where the model is to
   be looked at, and writes the whole where the page finds it ([write]); what the page asks
   of the model is answered from that file ([read], cli/scene.ml).

   The form is OCaml's own (Marshal), which is only safe between programs built from the
   same kernel. So a file starts with the kernel's stamp, and a reader with another
   stamp refuses it. *)
type built = { graph : Graph.t; features : Graph.feature list; sketches : Sketch.drawn list; assets : Asset.placed list; meant : (string * string) list  (** pairs of parts said to be meant to touch (Clash.allow) *) }

(* What has been built in this program so far, around its graph. *)
(* A feature's colour: its own, or that of the nearest feature around it that has one. *)
let rec coloured (features : Graph.feature list) (f : Graph.feature) =
  match f.colour with Some c -> Some c | None -> Option.bind (List.find_opt (fun (x : Graph.feature) -> x.fid = f.parent) features) (coloured features)

let built graph = { graph; features = !Graph.features; sketches = List.rev !Sketch.drawn; assets = List.rev !Asset.shown; meant = List.rev !Clash.meant }

type t = { name : string; lo : float array; hi : float array; built : built }

(* What a feature's program does: build, and write what was built to its output. *)
let main build =
  let graph = build () in
  set_binary_mode_out stdout true;
  Marshal.to_channel stdout (built graph) []

let built_of ic : built = Marshal.from_channel ic

let write ~kernel path (m : t) =
  let tmp = path ^ ".tmp" in
  Out_channel.with_open_bin tmp (fun oc -> output_string oc (kernel ^ "\n"); Marshal.to_channel oc m []);
  Sys.rename tmp path

let read ~kernel path : t =
  In_channel.with_open_bin path (fun ic ->
      if input_line ic <> kernel then failwith "this model was written by another build of the kernel";
      Marshal.from_channel ic)
