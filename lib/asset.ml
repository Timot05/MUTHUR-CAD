(* Something made elsewhere, shown with a model where it belongs: a STEP file, or a mesh
   (STL). It is seen and nothing more. It is no part of the model's field: nothing cuts
   it, it cuts nothing, and the kernel does not know when it runs into the model.

   A model's code says where one is, as it says where a feature is:

     Asset.show ~frame ~colour:"#3a3d42" "../parts/switch.step"

   in the frame the code is in at that point, or in [frame] given in that one, and of
   [colour] if one is given. The file's
   own origin is put at the frame's, its axes along the frame's; lengths are millimetres.
   The path is from the design folder. The page draws it with the model.

   A STEP file is read by Step and its faces cut into triangles by Mesh; where it is an
   assembly, each part is put where the file places it. *)

type placed = { feature : int; frame : Frame.t;  (** in the world *) path : string; name : string; colour : Colour.t option }

let shown : placed list ref = ref []

let show ?(frame = Frame.identity) ?name ?colour path =
  let name = Option.value ~default:(Filename.remove_extension (Filename.basename path)) name in
  shown := { feature = !Graph.current; frame = Frame.compose !Graph.ambient frame; path; name; colour = Option.map Colour.of_string colour } :: !shown

(* ---- Triangles ---- *)

type mesh = { points : float array;  (** x, y, z a vertex *) normals : float array;  (** outward, a vertex *) triangles : int array  (** three vertices each *) }

(* A STEP file's faces as triangles, no edge further than [tol] from its surface. A part
   placed several times in the file is there several times. *)
let of_step ?(tol = 0.05) path =
  let file = Step.read path in
  let m = Mesh.build ~tol (Step.faces file) in
  let of_face = Hashtbl.create 256 in
  Array.iteri (fun t f -> Hashtbl.replace of_face m.faces.(f).Step.id (t :: Option.value ~default:[] (Hashtbl.find_opt of_face m.faces.(f).Step.id))) m.face;
  let points = Buffer.create 65536 and normals = Buffer.create 65536 and triangles = ref [] and count = ref 0 in
  let add b (v : float array) = Array.iter (fun x -> Buffer.add_int64_le b (Int64.bits_of_float x)) v in
  let v3 a i = [| a.(3 * i); a.((3 * i) + 1); a.((3 * i) + 2) |] in
  (* the triangles of some faces, moved by a placement: their vertices are written anew *)
  let put (at : Step.placement) faces =
    let index = Hashtbl.create 1024 in
    let vertex i =
      match Hashtbl.find_opt index i with
      | Some k -> k
      | None ->
          add points (Step.move at (v3 m.points i));
          add normals (Step.move { at with o = [| 0.; 0.; 0. |] } (v3 m.normals i));
          Hashtbl.replace index i !count;
          incr count;
          !count - 1
    in
    List.iter (fun face -> List.iter (fun t -> triangles := vertex m.triangles.((3 * t) + 2) :: vertex m.triangles.((3 * t) + 1) :: vertex m.triangles.(3 * t) :: !triangles) (Option.value ~default:[] (Hashtbl.find_opt of_face face))) faces
  in
  (* [inner] placed in [outer]: a point of the inner one, in the outer one's coordinates *)
  let within (outer : Step.placement) (inner : Step.placement) =
    let turn v = Step.move { outer with o = [| 0.; 0.; 0. |] } v in
    { Step.o = Step.move outer inner.o; x = turn inner.x; y = turn inner.y; z = turn inner.z }
  in
  let rec product at (p : Step.product) = put at p.faces; List.iter (fun (i : Step.instance) -> product (within at i.at) i.product) p.placed in
  (match Step.tree file with [] -> put Step.identity (Array.to_list (Array.map (fun (f : Step.face) -> f.id) m.faces)) | roots -> List.iter (product Step.identity) roots);
  let floats b = let bytes = Buffer.to_bytes b in Array.init (Bytes.length bytes / 8) (fun i -> Int64.float_of_bits (Bytes.get_int64_le bytes (8 * i))) in
  { points = floats points; normals = floats normals; triangles = Array.of_list (List.rev !triangles) }

(* An STL file, in either of its forms: every triangle has its own three vertices, and
   the normal the file gives it, or its own where the file gives none. *)
let of_stl path =
  let s = In_channel.with_open_bin path In_channel.input_all in
  let n = String.length s in
  let binary = n >= 84 && n = 84 + (50 * Int32.to_int (String.get_int32_le s 80)) in
  let corners =
    if binary then List.init (Int32.to_int (String.get_int32_le s 80)) (fun t -> Array.init 12 (fun k -> Int32.float_of_bits (String.get_int32_le s (84 + (50 * t) + (4 * k)))))
    else begin
      (* "facet normal x y z ... vertex x y z" three times: the numbers after those two words, in order *)
      let words = List.filter (( <> ) "") (String.split_on_char ' ' (String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s)) in
      let rec numbers acc = function
        | ("normal" | "vertex") :: x :: y :: z :: rest -> numbers (float_of_string z :: float_of_string y :: float_of_string x :: acc) rest
        | _ :: rest -> numbers acc rest
        | [] -> Array.of_list (List.rev acc)
      in
      let all = numbers [] words in
      List.init (Array.length all / 12) (fun t -> Array.sub all (12 * t) 12)
    end
  in
  let count = List.length corners in
  let points = Array.make (9 * count) 0. and normals = Array.make (9 * count) 0. in
  List.iteri
    (fun t c ->
      let p k = Array.sub c (3 + (3 * k)) 3 in
      let own = Step.unit (Step.cross Step.(p 1 -| p 0) Step.(p 2 -| p 0)) and given = Array.sub c 0 3 in
      let normal = if Step.norm given > 0.5 then Step.unit given else own in
      for k = 0 to 2 do
        Array.blit (p k) 0 points ((9 * t) + (3 * k)) 3;
        Array.blit normal 0 normals ((9 * t) + (3 * k)) 3
      done)
    corners;
  { points; normals; triangles = Array.init (3 * count) Fun.id }

let read path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".step" | ".stp" -> of_step path
  | ".stl" -> of_stl path
  | other -> failwith (Printf.sprintf "%s: an asset is a STEP file (.step, .stp) or an STL file, not %s" path (if other = "" then "a file without an ending" else other))

(* As the page takes it: how many vertices and how many triangle corners, then each
   vertex's point and normal as six 32-bit numbers, then the corners, a 32-bit vertex
   number each. Little-endian. *)
let written (m : mesh) =
  let vertices = Array.length m.points / 3 in
  let b = Buffer.create (8 + (24 * vertices) + (4 * Array.length m.triangles)) in
  let int v = Buffer.add_int32_le b (Int32.of_int v) and float v = Buffer.add_int32_le b (Int32.bits_of_float v) in
  int vertices;
  int (Array.length m.triangles);
  for i = 0 to vertices - 1 do
    for k = 0 to 2 do float m.points.((3 * i) + k) done;
    for k = 0 to 2 do float m.normals.((3 * i) + k) done
  done;
  Array.iter int m.triangles;
  Buffer.contents b
