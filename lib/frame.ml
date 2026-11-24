(* Frames: where a shape sits in space. A frame maps its own coordinates to the world's:
   world = r local + t, with r a rotation. Frames are chained forwards, each given in
   the one before; there is no solver here, so nothing is found from mates yet.

   A shape is put in a frame by building it there (Graph.within, or a feature with a
   frame) or by moving it afterwards (Graph.place). Either way it is a rigid move: the
   field reads the world point brought back into the frame, and every slope bound stays
   as it was. *)
module E = Expr

type t = { r : float array array;  (** rows *) t : float array }

let identity = { r = [| [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; 0.; 1. |] |]; t = [| 0.; 0.; 0. |] }
let translate (x, y, z) = { identity with t = [| x; y; z |] }

(* A turn of [degrees] about the vector [axis], through the origin (Rodrigues). *)
let rotate (ax, ay, az) degrees =
  let l = sqrt ((ax *. ax) +. (ay *. ay) +. (az *. az)) in
  let k = [| ax /. l; ay /. l; az /. l |] and a = degrees *. Float.pi /. 180. in
  let c = cos a and s = sin a in
  let cross = [| [| 0.; -.k.(2); k.(1) |]; [| k.(2); 0.; -.k.(0) |]; [| -.k.(1); k.(0); 0. |] |] in
  { identity with r = Array.init 3 (fun i -> Array.init 3 (fun j -> (if i = j then c else 0.) +. (s *. cross.(i).(j)) +. ((1. -. c) *. k.(i) *. k.(j)))) }

let apply f p = Array.init 3 (fun i -> (f.r.(i).(0) *. p.(0)) +. (f.r.(i).(1) *. p.(1)) +. (f.r.(i).(2) *. p.(2)) +. f.t.(i))
let direction f d = Array.init 3 (fun i -> (f.r.(i).(0) *. d.(0)) +. (f.r.(i).(1) *. d.(1)) +. (f.r.(i).(2) *. d.(2)))

(* [child] given in [parent]'s coordinates, as one frame in the world's. *)
let compose parent child =
  { r = Array.init 3 (fun i -> Array.init 3 (fun j -> (parent.r.(i).(0) *. child.r.(0).(j)) +. (parent.r.(i).(1) *. child.r.(1).(j)) +. (parent.r.(i).(2) *. child.r.(2).(j))));
    t = apply parent child.t }

(* A forward chain: each frame given in the one before it. *)
let chain = List.fold_left compose identity

(* The frame's own coordinates as formulas of the world's: local = r^T (world - t). *)
let coords f =
  let world = [| E.x; E.y; E.z |] in
  Array.init 3 (fun j -> Array.fold_left E.add (E.const 0.) (Array.init 3 (fun i -> E.mul (E.const f.r.(i).(j)) (E.sub world.(i) (E.const f.t.(i))))))
