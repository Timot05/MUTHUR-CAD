(* Primitives written as true distances, so their slope bound is 1 everywhere. Negative
   inside. Placing them anywhere in space comes with frames. *)
module E = Expr
open E.Infix

(* The half-space n . p <= d; n need not be unit. *)
let plane name (nx, ny, nz) d =
  let l = Float.sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
  Graph.piece name ((c (nx /. l) * E.x) + (c (ny /. l) * E.y) + (c (nz /. l) * E.z) - c (d /. l))

let sphere name (cx, cy, cz) r = Graph.piece name (E.sqrt (E.square (E.x - c cx) + E.square (E.y - c cy) + E.square (E.z - c cz)) - c r)

(* An endless cylinder along z through (cx, cy). *)
let cylinder name (cx, cy) r = Graph.piece name (E.sqrt (E.square (E.x - c cx) + E.square (E.y - c cy)) - c r)

(* An endless cone about the z axis: its radius is [radius] at z = 0 and grows along +z,
   its side leaning [half_angle] degrees from the axis; its tip is at z = -radius / tan
   half_angle, and nothing lies beyond it. The field is the distance to the side
   wherever the side is what is nearest, which is everywhere but past the tip, and its
   slope is 1 everywhere. *)
let cone name ~radius ~half_angle =
  let a = half_angle *. Float.pi /. 180. in
  Graph.piece name (c (Float.cos a) * (E.sqrt (E.square E.x + E.square E.y) - c radius - (c (Float.tan a) * E.z)))

(* A torus about the z axis at the origin: ring radius [ring], tube radius [tube]. *)
let torus name ~ring ~tube = Graph.piece name (E.sqrt (E.square (E.sqrt (E.square E.x + E.square E.y) - c ring) + E.square E.z) - c tube)
