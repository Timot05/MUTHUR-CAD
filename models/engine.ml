(* A simplified engine block, ported from mk36's experiments/007_engine_block: a cast
   body, four cylinder bores open at the deck, a crankcase open at the bottom and split
   into bays by bearing webs, and the crankshaft bore through the webs and end walls.
   Dimensions in decimetres.

   One change from mk36: a hole is written as a true distance, r - sqrt (r2), where mk36
   used (r^2 - r2) / 2r. The surface is the same; the slope is 1 everywhere, where
   mk36's grows with the distance from the axis and would have to be declared as 9. *)
open Mk52
module E = Expr
module G = Graph
open E.Infix

let x = E.x and y = E.y and z = E.z
let sq = E.square
let bore_r = 0.3775 and pitch = 0.86
let bores = [ -1.5; -0.5; 0.5; 1.5 ]
let half_width = 0.6 and front = -1.9 and back = 1.9 and deck = 2.2
let bore_bottom = 0.7
let cavity_half_width = 0.5 and cavity_top = 0.85
let web = 0.16 and end_wall = 0.12
let crank_z = 0.3 and crank_r = 0.22
let all = function [] -> invalid_arg "all" | f :: rest -> List.fold_left G.intersection f rest
let any = function [] -> invalid_arg "any" | f :: rest -> List.fold_left G.union f rest

(* Half-spaces, field <= 0 inside: [le name v h] is v <= h, [ge name v h] is v >= h. *)
let le name v h = G.piece name (v - c h)
let ge name v h = G.piece name (c h - v)

(* The material around a hole of radius r, where r2 is the squared distance to its axis. *)
let hole name r2 r = G.piece name (c r - E.sqrt r2)

let body () =
  G.feature __POS__ "body" (fun () ->
      all [ le "right" x half_width; ge "left" x (-.half_width); le "back" y back; ge "front" y front; le "deck" z deck; ge "pan-rail" z 0. ])

(* Material outside bore i: around its cylinder, or below its bottom. *)
let bore i yc =
  let n = Printf.sprintf "bore%d" Stdlib.(i + 1) in
  G.feature __POS__ n (fun () -> any [ hole n (sq x + sq (y - c yc)) bore_r; le (n ^ ".bottom") z bore_bottom ])

(* Material around the crankcase: outside the cavity (beyond one of its five walls; the
   bottom is open), or in one of the bearing webs that split it into four bays. *)
let crankcase () =
  G.feature __POS__ "crankcase" @@ fun () ->
  let cavity =
    [ ge "case.right" x cavity_half_width; le "case.left" x (-.cavity_half_width); le "case.front" y (front +. end_wall);
      ge "case.back" y (back -. end_wall); ge "case.roof" z cavity_top ]
  in
  let webs =
    List.mapi
      (fun i yw ->
        let n = Printf.sprintf "web%d" Stdlib.(i + 1) in
        G.feature __POS__ n (fun () -> all [ ge (n ^ ".front") y (yw -. (web /. 2.)); le (n ^ ".back") y (yw +. (web /. 2.)) ]))
      [ -.pitch; 0.; pitch ]
  in
  any (cavity @ webs)

let crank () = G.feature __POS__ "crank-bore" (fun () -> hole "crank-bore" (sq x + sq (z - c crank_z)) crank_r)

let graph = function
  | "block-body" -> body ()
  | "block-bores" -> all (body () :: List.mapi (fun i b -> bore i (b *. pitch)) bores)
  | "block-crankcase" -> all [ body (); crankcase (); crank () ]
  | "block" -> all ((body () :: List.mapi (fun i b -> bore i (b *. pitch)) bores) @ [ crankcase (); crank () ])
  | n -> invalid_arg ("unknown model " ^ n)

(* What mk36 gives at GRID=64: corners, edges, closed edges; seconds on ten cores and on one. *)
let expected =
  [ ("block-body", (8, 12, 0), (0.130, 0.265)); ("block-bores", (8, 20, 8), (0.446, 0.958)); ("block-crankcase", (40, 70, 10), (0.508, 1.025));
    ("block", (64, 110, 14), (0.940, 2.002)) ]

let lo = [| -0.73; -2.03; -0.13 |] and hi = [| 0.71; 2.01; 2.33 |]
