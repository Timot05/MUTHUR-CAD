(* Shapes with corners, ported from mk36's experiments/002_corners: the same formulas,
   so the two kernels can be compared edge for edge. *)
open Mk52
module E = Expr
module G = Graph

(* a x + b y + c z - d <= 0, named. The normals here are unit or declared. *)
let plane name (a, b, c, d) =
  let lip = sqrt ((a *. a) +. (b *. b) +. (c *. c)) in
  G.piece ~lip name E.(sub (add (add (mul (const a) x) (mul (const b) y)) (mul (const c) z)) (const d))

let box p (x0, x1) (y0, y1) (z0, z1) =
  [ plane (p ^ "+x") (1., 0., 0., x1); plane (p ^ "-x") (-1., 0., 0., -.x0); plane (p ^ "+y") (0., 1., 0., y1);
    plane (p ^ "-y") (0., -1., 0., -.y0); plane (p ^ "+z") (0., 0., 1., z1); plane (p ^ "-z") (0., 0., -1., -.z0) ]

let all = function [] -> invalid_arg "all" | f :: rest -> List.fold_left G.intersection f rest
let cube_faces = box "" (-0.5, 0.5) (-0.5, 0.5) (-0.5, 0.5)
let face n = List.nth cube_faces n

let rec graph = function
  | "cube" -> all cube_faces
  | "cube-fillet" -> all (G.fillet_intersection ~width:0.3 ~name:"fillet" (face 0) (face 2) :: List.filteri (fun i _ -> i <> 0 && i <> 2) cube_faces)
  | "pyramid" ->
      let s = 0.6 in
      all
        [ plane "+x" (1., 0., s, s *. 0.4); plane "-x" (-1., 0., s, s *. 0.4); plane "+y" (0., 1., s, s *. 0.4);
          plane "-y" (0., -1., s, s *. 0.4); plane "base" (0., 0., -1., 0.4) ]
  | "l-block" -> G.union (all (box "a" (-0.5, 0.5) (-0.5, 0.5) (-0.5, 0.))) (all (box "b" (-0.4, 0.1) (-0.3, 0.3) (-0.3, 0.5)))
  | "cube-hole" ->
      (* 0.0625 - (x^2 + y^2): not a distance; its slope is 2 r, at most 2.4 in the domain. *)
      all (cube_faces @ [ G.piece ~lip:2.4 "hole" E.(sub (const 0.0625) (add (square x) (square y))) ])
  | "cube-three-fillets" ->
      let f = G.fillet_intersection ~width:0.3 in
      all (f ~name:"fillet-z" (f ~name:"fillet-xy" (face 0) (face 2)) (face 4) :: [ face 1; face 3; face 5 ])
  | "cube-shell" -> G.shell ~thickness:0.1 (graph "cube")
  | "cube-shell-open" -> G.shell ~open_faces:[ "+z" ] ~thickness:0.1 (graph "cube")
  | "cube-fillet-shell-open" -> G.shell ~open_faces:[ "+z" ] ~thickness:0.1 (graph "cube-fillet")
  | "l-block-shell-open" -> G.shell ~open_faces:[ "a-z" ] ~thickness:0.1 (graph "l-block")
  | n -> invalid_arg ("unknown model " ^ n)

(* Each model with what mk36 gives at GRID=64 on an M1 Max: corners, edges and closed
   edges, then seconds on ten cores (the best of the runs measured) and on one. *)
let expected =
  [ ("cube", (8, 12, 0), (0.281, 1.045)); ("cube-fillet", (10, 15, 0), (0.312, 1.238)); ("pyramid", (5, 8, 0), (0.227, 0.818));
    ("l-block", (16, 24, 0), (0.454, 2.109)); ("cube-hole", (8, 14, 2), (0.406, 1.276)); ("cube-three-fillets", (14, 22, 0), (0.446, 1.379));
    ("cube-shell", (16, 24, 0), (0.766, 2.659)); ("cube-shell-open", (16, 24, 0), (0.720, 2.738));
    ("cube-fillet-shell-open", (20, 30, 0), (0.825, 3.075)); ("l-block-shell-open", (32, 48, 0), (1.467, 5.707)) ]

(* Asymmetric, so cell walls avoid the shapes' round coordinates. *)
let lo = [| -0.83; -0.81; -0.8 |] and hi = [| 0.79; 0.84; 0.82 |]
