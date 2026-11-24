(* Small shapes that are hard for topology extraction: two or three primitives each,
   chosen for what goes wrong, with the corners and edges the exact solid has. Lengths
   in millimetres; at depth 6 a cell is 1 mm.

   The kinds of trouble:
   - singular: two surfaces tangent at a point of their own intersection curve, so the
     curve crosses itself there and has no tangent;
   - touching: two surfaces that meet without crossing, at a point or along a curve;
   - grazing: surfaces that cross at a very small angle, or a very sharp edge;
   - small: features thinner or smaller than a cell;
   - aligned: faces lying on cell walls, on each other, or the same shape twice;
   - plain: nothing wrong; there to catch false alarms. *)
open Mk52
module E = Expr
module G = Graph
open E.Infix

type case = { name : string; kind : string; why : string; expected : int * int * int;  (** corners, edges, closed edges *) lo : float array; hi : float array; graph : unit -> G.t }

let lo = [| -31.; -29.; -27. |] and hi = [| 33.; 35.; 30. |]
let x = E.x and y = E.y and z = E.z
let sq = E.square

(* Cylinders of radius r along x and along y, through the origin; along z, Solid.cylinder. *)
let along_x name r = G.piece name (E.sqrt (sq y + sq z) - c r)
let along_y ?(dz = 0.) name r = G.piece name (E.sqrt (sq x + sq (z - c dz)) - c r)
let all = function [] -> invalid_arg "all" | f :: rest -> List.fold_left G.intersection f rest

let box p (x0, x1) (y0, y1) (z0, z1) =
  all
    [ Solid.plane (p ^ "+x") (1., 0., 0.) x1; Solid.plane (p ^ "-x") (-1., 0., 0.) (-.x0); Solid.plane (p ^ "+y") (0., 1., 0.) y1;
      Solid.plane (p ^ "-y") (0., -1., 0.) (-.y0); Solid.plane (p ^ "+z") (0., 0., 1.) z1; Solid.plane (p ^ "-z") (0., 0., -1.) (-.z0) ]

(* A plate from z = -t to 0 with a hole of radius r through it, off the cells' centres. *)
let plate t r = all [ Solid.plane "top" (0., 0., 1.) 0.; Solid.plane "bottom" (0., 0., -1.) t; G.complement (Solid.cylinder "hole" (1.3, -0.7) r) ]

(* A cone with its apex at (0, 0, h), opening downwards with half-angle a degrees: the
   distance to its slanted surface. Its gradient breaks at the apex and along the axis. *)
let cone h a =
  let a = a *. Float.pi /. 180. in
  G.piece "cone" ((c (cos a) * E.sqrt (sq x + sq y)) + (c (sin a) * (z - c h)))

let case ?(lo = lo) ?(hi = hi) name kind expected why graph = { name; kind; why; expected; lo; hi; graph }

let cases =
  [ case "two-cylinders-equal" "singular" (2, 4, 0) "two cylinders of the same radius crossing square: the curve crosses itself at two points where they are tangent" (fun () ->
        G.intersection (along_x "cx" 10.) (along_y "cy" 10.));
    case "two-cylinders-unequal" "plain" (0, 2, 2) "the same with radii 10 and 7: two separate loops, nothing singular" (fun () -> G.intersection (along_x "cx" 10.) (along_y "cy" 7.));
    case "viviani" "singular" (1, 2, 0) "a sphere and a cylinder of half its radius through its centre: a figure of eight, tangent where it crosses itself" (fun () ->
        G.intersection (Solid.sphere "sphere" (0., 0., 0.) 20.) (Solid.cylinder "cylinder" (10., 0.) 10.));
    case "villarceau" "singular" (2, 4, 0) "a torus cut by the plane through its centre that touches it twice: two circles crossing at the two points of tangency" (fun () ->
        let s = 1. /. 3. in
        G.intersection (Solid.torus "torus" ~ring:18. ~tube:6.) (Solid.plane "cut" (-.s, 0., Float.sqrt (1. -. (s *. s))) 0.));
    case "sphere-on-plane" "touching" (0, 0, 0) "a sphere resting on a plane: they touch at one point and there is no edge" (fun () ->
        G.union (Solid.plane "ground" (0., 0., 1.) 0.) (Solid.sphere "ball" (0., 0., 10.) 10.));
    case "torus-under-plane" "touching" (0, 0, 0) "a torus cut by a plane that only touches its top circle: nothing is cut and there is no edge" (fun () ->
        G.intersection (Solid.torus "torus" ~ring:18. ~tube:6.) (Solid.plane "lid" (0., 0., 1.) 6.));
    case "plane-through-edge" "touching" (0, 1, 0) "a third plane through the edge of two others, cutting nothing: one edge, with a third face touching it all along" (fun () ->
        all [ Solid.plane "A" (1., 0., 0.) 0.; Solid.plane "B" (0., 1., 0.) 0.; Solid.plane "C" (1., 1., 0.) 0. ]);
    case "bump-1mm" "grazing" (0, 2, 0) "a cylinder rising 1 mm out of a plane: two edges 8.7 mm apart, meeting the plane at 26 degrees" (fun () ->
        G.union (Solid.plane "ground" (0., 0., 1.) 0.) (along_y ~dz:(-9.) "bump" 10.));
    case "bump-20um" "grazing" (0, 2, 0) "the same rising 0.02 mm: two edges 1.3 mm apart with the same two faces, meeting at 3.6 degrees" (fun () ->
        G.union (Solid.plane "ground" (0., 0., 1.) 0.) (along_y ~dz:(-9.98) "bump" 10.));
    case "knife-15" "grazing" (0, 1, 0) "two planes 15 degrees apart: a knife edge, its two faces nearly back to back" (fun () ->
        let t = 165. *. Float.pi /. 180. in
        G.intersection (Solid.plane "A" (1., 0., 0.) 0.) (Solid.plane "B" (cos t, 0., sin t) 0.));
    case "plate-3mm" "plain" (0, 2, 2) "a plate three cells thick with a hole: two circles" (fun () -> plate 3. 5.);
    case "plate-1mm" "small" (0, 2, 2) "the same one cell thick" (fun () -> plate 1. 5.);
    case "plate-0.4mm" "small" (0, 2, 2) "the same 0.4 of a cell thick" (fun () -> plate 0.4 5.);
    case "hole-0.6mm" "small" (0, 2, 2) "a hole 0.6 mm across, smaller than a cell, through a 10 mm plate" (fun () -> plate 10. 0.3);
    case "cube-on-cell-walls" "aligned" (8, 12, 0) "a cube whose faces, edges and corners lie exactly on cell walls" ~lo:[| -32.; -32.; -32. |] ~hi:[| 32.; 32.; 32. |] (fun () ->
        box "" (-16., 16.) (-16., 16.) (-16., 16.));
    case "coplanar-boxes" "aligned" (16, 24, 0) "two overlapping boxes with their tops and bottoms in the same planes: as one solid, a prism with eight sides" (fun () ->
        G.union (box "a" (-20., 5.) (-10., 10.) (-10., 0.)) (box "b" (-5., 20.) (-6., 6.) (-10., 0.)));
    case "same-cube-twice" "aligned" (8, 12, 0) "a cube united with itself: every face lies on its twin" (fun () -> G.union (box "" (-12., 9.) (-8., 11.) (-10., 7.)) (box "" (-12., 9.) (-8., 11.) (-10., 7.)));
    case "torus" "plain" (0, 0, 0) "a torus alone: no edge to find, and nothing to flag" (fun () -> Solid.torus "torus" ~ring:18. ~tube:6.);
    case "two-cubes-apart" "plain" (16, 24, 0) "two cubes 3 mm apart: two separate solids" (fun () -> G.union (box "a" (-20., -3.) (-8., 9.) (-7., 10.)) (box "b" (0., 17.) (-8., 9.) (-7., 10.)));
    case "cone" "touching" (0, 1, 1) "a cone standing on a plane: one circle; the apex is a point with no tangent plane, not a corner" (fun () ->
        G.intersection (cone 18. 30.) (Solid.plane "base" (0., 0., -1.) 0.)) ]
