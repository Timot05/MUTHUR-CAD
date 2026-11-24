(* A free-form face as one NURBS patch that is a height: h = S(u, v), a bicubic B-spline
   over a rectangle of two coordinates, with h along the third. A face that never turns
   over, seen along some axis, can be written this way, and then nothing is fitted to get
   a field: h - S(u, v) is zero on the skin, negative below it, and inside one knot span
   it is one polynomial. So the patch goes into the graph exactly: a piece per span,
   chosen by guard planes at the knots. The spans meet with matching value, slope and
   curvature, so no select has a jump.

   The field is a height above the skin, not a distance: its slope is sqrt (1 + |grad S|^2),
   which is measured and declared. Outside the rectangle the skin carries on level (u and
   v held at the edge), so the bound holds everywhere. *)
module E = Expr
module G = Graph

(* Knots are clamped: [ku] has spans + 7 values, [net] is (spans in u + 3) x (spans in v + 3) heights. *)
type t = { ku : float array; kv : float array; net : float array array }

(* The knot span holding u and the degree + 1 basis functions that are not zero there
   (The NURBS Book, algorithms A2.1 and A2.2). *)
let basis knots degree count u =
  let span =
    if u >= knots.(count) then count - 1
    else
      let lo = ref degree and hi = ref count in
      while !hi - !lo > 1 do let mid = (!lo + !hi) / 2 in if u < knots.(mid) then hi := mid else lo := mid done;
      !lo
  in
  let b = Array.make (degree + 1) 0. and left = Array.make (degree + 1) 0. and right = Array.make (degree + 1) 0. in
  b.(0) <- 1.;
  for j = 1 to degree do
    left.(j) <- u -. knots.(span + 1 - j);
    right.(j) <- knots.(span + j) -. u;
    let saved = ref 0. in
    for r = 0 to j - 1 do
      let t = b.(r) /. (right.(r + 1) +. left.(j - r)) in
      b.(r) <- !saved +. (right.(r + 1) *. t);
      saved := left.(j - r) *. t
    done;
    b.(j) <- !saved
  done;
  (span, b)

let degree = 3
let clamped a b spans = Array.init (spans + 7) (fun k -> a +. ((b -. a) *. float_of_int (max 0 (min spans (k - 3))) /. float_of_int spans))
let domain s = (s.ku.(3), s.ku.(Array.length s.net), s.kv.(3), s.kv.(Array.length s.net.(0)))

let height s u v =
  let nu = Array.length s.net and nv = Array.length s.net.(0) in
  let u = Float.max s.ku.(3) (Float.min s.ku.(nu) u) and v = Float.max s.kv.(3) (Float.min s.kv.(nv) v) in
  let su, bu = basis s.ku degree nu u and sv, bv = basis s.kv degree nv v in
  let h = ref 0. in
  for a = 0 to 3 do for b = 0 to 3 do h := !h +. (bu.(a) *. bv.(b) *. s.net.(su - 3 + a).(sv - 3 + b)) done done;
  !h

(* The patch nearest the points (u, v, h) by least squares, with [nu] x [nv] equal spans
   over the rectangle. [smooth] weighs the net's second differences against the points:
   it is what decides the net where there are no points (a hole in the face, a corner
   the face does not reach). *)
let fit ?(smooth = 1e-4) ~nu ~nv (u0, u1, v0, v1) pts =
  let ku = clamped u0 u1 nu and kv = clamped v0 v1 nv in
  let cu = nu + 3 and cv = nv + 3 in
  let n = cu * cv in
  let ata = Array.make_matrix n n 0. and atb = Array.make n 0. in
  Array.iter
    (fun (u, v, h) ->
      let su, bu = basis ku degree cu u and sv, bv = basis kv degree cv v in
      for a = 0 to 15 do
        let i = ((su - 3 + (a / 4)) * cv) + sv - 3 + (a mod 4) and wa = bu.(a / 4) *. bv.(a mod 4) in
        atb.(i) <- atb.(i) +. (wa *. h);
        for b = 0 to 15 do
          let j = ((su - 3 + (b / 4)) * cv) + sv - 3 + (b mod 4) in
          ata.(i).(j) <- ata.(i).(j) +. (wa *. bu.(b / 4) *. bv.(b mod 4))
        done
      done)
    pts;
  let w = ref 0. in
  for i = 0 to n - 1 do w := !w +. ata.(i).(i) done;
  let w = smooth *. !w /. float_of_int n in
  let bend i j k = List.iter (fun (a, ca) -> List.iter (fun (b, cb) -> ata.(a).(b) <- ata.(a).(b) +. (w *. ca *. cb)) [ (i, 1.); (j, -2.); (k, 1.) ]) [ (i, 1.); (j, -2.); (k, 1.) ] in
  for i = 0 to cu - 1 do
    for j = 0 to cv - 1 do
      if i > 0 && i < cu - 1 then bend (((i - 1) * cv) + j) ((i * cv) + j) (((i + 1) * cv) + j);
      if j > 0 && j < cv - 1 then bend ((i * cv) + j - 1) ((i * cv) + j) ((i * cv) + j + 1)
    done
  done;
  let c = Fit.solve ata atb in
  { ku; kv; net = Array.init cu (fun i -> Array.init cv (fun j -> c.((i * cv) + j))) }

(* The steepest the skin gets, sampled eight times a span each way. *)
let steepest s =
  let u0, u1, v0, v1 = domain s in
  let nu = 8 * (Array.length s.net - 3) and nv = 8 * (Array.length s.net.(0) - 3) in
  let du = (u1 -. u0) /. float_of_int nu and dv = (v1 -. v0) /. float_of_int nv in
  let m = ref 0. in
  for i = 0 to nu - 1 do
    for j = 0 to nv - 1 do
      let u = u0 +. (du *. float_of_int i) and v = v0 +. (dv *. float_of_int j) in
      let h = height s u v in
      let a = (height s (u +. du) v -. h) /. du and b = (height s u (v +. dv) -. h) /. dv in
      m := Float.max !m ((a *. a) +. (b *. b))
    done
  done;
  Float.sqrt !m

(* One span's 16 coefficients: S = sum a.(p).(q) s^p t^q, with s and t from 0 to 1 across it. *)
let span s i j =
  (* the four basis functions over a span, each as a cubic in its 0 .. 1 *)
  let powers knots count k =
    let a = knots.(k + 3) and b = knots.(k + 4) in
    let at = [| 0.; 1. /. 3.; 2. /. 3.; 1. |] in
    let values = Array.map (fun x -> snd (basis knots degree count (a +. ((b -. a) *. Float.min x (1. -. 1e-12))))) at in
    Array.init 4 (fun f -> Fit.solve (Array.map (fun x -> [| 1.; x; x *. x; x *. x *. x |]) at) (Array.map (fun r -> r.(f)) values))
  in
  let pu = powers s.ku (Array.length s.net) i and pv = powers s.kv (Array.length s.net.(0)) j in
  Array.init 4 (fun p -> Array.init 4 (fun q -> let t = ref 0. in for a = 0 to 3 do for b = 0 to 3 do t := !t +. (s.net.(i + a).(j + b) *. pu.(a).(p) *. pv.(b).(q)) done done; !t))

(* The solid under the skin, as a graph: [h], [u] and [v] are the three coordinates as
   formulas (the world's x, y, z in some order, or a frame's). *)
let graph ?(name = "skin") ~h ~u ~v s =
  let nu = Array.length s.net - 3 and nv = Array.length s.net.(0) - 3 in
  let lip = 1.02 *. Float.sqrt (1. +. (steepest s ** 2.)) in
  (* leaf 0 is before the first knot and the last leaf past the last: there the coordinate is held *)
  let local coord knots n k = if k = 0 then E.const 0. else if k = n + 1 then E.const 1. else E.div (E.sub coord (E.const knots.(k + 2))) (E.const (knots.(k + 3) -. knots.(k + 2))) in
  let ss = Array.init (nu + 2) (local u s.ku nu) and ts = Array.init (nv + 2) (local v s.kv nv) in
  let leaf i j =
    let a = span s (max 0 (min (nu - 1) (i - 1))) (max 0 (min (nv - 1) (j - 1))) in
    let horner x k = Array.fold_right (fun c acc -> E.add c (E.mul x acc)) k (E.const 0.) in
    G.piece ~lip name (E.sub h (horner ss.(i) (Array.map (fun row -> horner ts.(j) (Array.map E.const row)) a)))
  in
  let rec tree lo hi leaf guard = if hi - lo = 1 then leaf lo else let mid = (lo + hi) / 2 in G.select ~smooth:true ~guard:(guard mid) ~le:(tree lo mid leaf guard) ~gt:(tree mid hi leaf guard) () in
  let gu = Array.init (nu + 2) (fun k -> if k = 0 then None else Some (G.expr (E.sub u (E.const s.ku.(k + 2))))) in
  let gv = Array.init (nv + 2) (fun k -> if k = 0 then None else Some (G.expr (E.sub v (E.const s.kv.(k + 2))))) in
  tree 0 (nu + 2) (fun i -> tree 0 (nv + 2) (leaf i) (fun k -> Option.get gv.(k))) (fun k -> Option.get gu.(k))

(* ---- As a file: the patch is data, not code ---- *)

let to_json ?(about = []) s =
  let nums a = Json.Arr (Array.to_list (Array.map (fun v -> Json.Num v) a)) in
  Json.Obj (about @ [ ("degree", Json.Num 3.); ("knots_u", nums s.ku); ("knots_v", nums s.kv); ("heights", Json.Arr (Array.to_list (Array.map nums s.net))) ])

let of_json j =
  let field k = match j with Json.Obj l -> List.assoc k l | _ -> failwith "spline: not an object" in
  let nums = function Json.Arr l -> Array.of_list (List.map (function Json.Num v -> v | _ -> failwith "spline: a number was expected") l) | _ -> failwith "spline: a list was expected" in
  { ku = nums (field "knots_u"); kv = nums (field "knots_v"); net = (match field "heights" with Json.Arr rows -> Array.of_list (List.map nums rows) | _ -> failwith "spline: bad heights") }
