(* Free-form surfaces as fitted pieces behind explicit guards.

   A parametric patch (a NURBS face, say) cannot be evaluated as a field: it gives
   points, not a value at a point. So it is cut into cells in its own (u, v), and each
   cell gets one cubic in x, y, z, fitted to be 0 on the patch and +-d at +-d along the
   normal (mk36's 012). Asking for the offsets as well as the zero is what makes the
   cubic read as a distance next to the skin and keeps stray zero sheets away.

   Away from the skin a cubic is not to be trusted. Each piece is its cubic only in a
   slab about its mean plane; past the slab it is the height above that plane, which has
   the right sign and slope 1.

   Pieces are chosen by a tree of guard planes: first across u, each plane through an
   iso-u curve and along the normals there, then across v inside each strip. Neighbours
   disagree a little at a guard. That step is measured here and stored on the select as
   its jump, so pruning stays sound. *)
module E = Expr
module G = Graph

(* ---- Least squares for one cubic ---- *)

let monomials x y z = [| 1.; x; y; z; x *. x; x *. y; x *. z; y *. y; y *. z; z *. z; x *. x *. x; x *. x *. y; x *. x *. z; x *. y *. y; x *. y *. z; x *. z *. z; y *. y *. y; y *. y *. z; y *. z *. z; z *. z *. z |]

(* a x = b by elimination with partial pivoting; a is n x n. *)
let solve a b =
  let n = Array.length b in
  let a = Array.map Array.copy a and b = Array.copy b in
  for i = 0 to n - 1 do
    let p = ref i in
    for r = i + 1 to n - 1 do if Float.abs a.(r).(i) > Float.abs a.(!p).(i) then p := r done;
    let t = a.(i) in a.(i) <- a.(!p); a.(!p) <- t;
    let t = b.(i) in b.(i) <- b.(!p); b.(!p) <- t;
    for r = i + 1 to n - 1 do
      let f = a.(r).(i) /. a.(i).(i) in
      for c = i to n - 1 do a.(r).(c) <- a.(r).(c) -. (f *. a.(i).(c)) done;
      b.(r) <- b.(r) -. (f *. b.(i))
    done
  done;
  let x = Array.make n 0. in
  for i = n - 1 downto 0 do
    let s = ref b.(i) in
    for c = i + 1 to n - 1 do s := !s -. (a.(i).(c) *. x.(c)) done;
    x.(i) <- !s /. a.(i).(i)
  done;
  x

(* value = scale * sum k_i m_i ((p - centre) / scale) *)
type cubic = { centre : float array; scale : float; k : float array }

let value q p =
  let m = monomials ((p.(0) -. q.centre.(0)) /. q.scale) ((p.(1) -. q.centre.(1)) /. q.scale) ((p.(2) -. q.centre.(2)) /. q.scale) in
  let s = ref 0. in
  Array.iteri (fun i v -> s := !s +. (q.k.(i) *. v)) m;
  q.scale *. !s

(* The cubic that is 0 at each point and d at d along its normal, for each d in
   [offsets], by least squares; the zeros weigh 30 times an offset. *)
let cubic pts normals ~offsets =
  let n = float_of_int (Array.length pts) in
  let centre = Array.init 3 (fun a -> Array.fold_left (fun s p -> s +. p.(a)) 0. pts /. n) in
  let far = List.fold_left (fun m d -> Float.max m (Float.abs d)) 0. offsets in
  let scale = far +. Array.fold_left (fun m p -> Float.max m (sqrt (((p.(0) -. centre.(0)) ** 2.) +. ((p.(1) -. centre.(1)) ** 2.) +. ((p.(2) -. centre.(2)) ** 2.)))) 0. pts in
  let ata = Array.make_matrix 20 20 0. and atb = Array.make 20 0. in
  let row p target w =
    let m = monomials ((p.(0) -. centre.(0)) /. scale) ((p.(1) -. centre.(1)) /. scale) ((p.(2) -. centre.(2)) /. scale) in
    for i = 0 to 19 do
      atb.(i) <- atb.(i) +. (w *. w *. m.(i) *. target /. scale);
      for j = 0 to 19 do ata.(i).(j) <- ata.(i).(j) +. (w *. w *. m.(i) *. m.(j)) done
    done
  in
  Array.iteri
    (fun i p ->
      row p 0. 30.;
      List.iter (fun d -> row (Array.mapi (fun a v -> v +. (d *. normals.(i).(a))) p) d 1.) offsets)
    pts;
  { centre; scale; k = solve ata atb }

let formula q =
  let open E.Infix in
  let u = (E.x - c q.centre.(0)) / c q.scale and v = (E.y - c q.centre.(1)) / c q.scale and w = (E.z - c q.centre.(2)) / c q.scale in
  let one = c 1. in
  let m = [| one; u; v; w; u * u; u * v; u * w; v * v; v * w; w * w; u * u * u; u * u * v; u * u * w; u * v * v; u * v * w; u * w * w; v * v * v; v * v * w; v * w * w; w * w * w |] in
  c q.scale * Array.fold_left E.add (c 0.) (Array.mapi (fun i t -> c q.k.(i) * t) m)

(* ---- A patch as a graph ---- *)

let dot a b = (a.(0) *. b.(0)) +. (a.(1) *. b.(1)) +. (a.(2) *. b.(2))
let sub a b = Array.init 3 (fun i -> a.(i) -. b.(i))
let cross a b = [| (a.(1) *. b.(2)) -. (a.(2) *. b.(1)); (a.(2) *. b.(0)) -. (a.(0) *. b.(2)); (a.(0) *. b.(1)) -. (a.(1) *. b.(0)) |]
let unit a = let l = sqrt (dot a a) in Array.map (fun v -> v /. l) a
let mean ps = let n = float_of_int (Array.length ps) in Array.init 3 (fun a -> Array.fold_left (fun s p -> s +. p.(a)) 0. ps /. n)
let move p d n = Array.mapi (fun a v -> v +. (d *. n.(a))) p

type piece = { q : cubic; origin : float array; up : float array; half : float;  (** the slab: |up . (p - origin)| < half *) graph : G.t }

(* The piece's value, as the graph gives it. *)
let piece_value pc p =
  let s = dot pc.up (sub p pc.origin) in
  if Float.abs s >= pc.half then s else value pc.q p

type result = { graph : G.t; pieces : int; worst_jump : float;  (** the largest step measured at any guard *) skin_jump : float  (** the largest within the band *) }

(* [surface u v] is the patch's point and unit outward normal, for u and v in 0 .. 1
   (and a little beyond: cells are fitted with a margin). [nu] x [nv] cells. The field
   is fitted within [band] of the skin and its jumps are measured down to [depth] inside
   it, which should cover wherever the solid built from it reaches. *)
let patch ?(name = "skin") ?(lip = 1.25) ~nu ~nv ~band ~depth surface =
  let m = 9 in
  let offsets = List.concat_map (fun f -> [ f *. band; -.f *. band ]) [ 1. /. 3.; 2. /. 3.; 1. ] in
  let plane normal origin = G.expr E.(sub (add (add (mul (const normal.(0)) x) (mul (const normal.(1)) y)) (mul (const normal.(2)) z)) (const (dot normal origin))) in
  let worst = ref 0. and near = ref 0. in
  let cell i j =
    let at margin = Array.init (m * m) (fun k -> surface ((float_of_int i +. (-.margin +. ((1. +. (2. *. margin)) *. float_of_int (k mod m) /. float_of_int (m - 1)))) /. float_of_int nu) ((float_of_int j +. (-.margin +. ((1. +. (2. *. margin)) *. float_of_int (k / m) /. float_of_int (m - 1)))) /. float_of_int nv)) in
    let wide = at 0.3 and own = at 0. in
    let q = cubic (Array.map fst wide) (Array.map snd wide) ~offsets in
    let origin = mean (Array.map fst own) and up = unit (mean (Array.map snd own)) in
    let sag = Array.fold_left (fun s (p, _) -> Float.max s (Float.abs (dot up (sub p origin)))) 0. wide in
    let half = band +. sag in
    (* the step where the cubic hands over to the plane, at the slab's two faces *)
    let step = Array.fold_left (fun s (p, _) -> List.fold_left (fun s h -> let p' = move p (h -. dot up (sub p origin)) up in Float.max s (Float.abs (value q p' -. h))) s [ half; -.half ]) 0. wide in
    worst := Float.max !worst step;
    let height = E.(sub (add (add (mul (const up.(0)) x) (mul (const up.(1)) y)) (mul (const up.(2)) z)) (const (dot up origin))) in
    let flat = G.piece name height and curved = G.piece ~lip name (formula q) in
    let inner = G.select ~jump:(2. *. step) ~smooth:true ~guard:(G.expr E.(sub (const half) height)) ~le:flat ~gt:curved () in
    { q; origin; up; half; graph = G.select ~jump:(2. *. step) ~smooth:true ~guard:(G.expr E.(add height (const half))) ~le:flat ~gt:inner () }
  in
  let cells = Array.init nu (fun i -> Array.init nv (fun j -> cell i j)) in
  (* A guard plane through the curve [curve k], k = 0 .. m - 1, along its normals; <= 0 on
     the side of [low]. The step between the pieces either side, by [pair k], measured
     along the curve from [depth] inside to [band] outside. *)
  let guard ?(n = m) curve low pair =
    let ps = Array.init n curve in
    let origin = mean (Array.map fst ps) and up = unit (mean (Array.map snd ps)) in
    let g = unit (cross (sub (fst ps.(n - 1)) (fst ps.(0))) up) in
    let g = if dot g (sub low origin) > 0. then Array.map Float.neg g else g in
    let step = ref 0. in
    Array.iteri
      (fun k (p, n) ->
        let a, b = pair k in
        for h = 0 to 12 do
          let d = -.depth +. ((depth +. band) *. float_of_int h /. 12.) in
          (* onto the plane itself, where the switch happens *)
          let x = move p d n in
          let x = move x (-.dot g (sub x origin)) g in
          let s = Float.abs (piece_value a x -. piece_value b x) in
          step := Float.max !step s;
          if Float.abs d <= band then near := Float.max !near s
        done)
      ps;
    worst := Float.max !worst !step;
    (plane g origin, 2. *. !step)
  in
  let frac k = float_of_int k /. float_of_int (m - 1) in
  let rec tree lo hi leaf split = if hi - lo = 1 then leaf lo else let mid = (lo + hi) / 2 in let g, jump = split mid in G.select ~jump ~smooth:true ~guard:g ~le:(tree lo mid leaf split) ~gt:(tree mid hi leaf split) () in
  let strip i =
    tree 0 nv
      (fun j -> cells.(i).(j).graph)
      (fun j ->
        let u k = (float_of_int i +. frac k) /. float_of_int nu and v = float_of_int j /. float_of_int nv in
        guard (fun k -> surface (u k) v) (fst (surface (u (m / 2)) (v -. (0.5 /. float_of_int nv)))) (fun _ -> (cells.(i).(j - 1), cells.(i).(j))))
  in
  let graph =
    tree 0 nu strip (fun i ->
        let u = float_of_int i /. float_of_int nu and n = (4 * nv) + 1 in
        let v k = float_of_int k /. float_of_int (n - 1) in
        guard ~n (fun k -> surface u (v k)) (fst (surface (u -. (0.5 /. float_of_int nu)) 0.5)) (fun k -> let j = min (nv - 1) (int_of_float (v k *. float_of_int nv)) in (cells.(i - 1).(j), cells.(i).(j))))
  in
  { graph; pieces = nu * nv; worst_jump = !worst; skin_jump = !near }
