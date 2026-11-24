(* A picture of a model made by the kernel itself: what its one evaluator says, a ray a
   pixel. No page and no graphics card: so an agent, or anyone at a terminal, sees a
   model by running a program, and sees exactly what the checks measure.

   The camera is the page's: it turns about a point ([target], or the middle of the
   domain), from [dist] domain diagonals away, [yaw] and [pitch] in degrees (pitch 90
   looks straight down; yaw 0 looks along -x, 90 along -y), z up.

   - The model: sphere tracing through the domain's box, each sample read with the
     decisions of the cell it is in (Octree), then one full evaluation at the hit for
     the normal and the face. A face has the colour the page gives it.
   - Edges: the extraction's curves, dark where seen and faint where hidden. *)

type camera = { yaw : float; pitch : float; dist : float; target : float array option }

type t = { w : int; h : int; rgb : Bytes.t; depth : float array  (** along the view, per pixel; infinity where nothing is *) }

let tan_half = Float.tan 0.35
let background = (0.902, 0.910, 0.925)

(* Far enough back that the whole domain fits: the page's own rule. *)
let fit ~w ~h = Float.min 8. (1.08 *. 0.5 /. Float.sin (Float.atan (tan_half *. Float.min 1. (float_of_int w /. float_of_int h))))

type view = { ro : float array; fwd : float array; right : float array; up : float array; diag : float }

let view cam ~lo ~hi =
  let c = match cam.target with Some t -> t | None -> Array.map2 (fun l h -> 0.5 *. (l +. h)) lo hi in
  let diag = sqrt (Array.fold_left ( +. ) 0. (Array.map2 (fun l h -> (h -. l) *. (h -. l)) lo hi)) in
  let a = cam.yaw *. Float.pi /. 180. and b = cam.pitch *. Float.pi /. 180. in
  let cp = cos b and sp = sin b and cy = cos a and sy = sin a in
  let fwd = [| -.cp *. cy; -.cp *. sy; -.sp |] in
  { ro = Array.mapi (fun i v -> v -. (fwd.(i) *. cam.dist *. diag)) c; fwd; right = [| -.sy; cy; 0. |]; up = [| -.sp *. cy; -.sp *. sy; cp |]; diag }

let dot a b = (a.(0) *. b.(0)) +. (a.(1) *. b.(1)) +. (a.(2) *. b.(2))
let put (im : t) x y (r, g, b) = let k = 3 * ((im.w * y) + x) and byte v = Char.chr (max 0 (min 255 (int_of_float (255. *. v)))) in Bytes.set im.rgb k (byte r); Bytes.set im.rgb (k + 1) (byte g); Bytes.set im.rgb (k + 2) (byte b)
let got (im : t) x y = let k = 3 * ((im.w * y) + x) and f i = float_of_int (Char.code (Bytes.get im.rgb i)) /. 255. in (f k, f (k + 1), f (k + 2))
let mix (r, g, b) (r', g', b') t = (r +. (t *. (r' -. r)), g +. (t *. (g' -. g)), b +. (t *. (b' -. b)))
let lit (r, g, b) l = (r *. l, g *. l, b *. l)

(* The light the page uses: from over the eye's shoulder. *)
let light v n rd =
  let l = Array.init 3 (fun i -> -.rd.(i) +. (0.45 *. v.up.(i)) -. (0.3 *. v.right.(i))) in
  0.38 +. (0.62 *. Float.max 0. (dot n l /. sqrt (dot l l)))

(* An empty picture, to draw into. *)
let blank ~w ~h =
  let im = { w; h; rgb = Bytes.create (3 * w * h); depth = Array.make (w * h) infinity } in
  for y = 0 to h - 1 do for x = 0 to w - 1 do put im x y background done done;
  im

let model ?(w = 800) ?(h = 600) ~camera ~lo ~hi (g : Graph.t) =
  let im = { w; h; rgb = Bytes.create (3 * w * h); depth = Array.make (w * h) infinity } in
  let v = view camera ~lo ~hi in
  let p = Prog.compile g in
  let c = Prog.ctx p in
  let tree = Octree.build p c ~lo ~hi ~depth:6 in
  let eps = v.diag *. 2e-4 and step = 1. /. g.lip in
  let x = Array.make 3 0. and rd = Array.make 3 0. in
  for py = 0 to h - 1 do
    for px = 0 to w - 1 do
      put im px py background;
      let a = ((2. *. (float_of_int px +. 0.5)) -. float_of_int w) /. float_of_int h and b = (float_of_int h -. (2. *. (float_of_int py +. 0.5))) /. float_of_int h in
      for i = 0 to 2 do rd.(i) <- v.fwd.(i) +. (tan_half *. ((a *. v.right.(i)) +. (b *. v.up.(i)))) done;
      let l = sqrt (dot rd rd) in
      for i = 0 to 2 do rd.(i) <- rd.(i) /. l done;
      let tn = ref 0. and tf = ref infinity in
      for i = 0 to 2 do
        let t0 = (lo.(i) -. v.ro.(i)) /. rd.(i) and t1 = (hi.(i) -. v.ro.(i)) /. rd.(i) in
        tn := Float.max !tn (Float.min t0 t1);
        tf := Float.min !tf (Float.max t0 t1)
      done;
      let t = ref !tn and left = ref 400 and hit = ref false in
      while (not !hit) && !left > 0 && !t <= !tf do
        for i = 0 to 2 do x.(i) <- v.ro.(i) +. (!t *. rd.(i)) done;
        let d = Float.abs (Prog.value p c (Octree.state tree x) x) in
        if d < eps then hit := true else (t := !t +. (d *. step); decr left)
      done;
      if !hit then begin
        let s = Prog.eval p c (Octree.state tree x) x in
        let n = s.d.g in
        let ln = sqrt (dot n n) in
        let within = dot n rd > 0. in
        let n = Array.map (fun k -> (if within then -.k else k) /. (if ln > 0. then ln else 1.)) n in
        let hue = Float.rem (float_of_int s.face *. 0.61803398875) 1. in
        let tone o = 0.62 +. (0.26 *. cos (6.2831853 *. (hue +. o))) in
        let base = (tone 0., tone 0.33, tone 0.67) in
        put im px py (lit (if within then lit base 0.72 else base) (light v n rd));
        im.depth.((w * py) + px) <- dot (Array.map2 ( -. ) x v.ro) v.fwd
      end
    done
  done;
  im

(* A point of space in the picture: column, row, and how far along the view. *)
let project (im : t) v p =
  let q = Array.map2 ( -. ) p v.ro in
  let z = dot q v.fwd in
  let k = float_of_int im.h /. (2. *. tan_half *. z) in
  ((0.5 *. float_of_int im.w) +. (k *. dot q v.right), (0.5 *. float_of_int im.h) -. (k *. dot q v.up), z)

(* Curves in space over the picture: each a list of points, in [ink], as strong as
   [seen] where nothing is in front and [hidden] where the model is. *)
let lines ?(ink = (0.11, 0.11, 0.13)) ?(seen = 0.9) ?(hidden = 0.22) ~camera ~lo ~hi (curves : float array list list) (im : t) =
  let v = view camera ~lo ~hi in
  let lift = v.diag *. 3e-3 in
  List.iter
    (fun pts ->
      let rec go = function
        | a :: (b :: _ as rest) ->
            let xa, ya, za = project im v a and xb, yb, zb = project im v b in
            if za > 0. && zb > 0. then begin
              let steps = max 1 (int_of_float (Float.max (Float.abs (xb -. xa)) (Float.abs (yb -. ya)))) in
              for s = 0 to steps do
                let f = float_of_int s /. float_of_int steps in
                let px = int_of_float (xa +. (f *. (xb -. xa))) and py = int_of_float (ya +. (f *. (yb -. ya))) and z = za +. (f *. (zb -. za)) in
                if px >= 0 && px < im.w && py >= 0 && py < im.h then put im px py (mix (got im px py) ink (if z <= im.depth.((im.w * py) + px) +. lift then seen else hidden))
              done
            end;
            go rest
        | _ -> ()
      in
      go pts)
    curves

let png (im : t) = Png.encode ~w:im.w ~h:im.h im.rgb
