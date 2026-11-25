(* A picture of a model made by the kernel itself: what its one evaluator says, a ray a
   pixel. No page and no graphics card: so an agent, or anyone at a terminal, sees a
   model by running a program, and sees exactly what the checks measure.

   The camera is the page's: it turns about a point ([target], or the middle of the
   domain), from [dist] domain diagonals away, [yaw] and [pitch] in degrees (pitch 90
   looks straight down; yaw 0 looks along -x, 90 along -y), z up.

   - The model: sphere tracing through the domain's box, each sample read with the
     decisions of the cell it is in (Octree), then one full evaluation at the hit for
     the normal and the face. A face has the colour the page gives it; or, where the
     picture is asked for in the model's own colours, the colour of the feature that
     made it, and a light grey where that has none.
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

let model ?(w = 800) ?(h = 600) ?colour ~camera ~lo ~hi (g : Graph.t) =
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
        let base =
          match colour with
          | Some of_feature -> Option.value ~default:(0.72, 0.72, 0.72) (of_feature s.feature)
          | None ->
              let hue = Float.rem (float_of_int s.face *. 0.61803398875) 1. in
              let tone o = 0.62 +. (0.26 *. cos (6.2831853 *. (hue +. o))) in
              (tone 0., tone 0.33, tone 0.67)
        in
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

(* ---- A cut ---- *)

(* A cut through a model: the plane where the coordinate [axis] is [at], and the part of
   it to show, from left to right and from bottom to top. It is seen so that the two
   other coordinates run to the right and up, the earlier of them to the right: a cut at
   a z is seen from the front (+z), x to the right and y up; at a y, from -y, x to the
   right and z up; at an x, from +x, y to the right and z up. *)
type cut = { axis : int; at : float; left : float; right : float; bottom : float; top : float }

let across_up axis = match axis with 0 -> (1, 2) | 1 -> (0, 2) | _ -> (0, 1)

(* What is in the cut, on a grid of cells over it: for each cell, row by row from the
   top, what [part] says of the feature that answers at its middle where that is solid,
   and -1 where it is air or outside the model's box. *)
let cut_cells ~cols ~rows (k : cut) ~part ~lo ~hi (g : Graph.t) =
  let p = Prog.compile g in
  let c = Prog.ctx p in
  let tree = Octree.build p c ~lo ~hi ~depth:6 in
  let a, b = across_up k.axis in
  let x = Array.make 3 k.at in
  Array.init (cols * rows) (fun i ->
    x.(a) <- k.left +. ((float_of_int (i mod cols) +. 0.5) /. float_of_int cols *. (k.right -. k.left));
    x.(b) <- k.top -. ((float_of_int (i / cols) +. 0.5) /. float_of_int rows *. (k.top -. k.bottom));
    if x.(0) < lo.(0) || x.(0) > hi.(0) || x.(1) < lo.(1) || x.(1) > hi.(1) || x.(2) < lo.(2) || x.(2) > hi.(2) then -1
    else
      let s = Prog.eval p c (Octree.state tree x) x in
      if s.d.v < 0. then part s.feature else -1)

(* Figures for the scale: three dots by five, a row a number of three bits. *)
let figure = function
  | '0' -> [| 7; 5; 5; 5; 7 |] | '1' -> [| 2; 6; 2; 2; 7 |] | '2' -> [| 7; 1; 7; 4; 7 |] | '3' -> [| 7; 1; 7; 1; 7 |] | '4' -> [| 5; 5; 7; 1; 1 |]
  | '5' -> [| 7; 4; 7; 1; 7 |] | '6' -> [| 7; 4; 7; 5; 7 |] | '7' -> [| 7; 1; 1; 1; 1 |] | '8' -> [| 7; 5; 7; 5; 7 |] | '9' -> [| 7; 5; 7; 1; 7 |]
  | '-' -> [| 0; 0; 7; 0; 0 |] | '.' -> [| 0; 0; 0; 0; 2 |] | _ -> [| 0; 0; 0; 0; 0 |]

let ink = (0.11, 0.11, 0.13)
let figure_w = 8  (* a figure and the space after it, in pixels: a dot is two *)

(* Text in figures, its upper left corner at (x, y). *)
let written (im : t) x y text =
  String.iteri
    (fun i ch ->
      Array.iteri
        (fun r bits ->
          for d = 0 to 2 do
            if bits land (4 lsr d) <> 0 then
              for dy = 0 to 1 do for dx = 0 to 1 do
                let px = x + (figure_w * i) + (2 * d) + dx and py = y + (2 * r) + dy in
                if px >= 0 && px < im.w && py >= 0 && py < im.h then put im px py ink
              done done
          done)
        (figure ch))
    text

(* A step for a scale's marks: 1, 2 or 5 times a power of ten, about [marks] of them in a span. *)
let scale_step span marks =
  let raw = span /. float_of_int marks in
  let ten = 10. ** Float.floor (Float.log10 raw) in
  let m = raw /. ten in
  ten *. if m < 1.5 then 1. else if m < 3.5 then 2. else if m < 7.5 then 5. else 10.

(* The cut as a picture, [w] pixels across the part shown (and as many down as keeps a
   square a square). Each part is filled with its [colour]; a dark line goes round what
   is solid and between two parts that touch; air is left pale, with a faint line at
   every mark of the scale, which is written along the left and under the cut. With the
   picture: each part in it, the largest first, and where in the cut it is (from left
   to right, and from bottom to top). *)
let section ?(w = 1000) (k : cut) ~part ~colour ~lo ~hi (g : Graph.t) =
  let h = max 1 (int_of_float (Float.round (float_of_int w *. (k.top -. k.bottom) /. (k.right -. k.left)))) in
  let cells = cut_cells ~cols:w ~rows:h k ~part ~lo ~hi g in
  let left = 52 and under = 20 and over = 8 and beside = 28 in
  let im = blank ~w:(left + w + beside) ~h:(over + h + under) in
  let paper = (0.975, 0.975, 0.98) and faint = (0.86, 0.87, 0.9) in
  let step = scale_step (k.right -. k.left) 8 in
  let marks from to_ = let first = Float.ceil (from /. step) in List.init (max 0 (int_of_float (Float.floor (to_ /. step) -. first) + 1)) (fun i -> (first +. float_of_int i) *. step) in
  let col u = int_of_float ((u -. k.left) /. (k.right -. k.left) *. float_of_int w) and row v = int_of_float ((k.top -. v) /. (k.top -. k.bottom) *. float_of_int h) in
  let lines_x = List.map col (marks k.left k.right) and lines_y = List.map row (marks k.bottom k.top) in
  (* of each part: its pixels, and the columns and rows they are in *)
  let found = Hashtbl.create 16 in
  for py = 0 to h - 1 do
    for px = 0 to w - 1 do
      let id = cells.((w * py) + px) in
      if id >= 0 then
        Hashtbl.replace found id (match Hashtbl.find_opt found id with Some (n, x0, x1, y0, y1) -> (n + 1, min x0 px, max x1 px, min y0 py, max y1 py) | None -> (1, px, px, py, py));
      let other dx dy = px + dx < w && py + dy < h && cells.((w * (py + dy)) + px + dx) <> id in
      put im (left + px) (over + py) (if other 1 0 || other 0 1 then ink else if id >= 0 then colour id else if List.mem px lines_x || List.mem py lines_y then faint else paper)
    done
  done;
  let label v = let s = Printf.sprintf "%.6g" (if Float.abs v < 1e-9 then 0. else v) in if String.length s > 6 then Printf.sprintf "%.4g" v else s in
  List.iter (fun v -> let s = label v in written im (left + col v - (figure_w * String.length s / 2)) (over + h + 6) s) (marks k.left k.right);
  List.iter (fun v -> let s = label v in written im (left - 6 - (figure_w * String.length s)) (over + row v - 5) s) (marks k.bottom k.top);
  let across px = k.left +. (float_of_int px /. float_of_int w *. (k.right -. k.left)) and up py = k.top -. (float_of_int py /. float_of_int h *. (k.top -. k.bottom)) in
  let parts = List.sort (fun (_, (a, _, _, _, _)) (_, (b, _, _, _, _)) -> compare b a) (Hashtbl.fold (fun id at acc -> (id, at) :: acc) found []) in
  (im, List.map (fun (id, (_, x0, x1, y0, y1)) -> (id, (across x0, across (x1 + 1)), (up (y1 + 1), up y0))) parts)

let png (im : t) = Png.encode ~w:im.w ~h:im.h im.rgb
