(* A triangle mesh of a STEP file's faces, each triangle knowing its face: the reference
   a model is compared with.

   A face is its surface cut by its boundary loops. The loops are brought into the
   surface's own (u, v); the region they bound is cut into triangles there (ear
   clipping, holes joined to the outside by bridges); and each triangle is split along
   its edges until no edge strays from the surface by more than [tol] or is longer than
   [longest]. Every vertex is a point of the surface itself, so distances measured at
   vertices are distances from the reference, whatever the triangles look like. An edge
   is split by a rule that reads only that edge, so both triangles on it split it the
   same way and no crack opens. *)

type t = {
  points : float array;  (** x, y, z per vertex *)
  normals : float array;  (** outward, per vertex *)
  triangles : int array;  (** three vertices per triangle *)
  face : int array;  (** per triangle: index into [faces] *)
  faces : Step.face array;
  area : float array;  (** per face: its area, integrated over its triangles in (u, v) *)
  skipped : (int * string) list;  (** faces not meshed, and why *)
}

let area2 poly = let n = Array.length poly in let s = ref 0. in for i = 0 to n - 1 do let (x0, y0) = poly.(i) and (x1, y1) = poly.((i + 1) mod n) in s := !s +. ((x0 *. y1) -. (x1 *. y0)) done; !s
let cross2 (ax, ay) (bx, by) (cx, cy) = ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax))

(* Do the open segments ab and cd cross? *)
let crosses a b c d =
  let s1 = cross2 a b c and s2 = cross2 a b d and s3 = cross2 c d a and s4 = cross2 c d b in
  ((s1 > 0. && s2 < 0.) || (s1 < 0. && s2 > 0.)) && ((s3 > 0. && s4 < 0.) || (s3 < 0. && s4 > 0.))

(* The region inside [outer] (anticlockwise) and outside [holes] (clockwise) as one
   polygon: each hole is joined to the outside by a bridge, walked there and back. The
   bridge goes from the hole's rightmost point to the nearest point of the polygon it can
   reach without crossing anything. Its two passes are held a hair apart (a ten-millionth
   of the polygon's size), so the polygon never touches itself: ear clipping is only
   right for a polygon that does not. *)
let join outer holes =
  let rightmost ring = Array.fold_left (fun m (u, _) -> Float.max m u) neg_infinity ring in
  List.fold_left
    (fun poly hole ->
      let n = Array.length poly and m = Array.length hole in
      let h = ref 0 in
      Array.iteri (fun i (u, _) -> if u > fst hole.(!h) then h := i) hole;
      let from = hole.(!h) in
      let clear q =
        let free ring = let k = Array.length ring in let rec go i = i >= k || ((not (crosses from q ring.(i) ring.((i + 1) mod k))) && go (i + 1)) in go 0 in
        free poly && free hole && List.for_all free holes
      in
      let by_distance = Array.init n Fun.id in
      let d i = ((fst poly.(i) -. fst from) ** 2.) +. ((snd poly.(i) -. snd from) ** 2.) in
      Array.sort (fun i j -> compare (d i) (d j)) by_distance;
      match Array.find_opt (fun i -> clear poly.(i)) by_distance with
      | None -> poly
      | Some best ->
          let (pu, pv) = poly.(best) and (hu, hv) = from in
          let du = hu -. pu and dv = hv -. pv in
          (* half the gap, to the left of the way out: the way out keeps to that side,
             the way back to the other *)
          let width u = Array.fold_left (fun m x -> Float.max m (u x)) neg_infinity poly -. Array.fold_left (fun m x -> Float.min m (u x)) infinity poly in
          let e = 0.5e-7 *. Float.max (width fst) (width snd) /. Float.max 1e-300 (Float.sqrt ((du *. du) +. (dv *. dv))) in
          let out (u, v) = (u -. (e *. dv), v +. (e *. du)) and back (u, v) = (u +. (e *. dv), v -. (e *. du)) in
          Array.concat
            [ Array.sub poly 0 best; [| out poly.(best); out from |]; Array.init (m - 1) (fun k -> hole.((!h + 1 + k) mod m)); [| back from; back poly.(best) |];
              Array.sub poly (best + 1) (n - best - 1) ])
    outer
    (List.sort (fun a b -> compare (rightmost b) (rightmost a)) holes)

(* An anticlockwise polygon as triangles of its own vertices, by cutting off ears. It is
   worked on scaled to a unit square, since u and v can be in different units. A vertex
   on a straight stretch is dropped: it is a corner of nothing, and it is what makes ear
   clipping go wrong. A corner is an ear if it turns left and no vertex that turns right
   lies in its triangle; of the ears, the best-shaped goes first, so the triangles are
   not slivers. The polygon is kept as a ring of links, and after each cut only the two
   neighbours are looked at again, so a face with thousands of boundary points takes
   seconds, not hours. *)
let ears poly =
  let n = Array.length poly in
  let us = Array.map fst poly and vs = Array.map snd poly in
  let lo a = Array.fold_left Float.min infinity a and hi a = Array.fold_left Float.max neg_infinity a in
  let su = Float.max 1e-300 (hi us -. lo us) and sv = Float.max 1e-300 (hi vs -. lo vs) in
  let p = Array.map (fun (u, v) -> ((u -. lo us) /. su, (v -. lo vs) /. sv)) poly in
  let eps = 1e-12 in
  let prev = Array.init n (fun i -> (i + n - 1) mod n) and next = Array.init n (fun i -> (i + 1) mod n) in
  let alive = Array.make n true and left = ref n and some = ref 0 in
  let turn i = cross2 p.(prev.(i)) p.(i) p.(next.(i)) in
  let reflex = Array.make n false and ear = Array.make n false and score = Array.make n 0. in
  let each f = let i = ref !some in for _ = 1 to !left do f !i; i := next.(!i) done in
  let look i =
    let t = turn i in
    reflex.(i) <- t < -.eps;
    ear.(i) <- false;
    if t > eps then begin
      let a = p.(prev.(i)) and b = p.(i) and c = p.(next.(i)) in
      let blocked = ref false in
      each (fun k -> if (not !blocked) && reflex.(k) then let q = p.(k) in if q <> a && q <> b && q <> c && cross2 a b q >= 0. && cross2 b c q >= 0. && cross2 c a q >= 0. then blocked := true);
      ear.(i) <- not !blocked;
      let d (x0, y0) (x1, y1) = ((x1 -. x0) ** 2.) +. ((y1 -. y0) ** 2.) in
      score.(i) <- t /. Float.max 1e-300 (Float.max (d a b) (Float.max (d b c) (d c a)))
    end
  in
  let drop i =
    alive.(i) <- false;
    next.(prev.(i)) <- next.(i);
    prev.(next.(i)) <- prev.(i);
    decr left;
    if !some = i then some := next.(i)
  in
  (* a neighbour left on a straight stretch goes too, and then its neighbours are looked at *)
  let rec settle i = if alive.(i) && !left > 3 then if Float.abs (turn i) <= eps then (let a = prev.(i) and c = next.(i) in drop i; settle a; settle c) else look i in
  each (fun i -> reflex.(i) <- turn i < -.eps);
  let all = Array.init n Fun.id in
  Array.iter (fun i -> if alive.(i) && !left > 3 && Float.abs (turn i) <= eps then (let a = prev.(i) and c = next.(i) in drop i; settle a; settle c)) all;
  each (fun i -> reflex.(i) <- turn i < -.eps);
  each look;
  let out = ref [] in
  while !left > 3 do
    let best = ref (-1) in
    each (fun i -> if ear.(i) && (!best < 0 || score.(i) > score.(!best)) then best := i);
    (* failing any ear, the flattest corner goes, with no triangle: a polygon that
       touches itself at a bridge can leave none that passes the test *)
    let i = if !best >= 0 then (out := (prev.(!best), !best, next.(!best)) :: !out; !best) else (let flat = ref !some in each (fun k -> if Float.abs (turn k) < Float.abs (turn !flat) then flat := k); !flat) in
    let a = prev.(i) and c = next.(i) in
    drop i;
    settle a;
    settle c
  done;
  if !left = 3 && turn !some > eps then out := (prev.(!some), !some, next.(!some)) :: !out;
  !out

(* A face's loops in its surface's (u, v).

   Some surfaces close on themselves: a cylinder, a cone and a sphere in u, a torus in u
   and in v. There a loop may go once round without closing. Two such loops bound a
   band: unrolled, the first runs one way along one side and the second back along the
   other, and the two ends that join them are the same line on the surface, a turn
   apart. One such loop alone bounds a cap, up to a sphere's pole or a cone's tip. A
   sphere or torus with no loop at all is whole. *)
let outline (f : Step.face) =
  (* Where a surface narrows to a point (a cone's tip, a sphere's poles) a point of the
     boundary has a v and no u of its own. *)
  let pinched p =
    match f.surface with
    | Step.Cone (place, r, angle) -> if Step.norm Step.(p -| (place.o -| ((r /. tan angle) *| place.z))) < 1e-6 *. (1. +. Float.abs r) then Some (-.r /. tan angle) else None
    | Sphere (place, r) -> let d = Step.(p -| place.o) in let h = Step.dot d place.z in if Step.norm Step.(d -| (h *| place.z)) < 1e-6 *. (1. +. r) then Some (if h > 0. then 0.5 *. Float.pi else -0.5 *. Float.pi) else None
    | _ -> None
  in
  (* A loop in (u, v), each point found near the one before it, so that it does not jump
     a turn. A loop through a pinched point is begun just after it and unrolled from
     there, and the point is given twice, with the u of the point before it and then of
     the point after it: unrolled, a wedge that reaches the tip has a whole side there. *)
  let flat loop =
    let pts = Array.of_list loop in
    let n = Array.length pts in
    let start = match List.find_opt (fun i -> pinched pts.(i) <> None) (List.init n Fun.id) with Some i -> i + 1 | None -> 0 in
    let out = ref [] and near = ref None and waiting = ref None in
    for j = 0 to n - 1 do
      let p = pts.((start + j) mod n) in
      match pinched p with
      | Some v -> (match !near with Some (u, _) -> out := (u, v) :: !out | None -> ()); waiting := Some v
      | None ->
          let ((u, _) as uv) = Step.locate f.surface ?near:!near p in
          (match !waiting with Some v -> out := (u, v) :: !out; waiting := None | None -> ());
          out := uv :: !out;
          near := Some uv
    done;
    (* the loop ended on the point it was begun after: its other side is the first point's *)
    (match (!waiting, List.rev !out) with Some v, (u, _) :: _ -> out := (u, v) :: !out | _ -> ());
    Array.of_list (List.rev !out)
  in
  let loops = List.map flat f.loops in
  let two_pi = 2. *. Float.pi and half_pi = 0.5 *. Float.pi in
  let round_u, round_v = match f.surface with Step.Cylinder _ | Cone _ | Sphere _ -> (true, false) | Torus _ -> (true, true) | _ -> (false, false) in
  (* how far round, in u and in v, a loop has gone when it is back at its first point *)
  let turn l =
    let u0, v0 = l.(0) in
    let u1, v1 = Step.locate f.surface ~near:l.(Array.length l - 1) (Step.at f.surface u0 v0) in
    ((if round_u && Float.abs (u1 -. u0) > 1. then u1 -. u0 else 0.), if round_v && Float.abs (v1 -. v0) > 1. then v1 -. v0 else 0.)
  in
  let swap = Array.map (fun (u, v) -> (v, u)) in
  (* Two loops that each go once round in their first coordinate, with how far each
     went, as one polygon. *)
  let band (a, ta) (b, tb) =
    let sa = if ta > 0. then 1. else -1. in
    let b = if tb > 0. = (ta > 0.) then Array.of_list (List.rev (Array.to_list b)) else b in
    let seam = fst a.(0) +. (sa *. two_pi) in
    let near u0 u = u +. (two_pi *. Float.round ((u0 -. u) /. two_pi)) in
    (* start b at its point nearest the seam, so the joining line is short *)
    let m = Array.length b in
    let k = ref 0 in
    Array.iteri (fun i (u, _) -> if Float.abs (near seam u -. seam) < Float.abs (near seam (fst b.(!k)) -. seam) then k := i) b;
    let _, back = List.fold_left (fun (prev, acc) j -> let u, v = b.((!k + j) mod m) in let u = near prev u in (u, (u, v) :: acc)) (seam, []) (List.init m Fun.id) in
    let back = List.rev back in
    let ub, vb = List.hd back in
    Array.of_list (Array.to_list a @ [ (seam, snd a.(0)) ] @ back @ [ (ub -. (sa *. two_pi), vb) ])
  in
  (* One loop that goes once round in u, closed at the point the surface narrows to: the
     pole on the loop's side of a sphere, the tip of a cone. *)
  let cap (a, ta) =
    let seam = fst a.(0) +. (if ta > 0. then two_pi else -.two_pi) in
    let mean = Array.fold_left (fun s (_, v) -> s +. v) 0. a /. float_of_int (Array.length a) in
    let close v = Some (Array.of_list (Array.to_list a @ [ (seam, snd a.(0)); (seam, v); (fst a.(0), v) ])) in
    match f.surface with Step.Sphere _ -> close (if mean > 0. then half_pi else -.half_pi) | Cone (_, r, angle) -> close (-.r /. tan angle) | _ -> None
  in
  (* On a surface that closes on itself a loop is found where its first point happens to
     lie, which for a hole may be a whole turn from the loop it is in. Each hole is moved
     by whole turns to where its middle is nearest the outside loop's. *)
  let within outer holes =
    let middle l coordinate = 0.5 *. (Array.fold_left (fun m p -> Float.min m (coordinate p)) infinity l +. Array.fold_left (fun m p -> Float.max m (coordinate p)) neg_infinity l) in
    let turns round coordinate l = if round then two_pi *. Float.round ((middle outer coordinate -. middle l coordinate) /. two_pi) else 0. in
    outer :: List.map (fun l -> let du = turns round_u fst l and dv = turns round_v snd l in if du = 0. && dv = 0. then l else Array.map (fun (u, v) -> (u +. du, v +. dv)) l) holes
  in
  if loops = [] then
    match f.surface with
    | Step.Sphere _ -> [ [| (0., -.half_pi); (two_pi, -.half_pi); (two_pi, half_pi); (0., half_pi) |] ]
    | Torus _ -> [ [| (0., 0.); (two_pi, 0.); (two_pi, two_pi); (0., two_pi) |] ]
    | _ -> []
  else
    let turns = List.map (fun l -> (l, turn l)) loops in
    let in_u = List.filter_map (fun (l, (tu, tv)) -> if tu <> 0. && tv = 0. then Some (l, tu) else None) turns in
    let in_v = List.filter_map (fun (l, (tu, tv)) -> if tv <> 0. && tu = 0. then Some (swap l, tv) else None) turns in
    let closed = List.filter_map (fun (l, t) -> if t = (0., 0.) then Some l else None) turns in
    if List.length in_u + List.length in_v + List.length closed <> List.length loops then []
    else
      match (in_u, in_v) with
      (* of loops that all close, the largest is the outside *)
      | [], [] -> ( match List.sort (fun a b -> compare (Float.abs (area2 b)) (Float.abs (area2 a))) closed with outer :: holes -> within outer holes | [] -> [])
      | [ a; b ], [] -> within (band a b) closed
      | [ a ], [] -> ( match cap a with Some p -> within p closed | None -> [])
      (* round a torus's tube: the same with u and v changed over *)
      | [], [ a; b ] -> within (swap (band a b)) closed
      | _ -> []

let build ?(tol = 0.05) ?longest ?(most = 250_000) (faces : Step.face list) =
  let faces = Array.of_list faces in
  let lo = Array.make 3 infinity and hi = Array.make 3 neg_infinity in
  Array.iter (fun (f : Step.face) -> List.iter (List.iter (fun p -> Array.iteri (fun a v -> lo.(a) <- Float.min lo.(a) v; hi.(a) <- Float.max hi.(a) v) p)) f.loops) faces;
  let longest = match longest with Some l -> l | None -> 0.03 *. Step.norm Step.(hi -| lo) in
  let points = Buffer.create 65536 and normals = Buffer.create 65536 and tris = ref [] and owner = ref [] and count = ref 0 and skipped = ref [] in
  let area = Array.make (Array.length faces) 0. in
  let add3 b (v : float array) = Array.iter (fun x -> Buffer.add_int64_le b (Int64.bits_of_float x)) v in
  Array.iteri
    (fun fi (f : Step.face) ->
      match f.surface with
      | Step.Unknown k -> skipped := (f.id, "a surface of kind " ^ k) :: !skipped
      | _ -> (
          match List.sort (fun a b -> compare (Float.abs (area2 b)) (Float.abs (area2 a))) (outline f) with
          | [] -> skipped := (f.id, "no boundary that closes") :: !skipped
          | outer :: holes ->
              let ccw l = if area2 l < 0. then Array.of_list (List.rev (Array.to_list l)) else l in
              let cw l = if area2 l > 0. then Array.of_list (List.rev (Array.to_list l)) else l in
              let poly = join (ccw outer) (List.map cw holes) in
              let index = Hashtbl.create 1024 in
              let vertex ((u, v) as uv) =
                match Hashtbl.find_opt index uv with
                | Some i -> i
                | None ->
                    let n = Step.normal f.surface u v in
                    add3 points (Step.at f.surface u v);
                    add3 normals (if f.outward then n else Array.map Float.neg n);
                    Hashtbl.replace index uv !count;
                    incr count;
                    !count - 1
              in
              let pos (u, v) = Step.at f.surface u v in
              let mid (ua, va) (ub, vb) = (0.5 *. (ua +. ub), 0.5 *. (va +. vb)) in
              (* an edge is split if it is long, or its middle is off the surface *)
              let split a b = let pa = pos a and pb = pos b and pm = pos (mid a b) in Step.norm Step.(pa -| pb) > longest || Step.norm Step.(pm -| (0.5 *| (pa +| pb))) > tol in
              (* The face's area is summed in (u, v), each triangle's area there times how
                 much the surface stretches it: the flat triangles themselves overstate
                 it wherever a thin one stands on a chord. *)
              let du = 1e-6 *. Float.max 1e-9 (Array.fold_left (fun m (u, _) -> Float.max m (Float.abs u)) 1. poly) and dv = 1e-6 *. Float.max 1e-9 (Array.fold_left (fun m (_, v) -> Float.max m (Float.abs v)) 1. poly) in
              let stretch (u, v) = Step.norm (Step.cross Step.((1. /. (2. *. du)) *| (pos (u +. du, v) -| pos (u -. du, v))) Step.((1. /. (2. *. dv)) *| (pos (u, v +. dv) -| pos (u, v -. dv)))) in
              let emit ((ua, va) as a) ((ub, vb) as b) ((uc, vc) as c) =
                area.(fi) <- area.(fi) +. (0.5 *. Float.abs (cross2 a b c) *. stretch ((ua +. ub +. uc) /. 3., (va +. vb +. vc) /. 3.));
                tris := (vertex a, vertex b, vertex c) :: !tris;
                owner := fi :: !owner
              in
              (* no face gets more than [most] triangles: past that, what is left is
                 taken as it is, coarser than asked *)
              let made = ref 0 in
              let emit a b c = incr made; emit a b c in
              let rec refine a b c depth =
                if depth = 0 || !made > most then emit a b c
                else
                  let d = depth - 1 in
                  match (split a b, split b c, split c a) with
                  | false, false, false -> emit a b c
                  | true, true, true -> let ab = mid a b and bc = mid b c and ca = mid c a in refine a ab ca d; refine ab b bc d; refine ca bc c d; refine ab bc ca d
                  | true, false, false -> let m = mid a b in refine a m c d; refine m b c d
                  | false, true, false -> let m = mid b c in refine a b m d; refine a m c d
                  | false, false, true -> let m = mid c a in refine a b m d; refine m b c d
                  | true, true, false -> let ab = mid a b and bc = mid b c in refine ab b bc d; refine a ab bc d; refine a bc c d
                  | false, true, true -> let bc = mid b c and ca = mid c a in refine ca bc c d; refine a b bc d; refine a bc ca d
                  | true, false, true -> let ab = mid a b and ca = mid c a in refine a ab ca d; refine ab b c d; refine ab c ca d
              in
              let t0 = Sys.time () in
              let cut = ears poly in
              let t1 = Sys.time () in
              List.iter (fun (a, b, c) -> refine poly.(a) poly.(b) poly.(c) 12) cut;
              if Sys.getenv_opt "MESH_DEBUG" <> None && (Sys.time () -. t0 > 0.5 || !made > most) then
                Printf.eprintf "face %d (#%d): %d boundary points, cut in %.2f s, %d triangles in %.2f s\n%!" fi f.id (Array.length poly) (t1 -. t0) !made (Sys.time () -. t1)))
    faces;
  let floats b = let bytes = Buffer.to_bytes b in Array.init (Bytes.length bytes / 8) (fun i -> Int64.float_of_bits (Bytes.get_int64_le bytes (8 * i))) in
  let tris = Array.of_list (List.rev !tris) in
  { points = floats points; normals = floats normals; triangles = Array.concat (Array.to_list (Array.map (fun (a, b, c) -> [| a; b; c |]) tris)); face = Array.of_list (List.rev !owner); faces; area; skipped = List.rev !skipped }
