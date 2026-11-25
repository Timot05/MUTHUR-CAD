(* A reader for STEP files (ISO 10303-21), for the kinds of entity our parts use: faces on
   planes, cylinders, cones, spheres, tori and NURBS surfaces, bounded by loops of lines,
   circles, ellipses and NURBS curves. It reads the file, evaluates each surface and curve, and gives each
   face's boundary. Anything else in a file is parsed and ignored; a face on a surface
   it does not know is reported, not guessed. Lengths come out in millimetres. *)

(* ---- The file: every entity as its name and arguments ---- *)

type value = Num of float | Str of string | Ref of int | Enum of string | Nil | List of value list | Typed of string * value list

(* A simple entity has one part; a complex one, written #1 = ( A(...) B(...) ), several. *)
type entity = (string * value list) list
type file = (int, entity) Hashtbl.t

let parse text : file =
  let n = String.length text and i = ref 0 in
  let peek () = if !i < n then text.[!i] else '\000' in
  let rec skip () =
    if !i < n then
      match text.[!i] with
      | ' ' | '\n' | '\r' | '\t' -> incr i; skip ()
      | '/' when !i + 1 < n && text.[!i + 1] = '*' ->
          i := !i + 2;
          while !i + 1 < n && not (text.[!i] = '*' && text.[!i + 1] = '/') do incr i done;
          i := !i + 2;
          skip ()
      | _ -> ()
  in
  let take pred =
    let s = !i in
    while !i < n && pred text.[!i] do incr i done;
    String.sub text s (!i - s)
  in
  let name_char c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let expect c = skip (); if peek () <> c then failwith (Printf.sprintf "STEP: expected '%c' at %d" c !i); incr i in
  let rec value () =
    skip ();
    match peek () with
    | '#' -> incr i; Ref (int_of_string (take (fun c -> c >= '0' && c <= '9')))
    | '\'' ->
        incr i;
        let b = Buffer.create 16 in
        let rec go () =
          if !i >= n then () else if text.[!i] = '\'' then if !i + 1 < n && text.[!i + 1] = '\'' then (Buffer.add_char b '\''; i := !i + 2; go ()) else incr i else (Buffer.add_char b text.[!i]; incr i; go ())
        in
        go ();
        Str (Buffer.contents b)
    | '.' -> incr i; let s = take name_char in expect '.'; Enum s
    | '$' | '*' -> incr i; Nil
    | '(' -> List (args ())
    | c when (c >= '0' && c <= '9') || c = '-' || c = '+' -> Num (float_of_string (take (fun c -> (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.' || c = 'E' || c = 'e')))
    | _ -> let s = take name_char in if s = "" then failwith (Printf.sprintf "STEP: unexpected '%c' at %d" (peek ()) !i); Typed (s, args ())
  and args () =
    expect '(';
    skip ();
    if peek () = ')' then (incr i; [])
    else
      let rec go acc =
        let v = value () in
        skip ();
        if peek () = ',' then (incr i; go (v :: acc)) else (expect ')'; List.rev (v :: acc))
      in
      go []
  in
  let table = Hashtbl.create 4096 in
  (* the entities are in the DATA section *)
  let rec find k = if k + 5 > n then failwith "STEP: no DATA section" else if String.sub text k 5 = "DATA;" then k + 5 else find (k + 1) in
  i := find 0;
  let rec statements () =
    skip ();
    if peek () = '#' then begin
      incr i;
      let id = int_of_string (take (fun c -> c >= '0' && c <= '9')) in
      expect '=';
      skip ();
      let parts =
        if peek () = '(' then begin
          incr i;
          let rec go acc = skip (); if peek () = ')' then (incr i; List.rev acc) else let s = take name_char in go ((s, args ()) :: acc) in
          go []
        end
        else let s = take name_char in [ (s, args ()) ]
      in
      expect ';';
      Hashtbl.replace table id parts;
      statements ()
    end
  in
  statements ();
  table

let read path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> parse (really_input_string ic (in_channel_length ic)))

(* ---- Reading entities ---- *)

let part (f : file) id name = match Hashtbl.find_opt f id with Some e -> List.assoc_opt name e | None -> None
let kind (f : file) id = match Hashtbl.find_opt f id with Some ((k, _) :: _ as e) -> if List.length e = 1 then k else String.concat "+" (List.map fst e) | _ -> "?"
let num = function Num v -> v | _ -> failwith "STEP: a number was expected"
let ref_ = function Ref r -> r | _ -> failwith "STEP: a reference was expected"
let list = function List l -> l | _ -> failwith "STEP: a list was expected"
let truth = function Enum "T" -> true | _ -> false

(* Millimetres per file unit: files in metres are scaled up. *)
let scale (f : file) =
  Hashtbl.fold
    (fun _ (e : entity) s -> match (List.assoc_opt "LENGTH_UNIT" e, List.assoc_opt "SI_UNIT" e) with Some _, Some [ Nil; Enum "METRE" ] -> 1000. | Some _, Some [ Enum "CENTI"; Enum "METRE" ] -> 10. | _ -> s)
    f 1.

(* Radians per file unit of angle: a file that counts in degrees says so, by naming the
   unit and giving its size (CONVERSION_BASED_UNIT('DEGREE', a measure of 0.01745...)). *)
let turn (f : file) =
  Hashtbl.fold
    (fun _ (e : entity) t ->
      match (List.assoc_opt "PLANE_ANGLE_UNIT" e, List.assoc_opt "CONVERSION_BASED_UNIT" e) with
      | Some _, Some [ Str name; Ref measure ] -> (
          match part f measure "PLANE_ANGLE_MEASURE_WITH_UNIT" with
          | Some (Typed (_, [ Num size ]) :: _) when size > 0. -> size
          | _ -> if String.uppercase_ascii name = "DEGREE" then Float.pi /. 180. else t)
      | _ -> t)
    f 1.

(* ---- Vectors ---- *)

let ( +| ) a b = Array.init 3 (fun i -> a.(i) +. b.(i))
let ( -| ) a b = Array.init 3 (fun i -> a.(i) -. b.(i))
let ( *| ) s a = Array.map (fun v -> s *. v) a
let dot a b = (a.(0) *. b.(0)) +. (a.(1) *. b.(1)) +. (a.(2) *. b.(2))
let cross a b = [| (a.(1) *. b.(2)) -. (a.(2) *. b.(1)); (a.(2) *. b.(0)) -. (a.(0) *. b.(2)); (a.(0) *. b.(1)) -. (a.(1) *. b.(0)) |]
let norm a = sqrt (dot a a)
let unit a = let l = norm a in if l > 0. then (1. /. l) *| a else a

(* ---- Points, directions, placements ---- *)

type placement = { o : float array; x : float array; y : float array; z : float array }

let args f id name = match part f id name with Some a -> a | None -> failwith (Printf.sprintf "STEP: #%d is a %s, not a %s" id (kind f id) name)
let point f s id = match args f id "CARTESIAN_POINT" with [ _; List c ] -> Array.of_list (List.map (fun v -> s *. num v) c) | _ -> failwith "STEP: bad point"
let direction f id = match args f id "DIRECTION" with [ _; List c ] -> unit (Array.of_list (List.map num c)) | _ -> failwith "STEP: bad direction"

let placement f s id =
  match args f id "AXIS2_PLACEMENT_3D" with
  | [ _; o; z; x ] ->
      let z = match z with Ref r -> direction f r | _ -> [| 0.; 0.; 1. |] in
      let x = match x with Ref r -> direction f r | _ -> if Float.abs z.(0) < 0.9 then [| 1.; 0.; 0. |] else [| 0.; 1.; 0. |] in
      let x = unit (x -| (dot x z *| z)) in
      { o = point f s (ref_ o); x; y = cross z x; z }
  | _ -> failwith "STEP: bad placement"

(* ---- NURBS ---- *)

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

(* Knots written with their multiplicities, as one flat array. *)
let knots mults values = Array.of_list (List.concat (List.map2 (fun m k -> List.init (int_of_float (num m)) (fun _ -> num k)) mults values))

type nurbs_curve = { cdegree : int; cpoints : float array array; cweights : float array; cknots : float array }
type nurbs_surface = { du : int; dv : int; net : float array array array;  (** [u][v] *) weights : float array array; ku : float array; kv : float array }

let curve_point c t =
  let n = Array.length c.cpoints in
  let t = Float.max c.cknots.(c.cdegree) (Float.min c.cknots.(n) t) in
  let span, b = basis c.cknots c.cdegree n t in
  let p = ref [| 0.; 0.; 0. |] and w = ref 0. in
  for k = 0 to c.cdegree do
    let i = span - c.cdegree + k in
    let bw = b.(k) *. c.cweights.(i) in
    p := !p +| (bw *| c.cpoints.(i));
    w := !w +. bw
  done;
  (1. /. !w) *| !p

let surface_point s u v =
  let nu = Array.length s.net and nv = Array.length s.net.(0) in
  let u = Float.max s.ku.(s.du) (Float.min s.ku.(nu) u) and v = Float.max s.kv.(s.dv) (Float.min s.kv.(nv) v) in
  let su, bu = basis s.ku s.du nu u and sv, bv = basis s.kv s.dv nv v in
  let p = ref [| 0.; 0.; 0. |] and w = ref 0. in
  for a = 0 to s.du do
    for b = 0 to s.dv do
      let i = su - s.du + a and j = sv - s.dv + b in
      let bw = bu.(a) *. bv.(b) *. s.weights.(i).(j) in
      p := !p +| (bw *| s.net.(i).(j));
      w := !w +. bw
    done
  done;
  (1. /. !w) *| !p

(* ---- Surfaces ---- *)

type surface =
  | Plane of placement
  | Cylinder of placement * float
  | Cone of placement * float * float  (** radius at the origin, half-angle *)
  | Sphere of placement * float
  | Torus of placement * float * float  (** ring radius, tube radius *)
  | Nurbs of nurbs_surface
  | Unknown of string

(* The (u, v) each surface is drawn over: a plane's own two axes; round a cylinder or
   cone, the angle and the height; on a sphere, longitude and latitude; on a torus, the
   angle round the ring and the angle round the tube; a NURBS surface's knot values. *)
let rec at surface u v =
  match surface with
  | Plane p -> p.o +| (u *| p.x) +| (v *| p.y)
  | Cylinder (p, r) -> p.o +| ((r *. cos u) *| p.x) +| ((r *. sin u) *| p.y) +| (v *| p.z)
  | Cone (p, r, a) -> let rr = r +. (v *. tan a) in p.o +| ((rr *. cos u) *| p.x) +| ((rr *. sin u) *| p.y) +| (v *| p.z)
  | Sphere (p, r) -> p.o +| ((r *. cos v *. cos u) *| p.x) +| ((r *. cos v *. sin u) *| p.y) +| ((r *. sin v) *| p.z)
  | Torus (p, ring, tube) -> let rr = ring +. (tube *. cos v) in p.o +| ((rr *. cos u) *| p.x) +| ((rr *. sin u) *| p.y) +| ((tube *. sin v) *| p.z)
  | Nurbs s -> surface_point s u v
  | Unknown _ -> [| nan; nan; nan |]

(* The unit normal on the side the surface's own (u, v) gives. *)
and normal surface u v =
  match surface with
  | Plane p -> p.z
  | Cylinder (p, _) -> (cos u *| p.x) +| (sin u *| p.y)
  | Cone (p, _, a) -> unit ((cos a *| ((cos u *| p.x) +| (sin u *| p.y))) -| (sin a *| p.z))
  | Sphere (p, _) | Torus (p, _, _) -> ((cos v *. cos u) *| p.x) +| ((cos v *. sin u) *| p.y) +| (sin v *| p.z)
  | Nurbs s ->
      (* central differences inside the domain, one-sided at its edges *)
      let u0, u1, v0, v1 = domain surface in
      let hu = 1e-5 *. (u1 -. u0) and hv = 1e-5 *. (v1 -. v0) in
      let ua = Float.max u0 (u -. hu) and ub = Float.min u1 (u +. hu) and va = Float.max v0 (v -. hv) and vb = Float.min v1 (v +. hv) in
      unit (cross (surface_point s ub v -| surface_point s ua v) (surface_point s u vb -| surface_point s u va))
  | Unknown _ -> [| 0.; 0.; 1. |]

(* A NURBS surface's knot range; the others have no bound of their own. *)
and domain = function
  | Nurbs s -> (s.ku.(s.du), s.ku.(Array.length s.net), s.kv.(s.dv), s.kv.(Array.length s.net.(0)))
  | _ -> (neg_infinity, infinity, neg_infinity, infinity)

(* The (u, v) of the point of the surface nearest p. [near] is the (u, v) of a point
   close by, to stay on the same turn of a cylinder and to start Newton on a NURBS. *)
let locate surface ?near p =
  let two_pi = 2. *. Float.pi in
  let turn u = match near with Some (u0, _) -> u +. (two_pi *. Float.round ((u0 -. u) /. two_pi)) | None -> u in
  let turn_v v = match near with Some (_, v0) -> v +. (two_pi *. Float.round ((v0 -. v) /. two_pi)) | None -> v in
  match surface with
  | Plane f -> (dot (p -| f.o) f.x, dot (p -| f.o) f.y)
  | Cylinder (f, _) | Cone (f, _, _) -> let d = p -| f.o in (turn (atan2 (dot d f.y) (dot d f.x)), dot d f.z)
  | Sphere (f, _) ->
      let d = p -| f.o in
      let flat = Float.sqrt ((dot d f.x ** 2.) +. (dot d f.y ** 2.)) in
      (* at a pole every longitude is the same point: keep the one we came from *)
      ((if flat < 1e-9 *. (1. +. norm d) then (match near with Some (u0, _) -> u0 | None -> 0.) else turn (atan2 (dot d f.y) (dot d f.x))), atan2 (dot d f.z) flat)
  | Torus (f, ring, _) ->
      let d = p -| f.o in
      (turn (atan2 (dot d f.y) (dot d f.x)), turn_v (atan2 (dot d f.z) (Float.sqrt ((dot d f.x ** 2.) +. (dot d f.y ** 2.)) -. ring)))
  | Unknown _ -> (0., 0.)
  | Nurbs s ->
      let u0, u1, v0, v1 = domain surface in
      let d (u, v) = norm (surface_point s u v -| p) in
      (* the nearest of a grid of points, to start from *)
      let search n =
        let best = ref (u0, v0) in
        for i = 0 to n do
          for j = 0 to n do
            let uv = (u0 +. ((u1 -. u0) *. float_of_int i /. float_of_int n), v0 +. ((v1 -. v0) *. float_of_int j /. float_of_int n)) in
            if d uv < d !best then best := uv
          done
        done;
        !best
      in
      (* Gauss-Newton on the two tangents, clamped to the domain *)
      let rec go (u, v) k =
        if k = 0 then (u, v)
        else
          let hu = 1e-6 *. (u1 -. u0) and hv = 1e-6 *. (v1 -. v0) in
          let su = (1. /. (2. *. hu)) *| (surface_point s (Float.min u1 (u +. hu)) v -| surface_point s (Float.max u0 (u -. hu)) v) in
          let sv = (1. /. (2. *. hv)) *| (surface_point s u (Float.min v1 (v +. hv)) -| surface_point s u (Float.max v0 (v -. hv))) in
          let r = p -| surface_point s u v in
          let a = dot su su and b = dot su sv and c = dot sv sv and e = dot su r and g = dot sv r in
          let det = (a *. c) -. (b *. b) in
          if Float.abs det < 1e-300 then (u, v)
          else
            let u' = Float.max u0 (Float.min u1 (u +. (((c *. e) -. (b *. g)) /. det))) and v' = Float.max v0 (Float.min v1 (v +. (((a *. g) -. (b *. e)) /. det))) in
            if Float.abs (u' -. u) < 1e-12 *. (u1 -. u0) && Float.abs (v' -. v) < 1e-12 *. (v1 -. v0) then (u', v') else go (u', v') (k - 1)
      in
      let found = go (match near with Some uv -> uv | None -> search 32) 30 in
      (* a hint can lead Newton astray: then search *)
      if near <> None && d found > 1e-3 *. (1. +. norm p) then (let again = go (search 48) 30 in if d again < d found then again else found) else found

(* [s] is the file's length in millimetres and [t] its angle in radians (scale, turn). *)
let surface f s t id =
  let grid l = Array.of_list (List.map (fun row -> Array.of_list (List.map (fun v -> point f s (ref_ v)) (list row))) l) in
  let nurbs shape with_knots rational =
    match (shape, with_knots) with
    | du :: dv :: List net :: _, [ List mu; List mv; List ku; List kv; _ ] ->
        let net = grid net in
        let weights = match rational with Some [ List w ] -> Array.of_list (List.map (fun row -> Array.of_list (List.map num (list row))) w) | _ -> Array.map (Array.map (fun _ -> 1.)) net in
        Nurbs { du = int_of_float (num du); dv = int_of_float (num dv); net; weights; ku = knots mu ku; kv = knots mv kv }
    | _ -> failwith "STEP: bad B-spline surface"
  in
  match Hashtbl.find_opt f id with
  | Some [ ("PLANE", [ _; p ]) ] -> Plane (placement f s (ref_ p))
  | Some [ ("CYLINDRICAL_SURFACE", [ _; p; r ]) ] -> Cylinder (placement f s (ref_ p), s *. num r)
  | Some [ ("CONICAL_SURFACE", [ _; p; r; a ]) ] -> Cone (placement f s (ref_ p), s *. num r, t *. num a)
  | Some [ ("SPHERICAL_SURFACE", [ _; p; r ]) ] -> Sphere (placement f s (ref_ p), s *. num r)
  | Some [ (("TOROIDAL_SURFACE" | "DEGENERATE_TOROIDAL_SURFACE"), _ :: p :: ring :: tube :: _) ] -> Torus (placement f s (ref_ p), s *. num ring, s *. num tube)
  | Some [ ("B_SPLINE_SURFACE_WITH_KNOTS", _ :: rest) ] ->
      let rec split k l = if k = 0 then ([], l) else match l with x :: t -> let a, b = split (k - 1) t in (x :: a, b) | [] -> ([], []) in
      let shape, with_knots = split 7 rest in
      nurbs shape with_knots None
  | Some e when List.mem_assoc "B_SPLINE_SURFACE" e -> nurbs (List.assoc "B_SPLINE_SURFACE" e) (List.assoc "B_SPLINE_SURFACE_WITH_KNOTS" e) (List.assoc_opt "RATIONAL_B_SPLINE_SURFACE" e)
  | _ -> Unknown (kind f id)

(* ---- Edges ---- *)

(* An edge as points from its start vertex to its end vertex: a line as its two ends, a
   circle every 5 degrees, a NURBS curve at eight points a knot span. [detail] scales how
   many: 0.5 gives half as many, for parts with thousands of faces. *)
let edge ?(detail = 1.) f s id =
  match args f id "EDGE_CURVE" with
  | [ _; va; vb; c; same ] ->
      let vertex v = match args f (ref_ v) "VERTEX_POINT" with [ _; p ] -> point f s (ref_ p) | _ -> failwith "STEP: bad vertex" in
      let a = vertex va and b = vertex vb in
      let a, b = if truth same then (a, b) else (b, a) in
      (* a curve on a surface (SolidCraft writes every edge so) is its curve in space *)
      let rec curve id = match Hashtbl.find_opt f id with Some [ (("SURFACE_CURVE" | "SEAM_CURVE" | "INTERSECTION_CURVE"), _ :: c :: _) ] -> curve (ref_ c) | e -> e in
      let pts =
        match curve (ref_ c) with
        | Some [ ("LINE", _) ] -> [ a; b ]
        | Some [ (("CIRCLE" | "ELLIPSE") as kind, _ :: p :: r :: rest) ] ->
            let p = placement f s (ref_ p) and r = s *. num r in
            (* an ellipse is a circle with a second radius along y *)
            let r2 = if kind = "ELLIPSE" then s *. num (List.hd rest) else r in
            let angle q = atan2 (dot (q -| p.o) p.y /. r2) (dot (q -| p.o) p.x /. r) in
            let t0 = angle a in
            let t1 = let t = angle b in if t <= t0 +. 1e-9 then t +. (2. *. Float.pi) else t in
            let n = max 2 (int_of_float (ceil (detail *. (t1 -. t0) /. (5. *. Float.pi /. 180.)))) in
            List.init (n + 1) (fun k -> let t = t0 +. ((t1 -. t0) *. float_of_int k /. float_of_int n) in if k = 0 then a else if k = n then b else p.o +| ((r *. cos t) *| p.x) +| ((r2 *. sin t) *| p.y))
        | Some e when List.mem_assoc "B_SPLINE_CURVE_WITH_KNOTS" e || List.mem_assoc "B_SPLINE_CURVE" e ->
            let shape, with_knots =
              match e with
              | [ ("B_SPLINE_CURVE_WITH_KNOTS", _ :: d :: pts :: form :: closed :: self :: rest) ] -> ([ d; pts; form; closed; self ], rest)
              | _ -> (List.assoc "B_SPLINE_CURVE" e, List.assoc "B_SPLINE_CURVE_WITH_KNOTS" e)
            in
            let curve =
              match (shape, with_knots) with
              | d :: List pts :: _, List m :: List k :: _ ->
                  let cpoints = Array.of_list (List.map (fun v -> point f s (ref_ v)) pts) in
                  let cweights = match List.assoc_opt "RATIONAL_B_SPLINE_CURVE" e with Some [ List w ] -> Array.of_list (List.map num w) | _ -> Array.map (fun _ -> 1.) cpoints in
                  { cdegree = int_of_float (num d); cpoints; cweights; cknots = knots m k }
              | _ -> failwith "STEP: bad B-spline curve"
            in
            let t0 = curve.cknots.(curve.cdegree) and t1 = curve.cknots.(Array.length curve.cpoints) in
            (* where the two vertices are along the curve: the nearest of 400 samples *)
            let nearest q = let best = ref t0 and bd = ref infinity in for k = 0 to 400 do let t = t0 +. ((t1 -. t0) *. float_of_int k /. 400.) in let x = norm (curve_point curve t -| q) in if x < !bd then (bd := x; best := t) done; !best in
            let ta = nearest a in
            let tb = let t = nearest b in if Float.abs (t -. ta) < 1e-9 *. (t1 -. t0) then (if ta -. t0 < t1 -. ta then t1 else t0) else t in
            let spans = Array.length curve.cpoints - curve.cdegree in
            let n = max 4 (int_of_float (ceil (8. *. detail *. float_of_int spans *. Float.abs (tb -. ta) /. (t1 -. t0)))) in
            List.init (n + 1) (fun k -> if k = 0 then a else if k = n then b else curve_point curve (ta +. ((tb -. ta) *. float_of_int k /. float_of_int n)))
        | _ -> [ a; b ]
      in
      if truth same then pts else List.rev pts
  | _ -> failwith "STEP: bad edge"

(* ---- Faces ---- *)

type face = { id : int; label : string; surface : surface; outward : bool;  (** the solid's outside is on the surface's own normal side *) loops : float array list list }

(* Every face, in the file's order: its surface and its boundary loops as closed
   polylines (the last point is not repeated). *)
let faces ?detail (f : file) =
  let s = scale f and t = turn f in
  let ids = List.sort compare (Hashtbl.fold (fun id (e : entity) acc -> if List.mem_assoc "ADVANCED_FACE" e then id :: acc else acc) f []) in
  List.map
    (fun id ->
      match args f id "ADVANCED_FACE" with
      | [ name; List bounds; surf; same ] ->
          let loop b =
            match Hashtbl.find_opt f (ref_ b) with
            | Some [ (("FACE_BOUND" | "FACE_OUTER_BOUND"), [ _; l; forward ]) ] -> (
                match args f (ref_ l) "EDGE_LOOP" with
                | [ _; List edges ] ->
                    let pts =
                      List.concat_map
                        (fun oe ->
                          match args f (ref_ oe) "ORIENTED_EDGE" with
                          | [ _; _; _; e; forward ] ->
                              let p = edge ?detail f s (ref_ e) in
                              let p = if truth forward then p else List.rev p in
                              (* each edge gives its start, not its end: the next edge starts there *)
                              List.filteri (fun k _ -> k < List.length p - 1) p
                          | _ -> failwith "STEP: bad oriented edge")
                        edges
                    in
                    if truth forward then pts else List.rev pts
                | _ -> failwith "STEP: bad loop")
            | _ -> []
          in
          { id; label = (match name with Str n -> n | _ -> ""); surface = surface f s t (ref_ surf); outward = truth same; loops = List.filter (fun l -> l <> []) (List.map loop bounds) }
      | _ -> failwith "STEP: bad face")
    ids

(* ---- Assemblies: the products of a file, and where each is placed in another ---- *)

(* A product is an assembly, with the products placed in it, or a part, with its solids
   and their faces (one may be both). An instance is a product placed: [at] takes a point
   in the product's own coordinates to those of what it is placed in. A product used
   several times is one product with several instances. *)
type product = { definition : int; name : string; solids : int list; faces : int list;  (** of all its solids, as ADVANCED_FACE ids *) placed : instance list }
and instance = { occurrence : int; product : product; at : placement }

let identity = { o = [| 0.; 0.; 0. |]; x = [| 1.; 0.; 0. |]; y = [| 0.; 1.; 0. |]; z = [| 0.; 0.; 1. |] }
let move (t : placement) p = t.o +| (p.(0) *| t.x) +| (p.(1) *| t.y) +| (p.(2) *| t.z)

(* The placement that takes what sits at [a] to [b]: a point keeps in b the coordinates it had in a. *)
let between (a : placement) (b : placement) =
  let turned e = (dot e a.x *| b.x) +| (dot e a.y *| b.y) +| (dot e a.z *| b.z) in
  let t = { identity with x = turned identity.x; y = turned identity.y; z = turned identity.z } in
  { t with o = b.o -| move t a.o }

(* The file's products that are placed in no other, each with all that is placed in it.
   The entities, as the standard has them: a PRODUCT_DEFINITION is a product; its shape
   is a representation (SHAPE_DEFINITION_REPRESENTATION), whose items are its solids, or
   one tied to it (SHAPE_REPRESENTATION_RELATIONSHIP). A NEXT_ASSEMBLY_USAGE_OCCURRENCE
   places one product in another, by a pair of axis placements, one in each product's
   representation (ITEM_DEFINED_TRANSFORMATION): which is whose is told by which
   representation lists it, since writers do not agree on their order. *)
let tree (f : file) =
  let s = scale f in
  let all name = List.sort compare (Hashtbl.fold (fun id (e : entity) acc -> match List.assoc_opt name e with Some a -> (id, a) :: acc | None -> acc) f []) in
  let first id = match Hashtbl.find_opt f id with Some ((_, a) :: _) -> a | _ -> [] in
  let refs l = List.filter_map (function Ref r -> Some r | _ -> None) l in
  let name pd =
    match first pd with
    | _ :: _ :: Ref formation :: _ -> ( match first formation with _ :: _ :: Ref product :: _ -> ( match first product with Str id :: Str n :: _ -> if n = "" then id else n | _ -> "") | _ -> "")
    | _ -> ""
  in
  (* a long name is written over several lines: it is one name *)
  let name pd = String.concat "" (String.split_on_char '\n' (String.concat "" (String.split_on_char '\r' (name pd)))) in
  (* what a product definition or an occurrence is the shape of: its representations, and those tied to them *)
  let shapes = Hashtbl.create 64 and placing = Hashtbl.create 64 in
  let defines pds = match part f pds "PRODUCT_DEFINITION_SHAPE" with Some [ _; _; Ref d ] -> Some d | _ -> None in
  List.iter (fun (_, a) -> match a with [ Ref pds; Ref rep ] -> Option.iter (fun d -> Hashtbl.add shapes d rep) (defines pds) | _ -> ()) (all "SHAPE_DEFINITION_REPRESENTATION");
  List.iter (fun (_, a) -> match a with [ Ref relation; Ref pds ] -> Option.iter (fun d -> Hashtbl.replace placing d relation) (defines pds) | _ -> ()) (all "CONTEXT_DEPENDENT_SHAPE_REPRESENTATION");
  let tied = List.filter_map (fun (_, a) -> match a with [ _; _; Ref a; Ref b ] -> Some (a, b) | _ -> None) (all "SHAPE_REPRESENTATION_RELATIONSHIP") in
  let representations d = let own = Hashtbl.find_all shapes d in List.sort_uniq compare (own @ List.concat_map (fun (a, b) -> (if List.mem a own then [ b ] else []) @ if List.mem b own then [ a ] else []) tied) in
  let items rep = match first rep with [ _; List l; _ ] -> refs l | _ -> [] in
  let shell id = match first id with [ _; List faces ] -> refs faces | [ _; _; Ref inner; _ ] -> ( match first inner with [ _; List faces ] -> refs faces | _ -> []) | _ -> [] in
  let faces_of solid =
    match Hashtbl.find_opt f solid with
    | Some e when List.mem_assoc "MANIFOLD_SOLID_BREP" e || List.mem_assoc "BREP_WITH_VOIDS" e -> ( match first solid with _ :: Ref outer :: rest -> shell outer @ List.concat_map (function List voids -> List.concat_map shell (refs voids) | _ -> []) rest | _ -> [])
    | Some e when List.mem_assoc "SHELL_BASED_SURFACE_MODEL" e -> ( match first solid with [ _; List shells ] -> List.concat_map shell (refs shells) | _ -> [])
    | _ -> []
  in
  let uses = List.filter_map (fun (id, a) -> match a with [ _; _; _; Ref parent; Ref child; _ ] -> Some (id, parent, child) | _ -> None) (all "NEXT_ASSEMBLY_USAGE_OCCURRENCE") in
  let at occurrence child =
    match Option.map (Hashtbl.find_opt f) (Hashtbl.find_opt placing occurrence) with
    | Some (Some relation) -> (
        match (List.assoc_opt "REPRESENTATION_RELATIONSHIP" relation, List.assoc_opt "REPRESENTATION_RELATIONSHIP_WITH_TRANSFORMATION" relation) with
        | Some [ _; _; Ref r1; _ ], Some [ Ref transformation ] -> (
            match first transformation with
            | [ _; _; Ref i1; Ref i2 ] ->
                let theirs = representations child in
                let own, parents = if List.exists (fun r -> List.mem i1 (items r)) theirs then (i1, i2) else if List.exists (fun r -> List.mem i2 (items r)) theirs then (i2, i1) else if List.mem r1 theirs then (i1, i2) else (i2, i1) in
                between (placement f s own) (placement f s parents)
            | _ -> identity)
        | _ -> identity)
    | _ -> identity
  in
  let made = Hashtbl.create 64 in
  let rec product path pd =
    match Hashtbl.find_opt made pd with
    | Some p -> p
    | None ->
        let solids = List.filter (fun i -> faces_of i <> []) (List.concat_map items (representations pd)) in
        (* a product placed in itself, which a broken file could say, is not followed *)
        let placed = List.filter_map (fun (id, parent, child) -> if parent = pd && not (List.mem child (pd :: path)) then Some { occurrence = id; product = product (pd :: path) child; at = at id child } else None) uses in
        let p = { definition = pd; name = name pd; solids; faces = List.concat_map faces_of solids; placed } in
        Hashtbl.replace made pd p;
        p
  in
  List.filter_map (fun (pd, _) -> if List.exists (fun (_, _, child) -> child = pd) uses then None else Some (product [] pd)) (all "PRODUCT_DEFINITION")

