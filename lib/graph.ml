(* The select graph: the one form a model has.

   A node is a named smooth formula (a piece), smooth arithmetic over other nodes (an
   apply), or a choice between two nodes by the sign of a third (a select: guard <= 0
   takes [le], otherwise [gt]). Selects may sit anywhere, under arithmetic and inside
   guards.

   Two annotations stand in for interval arithmetic:
   - [lip]: a bound on the node's slope, wherever it is the one selected;
   - [jump]: how far the node's value can step across the guards below it (0 unless
     fitted pieces disagree at a guard).
   From one sample at a cell's centre they give an interval for the whole cell
   (Octree). They are declared, not derived: a wrong one prunes wrongly. *)

module E = Expr

(* Where an apply's result gets its face name: input i's name (an offset of A is still
   A), a new name made from the operation and its inputs' names (fillet[A,B]), the
   inputs' one name where they all have it and a made one where they differ (a loft:
   the face "side" of two sections is still "side" between them), or none (a guard is
   never a face). *)
type naming = Keep of int | Made of string | Joined of string | Unnamed

type t = { id : int; lip : float; jump : float; feature : int; node : node }

and node =
  | Piece of { name : string; field : E.t }
  | Apply of { formula : E.t; inputs : t array; naming : naming }
  | Select of { guard : t; le : t; gt : t; smooth : bool; step : float }
      (** [smooth]: the two sides meet tangentially (a seam), not at a crease. An edge is
          wherever the faces on the two sides have different names, either way. [step]:
          how far the two sides can disagree at the guard. *)

(* ---- Features and frames ---- *)

(* A feature is a named part of a model with the place in the code that defines it.
   Every node made while its function runs carries its id, so a face leads to a feature
   and a feature to a file and line. Features nest, and a face is named within its
   feature: "top" made inside "bar" inside "link 2" is the face "link 2/bar/top", so the
   same shape built twice gives different faces. Feature 0 is "no feature".

   A feature may sit in a frame, given in the frame of the feature around it: that is a
   forward chain. Everything built inside is built in that frame.

   A group is a feature that is only a name around others: it builds no solid of its
   own, and what it holds are parts of whatever it is in, as if it were not there. *)
type feature = {
  fid : int;
  fname : string;
  path : string;
  file : string;
  line : int;
  parent : int;
  frame : Frame.t option;  (** in the world *)
  colour : Colour.t option;  (** its own: what is made inside it is that colour too, unless it has one of its own *)
  group : bool;
  mutable result : t option;  (** what it built; of a group, the features in it together *)
}

let features : feature list ref = ref []
let current = ref 0
let counter = ref 0
let ambient = ref Frame.identity

(* The ambient frame's coordinates as formulas, when it is not the world's. *)
let coords : E.t array option ref = ref None
let find_feature fid = List.find_opt (fun f -> f.fid = fid) !features
let prefix () = match find_feature !current with Some f -> f.path ^ "/" | None -> ""
let local e = match !coords with Some c -> E.subst c e | None -> e

let mk ~lip ~jump node =
  incr counter;
  { id = !counter; lip; jump; feature = !current; node }

(* Whatever [build] makes is made in [frame], itself given in the frame around it. *)
let within frame build =
  let saved = (!ambient, !coords) in
  ambient := Frame.compose !ambient frame;
  coords := Some (Frame.coords !ambient);
  Fun.protect ~finally:(fun () -> ambient := fst saved; coords := snd saved) build

(* What [build] makes, made in a feature of that name, in [frame] if one is given and of
   [colour] ("#rrggbb") if one is given. [result]: the feature's solid, from what was
   built. *)
let named ?frame ?colour ~group (file, line, _, _) fname build result =
  let run () =
    let colour = Option.map Colour.of_string colour in
    let f = { fid = List.length !features + 1; fname; path = prefix () ^ fname; file; line; parent = !current; frame = Option.map (fun _ -> !ambient) frame; colour; group; result = None } in
    features := f :: !features;
    current := f.fid;
    Fun.protect ~finally:(fun () -> current := f.parent) (fun () ->
        let built = build () in
        f.result <- result f built;
        built)
  in
  match frame with Some fr -> within fr run | None -> run ()

(* [feature __POS__ "name" (fun () -> ...)]: the graph the function builds, as a feature. *)
let feature ?frame ?colour pos fname build = named ?frame ?colour ~group:false pos fname build (fun _ g -> Some g)

(* As given: no feature's name in front, no frame. For rebuilding nodes that exist. *)
let raw_piece ?(lip = 1.) name field = mk ~lip ~jump:0. (Piece { name; field })

let raw_apply ?(naming = Unnamed) ~lip formula inputs =
  let jump = List.fold_left (fun m (i : t) -> m +. i.jump) 0. inputs in
  mk ~lip ~jump (Apply { formula; inputs = Array.of_list inputs; naming })

let piece ?lip name field = raw_piece ?lip (prefix () ^ name) (local field)
let apply ?naming ~lip formula inputs = raw_apply ?naming ~lip (local formula) inputs

(* A plain formula over the coordinates as an unnamed node: for guards written directly. *)
let expr ?(lip = 1.) e = apply ~lip e []

let select ?(jump = 0.) ?(smooth = false) ~guard ~le ~gt () =
  mk ~lip:(Float.max le.lip gt.lip) ~jump:(jump +. Float.max le.jump gt.jump) (Select { guard; le; gt; smooth; step = jump })

let x0 = E.slot 0 and x1 = E.slot 1
let diff a b = apply ~lip:(a.lip +. b.lip) (E.sub x0 x1) [ a; b ]
let union a b = select ~guard:(diff a b) ~le:a ~gt:b ()
let intersection a b = select ~guard:(diff a b) ~le:b ~gt:a ()
let offset r a = apply ~naming:(Keep 0) ~lip:a.lip (E.sub x0 (E.const r)) [ a ]
let complement a = apply ~naming:(Keep 0) ~lip:a.lip (E.neg x0) [ a ]
let subtract a b = intersection a (complement b)

(* [group __POS__ "name" (fun () -> ...)]: the features the function makes, under one
   name. It may build anything: a list of parts, a body and what is cut from it. Shown
   alone, a group is the features made in it, together. *)
let group ?frame ?colour pos fname build =
  named ?frame ?colour ~group:true pos fname build (fun f _ ->
      match List.rev (List.filter_map (fun x -> if x.parent = f.fid then x.result else None) !features) with [] -> None | first :: rest -> Some (List.fold_left union first rest))

(* Polynomial smooth union of width k: with d = b - a and h = (d + k) / 2k,
   blend = h a + (1 - h) b - k h (1 - h) where |d| < k, and min (a, b) outside. The
   blend's gradient is h grad a + (1 - h) grad b, so its slope is at most the larger. *)
let blend ~sign ~width ~name a b =
  let k = E.const width in
  let d = if sign > 0. then E.sub x1 x0 else E.sub x0 x1 in
  let h = E.div (E.add d k) (E.mul (E.const 2.) k) in
  let g = E.sub (E.const 1.) h in
  let mix = E.add (E.mul h x0) (E.mul g x1) and bump = E.mul k (E.mul h g) in
  let lip = Float.max a.lip b.lip and both = a.lip +. b.lip in
  let blend = apply ~naming:(Made name) ~lip (if sign > 0. then E.sub mix bump else E.add mix bump) [ a; b ] in
  let inner = select ~smooth:true ~guard:(apply ~lip:both (E.sub d k) [ a; b ]) ~le:blend ~gt:a () in
  select ~smooth:true ~guard:(apply ~lip:both (E.add d k) [ a; b ]) ~le:b ~gt:inner ()

let fillet = blend ~sign:1.

(* The mirror: smooth max. *)
let fillet_intersection = blend ~sign:(-1.)

(* A fillet of constant radius where a and b meet at a convex edge (an intersection):
   the solid moved inwards by r, then outwards by r. With u = a + r and v = b + r, the
   distance to the inner solid is max (u, v), except where its nearest point is its
   edge: there it is the distance to that edge, sqrt (u^2 + v^2 - 2 c u v) / sqrt (1 - c^2).
   [cos] = c is the cosine between the gradients of a and b. Exact where a and b are
   distances whose gradients keep that angle: two planes, or a plane and a cylinder
   square to it. Elsewhere it is the fillet for that angle only. *)
let round_intersection ~radius ~cos:c ~name a b =
  let u = offset (-.radius) a and v = offset (-.radius) b in
  let k = E.const c in
  let sharp = intersection u v in
  let edge = E.div (E.sqrt (E.sub (E.add (E.square x0) (E.square x1)) (E.mul (E.mul (E.const 2.) k) (E.mul x0 x1)))) (E.const (Float.sqrt (1. -. (c *. c)))) in
  let arc = apply ~naming:(Made name) ~lip:(Float.max a.lip b.lip) edge [ u; v ] in
  let lip = a.lip +. b.lip in
  let past_v = select ~smooth:true ~guard:(apply ~lip (E.sub x1 (E.mul k x0)) [ u; v ]) ~le:sharp ~gt:arc () in
  offset radius (select ~smooth:true ~guard:(apply ~lip (E.sub x0 (E.mul k x1)) [ u; v ]) ~le:sharp ~gt:past_v ())

(* The same at a concave edge (a union): the complements' fillet, complemented. *)
let round_union ~radius ~cos ~name a b = complement (round_intersection ~radius ~cos ~name (complement a) (complement b))

(* Every formula rewritten by [f], names, features and bounds kept: for rigid moves,
   which leave slopes as they are. *)
let map_formulas f g =
  let memo = Hashtbl.create 64 in
  let rec go g =
    match Hashtbl.find_opt memo g.id with
    | Some v -> v
    | None ->
        let v =
          match g.node with
          | Piece p -> { g with node = Piece { p with field = f p.field } }
          | Apply a -> { g with node = Apply { a with formula = f a.formula; inputs = Array.map go a.inputs } }
          | Select s -> { g with node = Select { s with guard = go s.guard; le = go s.le; gt = go s.gt } }
        in
        incr counter;
        let v = { v with id = !counter } in
        Hashtbl.replace memo g.id v;
        v
  in
  go g

(* The shape as it sits in [frame]: a rigid move of what is already built. *)
let place frame g = map_formulas (E.subst (Frame.coords frame)) g

(* Each piece replaced by [f name field] (made with [raw_piece]: the name and field are
   already in place), everything above rebuilt over the new pieces. *)
let map_pieces f g =
  let memo = Hashtbl.create 64 in
  let rec go g =
    match Hashtbl.find_opt memo g.id with
    | Some v -> v
    | None ->
        let v =
          match g.node with
          | Piece p -> { (f p.name p.field) with feature = g.feature }
          | Apply a -> { (raw_apply ~naming:a.naming ~lip:g.lip a.formula (Array.to_list (Array.map go a.inputs))) with feature = g.feature }
          | Select s -> select ~jump:s.step ~smooth:s.smooth ~guard:(go s.guard) ~le:(go s.le) ~gt:(go s.gt) ()
        in
        Hashtbl.replace memo g.id v;
        v
  in
  go g

(* [f] applied to every face (every node under the selects), pushed below them: a
   select of mapped faces is the same function as the mapped select, but each face is
   now its own node. Guards keep their values: they read the mapped nodes back through
   [inverse] (inverse (f v) = v), so a mapped select is still on the way to its guard's
   value, which the edge walker relies on to find where a curve's faces change. *)
let map_faces f inverse g =
  let values = Hashtbl.create 64 and guards = Hashtbl.create 64 in
  let memo table (g : t) make =
    match Hashtbl.find_opt table g.id with
    | Some v -> v
    | None ->
        let v = make () in
        Hashtbl.replace table g.id v;
        v
  in
  let rec value g =
    memo values g (fun () ->
        match g.node with
        | Select s -> select ~jump:s.step ~smooth:s.smooth ~guard:(guard s.guard) ~le:(value s.le) ~gt:(value s.gt) ()
        | _ -> f g)
  and guard h =
    memo guards h (fun () ->
        match h.node with
        | Apply { naming = Unnamed; formula; inputs } -> raw_apply ~lip:h.lip formula (Array.to_list (Array.map guard inputs))
        | _ -> inverse (value h))
  in
  value g

(* A wall of [thickness] inside [g]: g and not (g moved inwards by t). Inner faces are
   named inner[name]. Faces in [open_faces] are pushed out by 2t in the inner copy only,
   so the hollow breaks through them. Exact where g is a distance. *)
let shell ?(open_faces = []) ~thickness g =
  let t = E.const thickness in
  let inner =
    if open_faces = [] then g
    else map_pieces (fun name field -> raw_piece name (if List.mem name open_faces then E.sub field (E.mul (E.const 2.) t) else field)) g
  in
  let flip = E.neg (E.add x0 t) in
  let cavity = map_faces (fun face -> apply ~naming:(Made "inner") ~lip:face.lip flip [ face ]) (fun m -> apply ~lip:m.lip flip [ m ]) inner in
  intersection g cavity
