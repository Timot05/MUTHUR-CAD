(* Top-down pruning. Each cell is tested once, at its centre.

   A node's value over a cell lies within [reach] of its value at the centre:
   reach = lip * (half the cell's diagonal) + jump. So:
   - a select whose guard is further than its reach from 0 is decided for the whole cell;
   - a cell whose field is further than its reach from 0 holds no surface.
   A child starts from its parent's decisions, so only the cells at the top pay for the
   whole model. Cells are cubes; a cell stops splitting when it holds no surface, has no
   undecided select left (one smooth face: nothing to find), or is at the last level.

   A last-level cell is kept for seeding only if an edge can pass through it: on a
   select's curve both of its sides give the surface, so with the select forced either
   way the field must be within reach of 0 at the centre. A side is read here just past
   its guard, where its declared slope need not hold exactly: the reach is widened by
   half for that. *)

type cell = {
  lo : float array;
  size : float;
  state : Bytes.t;  (** per node: 0 undecided, 1 le, 2 gt *)
  live : int;  (** undecided selects still on the way from the root *)
  surface : bool;
  mutable kids : cell option array;  (** by octant: bit 0 x, bit 1 y, bit 2 z; empty for a leaf *)
}

type t = { root : cell; whole : Bytes.t; cells : int; leaves : cell list  (** last-level cells with a surface and a live select *) }

let centre c = Array.map (fun l -> l +. (0.5 *. c.size)) c.lo
let radius c = 0.5 *. sqrt 3. *. c.size

(* The cell's decisions, from its parent's and one evaluation at its centre.

   [joins]: the unions that join the model's bodies, where the page may take a body out
   by a switch (Joined). One of those gives a side up only where that side is outside all
   over the cell as well: with the side it takes switched off, the other is the model. *)
let prune ?(joins = Bytes.empty) (p : Prog.t) (c : Prog.ctx) parent x r =
  let st = ref parent and live = ref [] in
  Prog.start p c ~full:false parent;
  let decide n side =
    if !st == parent then (st := Bytes.copy parent; c.state <- !st);
    Bytes.unsafe_set !st n side
  in
  let rec visit n =
    if c.seen.(n) <> c.epoch then begin
      c.seen.(n) <- c.epoch;
      let ins = p.inputs.(n) in
      match p.kind.(n) with
      | 0 -> ()
      | 1 -> Array.iter visit ins
      | _ -> (
          match Bytes.unsafe_get parent n with
          | '\001' -> visit ins.(1)
          | '\002' -> visit ins.(2)
          | _ ->
              let g = ins.(0) in
              Prog.ev p c x g;
              let v = c.v.(p.last.(g)) and reach = (p.lip.(g) *. r) +. p.jump.(g) in
              (* the guard read both sides: each has its value at the centre *)
              let spare s = n >= Bytes.length joins || Bytes.unsafe_get joins n = '\000' || c.v.(p.last.(ins.(s))) -. (p.lip.(ins.(s)) *. r) -. p.jump.(ins.(s)) > 0. in
              if v -. reach > 0. && spare 1 then (decide n '\002'; visit ins.(2))
              else if v +. reach <= 0. && spare 2 then (decide n '\001'; visit ins.(1))
              else (live := n :: !live; visit g; visit ins.(1); visit ins.(2)))
    end
  in
  visit p.root;
  Prog.ev p c x p.root;
  let f = c.v.(p.last.(p.root)) in
  (!st, !live, not (Float.abs f > (p.lip.(p.root) *. r) +. p.jump.(p.root)))

(* Octant [o] of a cell at [lo], of side h: where it starts, unless that is beyond [hi]. *)
let octant lo h hi o =
  let l = Array.mapi (fun a v -> if o land (1 lsl a) <> 0 then v +. h else v) lo in
  if l.(0) >= hi.(0) || l.(1) >= hi.(1) || l.(2) >= hi.(2) then None else Some l

let build (p : Prog.t) (c : Prog.ctx) ~lo ~hi ~depth =
  let size = Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo) in
  let cells = ref 0 and leaves = ref [] in
  let rec make lo size parent d =
    incr cells;
    let cell0 = { lo; size; state = parent; live = 0; surface = true; kids = [||] } in
    let state, live, surface = prune p c parent (centre cell0) (radius cell0) in
    let cell = { cell0 with state; live = List.length live; surface } in
    if surface && live <> [] then
      if d = depth then begin
        let x = centre cell and reach = 1.5 *. ((p.lip.(p.root) *. radius cell) +. p.jump.(p.root)) in
        let near s side = Float.abs (Prog.value ~force:(fun n -> if n = s then side else 0) p c state x) <= reach in
        if List.exists (fun s -> near s 1 && near s 2) live then leaves := cell :: !leaves
      end
      else begin
        let h = 0.5 *. size in
        cell.kids <- Array.init 8 (fun o -> Option.map (fun l -> make l h state (d + 1)) (octant lo h hi o))
      end;
    cell
  in
  let whole = Prog.whole p in
  let root = make lo size whole 0 in
  { root; whole; cells = !cells; leaves = List.rev !leaves }

(* Every cell the pruning stops at: one that holds no surface, or one at the last level.
   [f] is given where the cell starts, its side, its decisions and whether it may hold
   surface. The decisions are made to hold within [pad] of the cell too, and with
   [joins] whatever bodies are switched off ([prune]). *)
let cells ?(pad = 0.) ?joins (p : Prog.t) (c : Prog.ctx) ~lo ~hi ~depth f =
  let rec visit l size parent d =
    let cell = { lo = l; size; state = parent; live = 0; surface = true; kids = [||] } in
    let state, _, surface = prune ?joins p c parent (centre cell) (radius cell +. pad) in
    if surface && d < depth then List.iter (fun o -> Option.iter (fun k -> visit k (0.5 *. size) state (d + 1)) (octant l (0.5 *. size) hi o)) [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    else f l size state surface
  in
  visit lo (Array.fold_left Float.max 0. (Array.map2 ( -. ) hi lo)) (Prog.whole p) 0

(* Every last-level cell that may hold surface, whether or not a select is left in it:
   [f] is given where the cell starts, its side and its decisions. *)
let shell p c ~lo ~hi ~depth f = cells p c ~lo ~hi ~depth (fun l size state surface -> if surface then f l size state)

(* The box of the model's surface within lo..hi, to within a last-level cell. None if
   the model has no surface there. *)
let bounds p c ~lo ~hi ~depth =
  let blo = Array.make 3 infinity and bhi = Array.make 3 neg_infinity in
  shell p c ~lo ~hi ~depth (fun l size _ -> Array.iteri (fun a v -> blo.(a) <- Float.min blo.(a) v; bhi.(a) <- Float.max bhi.(a) (v +. size)) l);
  if blo.(0) > bhi.(0) then None else Some (Array.map2 Float.max lo blo, Array.map2 Float.min hi bhi)

let holds c x = x.(0) >= c.lo.(0) && x.(1) >= c.lo.(1) && x.(2) >= c.lo.(2) && x.(0) <= c.lo.(0) +. c.size && x.(1) <= c.lo.(1) +. c.size && x.(2) <= c.lo.(2) +. c.size

(* The smallest cell holding x, if the tree holds it. *)
let find t x =
  if not (holds t.root x) then None
  else
    let rec go c =
      if Array.length c.kids = 0 then c
      else
        let h = 0.5 *. c.size in
        let o = (if x.(0) >= c.lo.(0) +. h then 1 else 0) lor (if x.(1) >= c.lo.(1) +. h then 2 else 0) lor if x.(2) >= c.lo.(2) +. h then 4 else 0 in
        match c.kids.(o) with Some k -> go k | None -> c
    in
    Some (go t.root)

(* The decisions to evaluate with at x: those of the smallest cell holding it. *)
let state t x = match find t x with Some c -> c.state | None -> t.whole
