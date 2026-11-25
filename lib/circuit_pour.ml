(* A pour: copper over all of a side that is left, on one net. It is one formula of the
   kernel's fields, the greater of
   - the board's own field, moved in by how far copper keeps from the edge, and
   - for every piece of another net on that side, and every hole: how far inside that
     piece grown by the clearance a point is.
   Below zero is pour. Growing a piece is taking a length off its field, so the gap
   left round it is the clearance exactly, round its corners too. Copper of the pour's
   own net is not kept clear of: the pour runs into it, which is how it is joined.

   What a field does not say is which parts of the pour hang together. For that the
   formula is read at the middles of small cells, and the cells below zero are
   gathered into islands, each cell with its four neighbours. An island that touches
   no copper of its net is on no net at all, and is dropped. *)
open Circuit_copper

type t = {
  net : string;
  side : side;
  x0 : float;
  y0 : float;
  cell : float;
  nx : int;
  ny : int;
  value : float array;  (** the formula at each cell's middle, row by row from y0 *)
  island : int array;  (** the island a cell is in, -1 for none *)
  kept : bool array;  (** by island *)
  touching : (int * string) list;  (** an island, and a piece of the net that lies on it *)
}

(* A field read at the middles of the cells within a box. *)
let over ~x0 ~y0 ~cell ~nx ~ny field (lx, ly) (hx, hy) f =
  let p = Prog.compile field in
  let c = Prog.ctx p and whole = Prog.whole p in
  let i0 = max 0 (int_of_float (Float.floor ((lx -. x0) /. cell))) and i1 = min (nx - 1) (int_of_float (Float.floor ((hx -. x0) /. cell))) in
  let j0 = max 0 (int_of_float (Float.floor ((ly -. y0) /. cell))) and j1 = min (ny - 1) (int_of_float (Float.floor ((hy -. y0) /. cell))) in
  for j = j0 to j1 do
    for i = i0 to i1 do
      f ((j * nx) + i) (Prog.value p c whole [| x0 +. ((float_of_int i +. 0.5) *. cell); y0 +. ((float_of_int j +. 0.5) *. cell); 0. |])
    done
  done

(* [outline]: the board's. [pieces]: all its copper. [holes]: where, and how wide.
   [clearance]: what the pour keeps from other nets. [edge]: and from the board's edge.
   [cell]: how fine the islands are told apart, and how fine the pour's own edge is
   known between two cells. *)
let fill ?(cell = 0.05) ?(cuts = []) ~outline ~pieces ~holes ~net ~side ~clearance ~edge () =
  let points = List.concat_map Sketch.points (Array.to_list outline) in
  let least pick = List.fold_left (fun m p -> Float.min m (pick p)) infinity points and most pick = List.fold_left (fun m p -> Float.max m (pick p)) neg_infinity points in
  let x0 = least fst and y0 = least snd in
  let nx = int_of_float (Float.ceil ((most fst -. x0) /. cell)) and ny = int_of_float (Float.ceil ((most snd -. y0) /. cell)) in
  let over = over ~x0 ~y0 ~cell ~nx ~ny in
  let value = Array.make (nx * ny) infinity in
  over (Sketch.field outline) (x0, y0) (most fst, most snd) (fun k v -> value.(k) <- v +. edge);
  (* an opening in the board is an edge of it too *)
  List.iter
    (fun cut ->
      let points = List.concat_map Sketch.points (Array.to_list cut) in
      let lx = List.fold_left (fun m p -> Float.min m (fst p)) infinity points and ly = List.fold_left (fun m p -> Float.min m (snd p)) infinity points in
      let hx = List.fold_left (fun m p -> Float.max m (fst p)) neg_infinity points and hy = List.fold_left (fun m p -> Float.max m (snd p)) neg_infinity points in
      over (Sketch.field cut) (lx -. edge -. cell, ly -. edge -. cell) (hx +. edge +. cell, hy +. edge +. cell) (fun k v -> value.(k) <- Float.max value.(k) (edge -. v)))
    cuts;
  let on_side (p : piece) = List.mem side p.on in
  let grown (lx, ly) (hx, hy) = ((lx -. clearance -. cell, ly -. clearance -. cell), (hx +. clearance +. cell, hy +. clearance +. cell)) in
  List.iter (fun (p : piece) -> if on_side p && p.net <> net then let lo, hi = grown p.lo p.hi in over (field p) lo hi (fun k v -> value.(k) <- Float.max value.(k) (clearance -. v))) pieces;
  List.iter (fun (at, d) -> let f = disc at (0.5 *. d) in over (form_field f) f.lo f.hi (fun k v -> value.(k) <- Float.max value.(k) (-.v))) holes;
  (* the islands *)
  let island = Array.make (nx * ny) (-1) and count = ref 0 in
  Array.iteri
    (fun start v ->
      if v < 0. && island.(start) < 0 then begin
        let waiting = Stack.create () in
        Stack.push start waiting;
        island.(start) <- !count;
        while not (Stack.is_empty waiting) do
          let k = Stack.pop waiting in
          let i = k mod nx and j = k / nx in
          List.iter
            (fun (a, b) -> if a >= 0 && a < nx && b >= 0 && b < ny then let m = (b * nx) + a in if value.(m) < 0. && island.(m) < 0 then (island.(m) <- !count; Stack.push m waiting))
            [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
        done;
        incr count
      end)
    value;
  (* and which of them its own net's copper lies on *)
  let touching = ref [] in
  List.iter
    (fun (p : piece) ->
      if on_side p && p.net = net then begin
        let on = Hashtbl.create 4 in
        over (field p) p.lo p.hi (fun k v -> if v < 0. && island.(k) >= 0 then Hashtbl.replace on island.(k) ());
        Hashtbl.iter (fun i () -> touching := (i, p.what) :: !touching) on
      end)
    pieces;
  let kept = Array.make !count false in
  List.iter (fun (i, _) -> kept.(i) <- true) !touching;
  { net; side; x0; y0; cell; nx; ny; value; island; kept; touching = !touching }

(* How far inside the pour a point is (below zero), an island dropped being no pour:
   between the cells' middles, by the four about it. *)
let at (p : t) (x, y) =
  let u = ((x -. p.x0) /. p.cell) -. 0.5 and v = ((y -. p.y0) /. p.cell) -. 0.5 in
  let i = int_of_float (Float.floor u) and j = int_of_float (Float.floor v) in
  let read a b = if a < 0 || a >= p.nx || b < 0 || b >= p.ny then infinity else let k = (b * p.nx) + a in if p.value.(k) < 0. && not p.kept.(p.island.(k)) then Float.abs p.value.(k) else p.value.(k) in
  let s = u -. float_of_int i and t = v -. float_of_int j in
  let mix a b w = if Float.is_finite a && Float.is_finite b then a +. (w *. (b -. a)) else if w < 0.5 then a else b in
  mix (mix (read i j) (read (i + 1) j) s) (mix (read i (j + 1)) (read (i + 1) (j + 1)) s) t

let islands (p : t) = (Array.fold_left (fun n k -> if k then n + 1 else n) 0 p.kept, Array.length p.kept)
