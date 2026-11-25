(* A picture of a board, each net its own colour: the board seen from the front, the
   other layers' copper as seen through it. One layer, or all with the front's over the
   rest, which are fainter.
   Every piece of copper is drawn from its field, a point at a time: where the field
   is below zero is copper, and the last pixel's width of it is how full the pixel at
   the edge is. A part is a pale line round its pads. *)
open Circuit_copper

type view = Side of side | Both_sides

let palette = [| (214, 69, 65); (52, 152, 219); (241, 196, 15); (46, 204, 113); (155, 89, 182); (230, 126, 34); (26, 188, 156); (236, 112, 160); (149, 165, 166); (127, 179, 213); (247, 220, 111) |]

(* The nets in order, each with its colour; copper on no net is grey. *)
let colours (b : Circuit_board.t) =
  let nets = List.sort_uniq compare (List.filter (( <> ) "") (List.map (fun p -> p.net) (Circuit_board.pieces b))) in
  List.mapi (fun i net -> (net, palette.(i mod Array.length palette))) nets

let picture ?(width = 1600) ?window (b : Circuit_board.t) view =
  let points = List.concat_map Sketch.points (Array.to_list b.outline) in
  let over f pick = List.fold_left (fun m p -> f m (pick p)) (pick (List.hd points)) points in
  let room = 1. in
  (* the whole board with a little room, or the part of it asked for *)
  let x0, y0, x1, y1 = match window with Some w -> w | None -> (over Float.min fst -. room, over Float.min snd -. room, over Float.max fst +. room, over Float.max snd +. room) in
  let scale = float_of_int width /. (x1 -. x0) in
  let height = int_of_float (Float.ceil ((y1 -. y0) *. scale)) in
  let rgb = Bytes.make (3 * width * height) '\244' in
  let px = 1. /. scale in
  (* a pixel's middle, and a colour laid over it by how full it is *)
  let place i j = [| x0 +. ((float_of_int i +. 0.5) *. px); y1 -. ((float_of_int j +. 0.5) *. px); 0. |] in
  let lay i j (r, g, bl) full =
    let at = 3 * ((j * width) + i) in
    let mix k v = Bytes.set rgb (at + k) (Char.chr (int_of_float (Float.round (((1. -. full) *. float_of_int (Char.code (Bytes.get rgb (at + k)))) +. (full *. float_of_int v))))) in
    mix 0 r; mix 1 g; mix 2 bl
  in
  (* a field drawn within a box of the plane: what is inside it, or with [line] only
     what is within that of its edge; [faint]: how much of the colour is laid *)
  let fill ?line ?(faint = 1.) field (lx, ly) (hx, hy) colour =
    if not (hx < x0 || lx > x1 || hy < y0 || ly > y1) then
    let p = Prog.compile field in
    let c = Prog.ctx p and whole = Prog.whole p in
    let i0 = max 0 (int_of_float ((lx -. x0) *. scale) - 2) and i1 = min (width - 1) (int_of_float ((hx -. x0) *. scale) + 2) in
    let j0 = max 0 (int_of_float ((y1 -. hy) *. scale) - 2) and j1 = min (height - 1) (int_of_float ((y1 -. ly) *. scale) + 2) in
    for j = j0 to j1 do
      for i = i0 to i1 do
        let v = Prog.value p c whole (place i j) in
        let v = match line with Some half -> Float.abs v -. half | None -> v in
        let full = Float.max 0. (Float.min 1. (0.5 -. (v /. px))) in
        if full > 0. then lay i j colour (faint *. full)
      done
    done
  in
  let colours = colours b in
  let colour net = Option.value ~default:(150, 150, 150) (List.assoc_opt net colours) in
  let pours = Circuit_board.poured b and pieces = Circuit_board.pieces b in
  let side_of ?(faint = 1.) side =
    List.iter
      (fun (pour : Circuit_pour.t) ->
        if pour.side = side then
          for j = 0 to height - 1 do
            for i = 0 to width - 1 do
              let x = place i j in
              let full = Float.max 0. (Float.min 1. (0.5 -. (Circuit_pour.at pour (x.(0), x.(1)) /. px))) in
              if full > 0. then lay i j (colour pour.net) (0.5 *. faint *. full)
            done
          done)
      pours;
    List.iter (fun piece -> if (not piece.bare) && piece.on = [ side ] then fill ~faint (field piece) piece.lo piece.hi (colour piece.net)) pieces
  in
  fill (Sketch.field b.outline) (x0, y0) (x1, y1) (match view with Side Front -> (24, 66, 44) | Side Back -> (22, 44, 78) | Side (Inner _) -> (60, 44, 22) | Both_sides -> (28, 34, 44));
  List.iter (fun cut -> fill (Sketch.field cut) (x0, y0) (x1, y1) (244, 244, 244)) b.cut;
  (match view with Side s -> side_of s | Both_sides -> List.iter (fun s -> if s <> Front then side_of ~faint:0.55 s) (List.rev b.layers); side_of Front);
  (* what goes through, over what is on one layer *)
  List.iter (fun piece -> if (not piece.bare) && List.length piece.on > 1 && (match view with Side s -> List.mem s piece.on | Both_sides -> true) then fill (field piece) piece.lo piece.hi (colour piece.net)) pieces;
  (* a part: a pale line round its pads *)
  List.iter
    (fun (part : Circuit_board.part) ->
      if (match view with Side s -> (part.back && s = Back) || ((not part.back) && s = Front) | Both_sides -> true) then begin
        let pads = List.filter (fun piece -> String.length piece.what > String.length part.name && String.sub piece.what 0 (String.length part.name + 1) = part.name ^ ".") pieces in
        let lx = List.fold_left (fun m p -> Float.min m (fst p.lo)) infinity pads and ly = List.fold_left (fun m p -> Float.min m (snd p.lo)) infinity pads in
        let hx = List.fold_left (fun m p -> Float.max m (fst p.hi)) neg_infinity pads and hy = List.fold_left (fun m p -> Float.max m (snd p.hi)) neg_infinity pads in
        let f = rounded (0.5 *. (lx +. hx), 0.5 *. (ly +. hy)) (hx -. lx +. 0.5) (hy -. ly +. 0.5) 0.3 0. in
        fill ~line:0.04 ~faint:0.75 (form_field f) f.lo f.hi (238, 238, 228)
      end)
    b.parts;
  List.iter (fun ((x, y), d) -> let f = disc (x, y) (0.5 *. d) in fill (form_field f) f.lo f.hi (20, 20, 20)) (Circuit_board.holes b);
  (Png.encode ~w:width ~h:height rgb, colours)
