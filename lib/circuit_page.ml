(* A board written out for the page that draws it with the graphics card
   (web/board.html): every piece of copper as sixteen numbers, its box, its shape, its
   layer, its colour and its net, in the order they are to be drawn (the back first,
   what goes through the board over that, the holes last); the outline as points; each
   pour as its cells' values. The page is one file with all of it inside. *)
open Circuit_copper

(* Numbers as text: 32 bits each, then base 64. *)
let floats l =
  let b = Bytes.create (4 * List.length l) in
  List.iteri (fun i x -> Bytes.set_int32_le b (4 * i) (Int32.bits_of_float x)) l;
  Png.base64 (Bytes.to_string b)

(* The parts' bodies, each a box of fifteen numbers in the board's own frame (its front
   at z = 0, z towards whoever looks at the front): the box's three edges from one
   corner, that corner, and its colour. A part on the back is seen from the front as in
   a mirror, and stands on the back face, downwards. *)
let bodies (b : Circuit_board.t) =
  List.concat_map
    (fun (p : Circuit_board.part) ->
      match p.footprint.body with
      | None -> []
      | Some ((x0, y0, z0), (x1, y1, z1)) ->
          let side = if p.back then -1. else 1. in
          let ex, ey = turned (side, 0.) p.turn and fx, fy = turned (0., 1.) p.turn in
          let cx, cy = turned (side *. x0, y0) p.turn ++ p.at in
          let r, g, bl = Option.value ~default:(0.13, 0.13, 0.14) p.colour in
          [ ex *. (x1 -. x0); ey *. (x1 -. x0); 0.; fx *. (y1 -. y0); fy *. (y1 -. y0); 0.; 0.; 0.; side *. (z1 -. z0); cx; cy; (if p.back then -.b.thickness -. z0 else z0); r; g; bl ])
    (List.rev b.parts)

(* The board as data: what the page that draws it alone is written with, and what the
   model's own page asks for to draw the copper on the board where it is in the model. *)
let data (b : Circuit_board.t) =
  let pieces = Circuit_board.pieces b in
  let nets = List.sort_uniq compare (List.filter (( <> ) "") (List.map (fun p -> p.net) pieces)) in
  let index = Hashtbl.create 64 in
  List.iteri (fun i n -> Hashtbl.replace index n i) nets;
  let colour net = match Hashtbl.find_opt index net with Some i -> let r, g, bl = Circuit_draw.palette.(i mod Array.length Circuit_draw.palette) in [ float_of_int r /. 255.; float_of_int g /. 255.; float_of_int bl /. 255. ] | None -> [ 0.6; 0.6; 0.6 ] in
  let layer side = let rec at i = function [] -> -1 | s :: rest -> if s = side then i else at (i + 1) rest in at 0 b.layers in
  let numbers (p : piece) ~layer ~colour ~net =
    let (lx, ly), (hx, hy) = (p.lo, p.hi) in
    [ lx; ly; hx; hy ]
    @ (match p.shape with
      | Line ((ax, ay), (bx, by), r) -> [ ax; ay; bx; by; r; 0.; 0. ]
      | Rounded ((cx, cy), hw, hh, r, turn) -> [ cx; cy; hw; hh; r; turn *. Float.pi /. 180.; 1. ]
      | Arc ((cx, cy), big, r, from, sweep) -> [ cx; cy; big; r; from; sweep; 2. ]
      | Any -> invalid_arg "an outline of its own is not drawn from numbers")
    @ [ layer ] @ colour @ [ net ]
  in
  let drawn (p : piece) = numbers p ~layer:(match p.on with [ s ] -> float_of_int (layer s) | _ -> -1.) ~colour:(colour p.net) ~net:(match Hashtbl.find_opt index p.net with Some i -> float_of_int i | None -> -1.) in
  (* copper of an outline of its own is drawn as a pour is: from its field's values at small cells *)
  let copper, own = List.partition (fun p -> p.shape <> Any) (List.filter (fun p -> not p.bare) pieces) in
  let on_one side = List.filter (fun p -> p.on = [ side ]) copper and through = List.filter (fun p -> List.length p.on > 1) copper in
  let holes = List.map (fun (at, d) -> piece "a hole" ~net:"" b.layers (disc at (0.5 *. d))) (Circuit_board.holes b) in
  let ordered = List.concat_map on_one (List.rev b.layers) @ through in
  let instances = List.concat_map drawn ordered @ List.concat_map (fun h -> numbers h ~layer:(-1.) ~colour:[ 0.07; 0.07; 0.07 ] ~net:(-2.)) holes in
  let loop sketch = List.concat_map (fun span -> match List.rev (Sketch.points span) with _ :: rest -> List.rev rest | [] -> []) (Array.to_list sketch) in
  let outline = List.concat_map (fun span -> match List.rev (Sketch.points span) with _ :: rest -> List.rev rest | [] -> []) (Array.to_list b.outline) in
  let xs = List.map fst outline and ys = List.map snd outline in
  let num x = Json.Num x in
  let own =
    List.map
      (fun (p : piece) ->
        let (lx, ly), (hx, hy) = (p.lo, p.hi) in
        let cell = Float.max 0.02 (Float.max (hx -. lx) (hy -. ly) /. 256.) in
        let x0 = lx -. cell and y0 = ly -. cell in
        let nx = int_of_float (Float.ceil ((hx -. lx) /. cell)) + 2 and ny = int_of_float (Float.ceil ((hy -. ly) /. cell)) + 2 in
        let read = reader (field p) in
        let values = List.init (nx * ny) (fun k -> read (x0 +. ((float_of_int (k mod nx) +. 0.5) *. cell), y0 +. ((float_of_int (k / nx) +. 0.5) *. cell))) in
        Json.Obj
          [ ("net", num (match Hashtbl.find_opt index p.net with Some i -> float_of_int i | None -> -1.)); ("layer", num (float_of_int (match p.on with s :: _ -> layer s | [] -> 0))); ("x0", num x0); ("y0", num y0); ("cell", num cell);
            ("nx", num (float_of_int nx)); ("ny", num (float_of_int ny)); ("colour", Json.Arr (List.map num (colour p.net))); ("solid", num 1.); ("values", Json.Str (floats values)) ])
      own
  in
  let pours =
    List.map
      (fun (p : Circuit_pour.t) ->
        (* an island that was dropped is no pour: its cells are said to be outside *)
        let values = Array.to_list (Array.mapi (fun k v -> if not (Float.is_finite v) then 1e6 else if v < 0. && not p.kept.(p.island.(k)) then Float.abs v else v) p.value) in
        Json.Obj
          [ ("net", num (match Hashtbl.find_opt index p.net with Some i -> float_of_int i | None -> -1.)); ("layer", num (float_of_int (layer p.side))); ("x0", num p.x0); ("y0", num p.y0); ("cell", num p.cell);
            ("nx", num (float_of_int p.nx)); ("ny", num (float_of_int p.ny)); ("colour", Json.Arr (List.map num (colour p.net))); ("values", Json.Str (floats values)) ])
      (Circuit_board.poured b)
  in
  let axis k = Json.Arr (List.init 3 (fun i -> num b.frame.r.(i).(k))) in
  Json.Obj
      [ ("name", Json.Str b.name); ("layers", Json.Arr (List.map (fun s -> Json.Str (said s)) b.layers)); ("nets", Json.Arr (List.map (fun n -> Json.Str n) nets));
        ("names", Json.Arr (List.map (fun (p : piece) -> Json.Str p.what) (ordered @ holes))); ("instances", Json.Str (floats instances));
        ("outline", Json.Arr (List.map (fun (x, y) -> Json.Arr [ num x; num y ]) outline));
        ("cuts", Json.Arr (List.map (fun cut -> Json.Arr (List.map (fun (x, y) -> Json.Arr [ num x; num y ]) (loop cut))) b.cut));
        ("box", Json.Arr (List.map num [ List.fold_left Float.min infinity xs; List.fold_left Float.min infinity ys; List.fold_left Float.max neg_infinity xs; List.fold_left Float.max neg_infinity ys ]));
        ("pours", Json.Arr (pours @ own));
      ("bodies", Json.Str (floats (bodies b)));
      (* where its plane is in its model: the frame's three axes and its origin, and how thick the board is *)
      ("x", axis 0); ("y", axis 1); ("z", axis 2); ("o", Json.Arr (List.map num (Array.to_list b.frame.t))); ("thickness", num b.thickness) ]

let write ~template (b : Circuit_board.t) =
  let data = data b in
  let mark = "/*BOARD*/null" in
  let page = In_channel.with_open_bin template In_channel.input_all in
  let rec find i = if i + String.length mark > String.length page then failwith (template ^ " has no place for a board in it") else if String.sub page i (String.length mark) = mark then i else find (i + 1) in
  let i = find 0 in
  String.sub page 0 i ^ Json.to_string data ^ String.sub page (i + String.length mark) (String.length page - i - String.length mark)
