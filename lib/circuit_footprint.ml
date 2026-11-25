(* A footprint: the pads a part is soldered to. One is made here, in a line of code
   ([chip], [header], [two_rows]), or read from a file of the kind makers and
   libraries give them in ([read]: a .kicad_mod file, which is text and open).

   Such a file draws with y down; a board here has y up, as the kernel has. A pad's
   place is turned over as it is read, so a footprint looks here as it does there. *)

type shape = Circle | Rect | Oval | Roundrect of float  (** the corners' radius, as a share of the shorter side *)

type pad = {
  number : string;
  through : bool;  (** a hole through the board, with copper on both sides *)
  shape : shape;
  at : float * float;
  turn : float;  (** degrees *)
  size : float * float;
  drill : float;  (** 0 for none *)
}

type t = {
  name : string;
  pads : pad list;
  court : (float * float) * (float * float);  (** the room the part takes on the board, about its origin: its body with what its maker asks round it *)
  body : ((float * float * float) * (float * float * float)) option;
      (** the part itself as a box, where that is known: from a corner to the one across, z up from the board's face *)
}

(* ---- Footprints made here ---- *)

(* A footprint is only its pads, and the common ones are a line of code each: nothing
   need be read from anywhere. Lengths in millimetres, y up. *)
let pad ?(turn = 0.) ?(drill = 0.) number shape at size = { number; through = drill > 0.; shape; at; turn; size; drill }

(* The box of some pads about the footprint's origin, each as wide as it reaches turned. *)
let box_of pads =
  List.fold_left
    (fun ((lx, ly), (hx, hy)) (d : pad) ->
      let w, h = d.size and x, y = d.at in
      let reach = if Float.abs (Float.rem d.turn 180.) < 1e-6 then (0.5 *. w, 0.5 *. h) else if Float.abs (Float.rem d.turn 180.) -. 90. |> Float.abs < 1e-6 then (0.5 *. h, 0.5 *. w) else let r = 0.5 *. sqrt ((w *. w) +. (h *. h)) in (r, r) in
      ((Float.min lx (x -. fst reach), Float.min ly (y -. snd reach)), (Float.max hx (x +. fst reach), Float.max hy (y +. snd reach))))
    ((infinity, infinity), (neg_infinity, neg_infinity))
    pads

(* A footprint of some pads, under a name. The room it takes is its maker's, or where
   none is said its pads' box with a quarter of a millimetre round it. *)
let make ?court ?body name pads =
  let court = match court with Some c -> c | None -> let (lx, ly), (hx, hy) = box_of pads in ((lx -. 0.25, ly -. 0.25), (hx +. 0.25, hy +. 0.25)) in
  { name; pads; court; body }

(* Two pads side by side, [apart] between their middles: a resistor, a capacitor, a
   light. Pad 1 is on the left. *)
let chip ?(name = "chip") ~apart size = make name [ pad "1" (Roundrect 0.25) (-0.5 *. apart, 0.) size; pad "2" (Roundrect 0.25) (0.5 *. apart, 0.) size ]

let r0603 = chip ~name:"0603" ~apart:1.65 (0.8, 0.95)
let led0603 = chip ~name:"0603 light" ~apart:1.575 (0.875, 0.95)

(* A row of n pins through the board, 2.54 apart, pin 1 at the top and square. *)
let header n = make (Printf.sprintf "header of %d" n) (List.init n (fun k -> pad ~drill:1.0 (string_of_int (k + 1)) (if k = 0 then Rect else Circle) (0., -2.54 *. float_of_int k) (1.7, 1.7)))

(* A chip with n legs in two rows, [pitch] between legs and [across] between the rows'
   middles: pin 1 top left, counted down the left and up the right. *)
let two_rows ?(name = "two rows") n ~pitch ~across size =
  let half = n / 2 in
  let y k = pitch *. ((float_of_int (half - 1) /. 2.) -. float_of_int k) in
  make name (List.init half (fun k -> pad (string_of_int (k + 1)) (Roundrect 0.25) (-0.5 *. across, y k) size) @ List.init half (fun k -> pad (string_of_int (half + k + 1)) (Roundrect 0.25) (0.5 *. across, y (half - 1 - k)) size))

let soic8 = two_rows ~name:"SOIC-8" 8 ~pitch:1.27 ~across:5.4 (1.55, 0.6)

(* A push button with a pad either end. *)
let button = make "button" [ pad "1" Rect (-3., 0.) (1.5, 1.4); pad "2" Rect (3., 0.) (1.5, 1.4) ]

(* ---- What footprint files are written in: lists in brackets ---- *)

type sexp = Atom of string | List of sexp list

let parse text =
  let n = String.length text in
  let rec skip i = if i < n && (text.[i] = ' ' || text.[i] = '\n' || text.[i] = '\t' || text.[i] = '\r') then skip (i + 1) else i in
  let rec item i =
    let i = skip i in
    if i >= n then failwith "a footprint file ends too soon"
    else if text.[i] = '(' then
      let rec items i acc =
        let i = skip i in
        if i >= n then failwith "a bracket in a footprint file is not closed" else if text.[i] = ')' then (List (List.rev acc), i + 1) else let x, i = item i in items i (x :: acc)
      in
      items (i + 1) []
    else if text.[i] = '"' then begin
      let b = Buffer.create 16 in
      let rec go i = if i >= n then failwith "a quote in a footprint file is not closed" else if text.[i] = '\\' && i + 1 < n then (Buffer.add_char b text.[i + 1]; go (i + 2)) else if text.[i] = '"' then i + 1 else (Buffer.add_char b text.[i]; go (i + 1)) in
      let j = go (i + 1) in
      (Atom (Buffer.contents b), j)
    end
    else
      let rec go j = if j < n && not (List.mem text.[j] [ ' '; '\n'; '\t'; '\r'; '('; ')' ]) then go (j + 1) else j in
      let j = go i in
      (Atom (String.sub text i (j - i)), j)
  in
  fst (item 0)

(* ---- Reading one ---- *)

(* A footprint from a file of that kind, by its path. *)
let read path =
  let name = Filename.remove_extension (Filename.basename path) in
  if not (Sys.file_exists path) then failwith (Printf.sprintf "there is no footprint file %s" path);
  let said key items = List.find_map (function List (Atom k :: rest) when k = key -> Some rest | _ -> None) items in
  let number = function Atom s -> float_of_string s | List _ -> failwith "a number was expected in a footprint file" in
  let items = match parse (In_channel.with_open_bin path In_channel.input_all) with List (Atom ("footprint" | "module") :: items) -> items | _ -> failwith (Printf.sprintf "%s is not a footprint file" path) in
  let pads =
    match items with
    | items ->
        List.filter_map
          (function
            | List (Atom "pad" :: Atom pad_number :: Atom kind :: Atom shape :: rest) when kind = "smd" || kind = "thru_hole" ->
                let at, turn = match said "at" rest with Some [ x; y ] -> ((number x, -.number y), 0.) | Some [ x; y; a ] -> ((number x, -.number y), number a) | _ -> ((0., 0.), 0.) in
                let size = match said "size" rest with Some [ w; h ] -> (number w, number h) | _ -> failwith (Printf.sprintf "%s: pad %s has no size" name pad_number) in
                let drill = match said "drill" rest with Some (Atom "oval" :: w :: _) -> number w | Some (d :: _) -> number d | _ -> 0. in
                let shape =
                  match shape with
                  | "circle" -> Circle
                  | "rect" -> Rect
                  | "oval" -> Oval
                  | "roundrect" -> Roundrect (match said "roundrect_rratio" rest with Some [ k ] -> number k | _ -> 0.25)
                  | other -> failwith (Printf.sprintf "%s: pad %s is a %s, a shape not read yet" name pad_number other)
                in
                Some { number = pad_number; through = kind = "thru_hole"; shape; at; turn; size; drill }
            | _ -> None)
          items
  in
  if pads = [] then failwith (Printf.sprintf "%s has no pads" name);
  (* the room it takes: the box of what is drawn on the layer for that, on the front *)
  let place = function x :: y :: _ -> Some (number x, -.number y) | _ -> None in
  let drawn =
    List.concat_map
      (function
        | List (Atom (("fp_line" | "fp_rect" | "fp_arc" | "fp_circle" | "fp_poly") as kind) :: rest) when said "layer" rest = Some [ Atom "F.CrtYd" ] -> (
            let at key = Option.bind (said key rest) place in
            match kind with
            | "fp_circle" -> ( match (at "center", at "end") with Some (cx, cy), Some (ex, ey) -> let r = sqrt (((ex -. cx) ** 2.) +. ((ey -. cy) ** 2.)) in [ (cx -. r, cy -. r); (cx +. r, cy +. r) ] | _ -> [])
            | "fp_poly" -> ( match said "pts" rest with Some pts -> List.filter_map (function List (Atom "xy" :: xy) -> place xy | _ -> None) pts | None -> [])
            | _ -> List.filter_map at [ "start"; "mid"; "end" ])
        | _ -> [])
      items
  in
  let court = match drawn with [] -> None | p :: rest -> Some (List.fold_left (fun ((lx, ly), (hx, hy)) (x, y) -> ((Float.min lx x, Float.min ly y), (Float.max hx x, Float.max hy y))) (p, p) rest) in
  (* The part's body: the box of its maker's 3D model, where the file names one that is
     there and can be read (a STEP file or a mesh: Asset), put as the file says: scaled,
     turned (KiCad turns the other way round each axis, x first), then moved. A path
     that starts at a name in ${} is looked for from the footprint's folder upwards,
     and a library's own models beside its footprints. *)
  let body =
    List.find_map
      (function
        | List (Atom "model" :: Atom model :: rest) -> (
            let xyz key = match said key rest with Some [ List [ Atom "xyz"; x; y; z ] ] -> Some (number x, number y, number z) | _ -> None in
            let after = match String.index_opt model '}' with Some i when String.length model > 1 && model.[0] = '$' -> Some (String.sub model (i + 1) (String.length model - i - 1)) | _ -> None in
            let rec upwards dir tail n = if n = 0 then None else let at = dir ^ tail in if Sys.file_exists at then Some at else upwards (Filename.dirname dir) tail (n - 1) in
            let found =
              match after with
              | None -> if Sys.file_exists model then Some model else None
              | Some tail -> ( match upwards (Filename.dirname path) tail 5 with Some at -> Some at | None -> upwards (Filename.dirname path) ("/3dmodels" ^ tail) 5)
            in
            (* a model drawn for the screen only has one to measure beside it, often *)
            let found = Option.bind found (fun at -> if Filename.check_suffix at ".wrl" then let step = Filename.remove_extension at ^ ".step" in if Sys.file_exists step then Some step else None else Some at) in
            match found with
            | None -> None
            | Some at -> (
                match Asset.read at with
                | exception (Failure _ | Sys_error _) -> None
                | mesh ->
                    let sx, sy, sz = Option.value ~default:(1., 1., 1.) (xyz "scale") and rx, ry, rz = Option.value ~default:(0., 0., 0.) (xyz "rotate") in
                    let ox, oy, oz = match (xyz "offset", xyz "at") with Some o, _ -> o | None, Some (x, y, z) -> (25.4 *. x, 25.4 *. y, 25.4 *. z) | None, None -> (0., 0., 0.) in
                    let turn about (a, b) = let t = -.about *. Float.pi /. 180. in ((a *. cos t) -. (b *. sin t), (a *. sin t) +. (b *. cos t)) in
                    let lo = [| infinity; infinity; infinity |] and hi = [| neg_infinity; neg_infinity; neg_infinity |] in
                    for k = 0 to (Array.length mesh.points / 3) - 1 do
                      let x = sx *. mesh.points.(3 * k) and y = sy *. mesh.points.((3 * k) + 1) and z = sz *. mesh.points.((3 * k) + 2) in
                      let y, z = turn rx (y, z) in
                      let z, x = turn ry (z, x) in
                      let x, y = turn rz (x, y) in
                      Array.iteri (fun a v -> lo.(a) <- Float.min lo.(a) v; hi.(a) <- Float.max hi.(a) v) [| x +. ox; y +. oy; z +. oz |]
                    done;
                    if lo.(0) > hi.(0) then None
                    else begin
                      (* Some models are drawn away from their own origin, and stand beside
                         their pads or half in the board. One whose middle is outside the
                         room its footprint takes is put on the middle of that room; one
                         with no pin through the board, and its foot well under the
                         board's face, is stood on the face. *)
                      let (cx0, cy0), (cx1, cy1) = match court with Some c -> c | None -> box_of pads in
                      let mx = 0.5 *. (lo.(0) +. hi.(0)) and my = 0.5 *. (lo.(1) +. hi.(1)) in
                      let dx, dy = if mx < cx0 || mx > cx1 || my < cy0 || my > cy1 then ((0.5 *. (cx0 +. cx1)) -. mx, (0.5 *. (cy0 +. cy1)) -. my) else (0., 0.) in
                      let dz = if lo.(2) < -0.1 && not (List.exists (fun (d : pad) -> d.through) pads) then -.lo.(2) else 0. in
                      Some ((lo.(0) +. dx, lo.(1) +. dy, lo.(2) +. dz), (hi.(0) +. dx, hi.(1) +. dy, hi.(2) +. dz))
                    end))
        | _ -> None)
      items
  in
  make ?court ?body name pads

(* ---- One written out as code ---- *)

(* A footprint as the line of OCaml that makes it, under a name of its own: so that one
   a maker gives as a file is read once, and is code from then on, to be read and
   changed like the rest. Lengths as the file has them, to a tenth of a micrometre. *)
let code ?name (f : t) =
  let num x = let x = if Float.abs x < 5e-5 then 0. else x in let t = Printf.sprintf "%.4f" x in
    (* without the noughts at its end, and with a point, as OCaml writes a float *)
    let rec cut i = if t.[i] = '0' then cut (i - 1) else i in
    let t = String.sub t 0 (cut (String.length t - 1) + 1) in
    if x < 0. then "(" ^ t ^ ")" else t
  in
  let pair (x, y) = Printf.sprintf "(%s, %s)" (num x) (num y) in
  let called = match name with Some n -> n | None -> String.map (function ('a' .. 'z' | '0' .. '9') as c -> c | 'A' .. 'Z' as c -> Char.lowercase_ascii c | _ -> '_') f.name in
  let called = if called = "" || (called.[0] >= '0' && called.[0] <= '9') then "f_" ^ called else called in
  let pad (d : pad) =
    Printf.sprintf "pad%s%s %S %s %s %s" (if d.turn <> 0. then Printf.sprintf " ~turn:%s" (num d.turn) else "") (if d.drill > 0. then Printf.sprintf " ~drill:%s" (num d.drill) else "") d.number
      (match d.shape with Circle -> "Circle" | Rect -> "Rect" | Oval -> "Oval" | Roundrect k -> Printf.sprintf "(Roundrect %s)" (num k))
      (pair d.at) (pair d.size)
  in
  let three (x, y, z) = Printf.sprintf "(%s, %s, %s)" (num x) (num y) (num z) in
  Printf.sprintf "let %s =\n  make %S\n    ~court:(%s, %s)\n%s    [ %s ]\n" called f.name (pair (fst f.court)) (pair (snd f.court))
    (match f.body with Some (lo, hi) -> Printf.sprintf "    ~body:(%s, %s)\n" (three lo) (three hi) | None -> "")
    (String.concat ";\n      " (List.map pad f.pads))
