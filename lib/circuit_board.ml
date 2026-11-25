(* A board, written as code: its outline, the parts on it, which of their pins are
   joined (the nets), and the copper that joins them.

     let b = Circuit_board.make "blink" (Circuit_board.rect 30. 20.) in
     Circuit_board.part b "R1" ~footprint:"Resistor_SMD/R_0603_1608Metric" (14., 12.);
     Circuit_board.net b "VCC" [ "J1.1"; "R1.1" ];
     Circuit_board.trace b ~net:"VCC" [ Circuit_board.pin b "J1.1"; Circuit_board.pin b "R1.1" ];

   A pin is a part's name and a pad's number, "R1.2". A trace is drawn through points,
   and a pin's place is one ([pin]), so a trace follows a part that is moved. What the
   nets say is what is meant; what the copper does is checked against it (Check). *)
module S = Sketch
open Circuit_copper

type part = { name : string; footprint : Circuit_footprint.t; at : float * float; turn : float; back : bool; colour : (float * float * float) option  (** its body's, where it is to be other than dark *) }

type t = {
  name : string;
  outline : S.t;
  layers : side list;  (** its layers of copper, from the front to the back *)
  clearance : float;  (** the least there may be between copper of two nets *)
  edge : float;  (** and between copper and the board's edge, or an opening in it *)
  mutable parts : part list;
  mutable nets : (string * string list) list;
  mutable copper : piece list;  (** traces and vias *)
  mutable added : piece list;  (** more of them, the last first: a board has thousands, each put before the rest *)
  mutable cut : S.t list;  (** openings cut through it, inside its outline *)
  mutable frame : Frame.t;  (** where its plane is in the model it goes with: its front is z = 0 there *)
  mutable thickness : float;
  mutable drilled : ((float * float) * float) list;  (** vias' holes: where, and how wide *)
  mutable pours : (string * side * float) list;  (** a net poured over a side, and how far it keeps from the edge *)
}

let make ?(clearance = 0.2) ?edge ?(layers = [ Front; Back ]) name outline = { name; outline; layers; clearance; edge = Option.value ~default:clearance edge; parts = []; nets = []; copper = []; added = []; cut = []; frame = Frame.identity; thickness = 1.6; drilled = []; pours = [] }

(* The outline of a plain board, w by h, a corner at the origin. *)
let rect w h = S.polygon ~name:(fun i -> List.nth [ "bottom edge"; "right edge"; "top edge"; "left edge" ] i) [ (0., 0.); (w, 0.); (w, h); (0., h) ]

(* The same with its corners rounded to r. *)
let rounded_rect w h r =
  S.(
    start (r, 0.) |> line "bottom edge" (w -. r, 0.) |> arc "corner 1" ~centre:(w -. r, r) (w, r) |> line "right edge" (w, h -. r) |> arc "corner 2" ~centre:(w -. r, h -. r) (w -. r, h)
    |> line "top edge" (r, h) |> arc "corner 3" ~centre:(r, h -. r) (0., h -. r) |> line "left edge" (0., r) |> arc "corner 4" ~centre:(r, r) (r, 0.) |> close)

(* A part, by its footprint, at a place, turned by [turn] degrees; on the back if said. *)
let part b name ~footprint ?(turn = 0.) ?(back = false) ?colour at =
  if List.exists (fun (p : part) -> p.name = name) b.parts then failwith (Printf.sprintf "%s: there are two parts called %s" b.name name);
  b.parts <- { name; footprint; at; turn; back; colour } :: b.parts

(* Where a pad of a part is on the board, and how it is turned. A part on the back is
   seen from the front as in a mirror. *)
let placed (p : part) (pad : Circuit_footprint.pad) =
  let x, y = pad.at in
  (turned (if p.back then (-.x, y) else (x, y)) p.turn ++ p.at, (if p.back then -.pad.turn else pad.turn) +. p.turn)

let pad_of b pin =
  match String.rindex_opt pin '.' with
  | None -> failwith (Printf.sprintf "%s: a pin is a part and a pad, as R1.2: %S is not" b.name pin)
  | Some i -> (
      let part = String.sub pin 0 i and number = String.sub pin (i + 1) (String.length pin - i - 1) in
      match List.find_opt (fun (p : part) -> p.name = part) b.parts with
      | None -> failwith (Printf.sprintf "%s: there is no part %s (for the pin %s)" b.name part pin)
      | Some p -> (
          match List.find_opt (fun (d : Circuit_footprint.pad) -> d.number = number) p.footprint.pads with
          | None -> failwith (Printf.sprintf "%s: %s (%s) has no pad %s" b.name part p.footprint.name number)
          | Some d -> (p, d)))

(* Where a pin is. *)
let pin b name = let p, d = pad_of b name in fst (placed p d)

(* The pins that are to be joined, under a name. *)
let net b name pins =
  List.iter (fun pin -> ignore (pad_of b pin)) pins;
  b.nets <- (name, pins) :: b.nets

let net_of b pin = Option.value ~default:"" (List.find_map (fun (name, pins) -> if List.mem pin pins then Some name else None) b.nets)

(* The same for many pins: every pin's net, looked up at once. *)
let nets_of b =
  let by_pin = Hashtbl.create 1024 in
  List.iter (fun (name, pins) -> List.iter (fun pin -> if not (Hashtbl.mem by_pin pin) then Hashtbl.replace by_pin pin name) pins) (List.rev b.nets);
  fun pin -> Option.value ~default:"" (Hashtbl.find_opt by_pin pin)

let add b pieces = b.added <- List.rev_append pieces b.added

let trace b ~net ?(side = Front) ?(width = 0.25) points = add b (Circuit_copper.trace ~net ~side ~width points)

(* Copper of any closed outline, as it is drawn: an electrode, a shield, a pad that is
   no rectangle. What it is called is for the reports. *)
let copper b ~net ?(side = Front) what outline = add b [ piece what ~net [ side ] (Circuit_copper.drawn outline) ]

(* A trace along a circle about [centre]: from the angle [from], in degrees
   counter-clockwise from the x axis, on through [sweep] degrees, the other way round
   below zero; 360 is the whole circle. In stretches of a third of a turn at most, each
   over the next where they join. *)
let arc b ~net ?(side = Front) ?(width = 0.25) ~centre ~radius from sweep =
  let n = max 1 (int_of_float (Float.ceil (Float.abs sweep /. 120.))) in
  let at t = let a = t *. Float.pi /. 180. in (fst centre +. (radius *. cos a), snd centre +. (radius *. sin a)) in
  add b
    (List.init n (fun k ->
         let s = from +. (sweep *. float_of_int k /. float_of_int n) and e = from +. (sweep *. float_of_int (k + 1) /. float_of_int n) in
         piece (Printf.sprintf "%s trace, an arc" (if net = "" then "a" else net)) ~net [ side ] (band (at s) (at (0.5 *. (s +. e))) (at e) (0.5 *. width))))

let via b ~net ?(diameter = 0.6) ?(drill = 0.3) at =
  add b [ Circuit_copper.via ~net ~diameter ~through:b.layers at ];
  b.drilled <- (at, drill) :: b.drilled

(* A hole with no copper, for a screw: all copper keeps clear of it. *)
let hole b ?(diameter = 2.2) at =
  add b [ piece ~bare:true (Printf.sprintf "the hole at (%g, %g)" (fst at) (snd at)) ~net:"" b.layers (disc at (0.5 *. diameter)) ];
  b.drilled <- (at, diameter) :: b.drilled

(* An opening cut through the board, inside its outline: an outline of its own. *)
let opening b loop = b.cut <- b.cut @ [ loop ]

(* A net poured over all of a side that is left (Pour). *)
let pour b ~net ?(side = Back) ?(edge = 0.3) () = b.pours <- b.pours @ [ (net, side, edge) ]

(* The pads, each a piece of copper on its pin's net. *)
let pads b =
  let net_of = nets_of b in
  List.concat_map
    (fun (p : part) ->
      List.map
        (fun (d : Circuit_footprint.pad) ->
          let at, turn = placed p d and w, h = d.size in
          let shape =
            match d.shape with
            | Circle -> disc at (0.5 *. w)
            | Rect -> rounded at w h 0. turn
            | Oval -> rounded at w h (0.5 *. Float.min w h) turn
            | Roundrect k -> rounded at w h (k *. Float.min w h) turn
          in
          let pin = p.name ^ "." ^ d.number in
          piece pin ~net:(net_of pin) (if d.through then b.layers else [ (if p.back then Back else Front) ]) shape)
        p.footprint.pads)
    (List.rev b.parts)

(* All its copper. *)
let pieces b = pads b @ b.copper @ List.rev b.added

(* Every hole: the vias', and those of the pads that go through. *)
let holes b =
  b.drilled
  @ List.concat_map (fun (p : part) -> List.filter_map (fun (d : Circuit_footprint.pad) -> if d.through && d.drill > 0. then Some (fst (placed p d), d.drill) else None) p.footprint.pads) b.parts

(* Its pours, each worked out against the rest of its copper. *)
let poured b = List.map (fun (net, side, edge) -> Circuit_pour.fill ~outline:b.outline ~cuts:b.cut ~pieces:(pieces b) ~holes:(holes b) ~net ~side ~clearance:b.clearance ~edge ()) b.pours

(* The boards made solid while a model was built, latest first: they go with what the
   model's program writes (Emit), so a command that has the model has its boards. *)
let made : t list ref = ref []

(* The board as a solid, to be a part like another: a plate of its outline, the front
   of it at z = 0 and [thickness] of it below, with its holes of a millimetre or more
   through it (a via's is left out: a board has thousands). It is built in the frame
   it is called in, where its copper is not: that stays in the board's own plane. *)
(* A board goes with the model being built, where the frame it is kept in is: its front
   at that frame's z = 0, its copper drawn there in the model's picture. *)
let keep ?(thickness = 1.6) b =
  b.frame <- !G.ambient;
  b.thickness <- thickness;
  made := b :: List.filter (fun (o : t) -> o.name <> b.name) !made

(* How far inside the board a place is: from its outline and from every opening in it,
   the nearest; below zero outside. For copper that is laid as far as the edge allows. *)
let inside b =
  let read loop = let p = Prog.compile (flat (fun () -> S.field loop)) in let c = Prog.ctx p and whole = Prog.whole p in fun (x, y) -> Prog.value p c whole [| x; y; 0. |] in
  let outline = read b.outline and cuts = List.map read b.cut in
  fun at -> List.fold_left (fun least cut -> Float.min least (cut at)) (-.outline at) cuts

let solid ?(thickness = 1.6) b =
  keep ~thickness b;
  S.extrude ~holes:(b.cut @ List.filter_map (fun (at, d) -> if d >= 1. then Some (S.circle "hole" at (0.5 *. d)) else None) (holes b)) ~lo:(-.thickness) ~hi:0. b.outline
