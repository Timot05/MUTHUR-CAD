(* Circuit boards to try the tool on, each a model: the board made solid, with the board
   itself beside it for mk52 board (lib/circuit.ml).

   - lights: an eight-legged chip told what to do over two wires, with three lights.
     J1 brings in power and the wires (from the top: 5 V, SDA, SCL, ground); R1 and R2
     pull the wires up; C1 is the chip's capacitor; SW1 takes RESET to ground; R3 to R5
     and D1 to D3 are a resistor and a light on each output. Ground is poured over
     both sides and joined by vias, and the supply reaches R2 round the back.
   - blink: a header, a resistor and a light. blink-broken is the same with two faults
     put in for the checks to find: the via left out, so ground is in two pieces, and
     a trace of the supply run past the light's pad too near. blink-cut has ground
     poured over the back and a trace of the supply across it, which cuts the pour in
     two and leaves a corner that reaches nothing. *)
open Mk52
module B = Circuit.Board
module F = Circuit.Footprint

let lo = [| -3.; -3.; -4. |] and hi = [| 47.; 33.; 3. |]

let lights () =
  let b = B.make "lights" (B.rounded_rect 44. 30. 3.) in
  List.iter (B.hole b) [ (3.5, 3.5); (40.5, 3.5); (3.5, 26.5); (40.5, 26.5) ];
  let part = B.part b in
  part "J1" ~footprint:(F.header 4) (4.5, 19.);
  part "U1" ~footprint:F.soic8 (20., 15.);
  part "C1" ~footprint:F.r0603 (26., 18.5);
  (* stood on end: pad 1 below, pad 2 above *)
  part "R1" ~footprint:F.r0603 ~turn:90. (9., 18.2);
  part "R2" ~footprint:F.r0603 ~turn:90. (9., 11.9);
  part "SW1" ~footprint:F.button (14., 6.);
  let rows = [ (1, 20.); (2, 15.); (3, 10.) ] in
  List.iter
    (fun (k, y) ->
      part (Printf.sprintf "R%d" (k + 2)) ~footprint:F.r0603 (33., y);
      (* turned round: its anode, pad 2, towards the resistor *)
      part (Printf.sprintf "D%d" k) ~footprint:F.led0603 ~turn:180. (38., y))
    rows;
  let net = B.net b in
  net "VCC" [ "J1.1"; "U1.8"; "C1.1"; "R1.2"; "R2.1" ];
  net "SDA" [ "J1.2"; "U1.1"; "R1.1" ];
  net "SCL" [ "J1.3"; "U1.2"; "R2.2" ];
  net "RESET" [ "U1.3"; "SW1.2" ];
  net "GND" [ "J1.4"; "U1.4"; "C1.2"; "SW1.1"; "D1.1"; "D2.1"; "D3.1" ];
  net "OUT1" [ "U1.7"; "R3.1" ];
  net "OUT2" [ "U1.6"; "R4.1" ];
  net "OUT3" [ "U1.5"; "R5.1" ];
  net "LIT1" [ "R3.2"; "D1.2" ];
  net "LIT2" [ "R4.2"; "D2.2" ];
  net "LIT3" [ "R5.2"; "D3.2" ];
  let pin = B.pin b in
  let front net points = B.trace b ~net points and back net points = B.trace b ~net ~side:Circuit.Copper.Back points in
  (* the supply: along the top to the capacitor and the chip, up from R1, and round the back to R2 *)
  front "VCC" [ pin "J1.1"; (4.5, 21.5); (25.175, 21.5); pin "C1.1" ];
  front "VCC" [ pin "U1.8"; (23.6, 16.905); pin "C1.1" ];
  front "VCC" [ pin "R1.2"; (9., 21.5) ];
  back "VCC" [ pin "J1.1"; (2.2, 19.); (2.2, 9.5); (9., 9.5) ];
  B.via b ~net:"VCC" (9., 9.5);
  front "VCC" [ (9., 9.5); pin "R2.1" ];
  (* the two wires, each with its resistor's other end on it *)
  front "SDA" [ pin "J1.2"; (12., 16.46); (12.445, 16.905); pin "U1.1" ];
  front "SDA" [ pin "R1.1"; (9., 16.46) ];
  front "SCL" [ pin "J1.3"; (10.5, 13.92); (12.215, 15.635); pin "U1.2" ];
  front "SCL" [ pin "R2.2"; (9., 13.92) ];
  front "RESET" [ pin "U1.3"; (15.9, 14.365); (15.9, 7.5); pin "SW1.2" ];
  (* the outputs fan out to the right *)
  front "OUT1" [ pin "U1.7"; (28., 15.635); (28., 19.); (29., 20.); pin "R3.1" ];
  front "OUT2" [ pin "U1.6"; (30.5, 14.365); (31.135, 15.); pin "R4.1" ];
  front "OUT3" [ pin "U1.5"; (28., 13.095); (28., 11.); (29., 10.); pin "R5.1" ];
  List.iter (fun (k, _) -> front (Printf.sprintf "LIT%d" k) [ pin (Printf.sprintf "R%d.2" (k + 2)); pin (Printf.sprintf "D%d.2" k) ]) rows;
  (* ground: each pad a short way to a via, and the pours do the rest *)
  let ground from via = front "GND" [ from; via ]; B.via b ~net:"GND" via in
  ground (pin "U1.4") (17.3, 11.6);
  ground (pin "C1.2") (26.825, 17.2);
  ground (pin "SW1.1") (9.4, 6.);
  List.iter (fun (k, y) -> ground (pin (Printf.sprintf "D%d.1" k)) (40.4, y)) rows;
  List.iter (fun at -> B.via b ~net:"GND" at) [ (22., 5.); (30., 25.); (14., 25.); (36., 5.) ];
  B.pour b ~net:"GND" ~side:Circuit.Copper.Front ~edge:0.4 ();
  B.pour b ~net:"GND" ~side:Circuit.Copper.Back ~edge:0.4 ();
  B.solid b

(* [how]: `Whole, `Broken or `Cut. *)
let blink how () =
  let b = B.make (match how with `Whole -> "blink" | `Broken -> "blink-broken" | `Cut -> "blink-cut") (B.rect 30. 20.) in
  B.part b "J1" ~footprint:(F.header 2) (5., 12.);
  B.part b "R1" ~footprint:F.r0603 (14., 12.);
  (* turned round, so its anode (pad 2) faces the resistor *)
  B.part b "D1" ~footprint:F.led0603 ~turn:180. (22., 12.);
  B.net b "VCC" [ "J1.1"; "R1.1" ];
  B.net b "LED" [ "R1.2"; "D1.2" ];
  B.net b "GND" [ "D1.1"; "J1.2" ];
  let pin = B.pin b and via = (26., 12.) in
  B.trace b ~net:"VCC" [ pin "J1.1"; pin "R1.1" ];
  B.trace b ~net:"LED" [ pin "R1.2"; pin "D1.2" ];
  (* out of the cathode to a via, and home on the back: by a trace, or by a pour *)
  B.trace b ~net:"GND" [ pin "D1.1"; via ];
  if how <> `Broken then B.via b ~net:"GND" via;
  (match how with
   | `Whole | `Broken -> B.trace b ~net:"GND" ~side:Circuit.Copper.Back [ via; (26., 6.); (5., 6.); pin "J1.2" ]
   | `Cut ->
       B.pour b ~net:"GND" ~side:Circuit.Copper.Back ();
       (* as near the edge as copper may come (0.2), which is nearer than the pour does (0.3): so it is cut *)
       B.trace b ~net:"VCC" ~side:Circuit.Copper.Back [ pin "J1.1"; (12., 12.); (12., 0.35) ];
       B.trace b ~net:"VCC" ~side:Circuit.Copper.Back [ (12., 12.); (12., 19.65); (2., 19.65); (2., 16.); (0.35, 16.) ]);
  if how = `Broken then B.trace b ~net:"VCC" [ pin "J1.1"; (5., 15.); (21.2, 15.); (21.2, 12.7) ];
  B.solid b

let all = [ ("lights", lights); ("blink", blink `Whole); ("blink-broken", blink `Broken); ("blink-cut", blink `Cut) ]
