(* A board's copper held against what its nets say.

   - Two pieces of different nets that are into each other are a short; nearer than
     the board's clearance, too near. How far apart two pieces are is worked out from
     their shapes (Circuit_copper.apart), exactly; or, where one is an outline of its
     own, searched for in the plane from the two fields (Circuit_copper.near).
   - Copper nearer the board's edge, or an opening in it, than the board allows: the
     least of the board's own field over the piece, searched for the same way.
   - A net's copper must be one piece: its pads, traces and vias are joined where they
     lie over each other on a side they share, and a net left in two groups or more
     is open. A via and a pad with a hole are on both sides, and join them. A pour
     joins whatever of its net lies on one island of it.

   Every pair is asked that shares a layer and whose boxes are near enough, found by
   a grid of the board and not by trying every piece with every other. *)
open Circuit_copper

type finding =
  | Short of piece * piece * float * float array  (** how far into each other, and where *)
  | Near of piece * piece * float * float array  (** how far apart, and where *)
  | Edge of piece * float * float array  (** how far from the board's edge (below zero: past it), and where *)
  | Open of string * string list list  (** a net, and what is in each of its groups *)

(* [fields]: every pair asked from the two fields, none from its numbers: slower, and
   what the numbers are held against. *)
let check ?(fields = false) (b : Circuit_board.t) =
  let pieces = Array.of_list (Circuit_board.pieces b) in
  let n = Array.length pieces in
  let cores = Array.map (fun (p : piece) -> core p.shape) pieces in
  let readers = Array.map (fun (p : piece) -> lazy (reader (field p))) pieces in
  let group = Array.init n Fun.id in
  let rec of_ i = if group.(i) = i then i else (let r = of_ group.(i) in group.(i) <- r; r) in
  let found = ref [] in
  let pair i j =
    let p = pieces.(i) and q = pieces.(j) in
    let same = p.net <> "" && p.net = q.net in
    let limit = if same then 0.02 else b.clearance in
    let boxes = Float.max (Float.max (fst q.lo -. fst p.hi) (fst p.lo -. fst q.hi)) (Float.max (snd q.lo -. snd p.hi) (snd p.lo -. snd q.hi)) in
    (* two of one net that are joined already need not be asked again *)
    if shares p q && boxes <= limit && (not (p.bare && q.bare)) && not (same && of_ i = of_ j) then begin
      (* from the two shapes' numbers, or from their fields where one has none *)
      let asked = match (cores.(i), cores.(j)) with Some a, Some c when not fields -> Some (apart a c) | _ -> near ~limit p (Lazy.force readers.(i)) q (Lazy.force readers.(j)) in
      match asked with
      | Some (d, at) -> if same then (if d < 0. then group.(of_ i) <- of_ j) else if d < limit then found := (if d < 0. then Short (p, q, -.d, at) else Near (p, q, d, at)) :: !found
      | None -> ()
    end
  in
  (* The pieces by the squares of a grid their boxes reach, a little wider: two that
     share no square are too far apart to matter. A pair is asked in the first square
     they share. *)
  let square = 1. in
  let squares = Hashtbl.create (4 * n) in
  let reach (p : piece) = let m = b.clearance in (int_of_float (Float.floor ((fst p.lo -. m) /. square)), int_of_float (Float.floor ((snd p.lo -. m) /. square)), int_of_float (Float.floor ((fst p.hi +. m) /. square)), int_of_float (Float.floor ((snd p.hi +. m) /. square))) in
  let reaches = Array.map reach pieces in
  Array.iteri (fun i (i0, j0, i1, j1) -> for a = i0 to i1 do for c = j0 to j1 do Hashtbl.replace squares (a, c) (i :: Option.value ~default:[] (Hashtbl.find_opt squares (a, c))) done done) reaches;
  Hashtbl.iter
    (fun (a, c) inside ->
      let inside = Array.of_list inside in
      Array.iteri
        (fun k i ->
          let i0, j0, _, _ = reaches.(i) in
          for l = k + 1 to Array.length inside - 1 do
            let j = inside.(l) in
            let k0, l0, _, _ = reaches.(j) in
            if a = max i0 k0 && c = max j0 l0 then pair (min i j) (max i j)
          done)
        inside)
    squares;
  (* The board's edge. A piece whose box is all further inside than the rule asks is
     passed by its middle alone; the rest are gone over. *)
  let inside = Circuit_board.inside b in
  Array.iteri
    (fun i (p : piece) ->
      if not p.bare then begin
        let (lx, ly), (hx, hy) = (p.lo, p.hi) in
        let reach = 0.5 *. sqrt (((hx -. lx) ** 2.) +. ((hy -. ly) ** 2.)) in
        if inside (0.5 *. (lx +. hx), 0.5 *. (ly +. hy)) -. reach < b.edge then
          match least ~below:b.edge ~lo:p.lo ~hi:p.hi (Lazy.force readers.(i)) inside with Some (d, at) -> found := Edge (p, d, at) :: !found | None -> ()
      end)
    pieces;
  (* a pour joins what of its net lies on one island of it *)
  let pours = Circuit_board.poured b in
  List.iter
    (fun (pour : Circuit_pour.t) ->
      let first = Hashtbl.create 4 in
      List.iter
        (fun (island, what) ->
          Array.iteri (fun i (p : piece) -> if p.what = what && p.net = pour.net then match Hashtbl.find_opt first island with Some j -> group.(of_ i) <- of_ j | None -> Hashtbl.add first island i) pieces)
        pour.touching)
    pours;
  (* each net's groups *)
  let nets = List.sort_uniq compare (List.filter (( <> ) "") (Array.to_list (Array.map (fun p -> p.net) pieces))) in
  let opens =
    List.filter_map
      (fun net ->
        let groups = Hashtbl.create 4 in
        Array.iteri (fun i p -> if p.net = net then Hashtbl.replace groups (of_ i) (p.what :: Option.value ~default:[] (Hashtbl.find_opt groups (of_ i)))) pieces;
        if Hashtbl.length groups > 1 then Some (Open (net, List.sort compare (Hashtbl.fold (fun _ l acc -> List.rev l :: acc) groups []))) else None)
      nets
  in
  (n, nets, pours, List.rev !found @ opens)

(* What was found, as text for whoever asked. *)
let report ?(all = false) ?fields (b : Circuit_board.t) =
  let n, nets, pours, found = check ?fields b in
  let at (x : float array) = Printf.sprintf "(%.3f, %.3f)" x.(0) x.(1) in
  (* a group said by its pins, and how much else is in it *)
  let said group =
    let pins, rest = List.partition (fun w -> String.contains w '.') group in
    (* many pins are said by the first few and how many more *)
    let few = if all || List.length pins <= 6 then pins else List.filteri (fun i _ -> i < 4) pins @ [ Printf.sprintf "and %d more pins" (List.length pins - 4) ] in
    String.concat ", " few ^ (if rest = [] then "" else Printf.sprintf "%s%d of trace or via" (if pins = [] then "" else " with ") (List.length rest))
  in
  let line = function
    | Short (p, q, d, x) -> Printf.sprintf "SHORT: %s (%s) and %s (%s) are %.3g into each other at %s." p.what (if p.net = "" then "no net" else p.net) q.what (if q.net = "" then "no net" else q.net) d (at x)
    | Near (p, q, d, x) -> Printf.sprintf "TOO NEAR: %s (%s) and %s (%s) are %.3g apart at %s; the least is %g." p.what (if p.net = "" then "no net" else p.net) q.what (if q.net = "" then "no net" else q.net) d (at x) b.clearance
    | Edge (p, d, x) ->
        if d < 0. then Printf.sprintf "OVER THE EDGE: %s (%s) is %.3g past the board's edge at %s." p.what (if p.net = "" then "no net" else p.net) (-.d) (at x)
        else Printf.sprintf "TOO NEAR THE EDGE: %s (%s) is %.3g from the board's edge at %s; the least is %g." p.what (if p.net = "" then "no net" else p.net) d (at x) b.edge
    | Open (net, groups) -> Printf.sprintf "OPEN: %s is in %d pieces: %s." net (List.length groups) (String.concat " | " (List.map said groups))
  in
  String.concat "\n"
    (Printf.sprintf "%s: %d parts, %d pieces of copper, %d nets%s; the least between two nets is %g, and to the edge %g." b.name (List.length b.parts) n (List.length nets)
       (if all || List.length nets <= 24 then Printf.sprintf " (%s)" (String.concat ", " nets) else "")
       b.clearance b.edge
    :: List.map
         (fun (pour : Circuit_pour.t) ->
           let kept, all = Circuit_pour.islands pour in
           Printf.sprintf "%s is poured over the %s: %d island%s on the net%s." pour.net (Circuit_copper.said pour.side) kept (if kept = 1 then "" else "s")
             (if all > kept then Printf.sprintf ", %d that reached none of it dropped" (all - kept) else ""))
         pours
    @
    if found = [] then [ "Every net is in one piece, and no two are nearer than that." ]
    else
      (* much found is said by how many of each, and the first of each *)
      let kinds = [ ("shorts", function Short _ -> true | _ -> false); ("places too near", function Near _ -> true | _ -> false); ("pieces too near the edge", function Edge _ -> true | _ -> false); ("nets in pieces", function Open _ -> true | _ -> false) ] in
      if all || List.length found <= 40 then List.map line found
      else
        List.concat_map
          (fun (what, is) ->
            let these = List.filter is found in
            if these = [] then [] else Printf.sprintf "%d %s; the first of them:" (List.length these) what :: List.map line (List.filteri (fun i _ -> i < 12) these))
          kinds)
