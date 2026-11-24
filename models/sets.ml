(* Every model set: its name, domain, models with mk36's counts (corners, edges, closed
   edges) and times (ten cores, one core), and how to build one. *)
type set = { name : string; lo : float array; hi : float array; expected : (string * (int * int * int) * (float * float)) list; graph : string -> Mk52.Graph.t }

let all =
  [ { name = "corners"; lo = Corners.lo; hi = Corners.hi; expected = Corners.expected; graph = Corners.graph };
    { name = "engine"; lo = Engine.lo; hi = Engine.hi; expected = Engine.expected; graph = Engine.graph } ]

(* The fitted skins' solids, at their middle cell count: to look at. *)
let skins = List.map (fun (s : Skins.skin) -> ("skin-" ^ s.name, s)) Skins.all

(* Any model by name: its domain and its graph. *)
let find name =
  match (List.assoc_opt name skins, List.find_opt (fun (c : Tricky.case) -> c.name = name) Tricky.cases) with
  | Some s, _ -> (s.lo, s.hi, fun () -> Skins.solid s (List.nth s.cells 1))
  | None, Some c -> (c.lo, c.hi, c.graph)
  | None, None when name = "arm" -> (Arm.lo, Arm.hi, Arm.graph)
  | None, None when name = "bracket" -> (Bracket.lo, Bracket.hi, Bracket.graph)
  | None, None ->
      let set = List.find (fun s -> List.exists (fun (n, _, _) -> n = name) s.expected) all in
      (set.lo, set.hi, fun () -> set.graph name)

let names = List.concat_map (fun s -> List.map (fun (n, _, _) -> n) s.expected) all @ List.map fst skins @ [ "arm"; "bracket" ] @ List.map (fun (c : Tricky.case) -> c.name) Tricky.cases
