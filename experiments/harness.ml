(* What every experiment shares: checks with a limit fixed beforehand, a field to
   sample, sample points that are the same on every run, and the common metrics.

   An experiment is one file with one question, a small shape whose answer is known, and
   a list of checks. `dune exec experiments/run.exe` runs them all and prints a table. *)
open Mk52

type check = { what : string; measured : float; limit : float; pass : bool; note : string }
type t = { id : int; question : string; run : unit -> check list }

(* Passes when [measured] is at most [limit]. *)
let at_most ?(note = "") what measured limit = { what; measured; limit; pass = measured <= limit; note }

(* Passes when [measured] is above [limit]: for checks that a fault is caught. *)
let above ?(note = "") what measured limit = { what; measured; limit; pass = measured > limit; note }

(* ---- A field to sample ---- *)

type field = { value : float array -> float; dual : float array -> Prog.dual; face : float array -> string; lip : float; jump : float }

let field (g : Graph.t) =
  let p = Prog.compile g in
  let c = Prog.ctx p and whole = Prog.whole p in
  { value = Prog.value p c whole; dual = (fun x -> (Prog.eval p c whole x).d); face = (fun x -> Prog.face_name p (Prog.eval p c whole x).face); lip = g.lip; jump = g.jump }

(* ---- Sample points: the same on every run ---- *)

let points ?(seed = 49) ~lo ~hi n =
  let r = Random.State.make [| seed |] in
  Array.init n (fun _ -> Array.init 3 (fun a -> lo.(a) +. Random.State.float r (hi.(a) -. lo.(a))))

let norm g = Float.sqrt ((g.(0) *. g.(0)) +. (g.(1) *. g.(1)) +. (g.(2) *. g.(2)))
let worst f pts = Array.fold_left (fun m x -> let v = f x in if Float.is_nan v then m else Float.max m v) 0. pts

(* ---- Metrics ---- *)

(* Points where the field's sign disagrees with [inside] (true inside). Points within
   [skin] of the exact surface are left out when [exact] is given: there the sign is down
   to rounding. *)
let wrong_signs f ~inside pts = Array.fold_left (fun n x -> if f.value x <= 0. <> inside x then n + 1 else n) 0 pts |> float_of_int

(* The largest | |grad f| - 1 | among points within [band] of the surface. *)
let slope_error f ~band pts = worst (fun x -> let d = f.dual x in if Float.abs d.v <= band then Float.abs (norm d.g -. 1.) else 0.) pts

(* The largest sampled |grad f|, to hold against the declared slope bound. *)
let steepest f pts = worst (fun x -> norm (f.dual x).g) pts

(* The largest |f (p) - f (q)| - jump over |p - q| for pairs [step] apart in a fixed
   random direction. Unlike the gradient, this sees a value that steps across a guard. *)
let steepest_step f ~step pts =
  let r = Random.State.make [| 7 |] in
  worst
    (fun x ->
      let d = Array.init 3 (fun _ -> Random.State.float r 2. -. 1.) in
      let l = norm d in
      let y = Array.mapi (fun a v -> v +. (step *. d.(a) /. l)) x in
      (Float.abs (f.value x -. f.value y) -. f.jump) /. step)
    pts

(* The largest |f (p) - f (q)| beyond what the slope bound allows, for pairs [step] apart:
   the largest step the field takes across a guard. *)
let largest_jump f ~step pts =
  let r = Random.State.make [| 7 |] in
  worst
    (fun x ->
      let d = Array.init 3 (fun _ -> Random.State.float r 2. -. 1.) in
      let l = norm d in
      let y = Array.mapi (fun a v -> v +. (step *. d.(a) /. l)) x in
      Float.abs (f.value x -. f.value y) -. (f.lip *. step))
    pts

(* The largest |f - exact|, among points within [band] of the exact surface if given. *)
let value_error ?band f ~exact pts =
  worst (fun x -> let e = exact x in match band with Some b when Float.abs e > b -> 0. | _ -> Float.abs (f.value x -. e)) pts

(* Newton from x onto the field's surface, if it gets there. *)
let on_surface f x0 =
  let rec go x k =
    if k = 0 then None
    else
      let d = f.dual x in
      let n2 = (d.g.(0) *. d.g.(0)) +. (d.g.(1) *. d.g.(1)) +. (d.g.(2) *. d.g.(2)) in
      if n2 = 0. || not (Float.is_finite n2) then None
      else
        let x' = Array.mapi (fun a v -> v -. (d.v *. d.g.(a) /. n2)) x in
        if Float.abs d.v < 1e-12 *. (1. +. norm x) then Some x' else go x' (k - 1)
  in
  go x0 60

(* Points on the surface: each sample moved onto it, those inside the box kept. *)
let surface_points f ~lo ~hi pts =
  let inside x = Array.for_all Fun.id (Array.mapi (fun a v -> v >= lo.(a) && v <= hi.(a)) x) in
  Array.of_list (List.filter_map (fun x -> match on_surface f x with Some y when inside y -> Some y | _ -> None) (Array.to_list pts))

(* How far an extraction is from the counts expected: corners, edges, closed edges, flags. *)
let topology_off ?(depth = 6) ~lo ~hi g (v, e, closed) =
  let t = Extract.extract ~depth ~lo ~hi g in
  let nc = List.length (List.filter (fun (x : Extract.edge) -> x.closed) t.edges) in
  (float_of_int (abs (List.length t.vertices - v) + abs (List.length t.edges - e) + abs (nc - closed) + List.length t.flags), t)

(* ---- The table ---- *)

let print_all experiments =
  let failed = ref 0 in
  List.iter
    (fun e ->
      let checks = e.run () in
      let bad = List.length (List.filter (fun c -> not c.pass) checks) in
      Printf.printf "\n%d. %s  [%s]\n" e.id e.question (if bad = 0 then Printf.sprintf "all %d checks pass" (List.length checks) else Printf.sprintf "%d of %d checks FAIL" bad (List.length checks));
      Printf.printf "   %-74s %12s %12s\n" "check" "measured" "limit";
      List.iter
        (fun c ->
          if (not c.pass) || Sys.getenv_opt "ALL" <> None then
            Printf.printf "   %-74s %12.4g %12.4g  %s%s\n" c.what c.measured c.limit (if c.pass then "ok" else "FAIL") (if c.note = "" then "" else "  (" ^ c.note ^ ")"))
        checks;
      if bad = 0 && Sys.getenv_opt "ALL" = None then Printf.printf "   (ALL=1 lists every check)\n";
      failed := !failed + bad)
    experiments;
  Printf.printf "\n%s\n%!" (if !failed = 0 then "Every check passes." else Printf.sprintf "%d checks fail." !failed);
  !failed
