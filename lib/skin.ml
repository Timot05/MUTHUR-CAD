(* Skins: where a solid has a face of no thickness, with air on both sides of it.

   Two faces that lie on one surface leave one: a cut that ends level with another face,
   a pocket whose floor is the top of a hollow, a profile turned about an axis it is
   closed along. The field touches zero there and does not change sign, so nothing is
   really there, and a cut or a mesh does not see it; but it is where the two faces
   were meant to pass one another and did not, and where the model's code should have
   one of them go a little further.

   They are looked for in every last cell of the octree that may hold surface: from the
   cell's middle onto where the field is nothing, along its slope, and from there a
   little way both ways along the slope. A surface has the solid on one side. A skin
   has air on both; so has a wall thinner than twice that little way, which is as much
   a mistake. What is found is gathered by the two faces it lies between. *)

type found = {
  between : string * string;  (** the faces answering either side of it *)
  cells : int;  (** how many cells of the octree it was found in *)
  at : float array;  (** one place on it *)
  lo : float array;
  hi : float array;  (** the box of the places it was found *)
}

(* The skins of a model within lo..hi, the largest first. [depth]: the octree's levels,
   7 unless said: 128 cells across the longest side. *)
let find ?(depth = 7) ~lo ~hi (g : Graph.t) =
  let p = Prog.compile g in
  let c = Prog.ctx p in
  let groups : (string * string, found) Hashtbl.t = Hashtbl.create 16 in
  let x = Array.make 3 0. and y = Array.make 3 0. in
  Octree.shell p c ~lo ~hi ~depth (fun l size state ->
      for i = 0 to 2 do x.(i) <- l.(i) +. (0.5 *. size) done;
      let s = Prog.eval p c state x in
      let g2 = (s.d.g.(0) *. s.d.g.(0)) +. (s.d.g.(1) *. s.d.g.(1)) +. (s.d.g.(2) *. s.d.g.(2)) in
      (* onto the zero, if it is in reach of the cell's own decisions *)
      if g2 > 1e-12 && Float.abs s.d.v <= size *. sqrt g2 then begin
        let n = Array.map (fun k -> k /. sqrt g2) s.d.g in
        for i = 0 to 2 do x.(i) <- x.(i) -. (s.d.v /. sqrt g2 *. n.(i)) done;
        let little = 0.05 *. size in
        let side k = for i = 0 to 2 do y.(i) <- x.(i) +. (k *. little *. n.(i)) done; Prog.eval p c state y in
        let a = side 1. and b = side (-1.) in
        if a.d.v > 0.25 *. little && b.d.v > 0.25 *. little && Float.abs (Prog.value p c state x) < 0.25 *. little then begin
          let fa = Prog.face_name p a.face and fb = Prog.face_name p b.face in
          let between = if fa <= fb then (fa, fb) else (fb, fa) in
          Hashtbl.replace groups between
            (match Hashtbl.find_opt groups between with
             | Some f -> { f with cells = f.cells + 1; lo = Array.map2 Float.min f.lo x; hi = Array.map2 Float.max f.hi x }
             | None -> { between; cells = 1; at = Array.copy x; lo = Array.copy x; hi = Array.copy x })
        end
      end);
  List.sort (fun a b -> compare b.cells a.cells) (Hashtbl.fold (fun _ f acc -> f :: acc) groups [])
