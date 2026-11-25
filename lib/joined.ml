(* The bodies of a model: what its plain unions join.

   The root of a model is often a union of unions: the parts of an assembly, a plate and
   the bosses on it. What those unions join are the model's bodies. A body taken out of
   the model changes nothing else in it: the union above it has one side fewer. So the
   page need not ask for a new model to take one out or put it back. The shaders it draws
   with (Shader, Region) read each body through a switch, where a union takes it, and the
   page sets the switches.

   That holds a decision of the octree to more than it promises elsewhere. A union is
   decided in a cell where one side is below the other all over it, and the other side is
   then left out of what the cell needs. With the first side switched off, the other is
   the model there. So where bodies are switched, a union between them gives a side up
   only where that side is also outside all over the cell (Octree.prune, [joins]): then
   it has no surface there whatever is switched off, and nothing is lost.

   What a switch cannot do is the kernel's still: a feature taken out from inside a body
   (a cut that no longer cuts) is a different body, and a new model ([plain]).

   A feature shown alone is the switches' to do as well, where its solid is a union of
   the model's own bodies ([alone]): every other body is switched off. *)

module P = Prog

type t = {
  node : int;  (** its node in the program *)
  over : int list list;  (** each way down to it from the root: the features of the unions on the way, and its own *)
  inside : int list;  (** the features with a node in it *)
}

type found = {
  bodies : t array;
  of_node : int array;  (** a node's body, or -1 *)
  joins : Bytes.t;  (** per node: whether it is one of the unions that join the bodies *)
}

(* What a body switched off is to the union above it: a field far outside everything. *)
let far = 1e4

(* No more bodies than the page has switches for (web/view.html, Uniforms.off). *)
let most = 1024

(* The two sides of a node that is a plain union (Graph.union): a select between them
   whose guard is the one less the other, and nothing else. *)
let union (p : P.t) n =
  if p.kind.(n) <> 2 then None
  else
    let g = p.inputs.(n).(0) and le = p.inputs.(n).(1) and gt = p.inputs.(n).(2) in
    let r = p.last.(g) in
    if p.kind.(g) = 1 && p.inputs.(g) = [| le; gt |] && p.first.(g) = r && p.code.(r) = 12 && p.a.(r) = p.last.(le) && p.b.(r) = p.last.(gt) then Some (le, gt) else None

(* The model's bodies. The unions are opened from the root down, a level at a time, as
   far as [most] bodies allow; a union left shut is a body whole. A model that is no
   union is one body. *)
let find (p : P.t) =
  let joins = Bytes.make p.nodes '\000' and of_node = Array.make p.nodes (-1) and nodes = ref [] and count = ref 0 in
  let waiting = Queue.create () in
  Queue.add p.root waiting;
  while not (Queue.is_empty waiting) do
    let n = Queue.pop waiting in
    if Bytes.get joins n = '\000' && of_node.(n) < 0 then
      match union p n with
      (* opened, it is one body more than shut *)
      | Some (le, gt) when !count + Queue.length waiting + 2 <= most ->
          Bytes.set joins n '\001';
          Queue.add le waiting;
          Queue.add gt waiting
      | _ ->
          of_node.(n) <- !count;
          incr count;
          nodes := n :: !nodes
  done;
  (* each way down to a body: the features met, none twice. A way is walked once however
     many unions share it. *)
  let over = Array.make !count [] and walked = Hashtbl.create 64 in
  let rec down n met =
    let met = if p.feature.(n) = 0 then met else List.sort_uniq compare (p.feature.(n) :: met) in
    if not (Hashtbl.mem walked (n, met)) then begin
      Hashtbl.replace walked (n, met) ();
      if of_node.(n) >= 0 then over.(of_node.(n)) <- met :: over.(of_node.(n)) else (down p.inputs.(n).(1) met; down p.inputs.(n).(2) met)
    end
  in
  down p.root [];
  let inside n =
    let seen = Hashtbl.create 64 and features = Hashtbl.create 8 in
    let rec go n =
      if not (Hashtbl.mem seen n) then begin
        Hashtbl.replace seen n ();
        if p.feature.(n) <> 0 then Hashtbl.replace features p.feature.(n) ();
        Array.iter go p.inputs.(n)
      end
    in
    go n;
    List.sort compare (Hashtbl.fold (fun f () acc -> f :: acc) features [])
  in
  { bodies = Array.of_list (List.rev_map (fun n -> { node = n; over = List.rev over.(of_node.(n)); inside = inside n }) !nodes); of_node; joins }

(* Whether a node is one of the unions that join the bodies. *)
let joins (f : found) n = Bytes.unsafe_get f.joins n <> '\000'

(* Whether taking out the features [gone] says is the switches' to do: every body is
   then either out whole, each way down to it passing a feature that is gone, or holds
   nothing that is gone. *)
let out gone (b : t) = List.for_all (List.exists gone) b.over
let plain (f : found) gone = Array.for_all (fun b -> out gone b || not (List.exists gone b.inside)) f.bodies

(* The bodies a graph of the model is made of, each with the ways down to it from the
   graph's root (as [over] has them from the model's): where the graph is a union, or a
   union of unions, of what the model's own unions join, as a part of an assembly is, or
   a group of parts. None where it is anything else: a body's cut, a solid the model
   uses only changed. *)
let alone (p : P.t) (f : found) (g : Graph.t) =
  let ways = Hashtbl.create 16 in
  let met_at feature met = if feature = 0 then met else List.sort_uniq compare (feature :: met) in
  (* through the model's own unions, to its bodies *)
  let rec within n met =
    let met = met_at p.feature.(n) met in
    match f.of_node.(n) with
    | -1 -> if joins f n then (within p.inputs.(n).(1) met; within p.inputs.(n).(2) met) else raise Exit
    | b -> if not (List.mem met (Option.value ~default:[] (Hashtbl.find_opt ways b))) then Hashtbl.replace ways b (met :: Option.value ~default:[] (Hashtbl.find_opt ways b))
  in
  (* through unions made for the graph alone, to nodes the model has *)
  let rec down (g : Graph.t) met =
    match (Hashtbl.find_opt p.index g.id, g.node) with
    | Some n, _ -> within n met
    | None, Graph.Select { guard = { node = Apply { formula = { node = Expr.B (Expr.Sub, { node = Expr.Slot 0; _ }, { node = Expr.Slot 1; _ }); _ }; inputs = [| a; b |]; _ }; _ }; le; gt; _ } when a == le && b == gt ->
        let met = met_at g.feature met in
        down le met;
        down gt met
    | None, _ -> raise Exit
  in
  match down g [] with () -> Some (List.sort compare (Hashtbl.fold (fun b met acc -> (b, List.rev met) :: acc) ways [])) | exception Exit -> None

(* A body's switch, as the shaders read it: a bit of the uniforms' words. *)
let switch b = Printf.sprintf "(u.off[%d].%c & %uu) != 0u" (b / 128) "xyzw".[b mod 128 / 32] (1 lsl (b mod 32))
