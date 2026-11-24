(* Smooth formulas over the coordinates x, y, z. No min, max, abs or comparison: a choice
   between formulas is a select in the graph (Graph), never hidden in arithmetic.

   A [Slot] is a formula's input, not a coordinate: slot 0, slot 1, ... stand for the
   values of the graph nodes an [Apply] is applied to. *)

type unary = Neg | Sqrt | Square | Sin | Cos | Exp | Ln
type binary = Add | Sub | Mul | Div

type t = { id : int; node : node }
and node = X of int | K of float | U of unary * t | B of binary * t * t | Slot of int

let counter = ref 0

let mk node =
  incr counter;
  { id = !counter; node }

let x = mk (X 0) and y = mk (X 1) and z = mk (X 2)
let const c = mk (K c)
let slot i = mk (Slot i)
(* Constructors fold constants and drop arithmetic with 0 and 1, so a plane written as
   a x + b y + c z - d costs only the terms it has. *)
let apply_unary op v =
  match op with Neg -> -.v | Sqrt -> Float.sqrt v | Square -> v *. v | Sin -> Float.sin v | Cos -> Float.cos v | Exp -> Float.exp v | Ln -> Float.log v

let rec unary op a =
  match (op, a.node) with
  | _, K v -> const (apply_unary op v)
  | Neg, U (Neg, e) -> e
  | _ -> mk (U (op, a))

and binary op a b =
  match (op, a.node, b.node) with
  | Add, K u, K v -> const (u +. v)
  | Sub, K u, K v -> const (u -. v)
  | Mul, K u, K v -> const (u *. v)
  | Div, K u, K v -> const (u /. v)
  | Add, K 0., _ -> b
  | (Add | Sub), _, K 0. -> a
  | Sub, K 0., _ -> unary Neg b
  | Mul, K 0., _ | Mul, _, K 0. -> const 0.
  | Mul, K 1., _ -> b
  | (Mul | Div), _, K 1. -> a
  | Mul, K -1., _ -> unary Neg b
  | Mul, _, K -1. -> unary Neg a
  | _ -> mk (B (op, a, b))

let neg = unary Neg and sqrt = unary Sqrt and square = unary Square

(* e to a whole power, n >= 0, by squaring: no other power, so no slope that is infinite
   at 0 (a power below 1) and nothing undefined below 0. *)
let rec pow e n =
  if n < 0 then failwith "Expr.pow: a whole power, 0 or more"
  else if n = 0 then const 1.
  else if n = 1 then e
  else let h = pow e (n / 2) in let h2 = binary Mul h h in if n mod 2 = 0 then h2 else binary Mul h2 e
let sin = unary Sin and cos = unary Cos
let add = binary Add and sub = binary Sub and mul = binary Mul and div = binary Div

(* The formula with each coordinate replaced by a formula: for placing shapes in frames. *)
let subst coords e =
  let memo = Hashtbl.create 64 in
  let rec go e =
    match Hashtbl.find_opt memo e.id with
    | Some r -> r
    | None ->
        let r = match e.node with X i -> coords.(i) | K _ | Slot _ -> e | U (op, a) -> unary op (go a) | B (op, a, b) -> binary op (go a) (go b) in
        Hashtbl.replace memo e.id r;
        r
  in
  go e

module Infix = struct
  let ( + ) = add and ( - ) = sub and ( * ) = mul and ( / ) = div
  let c = const
end
