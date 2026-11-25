(* The graph compiled once into flat arrays, and the one evaluator everything reads.

   Every node gets an index and every arithmetic step a register. Evaluation walks from
   the root and is lazy: a select computes its guard, then only the branch it takes.

   A cell's pruned graph is not a new graph: it is this program plus a [state], one byte
   per node, saying which selects are already decided in the cell (Octree). A decided
   select goes straight to its branch: its guard is not computed and it is not listed
   among the selects reached. *)

module E = Expr
module G = Graph

type t = {
  nodes : int;
  kind : int array;  (** 0 piece, 1 apply, 2 select *)
  first : int array;  (** the node's operations: registers first .. last *)
  last : int array;  (** the last one is the node's result *)
  inputs : int array array;  (** an apply's inputs; a select's guard, le, gt *)
  lip : float array;
  jump : float array;
  keep : int array;  (** face: >= 0 the input whose face is kept, -1 a fixed or made face, -3 a joined one, -2 none *)
  made : string array;  (** the operation's name, for a made face *)
  fixed : int array;  (** a piece's face *)
  feature : int array;  (** the feature that made the node *)
  root : int;
  code : int array;  (** per register: the operation *)
  a : int array;
  b : int array;
  k : float array;
  mutable names : string array;  (** face names, by face id *)
  ids : (string, int) Hashtbl.t;
  blends : (int * int list, int) Hashtbl.t;
}

let face_id p name =
  match Hashtbl.find_opt p.ids name with
  | Some i -> i
  | None ->
      let i = Array.length p.names in
      p.names <- Array.append p.names [| name |];
      Hashtbl.replace p.ids name i;
      i

let face_name p i = if i < 0 then "?" else p.names.(i)

let o_x = 0 and o_k = 1 and o_copy = 2 and o_hold = 3
let o_unary = function E.Neg -> 4 | Sqrt -> 5 | Square -> 6 | Sin -> 7 | Cos -> 8 | Exp -> 9 | Ln -> 10
let o_binary = function E.Add -> 11 | Sub -> 12 | Mul -> 13 | Div -> 14

let compile (g : G.t) =
  let index = Hashtbl.create 256 in
  let kind = ref [] and first = ref [] and last = ref [] and inputs = ref [] and lip = ref [] and jump = ref [] in
  let keep = ref [] and made = ref [] and fixed = ref [] and feature = ref [] and nodes = ref 0 in
  let code = ref [] and a = ref [] and b = ref [] and k = ref [] and regs = ref 0 in
  let lasts = Hashtbl.create 256 in
  let emit c x y v =
    code := c :: !code;
    a := x :: !a;
    b := y :: !b;
    k := v :: !k;
    incr regs;
    !regs - 1
  in
  let p0 = { nodes = 0; kind = [||]; first = [||]; last = [||]; inputs = [||]; lip = [||]; jump = [||]; keep = [||]; made = [||]; fixed = [||]; feature = [||];
             root = 0; code = [||]; a = [||]; b = [||]; k = [||]; names = [||]; ids = Hashtbl.create 64; blends = Hashtbl.create 64 } in
  (* A formula's operations; a slot reads the result register of the input it stands for. *)
  let formula (e : E.t) ins =
    let memo = Hashtbl.create 16 in
    let rec go (e : E.t) =
      match Hashtbl.find_opt memo e.id with
      | Some r -> r
      | None ->
          let r =
            match e.node with
            | E.X i -> emit o_x i 0 0.
            | K c -> emit o_k 0 0 c (* set once, in the scratch *)
            | Slot i -> Hashtbl.find lasts ins.(i)
            | U (op, x) -> let x = go x in emit (o_unary op) x 0 0.
            | B (op, x, y) -> let x = go x in let y = go y in emit (o_binary op) x y 0.
          in
          Hashtbl.replace memo e.id r;
          r
    in
    let start = !regs in
    let r = go e in
    (* the result must be this node's own last register *)
    if r <> !regs - 1 || !regs = start then ignore (emit o_copy r 0 0.);
    start
  in
  let rec node (g : G.t) =
    match Hashtbl.find_opt index g.id with
    | Some n -> n
    | None ->
        let ins, start, kd, kp, md, fx =
          match g.node with
          | G.Piece p -> ([||], formula p.field [||], 0, -1, "", face_id p0 p.name)
          | Apply ap ->
              let ins = Array.map node ap.inputs in
              let kp, md = match ap.naming with G.Keep i -> (i, "") | Made op -> (-1, op) | Joined op -> (-3, op) | Unnamed -> (-2, "") in
              (ins, formula ap.formula ins, 1, kp, md, -1)
          | Select s ->
              let ins = [| node s.guard; node s.le; node s.gt |] in
              (ins, emit o_hold 0 0 0., 2, -2, "", -1)
        in
        let n = !nodes in
        incr nodes;
        Hashtbl.replace index g.id n;
        Hashtbl.replace lasts n (!regs - 1);
        kind := kd :: !kind; first := start :: !first; last := (!regs - 1) :: !last; inputs := ins :: !inputs;
        lip := g.lip :: !lip; jump := g.jump :: !jump; keep := kp :: !keep; made := md :: !made; fixed := fx :: !fixed; feature := g.feature :: !feature;
        n
  in
  let root = node g in
  let arr l = Array.of_list (List.rev l) in
  { p0 with nodes = !nodes; kind = arr !kind; first = arr !first; last = arr !last; inputs = arr !inputs; lip = arr !lip; jump = arr !jump;
            keep = arr !keep; made = arr !made; fixed = arr !fixed; feature = arr !feature; root; code = arr !code; a = arr !a; b = arr !b; k = arr !k }

let selects p = Array.fold_left (fun n k -> if k = 2 then n + 1 else n) 0 p.kind

(* ---- Evaluation ---- *)

(* Scratch for one evaluation at a time: values and gradients per register, and per node
   whether it was computed at this point ([stamp] = [epoch]) and its face. *)
type ctx = {
  v : float array;
  dx : float array;
  dy : float array;
  dz : float array;
  stamp : int array;
  seen : int array;  (** for walks over the nodes (Octree) *)
  face : int array;
  part : int array;  (** the piece of surface the node's face is of *)
  feat : int array;  (** the feature of the node's face *)
  mutable epoch : int;
  mutable full : bool;  (** gradients, faces and reached selects too; otherwise values only *)
  mutable state : Bytes.t;
  mutable force : int -> int;
  mutable reached : int list;
  mutable count : int;  (** point evaluations, for reports *)
  met : (int * int list, int) Hashtbl.t;  (** the made faces this scratch has met (see [made_face]) *)
}

let unforced _ = 0

let ctx p =
  let r = Array.length p.code in
  { v = Array.copy p.k; dx = Array.make r 0.; dy = Array.make r 0.; dz = Array.make r 0.; stamp = Array.make p.nodes 0; seen = Array.make p.nodes 0; face = Array.make p.nodes (-1); part = Array.make p.nodes (-1); feat = Array.make p.nodes 0;
    epoch = 0; full = false; state = Bytes.empty; force = unforced; reached = []; count = 0; met = Hashtbl.create 64 }

(* No select decided: the whole graph. *)
let whole p = Bytes.make p.nodes '\000'

(* Typed, so the compiler reads floats unboxed and skips the array-kind test. *)
let[@inline] get (a : float array) i = Array.unsafe_get a i
let[@inline] set (a : float array) i (v : float) = Array.unsafe_set a i v
let[@inline] geti (a : int array) i = Array.unsafe_get a i

(* A node's operations, values only. Constants were set when the scratch was made. *)
let run p c x n =
  let v = c.v in
  for i = p.first.(n) to p.last.(n) do
    let a = geti p.a i and b = geti p.b i in
    match geti p.code i with
    | 0 -> set v i x.(a)
    | 2 -> set v i (get v a)
    | 4 -> set v i (-.get v a)
    | 5 -> set v i (sqrt (get v a))
    | 6 -> set v i (get v a *. get v a)
    | 7 -> set v i (sin (get v a))
    | 8 -> set v i (cos (get v a))
    | 9 -> set v i (exp (get v a))
    | 10 -> set v i (log (get v a))
    | 11 -> set v i (get v a +. get v b)
    | 12 -> set v i (get v a -. get v b)
    | 13 -> set v i (get v a *. get v b)
    | 14 -> set v i (get v a /. get v b)
    | _ -> ()
  done

(* The same with gradients: forward differentiation along x, y and z. *)
let run_full p c x n =
  let v = c.v and dx = c.dx and dy = c.dy and dz = c.dz in
  let lin i s a =
    set dx i (s *. get dx a);
    set dy i (s *. get dy a);
    set dz i (s *. get dz a)
  in
  let lin2 i s a t b =
    set dx i ((s *. get dx a) +. (t *. get dx b));
    set dy i ((s *. get dy a) +. (t *. get dy b));
    set dz i ((s *. get dz a) +. (t *. get dz b))
  in
  for i = p.first.(n) to p.last.(n) do
    let a = geti p.a i and b = geti p.b i in
    match geti p.code i with
    | 0 ->
        set v i x.(a);
        set dx i (if a = 0 then 1. else 0.);
        set dy i (if a = 1 then 1. else 0.);
        set dz i (if a = 2 then 1. else 0.)
    | 2 -> set v i (get v a); lin i 1. a
    | 4 -> set v i (-.get v a); lin i (-1.) a
    | 5 -> let s = sqrt (get v a) in set v i s; lin i (0.5 /. s) a
    | 6 -> let u = get v a in set v i (u *. u); lin i (2. *. u) a
    | 7 -> let u = get v a in set v i (sin u); lin i (cos u) a
    | 8 -> let u = get v a in set v i (cos u); lin i (-.sin u) a
    | 9 -> let e = exp (get v a) in set v i e; lin i e a
    | 10 -> let u = get v a in set v i (log u); lin i (1. /. u) a
    | 11 -> set v i (get v a +. get v b); lin2 i 1. a 1. b
    | 12 -> set v i (get v a -. get v b); lin2 i 1. a (-1.) b
    | 13 -> let u = get v a and w = get v b in set v i (u *. w); lin2 i w a u b
    | 14 -> let u = get v a and w = get v b in set v i (u /. w); lin2 i (1. /. w) a (-.u /. (w *. w)) b
    | _ -> ()
  done

(* A made face is named the first time it is met, in the program itself, which several
   cores may be evaluating at once, each with a scratch of its own: so a scratch keeps
   the made faces it has met, and the program's are read and added to by one core at a
   time. *)
let naming = Mutex.create ()

let made_face p c n =
  let ins = Array.to_list (Array.map (fun i -> c.face.(i)) p.inputs.(n)) in
  match Hashtbl.find_opt c.met (n, ins) with
  | Some f -> f
  | None when p.keep.(n) = -3 && List.for_all (( = ) (List.hd ins)) ins -> List.hd ins
  | None ->
      let f =
        Mutex.protect naming (fun () ->
            match Hashtbl.find_opt p.blends (n, ins) with
            | Some f -> f
            | None ->
                let f = face_id p (Printf.sprintf "%s[%s]" p.made.(n) (String.concat "," (List.map (face_name p) ins))) in
                Hashtbl.replace p.blends (n, ins) f;
                f)
      in
      Hashtbl.replace c.met (n, ins) f;
      f

(* The piece of surface at a node, as a number: a piece's own node; through an
   operation, its inputs' pieces with the operation, as a made face is its inputs'
   faces with it. Two pieces can have one face name (the spans of a sketch given one
   name, turned or pulled) and still meet in an edge: a mesh must tell them apart. *)
let made_part p c n =
  let ins = p.inputs.(n) in
  let first = c.part.(ins.(0)) in
  if p.keep.(n) = -3 && Array.for_all (fun i -> c.part.(i) = first) ins then first
  else Array.fold_left (fun h i -> (h * 1000003) lxor c.part.(i)) (p.nodes + n) ins

let rec ev p c x n =
  if geti c.stamp n <> c.epoch then begin
    Array.unsafe_set c.stamp n c.epoch;
    let ins = Array.unsafe_get p.inputs n in
    match geti p.kind n with
    | 0 -> if c.full then (run_full p c x n; c.face.(n) <- p.fixed.(n); c.part.(n) <- n; c.feat.(n) <- p.feature.(n)) else run p c x n
    | 1 ->
        for i = 0 to Array.length ins - 1 do ev p c x (geti ins i) done;
        if c.full then begin
          run_full p c x n;
          let kp = p.keep.(n) in
          c.face.(n) <- (if kp >= 0 then c.face.(ins.(kp)) else if kp = -1 || kp = -3 then made_face p c n else -1);
          c.part.(n) <- (if kp >= 0 then c.part.(ins.(kp)) else if kp = -1 || kp = -3 then made_part p c n else -1);
          c.feat.(n) <- (if kp >= 0 then c.feat.(ins.(kp)) else p.feature.(n))
        end
        else run p c x n
    | _ ->
        let side =
          match Bytes.unsafe_get c.state n with
          | '\001' -> 1
          | '\002' -> 2
          | _ ->
              let g = geti ins 0 in
              ev p c x g;
              if c.full then c.reached <- n :: c.reached;
              let f = c.force n in
              if f <> 0 then f else if get c.v (geti p.last g) <= 0. then 1 else 2
        in
        let br = geti ins side in
        ev p c x br;
        let r = geti p.last n and s = geti p.last br in
        set c.v r (get c.v s);
        if c.full then (c.dx.(r) <- c.dx.(s); c.dy.(r) <- c.dy.(s); c.dz.(r) <- c.dz.(s); c.face.(n) <- c.face.(br); c.part.(n) <- c.part.(br); c.feat.(n) <- c.feat.(br))
  end

(* A new point: forget what was computed. *)
let start p c ~full state =
  ignore p;
  c.epoch <- c.epoch + 1;
  c.full <- full;
  c.state <- state;
  c.reached <- [];
  c.count <- c.count + 1

(* The value only. [force s] = 1 takes the le side of select s, 2 the gt side, 0 leaves
   it to its guard. *)
let value ?(force = unforced) p c state x =
  start p c ~full:false state;
  c.force <- force;
  ev p c x p.root;
  c.force <- unforced;
  c.v.(p.last.(p.root))

type dual = { v : float; g : float array }

(* The field at a point: its value and gradient, the face active there (and which piece
   of surface it is of, for when two of a name meet), and every
   undecided select on the way with its guard's value and gradient. *)
type sample = { d : dual; face : int; part : int; feature : int; reached : (int * dual) list }

let dual (c : ctx) r = { v = c.v.(r); g = [| c.dx.(r); c.dy.(r); c.dz.(r) |] }

let eval ?(force = unforced) p c state x =
  start p c ~full:true state;
  c.force <- force;
  ev p c x p.root;
  c.force <- unforced;
  { d = dual c p.last.(p.root); face = c.face.(p.root); part = c.part.(p.root); feature = c.feat.(p.root); reached = List.map (fun s -> (s, dual c p.last.(p.inputs.(s).(0)))) c.reached }
