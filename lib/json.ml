(* JSON, read and written: for talking to programs that are not ours (the page,
   an agent's tool calls). Numbers are floats; a \u escape is read as UTF-8. *)

type t = Null | Bool of bool | Num of float | Str of string | Arr of t list | Obj of (string * t) list

let parse text =
  let n = String.length text and i = ref 0 in
  let fail what = failwith (Printf.sprintf "JSON: %s at %d" what !i) in
  let peek () = if !i < n then text.[!i] else '\000' in
  let rec skip () = if !i < n && (text.[!i] = ' ' || text.[!i] = '\n' || text.[!i] = '\r' || text.[!i] = '\t') then (incr i; skip ()) in
  let word w v = if !i + String.length w <= n && String.sub text !i (String.length w) = w then (i := !i + String.length w; v) else fail "unexpected word" in
  let string () =
    incr i;
    let b = Buffer.create 16 in
    let rec go () =
      if !i >= n then fail "unfinished string"
      else
        match text.[!i] with
        | '"' -> incr i
        | '\\' ->
            (match text.[!i + 1] with
            | 'n' -> Buffer.add_char b '\n'
            | 't' -> Buffer.add_char b '\t'
            | 'r' -> Buffer.add_char b '\r'
            | 'b' -> Buffer.add_char b '\b'
            | 'f' -> Buffer.add_char b '\012'
            | 'u' ->
                Buffer.add_utf_8_uchar b (Uchar.of_int (int_of_string ("0x" ^ String.sub text (!i + 2) 4)));
                i := !i + 4
            | c -> Buffer.add_char b c);
            i := !i + 2;
            go ()
        | c -> Buffer.add_char b c; incr i; go ()
    in
    go ();
    Buffer.contents b
  in
  let rec value () =
    skip ();
    match peek () with
    | '{' ->
        incr i;
        skip ();
        if peek () = '}' then (incr i; Obj [])
        else
          let rec go acc =
            skip ();
            if peek () <> '"' then fail "a key was expected";
            let k = string () in
            skip ();
            if peek () <> ':' then fail "':' was expected";
            incr i;
            let v = value () in
            skip ();
            if peek () = ',' then (incr i; go ((k, v) :: acc)) else if peek () = '}' then (incr i; Obj (List.rev ((k, v) :: acc))) else fail "',' or '}' was expected"
          in
          go []
    | '[' ->
        incr i;
        skip ();
        if peek () = ']' then (incr i; Arr [])
        else
          let rec go acc =
            let v = value () in
            skip ();
            if peek () = ',' then (incr i; go (v :: acc)) else if peek () = ']' then (incr i; Arr (List.rev (v :: acc))) else fail "',' or ']' was expected"
          in
          go []
    | '"' -> Str (string ())
    | 't' -> word "true" (Bool true)
    | 'f' -> word "false" (Bool false)
    | 'n' -> word "null" Null
    | _ ->
        let s = !i in
        while !i < n && (match text.[!i] with '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true | _ -> false) do incr i done;
        if !i = s then fail "a value was expected" else Num (float_of_string (String.sub text s (!i - s)))
  in
  value ()

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 32 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Num v -> if Float.is_integer v && Float.abs v < 1e15 then Printf.sprintf "%.0f" v else if Float.is_finite v then Printf.sprintf "%.9g" v else "null"
  | Str s -> quote s
  | Arr l -> "[" ^ String.concat "," (List.map to_string l) ^ "]"
  | Obj l -> "{" ^ String.concat "," (List.map (fun (k, v) -> quote k ^ ":" ^ to_string v) l) ^ "}"

(* Reading: a missing member is Null, and a value of another kind gives the default. *)
let member k = function Obj l -> ( match List.assoc_opt k l with Some v -> v | None -> Null) | _ -> Null
let string ?(default = "") = function Str s -> s | _ -> default
let number ?(default = 0.) = function Num v -> v | _ -> default
let bool ?(default = false) = function Bool b -> b | _ -> default
let list = function Arr l -> l | _ -> []
