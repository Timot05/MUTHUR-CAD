(* A colour, as a model's code says it and as the page takes it: "#rrggbb" in the code,
   red, green and blue from 0 to 1 after. *)
type t = float * float * float

let of_string s : t =
  let part i = int_of_string_opt ("0x" ^ String.sub s i 2) in
  match if String.length s = 7 && s.[0] = '#' then (part 1, part 3, part 5) else (None, None, None) with
  | Some r, Some g, Some b -> (float r /. 255., float g /. 255., float b /. 255.)
  | _ -> failwith (Printf.sprintf "%S is not a colour: one is written #rrggbb, as #d9d4c7" s)
