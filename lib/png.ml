(* A picture written as a PNG file, with nothing but the standard library: the bytes of
   each row less the pixel to their left (so flat and shaded areas become runs of
   nothing), then DEFLATE with the fixed code and matches found through one table of
   where each three bytes were last seen. Small enough for an agent to be sent, at a
   fifth to a tenth of the raw picture. *)

let crc_table = Array.init 256 (fun n -> let c = ref n in for _ = 0 to 7 do c := if !c land 1 = 1 then 0xedb88320 lxor (!c lsr 1) else !c lsr 1 done; !c)
let crc s = let c = ref 0xffffffff in String.iter (fun ch -> c := crc_table.((!c lxor Char.code ch) land 0xff) lxor (!c lsr 8)) s; !c lxor 0xffffffff

let deflate (data : Bytes.t) =
  let out = Buffer.create (Bytes.length data / 4) and acc = ref 0 and held = ref 0 in
  (* bits go out lowest first; a code's bits go out highest first *)
  let bits v n = acc := !acc lor (v lsl !held); held := !held + n; while !held >= 8 do Buffer.add_char out (Char.chr (!acc land 0xff)); acc := !acc lsr 8; held := !held - 8 done in
  let code v n = let r = ref 0 in for i = 0 to n - 1 do if v land (1 lsl i) <> 0 then r := !r lor (1 lsl (n - 1 - i)) done; bits !r n in
  let symbol s = if s < 144 then code (0x30 + s) 8 else if s < 256 then code (0x190 + s - 144) 9 else if s < 280 then code (s - 256) 7 else code (0xc0 + s - 280) 8 in
  let lengths = [| 3; 4; 5; 6; 7; 8; 9; 10; 11; 13; 15; 17; 19; 23; 27; 31; 35; 43; 51; 59; 67; 83; 99; 115; 131; 163; 195; 227; 258 |]
  and lextra = [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 0 |]
  and dists = [| 1; 2; 3; 4; 5; 7; 9; 13; 17; 25; 33; 49; 65; 97; 129; 193; 257; 385; 513; 769; 1025; 1537; 2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577 |]
  and dextra = [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 13; 13 |] in
  let last table v = let k = ref (Array.length table - 1) in while table.(!k) > v do decr k done; !k in
  let n = Bytes.length data and seen = Array.make 65536 (-1) in
  let get i = Char.code (Bytes.unsafe_get data i) in
  bits 1 1; bits 1 2;
  let i = ref 0 in
  while !i < n do
    let matched =
      if !i + 3 > n then 0
      else begin
        let h = ((get !i * 2654435 ) lxor (get (!i + 1) * 40503) lxor (get (!i + 2) * 7919)) land 0xffff in
        let j = seen.(h) in
        seen.(h) <- !i;
        if j < 0 || !i - j > 32768 then 0
        else begin
          let l = ref 0 in
          while !l < 258 && !i + !l < n && get (j + !l) = get (!i + !l) do incr l done;
          if !l >= 3 then begin
            let k = last lengths !l in
            symbol (257 + k); bits (!l - lengths.(k)) lextra.(k);
            let d = last dists (!i - j) in
            code d 5; bits (!i - j - dists.(d)) dextra.(d);
            !l
          end
          else 0
        end
      end
    in
    if matched = 0 then (symbol (get !i); incr i) else i := !i + matched
  done;
  symbol 256;
  if !held > 0 then bits 0 (8 - !held);
  Buffer.contents out

(* [rgb]: w * h pixels, three bytes each, rows from the top. *)
let encode ~w ~h (rgb : Bytes.t) =
  let raw = Bytes.create (h * ((3 * w) + 1)) in
  for y = 0 to h - 1 do
    let row = y * ((3 * w) + 1) in
    Bytes.set raw row '\001';
    for x = 0 to (3 * w) - 1 do
      let v = Char.code (Bytes.get rgb ((3 * w * y) + x)) and left = if x >= 3 then Char.code (Bytes.get rgb ((3 * w * y) + x - 3)) else 0 in
      Bytes.set raw (row + 1 + x) (Char.chr ((v - left) land 0xff))
    done
  done;
  let a = ref 1 and b = ref 0 in
  Bytes.iter (fun ch -> a := (!a + Char.code ch) mod 65521; b := (!b + !a) mod 65521) raw;
  let be32 v = String.init 4 (fun i -> Char.chr ((v lsr (8 * (3 - i))) land 0xff)) in
  let chunk kind body = be32 (String.length body) ^ kind ^ body ^ be32 (crc (kind ^ body)) in
  "\137PNG\r\n\026\n" ^ chunk "IHDR" (be32 w ^ be32 h ^ "\008\002\000\000\000") ^ chunk "IDAT" ("\120\001" ^ deflate raw ^ be32 ((!b lsl 16) lor !a)) ^ chunk "IEND" ""

(* For sending a picture as text. *)
let base64 s =
  let abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" and n = String.length s in
  let b = Buffer.create ((n * 4 / 3) + 4) in
  let at i = if i < n then Char.code s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let v = (at !i lsl 16) lor (at (!i + 1) lsl 8) lor at (!i + 2) in
    Buffer.add_char b abc.[v lsr 18]; Buffer.add_char b abc.[(v lsr 12) land 63];
    Buffer.add_char b (if !i + 1 < n then abc.[(v lsr 6) land 63] else '='); Buffer.add_char b (if !i + 2 < n then abc.[v land 63] else '=');
    i := !i + 3
  done;
  Buffer.contents b
