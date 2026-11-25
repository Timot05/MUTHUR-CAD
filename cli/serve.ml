(* The server behind the page (web/view.html): mk52 open starts one in a design folder,
   and it stays until mk52 stop. It listens on this machine only.

   It holds no model and no kernel. What the page needs worked out, it runs the mk52
   command for, as a person would in a terminal, so a kernel built again is the kernel
   the next request gets; the page itself is read from the tool each time it is asked
   for. What it does on its own is watch the design's code: a project whose code was
   saved is built again (mk52 refresh), and the page sees the models change.

     GET  /                          the page
     GET  /look                      what is open, the model to bring to the front, and
                                     what the last build said
     GET  /scene?name=&solo=&hide=   how a model is drawn (mk52 scene), with the
                                     features in hide (5,8) taken out
     GET  /gates?name=&solo=         where its shader's gates are entered: the table
     GET  /regions?name=&solo=&most= the model cut into at most so many regions, a shader
                                     each (mk52 regions)
     GET  /asset?id=                 something a model shows from outside, as triangles
     GET  /edges?name=               its edges and corners, or null
     GET  /pick?name=&solo=&ray=     what a ray hits (mk52 pick): ox,oy,oz,dx,dy,dz
     POST /close?name=               mk52 close, for the person
     POST /state?name=               what the page shows of a model, kept for an agent *)
open Mk52

let tool = Place.command ()
let read path = In_channel.with_open_bin path In_channel.input_all

(* The command run here: whether it succeeded, what it printed, what it said went wrong. *)
let run args =
  let said = Filename.temp_file "mk52" ".said" in
  Fun.protect
    ~finally:(fun () -> Sys.remove said)
    (fun () ->
      let from, into = Unix.pipe ~cloexec:true () and err = Unix.openfile said [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
      let pid = Unix.create_process tool (Array.of_list (tool :: args)) Unix.stdin into err in
      List.iter Unix.close [ into; err ];
      (* read before waiting: a scene is larger than a pipe holds *)
      let printed = In_channel.input_all (Unix.in_channel_of_descr from) in
      Unix.close from;
      let ok = match Unix.waitpid [] pid with _, Unix.WEXITED 0 -> true | _ -> false in
      (ok, printed, read said))

(* ---- The design's code, watched ---- *)

let lock = Mutex.create ()
let building = ref false
let said = ref ""

(* Each project with when its code was last saved. *)
let saved () =
  let changed f = try (Unix.stat f).Unix.st_mtime with Unix.Unix_error _ -> 0. in
  List.map (fun p -> (p, List.fold_left (fun t f -> if Filename.basename f = "entry.ml" then t else Float.max t (changed f)) 0. (try Place.sources p with Sys_error _ -> []))) (Place.projects ())

(* A project whose code changed is built again by the command: its functions listed, its
   open models rewritten. At the start that is every project. *)
let watch () =
  let built = ref [] in
  while true do
    let now = saved () in
    if now <> !built then begin
      let stale = List.filter_map (fun (p, t) -> if List.assoc_opt p !built = Some t then None else Some p) now in
      built := now;
      Mutex.protect lock (fun () -> building := true);
      let ok, printed, err = run ("refresh" :: stale) in
      Mutex.protect lock (fun () -> building := false; said := if ok then "" else String.trim (printed ^ err))
    end;
    Thread.delay 0.4
  done

(* ---- What the page asks ---- *)

(* When a file was written, as text: a number in JSON would lose the end of it. *)
let stamp path = try Printf.sprintf "%.6f" (Unix.stat path).Unix.st_mtime with Unix.Unix_error _ -> ""

(* A page is looking: noted at most once a second, for mk52 view, which opens a page
   only when none is. *)
let noted = ref 0.

let look () =
  let now = Unix.gettimeofday () in
  if now -. !noted > 1. then (noted := now; Out_channel.with_open_bin Place.seen (fun _ -> ()));
  let opened = List.map (fun name -> Json.Obj [ ("name", Json.Str name); ("written", Json.Str (stamp (Place.model name) ^ " " ^ stamp (Place.edges name))) ]) (Place.opened ()) in
  let front = try String.trim (read Place.front) with Sys_error _ -> "" in
  let building, said = Mutex.protect lock (fun () -> (!building, !said)) in
  Json.Obj
    [ ("folder", Json.Str (Filename.basename (Sys.getcwd ()))); ("open", Json.Arr opened); ("front", Json.Str front); ("fronted", Json.Str (stamp Place.front));
      ("building", Json.Bool building); ("said", Json.Str said) ]

(* ---- HTTP, as little of it as the page needs ---- *)

type request = { verb : string; path : string; query : (string * string) list; headers : (string * string) list; body : string }

exception Refused of string * string

let refuse status why = raise (Refused (status, why))

let decode s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      match s.[i] with
      | '%' when i + 2 < String.length s -> (
          match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with Some c -> Buffer.add_char b (Char.chr c); go (i + 3) | None -> Buffer.add_char b '%'; go (i + 1))
      | '+' -> Buffer.add_char b ' '; go (i + 1)
      | c -> Buffer.add_char b c; go (i + 1)
  in
  go 0;
  Buffer.contents b

let request ic =
  let line () = match In_channel.input_line ic with Some l -> String.trim l | None -> refuse "400 Bad Request" "no request" in
  let verb, target = match String.split_on_char ' ' (line ()) with [ v; t; _ ] -> (v, t) | _ -> refuse "400 Bad Request" "not a request" in
  let rec headers acc =
    match line () with
    | "" -> acc
    | l -> ( match String.index_opt l ':' with Some i -> headers ((String.lowercase_ascii (String.sub l 0 i), String.trim (String.sub l (i + 1) (String.length l - i - 1))) :: acc) | None -> headers acc)
  in
  let headers = headers [] in
  let path, query = match String.index_opt target '?' with Some i -> (String.sub target 0 i, String.sub target (i + 1) (String.length target - i - 1)) | None -> (target, "") in
  let query = List.filter_map (fun kv -> match String.index_opt kv '=' with Some i -> Some (String.sub kv 0 i, decode (String.sub kv (i + 1) (String.length kv - i - 1))) | None -> None) (String.split_on_char '&' query) in
  let length = match Option.bind (List.assoc_opt "content-length" headers) int_of_string_opt with Some n when n >= 0 && n <= 1_000_000 -> n | Some _ -> refuse "413 Payload Too Large" "too much sent" | None -> 0 in
  { verb; path; query; headers; body = Option.value ~default:"" (In_channel.really_input_string ic length) }

(* What is asked for, as (kind, body). Only this machine's own page is answered: a page
   from elsewhere says where it is from, and is refused. *)
let answer port r =
  let here = [ Printf.sprintf "localhost:%d" port; Printf.sprintf "127.0.0.1:%d" port ] in
  if not (List.mem (Option.value ~default:"" (List.assoc_opt "host" r.headers)) here) then refuse "403 Forbidden" "asked under another name";
  (match List.assoc_opt "origin" r.headers with Some o when not (List.mem o (List.map (( ^ ) "http://") here)) -> refuse "403 Forbidden" "asked from another page" | _ -> ());
  let param k = match List.assoc_opt k r.query with Some v -> v | None -> refuse "400 Bad Request" (k ^ " is missing") in
  (* a model's name goes into a path and onto a command line: letters, digits, _ and single dots *)
  let name () =
    let n = param "name" in
    let plain = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true | _ -> false in
    if n = "" || n.[0] = '.' || (not (String.for_all plain n)) || List.mem "" (String.split_on_char '.' n) then refuse "400 Bad Request" "not a model's name";
    n
  in
  let solo () = match int_of_string_opt (param "solo") with Some f when f >= 0 -> f | _ -> refuse "400 Bad Request" "solo is a feature's number" in
  (* the features taken out of what is shown: hide=5,8, or none *)
  let hidden () =
    match List.assoc_opt "hide" r.query with
    | None | Some "" -> []
    | Some v -> List.sort_uniq compare (List.map (fun f -> match int_of_string_opt f with Some f when f > 0 -> f | _ -> refuse "400 Bad Request" "hide is a list of features' numbers") (String.split_on_char ',' v))
  in
  (* what is shown, as the command takes it: 3, or 3:5,8 *)
  let view () = string_of_int (solo ()) ^ match hidden () with [] -> "" | h -> ":" ^ String.concat "," (List.map string_of_int h) in
  let json = "application/json" in
  let asked args = match run args with true, printed, _ -> (json, printed) | false, printed, err -> refuse "500 Internal Server Error" (String.trim (printed ^ err)) in
  let file path = try read path with Sys_error _ -> refuse "404 Not Found" "not there" in
  match (r.verb, r.path) with
  | "GET", "/" -> ("text/html; charset=utf-8", file (Place.page ()))
  | "GET", "/look" -> (json, Json.to_string (look ()))
  | "GET", "/scene" -> asked [ "scene"; name (); view () ]
  | "GET", "/gates" -> ("application/octet-stream", file (Scene.gates (name ()) { solo = solo (); hidden = hidden () }))
  | "GET", "/regions" ->
      let most = match int_of_string_opt (param "most") with Some n when n >= 1 && n <= 4096 -> n | _ -> refuse "400 Bad Request" "most is a number of regions" in
      asked [ "regions"; name (); view (); string_of_int most ]
  | "GET", "/asset" ->
      let id = param "id" in
      if id = "" || not (String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) id) then refuse "400 Bad Request" "not an asset's number";
      ("application/octet-stream", file (Place.asset id))
  | "GET", "/edges" -> (json, try read (Place.edges (name ())) with Sys_error _ -> "null")
  | "GET", "/pick" ->
      let ray = String.split_on_char ',' (param "ray") in
      if List.length ray <> 6 || List.exists (fun v -> float_of_string_opt v = None) ray then refuse "400 Bad Request" "ray is ox,oy,oz,dx,dy,dz";
      asked ([ "pick"; name (); view () ] @ ray)
  | "POST", "/close" ->
      let ok, printed, err = run [ "close"; name () ] in
      (json, Json.to_string (Json.Obj [ ("ok", Json.Bool ok); ("said", Json.Str (if ok then "" else String.trim (printed ^ err))) ]))
  | "POST", "/state" ->
      ignore (try Json.parse r.body with _ -> refuse "400 Bad Request" "not JSON");
      Out_channel.with_open_bin (Place.state (name ())) (fun oc -> output_string oc r.body);
      (json, "true")
  | _ -> refuse "404 Not Found" "nothing by that name"

let serve port client =
  let ic = Unix.in_channel_of_descr client and oc = Unix.out_channel_of_descr client in
  let send status kind body =
    Printf.fprintf oc "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n%s" status kind (String.length body) body;
    flush oc
  in
  (try
     match answer port (request ic) with
     | kind, body -> send "200 OK" kind body
     | exception Refused (status, why) -> send status "text/plain; charset=utf-8" why
     | exception (Failure why | Sys_error why) -> send "500 Internal Server Error" "text/plain; charset=utf-8" why
   with Sys_error _ | Unix.Unix_error _ -> ());
  try Unix.close client with Unix.Unix_error _ -> ()

(* The first free port from 8752 up: a design folder each has its own server. *)
let listen () =
  let rec from port =
    let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    match Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
    | () -> Unix.listen sock 64; (sock, port)
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) when port < 8800 -> Unix.close sock; from (port + 1)
  in
  from 8752

let main () =
  let sock, port = listen () in
  ignore (Place.folder Place.made);
  Out_channel.with_open_bin Place.server (fun oc -> Printf.fprintf oc "%d %d\n" (Unix.getpid ()) port);
  (* told to stop, it goes at once, without the leave-taking of exit: that flushes every
     open channel, and one to a page that went away in the middle of an answer can be
     left locked for good, which kept a stopped server from ever going *)
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> (try Sys.remove Place.server with Sys_error _ -> ()); Unix._exit 0));
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  ignore (Thread.create watch ());
  while true do
    match Unix.accept ~cloexec:true sock with
    | client, _ -> ignore (Thread.create (serve port) client)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
  done
