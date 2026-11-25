(* Work on several cores at once (OCaml's domains).

   [map f items]: f of every item, in the items' order, the items shared out among as
   many cores as the machine advises, up to eight. What f changes must be its own: a
   Prog.ctx, a table, a buffer made inside it. What it only reads may be shared. *)

let cores = max 1 (min 8 (Domain.recommended_domain_count ()))

let map f items =
  let items = Array.of_list items in
  let n = Array.length items in
  if n <= 1 || cores = 1 then Array.to_list (Array.map f items)
  else begin
    let out = Array.make n None and next = Atomic.make 0 in
    let rec work () =
      let i = Atomic.fetch_and_add next 1 in
      if i < n then (out.(i) <- Some (f items.(i)); work ())
    in
    let others = List.init (min cores n - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join others;
    Array.to_list (Array.map Option.get out)
  end
