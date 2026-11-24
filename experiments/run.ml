(* dune exec experiments/run.exe -- [number ...]
   Every experiment, or the ones numbered. Only failing checks are listed; ALL=1 lists
   every check. Exits with 1 if any check fails. *)
let all = [ E01_kernel.experiment; E02_field.experiment; E03_edge.experiment; E04_corner.experiment; E05_blend.experiment; E06_offset.experiment; E07_fitted.experiment; E11_frames.experiment; E13_tricky.experiment; E15_sketch.experiment; E16_loft.experiment ]

let () =
  let wanted = List.map int_of_string (List.tl (Array.to_list Sys.argv)) in
  let chosen = List.filter (fun (e : Harness.t) -> wanted = [] || List.mem e.id wanted) all in
  if Harness.print_all chosen > 0 then exit 1
