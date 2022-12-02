let maybe_read_int_opt () =
  try Some (read_int_opt ())
  with End_of_file -> None

let rec count x s =
  match maybe_read_int_opt () with
  | None -> x
  | Some None when (s > x) -> count s 0
  | Some None -> count x 0
  | Some Some n -> count x (s + n)

let () = print_int (count 0 0)
