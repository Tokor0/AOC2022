let maybe_read_line () =
  try Some (read_line ())
  with End_of_file -> None 

let y s i = int_of_char (s.[i])
let numerate s =
  match (String.length s) with
  | n when n >= 3 -> ((y s 0) - 64, (y s 2) - 87)
  | _ -> (0, 0)

(*a and b are one of n, n + 1 or n + 2, where n is any integer.*)
let round_result (a, b) =
  match (a, b) with
  | a, b when a = b -> 3 + b
  | a, b when (a - b = -1) || (a - b = 2) -> 6 + b
  | _, _ -> 0 + b

let rec count sum =
  match maybe_read_line () with
  | None -> sum
  | Some s -> count (sum + round_result (numerate s))

let () = print_int (count 0)
