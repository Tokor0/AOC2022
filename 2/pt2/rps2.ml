let maybe_read_line () =
  try Some (read_line ())
  with End_of_file -> None 

let y s i = int_of_char (s.[i])
let numerate s =
  match (String.length s) with
  | n when n >= 3 -> ((y s 0) - 65, (y s 2) - 88)
  | _ -> (0, 0)

let round_result (a, b) =
  match (a, b) with
  | a, b when b = 0 -> ((a + 2) mod 3) + 1
  | a, b when b = 1 -> a + 4
  | a, _ -> ((a + 4) mod 3) + 7

let rec count sum =
  match maybe_read_line () with
  | None -> sum
  | Some s -> count (sum + round_result (numerate s))

let () = print_int (count 0)
