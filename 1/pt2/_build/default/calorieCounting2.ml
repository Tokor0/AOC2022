let maybe_read_int_opt () =
  try Some (read_int_opt ())
  with End_of_file -> None

let rec ord_insert x xs =
  match xs with
  | [] -> [x]
  | h :: _ when (h >= x) -> x :: xs
  | h :: t -> h :: ord_insert x t

let rec sum xs =
  match xs with
  | [] -> 0
  | h :: t -> h + sum t

let pop xs =
  match xs with
  | [] -> []
  | _ :: t -> t

let rec count xs s =
  match maybe_read_int_opt () with
  | None -> sum xs
  | Some None -> count (pop (ord_insert s xs)) 0
  | Some Some n -> count xs (s + n)

let () = print_int (count [0; 0; 0] 0)
