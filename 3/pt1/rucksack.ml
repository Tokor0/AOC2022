let maybe_read_line () =
    try Some (read_line ())
    with End_of_file -> None

let split s =
    match String.length s with
    | 0 -> ("", "")
    | n -> (String.sub s 0 (n / 2), String.sub s (n / 2) (n / 2))

let byterate c =
    match Char.code c with
    | n when n >= 97 -> 1 lsl (n - 97)
    | n -> 1 lsl (n - 39)

let byterate_lor x c = x lor (byterate c)
let to_int s = String.fold_left byterate_lor 0 s
let find_common (s1, s2) = (to_int s1) land (to_int s2)

let rec priority i x =
    match x with
    | 0 -> i
    | n -> priority (i + 1) (n lsr 1)

let rec count sum =
    match maybe_read_line () with
    | None -> sum
    | Some s -> count (sum + (priority 0 (find_common (split s))))

let () = begin print_int (count 0); print_newline () end
