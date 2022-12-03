let mby_read_line () =
    try Some (read_line ())
    with End_of_file -> None

let rec read_lines n =
    match n with
    | n when n <= 0 -> []
    | n -> (mby_read_line ()) :: (read_lines (n - 1))

let mby_read_lines n =
    match read_lines n  with
    | [] -> None
    | s when List.exists Option.is_none s -> None
    | s -> Some (List.map Option.get s)

let byterate c =
    match Char.code c with
    | n when n >= 97 -> 1 lsl (n - 97)
    | n -> 1 lsl (n - 39)

let to_int s = String.fold_left (fun x c -> x lor (byterate c)) 0 s 
let find_common l = List.fold_right (fun a b -> (to_int a) land b) l max_int

let rec priority ?(i=0) x =
    match x with
    | 0 -> i
    | n -> priority ~i:(i + 1) (n lsr 1)

let rec count sum =
    match mby_read_lines 3 with
    | None -> sum
    | Some s -> count (sum + (priority (find_common s)))

let () = begin print_int (count 0); print_newline () end
