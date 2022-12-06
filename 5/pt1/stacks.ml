let mby_read_line () =
    try Some (read_line ())
    with End_of_file -> None

let rec read_crate ic = Scanf.bscanf ic " [%c%n%l]" (fun c n l -> c, n / 4)
let mby_read_crate ic =
    try Some (read_crate ic)
    with | End_of_file | Scanf.Scan_failure _ -> None
let rec read_crates ic =
    match mby_read_crate ic with
    | None -> []
    | Some crate -> crate :: read_crates ic
let str_read_crates s = s |> Scanf.Scanning.from_string |> read_crates

let rec crate_indices () =
    match mby_read_line () with
    | None -> failwith "No input, bozo?"
    | Some s -> match String.sub s 0 2 with
        | " 1" -> []
        | _ -> match str_read_crates s with
            | [] -> failwith "Bozo input?"
            | l -> l :: crate_indices ()

let rec inner_prepend l x i =
    match l with
    | [] -> invalid_arg "Index out of bounds."
    | h :: t -> match i with
        | 0 -> (x :: h) :: t
        | n -> h :: inner_prepend t x (i - 1)
let rec inner_pop i l =
    match l with
    | [] -> invalid_arg "Index out of bounds."
    | h :: t -> match i with
        | 0 -> (List.tl h) :: t
        | n -> h :: inner_pop (i - 1) t

let rec list_gen a l =
    match l with
    | n when n <= 0 -> []
    | n -> a :: list_gen a (l - 1)
let structure l = list_gen [] (snd (List.hd l) + 1)
let rec listify l strc =
    match l with
    | [] -> strc
    | h :: t -> listify t ((fun l (c, i) -> inner_prepend l c i) strc h)

let form_init l = l |> structure |> (listify l)
let read_init () = crate_indices () |> List.concat |> List.rev |> form_init

let move (a, b) l = inner_prepend l (List.hd (List.nth l a)) b |> inner_pop a
let rec n_move n m l =
    match n with
    | n when n <= 0 -> l
    | n -> n_move (n - 1) m (move m l)
let read_move () = Scanf.scanf " move %i from %i to %i " (fun n a b -> n, a, b)
let mby_read_move () =
    try Some (read_move ())
    with | End_of_file | Scanf.Scan_failure _ -> None
let rec do_moves l =
    match mby_read_move () with
    | None -> l
    | Some m -> (fun (n, a, b) -> n_move n (a - 1, b - 1) l) m |> do_moves

let rec print_top l =
    String.init (List.length l) (fun i -> List.hd (List.nth l i))
    |> print_endline

let () = print_top (do_moves (read_init ()))

