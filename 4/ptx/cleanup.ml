let read_line_opt () =
    try Some (read_line ())
    with End_of_file -> None

let rec pow x y =
    match y with
    | y when y < 0 -> invalid_arg "The exponent should be in Z+ or 0."
    | 0 -> 1
    | y -> x * (pow x (y - 1))

let rec dec_list n =
    match n with
    | n when n <= 0 -> []
    | n -> (pow 10 (n - 1)) :: (dec_list (n - 1))

let to_int_list s =
    String.fold_right begin
        fun h t -> match h with
        | '0'..'9' -> (Char.code h) - (Char.code '0') :: t
        | _ -> invalid_arg "The string does not represent an integer."
    end s []

let compose_digits l =
    List.fold_right2 (fun a b sum -> a * b + sum) l (dec_list (List.length l)) 0

let to_int s = s |> to_int_list |> compose_digits

let to_pair l =
    match l with
    | a :: b :: [] -> (a, b)
    | _ -> invalid_arg "The list is not represent a pair."

let get_bounds s =
    match s with
    | "" -> []
    | s -> List.map
        (fun s -> to_pair (List.map to_int (String.split_on_char '-' s)))
        (String.split_on_char ',' s)

let within x (a, b) = (a <= x) && (x <= b)
let or_within ((a, b), (c, d)) = (within a (c, d)) || (within c (a, b))
let contains (a, b) (c, d) = (within a (c, d)) && (within b (c, d))
let or_contains (a, b) = (contains a b) || (contains b a)

let test f s =
    match get_bounds s with
    | [] -> 0
    | l -> (fun b -> if b then 1 else 0) (f (to_pair l))

let rec count ?(i = (0, 0)) () =
    match read_line_opt () with
    | None -> i
    | Some s -> count ~i:((fun (a, b)
        -> (a + test or_contains s, b + test or_within s)) i) ()

let print_pair (a, b) = print_int a; print_char ' '; print_int b

let () = begin
    print_pair (count ());
    print_newline ();
    end
