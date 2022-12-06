let rec list_sub s p l =
    match l with
    | l when l <= 0 -> []
    | l -> s.[p - l] :: list_sub s p (l - 1)
let rec dupe_check l =
    match l with
    | [] -> true
    | h :: t -> if List.exists (fun a -> a = h) t then false else dupe_check t
let rec sliding_window l ?(p = l) s =
    match p with
    | p when dupe_check (list_sub s p l) -> p
    | p when (p - 1) >= String.length s -> failwith "Not found!"
    | p -> sliding_window l s ~p:(p + 1)
let () = read_line () |> sliding_window 14 |> print_int

