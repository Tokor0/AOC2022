let acc = ref [] in
    try
        while true do
            acc := read_line () :: !acc;
        done
    with
        End_of_file -> print_string (String.concat "\n" !acc)
