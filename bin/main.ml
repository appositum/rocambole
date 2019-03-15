let rec repl () =
  print_string "> ";

  let input = read_line () in
  print_endline input;
  repl ()

let () = repl ()
