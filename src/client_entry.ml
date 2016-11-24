let () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to Club Ocaml.\n");
  print_endline "Please enter the IP of the server you wish to connect to\n";
  print_string  "> ";
  let file_name = read_line () in
  Client.main file_name
