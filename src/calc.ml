let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      match result with
      | Int i -> Printf.printf "Int: %d" i; exit 0
      | Add (i, j) ->
          let a = match i with
          | Int b -> b
          | _ -> 100
          in
          let b = match j with
          | Int c -> c
          | _ -> 100
          in
          Printf.printf "%d + %d" a b; exit 0
      | _ -> Printf.printf "ff";
      print_newline(); flush stdout
    done
  with _ ->
    Printf.printf "ffgg";
    exit 0