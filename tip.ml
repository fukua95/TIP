open Batteries

let parse_file in_file =
  let pi = open_in in_file in
  let lexbuf = Lexer.create in_file pi in
  let result = Parser.prog Lexer.main lexbuf in
  Parsing.clear_parser (); close_in pi; result

let main () =
  let _ = parse_file "test.tip" in
  ()

let () = main ()
