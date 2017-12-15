open Batteries

let search_path = ref [""]

let arg_defs = [
  ("-I", Arg.String (fun f -> search_path := f :: !search_path), "Append a directory to the search path");
]

let parse_args () =
  let in_file = ref (None : string option) in
  Arg.parse arg_defs
    (fun s ->
       match !in_file with
       | Some _ -> failwith "You must specify exactly one input file"
       | None -> in_file := Some s)
    "";
  match !in_file with
  | None -> failwith "You must specify an input file"
  | Some s -> s

let parse_file in_file =
  let pi = open_in in_file in
  let lexbuf = Lexer.create in_file pi in
  let result = Parser.prog Lexer.main lexbuf in
  Parsing.clear_parser (); close_in pi; result

let main () =
  let in_file = parse_args () in
  let _ = parse_file in_file in
  ()

let () = main ()
