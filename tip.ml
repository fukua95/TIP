open Batteries
open Format
open Ast

let search_path = ref [""]
let run_flag = ref false

let arg_defs = [
  ("-I", Arg.String (fun f -> search_path := f :: !search_path), "Append a directory to the search path");
  ("-run", Arg.Unit (fun () -> run_flag := true), "Run the program");
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

let open_file in_file =
  let rec try_next = function
    | [] -> err ("Could not find " ^ in_file)
    | d :: rest ->
      let name = if d = "" then in_file else Filename.concat d in_file in
      try open_in name with Sys_error m -> try_next rest
  in
  try_next !search_path

let parse_file in_file =
  let pi = open_file in_file in
  let lexbuf = Lexer.create in_file pi in
  let result = try Parser.prog Lexer.main lexbuf with Parser.Error -> error (Lexer.get_info lexbuf) "Parse error" in
  close_in pi; result

let main () =
  let in_file = parse_args () in
  let prog = parse_file in_file in
  if !run_flag then let _ = Interpreter.interp prog in ()

let () = set_max_boxes 1000
let () = set_margin 67
let res =
  Printexc.catch (fun () -> try main (); 0 with Exit x -> x) ()
let () = print_flush ()
let () = exit res
