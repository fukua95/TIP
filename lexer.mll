{
open Ast

module List = BatList
module Hashtbl = BatHashtbl
module Lexing = BatLexing

let reserved_words = [
  ("if", fun i -> Parser.IF i);
  ("else", fun i -> Parser.ELSE i);
  ("while", fun i -> Parser.WHILE i);
  ("return", fun i -> Parser.RETURN i);
  ("var", fun i -> Parser.VAR i);
  ("input", fun i -> Parser.INPUT i);
  ("output", fun i -> Parser.OUTPUT i);
  ("alloc", fun i -> Parser.ALLOC i);
  ("null", fun i -> Parser.NULL i);
  ("error", fun i -> Parser.ERROR i);

  ("(", fun i -> Parser.LPAREN i);
  (")", fun i -> Parser.RPAREN i);
  ("{", fun i -> Parser.LCURLY i);
  ("}", fun i -> Parser.RCURLY i);
  (";", fun i -> Parser.SEMI i);
  ("=", fun i -> Parser.EQ i);
  ("==", fun i -> Parser.EQEQ i);
  (">", fun i -> Parser.GT i);
  ("+", fun i -> Parser.PLUS i);
  ("-", fun i -> Parser.MINUS i);
  ("*", fun i -> Parser.STAR i);
  ("/", fun i -> Parser.SLASH i);
  ("&", fun i -> Parser.AMPERSAND i);
  (",", fun i -> Parser.COMMA i);
]

let symbol_table = Hashtbl.create 1024
let () = List.iter (fun (str, f) -> Hashtbl.add symbol_table str f) reserved_words

let create_id i str =
  try (Hashtbl.find symbol_table str) i
  with _ -> Parser.ID { i = i; v = str }

let line_no = ref 1
and depth = ref 0
and start = ref 0

and file_name = ref ""

let create in_file stream =
  if not (Filename.is_implicit in_file) then file_name := in_file
  else file_name := Filename.concat (Sys.getcwd ()) in_file;
  line_no := 1; start := 0; Lexing.from_channel stream

let new_line lexbuf = incr line_no; start := (Lexing.lexeme_start lexbuf)

let get_info lexbuf = create_info (!file_name) (!line_no) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme
}

let while = [' ' '\009' '\012']
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9']*

rule main =
  parse
  | while+ { main lexbuf }
  | while*("\r")?"\n" { new_line lexbuf; main lexbuf }
  | "//" { comment lexbuf; main lexbuf }
  | "/*" { depth := 1; mcomment lexbuf; main lexbuf }
  | ['0'-'9']+ { Parser.INTV { i = get_info lexbuf; v = int_of_string (text lexbuf) } }
  | id { create_id (get_info lexbuf) (text lexbuf) }
  | "==" { create_id (get_info lexbuf) (text lexbuf) }
  | ['(' ')' '{' '}' ';' '=' '>' '+' '-' '*' '/' '&' ','] { create_id (get_info lexbuf) (text lexbuf) }
  | eof { Parser.EOF (get_info lexbuf) }

and comment =
  parse
  | [^ '\n'] { comment lexbuf }
  | "\n" { new_line lexbuf }

and mcomment =
  parse
  | "/*" { incr depth; mcomment lexbuf }
  | "*/" { decr depth; if !depth > 0 then mcomment lexbuf }
  | [^ '\n'] { mcomment lexbuf }
  | "\n" { new_line lexbuf; mcomment lexbuf }
