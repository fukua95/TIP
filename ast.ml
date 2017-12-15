open Batteries
open Format

type info = FI of string * int * int | UNKNOWN
type 'a with_info = {
  i : info;
  v : 'a;
}

let dummy_info = UNKNOWN
let create_info f l c = FI (f, l, c)

exception Exit of int

let errf f =
  print_flush ();
  open_vbox 0;
  open_hvbox 0; f (); print_cut (); close_box (); print_newline ();
  raise (Exit 1)

let print_info = function
  | FI (f, l, c) -> print_string f; print_string ":"; print_int l; print_string "."; print_int c; print_string ":"
  | UNKNOWN -> print_string "<Unknown file and line>:"

let err_at fi f = errf (fun () -> print_info fi; print_space (); f ())

let err s = errf (fun () -> print_string "Error: "; print_string s)

let error fi s = err_at fi (fun () -> print_string s)

type binop = Plus | Minus | Times | Divide | Eqq | GreaterThan
type unop = Ref | Deref

type exp =
  | ECallFunc of exp * exp list * bool * info
  | EIdentifier of string * info
  | EBinop of binop * exp * exp * info
  | EUnop of unop * exp * info
  | ENumber of int * info
  | EInput of info
  | EAlloc of info
  | ENull of info

type stmt =
  | SAssign of exp * exp * info
  | SBlock of stmt list * info
  | SIf of exp * stmt * stmt option * info
  | SOutput of exp * info
  | SWhile of exp * stmt * info
  | SError of exp * info

type decl = string * string list * string list * stmt * exp * info

type program = decl list

let get_exp_info = function
  | ECallFunc (_, _, _, fi) -> fi
  | EIdentifier (_, fi) -> fi
  | EBinop (_, _, _, fi) -> fi
  | EUnop (_, _, fi) -> fi
  | ENumber (_, fi) -> fi
  | EInput fi -> fi
  | EAlloc fi -> fi
  | ENull fi -> fi
