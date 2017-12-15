open Batteries

type info = FI of string * int * int | UNKNOWN
type 'a with_info = {
  i : info;
  v : 'a;
}

let dummy_info = UNKNOWN
let create_info f l c = FI (f, l, c)

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
