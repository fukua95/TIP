open Batteries

type info
type 'a with_info = {
  i : info;
  v : 'a;
}

val dummy_info : info
val create_info : string -> int -> int -> info

exception Exit of int

val errf : (unit -> unit) -> 'a
val err_at : info -> (unit -> unit) -> 'a
val err : string -> 'a
val error : info -> string -> 'a

val print_info : info -> unit

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

val get_exp_info : exp -> info
