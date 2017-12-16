open Batteries

type entity =
  | EnFunName of string
  | EnFunParam of string * string
  | EnFunLocal of string * string
  | EnExp of Ast.exp

type ty =
  | TyInt
  | TyRef of ty
  | TyArrow of ty list * ty
  | TyRec of int * ty
  | TyVar of int
