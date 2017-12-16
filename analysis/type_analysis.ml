open Batteries
open Ast

type entity =
  | EnFunName of string
  | EnFunParam of string * string
  | EnFunLocal of string * string
  | EnExp of exp

type ty =
  | TyInt
  | TyRef of ty
  | TyArrow of ty list * ty
  | TyRec of int * ty
  | TyVar of entity
  | TyFresh of int

let analyze prog =
  let fresh =
    let counter = ref 0 in
    fun () -> let res = !counter in incr counter; TyFresh res
  in
  let parent = Hashtbl.create 10 in
  let mk_set t = if not (Hashtbl.mem parent t) then Hashtbl.add parent t t in
  let rec find t =
    mk_set t;
    if Hashtbl.find parent t <> t then Hashtbl.replace parent t (find (Hashtbl.find parent t));
    Hashtbl.find parent t
  in
  let mk_union a b = Hashtbl.add parent a b in
  let rec unify t1 t2 =
    let r1 = find t1 in
    let r2 = find t2 in
    if r1 <> r2 then
      (match r1 with
       | TyVar _
       | TyFresh _ -> mk_union r1 r2
       | _ ->
         (match r2 with
          | TyVar _
          | TyFresh _ -> mk_union r2 r1
          | _ ->
            (match (r1, r2) with
             | (TyInt, TyInt) -> mk_union r1 r2
             | (TyRef i1, TyRef i2) -> mk_union r1 r2; unify i1 i2
             | (TyArrow (args1, res1), TyArrow (args2, res2)) ->
               if List.length args1 = List.length args2 then
                 (mk_union r1 r2;
                  List.iter2 unify args1 args2;
                  unify res1 res2)
               else err "Cannot unify types"
             | _ -> err "Cannot unify types")))
  in
  let exp2ty env = function
    | EIdentifier (x, _) -> Hashtbl.find env x
    | exp -> TyVar (EnExp exp)
  in
  let rec collect_exp env exp =
    let e2t = exp2ty env in
    let ce = collect_exp env in
    match exp with
    | ENumber _ -> unify (e2t exp) TyInt
    | EInput _ -> unify (e2t exp) TyInt
    | EAlloc _ -> unify (e2t exp) (TyRef (fresh ()))
    | ENull _ -> unify (e2t exp) (TyRef (fresh ()))
    | EUnop (Deref, target, _) -> unify (e2t target) (TyRef (e2t exp)); ce target
    | EUnop (Ref, target, _) -> unify (e2t exp) (TyRef (e2t target)); ce target
    | EBinop (Eqq, exp1, exp2, _) -> unify (e2t exp1) (e2t exp2); unify (e2t exp) TyInt; ce exp1; ce exp2
    | EBinop (_, exp1, exp2, _) -> unify (e2t exp1) (e2t exp2); unify (e2t exp1) TyInt; unify (e2t exp) TyInt; ce exp1; ce exp2
    | ECallFunc (callee, args, _, _) -> unify (e2t callee) (TyArrow (List.map e2t args, e2t exp)); ce callee; List.iter ce args
    | EIdentifier _ -> ()
  in
  let rec collect_stmt env =
    let cs = collect_stmt env in
    let ce = collect_exp env in
    let e2t = exp2ty env in
    function
    | SAssign (lhs, rhs, _) -> unify (e2t lhs) (e2t rhs); ce lhs; ce rhs
    | SBlock (stmts, _) -> List.iter cs stmts
    | SIf (cond, th, el, _) -> unify (e2t cond) TyInt; ce cond; cs th; Option.may cs el
    | SOutput (exp, _) -> unify (e2t exp) TyInt; ce exp
    | SError (exp, _) -> unify (e2t exp) TyInt; ce exp
    | SWhile (cond, body, _) -> unify (e2t cond) TyInt; ce cond; cs body
  in
  let genv = Hashtbl.create 10 in
  List.iter (fun (fname, params, locals, body, ret, _) ->
      Hashtbl.add genv fname (TyVar (EnFunName fname))) prog;
  List.iter (fun (fname, params, locals, body, ret, _) ->
      let env = Hashtbl.copy genv in
      List.iter (fun param -> Hashtbl.add env param (TyVar (EnFunParam (fname, param)))) params;
      List.iter (fun local -> Hashtbl.add env local (TyVar (EnFunLocal (fname, local)))) locals;
      collect_stmt env body;
      unify (TyVar (EnFunName fname)) (TyArrow (List.map (fun param -> TyVar (EnFunParam (fname, param))) params, exp2ty env ret));
      collect_exp env ret) prog;
  failwith "TODO"
