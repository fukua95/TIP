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
  | TyRec of ty * ty
  | TyVar of entity
  | TyFresh of entity

let analyze prog =
  let fresh = function
    | TyVar en -> TyFresh en
    | TyFresh en -> TyFresh en
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
    | EAlloc _ -> unify (e2t exp) (TyRef (fresh (e2t exp)))
    | ENull _ -> unify (e2t exp) (TyRef (fresh (e2t exp)))
    | EUnop (Deref, target, _) -> unify (e2t target) (TyRef (e2t exp)); ce target
    | EUnop (Ref, target, _) -> unify (e2t exp) (TyRef (e2t target)); ce target
    | EBinop (Eqq, exp1, exp2, _) -> unify (e2t exp1) (e2t exp2); unify (e2t exp) TyInt; ce exp1; ce exp2
    | EBinop (_, exp1, exp2, _) -> unify (e2t exp1) (e2t exp2); unify (e2t exp1) TyInt; unify (e2t exp) TyInt; ce exp1; ce exp2
    | ECallFunc (callee, args, _, _) -> unify (e2t callee) (TyArrow (List.map e2t args, e2t exp)); ce callee; List.iter ce args
    | EIdentifier _ -> ()
  in
  let rec collect_stmt env stmt =
    let cs = collect_stmt env in
    let ce = collect_exp env in
    let e2t = exp2ty env in
    match stmt with
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
  let sol = Hashtbl.filter (fun t ->
      match t with
      | TyVar _
      | TyFresh _ -> true
      | _ -> false) parent
  in
  let visited = Hashtbl.create 10 in
  let rec appear_free_in v t =
    match t with
    | TyVar _
    | TyFresh _ -> v = t
    | TyInt -> false
    | TyRef r -> appear_free_in v r
    | TyArrow (args, res) -> List.for_all (appear_free_in v) args && appear_free_in v res
    | TyRec (bv, body) -> bv <> v && appear_free_in v body
  in
  let rec subst x s t =
    match t with
    | TyVar _
    | TyFresh _ -> if x = t then s else t
    | TyInt -> TyInt
    | TyRef r -> TyRef (subst x s r)
    | TyArrow (args, res) -> TyArrow (List.map (subst x s) args, subst x s res)
    | TyRec (bv, body) -> if bv = x then t else TyRec (bv, subst x s body)
  in
  let rec close t =
    match t with
    | TyVar _
    | TyFresh _ ->
      if (not (Hashtbl.mem visited t)) && (Hashtbl.find_option sol t <> Some t) then
        (Hashtbl.add visited t true;
         let cterm = close (Hashtbl.find sol t) in
         let new_v = fresh t in
         Hashtbl.remove visited t;
         if appear_free_in new_v cterm then TyRec (new_v, subst t new_v cterm) else cterm)
      else
        fresh t
    | TyInt -> TyInt
    | TyRef r -> TyRef (close r)
    | TyArrow (args, res) -> TyArrow (List.map close args, close res)
    | TyRec (bv, body) -> TyRec (bv, close body)
  in
  let closed_sol = Hashtbl.filter_map (fun tf tt ->
      match tf with
      | TyVar en -> Some (close (TyVar en))
      | _ -> None) sol in
  Hashtbl.to_list closed_sol
