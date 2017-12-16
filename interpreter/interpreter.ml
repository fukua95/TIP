open Batteries
open Ast

type value =
  | VInt of int
  | VNull
  | VRef of int
  | VFun of decl

type env = (string, int) Hashtbl.t
type store = (int, value) Hashtbl.t * int ref

let rec interp prog =
  let (fname, params, locals, body, ret, _) =
    try List.find (fun (fname, _, _, _, _, _) -> fname = "main") prog
    with Not_found -> err "Missing main function"
  in
  let store = Hashtbl.create 10 in
  let genv = Hashtbl.create 10 in
  let next = ref 0 in
  List.iter (fun decl ->
      let (fname, _, _, _, _, _) = decl in
      let v = VFun decl in
      let l = !next in
      incr next;
      Hashtbl.replace genv fname l;
      Hashtbl.add store l v) prog;
  let env = Hashtbl.copy genv in
  List.iter (fun param ->
      let v = VInt (read_int ()) in
      let l = !next in
      incr next;
      Hashtbl.replace env param l;
      Hashtbl.add store l v) params;
  let res = interp_func genv env (store, next) locals body ret in
  match res with
  | VInt n -> print_string "Main return: "; print_int n; print_newline ()
  | _ -> err "Main function doesn't return an integer"

and interp_func genv env (store, next) locals body ret =
  List.iter (fun local -> let l = !next in incr next; Hashtbl.replace env local l) locals;
  let () = interp_stmt genv env (store, next) body in
  interp_exp genv env (store, next) ret

and interp_stmt genv env (store, next) = function
  | SAssign (lhs, rhs, fi) ->
    let l = interp_ref genv env (store, next) lhs in
    let v = interp_exp genv env (store, next) rhs in
    Hashtbl.replace store l v
  | SBlock (stmts, fi) -> List.iter (interp_stmt genv env (store, next)) stmts
  | SIf (cond, th, el, fi) ->
    let res_cond = interp_exp genv env (store, next) cond in
    (match res_cond with
     | VInt 0 -> Option.default () (Option.map (interp_stmt genv env (store, next)) el)
     | VInt _ -> interp_stmt genv env (store, next) th
     | _ -> error fi "Conditioning on a non-integer")
  | SOutput (e, fi) ->
    let res_e = interp_exp genv env (store, next) e in
    (match res_e with
     | VInt n -> print_string "Program output: "; print_int n; print_newline ()
     | _ -> error fi "Outputting a non-integer")
  | SWhile (cond, body, fi) ->
    let res_cond = interp_exp genv env (store, next) cond in
    (match res_cond with
     | VInt 0 -> ()
     | VInt _ -> interp_stmt genv env (store, next) body; interp_stmt genv env (store, next) (SWhile (cond, body, fi))
     | _ -> error fi "Conditioning on a non-integer")
  | SError (e, fi) ->
    let res_e = interp_exp genv env (store, next) e in
    (match res_e with
     | VInt n -> error fi ("Execution error, code: " ^ string_of_int n)
     | _ -> error fi "Raising a non-integer")

and interp_ref genv env (store, next) = function
  | EIdentifier (x, _) -> Hashtbl.find env x
  | EUnop (Deref, target, fi) ->
    let res_target = interp_exp genv env (store, next) target in
    (match res_target with
     | VRef l -> l
     | _ -> error fi "Dereferencing an invalid pointer")
  | exp -> error (get_exp_info exp) "Not a left-value expression"

and interp_exp genv env (store, next) = function
  | EIdentifier (x, fi) ->
    let l = Hashtbl.find env x in
    (try Hashtbl.find store l with Not_found -> error fi "Uninitialized variable")
  | ENumber (n, _) -> VInt n
  | EInput _ -> VInt (read_int ())
  | ENull _ -> VNull
  | EAlloc _ -> let l = !next in incr next; VRef l
  | EBinop (bop, exp1, exp2, fi) ->
    let res1 = interp_exp genv env (store, next) exp1 in
    let res2 = interp_exp genv env (store, next) exp2 in
    if bop = Eqq then VInt (if res1 = res2 then 1 else 0)
    else
      (match (res1, res2) with
       | (VInt n1, VInt n2) ->
         (match bop with
          | Plus -> VInt (n1 + n2)
          | Minus -> VInt (n1 - n2)
          | Times -> VInt (n1 * n2)
          | Divide -> VInt (n1 / n2)
          | GreaterThan -> VInt (if n1 > n2 then 1 else 0)
          | Eqq -> VInt (if n1 = n2 then 1 else 0))
       | _ -> error fi "Arithmetic operation on non-integers")
  | EUnop (Deref, target, fi) ->
    let res_target = interp_exp genv env (store, next) target in
    (match res_target with
     | VRef l -> Hashtbl.find store l
     | _ -> error fi "Dereferencing an invalid pointer")
  | EUnop (Ref, target, fi) ->
    let ref_target = interp_ref genv env (store, next) target in
    VRef ref_target
  | ECallFunc (callee, args, _, fi) ->
    let func = interp_exp genv env (store, next) callee in
    (match func with
     | VFun (_, params, locals, body, ret, _) ->
       if List.length params <> List.length args then error fi "Calling a function with wrong number of arguments"
       else
         let new_env = Hashtbl.copy genv in
         List.fold_right (fun (arg, param) () ->
             let res_arg = interp_exp genv env (store, next) arg in
             let l = !next in
             incr next;
             Hashtbl.replace new_env param l;
             Hashtbl.add store l res_arg
           ) (List.combine args params) ();
         interp_func genv new_env (store, next) locals body ret
     | _ -> error fi "Calling a non-function")

