%{
open Ast

module List = BatList
module Option = BatOption
%}

%token <Ast.info> IF
%token <Ast.info> ELSE
%token <Ast.info> WHILE
%token <Ast.info> RETURN
%token <Ast.info> VAR
%token <Ast.info> INPUT
%token <Ast.info> OUTPUT
%token <Ast.info> ALLOC
%token <Ast.info> NULL
%token <Ast.info> ERROR

%token <string Ast.with_info> ID
%token <int Ast.with_info> INTV

%token <Ast.info> LPAREN
%token <Ast.info> RPAREN
%token <Ast.info> LCURLY
%token <Ast.info> RCURLY
%token <Ast.info> SEMI
%token <Ast.info> EQ
%token <Ast.info> EQEQ
%token <Ast.info> GT
%token <Ast.info> PLUS
%token <Ast.info> MINUS
%token <Ast.info> STAR
%token <Ast.info> SLASH
%token <Ast.info> AMPERSAND
%token <Ast.info> COMMA
%token <Ast.info> EOF

%start <Ast.program> prog

%%

prog:
  | decls = list(decl); EOF
    { decls }
  ;

decl:
  | fname = ID; LPAREN; params = separated_list(COMMA, ID); RPAREN; LCURLY; var_decls = list(var_decl); body = list(stmt); RETURN; ret = exp; SEMI; RCURLY
    { (fname.v, List.map (fun param -> param.v) params, List.concat var_decls, SBlock (body, dummy_info), ret, fname.i) }
  ;

var_decl:
  | VAR; locals = separated_list(COMMA, ID); SEMI
    { List.map (fun local -> local.v) locals }
  ;

stmt:
  | lhs = exp; EQ; rhs = exp; SEMI
    { SAssign (lhs, rhs, get_exp_info lhs) }
  | fi = LCURLY; stmts = list(stmt); RCURLY
    { SBlock (stmts, fi) }
  | fi = IF; LPAREN; cond = exp; RPAREN; thens = stmt; elses = option(else_branch)
    { SIf (cond, thens, elses, fi) }
  | fi = OUTPUT; exp = exp; SEMI
    { SOutput (exp, fi) }
  | fi = WHILE; LPAREN; cond = exp; RPAREN; body = stmt
    { SWhile (cond, body, fi) }
  | fi = ERROR; exp = exp; SEMI
    { SError (exp, fi) }
  ;

else_branch:
  | ELSE; elses = stmt
    { elses }
  ;

exp:
  | lhs = arith; EQEQ; rhs = exp
    { EBinop (Eqq, lhs, rhs, get_exp_info lhs) }
  | lhs = arith; GT; rhs = exp
    { EBinop (GreaterThan, lhs, rhs, get_exp_info lhs) }
  | arith = arith
    { arith }
  ;

arith:
  | lhs = arith; PLUS; rhs = factor
    { EBinop (Plus, lhs, rhs, get_exp_info lhs) }
  | lhs = arith; MINUS; rhs = factor
    { EBinop (Minus, lhs, rhs, get_exp_info lhs) }
  | fact = factor
    { fact }
  ;

factor:
  | lhs = factor; STAR; rhs = atom
    { EBinop (Times, lhs, rhs, get_exp_info lhs) }
  | lhs = factor; SLASH; rhs = atom
    { EBinop (Divide, lhs, rhs, get_exp_info lhs) }
  | atom = atom
    { atom }
  ;

atom:
  | var = ID
    { EIdentifier (var.v, var.i) }
  | num = INTV
    { ENumber (num.v, num.i) }
  | fi = INPUT
    { EInput fi }
  | fi = ALLOC
    { EAlloc fi }
  | fi = NULL
    { ENull fi }
  | fname = ID; LPAREN; args = separated_list(COMMA, exp); RPAREN
    { ECallFunc (EIdentifier (fname.v, fname.i), args, false, fname.i) }
  | fi = LPAREN; callee = exp; RPAREN; LPAREN; args = separated_list(COMMA, exp); RPAREN
    { ECallFunc (callee, args, true, fi) }
  | fi = AMPERSAND; atom = atom
    { EUnop (Ref, atom, fi) }
  | fi = STAR; atom = atom
    { EUnop (Deref, atom, fi) }
  | fi = MINUS; num = INTV
    { ENumber (- num.v, num.i) }
  | LPAREN; exp = exp; RPAREN
    { exp }
  ;
