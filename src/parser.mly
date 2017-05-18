%{
  open Lexing
  let var_sym = Sym.namespace ()
  let lbl_sym = Sym.namespace ()

  module U = UntypedAst

  let env_union env1 env2 =
    Sym.Map.merge
      (fun var_sym type1 type2 ->
       match type1, type2 with
       | None, None -> assert false
       | None, Some t
       | Some t, None -> Some t
       | Some _, Some _ ->
          Error.syntax_msg_error @@
            Printf.sprintf
              "Shared variable %s declared twice"
              (Sym.name var_sym))
      env1 env2

%}

%token LPar RPar LCurly RCurly
%token Plus Minus Times Divide
%token Eq Neq Gt Ge Lt Le
%token Not Or And
%token At Colon
%token Comma Semicolon SharpLine
%token MFence Pass While If For Label
%token IntType BoolType
%token <bool> Bool
%token <int> Int
%token <string> Id
%token Eof

%left Or
%left And
%left Eq Neq Gt Ge Lt Le
%left Plus Minus
%left Times Divide
%nonassoc unop

%start
<UntypedAst.program *
   (Property.zone option *
      (Sym.t * Source.thread_id option) UntypedAst.expression Location.loc)
     list>
  program

%%

(* Useful stuff *)

%inline loc(X) :
| x = X { Location.mk x $startpos $endpos }

lbl_sym :
| lbl = Id { lbl_sym lbl }

var_sym :
| x = Id { var_sym x }

%inline arith_unop :
| Minus { U.Neg }

%inline logic_unop :
| Not { U.Not }

%inline arith_binop :
| Plus { U.Add }
| Minus { U.Sub }
| Times { U.Mul }
| Divide { U.Div }

%inline logic_binop :
| And { U.And }
| Or { U.Or }

%inline arith_relop :
| Eq { U.Eq }
| Neq { U.Neq }
| Lt { U.Lt }
| Gt { U.Gt }
| Le { U.Le }
| Ge { U.Ge }

(* Program parsing *)

program :
| properties = property_def
  globals = globals
  initial = loc(expression(var_sym)) SharpLine
  threads = separated_nonempty_list(SharpLine, thread)
  Eof {
    { UntypedAst.globals; initial; threads }, properties
  }
| error {
    let err_loc = { Location.startpos = $startpos; endpos = $endpos } in
    Error.syntax_loc_error err_loc ""
  }


(* Global variables *)

var_type :
| IntType { Env.Int }
| BoolType { Env.Bool }

var_sametype_decl :
| t = var_type vars = separated_list(Comma, var_sym) Semicolon {
    List.fold_left (fun env_acc v -> Sym.Map.add v t env_acc) Sym.Map.empty vars
  }

globals :
| decls = nonempty_list(var_sametype_decl) {
    List.fold_left env_union Sym.Map.empty decls
  }
(* Property definitions *)

property_def :
| { [] }
| LCurly properties = list(property) RCurly { properties }

property :
| zone = zone_option condition = loc(property_expression) {
    (zone, condition)
  }

thread_info :
| thread_id = Int Colon { thread_id }

maybe_threaded(X) :
| tid = thread_info ? x = X { x, tid }

threaded_loc(X) :
| thread_id = loc(Int) Colon x = X { thread_id, x }

zone_option :
| At At { None }
| At LPar zone = separated_list(Comma, threaded_loc(loc(lbl_sym))) RPar {
    Some zone
  }

(* Program code *)

thread :
| body = loc(body) {
    { UntypedAst.body }
  }

body :
| { UntypedAst.Nothing }
| body = nonempty_body { body }

nonempty_body :
| ins = instruction { ins }
| ins = loc(instruction) seq = loc(nonempty_body) {
    UntypedAst.Seq (ins, seq)
  }

instruction :
| Pass { UntypedAst.Pass }
| Label lbl = loc(lbl_sym) { UntypedAst.Label lbl }
| MFence { UntypedAst.MFence }
| x = loc(var_sym) Eq e = loc(program_expression) {
    UntypedAst.Assign (x, e)
  }
| If cond = loc(program_expression) LCurly body = loc(body) RCurly {
    UntypedAst.If (cond, body)
  }
| While cond = loc(program_expression) LCurly body = loc(body) RCurly {
    UntypedAst.While (cond, body)
  }
| For i = loc(var_sym) Semicolon
  from_exp = loc(program_expression) Semicolon
  to_exp = loc(program_expression)
  LCurly body = loc(body) RCurly {
    UntypedAst.For (i, from_exp, to_exp, body)
  }

program_expression:
| e = expression (var_sym) { e }

property_expression:
| e = expression(maybe_threaded(var_sym)) { e}

expression(var_id) :
| n = loc(Int) { U.Int n }
| b = loc(Bool) { U.Bool b }
| x = loc(var_id) { U.Var x }
| LPar e = expression(var_id) RPar { e }
| o = loc(arith_unop) e = loc(expression(var_id)) {
    U.ArithUnop (o, e)
  } %prec unop
| o = loc(logic_unop) e = loc(expression(var_id)) {
    U.LogicUnop (o, e)
  } %prec unop
| e1 = loc(expression(var_id))
  o = loc(arith_binop)
  e2 = loc(expression(var_id)) {
    U.ArithBinop (o, e1, e2)
  }
| e1 = loc(expression(var_id))
  o = loc(logic_binop)
  e2 = loc(expression(var_id)) {
    U.LogicBinop (o, e1, e2)
  }
| e1 = loc(expression(var_id))
  o = loc(arith_relop)
  e2 = loc(expression(var_id)) {
    U.ArithRelop (o, e1, e2)
  }
