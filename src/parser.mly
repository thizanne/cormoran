%{
  open Lexing
  let var_sym = Sym.namespace ()
  let lbl_sym = Sym.namespace ()

  module U = UntypedAst

%}

%token LPar RPar LCurly RCurly
%token Plus Minus Times Divide
%token Eq Neq Gt Ge Lt Le
%token Not Or And
%token At Pipe Colon
%token Comma Semicolon SharpLine
%token MFence Assign Pass While If For Label
%token <bool> Bool
%token <int> Int
%token <string> Id
%token Eof

%left Or
%left And
%nonassoc Not
%left Eq Neq Gt Ge Lt Le
%left Plus Minus
%left Times Divide


%start
<UntypedAst.program *
   (Property.zone option *
      Sym.t Context.MaybeThreaded.t UntypedAst.expression Location.loc)
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

program :
| properties = property_def
  initial = shared_decs SharpLine
    threads = separated_nonempty_list(SharpLine, thread)
    Eof {
    { UntypedAst.initial; threads }, properties
  }
| error {
    let open Error in
    let err_loc = { Location.startpos = $startpos; endpos = $endpos } in
    raise @@ Error { error = SyntaxError; err_loc; err_msg = "" }
  }

(* Property definitions *)

property_def :
| { [] }
| LCurly properties = list(property) RCurly { properties }

property :
| zone = zone_option condition = loc(property_expression) {
    (zone, condition)
  }

threaded(X) :
| thread_id = Int Colon x = X { thread_id, x }

maybe_threaded(X) :
| x = X {
    Context.MaybeThreaded.{ item = x; thread_id = None }
  }
| tid_x = threaded(X) {
    let tid, x = tid_x in
    Context.MaybeThreaded.{ item = x; thread_id = Some tid }
  }

threaded_loc(X) :
| thread_id = loc(Int) Colon x = X { thread_id, x }

zone_option :
| At At { None }
| At LPar zone = separated_list(Comma, threaded_loc(intervals)) RPar {
    Some zone
  }

intervals :
| intervals = separated_list(Pipe, interval) { intervals }

interval :
| single = loc(lbl_sym) {
    let lbl = Some single in
    { Property.initial = lbl; final = lbl }
  }
| initial = loc(lbl_sym)? Minus final = loc(lbl_sym)? {
    { Property.initial; final }
  }

(* Initial memory values *)

shared_decs :
| vars = separated_list(Comma, shared_dec) {
    let open Batteries in
    vars |> List.enum |> Sym.Map.of_enum
  }

shared_dec :
| x = var_sym Eq n = Int { x, n }

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
| x = loc(var_sym) Assign e = loc(program_expression) {
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
  }
| o = loc(logic_unop) e = loc(expression(var_id)) {
    U.LogicUnop (o, e)
  }
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
