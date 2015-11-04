%{
  open Lexing
  let var_sym = Sym.namespace ()
  let lbl_sym = Sym.namespace ()

  module Expression = UntypedAst.Expression
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
      UntypedAst.Expression.InProperty.t Location.loc)
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
| Minus { Operators.Neg }

%inline logic_unop :
| Not { Operators.Not }

%inline arith_binop :
| Plus { Operators.Add }
| Minus { Operators.Sub }
| Times { Operators.Mul }
| Divide { Operators.Div }

%inline arith_rel :
| Eq { Operators.Eq }
| Neq { Operators.Neq }
| Lt { Operators.Lt }
| Gt { Operators.Gt }
| Le { Operators.Le }
| Ge { Operators.Ge }

%inline logic_binop :
| And { Operators.And }
| Or { Operators.Or }

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

(* A lot of duplication here because we cannot easily parameterize a rule
  over a module. Going back to expressions being a simple
  parameterized type over the types of variables might be the good
  option. *)

program_expression :
| n = loc(Int) { Expression.InProgram.Int n }
| b = loc(Bool) { Expression.InProgram.Bool b }
| x = loc(var_sym) { Expression.InProgram.Var x }
| LPar e = program_expression RPar { e }
| o = loc(arith_unop) e = loc(program_expression) {
    Expression.InProgram.ArithUnop (o, e)
  }
| e1 = loc(program_expression)
  o = loc(arith_binop)
  e2 = loc(program_expression) {
    Expression.InProgram.ArithBinop (o, e1, e2)
  }
| o = loc(logic_unop) c = loc(program_expression) {
    Expression.InProgram.LogicUnop (o, c)
  }
| c1 = loc(program_expression)
  o = loc(logic_binop)
  c2 = loc(program_expression) {
    Expression.InProgram.LogicBinop (o, c1, c2)
  }
| e1 = loc(program_expression)
  r = loc(arith_rel)
  e2 = loc(program_expression) {
    Expression.InProgram.ArithRel (r, e1, e2)
  }

property_expression :
| n = loc(Int) { Expression.InProperty.Int n }
| b = loc(Bool) { Expression.InProperty.Bool b }
| x = loc(maybe_threaded(var_sym)) { Expression.InProperty.Var x }
| LPar e = property_expression RPar { e }
| o = loc(arith_unop) e = loc(property_expression) {
    Expression.InProperty.ArithUnop (o, e)
  }
| e1 = loc(property_expression)
  o = loc(arith_binop)
  e2 = loc(property_expression) {
    Expression.InProperty.ArithBinop (o, e1, e2)
  }
| o = loc(logic_unop) c = loc(property_expression) {
    Expression.InProperty.LogicUnop (o, c)
  }
| c1 = loc(property_expression)
  o = loc(logic_binop)
  c2 = loc(property_expression) {
    Expression.InProperty.LogicBinop (o, c1, c2)
  }
| e1 = loc(property_expression)
  r = loc(arith_rel)
  e2 = loc(property_expression) {
    Expression.InProperty.ArithRel (r, e1, e2)
  }
