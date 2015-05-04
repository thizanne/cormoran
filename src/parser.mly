%{
  open Lexing
  let var_sym = Symbol.namespace ()
  let lbl_sym = Symbol.namespace ()
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
%token <string> LowerId
%token <string> UpperId
%token Eof

%left Or
%left And
%nonassoc Not
%left Plus Minus
%left Times Divide

%start <Program.t> program

%%

(* Useful stuff *)

%inline loc(X) :
| x = X { Location.mk x $startpos $endpos }

%inline threaded(X) :
| thread_id = Int Colon x = X { Program.create_threaded ~thread_id x }

lbl_sym :
| lbl = LowerId { lbl_sym lbl }

local_sym :
| x = LowerId { var_sym x }

shared_sym :
| x = UpperId { var_sym x }

var :
| var_name = local_sym { { Program.var_type = Program.Local; var_name } }
| var_name = shared_sym { { Program.var_type = Program.Shared; var_name } }

%inline arith_unop :
| Minus { Program.Neg }

%inline logic_unop :
| Not { Program.Not }

%inline arith_binop :
| Plus { Program.Add }
| Minus { Program.Sub }
| Times { Program.Mul }
| Divide { Program.Div }

%inline arith_rel :
| Eq { Program.Eq }
| Neq { Program.Neq }
| Lt { Program.Lt }
| Gt { Program.Gt }
| Le { Program.Le }
| Ge { Program.Ge }

%inline logic_binop :
| And { Program.And }
| Or { Program.Or }

program :
| LCurly properties = list(property) RCurly
  initial = shared_decs SharpLine
    threads = separated_nonempty_list(SharpLine, thread)
    Eof {
    { Program.initial; threads; properties }
  }
| error {
    let open Error in
    let err_loc = { Location.startpos = $startpos; endpos = $endpos } in
    raise @@ Error { error = SyntaxError; err_loc; err_msg = "" }
  }

(* Property definitions *)

property :
| zone = zone_option condition = condition(threaded(var)) {
    { Program.Property.zone; condition }
  }

zone_option :
| At At { None }
| At LPar zone = separated_list(Comma, threaded(intervals)) RPar { Some zone }

intervals :
| intervals = separated_list(Pipe, interval) { intervals }

interval :
| single = lbl_sym {
    let lbl = Some single in
    { Program.Property.initial = lbl; final = lbl }
  }
| initial = lbl_sym? Minus final = lbl_sym? {
    { Program.Property.initial; final }
  }

(* Initial memory values *)

shared_decs :
| vars = separated_list(Comma, shared_dec) {
    let open Batteries in
    vars |> List.enum |> Symbol.Map.of_enum
  }

shared_dec :
| x = shared_sym Eq n = Int { x, n }

(* Program code *)

thread :
| body = body {
    { Program.locals = Symbol.Set.empty; body }
  }

body :
| { Program.Nothing }
| body = nonempty_body { body }

nonempty_body :
| ins = instruction { ins }
| ins = loc(instruction) seq = loc(nonempty_body) {
    Program.Seq (ins, seq)
  }

instruction :
| Pass { Program.Pass }
| Label lbl = loc(lbl_sym) { Program.Label lbl }
| MFence { Program.MFence }
| x = loc(var) Assign e = loc(expression(var)) {
    Program.Assign (x, e)
  }
| If cond = loc(condition(var)) LCurly body = loc(body) RCurly {
    Program.If (cond, body)
  }
| While cond = loc(condition(var)) LCurly body = loc(body) RCurly {
    Program.While (cond, body)
  }
| For i = loc(var) Semicolon
  from_exp = loc(expression(var)) Semicolon to_exp = loc(expression(var))
  LCurly body = loc(body) RCurly {
    Program.For (i, from_exp, to_exp, body)
  }

expression(var) :
| n = loc(Int) { Program.Int n }
| x = loc(var) { Program.Var x }
| LPar e = expression(var) RPar { e }
| o = loc(arith_unop) e = loc(expression(var)) {
    Program.ArithUnop (o, e)
  }
| e1 = loc(expression(var)) o = loc(arith_binop) e2 = loc(expression(var)) {
    Program.ArithBinop (o, e1, e2)
  }

condition(var) :
| b = loc(Bool) { Program.Bool b }
| LPar c = condition(var) RPar { c }
| o = loc(logic_unop) c = loc(condition(var)) {
    Program.LogicUnop (o, c)
  }
| c1 = loc(condition(var)) o = loc(logic_binop) c2 = loc(condition(var)) {
    Program.LogicBinop (o, c1, c2)
  }
| e1 = loc(expression(var)) r = loc(arith_rel) e2 = loc(expression(var)) {
    Program.ArithRel (r, e1, e2)
  }
