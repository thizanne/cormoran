%{
  open Lexing
  let var_sym = Symbol.namespace ()
  let lbl_sym = Symbol.namespace ()
%}

%token LPar RPar LCurly RCurly
%token Plus Minus Times Divide
%token Eq Neq Gt Ge Lt Le
%token Not Or And
%token At BigAnd Pipe Colon
%token Comma Semicolon SharpLine
%token MFence Assign Pass While If For Label
%token <bool> Bool
%token <int> Int
%token <string> Id
%token Eof

%left BigAnd
%left Or
%left And
%nonassoc Not
%left Plus Minus
%left Times Divide

%start <Program.t> program

%%

%inline loc(X) :
| x = X { Location.mk x $startpos $endpos }

program :
| LCurly property = property RCurly
  initial = shared_decs SharpLine
    threads = separated_nonempty_list(SharpLine, thread)
    Eof {
    { Program.initial; threads; property }
  }
| error {
    let open Error in
    let err_loc = { Location.startpos = $startpos; endpos = $endpos } in
    raise @@ Error { error = SyntaxError; err_loc; err_msg = "" }
  }

property :
| z = zone_option tid = tid_option c = condition {
    Program.Property.Condition(z, tid, c)
  }
| p1 = property BigAnd p2 = property {
    Program.Property.And (p1, p2)
  }

zone_option :
| At At { None }
| At LPar zone = separated_list(Comma, thread_zone_threaded) RPar { Some zone }

%inline tid_option :
| { None }
| tid = Int Colon { Some tid }

thread_zone_threaded :
| thread_id = Int Colon intervals = separated_list(Pipe, interval) {
    Program.create_threaded ~thread_id intervals
  }

interval :
| initial = Id? Minus final = Id? {
    let initial = BatOption.map lbl_sym initial in
    let final = BatOption.map lbl_sym final in
    { Program.Property.initial; final }
  }

shared_decs :
| vars = separated_list(Comma, shared_dec) {
    let open Batteries in
    vars |> List.enum |> Symbol.Map.of_enum
  }

shared_dec :
| x = var_sym Eq n = Int { x, n }

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
| Label lbl = loc(label) { Program.Label lbl }
| MFence { Program.MFence }
| x = loc(var) Assign e = loc(expression) {
    Program.Assign (x, e)
  }
| If cond = loc(condition) LCurly body = loc(body) RCurly {
    Program.If (cond, body)
  }
| While cond = loc(condition) LCurly body = loc(body) RCurly {
    Program.While (cond, body)
  }
| For i = loc(var) Semicolon
  from_exp = loc(expression) Semicolon to_exp = loc(expression)
  LCurly body = loc(body) RCurly {
    Program.For (i, from_exp, to_exp, body)
  }

label :
| lbl = Id { lbl_sym lbl }

var :
| var_name = var_sym {
    { Program.var_type = Program.Shared; var_name }
  }

var_sym :
| x = Id { var_sym x }

expression :
| n = loc(Int) { Program.Int n }
| x = loc(var) { Program.Var x }
| LPar e = expression RPar { e }
| o = loc(arith_unop) e = loc(expression) {
    Program.ArithUnop (o, e)
  }
| e1 = loc(expression) o = loc(arith_binop) e2 = loc(expression) {
    Program.ArithBinop (o, e1, e2)
  }

condition :
| b = loc(Bool) { Program.Bool b }
| LPar c = condition RPar { c }
| o = loc(logic_unop) c = loc(condition) {
    Program.LogicUnop (o, c)
  }
| c1 = loc(condition) o = loc(logic_binop) c2 = loc(condition) {
    Program.LogicBinop (o, c1, c2)
  }
| e1 = loc(expression) r = loc(arith_rel) e2 = loc(expression) {
    Program.ArithRel (r, e1, e2)
  }

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
