%{
  open Lexing
  let var_sym = Symbol.namespace ()
%}

%token LPar RPar LCurly RCurly
%token Plus Minus Times Divide
%token Eq Neq Gt Ge Lt Le
%token Not Or And
%token Comma Semicolon SharpLine
%token MFence Assign Pass While If For
%token <bool> Bool
%token <int> Int
%token <string> Id
%token Eof

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
| mem = shared_decs SharpLine
    t = separated_nonempty_list(SharpLine, thread)
    Eof {
    { Program.initial = mem; threads = t}
  }
| error {
    let open Error in
    let err_loc = { Location.startpos = $startpos; endpos = $endpos } in
    raise @@ Error { error = SyntaxError; err_loc; err_msg = "" }
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
