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

%start <Syntax.program> program

%%

%inline loc(X) :
| x = X { Location.mk x $startpos $endpos }

program :
| mem = shared_decs SharpLine
    t = separated_nonempty_list(SharpLine, thread)
    Eof {
    { Syntax.initial = mem; threads = t}
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
| x = var Eq n = Int { x, n }

thread :
| body = body {
    { Syntax.locals = Symbol.Set.empty; body }
  }

body :
| { Syntax.Nothing }
| body = nonempty_body { body }

nonempty_body :
| ins = instruction { ins }
| ins = loc(instruction) seq = loc(nonempty_body) {
    Syntax.Seq (ins, seq)
  }

instruction :
| Pass { Syntax.Pass }
| MFence { Syntax.MFence }
| r = loc(var) Assign e = loc(expression) {
    Syntax.Assign (r, e)
  }
| If cond = loc(condition) LCurly body = loc(body) RCurly {
    Syntax.If (cond, body)
  }
| While cond = loc(condition) LCurly body = loc(body) RCurly {
    Syntax.While (cond, body)
  }
| For i = loc(var) Semicolon
  from_exp = loc(expression) Semicolon to_exp = loc(expression)
  LCurly body = loc(body) RCurly {
    Syntax.For (i, from_exp, to_exp, body)
  }

var :
| x = Id { var_sym x }

expression :
| n = loc(Int) { Syntax.Int n }
| x = loc(var) { Syntax.Var x }
| LPar e = expression RPar { e }
| o = loc(arith_unop) e = loc(expression) {
    Syntax.ArithUnop (o, e)
  }
| e1 = loc(expression) o = loc(arith_binop) e2 = loc(expression) {
    Syntax.ArithBinop (o, e1, e2)
  }

condition :
| b = loc(Bool) { Syntax.Bool b }
| LPar c = condition RPar { c }
| o = loc(logic_unop) c = loc(condition) {
    Syntax.LogicUnop (o, c)
  }
| c1 = loc(condition) o = loc(logic_binop) c2 = loc(condition) {
    Syntax.LogicBinop (o, c1, c2)
  }
| e1 = loc(expression) r = loc(arith_rel) e2 = loc(expression) {
    Syntax.ArithRel (r, e1, e2)
  }

%inline arith_unop :
| Minus { Syntax.Neg }

%inline logic_unop :
| Not { Syntax.Not }

%inline arith_binop :
| Plus { Syntax.Add }
| Minus { Syntax.Sub }
| Times { Syntax.Mul }
| Divide { Syntax.Div }

%inline arith_rel :
| Eq { Syntax.Eq }
| Neq { Syntax.Neq }
| Lt { Syntax.Lt }
| Gt { Syntax.Gt }
| Le { Syntax.Le }
| Ge { Syntax.Ge }

%inline logic_binop :
| And { Syntax.And }
| Or { Syntax.Or }
