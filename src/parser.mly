%{
 open Lexing
 let var_sym = Symbol.namespace ()
 let lbl_sym = Symbol.namespace ()
%}

%token LPar RPar LCurly RCurly
%token Plus Minus Times Divide
%token Eq Neq Gt Ge Lt Le
%token Not Or And
%token Label Comma Equal Semicolon Sharp
%token Goto MFence Assign Pass While If For
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

%start <Syntax.program> program

%%

%inline loc(X) :
| x = X { Location.mk x $startpos $endpos }

program :
| mem = shared_decs Sharp t = separated_nonempty_list(Sharp, thread) Eof {
    { Syntax.initial = mem; threads = t}
  }
| error {
    raise (Error.Error [Error.SyntaxError, $startpos, $endpos, ""])
  }

shared_decs :
| vars = separated_list(Comma, shared_dec) { vars }

shared_dec :
| x = var Equal n = Int { x, n }

thread :
| body = body {
    { Syntax.locals = []; body }
  }

body :
| { Syntax.Pass }
| ins = instruction { ins }
| ins = loc(instruction) Semicolon seq = loc(body) {
    Syntax.Seq (ins, seq)
  }

instruction :
| Pass { Syntax.Pass }
| MFence { Syntax.MFence }
| Label lbl = loc(label) { Syntax.Label lbl }
| Goto lbl = loc(label) { Syntax.Goto lbl }
| r = loc(var) Assign e = loc(expression) {
    Syntax.Assign (r, e)
  }
| If cond = loc(expression) LCurly body = loc(body) RCurly {
    Syntax.If (cond, body)
  }
| While cond = loc(expression) LCurly body = loc(body) RCurly {
    Syntax.While (cond, body)
  }
| For i = loc(var) Semicolon
  from_exp = loc(expression) Semicolon to_exp = loc(expression)
  LCurly body = loc(body) RCurly {
    Syntax.For (i, from_exp, to_exp, body)
  }

var :
| x = Id { var_sym x }

label :
| lbl = Id { lbl_sym lbl }

expression :
| b = loc(Bool) { Syntax.Bool b }
| n = loc(Int) { Syntax.Int n }
| x = loc(var) { Syntax.Var x }
| o = loc(unop) e = loc(expression) {
    Syntax.Unop (o, e)
  }
| e1 = loc(expression) o = loc(binop) e2 = loc(expression) {
    Syntax.Binop (o, e1, e2)
  }
| LPar e = expression RPar { e }

%inline unop :
| Not { Syntax.Not }
| Minus { Syntax.Neg }

%inline binop :
| Plus { Syntax.Add }
| Minus { Syntax.Sub }
| Times { Syntax.Mul }
| Divide { Syntax.Div }
| Eq { Syntax.Eq }
| Neq { Syntax.Neq }
| Lt { Syntax.Lt }
| Gt { Syntax.Gt }
| Le { Syntax.Le }
| Ge { Syntax.Ge }
| And { Syntax.And }
| Or { Syntax.Or }
