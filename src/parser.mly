%{
 open Lexing
 module Program = Syntax.Program (Syntax.Untyped)
 module Untyped = Syntax.Untyped
%}

%token LCurly RCurly
%token Plus Minus Times Divide
%token Label Local Comma Equal Semicolon Sharp
%token Cmp Jnz Jz Jmp MFence Affect Pass While
%token <int> Int
%token <string> Id
%token Eof

%left Plus
%left Minus
%left Times
%left Divide

%start <Syntax.UntypedProgram.t> program

%%

%inline loc(X) :
| x = X { {Syntax.item = x; startpos = $startpos; endpos = $endpos} }

program :
| mem = shared_decs Sharp t = separated_nonempty_list(Sharp, thread)  Eof
    { {Program.initial = mem; threads = Array.of_list t} }
| error { raise (Error.Error [Error.SyntaxError, $startpos, $endpos, ""]) }

shared_decs :
| vars = separated_list(Comma, shared_dec) { vars }

shared_dec :
| i = Id Equal n = Int { i, n }

thread :
| ins = instructions { {Program.locals = []; ins = Array.of_list ins} }
| loc = local_dec Semicolon ins = instructions { {Program.locals = loc; ins = Array.of_list ins} }

local_dec :
| Local locals = separated_list(Comma, Id) { locals }

instructions :
| ins = nonempty_list(loc(instruction)) { ins }

instruction :
| r = loc(Id) Affect e = loc(expression) { Untyped.Affect (r, e) }
| Cmp r = loc(Id) v1 = loc(value) v2 = loc(value) { Untyped.Cmp (r, v1, v2) }
| Pass { Untyped.Pass }
| MFence { Untyped.MFence }
| Label lbl = loc(Id) { Untyped.Label lbl }
| Jnz r = loc(Id) lbl = loc(Id) { Untyped.Jnz (r, lbl) }
| Jz r = loc(Id) lbl = loc(Id) { Untyped.Jz (r, lbl) }
| Jmp lbl = loc(Id) { Untyped.Jmp lbl }

expression :
| v = loc(value) { Syntax.Val v }
| e1 = loc(expression) o = loc(op) e2 = loc(expression)
    { Syntax.Op (o, e1, e2) }

value :
| n = loc(Int) { Syntax.Int n }
| v = loc(Id) { Syntax.Var v }

%inline op :
| Plus { '+' }
| Minus { '-' }
| Times { '*' }
| Divide { '/' }
