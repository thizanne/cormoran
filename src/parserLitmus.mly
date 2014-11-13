%{
 open Lexing
 module Program = Syntax.Program (Syntax.Typed)
 module Typed = Syntax.Typed

 let rec transform = function
   | [] -> failwith "transform1"
   | [x] ->
     List.map
       (function Some i -> [i] | None -> []) x
   | line :: lines ->
     let r = transform lines in
     List.map2
       (fun ins li ->
         match ins with
         | None -> li
         | Some ins -> ins :: li
       ) line r

  let rec inser x = function
    | [] -> [x]
    | y :: ys as yss ->
      if x = y then yss
      else if x < y then x :: yss
      else y :: inser x ys

  let rec inser2 x = function
    | [] -> [x, 0]
    | (y1, y2) :: ys as yss ->
      if x = y1 then yss
      else if x < y1 then (x, 0) :: yss
      else (y1, y2) :: inser2 x ys

  let rec locals =
    let open Syntax in
    let open Typed in
    function
    | [] -> []
    | x :: xs -> begin match x.item with
        | Read (r, _)
        | Write (_, {item = Var r; _})
        | RegOp (r, _) -> inser r.item (locals xs)
        | _ -> locals xs
      end

  let globals acc threads =
    let open Syntax in
    let open Typed in
    let open TypedProgram in
    let rec global_thread acc = function
      | [] -> acc
      | x :: xs -> begin match x.item with
          | Read (_, v)
          | Write (v, _) -> global_thread (inser2 v.item acc) xs
          | _ -> global_thread acc xs
        end in
    let rec aux acc = function
      | [] -> List.sort Pervasives.compare acc
      | t :: ts -> aux (global_thread acc t.ins) ts in
    aux (List.sort Pervasives.compare acc) threads
%}

%token LCurly RCurly LPar RPar
%token Colon Comma Semi Pipe And Equals Mov MFence Exists
%token Eof
%token <string> Var
%token <int> Int
%token <string> Reg

%start <Syntax.TypedProgram.t> program

%%

%inline loc(X) :
| x = X { {Syntax.item = x; startpos = $startpos; endpos = $endpos} }

program :
| init = init_dec c = code e = exists Eof {
    { Program.initial = globals init c; threads = c }
  }
| error { raise (Error.Error [Error.SyntaxError, $startpos, $endpos, ""]) }

init_dec :
  (* Due to a hack with LexerLitmus.drop_prelude, LCurly is actually
     dropped *)
| LCurly? li = separated_list(Semi, init_var) RCurly { li }

init_var :
| v = Var Equals x = Int { v, x }

code :
| lines = line+ {
      let threads = transform lines in
      List.map
        (fun t -> { Program.locals = locals t; ins = t })
        threads
    }

line :
| ins = separated_nonempty_list(Pipe, loc(instruction)?) Semi { ins }

instruction :
| MFence { Typed.Mfence }
| Mov d = loc(Reg) Comma v = loc(expr) {
    Typed.RegOp (d, v)
  }
| Mov d = loc(Reg) Comma v = loc(Var) { Typed.Read (d, v) }
| Mov d = loc(Var) Comma v = loc(value) { Typed.Write (d, v) }

expr :
| v = loc(value) { Syntax.Val v }

value :
| x = loc(Int) { Syntax.Int x }
| r = loc(Reg) { Syntax.Var r }

exists :
| Exists LPar separated_list(And, equality) RPar {}

equality :
| var Equals Int {}

var :
| Reg {}
| Var {}
