{
  open ParserLitmus
  open Error
  open Lexing
  open Printf

  let proc = ref 0
}

let digits = ['0' - '9']
let alpha = ['a' - 'z']
let empty = ['\t' ' ' '\r']

rule drop_prelude = parse
  | '\n' { Lexing.new_line lexbuf; drop_prelude lexbuf }
  | '{' { () }
  | _ { drop_prelude lexbuf }

and lexer = parse
  | eof { Eof }
  | '\n' { Lexing.new_line lexbuf; lexer lexbuf}
  | empty+ { lexer lexbuf }
  | "{" { LCurly }
  | "}" { RCurly }
  (*  | ":" { Colon } *)
  | "," { Comma }
  | ";" { proc := 0; Semi }
  | "|" { incr proc; Pipe }
  | "(" { LPar }
  | ")" { RPar }
  | "/\\" { And }
  | "=" { Equals }
  | "MOV" { Mov }
  | "MFENCE" { MFence }
  | "exists" { Exists }
  | alpha+ as var { Var var }
  | digits+ as num { Int (int_of_string num) }
  | digits ':' "E" ['A' - 'D'] "X" as reg { Reg reg }
  | "[" (alpha+ as var) "]" { Var var }
  | "$" (digits+ as num) { Int (int_of_string num) }
  | empty* 'P' ['0'-'9'] [^'\n']+ { lexer lexbuf }
  | "E" ['A' - 'D'] "X" as reg { Reg (sprintf "%d:%s" !proc reg) }
