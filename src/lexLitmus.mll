{
  open ParseLitmus
  open Error
  open Lexing
}

let digits = ['0' - '9']
let alpha = ['a' - 'z']
let empty = ['\t' ' ' '\r']

rule lexer proc = parse
  | eof { Eof }
  | '\n' { Lexing.new_line lexbuf; lexer lexbuf}
  | empty+ { lexer lexbuf }
  | "{" { LCurly }
  | "}" { RCurly }
  | "[" alpha+ as var "]" { Var var }
  | "$" digits+ as num { Int (int_of_string num) }
  | ":" { Colon }
  | "," { Comma }
  | ";" { Semi }
  | "|" { Pipe }
  | "(" { LPar }
  | ")" { RPar }
  | "/\\" { And }
  | "=" { Equals }
  | "MOV" { Mov }
  | "MFENCE" { MFence }
  | "exists" { Exists }
  | ("E" ['A' - 'D'] "X") as reg { Reg (sprintf "%d:%s" proc reg) }
