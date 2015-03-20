{
  open ParserLitmus
  open Lexing
}

let digits = ['0' - '9']
let alpha = ['a' - 'z']
let empty = ['\t' ' ' '\r']
let register = 'E' ['A' - 'D'] 'X'

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
  | ";" { Semi }
  | "|" { Pipe }
  | "(" { LPar }
  | ")" { RPar }
  | "/\\" { And }
  | "=" { Equals }
  | "MOV" { Mov }
  | "MFENCE" { MFence }
  | "exists" { Exists }
  | alpha+ as var { Shared var }
  | register as reg { Reg reg }
  | digits+ as num { Int (int_of_string num) }
  | "[" (alpha+ as var) "]" { Shared var }
  | "$" (digits+ as num) { Int (int_of_string num) }
  | empty* 'P' ['0'-'9'] [^'\n']+ { lexer lexbuf }
  | (digits+ as tid) ':' (register as reg) {
      ThreadedReg (int_of_string tid, reg)
    }
