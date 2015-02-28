{
  open Parser
  open Error
  open Lexing
}

let digits = ['0' - '9']
let alpha = ['a' - 'z']
let empty = ['\t' ' ' '\r']

rule lexer = parse
  | "//" [^'\n']* { lexer lexbuf }
  | "#" [^'\n']* { Sharp }
  | "/*" { comment 1 lexbuf }
  | eof { Eof }
  | '\n' { Lexing.new_line lexbuf; lexer lexbuf }
  | empty+ { lexer lexbuf }
  | "{" { LCurly }
  | "}" { RCurly }
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Times }
  | "/" { Divide }
  | "," { Comma }
  | "=" { Equal }
  | ":=" { Affect }
  | ";" { Semicolon }
  | "cmp" { Cmp }
  | "jnz" { Jnz }
  | "jz" { Jz }
  | "jmp" { Jmp }
  | "while" { While }
  | "label" { Label }
  | "local" { Local }
  | "pass" { Pass }
  | "mfence" { MFence }
  | digits+ as n { Int (int_of_string n) }
  | alpha (alpha | digits | "_")* as x { Id x }

and comment depth = parse
  | "/*" { comment (depth + 1) lexbuf }
  | "*/" {
    if depth = 1 then lexer lexbuf
    else comment (depth - 1) lexbuf
  }
  | eof {
    raise
    (Error
       [LexingError,
        lexeme_start_p lexbuf, lexeme_start_p lexbuf,
        "Non closed commentary"])
  }
  | '\n' { new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }
