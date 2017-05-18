{
  open Parser
  open Error
  open Lexing
}

let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let empty = ['\t' ' ' '\r']

let alpha = lower | upper
let id_char = alpha | digit | "_"

rule lexer = parse
  | "//" [^'\n']* { lexer lexbuf }
  | "#" [^'\n']* { SharpLine }
  | "/*" { comment 1 lexbuf }
  | eof { Eof }
  | '\n' { Lexing.new_line lexbuf; lexer lexbuf }
  | empty+ { lexer lexbuf }
  | "(" { LPar }
  | ")" { RPar }
  | "{" { LCurly }
  | "}" { RCurly }
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Times }
  | "/" { Divide }
  | "," { Comma }
  | "=" { Eq }
  | "<>" | "!=" { Neq }
  | "<" { Lt }
  | "<=" { Le }
  | ">" { Gt }
  | ">=" { Ge }
  | "not" | "!" { Not }
  | "or" | "||" { Or }
  | "and" | "&&" { And }
  | ";" { Semicolon }
  | "int" { IntType }
  | "bool" { BoolType }
  | "if" { If }
  | "while" { While }
  | "for" { For }
  | "pass" { Pass }
  | "mfence" { MFence }
  | "true" { Bool true }
  | "false" { Bool false }
  | "label" { Label }
  | "@" { At }
  | ":" { Colon }
  | digit+ as n { Int (int_of_string n) }
  | (lower id_char*) as x { Id x }

and comment depth = parse
  | "/*" { comment (depth + 1) lexbuf }
  | "*/" {
    if depth = 1 then lexer lexbuf
    else comment (depth - 1) lexbuf
  }
  | eof {
    raise (Error {
      error = LexingError;
      err_loc = {
        Location.startpos = lexeme_start_p lexbuf;
        Location.endpos = lexeme_start_p lexbuf;
      };
      err_msg = "Non closed commentary";
    })
  }
  | '\n' { new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }
