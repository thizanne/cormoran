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
  | '\n' { }
  | "{"
  | "}"
  | "[" alpha+ as var "]"
  | "$" digits+ as num
  | ":"
  | ","
  | ";"
  | "|"
  | "("
  | ")"
  | "/\\"
  | "="
  | "MOV"
  | "MFENCE"
  | "exists"
  | "EAX"
  | "EBX"
  | "ECX"
  | "EDX"
