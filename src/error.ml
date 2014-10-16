open Printf

type t =
  | LexingError
  | SyntaxError
  | TypeError
  | NameError

exception Error of (t * Lexing.position * Lexing.position * string) list

let name_of_error = function
  | LexingError -> "Lexing error"
  | SyntaxError -> "Syntax error"
  | TypeError -> "Type error"
  | NameError -> "Name error"

let msg_of_error (e, startpos, endpos, msg) =
  let open Lexing in
  sprintf "%s:\n\tFrom line %d, column %d to line %d, column %d\n\t%s"
    (name_of_error e)
    startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
    endpos.pos_lnum (endpos.pos_cnum - endpos.pos_bol)
    msg
