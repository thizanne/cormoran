(*
open Batteries doesn't work with [@@deriving show]
see https://github.com/ocaml-batteries-team/batteries-included/issues/597
*)

open Printf

type t =
  | LexingError
  | SyntaxError
  | TypeError
  | NameError
  | NotImplementedError
      [@@deriving show]

exception Error of (t * Lexing.position * Lexing.position * string) list

let msg_of_error (e, startpos, endpos, msg) =
  let open Lexing in
  sprintf "%s:\n\tFrom line %d, column %d to line %d, column %d\n\t%s"
    (show e)
    startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
    endpos.pos_lnum (endpos.pos_cnum - endpos.pos_bol)
    msg
