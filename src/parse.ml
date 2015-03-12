open Batteries

(*
let parse_litmus lexbuf =
  LexerLitmus.drop_prelude lexbuf;
  ParserLitmus.program LexerLitmus.lexer lexbuf
*)

let parse_litmus lexbuf =
  (* TODO: update litmus parsing *)
  Error.not_implemented_msg_error
    "Litmus parsing not yet implemented for the new interface"

let parse_imp lexbuf =
  Parser.program Lexer.lexer lexbuf
  |> Typing.type_program,
  []

let parse use_litmus =
  if !use_litmus
  then parse_litmus
  else parse_imp
