open Batteries

let parse_litmus lexbuf =
  LexerLitmus.drop_prelude lexbuf;
  ParserLitmus.program LexerLitmus.lexer lexbuf

let parse_imp lexbuf =
  Parser.program Lexer.lexer lexbuf
  |> Typing.type_program

let parse_lexbuf litmus =
  if litmus
  then parse_litmus
  else parse_imp

let get_lexbuf filename =
  (* filename is expected to be valid *)
  let open Lexing in
  let lexbuf = from_channel @@ open_in filename in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf

let parse_source param =
  parse_lexbuf param.Param.litmus @@ get_lexbuf param.Param.sourcefile
