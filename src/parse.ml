let parse_litmus lexbuf =
  LexerLitmus.drop_prelude lexbuf;
  ParserLitmus.program LexerLitmus.lexer lexbuf

let parse_imp lexbuf =
  Parser.program Lexer.lexer lexbuf
  |> Typing.type_program,
  []

let parse use_litmus =
  if !use_litmus
  then parse_litmus
  else parse_imp