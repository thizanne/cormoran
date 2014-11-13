open Error
open Printf

let litmus = ref false

let speclist = [
  "--litmus", Arg.Set litmus, "Use litmus syntax";
]

let analyse file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program =
      if !litmus then begin
        LexerLitmus.drop_prelude lexbuf;
        ParserLitmus.program LexerLitmus.lexer lexbuf
      end
      else Parser.program Lexer.lexer lexbuf |> Typing.type_program in
    let module Analyser = Interleaving.Make (Marked) in
    let result = Analyser.analyse program in
    Analyser.print result
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist analyse "Analyse a program. Options available:"
