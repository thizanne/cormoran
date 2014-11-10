open Error
open Printf

let prgm_file = open_in Sys.argv.(1)

let () =
  try
    let program = Parser.program Lexer.lexer (Lexing.from_channel prgm_file) in
    let program = Typing.type_program program in
    let module Analyser = Interleaving.Make (Marked) in
    let result = Analyser.analyse program in
    Analyser.print result
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li
