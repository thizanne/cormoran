open Error

let prgm_file = open_in Sys.argv.(1)

let () =
  let program = Parser.program Lexer.lexer (Lexing.from_channel prgm_file) in
  try
    let (_ : Syntax.TypedProgram.t) = Typing.type_program program in
    print_endline "Type check ok"
  with
    | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li
