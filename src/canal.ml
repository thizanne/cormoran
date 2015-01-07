open Error
open Printf

let litmus = ref false
let cond_check = ref true
let domain = ref "order"

let speclist = [
  "--litmus", Arg.Set litmus, "Use litmus syntax";
  "--domain", Arg.Set_string domain, "Use the marked points domain";
  "--no-cond", Arg.Clear cond_check, "Litmus: don't check final condition";
]

let domains : (string * (module Domain.Domain)) list = [
  "order", (module Order);
  "mark", (module Mark);
  "concrete", (module Concrete);
]

let speclist =
  speclist
  |> List.map (fun (a, b, c) -> (a, b, " " ^ c))
  |> Arg.align

let last_point program =
  let open Syntax.TypedProgram in
  List.map
    (fun t -> List.length t.ins)
    program.threads

let analyse file =
  printf "Analysing file %s...\n" file;
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, cond =
      if !litmus then begin
        LexerLitmus.drop_prelude lexbuf;
        ParserLitmus.program LexerLitmus.lexer lexbuf
      end
      else Parser.program Lexer.lexer lexbuf
           |> Typing.type_program,
           [] in
    let module D = (val List.assoc !domain domains) in
    let module Analyser = Interleaving.Make (D) in
    let result = Analyser.analyse program in
    if !litmus && !cond_check then
      if D.satisfies cond @@
        Hashtbl.find result (last_point program)
      then print_endline "Condition satisfied"
      else print_endline "Condition not satisfied"
    else Analyser.print result
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist analyse "Analyse a program. Options available:"
