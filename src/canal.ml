open Error
open Printf

let use_litmus = ref false
let cond_check = ref true
let domain = ref "order"

let speclist = [
  "--litmus", Arg.Set use_litmus, "Use litmus syntax";
  "--domain", Arg.Set_string domain, "Domain: order (default), mark, concrete";
  "--no-cond", Arg.Clear cond_check, "Litmus: don't check final condition";
]

let domains : (string * (module Domain.Domain)) list = [
  "top", (module Top);
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
  Array.map
    (fun t -> Array.length t.ins)
    program.threads
  |> Array.to_list

let analyze file =
  printf "Analysing file %s...\n" file;
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, cond = Parse.parse use_litmus lexbuf in
    let module D = (val List.assoc !domain domains) in
    let module Analyzer = Linear.Make (D) in
    let result = Analyzer.analyze program in
    if !use_litmus && !cond_check then
      if D.satisfies cond @@
        Hashtbl.find result (last_point program)
      then print_endline "Condition satisfied"
      else print_endline "Condition not satisfied"
    else Analyzer.print result
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist analyze "Analyze a program. Options available:"
