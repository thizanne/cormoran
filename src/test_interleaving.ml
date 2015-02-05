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
  "order", (module Order);
  "mark", (module Mark);
  "concrete", (module Concrete);
]

let speclist =
  speclist
  |> List.map (fun (a, b, c) -> (a, b, " " ^ c))
  |> Arg.align

module D = (val List.assoc !domain domains)

let print_cfg file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, _cond = Parse.parse use_litmus lexbuf in
    let g = Cfg.make program in
    let module Analyze = Interleaving.Make (D) in
    let _data = Analyze.analyze (fun _ -> D.init program) g in
    Cfg.Dot.output_graph stdout g
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist print_cfg
    "Prints the Control Flow Graph of a program. Options available:"
