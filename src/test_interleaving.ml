open Error
open Printf

let use_litmus = ref false
let cond_check = ref true
let domain = ref "order"

let speclist = [
  "--litmus",
  Arg.Set use_litmus,
  "Use litmus syntax";

  "--domain",
  Arg.Set_string domain,
  "Domain: abstract (default), order, mark, concrete, top";

  "--no-cond",
  Arg.Clear cond_check,
  "Litmus: don't check final condition";
]

let speclist =
  speclist
  |> List.map (fun (a, b, c) -> (a, b, " " ^ c))
  |> Arg.align

let domains : (string * (module Domain.Domain)) list = [
  "abstract", (module Abstract);
  "top", (module Top);
  "order", (module Order);
  "mark", (module Mark);
  "concrete", (module Concrete);
]

let print_cfg file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, _cond = Parse.parse use_litmus lexbuf in
    let g = Cfg.make program in
    let module D = (val List.assoc !domain domains) in
    let module Analysis = Interleaving.Make (D) in
    let analyze = Analysis.make_analyze program in
    let module Result = struct module Domain = D let data = analyze end in
    let module Dot = Cfg.Dot (Result) in
    Dot.output_graph stdout g
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist print_cfg
    "Prints the Control Flow Graph of a program. Options available:"
