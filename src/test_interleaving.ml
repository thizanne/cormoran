open Batteries
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
  "Domain: polka (default), oct, order, concrete, top";

  "--no-cond",
  Arg.Clear cond_check,
  "Litmus: don't check final condition";
]

let speclist =
  speclist
  |> List.map (fun (a, b, c) -> (a, b, " " ^ c))
  |> Arg.align

let domains : (string * (module Domain.Outer)) list = [
  "polka", (module Abstract.Make (ApronAdapter.Polka));
  "oct", (module Abstract.Make (ApronAdapter.Oct));
  "top", (module Top);
  "order", (module Abstract.Make (InnerConcrete));
  "concrete", (module Concrete);
]

let print_cfg file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, _cond = Parse.parse use_litmus lexbuf in
    let module D = (val List.assoc !domain domains) in
    let g = Cfg.of_program program in
    let module Analysis = Interleaving.Make (D) in
    let analyze = Analysis.make_analyze g in
    let module Result = struct module Domain = D let data = analyze end in
    let module Dot = ExportCfg.Dot (Result) in
    Dot.output_graph Legacy.stdout g.Cfg.graph
  with
  | Error e -> prerr_endline (to_string e)

let () =
  Arg.parse speclist print_cfg
    "Prints the Control Flow Graph of a program. Options available:"
