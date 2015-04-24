open Batteries
open Printf

let use_litmus = ref false

let speclist = [
  "--litmus", Arg.Set use_litmus, "Use litmus syntax";
]

(*
(* Useless with only one arg in the speclist *)
let speclist =
  speclist
  |> List.map (fun (a, b, c) -> (a, b, " " ^ c))
  |> Arg.align
*)

module Dot = ExportCfg.Dot (Top)
let data _ = Top.bottom

let print_cfg file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, _cond = Parse.parse use_litmus lexbuf in
    let g = Cfg.of_program program in
    Dot.output_graph IO.stdout data g
  with
  | Error.Error e -> prerr_endline (Error.to_string e)

let () =
  Arg.parse speclist print_cfg
    "Prints the Control Flow Graph of a program. Options available:"
