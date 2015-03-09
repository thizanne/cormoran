open Batteries
open Error
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

module Dot = Cfg.Dot (Analysis.EmptyResult (Top))

let print_cfg file =
  try
    let lexbuf = Lexing.from_channel @@ open_in file in
    let program, _cond = Parse.parse use_litmus lexbuf in
    let g = Cfg.make program in
    Dot.output_graph stdout g
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li

let () =
  Arg.parse speclist print_cfg
    "Prints the Control Flow Graph of a program. Options available:"
