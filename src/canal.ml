open Batteries
open Cmdliner

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "canal" ~version:"0.1" ~doc ~man

let make_analysis d =
  let module D = (val d : Domain.Outer) in
  let module A = Interleaving.Make (D) in
  A.make_analyze

let () =
  let module P = Param in
  let open P.CommandTerm in
  let open Term in
  let program = P.Parse.parse $ use_litmus $ filename in
  let program = pure fst $ program in
  let g = pure Cfg.of_program $ program in
  let d = P.Domain.get $ domain in
  let module Analysis = Interleaving.Make (D) in
  let analyze = Analysis.make_analyze g in
  let module Result = struct module Domain = D let data = analyze end in
  let module Dot = ExportCfg.Dot (Result) in
  Dot.output_graph Legacy.stdout g.Cfg.graph

let () =
  match Term.eval (Term.(pure failwith $ pure "lol"), info) with
  | `Error _ -> exit 1
  | `Ok s -> print_string s
  | _ -> exit 0
