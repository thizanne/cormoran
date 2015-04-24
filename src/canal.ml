open Batteries
open Cmdliner

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "canal" ~version:"0.1" ~doc ~man

let main domain widening_delay use_litmus filename =
  let open Param in
  let (program, _) = Parse.parse_filename use_litmus filename in
  let g = Cfg.of_program program in
  let module D = (val Domain.get domain) in
  let module Analysis = Interleaving.Make (D) in
  let data = Analysis.analyze g widening_delay in
  let module Dot = ExportCfg.Dot (D) in
  Dot.output_graph IO.stdout data g

let main_term =
  let open Param.CommandTerm in
  Term.(pure main $ domain $ widening_delay $ use_litmus $ filename)

let () = match Term.eval (main_term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
