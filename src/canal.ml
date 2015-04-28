open Batteries
open Cmdliner

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "canal" ~version:"0.1" ~doc ~man

let main domain widening_delay use_litmus sourcefile outputfile =
  let module P = Param in
  let (program, _) = P.Parse.parse_filename use_litmus sourcefile in
  let g = Cfg.of_program program in
  let module D = (val P.Domain.get domain) in
  let module Analysis = Interleaving.Make (D) in
  let data = Analysis.analyze g widening_delay in
  let module Dot = ExportCfg.Dot (D) in
  let module Prop = Property.Make (D) in
  Dot.output_graph (P.Output.get_output outputfile) data g;
  if Prop.satisfies g data
  then print_endline "Property verified."
  else print_endline "Property could not be verified: either it is wrong, or the domain is too imprecise."

let main_term =
  let open Param.CommandTerm in
  Term.(pure main $ domain $ widening_delay $ use_litmus $ sourcefile $ outputfile)

let () = match Term.eval (main_term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
