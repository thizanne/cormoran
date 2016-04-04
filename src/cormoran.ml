open Batteries
open Cmdliner

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "cormoran" ~version:"0.1" ~doc ~man

let main domain widening_delay use_litmus sourcefile outputfile =
  try
    let module P = Param in
    let (program, properties) =
      P.Parse.parse_filename ~use_litmus sourcefile in
    let g = Control.ProgramStructure.of_program program in
    let module D = (val P.Domain.get domain) in
    let module Analysis = Interleaving.Make (D) in
    let init = D.init program in
    let data = Analysis.analyze g widening_delay init in
    let module Dot = ExportCfg.Dot (D) in
    let module Prop = Property.Make (D) in
    Dot.output_graph (P.Output.get_output outputfile) data g;
    (* Printf.printf "done.\nChecking properties...\n"; *)
    List.iteri
      (fun i prop ->
         Printf.printf "Property %d %s\n" (i + 1)
           (if Prop.satisfies prop g data
            then "verified."
            else "could not be verified (wrong, or domain too imprecise)."))
      properties
  with
  | Error.Error e ->
    prerr_endline @@ Error.to_string e

let main_term =
  let open Param.CommandTerm in
  Term.(pure main $ domain $ widening_delay $ use_litmus $ sourcefile $ outputfile)

let () = match Term.eval (main_term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
