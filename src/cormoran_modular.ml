open Batteries
open Cmdliner

module T = TypedAst

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "cormoran" ~version:"0.1" ~doc ~man

let main _domain widening_delay use_litmus sourcefile =
  try
    let module P = Param in
    let (program, properties) =
      P.Parse.parse_filename ~use_litmus sourcefile in
    let control =
      List.map Control.ThreadStructure.of_thread program.T.threads in
    let module Inner = BddapronAdapter.Oct in
    let module Inner' = ApronAdapter.Oct in
    let module C = ControlAbstractionFromLabels in
    let module PSO = MarkThread.Make (Inner) (C) in
    let module SC = SequentialConsistency.Make (Inner) (C) in
    let module TA = SC in
    let module Analysis = Modular.ProgramAnalysis (TA) in
    let data = Analysis.analyse program control widening_delay widening_delay in
    let module Prop = Property.MakeModular (TA.StateAbstraction) (TA.Application) in
    (* Printf.printf "done.\nChecking properties...\n"; *)
    List.iteri
      (fun i prop ->
         Printf.printf "Property %d %s\n" (i + 1)
           (if Prop.satisfies prop control data
            then "verified."
            else "could not be verified (wrong, or domain too imprecise)."))
      properties
  with
  | Error.Error e ->
    prerr_endline @@ Error.to_string e

let main_term =
  let open Param.CommandTerm in
  Term.(pure main $ domain $ widening_delay $ use_litmus $ sourcefile)

let () =
  try
    match Term.eval (main_term, info) ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
    Failure s -> print_endline s; ignore s; print_endline "\n\nERREUR"
