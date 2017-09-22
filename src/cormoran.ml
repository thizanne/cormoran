open Batteries
open Cmdliner

let info =
  let doc = "Analyses a program under TSO." in
  let man = [
    `S "BUGS";
    `P "Probably.";
  ] in
  Term.info "cormoran" ~version:"0.1" ~doc ~man

let main param =
  try
    let module P = Param in
    let (program, properties) =
      FrontEnd.parse_source param in
    let module Analysis = (val Analysis.get_analysis param) in
    let control = Analysis.get_control program in
    let data = Analysis.analyse param program control in
    (* Printf.printf "done.\nChecking properties...\n"; *)
    List.iteri
      (fun i prop ->
         if Analysis.check_property prop control data
         then
           Printf.printf "\x1b[32mProperty %d verified." (i + 1)
         else
           Printf.printf "\x1b[31mProperty %d could not be verified." (i + 1);
         Printf.printf "\x1b[39;49m\n" (* reset colors *)
      )
      properties;
    if param.Param.print_result
    then Analysis.print_result IO.stdout program control data;
    Printf.printf "Exporting graph\n%!";
    Analysis.export_graph param control data
  with
  | Error.Error e ->
    prerr_endline @@ Error.to_string e

let main_term =
  Term.(pure main $ Param.cmdliner_term ())

let () = match Term.eval ~catch:false (main_term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
