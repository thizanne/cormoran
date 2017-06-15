open Batteries

module Make (D : Domain.Common) = struct

  module DotParam
      (G : Graph.Sig.P)
      (Print : sig
         val vertex : 'a IO.output -> G.V.t -> unit
         val edge : 'a IO.output -> G.E.label -> unit
       end)
      (Data : sig
         val data : G.V.t -> D.t
       end)
  =
  struct
    include G

    let vertex_attributes v =
      let label v =
        Printf.sprintf2 "%a" D.print (Data.data v)
        |> String.replace_chars
          (function
            | '\n' -> "<BR/>"
            | '>' -> "&gt;"
            | '<' -> "&lt;"
            | c -> String.make 1 c)
      in [
        `Shape `Box;
        `Fillcolor 0xdddddd;
        `Style `Filled;
        `Style `Rounded;
        `HtmlLabel (
          Printf.sprintf2 " \
<TABLE BORDER=\"0\" ALIGN=\"CENTER\"> \
<TR><TD BORDER=\"0\">%a</TD></TR> \
<TR><TD BORDER=\"1\" BGCOLOR=\"white\"> \
<FONT POINT-SIZE=\"12\"> %s </FONT> \
</TD> \
</TR> \
</TABLE>"
            Print.vertex v
            (label v)
        )
      ]

    let edge_attributes (_, e, _) = [
      `Label (Printf.sprintf2 "%a" Print.edge e);
      `Fontsize 12;
      `Arrowsize 0.5;
    ]

    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_name v =
      "\"" ^ (Printf.sprintf2 "%a" Print.vertex v) ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end

  let output_dot out data g =
    let module Param =
      DotParam
        (Control.ProgramStructure.Graph)
        (struct
          let vertex = Control.State.print
          let edge output (thread_id, op) =
            Printf.fprintf output "%d :: %a"
              thread_id Operation.print op
        end)
        (struct let data = data end) in
    let module Dot = Graph.Graphviz.Dot (Param) in
    Dot.fprint_graph
      (Format.formatter_of_output out)
      g.Control.ProgramStructure.graph

  let output_thread_dot out data g =
    let module Param =
      DotParam
        (Control.ThreadStructure.Graph)
        (struct
          let vertex = Control.Label.print
          let edge = Operation.print
        end)
        (struct let data = data end) in
    let module Dot = Graph.Graphviz.Dot (Param) in
    Dot.fprint_graph
      (Format.formatter_of_output out)
      g.Control.ThreadStructure.graph

  (* FIXME : this probably doesn't work if the filename from the user
     specifies a directory *)
  (* TODO : This should be factorised instead of repeating
     interleaving and modular everywhere *)

  let export_dirname = "_export"

  let create_export_dir () =
    try Unix.mkdir export_dirname 0o755 (* RWE for user, RE for others *)
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let dot_filename basename =
    let filename = basename ^ ".dot" in
    Filename.concat export_dirname filename

  let img_filename basename extension =
    let filename = Printf.sprintf "%s.%s" basename extension in
    Filename.concat export_dirname filename

  let final_filename basename extension =
    Printf.sprintf "%s.%s" basename extension

  let thread_basename basename tid =
    Printf.sprintf "%s__T%d" basename tid

  let invoke_dot basename extension =
    match
      Printf.ksprintf Unix.system
        "dot -o %s -T %s %s"
        (Filename.quote @@ img_filename basename extension)
        (Filename.quote extension)
        (Filename.quote @@ dot_filename basename)
    with
    | Unix.WEXITED 0 -> ()
    | _ -> prerr_endline "Invocation of dot failed. Dotfile still available."

  let export_graph basename extension control data =
    File.with_file_out (dot_filename basename)
      (fun out -> output_dot out data control);
    invoke_dot basename extension

  let export_interleaving_graph filename control data =
    create_export_dir ();
    let basename = Filename.remove_extension filename in
    let extension =
      (* Remove the period *)
      String.lchop (Filename.extension filename) in
    export_graph basename extension control data;
    IO.copy
      (File.open_in (img_filename basename extension))
      (File.open_out (final_filename basename extension))

  let export_thread_graph basename extension tid thread_control thread_data =
    let basename = thread_basename basename tid in
    File.with_file_out (dot_filename basename)
      (fun out -> output_thread_dot out thread_data thread_control);
    invoke_dot basename extension;
    IO.copy
      (File.open_in (img_filename basename extension))
      (File.open_out (final_filename basename extension))

  let print_thread_exports basename extension output thread_number =
    Enum.print
      ~first:"" ~last:"" ~sep:" "
      (fun output tid -> String.print output
          (Filename.quote @@
           img_filename (thread_basename basename tid) extension))
      output
      (0 --^ thread_number)

  let collate_thread_exports thread_number basename extension =
    match
      Printf.ksprintf2 Unix.system
        "convert %a +append %s"
        (print_thread_exports basename extension)
        thread_number
        (Filename.quote @@ final_filename basename extension)
    with
    | Unix.WEXITED 0 -> ()
    | _ ->
      prerr_endline
        "Invocation of convert failed. Source images still available."

  let export_modular_graph filename controls all_data =
    create_export_dir ();
    let basename = Filename.remove_extension filename in
    let extension =
      (* Remove the period *)
      String.lchop (Filename.extension filename) in
    List.iter2i
      (export_thread_graph basename extension) controls all_data;
    collate_thread_exports (List.length controls) basename extension
end
