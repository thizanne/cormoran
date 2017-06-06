open Batteries

let edge_label (thread_id, op) =
  let output = IO.output_string () in
  Printf.fprintf output "%d :: %a"
    thread_id
    Operation.print op;
  IO.close_out output

module Dot (D : Domain.ProgramState) = struct

  module DotParam
      (Data : sig
         val data : Control.State.t -> D.t
       end)
  =
  struct
    include Control.ProgramStructure.Graph

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
          Printf.sprintf " \
<TABLE BORDER=\"0\" ALIGN=\"CENTER\"> \
<TR><TD BORDER=\"0\">%s</TD></TR> \
<TR><TD BORDER=\"1\" BGCOLOR=\"white\"> \
<FONT POINT-SIZE=\"12\"> %s </FONT> \
</TD> \
</TR> \
</TABLE>"
            (Printf.sprintf2 "%a" Control.State.print v)
            (label v)
        )
      ]

    let edge_attributes (_, e, _) = [
      `Label (edge_label e);
      `Fontsize 12;
      `Arrowsize 0.5;
    ]

    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_name v =
      "\"" ^ (Printf.sprintf2 "%a" Control.State.print v) ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end

  let output_graph out data g =
    let module Param = DotParam (struct let data = data end) in
    let module Dot = Graph.Graphviz.Dot (Param) in
    Dot.fprint_graph (Format.formatter_of_output out) g.Control.ProgramStructure.graph
end
