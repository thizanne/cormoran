open Batteries
open Printf
open Util
open Flow

let edge_label {Operation.thread; op} =
  let output = IO.output_string () in
  Int.print output thread;
  String.print output ":";
  begin match op with
    | Operation.Identity ->
      String.print output "Id"
    | Operation.MFence ->
      String.print output "MFence"
    | Operation.Filter c ->
      PrintAst.print_condition output c
    | Operation.Assign (x, _var_type, e) ->
      Printf.fprintf output "%a := %a"
        Symbol.print x
        PrintAst.print_expression e
  end;
  IO.close_out output

module Dot (R : Analysis.Result) = Graph.Graphviz.Dot (
  struct
    include Flow.G

    let vertex_attributes v =
      let label v =
        print_to_string R.Domain.print (R.data v)
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
          sprintf "
<TABLE BORDER=\"0\" ALIGN=\"CENTER\">
<TR><TD BORDER=\"0\">%s</TD></TR>
<TR><TD BORDER=\"1\" BGCOLOR=\"white\">
<FONT POINT-SIZE=\"12\"> %s </FONT>
</TD>
</TR>
</TABLE>"
            (print_to_string (List.print ~first:"" ~last:"" Int.print) v)
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
      "\"" ^ (print_to_string (List.print Int.print) v) ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)
