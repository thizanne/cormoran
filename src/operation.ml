open Batteries

type t =
  | Identity
  | MFence
  | Filter of TypedAst.program_condition
  | Assign :
        't TypedAst.program_var *
        't TypedAst.program_expression ->
    t

let compare = Pervasives.compare

let rec print output = function
  | Identity -> String.print output "Identity"
  | MFence -> String.print output "MFence"
  | Filter c ->
    PrintAst.print_prog_expr output c
  | Assign (var, exp) ->
    Printf.fprintf output
      "%a := %a"
      PrintAst.(program_var_printer.f) var
      PrintAst.print_prog_expr exp
