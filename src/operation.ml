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

let print output = function
  | Identity -> String.print output "Identity"
  | MFence -> String.print output "MFence"
  | Filter c ->
    Printf.fprintf output
      "Filter (%a)"
      TypedAst.print_expr c
  | Assign (var, exp) ->
    Printf.fprintf output
      "Assign (%a, %a)"
      Sym.print var.TypedAst.var_sym
      TypedAst.print_expr exp
