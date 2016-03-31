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
