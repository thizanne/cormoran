open Batteries

type t =
  | Identity
  | MFence of Control.thread_id
  | Filter of TypedAst.property_condition
  | Assign :
      Control.thread_id *
      't TypedAst.program_var *
      't TypedAst.program_expression ->
    t

let compare = Pervasives.compare

let default = Identity
