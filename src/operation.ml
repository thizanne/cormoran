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

let print = function
  (* FIXME : this sucks. Do it properly. *)
  | Identity -> print_string "Identity"
  | MFence -> print_string "MFence"
  | Filter _ -> print_string "Filter _"
  | Assign _ -> print_string "Assign _"
