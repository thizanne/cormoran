open Batteries

module ProgramG : Graph.Sig.P
  with type V.t = Control.State.t
   and type V.label = Control.State.t
   and type E.t = Control.State.t * Operation.t * Control.State.t
   and type E.label = Operation.t

type t = private {
  program : TypedAst.program;
  graph : ProgramG.t;
  labels : Control.Label.t Sym.Map.t array;
  final_state : Control.State.t;
}

val of_program : TypedAst.program -> t
