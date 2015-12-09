open Batteries

module State : sig
  type t = Control.State.t

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module G : Graph.Sig.P
  with type V.t = State.t
   and type V.label = State.t
   and type E.t = State.t * Operation.t * State.t
   and type E.label = Operation.t

type t = private {
  program : TypedAst.program;
  graph : G.t;
  labels : Control.Label.t Sym.Map.t array;
  final_state : Control.State.t;
}

val of_program : TypedAst.program -> t
