open Batteries

module State : sig
  type t = Program.Control.State.t

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
  program : Program.var Program.t;
  graph : G.t;
  labels : Program.Control.Label.t Sym.Map.t array;
  final_state : Program.Control.State.t;
}

val of_program : Program.var Program.t -> t
