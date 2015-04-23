open Batteries

module State : sig
  type t = Program.Control.State.t

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module Operation : sig
  type operation =
    | Identity
    | MFence
    | Filter of Program.condition
    | Assign of Program.var * Program.expression

  type t = operation Program.threaded

  val compare : t -> t -> int
  val default : t
end

module G : Graph.Sig.P
  with type V.t = State.t
   and type V.label = State.t
   and type E.t = State.t * Operation.t * State.t
   and type E.label = Operation.t

type t = private {
  program : Program.t;
  graph : G.t;
  labels : Program.Control.Label.t Symbol.Map.t array;
}

val of_program : Program.t -> t
