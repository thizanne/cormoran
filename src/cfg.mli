open Batteries
open Graph

module State : sig
  type t = Program.control_state

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

val of_program : Program.t -> G.t
