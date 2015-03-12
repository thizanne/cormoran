open Batteries
open Graph

module State : sig
  type t = Syntax.control_state

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module Operation : sig
  type operation =
    | Identity
    | MFence
    | Filter of Syntax.condition
    | Assign of Syntax.var * Syntax.expression

  type t = {
    thread : int;
    op : operation;
  }

  val compare : t -> t -> int
  val default : t
end

module G : Graph.Sig.P
  with type V.t = State.t
   and type V.label = State.t
   and type E.t = State.t * Operation.t * State.t
   and type E.label = Operation.t

val of_program : Syntax.program -> G.t
