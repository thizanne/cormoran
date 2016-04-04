open Batteries

module Label : sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val initial : t
  val succ : t -> t
  val enum : initial:t -> final:t -> t Enum.t
  val compare : t -> t -> int
  val print : 'a IO.output -> t -> unit
end

module State : sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val empty : t
  val add_label : Label.t -> t -> t
  val from_label_list : Label.t list -> t
  val tid_label : t -> Source.thread_id -> Label.t
  val is_initial : t -> bool
  val initial : int -> t
  val compare : t -> t -> int
  val print : 'a IO.output -> t -> unit
end

module ThreadStructure : sig
  module Graph : Graph.Sig.P
    with type V.t = Label.t
     and type V.label = Label.t
     and type E.t = Label.t * Operation.t * Label.t
     and type E.label = Operation.t

  type t = private {
    graph : Graph.t;
    labels : Label.t Sym.Map.t;
    final : Label.t;
  }

  val of_thread : TypedAst.thread -> t
end

module ProgramStructure : sig
  module Graph : Graph.Sig.P
    with type V.t = State.t
     and type V.label = State.t
     and type E.t = State.t * (Source.thread_id * Operation.t) * State.t
     and type E.label = Source.thread_id * Operation.t

  type t = private {
    graph : Graph.t;
    labels : Label.t Sym.Map.t list;
    final : State.t;
  }

  val of_program : TypedAst.program -> t
end
