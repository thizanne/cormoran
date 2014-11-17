module type Domain = sig
  type t

  val empty : t
  val init : Syntax.TypedProgram.t -> t
  val transfer : t -> int -> Syntax.Typed.t -> t
  val union : t -> t -> t

  val satisfies : Condition.t -> t -> bool

  val print : t -> unit
end
