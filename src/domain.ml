open Batteries

module type Domain = sig
  type t

  val bottom : t
  val init : Syntax.TypedProgram.t -> t
  val transfer : t -> int -> Syntax.Typed.t -> t
  val join : t -> t -> t

  val satisfies : Constraint.t -> t -> bool

  val print : 'a IO.output -> t -> unit
end
