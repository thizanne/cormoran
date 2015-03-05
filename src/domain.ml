open Batteries

module type Outer = sig
  type t

  val bottom : t
  val equal : t -> t -> bool

  val init : Syntax.program -> t
  val transfer : t -> Flow.Operation.t -> t
  val join : t -> t -> t

  val satisfies : (string * int) list -> t -> bool

  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t

  val is_bottom : t -> bool

  val equal : t -> t -> bool

  val init : Syntax.program -> t

  val join : t -> t -> t
  val meet : t -> t -> t

  val join_array : t array -> t

  val meet_cons : t -> Constraint.t -> t
  val meet_cons_array : t -> Constraint.t array -> t

  val assign_expr :
    t -> ?dest:t option ->
    Expression.var -> Expression.t ->
    t

  val assign_expr_array :
    t -> ?dest: t option ->
    Expression.var array -> Expression.t array ->
    t

  val print : 'a IO.output -> t -> unit

end
