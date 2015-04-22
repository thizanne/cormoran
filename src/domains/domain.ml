open Batteries

module type Outer = sig
  type t

  val bottom : t

  val equal : t -> t -> bool

  val init : Program.t -> t

  val transfer : t -> Cfg.Operation.t -> t

  val join : t -> t -> t

  val widening : t -> t -> t

  val satisfies : Program.Property.t -> t -> bool

  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t

  val is_bottom : t -> bool

  val equal : t -> t -> bool

  val init : Program.t -> t

  val join : t -> t -> t

  val meet : t -> t -> t

  val meet_cons : t -> Program.condition Program.threaded -> t

  val widening : t -> t -> t

  val assign_expr :
    t ->
    Program.var Program.threaded ->
    Program.expression Program.threaded ->
    t

  val print : 'a IO.output -> t -> unit
end
