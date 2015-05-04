open Batteries

module type Outer = sig
  type t

  val bottom : t

  val is_bottom : t -> bool

  val equal : t -> t -> bool

  val init : Program.t -> t

  (* TODO: reverse order of arguments *)
  val transfer : t -> Cfg.Operation.t -> t

  val join : t -> t -> t

  val widening : t -> t -> t

  val satisfies : Program.var Program.threaded Program.condition -> t -> bool

  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t

  val is_bottom : t -> bool

  val equal : t -> t -> bool

  val init : Program.t -> t

  val join : t -> t -> t

  val meet : t -> t -> t

  val meet_cons : t -> Program.var Program.threaded Program.condition -> t

  val widening : t -> t -> t

  val assign_expr :
    t ->
    Program.thread_id ->
    Program.var ->
    Program.var Program.expression ->
    t

  val print : 'a IO.output -> t -> unit
end
