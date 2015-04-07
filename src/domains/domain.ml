open Batteries

module type Outer = sig
  type t

  val bottom : t

  val equal : t -> t -> bool

  val init : Program.t -> t

  val transfer : t -> Cfg.Operation.t -> t

  val join : t -> t -> t

  val widening : t -> t -> t

  (* This is only for compatibility reasons with some old code, and
     will be removed with a proper property verification *)
  val satisfies : (string * int) list -> t -> bool

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
