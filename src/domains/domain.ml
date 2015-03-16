open Batteries

module type Outer = sig
  type t

  val bottom : t
  val equal : t -> t -> bool

  val init : Program.t -> t
  val transfer : t -> Cfg.Operation.t -> t
  val join : t -> t -> t

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

  val join_array : t array -> t

  val meet_cons : t -> Program.condition -> t
  val meet_cons_array : t -> Program.condition array -> t

  val assign_expr :
    t -> ?dest:t option ->
    Program.var -> Program.expression ->
    t

  val assign_expr_array :
    t -> ?dest: t option ->
    Program.var array -> Program.expression array ->
    t

  val print : 'a IO.output -> t -> unit

end
