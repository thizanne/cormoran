open Batteries

type update = {
  var : Symbol.t;
  origin : Program.thread_id;
  destinations : Program.thread_id list;
}

module type Outer = sig
  type t

  val bottom : t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init : Program.var Program.t -> t
  val transfer : Cfg.Operation.t -> t -> t
  val join : t -> t -> t
  val widening : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init : Program.var Program.t -> t
  val join : t -> t -> t
  val meet : t -> t -> t
  val meet_cons : t -> Program.var_view Program.condition -> t
  val widening : t -> t -> t
  val assign_expr :
    t ->
    Program.thread_id -> (* thread_id of the variable *)
    Program.var ->
    Program.thread_id -> (* thread_id of the expression *)
    Program.var Program.expression ->
    t
  val print : 'a IO.output -> t -> unit
end

module type ConsistencyAbstraction = sig
  type t
  val compare : t -> t -> int
  val tid_is_consistent : t -> Program.thread_id -> bool
  val write : t -> Program.thread_id -> Symbol.t -> t
  val init : Program.var Program.t -> t
  val make_update : t -> update -> t
  val get_mop_updates : t -> Program.thread_id -> Symbol.t -> update list list
  val print : 'a IO.output -> t -> unit
end
