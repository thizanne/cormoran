open Batteries

type update = {
  var : Sym.t;
  origin : Program.thread_id;
  destinations : Program.thread_id list;
}

module type Outer = sig
  type t
  val bottom : t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init : Program.var Program.t -> t
  val transfer : Operation.t -> t -> t
  val join : t -> t -> t
  val widening : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init : (Sym.t * int option) list -> t (* TODO: best input type *)
  val join : t -> t -> t
  val meet : t -> t -> t
  val meet_cons : Sym.t Program.condition -> t -> t
  val widening : t -> t -> t
  val fold : Sym.t -> Sym.t -> t -> t (* dest <- source *)
  val expand : Sym.t -> Sym.t -> t -> t (* source -> dest *)
  val drop : Sym.t -> t -> t
  val add : Sym.t -> t -> t
  val assign_expr :
    Sym.t ->
    Sym.t Program.expression ->
    t ->
    t
  val print : 'a IO.output -> t -> unit
end

module type ConsistencyAbstraction = sig
  type t
  val compare : t -> t -> int
  val tid_is_consistent : t -> Program.thread_id -> bool
  val write : t -> Program.thread_id -> Sym.t -> t
  val init : Program.var Program.t -> t
  val make_update : t -> update -> t
  val get_mop_updates : t -> Program.thread_id -> Sym.t -> update list list
  val print : 'a IO.output -> t -> unit
end
