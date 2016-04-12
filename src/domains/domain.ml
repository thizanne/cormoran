open Batteries

module T = TypedAst

module type Common = sig
  type t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

module type Outer = sig
  include Common
  val bottom : t
  val init : T.program -> t
  val transfer : Source.thread_id -> Operation.t -> t -> t
  val meet_cond : T.property_condition -> t -> t
  val widening : t -> t -> t
end

type 't inner_var = ('t, Sym.t) T.var
type 't inner_expression = ('t, Sym.t) T.expression

module type Inner = sig
  include Common
  val init :
    (int inner_var * int option) list ->
    (bool inner_var * bool option) list ->
    t
  val meet : t -> t -> t
  val meet_cons : bool inner_expression -> t -> t
  val widening : t -> t -> t
  val fold : 't inner_var -> 't inner_var -> t -> t (* dest <- source *)
  val expand : 't inner_var -> 't inner_var -> t -> t (* source -> dest *)
  val drop : _ inner_var -> t -> t
  val add : _ inner_var -> t -> t
  val assign_expr : 't inner_var -> 't inner_expression -> t -> t
end
