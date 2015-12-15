open Batteries

module T = TypedAst

module type Outer = sig
  type t
  val bottom : t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init : T.program -> t
  val transfer : Operation.t -> t -> t
  val join : t -> t -> t
  val widening : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

module type Inner = sig
  type t
  type 't var = ('t, Sym.t) T.var
  type 't expression = ('t, Sym.t) T.expression
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val init :
    (int var * int option) list ->
    (bool var * bool option) list ->
    t
  val join : t -> t -> t
  val meet : t -> t -> t
  val meet_cons : bool expression -> t -> t
  val widening : t -> t -> t
  val fold : 't var -> 't var -> t -> t (* dest <- source *)
  val expand : 't var -> 't var -> t -> t (* source -> dest *)
  val drop : _ var -> t -> t
  val add : _ var -> t -> t
  val assign_expr : 't var -> 't expression -> t -> t
  val print : 'a IO.output -> t -> unit
end
