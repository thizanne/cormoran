open Batteries

module T = TypedAst
module TS = Control.ThreadStructure

module type Common = sig
  type t
  val is_bottom : t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

module type ProgramState = sig
  include Common
  val bottom : t
  val top : T.program -> t
  val transfer : Source.thread_id -> Operation.t -> t -> t
  val meet_cond : T.property_condition -> t -> t
  val widening : t -> t -> t
end

module type ThreadState = sig
  include Common
  val bottom : T.program -> TS.t list -> Source.thread_id -> t
  val top : T.program -> TS.t list -> Source.thread_id -> t (* top but with empty buffers *)
  val meet_cond : T.property_condition -> t -> t
  val are_consistent : t list -> bool
  val widening : t -> t -> t
  val meet_label : Source.thread_id -> Control.Label.t -> t -> t
end

module type ControlAbstraction = sig
  type label = int
  type t
  val nb_threads : t -> int
  val alpha : t -> Source.thread_id -> Control.Label.t -> label
  val max_alpha : t -> Source.thread_id -> label
  val of_threads : TS.t list -> t
end

module type Interferences = sig
  type t
  val bottom : T.program -> TS.t list -> t
  val equal : t -> t -> bool
  val join : t -> t -> t
  val widening : t -> t -> t
  val print : 'a IO.output -> t -> unit
end

type 't inner_var = ('t, Sym.t) T.var
type 't inner_expression = ('t, Sym.t) T.expression

(* TODO: replace Labels with finite types *)

module type Inner = sig
  include Common
  module Env : sig
    type t
    val empty : t
    val add : _ inner_var -> t -> t
    val drop : _ inner_var -> t -> t
    val join : t -> t -> t (* Least common environment *)
    val add_label : Sym.t -> int -> t -> t (* int is abstract label max *)
    val drop_label : Sym.t -> t -> t
  end
  val bottom : t (* Bottom with no variable defined. Adding variables should keep it bottom. *)
  val top : t (* Top with no variable. Adding variables should keep it top. *)
  val get_env : t -> Env.t
  val change_env : Env.t -> t -> t
  val rename : 't inner_var -> 't inner_var -> t -> t (* old -> new *)
  val rename_label : Sym.t -> Sym.t -> t -> t (* old -> new *)
  val meet : t -> t -> t
  val unify : t -> t -> t (* Unify (meet) on least common environment *)
  val meet_cons : bool inner_expression -> t -> t
  val widening : t -> t -> t
  val fold : 't inner_var -> 't inner_var -> t -> t (* dest <- source *)
  val expand : 't inner_var -> 't inner_var -> t -> t (* source -> dest *)
  val assign_expr : 't inner_var -> 't inner_expression -> t -> t
  val set_label : Sym.t -> int -> t -> t (* int is abstract label affectation *)
  val meet_label : Sym.t -> int -> t -> t (* int is abstract label to meet with *)
  val assign_label : Sym.t -> Sym.t -> t -> t (* first becomes equal to second *)
end

module LiftEnvUtils (I : Inner) : sig
  include Inner
  val add : _ inner_var -> t -> t
  val drop :  _ inner_var -> t -> t
  val add_label : Sym.t -> int -> t -> t
  val drop_label : Sym.t -> t -> t
  val join_lce : t -> t -> t (* Join on least common environment *)
end
=
struct
  include I

  let lift f =
    fun abstr ->
      change_env (f @@ get_env abstr) abstr

  let add var =
    lift (Env.add var)

  let drop var =
    lift (Env.drop var)

  let add_label label label_max =
    lift (Env.add_label label label_max)

  let drop_label label =
    lift (Env.drop_label label)

  let join_lce x y =
    let lce = Env.join (get_env x) (get_env y) in
    let x_lce, y_lce = change_env lce x, change_env lce y in
    join x_lce y_lce
end
