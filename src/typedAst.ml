open Batteries

module L = Location
module Ty = Types
module C = Context

module ProgramExpression = Expression.Make (C.Visible)
type 't program_var = 't ProgramExpression.var

module PropertyExpression = Expression.Make (C.Sourced)
type property_condition = bool PropertyExpression.t

(*

let rec map_expr :
  type a.
  ('info1, 'info2) var_mapper ->
  ('info1, a) expression ->
  ('info2, a) expression =
  fun mapper exp ->
    match exp with
    | Int n -> Int n
    | Bool b -> Bool b
    | Var v -> Var (L.comap mapper.f v)
    | Unop (op, exp) ->
      Unop (op, map_expr_loc mapper exp)
    | Binop (op, exp1, exp2) ->
      Binop (
        op,
        map_expr_loc mapper exp1,
        map_expr_loc mapper exp2
      )

and map_expr_loc :
  type a.
  ('info1, 'info2) var_mapper ->
  ('info1, a) expression L.loc ->
  ('info2, a) expression L.loc
  =
  fun mapper ->
    L.comap (map_expr mapper)

let add_thread_info tid expr =
  let put_thread v = var_id_map (MT.create_some tid) v in
  let mapper = { f = put_thread } in
  map_expr mapper expr

type ('info1, 'info2) var_mapper = {
  f : 'a. ('info1, 'a) var -> ('info2, 'a) var
}

type ('info, 'acc) var_folder = {
  f : 'a. ('info, 'a) var -> 'acc -> 'acc
}

let rec fold_expr :
  type a. ('info, 'acc) var_folder -> 'acc -> ('info, a) expression -> 'acc =
  fun f acc exp ->
    match exp with
    | Int _ -> acc
    | Bool _ -> acc
    | Var v -> f.f v.L.item acc
    | Unop (_, exp) -> fold_expr f acc exp.L.item
    | Binop (_, exp1, exp2) ->
      fold_expr f (fold_expr f acc exp2.L.item) exp1.L.item
*)

type body =
  | Nothing
  | Pass
  | MFence
  | Label of Sym.t Location.loc
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign :
      't ProgramExpression.var Location.loc *
      't ProgramExpression.t Location.loc ->
    body
  | If of
      bool ProgramExpression.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      bool ProgramExpression.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      int ProgramExpression.var Location.loc * (* Indice *)
      int ProgramExpression.t Location.loc * (* From *)
      int ProgramExpression.t Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  locals : Sym.Set.t;
  body : body Location.loc;
}

type program = {
  initial : Env.constant Sym.Map.t;
  threads : thread list;
}
