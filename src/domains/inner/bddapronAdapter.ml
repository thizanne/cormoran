open Apron
open Bddapron
open Batteries
open Printf
open Util

module T = TypedAst
module Ty = Types
module L = Location

module BddDomain = Bdddomain1

module Make (N : ApronAdapter.Numerical) = struct

  type t = (string, N.t) BddDomain.t

  let apron_man =
    N.manager

  let man =
    BddDomain.make_man apron_man

  let cudd = Cudd.Man.make_v ()

  let () =
  Cudd.Man.print_limit := 200;
  Cudd.Man.set_gc 10000
    (fun () -> printf "@.CUDD GC@.")
    (fun () -> printf "@.CUDD REORDER@.")

  let empty_env =
    Env.make ~symbol:Env.string_symbol ~relational:true cudd

  let cond =
    Cond.make ~symbol:Env.string_symbol cudd

  let is_bottom abstr = BddDomain.is_bottom man abstr

  let equal abstr1 abstr2 = BddDomain.is_eq man abstr1 abstr2

  let init =
    BddDomain.top man empty_env

  let rec expr_int env (exp : int Domain.inner_expression) =
    match exp with
    | T.Int n ->
      Expr1.Apron.cst env cond @@ Coeff.s_of_int n.L.item
    | T.Var v ->
      Expr1.Apron.var env cond @@ Sym.name v.L.item.T.var_spec
    | T.Unop ({ L.item = T.Neg; _ }, exp) ->
      Expr1.Apron.negate cond @@ expr_int env exp.L.item
    | T.Binop (op, exp1, exp2) ->
      match op.L.item with
      | T.Add ->
        Expr1.Apron.add cond
          (expr_int env exp1.L.item)
          (expr_int env exp2.L.item)
      | T.Sub ->
        Expr1.Apron.sub cond
          (expr_int env exp1.L.item)
          (expr_int env exp2.L.item)
      | T.Mul ->
        Expr1.Apron.mul cond
          (expr_int env exp1.L.item)
          (expr_int env exp2.L.item)
      | T.Div ->
        Expr1.Apron.div cond
          (expr_int env exp1.L.item)
          (expr_int env exp2.L.item)

  let rec expr_bool env (exp : bool Domain.inner_expression) =
    match exp with
    | T.Bool b ->
      Expr1.Bool.of_bool env cond b.L.item
    | T.Var v ->
      Expr1.Bool.var env cond @@ Sym.name v.L.item.T.var_spec
    | T.Unop ({ L.item = T.Not; _ }, exp) ->
      Expr1.Bool.dnot cond @@ expr_bool env exp.L.item
    | T.Binop (op, exp1, exp2) ->
      match op.L.item with
      | T.And ->
        Expr1.Bool.dand cond
          (expr_bool env exp1.L.item)
          (expr_bool env exp2.L.item)
      | T.Or ->
        Expr1.Bool.dor cond
          (expr_bool env exp1.L.item)
          (expr_bool env exp2.L.item)
      | T.Eq ->
        Expr1.Apron.(
          eq cond @@
          sub cond (expr_int env exp1.L.item) (expr_int env exp2.L.item)
        )
      | T.Neq ->
        Expr1.Bool.dnot cond @@
        Expr1.Apron.(
          eq cond @@
          sub cond (expr_int env exp1.L.item) (expr_int env exp2.L.item)
        )
      | T.Gt ->
        Expr1.Apron.(
          sup cond @@
          sub cond (expr_int env exp1.L.item) (expr_int env exp2.L.item)
        )
      | T.Lt ->
        Expr1.Apron.(
          sup cond @@
          sub cond (expr_int env exp2.L.item) (expr_int env exp1.L.item)
        )
      | T.Ge ->
        Expr1.Apron.(
          supeq cond @@
          sub cond (expr_int env exp1.L.item) (expr_int env exp2.L.item)
        )
      | T.Le ->
        Expr1.Apron.(
          supeq cond @@
          sub cond (expr_int env exp2.L.item) (expr_int env exp1.L.item)
        )

  let join = BddDomain.join man

  let meet = BddDomain.meet man

  let meet_cons cons abstr =
    let env = BddDomain.get_env abstr in
    BddDomain.meet_condition man cond abstr (expr_bool env cons)

  let assign_expr :
    type a. a Domain.inner_var -> a Domain.inner_expression -> _ -> _ =
    fun var exp abstr ->
      let env = BddDomain.get_env abstr in
      let exp = match var.T.var_type with
        | Ty.Int -> Expr1.Apron.to_expr @@ expr_int env exp
        | Ty.Bool -> Expr1.Bool.to_expr @@ expr_bool env exp in
      BddDomain.assign_lexpr ~relational:true man cond abstr
        [Sym.name var.T.var_spec]
        [exp]
        None

  let widening abstr1 abstr2 =
    BddDomain.widening man abstr1 abstr2

  let typ_of_var : type a. (a, _) T.var  -> _ =
    function { T.var_type; _ } -> match var_type with
      | Ty.Int -> `Int
      | Ty.Bool -> `Bool

  let add var abstr =
    let name = Sym.name var.T.var_spec in
    let typ = typ_of_var var in
    let env = BddDomain.get_env abstr in
    let env = Env.add_vars env [name, typ] in
    BddDomain.change_environment man abstr env

  let drop :
    type a. a Domain.inner_var -> _ -> _ =
    fun var abstr ->
      let env = BddDomain.get_env abstr in
      let env = Env.remove_vars env [Sym.name var.T.var_spec] in
      BddDomain.change_environment man abstr env

  let swap :
    type t. t Domain.inner_var -> t Domain.inner_var -> _ =
    fun a b abstr ->
      let a_name = Sym.name a.T.var_spec in
      let b_name = Sym.name b.T.var_spec in
      let env = BddDomain.get_env abstr in
      let make_exp name = match a.T.var_type with
        (* b should have the same type *)
        | Ty.Int -> Expr1.Apron.(to_expr @@ var env cond name)
        | Ty.Bool -> Expr1.Bool.(to_expr @@ var env cond name)
      in
      BddDomain.assign_lexpr ~relational:true man cond abstr
        [a_name; b_name] [make_exp b_name; make_exp a_name] None

  let fold dest source abstr =
    drop source @@ join abstr (swap dest source abstr)

  let expand source dest abstr =
    let abstr = add dest abstr in
    meet abstr (swap dest source abstr)

  let print output abstr =
    let fmt = Format.formatter_of_output output in
    BddDomain.print fmt abstr;
    Format.pp_print_flush fmt ()
end

module Polka = Make (struct
    type t = Polka.loose Polka.t
    let manager = Polka.manager_alloc_loose ()
end)

module Oct = Make (struct
    type t = Oct.t
    let manager = Oct.manager_alloc ()
end)
