open Bddapron
open Batteries
open Printf
open Util

module T = TypedAst
module Ty = Types
module L = Location

module BddDomain = Mtbdddomain1

let typ_of_var : type a. (a, _) T.var  -> _ =
  function { T.var_type; _ } -> match var_type with
    | Ty.Int -> `Int
    | Ty.Bool -> `Bool

module Make (N : ApronAdapter.Numerical) = struct

  let cudd = Cudd.Man.make_v ()

  let () =
    Cudd.Man.print_limit := 1_000_000;
    Cudd.Man.set_gc 1_000_000
      Gc.full_major
      Gc.full_major

  let cond =
    Cond.make ~symbol:Bddapron.Env.string_symbol cudd

  module Env = struct
    (* TODO: Use Sym.t instead of string as the symbol type *)
    (* Inside this module, Env is Bddapron.Env *)
    type t = string Env.t

    let empty = Env.make
        ~bddsize:500
        ~symbol:Env.string_symbol
        ~relational:true
        cudd

    let bddap_var { T.var_spec; _ } =
      Sym.name var_spec

    let bddap_var_loc v =
      bddap_var v.L.item

    let label_var label =
      Sym.name label

    let add var env =
      let name = bddap_var var in
      let typ = typ_of_var var in
      Env.add_vars env [name, typ]

    let drop var env =
      Env.remove_vars env [bddap_var var]

    let join env1 env2 =
      Env.lce env1 env2

    let add_label label label_max env =
      let label_var = label_var label in
      let typ = `Bint (false, label_max) in
      Env.add_vars env [label_var, typ]

    let drop_label label env =
      Env.remove_vars env [label_var label]

    let typ_of_var = Env.typ_of_var
  end

  type t = (string, N.t) BddDomain.t

  let apron_man =
    N.manager

  let man =
    BddDomain.make_man apron_man

  let is_bottom abstr = BddDomain.is_bottom man abstr

  let equal abstr1 abstr2 =
    abstr1 == abstr2 ||
    BddDomain.is_eq man abstr1 abstr2

  let top =
    BddDomain.top man Env.empty

  let bottom =
    BddDomain.bottom man Env.empty

  let get_env abstr = BddDomain.get_env abstr

  let change_env env abstr = BddDomain.change_environment man abstr env

  let rename var_old var_new abstr =
    BddDomain.rename man abstr
      [Env.bddap_var var_old, Env.bddap_var var_new]

  let rename_label lbl_old lbl_new abstr =
    BddDomain.rename man abstr
      [Env.label_var lbl_old, Env.label_var lbl_new]

  (* Due to the absence of fold and expand in Bddapron, part of
     Domain.LiftEnvUtils is here to implement them *)

  let lift f =
    fun abstr ->
      change_env (f @@ get_env abstr) abstr

  let add var =
    lift (Env.add var)

  let drop var =
    lift (Env.drop var)

  let rec expr_int env (exp : int Domain.inner_expression) =
    match exp with
    | T.Int n ->
      Expr1.Apron.cst env cond @@ Apron.Coeff.s_of_int n.L.item
    | T.Var v ->
      Expr1.Apron.var env cond @@ Env.bddap_var_loc v
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
      Expr1.Bool.var env cond @@ Env.bddap_var_loc v
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

  let unify = BddDomain.unify man

  let meet_cons cons abstr =
    (* FIXME: meet with neq does not work properly *)
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
        [Env.bddap_var var]
        [exp]
        None

  let widening abstr1 abstr2 =
    BddDomain.widening man abstr1 abstr2

  let swap :
    (* TODO: probably sufficient to swap the dimensions of a and b in the env *)
    type t. t Domain.inner_var -> t Domain.inner_var -> _ =
    fun a b abstr ->
      let a_name = Env.bddap_var a in
      let b_name = Env.bddap_var b in
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

  let set_label lbl_sym new_value abstr =
    let env = BddDomain.get_env abstr in
    let label_var = Env.label_var lbl_sym in
    match Env.typ_of_var env label_var with
    | `Bint (signed, bound) ->
      let exp =
        Expr1.Bint.to_expr @@
        Expr1.Bint.of_int env cond
          (`Bint (signed, bound)) new_value in
      BddDomain.assign_lexpr ~relational:true man cond abstr
        [label_var] [exp] None
    | _ -> assert false

  let assign_label lbl_sym_dst lbl_sym_src abstr =
    let env = BddDomain.get_env abstr in
    let dst_var = Env.label_var lbl_sym_dst in
    let src_var = Env.label_var lbl_sym_src in
    let src_exp = Expr1.var env cond src_var in
    BddDomain.assign_lexpr ~relational:true man cond abstr
      [dst_var] [src_exp] None

  let meet_label lbl_sym label_value abstr =
    let env = BddDomain.get_env abstr in
    let label_expr =
      Expr1.Bint.of_expr @@
      Expr1.var env cond @@
      Env.label_var lbl_sym in
    let cons = Expr1.Bint.eq_int cond label_expr label_value in
    BddDomain.meet_condition man cond abstr cons

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
