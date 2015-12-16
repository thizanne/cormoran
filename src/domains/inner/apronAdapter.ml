open Apron
open Batteries
open Printf
open Util

module T = TypedAst
module Ty = Types
module L = Location

module type Numerical = sig
  type t
  val manager : t Manager.t
end

module Make (N : Numerical) = struct

  type t = N.t Abstract1.t

  let man = N.manager

  let is_bottom = Abstract1.is_bottom man

  let equal = Abstract1.is_eq man

  let transtype_to_int { T.var_sym; var_type = Ty.Bool; var_spec }=
    { var_sym; var_spec; T.var_type = Ty.Int }

  let ap_unop : type t. t T.unop L.loc -> _ =
    fun op -> match op.L.item with
      | T.Neg -> Texpr1.Neg
      | T.Not ->
        Error.not_implemented_error op
          "Apron domains don't implement boolean expressions"

  let ap_binop : type t. t T.binop L.loc -> _ =
    fun op -> match op.L.item with
    | T.Add -> Texpr1.Add
    | T.Sub -> Texpr1.Sub
    | T.Mul -> Texpr1.Mul
    | T.Div -> Texpr1.Div
    | _ ->
      Error.not_implemented_error op
        "Apron domains don't implement boolean expressions"

  let ap_var x =
    Var.of_string @@ Sym.name x.T.var_spec

  let texpr1 env expr =
    let rec to_expr : type t. (t, Sym.t) T.expression -> _ = function
      | T.Int n ->
        Texpr1.Cst (Coeff.s_of_int n.L.item)
      | T.Bool b ->
        Texpr1.Cst
          (Coeff.s_of_int (if b.L.item then 1 else 0))
      | T.Var x ->
        Texpr1.Var (ap_var x.L.item)
      | T.Unop (op, expr) ->
        Texpr1.Unop (
          ap_unop op,
          to_expr expr.L.item,
          Texpr0.Int,
          Texpr0.Zero
        )
      | T.Binop (op, expr1, expr2) ->
        Texpr1.Binop (
          ap_binop op,
          to_expr expr1.L.item,
          to_expr expr2.L.item,
          Texpr0.Int,
          Texpr0.Zero
        )
    in Texpr1.of_expr env (to_expr expr)

  let int_texpr env n =
    Texpr1.cst env (Coeff.s_of_int n)

  let maybe_add_initial env abstr (x, value) = match value with
    | None -> abstr
    | Some n ->
      Abstract1.assign_texpr man abstr (ap_var x) (int_texpr env n) None

  let init int_initials bool_initials =
    let bool_initials =
      List.map
        (Tuple2.map transtype_to_int (Option.map Bool.to_int))
        bool_initials in
    let initials = int_initials @ bool_initials in
    let env = Environment.make
        (Array.of_list @@ List.map (ap_var @@@ fst) initials)
        [||] (* No real variables *) in
    List.fold_left
      (maybe_add_initial env)
      (Abstract1.top man env)
      initials

  let join = Abstract1.join man

  let meet = Abstract1.meet man

  let mk_not cond =
    L.mkdummy @@ T.Unop (L.mkdummy T.Not, cond)

  let reduce_not = function
    | T.Bool b ->
      T.Bool (L.comap ( not ) b)
    | T.Var v ->
      T.Binop (
        L.mkdummy T.Eq,
        L.cobind T.var @@ L.comap transtype_to_int v,
        L.mkdummy @@ T.Int (L.mkdummy 0)
      )
    | T.Unop ({ L.item = T.Not; _ }, cond) ->
      cond.L.item
    | T.Binop (rel, expr1, expr2) ->
      match rel.L.item with
      | T.Eq ->
        T.Binop (L.mkdummy T.Neq, expr1, expr2)
      | T.Neq ->
        T.Binop (L.mkdummy T.Eq, expr1, expr2)
      | T.Lt ->
        T.Binop (L.mkdummy T.Ge, expr1, expr2)
      | T.Gt ->
        T.Binop (L.mkdummy T.Le, expr1, expr2)
      | T.Le ->
        T.Binop (L.mkdummy T.Gt, expr1, expr2)
      | T.Ge ->
        T.Binop (L.mkdummy T.Lt, expr1, expr2)
      | T.And ->
        T.Binop (L.mkdummy T.Or, mk_not expr1, mk_not expr2)
      | T.Or ->
        T.Binop (L.mkdummy T.And, mk_not expr1, mk_not expr2)

  let tcons1 env rel e1 e2 =
    let expr e1 e2 =
      Texpr1.binop
        Texpr1.Sub
        (texpr1 env e1)
        (texpr1 env e2)
        Texpr1.Int Texpr1.Zero in
    match rel with
    | T.Eq -> Tcons1.make (expr e1 e2) Tcons1.EQ
    | T.Neq -> Tcons1.make (expr e1 e2) Tcons1.DISEQ
    | T.Lt -> Tcons1.make (expr e2 e1) Tcons1.SUP
    | T.Le -> Tcons1.make (expr e2 e1) Tcons1.SUPEQ
    | T.Gt -> Tcons1.make (expr e1 e2) Tcons1.SUP
    | T.Ge -> Tcons1.make (expr e1 e2) Tcons1.SUPEQ

  let rec meet_cons_rel rel expr1 expr2 abstr =
    let env = Abstract1.env abstr in
    match rel with
    | T.Eq | T.Lt | T.Le | T.Gt | T.Ge ->
      let earray = Tcons1.array_make env 1 in
      Tcons1.array_set earray 0
        (tcons1 env rel expr1.L.item expr2.L.item);
      Abstract1.meet_tcons_array man abstr earray
    | T.Neq ->
      join (* meet with Diseq does not work in Apron *)
        (meet_cons (T.Binop (L.mkdummy T.Lt, expr1, expr2)) abstr)
        (meet_cons (T.Binop (L.mkdummy T.Gt, expr1, expr2)) abstr)

  and meet_cons cons abstr =
    let env = Abstract1.env abstr in
    match cons with
    | T.Bool { L.item = true; _ } -> abstr
    | T.Bool { L.item = false; _ } -> Abstract1.bottom man env
    | T.Var v ->
      let int_var = L.comap transtype_to_int v in
      let var_neq_zero = T.Binop (
          L.mkdummy T.Neq,
          L.cobind T.var int_var,
          L.mkdummy (T.Int (L.mkdummy 0))
        ) in
      meet_cons var_neq_zero abstr
    | T.Unop ({ L.item = T.Not; _ }, c) ->
      meet_cons (reduce_not c.L.item) abstr
    | T.Binop (op, expr1, expr2) ->
      begin match op.L.item with
        | T.Eq -> meet_cons_rel T.Eq expr1 expr2 abstr
        | T.Lt -> meet_cons_rel T.Lt expr1 expr2 abstr
        | T.Le -> meet_cons_rel T.Le expr1 expr2 abstr
        | T.Gt -> meet_cons_rel T.Gt expr1 expr2 abstr
        | T.Ge -> meet_cons_rel T.Ge expr1 expr2 abstr
        | T.Neq -> meet_cons_rel T.Neq expr1 expr2 abstr
        | T.And -> meet_cons expr1.L.item @@ meet_cons expr2.L.item @@ abstr
        | T.Or -> join (meet_cons expr1.L.item abstr) (meet_cons expr2.L.item abstr)
      end

  let assign_expr var exp abstr =
    Abstract1.assign_texpr man abstr
      (ap_var var)
      (texpr1 (Abstract1.env abstr) exp)
      None

  let widening abstr1 abstr2 =
    Abstract1.widening man abstr1 abstr2

  let fold dest source abstr =
    Abstract1.fold man abstr [|ap_var dest; ap_var source|]

  let expand source dest abstr =
    Abstract1.expand man abstr (ap_var source) [|ap_var dest|]

  let add var abstr =
    let env = Abstract1.env abstr in
    Abstract1.change_environment man abstr
      (Environment.add env [|ap_var var|] [| |])
      false

  let drop var abstr =
    let env = Abstract1.env abstr in
    Abstract1.change_environment man abstr
      (Environment.remove env [|ap_var var|])
      false

  let print output abstr =
    let fmt = Format.formatter_of_output output in
    Abstract1.print fmt abstr;
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
