open Apron
open Batteries
open Printf
open Util

module T = TypedAst
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

  let init initials =
    let env = Environment.make
        (Array.of_list @@ List.map (ap_var @@@ fst) initials)
        [||] (* No real variables *) in
    List.fold_left
      (maybe_add_initial env)
      (Abstract1.top man env)
      initials

  let join = Abstract1.join man

  let meet = Abstract1.meet man

  let not_rel = function
    | T.Eq -> T.Neq
    | T.Neq -> T.Eq
    | T.Lt -> T.Ge
    | T.Gt -> T.Le
    | T.Le -> T.Gt
    | T.Ge -> T.Lt

  let not_op = function
    | T.And -> T.Or
    | T.Or -> T.And

  let reduce_not = function
    | T.Bool b ->
      T.Bool (L.comap ( not ) b)
    | T.Var _ -> assert false
    | T.Unop ({ L.item = T.Not; _ }, cond) ->
      cond.L.item
    | T.Binop (rel, expr1, expr2) ->
      T.Binop (L.comap not_rel rel, expr1, expr2)
    | T.Binop (op, cond1, cond2) ->
      T.Binop (
        L.comap not_op op,
        L.mkdummy @@ T.LogicUnop (L.mkdummy T.Not, cond1),
        L.mkdummy @@ T.LogicUnop (L.mkdummy T.Not, cond2)
      )

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

  let meet_cons cons abstr =
    let env = Abstract1.env abstr in
    let rec aux abstr = function
      | T.Bool { L.item = true; _ } -> abstr
      | T.Bool { L.item = false; _ } -> Abstract1.bottom man env
      | T.Var _ -> assert false
      | T.Unop ({ L.item = T.Not; _ }, c) ->
        aux abstr (reduce_not c.L.item)
      | T.Binop ({ L.item = T.Neq; loc }, e1, e2) ->
        join (* meet with Diseq does not work in Apron *)
          (aux abstr @@ T.Binop ({ L.item = T.Lt; loc}, e1, e2))
          (aux abstr @@ T.Binop ({ L.item = T.Gt; loc}, e1, e2))
      | T.Binop ({ L.item = T.Eq as rel; _ }, e1, e2) ->
        let earray = Tcons1.array_make env 1 in
        Tcons1.array_set earray 0
          (tcons1 env rel e1.L.item e2.L.item);
        Abstract1.meet_tcons_array man abstr earray
      | T.Binop (op, c1, c2) ->
        begin match op.L.item with
          | T.And -> aux (aux abstr c1.L.item) c2.L.item
          | T.Or ->
            join
              (aux abstr c1.L.item)
              (aux abstr c2.L.item)
        end
    in aux abstr cons

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
