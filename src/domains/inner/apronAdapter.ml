open Apron
open Batteries
open Printf
open Util

module P = Program
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

  let ap_unop = function
    | P.Neg -> Texpr1.Neg

  let ap_binop = function
    | P.Add -> Texpr1.Add
    | P.Sub -> Texpr1.Sub
    | P.Mul -> Texpr1.Mul
    | P.Div -> Texpr1.Div

  let ap_var x =
    Var.of_string @@ Symbol.name x

  let texpr1 env expr =
    let rec to_expr = function
      | P.Int n ->
        Texpr1.Cst (Coeff.s_of_int n.L.item)
      | P.Var x ->
        Texpr1.Var (ap_var x.L.item)
      | P.ArithUnop (op, expr) ->
        Texpr1.Unop (
          ap_unop op.L.item,
          to_expr expr.L.item,
          Texpr0.Int,
          Texpr0.Zero
        )
      | P.ArithBinop (op, expr1, expr2) ->
        Texpr1.Binop (
          ap_binop op.L.item,
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
    | P.Eq -> P.Neq
    | P.Neq -> P.Eq
    | P.Lt -> P.Ge
    | P.Gt -> P.Le
    | P.Le -> P.Gt
    | P.Ge -> P.Lt

  let not_op = function
    | P.And -> P.Or
    | P.Or -> P.And

  let reduce_not = function
    | P.Bool b ->
      P.Bool (L.comap ( not ) b)
    | P.ArithRel (rel, expr1, expr2) ->
      P.ArithRel (L.comap not_rel rel, expr1, expr2)
    | P.LogicUnop ({ L.item = P.Not; _}, cond) ->
      cond.L.item
    | P.LogicBinop (op, cond1, cond2) ->
      P.LogicBinop (
        L.comap not_op op,
        L.mkdummy @@ P.LogicUnop (L.mkdummy P.Not, cond1),
        L.mkdummy @@ P.LogicUnop (L.mkdummy P.Not, cond2)
      )

  let tcons1 env rel e1 e2 =
    let expr e1 e2 =
      Texpr1.binop
        Texpr1.Sub
        (texpr1 env e1)
        (texpr1 env e2)
        Texpr1.Int Texpr1.Zero in
    match rel with
    | P.Eq -> Tcons1.make (expr e1 e2) Tcons1.EQ
    | P.Neq -> Tcons1.make (expr e1 e2) Tcons1.DISEQ
    | P.Lt -> Tcons1.make (expr e2 e1) Tcons1.SUP
    | P.Le -> Tcons1.make (expr e2 e1) Tcons1.SUPEQ
    | P.Gt -> Tcons1.make (expr e1 e2) Tcons1.SUP
    | P.Ge -> Tcons1.make (expr e1 e2) Tcons1.SUPEQ

  let meet_cons cons abstr =
    let env = Abstract1.env abstr in
    let rec aux abstr = function
      | P.Bool { L.item = true; _ } -> abstr
      | P.Bool { L.item = false; _ } -> Abstract1.bottom man env
      | P.LogicUnop ({ L.item = P.Not; _ }, c) ->
        aux abstr (reduce_not c.L.item)
      | P.ArithRel ({ L.item = P.Neq; loc }, e1, e2) ->
        join (* meet with Diseq does not work in Apron *)
          (aux abstr @@ P.ArithRel ({ L.item = P.Lt; loc}, e1, e2))
          (aux abstr @@ P.ArithRel ({ L.item = P.Gt; loc}, e1, e2))
      | P.ArithRel (rel, e1, e2) ->
        let earray = Tcons1.array_make env 1 in
        Tcons1.array_set earray 0
          (tcons1 env rel.L.item e1.L.item e2.L.item);
        Abstract1.meet_tcons_array man abstr earray
      | P.LogicBinop (op, c1, c2) ->
        begin match op.L.item with
          | P.And -> aux (aux abstr c1.L.item) c2.L.item
          | P.Or ->
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
