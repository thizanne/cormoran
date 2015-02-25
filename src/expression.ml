open Apron
open Batteries
open Printf

type var =
  | Local of Symbol.t
  | Shared of Symbol.t * int

let local_var r =
  Local r

let shared_var x t =
  Shared (x, t)

let to_apron_var = function
  | Local r -> Apron.Var.of_string r
  | Shared (x, t) -> Apron.Var.of_string (sprintf "%s_%d" x t)

let apron_local r =
  to_apron_var (local_var r)

let apron_shared x t =
  to_apron_var (shared_var x t)

let print_var output = function
  | Local r -> String.print output r
  | Shared (x, t) -> String.print output (sprintf "%s_%d" x t)

type binop =
  | Add
  | Sub
  | Mul
  | Div

type unop =
  | Neg

type t =
  | EInt of int
  | EVar of var
  | EUnop of unop * t
  | EBinop of binop * t * t

let int n =
  EInt n

let local r =
  EVar (local_var r)

let shared x t =
  EVar (shared_var x t)

let unop op e =
  EUnop (op, e)

let binop op e1 e2 =
  EBinop (op, e1, e2)

let neg =
  unop Neg

let add, sub, mul, div =
  binop Add, binop Sub, binop Mul, binop Div

let to_apron_unop = function
  | Neg -> Texpr1.Neg

let to_apron_binop = function
  | Add -> Texpr1.Add
  | Sub -> Texpr1.Sub
  | Mul -> Texpr1.Mul
  | Div -> Texpr1.Div

let binop_of_char c =
  List.assoc c [
    '+', Add;
    '-', Sub;
    '*', Mul;
    '/', Div;
  ]

let fun_of_unop = function
  | Neg -> ( ~- )

let fun_of_binop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )

let of_syntax_value =
  let open Syntax in function
  | Int n -> EInt n.item
  | Var r -> EVar (Local r.item)

let rec of_syntax =
  let open Syntax in function
  | Val v -> of_syntax_value v.item
  | Op (op, e1, e2) ->
    EBinop (
      binop_of_char op.item,
      of_syntax e1.item,
      of_syntax e2.item
    )

let rec to_texpr_expr =
  function
  | EInt n -> Texpr1.Cst (Coeff.s_of_int n)
  | EVar r -> Texpr1.Var (to_apron_var r)
  | EUnop (op, e) ->
    Texpr1.Unop (
      to_apron_unop op,
      to_texpr_expr e,
      Texpr1.Int, Texpr1.Zero
    )
  | EBinop (op, e1, e2) ->
      Texpr1.Binop (
      to_apron_binop op,
      to_texpr_expr e1, to_texpr_expr e2,
      Texpr1.Int, Texpr1.Zero
    )

let to_texpr env e =
  Texpr1.of_expr env (to_texpr_expr e)
