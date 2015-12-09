open Batteries
open Printf

module T = TypedAst
module L = Location

type precedence = int

let string_of_unop :
  type t. t T.unop -> string =
  function
  | T.Neg -> "-"
  | T.Not -> "!"

let string_of_binop :
  type t. t T.binop -> string =
  function
  | T.Add -> "+"
  | T.Sub -> "-"
  | T.Mul -> "×"
  | T.Div  -> "/"
  | T.Eq -> "="
  | T.Neq -> "≠"
  | T.Lt -> "<"
  | T.Gt -> ">"
  | T.Le -> "≤"
  | T.Ge  -> "≥"
  | T.And -> "∧"
  | T.Or -> "∨"

let or_precedence = 0
let and_precedence = 1
let rel_precedence = 2
let add_precedence = 3
let mul_precedence = 4
let unop_precedence = 5

let binop_precedence :
  type a. a T.binop -> precedence =
  function
  | T.Add -> add_precedence
  | T.Sub -> add_precedence
  | T.Mul -> mul_precedence
  | T.Div -> mul_precedence
  | T.Or -> or_precedence
  | T.And -> and_precedence
  | T.Eq -> rel_precedence
  | T.Neq -> rel_precedence
  | T.Gt -> rel_precedence
  | T.Ge -> rel_precedence
  | T.Lt -> rel_precedence
  | T.Le -> rel_precedence

let expr_precedence :
  type t. (_, t) T.expression -> precedence =
  function
  | T.Int _ -> unop_precedence
  | T.Bool _ -> unop_precedence
  | T.Var _ -> unop_precedence
  | T.Unop _ -> unop_precedence
  | T.Binop ( op, _, _) ->
    binop_precedence op.L.item

type ('a, 'id) var_printer = {
  f : 't. 'a IO.output -> ('id, 't) T.var -> unit
}

let rec print_inside_prec :
  type t. precedence -> _ -> _ -> (_, t) T.expression -> unit =
  fun prec print_var output expr ->
    fprintf output
      (if expr_precedence expr > prec
       then "%a"
       else "(%a)")
      (print_expression print_var) expr

and print_expression :
  type t. _ -> _ -> (_, t) T.expression -> unit =
  fun print_var output expression ->
    match expression with
    | T.Int n ->
      Int.print output n.L.item
    | T.Bool b ->
      Bool.print output b.L.item
    | T.Var x ->
      print_var.f output x.L.item
    | T.Unop (op, e) ->
      fprintf output "%s%a"
        (string_of_unop op.L.item)
        (print_inside_prec unop_precedence print_var)
        e.L.item
    | T.Binop (op, e1, e2) ->
      fprintf output "%a %s %a"
        (print_inside_prec (binop_precedence op.L.item) print_var)
        e1.L.item
        (string_of_binop op.L.item)
        (print_inside_prec (binop_precedence op.L.item) print_var)
        e2.L.item

let program_var_printer = {
  f = fun output { T.var_id; _ } ->
    Sym.print output var_id
}

let property_var_printer = {
  f = fun output { T.var_id; _ } ->
    Context.MaybeThreaded.print Sym.print output var_id
}
