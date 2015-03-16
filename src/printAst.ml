open Batteries
open Program
open Location

let string_of_arith_unop = function
  | Neg -> "-"

let string_of_logic_unop = function
  | Not -> "!"

let string_of_arith_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "×"
  | Div  -> "/"

let string_of_arith_rel = function
  | Eq -> "="
  | Neq -> "≠"
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "≤"
  | Ge  -> "≥"

let string_of_logic_binop = function
  | And -> "∧"
  | Or -> "∨"

let unop_priority = 2

let arith_op_priority = function
  | Add
  | Sub -> 0
  | Mul
  | Div -> 1

let arith_priority = function
  | Int _
  | Var _
  | ArithUnop _ -> unop_priority
  | ArithBinop (op, _, _) -> arith_op_priority op.item

let logic_op_priority = function
  | Or -> 0
  | And -> 1

let logic_priority = function
  | ArithRel _ -> 1
  | Bool _
  | LogicUnop _ -> unop_priority
  | LogicBinop (op, _, _) -> logic_op_priority op.item

let paren_print print output item =
  Printf.fprintf output "(%a)"
    print item

let prio_print outer_prio get_inner_prio print output item =
  if get_inner_prio item < outer_prio
  then paren_print print output item
  else print output item

let rec print_expression output = function
  | Int n ->
    Int.print output n.item
  | Var x ->
    Symbol.print output x.item.var_name
  | ArithUnop (op, e) ->
    Printf.fprintf output "%s%a"
      (string_of_arith_unop op.item)
      (prio_print unop_priority arith_priority print_expression) e.item
  | ArithBinop (op, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      (prio_print (arith_op_priority op.item) arith_priority print_expression)
      e1.item
      (string_of_arith_binop op.item)
      (prio_print (arith_op_priority op.item) arith_priority print_expression)
      e2.item

let rec print_condition output = function
  | Bool b -> Bool.print output b.item
  | LogicUnop (op, c) ->
    Printf.fprintf output "%s %a"
      (string_of_logic_unop op.item)
      (prio_print unop_priority logic_priority print_condition) c.item
  | LogicBinop (op, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      (prio_print (logic_op_priority op.item) logic_priority print_condition)
      e1.item
      (string_of_logic_binop op.item)
      (prio_print (logic_op_priority op.item) logic_priority print_condition)
      e2.item
  | ArithRel (rel, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      print_expression e1.item
      (string_of_arith_rel rel.item)
      print_expression e2.item
