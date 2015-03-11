open Batteries
open Syntax
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

let arith_op_priority = function
  | Add
  | Sub -> 0
  | Mul
  | Div -> 1

let arith_priority = function
  | Int _
  | Var _
  | ArithUnop _ -> 2
  | ArithBinop (op, _, _) -> arith_op_priority op.item

let logic_op_priority = function
  | Or -> 0
  | And -> 1

let logic_priority = function
  | ArithRel _ -> 1
  | Bool _
  | LogicUnop _ -> 2
  | LogicBinop (op, _, _) -> logic_op_priority op.item

let paren_print print output item =
  Printf.fprintf output "(%a)"
    print item

let prio_print op op_prio item_prio print output item =
  if item_prio item < op_prio op
  then paren_print print output item
  else print output item

let rec print_expression output = function
  | Int n ->
    Int.print output n.item
  | Var (_var_type, x) ->
    Symbol.print output x.item
  | ArithUnop (op, e) ->
    Printf.fprintf output "%s%a"
      (string_of_arith_unop op.item)
      print_expression e.item
  | ArithBinop (op, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      (prio_print op.item arith_op_priority arith_priority print_expression)
      e1.item
      (string_of_arith_binop op.item)
      (prio_print op.item arith_op_priority arith_priority print_expression)
      e2.item

let rec print_condition output = function
  | Bool b -> Bool.print output b.item
  | LogicUnop (op, c) ->
    Printf.fprintf output "%s %a"
      (string_of_logic_unop op.item)
      print_condition c.item
  | LogicBinop (op, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      (prio_print op.item logic_op_priority logic_priority print_condition)
      e1.item
      (string_of_logic_binop op.item)
      (prio_print op.item logic_op_priority logic_priority print_condition)
      e2.item
  | ArithRel (rel, e1, e2) ->
    Printf.fprintf output "%a %s %a"
      print_expression e1.item
      (string_of_arith_rel rel.item)
      print_expression e2.item
