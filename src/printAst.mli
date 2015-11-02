open Batteries

val string_of_arith_unop : Operators.arith_one -> string
val string_of_logic_unop : Operators.logic_one -> string
val string_of_arith_binop : Operators.arith_two -> string
val string_of_arith_rel : Operators.logic_arith_two -> string
val string_of_logic_binop : Operators.logic_two -> string

val print_expression :
  ('a IO.output -> 'b -> unit) ->
  'a Batteries.IO.output ->
  'b Program.expression ->
  unit

val print_condition :
  ('a IO.output -> 'b -> unit) ->
  'a BatInnerIO.output ->
  'b Program.condition ->
  unit
