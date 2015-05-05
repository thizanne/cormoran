open Batteries

val string_of_arith_unop : Program.arith_unop -> string
val string_of_logic_unop : Program.logic_unop -> string
val string_of_arith_binop : Program.arith_binop -> string
val string_of_arith_rel : Program.arith_rel -> string
val string_of_logic_binop : Program.logic_binop -> string

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
