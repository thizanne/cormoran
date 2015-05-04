val string_of_arith_unop : Program.arith_unop -> string
val string_of_logic_unop : Program.logic_unop -> string
val string_of_arith_binop : Program.arith_binop -> string
val string_of_arith_rel : Program.arith_rel -> string
val string_of_logic_binop : Program.logic_binop -> string

val print_expression : 'a Batteries.IO.output -> Program.var Program.expression -> unit
val print_condition : 'a BatInnerIO.output -> Program.var Program.threaded Program.condition -> unit
