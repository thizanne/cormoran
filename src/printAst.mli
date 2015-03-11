val string_of_arith_unop : Syntax.arith_unop -> string
val string_of_logic_unop : Syntax.logic_unop -> string
val string_of_arith_binop : Syntax.arith_binop -> string
val string_of_arith_rel : Syntax.arith_rel -> string
val string_of_logic_binop : Syntax.logic_binop -> string

val print_expression : 'a Batteries.IO.output -> Syntax.expression -> unit
val print_condition : 'a BatInnerIO.output -> Syntax.condition -> unit
