open Batteries

type ('a, 'spec) var_printer = {
  f : 't. 'a IO.output -> ('t, 'spec) TypedAst.var -> unit
}

val string_of_unop : _ TypedAst.unop -> string
val string_of_binop : _ TypedAst.binop -> string

val print_expression :
  ('a, 'b) var_printer ->
  'a IO.output ->
  (_, 'b) TypedAst.expression ->
  unit

val program_var_printer : (_, Types.origin) var_printer
val property_var_printer : (_, Source.t) var_printer

val print_prog_expr : 'a IO.output -> _ TypedAst.program_expression -> unit
val print_prop_cond : 'a IO.output -> TypedAst.property_condition -> unit
