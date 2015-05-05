val type_program :
  Symbol.t Program.t *
  (Symbol.t * Program.thread_id option) Property.t list ->

  Program.var Program.t *
  Program.var_view Property.t list
