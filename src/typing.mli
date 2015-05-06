val type_program :
  Symbol.t Program.t *
  (Program.thread_id option * Symbol.t) Property.t list ->

  Program.var Program.t *
  Program.var_view Property.t list
