val type_program :
  Sym.t Program.t *
  (Program.thread_id option * Sym.t) Property.t list ->

  Program.var Program.t *
  Program.var_view Property.t list
