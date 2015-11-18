val type_program :
  UntypedAst.program *
  (Property.zone option *
   Sym.t Context.MaybeThreaded.t UntypedAst.expression Location.loc)
    list ->

  Program.var Program.t *
  Program.var_view Property.t list
