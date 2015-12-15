val type_program :
  UntypedAst.program *
  (Property.zone option *
   Sym.t Context.MaybeThreaded.t UntypedAst.expression Location.loc)
    list ->

  TypedAst.program *
  Property.t list
