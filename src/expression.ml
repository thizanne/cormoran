module type S = sig
  type 't var

  type _ t =
    | Int :
        int Location.loc ->
      int t
    | Bool :
        bool Location.loc ->
      bool t
    | Var :
        't var Location.loc -> 't t
    | Unop :
        ('a -> 'b) Operators.one Location.loc *
        'a t Location.loc ->
      'b t
    | Binop :
        ('a -> 'b -> 'c) Operators.two Location.loc *
        'a t Location.loc *
        'b t Location.loc ->
      'c t
end

module Make (C : Context.Context) : S = struct

  type 't var = 't Var.t C.t

  type _ t =
    | Int :
        int Location.loc ->
      int t
    | Bool :
        bool Location.loc ->
      bool t
    | Var :
        't var Location.loc -> 't t
    | Unop :
        ('a -> 'b) Operators.one Location.loc *
        'a t Location.loc ->
      'b t
    | Binop :
        ('a -> 'b -> 'c) Operators.two Location.loc *
        'a t Location.loc *
        'b t Location.loc ->
      'c t

end
