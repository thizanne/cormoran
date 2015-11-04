open Batteries

type _ var_type =
  | Int : int var_type
  | Bool : bool var_type

type origin =
  | Local
  | Shared

type 'a var = {
  var_type : 'a var_type;
  var_origin : origin;
  var_name : Sym.t;
}

module Expression = struct
  module Make (Context : Context.Context) = struct
    type _ t =
      | Int :
          int Location.loc ->
        int t
      | Bool :
          bool Location.loc ->
        bool t
      | Var :
          'a var Context.t Location.loc ->
        'a t
      | ArithUnop :
          Operators.arith_one Location.loc *
          int t Location.loc ->
        int t
      | ArithBinop :
          Operators.arith_two Location.loc *
          int t Location.loc *
          int t Location.loc ->
        int t
      | LogicUnop :
          Operators.logic_one Location.loc *
          bool t Location.loc ->
        bool t
      | LogicBinop :
          Operators.logic_two Location.loc *
          bool t Location.loc *
          bool t Location.loc ->
        bool t
      | ArithRel :
          Operators.logic_arith_two Location.loc *
          int t Location.loc *
          int t Location.loc ->
        bool t
  end

  module InProgram = Make (Context.Identity)

  module InProperty = Make (Context.Threaded)
end

type body =
  | Nothing
  | Pass
  | MFence
  | Label of Sym.t Location.loc
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign :
      'a var Location.loc *
      'a Expression.InProgram.t Location.loc ->
    body
  | If of
      bool Expression.InProgram.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      bool Expression.InProgram.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      int var Location.loc * (* Indice *)
      int Expression.InProgram.t Location.loc * (* From *)
      int Expression.InProgram.t Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type t = {
  initial : int Sym.Map.t;
  threads : thread list;
}
