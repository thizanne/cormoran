open Batteries

module Expression = struct
  module Make (Context : Context.Context) = struct
    type t =
      | Int of int Location.loc
      | Bool of bool Location.loc
      | Var of Sym.t Context.t Location.loc
      | ArithUnop of
          Operators.arith_one Location.loc *
          t Location.loc
      | ArithBinop of
          Operators.arith_two Location.loc *
          t Location.loc *
          t Location.loc
      | LogicUnop of
          Operators.logic_one Location.loc *
          t Location.loc
      | LogicBinop of
          Operators.logic_two Location.loc *
          t Location.loc *
          t Location.loc
      | ArithRel of
          Operators.logic_arith_two Location.loc *
          t Location.loc *
          t Location.loc
  end

  module InProgram = Make (Context.Identity)

  module InProperty = Make (Context.MaybeThreaded)
end

type body =
  | Nothing
  | Pass
  | MFence
  | Label of Sym.t Location.loc
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign of
      Sym.t Location.loc *
      Expression.InProgram.t Location.loc
  | If of
      Expression.InProgram.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      Expression.InProgram.t Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      Sym.t Location.loc * (* Indice *)
      Expression.InProgram.t Location.loc * (* From *)
      Expression.InProgram.t Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type program = {
  initial : int Sym.Map.t;
  threads : thread list;
}
