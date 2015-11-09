open Batteries

type 'var expression =
  | Int of int Location.loc
  | Bool of bool Location.loc
  | Var of 'var Location.loc
  | ArithUnop of
      Operators.arith_one Location.loc *
      'var expression Location.loc
  | ArithBinop of
      Operators.arith_two Location.loc *
      'var expression Location.loc *
      'var expression Location.loc
  | LogicUnop of
      Operators.logic_one Location.loc *
      'var expression Location.loc
  | LogicBinop of
      Operators.logic_two Location.loc *
      'var expression Location.loc *
      'var expression Location.loc
  | ArithRel of
      Operators.logic_arith_two Location.loc *
      'var expression Location.loc *
      'var expression Location.loc

type body_expression =
  Sym.t expression

type property_expression =
  Sym.t Context.MaybeThreaded.t expression

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
      body_expression Location.loc
  | If of
      body_expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      body_expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      Sym.t Location.loc * (* Indice *)
      body_expression Location.loc * (* From *)
      body_expression Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type program = {
  initial : int Sym.Map.t;
  threads : thread list;
}
