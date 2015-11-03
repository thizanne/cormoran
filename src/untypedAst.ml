open Batteries

type expression =
  | Int of int Location.loc
  | Var of Sym.t Location.loc
  | ArithUnop of
      Operators.arith_one Location.loc *
      expression Location.loc
  | ArithBinop of
      Operators.arith_two Location.loc *
      expression Location.loc *
      expression Location.loc

type condition =
  | Bool of bool Location.loc
  | LogicUnop of
      Operators.logic_one Location.loc *
      condition Location.loc
  | LogicBinop of
      Operators.logic_two Location.loc *
      condition Location.loc *
      condition Location.loc
  | ArithRel of
      Operators.logic_arith_two Location.loc *
      expression Location.loc *
      expression Location.loc

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
      expression Location.loc
  | If of
      condition Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      condition Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      Sym.t Location.loc * (* Indice *)
      expression Location.loc * (* From *)
      expression Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type t = {
  initial : int Sym.Map.t;
  threads : thread list;
}
