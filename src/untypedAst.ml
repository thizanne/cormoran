open Batteries

type unop =
  | Neg
  | Not

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or

type 'var expression =
  | Int of int Location.loc
  | Bool of bool Location.loc
  | Var of 'var Location.loc
  | Unop of
      unop Location.loc *
      'var expression Location.loc
  | Binop of
      binop Location.loc *
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
