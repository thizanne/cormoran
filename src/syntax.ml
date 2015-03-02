open Batteries

type position = int list

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

type expression =
  | Bool of bool Location.loc
  | Int of int Location.loc
  | Var of Symbol.t Location.loc
  | Unop of
      unop Location.loc *
      expression Location.loc
  | Binop of
      binop Location.loc *
      expression Location.loc *
      expression Location.loc

type t  =
  | Pass
  | MFence
  | Label of Symbol.t Location.loc
  | Goto of Symbol.t Location.loc
  | Seq of
      t Location.loc *
      t Location.loc
  | Assign of
      Symbol.t Location.loc *
      expression Location.loc
  | If of
      expression Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | While of
      expression Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | For of
      Symbol.t Location.loc * (* Indice symbol *)
      expression Location.loc * (* From *)
      expression Location.loc * (* To *)
      t Location.loc (* Body *)

type thread = {
  locals : Symbol.t list;
  body : t;
}

type program = {
  initial : (Symbol.t * int) list;
  threads : thread list;
}
