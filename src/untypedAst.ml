open Batteries

type arith_unop =
  | Neg

type logic_unop =
  | Not

type arith_binop =
  | Add
  | Sub
  | Mul
  | Div

type logic_binop =
  | And
  | Or

type arith_relop =
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

type 'var expression =
  | Int of int Location.loc
  | Bool of bool Location.loc
  | Var of 'var Location.loc
  | ArithUnop of
      arith_unop Location.loc *
      'var expression Location.loc
  | ArithBinop of
      arith_binop Location.loc *
      'var expression Location.loc *
      'var expression Location.loc
  | LogicUnop of
      logic_unop Location.loc *
      'var expression Location.loc
  | LogicBinop of
      logic_binop Location.loc *
      'var expression Location.loc *
      'var expression Location.loc
  | ArithRelop of
      arith_relop Location.loc *
      'var expression Location.loc *
      'var expression Location.loc

type body_expression =
  Sym.t expression

type property_expression =
  (Sym.t * Source.thread_id option) expression

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

type thread = {
  body : body Location.loc;
}

type program = {
  globals : Env.ty Sym.Map.t;
  initial : (Sym.t * Source.thread_id option) expression Location.loc;
  threads : thread list;
}
