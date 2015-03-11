open Batteries

type position = int list

type arith_unop =
  | Neg

type logic_unop =
  | Not

type arith_binop =
  | Add
  | Sub
  | Mul
  | Div

type arith_rel =
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

type logic_binop =
  | And
  | Or

type var_type =
  | Local
  | Shared

type expression =
  (* TODO: replace var_type ref with var_type, and make Typing rebuild
     an AST instead of just setting the references *)
  | Int of int Location.loc
  | Var of
      var_type ref *
      Symbol.t Location.loc
  | ArithUnop of
      arith_unop Location.loc *
      expression Location.loc
  | ArithBinop of
      arith_binop Location.loc *
      expression Location.loc *
      expression Location.loc

type condition =
  | Bool of bool Location.loc
  | LogicUnop of
      logic_unop Location.loc *
      condition Location.loc
  | LogicBinop of
      logic_binop Location.loc *
      condition Location.loc *
      condition Location.loc
  | ArithRel of
      arith_rel Location.loc *
      expression Location.loc *
      expression Location.loc

type t  =
  | Nothing
  | Pass
  | MFence
  | Seq of
      t Location.loc *
      t Location.loc
  | Assign of
      var_type ref *
      Symbol.t Location.loc *
      expression Location.loc
  | If of
      condition Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | While of
      condition Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | For of
      Symbol.t Location.loc * (* Indice symbol *)
      expression Location.loc * (* From *)
      expression Location.loc * (* To *)
      t Location.loc (* Body *)

type thread = {
  locals : Symbol.Set.t;
  body : t;
}

type program = {
  initial : int Symbol.Map.t;
  threads : thread list;
}
