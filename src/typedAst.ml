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

type _ expression =
  | Int :
      int Location.loc ->
    int expression
  | Bool :
      bool Location.loc ->
    bool expression
  | Var :
      'a var Location.loc ->
    'a expression
  | ArithUnop :
      Operators.arith_one Location.loc *
      int expression Location.loc ->
    int expression
  | ArithBinop :
      Operators.arith_two Location.loc *
      int expression Location.loc *
      int expression Location.loc ->
    int expression
  | LogicUnop :
      Operators.logic_one Location.loc *
      bool expression Location.loc ->
    bool expression
  | LogicBinop :
      Operators.logic_two Location.loc *
      bool expression Location.loc *
      bool expression Location.loc ->
    bool expression
  | ArithRel :
      Operators.logic_arith_two Location.loc *
      int expression Location.loc *
      int expression Location.loc ->
    bool expression

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
      'a expression Location.loc ->
    body
  | If of
      bool expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      bool expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      int var Location.loc * (* Indice *)
      int expression Location.loc * (* From *)
      int expression Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type t = {
  initial : int Sym.Map.t;
  threads : thread list;
}
