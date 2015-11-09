open Batteries

type ('id, 't) var = {
  var_type : 't Types.t;
  var_origin : Types.origin;
  var_id : 'id;
}

type (_, _) expression =
  (* (type of variables identifiers, type of the expression) *)
  | Int :
      int Location.loc ->
    ('a, int) expression
  | Bool :
      bool Location.loc ->
    ('a, bool) expression
  | Var :
      ('id, 't) var Location.loc ->
    ('id, 't) expression
  | ArithUnop :
      Operators.arith_one Location.loc *
      ('a, int) expression Location.loc ->
    ('a, int) expression
  | ArithBinop :
      Operators.arith_two Location.loc *
      ('a, int) expression Location.loc *
      ('a, int) expression Location.loc ->
    ('a, int) expression
  | LogicUnop :
      Operators.logic_one Location.loc *
      ('a, bool) expression Location.loc ->
    ('a, bool) expression
  | LogicBinop :
      Operators.logic_two Location.loc *
      ('a, bool) expression Location.loc *
      ('a, bool) expression Location.loc ->
    ('a, bool) expression
  | ArithRel :
      Operators.logic_arith_two Location.loc *
      ('a, int) expression Location.loc *
      ('a, int) expression Location.loc ->
    ('a, bool) expression

type body =
  | Nothing
  | Pass
  | MFence
  | Label of Sym.t Location.loc
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign :
      (Sym.t, 't) var Location.loc *
      (Sym.t, 't) expression Location.loc ->
    body
  | If of
      (Sym.t, bool) expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      (Sym.t, bool) expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      (Sym.t, int) var Location.loc * (* Indice *)
      (Sym.t, int) expression Location.loc * (* From *)
      (Sym.t, int) expression Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  body : body Location.loc;
}

type program = {
  initial : int Sym.Map.t;
  threads : thread list;
}
