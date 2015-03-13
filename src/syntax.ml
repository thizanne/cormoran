open Batteries
open Location

type control_label = int

type control_state = control_label list

type arith_unop =
  | Neg

let fun_of_arith_unop = function
  | Neg -> ( ~- )

type logic_unop =
  | Not

let fun_of_logic_unop = function
  | Not -> ( not )

type arith_binop =
  | Add
  | Sub
  | Mul
  | Div

let fun_of_arith_binop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )

type arith_rel =
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

let fun_of_arith_rel =
  let open Int in
  function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge  -> ( >= )

type logic_binop =
  | And
  | Or

let fun_of_logic_binop = function
  | And -> ( && )
  | Or -> ( || )

type var_type =
  | Local
  | Shared

type var = {
  mutable var_type : var_type;
  var_name : Symbol.t;
}

let is_local v =
  v.var_type = Local

type expression =
  | Int of int Location.loc
  | Var of var Location.loc
  | ArithUnop of
      arith_unop Location.loc *
      expression Location.loc
  | ArithBinop of
      arith_binop Location.loc *
      expression Location.loc *
      expression Location.loc

let rec shared_in_expr = function
  | Int _ -> []
  | Var v ->
    if is_local v.item then [] else [v.item.var_name]
  | ArithUnop (_, e) ->
    shared_in_expr e.item
  | ArithBinop (_, e1, e2) ->
    shared_in_expr e1.item @ shared_in_expr e2.item

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
      var Location.loc *
      expression Location.loc
  | If of
      condition Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | While of
      condition Location.loc * (* Condition *)
      t Location.loc (* Body *)
  | For of
      var Location.loc * (* Indice *)
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
