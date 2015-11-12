open Batteries

type ('id, 't) var = {
  var_type : 't Types.t;
  var_origin : Types.origin;
  var_id : 'id;
}

type _ unop =
  | Neg : (int -> int) unop
  | Not : (bool -> bool) unop

let unop_fun : type a b. (a -> b) unop -> a -> b =
  function
  | Neg -> ( ~- )
  | Not -> ( not )

type _ binop =
  | Add : (int -> int -> int) binop
  | Sub : (int -> int -> int) binop
  | Mul : (int -> int -> int) binop
  | Div : (int -> int -> int) binop
  | Eq : (int -> int -> bool) binop
  | Neq : (int -> int -> bool) binop
  | Lt : (int -> int -> bool) binop
  | Gt : (int -> int -> bool) binop
  | Le : (int -> int -> bool) binop
  | Ge : (int -> int -> bool) binop
  | And : (bool -> bool -> bool) binop
  | Or : (bool -> bool -> bool) binop

let binop_fun : type a b c. (a -> b -> c) binop -> a -> b -> c =
  let open Int in
  function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge  -> ( >= )
  | And -> ( && )
  | Or -> ( || )

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
  | Unop :
      ('a -> 'b) unop Location.loc *
      ('id, 'a) expression Location.loc ->
    ('id, 'b) expression
  | Binop :
      ('a -> 'b -> 'c) binop Location.loc *
      ('id, 'a) expression Location.loc *
      ('id, 'b) expression Location.loc ->
    ('id, 'c) expression

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
