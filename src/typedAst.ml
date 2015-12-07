open Batteries

module L = Location
module MT = Context.MaybeThreaded

type ('id, 't) var = {
  var_type : 't Types.t;
  var_origin : Types.origin;
  var_id : 'id;
}

let var_id_map f { var_type; var_origin; var_id } =
  { var_type; var_origin; var_id = f var_id }

type 't program_var = (Sym.t, 't) var

type 't property_var = (Sym.t MT.t, 't) var

let is_shared { var_origin; _ } = match var_origin with
  | Types.Local -> false
  | Types.Shared -> true

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

type 't program_expression = (Sym.t, 't) expression

type property_condition = (Sym.t MT.t, bool) expression

type ('id1, 'id2) var_mapper = {
  f : 'a. ('id1, 'a) var -> ('id2, 'a) var
}

let rec map_expr :
  type a.
  ('id1, 'id2) var_mapper -> ('id1, a) expression -> ('id2, a) expression
  =
  fun mapper exp ->
    match exp with
    | Int n -> Int n
    | Bool b -> Bool b
    | Var v -> Var (L.comap mapper.f v)
    | Unop (op, exp) ->
      Unop (op, map_expr_loc mapper exp)
    | Binop (op, exp1, exp2) ->
      Binop (
        op,
        map_expr_loc mapper exp1,
        map_expr_loc mapper exp2
      )

and map_expr_loc :
  type a.
  ('id1, 'id2) var_mapper ->
  ('id1, a) expression L.loc ->
  ('id2, a) expression L.loc
  =
  fun mapper ->
    L.comap (map_expr mapper)

let add_thread_info tid expr =
  let put_thread v = var_id_map (MT.create_some tid) v in
  let mapper = { f = put_thread } in
  map_expr mapper expr

type ('id, 'acc) var_folder = {
  f : 'a. ('id, 'a) var -> 'acc -> 'acc
}

let rec fold_expr :
  type a. ('id, 'acc) var_folder -> 'acc -> ('id, a) expression -> 'acc =
  fun f acc exp ->
    match exp with
    | Int _ -> acc
    | Bool _ -> acc
    | Var v -> f.f v.L.item acc
    | Unop (_, exp) -> fold_expr f acc exp.L.item
    | Binop (_, exp1, exp2) ->
      fold_expr f (fold_expr f acc exp2.L.item) exp1.L.item

type body =
  | Nothing
  | Pass
  | MFence
  | Label of Sym.t Location.loc
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign :
      't program_var Location.loc *
      't program_expression Location.loc ->
    body
  | If of
      bool program_expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      bool program_expression Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      int program_var Location.loc * (* Indice *)
      int program_expression Location.loc * (* From *)
      int program_expression Location.loc * (* To *)
      body Location.loc (* Body *)

type thread = {
  locals : Sym.Set.t;
  body : body Location.loc;
}

type constant =
  | ConstInt of int
  | ConstBool of bool

type program = {
  initial : constant Sym.Map.t;
  threads : thread list;
}
