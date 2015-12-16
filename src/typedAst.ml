open Batteries

module L = Location
module Ty = Types

type ('t, 'spec) var = {
  var_sym : Sym.t;
  var_type : 't Types.t;
  var_spec : 'spec;
}

let var_spec_map f { var_sym; var_type; var_spec } =
  { var_sym; var_type; var_spec = f var_spec }

type 't program_var = ('t, Ty.origin) var

type 't property_var = ('t, Source.t) var

let is_shared { var_spec; _ } = match var_spec with
  | Types.Local -> false
  | Types.Shared -> true

let is_local { var_spec; _ } = match var_spec with
  | Types.Local -> true
  | Types.Shared -> false

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
  (* (type of the expression, type of variables specs) *)
  | Int :
      int Location.loc ->
    (int, 'a) expression
  | Bool :
      bool Location.loc ->
    (bool, 'a) expression
  | Var :
      ('t, 'spec) var Location.loc ->
    ('t, 'spec) expression
  | Unop :
      ('a -> 'b) unop Location.loc *
      ('a, 'spec) expression Location.loc ->
    ('b, 'spec) expression
  | Binop :
      ('a -> 'b -> 'c) binop Location.loc *
      ('a, 'spec) expression Location.loc *
      ('b, 'spec) expression Location.loc ->
    ('c, 'spec) expression

let var v = Var v

type 't program_expression = ('t, Ty.origin) expression

type property_condition = (bool, Source.t) expression

type ('spec1, 'spec2) var_mapper = {
  f : 'a. ('a, 'spec1) var -> ('a, 'spec2) var
}

let rec map_expr :
  type t.
  (_, _) var_mapper -> (t, _) expression -> (t, _) expression
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
  type t.
  (_, _) var_mapper ->
  (t, _) expression L.loc ->
  (t, _) expression L.loc
  =
  fun mapper ->
    L.comap (map_expr mapper)

let add_source tid expr =
  let put_thread v = var_spec_map (Source.threaded tid) v in
  let mapper = { f = put_thread } in
  map_expr mapper expr

type ('spec, 'acc) var_folder = {
  f : 't. ('t, 'spec) var -> 'acc -> 'acc
}

let rec fold_expr :
  type t. (_, _) var_folder -> _ -> (t, _) expression -> _ =
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
