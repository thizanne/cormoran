open Batteries

(* TODO: make these types private *)

type control_label = int

type control_state = control_label list

let is_initial =
  List.for_all (( = ) 0)

type thread_id = int

(* TODO: unify threaded with Location.loc as a context comonad *)

type 'a threaded = {
  thread_id : thread_id;
  elem : 'a [@main];
} [@@deriving create]

let print_threaded print_elem output { thread_id; elem } =
  Printf.fprintf output "%a:%d" print_elem elem thread_id

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

let print_var output x =
  Symbol.print output x.var_name

let local_var var_name = {
  var_name;
  var_type = Local;
}

let shared_var var_name = {
  var_name;
  var_type = Shared;
}

let is_local v =
  v.var_type = Local

let is_shared v =
  v.var_type = Shared

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

let var v = Var v

let rec shared_in_expr = function
  | Int _ -> []
  | Var v ->
    if is_local v.Location.item
    then []
    else [v.Location.item.var_name]
  | ArithUnop (_, e) ->
    shared_in_expr e.Location.item
  | ArithBinop (_, e1, e2) ->
    shared_in_expr e1.Location.item @
    shared_in_expr e2.Location.item

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

type body  =
  | Nothing
  | Pass
  | MFence
  | Seq of
      body Location.loc *
      body Location.loc
  | Assign of
      var Location.loc *
      expression Location.loc
  | If of
      condition Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | While of
      condition Location.loc * (* Condition *)
      body Location.loc (* Body *)
  | For of
      var Location.loc * (* Indice *)
      expression Location.loc * (* From *)
      expression Location.loc * (* To *)
      body Location.loc (* Body *)

let seq body1 body2 =
  Location.mk (Seq (body1, body2))
    Location.(body1.loc.startpos)
    Location.(body2.loc.endpos)

type thread = {
  locals : Symbol.Set.t;
  body : body;
}

(* TODO: add a phantom type to ensure programs are typed before being
   analysed *)

type t = {
  initial : int Symbol.Map.t;
  threads : thread list;
}

let initial_state p =
  List.map (fun _ -> 0) p.threads
