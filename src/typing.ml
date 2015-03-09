open Batteries
open Location
open Printf
open Syntax
open Error

let undefined_var v =
  name_error v @@ sprintf "Var %s is not defined" (Symbol.name v.item)

type var_type =
  | Local
  | Shared

let add_local_if_absent x env =
  Symbol.Map.modify_opt x
    (function
      | None -> Some Local
      | Some typ -> Some typ)
    env

let rec check_expression env = function
  | Int _ -> ()
  | Var v ->
    if not @@ Symbol.Map.mem v.item env then
      type_error v @@
      sprintf "Var %s is not defined"
        (Symbol.name v.item)
  | ArithUnop (_, e) ->
    check_expression env e.item
  | ArithBinop (_, e1, e2) ->
    check_expression env e1.item;
    check_expression env e2.item

let rec check_condition env = function
  | Bool _ -> ()
  | LogicUnop (_, c) ->
    check_condition env c.item
  | LogicBinop (_, c1, c2) ->
    check_condition env c1.item;
    check_condition env c2.item
  | ArithRel (_, e1, e2) ->
    check_expression env e1.item;
    check_expression env e2.item

let rec type_body env = function
  | Nothing
  | Pass
  | MFence -> env
  | Seq (b1, b2) ->
    type_body (type_body env b1.item) b2.item
  | Assign (x, exp) ->
    check_expression env exp.item;
    add_local_if_absent x.item env
  | If (cond, body) ->
    check_condition env cond.item;
    type_body env body.item
  | While (cond, body) ->
    check_condition env cond.item;
    type_body env body.item
  | For (i, exp_from, exp_to, body) ->
    let env_i = add_local_if_absent i.item env in
    check_expression env_i exp_from.item;
    check_expression env_i exp_to.item;
    type_body env_i body.item

let type_thread shared_env { locals; body } =
  let env = type_body shared_env body in {
    body;
    locals =
      env
      |> Symbol.Map.filterv (( = ) Local)
      |> Symbol.Map.keys
      |> Symbol.Set.of_enum
  }

let type_program { initial; threads } =
  let shared_env = Symbol.Map.map (fun _ -> Shared) initial in {
    initial;
    threads = List.map (type_thread shared_env) threads
  }
