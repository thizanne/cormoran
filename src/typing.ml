open Batteries
open Location
open Printf
open Error
open Program

let add_local_if_absent x env =
  Symbol.Map.modify_opt x
    (function
      | None -> Some Local
      | Some typ -> Some typ)
    env

let check_expression env shared_allowed expr =
  let rec has_shared shared_allowed = function
    | Int _ -> false
    | Var v ->
      begin match Symbol.Map.find v.item.var_name env with
        | Local ->
          let () = v.item.var_type <- Local in false
        | Shared ->
          if shared_allowed
          then let () = v.item.var_type <- Shared in true
          else type_error expr
              "This expression has too many shared variables"
        | exception Not_found ->
          type_error v @@
          sprintf "Var %s is not defined"
            (Symbol.name v.item.var_name)
      end
    | ArithUnop (_, e) ->
      has_shared shared_allowed e.item
    | ArithBinop (_, e1, e2) ->
      has_shared
        (shared_allowed && (not (has_shared shared_allowed e1.item)))
        e2.item
  in ignore (has_shared shared_allowed expr.item)

let rec check_condition env = function
  | Bool _ -> ()
  | LogicUnop (_, c) ->
    check_condition env c.item
  | LogicBinop (_, c1, c2) ->
    check_condition env c1.item;
    check_condition env c2.item
  | ArithRel (_, e1, e2) ->
    check_expression env false e1;
    check_expression env false e2

let rec type_body env = function
  | Nothing
  | Pass
  | MFence -> env
  | Seq (b1, b2) ->
    type_body (type_body env b1.item) b2.item
  | Assign ({ item = { var_type; var_name } as x; _ }, exp) ->
    begin match Symbol.Map.Exceptionless.find var_name env with
      | Some Local
      | None ->
        check_expression env true exp;
        x.var_type <- Local;
        Symbol.Map.add var_name Local env
      | Some Shared ->
        check_expression env false exp;
        x.var_type <- Shared;
        env
    end
  | If (cond, body) ->
    check_condition env cond.item;
    type_body env body.item
  | While (cond, body) ->
    check_condition env cond.item;
    type_body env body.item
  | For (i, exp_from, exp_to, body) ->
    begin match Symbol.Map.Exceptionless.find i.item.var_name env with
      | None
      | Some Local ->
        i.item.var_type <- Local;
        let env_i = add_local_if_absent i.item.var_name env in
        check_expression env_i false exp_from;
        check_expression env_i false exp_to;
        type_body env_i body.item
      | Some Shared ->
        type_error i "For indices must not be shared variables"
    end

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
