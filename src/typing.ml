open Batteries
open Printf
open Error
open Program

module L = Location

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
      begin match Symbol.Map.find v.L.item.var_name env with
        | Local -> false
        | Shared ->
          if shared_allowed
          then true
          else type_error expr
              "This expression has too many shared variables"
        | exception Not_found ->
          type_error v @@
          sprintf "Var %s is not defined"
            (Symbol.name v.L.item.var_name)
      end
    | ArithUnop (_, e) ->
      has_shared shared_allowed e.L.item
    | ArithBinop (_, e1, e2) ->
      has_shared
        (shared_allowed && (not (has_shared shared_allowed e1.L.item)))
        e2.L.item
  in ignore (has_shared shared_allowed expr.L.item)

let rec check_condition env = function
  | Bool _ -> ()
  | LogicUnop (_, c) ->
    check_condition env c.L.item
  | LogicBinop (_, c1, c2) ->
    check_condition env c1.L.item;
    check_condition env c2.L.item
  | ArithRel (_, e1, e2) ->
    check_expression env false e1;
    check_expression env false e2

let type_expression env (b : bool) (e : Symbol.t expression L.loc) :
  var expression L.loc =
  assert false

let type_condition env (c : Symbol.t condition L.loc) : var condition L.loc =
  assert false

let rec type_body ((env, labels) as info) { L.item = b; loc } =
  match b with
  | Nothing -> { L.item = Nothing; loc }, info
  | Pass -> { L.item = Pass; loc }, info
  | MFence -> { L.item = MFence; loc }, info
  | Label lbl ->
    if Symbol.Set.mem lbl.L.item labels
    then name_error lbl "Label already defined on this thread"
    else {
      L.item = Label lbl;
      loc;
    }, (env, Symbol.Set.add lbl.L.item labels)
  | Seq (b1, b2) ->
    let b1', info' = type_body info b1 in
    let b2', info'' = type_body info' b2 in
    { L.item = Seq (b1', b2'); loc }, info''
  | Assign ({ L.item = var_name; loc = var_loc }, exp) ->
    let var_type, exp', env' =
      match Symbol.Map.Exceptionless.find var_name env with
      | Some Local
      | None ->
        Local,
        type_expression env true exp,
        Symbol.Map.add var_name Local env
      | Some Shared ->
        Shared,
        type_expression env false exp,
        env
    in
    let var = { L.item = { var_type; var_name }; loc = var_loc } in
    { L.item = Assign (var, exp'); loc }, (env', labels)
  | If (cond, body) ->
    let cond' = type_condition env cond in
    let body', info' = type_body info body in
    { L.item = If (cond', body'); loc }, info'
  | While (cond, body) ->
    let cond' = type_condition env cond in
    let body', info' = type_body info body in
    { L.item = While (cond', body'); loc }, info'
  | For ({ L.item = i_name; loc = i_loc } as i, exp_from, exp_to, body) ->
    let info_i =
      match Symbol.Map.Exceptionless.find i_name env with
      | None
      | Some Local ->
        let env_i = add_local_if_absent i_name env in
        type_body (env_i, labels) body
      | Some Shared ->
        type_error i "For indices must not be shared variables"
    in
    let var_i = {
      L.item = { var_name = i_name; var_type = Local };
      loc = i_loc
    } in
    let exp_from' = type_expression info_i false exp_from in
    let exp_to' = type_expression info_i false exp_to in
    let body', info' = type_body info_i body in {
      L.item = For (var_i, exp_from', exp_to', body');
      loc
    }, info'

let type_thread shared_env { locals; body } =
  let env, _ = type_body (shared_env, Symbol.Set.empty) body in {
    body;
    locals =
      env
      |> Symbol.Map.filterv (( = ) Local)
      |> Symbol.Map.keys
      |> Symbol.Set.of_enum
  }

let type_program { initial; threads } =
  (* TODO: check that the property is well formed *)
  let shared_env = Symbol.Map.map (fun _ -> Shared) initial in {
    initial;
    threads = List.map (type_thread shared_env) threads;
    properties
  }
