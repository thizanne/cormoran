open Batteries
open Util

module L = Location
module T = TypedAst
module U = UntypedAst
module Ty = Types
module MT = Context.MaybeThreaded

type 'id var_loc_typer =
  (* We need higher order polymorphism in type_expression *)
  { f : 't. 't Ty.t -> 'id L.loc -> ('id, 't) T.var L.loc }

let type_program_var env expected_type loc var_sym =
    let found_type, found_origin = Env.get_entry loc var_sym env in
    if Env.are_compatible_types found_type expected_type
    then {
      T.var_type = expected_type;
      var_origin = found_origin;
      var_id = var_sym;
    }
    else
      Error.type_loc_error loc @@
      Printf.sprintf2 "Var %s has type %a but type %a was expected"
        (Sym.name var_sym)
        Env.print_ty found_type
        Ty.print expected_type

let type_program_var_loc env expected_type { L.item = var_sym; loc } =
  { L.item = type_program_var env expected_type loc var_sym; loc }

let program_var_loc_typer env =
  { f = fun ty var_loc -> type_program_var_loc env ty var_loc }

let type_arith_unop_loc =
  L.comap (
    function
    | U.Neg -> T.Neg
  )

let type_logic_unop_loc =
  L.comap (
    function
    | U.Not -> T.Not
  )

let type_arith_binop_loc =
  L.comap (
    function
    | U.Add -> T.Add
    | U.Sub -> T.Sub
    | U.Mul -> T.Mul
    | U.Div -> T.Div
  )

let type_logic_binop_loc =
  L.comap (
    function
    | U.And -> T.And
    | U.Or -> T.Or
  )

let type_arith_relop_loc =
  L.comap (
    function
    | U.Eq -> T.Eq
    | U.Neq -> T.Neq
    | U.Lt -> T.Lt
    | U.Gt -> T.Gt
    | U.Le -> T.Le
    | U.Ge -> T.Ge
  )

let exp_loc_type_expected env exp_loc = match exp_loc.L.item with
  | U.Bool _ -> Env.Bool
  | U.Int _ -> Env.Int
  | U.Var v -> Env.get_loc_type v env
  | U.ArithUnop _ -> Env.Int
  | U.ArithBinop _ -> Env.Int
  | U.LogicUnop _ -> Env.Bool
  | U.LogicBinop _ -> Env.Bool
  | U.ArithRelop _ -> Env.Bool

let rec type_expression :
  type t.
  'id var_loc_typer ->
  t Ty.t ->
  L.t ->
  'id U.expression ->
  ('id, t) T.expression =
  fun var_loc_typer expected_type loc expr ->
    match expected_type, expr with
    | Ty.Int, U.Int n -> T.Int n
    | Ty.Bool, U.Bool b -> T.Bool b
    | _, U.Var var ->
      T.Var (var_loc_typer.f expected_type var)
    | Ty.Int, U.ArithUnop (op, exp) ->
      T.Unop (
        type_arith_unop_loc op,
        type_expression_loc var_loc_typer Ty.Int exp
      )
    | Ty.Bool, U.LogicUnop (op, exp) ->
      T.Unop (
        type_logic_unop_loc op,
        type_expression_loc var_loc_typer Ty.Bool exp
      )
    | Ty.Int, U.ArithBinop (op, exp1, exp2) ->
      T.Binop (
        type_arith_binop_loc op,
        type_expression_loc var_loc_typer Ty.Int exp1,
        type_expression_loc var_loc_typer Ty.Int exp2
      )
    | Ty.Bool, U.LogicBinop (op, exp1, exp2) ->
      T.Binop (
        type_logic_binop_loc op,
        type_expression_loc var_loc_typer Ty.Bool exp1,
        type_expression_loc var_loc_typer Ty.Bool exp2
      )
    | Ty.Bool, U.ArithRelop (op, exp1, exp2) ->
      T.Binop (
        type_arith_relop_loc op,
        type_expression_loc var_loc_typer Ty.Int exp1,
        type_expression_loc var_loc_typer Ty.Int exp2
      )
    | _ ->
      Error.type_loc_error loc @@
      Printf.sprintf2 "This expression cannot have type %a"
        Ty.print expected_type

and type_expression_loc :
  type t.
  'id var_loc_typer ->
  t Ty.t ->
  'id U.expression L.loc ->
  ('id, t) T.expression L.loc =
  fun var_loc_typer ty { L.item = exp; loc } ->
    { L.item = type_expression var_loc_typer ty loc exp; loc }

let rec type_body ((env, labels) as info) = function
  | U.Nothing -> T.Nothing, (env, labels)
  | U.Pass -> T.Pass, (env, labels)
  | U.MFence -> T.MFence, (env, labels)
  | U.Label lbl ->
    if Sym.Set.mem lbl.L.item labels
    then Error.name_error lbl "Label already defined on this thread"
    else T.Label lbl, (env, Sym.Set.add lbl.L.item labels)
  | U.Seq (first, second) ->
    let first, info = type_body_loc info first in
    let second, info = type_body_loc info second in
    T.Seq (first, second), info
  | U.Assign (var, exp) ->
    let var_type, var_origin =
      match Env.get_loc_entry_option var env with
      | None -> exp_loc_type_expected env exp, Ty.Local
      | Some entry -> entry in
    let env = Env.add var.L.item var_type var_origin env in
    begin
      match var_type with
      | Env.Int ->
        T.Assign (
          type_program_var_loc env Ty.Int var,
          type_expression_loc (program_var_loc_typer env) Ty.Int exp
        )
      | Env.Bool ->
        T.Assign (
          type_program_var_loc env Ty.Bool var,
          type_expression_loc (program_var_loc_typer env) Ty.Bool exp
        )
    end, (env, labels)
  | U.If (condition, body) ->
    let body, info = type_body_loc info body in
    T.If (
      type_expression_loc (program_var_loc_typer env) Ty.Bool condition,
      body
    ), info
  | U.While (condition, body) ->
    let body, info = type_body_loc info body in
    T.While (
      type_expression_loc (program_var_loc_typer env) Ty.Bool condition,
      body
    ), info
  | U.For (var, exp_from, exp_to, body) ->
    let env =  match Env.get_loc_entry_option var env with
      | None -> Env.add var.L.item Env.Int Ty.Local env
      | Some (Env.Int, _) -> env
      | Some (Env.Bool, _ ) ->
        Error.type_error var "A `for` indice must have type Int, not Bool" in
    let var = type_program_var_loc env Ty.Int var in
    let exp_from =
      type_expression_loc (program_var_loc_typer env) Ty.Int exp_from in
    let exp_to =
      type_expression_loc (program_var_loc_typer env) Ty.Int exp_to in
    let body, info = type_body_loc (env, labels) body in
    T.For (var, exp_from, exp_to, body), info

and type_body_loc info { L.item = body; loc } =
  let body, info = type_body info body in
  { L.item = body; loc }, info

let count_shared =
  T.fold_expr
    { T.f = fun var acc -> if T.is_shared var then succ acc else acc }
    0

let check_label_defined labels bound =
  match bound with
  | None -> ()
  | Some label ->
    if not (Sym.Set.mem label.L.item labels)
    then
      Error.name_error label @@
      Printf.sprintf "Label %s undefined"
        (Sym.name label.L.item)

let check_interval labels { Property.initial; final } =
  check_label_defined labels initial;
  check_label_defined labels final

let check_thread_zone labels thread_zone =
  List.iter (check_interval labels) thread_zone

let check_zone all_labels zone =
  let tid_found = Array.make (List.length all_labels) false in
  List.iter
    (fun ({ L.item = tid; _ } as tid_loc, thread_zone) ->
       (* Check that each tid is present at most once *)
       if tid_found.(tid)
       then
         Error.type_error tid_loc @@
         Printf.sprintf "Thread id %d already present in zone definition" tid
       else tid_found.(tid) <- true;

       (* Check that labels are correct *)
       check_thread_zone (List.nth all_labels tid) thread_zone)
    zone

let type_property_var
    (global_env, thread_envs)
    expected_type loc ({ MT.item = var_sym; thread_id } as mt_var)
  =
  let env = match thread_id with
    | None -> global_env
    | Some tid -> List.nth thread_envs tid in
  let found_type, found_origin = Env.get_entry loc var_sym env in
  if Env.are_compatible_types found_type expected_type
  then {
    T.var_type = expected_type;
    var_origin = found_origin;
    var_id = mt_var;
  }
  else
    Error.type_loc_error loc @@
    Printf.sprintf2 "Var %s has type %a but type %a was expected"
      (Sym.name var_sym)
      Env.print_ty found_type
      Ty.print expected_type

let type_property_var_loc all_envs expected_type { L.item = mt_var; loc } =
  { L.item = type_property_var all_envs expected_type loc mt_var; loc }

let property_var_loc_typer all_envs =
  { f = fun ty var_loc -> type_property_var_loc all_envs ty var_loc }

let type_property all_labels all_envs
    (zone, condition) =
  let () = match zone with
    | None -> ()
    | Some zone -> check_zone all_labels zone in

  let condition =
    type_expression_loc
      (property_var_loc_typer all_envs)
      Ty.Bool condition in

  { Property.zone; condition }

let initial_constant_entry = function
  | U.ConstInt _ -> Env.Int, Ty.Shared
  | U.ConstBool _ -> Env.Bool, Ty.Shared

let type_initial_constant = function
  | U.ConstInt n -> T.ConstInt n
  | U.ConstBool b -> T.ConstBool b

let type_program ({ U.initial; threads }, properties) =
  let shared_env = Sym.Map.map initial_constant_entry initial in
  let thread_results =
    List.map
      (fun t -> type_body_loc (shared_env, Sym.Set.empty) t.U.body)
      threads in

  (* Yup, this is non-optimal. Who cares, it's fast anyway. *)
  let thread_envs = List.map (fst @@@ snd) thread_results in
  let all_labels = List.map (snd @@@ snd) thread_results in
  let bodies = List.map fst thread_results in

  let properties =
    List.map
      (type_property all_labels (shared_env, thread_envs))
      properties in

  let thread thread_env body = {
    T.body;
    locals =
      thread_env
      |> Sym.Map.map snd
      |> Sym.Map.filterv (( = ) Ty.Local)
      |> Sym.Map.keys
      |> Sym.Set.of_enum;
  } in

  let initial = Sym.Map.map type_initial_constant initial in
  let threads = List.map2 thread thread_envs bodies in

  { T.initial; threads }, properties
