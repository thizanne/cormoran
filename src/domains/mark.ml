open Batteries

module T = TypedAst
module Ty = Types
module O = Operation
module L = Location
module Dom = Domain

module Key = struct
  type presence =
    | Zero
    | One
    | MoreThanOne
    [@@ deriving ord]

  let up_presence = function
    | Zero-> One
    | One -> MoreThanOne
    | MoreThanOne -> MoreThanOne

  let down_presence = function
    | Zero-> raise @@ Invalid_argument "down_presence"
    | One -> Zero
    | MoreThanOne -> One

  let presence_is_zero = function
    | Zero -> true
    | One
    | MoreThanOne -> false

  let print_presence output = function
    | Zero -> String.print output "0"
    | One -> String.print output "1"
    | MoreThanOne -> String.print output ">1"

  module M = Sym.Map

  type thread_presences = presence M.t

  type t = thread_presences list

  let compare =
    List.compare (Sym.Map.compare compare_presence)

  let init_thread_presences prog =
    Sym.Map.map
      (fun _init -> Zero)
      prog.T.initial

  let init prog =
    List.map (fun _ -> init_thread_presences prog) prog.T.threads

  let tid_is_consistent keys tid =
    M.for_all (fun _key -> presence_is_zero) @@ List.nth keys tid

  let get_presence tid var keys =
    M.find var @@ List.nth keys tid

  let down tid var keys =
    List.modify_at tid (M.modify var down_presence) keys

  let up tid var keys =
    List.modify_at tid (M.modify var up_presence) keys

  let get_flushable_tids keys x =
    List.filteri_map
      (fun tid buf ->
         if presence_is_zero (M.find x buf) then None
         else Some tid)
      keys

  let print output =
    List.print ~first:"[" ~last:"]" ~sep:"]\n["
      (M.print ~first:"" ~last:"" ~sep:";" ~kvsep:"→"
         Sym.print print_presence)
      output
end

module Make (Inner : Domain.Inner) = struct

  module M = Map.Make (Key)

  type t = Inner.t M.t

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

  let bottom = M.empty

  let is_bottom =
    M.for_all (fun _key d -> Inner.is_bottom d)

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  let print output =
    M.print
      ~first:"" ~last:"" ~kvsep:":\n\n" ~sep:"\n────────\n"
      Key.print Inner.print output

  let inner_var_sym = Sym.namespace ()

  let sym_local tid reg =
    inner_var_sym @@ Printf.sprintf "%d:%s" tid (Sym.name reg)

  let sym_local_var tid var =
    sym_local tid var.T.var_sym

  let sym_mem var =
    inner_var_sym @@ Printf.sprintf "%s:mem" (Sym.name var)

  let sym_top tid var =
    inner_var_sym @@ Printf.sprintf "%s:top:%d" (Sym.name var) tid

  let sym_bot tid var =
    inner_var_sym @@ Printf.sprintf "%s:bot:%d" (Sym.name var) tid

  let inner_var make_sym var =
    { var with T.var_spec = make_sym var.T.var_sym }

  let inner_var_local tid = inner_var (sym_local tid)

  let inner_var_mem = inner_var sym_mem

  let inner_var_top tid = inner_var (sym_top tid)

  let inner_var_bot tid = inner_var (sym_bot tid)

  (* TODO: make Sym able to generate fresh names *)
  let inner_var_tmp var =
    { var with T.var_spec = inner_var_sym "::tmp::" }

  let add_join bufs abstr d =
    (* Adds (bufs, abstr) as a d element, making a join if the bufs
       key is already present *)
    M.modify_def abstr bufs (Inner.join abstr) d

  let initial_thread_locals tid { T.locals; _ } =
    Sym.Map.fold
      (fun var_sym ty (ints, bools) ->
         match ty with
         | Env.Int ->
           let var =
             { T.var_sym; var_type = Ty.Int; var_spec = sym_local tid var_sym } in
           (var, None) :: ints, bools
         | Env.Bool ->
           let var =
             { T.var_sym; var_type = Ty.Bool; var_spec = sym_local tid var_sym } in
           ints, (var, None) :: bools)
      locals
      ([], [])

  let initial_mem { T.initial; _ } =
    Sym.Map.fold
      (fun var_sym ty (ints, bools) ->
         match ty with
         | T.ConstInt n ->
           let var =
             { T.var_sym; var_type = Ty.Int; var_spec = sym_mem var_sym } in
           (var, Some n) :: ints, bools
         | T.ConstBool b ->
           let var =
             { T.var_sym; var_type = Ty.Bool; var_spec = sym_mem var_sym } in
           ints, (var, Some b) :: bools)
      initial
      ([], [])

  let init prog =
    let local_ints, local_bools =
      List.fold_lefti
        (fun (int_acc, bool_acc) tid thread ->
           let int_thread, bool_thread = initial_thread_locals tid thread
           in int_thread @ int_acc, bool_thread @ bool_acc)
        ([], [])
        prog.T.threads in
    let mem_ints, mem_bools =
      initial_mem prog in
    let initial_ints, initial_bools =
      local_ints @ mem_ints, local_bools @ mem_bools in
    M.singleton (Key.init prog) (Inner.init initial_ints initial_bools)

  let sym_var_view key { T.thread_id; var = { T.var_type; var_name } } =
    match var_type with
    | T.Local -> sym_local thread_id var_name
    | T.Shared ->
      match Key.get_presence thread_id var_name key with
      | Key.Zero -> sym_mem var_name
      | Key.One
      | Key.MoreThanOne -> sym_top thread_id var_name

  let rec symbolize_expr sym = function
    (* TODO: write a proper mapper *)
    | T.Int n -> T.Int n
    | T.Var var ->
      T.Var (L.comap sym var)
    | T.ArithUnop (op, e) ->
      T.ArithUnop (op, L.comap (symbolize_expr sym) e)
    | T.ArithBinop (op, e1, e2) ->
      T.ArithBinop (
        op,
        L.comap (symbolize_expr sym) e1,
        L.comap (symbolize_expr sym) e2
      )

  let rec symbolize_cond sym = function
    | T.Bool b -> T.Bool b
    | T.LogicUnop (op, c) ->
      T.LogicUnop (op, L.comap (symbolize_cond sym) c)
    | T.LogicBinop (op, c1, c2) ->
      T.LogicBinop (
        op,
        L.comap (symbolize_cond sym) c1,
        L.comap (symbolize_cond sym) c2
      )
    | T.ArithRel (op, e1, e2) ->
      T.ArithRel (
        op,
        L.comap (symbolize_expr sym) e1,
        L.comap (symbolize_expr sym) e2
      )

  let write tid x e key abstr =
    let x_sym = x.T.var_sym in
    let x_top = inner_var_top tid x in
    let x_bot = inner_var_bot tid x in
    match Key.get_presence tid x.T.var_sym key with
    | Key.Zero ->
      Key.up tid x_sym key,
      (* x_top := add e *)
      abstr
      |> Inner.add x_top
      |> Inner.assign_expr x_top e
    | Key.One ->
      Key.up tid x_sym key,
      (* x_bot :=add x_top; x_top := e *)
      abstr
      |> Inner.add x_bot
      |> Inner.assign_expr x_bot (T.Var (L.mkdummy x_top))
      |> Inner.assign_expr x_top e
    | Key.MoreThanOne ->
      (* cf Numeric Domains with Summarized Dimensions, Gopan et al. Tacas04 *)
      let x_tmp = inner_var_tmp x in
      key,
      (* x_bot[*] := x_top; x_top := e *)
      abstr
      |> Inner.add x_tmp
      |> Inner.assign_expr x_tmp (T.Var (L.mkdummy x_top))
      |> Inner.fold x_bot x_tmp
      |> Inner.assign_expr x_top e

  let sym_hybrid_expr tid x sym_x =
    (* Symize an expression of Program.var which may contain
       a shared variable x. The other variables are symbolized as
       registers. *)
    let sym { T.var_name; var_type } = match var_type with
      | T.Local -> sym_local tid var_name
      | T.Shared ->
        if Sym.Ord.compare var_name x == 0
        then sym_x x
        else raise @@ Invalid_argument "sym_hybrid_expr"
    in
    symbolize_expr sym

  let read tid r x expr key abstr =
    (* x is expected to be the symbol of the shared variable in expr *)
    let x_mem_expr = sym_hybrid_expr tid x sym_mem expr in
    let x_top_expr = sym_hybrid_expr tid x (sym_top tid) expr in
    match Key.get_presence tid x key with
    | Key.Zero->
      Inner.assign_expr r x_mem_expr abstr
    | Key.One
    | Key.MoreThanOne ->
      Inner.assign_expr r x_top_expr abstr

  let local_op r e abstr =
    Inner.assign_expr r e abstr

  let add_flush_x_tid x key abstr acc tid =
    (* Adds to acc the result(s) of one flush of x from tid *)
    let x_sym = x.T.var_sym in
    let x_mem = inner_var_mem x in
    let x_top = inner_var_top tid x in
    let x_bot = inner_var_bot tid x in
    let x_bot_expr = T.Var (L.mkdummy x_bot) in
    let x_top_expr = T.Var (L.mkdummy x_top) in
    let x_tmp = inner_var_tmp x in
    let x_tmp_expr = T.Var (L.mkdummy x_tmp) in
    match Key.get_presence tid x_sym key with
    | Key.Zero -> raise @@ Invalid_argument "flush_x_tid"
    | Key.One ->
      let key = Key.down tid x_sym key in
      let abstr =
        abstr
        |> Inner.assign_expr x_mem x_top_expr
        |> Inner.drop x_top in
      add_join key abstr acc
    | Key.MoreThanOne ->
      (* >1 -> >1 => x_mem := x_bot[*] *)
      let abstr_gt1 =
        abstr
        |> Inner.expand x_bot x_tmp
        |> Inner.assign_expr x_mem x_tmp_expr
        |> Inner.drop x_tmp in
      (* >1 -> 1 => x_mem := x_bot[*]; del x_bot *)
      let key_1 = Key.down tid x_sym key in
      let abstr_1 =
        (* x_tmp is not needed here *)
        abstr
        |> Inner.assign_expr x_mem x_bot_expr
        |> Inner.drop x_bot in
      (* Make the joins *)
      acc
      |> add_join key abstr_gt1
      |> add_join key_1 abstr_1

  let add_flush_x x key abstr acc_init =
    (* Adds to acc the result(s) of one flush of x from each relevant
       tid *)
    let tids = Key.get_flushable_tids key x.T.var_sym in
    List.fold_left
      (add_flush_x_tid x key abstr)
      acc_init
      tids

  let iterate_one_flush x d =
    (* Does one iteration of flushing one x from each point of d,
       adding the result to acc_init *)
    M.fold (add_flush_x x) d d

  let rec close_by_flush x d =
    let d' = iterate_one_flush x d in
    if equal d d' then d
    else close_by_flush x d'

  let shared_var_folder =
    let f : type a. a T.program_var -> _ -> _ =
      fun var acc ->
        match var.T.var_spec with
        | Ty.Local -> acc
        | Ty.Shared -> var :: acc
    in
    { T.f }

  let transfer op d =
    match op with
    | O.Identity -> d
    | O.MFence tid ->
      M.filter (fun key _ -> Key.tid_is_consistent key tid) d
    | O.Filter cond ->
      d
      |> M.mapi
        (fun key -> Inner.meet_cons @@ symbolize_cond (sym_var_view key) cond)
      |> normalize
    | O.Assign (tid, x, expr) ->
      begin match x.T.var_spec, T.fold_expr shared_var_folder [] expr with
        | Ty.Local, [] ->
          M.map
            (local_op
               (inner_var_local tid x)
               (symbolize_expr (sym_local_var tid) expr))
            d
        | Ty.Shared, [] ->
          M.Labels.fold
            ~f:(fun ~key ~data:abstr acc ->
                let key', abstr' =
                  write tid x
                    (symbolize_expr (sym_local_var tid) expr)
                    key abstr
                in M.add key' abstr' acc)
            ~init:bottom
            d
          |> close_by_flush x
        | Ty.Local, [y] ->
          M.mapi
            (read tid (inner_var_local tid x) y expr)
            d
          |> close_by_flush y
        | _ -> raise @@ Invalid_argument "Mark.transfer"
      end

  let join =
    M.merge
      (fun _key abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> abstr1
         | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2))

  let widening =
    M.merge
      (fun _key abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> failwith "Abstract.widening"
         | Some abstr1, Some abstr2 ->
           Some (Inner.widening abstr1 abstr2))
end
