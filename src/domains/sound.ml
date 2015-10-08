open Batteries

module P = Program
module O = Cfg.Operation
module L = Location

module Key = struct
  type source =
    | Mem
    | TopBuffer
    | BottomBuffer
    [@@ deriving ord]

  let down_source = function
    | Mem -> TopBuffer
    | TopBuffer -> BottomBuffer
    | BottomBuffer -> BottomBuffer

  let up_source = function
    | Mem -> raise @@ Invalid_argument "up_source"
    | TopBuffer -> Mem
    | BottomBuffer -> TopBuffer

  let source_is_mem = function
    | Mem -> true
    | TopBuffer
    | BottomBuffer -> false

  module M = Symbol.Map

  type thread_sources = source M.t

  type t = thread_sources list

  let compare =
    List.compare (Symbol.Map.compare compare_source)

  let print _output _ = failwith "Sound.Key.print not implemented"

  let init_thread_sources prog =
    Symbol.Map.fold
      (fun var _init -> M.add var Mem)
      prog.Program.initial
      M.empty

  let init prog =
    List.map (fun _ -> init_thread_sources prog) prog.Program.threads

  let tid_is_consistent keys tid =
    M.for_all (fun _key -> source_is_mem) @@ List.nth keys tid

  let get_source tid var keys =
    M.find var @@ List.nth keys tid

  let up tid var keys =
    List.modify_at tid (M.modify var up_source) keys

  let down tid var keys =
    List.modify_at tid (M.modify var down_source) keys
end

module Make (Inner : Domain.Inner) = struct

  module M = Map.Make (Key)

  type t = Inner.t M.t

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

  let is_bottom =
    M.for_all (fun _key d -> Inner.is_bottom d)

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  let print output =
    M.print
      ~first:"" ~last:"" ~kvsep:":\n" ~sep:"\n────────\n"
      Key.print Inner.print output

  let abstract_var_sym = Symbol.namespace ()

  let sym_local tid reg =
    abstract_var_sym @@ Printf.sprintf "%d:%s" tid (Symbol.name reg)

  let sym_local_var tid var =
    sym_local tid var.P.var_name

  let sym_thread_locals tid { P.locals; _ } =
    Symbol.Set.fold
      (fun sym acc -> sym_local tid sym :: acc)
      locals
      []

  let sym_mem var =
    abstract_var_sym @@ Printf.sprintf "%s:mem" (Symbol.name var)

  let sym_top tid var =
    abstract_var_sym @@ Printf.sprintf "%s:top:%d" (Symbol.name var) tid

  let sym_bot tid var =
    abstract_var_sym @@ Printf.sprintf "%s:bot:%d" (Symbol.name var) tid

  let init prog =
    let local_syms =
      List.fold_lefti
        (fun syms tid thread -> syms @ sym_thread_locals tid thread)
        []
        prog.P.threads in
    let mem_syms =
      Symbol.Map.fold
        (fun var _init acc -> sym_mem var :: acc)
        prog.P.initial
        []
    in
    M.singleton (Key.init prog) (Inner.init @@ local_syms @ mem_syms)

  let sym_local_view { P.thread_id; var } =
    match var.P.var_type with
    | P.Local -> sym_local thread_id var.P.var_name
    | P.Shared -> raise @@ Invalid_argument "sym_local_view"

  let rec symbolize_expr sym = function
    (* TODO: write a proper mapper *)
    | P.Int n -> P.Int n
    | P.Var var ->
      P.Var (L.comap sym var)
    | P.ArithUnop (op, e) ->
      P.ArithUnop (op, L.comap (symbolize_expr sym) e)
    | P.ArithBinop (op, e1, e2) ->
      P.ArithBinop (
        op,
        L.comap (symbolize_expr sym) e1,
        L.comap (symbolize_expr sym) e2
      )

  let rec symbolize_cond sym = function
    | P.Bool b -> P.Bool b
    | P.LogicUnop (op, c) ->
      P.LogicUnop (op, L.comap (symbolize_cond sym) c)
    | P.LogicBinop (op, c1, c2) ->
      P.LogicBinop (
        op,
        L.comap (symbolize_cond sym) c1,
        L.comap (symbolize_cond sym) c2
      )
    | P.ArithRel (op, e1, e2) ->
      P.ArithRel (
        op,
        L.comap (symbolize_expr sym) e1,
        L.comap (symbolize_expr sym) e2
      )

  let write tid (key, abstr) x e =
    let x_top = sym_top tid x in
    let x_bot = sym_bot tid x in
    match Key.get_source tid x key with
    | Key.Mem ->
      Key.up tid x key,
      (* x_top := add e *)
      abstr
      |> Inner.add x_top
      |> Inner.assign_expr x_top e
    | Key.TopBuffer ->
      Key.up tid x key,
      (* x_bot :=add x_top; x_top := e *)
      abstr
      |> Inner.add x_bot
      |> Inner.assign_expr x_bot (P.Var (L.mkdummy x_top))
      |> Inner.assign_expr x_top e
    | Key.BottomBuffer ->
      (* cf Numeric Domains with Summarized Dimensions, Gopan et al. *)
      let x_tmp = abstract_var_sym ":::" in
      key,
      (* x_bot[*] := x_top; x_top := e *)
      abstr
      |> Inner.add x_tmp
      |> Inner.assign_expr x_tmp (P.Var (L.mkdummy x_top))
      |> Inner.fold x_bot x_tmp
      |> Inner.assign_expr x_top e

  let read tid r x expr key abstr =
    (* x is expected to be the symbol of the shared variable in expr *)
    let x_mem_expr = symbolize_expr sym_mem expr in
    let x_top_expr = symbolize_expr (sym_top tid) expr in
    match Key.get_source tid x key with
    | Key.Mem ->
      Inner.assign_expr r x_mem_expr abstr
    | Key.TopBuffer
    | Key.BottomBuffer ->
      Inner.assign_expr r x_top_expr abstr

  let local_op r e abstr =
    Inner.assign_expr r e abstr

  let transfer op d =
    match op with
    | O.Identity -> d
    | O.MFence tid ->
      M.filter (fun key _ -> Key.tid_is_consistent key tid) d
    | O.Filter cond ->
      d
      |> M.map (Inner.meet_cons @@ symbolize_cond sym_local_view cond)
      |> normalize
    | O.Assign (tid, x, expr) ->
      begin match x.P.var_type, P.shared_in_expr expr with
        | P.Local, [] ->
          M.map
            (local_op
               (sym_local tid x.P.var_name)
               (symbolize_expr (sym_local_var tid) expr))
            d
        | P.Shared, [] ->
          failwith "TODO"
        | P.Local, [y] ->
          M.mapi
            (read tid (sym_local tid x.P.var_name) y expr)
            d
        | _ -> failwith "transfer"
      end (* TODO close flushes *)

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
