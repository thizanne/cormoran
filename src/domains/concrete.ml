open Batteries
open Util
open Location

module O = Operation
module T = TypedAst
module Ty = Types

type state = {
  regs : (Sym.t * T.constant option) list list;
  mem : (Sym.t * T.constant option) list;
  buf : (Sym.t * T.constant option) list list;
}

module D = Set.Make (struct type t = state let compare = compare end)

type t = D.t

let bottom = D.empty

let is_bottom = D.is_empty

let equal = D.equal

let join = D.union

let widening _abstr1 abstr2 =
  abstr2

let smap f s = D.fold (fun x e -> D.add (f x) e) s D.empty

let set_reg state t x v = {
  state with
  regs =
    set_nth t ((x, v) :: List.nth state.regs t) state.regs
}

let get_local tid state r =
  List.assoc r (List.nth state.regs tid)

let get_shared tid state x =
  try
    List.assoc x (List.nth state.buf tid)
  with Not_found ->
    List.assoc x state.mem

let get_var tid state { T.var_sym; var_spec; _ } =
  match var_spec with
  | Ty.Local -> get_local tid state var_sym
  | Ty.Shared -> get_shared tid state var_sym

let get_sourced_var state var =
  match var.T.var_spec with
  | Source.Local thread_id ->
    get_local thread_id state var.T.var_sym
  | Source.View thread_id ->
    get_shared thread_id state var.T.var_sym
  | Source.Memory ->
    List.assoc var.T.var_sym state.mem

let set_local tid state x n = {
  state with
  regs =
    List.modify_at tid
      (List.modify_opt x (fun _ -> Some n))
      state.regs
}

let set_shared tid state x n = {
  state with
  buf =
    List.modify_at tid
      (List.cons (x, n))
      state.buf
}

let set_var tid state x n = match x.T.var_spec with
  | Ty.Local -> set_local tid state x.T.var_sym n
  | Ty.Shared -> set_shared tid state x.T.var_sym n

let nth_buf p t =
  List.nth p.buf t

let is_empty_buffer p t =
  nth_buf p t = []

let older_in_buf p t =
  fst @@ last @@ nth_buf p t

let is_older_in_buf p t x =
  try
    older_in_buf p t = x
  with Not_found -> false

let x_in_buf p t x =
  List.mem x (nth_buf p t)

let threads_x_older p x =
  0 -- (List.length p.buf - 1)
  |> Enum.filter (fun t -> is_older_in_buf p t x)
  |> List.of_enum

let flush s t =
  let (x, v) = last @@ nth_buf s t in {
    s with
    mem = set_assoc x v s.mem;
    buf =
      set_nth t (front @@ List.nth s.buf t) s.buf;
  }

let rec inser_all_first_pos x = function
  | [] -> [[x]]
  | y :: ys as yss ->
    if x = y
    then [x :: yss]
    else
      (x :: yss) ::
      List.map (fun yy -> y :: yy)
        (inser_all_first_pos x ys)

let rec all_combi = function
  | [] -> [[]]
  | [] :: bufs -> all_combi bufs
  | (t :: ts) :: bufs ->
    let ps = all_combi (ts :: bufs) in
    ps @
    List.flatten
      (List.map (inser_all_first_pos t) ps)

let flush_after_mop p _x =
  p.buf
  |> List.mapi (fun i buf -> List.make (List.length buf) i)
  |> all_combi
  |> List.map (List.fold_left flush p)

let rec get_expr :
  type t. _ -> t Ty.t -> (t, _) T.expression -> _ =
  fun get_var ty expr ->
    match ty, expr with
    | _, T.Int { Location.item = n; _ } -> Some (T.ConstInt n)
    | _, T.Bool { Location.item = b; _ } -> Some (T.ConstBool b)
    | _, T.Var { Location.item = v; _ } -> (* get_var v *) assert false
    | Ty.Int, T.Unop (op, expr) ->
      Option.map
        (T.arith_one_fun op.Location.item)
        (get_expr get_var expr.Location.item)
    | _, T.ArithBinop (op, expr1, expr2) ->
      begin
        try
          option_map2
            (T.arith_two_fun op.Location.item)
            (get_expr get_var expr1.Location.item)
            (get_expr get_var expr2.Location.item)
        with
          Division_by_zero -> None
      end

let rec validates_cond p =
  let open Program in
  let open Location in
  function
  | Bool b -> b.item
  | LogicUnop (op, c) ->
    T.logic_one_fun op.item
      (validates_cond p c.item)
  | LogicBinop (op, c1, c2) ->
    T.logic_two_fun op.item
      (validates_cond p c1.item)
      (validates_cond p c2.item)
  | ArithRel (rel, e1, e2) ->
    begin match
        get_expr (get_var_view p) e1.item,
        get_expr (get_var_view p) e2.item
      with
      | None, _ -> true
      | _, None -> true
      | Some n1, Some n2 -> T.logic_arith_two_fun rel.item n1 n2
    end

let transfer op domain = match op with
  | O.Identity -> domain
  | O.MFence tid -> D.filter (fun p -> is_empty_buffer p tid) domain
  | O.Filter c -> D.filter (fun p -> validates_cond p c) domain
  | O.Assign (tid, x, expr) ->
    let flush = match x.T.var_type, Program.shared_in_expr expr with
      | T.Local, [] -> List.singleton
      | T.Shared, [] -> fun p -> flush_after_mop p x.T.var_sym
      | T.Local, [y] -> fun p -> flush_after_mop p y
      | _ -> Error.not_implemented_msg_error "Several shared in expr"
    in
    let domain =
      domain
      |> D.elements
      |> List.map (fun p -> set_var tid p x (get_expr (get_var tid p) expr))
      |> List.map flush
      |> List.flatten
    in
    List.fold_right D.add domain D.empty

let initial_vars program =
  Sym.Map.map Option.some program.T.initial
  |> Sym.Map.enum
  |> List.of_enum

let initial_state program = {
  regs =
    List.map
      (fun { T.locals; _ } ->
         Sym.Set.fold
           (fun x acc -> (x, None) :: acc)
           locals [])
      program.T.threads;
  mem = initial_vars program;
  buf = List.map (fun _ -> []) program.T.threads
}

let init program = D.singleton (initial_state program)

let val_print output (x, n) =
  Printf.fprintf output "%a=%a"
    Sym.print x
    print_int_option n

let print_mem output =
  List.print val_print output

let print_thread output =
  List.print ~sep:"\n" ~first:"" ~last:"" print_mem output

let print_point output { regs; buf; mem } =
  Printf.fprintf output "Locals:\n%a\n\nBuffers:\n%a\n\nMemory:\n%a"
    print_thread regs
    print_thread buf
    print_mem mem

let print output =
  D.print print_point output
    ~first:"" ~last:"" ~sep:"\n────────\n"
