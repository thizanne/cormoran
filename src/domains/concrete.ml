open Batteries
open Util
open Location

module Op = Cfg.Operation
module P = Program

type state = {
  regs : (Symbol.t * int option) list list;
  mem : (Symbol.t * int option) list;
  buf : (Symbol.t * int option) list list;
}

module D = Set.Make (struct type t = state let compare = compare end)

type t = D.t

let bottom = D.empty

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

let get_reg s t r =
  List.assoc r (List.nth s.regs t)

let get_var state t x =
  try
    List.assoc x (List.nth state.buf t)
  with Not_found ->
    List.assoc x state.mem

let set_local state tid x n = {
  state with
  regs =
    List.modify_at tid
      (List.modify_opt x (fun _ -> Some n))
      state.regs
}

let set_shared state tid x n = {
  state with
  buf =
    List.modify_at tid
      (List.cons (x, n))
      state.buf
}

let set_var state tid x n = match x.P.var_type with
  | P.Local -> set_local state tid x.P.var_name n
  | P.Shared -> set_shared state tid x.P.var_name n

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

let flush_after_mop p x =
  p.buf
  |> List.mapi (fun i buf -> i, buf)
  |> List.filter
    (fun (_i, buf) ->
       try fst @@ last buf = x with Not_found -> false)
  |> List.map (fun (i, buf) -> List.make (List.length buf) i)
  |> all_combi
  |> List.map (List.fold_left flush p)

let rec get_expr p thread =
  function
  | P.Int { Location.item = n; _ } -> Some n
  | P.Var { Location.item = v; _ } ->
    if P.is_local v
    then get_reg p thread v.P.var_name
    else get_var p thread v.P.var_name
  | P.ArithUnop (op, expr) ->
    Option.map
      (P.fun_of_arith_unop op.Location.item)
      (get_expr p thread expr.Location.item)
  | P.ArithBinop (op, expr1, expr2) ->
    begin
      try
        option_map2
          (P.fun_of_arith_binop op.Location.item)
          (get_expr p thread expr1.Location.item)
          (get_expr p thread expr2.Location.item)
      with
        Division_by_zero -> None
    end

let rec validates_cond p thread =
  let open Program in
  let open Location in
  function
  | Bool b -> b.item
  | LogicUnop (op, c) ->
    fun_of_logic_unop op.item
      (validates_cond p thread c.item)
  | LogicBinop (op, c1, c2) ->
    fun_of_logic_binop op.item
      (validates_cond p thread c1.item)
      (validates_cond p thread c2.item)
  | ArithRel (rel, e1, e2) ->
    begin match get_expr p thread e1.item, get_expr p thread e2.item with
      | None, _ -> true
      | _, None -> true
      | Some n1, Some n2 -> fun_of_arith_rel rel.item n1 n2
    end

let transfer domain {P.thread_id = t; elem = op} = match op with
  | Op.Identity -> domain
  | Op.MFence -> D.filter (fun p -> is_empty_buffer p t) domain
  | Op.Filter c -> D.filter (fun p -> validates_cond p t c) domain
  | Op.Assign (x, expr) ->
    let flush = match x.P.var_type, Program.shared_in_expr expr with
      | P.Local, [] -> List.singleton
      | P.Shared, [] -> fun p -> flush_after_mop p x.P.var_name
      | P.Local, [y] -> fun p -> flush_after_mop p y
      | _ -> Error.not_implemented_msg_error "Several shared in expr"
    in
    let domain =
      domain
      |> D.elements
      |> List.map (fun p -> set_var p t x (get_expr p t expr))
      |> List.map flush
      |> List.flatten
    in
    List.fold_right D.add domain D.empty

let initial_vars program =
  Symbol.Map.map Option.some program.P.initial
  |> Symbol.Map.enum
  |> List.of_enum

let initial_state program = {
  regs =
    List.map
      (fun { P.locals; _ } ->
         Symbol.Set.fold
           (fun x acc -> (x, None) :: acc)
           locals [])
      program.P.threads;
  mem = initial_vars program;
  buf = List.map (fun _ -> []) program.P.threads
}

let init program = D.singleton (initial_state program)

let val_print output (x, n) =
  Printf.fprintf output "%a=%a"
    Symbol.print x
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

let is_totally_flushed p =
  List.for_all (( = ) []) p.buf

let state_sat_cond (var, value) s =
  Error.not_implemented_msg_error "Sat not implemented"

let state_sat cond s =
  is_totally_flushed s &&
  List.for_all
    (fun c -> state_sat_cond c s)
    cond

let satisfies cond domain =
  D.exists (fun p -> state_sat cond p) domain
