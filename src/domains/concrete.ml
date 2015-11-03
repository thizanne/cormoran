open Batteries
open Util
open Location

module O = Operators
module Op = Cfg.Operation
module P = Program

type state = {
  regs : (Sym.t * int option) list list;
  mem : (Sym.t * int option) list;
  buf : (Sym.t * int option) list list;
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

let get_var tid state { P.var_name; var_type } =
  match var_type with
  | P.Local -> get_local tid state var_name
  | P.Shared -> get_shared tid state var_name

let get_var_view state { P.thread_id; var } =
  get_var thread_id state var

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

let set_var tid state x n = match x.P.var_type with
  | P.Local -> set_local tid state x.P.var_name n
  | P.Shared -> set_shared tid state x.P.var_name n

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

let rec get_expr get_var =
  function
  | P.Int { Location.item = n; _ } -> Some n
  | P.Var { Location.item = v; _ } -> get_var v
  | P.ArithUnop (op, expr) ->
    Option.map
      (O.arith_one_fun op.Location.item)
      (get_expr get_var expr.Location.item)
  | P.ArithBinop (op, expr1, expr2) ->
    begin
      try
        option_map2
          (O.arith_two_fun op.Location.item)
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
    O.logic_one_fun op.item
      (validates_cond p c.item)
  | LogicBinop (op, c1, c2) ->
    O.logic_two_fun op.item
      (validates_cond p c1.item)
      (validates_cond p c2.item)
  | ArithRel (rel, e1, e2) ->
    begin match
        get_expr (get_var_view p) e1.item,
        get_expr (get_var_view p) e2.item
      with
      | None, _ -> true
      | _, None -> true
      | Some n1, Some n2 -> O.logic_arith_two_fun rel.item n1 n2
    end

let transfer op domain = match op with
  | Op.Identity -> domain
  | Op.MFence tid -> D.filter (fun p -> is_empty_buffer p tid) domain
  | Op.Filter c -> D.filter (fun p -> validates_cond p c) domain
  | Op.Assign (tid, x, expr) ->
    let flush = match x.P.var_type, Program.shared_in_expr expr with
      | P.Local, [] -> List.singleton
      | P.Shared, [] -> fun p -> flush_after_mop p x.P.var_name
      | P.Local, [y] -> fun p -> flush_after_mop p y
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
  Sym.Map.map Option.some program.P.initial
  |> Sym.Map.enum
  |> List.of_enum

let initial_state program = {
  regs =
    List.map
      (fun { P.locals; _ } ->
         Sym.Set.fold
           (fun x acc -> (x, None) :: acc)
           locals [])
      program.P.threads;
  mem = initial_vars program;
  buf = List.map (fun _ -> []) program.P.threads
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
