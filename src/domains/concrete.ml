open Batteries
open Util
open Printf
open Location
open Program

module Op = Cfg.Operation

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

let set_var state t x v = {
  state with
  buf =
    set_nth t ((x, v) :: List.nth state.buf t) state.buf
}

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
  let (x, v) = last @@ nth_buf s t in
  {
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
  let open Program in
  function
  | Int { Location.item = n; _ } -> Some n
  | Var { Location.item = v; _ } ->
    if is_local v
    then get_reg p thread v.var_name
    else get_var p thread v.var_name
  | ArithUnop (op, expr) ->
    Option.map
      (fun_of_arith_unop op.Location.item)
      (get_expr p thread expr.Location.item)
  | ArithBinop (op, expr1, expr2) ->
    begin
      try
        option_map2
          (fun_of_arith_binop op.Location.item)
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

let transfer domain {Op.thread = t; op} = match op with
  | Op.Identity -> domain
  | Op.MFence -> D.filter (fun p -> is_empty_buffer p t) domain
  | Op.Filter c -> D.filter (fun p -> validates_cond p t c) domain
  | Op.Assign (x, expr) ->
    let flush = match Program.shared_in_expr expr with
      | [] -> List.singleton
      | [x] -> fun p -> flush_after_mop p x
      | _ -> Error.not_implemented_msg_error "Several shared in expr"
    in
    let domain =
      domain
      |> D.elements
      |> List.map (fun p -> set_var p t x.var_name (get_expr p t expr))
      |> List.map flush
      |> List.flatten
    in
    List.fold_right D.add domain D.empty

let initial_vars program =
  Symbol.Map.map Option.some program.initial
  |> Symbol.Map.enum
  |> List.of_enum


let initial_state program = {
  regs =
    List.map
      (fun { locals; _ } ->
         Symbol.Set.fold
           (fun x acc -> (x, None) :: acc)
           locals [])
      program.threads;
  mem = initial_vars program;
  buf = List.map (fun _ -> []) program.threads
}

let init program = D.singleton (initial_state program)

let str_var (x, v) =
  sprintf "%s → %s" (Symbol.name x) (str_int_option v)

let rec string_of_list ?(sep="; ") string_of_elem = function
  | [] -> ""
  | [x] -> string_of_elem x
  | x :: xs ->
    string_of_elem x ^ sep ^ string_of_list ~sep string_of_elem xs

let str_buf buf =
  "[" ^ string_of_list str_var buf ^ "]"

let str_point {regs; mem; buf} =
  string_of_list ~sep:"\n" str_buf regs ^ "\n" ^
  string_of_list ~sep:"\n" str_buf buf ^ "\n" ^
  string_of_list str_var mem ^ "\n"

let to_string d =
  D.fold
    (fun p acc ->
       str_point p ^ "\n\n" ^ acc)
    d ""

let print output d =
  IO.nwrite output (to_string d)

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
