open Util
open Printf
open Syntax
open Syntax.Typed
open Syntax.TypedProgram

type state = {
  regs : (string * int option) list;
  mem : (string * int option) list;
  buf : (string * int option) list list;
}

module S = Set.Make (struct type t = state let compare = compare end)

type t = S.t

let empty = S.empty

let union = S.union

let smap f s = S.fold (fun x e -> S.add (f x) e) s S.empty

let set_reg state r n = {
  state with regs = set_assoc r n state.regs
}

let get_value s v = match v with
  | Int n -> Some n.item
  | Var r -> List.assoc r.item s.regs

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

let rec get_expr state = function
  | Val v -> get_value state v.item
  | Op (op, e1, e2) ->
    let e1 = get_expr state e1.item in
    let e2 = get_expr state e2.item in
    begin match (e1, e2) with
      | Some v1, Some v2 -> (fun_of_op op.item) v1 v2
      | _, _ -> None
    end

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
  |> List.filter (fun t -> is_older_in_buf p t x)

let flush s t =
  let (x, v) = last @@ nth_buf s t in
  {
    s with
    mem = set_assoc x v s.mem;
    buf =
      set_nth t (first @@ List.nth s.buf t) s.buf;
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
  |> List.map (fun (i, buf) -> repeat (List.length buf) i)
  |> all_combi
  |> List.map (List.fold_left flush p)

let transfer domain t = function
  | Pass -> domain
  | Read (r, x) ->
    let domain =
      domain
      |> S.elements
      |> List.map (fun p -> set_reg p r.item (get_var p t x.item))
      |> List.map (fun p -> flush_after_mop p x.item)
      |> List.flatten
    in
    List.fold_right S.add domain S.empty
  | Write (x, v) ->
    let domain =
      domain
      |> S.elements
      |> List.map (fun p -> set_var p t x.item (get_value p v.item))
      |> List.map (fun p -> flush_after_mop p x.item)
      |> List.flatten
    in
    List.fold_right S.add domain S.empty
  | RegOp (r, e)  ->
    smap
      (fun p -> set_reg p r.item (get_expr p e.item)) domain
  | Cmp (r, v1, v2) ->
    smap
      (fun p -> set_reg p r.item (
           let v1 = get_value p v1.item in
           let v2 = get_value p v2.item in
           match (v1, v2) with
           | Some n1, Some n2 ->
             Some (
               if n1 < n2 then -1
               else if n1 > n2 then 1
               else 0)
           | _, _ -> None))
      domain
  | MFence -> S.filter (fun p -> is_empty_buffer p t) domain
  | Label _ -> domain
  | Jnz (_, _) | Jz (_, _) | Jmp _ -> failwith "Jumps not implemented"

let initial_vars program =
  List.map (fun (x, v) -> x, Some v) program.initial

let initial_state program = {
  regs =
    Array.fold_left ( @ ) []
      (Array.map
         (fun t -> List.map (fun r -> r, None) t.locals)
         program.threads);
  mem = initial_vars program;
  buf =
    repeat (Array.length program.threads) [];
}

let init program = S.singleton (initial_state program)

let str_var (x, v) =
  sprintf "%s → %s" x (str_int_option v)

let str_buf buf =
  "[" ^ string_of_list str_var buf ^ "]"

let str_point {regs; mem; buf} =
  string_of_list str_var regs ^ "\n" ^
  string_of_list ~sep:"\n" str_buf buf ^ "\n" ^
  string_of_list str_var mem ^ "\n"

let to_string d =
  S.fold
    (fun p acc ->
       str_point p ^ "\n\n" ^ acc)
    d ""

let is_totally_flushed p =
  List.for_all (( = ) []) p.buf

let state_sat_cond (var, value) s =
  try
    List.assoc var s.mem = Some value
  with Not_found ->
    List.assoc var s.regs = Some value

let state_sat cond s =
  is_totally_flushed s &&
  List.for_all
    (fun c -> state_sat_cond c s)
    cond

let satisfies cond domain =
  S.exists (fun p -> state_sat cond p) domain
