open Batteries
open Util

module L = Location
module P = Program

module Point = struct
  include Map.Make (struct
      type t = P.var_view
      let compare = Pervasives.compare
    end)

  let rec get_expr view p { L.item = expr; _ } = match expr with
    | P.Int n -> Some n.L.item
    | P.Var v -> find (view v.L.item) p
    | P.ArithUnop (op, e) ->
      Option.map
        (P.fun_of_arith_unop op.L.item)
        (get_expr view p e)
    | P.ArithBinop (op, e1, e2) ->
      option_map2
        (P.fun_of_arith_binop op.L.item)
        (get_expr view p e1)
        (get_expr view p e2)

  let rec sat_cons p { L.item = cons; _ } = match cons with
    | P.Bool b -> b.L.item
    | P.LogicUnop (op, c) ->
      P.fun_of_logic_unop
        op.L.item
        (sat_cons p c)
    | P.LogicBinop (op, c1, c2) ->
      P.fun_of_logic_binop
        op.L.item
        (sat_cons p c1)
        (sat_cons p c2)
    | P.ArithRel (rel, e1, e2) ->
      option_map2
        (P.fun_of_arith_rel rel.L.item)
        (get_expr (fun v -> v) p e1)
        (get_expr (fun v -> v) p e2)
      |> Option.default true

  let assign_expr p var_tid var exp_tid exp =
    add
      (P.create_var_view ~thread_id:var_tid var)
      (get_expr (P.create_var_view ~thread_id:exp_tid) p exp)
      p

  let init program =
    let module LL = LazyList in
    let add_shared_x x n acc =
      (* appends (0:x, Some n); ...; (N:x, Some n) to acc *)
      List.fold_lefti
        (fun acc thread_id thread ->
           LL.cons
             (P.create_var_view ~thread_id (P.shared_var x), Some n)
             acc)
        acc
        program.P.threads in
    let shared =
      (* LazyList of (x_t, value of x) initial shared vars *)
      Symbol.Map.fold
        add_shared_x
        program.P.initial
        LL.nil in
    let add_locals_thread acc thread_id thread =
      (* appends locals of the thread to acc with None value *)
      Symbol.Set.fold
        (fun x acc ->
           LL.cons
             (P.create_var_view ~thread_id (P.local_var x), None)
             acc)
        thread.Program.locals
        acc in
    let locals =
      (* LazyList of the local vars of program with None value *)
      List.fold_lefti
        add_locals_thread
        LL.nil
        program.P.threads in
    LL.fold_left
      (fun acc (x, v) -> add x v acc)
      empty
      (LL.append shared locals)
end

module D = Set.Make (struct
    type t = int option Point.t
    let compare = Point.compare (Option.compare ~cmp:Int.compare)
  end)

type t = D.t

let is_bottom = D.is_empty

let equal = D.equal

let init program =
  D.singleton (Point.init program)

let join = D.union

let meet = D.inter

let widening _abstr1 abstr2 =
  abstr2

let join_array =
  Array.fold_left join D.empty

let meet_cons d cons =
  D.filter (fun p -> Point.sat_cons p @@ L.mkdummy cons) d

let assign_expr d var_tid var exp_tid exp =
  D.map
    (fun p -> Point.assign_expr p var_tid var exp_tid (L.mkdummy exp))
    d

let print output =
  D.print
    ~first:"" ~last:"" ~sep:";\n"
    (Point.print
       ~first:"" ~sep:", " ~last:"" ~kvsep:" = "
       Program.print_var_view
       print_int_option)
    output
