open Batteries
open Util

module L = Location
module P = Program

module Point = struct
  include Map.Make (struct
      type t = P.var P.threaded
      let compare = Pervasives.compare
    end)

  let rec get_expr p { P.elem = expr; thread_id } = match expr with
    | P.Int n -> Some n.L.item
    | P.Var v -> find (P.create_threaded ~thread_id v.L.item) p
    | P.ArithUnop (op, e) ->
      Option.map
        (P.fun_of_arith_unop op.L.item)
        (get_expr p @@ P.create_threaded ~thread_id e.L.item)
    | P.ArithBinop (op, e1, e2) ->
      option_map2
        (P.fun_of_arith_binop op.L.item)
        (get_expr p @@ P.create_threaded ~thread_id e1.L.item)
        (get_expr p @@ P.create_threaded ~thread_id e2.L.item)

  let sat_cons p { P.elem = cons; thread_id } =
    let rec aux = function
      | P.Bool b -> b.L.item
      | P.LogicUnop (op, c) ->
        P.fun_of_logic_unop op.L.item (aux c.L.item)
      | P.LogicBinop (op, c1, c2) ->
        P.fun_of_logic_binop op.L.item (aux c1.L.item) (aux c2.L.item)
      | P.ArithRel (rel, e1, e2) ->
        option_map2
          (P.fun_of_arith_rel rel.L.item)
          (get_expr p @@ P.create_threaded ~thread_id e1.L.item)
          (get_expr p @@ P.create_threaded ~thread_id e2.L.item)
        |> Option.default true
    in aux cons

  let assign_expr p var expr =
    add var (get_expr p expr) p

  let assign_expr_array p vars exprs =
    let vars = Array.enum vars in
    let exprs = Array.enum exprs in
    let exprs = Enum.map (get_expr p) exprs in
    Enum.fold2 add p vars exprs

  let init program =
    let module LL = LazyList in
    let add_shared_x x n acc =
      (* appends (0:x, Some n); ...; (N:x, Some n) to acc *)
      List.fold_lefti
        (fun acc thread_id thread ->
           LL.cons
             (P.create_threaded ~thread_id (P.shared_var x), Some n)
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
             (P.create_threaded ~thread_id (P.local_var x), None)
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

let join_array =
  Array.fold_left join D.empty

let meet_cons d cons =
  D.filter (fun p -> Point.sat_cons p cons) d

let meet_cons_array =
  Array.fold_left meet_cons

let assign_expr d var expr =
  D.map (fun p -> Point.assign_expr p var expr) d

let assign_expr_array d ?(dest=None) vars exprs =
  let d' = D.map (fun p -> Point.assign_expr_array p vars exprs) d in
  Option.map_default (meet d') d' dest

let print output =
  D.print
    ~first:"" ~last:"" ~sep:";\n"
    (Point.print
       ~first:"" ~sep:", " ~last:""
       (Program.print_threaded Program.print_var)
       print_int_option)
    output
