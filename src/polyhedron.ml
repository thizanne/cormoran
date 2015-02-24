open Apron
open Batteries

type t = Polka.loose Polka.t Abstract1.t

let man = Polka.manager_alloc_loose ()

let is_bottom = Abstract1.is_bottom man

let equal = Abstract1.is_eq man

let init program =
  let open LazyList in
  let open Syntax.TypedProgram in
  let threads = init (Array.length program.threads) (fun x -> x) in
  let shared = (* Enumeration of (x, value of x) initial shared vars *)
    of_list program.initial
    |> map
      (fun (x, v) ->
         map
           (fun t -> Expression.apron_shared x t, v)
           threads)
    |> concat in
  let local = (* LazyListeration of all thread-local vars *)
    of_array program.threads
    |> map (fun th -> of_list th.locals)
    |> concat
    |> map Expression.apron_local in
  let env = Environment.make
      (to_array @@ append (map fst shared) local)
      [||] (* No real variables *) in
  fold_left
    (fun acc (x_t, n) ->
       Abstract1.assign_texpr man acc x_t
         (Expression.to_texpr env (Expression.int n))
         None)
    (Abstract1.top man env)
    shared

let join = Abstract1.join man

let meet = Abstract1.meet man

let join_array = Abstract1.join_array man

let earray_1 tcons =
  let earray = Tcons1.array_make (Tcons1.get_env tcons) 1 in
  Tcons1.array_set earray 0 tcons;
  earray

let earray env array =
  let earray =
    Tcons1.array_make env (Array.length array) in
  for i = 0 to Array.length array - 1 do
    Tcons1.array_set earray i (Constraint.to_tcons env array.(i))
  done;
  earray

let meet_cons abstr cons =
  Abstract1.meet_tcons_array man abstr
    (earray_1 (Constraint.to_tcons (Abstract1.env abstr) cons))

let meet_cons_array abstr array =
  Abstract1.meet_tcons_array man abstr (earray (Abstract1.env abstr) array)

let assign_expr abstr ?(dest=None) var expr =
  Abstract1.assign_texpr man abstr
    (Expression.to_apron_var var)
    (Expression.to_texpr (Abstract1.env abstr) expr)
    dest

let assign_expr_array abstr ?(dest=None) vars exprs =
  Abstract1.assign_texpr_array man abstr
    (Array.map Expression.to_apron_var vars)
    (Array.map (Expression.to_texpr (Abstract1.env abstr)) exprs)
    dest

let print output abstr =
  let fmt = Format.formatter_of_output output in
  Abstract1.print fmt abstr;
  Format.pp_print_flush fmt ()
