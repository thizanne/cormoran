open Batteries
open Util
open Printf

module L = Location
module P = Program
module O = Operators

module Point = struct
  (* A point is a mapping from variables to their optional value *)
  module M = Symbol.Map
  type t = int option M.t

  let compare = M.compare @@ Option.compare ~cmp:Int.compare

  let print = M.print

  let get_var p x =
    M.find x p

  let rec get_expr p = function
    | P.Int n -> Some n.L.item
    | P.Var v -> M.find v.L.item p
    | P.ArithUnop (op, e) ->
      Option.map
        (O.arith_one_fun op.L.item)
        (get_expr_loc p e)
    | P.ArithBinop (op, e1, e2) ->
      option_map2
        (O.arith_two_fun op.L.item)
        (get_expr_loc p e1)
        (get_expr_loc p e2)

  and get_expr_loc p e = get_expr p e.L.item

  let rec sat_cons p = function
    | P.Bool b -> b.L.item
    | P.LogicUnop (op, c) ->
      O.logic_one_fun
        op.L.item
        (sat_cons_loc p c)
    | P.LogicBinop (op, c1, c2) ->
      O.logic_two_fun
        op.L.item
        (sat_cons_loc p c1)
        (sat_cons_loc p c2)
    | P.ArithRel (rel, e1, e2) ->
      option_map2
        (O.logic_arith_two_fun rel.L.item)
        (get_expr_loc p e1)
        (get_expr_loc p e2)
      |> Option.default true

  and sat_cons_loc p cons = sat_cons p cons.L.item

  let assign_value var value point =
    try
      M.modify var (fun _ -> value) point
    with Not_found ->
      failwith @@
      sprintf "Point.assign_value: unknown var %s" (Symbol.name var)

  let new_var var value point =
    try
      ignore (M.find var point);
      failwith @@ sprintf
        "InnerConcrete expand: name collision on %s"
        (Symbol.name var)
    with Not_found ->
      M.add var value point

  let assign_expr var expr point =
    try
      M.modify var (fun _ -> get_expr point expr) point
    with Not_found ->
      failwith @@ sprintf
        "InnerConcrete assign_expr: unknown var %s"
        (Symbol.name var)

  let init symbols =
    List.fold_left
      (fun acc (x, v) -> M.add x v acc)
      M.empty
      symbols

  let drop x p =
    M.remove x p

  let add x p =
    try
      ignore (M.find x p);
      failwith @@ sprintf
        "InnerConcrete add: name collision on %s"
        (Symbol.name x)
    with Not_found ->
      M.add x None p

  let fold x y p =
    let v, p1 = M.extract y p in
    let p2 = M.add x v p1 in
    p1, p2

  let coincide_except_on x p1 p2 =
    M.for_all
      (fun y v ->
         Symbol.Ord.compare x y = 0 ||
         M.find y p1 = v)
      p2
end

(* An "abstract" element is a set of point *)
module D = Set.Make (Point)

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

let meet_cons cons d =
  D.filter (fun p -> Point.sat_cons p cons) d

let assign_expr var expr d =
  D.map
    (fun p -> Point.assign_expr var expr p)
    d

let fold x y d =
  D.Labels.fold
    ~f:(fun p acc ->
        let p1, p2 = Point.fold x y p
        in acc |> D.add p1 |> D.add p2)
    ~init:D.empty
    d

let rec expand x y d =
  if D.is_empty d then D.empty
  else
    let p = D.choose d in
    let d1, d2 = D.partition (Point.coincide_except_on x p) d in
    let x_values = D.fold (fun p acc -> Point.get_var p x :: acc) d1 [] in
    let couples = List.cartesian_product x_values x_values in
    let expanded_points =
      List.map
        (fun (v1, v2) ->
           Point.(assign_value x v1 @@ new_var y v2 @@ p))
        couples in
    List.fold_right D.add expanded_points @@ expand x y d2

let drop x d =
  D.map (Point.drop x) d

let add x d =
  D.map (Point.add x) d

let print output =
  D.print
    ~first:"" ~last:"" ~sep:";\n"
    (Point.print
       ~first:"" ~sep:", " ~last:"" ~kvsep:" = "
       Symbol.print
       print_int_option)
    output
