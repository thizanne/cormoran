open Batteries
open Util
open Printf

(* Caution: despite its name, this module actually implements an
   abstraction of the concrete inner domain, since it keeps no
   information about relations that can exist between variables whose
   values are None (that is every possible value).
*)

(* TODO: fix it. Meeting [x = None] with constraint [x = 0] should
   return [x = 0]. Probably implement it by getting rid of get_expr
   and have some function factorising assign_expr and meet_cons in
   Point (at least for some trivial constraints like x = N, which is
   necessary for initial property instead of initial values --
   probably too much work for nothing to try to deal with constraints
   like [x = y]. *)

module Dom = Domain
module L = Location
module T = TypedAst
module Ty = Types

let ( // ) x_opt y_opt = match x_opt, y_opt with
  | None, _
  | _, None
  | _, Some 0 -> None
  | Some x, Some y -> Some (x / y)

let ( ||| ) a_opt b_opt = match a_opt, b_opt with
  | None, None
  | None, Some false
  | Some false, None -> None
  | Some true, None
  | None, Some true -> Some true
  | Some a, Some b -> Some (a || b)

let ( &&& ) a_opt b_opt = match a_opt, b_opt with
  | None, None
  | None, Some true
  | Some true, None -> None
  | Some false, None
  | None, Some false -> Some false
  | Some a, Some b -> Some (a && b)

let unop_fun : type a b. (a -> b) T.unop -> a option -> b option =
  function
  | T.Neg -> Option.map ( ~- )
  | T.Not -> Option.map ( not )

let binop_fun :
  (* TODO better organization of this *)
  type a b c. (a -> b -> c) T.binop -> a option -> b option -> c option =
  let open Int in
  function
  | T.Add -> Util.option_map2 ( + )
  | T.Sub -> Util.option_map2 ( - )
  | T.Mul -> Util.option_map2 ( * )
  | T.Div -> ( // )
  | T.Eq -> Util.option_map2 ( = )
  | T.Neq -> Util.option_map2 ( <> )
  | T.Lt -> Util.option_map2 ( < )
  | T.Gt -> Util.option_map2 ( > )
  | T.Le -> Util.option_map2 ( <= )
  | T.Ge  -> Util.option_map2 ( >= )
  | T.And -> ( &&& )
  | T.Or -> ( ||| )

module Point = struct
  (* A point is a mapping from variables to their optional value *)
  module M = Sym.Map
  type t = int option M.t * bool option M.t

  let compare = Tuple2.comp
      (M.compare @@ Option.compare ~cmp:Int.compare)
      (M.compare @@ Option.compare ~cmp:Bool.compare)

  let print_env print_value =
    M.print
      ~first:"" ~last:"" ~sep:", "
      Sym.print
      (Util.print_option print_value)

  let print output =
    Tuple2.print
      ~first:"" ~last:"" ~sep:", "
      (print_env Int.print)
      (print_env Bool.print)
      output

  let get_var :
    type a. t -> a Dom.inner_var -> a option =
    fun (ints, bools) { T.var_spec; var_type; _ } ->
      match var_type with
        | Ty.Int -> M.find var_spec ints
        | Ty.Bool -> M.find var_spec bools

  let rec get_expr :
    type a. t -> (a, _) T.expression -> a option =
    fun p expr -> match expr with
      | T.Int n -> Some (n.L.item)
      | T.Bool b -> Some (b.L.item)
      | T.Var v -> get_var p v.L.item
      | T.Unop (op, e) ->
        unop_fun op.L.item @@ get_expr_loc p e
      | T.Binop (op, e1, e2) ->
        binop_fun op.L.item
          (get_expr_loc p e1)
          (get_expr_loc p e2)

  and get_expr_loc :
    type a. t -> (a, _) T.expression L.loc -> a option =
    fun p e ->
      get_expr p e.L.item

  let can_sat_cons p cons =
    Option.default true (get_expr p cons)

  let env_assign_value var value env =
    try
      M.modify var (fun _ -> value) env
    with Not_found ->
      failwith @@
      sprintf "Point.assign_value: unknown var %s" (Sym.name var)

  let assign_value :
    type a. a Dom.inner_var -> a option -> t -> t =
    fun { T.var_type; var_spec; _ } value (ints, bools) ->
      match var_type with
      | Ty.Int -> env_assign_value var_spec value ints, bools
      | Ty.Bool -> ints, env_assign_value var_spec value bools

  let env_add_assign var value point =
    try
      ignore (M.find var point);
      failwith @@ sprintf
        "InnerConcrete expand: name collision on %s"
        (Sym.name var)
    with Not_found ->
      M.add var value point

  let add_assign :
    type a. a Dom.inner_var -> a option -> t -> t =
    fun { T.var_type; var_spec; _ } value (ints, bools) ->
      match var_type with
      | Ty.Int -> env_add_assign var_spec value ints, bools
      | Ty.Bool -> ints, env_add_assign var_spec value bools

  let assign_expr :
    type a. a Dom.inner_var -> a Dom.inner_expression -> t -> t =
    fun { T.var_spec; var_type; _ } expr (ints, bools) ->
      let result = get_expr (ints, bools) expr in
      let update = M.modify var_spec (fun _ -> result) in
      try
        match var_type with
        | Ty.Int -> update ints, bools
        | Ty.Bool -> ints, update bools
      with Not_found ->
        failwith @@ sprintf
          "InnerConcrete assign_expr: unknown var %s"
          (Sym.name var_spec)

  let init =
    M.empty, M.empty

  let drop :
    type a. a Dom.inner_var -> t -> t =
    fun { T.var_type; var_spec; _ } (ints, bools) ->
      match var_type with
      | Ty.Int -> M.remove var_spec ints, bools
      | Ty.Bool -> ints, M.remove var_spec bools

  let add x p =
    add_assign x None p

  let env_fold x y env =
    (* Returns two new environments where y is removed and x has
       respectively the original value of x and of y *)
    let v, p1 = M.extract y env in
    let p2 = M.add x v p1 in
    p1, p2

  let fold :
    type a. a Dom.inner_var -> a Dom.inner_var -> t -> t * t =
    fun { T.var_type; var_spec = x; _ } { T.var_spec = y; _ }  (ints, bools) ->
      (* Returns two points where y is removed and x has respectively
         the original value of x and of y *)
      match var_type with
      | Ty.Int ->
        Tuple2.mapn (fun ints -> ints, bools) (env_fold x y ints)
      | Ty.Bool ->
        Tuple2.mapn (fun bools -> ints, bools) (env_fold x y bools)

  let env_coincide_except_on x env1 env2 =
    M.for_all
      (fun y v ->
         Sym.Ord.compare x y = 0 ||
         M.find y env1 = v)
      env2

  let coincide_except_on :
    type a. a Dom.inner_var -> t -> t -> bool =
    fun { T.var_type; var_spec = x; _ } (ints1, bools1) (ints2, bools2) ->
      match var_type with
      | Ty.Int ->
        env_coincide_except_on x ints1 ints2 &&
        M.equal (Option.eq ~eq:Bool.equal) bools1 bools2
      | Ty.Bool ->
        env_coincide_except_on x bools1 bools2 &&
        M.equal (Option.eq ~eq:Int.equal) ints1 ints2
end

(* An "abstract" element is a set of point *)
module D = Set.Make (Point)

type t = D.t

let is_bottom = D.is_empty

let equal = D.equal

let init =
  D.singleton (Point.init)

let join = D.union

let meet = D.inter

let widening _abstr1 abstr2 =
  abstr2

let meet_cons cons d =
  D.filter (fun p -> Point.can_sat_cons p cons) d

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
           Point.(assign_value x v1 @@ add_assign y v2 @@ p))
        couples in
    List.fold_right D.add expanded_points @@ expand x y d2

let drop x d =
  D.map (Point.drop x) d

let add x d =
  D.map (Point.add x) d

let print output =
  D.print
    ~first:"" ~last:"" ~sep:";\n"
    Point.print
    output
