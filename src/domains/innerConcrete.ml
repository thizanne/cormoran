open Batteries
open Util
open Syntax.TypedProgram

module Point = struct
  include Map.Make (struct
      type t = Expression.var
      let compare = Pervasives.compare
    end)

  let rec get_expr p =
    let open Expression in function
      | EInt n -> Some n
      | EVar v -> find v p
      | EUnop (op, e) ->
        Option.map (fun_of_unop op) (get_expr p e)
      | EBinop (op, e1, e2) ->
        let f = fun_of_binop op in
        begin match get_expr p e1, get_expr p e2 with
          | None, _ -> None
          | _, None -> None
          | Some v1, Some v2 ->
            try Some (f v1 v2)
            with Division_by_zero -> None
        end

  let sat_cons p { Constraint.typ; e1; e2 } =
    match get_expr p e1, get_expr p e2 with
    | None, _
    | _, None -> true
    | Some v1, Some v2 -> Constraint.fun_of_typ typ v1 v2

  let assign_expr p var expr =
    add var (get_expr p expr) p

  let assign_expr_array p vars exprs =
    let vars = Array.enum vars in
    let exprs = Array.enum exprs in
    let exprs = Enum.map (get_expr p) exprs in
    Enum.fold2 add p vars exprs

  let init program =
    let open LazyList in
    let open Syntax.TypedProgram in
    let threads = init (Array.length program.threads) (fun x -> x) in
    let shared = (* Lazy list of (x_t, Some value of x) initial shared vars *)
      of_list program.initial
      |> map
        (fun (x, v) ->
           map
             (fun t -> Expression.shared_var x t, Some v)
             threads)
      |> concat in
    let local = (* LazyList of (r, None) for all r local vars *)
      of_array program.threads
      |> map (fun th -> of_list th.locals)
      |> concat
      |> map Expression.local_var
      |> map (fun r -> r, None) in
    LazyList.fold_left
      (fun acc (x, v) -> add x v acc)
      empty
      (append shared local)
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

let assign_expr d ?(dest=None) var expr =
  let d' = D.map (fun p -> Point.assign_expr p var expr) d in
  Option.map_default (meet d') d' dest

let assign_expr_array d ?(dest=None) vars exprs =
  let d' = D.map (fun p -> Point.assign_expr_array p vars exprs) d in
  Option.map_default (meet d') d' dest

let print output =
  D.print
    ~first:"" ~last:"" ~sep:";\n"
    (Point.print
       ~first:"" ~sep:", " ~last:""
       Expression.print_var print_int_option)
    output
