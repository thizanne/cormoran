open Batteries

type _ t =
  | Int : int t
  | Bool : bool t
  | Arrow : 'a t * 'b t -> ('a -> 'b) t

let rec equal : type a b. a t -> b t -> bool =
  fun t1 t2 -> match t1, t2 with
    | Int, Int -> true
    | Bool, Bool -> true
    | Arrow (a, b), Arrow (c, d) -> equal a c && equal b d
    | _, _ -> false

let rec to_string : type a. a t -> string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Arrow (a, b) -> to_string a ^ " -> " ^ to_string b

type origin =
  | Local
  | Shared
