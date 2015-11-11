open Batteries

type _ t =
  | Int : int t
  | Bool : bool t

let equal : type a b. a t -> b t -> bool =
  fun t1 t2 -> match t1, t2 with
    | Int, Int -> true
    | Bool, Bool -> true
    | Int, Bool -> false
    | Bool, Int -> false

let to_string : type a. a t -> string = function
  | Int -> "Int"
  | Bool -> "Bool"

type origin =
  | Local
  | Shared
