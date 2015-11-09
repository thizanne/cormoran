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

type origin =
  | Local
  | Shared
