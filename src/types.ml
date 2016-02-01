open Batteries

type _ t =
  | Int : int t
  | Bool : bool t

let equal : type a b. a t -> b t -> bool =
  fun t1 t2 -> match t1, t2 with
    | Int, Int -> true
    | Bool, Bool -> true
    | _, _ -> false

let print : type a. 'b IO.output -> a t -> unit =
  fun output ty ->
    match ty with
    | Int -> String.print output "Int"
    | Bool -> String.print output "Bool"

type origin =
  | Local
  | Shared
