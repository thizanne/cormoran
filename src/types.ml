open Batteries

type _ t =
  | Int : int t
  | Bool : bool t
  | Arrow : 'a t * 'b t -> ('a -> 'b) t

let ( @-> ) a b =
  Arrow (a, b)

let rec equal : type a b. a t -> b t -> bool =
  fun t1 t2 -> match t1, t2 with
    | Int, Int -> true
    | Bool, Bool -> true
    | Arrow (a, b), Arrow (c, d) -> equal a c && equal b d
    | _, _ -> false

let rec print : type a. 'b IO.output -> a t -> unit =
  fun output ty ->
    match ty with
    | Int -> String.print output "Int"
    | Bool -> String.print output "Bool"
    | Arrow (a, b) ->
      Printf.fprintf output "%a -> %a" print a print b

type origin =
  | Local
  | Shared
