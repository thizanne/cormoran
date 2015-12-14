(* TODO: write mli *)
open Batteries

module L = Location
module Ty = Types

type constant =
  | ConstInt of int
  | ConstBool of bool

type ty =
  | Int
  | Bool

let print_ty output = function
  | Int -> String.print output "Int"
  | Bool -> String.print output "Bool"

type origin = Ty.origin (* To change if origin is made a gadt*)

type t = (ty * origin) Sym.Map.t

let are_compatible_types : type a. ty -> a Ty.t -> bool =
  fun t1 t2 ->
    match t1, t2 with
    | Int, Ty.Int -> true
    | Bool, Ty.Bool -> true
    | _, _ -> false

let get_entry loc var_sym env =
  match Sym.Map.Exceptionless.find var_sym env with
  | None ->
    Error.name_loc_error loc @@
    Printf.sprintf "Var %s is not defined"
      (Sym.name var_sym)
  | Some entry -> entry

let get_loc_entry { L.item = var_sym; loc } env =
  get_entry loc var_sym env

let get_loc_type var_loc env =
  fst @@ get_loc_entry var_loc env

let get_loc_entry_option var_loc env =
  Sym.Map.Exceptionless.find var_loc.L.item env

let add var ty origin env =
  Sym.Map.add var (ty, origin) env
