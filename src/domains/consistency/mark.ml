open Batteries
open Util

module P = Program

(* A buffer is abstracted by keeping only the information of the variables present in it *)
module Buf = Symbol.Set

type t = Buf.t list

let compare = List.compare Buf.compare

let tid_is_consistent bufs tid =
  Buf.is_empty @@ List.nth bufs tid

let write bufs tid var =
  List.modify_at tid (Buf.add var) bufs

let init prog =
  List.init (List.length prog.P.threads) (fun _ -> Buf.empty)

let make_update bufs { Domain.var; origin; destinations } =
  (* TODO: assert that var is absent in every buffer of the
     destinations and is present in origin *)
  List.modify_at origin (Buf.remove var) bufs

let print output =
  List.print (Set.print ~first:"{" ~last:"}" ~sep:"; " Symbol.print) output
