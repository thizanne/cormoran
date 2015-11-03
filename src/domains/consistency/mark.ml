open Batteries
open Util

module P = Program

(* A buffer is abstracted by keeping only the information of the variables present in it *)
module Buf = Sym.Set

type t = Buf.t list

let compare = List.compare Buf.compare

let tid_is_consistent bufs tid =
  Buf.is_empty @@ List.nth bufs tid

let write bufs tid var =
  List.modify_at tid (Buf.add var) bufs

let flush bufs tid var =
  List.modify_at tid (Buf.remove var) bufs

let init prog =
  List.init (List.length prog.P.threads) (fun _ -> Buf.empty)

let make_update bufs update =
  (* TODO: assert that var is absent in every buffer of the
     destinations and is present in origin *)
  List.modify_at update.Domain.origin (Buf.remove update.Domain.var) bufs

let rec create_updates bufs var = function
  | [] -> []
  | origin :: origins ->
    let destinations =
      List.filteri_map
        (fun i buf -> if Buf.mem var buf then None else Some i)
        bufs in
    let update = { Domain.var; origin; destinations } in
    let bufs = flush bufs origin var in
    update :: create_updates bufs var origins

let get_mop_updates bufs _issuer var =
  List.filteri_map
    (fun i buf -> if Buf.mem var buf then Some i else None)
    bufs
  |> ordered_parts
  |> List.sort_uniq (List.compare Int.compare)
  |> List.map (create_updates bufs var)

let print output =
  List.print (Buf.print ~first:"{" ~last:"}" ~sep:"; " Sym.print) output
