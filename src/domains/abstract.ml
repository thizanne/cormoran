open Batteries
open Util

module P = Program
module O = Cfg.Operation
module L = Location

(* A buffer is abstracted by a Deque giving the order of the more
   recent entries of each variable in this buffer.
*)

module Bufs : Domain.BufferAbstraction = Buffers.UnsoundOrdered

(* An abstract domain is the map from buffer abstractions to
   a numerical domain *)

module Coh : Domain.ConsistencyAbstraction =
  (val Obj.magic 0 : Domain.ConsistencyAbstraction)

module M = Map.Make (Coh)

module Make (Inner : Domain.Inner) = struct
  type t = Inner.t M.t

  let bottom = M.empty

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

  let is_bottom d =
    M.is_empty (normalize d)

  let print output =
    M.print
      ~first:"" ~last:"" ~kvsep:":\n" ~sep:"\n────────\n"
      Coh.print Inner.print output

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  let init prog =
    M.singleton (Coh.init prog) (Inner.init prog)

  let add_join bufs abstr d =
    (* Adds (bufs, abstr) as a d element, making a join if the bufs
       key is already present *)
    M.modify_def abstr bufs (Inner.join abstr) d

  let make_update (coh, abstr) update =
    (* Returns the (coh, abstr) corresponding to the update *)
    let shared_var = P.shared_var update.Coh.var in
    let var_exp = P.Var (L.mkdummy shared_var) in
    let abstr =
      List.fold_left
        (fun acc dest ->
           Inner.assign_expr acc dest shared_var update.Coh.origin var_exp)
        abstr
        update.Coh.destinations
    in
    Coh.update coh update, abstr

  (*
let rec join_flush_list tid_list bufs abstr d = match tid_list with
    (* Takes a list of tids, an element of a domain, and adds the
       successive results of flushing the first tid of the list to the
       domain *)
    | [] -> d
    | tid :: tids ->
      let (bufs, abstr) = flush tid bufs abstr in
      let d = add_join bufs abstr d in
      join_flush_list tids bufs abstr d

*)

  let make_several_updates updates coh abstr d =
    (* Takes a list of updates, an element of the domain, and adds the
       result of the application of these updates to the domain *)
    let coh', abstr' = List.fold_left make_update (coh, abstr) updates
    in add_join coh' abstr' d

  let flush_mop tid x coh abstr d =
    (* Updates the abstract domain d to take into account every possible
       combination of flush from the numerical domain abstr after a memory
       operation on the variable x by the thread tid *)
    let updates = Coh.get_mop_updates tid coh x in
    List.fold_left
      (fun acc tid_list -> join_flush_list tid_list coh abstr acc)
      d updates

  let close_after_mop x d =
    (* Compute the flush closure of the domain d after a memory
       operation on the shared variable x *)
    M.fold (flush_mop x) d d

  let transfer op d =
    match op with
    | O.Identity -> d
    | O.MFence tid ->
      M.filter (fun coh _ -> Coh.tid_is_consistent coh tid) d
    | O.Assign (tid, x, expr) ->
      let d = (* Make the assignation on inner abstract variables *)
        M.map
          (fun abstr -> Inner.assign_expr abstr tid x tid expr)
          d in
      let d = (* Add x to the tid-th buffer if it's a write *)
        match x.P.var_type with
        | P.Local -> d
        | P.Shared ->
          M.Labels.fold
            ~f:(fun ~key:bufs ~data:abstr acc ->
                add_join
                  (Bufs.write bufs tid x.P.var_name)
                  abstr
                  acc)
            ~init:M.empty
            d in
      let d = (* Close by partial flush if needed *)
        begin match x.P.var_type, P.shared_in_expr expr with
          | P.Local, [] -> d
          | P.Shared, [] -> close_after_mop x.P.var_name d
          | P.Local, [y] -> close_after_mop y d
          | _ -> failwith "Abstract.transfer"
        end
      in d
    | O.Filter cond ->
      M.map
        (fun abstr -> Inner.meet_cons abstr cond)
        d
      |> normalize

  let join =
    M.merge
      (fun _bufs abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> abstr1
         | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2))

  let widening =
    M.merge
      (fun _bufs abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> failwith "Abstract.widening"
         | Some abstr1, Some abstr2 ->
           Some (Inner.widening abstr1 abstr2))
end
