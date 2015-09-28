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

module M = Map.Make (Bufs)

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
      Bufs.print Inner.print output

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  let init prog =
    M.singleton (Bufs.init prog) (Inner.init prog)

  let add_join bufs abstr d =
    (* Adds (bufs, abstr) as a d element, making a join if the bufs
       key is already present *)
    M.modify_def abstr bufs (Inner.join abstr) d

  let flush tid bufs abstr =
    (* Returns the (bufs, abstr) element corresponding to the flush of
       the older variable in the buffer of the thread tid *)
    let sym_x, flushed_bufs = Bufs.flush bufs tid in
    let var_x = P.shared_var sym_x in
    let x_t_exp = P.Var (L.mkdummy var_x) in
    (* Assign the value of x_t to every x_i in the numerical domain
       abstr where x_i is not present in buffer i *)
    let abstr =
      List.fold_left
        (fun acc tid' -> Inner.assign_expr acc tid' var_x tid x_t_exp)
        abstr
        (Bufs.with_no_var bufs var_x.P.var_name) in
    flushed_bufs, abstr

  let rec join_flush_list tid_list bufs abstr d = match tid_list with
    (* Takes a list of tids, an element of a domain, and adds the
       successive results of flushing the first tid of the list to the
       domain *)
    | [] -> d
    | tid :: tids ->
      let (bufs, abstr) = flush tid bufs abstr in
      let d = add_join bufs abstr d in
      join_flush_list tids bufs abstr d

  let flush_mop x bufs abstr d =
    (* Updates the abstract domain d to take into account every possible
       combination of flush from the numerical domain abstr after a memory
       operation on the variable x by the thread tid *)
    let to_flush = Bufs.flush_lists_after_mop bufs x in
    List.fold_left
      (fun acc tid_list -> join_flush_list tid_list bufs abstr acc)
      d to_flush

  let close_after_mop x d =
    (* Compute the flush closure of the domain d after a memory
       operation on the shared variable x *)
    M.fold (flush_mop x) d d

  let transfer op d =
    match op with
    | O.Identity -> d
    | O.MFence tid ->
      M.filter (fun bufs _ -> Bufs.nth_is_empty bufs tid) d
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
