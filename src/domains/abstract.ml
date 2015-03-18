open Batteries
open Util

module P = Program
module O = Cfg.Operation
module L = Location

(* A buffer is abstracted by a Deque giving the order of the more
   recent entries of each variable in this buffer.
*)

module Bufs : sig
  type buf
  val is_empty : buf -> bool
  val last : buf -> Symbol.t (* can raise Not_found *)
  type t
  val compare : t -> t -> int
  val nth : t -> int -> buf
  val write : t -> Program.thread_id -> Symbol.t -> t
  val init : Program.t -> t
  val flush : t -> Program.thread_id -> t
  val all_flush_list : t -> Symbol.t -> Program.thread_id list list
  val get_no_var : t -> Symbol.t -> Program.thread_id list
  val print : 'a BatIO.output -> t -> unit
end
=
struct
  type buf = Symbol.t Deque.t

  let is_empty = Deque.is_empty

  let last buf = match Deque.rear buf with
    | Some (_, x) -> x
    | None -> raise Not_found

  let is_absent buf x =
    Deque.find (( = ) x) buf = None

  let move_to_head x buf =
    Deque.fold_left
      (fun acc y -> if x = y then acc else Deque.snoc acc y)
      (Deque.of_list [x])
      buf

  type t = buf list (* TODO: change list to persistent arrays *)

  let compare = List.compare
      (fun dq1 dq2 ->
         Enum.compare Symbol.Ord.compare
           (Deque.enum dq1) (Deque.enum dq2))

  let nth = List.at

  let write bufs tid var =
    List.modify_at tid (move_to_head var) bufs

  let init prog =
    List.init (List.length prog.P.threads) (fun _ -> Deque.empty)

  let flush bufs tid =
    List.modify_at tid
      (fun buf -> match Deque.rear buf with
         | Some (buf', _) -> buf'
         | None -> failwith "flush")
      bufs

  let all_flush_list bufs x =
    (* TODO can probably be done better *)
    let t_list tid buf =
      (* ~= List.make (Deque.length buf) t, if Deque.length existed *)
      Deque.fold_left (fun acc _ -> tid :: acc) [] buf
    in

    bufs
    |> List.filteri_map
      (fun tid buf ->
         match Deque.rear buf with
         | Some (_, y) when y = x -> Some (t_list tid buf)
         | _ -> None)
    |> List.concat
    |> ordered_parts
    |> List.sort_uniq (List.compare Int.compare)

  let get_no_var bufs x =
    (* Given buffers and a symbol x, returns the list of the thread ids
       whose buffer do not contain x *)
    List.filteri_map
      (fun t buf ->
         if is_absent buf x then Some t
         else None) bufs

  let print output =
    List.print (Deque.print Symbol.print) output

end

(* An abstract domain is the map from buffer abstractions to
   a numerical domain *)

module M = Map.Make (Bufs)

module Make (Inner : Domain.Inner) = struct
  type t = Inner.t M.t

  let bottom = M.empty

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

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

  let flush t bufs abstr d =
    (* Updates the abstract domain d to take into account the flush of
       the oldest entry of the t-th buffer from the numerical domain
       abstr *)
    let var_x = P.shared_var @@ Bufs.last @@ Bufs.nth bufs t in
    let x i = P.create_threaded ~thread_id:i var_x in
    let x_t = P.create_threaded ~thread_id:t (P.var (L.mkdummy var_x)) in
    (* Assign the value of x_t to every x_i in the numerical domain
       abstr where x_i is not present in buffer i *)
    let abstr =
      List.fold_left
        (fun acc i -> Inner.assign_expr acc (x i) x_t)
        abstr
        (Bufs.get_no_var bufs var_x.P.var_name) in
    (* Make the joins *)
    add_join (Bufs.flush bufs t) abstr d

  let flush_mop x bufs abstr d =
    (* Updates the abstract domain d to take into account every possible
       combination of flush from the numerical domain abstr after a memory
       operation on the variable x by the thread tid *)
    let to_flush = Bufs.all_flush_list bufs x in
    List.fold_left
      (fun acc ts ->
         List.fold_left
           (fun acc tid -> flush tid bufs abstr acc)
           acc ts)
      d to_flush

  let close_after_mop x d =
    (* Compute the flush closure of the domain d after a memory
       operation on the shared variable x *)
    M.fold (flush_mop x) d d

  let transfer d { P.thread_id; elem = ins } =
    match ins with
    | O.Identity -> d
    | O.MFence ->
      M.filter (fun bufs _ -> Bufs.is_empty (Bufs.nth bufs thread_id)) d
    | O.Assign (x, expr) ->
      let d = (* Make the assignation on inner abstract variables *)
        M.map
          (fun abstr ->
             Inner.assign_expr abstr
               (P.create_threaded ~thread_id x)
               (P.create_threaded ~thread_id expr))
          d in
      let d = (* Add x to the tid-th buffer if it's a write *)
        match x.P.var_type with
        | P.Local -> d
        | P.Shared ->
          M.Labels.fold
            ~f:(fun ~key:bufs ~data:abstr acc ->
                add_join
                  (Bufs.write bufs thread_id x.P.var_name)
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
        (fun abstr ->
           Inner.meet_cons abstr @@ P.create_threaded ~thread_id cond)
        d
      |> normalize

  let join =
    M.merge
      (fun _bufs abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> abstr1
         | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2))

  let satisfies _ =
    Error.not_implemented_msg_error
      "Constraint satisfaction is not implemented on domain Abstract"
end
