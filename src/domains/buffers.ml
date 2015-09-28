open Batteries
open Util

module P = Program

module UnsoundOrdered : Domain.BufferAbstraction = struct
  type buf = Symbol.t Deque.t

  let older buf = match Deque.rear buf with
    | None -> None
    | Some (_, x) -> Some x

  let is_absent buf x =
    Deque.find (( = ) x) buf = None

  let move_to_head x buf =
    Deque.fold_left
      (fun acc y -> if x = y then acc else Deque.snoc acc y)
      (Deque.of_list [x])
      buf

  type t = buf list

  let compare = List.compare
      (fun dq1 dq2 ->
         Enum.compare Symbol.Ord.compare
           (Deque.enum dq1) (Deque.enum dq2))

  let nth = List.at

  let nth_is_empty bufs tid =
    Deque.is_empty @@ List.at bufs tid

  let write bufs tid var =
    List.modify_at tid (move_to_head var) bufs

  let init prog =
    List.init (List.length prog.P.threads) (fun _ -> Deque.empty)

  let rec flush bufs tid = match bufs, tid with
    | [], _ -> raise @@ Invalid_argument "Bufs.flush: non existing tid"
    | b :: bs, 0 ->
      begin match Deque.rear b with
        | Some (front, x) -> x, front :: bs
        | None -> raise @@ Invalid_argument "Bufs.flush: empty buffer"
      end
    | b :: bs, _ ->
      let x, bufs' = flush bs (pred tid) in
      x, b :: bufs'

  let flush_lists_after_mop bufs x =
    (* Returns the list of all the sequences of flush that must be
       done on an element with the buffers bufs after a memory
       operation on the shared variable x, to preserve the
       closed-by-flush invariant property.

       Due to commutativity of flushes of different variables, some
       different sequences may actually be equivalent (ie they lead to
       the same flush results). Generating all of them should not lead
       to any precision loss, but will increase the number of abstract
       computations.

       For instance, when the buffers are [[x; y]; [x; y]], the sequence [0;
       1; 0; 1] is equivalent to the sequence [0; 0; 1; 1].

       TODO: the current implementation generates different equivalent
       sequences. *)

    let t_list tid =
      List.make (Deque.size (nth bufs tid)) tid
    in

    bufs
    |> List.filteri_map
      (fun tid buf -> match older buf with
         | None -> None
         | Some y -> if x = y then Some (t_list tid) else None)
    |> List.concat
    |> ordered_parts
    |> List.sort_uniq (List.compare Int.compare)

  let with_no_var bufs x =
    (* Given buffers and a symbol x, returns the list of the thread ids
       whose buffer do not contain x *)
    List.filteri_map
      (fun t buf ->
         if is_absent buf x then Some t
         else None) bufs

  let print output =
    List.print (Deque.print Symbol.print) output

end
