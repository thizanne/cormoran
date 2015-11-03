open Batteries
open Util

module P = Program

type buf = Sym.t Deque.t

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
       Enum.compare Sym.Ord.compare
         (Deque.enum dq1) (Deque.enum dq2))

let nth = List.at

let tid_is_consistent bufs tid =
  Deque.is_empty @@ List.at bufs tid

let write bufs tid var =
  List.modify_at tid (move_to_head var) bufs

let init prog =
  List.init (List.length prog.P.threads) (fun _ -> Deque.empty)

let rec make_update bufs { Domain.var; origin; destinations } = match bufs, origin with
  (* TODO: assert that var is absent in every buffer of the
     destinations and is the last one in origin *)
  | [], _ -> raise @@ Invalid_argument "Bufs.flush: non existing tid"
  | b :: bs, 0 ->
    begin match Deque.rear b with
      | Some (front, _x) -> front :: bs
      | None -> raise @@ Invalid_argument "Bufs.flush: empty buffer"
    end
  | b :: bs, _ -> b :: make_update bs { Domain.var; destinations; origin = pred origin }

let get_destinations bufs var =
  (* Get the destinations of an update of the variable var. *)
  List.filteri_map
    (fun dest buf -> if is_absent buf var then Some dest else None)
    bufs

let rec create_updates bufs = function
  (* FIXME: remove a var from origin buffer once it has been flushed *)
  | [] -> []
  | origin :: origins ->
    let var = Option.get @@ older @@ nth bufs origin in {
      Domain.var;
      origin;
      destinations = get_destinations bufs var
    } :: create_updates bufs origins

let get_mop_updates bufs _tid x =
  (* Returns the list of all the sequences of updates that must be
     done on an element with the buffers bufs after a memory
     operation on the shared variable x, to preserve the
     closed-by-flush invariant property.
  *)

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
  |> List.map (create_updates bufs)

let with_no_var bufs x =
  (* Given buffers and a symbol x, returns the list of the thread ids
     whose buffer do not contain x *)
  List.filteri_map
    (fun t buf ->
       if is_absent buf x then Some t
       else None) bufs

let print output =
  List.print (Deque.print Sym.print) output
