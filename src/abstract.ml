open Apron
open Batteries
open Error
open Format
open Syntax
open Syntax.Typed
open Syntax.TypedProgram
open Util

(* A buffer is abstracted by a Deque giving the order of the more
   recent entries of each variable in this buffer.
*)

module Bufs : sig
  type buf
  val is_empty : buf -> bool
  val last : buf -> Symbol.t (* can raise Not_found *)
  val is_last : buf -> Symbol.t -> bool
  type t
  val compare : t -> t -> int
  val nb_threads : t -> int
  val nth : t -> int -> buf
  val write : t -> int -> Symbol.t -> t
  val init : Syntax.TypedProgram.t -> t
  val flush : t -> int -> t
  val all_flush_list : t -> Symbol.t -> int list list
  val get_no_var : t -> Symbol.t -> int list
  val print : 'a BatIO.output -> t -> unit
end
=
struct
  type buf = Symbol.t Deque.t

  let is_empty = Deque.is_empty

  let last buf = match Deque.rear buf with
    | Some (_, x) -> x
    | None -> raise Not_found

  let is_last buf x =
    try last buf = x
    with Not_found -> false

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
         Enum.compare Symbol.compare
           (Deque.enum dq1) (Deque.enum dq2))

  let nb_threads = List.length

  let nth = List.at

  let rec write bufs t var =
    List.modify_at t (move_to_head var) bufs

  let init prog =
    List.init (Array.length prog.threads) (fun _ -> Deque.empty)

  let rec flush bufs t =
    List.modify_at t
      (fun buf -> match Deque.rear buf with
         | Some (buf', _) -> buf'
         | None -> failwith "flush")
      bufs

  let all_flush_list bufs x =
    (* TODO can probably be done better *)
    let t_list t buf =
      (* ~= List.make (Deque.length buf) t, if Deque.length existed *)
      Deque.fold_left (fun acc x -> t :: acc) [] buf
    in

    bufs
    |> List.filteri_map
      (fun t buf ->
         match Deque.rear buf with
         | Some (_, y) when y = x -> Some (t_list t buf)
         | _ -> None)
    |> List.concat
    |> ordered_parts
    |> List.sort_uniq (List.compare Int.compare)

  let get_no_var bufs x =
    List.filteri_map
      (fun t buf ->
         if is_absent buf x then Some t
         else None) bufs

  let print output =
    List.print (Deque.print Symbol.print) output

end

let man = Polka.manager_alloc_loose ()

(* An abstract domain is the map from buffer abstractions to
   a numerical domain *)

module M = Map.Make (Bufs)

module Make (Inner : Domain.Inner) = struct
  (*  type t = Polka.loose Polka.t Abstract1.t M.t *)

  type t = Inner.t M.t

  let bottom = M.empty

  let normalize =
    M.filterv (fun abstr -> not (Inner.is_bottom abstr))

  let print output =
    M.print ~first:"" ~last:"" Bufs.print Inner.print output

  let equal d1 d2 =
    M.equal Inner.equal (normalize d1) (normalize d2)

  (* TODO: The construction of the name of a shared variable is ad-hoc,
     only appending the number of the thread to the name of the
     variable. This could be improved by a better management of
     identifiers and a conflict check (especially with register
     names). *)

  let shared_var v t =
    Var.of_string @@ sprintf "%s_%d" v t

  let local_var v =
    Var.of_string v

  let init prog =
    M.singleton (Bufs.init prog) (Inner.init prog)

  let add_join bufs abstr d =
    M.modify_def abstr bufs (Inner.join abstr) d

  let flush t bufs abstr d =
    (* Updates the abstract domain d to take into account the flush of
       the oldest entry of the t-th buffer from the numerical domain
       abstr *)
    let x = Bufs.last (Bufs.nth bufs t) in
    (* Assign the value of x_t to every x_i in the numerical domain
       abstr where x_i is not present in buffer i *)
    let var_array = (* [|x_0; x_1; ...|] as Var.t *)
      Bufs.get_no_var bufs x
      |> List.map (Expression.shared_var x)
      |> Array.of_list in
    let texpr_array = (* [|x_t; x_t; ...|] as Texpr1.t *)
      Array.make (Array.length var_array) (Expression.shared x t) in
    let abstr =
      Inner.assign_expr_array abstr var_array texpr_array in

    (* Make the joins *)
    add_join (Bufs.flush bufs t) abstr d

  let flush_mop x t bufs abstr d =
    (* Updates the abstract domain d to take into account every possible
       combination of flush from the numerical domain abstr after a memory
       operation on the variable x by the thread t *)
    let to_flush = Bufs.all_flush_list bufs x in
    List.fold_left
      (fun acc ts ->
         List.fold_left
           (fun acc t -> flush t bufs abstr acc)
           acc ts)
      d to_flush

  let close_after_mop x t d =
    (* Compute the flush closure of the domain d after a memory
       operation on the shared variable x *)
    M.fold (flush_mop x t) d d

  let transfer d t ins =
    match ins with
    | Pass
    | Label _
    | Jmp _ -> d
    | MFence ->
      M.filter (fun bufs _ -> Bufs.is_empty (Bufs.nth bufs t)) d
    | RegOp (r, e) ->
      M.map
        (fun abstr ->
           Inner.assign_expr abstr (Expression.local_var r.item)
             (Expression.of_syntax e.item)) d
    | Cmp (r0, v1, v2) ->
      let cons typ = Constraint.make typ
          (Expression.of_syntax_value v1.item)
          (Expression.of_syntax_value v2.item) in
      let affect_r0 n abstr =
        Inner.assign_expr abstr (Expression.local_var r0.item) (Expression.int n) in
      M.map
        (fun abstr ->
           Inner.join_array [|
             Inner.meet_cons abstr (cons Constraint.Gt) |> affect_r0 1;
             Inner.meet_cons abstr (cons Constraint.Lt) |> affect_r0 (-1);
             Inner.meet_cons abstr (cons Constraint.Eq) |> affect_r0 0;
           |]) d
    | Jz (r, _) ->
      M.map
        (fun abstr ->
           Inner.meet_cons abstr @@
           Constraint.make Constraint.Eq
             (Expression.local r.item)
             (Expression.int 0))
        d
      |> normalize
    | Jnz (r, _) ->
      M.map
        (fun abstr ->
           Inner.join
             (Inner.meet_cons abstr @@
              Constraint.make Constraint.Gt
                (Expression.local r.item)
                (Expression.int 0))
             (Inner.meet_cons abstr @@
              Constraint.make Constraint.Lt
                (Expression.local r.item)
                (Expression.int 0)))
        d
      |> normalize
    | Read (r, x) ->
      d
      |> M.map
        (fun abstr ->
           Inner.assign_expr abstr
             (Expression.local_var r.item)
             (Expression.shared x.item t))
      |> close_after_mop x.item t
    | Write (x, v) ->
      d
      |> M.map (* x_t = v *)
        (fun abstr ->
           Inner.assign_expr abstr
             (Expression.shared_var x.item t)
             (Expression.of_syntax_value v.item))
      |> M.Labels.fold (* Add x to the t-th buffer *)
        ~f:(fun ~key:bufs ~data:abstr acc ->
            add_join (Bufs.write bufs t x.item) abstr acc) ~init:M.empty
      |> close_after_mop x.item t (* Close by partial flush *)

  let join =
    M.merge
      (fun _bufs abstr1 abstr2 -> match (abstr1, abstr2) with
         | None, _ -> abstr2
         | _, None -> abstr1
         | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2))

  let satisfies t =
    raise (Error [
        NotImplementedError,
        Lexing.dummy_pos, Lexing.dummy_pos,
        "Constraint satisfaction is not implemented on domain Abstract"
      ])
end
