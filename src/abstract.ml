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

(* An abstract domain is the map from buffer abstractions to
   a numerical domain *)

module M = Map.Make (Bufs)

type t = Polka.loose Polka.t Abstract1.t M.t

let bottom = M.empty

let poly_print output abstr =
  let fmt = Format.formatter_of_output output in
  Abstract1.print fmt abstr;
  Format.pp_print_flush fmt ()

let print output =
  M.print ~first:"" ~last:"" Bufs.print poly_print output

let man = Polka.manager_alloc_loose ()

let equal d1 d2 =
  M.equal (Abstract1.is_eq man)
    (M.filterv (fun abstr -> not (Abstract1.is_bottom man abstr)) d1)
    (M.filterv (fun abstr -> not (Abstract1.is_bottom man abstr)) d2)

(* TODO: The construction of the name of a shared variable is ad-hoc,
   only appending the number of the thread to the name of the
   variable. This could be improved by a better management of
   identifiers and a conflict check (especially with register
   names). *)

let shared_var v t =
  Var.of_string @@ sprintf "%s_%d" v t

let local_var v =
  Var.of_string v

let texpr_int env n =
  Texpr1.cst env (Coeff.s_of_int n)

let binop c =
  let open Texpr0 in
  List.assoc c ['+', Add; '-', Sub; '*', Mul; '/', Div]

let texpr_local env r =
  Texpr1.var env (local_var r)

let texpr_neg env r =
  Texpr1.unop Texpr1.Neg (texpr_local env r) Texpr1.Int Texpr1.Zero

let texpr_shared env x t =
  Texpr1.var env (shared_var x t)

let texpr_val env = function
  | Int n -> texpr_int env n.item
  | Var v -> Texpr1.var env (local_var v.item)

let rec texpr env = function
  | Val v -> texpr_val env v.item
  | Op (op, e1, e2) ->
    Texpr1.binop
      (binop op.item)
      (texpr env e1.item) (texpr env e2.item)
      Texpr1.Int Texpr1.Zero

let init prog =
  let open LazyList in
  let threads = init (Array.length prog.threads) (fun x -> x) in
  let shared = (* Enumeration of (x, value of x) initial shared vars *)
    of_list prog.initial
    |> map
      (fun (x, v) -> map (fun t -> shared_var x t, v) threads)
    |> concat in
  let local = (* LazyListeration of all thread-local vars *)
    of_array prog.threads
    |> map (fun th -> of_list th.locals)
    |> concat
    |> map local_var in
  let env = Environment.make
      (to_array @@ append (map fst shared) local)
      [||] (* No real variables *) in
  let abstr =
    fold_left
      (fun acc (x_t, n) ->
         Abstract1.assign_texpr man acc x_t (texpr_int env n) None)
      (Abstract1.top man env) shared in
  M.singleton (Bufs.init prog) abstr

let add_join bufs abstr d =
  M.modify_def abstr bufs (Abstract1.join man abstr) d

let flush t bufs abstr d =
  (* Updates the abstract domain d to take into account the flush of
     the oldest entry of the t-th buffer from the numerical domain
     abstr *)
  let env = Abstract1.env abstr in
  let x = Bufs.last (Bufs.nth bufs t) in
  (* Assign the value of x_t to every x_i in the numerical domain
     abstr where x_i is not present in buffer i *)
  let var_array = (* [|x_0; x_1; ...|] as Var.t *)
    Bufs.get_no_var bufs x
    |> List.map (shared_var x)
    |> Array.of_list in
  let texpr_array = (* [|x_t; x_t; ...|] as Texpr1.t *)
    Array.make (Array.length var_array) (texpr_shared env x t) in
  let abstr =
    Abstract1.assign_texpr_array man abstr var_array texpr_array None in

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
  let env = M.choose d |> snd |> Abstract1.env in

  let tcons_array tcons =
    let earray = Tcons1.array_make env 1 in
    Tcons1.array_set earray 0 tcons;
    earray
  in

  match ins with
  | Pass
  | Label _
  | Jmp _ -> M.map (Abstract1.copy man) d
  | MFence ->
    M.filter (fun bufs _ -> Bufs.is_empty (Bufs.nth bufs t)) d
  | RegOp (r, e) ->
    M.map
      (fun abstr ->
         Abstract1.assign_texpr man abstr (local_var r.item)
           (texpr env e.item) None) d
  | Cmp (r0, v1, v2) ->
    let tcons op =
      sprintf "%s %c %s"
        (string_of_value v1.item) op (string_of_value v2.item)
      |> Parser.tcons1_of_string env in
    let affect_r0 n abstr =
      Abstract1.assign_texpr man abstr (local_var r0.item)
        (texpr_int env n) None in
    let suparray = tcons_array @@ tcons '>' in
    let infarray = tcons_array @@ tcons '<' in
    let eqarray = tcons_array @@ tcons '=' in
    M.map
      (fun abstr -> Abstract1.join_array man [|
           Abstract1.meet_tcons_array man abstr suparray |> affect_r0 1;
           Abstract1.meet_tcons_array man abstr infarray |> affect_r0 (-1);
           Abstract1.meet_tcons_array man abstr eqarray  |> affect_r0 0;
         |]) d
  | Jz (r, _) ->
    let earray = tcons_array (Tcons1.make (texpr_local env r.item) Tcons1.EQ) in
    M.map (fun abstr -> Abstract1.meet_tcons_array man abstr earray) d
  | Jnz (r, _) ->
    let earray_sup = tcons_array (Tcons1.make (texpr_local env r.item) Tcons1.SUP) in
    let earray_inf = tcons_array (Tcons1.make (texpr_neg env r.item) Tcons1.SUP) in
    M.map
      (fun abstr ->
         Abstract1.join man
           (Abstract1.meet_tcons_array man abstr earray_sup)
           (Abstract1.meet_tcons_array man abstr earray_inf))
      d
  | Read (r, x) ->
    let var_r = local_var r.item in
    let texpr_x = Texpr1.var env (shared_var x.item t) in
    d
    |> M.map (fun abstr -> Abstract1.assign_texpr man abstr var_r texpr_x None)
    |> close_after_mop x.item t
  | Write (x, v) ->
    let var_xt = shared_var x.item t in
    let texpr_v = texpr_val env v.item in
    d
    |> M.map (* x_t = v *)
      (fun abstr -> Abstract1.assign_texpr man abstr var_xt texpr_v None)
    |> M.Labels.fold (* Add x to the t-th buffer *)
      ~f:(fun ~key:bufs ~data:abstr acc ->
          add_join (Bufs.write bufs t x.item) abstr acc) ~init:M.empty
    |> close_after_mop x.item t (* Close by partial flush *)

let join =
  M.merge
    (fun bufs abstr1 abstr2 -> match (abstr1, abstr2) with
       | None, _ -> abstr2
       | _, None -> abstr1
       | Some abstr1, Some abstr2 -> Some (Abstract1.join man abstr1 abstr2))

let satisfies t =
  raise (Error [
    NotImplementedError,
    Lexing.dummy_pos, Lexing.dummy_pos,
    "Constraint satisfaction is not implemented on domain Abstract"
  ])
