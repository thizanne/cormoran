open Apron
open Batteries
open Format
open Syntax
open Syntax.Typed

(* A buffer is abstracted by a Deque giving the order of the more
   recent entries of each variable in this buffer.
*)

module Bufs : sig
  type buf
  type t
  val compare : t -> t -> int
  val nb_threads : t -> int
  val nth : t -> int -> buf
  val add : t -> int -> Symbol.t -> t
  val flush : t -> int -> t
  val all_flush_list : t -> Symbol.t -> (int * Symbol.t) list list
end
=
struct
  type buf = Symbol.t Deque.t

  type t = buf list (* TODO: change list to persistent arrays *)

  let compare = compare

  let nb_threads = List.length

  let nth = List.at

  let rec add bufs t var =
    List.modify_at t (Deque.cons var) bufs

  let rec flush bufs t =
    List.modify_at t
      (fun buf -> match Deque.rear buf with
         | Some (buf', _) -> buf'
         | None -> failwith "flush")
      bufs

  let all_flush_list bufs x =
    (* TODO can probably be done better *)
    let t_list t buf =
      Deque.fold_left (fun acc x -> (t, x) :: acc) [] buf
    in
    bufs
    |> List.filteri_map (fun t buf ->
        match Deque.rear buf with
        | Some (_, y) when y = x -> Some (t_list t buf)
        | _ -> None)
    |> List.n_cartesian_product
    |> List.sort_uniq Pervasives.compare

end

(* An abstract domain is the map from buffer abstractions to
   a numerical domain *)

module M = Map.Make (Bufs)

type t = Polka.loose Polka.t Abstract1.t M.t

let man = Polka.manager_alloc_loose ()

(* TODO: The construction of the name of a shared variable is ad-hoc,
   only appending the number of the thread to the name of the
   variable. This could be improved by a better management of
   identifiers and a conflict check (especially with register
   names). *)

let shared_var v t =
  Var.of_string @@ sprintf "%s_%d" v.item t

let local_var v =
  Var.of_string v.item

let binop c =
  let open Texpr0 in
  List.assoc c ['+', Add; '-', Sub; '*', Mul; '/', Div]

let texpr_shared env x t =
  Texpr1.var env (shared_var x t)

let texpr_val = function
  | Int n -> Texpr1.Cst (Coeff.s_of_int n.item)
  | Var v -> Texpr1.Var (local_var v)

let texpr env expr =
  let rec aux = function
    | Val v -> texpr_val v.item
    | Op (op, e1, e2) ->
      Texpr1.Binop (
        binop op.item,
        aux e1.item, aux e2.item,
        Texpr1.Int, Texpr1.Zero
      )
  in Texpr1.of_expr env (aux expr)

let flush t x bufs abstr d =
  (* Updates the abstract domain d to take into account the flush of
     the oldest entry of the t-th buffer from the numerical domain
     abstr *)
  let env = Abstract1.env abstr in

  (* Assign the value of x_t to every x_i in the numerical domain
     abstr *)
  let var_array = (* [|x_0; x_1; ...|] as Var.t *)
    Array.init (Bufs.nb_threads bufs) (shared_var x) in
  let texpr_array = (* [|x_t; x_t; ...|] as Texpr1.t *)
    Array.make (Bufs.nb_threads bufs) (texpr_shared env x t) in
  let abstr =
    Abstract1.assign_texpr_array man abstr var_array texpr_array None in

  (* Make the joins *)
  Map.modify_def
    abstr
    (Bufs.flush bufs t)
    (fun abstr' -> Abstract1.join man abstr' abstr)
    d

let flush_mop x bufs abstr d =
  (* Updates the abstract domain d to take into account every possible
     combination of flush from the numerical domain abstr after a memory
     operation on the variable x *)
  let to_flush = Bufs.all_flush_list bufs x in
  assert false

let transfer d t ins =
  let op_of_jump = function
    | Jnz _ -> "!="
    | Jz _ -> "="
    | _ -> failwith "op_of_jump"
  in

  let env = Map.choose d |> snd |> Abstract1.env in

  match ins with
  | Pass
  | Label _
  | Jmp _ -> d
  | MFence ->
    Map.filter (fun bufs _ -> List.nth bufs t = []) d
  | RegOp (r, e) ->
    Map.map
      (fun abstr ->
         Abstract1.assign_texpr man abstr (local_var r)
           (texpr env e.item) None) d
  | Cmp (r0, v1, v2) ->
    let earray () = Tcons1.array_make env 1 in
    let suparray, infarray, eqarray = earray (), earray(), earray() in
    let tcons op =
      sprintf "%s %c %s"
        (string_of_value v1.item) op (string_of_value v2.item)
      |> Parser.tcons1_of_string env in
    let affect n abstr =
      Abstract1.assign_texpr man abstr (Var.of_string r0.item)
        (Texpr1.cst env (Coeff.s_of_int n)) None in
    begin
      Tcons1.array_set suparray 0 (tcons '>');
      Tcons1.array_set infarray 0 (tcons '<');
      Tcons1.array_set eqarray 0 (tcons '=');
      Map.map
        (fun abstr -> Abstract1.join_array man [|
             Abstract1.meet_tcons_array man abstr suparray
             |> affect 1;
             Abstract1.meet_tcons_array man abstr infarray
             |> affect (-1);
             Abstract1.meet_tcons_array man abstr eqarray
             |> affect 0;
           |]) d
    end
  | Jnz (r, _)
  | Jz (r, _) ->
    let earray = Tcons1.array_make env 1 in
    let () =
      sprintf "%s %s 0" r.item (op_of_jump ins)
      |> Parser.tcons1_of_string env
      |> Tcons1.array_set earray 0 in
    Map.map
      (fun abstr -> Abstract1.meet_tcons_array man abstr earray)
      d
  | Read (r, x) ->
    failwith "TODO"
  | Write (x, v) -> failwith "TODO"
