open Util
open Printf
open Syntax
open Syntax.Typed
open Syntax.TypedProgram

type mark =
  | MPos
  | MNeg

type point = {
  regs : (string * int option) list;
  vars : (string * (int option * mark)) list list;
}

module S = Set.Make (struct type t = point let compare = compare end)

type t = S.t

let empty = S.empty

let union = S.union

let smap f s = S.fold (fun x e -> S.add (f x) e) s S.empty

let get_reg point r =
  List.assoc r point.regs

let get_var_mark point t x =
  List.assoc x @@ List.nth point.vars t

let get_var point t x =
  fst @@ get_var_mark point t x

let get_mark point t x =
  snd @@ get_var_mark point t x

let get_mem_var point x =
  let rec aux = function
    | [] -> failwith "get_mem_var"
    | t :: ts ->
      begin match List.assoc x t with
        | value, MNeg -> value
        | _, MPos -> aux ts
        | exception Not_found -> aux ts
      end in
  aux point.vars

let get_value point = function
  | Int n -> Some n.item
  | Var r -> get_reg point r.item

let rec get_expr point = function
  | Val v -> get_value point v.item
  | Op (op, e1, e2) ->
    let e1 = get_expr point e1.item in
    let e2 = get_expr point e2.item in
    begin match (e1, e2) with
      | Some v1, Some v2 -> (fun_of_op op.item) v1 v2
      | _, _ -> None
    end

let set_reg point r n = {
  point with regs = set_assoc r n point.regs
}

let set_var_mark point t x n mark = {
  point with vars =
               set_nth t
                 (set_assoc x (n, mark) (List.nth point.vars t))
                 point.vars
}

let set_var point t x n =
  set_var_mark point t x n (get_mark point t x)

let set_mark point t x mark =
  set_var_mark point t x (get_var point t x) mark

let set_all_vars_mneg point x n = {
  point with vars =
               List.map
                 (set_assoc x (n, MNeg))
                 point.vars
}

let neg_marked t x =
  S.filter (fun p -> get_mark p t x = MNeg)

let flush p t x =
  let n = get_var p t x in
  set_mark {
    p with vars =
             List.map
               (fun vt ->
                  if snd @@ List.assoc x vt = MNeg
                  then set_assoc x (n, MNeg) vt
                  else vt)
               p.vars
  } t x MNeg

let threads_mpos p x =
  let rec aux n = function
    | [] -> []
    | vn :: vars ->
      if snd @@ List.assoc x vn = MPos
      then n :: aux (succ n) vars
      else aux (succ n) vars
  in aux 0 p.vars

let threads_to_flush p x =
  ordered_parts @@ threads_mpos p x

let flush_many p ns x =
  List.fold_left (fun p n -> flush p n x) p ns

let flush_after_mop p x =
  List.map (fun ts -> flush_many p ts x) (threads_to_flush p x)

let all_flushes_after_mop domain x =
  let points = S.fold (fun p li -> flush_after_mop p x.item :: li) domain [] in
  let points = List.fold_left ( @ ) [] points in
  List.fold_right S.add points S.empty

let transfer domain t = function
  | Read (r, x) ->
    let domain = smap
        (fun p -> set_reg p r.item (get_var p t x.item)) domain in
    all_flushes_after_mop domain x
  | Write (x, v) ->
    let domain = smap
        (fun p -> set_var_mark p t x.item (get_value p v.item) MPos) domain in
    all_flushes_after_mop domain x
  | RegOp (r, e) ->
    smap
      (fun p -> set_reg p r.item (get_expr p e.item)) domain
  | Cmp (r, v1, v2) ->
    smap
      (fun p -> set_reg p r.item (
           let v1 = get_value p v1.item in
           let v2 = get_value p v2.item in
           match (v1, v2) with
           | Some n1, Some n2 ->
             Some (
               if n1 < n2 then -1
               else if n1 > n2 then 1
               else 0)
           | _, _ -> None))
      domain
  | Mfence ->
    S.filter
      (fun p ->
         List.for_all (fun (_, (_, m)) -> m = MNeg) @@
         List.nth p.vars t)
      domain
  | Label _ -> domain
  | Jnz (_, _) | Jz (_, _) | Jmp _ -> failwith "Jumps not implemented"

let initial_vars program =
  List.map (fun (x, v) -> x, (Some v, MNeg)) program.initial

let initial_point program = {
  regs =
    List.fold_left ( @ ) []
      (List.map
         (fun t -> List.map (fun r -> r, None) t.locals)
         program.threads);
  vars =
    (let vars = initial_vars program in
     List.map (fun _ -> vars) program.threads);
}

let init program = S.singleton (initial_point program)

let str_mark = function
  | MPos -> "●"
  | MNeg -> "○"

let print_vars =
  let rec aux t = function
    | [] -> ()
    | vars :: next ->
      print_list
        (fun (x, (v, m)) ->
           printf "%s_%d → %s %s" x t (str_int_option v) (str_mark m))
        vars;
      begin match next with
        | [] -> ()
        | _ -> printf "; "; aux (succ t) next
      end
  in aux 0

let print_point {regs; vars} =
  print_list (fun (r, v) -> printf "%s → %s" r (str_int_option v)) regs;
  print_newline ();
  print_vars vars;
  print_newline ()

let print =
  S.iter
    (fun p ->
       print_point p; print_newline (); print_newline())

let point_sat_cond (var, value) p =
  try
    begin try
        List.assoc var p.regs = Some value
      with
      | Not_found ->
        p.vars
        |> List.map (fun vars -> fst @@ List.assoc var vars)
        |> List.exists (( = ) (Some value))
    end
  with
  | Not_found -> failwith "point_sat_cond"

let point_sat cond p =
  List.for_all
    (fun c -> point_sat_cond c p)
    cond

let satisfies cond domain =
  S.exists (fun p -> point_sat cond p) domain
