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

let smap f s = S.fold (fun x e -> S.add (f x) e) s S.empty

let set_assoc k v li =
  List.map
    (fun (k', v') -> k', if k' = k then v else v') li

let set_nth n v li =
  List.mapi (fun i x -> if i = n then v else x) li

let get_reg point r =
  List.assoc r point.regs

let get_var_mark point t x =
  List.assoc x @@ List.nth point.vars t

let get_var point t x =
  fst @@ get_var_mark point t x

let get_mark point t x =
  snd @@ get_var_mark point t x

let get_value point = function
  | Int n -> Some n.item
  | Var r -> get_reg point r.item

let fun_of_op op x y = match op with
  | '+' -> Some (x + y)
  | '-' -> Some (x - y)
  | '*' -> Some (x * y)
  | '/' -> if y = 0 then None else Some (x / y)
  | _ -> failwith "fun_of_op"

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
    let n = get_var p t x in {
      p with vars =
        List.map
          (fun vt ->
            if snd @@ List.assoc x vt = MNeg
            then set_assoc x (n, MNeg) vt
            else vt)
          p.vars
    }

let rec insert x = function
  | [] -> [[x]]
  | y :: ys ->
      (x :: y :: ys) ::
        List.map (fun yy -> y :: yy) (insert x ys)

let rec all_perm = function
  | [] -> [[]]
  | x :: xs ->
      let xs = all_perm xs in
      List.fold_left ( @ ) xs @@ List.map (insert x) xs

let threads_mpos p t x =
  let rec aux n = function
    | [] -> []
    | vn :: vars ->
        if snd @@ List.assoc x vn = MPos
        then n :: aux (succ n) vars
        else aux (succ n) vars
  in aux 0 p.vars

let threads_to_flush p t x =
  match get_mark p t x with
    | MPos -> [[]]
    | MNeg -> all_perm @@ threads_mpos p t x

let flush_many p ns x =
  List.fold_left (fun p n -> flush p n x) p ns

let flush_after_read p t x =
  List.map (fun ts -> flush_many p ts x) (threads_to_flush p t x)

let transfer domain t = function
  | Read (r, x) ->
      let domain = smap
        (fun p -> set_reg p r.item (get_var p t x.item)) domain in
      let points = S.fold
        (fun p li -> flush_after_read p t x.item :: li) domain [] in
      let points = List.fold_left ( @ ) [] points in
      List.fold_right S.add points S.empty
  | Write (x, v) ->
      let domain = smap
        (fun p -> set_var p t x.item (get_value p v.item)) domain in
      let new_points = neg_marked t domain in
      let domain = smap (fun p -> set_mark p t MPos) domain in
      let new_points =
        smap
          (fun p ->
            set_all_vars p x.item (get_value p v.item)) new_points in
      S.union domain new_points
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
          List.for_all
            (List.for_all
               (fun (_, (_, m)) -> m = MNeg))
            p.vars)
        domain
  | Label _ -> domain
  | Jnz (_, _) | Jz (_, _) | Jmp _ -> failwith "Jumps not implemented"

let initial_vars program =
  List.map (fun (x, v) -> x, (Some v, MNeg)) program.initial

let initial_point program = {
  regs = List.fold_left ( @ ) []
    (List.map
       (fun t -> List.map (fun r -> r, None) t.locals)
       program.threads);
  vars =
    (let vars = initial_vars program in
     List.map (fun _ -> vars) program.threads);
}

let repeat n v =
  let rec aux acc n =
    if n = 0 then acc
    else aux (v :: acc) (pred n)
  in aux [] n

let init_domain program =
  let n_threads = List.length program.threads in
  let n_ins = List.map (fun t -> List.length t.ins) program.threads in
  let result = Hashtbl.create (List.fold_left ( * ) 1 n_ins) in
  Hashtbl.add result (repeat n_threads 0) (S.singleton (initial_point program));
  result

let rec state_pred = function
  | [] -> []
  | i :: is -> ((pred i) :: is) :: List.map (fun s -> i :: s) (state_pred is)

let nth_ins program t i =
  List.nth (List.nth program.threads t).ins i

let analyse program =

  let result = init_domain program in

  let analyse_from_pred t s =
    if List.mem (-1) s then S.empty
    else
      let i = List.nth s t in
      transfer
        (Hashtbl.find result s) t
        (nth_ins program t i).item in

  let rec analyse_state s =
    let pred = state_pred s in
    List.iter
      (fun s' ->
        if not (List.mem (-1) s') then
          try ignore (Hashtbl.find result s') with
              Not_found -> analyse_state s')
      pred;
    List.mapi analyse_from_pred pred
    |> List.fold_left S.union S.empty
    |> Hashtbl.add result s in

  analyse_state (List.map (fun t -> List.length t.ins) program.threads);
  result
