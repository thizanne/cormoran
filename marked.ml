open Syntax
open Syntax.Typed
open Syntax.TypedProgram

type mark =
  | MPos
  | MNeg

type point = {
  regs : (string * int option) list;
  vars : (int * (string * int option) list) list;
  marks : mark list;
}

let set_assoc k v =
  List.map
    (fun (k', v') -> k', if k' = k then v else v')

let get_reg point r = List.assoc r point.regs

let get_var point t x = List.assoc x (List.assoc t point.vars)

let get_value point = function
  | Int n -> Some n.item
  | Var r -> get_reg point r.item

let get_mark point = List.nth point.marks

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

let set_var point t x n = {
  point with vars =
    List.map
      (fun (t', vars') ->
        t', if t' = t then set_assoc x n vars' else vars')
      point.vars
}

let set_all_vars point x n = {
  point with vars =
    List.map
      (fun (t', vars') ->
        t', set_assoc x n vars')
      point.vars
}

let set_mark point n mark =
  let rec set_mark marks n = match (marks, n) with
    | [], _ -> failwith "set_mark"
    | m :: ms, 0 -> mark :: ms
    | m :: ms, n -> m :: set_mark ms (pred n)
  in { point with marks = set_mark point.marks n}

let neg_marked t =
  List.filter (fun p -> get_mark p t = MNeg)

let transfer domain t = function
  | Read (r, x) ->
      List.map
        (fun p -> set_reg p r.item (get_var p t x.item)) domain
  | Write (x, v) ->
      let domain = List.map
        (fun p -> set_var p t x.item (get_value p v.item)) domain in
      let new_points = neg_marked t domain in
      let domain = List.map (fun p -> set_mark p t MPos) domain in
      let new_points =
        List.map
          (fun p ->
            set_all_vars p x.item (get_value p v.item)) new_points in
      domain @ new_points
  | RegOp (r, e) ->
      List.map
        (fun p -> set_reg p r.item (get_expr p e.item)) domain
  | Cmp (r, v1, v2) ->
      List.map
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
  | Mfence -> neg_marked t domain
  | Label _ -> domain
  | Jnz (_, _) | Jz (_, _) | Jmp _ -> failwith "Jumps not implemented"

let initial_vars program =
  List.map (fun (x, v) -> x, Some v) program.initial

let initial_point program = {
  regs = List.fold_left ( @ ) []
    (List.map
       (fun t -> List.map (fun r -> r, None) t.locals)
       program.threads);
  vars =
    (let vars = initial_vars program in
    List.mapi (fun n _ -> n, vars) program.threads);
  marks = List.map (fun _ -> MNeg) program.threads;
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
  Hashtbl.add result (repeat n_threads 0) [initial_point program];
  result

let rec state_pred = function
  | [] -> []
  | i :: is -> ((pred i) :: is) :: List.map (fun s -> i :: s) (state_pred is)

let nth_ins program t i =
  List.nth (List.nth program.threads t).ins i

let rec print_list = function
  | [] -> ()
  | t :: q -> print_int t; print_string "; "; print_list q

let analyse program =

  let result = init_domain program in

  let rec analyse_state s =
    let pred = state_pred s in
    List.iter
      (fun s' ->
        if not (List.mem (-1) s') then
          try ignore (Hashtbl.find result s') with
              Not_found -> analyse_state s')
      pred;
    Hashtbl.add result s
         (
           (List.fold_left ( @ ) []
                (List.mapi
                   (fun t s' ->
                     if List.mem (-1) s' then []
                     else
                       let i = List.nth s' t in
                       transfer
                         (Hashtbl.find result s')
                         t
                         (nth_ins program t i).item
                   )
                   pred
                )
           )
         )
  in analyse_state (List.map (fun t -> List.length t.ins) program.threads);
  result
