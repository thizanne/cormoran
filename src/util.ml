open Batteries

(*
   Lexing
*)

let file_lexbuf filename =
  let open Lexing in
  let lexbuf = from_channel @@ open_in filename in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf

(*
   Print
*)

let print_to_string print item =
  let output = IO.output_string () in
  print output item;
  IO.close_out output

(*
  Option
*)

let str_int_option = function
  | None -> "∅"
  | Some x -> string_of_int x

let print_int_option output opt =
  String.print output (str_int_option opt)

let option_map2 f op1 op2 = match op1, op2 with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let option_bind2 f op1 op2 = match op1, op2 with
  | None, _ -> None
  | _, None -> None
  | Some x, Some y -> f x y

(*
  List
*)

let set_assoc k v li =
  List.map
    (fun (k', v') -> k', if k' = k then v else v') li

let set_nth n v li =
  List.mapi (fun i x -> if i = n then v else x) li

let rec incr_nth n = function
  | [] -> failwith "incr_nth"
  | x :: xs ->
    if n = 0
    then succ x :: xs
    else x :: incr_nth (pred n) xs

let rec last = function
  | [] -> raise Not_found
  | [x] -> x
  | _ :: xs -> last xs

let rec front = function
  | [] -> failwith "front"
  | [_] -> []
  | x :: xs -> x :: front xs

let rec inser_all_pos x = function
  | [] -> [[x]]
  | y :: ys ->
    (x :: y :: ys) ::
    List.map (fun yy -> y :: yy) (inser_all_pos x ys)

let rec ordered_parts = function
  | [] -> [[]]
  | x :: xs ->
    let xs = ordered_parts xs in
    List.flatten @@
    xs :: List.map (inser_all_pos x) xs

(*
  Misc
*)

let fun_of_op op x y = match op with
  | '+' -> Some (x + y)
  | '-' -> Some (x - y)
  | '*' -> Some (x * y)
  | '/' -> if y = 0 then None else Some (x / y)
  | _ -> failwith "fun_of_op"
