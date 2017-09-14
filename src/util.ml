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
  Option
*)

let str_int_option = function
  | None -> "∅"
  | Some x -> string_of_int x

let print_int_option output opt =
  String.print output (str_int_option opt)

let print_option print output = function
  | None -> String.print output "∅"
  | Some x -> print output x

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
   Timing
*)

let time1 f =
  let cumulative_time = ref 0. in
  fun arg1 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time2 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time3 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 arg3 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 arg3 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time4 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 arg3 arg4 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 arg3 arg4 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time5 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 arg3 arg4 arg5 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 arg3 arg4 arg5 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time6 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 arg3 arg4 arg5 arg6 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 arg3 arg4 arg5 arg6 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res

let time7 f =
  let cumulative_time = ref 0. in
  fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 ->
    let t1 = Unix.gettimeofday () in
    let res = f arg1 arg2 arg3 arg4 arg5 arg6 arg7 in
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    Printf.printf
      "\x1b[33mTiming: %f, cumulative: %f\x1b[39;49m\n%!"
      time (!cumulative_time +. time);
    cumulative_time := !cumulative_time +. time;
    res
