open Error
open Printf

let prgm_file = open_in Sys.argv.(1)

let rec print_list p = function
  | [] -> ()
  | [x] -> p x
  | x :: xs -> p x; print_string "; "; print_list p xs

let str_mark = function
  | Marked.MPos -> "●"
  | Marked.MNeg -> "○"

let str_option = function
  | None -> "∅"
  | Some x -> string_of_int x

let print_vars =
  let rec aux t = function
    | [] -> ()
    | vars :: next ->
      print_list
        (fun (x, (v, m)) ->
           printf "%s_%d → %s %s" x t (str_option v) (str_mark m))
        vars;
      begin match next with
        | [] -> ()
        | _ -> printf "; "; aux (succ t) next
      end
  in aux 0

let print_point {Marked.regs; vars} =
  print_list (fun (r, v) -> printf "%s → %s" r (str_option v)) regs;
  print_newline ();
  print_vars vars;
  print_newline ()

let rec print_points =
  Marked.S.iter
    (fun p ->
       print_point p; print_newline (); print_newline())

let print_hashtbl d =
  Hashtbl.fold (fun k v li -> (k, v) :: li) d []
  |> List.sort Pervasives.compare
  |> List.iter
    (fun (s, p) ->
       print_string "############\n";
       print_list print_int s;
       print_string " :\n\n";
       print_points p;
       print_newline ())

let () =
  try
    let program = Parser.program Lexer.lexer (Lexing.from_channel prgm_file) in
    let program = Typing.type_program program in
    let result = Marked.analyse program in
    print_hashtbl result
  with
  | Error li -> List.iter (fun e -> print_endline (msg_of_error e ^ "\n")) li
