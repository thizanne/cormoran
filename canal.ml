open Error
open Printf

let prgm_file = open_in Sys.argv.(1)

let rec print_list p = function
  | [] -> ()
  | [x] -> p x
  | x :: xs -> p x; print_string "; "; print_list p xs

let print_mark = function
  | Marked.MPos -> print_string "●"
  | Marked.MNeg -> print_string "○"

let str_option = function
  | None -> "∅"
  | Some x -> string_of_int x

let print_vars =
  print_list
    (fun (t, vs) ->
      print_list
        (fun (x, v) ->
          printf "%s_%d → %s" x t (str_option v)) vs)

let print_point {Marked.regs; vars; marks} =
  print_list (fun (r, v) -> printf "%s → %s" r (str_option v)) regs;
  print_newline ();
  print_vars vars;
  print_newline();
  print_list print_mark marks

let rec print_points = function
  | [] -> ()
  | p :: ps -> print_point p; print_newline (); print_newline(); print_points ps


let print_hashtbl =
  Hashtbl.iter
    (fun s p ->
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
