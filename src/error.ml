(*
open Batteries doesn't work with [@@deriving show]
see https://github.com/ocaml-batteries-team/batteries-included/issues/597
*)

open Printf

type error =
  | LexingError
  | SyntaxError
  | TypeError
  | NameError
  | NotImplementedError
      [@@deriving show]

type t = {
  error : error;
  err_loc : Location.t;
  err_msg : string;
}

exception Error of t

let on_loc_error error err_loc err_msg =
  raise @@ Error {error; err_loc; err_msg}

let lexing_loc_error item err_msg =
  on_loc_error LexingError item err_msg

let syntax_loc_error item err_msg =
  on_loc_error SyntaxError item err_msg

let type_loc_error item err_msg =
  on_loc_error TypeError item err_msg

let name_loc_error item err_msg =
  on_loc_error NameError item err_msg

let not_implemented_loc_error item err_msg =
  on_loc_error NotImplementedError item err_msg

let on_item_error error item err_msg =
  raise @@ Error {error; err_loc = item.Location.loc; err_msg}

let lexing_error item err_msg =
  on_item_error LexingError item err_msg

let syntax_error item err_msg =
  on_item_error SyntaxError item err_msg

let type_error item err_msg =
  on_item_error TypeError item err_msg

let name_error item err_msg =
  on_item_error NameError item err_msg

let not_implemented_error item err_msg =
  on_item_error NotImplementedError item err_msg

let msg_error error err_msg =
  raise @@ Error {error; err_loc = Location.dummy; err_msg}

let lexing_msg_error err_msg =
  msg_error LexingError err_msg

let syntax_msg_error err_msg =
  msg_error SyntaxError err_msg

let type_msg_error err_msg =
  msg_error TypeError err_msg

let name_msg_error err_msg =
  msg_error NameError err_msg

let not_implemented_msg_error err_msg =
  msg_error NotImplementedError err_msg

let to_string {error; err_loc = {Location.startpos; endpos}; err_msg} =
  let open Lexing in
  sprintf "%s:\nFrom line %d, column %d to line %d, column %d\n%s"
    (show_error error)
    startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
    endpos.pos_lnum (endpos.pos_cnum - endpos.pos_bol)
    err_msg
