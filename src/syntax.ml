open Batteries

type 'a loc = {
  item : 'a;
  startpos : Lexing.position;
  endpos : Lexing.position;
}

let dummy_loc item = {
  item;
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}


type position = int list

type value =
  | Int of int loc
  | Var of string loc

let string_of_value = function
  | Int n -> string_of_int n.item
  | Var v -> v.item

type expression =
  | Val of value loc
  | Op of char loc * expression loc * expression loc

module Untyped =
struct
  type t  =
    | Pass
    | Affect of string loc * expression loc
    | Cmp of string loc * value loc * value loc
    | MFence
    | Label of string loc
    | Jnz of string loc * string loc
    | Jz of string loc * string loc
    | Jmp of string loc
end

module Typed =
struct
  type t =
    | Pass
    | Read of string loc * string loc
    | Write of string loc * value loc
    | RegOp of string loc * expression loc
    | Cmp of string loc * value loc * value loc
    | MFence
    | Label of string loc
    | Jnz of string loc * string loc (* register, label *)
    | Jz of string loc * string loc
    | Jmp of string loc
end

module Program (Ins : sig type t end) =
struct
  type thread = {
    locals : string list;
    ins : Ins.t loc array;
  }

  type t = {
    initial : (string * int) list;
    threads : thread array;
  }

  let nth_ins program t i =
    program.threads.(t).ins.(i)

  let initial_position program =
    List.make (Array.length program.threads) 0
end

module UntypedProgram = Program (Untyped)
module TypedProgram = Program (Typed)
