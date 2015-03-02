open Batteries

type t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
}

type 'a loc = {
  item : 'a;
  loc : t;
}

let dummy = {
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}

let mk startpos endpos =
  { startpos; endpos }

let mkloc item loc =
  { item; loc }

let mkdummy item =
  { item; loc = dummy }
