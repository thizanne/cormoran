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

let mk item startpos endpos =
  { item; loc = { startpos; endpos } }

let mkloc item loc =
  { item; loc }

let mkdummy item =
  { item; loc = dummy }

let cobind f x =
  { item = f x; loc = x.loc }

let comap f { item; loc } =
  { item = f item; loc }
