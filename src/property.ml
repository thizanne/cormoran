open Batteries

(* A thread code portion delimited by two labels *)
type zone = {
  initial : Symbol.t option;
  final : Symbol.t option;
}

type t =
  | CriticalSection of zone Program.threaded list
  | FinalCondition of Program.condition (* Also requires empty buffers *)
  | ZoneCondition of
      zone Program.threaded list *
      Program.condition Program.threaded
  | And of t * t
  | Or of t * t
