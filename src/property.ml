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

module Make (D : Domain.Outer) = struct

  let rec satisfies g data = function
    | CriticalSection zones ->
      failwith "TODO"
    (* forall state: data is bottom *)
    | FinalCondition c ->
      failwith "TODO"
    (* for final state: data is bottom *)
    | ZoneCondition (zones, conditions) ->
      failwith "TODO"
    (* for all zones: all states in data meet the condition *)
    | And (p1, p2) ->
      satisfies g data p1 && satisfies g data p2
    | Or (p1, p2) ->
      satisfies g data p1 || satisfies g data p2

end
