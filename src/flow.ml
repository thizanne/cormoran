open Batteries
open Printf

module State =
struct
  type t = Syntax.position
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Operation =
struct
  type operation =
    | Identity
    | MFence
    | Filter of Syntax.condition
    | Assign of Syntax.var * Syntax.expression

  type t = {
    thread : int;
    op : operation;
  }

  let compare = Pervasives.compare

  let default = {
    thread = -1;
    op = Identity;
  }
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (State) (Operation)
