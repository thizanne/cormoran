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
    | Write of Symbol.t * Syntax.expression
    | Read of Symbol.t * Symbol.t
    | RegOp of Symbol.t * Syntax.expression
    | Filter of Syntax.condition

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
