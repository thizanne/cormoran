open Batteries
open Printf
open Graph

module ThreadState = struct
  type t = Program.control_label
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module State = struct
  type t = Program.control_state
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Operation = struct
  type operation =
    | Identity
    | MFence
    | Filter of Program.condition
    | Assign of Program.var * Program.expression

  type t = operation Program.threaded

  let compare = Pervasives.compare

  let default = {
    Program.thread_id = -1;
    elem = Identity;
  }
end

module ThreadG =
  Persistent.Digraph.ConcreteLabeled (ThreadState) (Operation)

module G =
  Persistent.Digraph.ConcreteLabeled (State) (Operation)

let of_thread thread_id { Program.body; _ } =

  let open Operation in
  let open Location in
  let module P = Program in

  let filter_not cond =
    Filter (P.LogicUnop (mkdummy P.Not, cond))
  in

  let filter_rel rel i exp =
    Filter (P.ArithRel (mkdummy rel, mkloc (P.Var i) i.loc, exp))
  in

  (* The following functions take as a parameter and return a couple
     `(graph, offset)`. The graph is a CFG of a single-thread program,
     thus with edges in the form of [n : int].

     These couples should observe the invariant that `offset` is the
     number present in the "last" vertex of the graph, that is the
     vertex corresponding to the end of the program. *)

  let add_op_edge op orig dest (acc, offset) =
    (* Adds an edge from orig to dest corresponding to the operation op.
       orig and dest are expected to be existing vertices in the graph *)
    ThreadG.add_edge_e acc
      (ThreadG.E.create orig
         (Program.create_threaded thread_id op)
         dest),
    offset
  in

  let single_edge op (acc, offset) =
    (* Adds a single vertex to the graph, and a single edge from the
       former last vertex of the graph to this vertex *)
    ThreadG.add_edge_e acc
      (ThreadG.E.create offset
         (Program.create_threaded thread_id op)
         (offset + 1)),
    (offset + 1)
  in

  let rec of_body (acc, offset) = function
    | P.Nothing ->
      acc, offset
    | P.Pass ->
      single_edge Identity (acc, offset)
    | P.MFence ->
      single_edge MFence (acc, offset)
    | P.Assign (x, e) ->
      single_edge (Assign (x.item, e.item)) (acc, offset)
    | P.Seq (b1, b2) ->
      of_body (of_body (acc, offset) b1.item) b2.item
    | P.If (cond, body) ->
      let acc', offset' = of_body (acc, offset + 1) body.item in
      (acc', offset')
      |> add_op_edge (Filter cond.item) offset (offset + 1)
      |> add_op_edge (filter_not cond) offset offset'
    | P.While (cond, body) ->
      let acc', offset' = of_body (acc, offset + 1) body.item in
      (acc', offset')
      |> add_op_edge (Filter cond.item) offset (offset + 1)
      |> add_op_edge (filter_not cond) offset offset'
      |> add_op_edge Identity offset' offset
    | P.For (i, exp_from, exp_to, body) ->
      let acc', offset' = of_body (acc, offset + 2) body.item in
      (acc', offset')
      |> add_op_edge (Assign (i.item, exp_from.item)) offset (offset + 1)
      |> add_op_edge (filter_rel P.Le i exp_to) (offset + 1) (offset + 2)
      |> add_op_edge (filter_rel P.Gt i exp_to) (offset + 1) offset'
      |> add_op_edge Identity offset' (offset + 1)

  in fst (of_body (ThreadG.add_vertex ThreadG.empty 0, 0) body)

module Oper = Oper.Make (Builder.P (G))

let combine cfg1 cfg2 =
  (* Combines two CFG.
     cfg1 is the cfg of a single thread,
     cfg2 is the cfg of a program. *)
  G.empty
  |> ThreadG.fold_vertex
    (fun i -> Oper.union (G.map_vertex (fun is -> i :: is) cfg2))
    cfg1
  |> ThreadG.fold_edges_e
    (fun (i, op, i') ->
       G.fold_vertex
         (fun is g -> G.add_edge_e g (G.E.create (i :: is) op (i' :: is)))
         cfg2)
    cfg1

let of_program { Program.threads; _ } =
  List.fold_righti
    (fun thread body g -> combine (of_thread thread body) g)
    threads
    (G.add_vertex G.empty [])
