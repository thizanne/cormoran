open Batteries
open Graph

module T = TypedAst
module L = Location

module Label = struct
  include Int

  let initial = 0
  let is_initial = ( = ) 0

  let hash = Hashtbl.hash

  let enum ~initial ~final =
    initial -- final
end

module State = struct
  type t = Label.t list

  let equal = List.eq Label.equal
  let hash = Hashtbl.hash
  let empty = []

  let tid_label = List.nth
  let add_label = List.cons
  let from_label_list labels = labels
  let compare = List.compare Label.compare

  let is_initial =
    List.for_all (( = ) 0)

  let initial nb_threads =
    List.make nb_threads 0

  let print output =
    List.print Int.print ~first:"" ~last:"" output
end

module ThreadStructure =
struct

  module Edge = struct
    type t = Operation.t list
    let default = [Operation.Identity]
    let compare = List.compare Operation.compare
  end

  module Graph =
    Persistent.Digraph.ConcreteLabeled (Label) (Edge)

  type t = {
    graph : Graph.t;
    labels : Label.t Sym.Map.t;
    final : Label.t;
  }

  let of_thread { T.body; _ } =

    let open Operation in
    let open Label in

    let assign_edge var expr =
      Assign (var.L.item, expr.L.item)
    in

    let filter_edge cond =
      Filter cond.L.item
    in

    let filter_not_edge cond =
      Filter (T.Unop (L.mkdummy T.Not, cond))
    in

    (* The following functions take as a parameter and return a tuple
       `(graph, labels, offset)`. The graph is a CFG of a single-thread
       program, thus with edges in the form of [n : int].

       These tuples should observe the invariant that `offset` is the
       number present in the "last" vertex of the graph, that is the
       vertex corresponding to the end of the program. `labels` should
       also be a map from label symbols to valid vertices of the
       graph. *)

    let add_op_edge op orig dest (acc, labels, offset) =
      (* Adds an edge from orig to dest corresponding to the operation op.
         orig and dest are expected to be existing vertices in the graph *)
      Graph.add_edge_e acc @@ Graph.E.create orig op dest,
      labels,
      offset
    in

    let add_single_vertex (acc, labels, offset) =
      (* Adds a single vertex to the graph *)
      Graph.add_vertex acc (succ offset),
      labels,
      (succ offset)
    in

    let add_single_edge op (acc, labels, offset) =
      (* Adds a single vertex to the graph, and a single edge from the
         former last vertex of the graph to this vertex *)
      Graph.add_edge_e acc @@ Graph.E.create offset op @@ succ offset,
      labels,
      succ offset
    in

    let rec of_body (acc, labels, offset) { L.item = body; _ } =
      match body with
      (* TODO: Control blocks structures are generating one node more
         than necessary. The building functions should be modified to
         specify the end of the graph to yield, thus making the bodies
         point on the structure head rather than on a new node linked to
         the head by Identity. *)
      | T.Nothing ->
        acc, labels, offset
      | T.Label lbl ->
        acc, Sym.Map.add lbl.L.item offset labels, offset
      | T.Pass ->
        add_single_edge [Identity] (acc, labels, offset)
      | T.MFence ->
        add_single_edge [MFence] (acc, labels, offset)
      | T.Assign (x, e) ->
        add_single_edge
          [assign_edge x e]
          (acc, labels, offset)
      | T.Seq (b1, b2) ->
        of_body (of_body (acc, labels, offset) b1) b2
      | T.If (cond, body) ->
        let acc', labels', offset' =
          of_body (acc, labels, succ offset) body in
        (acc', labels', offset')
        |> add_op_edge [filter_edge cond] offset (succ offset)
        |> add_op_edge [filter_not_edge cond] offset offset'
      | T.While (cond, body) ->
        let acc', labels', offset' =
          of_body (acc, labels, succ offset) body in
        (acc', labels', offset')
        |> add_single_vertex
        |> add_op_edge [Identity] offset' offset
        |> add_op_edge [filter_edge cond] offset (succ offset)
        |> add_op_edge [filter_not_edge cond] offset (succ offset')
    in

    let graph, labels, final =
      of_body (
        Graph.add_vertex Graph.empty Label.initial,
        Sym.Map.empty,
        Label.initial
      ) body in
    { graph; labels; final }

end

module ProgramStructure = struct

  module TS = ThreadStructure

  module Edge = struct
    type t = Source.thread_id * Operation.t list
    let default = -1, [Operation.Identity]
    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:(List.compare Operation.compare)
  end

  module Graph =
    Persistent.Digraph.ConcreteLabeled (State) (Edge)

  type t = {
    graph : Graph.t;
    labels : Label.t Sym.Map.t list;
    final : State.t;
  }

  module Oper = Oper.Make (Builder.P (Graph))

  let combine
      tid
      { TS.graph = graph1; labels = labels1; final = final1 }
      { graph = graph2; labels = labels2; final = final2 }
    =
    (* Combines two CFG.
       tid is the tid of the thread to add,
       graph1 is the cfg of this single thread,
       graph2 is the cfg of a program. *)
    let ( ++ ) = State.add_label in
    let graph =
      Graph.empty
      |> TS.Graph.fold_vertex
        (fun i -> Oper.union (Graph.map_vertex (fun is -> i ++ is) graph2))
        graph1
      |> TS.Graph.fold_edges_e
        (fun (i, op, i') ->
           Graph.fold_vertex
             (fun is g ->
                Graph.add_edge_e g
                  (Graph.E.create
                     (i ++ is)
                     (tid, op)
                     (i' ++ is)))
             graph2)
        graph1 in
    let labels = labels1 :: labels2 in
    let final = final1 ++ final2 in
    { graph; labels; final }

  let of_program { T.threads; _ } =
    List.fold_righti
      (fun thread body g -> combine thread (TS.of_thread body) g)
      threads
      {
        graph = Graph.add_vertex Graph.empty State.empty;
        labels = [];
        final = State.empty;
      }
end
