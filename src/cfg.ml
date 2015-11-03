open Batteries
open Graph

module O = Operators
module L = Location

module ThreadState = struct
  type t = Program.Control.Label.t
  let compare = Program.Control.Label.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module State = struct
  type t = Program.Control.State.t
  let compare = Program.Control.State.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Operation = struct
  type t =
    | Identity
    | MFence of Program.thread_id
    | Filter of Program.var_view Program.condition
    | Assign of
        Program.thread_id *
        Program.var *
        Program.var Program.expression

  let compare = Pervasives.compare

  let default = Identity
end

module ThreadG =
  Persistent.Digraph.ConcreteLabeled (ThreadState) (Operation)

module G =
  Persistent.Digraph.ConcreteLabeled (State) (Operation)

type t = {
  program : Program.var Program.t;
  graph : G.t;
  labels : Program.Control.Label.t Sym.Map.t array;
  final_state : Program.Control.State.t;
}

let cfg_of_thread thread_id { Program.body; _ } =

  let open Operation in
  let module P = Program in
  let open P.Control.Label in

  let rec expr_view = function
    (* Turns a var expression into a var_view expression *)
    | P.Int n -> P.Int n
    | P.Var v -> P.Var (L.comap (P.create_var_view ~thread_id) v)
    | P.ArithUnop (op, exp') ->
      P.ArithUnop (op, L.comap expr_view exp')
    | P.ArithBinop (op, exp1, exp2) ->
      P.ArithBinop (op, L.comap expr_view exp1, L.comap expr_view exp2)
  in

  let rec cond_view = function
    (* Turns a var condition into a var_view condition *)
    | P.Bool b -> P.Bool b
    | P.LogicUnop (op, cond') ->
      P.LogicUnop (op, L.comap cond_view cond')
    | P.LogicBinop (op, cond1, cond2) ->
      P.LogicBinop (op, L.comap cond_view cond1, L.comap cond_view cond2)
    | P.ArithRel (rel, exp1, exp2) ->
      P.ArithRel (rel, L.comap expr_view exp1, L.comap expr_view exp2)
  in

  let filter cond =
    Filter (cond_view cond.L.item)
  in

  let filter_not cond =
    (* Turns a var condition into the negation of its var_view condition *)
    Filter (cond_view (P.LogicUnop (L.mkdummy O.Not, cond)))
  in

  let filter_rel rel i exp =
    (* Builds the Filter for continuing a for loop *)
    Filter (
      cond_view (
        P.ArithRel (
          L.mkdummy rel,
          L.mkloc (P.Var i) i.L.loc,
          exp
        )
      )
    )
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
    ThreadG.add_edge_e acc @@ ThreadG.E.create orig op dest,
    labels,
    offset
  in

  let add_single_vertex (acc, labels, offset) =
    (* Adds a single vertex to the graph *)
    ThreadG.add_vertex acc (succ offset),
    labels,
    (succ offset)
  in

  let add_single_edge op (acc, labels, offset) =
    (* Adds a single vertex to the graph, and a single edge from the
       former last vertex of the graph to this vertex *)
    ThreadG.add_edge_e acc @@ ThreadG.E.create offset op @@ succ offset,
    labels,
    succ offset
  in

  let rec cfg_of_body (acc, labels, offset) { L.item = body; _ } =
    match body with
    (* TODO: Control blocks structures are generating one node more
       than necessary. The building functions should be modified to
       specify the end of the graph to yield, thus making the bodies
       point on the structure head rather than on a new node linked to
       the head by Identity. *)
    | P.Nothing ->
      acc, labels, offset
    | P.Label lbl ->
      acc, Sym.Map.add lbl.L.item offset labels, offset
    | P.Pass ->
      add_single_edge Identity (acc, labels, offset)
    | P.MFence ->
      add_single_edge (MFence thread_id) (acc, labels, offset)
    | P.Assign (x, e) ->
      add_single_edge
        (Assign (thread_id, x.L.item, e.L.item))
        (acc, labels, offset)
    | P.Seq (b1, b2) ->
      cfg_of_body (cfg_of_body (acc, labels, offset) b1) b2
    | P.If (cond, body) ->
      let acc', labels', offset' =
        cfg_of_body (acc, labels, succ offset) body in
      (acc', labels', offset')
      |> add_op_edge (filter cond) offset (succ offset)
      |> add_op_edge (filter_not cond) offset offset'
    | P.While (cond, body) ->
      let acc', labels', offset' =
        cfg_of_body (acc, labels, succ offset) body in
      (acc', labels', offset')
      |> add_single_vertex
      |> add_op_edge Identity offset' offset
      |> add_op_edge (filter cond) offset (succ offset)
      |> add_op_edge (filter_not cond) offset (succ offset')
    | P.For (i, exp_from, exp_to, body) ->
      let acc', labels', offset' =
        cfg_of_body (acc, labels, succ @@ succ offset) body in
      let iplus1 =
        P.ArithBinop (
          L.mkdummy O.Add,
          L.mkdummy @@ P.Var (L.mkdummy i.L.item),
          L.mkdummy @@ P.Int (L.mkdummy 1)
        ) in
      (acc', labels', offset')
      |> add_single_vertex
      |> add_op_edge (Assign (thread_id, i.L.item, exp_from.L.item))
        offset
        (succ offset)
      |> add_op_edge (filter_rel O.Le i exp_to)
        (succ offset)
        (succ @@ succ offset)
      |> add_op_edge (filter_rel O.Gt i exp_to)
        (succ offset)
        (succ offset')
      |> add_op_edge (Assign (thread_id, i.L.item, iplus1))
        offset'
        (succ offset)
  in

  cfg_of_body (
    ThreadG.add_vertex ThreadG.empty Program.Control.Label.initial,
    Sym.Map.empty,
    Program.Control.Label.initial
  ) body

module Oper = Oper.Make (Builder.P (G))

let combine (cfg1, labels1, final1) (cfg2, labels2, final2) =
  (* Combines two CFG.
     cfg1 is the cfg of a single thread,
     cfg2 is the cfg of a program. *)
  let ( ++ ) = Program.Control.State.add_label in
  G.empty
  |> ThreadG.fold_vertex
    (fun i -> Oper.union (G.map_vertex (fun is -> i ++ is) cfg2))
    cfg1
  |> ThreadG.fold_edges_e
    (fun (i, op, i') ->
       G.fold_vertex
         (fun is g -> G.add_edge_e g (G.E.create (i ++ is) op (i' ++ is)))
         cfg2)
    cfg1,
  labels1 :: labels2,
  final1 ++ final2

let cfg_of_program { Program.threads; _ } =
  List.fold_righti
    (fun thread body g -> combine (cfg_of_thread thread body) g)
    threads
    (G.add_vertex G.empty Program.Control.State.empty,
     [],
     Program.Control.State.empty)

let of_program program =
  let graph, labels, final_state = cfg_of_program program in
  { program; graph; labels = Array.of_list labels; final_state }
