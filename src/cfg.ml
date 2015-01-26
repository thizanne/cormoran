open Batteries
open Util
module S = Syntax
module T = Syntax.Typed
open Syntax.TypedProgram

module V =
struct
  type t = int list
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module E =
struct
  type t = S.Typed.t
  let compare = Pervasives.compare
  let default = S.Typed.Pass
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (V) (E)

let label_positions { threads; _ } =
  (* Return the list of lists of (label, position) for each thread *)
  let open Syntax in
  let lbl_pos_t =
    Array.fold_lefti
      (fun acc i ins ->
         match ins.item with
         | T.Label { item = s } ->
           (s, i) :: acc
         | _ -> acc)
      []
  in Array.map (fun t -> lbl_pos_t t.ins) threads

let dual_jump = function
  | T.Jz (r, lbl) -> T.Jnz (r, lbl)
  | T.Jnz (r, lbl) -> T.Jz (r, lbl)
  | _ -> failwith "dual_jump"

let add_ins_edges lbls g pos t ins =
  (* Adds in g the edges corresponding to the execution in position
     pos of the instruction ins by the t-th thread, given the positions of
     the labels of this thread *)
  let open Syntax in
  let open Syntax.Typed in
  match ins with
  | Pass
  | Read _
  | Write _
  | RegOp _
  | Cmp _
  | MFence
  | Label _ ->
    let succ_pos = incr_nth t pos in
    let edge = G.E.create pos ins succ_pos in
    G.add_edge_e g edge
  | Jmp lbl ->
    let succ_pos =
      set_nth t (List.assoc lbl.item lbls) pos in
    let edge = G.E.create pos ins succ_pos in
    G.add_edge_e g edge
  | Jz (r, lbl)
  | Jnz (r, lbl) ->
    let succ_pos_1 = incr_nth t pos in
    let succ_pos_2 =
      set_nth t (List.assoc lbl.item lbls) pos in
    let edge_1 = G.E.create pos (dual_jump ins) succ_pos_1 in
    let edge_2 = G.E.create pos ins succ_pos_2 in
    G.add_edge_e (G.add_edge_e g edge_1) edge_2

let add_pos_edges lbls g pos prog =
  (* Adds in the not-yet-complete CFG g of the program prog all the
     edges whose origin is vertex labelled with the position pos *)
  List.fold_lefti
    (fun g i p ->
       add_ins_edges lbls.(i) g pos i
         (nth_ins prog i p).S.item)
    g pos

let init prog =
  (* Builds the graph containing the vertices of the CFG of prog, with
     no edge *)
  Array.fold_right
    (fun thread pos ->
       Array.mapi
         (fun i _ -> List.map (fun t -> i :: t) pos)
         thread.ins
       |> Array.fold_left ( @ ) [])
    prog.threads [[]]
  |> List.fold_left G.add_vertex G.empty

let make prog =
  (* Builds the CFG of prog *)
  (* TODO This could be efficiently regrouped with init to treat
     positions in one pass *)
  let g = init prog in
  let lbls = label_positions prog in
  G.fold_vertex
    (fun pos g ->
       add_pos_edges lbls g pos prog)
    g g

module Dot = Graph.Graphviz.Dot (
  struct
    include G
    let edge_attributes (a, e, b) = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = [`Shape `Box]
    let vertex_name = string_of_int_list
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)
