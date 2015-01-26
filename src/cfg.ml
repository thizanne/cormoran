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
  let open Syntax in
  let rec aux acc i = function
    | [] -> acc
    | {item = T.Label { item = s }} :: xs ->
      aux ((s, i) :: acc) (succ i) xs
    | _ :: xs -> aux acc (succ i) xs
  in List.map (fun t -> aux [] 0 t.ins) threads

let dual_jump = function
  | T.Jz (r, lbl) -> T.Jnz (r, lbl)
  | T.Jnz (r, lbl) -> T.Jz (r, lbl)
  | _ -> failwith "dual_jump"

let connect_vertex lbls g pos t ins =
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

let init prog =

  let rec all_pos = function
    | [] -> [[]]
    | thread :: threads ->
      let pos = all_pos threads in
      List.mapi
        (fun k _ -> List.map (fun t -> k :: t) pos)
        thread.ins
      |> List.flatten
  in

  all_pos prog.threads
  |> List.fold_left G.add_vertex G.empty
