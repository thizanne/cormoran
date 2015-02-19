open Batteries
open Util
module S = Syntax
module T = Syntax.Typed
open Syntax.TypedProgram
open Printf

module V =
struct
  type t = Syntax.position
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module E =
struct
  type t = {
    thread : int;
    ins : S.Typed.t;
  }
  let compare = Pervasives.compare
  let default = {
    thread = -1;
    ins = S.Typed.Pass;
  }
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

let add_ins_edges lbls g pos thread ins =
  (* Adds in g the edges corresponding to the execution in position
     pos of the instruction ins by the t-th thread, given the positions of
     the labels of this thread *)
  let open Syntax in
  let open Syntax.Typed in
  let open E in
  match ins with
  | Pass
  | Read _
  | Write _
  | RegOp _
  | Cmp _
  | MFence
  | Label _ ->
    let succ_pos = incr_nth thread pos in
    let edge = G.E.create pos {thread; ins} succ_pos in
    G.add_edge_e g edge
  | Jmp lbl ->
    let succ_pos =
      set_nth thread (List.assoc lbl.item lbls) pos in
    let edge = G.E.create pos {thread; ins} succ_pos in
    G.add_edge_e g edge
  | Jz (r, lbl)
  | Jnz (r, lbl) ->
    let succ_pos_1 = incr_nth thread pos in
    let succ_pos_2 =
      set_nth thread (List.assoc lbl.item lbls) pos in
    let edge_1 = G.E.create pos {thread; ins = dual_jump ins} succ_pos_1 in
    let edge_2 = G.E.create pos {thread; ins} succ_pos_2 in
    G.add_edge_e (G.add_edge_e g edge_1) edge_2

let add_pos_edges lbls g pos prog =
  (* Adds in the not-yet-complete CFG g of the program prog all the
     edges whose origin is vertex labelled with the position pos *)
  List.fold_lefti
    (fun g i p ->
       try
         add_ins_edges lbls.(i) g pos i
           (nth_ins prog i p).S.item
       with
       (* We already are at the end of a thread *)
         Invalid_argument "index out of bounds" -> g)
    g pos

let init prog =
  (* Builds the graph containing the vertices of the CFG of prog, with
     no edge *)

  (* TODO: append ... [|ins.(0)|] is used to add a final
     position. This works but is ugly. *)
  Array.fold_right
    (fun thread pos ->
       Array.mapi
         (fun i _ ->
            List.map (fun t -> i :: t) pos)
         (Array.append thread.ins [|thread.ins.(0)|])
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

let edge_label {E.thread; ins} =
  let open Syntax.Typed in
  let open Syntax in

  let str_value v =
    match v with
    | Int x -> string_of_int x.item
    | Var r -> r.item
  in

  let rec str_exp e =
    match e with
    | Val v -> str_value v.item
    | Op (op, e1, e2) ->
      sprintf "%s %c %s" (str_exp e1.item) op.item (str_exp e2.item)
  in

  sprintf "%d:%s" thread
    begin match ins with
      | Pass -> "<I>id</I>"
      | Label lbl -> "<I>id</I>"
      | MFence -> "MFence"
      | Jmp _ -> ""
      | Jnz (r, _) ->
        sprintf "%s ≠ 0" r.item
      | Jz (r, _) ->
        sprintf "%s = 0" r.item
      | Read (r, x) ->
        sprintf "%s ← %s" r.item x.item
      | Write (x, v) ->
        sprintf "%s ← %s" x.item (str_value v.item)
      | RegOp (r, e) ->
        sprintf "%s ← %s" r.item (str_exp e.item)
      | Cmp (r, v1, v2) ->
        sprintf "%s ← %s <?> %s" r.item (str_value v1.item) (str_value v2.item)
    end

module Dot (R : Analysis.Result) = Graph.Graphviz.Dot (
  struct
    include G

    let vertex_attributes v =
      let label v =
        let out = IO.output_string () in
        let () = R.Domain.print out @@ R.data v in
        IO.close_out out
        |> String.replace_chars
          (function
            | '\n' -> "<BR/>"
            | '>' -> "&gt;"
            | '<' -> "&lt;"
            | c -> String.make 1 c)
      in [
        `Shape `Box;
        `Fillcolor 0xdddddd;
        `Style `Filled;
        `Style `Rounded;
        `HtmlLabel (
          sprintf "
<TABLE BORDER=\"0\" ALIGN=\"CENTER\">
<TR><TD BORDER=\"0\">%s</TD></TR>
<TR><TD BORDER=\"1\" BGCOLOR=\"white\">
<FONT POINT-SIZE=\"12\">%s</FONT>
</TD>
</TR>
</TABLE>"
            (string_of_pos v)
            (label v)
        )
      ]

    let edge_attributes (_, e, _) = [
      `HtmlLabel (edge_label e);
      `Fontsize 12;
      `Arrowsize 0.5;
    ]

    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_name v =
      "\"" ^ string_of_pos v ^ "\""
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)
