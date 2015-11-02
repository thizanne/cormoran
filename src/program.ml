open Batteries

type thread_id = int

type var_type =
  | Local
  | Shared

type var = {
  var_type : var_type;
  var_name : Symbol.t;
}

let print_var output x =
  Symbol.print output x.var_name

type var_view = {
  thread_id : thread_id;
  var : var [@main];
} [@@deriving create]

let print_var_view output view =
  Printf.fprintf output "%d:%a"
    view.thread_id
    print_var view.var

let local_var var_name = {
  var_name;
  var_type = Local;
}

let shared_var var_name = {
  var_name;
  var_type = Shared;
}

let is_local v =
  v.var_type = Local

let is_shared v =
  v.var_type = Shared

type 'a expression =
  | Int of int Location.loc
  | Var of 'a Location.loc
  | ArithUnop of
      Operators.arith_one Location.loc *
      'a expression Location.loc
  | ArithBinop of
      Operators.arith_two Location.loc *
      'a expression Location.loc *
      'a expression Location.loc

let var v = Var v

let rec predicate_var_in_expr f = function
  | Int _ -> []
  | Var v ->
    if f v.Location.item
    then [v.Location.item.var_name]
    else []
  | ArithUnop (_, e) ->
    predicate_var_in_expr f e.Location.item
  | ArithBinop (_, e1, e2) ->
    predicate_var_in_expr f e1.Location.item @
    predicate_var_in_expr f e2.Location.item

let shared_in_expr = predicate_var_in_expr is_shared
let local_in_expr = predicate_var_in_expr is_local

type 'a condition =
  | Bool of bool Location.loc
  | LogicUnop of
      Operators.logic_one Location.loc *
      'a condition Location.loc
  | LogicBinop of
      Operators.logic_two Location.loc *
      'a condition Location.loc *
      'a condition Location.loc
  | ArithRel of
      Operators.logic_arith_two Location.loc *
      'a expression Location.loc *
      'a expression Location.loc

type 'a body =
  | Nothing
  | Pass
  | MFence
  | Label of Symbol.t Location.loc
  | Seq of
      'a body Location.loc *
      'a body Location.loc
  | Assign of
      'a Location.loc *
      'a expression Location.loc
  | If of
      'a condition Location.loc * (* Condition *)
      'a body Location.loc (* Body *)
  | While of
      'a condition Location.loc * (* Condition *)
      'a body Location.loc (* Body *)
  | For of
      'a Location.loc * (* Indice *)
      'a expression Location.loc * (* From *)
      'a expression Location.loc * (* To *)
      'a body Location.loc (* Body *)

let seq body1 body2 =
  Location.mk (Seq (body1, body2))
    Location.(body1.loc.startpos)
    Location.(body2.loc.endpos)

type 'a thread = {
  locals : Symbol.Set.t;
  body : 'a body Location.loc;
}

type 'a t = {
  initial : int Symbol.Map.t;
  threads : 'a thread list;
}

module Control : sig
  type 'a program = 'a t

  module Label : sig
    type t
    val initial : t
    val succ : t -> t
    val enum : initial:t -> final:t -> t Enum.t
    val compare : t -> t -> int
    val print : 'a IO.output -> t -> unit
  end

  module State : sig
    type t
    val empty : t
    val add_label : Label.t -> t -> t
    val from_label_list : Label.t list -> t
    val tid_label : t -> thread_id -> Label.t
    val is_initial : t -> bool
    val initial : _ program -> t
    val compare : t -> t -> int
    val print : 'a IO.output -> t -> unit
  end
end
=
struct
  type 'a program = 'a t

  module Label = struct
    include Int

    let initial = 0

    let enum ~initial ~final =
      initial -- final
  end

  module State = struct
    type t = Label.t list

    let empty = []

    let tid_label = List.nth

    let add_label = List.cons

    let from_label_list labels = labels

    let compare = List.compare Label.compare

    let is_initial =
      List.for_all (( = ) 0)

    let initial program =
      List.map (fun _ -> 0) program.threads

    let print output =
      List.print Int.print ~first:"" ~last:"" output
  end

end
