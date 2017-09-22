(* FIXME: factorise this with markThread and markThreadNoLocal *)
open Batteries

module T = TypedAst
module Ty = Types
module O = Operation
module L = Location
module Dom = Domain

module ExistVar = struct
  type t = Pack : _ T.program_var -> t
  let compare = Pervasives.compare
end

module VarSet = struct
  include Set.Make (ExistVar)

  let single_var var =
    singleton (ExistVar.Pack var)

  let add_var var set =
    add (ExistVar.Pack var) set

  let of_expr expr =
    T.fold_expr
      { T.fold =
          fun var acc ->
            if T.is_shared var
            then add_var var acc
            else acc }
      empty expr
end

module VarMap = struct
  include Map.Make (ExistVar)

  let single_var_binding var =
    singleton (ExistVar.Pack var)

  let find_var var =
    find (ExistVar.Pack var)

  let union_join join =
    (* Computes the union of two maps. [join] is used when a binding
       is present in both maps. *)
    merge
      (fun _key x1 x2 -> match (x1, x2) with
         | None, x
         | x, None -> x
         | Some x1, Some x2 -> Some (join x1 x2))
end

module Key = struct
  (* TODO: remove this when 4.03 is used. ppx_deriving uses
     [@@ocaml.warning "-A"], which is needed to remove warning 39 and
     not supported in 4.02. *)
  [@@@ocaml.warning "-39"]

  type presence =
    | Zero
    | One
    | MoreThanOne
  [@@ deriving ord]

  let up_presence = function
    | Zero -> One
    | One -> MoreThanOne
    | MoreThanOne -> MoreThanOne

  let down_presence = function
    | Zero -> raise @@ Invalid_argument "down_presence"
    | One -> Zero
    | MoreThanOne -> One

  let presence_is_zero = function
    | Zero -> true
    | One
    | MoreThanOne -> false

  let print_presence output = function
    | Zero -> String.print output "0"
    | One -> String.print output "1"
    | MoreThanOne -> String.print output ">1"

  module M = Sym.Map

  type t = presence M.t

  let compare =
    Sym.Map.compare compare_presence

  let init prog =
    Sym.Map.map
      (fun _init -> Zero)
      prog.T.globals

  let is_consistent =
    M.for_all (fun _sym -> presence_is_zero)

  let get_presence var_sym =
    M.find var_sym

  let down var =
    M.modify var down_presence

  let up var =
    M.modify var up_presence

  let print output =
    M.print ~first:"" ~last:"" ~sep:";" ~kvsep:"→"
      Sym.print print_presence
      output
end

module Make (Inner : Domain.Inner) (C : Domain.ControlAbstraction) : sig
  module StateAbstraction : Domain.ThreadState
  module Interferences : Domain.Interferences
  module Application : Modular.Application
    with type state = StateAbstraction.t
     and type interference = Interferences.t
end
=

struct

  module Inner = Domain.LiftEnvUtils (Inner)

  (* Common utility functions *)

  let inner_var_sym = Sym.namespace ()

  let sym_local tid reg =
    inner_var_sym @@ Printf.sprintf "%d:%s" tid (Sym.name reg)

  let sym_mem var =
    inner_var_sym @@ Printf.sprintf "%s:mem" (Sym.name var)

  let sym_mem_prime var =
    inner_var_sym @@ Printf.sprintf "%s:mem'" (Sym.name var)

  let sym_local_prime tid var =
    inner_var_sym @@ Printf.sprintf "%d:%s'" tid (Sym.name var)

  let sym_top tid var =
    inner_var_sym @@ Printf.sprintf "%s:top:%d" (Sym.name var) tid

  let sym_bot tid var =
    inner_var_sym @@ Printf.sprintf "%s:bot:%d" (Sym.name var) tid

  let sym_label tid =
    inner_var_sym @@ Printf.sprintf "Label::%d" tid

  let sym_label_prime tid =
    inner_var_sym @@ Printf.sprintf "Label::%d'" tid

  let inner_var make_sym var =
    { var with T.var_spec = make_sym var.T.var_sym }

  let inner_var_local tid = inner_var (sym_local tid)

  let inner_var_mem x = inner_var sym_mem x

  let inner_var_mem_prime x = inner_var sym_mem_prime x

  let inner_var_local_prime tid x = inner_var (sym_local_prime tid) x

  let inner_var_top tid = inner_var (sym_top tid)

  let inner_var_bot tid = inner_var (sym_bot tid)

  (* TODO: make Sym able to generate fresh names *)
  let inner_var_tmp var =
    { var with T.var_spec = inner_var_sym "::Mark:tmp::" }

  let inner_of_property key =
    (* Mapper which puts as var_spec the inner symbol of a property
       variable. *)
    let map { T.var_sym; var_type; var_spec } =
      let inner_sym = match var_spec with
        | Source.Local thread_id -> sym_local thread_id var_sym
        | Source.Memory -> sym_mem var_sym
        | Source.View thread_id ->
          match Key.get_presence var_sym key with
          | Key.Zero -> sym_mem var_sym
          | Key.One
          | Key.MoreThanOne -> sym_top thread_id var_sym
      in { T.var_sym; var_type; var_spec = inner_sym }
    in { T.map }

  let inner_of_program thread_id key =
    (* Mapper which puts as var_spec the inner symbol of a program
       variable. *)
    let map { T.var_sym; var_type; var_spec } =
      let inner_sym = match var_spec with
        | Ty.Local -> sym_local thread_id var_sym
        | Ty.Shared ->
          match Key.get_presence var_sym key with
          | Key.Zero -> sym_mem var_sym
          | Key.One
          | Key.MoreThanOne -> sym_top thread_id var_sym
      in { T.var_sym; var_type; var_spec = inner_sym }
    in { T.map }

  let initial_varset origin var_map =
    Sym.Map.fold
      (fun var_sym ty varset_acc ->
         match ty with
         | Env.Int ->
           VarSet.add_var
             { T.var_sym; var_type = Ty.Int; var_spec = origin }
             varset_acc
         | Env.Bool ->
           VarSet.add_var
             { T.var_sym; var_type = Ty.Bool; var_spec = origin }
             varset_acc)
      var_map
      VarSet.empty

  let env_add_mem varset env =
    VarSet.fold
      (fun (ExistVar.Pack var) env_acc ->
          Inner.Env.add (inner_var_mem var) env_acc)
      varset env

  let env_add_memvar_prime var env =
    Inner.Env.add (inner_var_mem_prime var) env

  let env_add_thread_locals tid varset env =
    VarSet.fold
      (fun (ExistVar.Pack var) env_acc ->
         Inner.Env.add (inner_var_local tid var) env_acc)
      varset env

  let env_add_thread_locals_prime tid varset env =
    VarSet.fold
      (fun (ExistVar.Pack var) env_acc ->
         Inner.Env.add (inner_var_local_prime tid var) env_acc)
      varset env

  let env_add_label tid max env =
    Inner.Env.add_label (sym_label tid) max env

  let env_add_label_prime tid max env =
    Inner.Env.add_label (sym_label_prime tid) max env

  let initial_shared prog =
    initial_varset Ty.Shared prog.T.globals

  let initial_locals prog =
    List.map (fun t -> initial_varset Ty.Local t.T.locals) prog.T.threads

  let max_labels control =
    List.of_enum @@
    Enum.map
      (C.max_alpha control)
      (0 --^ C.nb_threads control)

  let initial_env shared locals max_labels =
    Inner.Env.empty
    |> env_add_mem shared
    |> List.fold_righti env_add_thread_locals locals
    |> List.fold_righti env_add_label max_labels

  let initial_inner_top shared locals max_labels =
    Inner.change_env
      (initial_env shared locals max_labels)
      Inner.top

  let initial_inner_bot shared locals max_labels =
    Inner.change_env
      (initial_env shared locals max_labels)
      Inner.bottom

  (* let extend_inner_mem var inner = *)
  (*   Inner.add (inner_var_mem_prime var) inner *)

  (* let extend_inner_thread_locals tid local_varset inner = *)
  (*   VarSet.fold *)
  (*     (fun (ExistVar.Pack var) inner_acc -> *)
  (*        Inner.add (inner_var_local_prime tid var) inner_acc) *)
  (*     local_varset *)
  (*     inner *)

  (* let extend_inner_labels control inner = *)
  (*   Enum.fold *)
  (*     (fun inner_acc tid  -> *)
  (*        Inner.add_label *)
  (*          (sym_label_prime tid) *)
  (*          (C.max_alpha control tid) *)
  (*          inner_acc) *)
  (*     inner *)
  (*     (0 --^ C.nb_threads control) *)

  let img_inner_mem var inner =
    inner
    |> Inner.drop (inner_var_mem var)
    |> Inner.rename (inner_var_mem_prime var) (inner_var_mem var)

  let img_inner_locals tid varset_locals inner =
    inner
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.drop (inner_var_local tid var) inner_acc)
      varset_locals
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.rename
           (inner_var_local_prime tid var)
           (inner_var_local tid var)
           inner_acc)
      varset_locals

  let img_inner_labels control inner =
    Enum.fold
      (fun inner_acc tid ->
         inner_acc
         |> Inner.drop_label @@ sym_label tid
         |> Inner.rename_label (sym_label_prime tid) (sym_label tid))
      inner
      (0 --^ C.nb_threads control)

  module StateAbstraction = struct

    module M = Map.Make (Key)

    type t = {
      thread_id : Source.thread_id;
      control : C.t; (* Control abstraction of the program *)
      shared : VarSet.t; (* shared variables defined in the abstraction *)
      locals : VarSet.t list; (* local variables defined in the abstraction *)
      abstr : Inner.t M.t
    }

    let normalize =
      (* TODO: check normalize - Bddapron.Bdddomain1 exports it, not
         mtbdd. Maybe this could be a Inner function, and maybe it
         solves the bug with bddapron newer versions. *)
      M.filterv (fun inner -> not (Inner.is_bottom inner))

    let is_bottom {abstr; _ } =
      M.for_all (fun _key inner -> Inner.is_bottom inner) abstr

    let equal_abstr abstr1 abstr2 =
      abstr1 == abstr2 ||
      M.equal Inner.equal (normalize abstr1) (normalize abstr2)

    let equal x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      equal_abstr x1.abstr x2.abstr

    let print output { abstr; _ } =
      M.print
        ~first:"" ~last:"" ~kvsep:":\n\n" ~sep:"\n────────\n"
        Key.print Inner.print output abstr

    let add_join bufs inner abstr =
      (* Adds (bufs, inner) as a abstr element, making a join if the
         bufs key is already present *)
      M.modify_def inner bufs (Inner.join inner) abstr

    let bottom prog thread_structs thread_id =
      {
        thread_id;
        control = C.of_threads thread_structs;
        shared = initial_varset Ty.Shared prog.T.globals;
        locals = List.map (fun t -> initial_varset Ty.Local t.T.locals) prog.T.threads;
        abstr = M.empty;
      }

    let top prog thread_structs thread_id =
      let control = C.of_threads thread_structs in
      let shared = initial_shared prog in
      let locals = initial_locals prog in
      let max_labels = max_labels control in
      let initial_inner =
        initial_inner_top shared locals max_labels
        |> Inner.drop_label (sym_label thread_id)
      in
      {
        thread_id;
        shared;
        locals;
        control;
        abstr = M.singleton (Key.init prog) initial_inner;
      }

    let join_partitions abstr =
      (* Join all the partitions of abstr into a single inner
         domain *)
      M.Labels.fold
        ~f:(fun ~key:_ ~data acc -> Inner.join_lce data acc)
        ~init:Inner.bottom
        abstr

    let are_consistent abstrs =
      let unified_abstr =
        List.fold_left
          (fun acc abstr ->
             let abstr = M.map (Inner.unify acc) abstr.abstr in
             join_partitions abstr)
          Inner.top
          abstrs
      in not (Inner.is_bottom unified_abstr)

    let meet_unsymbolised_cond symbolise cond =
      M.mapi
        (fun key -> Inner.meet_cons @@ T.map_expr (symbolise key) cond)

    let local_assign tid r expr key inner =
      Inner.assign_expr
        (inner_var_local tid r)
        (T.map_expr (inner_of_program tid key) expr)
        inner

    let local_assign_prime tid r expr key inner =
      Inner.assign_expr
        (inner_var_local_prime tid r)
        (T.map_expr (inner_of_program tid key) expr)
        inner

    let write tid x expr key inner =
      let x_sym = x.T.var_sym in
      let x_top = inner_var_top tid x in
      let x_bot = inner_var_bot tid x in
      let inner_expr = T.map_expr (inner_of_program tid key) expr in
      match Key.get_presence x.T.var_sym key with
      | Key.Zero ->
        Key.up x_sym key,
        (* x_top := add e *)
        inner
        |> Inner.add x_top
        |> Inner.assign_expr x_top inner_expr
      | Key.One ->
        Key.up x_sym key,
        (* x_bot :=add x_top; x_top := e *)
        inner
        |> Inner.add x_bot
        |> Inner.assign_expr x_bot (T.Var (L.mkdummy x_top))
        |> Inner.assign_expr x_top inner_expr
      | Key.MoreThanOne ->
        (* cf Numeric Domains with Summarized Dimensions, Gopan et al. Tacas04 *)
        let x_tmp = inner_var_tmp x in
        key,
        (* x_bot[*] := x_top; x_top := e *)
        inner
        |> Inner.add x_tmp
        |> Inner.assign_expr x_tmp (T.Var (L.mkdummy x_top))
        |> Inner.fold x_bot x_tmp
        |> Inner.assign_expr x_top inner_expr

    let meet_cond cond x =
      (* normalisation is not needed since only a is_bottom will be done
         on the result *)
      {
        x with
        abstr =
          normalize @@
          meet_unsymbolised_cond inner_of_property cond x.abstr
      }

    let meet_label label_tid label x =
      if label_tid = x.thread_id
      (* information on tid label is not present in abstr variables, but at
         an outer level *)
      then x
      else {
        x with
        abstr =
          normalize @@
          M.map
            (fun inner ->
               let abstract_label = C.alpha x.control label_tid label in
               Inner.meet_label (sym_label label_tid) abstract_label inner)
            x.abstr
      }

    let join x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      {
        x1 with
        abstr =
          M.merge
            (fun _bufs inner1 inner2 -> match (inner1, inner2) with
               | None, _ -> inner2
               | _, None -> inner1
               | Some inner1, Some inner2 -> Some (Inner.join inner1 inner2))
            x1.abstr x2.abstr
      }

    let widening x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      {
        x1 with
        abstr =
          M.merge
            (fun _key abstr1 abstr2 -> match (abstr1, abstr2) with
               | None, _ -> abstr2
               | _, None -> abstr1
               | Some inner1, Some inner2 ->
                 Some (Inner.widening inner1 inner2))
            x1.abstr x2.abstr
      }
  end

  module Interferences = struct
    type t = Inner.t * Inner.t VarMap.t
    (* We partition the interferences acording to the shared variable
       they modify. No concrete interference should modify two shared
       variables so the partitioning should cause no issue. *)

    let local_intf_final_env shared locals control =
      (* The final environment for a local interference. Contains the
         original initial variables, plus the primed local variables
         and labels. *)
      let max_labels = max_labels control in
      initial_env shared locals max_labels
      |> List.fold_righti env_add_label_prime max_labels
      |> List.fold_righti env_add_thread_locals_prime locals

    let shared_intf_final_env var shared locals control =
      (* The final environment for a shared interference. Contains the
         original initial variables, plus the primed shared variable
         and labels. *)
      let max_labels = max_labels control in
      initial_env shared locals max_labels
      |> env_add_memvar_prime var
      |> List.fold_righti env_add_label_prime max_labels

    let inner_bot_noprime shared locals control =
      initial_inner_bot
        shared
        locals
        (max_labels control)

    let inner_bot_local shared locals control =
      inner_bot_noprime shared locals control
      |> Inner.change_env (local_intf_final_env shared locals control)

    let inner_bot_shared var shared locals control =
      inner_bot_noprime shared locals control
      |> Inner.change_env (shared_intf_final_env var shared locals control)

    let bottom prog thread_structs =
      inner_bot_local
        (initial_shared prog)
        (initial_locals prog)
        (C.of_threads thread_structs),
      VarMap.empty

    let set_shared_identity var inner =
      (* Sets the prime version of every variable in the shared varset
         to be equal to the non-primed version *)
           Inner.assign_expr
             (inner_var_mem_prime var)
             (T.Var (L.mkdummy @@ inner_var_mem var))
             inner

    let set_local_identity thread_id local inner =
      (* Sets the prime version of every variable in the locals varset
         to be equal to the non-primed version *)
      VarSet.fold
        (fun (ExistVar.Pack v) inner_acc ->
           Inner.assign_expr
             (inner_var_local_prime thread_id v)
             (T.Var (L.mkdummy @@ inner_var_local thread_id v))
             inner_acc)
        local
        inner

    let set_label_identity thread_id inner =
      Inner.assign_label
        (sym_label_prime thread_id)
        (sym_label thread_id)
        inner

    let local_intf_id shared locals control =
      (* Inner part of the identity interference *)
      initial_inner_top
        shared
        locals
        (max_labels control)
      |> Inner.change_env (local_intf_final_env shared locals control)
      |> List.fold_righti set_local_identity locals
      |> Seq.(fold_right set_label_identity @@ 0 --^ C.nb_threads control)

    let shared_intf_id var shared locals control =
      (* Inner part of the identity interference *)
      initial_inner_top
        shared
        locals
        (max_labels control)
      |> Inner.change_env (shared_intf_final_env var shared locals control)
      |> set_shared_identity var
      |> Seq.(fold_right set_label_identity @@ 0 --^ C.nb_threads control)

    let join (inner1, map1) (inner2, map2) =
      Inner.join inner1 inner2,
      VarMap.merge
        (fun _key elem1 elem2 ->
           match (elem1, elem2) with
           | None, Some abstr
           | Some abstr, None -> Some abstr
           | Some abstr1, Some abstr2 -> Some (Inner.join abstr1 abstr2)
           | None, None -> assert false)
        map1 map2

    let equal (inner1, map1) (inner2, map2) =
      Inner.equal inner1 inner2 &&
      VarMap.equal Inner.equal map1 map2

    let widening (inner1, map1) (inner2, map2) =
      Inner.widening inner1 inner2,
      VarMap.merge
        (fun _key elem1 elem2 -> match (elem1, elem2) with
           | None, _ -> elem2
           | _, None -> elem1
           | Some abstract1, Some abstract2 ->
             Some (Inner.widening abstract1 abstract2))
        map1 map2

    let print output (inner, map) =
      Printf.fprintf output "%a\n%a"
        Inner.print inner
        (VarMap.print ~first:"{\n" ~last:"\n}" ~sep:"\n\n" ~kvsep:":\n"
           (fun output (ExistVar.Pack v) -> Sym.print output v.T.var_sym)
           Inner.print)
        map
  end

  module Application = struct
    type state = StateAbstraction.t
    type interference = Interferences.t

    let inner_intf_from_inner intf_id lbl1 lbl2 tid control effect inner =
      (* Generates the inner interference from a given inner abstract
         state, an effect ("no effect" is the identity), and the
         source and destination labels. intf_id is the identity
         interference and its environment is used as the final
         environment. *)
      intf_id
      |> Inner.unify inner
      |> Inner.set_label (sym_label tid) (C.alpha control tid lbl1)
      |> Inner.set_label (sym_label_prime tid) (C.alpha control tid lbl2)
      |> effect
      |> Inner.change_env (Inner.get_env intf_id)

    let inner_intf_from_partitions
        intf_id
        lbl1 lbl2
        thread_id shared locals
        control
        abstr
        effect
      =
      (* Generates the inner interference from each partition of the
         abstract state and makes the join. *)
      StateAbstraction.M.Labels.fold
        ~f:(fun ~key ~data:inner inner_acc ->
            inner_intf_from_inner
              intf_id
              lbl1 lbl2
              thread_id
              control
              (effect key)
              inner
            |> Inner.join inner_acc)
        ~init:(
          Interferences.inner_bot_noprime shared locals control
          |> Inner.change_env (Inner.get_env intf_id)
        )
        abstr

    let interference_label_only
        lbl1 lbl2
        thread_id shared locals
        control abstr =
      (* Generates a full interference that only changes the labels *)
      inner_intf_from_partitions
        (Interferences.local_intf_id shared locals control)
        lbl1 lbl2
        thread_id shared locals
        control abstr
        (fun _key inner -> inner)
      ,
      VarMap.empty

    let add_flush_x lbl tid shared locals control x key inner state =
      (* Adds to acc the result(s) of one flush of x from tid. If tid
         has no entry for x in its buffer, acc is returned
         unchanged. Also returns the interferences generated by this
         flush. *)
      let x_sym = x.T.var_sym in
      let x_mem = inner_var_mem x in
      let x_mem_prime = inner_var_mem_prime x in
      let x_top = inner_var_top tid x in
      let x_bot = inner_var_bot tid x in
      let x_bot_expr = T.Var (L.mkdummy x_bot) in
      let x_top_expr = T.Var (L.mkdummy x_top) in
      let x_tmp = inner_var_tmp x in
      let x_tmp_expr = T.Var (L.mkdummy x_tmp) in
      let intf_bot = Interferences.inner_bot_shared x shared locals control in
      let intf_id =
        Interferences.shared_intf_id x shared locals control in
      match Key.get_presence x_sym key with
      | Key.Zero -> state, intf_bot
      | Key.One ->
        let key' = Key.down x_sym key in
        let inner' =
          inner
          |> Inner.assign_expr x_mem x_top_expr
          |> Inner.drop x_top in
        let inner_intf =
          inner_intf_from_inner
            intf_id
            lbl lbl
            tid control
            (Inner.assign_expr x_mem_prime x_top_expr)
            inner
        in
        StateAbstraction.add_join key' inner' state,
        inner_intf
      | Key.MoreThanOne ->
        (* >1 -> >1 => x_mem := x_bot[*] *)
        let effect_gt1 x_dst =
          Inner.expand x_bot x_tmp
          %> Inner.assign_expr x_dst x_tmp_expr
          %> Inner.drop x_tmp in
        let inner_gt1 =
          inner
          |> effect_gt1 x_mem in
        let intf_gt1 =
          inner_intf_from_inner
            intf_id
            lbl lbl
            tid control
            (effect_gt1 x_mem_prime)
            inner in
        (* >1 -> 1 => x_mem := x_bot[*]; del x_bot *)
        let key_1 = Key.down x_sym key in
        let inner_1 =
          (* x_tmp is not needed here *)
          inner
          |> Inner.assign_expr x_mem x_bot_expr
          |> Inner.drop x_bot in
        let intf_1 =
          inner_intf_from_inner
            intf_id
            lbl lbl
            tid control
            (Inner.assign_expr x_mem_prime x_bot_expr)
            inner in
        (* Make the joins *)
        state
        |> StateAbstraction.add_join key inner_gt1
        |> StateAbstraction.add_join key_1 inner_1,
        Inner.join intf_1 intf_gt1

    let iterate_one_flush lbl tid shared locals control x abstr =
      (* Does one iteration of flushing one x from each point of abstr
         if possible, adding the results to abstr, returning the
         generated interferences. *)
      StateAbstraction.M.Labels.fold
        ~f:(fun ~key ~data (acc_abstr, acc_intf) ->
            let completed_abstr, new_intf =
              add_flush_x lbl tid shared locals control x key data acc_abstr in
            completed_abstr,
            Inner.join acc_intf new_intf)
        ~init:(abstr, Interferences.inner_bot_shared x shared locals control)
        abstr

    let close_by_flush_wrt_var lbl tid shared locals control x abstr =
      let intf_inner_bot = Interferences.inner_bot_shared x shared locals control in
      let rec close acc_abstr acc_intf =
        let flushed_abstr, flush_intf =
          iterate_one_flush lbl tid shared locals control x acc_abstr in
        let joined_intf = Inner.join acc_intf flush_intf in
        if StateAbstraction.equal_abstr acc_abstr flushed_abstr
        (* FIXME: do we need to return joined_intf or acc_intf ?
           joined_intf is definitely sound but maybe not precise
           enough. *)
        then flushed_abstr, joined_intf
        else close flushed_abstr joined_intf
      in
      match x.T.var_spec with
      | Ty.Local -> abstr, intf_inner_bot
      | Ty.Shared -> close abstr intf_inner_bot

    let apply_inner img tid control inner_intf inner =
      (* inner_intf is expected to contain only interferences
         appliable at the relevant control label of inner. img
         should remove the right unprimed variables and rename the
         corresponding primed variables. It should be either
         [img_inner_mem a_shared_var] or [List.fold_righti
         img_inner_locals varset_locals]. *)
      inner
      |> Inner.unify inner_intf
      |> img_inner_labels control
      |> img
      |> Inner.drop_label (sym_label tid)

    let apply_local_intf lbl
        { StateAbstraction.thread_id; shared; locals; control; abstr }
        inner_intf =
      (* FIXME: easy factorisation with apply_singlevar_intf *)
      let inner_intf =
        Inner.meet_label
          (sym_label thread_id)
          (C.alpha control thread_id lbl)
          inner_intf in
      let img_local = List.fold_righti img_inner_locals locals in
      let abstr' =
        StateAbstraction.M.map
          (apply_inner img_local thread_id control inner_intf)
          abstr in
      { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' }

    let apply_singlevar_intf lbl
        { StateAbstraction.thread_id; shared; locals; control; abstr }
        modified_var inner_intf =
      (* We know the interferences cannot change the partitioning, so
         it's sound to apply them partition-by-partition. Then a flush
         closure must be computed for soundness, since interference
         application does not commute with flush closure.

         This flush closure will itself generate interferences, but
         they should not be taken into account since interferences
         generated by a thread do not affect the analysis of this
         thread. *)
      let inner_intf =
        Inner.meet_label
          (sym_label thread_id)
          (C.alpha control thread_id lbl)
          inner_intf in
      let img_shared = img_inner_mem modified_var in
      let abstr', generated_inner_intf =
        abstr
        |> StateAbstraction.M.map
          (apply_inner img_shared thread_id control inner_intf)
        |> close_by_flush_wrt_var
          lbl thread_id shared locals control modified_var in
      (* Printf.printf "########\n\nApplication by thread %d at label %a\n\nSource:\n%a\n\nInterference:\n%a\n\nDestination:\n%a\n\n\n" *)
      (*   thread_id *)
      (*   Control.Label.print lbl *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr } *)
      (*   Interferences.print (varset_intf, inner_intf) *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' }; *)
      { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' },
      VarMap.single_var_binding modified_var generated_inner_intf

    let apply lbl
        ({ StateAbstraction.shared; locals; control; _ } as state)
        (local_intf, varmap_intf) =
      let state' = apply_local_intf lbl state local_intf in
      let state'', intf'' =
        VarMap.fold
          (fun (ExistVar.Pack var) var_intf (acc_abstr, acc_intf) ->
             let abstr', intf' = apply_singlevar_intf lbl state var var_intf in
             StateAbstraction.join abstr' acc_abstr,
             VarMap.union_join Inner.join intf' acc_intf)
          varmap_intf
          (state', VarMap.empty) in
      state'',
      (Interferences.inner_bot_local shared locals control, intf'')

    let generate_from_abstr op lbl1 lbl2 tid shared locals control abstr =
      match op with
      | O.Identity ->
        abstr,
        interference_label_only lbl1 lbl2 tid shared locals control abstr
      | O.MFence ->
        let result =
          StateAbstraction.M.filter (fun key _ -> Key.is_consistent key) abstr in
        result,
        interference_label_only lbl1 lbl2 tid shared locals control result
      | O.Filter cond ->
        let result =
          abstr
          |> StateAbstraction.meet_unsymbolised_cond (inner_of_program tid) cond
          |> StateAbstraction.normalize in
        result,
        interference_label_only lbl1 lbl2 tid shared locals control result
      | O.Assign (x, expr) ->
        begin match x.T.var_spec with
          | Ty.Local ->
            let result =
              StateAbstraction.(M.mapi (local_assign tid x expr) abstr) in
            let local_intf =
              inner_intf_from_partitions
                (Interferences.local_intf_id shared locals control)
                lbl1 lbl2 tid shared locals control abstr
                (StateAbstraction.local_assign_prime tid x expr)
            in
            result,
            (local_intf, VarMap.empty)
          | Ty.Shared ->
             let result =
              StateAbstraction.M.Labels.fold
                ~f:(fun ~key ~data:inner acc_abstr ->
                    let new_key, new_inner =
                      StateAbstraction.write tid x expr key inner in
                    StateAbstraction.add_join new_key new_inner acc_abstr)
                ~init:StateAbstraction.M.empty
                abstr in
             (* Interferences from a write are split in two parts: the
                first one (putting the variable in the buffer) simply
                updates the label, nothing else is visible from other
                threads. The second one comes from the flush. *)
             let (write_intf, _empty_map) =
               interference_label_only
                 lbl1 lbl2
                 tid shared locals
                 control abstr in
            let result', flush_intf =
              close_by_flush_wrt_var
                lbl2
                tid shared locals control
                x
                result in
            result',
            (write_intf, VarMap.single_var_binding x flush_intf)
        end

    let generate op lbl1 lbl2 { StateAbstraction.thread_id; shared; locals; control; abstr } =
      let abstr', intf =
        generate_from_abstr op lbl1 lbl2 thread_id shared locals control abstr in
      (* Printf.printf "#######\nThread %d from label %a to label %a\n\nInput:\n\n%a\n\nOutput:\n\n%a\n\nGenerated :\n\n%a\n\n\n" *)
      (*   thread_id *)
      (*   Control.Label.print lbl1 *)
      (*   Control.Label.print lbl2 *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr } *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' } *)
      (*   Interferences.print intf; *)
      { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' },
      intf
  end
end
