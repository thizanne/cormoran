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

  (* let sym_local_prime tid var = *)
  (*   inner_var_sym @@ Printf.sprintf "%d:%s'" tid (Sym.name var) *)

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

  (* let inner_var_local_prime tid x = inner_var (sym_local_prime tid) x *)

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

  let env_add_mem ~add_prime varset env =
    let maybe_add_prime var env =
      if add_prime
      then Inner.Env.add (inner_var_mem_prime var) env
      else env
    in
    VarSet.fold
      (fun (ExistVar.Pack var) env_acc ->
         env_acc
         |> Inner.Env.add (inner_var_mem var)
         |> maybe_add_prime var)
      varset env

  (* let env_add_locals ~add_prime tid varset env = *)
  (*   let maybe_add_prime var env = *)
  (*     if add_prime *)
  (*     then Inner.Env.add (inner_var_local_prime tid var) env *)
  (*     else env *)
  (*   in *)
  (*   VarSet.fold *)
  (*     (fun (ExistVar.Pack var) env_acc -> *)
  (*        env_acc *)
  (*        |> Inner.Env.add (inner_var_local tid var) *)
  (*        |> maybe_add_prime var) *)
  (*     varset env *)

  let env_add_label ~add_prime tid max env =
    let new_env = Inner.Env.add_label (sym_label tid) max env in
    if add_prime
    then Inner.Env.add_label (sym_label_prime tid) max new_env
    else new_env

  let initial_shared prog =
    initial_varset Ty.Shared prog.T.globals

  let initial_thread_locals prog thread_id =
    initial_varset Ty.Local @@
    (List.nth prog.T.threads thread_id).T.locals

  (* let initial_locals prog = *)
  (*   List.map (fun t -> initial_varset Ty.Local t.T.locals) prog.T.threads *)

  let max_labels control =
    List.of_enum @@
    Enum.map
      (C.max_alpha control)
      (0 --^ C.nb_threads control)

  let initial_common_env ~add_prime shared max_labels =
    Inner.Env.empty
    |> env_add_mem ~add_prime shared
    |> List.fold_righti (env_add_label ~add_prime) max_labels

  let initial_common_top ~add_prime shared max_labels =
    Inner.change_env
      (initial_common_env ~add_prime shared max_labels)
      Inner.top

  let initial_common_bot ~add_prime shared max_labels =
    Inner.change_env
      (initial_common_env ~add_prime shared max_labels)
      Inner.bottom

  let inner_add_locals thread_id locals inner =
    VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.add (inner_var_local thread_id var) inner_acc)
      locals
      inner

  let extend_inner_mem varset inner =
    VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.add (inner_var_mem_prime var) inner_acc)
      varset
      inner

  (* let extend_inner_locals tid varset inner = *)
  (*   VarSet.fold *)
  (*     (fun (ExistVar.Pack var) inner_acc -> *)
  (*        Inner.add (inner_var_local_prime tid var) inner_acc) *)
  (*     varset *)
  (*     inner *)

  let extend_inner_labels control inner =
    Enum.fold
      (fun inner_acc tid  ->
         Inner.add_label
           (sym_label_prime tid)
           (C.max_alpha control tid)
           inner_acc)
      inner
      (0 --^ C.nb_threads control)

  let img_inner_mem varset inner =
    inner
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.drop (inner_var_mem var) inner_acc)
      varset
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.rename
           (inner_var_mem_prime var)
           (inner_var_mem var)
           inner_acc)
      varset

  (* let img_inner_locals tid varset inner = *)
  (*   inner *)
  (*   |> VarSet.fold *)
  (*     (fun (ExistVar.Pack var) inner_acc -> *)
  (*        Inner.drop (inner_var_local tid var) inner_acc) *)
  (*     varset *)
  (*   |> VarSet.fold *)
  (*     (fun (ExistVar.Pack var) inner_acc -> *)
  (*        Inner.rename *)
  (*          (inner_var_local_prime tid var) *)
  (*          (inner_var_local tid var) *)
  (*          inner_acc) *)
  (*     varset *)

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
      locals : VarSet.t; (* local variables defined in the abstraction *)
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
      assert (VarSet.equal x1.locals x2.locals);
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
        locals = initial_thread_locals prog thread_id;
        abstr = M.empty;
      }

    let top prog thread_structs thread_id =
      let control = C.of_threads thread_structs in
      let shared = initial_shared prog in
      let locals = initial_thread_locals prog thread_id in
      let max_labels = max_labels control in
      let initial_inner =
        initial_common_top ~add_prime:false shared max_labels
        |> inner_add_locals thread_id locals
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

    (* let local_assign_prime tid r expr key inner = *)
    (*   Inner.assign_expr *)
    (*     (inner_var_local_prime tid r) *)
    (*     (T.map_expr (inner_of_program tid key) expr) *)
    (*     inner *)

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
      assert (VarSet.equal x1.locals x2.locals);
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
      assert (VarSet.equal x1.locals x2.locals);
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
    type t = VarSet.t * Inner.t
    (* Invariant : if `(vars, inner) : t`, then vars is a safe
       overapproximation of the set of shared variables whose flush
       needs to be taken into account after interference
       application. *)

    let inner_bot shared control =
      initial_common_bot
        ~add_prime:true
        shared
        (max_labels control)

    let bottom prog thread_structs =
      VarSet.empty,
      inner_bot
        (initial_shared prog)
        (C.of_threads thread_structs)

    let set_shared_identity shared inner =
      (* Sets the prime version of every variable in the shared varset
         to be equal to the non-primed version *)
      VarSet.fold
        (fun (ExistVar.Pack v) inner_acc ->
           Inner.assign_expr
             (inner_var_mem_prime v)
             (T.Var (L.mkdummy @@ inner_var_mem v))
             inner_acc)
        shared
        inner

    (* let set_local_identity thread_id local inner = *)
    (*   (\* Sets the prime version of every variable in the locals varset *)
    (*      to be equal to the non-primed version *\) *)
    (*   VarSet.fold *)
    (*     (fun (ExistVar.Pack v) inner_acc -> *)
    (*        Inner.assign_expr *)
    (*          (inner_var_local_prime thread_id v) *)
    (*          (T.Var (L.mkdummy @@ inner_var_local thread_id v)) *)
    (*          inner_acc) *)
    (*     local *)
    (*     inner *)

    let set_label_identity thread_id inner =
      Inner.assign_label
        (sym_label_prime thread_id)
        (sym_label thread_id)
        inner

    let inner_identity shared control =
      (* Inner part of the identity interference *)
      initial_common_top
        ~add_prime:true
        shared
        (max_labels control)
      |> set_shared_identity shared
      |> Seq.(fold_right set_label_identity @@ 1 --^ C.nb_threads control)

    let join (vars1, inner1) (vars2, inner2) =
      VarSet.union vars1 vars2,
      Inner.join inner1 inner2

    let equal (_, inner1) (_, inner2) =
      (* The set of interferences is the important part: if both are
         identical, there is no need to check that the
         overapproximations of the modified variables are too. *)
      Inner.equal inner1 inner2

    let widening (_vars1, inner1) (vars2, inner2) =
      vars2, Inner.widening inner1 inner2

    let print output (vars, inner) =
      Printf.fprintf output "%a\n%a"
        (VarSet.print ~first:"{" ~last:"}" ~sep:", " @@
         fun output (ExistVar.Pack v) ->
         Sym.print output v.T.var_sym)
        vars
        Inner.print
        inner
  end

  module Application = struct
    type state = StateAbstraction.t
    type interference = Interferences.t

  let normalise_env control shared inner =
    (* Changes the environment of inner so that it contains exactly
       labels and the variables present in shared and their primed
       versions. *)
    Inner.change_env
      (initial_common_env ~add_prime:true shared @@ max_labels control)
      inner

    let generate_inner lbl1 lbl2 tid shared control effect inner =
      Interferences.inner_identity shared control
      |> Inner.unify inner
      |> Inner.set_label (sym_label tid) (C.alpha control tid lbl1)
      |> Inner.set_label (sym_label_prime tid) (C.alpha control tid lbl2)
      |> effect
      |> normalise_env control shared

    let generate_same_partitions
        lbl1 lbl2
        thread_id shared
        control
        abstr
        effect =
      VarSet.empty,
      StateAbstraction.M.Labels.fold
        ~f:(fun ~key ~data:inner inner_acc ->
            generate_inner
              lbl1 lbl2
              thread_id shared
              control
              (effect key)
              inner
            |> Inner.join inner_acc)
        ~init:(Interferences.inner_bot shared control)
        abstr

    let add_flush_x lbl tid shared control x key inner
        (acc_abstr, acc_intf) =
      (* Adds to acc the result(s) of one flush of x from tid. If tid
         has no entry for x in its buffer, acc is returned
         unchanged. Also adds to the interferences those generated by
         this flush. *)
      let x_sym = x.T.var_sym in
      let x_mem = inner_var_mem x in
      let x_mem_prime = inner_var_mem_prime x in
      let x_top = inner_var_top tid x in
      let x_bot = inner_var_bot tid x in
      let x_bot_expr = T.Var (L.mkdummy x_bot) in
      let x_top_expr = T.Var (L.mkdummy x_top) in
      let x_tmp = inner_var_tmp x in
      let x_tmp_expr = T.Var (L.mkdummy x_tmp) in
      match Key.get_presence x_sym key with
      | Key.Zero -> acc_abstr, acc_intf
      | Key.One ->
        let key' = Key.down x_sym key in
        let inner' =
          inner
          |> Inner.assign_expr x_mem x_top_expr
          |> Inner.drop x_top in
        let inner_intf =
          generate_inner
            lbl lbl
            tid shared control
            (Inner.assign_expr x_mem_prime x_top_expr)
            inner
        in
        StateAbstraction.add_join key' inner' acc_abstr,
        Inner.join acc_intf inner_intf
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
          generate_inner
            lbl lbl
            tid shared control
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
          generate_inner
            lbl lbl
            tid shared control
            (Inner.assign_expr x_mem_prime x_bot_expr)
            inner in
        (* Make the joins *)
        acc_abstr
        |> StateAbstraction.add_join key inner_gt1
        |> StateAbstraction.add_join key_1 inner_1,
        Inner.join
          acc_intf
          (Inner.join intf_1 intf_gt1)

    let iterate_one_flush lbl tid shared control x abstr inner_intf =
      (* Does one iteration of flushing one x from each point of abstr if
         possible, adding the results to abstr *)
      StateAbstraction.M.Labels.fold
        ~f:(fun ~key ~data ->
            add_flush_x lbl tid shared control x key data)
        ~init:(abstr, inner_intf)
        abstr

    let close_by_flush_wrt_var lbl tid shared control x inner_intf abstr =
      let rec close acc_abstr acc_intf =
        let acc_abstr', acc_intf' =
          iterate_one_flush lbl tid shared control x acc_abstr acc_intf in
        if StateAbstraction.equal_abstr acc_abstr acc_abstr'
        then acc_abstr', acc_intf'
        else close acc_abstr' acc_intf'
      in
      match x.T.var_spec with
      | Ty.Local -> abstr, inner_intf
      | Ty.Shared -> close abstr inner_intf

    let close_by_flush_wrt_varset lbl tid shared control varset_modified abstr =
      VarSet.fold
        (fun (ExistVar.Pack var) (acc_abstr, acc_intf) ->
           close_by_flush_wrt_var
             lbl
             tid shared control
             var
             acc_intf acc_abstr)
        varset_modified
        (abstr, Interferences.inner_bot shared control)

    let apply_inner tid control varset_shared inner_intf inner =
      (* inner_intf is expected to contain only interferences
         appliable at the relevant control label of inner *)
      inner
      |> extend_inner_mem varset_shared
      |> extend_inner_labels control
      |> Inner.unify inner_intf
      |> img_inner_mem varset_shared
      |> img_inner_labels control
      |> Inner.drop_label (sym_label tid)

    let apply lbl
        { StateAbstraction.thread_id; shared; locals; control; abstr }
        (varset_intf, inner_intf) =
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
      let abstr', generated_intf =
        abstr
        |> StateAbstraction.M.map
          (apply_inner thread_id control shared inner_intf)
        |> close_by_flush_wrt_varset lbl thread_id shared control varset_intf in
      (* Printf.printf "########\n\nApplication by thread %d at label %a\n\nSource:\n%a\n\nInterference:\n%a\n\nDestination:\n%a\n\n\n" *)
      (*   thread_id *)
      (*   Control.Label.print lbl *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr } *)
      (*   Interferences.print (varset_intf, inner_intf) *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' }; *)
      { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' },
      (varset_intf, generated_intf)

    let generate_from_abstr op lbl1 lbl2 tid shared control abstr =
      match op with
      | O.Identity ->
        abstr,
        generate_same_partitions
          lbl1 lbl2 tid shared control abstr (fun _key inner -> inner)
      | O.MFence ->
        let result =
          StateAbstraction.M.filter (fun key _ -> Key.is_consistent key) abstr in
        result,
        generate_same_partitions
          lbl1 lbl2 tid shared control result (fun _key inner -> inner)
      | O.Filter cond ->
        let result =
          abstr
          |> StateAbstraction.meet_unsymbolised_cond (inner_of_program tid) cond
          |> StateAbstraction.normalize in
        result,
        generate_same_partitions
          lbl1 lbl2 tid shared control result (fun _key inner -> inner)
      | O.Assign (x, expr) ->
        match x.T.var_spec with
        | Ty.Local ->
          let result =
            StateAbstraction.(M.mapi (local_assign tid x expr) abstr) in
          let local_intf =
            generate_same_partitions
              lbl1 lbl2 tid shared control abstr
              (fun _key inner -> inner) in
          result, local_intf
        | Ty.Shared ->
          let result, write_inner_intf =
          StateAbstraction.M.Labels.fold
            ~f:(fun ~key ~data:inner (acc_abstr, acc_inner_intf) ->
                let new_key, new_inner =
                  StateAbstraction.write tid x expr key inner in
                let new_intf =
                  generate_inner
                    lbl1 lbl2
                    tid shared control
                    (fun inner -> inner (* No interference on buffered write *))
                    inner in
                StateAbstraction.add_join new_key new_inner acc_abstr,
                Inner.join acc_inner_intf new_intf)
            ~init:(
              StateAbstraction.M.empty,
              Interferences.inner_bot shared control
            )
            abstr in
          let result', inner_intf =
            close_by_flush_wrt_var
              lbl2
              tid shared control
              x
              write_inner_intf result in
          result',
          (VarSet.single_var x, inner_intf)

    let generate op lbl1 lbl2 { StateAbstraction.thread_id; shared; locals; control; abstr } =
      let abstr', intf =
        generate_from_abstr op lbl1 lbl2 thread_id shared control abstr in
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
