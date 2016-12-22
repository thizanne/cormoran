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

  let inner_of_property =
    (* Mapper which puts as var_spec the inner symbol of a property
       variable. *)
    let map { T.var_sym; var_type; var_spec } =
      let inner_sym = match var_spec with
        | Source.Local thread_id -> sym_local thread_id var_sym
        | Source.Memory
        | Source.View _ -> sym_mem var_sym
      in { T.var_sym; var_type; var_spec = inner_sym }
    in { T.map }

  let inner_of_program thread_id =
    (* Mapper which puts as var_spec the inner symbol of a program
       variable. *)
    let map { T.var_sym; var_type; var_spec } =
      let inner_sym = match var_spec with
        | Ty.Local -> sym_local thread_id var_sym
        | Ty.Shared -> sym_mem var_sym
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

  let env_add_locals ~add_prime tid varset env =
    let maybe_add_prime var env =
      if add_prime
      then Inner.Env.add (inner_var_local_prime tid var) env
      else env
    in
    VarSet.fold
      (fun (ExistVar.Pack var) env_acc ->
         env_acc
         |> Inner.Env.add (inner_var_local tid var)
         |> maybe_add_prime var)
      varset env

  let env_add_label ~add_prime tid max env =
    let new_env = Inner.Env.add_label (sym_label tid) max env in
    if add_prime
    then Inner.Env.add_label (sym_label_prime tid) max new_env
    else new_env

  let initial_shared prog =
    initial_varset Ty.Shared prog.T.globals

  let initial_locals prog =
    List.map (fun t -> initial_varset Ty.Local t.T.locals) prog.T.threads

  let max_labels control =
    List.of_enum @@
    Enum.map
      (C.max_alpha control)
      (0 --^ C.nb_threads control)

  let initial_env ~add_prime shared locals max_labels =
    Inner.Env.empty
    |> env_add_mem ~add_prime shared
    |> List.fold_righti (env_add_locals ~add_prime) locals
    |> List.fold_righti (env_add_label ~add_prime) max_labels

  let initial_inner_top ~add_prime shared locals max_labels =
    Inner.change_env
      (initial_env ~add_prime shared locals max_labels)
      Inner.top

  let initial_inner_bot ~add_prime shared locals max_labels =
    Inner.change_env
      (initial_env ~add_prime shared locals max_labels)
      Inner.bottom

  let extend_inner_mem varset inner =
    VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.add (inner_var_mem_prime var) inner_acc)
      varset
      inner

  let extend_inner_locals tid varset inner =
    VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.add (inner_var_local_prime tid var) inner_acc)
      varset
      inner

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

  let img_inner_locals tid varset inner =
    inner
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.drop (inner_var_local tid var) inner_acc)
      varset
    |> VarSet.fold
      (fun (ExistVar.Pack var) inner_acc ->
         Inner.rename
           (inner_var_local_prime tid var)
           (inner_var_local tid var)
           inner_acc)
      varset

  let img_inner_labels control inner =
    Enum.fold
      (fun inner_acc tid ->
         inner_acc
         |> Inner.drop_label @@ sym_label tid
         |> Inner.rename_label (sym_label_prime tid) (sym_label tid))
      inner
      (0 --^ C.nb_threads control)

  let normalise_env control shared locals inner =
    (* Changes the environment of inner so that it contains exactly
       labels and the variables present either in shared or in a local
       element and their primed versions. *)
    Inner.change_env
      (initial_env ~add_prime:true shared locals @@ max_labels control)
      inner

  module StateAbstraction = struct

    type t = {
      thread_id : Source.thread_id;
      control : C.t; (* Control abstraction of the program *)
      shared : VarSet.t; (* shared variables defined in the abstraction *)
      locals : VarSet.t list; (* local variables defined in the abstraction *)
      abstr : Inner.t
    }

    let is_bottom {abstr; _ } =
      Inner.is_bottom abstr

    let equal_abstr abstr1 abstr2 =
      Inner.equal abstr1 abstr2

    let equal x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      equal_abstr x1.abstr x2.abstr

    let print output { abstr; _ } =
      Inner.print output abstr

    let bottom prog thread_structs thread_id =
      let control = C.of_threads thread_structs in
      let shared = initial_shared prog in
      let locals = initial_locals prog in
      let max_labels = max_labels control in
      let initial_inner =
        initial_inner_bot ~add_prime:false shared locals max_labels
        |> Inner.drop_label (sym_label thread_id)
      in
      {
        thread_id;
        shared;
        locals;
        control;
        abstr = initial_inner;
      }

    let top prog thread_structs thread_id =
      let control = C.of_threads thread_structs in
      let shared = initial_shared prog in
      let locals = initial_locals prog in
      let max_labels = max_labels control in
      let initial_inner =
        initial_inner_top ~add_prime:false shared locals max_labels
        |> Inner.drop_label (sym_label thread_id)
      in
      {
        thread_id;
        shared;
        locals;
        control;
        abstr = initial_inner;
      }

    let are_consistent abstrs =
      let unified_abstr =
        List.fold_left
          (fun acc abstr -> Inner.unify acc abstr.abstr)
          Inner.top
          abstrs in
      print_endline "Unified abstr:\n";
      Inner.print IO.stdout unified_abstr;
      print_newline ();
      not (Inner.is_bottom unified_abstr)

    let meet_unsymbolised_cond symbolise cond =
      Inner.meet_cons @@ T.map_expr symbolise cond

    let local_assign tid r expr inner =
      Inner.assign_expr
        (inner_var_local tid r)
        (T.map_expr (inner_of_program tid) expr)
        inner

    let local_assign_prime tid r expr inner =
      Inner.assign_expr
        (inner_var_local_prime tid r)
        (T.map_expr (inner_of_program tid) expr)
        inner

    let write tid x expr inner =
      Inner.assign_expr
        (inner_var_mem x)
        (T.map_expr (inner_of_program tid) expr)
        inner

    let write_prime tid x expr inner =
      Inner.assign_expr
        (inner_var_mem_prime x)
        (T.map_expr (inner_of_program tid) expr)
        inner

    let meet_cond cond x =
      {
        x with
        abstr =
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
          let abstract_label = C.alpha x.control label_tid label in
          Inner.meet_label (sym_label label_tid) abstract_label x.abstr
      }

    let join x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      {
        x1 with
        abstr = Inner.join x1.abstr x2.abstr
      }

    let widening x1 x2 =
      assert (x1.thread_id = x2.thread_id);
      assert (VarSet.equal x1.shared x2.shared);
      assert (List.for_all2 VarSet.equal x1.locals x2.locals);
      {
        x1 with
        abstr = Inner.widening x1.abstr x2.abstr
      }
  end

  module Interferences = struct
    type t = Inner.t

    let inner_bot shared locals control =
      initial_inner_bot
        ~add_prime:true
        shared
        locals
        (max_labels control)

    let bottom prog thread_structs =
      inner_bot
        (initial_shared prog)
        (initial_locals prog)
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

    let inner_identity shared locals control =
      (* Inner part of the identity interference *)
      initial_inner_top
        ~add_prime:true
        shared
        locals
        (max_labels control)
      |> set_shared_identity shared
      |> List.fold_righti set_local_identity locals
      |> Seq.(fold_right set_label_identity @@ 1 --^ C.nb_threads control)

    let join inner1 inner2 =
      Inner.join inner1 inner2

    let equal inner1 inner2 =
      (* The set of interferences is the important part: if both are
         identical, there is no need to check that the
         overapproximations of the modified variables are too. *)
      Inner.equal inner1 inner2

    let widening inner1 inner2 =
      Inner.widening inner1 inner2

    let print output inner =
      Inner.print output inner
  end

  module Application = struct
    type state = StateAbstraction.t
    type interference = Interferences.t

    let generate_inner lbl1 lbl2 tid shared locals control effect inner =
      Interferences.inner_identity shared locals control
      |> Inner.unify inner
      |> Inner.set_label (sym_label tid) (C.alpha control tid lbl1)
      |> Inner.set_label (sym_label_prime tid) (C.alpha control tid lbl2)
      |> effect
      |> normalise_env control shared locals

    let apply_inner tid control varset_shared varset_locals inner_intf inner =
      (* inner_intf is expected to contain only interferences
         appliable at the relevant control label of inner *)
      inner
      |> extend_inner_mem varset_shared
      |> List.fold_righti extend_inner_locals varset_locals
      |> extend_inner_labels control
      |> Inner.unify inner_intf
      |> img_inner_mem varset_shared
      |> List.fold_righti img_inner_locals varset_locals
      |> img_inner_labels control
      |> Inner.drop_label (sym_label tid)

    let apply lbl
        { StateAbstraction.thread_id; shared; locals; control; abstr }
        inner_intf =
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
      let abstr' =
        apply_inner thread_id control shared locals inner_intf abstr in
      (* Printf.printf "########\n\nApplication by thread %d at label %a\n\nSource:\n%a\n\nInterference:\n%a\n\nDestination:\n%a\n\n\n" *)
      (*   thread_id *)
      (*   Control.Label.print lbl *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr } *)
      (*   Interferences.print (varset_intf, inner_intf) *)
      (*   StateAbstraction.print { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' }; *)
      { StateAbstraction.thread_id; shared; locals; control; abstr = abstr' },
      Interferences.inner_bot shared locals control

    let generate_from_abstr op lbl1 lbl2 tid shared locals control abstr =
      match op with
      | O.Identity
      | O.MFence ->
        abstr,
        generate_inner
          lbl1 lbl2 tid shared locals control (fun inner -> inner) abstr
      | O.Filter cond ->
        let result =
          StateAbstraction.meet_unsymbolised_cond
            (inner_of_program tid)
            cond abstr in
        result,
        generate_inner
          lbl1 lbl2 tid shared locals control (fun inner -> inner) result
      | O.Assign (x, expr) ->
        match x.T.var_spec with
        | Ty.Local ->
          let result = StateAbstraction.local_assign tid x expr abstr in
          let local_intf =
            generate_inner
              lbl1 lbl2 tid shared locals control
              (StateAbstraction.local_assign_prime tid x expr)
              abstr in
          result, local_intf
        | Ty.Shared ->
          let result =
            StateAbstraction.write tid x expr abstr in
          let write_intf =
            generate_inner
              lbl1 lbl2 tid shared locals control
              (StateAbstraction.write_prime tid x expr)
              abstr in
          result, write_intf

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
