(*
   Copyright (C) 2024 International Digital Economy Academy.
   This program is licensed under the MoonBit Public Source
   License as published by the International Digital Economy Academy,
   either version 1 of the License, or (at your option) any later
   version. This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the MoonBit
   Public Source License for more details. You should have received a
   copy of the MoonBit Public Source License along with this program. If
   not, see
   <https://www.moonbitlang.com/licenses/moonbit-public-source-license-v1>.
*)


module Lst = Basic_lst
module Ident = Basic_ident

let type_of_typed_expr (te : Typedtree.expr) =
  match te with
  | Texpr_apply { ty; _ }
  | Texpr_array { ty; _ }
  | Texpr_constant { ty; _ }
  | Texpr_constr { ty; _ }
  | Texpr_function { ty; _ }
  | Texpr_ident { ty; _ }
  | Texpr_method { ty; _ }
  | Texpr_unresolved_method { ty; _ }
  | Texpr_as { ty; _ }
  | Texpr_if { ty; _ }
  | Texpr_letfn { ty; _ }
  | Texpr_letrec { ty; _ }
  | Texpr_let { ty; _ }
  | Texpr_sequence { ty; _ }
  | Texpr_tuple { ty; _ }
  | Texpr_record { ty; _ }
  | Texpr_record_update { ty; _ }
  | Texpr_field { ty; _ }
  | Texpr_match { ty; _ }
  | Texpr_letmut { ty; _ }
  | Texpr_loop { ty; _ }
  | Texpr_for { ty; _ }
  | Texpr_foreach { ty; _ }
  | Texpr_while { ty; _ }
  | Texpr_assign { ty; _ }
  | Texpr_mutate { ty; _ }
  | Texpr_hole { ty; _ }
  | Texpr_return { ty; _ }
  | Texpr_raise { ty; _ }
  | Texpr_try { ty; _ }
  | Texpr_exclamation { ty; _ }
  | Texpr_constraint { ty; _ }
  | Texpr_pipe { ty; _ }
  | Texpr_break { ty; _ }
  | Texpr_continue { ty; _ }
  | Texpr_interp { ty; _ }
  | Texpr_guard { ty; _ }
  | Texpr_guard_let { ty; _ }
  | Texpr_array_as_view { ty; _ } ->
      ty
  | Texpr_is _ | Texpr_and _ | Texpr_or _ -> Stype.bool
  | Texpr_unit _ -> Stype.unit

let type_of_pat (pat : Typedtree.pat) =
  match pat with
  | Tpat_alias { ty; _ }
  | Tpat_any { ty; _ }
  | Tpat_array { ty; _ }
  | Tpat_constant { ty; _ }
  | Tpat_constr { ty; _ }
  | Tpat_or { ty; _ }
  | Tpat_tuple { ty; _ }
  | Tpat_var { ty; _ }
  | Tpat_range { ty; _ }
  | Tpat_record { ty; _ }
  | Tpat_constraint { ty; _ }
  | Tpat_map { ty; _ } ->
      ty

let stype_of_typ (typ : Typedtree.typ) =
  match typ with
  | Tany { ty; _ }
  | Tarrow { ty; _ }
  | T_tuple { ty; _ }
  | Tname { ty; _ }
  | Tobject { ty; _ } ->
      ty

let rec not_in (name : Basic_ident.t) (fn : Typedtree.expr) =
  match fn with
  | Texpr_apply { func; args; _ } ->
      not_in name func
      && Lst.for_all args (fun arg -> not_in name arg.arg_value)
  | Texpr_method _ | Texpr_unresolved_method _ -> true
  | Texpr_ident { id; _ } -> not (Basic_ident.equal id.var_id name)
  | Texpr_as { expr; _ } -> not_in name expr
  | Texpr_array { exprs; _ } -> List.for_all (not_in name) exprs
  | Texpr_constant _ -> true
  | Texpr_while { loop_cond; loop_body; _ } ->
      not_in name loop_cond && not_in name loop_body
  | Texpr_for { binders; condition; steps; body; _ } ->
      Lst.for_all binders (fun (_, init) -> not_in name init)
      && (match condition with
         | None -> true
         | Some condition -> not_in name condition)
      && Lst.for_all steps (fun (_, step) -> not_in name step)
      && not_in name body
  | Texpr_foreach { expr; body; else_block; _ } -> (
      not_in name expr && not_in name body
      &&
      match else_block with
      | None -> true
      | Some else_block -> not_in name else_block)
  | Texpr_function { func; _ } -> not_in name func.body
  | Texpr_loop { params = _; body; args; ty = _; label = _ } ->
      Lst.for_all args (not_in name) && not_in name body
  | Texpr_if { cond; ifso; ifnot; _ } -> (
      not_in name cond && not_in name ifso
      && match ifnot with None -> true | Some ifnot -> not_in name ifnot)
  | Texpr_letrec { bindings; body; _ } ->
      Lst.for_all bindings (fun (_binder, fn) -> not_in name fn.body)
      && not_in name body
  | Texpr_letfn { binder = _; fn; body; _ } ->
      not_in name fn.body && not_in name body
  | Texpr_let { rhs; pat_binders = _; body; _ } ->
      not_in name rhs && not_in name body
  | Texpr_sequence { exprs; last_expr; _ } ->
      Lst.for_all exprs (not_in name) && not_in name last_expr
  | Texpr_tuple { exprs; _ } -> List.for_all (not_in name) exprs
  | Texpr_record { fields; _ } ->
      Lst.for_all fields (fun (Field_def def) -> not_in name def.expr)
  | Texpr_record_update { record; fields; _ } ->
      not_in name record
      && Lst.for_all fields (fun (Field_def def) -> not_in name def.expr)
  | Texpr_field { record; _ } -> not_in name record
  | Texpr_mutate { record; field; _ } -> not_in name record && not_in name field
  | Texpr_match { expr; cases; _ } ->
      not_in name expr
      && Lst.for_all cases (fun { action; pat_binders = _; _ } ->
             not_in name action)
  | Texpr_letmut { binder = _; expr; body; _ } ->
      not_in name expr && not_in name body
  | Texpr_assign { expr; _ } -> not_in name expr
  | Texpr_hole _ | Texpr_unit _ | Texpr_break _ | Texpr_continue _
  | Texpr_constr _ ->
      true
  | Texpr_return { return_value; _ } -> (
      match return_value with
      | None -> true
      | Some return_value -> not_in name return_value)
  | Texpr_raise { error_value; _ } -> not_in name error_value
  | Texpr_try { body; catch; catch_all = _; try_else; ty = _; err_ty = _ } -> (
      not_in name body
      && Lst.for_all catch (fun { action; pat_binders = _; _ } ->
             not_in name action)
      &&
      match try_else with
      | None -> true
      | Some try_else ->
          Lst.for_all try_else (fun { action; pat_binders = _; _ } ->
              not_in name action))
  | Texpr_exclamation { expr; _ } -> not_in name expr
  | Texpr_constraint { expr; _ } -> not_in name expr
  | Texpr_pipe { lhs; rhs; _ } ->
      let not_in_rhs name = function
        | Typedtree.Pipe_partial_apply { func; args; _ } ->
            not_in name func
            && Lst.for_all args (fun arg -> not_in name arg.arg_value)
        | Pipe_invalid { expr; _ } -> not_in name expr
      in
      not_in name lhs && not_in_rhs name rhs
  | Texpr_interp { elems; _ } ->
      Lst.for_all elems (function
        | Interp_lit _ -> true
        | Interp_expr { expr; to_string } ->
            not_in name expr && not_in name to_string)
  | Texpr_guard { cond; otherwise; body; _ } -> (
      not_in name cond && not_in name body
      &&
      match otherwise with
      | None -> true
      | Some otherwise -> not_in name otherwise)
  | Texpr_guard_let { pat = _; rhs; otherwise; body; _ } -> (
      not_in name rhs && not_in name body
      &&
      match otherwise with
      | None -> true
      | Some otherwise ->
          Lst.for_all otherwise (fun { action; pat_binders = _; _ } ->
              not_in name action))
  | Texpr_is { expr; _ } -> not_in name expr
  | Texpr_array_as_view { array; _ } -> not_in name array
  | Texpr_and { lhs; rhs; _ } | Texpr_or { lhs; rhs; _ } ->
      not_in name lhs && not_in name rhs

let is_rec (binder : Typedtree.binder) (fn : Typedtree.fn) =
  not (not_in binder.binder_id fn.body)

let free_vars_visitor =
  object
    inherit [_] Typedtree.iter

    method! visit_binder (exclude, free_vars) binder =
      Ident.Hashset.add exclude binder.binder_id;
      Ident.Hashset.remove free_vars binder.binder_id

    method! visit_var (exclude, free_vars) var =
      match var.var_id with
      | Pident _ as id ->
          if not (Ident.Hashset.mem exclude id) then
            Ident.Hashset.add free_vars id
      | Pdot _ | Plocal_method _ | Pdyntrait_method _ -> ()
  end

let free_vars expr =
  let exclude = Ident.Hashset.create 7 in
  let free_vars = Ident.Hashset.create 7 in
  free_vars_visitor#visit_expr (exclude, free_vars) expr;
  free_vars

let pat_binders_of_cond (t : Typedtree.expr) =
  (let rec go (t : Typedtree.expr) (acc : Typedtree.pat_binders) =
     match t with
     | Texpr_is { pat_binders; _ } -> List.append pat_binders acc
     | Texpr_and { lhs; rhs } -> go lhs (go rhs acc)
     | _ -> acc
   in
   go t []
    : Typedtree.pat_binders)

let cond_contains_is (t : Typedtree.expr) =
  (let rec go (t : Typedtree.expr) =
     match t with
     | Texpr_is _ -> true
     | Texpr_and { lhs; rhs } | Texpr_or { lhs; rhs } -> go lhs || go rhs
     | _ -> false
   in
   go t
    : bool)
