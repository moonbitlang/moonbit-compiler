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


module Ident = Basic_core_ident
module Ident_set = Ident.Set
module Lst = Basic_lst

let free_vars ~(exclude : Ident.Set.t) (fn : Mcore.fn) =
  (let go_ident ~env ~acc (id : Ident.t) ty =
     match id with
     | Pident _ | Pmutable_ident _ ->
         if Ident_set.mem env id then acc else Ident.Map.add acc id ty
     | Pdot _ | Plocal_method _ -> acc
   in
   let rec go_func ~env ~acc (func : Mcore.fn) =
     let new_env =
       Lst.fold_left func.params env (fun env ->
           fun p -> Ident_set.add env p.binder)
     in
     go ~env:new_env ~acc func.body
   and go ~env ~acc (expr : Mcore.expr) =
     match expr with
     | Cexpr_const _ -> acc
     | Cexpr_unit _ -> acc
     | Cexpr_var { id; ty; prim = _ } -> go_ident ~env ~acc id ty
     | Cexpr_prim { prim = _; args; ty = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_and { lhs; rhs } | Cexpr_or { lhs; rhs } ->
         let acc1 = go ~env ~acc lhs in
         go ~env ~acc:acc1 rhs
     | Cexpr_let { name; rhs; body; ty = _ } ->
         let new_env = Ident_set.add env name in
         let new_acc = go ~env ~acc rhs in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_letfn { name; fn; body; ty = _; kind = _ } ->
         let new_env = Ident_set.add env name in
         let new_acc = go_func ~env:new_env ~acc fn in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_function { func; ty = _ } -> go_func ~env ~acc func
     | Cexpr_apply { func; args; kind; ty = _; prim = _ } ->
         let new_acc =
           match kind with
           | Normal { func_ty } -> go_ident ~env ~acc func func_ty
           | Join -> acc
         in
         Lst.fold_left args new_acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_object { self; methods_key = _; ty = _ } -> go ~env ~acc self
     | Cexpr_letrec { bindings; body; ty = _ } ->
         let new_env =
           Lst.fold_left bindings env (fun env ->
               fun (binder, _) -> Ident_set.add env binder)
         in
         let new_acc =
           Lst.fold_left bindings acc (fun acc ->
               fun (_, fn) -> go_func ~env:new_env ~acc fn)
         in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_constr { tag = _; args; ty = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_tuple { exprs; ty = _ } ->
         Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
     | Cexpr_record { fields; ty = _ } ->
         Lst.fold_left fields acc (fun acc ->
             fun field -> go ~env ~acc field.expr)
     | Cexpr_record_update { record; fields; fields_num = _; ty = _ } ->
         let new_acc = go ~env ~acc record in
         Lst.fold_left fields new_acc (fun acc ->
             fun field -> go ~env ~acc field.expr)
     | Cexpr_field { record; accessor = _; pos = _; ty = _ } ->
         go ~env ~acc record
     | Cexpr_mutate { record; label = _; field; pos = _; ty = _ } ->
         let new_acc = go ~env ~acc record in
         go ~env ~acc:new_acc field
     | Cexpr_array { exprs; ty = _ } ->
         Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
     | Cexpr_assign { var; expr; ty = _ } ->
         let new_acc = go_ident ~env ~acc var (Mcore.type_of_expr expr) in
         go ~env ~acc:new_acc expr
     | Cexpr_sequence { exprs; last_expr; ty = _ } ->
         let new_acc =
           Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
         in
         go ~env ~acc:new_acc last_expr
     | Cexpr_if { cond; ifso; ifnot; ty = _ } -> (
         let acc1 = go ~env ~acc cond in
         let acc2 = go ~env ~acc:acc1 ifso in
         match ifnot with Some ifnot -> go ~env ~acc:acc2 ifnot | None -> acc2)
     | Cexpr_switch_constr { obj; cases; default; ty = _ } -> (
         let acc1 = go ~env ~acc obj in
         let acc2 =
           Lst.fold_left cases acc1 (fun acc ->
               fun (_tag, binder, case) ->
                let new_env =
                  match binder with
                  | None -> env
                  | Some binder -> Ident_set.add env binder
                in
                go ~env:new_env ~acc case)
         in
         match default with
         | Some default -> go ~env ~acc:acc2 default
         | None -> acc2)
     | Cexpr_switch_constant { obj; cases; default; ty = _ } ->
         let acc1 = go ~env ~acc obj in
         let acc2 =
           Lst.fold_left cases acc1 (fun acc ->
               fun (_const, case) -> go ~env ~acc case)
         in
         go ~env ~acc:acc2 default
     | Cexpr_loop { params; body; args; label = _; ty = _ } ->
         let new_env =
           Lst.fold_left params env (fun env ->
               fun p -> Ident_set.add env p.binder)
         in
         let new_acc =
           Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
         in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_break { arg; label = _; ty = _ } -> (
         match arg with Some arg -> go ~env ~acc arg | None -> acc)
     | Cexpr_continue { args; label = _; ty = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_return { expr; _ } -> go ~env ~acc expr
     | Cexpr_handle_error { obj; _ } -> go ~env ~acc obj
   in
   let init_env =
     Lst.fold_left fn.params exclude (fun env ->
         fun p -> Ident_set.add env p.binder)
   in
   go ~env:init_env ~acc:Ident.Map.empty fn.body
    : Mtype.t Ident.Map.t)
