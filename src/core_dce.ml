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
module Vec = Basic_vec

let rec is_pure (expr : Core.expr) =
  match expr with
  | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ | Cexpr_function _ -> true
  | Cexpr_apply _ | Cexpr_mutate _ | Cexpr_assign _ | Cexpr_break _
  | Cexpr_continue _ | Cexpr_return _
  | Cexpr_handle_error { handle_kind = Joinapply _ | Return_err _; _ } ->
      false
  | Cexpr_handle_error { handle_kind = To_result; obj; _ } -> is_pure obj
  | Cexpr_prim { prim; args; _ } ->
      Primitive.is_pure prim && Lst.for_all args is_pure
  | Cexpr_and { lhs; rhs; _ } -> is_pure lhs && is_pure rhs
  | Cexpr_or { lhs; rhs; _ } -> is_pure lhs && is_pure rhs
  | Cexpr_as { expr; _ } -> is_pure expr
  | Cexpr_constr { args; _ } -> Lst.for_all args is_pure
  | Cexpr_tuple { exprs; _ } -> Lst.for_all exprs is_pure
  | Cexpr_let { rhs; body; _ } -> is_pure rhs && is_pure body
  | Cexpr_letfn { kind = Nonrec | Rec; body; _ } -> is_pure body
  | Cexpr_letfn { kind = Tail_join | Nontail_join; fn; body; _ } ->
      is_pure fn.body && is_pure body
  | Cexpr_letrec { body; _ } -> is_pure body
  | Cexpr_record { fields; _ } ->
      Lst.for_all fields (fun { expr; _ } -> is_pure expr)
  | Cexpr_record_update { record; fields; _ } ->
      is_pure record && Lst.for_all fields (fun { expr; _ } -> is_pure expr)
  | Cexpr_field { record; _ } -> is_pure record
  | Cexpr_array { exprs; _ } -> Lst.for_all exprs is_pure
  | Cexpr_sequence { exprs; last_expr; _ } ->
      Lst.for_all exprs is_pure && is_pure last_expr
  | Cexpr_if { cond; ifso; ifnot; _ } -> (
      is_pure cond && is_pure ifso
      && match ifnot with None -> true | Some x -> is_pure x)
  | Cexpr_switch_constr { obj; cases; default; _ } -> (
      is_pure obj
      && Lst.for_all cases (fun (_, _, action) -> is_pure action)
      && match default with None -> true | Some x -> is_pure x)
  | Cexpr_switch_constant { obj; cases; default; _ } ->
      is_pure obj
      && Lst.for_all cases (fun (_, action) -> is_pure action)
      && is_pure default
  | Cexpr_loop { body; args; _ } -> Lst.for_all args is_pure && is_pure body

let dce_visitor =
  (object (self)
     inherit [_] Core.Map.map

     method! visit_var (env : Dce_context.env) id =
       Dce_context.mark_as_used env id ~dce_visitor:self#visit_expr;
       id

     method! visit_Cexpr_let env name rhs body _ty loc =
       Dce_context.add_value env name rhs;
       let body = self#visit_expr env body in
       if Dce_context.used env name then
         Core.let_ ~loc name (Dce_context.get_analyzed_value env name) body
       else if is_pure rhs then body
       else Core.let_ ~loc name (self#visit_expr env rhs) body

     method! visit_Cexpr_letfn env name fn body _ty kind loc =
       Dce_context.add_func env name fn;
       let body = self#visit_expr env body in
       if Dce_context.used env name then
         Core.letfn ~loc ~kind name (Dce_context.get_analyzed_fn env name) body
       else body

     method! visit_Cexpr_letrec env bindings body _ty loc =
       Lst.iter bindings ~f:(fun (name, fn) -> Dce_context.add_func env name fn);
       let body = self#visit_expr env body in
       let bindings =
         Lst.fold_right bindings [] (fun (name, _) ->
             fun acc ->
              if Dce_context.used env name then
                (name, Dce_context.get_analyzed_fn env name) :: acc
              else acc)
       in
       if bindings = [] then body else Core.letrec ~loc bindings body

     method! visit_Cexpr_switch_constr env obj cases default _ty loc =
       let obj = self#visit_expr env obj in
       let cases =
         Lst.map cases (fun (tag, binder, action) ->
             match binder with
             | None -> (tag, None, self#visit_expr env action)
             | Some binder ->
                 let action = self#visit_expr env action in
                 if Dce_context.used env binder then (tag, Some binder, action)
                 else (tag, None, action))
       in
       let default = Option.map (self#visit_expr env) default in
       Core.switch_constr ~loc ~default obj cases
   end
    :> < visit_expr : Dce_context.env -> Core.expr -> Core.expr >)

let dce_expr = dce_visitor#visit_expr

let mark_used_and_process_def env id =
  Dce_context.mark_as_used env id ~dce_visitor:dce_expr

let eliminate_dead_code (prog : Core.program) =
  let ctx = Dce_context.make () in
  Lst.iter prog ~f:(fun top ->
      match top with
      | Ctop_expr _ -> ()
      | Ctop_let { binder; expr } -> Dce_context.add_value ctx binder expr
      | Ctop_fn { binder; func; subtops; _ } ->
          Dce_context.add_func ctx binder func;
          Lst.iter subtops ~f:(fun { binder; fn } ->
              Dce_context.add_func ctx binder fn)
      | Ctop_stub _ -> ());
  let analyzed_top_exprs = Vec.empty () in
  Lst.iter prog ~f:(fun top ->
      match top with
      | Ctop_expr { expr; is_main; loc_ } ->
          Vec.push analyzed_top_exprs
            (Core.Ctop_expr { expr = dce_expr ctx expr; is_main; loc_ })
      | Ctop_let { binder; expr = _; is_pub_ } ->
          if is_pub_ then mark_used_and_process_def ctx binder
      | Ctop_fn { binder; is_pub_; _ } -> (
          if is_pub_ then mark_used_and_process_def ctx binder
          else
            match binder with
            | Pdot (Qmethod _ | Qext_method _) ->
                mark_used_and_process_def ctx binder
            | Pdot (Qregular _) | Pdot (Qregular_implicit_pkg _) -> ()
            | Pident _ | Pmutable_ident _ | Plocal_method _ -> assert false)
      | Ctop_stub { binder; is_pub_; _ } ->
          if is_pub_ then mark_used_and_process_def ctx binder);
  let rec aux (prog : Core.program) i =
    match prog with
    | [] -> []
    | top :: rest -> (
        match top with
        | Ctop_expr _ -> Vec.get analyzed_top_exprs i :: aux rest (i + 1)
        | Ctop_let { binder; expr = _; is_pub_; loc_ } ->
            if not (Dce_context.used ctx binder) then aux rest i
            else
              Ctop_let
                {
                  binder;
                  expr = Dce_context.get_analyzed_value ctx binder;
                  is_pub_;
                  loc_;
                }
              :: aux rest i
        | Ctop_fn { binder; func = _; subtops; ty_params_; is_pub_; loc_ } ->
            if not (Dce_context.used ctx binder) then aux rest i
            else
              let func = Dce_context.get_analyzed_fn ctx binder in
              let subtops =
                Lst.fold_right subtops [] (fun { binder; fn = _ } ->
                    fun acc ->
                     (if Dce_context.used ctx binder then
                        { binder; fn = Dce_context.get_analyzed_fn ctx binder }
                        :: acc
                      else acc
                       : Core.subtop_fun_decl list))
              in
              Core.Ctop_fn { binder; func; subtops; ty_params_; is_pub_; loc_ }
              :: aux rest i
        | Ctop_stub { binder; _ } ->
            if Dce_context.used ctx binder then top :: aux rest i
            else aux rest i)
      [@@tail_mod_cons]
  in
  aux prog 0
