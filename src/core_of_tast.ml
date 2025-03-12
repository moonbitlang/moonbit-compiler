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
module Type_path = Basic_type_path
module Lst = Basic_lst
module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Syntax = Parsing_syntax

let ghost_loc_ = Rloc.no_location
let true_expr : Core.expr = Core.const ~loc:ghost_loc_ (C_bool true)
let false_expr : Core.expr = Core.const ~loc:ghost_loc_ (C_bool false)
let mutable_var_label : Syntax.label = { label_name = "val"; loc_ = ghost_loc_ }
let mutable_var_type (ty : Core.typ) = (Builtin.type_ref ty : Core.typ)

type foreach_context = {
  result_var : Ident.t;
  exit_join : Ident.t;
  mutable has_early_exit : bool;
  foreach_result_ty : Stype.t;
  jump_outer_ctx : Foreach_util.jump_outer_ctx;
}

type return_context =
  | No_return
  | Normal_return of { return_join : Ident.t; mutable need_return_join : bool }
  | Foreach_return of foreach_context

type error_context = { raise_join : Ident.t; mutable need_raise_join : bool }

type loop_context =
  | Loop_label of Label.t
  | For_loop_info of {
      label : Label.t;
      continue_join : Ident.t;
      mutable need_for_loop_join : bool;
    }
  | Foreach of foreach_context

type labeled_loop_context =
  (Foreach_util.labelled_jump_info option * loop_context) list

let get_outer_labels (ctx : labeled_loop_context) =
  (Lst.fold_right ctx [] (fun (item, _) ->
       fun acc -> match item with None -> acc | Some info -> info :: acc)
    : Foreach_util.labelled_jump_info list)

let find_loop_ctx ~(label : Label.t option) (ctx : labeled_loop_context) =
  (match label with
   | None -> `Normal_jump (snd (List.hd ctx))
   | Some label ->
       let rec find_loop_ctx = function
         | [] -> assert false
         | ((label_info : Foreach_util.labelled_jump_info option), loop_ctx)
           :: tl -> (
             match label_info with
             | Some label_info when Label.equal label_info.label label ->
                 `Normal_jump loop_ctx
             | Some _ | None -> (
                 match loop_ctx with
                 | Foreach foreach_ctx -> `Jump_out_foreach foreach_ctx
                 | For_loop_info _ | Loop_label _ -> find_loop_ctx tl))
       in
       find_loop_ctx ctx
    : [ `Normal_jump of loop_context | `Jump_out_foreach of foreach_context ])

type transl_context = {
  return_ctx : return_context;
  error_ctx : error_context option;
  loop_ctx : labeled_loop_context;
  error_ty : Stype.t option;
  return_ty : Stype.t;
  wrapper_info : Stype.t option;
  base : Loc.t;
}

let wrap_ok_prim expr ~result_ty =
  let rec tail_is_optimizable (expr : Core.expr) =
    match expr with
    | Cexpr_let { body; _ }
    | Cexpr_letfn { body; kind = Rec | Nonrec; _ }
    | Cexpr_letrec { body; _ }
    | Cexpr_sequence { last_expr = body; _ } ->
        tail_is_optimizable body
    | Cexpr_letfn { body; kind = Tail_join | Nontail_join; fn; _ } ->
        tail_is_optimizable body || tail_is_optimizable fn.body
    | Cexpr_if { ifso; ifnot; _ } -> (
        tail_is_optimizable ifso
        ||
        match ifnot with
        | Some ifnot -> tail_is_optimizable ifnot
        | None -> false)
    | Cexpr_switch_constr { cases; default; _ } -> (
        Lst.exists cases (fun (_, _, action) -> tail_is_optimizable action)
        || match default with Some x -> tail_is_optimizable x | _ -> false)
    | Cexpr_switch_constant { cases; default; _ } ->
        Lst.exists cases (fun (_, action) -> tail_is_optimizable action)
        || tail_is_optimizable default
    | Cexpr_handle_error { handle_kind = Joinapply _ | Return_err _; _ }
    | Cexpr_return _
    | Cexpr_apply { kind = Join; _ } ->
        true
    | Cexpr_loop _ | Cexpr_break _ | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _
    | Cexpr_prim _ | Cexpr_and _ | Cexpr_or _ | Cexpr_function _ | Cexpr_apply _
    | Cexpr_constr _ | Cexpr_tuple _ | Cexpr_record _ | Cexpr_record_update _
    | Cexpr_field _ | Cexpr_mutate _ | Cexpr_array _ | Cexpr_assign _
    | Cexpr_continue _ | Cexpr_as _ | Cexpr_handle_error _ ->
        false
  in
  let ok_tag = Builtin.constr_ok.cs_tag in
  let wrap_ok_expr expr =
    Core.prim ~ty:result_ty
      (Primitive.Pmake_value_or_error { tag = ok_tag })
      [ expr ]
  in
  let rec push_ok_to_tail (expr : Core.expr) =
    (match expr with
     | Cexpr_let { body; loc_; name; rhs; ty = _ } ->
         Core.let_ ~loc:loc_ name rhs (push_ok_to_tail body)
     | Cexpr_letfn
         { name; body; kind = (Rec | Nonrec) as kind; fn; loc_; ty = _ } ->
         Core.letfn ~loc:loc_ ~kind name fn (push_ok_to_tail body)
     | Cexpr_letfn
         {
           name;
           body;
           kind = (Tail_join | Nontail_join) as kind;
           fn;
           loc_;
           ty = _;
         } ->
         let fn = { fn with body = push_ok_to_tail fn.body } in
         Core.letfn ~loc:loc_ ~kind name fn (push_ok_to_tail body)
     | Cexpr_letrec { body; bindings; loc_; ty = _ } ->
         Core.letrec ~loc:loc_ bindings (push_ok_to_tail body)
     | Cexpr_sequence { exprs; last_expr = body; ty = _ } ->
         Core.sequence exprs (push_ok_to_tail body)
     | Cexpr_if { ifso; ifnot; cond; loc_; ty = _ } ->
         let ifnot =
           match ifnot with
           | Some ifnot -> push_ok_to_tail ifnot
           | None -> push_ok_to_tail (Core.unit ())
         in
         Core.if_ ~loc:loc_ cond ~ifso:(push_ok_to_tail ifso) ~ifnot
     | Cexpr_switch_constr { cases; default; obj; loc_; ty = _ } ->
         let cases =
           Lst.map cases (fun (tag, arg, action) ->
               (tag, arg, push_ok_to_tail action))
         in
         let default = Option.map push_ok_to_tail default in
         Core.switch_constr ~loc:loc_ obj cases ~default
     | Cexpr_switch_constant { cases; default; obj; loc_; ty = _ } ->
         let cases =
           Lst.map cases (fun (c, action) -> (c, push_ok_to_tail action))
         in
         let default = push_ok_to_tail default in
         Core.switch_constant ~loc:loc_ ~default obj cases
     | Cexpr_handle_error { obj; handle_kind = Return_err _; _ } -> obj
     | Cexpr_return { expr; return_kind; loc_; ty = _ } ->
         Core.return ~loc:loc_ ~return_kind expr ~ty:result_ty
     | Cexpr_apply { kind = Join; func; args; ty = _; ty_args_; prim; loc_ } ->
         Core.apply ~loc:loc_ ~ty_args_ ~prim ~kind:Join func args ~ty:result_ty
     | Cexpr_loop _ | Cexpr_break _ | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _
     | Cexpr_prim _ | Cexpr_and _ | Cexpr_or _ | Cexpr_function _
     | Cexpr_apply _ | Cexpr_constr _ | Cexpr_tuple _ | Cexpr_record _
     | Cexpr_record_update _ | Cexpr_field _ | Cexpr_mutate _ | Cexpr_array _
     | Cexpr_assign _ | Cexpr_continue _ | Cexpr_as _ | Cexpr_handle_error _ ->
         wrap_ok_expr expr
      : Core.expr)
  in
  if tail_is_optimizable expr then push_ok_to_tail expr else wrap_ok_expr expr

let name_of_default_arg ~label (name_of_fn : Basic_ident.t) =
  match name_of_fn with
  | Plocal_method _ -> assert false
  | Pident _ -> assert false
  | Pdyntrait_method _ -> assert false
  | Pdot (Qregular { pkg; name }) | Pdot (Qregular_implicit_pkg { pkg; name })
    ->
      Ident.of_qual_ident
        (Qregular
           {
             pkg;
             name =
               Stdlib.String.concat ""
                 [ name; "."; label; ".default" ] [@merlin.hide];
           })
  | Pdot (Qmethod { self_typ; name }) ->
      Ident.of_qual_ident
        (Qmethod
           {
             self_typ;
             name =
               Stdlib.String.concat ""
                 [ name; "."; label; ".default" ] [@merlin.hide];
           })
  | Pdot (Qext_method _) -> assert false

let rec transl_expr ~(is_tail : bool) ~(need_wrap_ok : bool)
    ~(global_env : Global_env.t) ~(tvar_env : Tvar_env.t) (ctx : transl_context)
    (texpr : Typedtree.expr) =
  (let go_tail ~need_wrap_ok texpr =
     transl_expr ~is_tail ~need_wrap_ok ~global_env ctx texpr ~tvar_env
       [@@inline]
   in
   let go_nontail texpr =
     transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env ctx texpr
       ~tvar_env
       [@@inline]
   in
   let wrap_ok core_expr =
     if need_wrap_ok then
       match ctx.wrapper_info with
       | Some result_ty -> wrap_ok_prim core_expr ~result_ty
       | None -> core_expr
     else core_expr
       [@@inline]
   in
   match texpr with
   | Texpr_constant { c; ty = _; loc_ } -> (
       match c with
       | C_bigint { v; _ } ->
           let func =
             match[@warning "-fragile-match"]
               Global_env.find_dot_method global_env
                 ~type_name:Type_path.Builtin.type_path_bigint
                 ~method_name:"from_string"
             with
             | method_info :: [] ->
                 Typedtree.Texpr_method
                   {
                     type_name =
                       Tname_path
                         {
                           name = Type_path.Builtin.type_path_bigint;
                           kind = Type;
                           loc_;
                         };
                     meth =
                       {
                         var_id = Basic_ident.of_qual_ident method_info.id;
                         loc_;
                       };
                     prim = method_info.prim;
                     ty = method_info.typ;
                     ty_args_ = [||];
                     arity_ = Some method_info.arity_;
                     loc_;
                   }
             | _ -> assert false
           in
           let arg : Typedtree.argument =
             {
               arg_value =
                 Texpr_constant
                   {
                     c = Constant.C_string (Basic_bigint.to_string v);
                     ty = Stype.string;
                     name_ = None;
                     loc_;
                   };
               arg_kind = Positional;
             }
           in
           wrap_ok
             (transl_apply ~global_env ~tvar_env ~ctx ~kind:`Normal ~loc_
                ~ty:Stype.bigint func [ arg ])
       | _ -> wrap_ok (Core.const ~loc:loc_ c))
   | Texpr_unit { loc_ } -> wrap_ok (Core.unit ~loc:loc_ ())
   | Texpr_ident { id = _; kind = Prim prim; ty; loc_ }
   | Texpr_method { type_name = _; meth = _; prim = Some prim; ty; loc_ }
     when not (Primitive.is_intrinsic prim) ->
       wrap_ok (Core.unsaturated_prim ~loc:loc_ ~ty prim)
   | Texpr_unresolved_method
       { trait_name; method_name; self_type = self_ty; ty = expect_ty; loc_ }
     -> (
       match[@warning "-fragile-match"] trait_name with
       | Tname_path { name = trait; _ } | Tname_alias { name = trait; _ } -> (
           match
             transl_trait_method ~global_env ~trait ~self_ty ~expect_ty
               ~method_name
           with
           | `Prim prim ->
               wrap_ok (Core.unsaturated_prim ~loc:loc_ ~ty:expect_ty prim)
           | `Regular (id, ty_args_, prim) ->
               wrap_ok (Core.var ~loc:loc_ ~ty:expect_ty ~ty_args_ ~prim id))
       | _ -> assert false)
   | Texpr_ident { id = { var_id }; kind = Mutable; ty; ty_args_; loc_ } ->
       wrap_ok
         (Core.field ~ty ~pos:0
            (Core.var ~loc:loc_ ~ty:(mutable_var_type ty) ~ty_args_
               (Ident.of_ident var_id))
            (Label mutable_var_label))
   | Texpr_ident { id = { var_id }; kind = Normal; ty; ty_args_; loc_ } ->
       wrap_ok (Core.var ~loc:loc_ ~ty ~ty_args_ (Ident.of_ident var_id))
   | Texpr_ident
       { id = { var_id }; kind = Value_constr tag; ty; ty_args_; loc_ } ->
       let constr_ty = Type.make_constr_type ty ~tag in
       wrap_ok
         (Core.prim ~ty
            (Pcast { kind = Constr_to_enum })
            [
              Core.var ~loc:loc_ ~ty:constr_ty ~ty_args_ (Ident.of_ident var_id);
            ])
   | Texpr_ident { id = { var_id }; kind = Prim prim; ty; ty_args_; loc_ } ->
       wrap_ok
         (Core.var ~loc:loc_ ~ty ~ty_args_ ~prim:(Some prim)
            (Ident.of_ident var_id))
   | Texpr_method { type_name = _; meth; prim; ty; ty_args_; loc_ } ->
       wrap_ok
         (Core.var ~loc:loc_ ~ty ~ty_args_ ~prim (Ident.of_ident meth.var_id))
   | Texpr_as { expr; trait; ty = _; is_implicit = _; loc_ } -> (
       match trait with
       | Tname_path { kind = Trait; name = trait }
       | Tname_alias { kind = Trait; name = trait } ->
           let obj_type =
             Stype.type_repr (Typedtree_util.type_of_typed_expr expr)
           in
           let expr = go_nontail expr in
           let expr_ty = Stype.type_repr (Core.type_of_expr expr) in
           wrap_ok
             (match expr_ty with
             | T_trait expr_trait ->
                 if Type_path.equal expr_trait trait then expr
                 else Core.as_ ~loc:loc_ ~trait ~obj_type expr
             | _ -> Core.as_ ~loc:loc_ ~trait ~obj_type expr)
       | Tname_tvar _ | Tname_path _ | Tname_alias _ -> assert false)
   | Texpr_let { pat; pat_binders; rhs; body; ty; loc_ } ->
       Transl_match.transl_let ~global_env pat ~pat_binders (go_nontail rhs)
         (go_tail ~need_wrap_ok body)
         ~ty ~loc:loc_
   | Texpr_letmut { binder; konstraint = _; expr; body; ty = _; loc_ } ->
       let expr = go_nontail expr in
       let name = Ident.of_ident binder.binder_id in
       Core.let_ ~loc:loc_ name
         (Core.record
            ~ty:(mutable_var_type (Core.type_of_expr expr))
            [ { label = mutable_var_label; pos = 0; expr; is_mut = true } ])
         (go_tail ~need_wrap_ok body)
   | Texpr_function { func; ty; is_raw_; loc_ } ->
       let ({ params; body; is_async } : Core.fn) =
         transl_fn ~base:ctx.base func ~global_env ~tvar_env
       in
       if is_raw_ then
         wrap_ok (Core.raw_function ~loc:loc_ ~ty ~is_async params body)
       else wrap_ok (Core.function_ ~loc:loc_ ~ty ~is_async params body)
   | Texpr_apply { kind_ = Dot_return_self; _ }
   | Texpr_exclamation { expr = Texpr_apply { kind_ = Dot_return_self; _ }; _ }
     ->
       let rec desugar_cascade (expr : Typedtree.expr)
           (k : Typedtree.expr -> Core.expr) =
         match expr with
         | Texpr_apply
             {
               func;
               args = { arg_value = self; arg_kind = Positional } :: args;
               ty = _;
               kind_ = Dot_return_self;
               loc_;
             } ->
             desugar_cascade self (fun actual_self ->
                 let args : Typedtree.argument list =
                   { arg_value = actual_self; arg_kind = Positional } :: args
                 in
                 let apply =
                   transl_apply ~global_env ~tvar_env ~ctx ~kind:`Normal ~loc_
                     ~ty:Stype.unit func args
                 in
                 Core.sequence2 apply (k actual_self))
         | Texpr_exclamation
             {
               expr =
                 Texpr_apply
                   {
                     func;
                     args = { arg_value = self; arg_kind = Positional } :: args;
                     ty = _;
                     kind_ = Dot_return_self;
                     loc_ = apply_loc;
                   };
               convert_to_result;
               ty = _;
               loc_;
             } ->
             assert (not convert_to_result);
             desugar_cascade self (fun actual_self ->
                 let args : Typedtree.argument list =
                   { arg_value = actual_self; arg_kind = Positional } :: args
                 in
                 let desugared_expr : Typedtree.expr =
                   Texpr_exclamation
                     {
                       expr =
                         Texpr_apply
                           {
                             func;
                             args;
                             ty = Stype.unit;
                             kind_ = Dot;
                             loc_ = apply_loc;
                           };
                       convert_to_result;
                       ty = Stype.unit;
                       loc_;
                     }
                 in
                 let apply = go_nontail desugared_expr in
                 Core.sequence2 apply (k actual_self))
         | Texpr_ident { kind = Normal | Prim _ | Value_constr _; _ } -> k expr
         | _ ->
             let ty = Typedtree_util.type_of_typed_expr expr in
             let tast_id = Basic_ident.fresh "self" in
             let tast_self : Typedtree.expr =
               Texpr_ident
                 {
                   id = { var_id = tast_id; loc_ = ghost_loc_ };
                   ty_args_ = [||];
                   arity_ = None;
                   kind = Normal;
                   ty;
                   loc_ = ghost_loc_;
                 }
             in
             let id = Ident.of_ident tast_id in
             Core.let_ id (go_nontail expr) (k tast_self)
       in
       desugar_cascade texpr (fun self -> go_tail ~need_wrap_ok self)
   | Texpr_apply { func = Texpr_constr _; args; ty }
     when Global_env.is_newtype global_env ty -> (
       match[@warning "-fragile-match"] args with
       | arg :: [] ->
           wrap_ok
             (Core.prim ~ty
                (Pcast { kind = Make_newtype })
                [ go_nontail arg.arg_value ])
       | _ -> assert false)
   | Texpr_and { lhs; rhs; loc_ } when Typedtree_util.cond_contains_is lhs ->
       wrap_ok
         (transl_cond_contain_is lhs ~global_env ~tvar_env ~ctx
            ~true_case:(go_tail ~need_wrap_ok:false rhs)
            ~false_case:false_expr ~loc_ ~in_pattern_guard:false)
   | Texpr_or { lhs; rhs; loc_ } when Typedtree_util.cond_contains_is lhs ->
       wrap_ok
         (transl_cond_contain_is lhs ~global_env ~tvar_env ~ctx
            ~true_case:true_expr
            ~false_case:(go_tail ~need_wrap_ok:false rhs)
            ~loc_ ~in_pattern_guard:false)
   | Texpr_apply { func; args; ty; loc_ } ->
       wrap_ok
         (transl_apply ~global_env ~tvar_env ~ctx ~kind:`Normal ~loc_ ~ty func
            args)
   | Texpr_letrec { bindings; body; ty = _; loc_ } ->
       let bindings =
         Lst.map bindings (fun (binder, fn) ->
             ( Ident.of_ident binder.binder_id,
               transl_fn ~global_env ~base:ctx.base fn ~tvar_env ))
       in
       let body = go_tail ~need_wrap_ok body in
       let fn_groups = Core_util.group_local_fn_bindings bindings in
       Lst.fold_right fn_groups body (fun fn_group ->
           fun body ->
            match fn_group with
            | Non_rec (name, fn) ->
                Core.letfn ~loc:loc_ ~kind:Nonrec name fn body
            | Rec ((name, fn) :: []) ->
                Core.letfn ~loc:loc_ ~kind:Rec name fn body
            | Rec bindings -> Core.letrec ~loc:loc_ bindings body)
   | Texpr_letfn { binder; fn; body; is_rec; ty = _; loc_ } ->
       let name = Ident.of_ident binder.binder_id in
       let ({ params; body = fn_body; is_async } : Core.fn) =
         transl_fn ~base:ctx.base ~global_env fn ~tvar_env
       in
       let body = go_tail ~need_wrap_ok body in
       let kind = if is_rec then Core.Rec else Nonrec in
       Core.letfn ~loc:loc_ ~kind name { params; body = fn_body; is_async } body
   | Texpr_constr { type_name = _; constr = _; tag; ty; loc_ } ->
       wrap_ok
         (match Stype.type_repr ty with
         | Tarrow { params_ty; ret_ty; is_async = _; err_ty = _ } ->
             let params, args =
               Lst.map_split params_ty (fun ty ->
                   let id = Ident.fresh "*x" in
                   let param : Core.param =
                     { binder = id; ty; loc_ = ghost_loc_ }
                   in
                   let arg = Core.var ~ty id in
                   (param, arg))
             in
             let body : Core.expr =
               if Global_env.is_newtype global_env ret_ty then
                 Core.prim ~ty:ret_ty (Pcast { kind = Make_newtype }) args
               else Core.constr ~ty:ret_ty tag args
             in
             Core.function_ ~loc:loc_ ~ty params body ~is_async:false
         | ty -> (
             match tag with
             | Constr_tag_regular { repr_ = Integer value; _ } ->
                 let c =
                   Constant.C_int { v = Int32.of_int value; repr = None }
                 in
                 Core.const ~loc:loc_ c
             | _ -> Core.constr ~loc:loc_ ~ty tag []))
   | Texpr_tuple { exprs; ty; loc_ } ->
       wrap_ok (Core.tuple ~loc:loc_ ~ty (Lst.map exprs go_nontail))
   | Texpr_record { type_name = _; fields; ty; loc_ } ->
       let fields =
         Lst.map fields (transl_field_def ~global_env ~tvar_env ctx)
       in
       wrap_ok (Core.record ~loc:loc_ ~ty fields)
   | Texpr_record_update { type_name = _; record; fields; all_fields; ty; loc_ }
     ->
       let fields_num = List.length all_fields in
       if fields_num <= 6 then
         Core.bind (go_nontail record) (fun record_id ->
             let record_var = Core.var ~ty record_id in
             let fields =
               Lst.map all_fields (fun f ->
                   let pos = f.pos in
                   match
                     Lst.find_first fields (fun (Field_def { pos = p; _ }) ->
                         p = pos)
                   with
                   | Some field ->
                       transl_field_def ~global_env ~tvar_env ctx field
                   | None ->
                       let label : Parsing_syntax.label =
                         { label_name = f.field_name; loc_ = ghost_loc_ }
                       in
                       let accessor : Parsing_syntax.accessor = Label label in
                       let field =
                         Core.field ~ty:f.ty_field record_var ~pos accessor
                       in
                       let is_mut = f.mut in
                       { label; expr = field; pos; is_mut })
             in
             wrap_ok (Core.record ~loc:loc_ ~ty fields))
       else
         let record = go_nontail record in
         let fields =
           Lst.map fields (transl_field_def ~global_env ~tvar_env ctx)
         in
         wrap_ok (Core.record_update ~loc:loc_ record fields fields_num)
   | Texpr_field { record; accessor = Newtype; pos = _; ty; loc_ } ->
       let core_record = go_nontail record in
       let newtype_info =
         Option.get
           (Global_env.get_newtype_info global_env
              (Typedtree_util.type_of_typed_expr record))
       in
       wrap_ok
         (if newtype_info.recursive then
            Core.prim ~loc:loc_ ~ty
              (Pcast { kind = Unfold_rec_newtype })
              [ core_record ]
          else Core.field ~loc:loc_ ~ty ~pos:0 core_record Newtype)
   | Texpr_field { record; accessor; pos; ty; loc_ } ->
       wrap_ok
         (match record with
         | Texpr_ident
             {
               id = { var_id };
               kind = Value_constr tag;
               ty = var_ty;
               ty_args_;
               loc_;
             } ->
             Core.prim ~ty
               (Penum_field { index = pos; tag })
               [
                 Core.var
                   ~ty:(Type.make_constr_type var_ty ~tag)
                   ~ty_args_ ~loc:loc_ (Ident.of_ident var_id);
               ]
         | _ ->
             let core_record = go_nontail record in
             Core.field ~loc:loc_ ~ty ~pos core_record accessor)
   | Texpr_mutate { record; label; field; pos; augmented_by; ty; loc_ } -> (
       let ty_field = Typedtree_util.type_of_typed_expr field in
       match record with
       | Texpr_ident
           {
             id = { var_id };
             kind = Value_constr tag;
             ty = var_ty;
             ty_args_;
             loc_;
           } ->
           let core_record =
             Core.var
               ~ty:(Type.make_constr_type var_ty ~tag)
               ~ty_args_ ~loc:loc_ (Ident.of_ident var_id)
           in
           let core_field = go_nontail field in
           let rhs =
             match augmented_by with
             | None -> core_field
             | Some fn ->
                 let lhs =
                   Core.prim ~ty:ty_field
                     (Penum_field { index = pos; tag })
                     [ core_record ]
                 in
                 make_apply ~global_env ~tvar_env ~loc:ghost_loc_ ~ty:ty_field
                   ctx fn [ lhs; core_field ]
           in
           wrap_ok
             (Core.prim ~loc:loc_ ~ty
                (Pset_enum_field { index = pos; tag })
                [ core_record; rhs ])
       | _ -> (
           let core_record = go_nontail record in
           let core_field = go_nontail field in
           match augmented_by with
           | None ->
               wrap_ok (Core.mutate ~loc:loc_ ~pos core_record label core_field)
           | Some fn ->
               Core.bind core_record (fun var ->
                   let core_record =
                     Core.var ~ty:(Core.type_of_expr core_record) var
                   in
                   let lhs =
                     Core.field ~loc:loc_ ~ty:ty_field ~pos core_record
                       (Label label)
                   in
                   let rhs =
                     make_apply ~loc:ghost_loc_ ~ty:ty_field ~global_env
                       ~tvar_env ctx fn [ lhs; core_field ]
                   in
                   wrap_ok (Core.mutate ~loc:loc_ ~pos core_record label rhs))))
   | Texpr_array { exprs; ty; is_fixed_array = true; loc_ } ->
       wrap_ok (Core.array ~loc:loc_ ~ty (Lst.map exprs go_nontail))
   | Texpr_array { exprs; ty; is_fixed_array = false; loc_ } ->
       wrap_ok (Core_util.make_array_make (Lst.map exprs go_nontail) ~loc_ ~ty)
   | Texpr_assign { var; expr; augmented_by; ty = _; loc_ } ->
       let expr = go_nontail expr in
       let id = Ident.of_ident var.var_id in
       let ty = Core.type_of_expr expr in
       let mut_var_ty = mutable_var_type ty in
       let var : Core.expr = Core.var ~loc:var.loc_ ~ty:mut_var_ty id in
       let rhs =
         match augmented_by with
         | None -> expr
         | Some fn ->
             let lhs =
               Core.field ~ty ~pos:0
                 (Core.var ~loc:loc_ ~ty:mut_var_ty id)
                 (Label mutable_var_label)
             in
             make_apply ~loc:ghost_loc_ ~ty ~global_env ~tvar_env ctx fn
               [ lhs; expr ]
       in
       wrap_ok (Core.mutate ~loc:loc_ ~pos:0 var mutable_var_label rhs)
   | Texpr_sequence { exprs; last_expr; ty = _; loc_ = _ } ->
       let exprs = Lst.map exprs go_nontail in
       let last_expr = go_tail ~need_wrap_ok last_expr in
       Core.sequence exprs last_expr
   | Texpr_if { cond; ifso; ifnot; ty = _; loc_ } ->
       wrap_ok
         (if Typedtree_util.cond_contains_is cond then
            let ifso = go_tail ~need_wrap_ok:false ifso in
            let ifnot =
              match ifnot with
              | Some ifnot -> (go_tail ~need_wrap_ok:false) ifnot
              | None -> Core.unit ()
            in
            transl_cond_contain_is cond ~global_env ~tvar_env ~ctx
              ~true_case:ifso ~false_case:ifnot ~loc_ ~in_pattern_guard:false
          else
            let cond = go_nontail cond in
            let ifso = go_tail ~need_wrap_ok:false ifso in
            let ifnot = Option.map (go_tail ~need_wrap_ok:false) ifnot in
            Core.if_ ~loc:loc_ cond ~ifso ?ifnot)
   | Texpr_is { pat = _; expr = _; pat_binders = _; loc_ } as cond ->
       wrap_ok
         (transl_cond_contain_is cond ~global_env ~tvar_env ~ctx
            ~true_case:true_expr ~false_case:false_expr ~loc_
            ~in_pattern_guard:false)
   | Texpr_match { expr; cases; ty; loc_ } ->
       let cases = transform_cases ~is_tail ~global_env ~tvar_env ~ctx cases in
       wrap_ok
         (Transl_match.transl_match ~global_env (go_nontail expr) cases ~ty
            ~loc:loc_)
   | Texpr_try { body; catch; catch_all; try_else; ty; err_ty; loc_ } ->
       let try_join = Ident.fresh "*try" in
       let try_join_param_id = Ident.fresh "*try_err" in
       let join_param_var = Core.var ~ty:err_ty try_join_param_id in
       let error_ctx = { raise_join = try_join; need_raise_join = false } in
       let try_ctx =
         { ctx with error_ctx = Some error_ctx; error_ty = Some err_ty }
       in
       let body =
         transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env try_ctx body
           ~tvar_env
       in
       let catch_cases =
         if catch_all then
           let binder_id = Basic_ident.fresh "*catchall" in
           let binder : Typedtree.binder = { binder_id; loc_ = ghost_loc_ } in
           let var : Typedtree.var =
             { var_id = binder_id; loc_ = ghost_loc_ }
           in
           let pat : Typedtree.pat =
             Tpat_var { binder; ty = err_ty; loc_ = ghost_loc_ }
           in
           let pat_binders : Typedtree.pat_binders =
             [ { binder; binder_typ = err_ty } ]
           in
           let error_value : Typedtree.expr =
             Texpr_ident
               {
                 id = var;
                 ty_args_ = [||];
                 arity_ = None;
                 kind = Normal;
                 ty = err_ty;
                 loc_ = ghost_loc_;
               }
           in
           let action : Typedtree.expr =
             Texpr_raise { error_value; ty; loc_ = ghost_loc_ }
           in
           let catch_all_case : Typedtree.match_case =
             { pat; pat_binders; action; guard = None }
           in
           transform_cases
             (catch @ [ catch_all_case ])
             ~is_tail ~global_env ~tvar_env ~ctx
         else transform_cases catch ~is_tail ~global_env ~tvar_env ~ctx
       in
       let catch_body =
         Transl_match.transl_match ~global_env join_param_var catch_cases ~ty
           ~loc:loc_
       in
       let body =
         match try_else with
         | None -> body
         | Some try_else ->
             let try_else_cases =
               transform_cases try_else ~is_tail ~global_env ~tvar_env ~ctx
             in
             Core.bind ~loc:loc_ body (fun tmp ->
                 let tmp_var = Core.var ~ty:(Core.type_of_expr body) tmp in
                 Transl_match.transl_match ~global_env tmp_var try_else_cases
                   ~ty ~loc:loc_)
       in
       wrap_ok
         (Core.letfn ~kind:Nontail_join try_join
            {
              params =
                [
                  { binder = try_join_param_id; ty = err_ty; loc_ = ghost_loc_ };
                ];
              body = catch_body;
              is_async = false;
            }
            body)
   | Texpr_while { loop_cond; loop_body; while_else; ty; loc_; label } ->
       let label, label_key =
         match label with
         | None -> (Label.fresh "*while", None)
         | Some label ->
             let break_type =
               match while_else with
               | None -> None
               | Some while_else ->
                   Some (Typedtree_util.type_of_typed_expr while_else)
             in
             let continue_types = [] in
             ( label.label,
               Some
                 ({
                    label = label.label;
                    break_type;
                    continue_types;
                    need_extra_no_payload_continue = false;
                  }
                   : Foreach_util.labelled_jump_info) )
       in
       let ctx =
         { ctx with loop_ctx = (label_key, Loop_label label) :: ctx.loop_ctx }
       in
       let body =
         transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env ctx
           loop_body ~tvar_env
       in
       let body_with_continue =
         Core.sequence2 body (Core.continue [] label ty)
       in
       let ifnot = Option.map (go_tail ~need_wrap_ok:false) while_else in
       if Typedtree_util.cond_contains_is loop_cond then
         let false_case =
           match ifnot with None -> Core.unit () | Some ifnot -> ifnot
         in
         let body =
           transl_cond_contain_is loop_cond ~global_env ~tvar_env ~ctx
             ~true_case:body_with_continue ~false_case ~loc_
             ~in_pattern_guard:false
         in
         Core.loop ~loc:loc_ [] body [] label
       else
         wrap_ok
           (let cond = go_nontail loop_cond in
            Core.loop ~loc:loc_ []
              (Core.if_ cond ~ifso:body_with_continue ?ifnot)
              [] label)
   | Texpr_break { arg; ty; loc_; label } ->
       if is_tail then
         match arg with
         | None -> Core.unit ()
         | Some arg -> go_tail ~need_wrap_ok:false arg
       else
         let arg = Option.map (go_tail ~need_wrap_ok:false) arg in
         let label =
           match label with None -> None | Some label -> Some label.label
         in
         transl_break ctx arg ~ty ~label ~loc_
   | Texpr_continue { args; ty; loc_; label } ->
       let args = Lst.map args go_nontail in
       let label =
         match label with None -> None | Some label -> Some label.label
       in
       transl_continue ctx args ~ty ~label ~loc_
   | Texpr_loop { params; body; args; ty; loc_; label } ->
       let params =
         Lst.map params (fun p ->
             (match p with
              | Param { binder; ty; konstraint = _; kind = _ } ->
                  {
                    binder = Ident.of_ident binder.binder_id;
                    ty;
                    loc_ = binder.loc_;
                  }
              | Discard_positional_param _ -> assert false
               : Core.param))
       in
       let label, label_key =
         match label with
         | None -> (Label.fresh "*loop", None)
         | Some label ->
             let break_type = Some ty in
             let continue_types =
               Lst.map args Typedtree_util.type_of_typed_expr
             in
             ( label.label,
               Some
                 ({
                    label = label.label;
                    break_type;
                    continue_types;
                    need_extra_no_payload_continue = false;
                  }
                   : Foreach_util.labelled_jump_info) )
       in
       let ctx =
         { ctx with loop_ctx = (label_key, Loop_label label) :: ctx.loop_ctx }
       in
       let body =
         transl_expr ~is_tail ~need_wrap_ok:false ~global_env ctx body ~tvar_env
       in
       wrap_ok (Core.loop ~loc:loc_ params body (Lst.map args go_nontail) label)
   | Texpr_for { binders; condition; steps; body; for_else; ty; loc_; label } ->
       let params, init_values =
         Lst.fold_right binders ([], []) (fun (binder, init) ->
             fun (params, init_values) ->
              let param : Core.param =
                {
                  binder = Ident.of_ident binder.binder_id;
                  ty = Typedtree_util.type_of_typed_expr init;
                  loc_ = binder.loc_;
                }
              in
              (param :: params, go_nontail init :: init_values))
       in
       let steps =
         Lst.map params (fun p ->
             match
               Lst.find_opt steps (fun (step_binder, step_expr) ->
                   if Ident.equal p.binder (Ident.of_ident step_binder.var_id)
                   then Some step_expr
                   else None)
             with
             | None -> Core.var ~ty:p.ty p.binder
             | Some step -> go_nontail step)
       in
       let body_loc = Typedtree.loc_of_typed_expr body in
       let continue_join = Ident.fresh "*continue" in
       let label, label_key =
         match label with
         | None -> (Label.fresh "*for", None)
         | Some label ->
             let break_type =
               match for_else with
               | None -> None
               | Some for_else ->
                   Some (Typedtree_util.type_of_typed_expr for_else)
             in
             let continue_types =
               Lst.map binders (fun (_, b) ->
                   Typedtree_util.type_of_typed_expr b)
             in
             ( label.label,
               Some
                 ({
                    label = label.label;
                    break_type;
                    continue_types;
                    need_extra_no_payload_continue = true;
                  }
                   : Foreach_util.labelled_jump_info) )
       in
       let loop_info =
         For_loop_info { continue_join; need_for_loop_join = false; label }
       in
       let ctx =
         { ctx with loop_ctx = (label_key, loop_info) :: ctx.loop_ctx }
       in
       let body =
         transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env ~tvar_env
           ctx body
       in
       let continue = Core.continue steps label ty in
       let body_with_continue =
         match loop_info with
         | For_loop_info { need_for_loop_join = true; _ } ->
             let apply = Core.join_apply ~ty continue_join [] in
             Core.letfn ~kind:Nontail_join continue_join
               { params = []; body = continue; is_async = false }
               (Core.sequence2 ~loc:body_loc body apply)
         | For_loop_info { need_for_loop_join = false; _ } ->
             Core.sequence2 ~loc:body_loc body continue
         | Loop_label _ | Foreach _ -> assert false
       in
       let ifnot = Option.map (go_tail ~need_wrap_ok:false) for_else in
       let loop_body =
         match condition with
         | None -> body_with_continue
         | Some cond ->
             Core.if_ ~loc:body_loc ~ifso:body_with_continue ?ifnot
               (go_nontail cond)
       in
       wrap_ok (Core.loop ~loc:loc_ params loop_body init_values label)
   | Texpr_foreach
       { binders; elem_tys; expr; body; else_block; ty; loc_; label } ->
       let expr = go_nontail expr in
       let result_var = Ident.fresh "*foreach_result" in
       let jump_outer_ctx =
         Foreach_util.make_jump_outer_ctx ~global_env ~tvar_env
           (get_outer_labels ctx.loop_ctx)
       in
       let foreach_result_ty =
         Foreach_util.type_foreach_result ty ctx.return_ty
           (match ctx.error_ty with
           | None -> Stype.unit
           | Some err_ty -> err_ty)
           jump_outer_ctx.payload_type
       in
       let exit_join = Ident.fresh "*foreach_exit" in
       let body_err_ctx =
         match ctx.error_ty with
         | None -> None
         | Some _ ->
             Some
               {
                 raise_join = Ident.fresh "*foreach_raise";
                 need_raise_join = false;
               }
       in
       let label_key =
         match label with
         | None -> None
         | Some label ->
             let break_type =
               match else_block with
               | None -> None
               | Some else_block ->
                   Some (Typedtree_util.type_of_typed_expr else_block)
             in
             let continue_types = [] in
             Some
               ({
                  label = label.label;
                  break_type;
                  continue_types;
                  need_extra_no_payload_continue = false;
                }
                 : Foreach_util.labelled_jump_info)
       in
       let loop_ctx =
         {
           result_var;
           exit_join;
           has_early_exit = false;
           foreach_result_ty;
           jump_outer_ctx;
         }
       in
       let body =
         transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env ~tvar_env
           {
             ctx with
             loop_ctx = (label_key, Foreach loop_ctx) :: ctx.loop_ctx;
             return_ctx = Foreach_return loop_ctx;
             error_ctx = body_err_ctx;
           }
           body
       in
       let body = Core.sequence2 body Foreach_util.iter_result_continue in
       let body =
         match body_err_ctx with
         | Some { need_raise_join = true; raise_join } -> (
             match[@warning "-fragile-match"] ctx.error_ty with
             | Some error_ty ->
                 loop_ctx.has_early_exit <- true;
                 (match ctx.error_ctx with
                 | None -> ()
                 | Some error_ctx -> error_ctx.need_raise_join <- true);
                 let err_value_param = Ident.fresh "*foreach_error" in
                 let err_value =
                   Core.constr ~ty:foreach_result_ty Foreach_util.error.cs_tag
                     [ Core.var ~ty:error_ty err_value_param ]
                 in
                 Core.letfn ~kind:Nontail_join raise_join
                   {
                     params =
                       [
                         {
                           binder = err_value_param;
                           ty = error_ty;
                           loc_ = ghost_loc_;
                         };
                       ];
                     body =
                       Core.sequence2
                         (Core.mutate ~pos:0
                            (Core.var
                               ~ty:(mutable_var_type loop_ctx.foreach_result_ty)
                               loop_ctx.result_var)
                            mutable_var_label err_value)
                         Foreach_util.iter_result_end;
                     is_async = false;
                   }
                   body
             | _ -> assert false)
         | _ -> body
       in
       let body =
         if loop_ctx.has_early_exit then
           (let exit_join_param = Ident.fresh "*foreach_body_result" in
            Core.letfn ~kind:Nontail_join exit_join
              {
                params =
                  [
                    {
                      binder = exit_join_param;
                      ty = Stype.type_iter_result;
                      loc_ = ghost_loc_;
                    };
                  ];
                body = Core.var ~ty:Stype.type_iter_result exit_join_param;
                is_async = false;
              })
             body
         else body
       in
       let callback =
         let params =
           Lst.map2 binders elem_tys (fun binder ->
               fun ty ->
                (match binder with
                 | None -> { binder = Ident.fresh "*_"; ty; loc_ = ghost_loc_ }
                 | Some { binder_id; loc_ } ->
                     { binder = Ident.of_ident binder_id; ty; loc_ }
                  : Core.param))
         in
         Core.function_
           ~ty:
             (Builtin.type_arrow elem_tys Stype.type_iter_result ~err_ty:None
                ~is_async:false)
           ~is_async:false params body
       in
       let main_loop =
         Core.bind ~loc:loc_ expr (fun expr_id ->
             Core.apply expr_id [ callback ] ~ty:Stype.type_iter_result
               ~loc:loc_
               ~kind:
                 (Normal
                    {
                      func_ty =
                        Builtin.type_arrow
                          [ Core.type_of_expr callback ]
                          Stype.type_iter_result ~err_ty:None ~is_async:false;
                    }))
       in
       let main_loop = Core.prim ~ty:Stype.unit Pignore [ main_loop ] in
       if not loop_ctx.has_early_exit then
         match else_block with
         | None -> wrap_ok main_loop
         | Some else_block ->
             Core.sequence2 main_loop (go_tail ~need_wrap_ok else_block)
       else
         let postprocess =
           let break_value = Ident.fresh "*break" in
           let return_value = Ident.fresh "*return" in
           let jump_outer_value = Ident.fresh "*jump_outer" in
           let error_case =
             match body_err_ctx with
             | Some _ -> (
                 match[@warning "-fragile-match"] ctx.error_ty with
                 | Some error_ty ->
                     let error_value = Ident.fresh "*error" in
                     ( Foreach_util.error.cs_tag,
                       Some error_value,
                       transl_raise ctx
                         (Foreach_util.get_first_enum_field error_value
                            Foreach_util.error.cs_tag ~ty:error_ty
                            ~constr_ty:foreach_result_ty)
                         ~ty ~loc_:ghost_loc_ )
                 | _ -> assert false)
             | None -> (Foreach_util.error.cs_tag, None, Core.prim ~ty Ppanic [])
           in
           let handle_jump_outer =
             if Label.Map.is_empty jump_outer_ctx.constrs then
               Core.prim ~ty Ppanic []
             else
               let generate_cases (label : Label.t)
                   ({
                      break_tag;
                      continue_tag;
                      break_type;
                      continue_types;
                      extra_continue_tag;
                      _;
                    } :
                     Foreach_util.jump_outer_constr_info) =
                 let break_binder, break_value =
                   match break_type with
                   | None -> (None, None)
                   | Some break_ty ->
                       let break_id = Ident.fresh "*break" in
                       let break_payload =
                         Foreach_util.get_first_enum_field break_id break_tag
                           ~constr_ty:jump_outer_ctx.payload_type ~ty:break_ty
                       in
                       (Some break_id, Some break_payload)
                 in
                 let break_case =
                   transl_break ctx break_value ~ty ~loc_ ~label:(Some label)
                 in
                 let continue_binder, continue_values =
                   if continue_types = [] then (None, [])
                   else
                     let id = Ident.fresh "*continue" in
                     let constr_ty =
                       Type.make_constr_type jump_outer_ctx.payload_type
                         ~tag:continue_tag
                     in
                     let v = Core.var ~ty:constr_ty id in
                     ( Some id,
                       Lst.mapi continue_types (fun i ->
                           fun ty ->
                            Core.prim ~ty
                              (Penum_field { index = i; tag = continue_tag })
                              [ v ]) )
                 in
                 let continue_case =
                   transl_continue ctx continue_values ~ty ~label:(Some label)
                     ~loc_
                 in
                 match extra_continue_tag with
                 | Some extra_continue_tag ->
                     let extra_continue_case =
                       transl_continue ctx [] ~ty ~label:(Some label) ~loc_
                     in
                     [
                       (break_tag, break_binder, break_case);
                       (continue_tag, continue_binder, continue_case);
                       (extra_continue_tag, None, extra_continue_case);
                     ]
                 | None ->
                     [
                       (break_tag, break_binder, break_case);
                       (continue_tag, continue_binder, continue_case);
                     ]
               in
               let cases =
                 Label.Map.fold jump_outer_ctx.constrs [] (fun label ->
                     fun info ->
                      fun acc -> Lst.append (generate_cases label info) acc)
               in
               Core.switch_constr
                 (Foreach_util.get_first_enum_field jump_outer_value
                    Foreach_util.jump_outer.cs_tag
                    ~ty:jump_outer_ctx.payload_type ~constr_ty:foreach_result_ty)
                 ~default:None cases
           in
           let jump_outer_case =
             ( Foreach_util.jump_outer.cs_tag,
               Some jump_outer_value,
               handle_jump_outer )
           in
           wrap_ok
             (Core.switch_constr
                (Core.field ~ty:foreach_result_ty ~pos:0
                   (Core.var
                      ~ty:(mutable_var_type foreach_result_ty)
                      result_var)
                   (Label mutable_var_label))
                ~default:None
                [
                  ( Foreach_util.continue.cs_tag,
                    None,
                    match else_block with
                    | None -> Core.unit ()
                    | Some else_block -> go_nontail else_block );
                  ( Foreach_util.break.cs_tag,
                    Some break_value,
                    Foreach_util.get_first_enum_field break_value
                      Foreach_util.break.cs_tag ~ty ~constr_ty:foreach_result_ty
                  );
                  ( Foreach_util.return.cs_tag,
                    Some return_value,
                    transl_return ~is_tail ctx
                      (Foreach_util.get_first_enum_field return_value
                         Foreach_util.return.cs_tag ~ty:ctx.return_ty
                         ~constr_ty:foreach_result_ty)
                      ~ty ~loc_:ghost_loc_ );
                  error_case;
                  jump_outer_case;
                ])
         in
         Core.let_ result_var
           (Core.record
              ~ty:(mutable_var_type foreach_result_ty)
              [
                {
                  label = mutable_var_label;
                  pos = 0;
                  is_mut = true;
                  expr =
                    Core.constr ~ty:foreach_result_ty
                      Foreach_util.continue.cs_tag [];
                };
              ])
           (Core.sequence2 main_loop postprocess)
   | Texpr_return { return_value; ty; loc_ } ->
       let return_value =
         match return_value with
         | Some rt -> go_nontail rt
         | None -> Core.unit ()
       in
       if need_wrap_ok then
         match ctx.wrapper_info with
         | Some result_ty -> wrap_ok_prim return_value ~result_ty
         | None -> assert false
       else transl_return ctx ~is_tail return_value ~ty ~loc_
   | Texpr_raise { error_value; ty; loc_ } ->
       let error_value = go_nontail error_value in
       let ty =
         if need_wrap_ok then
           match ctx.wrapper_info with
           | Some result_ty -> result_ty
           | None -> ty
         else ty
       in
       transl_raise ctx error_value ~ty ~loc_
   | Texpr_exclamation { expr; ty; loc_; convert_to_result } -> (
       let expr = go_nontail expr in
       let result_ty = Core.type_of_expr expr in
       let ok_ty, _ =
         match result_ty with
         | T_constr { type_constructor; tys = [ ok_ty; err_ty ] }
           when Type_path.equal type_constructor
                  Type_path.Builtin.type_path_multi_value_result ->
             (ok_ty, err_ty)
         | _ -> assert false
       in
       if convert_to_result then
         wrap_ok (Core.handle_error ~loc:loc_ expr To_result ~ty)
       else if need_wrap_ok then expr
       else
         match ctx.error_ctx with
         | Some error_ctx ->
             error_ctx.need_raise_join <- true;
             Core.handle_error ~loc:loc_ expr (Joinapply error_ctx.raise_join)
               ~ty:ok_ty
         | None ->
             let ctx_ok_ty = ctx.return_ty in
             Core.handle_error ~loc:loc_ expr
               (Return_err { ok_ty = ctx_ok_ty })
               ~ty:ok_ty)
   | Texpr_hole { ty; loc_; kind = _ } ->
       Core.panic ~loc:loc_
         (if need_wrap_ok then Option.get ctx.wrapper_info else ty)
   | Texpr_constraint { expr; _ } -> go_tail ~need_wrap_ok expr
   | Texpr_pipe
       {
         lhs = expr;
         ty;
         rhs =
           Pipe_partial_apply
             {
               args;
               func =
                 ( Texpr_ident _ | Texpr_method _ | Texpr_unresolved_method _
                 | Texpr_constr _ ) as func;
               loc_ = rhs_loc_;
             };
         loc_ = _;
       }
     when Lst.for_all args (fun { arg_kind; _ } -> arg_kind = Positional) ->
       wrap_ok
         (transl_apply ~global_env ~tvar_env ~ctx ~ty ~loc_:rhs_loc_
            ~kind:(`Pipe (Typedtree.loc_of_typed_expr expr))
            func
            ({ arg_value = expr; arg_kind = Positional } :: args))
   | Texpr_pipe { lhs = expr; rhs = body; ty; loc_ } ->
       let var_id = Basic_ident.fresh "*lhs" in
       let body =
         let arg =
           Typedtree.Texpr_ident
             {
               id = { var_id; loc_ = ghost_loc_ };
               kind = Normal;
               ty = Typedtree_util.type_of_typed_expr expr;
               ty_args_ = [||];
               arity_ = None;
               loc_ = ghost_loc_;
             }
         in
         match body with
         | Typedtree.Pipe_partial_apply { func; args; loc_ } ->
             wrap_ok
               (transl_apply ~global_env ~tvar_env ~ctx ~ty ~loc_
                  ~kind:(`Pipe (Typedtree.loc_of_typed_expr expr))
                  func
                  ({ arg_value = arg; arg_kind = Positional } :: args))
         | _ -> assert false
       in
       Core.let_ ~loc:loc_ (Ident.of_ident var_id) (go_nontail expr) body
   | Texpr_interp { elems; ty = _ } ->
       let core_elems =
         Lst.map elems (function
           | Interp_lit s -> Core.const (C_string s)
           | Interp_expr { expr; to_string } ->
               transl_apply ~global_env ~tvar_env ~ctx ~kind:`Normal
                 ~ty:Stype.string
                 ~loc_:(Typedtree.loc_of_typed_expr expr)
                 to_string
                 [ { arg_value = expr; arg_kind = Positional } ])
       in
       wrap_ok
         (match core_elems with
         | [] -> Core.const (C_string "")
         | x :: xs ->
             Lst.fold_left xs x (fun acc ->
                 fun x ->
                  Core.prim ~ty:Stype.string
                    (Pccall { func_name = "add_string"; arity = 2 })
                    [ acc; x ]))
   | Texpr_guard { cond; otherwise; body; loc_ = loc; ty } ->
       let body = go_tail ~need_wrap_ok:false body in
       let ifnot =
         match otherwise with
         | Some expr -> go_tail ~need_wrap_ok:false expr
         | None -> Core.prim ~loc ~ty Primitive.Ppanic []
       in
       wrap_ok
         (if Typedtree_util.cond_contains_is cond then
            transl_cond_contain_is cond ~global_env ~tvar_env ~ctx
              ~true_case:body ~false_case:ifnot ~loc_:loc
              ~in_pattern_guard:false
          else
            let cond = go_nontail cond in
            let ifnot = Some ifnot in
            Core.if_ ~loc cond ~ifso:body ?ifnot)
   | Texpr_guard_let { pat; rhs; pat_binders; otherwise; body; loc_; ty } ->
       let ok_case =
         { Typedtree.pat; pat_binders; action = body; guard = None }
       in
       let fail_cases =
         match otherwise with Some cases -> cases | None -> []
       in
       let expr =
         Typedtree.Texpr_match
           {
             expr = rhs;
             cases = ok_case :: fail_cases;
             ty;
             loc_;
             match_loc_ = loc_;
           }
       in
       go_tail ~need_wrap_ok expr
   | Texpr_array_as_view { array; ty; loc_ } ->
       let array = go_nontail array in
       Core.bind array (fun arr_id ->
           let arr_ty = Core.type_of_expr array in
           let array = Core.var ~ty:arr_ty arr_id in
           Core_util.make_array_unsafe_as_view array ~ty ~loc_)
   | Texpr_and { lhs; rhs; loc_ } ->
       let lhs = go_nontail lhs in
       let rhs = go_nontail rhs in
       wrap_ok (Core.and_ ~loc:loc_ lhs rhs)
   | Texpr_or { lhs; rhs; loc_ } ->
       let lhs = go_nontail lhs in
       let rhs = go_nontail rhs in
       wrap_ok (Core.or_ ~loc:loc_ lhs rhs)
    : Core.expr)

and transform_cases (cases : Typedtree.match_case list) ~(is_tail : bool)
    ~(global_env : Global_env.t) ~(tvar_env : Tvar_env.t)
    ~(ctx : transl_context) =
  (let go_expr expr =
     transl_expr ctx ~need_wrap_ok:false ~global_env ~tvar_env ~is_tail expr
       [@@inline]
   in
   let rec go (cases : Typedtree.match_case list) =
     (match cases with
      | [] -> []
      | { pat; pat_binders; action; guard } :: rest ->
          let guard, guard_binders =
            match guard with
            | None -> (None, [])
            | Some guard ->
                let loc = Typedtree.loc_of_typed_expr guard in
                let guard_binders = Typedtree_util.pat_binders_of_cond guard in
                ( Some
                    (if Typedtree_util.cond_contains_is guard then
                       fun ~true_case ->
                       fun ~false_case ->
                        transl_cond_contain_is guard ~global_env ~tvar_env ~ctx
                          ~true_case ~false_case ~loc_:loc
                          ~in_pattern_guard:true
                     else fun ~true_case ->
                       fun ~false_case ->
                        Core.if_ ~loc (go_expr guard) ~ifso:true_case
                          ?ifnot:(Some false_case)),
                  guard_binders )
          in
          let case : Transl_match.case =
            {
              pat;
              pat_binders;
              action = go_expr action;
              guard;
              guard_binders;
              inline_action = false;
            }
          in
          case :: go rest
       : Transl_match.case list)
       [@@tail_mod_cons]
   in
   go cases
    : Transl_match.case list)

and transl_cond_contain_is (cond : Typedtree.expr) ~global_env ~tvar_env ~ctx
    ~(true_case : Core.expr) ~(false_case : Core.expr)
    ~(in_pattern_guard : bool) ~loc_ =
  let go_nontail expr =
    transl_expr ~global_env ~is_tail:false ~need_wrap_ok:false ctx expr
      ~tvar_env
      [@@inline]
  in
  let go_cond cond ~true_case ~false_case =
    transl_cond_contain_is cond ~global_env ~tvar_env ~ctx ~true_case
      ~false_case ~loc_ ~in_pattern_guard
      [@@inline]
  in
  match cond with
  | Texpr_is { pat; expr; pat_binders; loc_ = _ } ->
      let ty = Core.type_of_expr true_case in
      let pat_ty = Typedtree_util.type_of_pat pat in
      let cases : Transl_match.case list =
        [
          {
            pat;
            pat_binders;
            action = true_case;
            guard = None;
            guard_binders = [];
            inline_action = in_pattern_guard;
          };
          {
            pat = Tpat_any { ty = pat_ty; loc_ = ghost_loc_ };
            pat_binders = [];
            action = false_case;
            guard = None;
            guard_binders = [];
            inline_action = false;
          };
        ]
      in
      Transl_match.transl_match ~global_env (go_nontail expr) cases ~ty
        ~loc:loc_
  | Texpr_and { lhs; rhs; loc_ } -> (
      match false_case with
      | Cexpr_const _ | Cexpr_apply { kind = Join; _ } ->
          let new_true_case = go_cond rhs ~true_case ~false_case in
          go_cond lhs ~true_case:new_true_case ~false_case
      | _ ->
          let binder = Ident.fresh "*else" in
          let new_false_case =
            Core.join_apply binder [] ~ty:(Core.type_of_expr true_case)
          in
          let new_true_case =
            go_cond rhs ~true_case ~false_case:new_false_case
          in
          let body =
            go_cond lhs ~true_case:new_true_case ~false_case:new_false_case
          in
          Core.joinlet_tail ~loc:loc_ binder [] false_case body)
  | Texpr_or { lhs; rhs; loc_ } -> (
      match true_case with
      | Cexpr_const _ | Cexpr_apply { kind = Join; _ } ->
          let new_false_case = go_cond rhs ~true_case ~false_case in
          go_cond lhs ~true_case ~false_case:new_false_case
      | _ ->
          let binder = Ident.fresh "*ifso" in
          let new_true_case =
            Core.join_apply binder [] ~ty:(Core.type_of_expr true_case)
          in
          let new_false_case =
            go_cond rhs ~true_case:new_true_case ~false_case
          in
          let body =
            go_cond lhs ~true_case:new_true_case ~false_case:new_false_case
          in
          Core.joinlet_tail ~loc:loc_ binder [] true_case body)
  | _ -> (
      match false_case with
      | Cexpr_unit _ ->
          Core.if_ ~loc:loc_ (go_nontail cond) ~ifso:true_case ?ifnot:None
      | _ ->
          Core.if_ ~loc:loc_ (go_nontail cond) ~ifso:true_case
            ?ifnot:(Some false_case))

and transl_apply ~global_env ~tvar_env ~ctx
    ~(kind : [ `Normal | `Pipe of Rloc.t ]) ~ty ~loc_ func args =
  let go_nontail expr =
    transl_expr ~global_env ~is_tail:false ~need_wrap_ok:false ctx expr
      ~tvar_env
      [@@inline]
  in
  let make_apply args =
    match Stype.type_repr (Typedtree_util.type_of_typed_expr func) with
    | Tarrow { err_ty = Some err_ty; _ } ->
        let result_ty = Stype.make_multi_value_result_ty ~ok_ty:ty ~err_ty in
        make_apply ~global_env ~tvar_env ctx func args ~loc:loc_ ~ty:result_ty
    | _ -> make_apply ~global_env ~tvar_env ctx func args ~loc:loc_ ~ty
      [@@inline]
  in
  let process_labelled_args ~func_ty ~make_default_arg arity =
    match[@warning "-fragile-match"] Stype.type_repr func_ty with
    | Tarrow { params_ty; ret_ty = _; err_ty = _; is_async = _ } ->
        let labelled_args = Hash_string.create 17 in
        let positional_args = Vec.empty () in
        Lst.iter args ~f:(fun arg ->
            match arg.arg_kind with
            | Positional -> Vec.push positional_args arg.arg_value
            | Labelled label
            | Labelled_pun label
            | Labelled_option { label; question_loc = _ }
            | Labelled_option_pun { label; question_loc = _ } ->
                Hash_string.add labelled_args label.label_name arg);
        let arg_bindings : (Ident.t * Core.expr * Core.expr) Vec.t =
          Vec.empty ()
        in
        let need_let = ref false in
        let loc_of_args =
          Fn_arity.to_list_map arity (fun param_kind ->
              match (kind, param_kind) with
              | `Pipe lhs_loc, Positional 0 -> lhs_loc
              | _, Positional index ->
                  Typedtree.loc_of_typed_expr (Vec.get positional_args index)
              | ( _,
                  ( Labelled { label; _ }
                  | Optional { label; _ }
                  | Autofill { label }
                  | Question_optional { label } ) ) -> (
                  match Hash_string.find_opt labelled_args label with
                  | None -> ghost_loc_
                  | Some arg -> Typedtree.loc_of_typed_expr arg.arg_value))
        in
        Fn_arity.iter2 arity params_ty (fun param_kind ->
            fun param_ty ->
             let arg_expr =
               match param_kind with
               | Optional { label; depends_on } -> (
                   match Hash_string.find_opt labelled_args label with
                   | Some arg -> go_nontail arg.arg_value
                   | None ->
                       let args_of_default_fn =
                         Lst.map depends_on (fun index ->
                             need_let := true;
                             let _, arg_id_expr, _ =
                               Vec.get arg_bindings index
                             in
                             arg_id_expr)
                       in
                       make_default_arg label param_ty args_of_default_fn)
               | Question_optional { label } -> (
                   match Hash_string.find_opt labelled_args label with
                   | None ->
                       Core.constr Builtin.constr_none.cs_tag []
                         ~ty:(Builtin.type_option param_ty)
                   | Some
                       {
                         arg_kind = Labelled_option _ | Labelled_option_pun _;
                         arg_value;
                       } ->
                       go_nontail arg_value
                   | Some { arg_kind = _; arg_value } ->
                       Core.constr Builtin.constr_some.cs_tag
                         [ go_nontail arg_value ]
                         ~ty:(Builtin.type_option param_ty))
               | Autofill { label } -> (
                   match Hash_string.find_opt labelled_args label with
                   | Some arg -> go_nontail arg.arg_value
                   | None when Type.same_type param_ty Stype.type_sourceloc ->
                       Core.const
                         (C_string (Rloc.loc_range_string ~base:ctx.base loc_))
                   | None when Type.same_type param_ty Stype.type_argsloc ->
                       let loc_of_args_expr =
                         Lst.map loc_of_args (fun loc ->
                             if Rloc.is_no_location loc then
                               Core.constr
                                 ~ty:(Builtin.type_option Stype.type_sourceloc)
                                 Builtin.constr_none.cs_tag []
                             else
                               Core.constr
                                 ~ty:(Builtin.type_option Stype.type_sourceloc)
                                 Builtin.constr_some.cs_tag
                                 [
                                   Core.const
                                     (C_string
                                        (Rloc.loc_range_string ~base:ctx.base
                                           loc));
                                 ])
                       in
                       let ty =
                         Builtin.type_array
                           (Builtin.type_option Stype.type_sourceloc)
                       in
                       Core_util.make_array_make loc_of_args_expr ~loc_ ~ty
                   | None -> assert false)
               | Positional index -> go_nontail (Vec.get positional_args index)
               | Labelled { label; _ } ->
                   go_nontail
                     (Hash_string.find_exn labelled_args label).arg_value
             in
             let arg_id = Ident.fresh "arg" in
             let arg_id_expr =
               Core.var ~ty:(Core.type_of_expr arg_expr) arg_id
             in
             Vec.push arg_bindings (arg_id, arg_id_expr, arg_expr));
        if !need_let then
          let apply_expr =
            make_apply
              (Vec.map_into_list arg_bindings
                 ~unorder:(fun (_, arg_id_expr, _) -> arg_id_expr))
          in
          Vec.fold_right arg_bindings apply_expr ~f:(fun (id, _, rhs) ->
              fun body -> Core.let_ id rhs body)
        else
          make_apply
            (Vec.map_into_list arg_bindings ~unorder:(fun (_, _, arg_expr) ->
                 arg_expr))
    | _ -> assert false
      [@@inline]
  in
  match func with
  | Texpr_ident
      { arity_ = Some arity; id = { var_id = id }; ty_args_; ty = func_ty; _ }
  | Texpr_method
      { arity_ = Some arity; meth = { var_id = id }; ty_args_; ty = func_ty; _ }
    when not (Fn_arity.is_simple arity) ->
      let make_default_arg label param_ty args =
        Core.apply ~loc:ghost_loc_
          ~kind:
            (Normal
               {
                 func_ty =
                   Builtin.type_arrow
                     (Lst.map args Core.type_of_expr)
                     param_ty ~err_ty:None ~is_async:false;
               })
          ~ty:param_ty ~ty_args_
          (name_of_default_arg ~label id)
          args
      in
      process_labelled_args ~func_ty ~make_default_arg arity
  | Texpr_constr { arity_; ty = func_ty; _ }
  | Texpr_unresolved_method { arity_ = Some arity_; ty = func_ty; _ }
    when not (Fn_arity.is_simple arity_) ->
      process_labelled_args ~func_ty
        ~make_default_arg:(fun _ -> fun _ -> fun _ -> assert false)
        arity_
  | _ -> make_apply (Lst.map args (fun arg -> go_nontail arg.arg_value))

and make_apply ~global_env ~tvar_env ctx (func : Typedtree.expr)
    (args : Core.expr list) ~loc ~ty =
  let make_apply () =
    match
      transl_expr ~global_env ~is_tail:false ~need_wrap_ok:false ctx func
        ~tvar_env
    with
    | Cexpr_var { id = func; ty = func_ty; ty_args_; prim } -> (
        match[@warning "-fragile-match"] Stype.type_repr func_ty with
        | Tarrow { is_async; _ } ->
            let kind : Core.apply_kind =
              if is_async then Async { func_ty } else Normal { func_ty }
            in
            Core.apply ~loc ~ty_args_ ~kind ~ty ~prim func args
        | _ -> assert false)
    | func -> (
        let id = Ident.fresh "*func" in
        let func_ty = Core.type_of_expr func in
        match[@warning "-fragile-match"] Stype.type_repr func_ty with
        | Tarrow { is_async; _ } ->
            let kind : Core.apply_kind =
              if is_async then Async { func_ty } else Normal { func_ty }
            in
            let app : Core.expr = Core.apply ~loc ~kind ~ty id args in
            Core.let_ id func app
        | _ -> assert false)
      [@@inline]
  in
  match func with
  | Texpr_ident { id = _; kind = Prim prim; ty = _ }
  | Texpr_method { type_name = _; meth = _; prim = Some prim; ty = _ } -> (
      match prim with
      | Pintrinsic intrinsic when not !Basic_config.no_opt -> (
          match Core_util.try_apply_intrinsic intrinsic args ~loc ~ty with
          | Some result -> result
          | None -> make_apply ())
      | Pintrinsic _ -> make_apply ()
      | _ -> Core.prim ~loc ~ty prim args)
  | Texpr_unresolved_method
      { trait_name; method_name; self_type = self_ty; ty = func_ty } -> (
      match[@warning "-fragile-match"] trait_name with
      | Tname_path { name = trait; _ } | Tname_alias { name = trait; _ } -> (
          match
            transl_trait_method ~global_env ~trait ~self_ty ~expect_ty:func_ty
              ~method_name
          with
          | `Prim prim -> Core.prim ~loc ~ty prim args
          | `Regular (id, ty_args_, prim) ->
              Core.apply ~loc ~ty
                ~kind:(Normal { func_ty })
                ~ty_args_ ~prim id args)
      | _ -> assert false)
  | Texpr_ident { id = { var_id = Pdot qual_name }; kind = _; ty = _; ty_args_ }
    -> (
      match Core_util.specialize qual_name ty_args_ with
      | Some prim -> Core.prim ~loc ~ty prim args
      | None -> make_apply ())
  | Texpr_constr { tag; type_name = _; constr = _; ty = _ } ->
      if Global_env.is_newtype global_env ty then
        Core.prim ~ty (Pcast { kind = Make_newtype }) args
      else Core.constr ~loc ~ty tag args
  | _ -> make_apply ()

and transl_fn ~global_env ~tvar_env ~base (fn : Typedtree.fn) =
  (let params =
     Lst.map fn.params (fun p ->
         (match p with
          | Param { binder; ty; konstraint = _; kind = _ } ->
              {
                binder = Ident.of_ident binder.binder_id;
                ty;
                loc_ = binder.loc_;
              }
          | Discard_positional_param { ty; loc_; _ } ->
              { binder = Ident.fresh "*discard_"; ty; loc_ }
           : Core.param))
   in
   match[@warning "-fragile-match"] Stype.type_repr fn.ty with
   | Tarrow { is_async; err_ty = err_type; _ } ->
       let return_value_ty = Typedtree_util.type_of_typed_expr fn.body in
       let wrapper_info =
         match err_type with
         | None -> None
         | Some err_ty ->
             Some
               (Stype.make_multi_value_result_ty ~ok_ty:return_value_ty ~err_ty)
       in
       let ctx =
         {
           return_ctx = No_return;
           error_ctx = None;
           loop_ctx = [];
           return_ty = return_value_ty;
           error_ty = err_type;
           wrapper_info;
           base;
         }
       in
       let need_wrap_ok =
         match err_type with Some _ -> true | None -> false
       in
       let body =
         transl_expr ~is_tail:true ~need_wrap_ok ~global_env ~tvar_env ctx
           fn.body
       in
       { params; body; is_async }
   | _ -> assert false
    : Core.fn)

and transl_field_def ~global_env ~tvar_env (ctx : transl_context)
    (field : Typedtree.field_def) =
  (let (Field_def { label; expr; pos; is_mut; is_pun = _ }) = field in
   {
     label;
     expr =
       transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env ~tvar_env ctx
         expr;
     pos;
     is_mut;
   }
    : Core.field_def)

and transl_return ctx ~is_tail return_value ~ty ~loc_ =
  match ctx.return_ctx with
  | Foreach_return loop_ctx ->
      let return_value =
        Core.constr ~ty:loop_ctx.foreach_result_ty Foreach_util.return.cs_tag
          [ return_value ]
      in
      loop_ctx.has_early_exit <- true;
      Core.sequence2
        (Core.mutate ~pos:0
           (Core.var
              ~ty:(mutable_var_type loop_ctx.foreach_result_ty)
              loop_ctx.result_var)
           mutable_var_label return_value)
        (Core.join_apply ~ty loop_ctx.exit_join
           [ Foreach_util.iter_result_end ])
  | No_return -> (
      match ctx.error_ty with
      | None ->
          if is_tail then return_value
          else Core.return return_value ~return_kind:Single_value ~ty ~loc:loc_
      | Some err_ty ->
          let ok_ty = Core.type_of_expr return_value in
          let result_ty = Stype.make_multi_value_result_ty ~ok_ty ~err_ty in
          Core.return return_value
            ~return_kind:
              (Error_result { is_error = false; return_ty = result_ty })
            ~ty ~loc:loc_)
  | Normal_return return_ctx ->
      if is_tail then return_value
      else (
        return_ctx.need_return_join <- true;
        Core.join_apply ~loc:loc_ ~ty return_ctx.return_join [ return_value ])

and transl_raise ctx error_value ~ty ~loc_ =
  match ctx.error_ctx with
  | Some error_ctx ->
      error_ctx.need_raise_join <- true;
      Core.join_apply ~loc:loc_ ~ty error_ctx.raise_join [ error_value ]
  | None ->
      let ok_ty = ctx.return_ty in
      let err_ty = Core.type_of_expr error_value in
      let result_ty = Stype.make_multi_value_result_ty ~ok_ty ~err_ty in
      Core.return error_value
        ~return_kind:(Error_result { is_error = true; return_ty = result_ty })
        ~ty ~loc:loc_

and transl_break ctx arg ~ty ~loc_ ~label =
  match find_loop_ctx ~label ctx.loop_ctx with
  | `Normal_jump (Loop_label label) | `Normal_jump (For_loop_info { label; _ })
    ->
      Core.break ~loc_ arg label ty
  | `Normal_jump (Foreach loop_ctx) ->
      let arg = match arg with None -> Core.unit () | Some arg -> arg in
      let break_value =
        Core.constr ~ty:loop_ctx.foreach_result_ty Foreach_util.break.cs_tag
          [ arg ]
      in
      loop_ctx.has_early_exit <- true;
      Core.sequence2
        (Core.mutate ~pos:0
           (Core.var
              ~ty:(mutable_var_type loop_ctx.foreach_result_ty)
              loop_ctx.result_var)
           mutable_var_label break_value)
        (Core.join_apply ~ty loop_ctx.exit_join
           [ Foreach_util.iter_result_end ])
  | `Jump_out_foreach foreach_ctx ->
      let arg = match arg with None -> [] | Some arg -> [ arg ] in
      let ({ break_tag; _ } : Foreach_util.jump_outer_constr_info) =
        Label.Map.find_exn foreach_ctx.jump_outer_ctx.constrs (Option.get label)
      in
      let payload_value =
        Core.constr ~ty:foreach_ctx.jump_outer_ctx.payload_type break_tag arg
      in
      let jump_outer_value =
        Core.constr ~ty:foreach_ctx.foreach_result_ty
          Foreach_util.jump_outer.cs_tag [ payload_value ]
      in
      let iter_result_var =
        Core.var
          ~ty:(mutable_var_type foreach_ctx.foreach_result_ty)
          foreach_ctx.result_var
      in
      foreach_ctx.has_early_exit <- true;
      Core.sequence2
        (Core.mutate ~pos:0 iter_result_var mutable_var_label jump_outer_value)
        (Core.join_apply ~ty foreach_ctx.exit_join
           [ Foreach_util.iter_result_end ])

and transl_continue ctx args ~ty ~label ~loc_ =
  match find_loop_ctx ~label ctx.loop_ctx with
  | `Normal_jump (Loop_label label) -> Core.continue ~loc:loc_ args label ty
  | `Normal_jump (For_loop_info for_loop_ctx) ->
      if args = [] then (
        for_loop_ctx.need_for_loop_join <- true;
        Core.join_apply ~loc:loc_ ~ty for_loop_ctx.continue_join [])
      else Core.continue ~loc:loc_ args for_loop_ctx.label ty
  | `Normal_jump (Foreach loop_ctx) ->
      loop_ctx.has_early_exit <- true;
      Core.join_apply ~ty loop_ctx.exit_join
        [ Foreach_util.iter_result_continue ]
  | `Jump_out_foreach foreach_ctx ->
      let ({ continue_tag; extra_continue_tag; _ }
            : Foreach_util.jump_outer_constr_info) =
        Label.Map.find_exn foreach_ctx.jump_outer_ctx.constrs (Option.get label)
      in
      let payload_value =
        match extra_continue_tag with
        | Some extra_continue_tag when args = [] ->
            Core.constr ~ty:foreach_ctx.jump_outer_ctx.payload_type
              extra_continue_tag []
        | _ ->
            Core.constr ~ty:foreach_ctx.jump_outer_ctx.payload_type continue_tag
              args
      in
      let jump_outer_value =
        Core.constr ~ty:foreach_ctx.foreach_result_ty
          Foreach_util.jump_outer.cs_tag [ payload_value ]
      in
      let iter_result_var =
        Core.var
          ~ty:(mutable_var_type foreach_ctx.foreach_result_ty)
          foreach_ctx.result_var
      in
      foreach_ctx.has_early_exit <- true;
      Core.sequence2
        (Core.mutate ~pos:0 iter_result_var mutable_var_label jump_outer_value)
        (Core.join_apply ~ty foreach_ctx.exit_join
           [ Foreach_util.iter_result_end ])

and transl_trait_method ~global_env ~trait ~method_name ~(self_ty : Stype.t)
    ~(expect_ty : Stype.t) =
  let resolve_by_path (type_name : Type_path.t) =
    match
      Global_env.find_trait_method global_env ~trait ~type_name ~method_name
    with
    | None -> assert false
    | Some mi -> (
        let actual_ty, _, ty_args =
          Poly_type.instantiate_method_no_constraint mi
        in
        Ctype.unify_exn expect_ty actual_ty;
        match mi.prim with
        | None | Some (Pintrinsic _) ->
            `Regular (Ident.of_qual_ident mi.id, ty_args, mi.prim)
        | Some p -> `Prim p)
      [@@inline]
  in
  let self_ty = Stype.type_repr self_ty in
  match self_ty with
  | Tparam { index; name_ } ->
      let id =
        Ident.of_local_method ~index ~tvar_name:name_ ~trait method_name
      in
      `Regular (id, [||], None)
  | T_constr { type_constructor; _ } -> resolve_by_path type_constructor
  | T_builtin builtin -> resolve_by_path (Stype.tpath_of_builtin builtin)
  | T_trait object_trait -> resolve_by_path object_trait
  | Tarrow _ -> assert false
  | Tvar _ -> assert false
  | T_blackhole -> assert false

let dummy_ctx ~base =
  {
    return_ctx = No_return;
    error_ctx = None;
    loop_ctx = [];
    return_ty = Stype.unit;
    error_ty = None;
    wrapper_info = None;
    base;
  }

let transl_top_expr expr ~global_env ~base =
  let return_join = Ident.fresh "*return" in
  let return_value_id = Ident.fresh "*return_value" in
  let return_ctx = Normal_return { return_join; need_return_join = false } in
  let ctx =
    {
      return_ctx;
      error_ctx = None;
      loop_ctx = [];
      return_ty = Typedtree_util.type_of_typed_expr expr;
      error_ty = None;
      wrapper_info = None;
      base;
    }
  in
  let body =
    transl_expr ~is_tail:true ~need_wrap_ok:false ~global_env
      ~tvar_env:Tvar_env.empty ctx expr
  in
  match[@warning "-fragile-match"] return_ctx with
  | Normal_return return_ctx ->
      if return_ctx.need_return_join then
        Core.letfn ~kind:Nontail_join return_join
          {
            params =
              [
                { binder = return_value_id; ty = Stype.unit; loc_ = ghost_loc_ };
              ];
            body = Core.var ~ty_args_:[||] ~ty:Stype.unit return_value_id;
            is_async = false;
          }
          body
      else body
  | _ -> assert false

let generate_default_exprs ~global_env ~(fn_binder : Typedtree.binder) ~is_pub
    ~ty_params_ ~(params : Typedtree.params) ~arity ~base =
  let prev_params : Core.param Vec.t = Vec.empty () in
  let default_exprs = Vec.empty () in
  Fn_arity.iter2 arity params (fun param_kind ->
      fun param ->
       match (param_kind, param) with
       | ( Optional { label = _; depends_on },
           Param { kind = Optional expr; binder; ty; _ } ) ->
           let subst = Ident.Hash.create 16 in
           let params =
             Lst.map depends_on (fun index ->
                 let param = Vec.get prev_params index in
                 let new_binder = Ident.rename param.binder in
                 Ident.Hash.add subst param.binder new_binder;
                 { param with binder = new_binder })
           in
           Vec.push prev_params
             {
               binder = Ident.of_ident binder.binder_id;
               ty;
               loc_ = binder.loc_;
             };
           let cexpr =
             Core_util.substitute ~subst
               (transl_expr ~is_tail:true ~need_wrap_ok:false ~global_env
                  ~tvar_env:ty_params_ (dummy_ctx ~base) expr)
           in
           Vec.push default_exprs
             (Core.Ctop_fn
                {
                  binder =
                    name_of_default_arg
                      ~label:(Basic_ident.base_name binder.binder_id)
                      fn_binder.binder_id;
                  ty_params_;
                  is_pub_ = is_pub;
                  loc_ = base;
                  func = { params; body = cexpr; is_async = false };
                  subtops = [];
                })
       | _, Param { kind = Optional _; _ } -> assert false
       | ( _,
           Param
             {
               kind = Positional | Labelled | Autofill | Question_optional;
               binder;
               ty;
               _;
             } ) ->
           Vec.push prev_params
             {
               binder = Ident.of_ident binder.binder_id;
               ty;
               loc_ = binder.loc_;
             }
       | _, Discard_positional_param _ -> ());
  Vec.to_list default_exprs
[@@inline]

let transl_impl ~global_env (impl : Typedtree.impl) =
  (match impl with
   | Timpl_expr { expr; is_main; expr_id = _; loc_ } ->
       [
         Ctop_expr
           { expr = transl_top_expr ~global_env ~base:loc_ expr; is_main; loc_ };
       ]
   | Timpl_letdef { binder; konstraint = _; expr; is_pub; loc_; attrs = _ } ->
       let binder = Ident.of_ident binder.binder_id in
       let expr =
         transl_expr ~is_tail:false ~need_wrap_ok:false ~global_env
           ~tvar_env:Tvar_env.empty (dummy_ctx ~base:loc_) expr
       in
       [ Ctop_let { binder; expr; is_pub_ = is_pub; loc_ } ]
   | Timpl_fun_decl
       {
         fun_decl =
           {
             kind = _;
             fn_binder;
             fn;
             is_pub;
             ty_params_;
             constraint_names = _;
             attrs = _;
           };
         arity_;
         loc_;
       } ->
       let binder = Ident.of_ident fn_binder.binder_id in
       let func = transl_fn ~base:loc_ ~global_env ~tvar_env:ty_params_ fn in
       Ctop_fn
         { binder; func; subtops = []; ty_params_; is_pub_ = is_pub; loc_ }
       :: generate_default_exprs ~fn_binder ~global_env ~is_pub ~ty_params_
            ~arity:arity_ ~params:fn.params ~base:loc_
   | Timpl_stub_decl { func_stubs; binder; is_pub; loc_; params; arity_; ret }
     -> (
       let default_exprs =
         generate_default_exprs ~fn_binder:binder ~global_env ~is_pub
           ~ty_params_:Tvar_env.empty ~arity:arity_ ~params ~base:loc_
       in
       let binder = Ident.of_ident binder.binder_id in
       let params_ty =
         Lst.map params
           (fun (Param { ty; _ } | Discard_positional_param { ty; _ }) -> ty)
       in
       let return_ty = Option.map Typedtree_util.stype_of_typ ret in
       match func_stubs with
       | Intrinsic -> default_exprs
       | Func_stub func_stubs ->
           Ctop_stub
             {
               binder;
               func_stubs;
               params_ty;
               return_ty;
               is_pub_ = is_pub;
               loc_;
             }
           :: default_exprs)
    : Core.top_item list)

let transl ~(global_env : Global_env.t) (output : Typedtree.output) =
  (let (Output { value_defs; _ }) = output in
   Lst.concat_map value_defs (transl_impl ~global_env)
    : Core.program)
