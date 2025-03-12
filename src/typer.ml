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


module Ident = Basic_ident
module Type_path = Basic_type_path
module Constr_info = Basic_constr_info
module Lst = Basic_lst
module Vec = Basic_vec
module Longident = Basic_longident
module Syntax = Parsing_syntax
module Operators = Parsing_operators
module Syntax_util = Parsing_syntax_util
module Literal = Lex_literal

let add_error = Typeutil.add_local_typing_error
let store_error = Typeutil.store_error
let take_info_partial = Typeutil.take_info_partial
let unify_expr_and_store_error = Ctype.unify_expr_and_store_error

let typing_type ~diagnostics ~tvar_env ~global_env typ =
  (Typeutil.typing_type ~allow_partial:true ~allow_private:true ~tvar_env
     ~types:(Global_env.get_all_types global_env)
     ~local_type_env:(Global_env.get_cur_local_type_env global_env)
     typ ~diagnostics
    : Typedtree.typ)

let typing_type_name ~diagnostics ~tvar_env ~global_env
    (type_name : Syntax.type_name) =
  (Typeutil.typing_type_name
     ~types:(Global_env.get_all_types global_env)
     ~tvar_env type_name ~allow_private:true
     ~local_type_env:(Global_env.get_cur_local_type_env global_env)
     ~diagnostics
    : (Typeutil.type_name * Typedtree.type_name) option)

let find_value ~(env : Local_env.t) ~(global_env : Global_env.t)
    (id : Longident.t) ~diagnostics ~loc =
  (match id with
   | Lident name -> (
       match Local_env.find_by_name_opt env name with
       | Some r -> r
       | None -> Global_env.find_value global_env id ~loc ~diagnostics)
   | Ldot _ -> Global_env.find_value global_env id ~loc ~diagnostics
    : Value_info.t)

type unify_result =
  | Ok
  | Error of Local_diagnostics.error
  | TraitUpCast of Type_path.t

let unify_expr_allow_trait_upcast ~global_env ~cenv ~expect_ty ~actual_ty loc =
  (let expect_ty' = Stype.type_repr expect_ty in
   let actual_ty' = Stype.type_repr actual_ty in
   let fallback () =
     match Ctype.unify_expr ~expect_ty ~actual_ty loc with
     | None -> Ok
     | Some err -> Error err
       [@@inline]
   in
   if Basic_prelude.phys_equal expect_ty' actual_ty' then Ok
   else
     match expect_ty' with
     | T_trait expect_trait -> (
         match actual_ty' with
         | T_trait actual_trait when Type_path.equal expect_trait actual_trait
           ->
             Ok
         | Tvar _ -> fallback ()
         | _ ->
             let closure =
               Trait_closure.compute_closure
                 ~types:(Global_env.get_all_types global_env)
                 [ { trait = expect_trait; loc_ = loc; src_ = Direct } ]
             in
             Basic_lst.iter closure
               ~f:(Poly_type.add_constraint cenv actual_ty');
             TraitUpCast expect_trait)
     | _ -> fallback ()
    : unify_result)

let handle_unify_result (result : unify_result) ~diagnostics ~expr =
  match result with
  | Ok -> expr
  | Error e ->
      add_error diagnostics e;
      expr
  | TraitUpCast trait ->
      Typedtree.Texpr_as
        {
          expr;
          ty = T_trait trait;
          trait =
            Tname_path { name = trait; kind = Trait; loc_ = Rloc.no_location };
          is_implicit = true;
          loc_ = Typedtree.loc_of_typed_expr expr;
        }

type maybe_typed = Typechecked of Typedtree.expr | Not_yet of Syntax.expr

let wrap_newtype_constr (newtype_constr : Typedecl_info.constructor)
    (expr : Typedtree.expr) newtype_typ (loc : Rloc.t) ~diagnostics =
  (match newtype_constr.cs_vis with
  | Readable ->
      if Stype.is_external newtype_typ then
        add_error diagnostics
          (Errors.readonly_type ~name:newtype_constr.constr_name ~loc)
  | Invisible | Read_write -> ());
  let ty = Typedtree_util.type_of_typed_expr expr in
  let constr_desc = newtype_constr in
  let constr_expr =
    Typedtree.Texpr_constr
      {
        type_name = None;
        constr = { name = newtype_constr.constr_name; loc_ = Rloc.no_location };
        tag = constr_desc.cs_tag;
        ty = Builtin.type_arrow [ ty ] newtype_typ ~err_ty:None ~is_async:false;
        arity_ = constr_desc.cs_arity_;
        loc_ = Rloc.no_location;
      }
  in
  Typedtree.Texpr_apply
    {
      func = constr_expr;
      args = [ { arg_value = expr; arg_kind = Positional } ];
      ty = newtype_typ;
      kind_ = Normal;
      loc_ = loc;
    }

type expect_ty = Expect_type of Stype.t | Ignored

let get_expected_type = function Expect_type ty -> ty | Ignored -> Stype.unit

let desugar_multiline_string ~loc_ elems =
  (if List.for_all Typeutil.is_raw_string elems then
     let lines =
       Lst.map elems (function
         | Syntax.Multiline_string s -> s
         | Syntax.Multiline_interp _ -> assert false)
     in
     let str = String.concat "\n" lines in
     Pexpr_constant
       { c = Const_string { string_val = str; string_repr = str }; loc_ }
   else
     let newline =
       Syntax.Interp_lit { str = "\n"; repr = ""; loc_ = Rloc.no_location }
     in
     let rec loop elems acc =
       match elems with
       | [] -> acc
       | x :: xs ->
           let acc =
             match x with
             | Syntax.Multiline_string str ->
                 Syntax.Interp_lit { str; repr = ""; loc_ = Rloc.no_location }
                 :: acc
             | Multiline_interp interps -> List.rev_append interps acc
           in
           if xs = [] then acc else loop xs (newline :: acc)
     in
     let rev_interps = loop elems [] in
     let elems =
       List.fold_left
         (fun acc ->
           fun interp ->
            match (interp, acc) with
            | _, [] -> [ interp ]
            | ( Syntax.Interp_lit { str = s1; repr = _ },
                Syntax.Interp_lit { str = s2; repr = _; loc_ } :: acc ) ->
                Interp_lit { str = s1 ^ s2; repr = ""; loc_ } :: acc
            | _, _ -> interp :: acc)
         [] rev_interps
     in
     Syntax.Pexpr_interp { elems; loc_ }
    : Syntax.expr)

let expect_bool = Expect_type Stype.bool

let rec infer_expr (env : Local_env.t) (expr : Syntax.expr)
    ~(control_ctx : Control_ctx.t) ~(tvar_env : Tvar_env.t)
    ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let check_go =
     check_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let infer_go =
     infer_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   match expr with
   | Pexpr_array_get _ -> assert false
   | Pexpr_array_set _ -> assert false
   | Pexpr_array_augmented_set _ -> assert false
   | Pexpr_unary _ -> assert false
   | Pexpr_array_get_slice { array; start_index; end_index; index_loc_; loc_ }
     ->
       let fallback () =
         let desugared_expr =
           Parsing_util.desugar_array_get_slice ~loc_ ~index_loc_ array
             start_index end_index
         in
         infer_go env desugared_expr
           [@@inline]
       in
       if start_index = None && end_index = None then
         let array = infer_go env array in
         match Typedtree_util.type_of_typed_expr array with
         | T_constr { type_constructor; tys }
           when Type_path.equal type_constructor
                  Type_path.Builtin.type_path_array ->
             let ty : Stype.t =
               T_constr
                 {
                   type_constructor = Type_path.Builtin.type_path_arrayview;
                   tys;
                   generic_ = false;
                   is_suberror_ = false;
                 }
             in
             Texpr_array_as_view { array; ty; loc_ }
         | _ -> fallback ()
       else fallback ()
   | Pexpr_constr { constr; loc_ } ->
       typing_constr_or_constant constr None ~expect_ty:None ~env ~loc:loc_
         ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_apply { func = Pexpr_constr { constr }; args; attr; loc_ } ->
       (if attr <> No_attr then
          let error =
            Errors.invalid_apply_attr ~kind:`Constructor
              ~attr:(Syntax.string_of_apply_attr attr)
              ~loc:loc_
          in
          add_error diagnostics error);
       typing_constr_or_constant constr (Some args) ~expect_ty:None ~env
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_apply { func; args; attr; loc_ } ->
       typing_application env (infer_go env func) args
         ~kind:(Normal : Typedtree.apply_kind)
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~attr ~diagnostics
   | Pexpr_is { expr; pat; loc_ } ->
       let expr = infer_go env expr in
       let expr_ty = Typedtree_util.type_of_typed_expr expr in
       let pat_binders, pat =
         Pattern_typer.check_pat pat expr_ty ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       Texpr_is { expr; pat; pat_binders; loc_ }
   | Pexpr_infix { op = { var_name = Lident "&&" }; lhs; rhs; loc_ } ->
       let tlhs = check_go env lhs expect_bool in
       let pat_binders = Typedtree_util.pat_binders_of_cond tlhs in
       let trhs =
         check_go (Typeutil.add_pat_binders env pat_binders) rhs expect_bool
       in
       Texpr_and { lhs = tlhs; rhs = trhs; loc_ }
   | Pexpr_infix { op = { var_name = Lident "||" }; lhs; rhs; loc_ } ->
       let tlhs = check_go env lhs expect_bool in
       let trhs = check_go env rhs expect_bool in
       Texpr_or { lhs = tlhs; rhs = trhs; loc_ }
   | Pexpr_infix
       {
         op = { var_name = Lident ("..=" | "..<"); loc_ = infix_loc_ };
         loc_;
         _;
       } ->
       add_error diagnostics (Errors.range_operator_only_in_for infix_loc_);
       Texpr_hole
         { ty = Stype.new_type_var Tvar_error; loc_; kind = Synthesized }
   | Pexpr_infix { op; lhs; rhs; loc_ } ->
       typing_infix_op env op (infer_go env lhs) rhs ~loc:loc_ ~global_env ~cenv
         ~tvar_env ~control_ctx ~diagnostics
   | Pexpr_array { exprs; loc_ } ->
       let ty = Stype.new_type_var Tvar_normal in
       let tes = Lst.map exprs (fun e -> check_go env e (Expect_type ty)) in
       Texpr_array
         {
           exprs = tes;
           ty =
             Stype.T_constr
               {
                 type_constructor = Type_path.Builtin.type_path_array;
                 tys = [ ty ];
                 generic_ = false;
                 is_suberror_ = false;
               };
           is_fixed_array = false;
           loc_;
         }
   | Pexpr_array_spread { elems; loc_ } ->
       typing_array_spread env elems ~kind:`Array ~loc:loc_ ~expect_ty:None
         ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_constant { c; loc_ } ->
       let ty, c =
         Typeutil.typing_constant c ~expect_ty:None ~loc:loc_ ~diagnostics
       in
       Texpr_constant { c; ty; name_ = None; loc_ }
   | Pexpr_multiline_string { elems; loc_ } ->
       infer_go env (desugar_multiline_string ~loc_ elems)
   | Pexpr_interp { elems; loc_ } ->
       typing_interp ~control_ctx ~global_env ~diagnostics ~tvar_env ~cenv env
         elems loc_
   | Pexpr_constraint { expr = e; ty; loc_ } ->
       let ty = typing_type ~tvar_env ~global_env ty ~diagnostics in
       let stype = Typedtree_util.stype_of_typ ty in
       Texpr_constraint
         {
           expr = check_go env e (Expect_type stype);
           konstraint = ty;
           ty = stype;
           loc_;
         }
   | Pexpr_while { loop_cond; loop_body; loc_; while_else; label } ->
       check_while env loop_cond loop_body while_else ~label
         ~expect_ty:(Expect_type (Stype.new_type_var Tvar_normal))
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_function
       {
         func =
           Lambda
             {
               parameters;
               params_loc_;
               body;
               return_type;
               kind_;
               has_error;
               is_async;
             };
         loc_;
       } ->
       let tfun, ty =
         infer_function env parameters params_loc_ body return_type ~has_error
           ~is_async ~kind_ ~tvar_env ~cenv ~global_env ~diagnostics
       in
       Texpr_function { func = tfun; ty; is_raw_ = false; loc_ }
   | Pexpr_function { func = Match _ } -> assert false
   | Pexpr_ident { id; loc_ } -> (
       let value_info =
         find_value id.var_name ~env ~global_env ~diagnostics ~loc:loc_
       in
       match value_info with
       | Local_imm { id = var_id; typ } ->
           let id : Typedtree.var = { var_id; loc_ = id.loc_ } in
           let ty, (kind : Typedtree.value_kind) =
             Type.deref_constr_type_to_local_value typ
           in
           Texpr_ident { id; kind; ty; ty_args_ = [||]; arity_ = None; loc_ }
       | Local_mut { id = var_id; typ } ->
           let id : Typedtree.var = { var_id; loc_ = id.loc_ } in
           Texpr_ident
             {
               id;
               kind = Mutable;
               ty = typ;
               ty_args_ = [||];
               arity_ = None;
               loc_;
             }
       | Toplevel_value
           {
             id = qid;
             typ;
             ty_params_;
             arity_;
             kind;
             doc_;
             direct_use_loc_;
             attrs;
             _;
           } ->
           Checked_attributes.check_alerts ~diagnostics attrs loc_;
           Docstring.check_alerts ~diagnostics (Docstring.pragmas doc_) loc_;
           let qid =
             match direct_use_loc_ with
             | Explicit_import _ | Implicit_import_all _ ->
                 Basic_qual_ident.make_implicit_pkg
                   ~pkg:(Basic_qual_ident.get_pkg qid)
                   ~name:(Basic_qual_ident.base_name qid)
             | Not_direct_use -> qid
           in
           let id : Typedtree.var =
             { var_id = Ident.of_qual_ident qid; loc_ = id.loc_ }
           in
           let kind : Typedtree.value_kind =
             match kind with
             | Normal -> Normal
             | Prim prim -> Prim prim
             | Const _ -> assert false
           in
           if Tvar_env.is_empty ty_params_ then
             Texpr_ident { id; kind; ty = typ; ty_args_ = [||]; arity_; loc_ }
           else
             let ty, ty_args_ =
               Poly_type.instantiate_value ~cenv ~loc:loc_ ty_params_ typ
             in
             Texpr_ident { id; kind; ty; ty_args_; arity_; loc_ })
   | Pexpr_if { cond; ifso; ifnot; loc_ } -> (
       let cond = check_go env cond expect_bool in
       let ifso_env =
         Typeutil.add_pat_binders env (Typedtree_util.pat_binders_of_cond cond)
       in
       match ifnot with
       | None ->
           let ifso = check_go ifso_env ifso Ignored in
           Texpr_if { cond; ifso; ifnot = None; ty = Stype.unit; loc_ }
       | Some ifnot ->
           let ifso = infer_go ifso_env ifso in
           let ty = Typedtree_util.type_of_typed_expr ifso in
           let ifnot = check_go env ifnot (Expect_type ty) in
           Texpr_if { cond; ifso; ifnot = Some ifnot; ty; loc_ })
   | Pexpr_guard { cond; otherwise; body; loc_ } ->
       let cond = check_go env cond expect_bool in
       let guard_env =
         Typeutil.add_pat_binders env (Typedtree_util.pat_binders_of_cond cond)
       in
       let body = infer_go guard_env body in
       let ty = Typedtree_util.type_of_typed_expr body in
       let otherwise =
         match otherwise with
         | None -> None
         | Some e2 -> Some (check_go env e2 (Expect_type ty))
       in
       Texpr_guard { cond; otherwise; body; ty; loc_ }
   | Pexpr_guard_let { pat; expr; otherwise; body; loc_ } ->
       Local_diagnostics.add_warning diagnostics
         {
           kind =
             Deprecated_syntax
               {
                 old_usage = "guard let";
                 purpose = "shorthand of pattern match";
                 new_usage = Some "guard with is-expression";
               };
           loc = Syntax.loc_of_pattern pat;
         };
       let pat_binders, pat, rhs =
         typing_let env pat expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let body = infer_go (Typeutil.add_pat_binders env pat_binders) body in
       let ty = Typedtree_util.type_of_typed_expr body in
       let otherwise =
         match otherwise with
         | None -> None
         | Some cases ->
             Some
               (check_cases env cases
                  ~pat_ty:(Typedtree_util.type_of_typed_expr rhs)
                  ~action_ty:(Expect_type ty) ~tvar_env ~cenv ~global_env
                  ~diagnostics ~control_ctx)
       in
       if Check_match.irrefutable_pattern pat then
         Local_diagnostics.add_warning diagnostics
           { kind = Unused_guard; loc = Typedtree.loc_of_pat pat };
       Texpr_guard_let { pat; rhs; pat_binders; otherwise; body; ty; loc_ }
   | Pexpr_try
       {
         body;
         catch;
         catch_all;
         try_else;
         try_loc_;
         catch_loc_;
         else_loc_;
         loc_;
       } ->
       let error_ctx = ref (Control_ctx.Open_ctx Empty_ctx) in
       let new_ctx = Control_ctx.with_error_ctx ~error_ctx control_ctx in
       let body =
         infer_expr ~control_ctx:new_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics env body
       in
       let body_ty = Typedtree_util.type_of_typed_expr body in
       let ty =
         if try_else = None then body_ty else Stype.new_type_var Tvar_normal
       in
       let error_ty = Control_ctx.error_ctx_to_stype !error_ctx in
       let catch =
         check_cases env catch ~pat_ty:error_ty ~action_ty:(Expect_type ty)
           ~tvar_env ~cenv ~global_env ~diagnostics ~control_ctx
       in
       let try_else =
         match try_else with
         | None -> None
         | Some try_else ->
             Some
               (check_cases env try_else ~pat_ty:body_ty
                  ~action_ty:(Expect_type ty) ~tvar_env ~cenv ~global_env
                  ~diagnostics ~control_ctx)
       in
       if not !(new_ctx.may_has_error) then (
         Local_diagnostics.add_warning diagnostics
           { kind = Useless_try; loc = try_loc_ };
         match Stype.type_repr error_ty with
         | Tvar link -> link := Tlink Stype.unit
         | _ -> ());
       if catch_all then (
         control_ctx.may_has_error := true;
         match control_ctx.error_ctx with
         | Some error_ctx ->
             store_error ~diagnostics
               (Control_ctx.check_error_in_ctx ~error_ty ~ctx:error_ctx
                  catch_loc_)
         | None ->
             add_error diagnostics
               (Errors.invalid_raise ~kind:`Catchall ~loc:catch_loc_));
       Texpr_try
         {
           body;
           catch;
           catch_all;
           try_else;
           ty;
           err_ty = error_ty;
           catch_loc_;
           else_loc_;
           loc_;
         }
   | Pexpr_letrec { bindings = funs; body; loc_ } ->
       let env_with_funs, tfuns =
         typing_letrec env funs ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let body = infer_go env_with_funs body in
       Texpr_letrec
         {
           bindings = tfuns;
           body;
           ty = Typedtree_util.type_of_typed_expr body;
           loc_;
         }
   | Pexpr_letfn { name; func; body; loc_ } -> (
       let env_with_funs, tfuns =
         typing_letrec env
           [ (name, func) ]
           ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let body = infer_go env_with_funs body in
       match tfuns with
       | (binder, fn) :: [] ->
           Texpr_letfn
             {
               binder;
               fn;
               body;
               ty = Typedtree_util.type_of_typed_expr body;
               is_rec = Typedtree_util.is_rec binder fn;
               loc_;
             }
       | _ -> assert false)
   | Pexpr_let { pattern = p; expr = e1; body = e2; loc_; _ } ->
       let pat_binders, tp, te1 =
         typing_let env p e1 ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let body = infer_go (Typeutil.add_pat_binders env pat_binders) e2 in
       Texpr_let
         {
           pat = tp;
           rhs = te1;
           body;
           ty = Typedtree_util.type_of_typed_expr body;
           pat_binders;
           loc_;
         }
   | Pexpr_sequence { exprs; last_expr; loc_ } ->
       let exprs = Lst.map exprs (fun expr -> check_go env expr Ignored) in
       let last_expr = infer_go env last_expr in
       let ty = Typedtree_util.type_of_typed_expr last_expr in
       Texpr_sequence { exprs; last_expr; ty; loc_ }
   | Pexpr_tuple { exprs; loc_ } ->
       let tes = Lst.map exprs (infer_go env) in
       Texpr_tuple
         {
           exprs = tes;
           ty =
             Builtin.type_product
               (Lst.map tes Typedtree_util.type_of_typed_expr);
           loc_;
         }
   | Pexpr_record { type_name; fields; loc_; trailing = _ } -> (
       let fallback type_name =
         infer_record env fields loc_ ~type_name ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
           [@@inline]
       in
       match type_name with
       | Some type_name -> (
           match
             typing_type_name ~global_env ~tvar_env type_name ~diagnostics
           with
           | Some
               ( Tname_defined
                   {
                     ty_constr;
                     ty_desc = Record_type { has_private_field_ = true; _ };
                     _;
                   },
                 type_name )
             when Type_path_util.is_foreign ty_constr ->
               let name = Type_path_util.name ty_constr in
               add_error diagnostics
                 (Errors.cannot_create_struct_with_priv_field ~name ~loc:loc_);
               fallback (Some type_name)
           | Some
               ( Tname_defined
                   ({ ty_desc = Record_type { fields = labels }; _ } as ty_decl),
                 type_name ) ->
               Checked_attributes.check_alerts ~diagnostics ty_decl.ty_attrs
                 loc_;
               Docstring.check_alerts ~diagnostics
                 (Docstring.pragmas ty_decl.ty_doc_)
                 loc_;
               let expect_ty : Stype.t =
                 T_constr
                   {
                     type_constructor = ty_decl.ty_constr;
                     tys = Type.type_var_list ty_decl.ty_arity Tvar_normal;
                     generic_ = false;
                     is_suberror_ = false;
                   }
               in
               let _, labels =
                 Poly_type.instantiate_record ~ty_record:(`Known expect_ty)
                   labels
               in
               type_guided_record_check fields labels expect_ty
                 ~type_name:(Some type_name) ~env ~control_ctx ~tvar_env ~cenv
                 ~global_env ~diagnostics ~loc:loc_
           | Some
               (Tname_local_type ({ kind = Struct labels; _ } as t), type_name)
             ->
               let expect_ty : Stype.t =
                 T_constr
                   {
                     type_constructor =
                       Type_path.local_type t.toplevel_id t.name;
                     tys = Tvar_env.get_types t.ty_params_;
                     generic_ = false;
                     is_suberror_ = false;
                   }
               in
               type_guided_record_check fields labels expect_ty
                 ~type_name:(Some type_name) ~env ~control_ctx ~tvar_env ~cenv
                 ~global_env ~diagnostics ~loc:loc_
           | Some (_, tast_type_name) ->
               add_error diagnostics
                 (Errors.not_a_record_type
                    ~name:(Longident.to_string type_name.name)
                    ~loc:type_name.loc_);
               fallback (Some tast_type_name)
           | None -> fallback None)
       | None -> fallback None)
   | Pexpr_record_update { type_name; record; fields; loc_ } ->
       let type_name, expected_record =
         match type_name with
         | Some type_name -> (
             match
               typing_type_name ~global_env ~tvar_env type_name ~diagnostics
             with
             | Some
                 ( Tname_defined ({ ty_desc = Record_type _; _ } as ty_decl),
                   type_name ) ->
                 let expect_ty : Stype.t =
                   T_constr
                     {
                       type_constructor = ty_decl.ty_constr;
                       tys = Type.type_var_list ty_decl.ty_arity Tvar_normal;
                       generic_ = false;
                       is_suberror_ = false;
                     }
                 in
                 (Some type_name, check_go env record (Expect_type expect_ty))
             | Some (Tname_local_type ({ kind = Struct _; _ } as t), type_name)
               ->
                 let expect_ty : Stype.t =
                   T_constr
                     {
                       type_constructor =
                         Type_path.local_type t.toplevel_id t.name;
                       tys = Tvar_env.get_types t.ty_params_;
                       generic_ = false;
                       is_suberror_ = false;
                     }
                 in
                 (Some type_name, check_go env record (Expect_type expect_ty))
             | Some (_, tast_type_name) ->
                 add_error diagnostics
                   (Errors.not_a_record_type
                      ~name:(Longident.to_string type_name.name)
                      ~loc:type_name.loc_);
                 (Some tast_type_name, infer_go env record)
             | None -> (None, infer_go env record))
         | None -> (None, infer_go env record)
       in
       let expected_record_ty =
         Typedtree_util.type_of_typed_expr expected_record
       in
       let typed_fields, all_fields =
         match
           Global_env.labels_of_record global_env ~loc:loc_ ~context:`Update
             expected_record_ty
         with
         | Ok all_fields ->
             let _, all_fields =
               Poly_type.instantiate_record
                 ~ty_record:(`Known expected_record_ty) all_fields
             in
             ( type_guided_record_update_check env all_fields fields
                 expected_record_ty ~control_ctx ~tvar_env ~cenv ~global_env
                 ~diagnostics,
               all_fields )
         | Error e ->
             add_error diagnostics e;
             let pseudo_typed =
               Lst.map fields (fun (Field_def { label; expr; _ }) ->
                   Typedtree.Field_def
                     {
                       label;
                       expr = infer_go env expr;
                       is_mut = false;
                       is_pun = false;
                       pos = 0;
                     })
             in
             (pseudo_typed, [])
       in
       Texpr_record_update
         {
           type_name;
           record = expected_record;
           all_fields;
           fields = typed_fields;
           ty = expected_record_ty;
           loc_;
         }
   | Pexpr_field
       {
         record = tuple;
         accessor = Index { tuple_index = index } as accessor;
         loc_;
       } ->
       let tuple = infer_go env tuple in
       let src_ty = Stype.type_repr (Typedtree_util.type_of_typed_expr tuple) in
       let tuple = deref_newtype ~global_env ~loc:loc_ tuple in
       let ty_tuple = Typedtree_util.type_of_typed_expr tuple in
       let ty =
         match
           Type.filter_product ~src_ty ~blame:Filtered_type ~arity:None ty_tuple
             loc_
         with
         | Ok tys -> (
             match Lst.nth_opt tys index with
             | Some ty -> ty
             | None ->
                 add_error diagnostics
                   (Errors.no_tuple_index ~required:index
                      ~actual:(List.length tys) ~loc:loc_);
                 Stype.new_type_var Tvar_error)
         | Partial (_, errs) ->
             Basic_lst.iter errs ~f:(add_error diagnostics);
             Stype.new_type_var Tvar_error
       in
       Texpr_field { record = tuple; accessor; ty; pos = index; loc_ }
   | Pexpr_field { record; accessor = Newtype; loc_ } -> (
       let record = infer_go env record in
       let ty_record = Typedtree_util.type_of_typed_expr record in
       match Global_env.get_newtype_info global_env ty_record with
       | None ->
           add_error diagnostics
             (Errors.not_a_newtype
                ~actual_ty:(Printer.type_to_string ty_record)
                ~loc:loc_);
           Texpr_field
             {
               record;
               accessor = Newtype;
               ty = Stype.new_type_var Tvar_error;
               pos = Typeutil.unknown_pos;
               loc_;
             }
       | Some info -> (
           let ty_res, ty_args =
             Poly_type.instantiate_constr info.newtype_constr
           in
           Ctype.unify_exn ty_res ty_record;
           match ty_args with
           | ty :: [] ->
               Texpr_field { record; accessor = Newtype; ty; pos = 0; loc_ }
           | _ -> assert false))
   | Pexpr_field { record; accessor = Label label as accessor; loc_ } ->
       let name = label.label_name in
       let record = infer_go env record in
       let ty_record =
         Stype.type_repr (Typedtree_util.type_of_typed_expr record)
       in
       let may_be_method () =
         match
           Type_constraint.resolve_method_by_type ty_record name ~tvar_env
             ~global_env ~src:Dot_src_direct ~loc:loc_
         with
         | Ok _ -> true
         | Error _ -> false
           [@@inline]
       in
       let actual_record = deref_newtype ~global_env ~loc:loc_ record in
       let ty_field, pos, _, _ =
         resolve_field ~global_env actual_record label ~may_be_method
           ~src_ty_record:ty_record ~diagnostics ~loc:loc_
       in
       Texpr_field
         { record = actual_record; accessor; ty = ty_field; pos; loc_ }
   | Pexpr_method { type_name; method_name; loc_ } -> (
       let fallback type_name err =
         (add_error diagnostics err;
          Texpr_method
            {
              type_name;
              meth =
                {
                  var_id = Ident.fresh method_name.label_name;
                  loc_ = method_name.loc_;
                };
              ty_args_ = [||];
              arity_ = None;
              prim = None;
              ty = Stype.new_type_var Tvar_error;
              loc_;
            }
           : Typedtree.expr)
           [@@inline]
       in
       let resolve_by_typename (type_name : Typedtree.type_name) =
         (match
            Type_constraint.resolve_method_by_type_name type_name
              method_name.label_name ~loc:loc_ ~tvar_env ~global_env
          with
          | Ok (Known_method method_info) ->
              Checked_attributes.check_alerts ~diagnostics method_info.attrs
                loc_;
              Docstring.check_alerts ~diagnostics
                (Docstring.pragmas method_info.doc_)
                loc_;
              let ty, ty_args_ =
                Poly_type.instantiate_method ~cenv ~loc:loc_ method_info
              in
              Texpr_method
                {
                  type_name;
                  meth =
                    {
                      var_id = Ident.of_qual_ident method_info.id;
                      loc_ = method_name.loc_;
                    };
                  prim = method_info.prim;
                  ty;
                  ty_args_;
                  arity_ = Some method_info.arity_;
                  loc_;
                }
          | Ok (Promised_method { method_id; method_ty; method_arity; prim }) ->
              Texpr_method
                {
                  type_name;
                  meth = { var_id = method_id; loc_ = method_name.loc_ };
                  prim;
                  ty = method_ty;
                  ty_args_ = [||];
                  arity_ = Some method_arity;
                  loc_;
                }
          | Error err -> fallback type_name err
           : Typedtree.expr)
           [@@inline]
       in
       match typing_type_name ~global_env ~tvar_env type_name ~diagnostics with
       | Some (Tname_trait { name; vis_ = Vis_default; _ }, type_name)
         when Type_path_util.is_foreign name ->
           fallback type_name
             (Errors.cannot_use_method_of_abstract_trait ~trait:name
                ~method_name:method_name.label_name ~loc:method_name.loc_)
       | Some (Tname_trait decl, type_name) -> (
           match
             Trait_decl.find_method decl method_name.label_name ~loc:loc_
           with
           | Error err -> fallback type_name err
           | Ok meth_decl ->
               let self_type = Stype.new_type_var Tvar_normal in
               let ty =
                 Poly_type.instantiate_method_decl meth_decl ~self:self_type
               in
               Basic_lst.iter
                 ~f:(Poly_type.add_constraint cenv self_type)
                 (Trait_closure.compute_closure
                    ~types:(Global_env.get_all_types global_env)
                    [ { trait = decl.name; loc_; src_ = Direct } ]);
               Texpr_unresolved_method
                 {
                   trait_name = type_name;
                   method_name = method_name.label_name;
                   self_type;
                   arity_ = Some meth_decl.method_arity;
                   ty;
                   loc_;
                 })
       | Some (_, type_name) -> resolve_by_typename type_name
       | None ->
           Texpr_hole
             { ty = Stype.new_type_var Tvar_error; loc_; kind = Synthesized })
   | Pexpr_dot_apply { self; method_name; args; return_self; attr; loc_ } ->
       let self = infer_go env self in
       let ty_self = Typedtree_util.type_of_typed_expr self in
       let method_expr =
         typing_self_method ty_self method_name
           ~src:Type_constraint.Dot_src_direct ~loc:method_name.loc_ ~tvar_env
           ~cenv ~global_env ~diagnostics
       in
       typing_application env method_expr args ~self
         ~kind:(if return_self then Dot_return_self else Dot)
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~attr ~diagnostics
   | Pexpr_as { expr; trait; loc_ } -> (
       let expr = infer_go env expr in
       match typing_type_name trait ~tvar_env ~global_env ~diagnostics with
       | Some
           ((Tname_trait trait_decl | Tname_trait_object trait_decl), type_name)
         ->
           if not trait.is_object then
             Local_diagnostics.add_warning diagnostics
               {
                 loc = Typedtree.loc_of_type_name type_name;
                 kind =
                   Deprecated_syntax
                     {
                       old_usage = "`Trait`";
                       purpose = "trait object type";
                       new_usage = Some "`&Trait`";
                     };
               };
           (match
              Trait_decl.check_object_safety
                ~name:(Type_path_util.name trait_decl.name)
                ~loc:trait.loc_ trait_decl.object_safety_
            with
           | Some x -> add_error diagnostics x
           | _ -> ());
           let closure =
             Trait_closure.compute_closure
               ~types:(Global_env.get_all_types global_env)
               [ { trait = trait_decl.name; loc_; src_ = Direct } ]
           in
           Basic_lst.iter closure
             ~f:
               (Poly_type.add_constraint cenv
                  (Typedtree_util.type_of_typed_expr expr));
           Texpr_as
             {
               expr;
               trait = type_name;
               ty = T_trait trait_decl.name;
               is_implicit = false;
               loc_;
             }
       | Some (_, type_name) ->
           add_error diagnostics
             (Errors.not_a_trait
                ~name:(Longident.to_string trait.name)
                ~loc:trait.loc_);
           Texpr_as
             {
               expr;
               trait = type_name;
               ty = Stype.new_type_var Tvar_error;
               is_implicit = false;
               loc_;
             }
       | None ->
           let tpath =
             match trait.name with
             | Lident name ->
                 Type_path.toplevel_type ~pkg:!Basic_config.current_package name
             | Ldot { pkg; id } -> Type_path.toplevel_type ~pkg id
           in
           Texpr_as
             {
               expr;
               trait =
                 Tname_path { name = tpath; kind = Type; loc_ = trait.loc_ };
               ty = Stype.new_type_var Tvar_error;
               is_implicit = false;
               loc_;
             })
   | Pexpr_mutate { record; accessor; field; augmented_by; loc_ } -> (
       match accessor with
       | Label label ->
           typing_mutate env record label field augmented_by loc_ ~control_ctx
             ~tvar_env ~cenv ~global_env ~diagnostics
       | Index _ | Newtype ->
           add_error diagnostics (Errors.tuple_not_mutable loc_);
           Texpr_tuple { exprs = []; ty = Stype.unit; loc_ })
   | Pexpr_match { expr; cases; match_loc_; loc_ } ->
       let expr = infer_go env expr in
       let ty1 = Typedtree_util.type_of_typed_expr expr in
       let ty2 = Stype.new_type_var Tvar_normal in
       let trows =
         check_cases ~tvar_env ~cenv ~global_env ~diagnostics ~control_ctx env
           cases ~pat_ty:ty1 ~action_ty:(Expect_type ty2)
       in
       Texpr_match { expr; cases = trows; ty = ty2; match_loc_; loc_ }
   | Pexpr_pipe { lhs; rhs; loc_ } ->
       typing_pipe env lhs rhs ~loc:loc_ ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics
   | Pexpr_letmut { binder; ty; expr; body; loc_ } ->
       let env, binder, expr, konstraint =
         typing_letmut env binder ty expr ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
       in
       let body = infer_go env body in
       let ty_body = Typedtree_util.type_of_typed_expr body in
       Texpr_letmut { binder; konstraint; expr; body; ty = ty_body; loc_ }
   | Pexpr_assign { var; expr; augmented_by; loc_ } -> (
       let check_rhs var ty_of_var =
         (match augmented_by with
          | None ->
              let expr = check_go env expr (Expect_type ty_of_var) in
              Texpr_assign
                { var; expr; augmented_by = None; ty = Stype.unit; loc_ }
          | Some op ->
              let tlhs : Typedtree.expr =
                Texpr_ident
                  {
                    id = var;
                    kind = Mutable;
                    ty = ty_of_var;
                    ty_args_ = [||];
                    arity_ = None;
                    loc_ = var.loc_;
                  }
              in
              let infix_expr =
                typing_infix_op env op tlhs expr ~loc:loc_ ~global_env ~cenv
                  ~tvar_env ~control_ctx ~diagnostics
              in
              let op_expr, expr =
                match infix_expr with
                | Texpr_apply
                    { func; args = [ _; { arg_value; _ } ]; ty = actual_ty; _ }
                  ->
                    unify_expr_and_store_error ~expect_ty:ty_of_var ~actual_ty
                      ~diagnostics loc_;
                    (func, arg_value)
                | _ -> assert false
              in
              Texpr_assign
                {
                  var;
                  expr;
                  augmented_by = Some op_expr;
                  ty = Stype.unit;
                  loc_;
                }
           : Typedtree.expr)
           [@@local]
       in
       let value_info =
         find_value var.var_name ~env ~global_env ~diagnostics ~loc:loc_
       in
       match value_info with
       | Local_mut { id = var_id; typ } ->
           let id : Typedtree.var = { var_id; loc_ = var.loc_ } in
           check_rhs id typ
       | Local_imm { id = var_id; typ } ->
           let id : Typedtree.var = { var_id; loc_ = var.loc_ } in
           add_error diagnostics
             (Errors.not_mutable ~id:var.var_name ~loc:var.loc_);
           check_rhs id typ
       | Toplevel_value { id; _ } ->
           add_error diagnostics
             (Errors.not_mutable ~id:var.var_name ~loc:var.loc_);
           let id : Typedtree.var =
             { var_id = Ident.of_qual_ident id; loc_ = var.loc_ }
           in
           check_rhs id (Stype.new_type_var Tvar_error))
   | Pexpr_hole { kind; loc_ } ->
       (match kind with
       | Incomplete -> add_error diagnostics (Errors.found_hole loc_)
       | Synthesized -> ()
       | Todo ->
           control_ctx.may_has_error := true;
           Local_diagnostics.add_warning diagnostics { kind = Todo; loc = loc_ });
       Texpr_hole { ty = Stype.new_type_var Tvar_normal; loc_; kind }
   | Pexpr_unit { loc_; _ } -> Texpr_unit { loc_ }
   | Pexpr_break { arg; loc_; label } ->
       check_break env arg ~loc:loc_ ~label
         ~expect_ty:(Stype.new_type_var Tvar_normal)
         ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
   | Pexpr_continue { args; loc_; label } ->
       check_continue env args ~loc:loc_ ~label
         ~expect_ty:(Stype.new_type_var Tvar_normal)
         ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
   | Pexpr_loop { args; body; label; loop_loc_; loc_ } ->
       check_loop ~label ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
         env args body ~loc:loc_ ~loop_loc_
         ~expect_ty:(Expect_type (Stype.new_type_var Tvar_normal))
   | Pexpr_for
       { binders; condition; continue_block; body; label; loc_; for_else } ->
       check_for ~label ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
         ~expect_ty:(Expect_type (Stype.new_type_var Tvar_normal))
         env binders condition continue_block body for_else ~loc:loc_
   | Pexpr_foreach
       {
         binders;
         expr =
           Pexpr_infix
             {
               op = { var_name = Lident (("..<" | "..=") as op) };
               lhs;
               rhs;
               loc_ = operator_loc;
             };
         body;
         else_block;
         label;
         loc_;
       } ->
       let inclusive = op = "..=" in
       typing_range_for_in env binders lhs rhs body else_block ~label ~inclusive
         ~operator_loc ~loc:loc_
         ~expect_ty:(Expect_type (Stype.new_type_var Tvar_normal))
         ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
   | Pexpr_foreach { binders; expr; body; else_block; label; loc_ } ->
       typing_foreach ~label ~global_env ~tvar_env ~cenv ~control_ctx
         ~diagnostics
         ~expect_ty:(Expect_type (Stype.new_type_var Tvar_normal))
         env binders expr body else_block ~loc:loc_
   | Pexpr_return { return_value; loc_ } ->
       let ty = Stype.new_type_var Tvar_normal in
       check_return_value env return_value ty ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics ~loc:loc_
   | Pexpr_raise { err_value; loc_ } ->
       let ty = Stype.new_type_var Tvar_normal in
       typing_raise env err_value ty ~loc:loc_ ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics
   | Pexpr_map { elems; loc_ } ->
       typing_map_expr env elems ~expect_ty:None ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics ~loc:loc_
   | Pexpr_static_assert { asserts; body } ->
       typing_static_assert ~global_env ~tvar_env ~control_ctx ~diagnostics
         ~expect_ty:None env asserts body
   | Pexpr_group _ -> assert false
    : Typedtree.expr)

and check_cases (env : Local_env.t) ~tvar_env ~cenv ~global_env ~diagnostics
    ~control_ctx (cases : Syntax.case list) ~pat_ty ~action_ty =
  (Lst.map cases (fun { pattern = p; guard; body = action } ->
       let pat_binders, tpat =
         Pattern_typer.check_pat p pat_ty ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let env1 = Typeutil.add_pat_binders env pat_binders in
       let guard, env2 =
         match guard with
         | None -> (None, env1)
         | Some guard ->
             let guard =
               check_expr env1 guard (Expect_type Stype.bool) ~control_ctx
                 ~tvar_env ~cenv ~global_env ~diagnostics
             in
             ( Some guard,
               Typeutil.add_pat_binders env1
                 (Typedtree_util.pat_binders_of_cond guard) )
       in
       let taction =
         check_expr env2 action action_ty ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
       in
       ({ pat = tpat; action = taction; pat_binders; guard }
         : Typedtree.match_case))
    : Typedtree.match_case list)

and check_return_value (env : Local_env.t) (return_value : Syntax.expr option)
    (expected_ty : Stype.t) ~loc ~control_ctx ~tvar_env ~cenv ~global_env
    ~diagnostics =
  (match control_ctx.return with
   | Some ret_ty ->
       let return_value =
         match return_value with
         | Some ret_value ->
             Some
               (check_expr env ret_value (Expect_type ret_ty) ~control_ctx
                  ~tvar_env ~cenv ~global_env ~diagnostics)
         | None ->
             unify_expr_and_store_error ~expect_ty:ret_ty ~actual_ty:Stype.unit
               ~diagnostics loc;
             None
       in
       Texpr_return { return_value; ty = expected_ty; loc_ = loc }
   | None -> (
       add_error diagnostics (Errors.invalid_return loc);
       match return_value with
       | Some ret_value ->
           infer_expr env ret_value ~control_ctx ~tvar_env ~cenv ~global_env
             ~diagnostics
       | None -> Texpr_unit { loc_ = loc })
    : Typedtree.expr)

and typing_raise (env : Local_env.t) (err_value : Syntax.expr)
    (expect_ty : Stype.t) ~loc ~control_ctx ~tvar_env ~cenv ~global_env
    ~diagnostics =
  (let loc_ = loc in
   let make_raise error_value =
     (Texpr_raise { error_value; ty = expect_ty; loc_ } : Typedtree.expr)
       [@@local]
   in
   control_ctx.may_has_error := true;
   match control_ctx.error_ctx with
   | Some error_ctx ->
       let error_value =
         infer_expr env err_value ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let actual_ty = Typedtree_util.type_of_typed_expr error_value in
       Poly_type.add_constraint cenv actual_ty
         {
           trait = Type_path.Builtin.type_path_error;
           loc_ = loc;
           src_ = Direct;
         };
       store_error ~diagnostics
         (Control_ctx.check_error_in_ctx ~error_ty:actual_ty ~ctx:error_ctx loc_);
       make_raise error_value
   | None ->
       add_error diagnostics (Errors.invalid_raise ~kind:`Raise ~loc);
       let error_value =
         infer_expr env err_value ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       Poly_type.add_constraint cenv
         (Typedtree_util.type_of_typed_expr error_value)
         {
           trait = Type_path.Builtin.type_path_error;
           loc_ = loc;
           src_ = Direct;
         };
       Texpr_raise { error_value; ty = expect_ty; loc_ }
    : Typedtree.expr)

and typing_infix_op (env : Local_env.t) (op : Syntax.var) (lhs : Typedtree.expr)
    (rhs : Syntax.expr) ~loc ~global_env ~cenv ~tvar_env ~control_ctx
    ~diagnostics =
  (let op_name =
     match op with { var_name = Lident name; _ } -> name | _ -> assert false
   in
   let op_info = Operators.find_exn op_name in
   let ty_lhs = Typedtree_util.type_of_typed_expr lhs in
   let op_expr, rhs =
     match
       ( Specialize_operator.try_specialize_op ~op_info ~ty_lhs ~loc:op.loc_,
         op_info )
     with
     | Some tmethod, _ -> (tmethod, Not_yet rhs)
     | None, { impl = Regular_function; method_name; _ } ->
         let op_expr =
           infer_expr env
             (Pexpr_ident
                {
                  id = { op with var_name = Lident method_name };
                  loc_ = op.loc_;
                })
             ~global_env ~cenv ~tvar_env ~control_ctx ~diagnostics
         in
         (op_expr, Not_yet rhs)
     | None, { impl = Trait_method trait; method_name; _ } ->
         let op : Typedtree.expr =
           match Global_env.find_trait_by_path global_env trait with
           | None ->
               add_error diagnostics
                 (Errors.pkg_not_imported ~name:(Type_path.get_pkg trait)
                    ~action:
                      (("use operator `" ^ op_name ^ "`"
                        : Stdlib.String.t)
                        [@merlin.hide])
                    ~loc:op.loc_);
               Texpr_hole
                 {
                   ty = Stype.new_type_var Tvar_error;
                   kind = Synthesized;
                   loc_ = op.loc_;
                 }
           | Some trait_decl -> (
               match
                 Trait_decl.find_method trait_decl method_name ~loc:op.loc_
               with
               | Error err ->
                   add_error diagnostics err;
                   Texpr_hole
                     {
                       ty = Stype.new_type_var Tvar_error;
                       kind = Synthesized;
                       loc_ = op.loc_;
                     }
               | Ok meth_decl ->
                   let trait_name = trait_decl.name in
                   let self_type = Stype.new_type_var Tvar_normal in
                   let ty =
                     Poly_type.instantiate_method_decl meth_decl ~self:self_type
                   in
                   Basic_lst.iter
                     ~f:(Poly_type.add_constraint cenv self_type)
                     (Trait_closure.compute_closure
                        ~types:(Global_env.get_all_types global_env)
                        [
                          { trait = trait_name; loc_ = op.loc_; src_ = Direct };
                        ]);
                   let trait_name : Typedtree.type_name =
                     Tname_path
                       {
                         name = trait_name;
                         kind = Trait;
                         loc_ = Rloc.no_location;
                       }
                   in
                   Texpr_unresolved_method
                     {
                       trait_name;
                       method_name;
                       self_type;
                       arity_ = Some meth_decl.method_arity;
                       ty;
                       loc_ = op.loc_;
                     })
         in
         (op, Not_yet rhs)
     | None, { impl = Method | Method_with_default _; _ } ->
         let ty_self, rhs_maybe_typed =
           if not (Typeutil.is_tvar ty_lhs) then (ty_lhs, Not_yet rhs)
           else
             let trhs =
               infer_expr env rhs ~global_env ~cenv ~tvar_env ~control_ctx
                 ~diagnostics
             in
             let ty_rhs = Typedtree_util.type_of_typed_expr trhs in
             if not (Typeutil.is_tvar ty_rhs) then (ty_rhs, Typechecked trhs)
             else
               match op_info.impl with
               | Method_with_default
                   (( T_unit | T_bool | T_byte | T_int16 | T_uint16 | T_char
                    | T_int | T_int64 | T_uint | T_uint64 | T_float | T_double
                    | T_string ) as default) ->
                   let self_ty : Stype.t =
                     T_constr
                       {
                         type_constructor = default;
                         tys = [];
                         generic_ = false;
                         is_suberror_ = false;
                       }
                   in
                   (self_ty, Typechecked trhs)
               | Method_with_default
                   ( T_option | T_result | T_error_value_result | T_fixedarray
                   | T_bytes | T_ref | T_error | Toplevel _ | Tuple _ | Constr _
                   | T_local _ ) ->
                   assert false
               | Method | Regular_function | Trait_method _ ->
                   (ty_lhs, Typechecked trhs)
         in
         let label : Syntax.label =
           { label_name = op_info.method_name; loc_ = op.loc_ }
         in
         let tmethod =
           typing_self_method ty_self label ~global_env ~tvar_env ~cenv
             ~src:(Type_constraint.Dot_src_infix op_info)
             ~loc:(Rloc.merge (Typedtree.loc_of_typed_expr lhs) op.loc_)
             ~diagnostics
         in
         (tmethod, rhs_maybe_typed)
   in
   match[@warning "-fragile-match"]
     take_info_partial ~diagnostics
       (Type.filter_arrow ~blame:Filtered_type ~has_error:false ~is_async:false
          2
          (Typedtree_util.type_of_typed_expr op_expr)
          loc)
   with
   | [ ty_param1; ty_param2 ], ty_res, ty_err ->
       (match ty_err with
       | None -> ()
       | Some err_ty ->
           let err_ty_str = Printer.type_to_string err_ty in
           let err = Errors.unhandled_error ~err_ty:err_ty_str ~loc in
           add_error diagnostics err);
       let tlhs =
         handle_unify_result ~diagnostics ~expr:lhs
           (unify_expr_allow_trait_upcast ~global_env ~cenv ~expect_ty:ty_param1
              ~actual_ty:(Typedtree_util.type_of_typed_expr lhs)
              (Typedtree.loc_of_typed_expr lhs))
       in
       let trhs =
         maybe_check env rhs ty_param2 ~tvar_env ~control_ctx ~cenv ~global_env
           ~diagnostics
       in
       Texpr_apply
         {
           func = op_expr;
           args =
             [
               { arg_value = tlhs; arg_kind = Positional };
               { arg_value = trhs; arg_kind = Positional };
             ];
           ty = ty_res;
           kind_ = Infix;
           loc_ = loc;
         }
   | _ -> assert false
    : Typedtree.expr)

and typing_application ?(expect_ty : Stype.t option)
    ?(self : Typedtree.expr option) (env : Local_env.t) (func : Typedtree.expr)
    (args : Syntax.argument list) ~(kind : Typedtree.apply_kind) ~loc
    ~control_ctx ~tvar_env ~cenv ~global_env ~(attr : Syntax.apply_attr)
    ~diagnostics =
  (let n = List.length args + match self with None -> 0 | _ -> 1 in
   let func_ty = Stype.type_repr (Typedtree_util.type_of_typed_expr func) in
   let err_ty = Stype.new_type_var Tvar_normal in
   let check_effect actual_err_type ~is_async =
     match attr with
     | (Question | Exclamation | No_attr) when is_async ->
         add_error diagnostics (Errors.async_call_not_marked loc)
     | Question -> (
         match actual_err_type with
         | None ->
             add_error diagnostics
               (Errors.invalid_apply_attr ~kind:`NoErrorType
                  ~attr:(Syntax.string_of_apply_attr attr)
                  ~loc)
         | Some actual_err_ty -> Ctype.unify_exn err_ty actual_err_ty)
     | Exclamation -> (
         control_ctx.may_has_error := true;
         match actual_err_type with
         | None ->
             add_error diagnostics
               (Errors.invalid_apply_attr ~kind:`NoErrorType
                  ~attr:(Syntax.string_of_apply_attr attr)
                  ~loc)
         | Some actual_err_type -> (
             match control_ctx.error_ctx with
             | None ->
                 let error = Errors.invalid_raise ~kind:(`Rethrow "!") ~loc in
                 add_error diagnostics error
             | Some error_ctx ->
                 store_error ~diagnostics
                   (Control_ctx.check_error_in_ctx ~error_ty:actual_err_type
                      ~ctx:error_ctx loc)))
     | Double_exclamation -> (
         control_ctx.may_has_error := true;
         if is_async then
           store_error ~diagnostics
             (Control_ctx.async_is_allowed control_ctx ~loc)
         else add_error diagnostics (Errors.double_excl_not_async_call loc);
         match actual_err_type with
         | None -> ()
         | Some actual_err_type -> (
             match control_ctx.error_ctx with
             | None ->
                 let error = Errors.invalid_raise ~kind:(`Rethrow "!!") ~loc in
                 add_error diagnostics error
             | Some error_ctx ->
                 store_error ~diagnostics
                   (Control_ctx.check_error_in_ctx ~error_ty:actual_err_type
                      ~ctx:error_ctx loc)))
     | No_attr -> (
         match actual_err_type with
         | None -> ()
         | Some actual_err_type ->
             let err_ty = Printer.type_to_string actual_err_type in
             add_error diagnostics (Errors.unhandled_error ~err_ty ~loc))
   in
   let delayed_error_type_check = ref None in
   let ty_params, ty_res, ty_err, should_report_error =
     match func_ty with
     | Tarrow { params_ty; ret_ty; err_ty; is_async } ->
         (match err_ty with
         | Some t when Typeutil.is_tvar t ->
             delayed_error_type_check :=
               Some (fun () -> check_effect err_ty ~is_async)
         | _ -> check_effect err_ty ~is_async);
         if is_async then
           store_error ~diagnostics
             (Control_ctx.async_is_allowed control_ctx ~loc);
         (params_ty, ret_ty, err_ty, true)
     | Tvar ({ contents = Tnolink tvar_kind } as link) ->
         let params_ty = Type.type_var_list n tvar_kind in
         let ret_ty = Stype.new_type_var tvar_kind in
         link :=
           Tlink
             (Tarrow
                {
                  params_ty;
                  ret_ty;
                  err_ty = None;
                  is_async = false;
                  generic_ = false;
                });
         if tvar_kind = Tvar_normal && attr <> No_attr then
           add_error diagnostics
             (Errors.invalid_apply_attr ~kind:`UnknownType
                ~attr:(Syntax.string_of_apply_attr attr)
                ~loc);
         if
           tvar_kind = Tvar_normal
           && Lst.exists args (fun a -> a.arg_kind <> Positional)
         then
           add_error diagnostics
             (Errors.nontoplevel_func_cannot_have_labelled_arg ~loc);
         (params_ty, ret_ty, None, false)
     | _ ->
         let params_ty = Type.type_var_list n Tvar_error in
         let ret_ty = Stype.new_type_var Tvar_error in
         (if func_ty <> T_blackhole then
            let expected, actual =
              ("function type", Printer.type_to_string func_ty)
            in
            add_error diagnostics (Errors.type_mismatch ~expected ~actual ~loc));
         (params_ty, ret_ty, None, false)
   in
   let ty_res_application =
     match kind with
     | Dot_return_self ->
         if not (Ctype.try_unify Stype.unit ty_res) then
           add_error diagnostics
             (Errors.cascade_type_mismatch
                ~actual:(Printer.type_to_string ty_res)
                ~loc);
         Typedtree_util.type_of_typed_expr (Option.get self)
     | Infix | Dot | Normal -> ty_res
   in
   let ty_res_with_attr =
     if attr = Question then
       Stype.make_result_ty ~ok_ty:ty_res_application ~err_ty
     else ty_res_application
   in
   (match expect_ty with
   | Some expect_ty ->
       unify_expr_and_store_error ~expect_ty ~actual_ty:ty_res_with_attr
         ~diagnostics loc
   | None -> ());
   let arity, error_kind =
     match func with
     | Texpr_ident { arity_ = Some arity; _ } -> (arity, "function")
     | Texpr_method { arity_ = Some arity; _ } -> (arity, "method")
     | Texpr_unresolved_method { arity_ = Some arity; _ } -> (arity, "method")
     | Texpr_constr { arity_ = arity; _ } -> (arity, "constructor")
     | _ -> (Fn_arity.simple (List.length ty_params), "function")
   in
   let typ_of_args =
     Fn_arity.to_hashtbl arity ty_params (fun param_kind ->
         fun ty -> (param_kind, ty))
   in
   let seen_labels = Basic_hash_string.create 17 in
   let last_positional_index = ref (-1) in
   let lookup_positional_arg () =
     incr last_positional_index;
     match
       Fn_arity.Hash.find_opt typ_of_args (Positional !last_positional_index)
     with
     | Some (_, typ) -> typ
     | None -> Stype.new_type_var Tvar_error
   in
   let lookup_labelled_arg (label : Syntax.label) =
     (match Basic_hash_string.find_opt seen_labels label.label_name with
     | Some _first_loc ->
         add_error diagnostics
           (Errors.duplicated_fn_label ~label:label.label_name
              ~second_loc:label.loc_)
     | None -> Basic_hash_string.add seen_labels label.label_name label.loc_);
     match Fn_arity.Hash.find_opt typ_of_args (Labelled label.label_name) with
     | Some _ as result -> result
     | None ->
         if should_report_error then
           add_error diagnostics
             (Errors.superfluous_arg_label ~label:label.label_name
                ~kind:error_kind ~loc:label.loc_);
         None
   in
   let check_arg (arg : Syntax.argument) =
     (let ty =
        match arg.arg_kind with
        | Positional -> lookup_positional_arg ()
        | Labelled_option { label; question_loc }
        | Labelled_option_pun { label; question_loc } -> (
            match lookup_labelled_arg label with
            | None -> Stype.new_type_var Tvar_error
            | Some (param_kind, ty) ->
                (match param_kind with
                | Question_optional _ -> ()
                | _ ->
                    add_error diagnostics
                      (Errors.invalid_question_arg_application
                         ~label:label.label_name ~loc:question_loc));
                Builtin.type_option ty)
        | Labelled label | Labelled_pun label -> (
            match lookup_labelled_arg label with
            | None -> Stype.new_type_var Tvar_error
            | Some (_, ty) -> ty)
      in
      {
        arg_value =
          check_expr env arg.arg_value (Expect_type ty) ~control_ctx ~tvar_env
            ~cenv ~global_env ~diagnostics;
        arg_kind = arg.arg_kind;
      }
       : Typedtree.argument)
   in
   let targs : Typedtree.argument list =
     match self with
     | Some self ->
         let actual_ty_self = Typedtree_util.type_of_typed_expr self in
         let expected_ty_self = lookup_positional_arg () in
         unify_expr_and_store_error ~expect_ty:expected_ty_self
           ~actual_ty:actual_ty_self ~diagnostics
           (Typedtree.loc_of_typed_expr self);
         { arg_value = self; arg_kind = Positional } :: Lst.map args check_arg
     | None -> Lst.map args check_arg
   in
   (match !delayed_error_type_check with None -> () | Some check -> check ());
   (if should_report_error then
      let actual = !last_positional_index + 1 in
      let expected = Fn_arity.count_positional arity in
      if actual <> expected then
        add_error diagnostics
          (Errors.fn_arity_mismatch
             ~func_ty:
               (Printer.toplevel_function_type_to_string ~arity
                  (Typedtree_util.type_of_typed_expr func))
             ~expected ~actual
             ~has_label:
               ((not (Fn_arity.is_simple arity))
               || Lst.exists args (fun arg ->
                      match arg.arg_kind with
                      | Positional -> false
                      | Labelled _ | Labelled_pun _ | Labelled_option _
                      | Labelled_option_pun _ ->
                          true))
             ~loc));
   if should_report_error then (
     let missing = Vec.empty () in
     Fn_arity.iter arity (fun param_kind ->
         match param_kind with
         | Positional _ | Optional _ | Autofill _ | Question_optional _ -> ()
         | Labelled { label; _ } ->
             if not (Basic_hash_string.mem seen_labels label) then
               Vec.push missing label);
     if not (Vec.is_empty missing) then
       add_error diagnostics
         (Errors.missing_fn_label ~labels:(Vec.to_list missing) ~loc));
   let apply : Typedtree.expr =
     Texpr_apply
       { func; args = targs; ty = ty_res_application; kind_ = kind; loc_ = loc }
   in
   match attr with
   | Exclamation ->
       Texpr_exclamation
         {
           expr = apply;
           loc_ = loc;
           ty = ty_res_with_attr;
           convert_to_result = false;
         }
   | Question ->
       if kind = Dot_return_self then
         add_error diagnostics (Errors.double_exclamation_with_cascade loc);
       Texpr_exclamation
         {
           expr = apply;
           loc_ = loc;
           ty = ty_res_with_attr;
           convert_to_result = true;
         }
   | Double_exclamation -> (
       match ty_err with
       | Some _ ->
           Texpr_exclamation
             {
               expr = apply;
               loc_ = loc;
               ty = ty_res_with_attr;
               convert_to_result = false;
             }
       | None -> apply)
   | No_attr -> apply
    : Typedtree.expr)

and typing_pipe ?(expect_ty : Stype.t option) (env : Local_env.t)
    (lhs : Syntax.expr) (rhs : Syntax.expr) ~loc ~control_ctx ~tvar_env ~cenv
    ~global_env ~diagnostics =
  (let infer_go =
     infer_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let rhs_loc = Syntax.loc_of_expression rhs in
   let from_typed_apply (tapply : Typedtree.expr) =
     (match tapply with
      | Texpr_apply { func; args = { arg_value = tlhs; _ } :: args; ty; _ } ->
          Texpr_pipe
            {
              lhs = tlhs;
              rhs = Pipe_partial_apply { func; args; loc_ = rhs_loc };
              ty;
              loc_ = loc;
            }
      | Texpr_exclamation
          {
            expr =
              Texpr_apply
                { func; args = { arg_value = tlhs; _ } :: args; ty; _ };
            loc_;
            ty = outer_ty;
            convert_to_result;
          } ->
          Texpr_exclamation
            {
              expr =
                Texpr_pipe
                  {
                    lhs = tlhs;
                    rhs = Pipe_partial_apply { func; args; loc_ = rhs_loc };
                    ty;
                    loc_ = loc;
                  };
              loc_;
              ty = outer_ty;
              convert_to_result;
            }
      | _ -> assert false
       : Typedtree.expr)
       [@@inline]
   in
   let fn_go func args ~attr =
     let func = infer_go env func in
     from_typed_apply
       (typing_application ?expect_ty env func
          ({ arg_value = lhs; arg_kind = Positional } :: args)
          ~kind:Normal ~loc ~control_ctx ~tvar_env ~cenv ~global_env ~attr
          ~diagnostics)
       [@@inline]
   in
   let constr_go constr args =
     (from_typed_apply
        (typing_constr_or_constant constr
           (Some ({ arg_value = lhs; arg_kind = Positional } :: args))
           ~expect_ty ~env ~loc ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics)
       : Typedtree.expr)
       [@@inline]
   in
   match rhs with
   | Pexpr_apply { func = (Pexpr_ident _ | Pexpr_method _) as func; args; attr }
     ->
       fn_go func args ~attr
   | Pexpr_ident _ | Pexpr_method _ -> fn_go rhs [] ~attr:No_attr
   | Pexpr_constr { constr } -> constr_go constr []
   | Pexpr_apply { func = Pexpr_constr { constr }; attr; args } ->
       (if attr <> No_attr then
          let error =
            Errors.invalid_apply_attr ~kind:`Constructor
              ~attr:(Syntax.string_of_apply_attr attr)
              ~loc
          in
          add_error diagnostics error);
       constr_go constr args
   | _ ->
       let lhs = infer_go env lhs in
       add_error diagnostics (Errors.unsupported_pipe_expr rhs_loc);
       let trhs = infer_go env rhs in
       Texpr_pipe
         {
           lhs;
           rhs =
             Pipe_invalid
               {
                 expr = trhs;
                 ty = Typedtree_util.type_of_typed_expr trhs;
                 loc_ = rhs_loc;
               };
           ty =
             (match expect_ty with
             | Some ty -> ty
             | None -> Stype.new_type_var Tvar_error);
           loc_ = loc;
         }
    : Typedtree.expr)

and typing_self_method (ty_self : Stype.t) (method_name : Syntax.label) ~src
    ~loc ~global_env ~tvar_env ~cenv ~diagnostics =
  (let name = method_name.label_name in
   match
     Type_constraint.resolve_method_by_type ty_self name ~tvar_env ~global_env
       ~src ~loc
   with
   | Ok (Known_method method_info) ->
       Checked_attributes.check_alerts ~diagnostics method_info.attrs loc;
       Docstring.check_alerts ~diagnostics
         (Docstring.pragmas method_info.doc_)
         loc;
       let ty_method, ty_args_ =
         Poly_type.instantiate_method ~cenv ~loc method_info
       in
       Ctype.unify_exn ty_self ty_self;
       Texpr_ident
         {
           id =
             {
               var_id = Ident.of_qual_ident method_info.id;
               loc_ = method_name.loc_;
             };
           ty_args_;
           arity_ = Some method_info.arity_;
           kind =
             (match method_info.prim with None -> Normal | Some p -> Prim p);
           ty = ty_method;
           loc_ = method_name.loc_;
         }
   | Ok (Promised_method { method_id; method_ty; method_arity; prim }) ->
       Texpr_ident
         {
           id = { var_id = method_id; loc_ = method_name.loc_ };
           ty_args_ = [||];
           arity_ = Some method_arity;
           kind = (match prim with None -> Normal | Some p -> Prim p);
           ty = method_ty;
           loc_ = method_name.loc_;
         }
   | Error err ->
       add_error diagnostics err;
       Texpr_ident
         {
           id = { var_id = Ident.fresh name; loc_ = method_name.loc_ };
           ty_args_ = [||];
           arity_ = None;
           kind = Normal;
           ty = Stype.new_type_var Tvar_error;
           loc_ = method_name.loc_;
         }
    : Typedtree.expr)

and typing_constr_or_constant (constr : Syntax.constructor)
    (args : Syntax.argument list option) ~(expect_ty : Stype.t option) ~env ~loc
    ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics =
  let name = constr.constr_name.name in
  let infer_args func (args : Syntax.argument list) =
    (let args =
       Lst.map args (fun arg ->
           ({
              arg_value =
                infer_expr env arg.arg_value ~control_ctx ~tvar_env ~cenv
                  ~global_env ~diagnostics;
              arg_kind = arg.arg_kind;
            }
             : Typedtree.argument))
     in
     let ty = Typedtree_util.type_of_typed_expr func in
     Texpr_apply { func; args; kind_ = Normal; ty; loc_ = loc }
      : Typedtree.expr)
      [@@local]
  in
  match
    Typeutil.resolve_constr_or_constant ~global_env ~tvar_env ~expect_ty ~constr
      ~creating_value:true ~diagnostics
  with
  | Some (`Constr constr_desc), type_name -> (
      let ty_res, ty_args = Poly_type.instantiate_constr constr_desc in
      let ty_constr =
        match ty_args with
        | [] -> ty_res
        | _ -> Builtin.type_arrow ty_args ty_res ~err_ty:None ~is_async:false
      in
      let constr_expr : Typedtree.expr =
        Texpr_constr
          {
            type_name;
            constr = constr.constr_name;
            tag = constr_desc.cs_tag;
            ty = ty_constr;
            arity_ = constr_desc.cs_arity_;
            loc_ = constr.loc_;
          }
      in
      match (ty_args, args) with
      | _, None ->
          (match expect_ty with
          | None -> ()
          | Some expect_ty ->
              store_error ~diagnostics
                (Ctype.unify_constr name ~expect_ty ~actual_ty:ty_constr loc));
          constr_expr
      | [], Some args ->
          Local_diagnostics.add_error diagnostics
            (Errors.constant_constr_cannot_have_args ~name ~loc:constr.loc_);
          infer_args constr_expr args
      | _ :: _, Some args ->
          (match expect_ty with
          | None -> ()
          | Some expect_ty ->
              store_error ~diagnostics
                (Ctype.unify_constr name ~expect_ty ~actual_ty:ty_res loc));
          typing_application ?expect_ty:None env constr_expr args ~kind:Normal
            ~loc ~control_ctx ~tvar_env ~cenv ~global_env ~attr:No_attr
            ~diagnostics)
  | Some (`Constant { id; kind; typ; _ }), _ -> (
      let constant_expr : Typedtree.expr =
        match kind with
        | Const c ->
            Texpr_constant
              {
                c;
                ty = typ;
                name_ = Some { var_id = Ident.of_qual_ident id; loc_ = loc };
                loc_ = constr.loc_;
              }
        | Prim _ | Normal ->
            Texpr_hole { ty = typ; loc_ = constr.loc_; kind = Synthesized }
      in
      match args with
      | None ->
          (match expect_ty with
          | Some expect_ty ->
              unify_expr_and_store_error ~expect_ty ~actual_ty:typ constr.loc_
                ~diagnostics
          | _ -> ());
          constant_expr
      | Some args ->
          (match Stype.type_repr typ with
          | Tvar { contents = Tnolink Tvar_error } | T_blackhole -> ()
          | typ ->
              Local_diagnostics.add_error diagnostics
                (Errors.type_mismatch ~expected:"function type"
                   ~actual:(Printer.type_to_string typ)
                   ~loc));
          infer_args constant_expr args)
  | None, type_name -> (
      let ty =
        match expect_ty with
        | Some expect_ty -> expect_ty
        | None -> Stype.new_type_var Tvar_error
      in
      let constr_expr : Typedtree.expr =
        let arity_ =
          match args with
          | None -> Fn_arity.simple 0
          | Some args -> Fn_arity.simple (List.length args)
        in
        Texpr_constr
          {
            type_name;
            constr = constr.constr_name;
            tag = Typeutil.unknown_tag;
            ty;
            arity_;
            loc_ = loc;
          }
      in
      match args with
      | None -> constr_expr
      | Some args -> infer_args constr_expr args)

and typing_array_spread (env : Local_env.t)
    (elems : Syntax.spreadable_elem list)
    ~(kind : [ `Array | `String | `Bytes ]) ~(loc : Rloc.t)
    ~(expect_ty : Stype.t option) ~(control_ctx : Control_ctx.t)
    ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let check_go =
     check_expr env ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let infer_go =
     infer_expr env ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let ty_elem, ty_elem_iter, self, push, push_iter =
     let loc_ = Rloc.no_location in
     let typing_self_method ty name =
       typing_self_method ty
         { label_name = name; loc_ }
         ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv ~diagnostics
         [@@inline]
     in
     match kind with
     | `Array ->
         let ty_elem = Stype.new_type_var Tvar_normal in
         let ty = Builtin.type_array ty_elem in
         (match expect_ty with
         | Some expect_ty ->
             unify_expr_and_store_error ~expect_ty ~actual_ty:ty ~diagnostics
               loc
         | None -> ());
         let self : Typedtree.expr =
           Texpr_array
             { exprs = []; ty; is_fixed_array = false; loc_ = Rloc.no_location }
         in
         let push : Typedtree.expr = typing_self_method ty "push" in
         Ctype.unify_exn
           (Typedtree_util.type_of_typed_expr push)
           (Builtin.type_arrow [ ty; ty_elem ] Stype.unit ~err_ty:None
              ~is_async:false);
         let push_iter : Typedtree.expr = typing_self_method ty "push_iter" in
         let ty_elem_iter = Builtin.type_iter ty_elem in
         Ctype.unify_exn
           (Typedtree_util.type_of_typed_expr push_iter)
           (Builtin.type_arrow [ ty; ty_elem_iter ] Stype.unit ~err_ty:None
              ~is_async:false);
         (ty_elem, ty_elem_iter, self, push, push_iter)
     | `String ->
         let func : Syntax.expr =
           Pexpr_method
             {
               type_name =
                 { name = Lident "StringBuilder"; is_object = false; loc_ };
               method_name = { label_name = "new"; loc_ };
               loc_;
             }
         in
         let self : Typedtree.expr =
           infer_go (Pexpr_apply { func; args = []; attr = No_attr; loc_ })
         in
         let sb_ty = Typedtree_util.type_of_typed_expr self in
         let push = typing_self_method sb_ty "write_char" in
         let push_iter = typing_self_method sb_ty "write_iter" in
         (Stype.char, Builtin.type_iter Stype.char, self, push, push_iter)
     | `Bytes ->
         let func : Syntax.expr =
           Pexpr_ident
             {
               id = { var_name = Ldot { pkg = "buffer"; id = "new" }; loc_ };
               loc_;
             }
         in
         let self : Typedtree.expr =
           infer_go (Pexpr_apply { func; args = []; attr = No_attr; loc_ })
         in
         let bf_ty = Typedtree_util.type_of_typed_expr self in
         let push = typing_self_method bf_ty "write_byte" in
         let push_iter = typing_self_method bf_ty "write_iter" in
         (Stype.byte, Builtin.type_iter Stype.byte, self, push, push_iter)
   in
   let self_ty = Typedtree_util.type_of_typed_expr self in
   Lst.fold_left elems self (fun self ->
       fun elem ->
        let self_dotdot_apply func arg =
          (Texpr_apply
             {
               func;
               args =
                 [
                   { arg_value = self; arg_kind = Positional };
                   { arg_value = arg; arg_kind = Positional };
                 ];
               kind_ = Dot_return_self;
               ty = self_ty;
               loc_ = Rloc.no_location;
             }
            : Typedtree.expr)
            [@@inline]
        in
        match elem with
        | Elem_regular expr ->
            let texpr = check_go expr (Expect_type ty_elem) in
            self_dotdot_apply push texpr
        | Elem_spread { expr; loc_ } ->
            let iter_expr : Syntax.expr =
              Pexpr_dot_apply
                {
                  self = expr;
                  method_name = { label_name = "iter"; loc_ };
                  args = [];
                  return_self = false;
                  attr = No_attr;
                  loc_;
                }
            in
            let texpr = check_go iter_expr (Expect_type ty_elem_iter) in
            self_dotdot_apply push_iter texpr)
    : Typedtree.expr)

and typing_map_expr (env : Local_env.t) (elems : Syntax.map_expr_elem list)
    ~(loc : Rloc.t) ~(expect_ty : Stype.t option) ~(control_ctx : Control_ctx.t)
    ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let map_tpath =
     Type_path.toplevel_type ~pkg:Basic_config.builtin_package "Map"
   in
   match
     Global_env.find_regular_method global_env ~type_name:map_tpath
       ~method_name:"from_array"
   with
   | None ->
       add_error diagnostics
         (Errors.pkg_not_imported ~name:Basic_config.builtin_package
            ~action:
              (("create value of type " ^ Type_path_util.name map_tpath
                : Stdlib.String.t)
                [@merlin.hide])
            ~loc);
       Texpr_hole
         { ty = Stype.new_type_var Tvar_error; loc_ = loc; kind = Synthesized }
   | Some method_info ->
       let ty, ty_args_ = Poly_type.instantiate_method ~cenv ~loc method_info in
       let map_from_array : Typedtree.expr =
         Texpr_ident
           {
             id =
               {
                 var_id = Ident.of_qual_ident method_info.id;
                 loc_ = Rloc.no_location;
               };
             kind = Normal;
             arity_ = Some method_info.arity_;
             ty_args_;
             ty;
             loc_ = Rloc.no_location;
           }
       in
       let elems_array : Syntax.expr =
         Pexpr_array
           {
             exprs =
               Lst.map elems
                 (fun (Map_expr_elem { key; expr; key_loc_; loc_ }) ->
                   let key_expr : Syntax.expr =
                     Pexpr_constant { c = key; loc_ = key_loc_ }
                   in
                   Syntax.Pexpr_tuple { exprs = [ key_expr; expr ]; loc_ });
             loc_ = Rloc.no_location;
           }
       in
       let is_json, expect_ty =
         match expect_ty with
         | Some expect_ty when Type.same_type expect_ty Stype.json ->
             (true, Some (Builtin.type_map Stype.string Stype.json))
         | _ -> (false, expect_ty)
       in
       let result =
         typing_application env map_from_array
           [ { arg_value = elems_array; arg_kind = Positional } ]
           ?expect_ty ~kind:Normal ~loc ~control_ctx ~tvar_env ~cenv ~global_env
           ~attr:No_attr ~diagnostics
       in
       if is_json then
         Json_literal.make_json_expr ~global_env ~diagnostics ~loc
           Json_literal.object_ result
       else result
    : Typedtree.expr)

and typing_static_assert ~global_env ~tvar_env ~diagnostics ~control_ctx
    ~expect_ty env asserts body =
  let assert_cenv = Poly_type.make () in
  Lst.iter asserts
    ~f:(fun { assert_type; assert_trait; assert_loc; assert_msg } ->
      let aux_diagnostics = Local_diagnostics.make ~base:Loc.no_location in
      let type_ =
        Typedtree_util.stype_of_typ
          (typing_type ~diagnostics:aux_diagnostics ~tvar_env ~global_env
             assert_type)
      in
      match
        Global_env.All_types.find_trait
          (Global_env.get_all_types global_env)
          assert_trait ~loc:assert_loc
      with
      | Error _ -> ()
      | Ok trait ->
          Poly_type.add_constraint assert_cenv type_
            {
              trait = trait.name;
              src_ = Static_assert assert_msg;
              loc_ = assert_loc;
            });
  let assert_diagnostics = Local_diagnostics.make ~base:Loc.no_location in
  Type_constraint.solve_constraints assert_cenv ~tvar_env ~global_env
    ~diagnostics:assert_diagnostics;
  Local_diagnostics.merge assert_diagnostics ~into:diagnostics;
  if Local_diagnostics.has_fatal_errors assert_diagnostics then
    Texpr_hole
      {
        kind = Synthesized;
        ty = Stype.new_type_var Tvar_error;
        loc_ = Rloc.no_location;
      }
  else
    let aux_cenv = Poly_type.make () in
    match expect_ty with
    | None ->
        infer_expr env body ~global_env ~tvar_env ~control_ctx ~cenv:aux_cenv
          ~diagnostics
    | Some expect_ty ->
        check_expr env body expect_ty ~global_env ~tvar_env ~control_ctx
          ~cenv:aux_cenv ~diagnostics

and check_expr (env : Local_env.t) (expr : Syntax.expr) (expect_ty : expect_ty)
    ~(control_ctx : Control_ctx.t) ~(tvar_env : Tvar_env.t)
    ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let check_go =
     check_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let fallback expect_ty =
     check_expr_no_cast env expr expect_ty ~control_ctx ~tvar_env ~cenv
       ~global_env ~diagnostics
       [@@inline]
   in
   match (expect_ty, expr) with
   | Expect_type expect_ty, _ when Typeutil.is_trait expect_ty ->
       let expr =
         infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let actual_ty = Typedtree_util.type_of_typed_expr expr in
       let loc_ = Typedtree.loc_of_typed_expr expr in
       handle_unify_result ~diagnostics ~expr
         (unify_expr_allow_trait_upcast ~global_env ~cenv ~expect_ty ~actual_ty
            loc_)
   | _, Pexpr_sequence { exprs; last_expr; loc_ } ->
       let exprs = Lst.map exprs (fun expr -> check_go env expr Ignored) in
       let last_expr = check_go env last_expr expect_ty in
       let ty = Typedtree_util.type_of_typed_expr last_expr in
       Texpr_sequence { exprs; last_expr; ty; loc_ }
   | _, Pexpr_let { pattern = p; expr = e; body; loc_; _ } ->
       let pat_binders, tp, te1 =
         typing_let env p e ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let body =
         check_go (Typeutil.add_pat_binders env pat_binders) body expect_ty
       in
       let ty = Typedtree_util.type_of_typed_expr body in
       Texpr_let { pat = tp; rhs = te1; body; ty; pat_binders; loc_ }
   | _, Pexpr_letfn { name; func; body; loc_ } -> (
       match[@warning "-fragile-match"]
         typing_letrec env
           [ (name, func) ]
           ~tvar_env ~cenv ~global_env ~diagnostics
       with
       | env_with_funs, (binder, fn) :: [] ->
           let body = check_go env_with_funs body expect_ty in
           let ty = Typedtree_util.type_of_typed_expr body in
           Texpr_letfn
             {
               binder;
               fn;
               body;
               ty;
               loc_;
               is_rec = Typedtree_util.is_rec binder fn;
             }
       | _ -> assert false)
   | _, Pexpr_letrec { bindings = funs; body; loc_ } ->
       let env_with_funs, bindings =
         typing_letrec env funs ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let body = check_go env_with_funs body expect_ty in
       let ty = Typedtree_util.type_of_typed_expr body in
       Texpr_letrec { bindings; body; ty; loc_ }
   | _, Pexpr_letmut { binder; ty; expr; body; loc_ } ->
       let env, binder, expr, konstraint =
         typing_letmut env binder ty expr ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
       in
       let body = check_go env body expect_ty in
       let ty = Typedtree_util.type_of_typed_expr body in
       Texpr_letmut { binder; konstraint; expr; body; ty; loc_ }
   | _, Pexpr_if { cond; ifso; ifnot = None; loc_ } ->
       let cond = check_go env cond expect_bool in
       let ifso_env =
         Typeutil.add_pat_binders env (Typedtree_util.pat_binders_of_cond cond)
       in
       let ifso = check_go ifso_env ifso Ignored in
       (match expect_ty with
       | Ignored -> ()
       | Expect_type expect_ty ->
           unify_expr_and_store_error ~expect_ty ~actual_ty:Stype.unit
             ~diagnostics loc_);
       Texpr_if { cond; ifso; ifnot = None; ty = Stype.unit; loc_ }
   | _, Pexpr_if { cond; ifso; ifnot = Some ifnot; loc_ } ->
       let cond = check_go env cond expect_bool in
       let ifso_env =
         Typeutil.add_pat_binders env (Typedtree_util.pat_binders_of_cond cond)
       in
       let ifso = check_go ifso_env ifso expect_ty in
       let ifnot = check_go env ifnot expect_ty in
       let ty = Typedtree_util.type_of_typed_expr ifso in
       Texpr_if { cond; ifso; ifnot = Some ifnot; ty; loc_ }
   | _, Pexpr_guard { cond; otherwise; body; loc_ } ->
       let cond = check_go env cond expect_bool in
       let guard_env =
         Typeutil.add_pat_binders env (Typedtree_util.pat_binders_of_cond cond)
       in
       let otherwise =
         match otherwise with
         | None -> None
         | Some otherwise -> Some (check_go env otherwise expect_ty)
       in
       let body = check_go guard_env body expect_ty in
       let ty = Typedtree_util.type_of_typed_expr body in
       Texpr_guard { cond; otherwise; body; ty; loc_ }
   | _, Pexpr_guard_let { pat; expr; otherwise; body; loc_ } ->
       Local_diagnostics.add_warning diagnostics
         {
           kind =
             Deprecated_syntax
               {
                 old_usage = "guard let";
                 purpose = "shorthand of pattern match";
                 new_usage = Some "guard with is-expression";
               };
           loc = Syntax.loc_of_pattern pat;
         };
       let pat_binders, pat, rhs =
         typing_let env pat expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let otherwise =
         match otherwise with
         | None -> None
         | Some otherwise ->
             Some
               (check_cases env otherwise
                  ~pat_ty:(Typedtree_util.type_of_typed_expr rhs)
                  ~action_ty:expect_ty ~tvar_env ~cenv ~global_env ~diagnostics
                  ~control_ctx)
       in
       let body =
         check_go (Typeutil.add_pat_binders env pat_binders) body expect_ty
       in
       let ty = Typedtree_util.type_of_typed_expr body in
       if Check_match.irrefutable_pattern pat then
         Local_diagnostics.add_warning diagnostics
           { kind = Unused_guard; loc = Typedtree.loc_of_pat pat };
       Texpr_guard_let { pat; rhs; pat_binders; otherwise; body; ty; loc_ }
   | ( _,
       Pexpr_try
         {
           body;
           catch;
           catch_all;
           try_else;
           try_loc_;
           catch_loc_;
           else_loc_;
           loc_;
         } ) ->
       let error_ctx = ref (Control_ctx.Open_ctx Empty_ctx) in
       let new_ctx = Control_ctx.with_error_ctx ~error_ctx control_ctx in
       let body_expect_ty =
         if try_else = None then expect_ty
         else Expect_type (Stype.new_type_var Tvar_normal)
       in
       let body =
         check_expr ~control_ctx:new_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics env body body_expect_ty
       in
       let error_ty = Control_ctx.error_ctx_to_stype !error_ctx in
       let catch =
         check_cases env catch ~pat_ty:error_ty ~action_ty:expect_ty ~tvar_env
           ~cenv ~global_env ~diagnostics ~control_ctx
       in
       let try_else =
         match try_else with
         | None -> None
         | Some try_else ->
             Some
               (check_cases env try_else
                  ~pat_ty:(Typedtree_util.type_of_typed_expr body)
                  ~action_ty:expect_ty ~tvar_env ~cenv ~global_env ~diagnostics
                  ~control_ctx)
       in
       if not !(new_ctx.may_has_error) then (
         Local_diagnostics.add_warning diagnostics
           { kind = Useless_try; loc = try_loc_ };
         match Stype.type_repr error_ty with
         | Tvar link -> link := Tlink Stype.unit
         | _ -> ());
       if catch_all then (
         control_ctx.may_has_error := true;
         match control_ctx.error_ctx with
         | Some error_ctx ->
             store_error ~diagnostics
               (Control_ctx.check_error_in_ctx ~error_ty ~ctx:error_ctx
                  catch_loc_)
         | None ->
             add_error diagnostics
               (Errors.invalid_raise ~kind:`Catchall ~loc:catch_loc_));
       let ty =
         match try_else with
         | None -> Typedtree_util.type_of_typed_expr body
         | Some ({ action; _ } :: _) -> Typedtree_util.type_of_typed_expr action
         | Some [] -> (
             match expect_ty with Ignored -> Stype.unit | Expect_type ty -> ty)
       in
       Texpr_try
         {
           body;
           catch;
           catch_all;
           try_else;
           ty;
           err_ty = error_ty;
           catch_loc_;
           else_loc_;
           loc_;
         }
   | _, Pexpr_match { expr; cases; match_loc_; loc_ } ->
       let expr =
         infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let ty = Typedtree_util.type_of_typed_expr expr in
       let cases =
         check_cases env cases ~pat_ty:ty ~action_ty:expect_ty ~tvar_env ~cenv
           ~global_env ~diagnostics ~control_ctx
       in
       let ty =
         match expect_ty with Ignored -> Stype.unit | Expect_type ty -> ty
       in
       Texpr_match { expr; cases; ty; match_loc_; loc_ }
   | _, Pexpr_loop { args; body; label; loop_loc_; loc_ } ->
       check_loop ~label ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
         env args body ~loc:loc_ ~loop_loc_ ~expect_ty
   | ( _,
       Pexpr_for
         { binders; condition; continue_block; body; label; for_else; loc_ } )
     ->
       check_for ~label ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
         ~expect_ty env binders condition continue_block body for_else ~loc:loc_
   | ( _,
       Pexpr_foreach
         {
           binders;
           expr =
             Pexpr_infix
               {
                 op = { var_name = Lident (("..<" | "..=") as op) };
                 lhs;
                 rhs;
                 loc_ = operator_loc;
               };
           body;
           else_block;
           label;
           loc_;
         } ) ->
       let inclusive = op = "..=" in
       typing_range_for_in env binders lhs rhs body else_block ~label ~inclusive
         ~operator_loc ~loc:loc_ ~expect_ty ~global_env ~tvar_env ~cenv
         ~control_ctx ~diagnostics
   | _, Pexpr_foreach { binders; expr; body; else_block; label; loc_ } ->
       typing_foreach ~label ~global_env ~tvar_env ~cenv ~control_ctx
         ~diagnostics ~expect_ty env binders expr body else_block ~loc:loc_
   | _, Pexpr_while { loop_cond; loop_body; label; loc_; while_else } ->
       check_while ~label env loop_cond loop_body while_else ~expect_ty
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | _, Pexpr_static_assert { asserts; body } ->
       typing_static_assert ~global_env ~tvar_env ~control_ctx ~diagnostics
         ~expect_ty:(Some expect_ty) env asserts body
   | Ignored, _ ->
       let expr =
         infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let actual_ty = Typedtree_util.type_of_typed_expr expr in
       let loc_ = Typedtree.loc_of_typed_expr expr in
       if not (Ctype.try_unify Stype.unit actual_ty) then
         add_error diagnostics
           (Errors.non_unit_cannot_be_ignored
              ~ty:(Printer.type_to_string actual_ty)
              ~loc:loc_);
       expr
   | Expect_type expect_ty, _ when Type.is_super_error expect_ty ->
       let error_value =
         infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       let error_value_ty = Typedtree_util.type_of_typed_expr error_value in
       let loc_ = Typedtree.loc_of_typed_expr error_value in
       if Typeutil.is_tvar error_value_ty then
         Ctype.unify_exn error_value_ty expect_ty
       else if
         not
           (Type.is_suberror error_value_ty
           || Type.is_super_error error_value_ty)
       then
         add_error diagnostics
           (Errors.not_error_subtype
              (Printer.type_to_string error_value_ty)
              loc_);
       error_value
   | Expect_type expect_ty, _ -> (
       match Global_env.get_newtype_info global_env expect_ty with
       | Some { newtype_constr; underlying_typ = _; recursive = _ } -> (
           match expr with
           | Pexpr_array_get _ | Pexpr_array_set _ | Pexpr_array_augmented_set _
           | Pexpr_unary _ | Pexpr_group _ ->
               assert false
           | Pexpr_constr { constr }
           | Pexpr_apply { func = Pexpr_constr { constr }; args = _; attr = _ }
             when newtype_constr.constr_name = constr.constr_name.name ->
               fallback expect_ty
           | Pexpr_array _ | Pexpr_array_spread _ | Pexpr_constr _
           | Pexpr_apply { func = Pexpr_constr _; args = _; attr = _ }
           | Pexpr_tuple _ | Pexpr_constant _ | Pexpr_interp _
           | Pexpr_function _ | Pexpr_record _ | Pexpr_record_update _
           | Pexpr_map _ | Pexpr_is _ -> (
               let loc = Syntax.loc_of_expression expr in
               match[@warning "-fragile-match"]
                 Poly_type.instantiate_constr newtype_constr
               with
               | expect_ty', underlying_typ :: [] ->
                   Ctype.unify_exn expect_ty' expect_ty;
                   wrap_newtype_constr newtype_constr (fallback underlying_typ)
                     expect_ty loc ~diagnostics
               | _ -> assert false)
           | Pexpr_apply _ | Pexpr_dot_apply _ | Pexpr_pipe _
           | Pexpr_constraint _ | Pexpr_field _ | Pexpr_method _ | Pexpr_infix _
           | Pexpr_ident _ | Pexpr_array_get_slice _ | Pexpr_assign _
           | Pexpr_mutate _ | Pexpr_unit _ | Pexpr_as _ -> (
               let inferred_expr =
                 infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
                   ~diagnostics
               in
               let inferred_ty =
                 Typedtree_util.type_of_typed_expr inferred_expr
               in
               let loc = Syntax.loc_of_expression expr in
               if Ctype.try_unify expect_ty inferred_ty then inferred_expr
               else
                 match[@warning "-fragile-match"]
                   Poly_type.instantiate_constr newtype_constr
                 with
                 | expect_ty', underlying_typ :: [] ->
                     Ctype.unify_exn expect_ty' expect_ty;
                     if Ctype.try_unify underlying_typ inferred_ty then
                       wrap_newtype_constr newtype_constr inferred_expr
                         expect_ty loc ~diagnostics
                     else
                       let expected, actual =
                         Printer.type_pair_to_string expect_ty inferred_ty
                       in
                       add_error diagnostics
                         (Errors.expr_unify ~expected ~actual ~loc);
                       inferred_expr
                 | _ -> assert false)
           | Pexpr_while _ | Pexpr_if _ | Pexpr_try _ | Pexpr_letfn _
           | Pexpr_letrec _ | Pexpr_let _ | Pexpr_sequence _ | Pexpr_match _
           | Pexpr_letmut _ | Pexpr_hole _ | Pexpr_return _ | Pexpr_raise _
           | Pexpr_break _ | Pexpr_continue _ | Pexpr_loop _ | Pexpr_foreach _
           | Pexpr_static_assert _ | Pexpr_guard _ | Pexpr_guard_let _
           | Pexpr_for _ | Pexpr_multiline_string _ ->
               fallback expect_ty)
       | None -> (
           let expect_ty = Stype.type_repr expect_ty in
           match (expect_ty, expr) with
           | ( T_constr { type_constructor = p; tys = actual_func_ty :: [] },
               Pexpr_function _ )
             when Type_path.equal p Type_path.Builtin.type_path_func_ref -> (
               let fn = fallback actual_func_ty in
               let free_vars = Typedtree_util.free_vars fn in
               match[@warning "-fragile-match"] fn with
               | Texpr_function { func; ty = _; loc_ } ->
                   (if Ident.Hashset.length free_vars > 0 then
                      let captures =
                        Lst.map
                          (Ident.Hashset.to_list free_vars)
                          Ident.base_name
                      in
                      add_error diagnostics
                        (Errors.func_ref_no_capture ~captures ~loc:loc_));
                   Texpr_function { func; ty = expect_ty; is_raw_ = true; loc_ }
               | _ -> assert false)
           | _ -> fallback expect_ty))
    : Typedtree.expr)

and check_expr_no_cast (env : Local_env.t) (expr : Syntax.expr)
    (expect_ty : Stype.t) ~(control_ctx : Control_ctx.t)
    ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let check_go =
     check_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let infer_go =
     infer_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   match expr with
   | Pexpr_array_augmented_set _ -> assert false
   | Pexpr_array_get _ -> assert false
   | Pexpr_array_set _ -> assert false
   | Pexpr_unary _ -> assert false
   | Pexpr_loop _ -> assert false
   | Pexpr_while _ -> assert false
   | Pexpr_try _ -> assert false
   | Pexpr_match _ -> assert false
   | Pexpr_guard _ -> assert false
   | Pexpr_letmut _ -> assert false
   | Pexpr_letfn _ -> assert false
   | Pexpr_let _ -> assert false
   | Pexpr_letrec _ -> assert false
   | Pexpr_sequence _ -> assert false
   | Pexpr_guard_let _ -> assert false
   | Pexpr_if _ -> assert false
   | Pexpr_for _ -> assert false
   | Pexpr_foreach _ -> assert false
   | Pexpr_static_assert _ -> assert false
   | Pexpr_constr { constr; loc_ } ->
       typing_constr_or_constant constr None ~expect_ty:(Some expect_ty) ~env
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_apply { func = Pexpr_constr { constr }; args; attr; loc_ } ->
       (if attr <> No_attr then
          let error =
            Errors.invalid_apply_attr ~kind:`Constructor
              ~attr:(Syntax.string_of_apply_attr attr)
              ~loc:loc_
          in
          add_error diagnostics error);
       typing_constr_or_constant constr (Some args) ~expect_ty:(Some expect_ty)
         ~env ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   | Pexpr_record { type_name; fields; loc_; trailing = _ } -> (
       let type_name, expect_ty =
         match type_name with
         | None -> (None, expect_ty)
         | Some type_name -> (
             match
               typing_type_name ~global_env ~tvar_env type_name ~diagnostics
             with
             | Some
                 ( Tname_defined ({ ty_desc = Record_type _; _ } as ty_decl),
                   type_name ) ->
                 let another_expect_ty : Stype.t =
                   T_constr
                     {
                       type_constructor = ty_decl.ty_constr;
                       tys = Type.type_var_list ty_decl.ty_arity Tvar_normal;
                       generic_ = false;
                       is_suberror_ = false;
                     }
                 in
                 unify_expr_and_store_error ~expect_ty
                   ~actual_ty:another_expect_ty ~diagnostics loc_;
                 (Some type_name, another_expect_ty)
             | Some (Tname_local_type ({ kind = Struct _; _ } as t), type_name)
               ->
                 let another_expect_ty : Stype.t =
                   T_constr
                     {
                       type_constructor =
                         Type_path.local_type t.toplevel_id t.name;
                       tys = Tvar_env.get_types t.ty_params_;
                       generic_ = false;
                       is_suberror_ = false;
                     }
                 in
                 unify_expr_and_store_error ~expect_ty
                   ~actual_ty:another_expect_ty ~diagnostics loc_;
                 (Some type_name, another_expect_ty)
             | Some (_, tast_type_name) ->
                 add_error diagnostics
                   (Errors.not_a_record_type
                      ~name:(Longident.to_string type_name.name)
                      ~loc:type_name.loc_);
                 (Some tast_type_name, expect_ty)
             | None -> (None, expect_ty))
       in
       if Typeutil.is_tvar expect_ty then (
         let typed_record =
           infer_record env fields loc_ ~type_name ~control_ctx ~tvar_env ~cenv
             ~global_env ~diagnostics
         in
         unify_expr_and_store_error ~expect_ty
           ~actual_ty:(Typedtree_util.type_of_typed_expr typed_record)
           ~diagnostics loc_;
         typed_record)
       else
         match
           Global_env.labels_of_record global_env expect_ty ~loc:loc_
             ~context:`Create
         with
         | Ok labels ->
             let _, labels =
               Poly_type.instantiate_record ~ty_record:(`Known expect_ty) labels
             in
             type_guided_record_check fields labels expect_ty ~type_name ~env
               ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics ~loc:loc_
         | Error error ->
             add_error diagnostics error;
             infer_record env fields loc_ ~type_name ~control_ctx ~tvar_env
               ~cenv ~global_env ~diagnostics)
   | Pexpr_record_update { type_name; record; fields; loc_ } -> (
       let type_name, expect_ty =
         match type_name with
         | None -> (None, expect_ty)
         | Some type_name -> (
             match
               typing_type_name ~global_env ~tvar_env type_name ~diagnostics
             with
             | Some
                 ( Tname_defined ({ ty_desc = Record_type _; _ } as ty_decl),
                   type_name ) ->
                 let another_expect_ty : Stype.t =
                   T_constr
                     {
                       type_constructor = ty_decl.ty_constr;
                       tys = Type.type_var_list ty_decl.ty_arity Tvar_normal;
                       generic_ = false;
                       is_suberror_ = false;
                     }
                 in
                 unify_expr_and_store_error ~expect_ty
                   ~actual_ty:another_expect_ty ~diagnostics loc_;
                 (Some type_name, another_expect_ty)
             | Some (_, tast_type_name) ->
                 add_error diagnostics
                   (Errors.not_a_record_type
                      ~name:(Longident.to_string type_name.name)
                      ~loc:type_name.loc_);
                 (Some tast_type_name, expect_ty)
             | None -> (None, expect_ty))
       in
       let typed_old_record = check_go env record (Expect_type expect_ty) in
       match
         Global_env.labels_of_record global_env expect_ty ~loc:loc_
           ~context:`Update
       with
       | Ok all_fields ->
           let _, all_fields =
             Poly_type.instantiate_record ~ty_record:(`Known expect_ty)
               all_fields
           in
           let typed_fields, all_fields =
             ( type_guided_record_update_check env all_fields fields expect_ty
                 ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics,
               all_fields )
           in
           Texpr_record_update
             {
               type_name;
               record = typed_old_record;
               all_fields;
               fields = typed_fields;
               ty = expect_ty;
               loc_;
             }
       | Error e ->
           add_error diagnostics e;
           infer_go env expr)
   | Pexpr_function { func = Match _ } -> assert false
   | Pexpr_function
       {
         func =
           Lambda
             {
               parameters;
               params_loc_;
               body;
               return_type;
               kind_;
               has_error;
               is_async;
             };
         loc_;
       } ->
       let n = List.length parameters in
       let check_param_annotation (p : Syntax.parameter) expect_param_ty =
         match Syntax.ty_of_param p with
         | None -> (expect_param_ty, None)
         | Some syntax_ty ->
             let typedtree_ty =
               typing_type syntax_ty ~tvar_env ~global_env ~diagnostics
             in
             let sty = Typedtree_util.stype_of_typ typedtree_ty in
             let binder_name = Syntax.binder_name_of_param p in
             let loc = Syntax.loc_of_param_binder p in
             store_error ~diagnostics
               (Ctype.unify_param binder_name ~expect_ty:expect_param_ty
                  ~actual_ty:sty loc);
             (sty, Some typedtree_ty)
       in
       let check_return_annotation expect_return_ty expect_err_ty =
         let unify_ret expect_ty actual_ty
             (annotation : Typeutil.ret_annotation) =
           match annotation with
           | Annotated (ret_ty, _) ->
               let loc = Typedtree.loc_of_typ ret_ty in
               unify_expr_and_store_error ~expect_ty ~actual_ty ~diagnostics loc
           | Has_super_error _ | No_annotation ->
               Ctype.unify_exn expect_ty actual_ty
             [@@inline]
         in
         let unify_err expect_ty (annotation : Typeutil.ret_annotation) =
           let do_unify ~actual_ty loc =
             match expect_ty with
             | Some expect_ty ->
                 unify_expr_and_store_error ~expect_ty ~actual_ty ~diagnostics
                   loc
             | None ->
                 let err =
                   Errors.error_type_mismatch ~loc ~expected_ty:"no error"
                     ~actual_ty:(Printer.type_to_string actual_ty)
                 in
                 add_error diagnostics err
               [@@inline]
           in
           let check_missing_error loc_ =
             match expect_ty with
             | Some _expect_ty ->
                 add_error diagnostics
                   (Errors.anonymous_missing_error_annotation loc_)
             | None -> ()
               [@@inline]
           in
           match annotation with
           | Annotated (_, Error_typ { ty }) ->
               let err_sty = Typedtree_util.stype_of_typ ty in
               do_unify ~actual_ty:err_sty (Typedtree.loc_of_typ ty);
               Some err_sty
           | Annotated (_, Default_error_typ { loc_ }) | Has_super_error loc_ ->
               let err_sty = Typeutil.default_err_type in
               do_unify ~actual_ty:err_sty loc_;
               Some err_sty
           | No_annotation -> expect_ty
           | Annotated (ret_ty, No_error_typ) ->
               check_missing_error (Typedtree.loc_of_typ ret_ty);
               None
             [@@inline]
         in
         let ({ ret_sty; err_sty = _; annotation } : Typeutil.ret_info) =
           Typeutil.handle_return_annotation return_type ~has_error
             ~typing_type:(typing_type ~tvar_env ~global_env ~diagnostics)
         in
         unify_ret expect_return_ty ret_sty annotation;
         let err_sty = unify_err expect_err_ty annotation in
         (ret_sty, err_sty, annotation)
       in
       let ({ params_ty; ret_ty; err_ty; is_async; ret_annotation }
             : Local_typing_worklist.typed_fn_annotation) =
         match Stype.type_repr expect_ty with
         | Tarrow
             {
               params_ty = expect_params_ty;
               ret_ty = expect_ret_ty;
               err_ty = expect_err_ty;
               is_async = expect_async;
             } as expect_ty ->
             let is_async =
               if is_async && not expect_async then (
                 add_error diagnostics
                   (Errors.generic_type_mismatch ~header:"Type Mismatch"
                      ~expected:(Printer.type_to_string expect_ty)
                      ~actual:"async function type" ~loc:loc_);
                 true)
               else expect_async
             in
             let n_expect = List.length expect_params_ty in
             if n_expect = n then
               let params_ty =
                 Lst.map2 parameters expect_params_ty check_param_annotation
               in
               let ret_ty, err_ty, ret_annotation =
                 check_return_annotation expect_ret_ty expect_err_ty
               in
               { params_ty; ret_ty; err_ty; is_async; ret_annotation }
             else
               let ty = Printer.type_to_string expect_ty in
               let err =
                 Errors.func_param_num_mismatch ~loc:loc_ ~expected:n_expect
                   ~actual:n ~ty
               in
               add_error diagnostics err;
               let expect_params_ty =
                 if n > n_expect then
                   expect_params_ty
                   @ Type.type_var_list (n - n_expect) Tvar_error
                 else Lst.take expect_params_ty n
               in
               let params_ty =
                 Lst.map2 parameters expect_params_ty check_param_annotation
               in
               let ret_ty, err_ty, ret_annotation =
                 check_return_annotation expect_ret_ty expect_err_ty
               in
               { params_ty; ret_ty; err_ty; is_async = false; ret_annotation }
         | Tvar ({ contents = Tnolink _ } as link) ->
             let typed_fn_annotation =
               typing_function_annotation parameters return_type ~has_error
                 ~is_async ~tvar_env ~global_env ~diagnostics
             in
             let params_ty = Lst.map typed_fn_annotation.params_ty fst in
             let ret_ty = typed_fn_annotation.ret_ty in
             let err_ty = typed_fn_annotation.err_ty in
             let arrow_ty =
               Stype.Tarrow
                 { params_ty; ret_ty; err_ty; is_async; generic_ = false }
             in
             link := Tlink arrow_ty;
             typed_fn_annotation
         | expect_ty ->
             (if expect_ty <> T_blackhole then
                let expected, actual =
                  (Printer.type_to_string expect_ty, "function type")
                in
                add_error diagnostics
                  (Errors.type_mismatch ~expected ~actual ~loc:loc_));
             typing_function_annotation parameters return_type ~has_error
               ~is_async ~tvar_env ~global_env ~diagnostics
       in
       let env, params =
         check_function_params env parameters params_ty ~is_global:false
           ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let error_ctx =
         match err_ty with
         | None -> None
         | Some t ->
             let ctx : Control_ctx.error_ctx =
               match Stype.type_repr t with
               | T_constr { type_constructor = Basic_type_path.T_error; _ } ->
                   Fixed_ctx Supererror
               | T_constr { type_constructor = p; is_suberror_ = true; _ } ->
                   Fixed_ctx (Suberror p)
               | Tparam { index; name_ } -> Fixed_ctx (Tparam { index; name_ })
               | Tvar { contents = Tnolink _ } as t ->
                   Ctype.unify_exn t Stype.error;
                   Open_ctx Empty_ctx
               | _ -> Open_ctx Empty_ctx
             in
             Some (ref ctx)
       in
       let control_ctx =
         Control_ctx.make_fn ~return:ret_ty ~error_ctx ~is_async
       in
       let body =
         check_expr env body (Expect_type ret_ty) ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
       in
       if not !(control_ctx.may_has_error) then
         Typeutil.report_unused_error_annotation ret_annotation diagnostics;
       let ret_constraint =
         match ret_annotation with
         | Annotated r -> Some r
         | Has_super_error _ | No_annotation -> None
       in
       Texpr_function
         {
           func =
             {
               params;
               params_loc_;
               body;
               ty = expect_ty;
               ret_constraint;
               kind_;
             };
           ty = expect_ty;
           is_raw_ = false;
           loc_;
         }
   | Pexpr_array { exprs; loc_ } -> (
       match Stype.type_repr expect_ty with
       | T_constr { type_constructor; tys = ty_elem :: [] }
         when Type_path.equal type_constructor
                Type_path.Builtin.type_path_fixedarray ->
           let texprs =
             Lst.map exprs (fun e -> check_go env e (Expect_type ty_elem))
           in
           Texpr_array
             { exprs = texprs; ty = expect_ty; is_fixed_array = true; loc_ }
       | T_constr { type_constructor; tys = ty_elem :: [] }
         when Type_path.equal type_constructor Type_path.Builtin.type_path_array
         ->
           let texprs =
             Lst.map exprs (fun e -> check_go env e (Expect_type ty_elem))
           in
           Texpr_array
             { exprs = texprs; ty = expect_ty; is_fixed_array = false; loc_ }
       | expect_ty when Type.same_type expect_ty Stype.json ->
           let texprs =
             Lst.map exprs (fun e -> check_go env e (Expect_type Stype.json))
           in
           Json_literal.make_json_expr ~global_env ~diagnostics ~loc:loc_
             Json_literal.array
             (Texpr_array
                {
                  exprs = texprs;
                  ty = Builtin.type_array Stype.json;
                  is_fixed_array = false;
                  loc_;
                })
       | T_builtin T_bytes -> (
           let exception Not_all_literal in
           let buf = Buffer.create 16 in
           let rec try_combine_byte_literal (elem : Syntax.expr list) =
             match elem with
             | [] -> Buffer.contents buf
             | Pexpr_constant { c = Const_byte b } :: rest ->
                 Buffer.add_int8 buf b.byte_val;
                 try_combine_byte_literal rest
             | Pexpr_constant { c = Const_int i } :: rest ->
                 (match Int32.of_string_opt i with
                 | Some v when v >= 0l && v <= 255l ->
                     Buffer.add_int8 buf (Int32.to_int v)
                 | _ -> raise_notrace Not_all_literal);
                 try_combine_byte_literal rest
             | _ -> raise_notrace Not_all_literal
           in
           try
             let bs = try_combine_byte_literal exprs in
             Texpr_constant
               {
                 c = C_bytes { v = bs; repr = None };
                 ty = expect_ty;
                 name_ = None;
                 loc_;
               }
           with Not_all_literal ->
             let texprs =
               Lst.map exprs (fun e -> check_go env e (Expect_type Stype.byte))
             in
             Texpr_array
               { exprs = texprs; ty = expect_ty; is_fixed_array = true; loc_ })
       | T_builtin T_string -> (
           let exception Not_all_literal in
           let vec = Basic_vec_int.empty () in
           let rec try_combine_char_literal (elem : Syntax.expr list) =
             match elem with
             | [] -> Basic_utf8.string_of_vec vec ~offset:0 ~len:vec.len
             | Pexpr_constant { c = Const_char c } :: rest ->
                 Basic_vec_int.push vec (Uchar.to_int c.char_val);
                 try_combine_char_literal rest
             | _ -> raise_notrace Not_all_literal
           in
           try
             let s = try_combine_char_literal exprs in
             Texpr_constant
               { c = C_string s; ty = expect_ty; name_ = None; loc_ }
           with Not_all_literal ->
             let func : Syntax.expr =
               Pexpr_method
                 {
                   type_name =
                     { name = Lident "StringBuilder"; is_object = false; loc_ };
                   method_name = { label_name = "new"; loc_ };
                   loc_;
                 }
             in
             let self : Typedtree.expr =
               infer_go env
                 (Pexpr_apply { func; args = []; attr = No_attr; loc_ })
             in
             let sb_ty = Typedtree_util.type_of_typed_expr self in
             let push =
               typing_self_method sb_ty
                 { label_name = "write_char"; loc_ = Rloc.no_location }
                 ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv
                 ~diagnostics
             in
             let sb_expr =
               Lst.fold_left exprs self (fun self ->
                   fun elem ->
                    let self_dotdot_apply func arg =
                      (Texpr_apply
                         {
                           func;
                           args =
                             [
                               { arg_value = self; arg_kind = Positional };
                               { arg_value = arg; arg_kind = Positional };
                             ];
                           kind_ = Dot_return_self;
                           ty = sb_ty;
                           loc_ = Rloc.no_location;
                         }
                        : Typedtree.expr)
                        [@@inline]
                    in
                    let texpr = check_go env elem (Expect_type Stype.char) in
                    self_dotdot_apply push texpr)
             in
             let sb_to_string =
               typing_self_method
                 (Typedtree_util.type_of_typed_expr sb_expr)
                 ({ label_name = "to_string"; loc_ = Rloc.no_location }
                   : Syntax.label)
                 ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv
                 ~diagnostics
             in
             Texpr_apply
               {
                 func = sb_to_string;
                 args = [ { arg_value = sb_expr; arg_kind = Positional } ];
                 kind_ = Normal;
                 ty = expect_ty;
                 loc_;
               })
       | _ ->
           let ty_elem = Stype.new_type_var Tvar_normal in
           let texprs =
             Lst.map exprs (fun e -> check_go env e (Expect_type ty_elem))
           in
           let ty = Builtin.type_array ty_elem in
           unify_expr_and_store_error ~expect_ty ~actual_ty:ty ~diagnostics loc_;
           Texpr_array { exprs = texprs; ty; is_fixed_array = false; loc_ })
   | Pexpr_array_spread { elems; loc_ } -> (
       match Stype.type_repr expect_ty with
       | expect_ty when Type.same_type expect_ty Stype.json ->
           Json_literal.make_json_expr ~global_env ~diagnostics ~loc:loc_
             Json_literal.array
             (typing_array_spread env elems ~kind:`Array ~loc:loc_
                ~expect_ty:(Some (Builtin.type_array Stype.json))
                ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics)
       | T_constr { type_constructor; tys = elem_ty :: [] }
         when Type_path.equal type_constructor Type_path.Builtin.type_path_iter
         ->
           let array_ty = Builtin.type_array elem_ty in
           let array_iter =
             typing_self_method array_ty
               ({ label_name = "iter"; loc_ = Rloc.no_location } : Syntax.label)
               ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv
               ~diagnostics
           in
           Ctype.unify_exn
             (Typedtree_util.type_of_typed_expr array_iter)
             (Builtin.type_arrow [ array_ty ] expect_ty ~err_ty:None
                ~is_async:false);
           let array_expr =
             typing_array_spread env elems ~kind:`Array ~loc:loc_
               ~expect_ty:(Some array_ty) ~control_ctx ~tvar_env ~cenv
               ~global_env ~diagnostics
           in
           Texpr_apply
             {
               func = array_iter;
               args = [ { arg_value = array_expr; arg_kind = Positional } ];
               kind_ = Normal;
               ty = expect_ty;
               loc_;
             }
       | T_builtin T_string ->
           let sb_expr =
             typing_array_spread env elems ~kind:`String ~loc:loc_
               ~expect_ty:(Some expect_ty) ~control_ctx ~tvar_env ~cenv
               ~global_env ~diagnostics
           in
           let sb_to_string =
             typing_self_method
               (Typedtree_util.type_of_typed_expr sb_expr)
               ({ label_name = "to_string"; loc_ = Rloc.no_location }
                 : Syntax.label)
               ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv
               ~diagnostics
           in
           Texpr_apply
             {
               func = sb_to_string;
               args = [ { arg_value = sb_expr; arg_kind = Positional } ];
               kind_ = Normal;
               ty = expect_ty;
               loc_;
             }
       | T_builtin T_bytes ->
           let buffer_expr =
             typing_array_spread env elems ~kind:`Bytes ~loc:loc_
               ~expect_ty:(Some expect_ty) ~control_ctx ~tvar_env ~cenv
               ~global_env ~diagnostics
           in
           let contents =
             typing_self_method
               (Typedtree_util.type_of_typed_expr buffer_expr)
               ({ label_name = "contents"; loc_ = Rloc.no_location }
                 : Syntax.label)
               ~src:Dot_src_direct ~loc:loc_ ~global_env ~tvar_env ~cenv
               ~diagnostics
           in
           Texpr_apply
             {
               func = contents;
               args = [ { arg_value = buffer_expr; arg_kind = Positional } ];
               kind_ = Normal;
               ty = expect_ty;
               loc_;
             }
       | expect_ty ->
           typing_array_spread env elems ~kind:`Array ~loc:loc_
             ~expect_ty:(Some expect_ty) ~control_ctx ~tvar_env ~cenv
             ~global_env ~diagnostics)
   | Pexpr_tuple { exprs; loc_ } ->
       let arity = List.length exprs in
       let tys =
         take_info_partial ~diagnostics
           (Type.filter_product ~blame:Filter_itself ~arity:(Some arity)
              expect_ty loc_)
       in
       let texprs =
         List.map2
           (fun e -> fun ty -> check_go env e (Expect_type ty))
           exprs tys
       in
       Texpr_tuple { exprs = texprs; ty = expect_ty; loc_ }
   | Pexpr_pipe { lhs; rhs; loc_ } ->
       typing_pipe env lhs rhs ~expect_ty ~loc:loc_ ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics
   | Pexpr_constant { c = Const_bool b; loc_ }
     when Type.same_type expect_ty Stype.json ->
       Json_literal.make_json_const_expr ~global_env ~diagnostics ~loc:loc_
         (if b then Json_literal.true_ else Json_literal.false_)
   | Pexpr_constant { c = Const_int rep | Const_double rep; loc_ }
     when Type.same_type expect_ty Stype.json ->
       let _, c =
         Typeutil.typing_constant (Const_double rep) ~expect_ty:None ~loc:loc_
           ~diagnostics
       in
       Json_literal.make_json_expr ~global_env ~diagnostics ~loc:loc_
         Json_literal.number
         (Texpr_constant { c; ty = Stype.double; name_ = None; loc_ })
   | Pexpr_constant { c = Const_string { string_val; string_repr = _ }; loc_ }
     when Type.same_type expect_ty Stype.json ->
       Json_literal.make_json_expr ~global_env ~diagnostics ~loc:loc_
         Json_literal.string
         (Texpr_constant
            { c = C_string string_val; ty = Stype.string; name_ = None; loc_ })
   | Pexpr_interp { elems; loc_ } when Type.same_type expect_ty Stype.json ->
       Json_literal.make_json_expr ~global_env ~diagnostics ~loc:loc_
         Json_literal.string
         (typing_interp ~expect_ty:Stype.json ~control_ctx ~global_env
            ~diagnostics ~tvar_env ~cenv env elems loc_)
   | Pexpr_interp { elems = _; loc_ } when Type.same_type expect_ty Stype.bytes
     ->
       add_error diagnostics
         (Errors.overloaded_string_interpolation_for_bytes ~loc:loc_);
       Texpr_hole
         { ty = Stype.new_type_var Tvar_error; loc_; kind = Synthesized }
   | Pexpr_constant { c; loc_ } ->
       let ty, c =
         Typeutil.typing_constant ~expect_ty:(Some expect_ty) c ~loc:loc_
           ~diagnostics
       in
       unify_expr_and_store_error ~expect_ty ~actual_ty:ty ~diagnostics loc_;
       Texpr_constant { c; ty = expect_ty; name_ = None; loc_ }
   | Pexpr_interp { elems; loc_ } ->
       typing_interp ~expect_ty ~control_ctx ~global_env ~diagnostics ~tvar_env
         ~cenv env elems loc_
   | Pexpr_constraint { expr; ty = ty_expr; loc_ } ->
       let ty = typing_type ty_expr ~tvar_env ~global_env ~diagnostics in
       let stype = Typedtree_util.stype_of_typ ty in
       unify_expr_and_store_error ~expect_ty ~actual_ty:stype ~diagnostics loc_;
       Texpr_constraint
         {
           expr = check_go env expr (Expect_type stype);
           konstraint = ty;
           ty = stype;
           loc_;
         }
   | Pexpr_mutate { record; accessor; field; augmented_by; loc_ } -> (
       unify_expr_and_store_error ~expect_ty ~actual_ty:Stype.unit ~diagnostics
         loc_;
       match accessor with
       | Label label ->
           typing_mutate env record label field augmented_by loc_ ~control_ctx
             ~tvar_env ~cenv ~global_env ~diagnostics
       | Index _ | Newtype ->
           add_error diagnostics (Errors.tuple_not_mutable loc_);
           Texpr_tuple { exprs = []; ty = Stype.unit; loc_ })
   | Pexpr_return { return_value; loc_ } ->
       check_return_value env return_value expect_ty ~control_ctx ~tvar_env
         ~cenv ~global_env ~diagnostics ~loc:loc_
   | Pexpr_raise { err_value; loc_ } ->
       typing_raise env err_value expect_ty ~loc:loc_ ~control_ctx ~tvar_env
         ~cenv ~global_env ~diagnostics
   | Pexpr_apply { func; args; attr; loc_ } ->
       typing_application env (infer_go env func) args ~expect_ty ~kind:Normal
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~attr ~diagnostics
   | Pexpr_unit { loc_; _ } ->
       unify_expr_and_store_error ~expect_ty ~actual_ty:Stype.unit ~diagnostics
         loc_;
       Texpr_unit { loc_ }
   | Pexpr_dot_apply { self; method_name; args; return_self; attr; loc_ } ->
       let self = infer_go env self in
       let ty_self = Typedtree_util.type_of_typed_expr self in
       let method_expr =
         typing_self_method ty_self method_name
           ~src:Type_constraint.Dot_src_direct ~loc:method_name.loc_ ~tvar_env
           ~cenv ~global_env ~diagnostics
       in
       typing_application env method_expr args ~self ~expect_ty
         ~kind:(if return_self then Dot_return_self else Dot)
         ~loc:loc_ ~control_ctx ~tvar_env ~cenv ~global_env ~attr ~diagnostics
   | Pexpr_break { arg; label; loc_ } ->
       check_break env arg ~label ~loc:loc_ ~expect_ty ~global_env ~tvar_env
         ~cenv ~control_ctx ~diagnostics
   | Pexpr_continue { args; label; loc_ } ->
       check_continue env args ~label ~loc:loc_ ~expect_ty ~global_env ~tvar_env
         ~cenv ~control_ctx ~diagnostics
   | Pexpr_map { elems; loc_ } ->
       typing_map_expr env elems ~expect_ty:(Some expect_ty) ~control_ctx
         ~tvar_env ~cenv ~global_env ~diagnostics ~loc:loc_
   | Pexpr_hole { kind; loc_ } ->
       (match kind with
       | Incomplete -> add_error diagnostics (Errors.found_hole loc_)
       | Synthesized -> ()
       | Todo ->
           control_ctx.may_has_error := true;
           Local_diagnostics.add_warning diagnostics { kind = Todo; loc = loc_ });
       Texpr_hole { ty = expect_ty; loc_; kind }
   | Pexpr_multiline_string { elems; loc_ } ->
       check_go env
         (desugar_multiline_string ~loc_ elems)
         (Expect_type expect_ty)
   | Pexpr_array_get_slice _ | Pexpr_field _ | Pexpr_method _ | Pexpr_infix _
   | Pexpr_ident _ | Pexpr_assign _ | Pexpr_as _ | Pexpr_is _ ->
       let texpr =
         infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       unify_expr_and_store_error ~expect_ty
         ~actual_ty:(Typedtree_util.type_of_typed_expr texpr)
         ~diagnostics
         (Syntax.loc_of_expression expr);
       texpr
   | Pexpr_group _ -> assert false
    : Typedtree.expr)

and maybe_check (env : Local_env.t) (maybe : maybe_typed) (expect_ty : Stype.t)
    ~(control_ctx : Control_ctx.t) ~(tvar_env : Tvar_env.t)
    ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (match maybe with
   | Typechecked texpr ->
       handle_unify_result ~diagnostics ~expr:texpr
         (unify_expr_allow_trait_upcast ~global_env ~cenv ~expect_ty
            ~actual_ty:(Typedtree_util.type_of_typed_expr texpr)
            (Typedtree.loc_of_typed_expr texpr))
   | Not_yet expr ->
       check_expr env expr (Expect_type expect_ty) ~control_ctx ~tvar_env ~cenv
         ~global_env ~diagnostics
    : Typedtree.expr)

and type_guided_record_check ~(type_name : Typedtree.type_name option)
    (fields : Syntax.field_def list) labels record_ty ~env ~control_ctx
    ~tvar_env ~cenv ~global_env ~diagnostics ~loc =
  (let superfluous =
     Typeutil.validate_record ~context:`Creation ~expected:labels
       (Lst.map fields (fun (Field_def { label; _ }) -> label))
       ~record_ty ~is_strict:true ~loc ~diagnostics
   in
   let check_label ty is_mut pos label_name =
     Lst.fold_right fields [] (fun (Field_def { label; expr; is_pun; _ }) ->
         fun acc ->
          if label.label_name = label_name then
            let expr =
              check_expr env expr (Expect_type ty) ~control_ctx ~tvar_env ~cenv
                ~global_env ~diagnostics
            in
            Typedtree.Field_def { label; expr; is_mut; is_pun; pos } :: acc
          else acc)
       [@@inline]
   in
   let check_superfluous label_name =
     check_label
       (Stype.new_type_var Tvar_error)
       false Typeutil.unknown_pos label_name
       [@@inline]
   in
   let fields =
     Lst.flat_map_append labels
       ~init:(Lst.concat_map superfluous check_superfluous)
       ~f:(fun ({ field_name; _ } as field_info) ->
         check_label field_info.ty_field field_info.mut field_info.pos
           field_name)
   in
   Texpr_record { type_name; fields; ty = record_ty; loc_ = loc }
    : Typedtree.expr)

and type_guided_record_update_check (env : Local_env.t)
    (old_fields : Typedecl_info.fields) (new_fields : Syntax.field_def list)
    (ty_record : Stype.t) ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
    =
  (let lookup_label (label : string) ~loc =
     (match Lst.find_first old_fields (fun f -> f.field_name = label) with
      | Some f -> Ok f
      | None ->
          Error
            (Errors.field_not_found
               ~ty:(Printer.type_to_string ty_record)
               ~label ~loc)
       : Typedecl_info.field Local_diagnostics.info)
   in
   let update (Field_def { label; expr; is_pun; loc_; _ } : Syntax.field_def) =
     let old = lookup_label label.label_name ~loc:loc_ in
     match old with
     | Error e ->
         add_error diagnostics e;
         Typedtree.Field_def
           {
             label;
             expr =
               infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
                 ~diagnostics;
             is_mut = false;
             is_pun = false;
             pos = 0;
           }
     | Ok old ->
         let new_expr =
           check_expr env expr (Expect_type old.ty_field) ~control_ctx ~tvar_env
             ~cenv ~global_env ~diagnostics
         in
         Typedtree.Field_def
           { label; expr = new_expr; is_mut = old.mut; is_pun; pos = old.pos }
   in
   let is_dup =
     Basic_duplicate_check.check_duplicate_by new_fields (fun x ->
         let (Syntax.Field_def { label; _ }) = x in
         label.label_name)
   in
   (match is_dup with
   | Some (Field_def { label = { label_name = label; loc_ = loc }; _ }) ->
       add_error diagnostics
         (Errors.duplicate_record_field ~context:`Creation ~label ~loc)
   | None -> ());
   Lst.map new_fields update
    : Typedtree.field_def list)

and infer_record (env : Local_env.t) (fields : Syntax.field_def list) loc
    ~(type_name : Typedtree.type_name option) ~control_ctx ~tvar_env ~cenv
    ~global_env ~diagnostics =
  (let handle_error err =
     (add_error diagnostics err;
      let ty = Stype.new_type_var Tvar_error in
      let fields =
        Lst.map fields (fun (Field_def { label; expr; is_pun; _ }) ->
            Typedtree.Field_def
              {
                label;
                expr =
                  infer_expr env expr ~control_ctx ~tvar_env ~cenv ~global_env
                    ~diagnostics;
                is_mut = false;
                is_pun;
                pos = 0;
              })
      in
      Texpr_record { type_name; fields; ty; loc_ = loc }
       : Typedtree.expr)
   in
   if fields = [] then handle_error (Errors.record_type_missing loc)
   else
     let labels = Lst.map fields (fun (Field_def { label; _ }) -> label) in
     match
       Basic_duplicate_check.check_duplicate_by labels (fun x -> x.label_name)
     with
     | Some { label_name = label; loc_ = loc } ->
         handle_error
           (Errors.duplicate_record_field ~context:`Creation ~label ~loc)
     | None -> (
         match Global_env.resolve_record global_env ~labels ~loc with
         | Error err -> handle_error err
         | Ok (ty_params, ty_record, labels) ->
             let ty_record, labels =
               Poly_type.instantiate_record
                 ~ty_record:(`Generic (ty_params, ty_record))
                 labels
             in
             type_guided_record_check fields labels ty_record ~type_name ~env
               ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics ~loc)
    : Typedtree.expr)

and infer_function (env : Local_env.t) (params : Syntax.parameters)
    (params_loc_ : Rloc.t) (body : Syntax.expr)
    (return_type : (Syntax.typ * Syntax.error_typ) option)
    ~(kind_ : Syntax.fn_kind) ~is_async ~tvar_env ~cenv ~global_env ~has_error
    ~diagnostics =
  (let typed_fn_annotation =
     typing_function_annotation params return_type ~tvar_env ~global_env
       ~has_error ~is_async ~diagnostics
   in
   check_function ~is_impl_method:false env params params_loc_ body
     typed_fn_annotation ~kind_ ~is_global:false ~is_in_test:false ~tvar_env
     ~cenv ~global_env ~diagnostics
    : Typedtree.fn * Stype.t)

and typing_function_annotation (params : Syntax.parameters)
    (return_type : (Syntax.typ * Syntax.error_typ) option)
    ~(has_error : Rloc.t option) ~is_async ~tvar_env ~global_env ~diagnostics =
  (let typing_annotation ann_opt =
     match ann_opt with
     | Some ann ->
         let ty = typing_type ann ~tvar_env ~global_env ~diagnostics in
         let stype = Typedtree_util.stype_of_typ ty in
         (stype, Some ty)
     | None -> (Stype.new_type_var Tvar_normal, None)
   in
   let ({ ret_sty; err_sty; annotation } : Typeutil.ret_info) =
     Typeutil.handle_return_annotation return_type ~has_error
       ~typing_type:(typing_type ~tvar_env ~global_env ~diagnostics)
   in
   {
     params_ty =
       Lst.map params (fun (p : Syntax.parameter) ->
           typing_annotation (Syntax.ty_of_param p));
     ret_ty = ret_sty;
     err_ty = err_sty;
     is_async;
     ret_annotation = annotation;
   }
    : Local_typing_worklist.typed_fn_annotation)

and typing_interp ?expect_ty ~control_ctx ~global_env ~diagnostics ~tvar_env
    ~cenv env elems loc_ =
  let infer_go =
    infer_expr ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
  in
  let elems =
    Lst.map elems (function
      | Syntax.Interp_source _ -> assert false
      | Interp_lit { str; repr = _ } -> Typedtree.Interp_lit str
      | Interp_expr { expr; loc_ } ->
          let expr = infer_go env expr in
          let self_type = Typedtree_util.type_of_typed_expr expr in
          let to_string : Typedtree.expr =
            Texpr_unresolved_method
              {
                trait_name =
                  Tname_path
                    {
                      name = Type_path.Builtin.trait_show;
                      kind = Trait;
                      loc_ = Rloc.no_location;
                    };
                method_name = "to_string";
                self_type;
                arity_ = Some (Fn_arity.simple 1);
                ty =
                  Builtin.type_arrow [ self_type ] Stype.string ~err_ty:None
                    ~is_async:false;
                loc_ = Rloc.no_location;
              }
          in
          Poly_type.add_constraint cenv self_type
            { trait = Type_path.Builtin.trait_show; loc_; src_ = Direct };
          Interp_expr { expr; to_string; loc_ })
  in
  let ty =
    match expect_ty with
    | Some ty when Type.same_type ty Stype.json -> Stype.json
    | Some expect_ty ->
        unify_expr_and_store_error ~expect_ty ~actual_ty:Stype.string
          ~diagnostics loc_;
        Stype.string
    | None -> Stype.string
  in
  Texpr_interp { elems; ty; loc_ }

and check_function_params (env : Local_env.t) (params : Syntax.parameters)
    (param_types : (Stype.t * Typedtree.typ option) list) ~(is_global : bool)
    ~tvar_env ~cenv ~global_env ~diagnostics =
  (let env = ref env in
   let params =
     Lst.map2 params param_types (fun (p : Syntax.parameter) ->
         fun (stype, ty) ->
          if
            (not is_global)
            &&
            match p with
            | Discard_positional _ | Positional _ -> false
            | Labelled _ | Optional _ | Question_optional _ -> true
          then
            add_error diagnostics
              (Errors.no_local_labelled_function (Syntax.loc_of_param_binder p));
          let kind : Typedtree.param_kind =
            match p with
            | Discard_positional _ | Positional _ -> Positional
            | Labelled _ -> Labelled
            | Optional { default = Pexpr_hole { kind = Incomplete; loc_ }; _ }
              ->
                if
                  Type.same_type stype Stype.type_sourceloc
                  || Type.same_type stype Stype.type_argsloc
                then Autofill
                else (
                  add_error diagnostics
                    (Errors.unsupported_autofill
                       ~ty:(Printer.type_to_string stype)
                       ~loc:(Syntax.loc_of_param_binder p));
                  Optional (Texpr_hole { loc_; ty = stype; kind = Incomplete }))
            | Optional { default; _ } ->
                Optional
                  (check_expr !env default (Expect_type stype) ~global_env
                     ~tvar_env ~cenv ~diagnostics ~control_ctx:Control_ctx.empty)
            | Question_optional _ -> Question_optional
          in
          let param_ty =
            match kind with
            | Question_optional -> Builtin.type_option stype
            | Positional | Labelled | Optional _ | Autofill -> stype
          in
          match p with
          | Discard_positional _ ->
              Typedtree.Discard_positional_param
                {
                  ty = param_ty;
                  konstraint = ty;
                  loc_ = Syntax.loc_of_param_binder p;
                }
          | Positional { binder; _ }
          | Labelled { binder; _ }
          | Optional { binder; _ }
          | Question_optional { binder; _ } ->
              let binder = Typeutil.fresh_binder binder in
              env := Typeutil.add_binder !env binder ~typ:param_ty ~mut:false;
              Typedtree.Param { binder; ty = param_ty; konstraint = ty; kind })
   in
   (!env, params)
    : Local_env.t * Typedtree.params)

and check_function (env : Local_env.t) (params : Syntax.parameters)
    (params_loc_ : Rloc.t) (body : Syntax.expr)
    (typed_fn_annotation : Local_typing_worklist.typed_fn_annotation)
    ~(kind_ : Syntax.fn_kind) ~(is_global : bool) ~tvar_env ~cenv ~global_env
    ~is_in_test ~is_impl_method ~diagnostics =
  (let ({
          params_ty = param_types;
          ret_ty = return_type;
          err_ty = error_type;
          is_async;
          ret_annotation = annotation;
        }
         : Local_typing_worklist.typed_fn_annotation) =
     typed_fn_annotation
   in
   let env, params =
     check_function_params env params param_types ~is_global ~tvar_env ~cenv
       ~global_env ~diagnostics
   in
   let error_ctx =
     match error_type with
     | None -> None
     | Some t ->
         let ctx : Control_ctx.error_ctx =
           match Stype.type_repr t with
           | T_constr { type_constructor = Basic_type_path.T_error; _ } ->
               Fixed_ctx Supererror
           | T_constr { type_constructor = p; is_suberror_ = true; _ } ->
               Fixed_ctx (Suberror p)
           | Tparam { index; name_ } -> Fixed_ctx (Tparam { index; name_ })
           | _ -> Open_ctx Empty_ctx
         in
         Some (ref ctx)
   in
   let control_ctx =
     Control_ctx.make_fn ~return:return_type ~error_ctx ~is_async
   in
   let body =
     check_expr env body (Expect_type return_type) ~control_ctx ~tvar_env ~cenv
       ~global_env ~diagnostics
   in
   if
     (not !(control_ctx.may_has_error))
     && (not is_in_test) && not is_impl_method
   then Typeutil.report_unused_error_annotation annotation diagnostics;
   let ret_constraint =
     match annotation with
     | Annotated r -> Some r
     | Has_super_error _ | No_annotation -> None
   in
   let func_typ =
     Builtin.type_arrow (Lst.map param_types fst) return_type ~err_ty:error_type
       ~is_async
   in
   ( { params; params_loc_; body; ty = func_typ; ret_constraint; kind_ },
     func_typ )
    : Typedtree.fn * Stype.t)

and deref_newtype ~(global_env : Global_env.t) (expr : Typedtree.expr) ~loc =
  (let ty = Stype.type_repr (Typedtree_util.type_of_typed_expr expr) in
   match ty with
   | T_constr { type_constructor = p; _ } -> (
       match Global_env.find_type_by_path global_env p with
       | Some { ty_desc = New_type { newtype_constr; _ }; _ } -> (
           match[@warning "-fragile-match"]
             Poly_type.instantiate_constr newtype_constr
           with
           | ty_res, ty_arg :: [] ->
               Ctype.unify_exn ty ty_res;
               Texpr_field
                 {
                   record = expr;
                   accessor = Newtype;
                   ty = ty_arg;
                   pos = 0;
                   loc_ = loc;
                 }
           | _ -> assert false)
       | _ -> expr)
   | _ -> expr
    : Typedtree.expr)

and resolve_field ~(global_env : Global_env.t) (record : Typedtree.expr)
    (label : Syntax.label) ~(may_be_method : unit -> bool)
    ~(src_ty_record : Stype.t) ~(loc : Rloc.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let name = label.label_name in
   let ty_record = Stype.type_repr (Typedtree_util.type_of_typed_expr record) in
   let error () =
     match Global_env.try_pick_field global_env name with
     | Some ({ pos; mut; vis; _ } as field_desc) ->
         let _, ty_field = Poly_type.instantiate_field field_desc in
         (ty_field, pos, mut, vis)
     | None ->
         (Stype.new_type_var Tvar_error, Typeutil.unknown_pos, true, Read_write)
       [@@inline]
   in
   let not_a_record () =
     let kind =
       match src_ty_record with
       | Tvar _ -> "unknown"
       | Tarrow _ -> "function"
       | Tparam _ -> "type parameter"
       | T_trait _ -> "trait"
       | T_builtin _ -> Printer.type_to_string ty_record
       | T_constr { type_constructor = p; _ } -> (
           match Global_env.find_type_by_path global_env p with
           | Some { ty_desc = Record_type _; _ } -> assert false
           | Some { ty_desc = Variant_type _ | ErrorEnum_type _; _ } ->
               "variant"
           | Some { ty_desc = New_type _; _ } -> "new type"
           | Some { ty_desc = Error_type _; _ } -> "error type"
           | Some { ty_desc = Extern_type | Abstract_type; _ } -> "abstract"
           | None -> Type_path_util.name p)
       | T_blackhole -> assert false
     in
     let ty = Printer.type_to_string src_ty_record in
     let may_be_method = if may_be_method () then Some name else None in
     add_error diagnostics (Errors.not_a_record ~ty ~may_be_method ~kind ~loc);
     error ()
       [@@inline]
   in
   let handle_struct_fields (fields : Typedecl_info.fields) =
     match Lst.find_first fields (fun f -> f.field_name = name) with
     | Some field_info ->
         let ty_record', ty_field = Poly_type.instantiate_field field_info in
         Ctype.unify_exn ty_record ty_record';
         (ty_field, field_info.pos, field_info.mut, field_info.vis)
     | _ ->
         let ty = Printer.type_to_string src_ty_record in
         let may_be_method = may_be_method () in
         add_error diagnostics
           (Errors.no_such_field ~ty ~field:name ~may_be_method ~loc);
         error ()
       [@@inline]
   in
   let handle_enum_fields (constrs : Typedecl_info.constructors)
       (p : Type_path.t) =
     match record with
     | Texpr_ident { kind = Value_constr tag; _ } -> (
         let constr =
           Lst.find_first constrs (fun constr ->
               Constr_info.equal constr.cs_tag tag)
         in
         match constr with
         | None -> not_a_record ()
         | Some ({ constr_name; _ } as constr_info) -> (
             let ty_res, ty_args = Poly_type.instantiate_constr constr_info in
             Ctype.unify_exn ty_res ty_record;
             match
               Fn_arity.find_constr_label constr_info.cs_arity_ ty_args
                 ~label:label.label_name
             with
             | None ->
                 add_error diagnostics
                   (Errors.constr_no_such_field ~ty:p ~constr:constr_name
                      ~field:label.label_name ~loc);
                 error ()
             | Some (ty, offset, mut) -> (ty, offset, mut, constr_info.cs_vis)))
     | _ -> not_a_record ()
       [@@inline]
   in
   match ty_record with
   | T_constr { type_constructor = T_local { name; _ } as p; _ } -> (
       match[@warning "-fragile-match"]
         Global_env.get_cur_local_type_env global_env
       with
       | Some local_type_env -> (
           match Local_type.find_type local_type_env name with
           | Some { kind = Struct fields; _ } -> handle_struct_fields fields
           | Some { kind = Enum constrs; _ } -> handle_enum_fields constrs p
           | Some { kind = Newtype _; _ } -> not_a_record ()
           | Some { kind = Placeholder; _ } | None -> assert false)
       | _ -> assert false)
   | T_constr { type_constructor = p; _ } -> (
       match Global_env.find_type_by_path global_env p with
       | Some { ty_desc = Record_type { fields }; _ } ->
           handle_struct_fields fields
       | Some { ty_desc = Variant_type constrs | ErrorEnum_type constrs; _ } ->
           handle_enum_fields constrs p
       | _ -> not_a_record ())
   | Tvar { contents = Tnolink Tvar_error } | T_blackhole -> error ()
   | _ -> not_a_record ()
    : Stype.t * int * bool * Typedecl_info.type_component_visibility)

and typing_mutate (env : Local_env.t) (record : Syntax.expr)
    (label : Syntax.label) (field : Syntax.expr)
    (augmented_by : Syntax.var option) loc ~control_ctx ~tvar_env ~cenv
    ~global_env ~(diagnostics : _) =
  (let name = label.label_name in
   let record =
     infer_expr env record ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
   in
   let ty_record = Stype.type_repr (Typedtree_util.type_of_typed_expr record) in
   let record =
     let loc = Rloc.merge (Typedtree.loc_of_typed_expr record) label.loc_ in
     deref_newtype ~global_env record ~loc
   in
   let ty_field, pos, is_mut, vis =
     resolve_field ~global_env record label
       ~may_be_method:(fun () -> false)
       ~src_ty_record:ty_record ~diagnostics ~loc
   in
   (if not is_mut then
      let error = Errors.immutable_field ~label:name ~loc in
      add_error diagnostics error);
   (if Stype.is_external ty_record && vis <> Read_write then
      let error = Errors.mutate_readonly_field ~label:name ~loc in
      add_error diagnostics error);
   match augmented_by with
   | None ->
       let field =
         check_expr env field (Expect_type ty_field) ~control_ctx ~tvar_env
           ~cenv ~global_env ~diagnostics
       in
       Texpr_mutate
         {
           record;
           label;
           field;
           augmented_by = None;
           ty = Stype.unit;
           pos;
           loc_ = loc;
         }
   | Some op ->
       let lhs : Typedtree.expr =
         Texpr_field
           {
             record;
             accessor = Label label;
             ty = ty_field;
             pos;
             loc_ = Typedtree.loc_of_typed_expr record;
           }
       in
       let infix_expr =
         typing_infix_op env op lhs field ~loc ~global_env ~cenv ~tvar_env
           ~control_ctx ~diagnostics
       in
       let op_expr, field =
         match infix_expr with
         | Texpr_apply { func; args = [ _; { arg_value; _ } ]; ty; _ } ->
             unify_expr_and_store_error ~expect_ty:ty_field ~actual_ty:ty
               ~diagnostics loc;
             (func, arg_value)
         | _ -> assert false
       in
       Texpr_mutate
         {
           record;
           label;
           field;
           augmented_by = Some op_expr;
           ty = Stype.unit;
           pos;
           loc_ = loc;
         }
    : Typedtree.expr)

and typing_letrec (env : Local_env.t)
    (funs : (Syntax.binder * Syntax.func) list) ~(tvar_env : Tvar_env.t) ~cenv
    ~(global_env : Global_env.t) ~(diagnostics : Local_diagnostics.t) =
  (let func_types =
     Lst.map funs (fun (f, func) ->
         match func with
         | Lambda { parameters; body = _; return_type; has_error; is_async } ->
             let typed_fn_annotation =
               typing_function_annotation parameters return_type ~has_error
                 ~is_async ~tvar_env ~global_env ~diagnostics
             in
             (Typeutil.fresh_binder f, typed_fn_annotation)
         | Match _ -> assert false)
   in
   let env_with_funs =
     Lst.fold_left func_types env (fun env ->
         fun (binder, typed_fn_ann) ->
          let typ =
            Builtin.type_arrow
              (Lst.map typed_fn_ann.params_ty fst)
              typed_fn_ann.ret_ty ~err_ty:typed_fn_ann.err_ty
              ~is_async:typed_fn_ann.is_async
          in
          Typeutil.add_binder env binder ~typ ~mut:false)
   in
   let tfuns =
     Lst.map2 funs func_types (fun (_, func) ->
         fun (binder, typed_fn_annotation) ->
          match func with
          | Lambda
              {
                parameters = ps;
                params_loc_;
                body = funbody;
                return_type = _;
                kind_;
                has_error = _;
                is_async = _;
              } ->
              let tfun, _ =
                check_function ~is_impl_method:false env_with_funs ps
                  params_loc_ funbody typed_fn_annotation ~kind_
                  ~is_global:false ~is_in_test:false ~tvar_env ~cenv ~global_env
                  ~diagnostics
              in
              (binder, tfun)
          | Match _ -> assert false)
   in
   (env_with_funs, tfuns)
    : Local_env.t * (Typedtree.binder * Typedtree.fn) list)

and typing_let (env : Local_env.t) (p : Syntax.pattern) (e : Syntax.expr)
    ~control_ctx ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t)
    ~(global_env : Global_env.t) ~(diagnostics : Local_diagnostics.t) =
  (match (p, e) with
   | (Ppat_constraint { ty = ty_expr; _ } as pc), _ ->
       let ty = typing_type ty_expr ~tvar_env ~global_env ~diagnostics in
       let stype = Typedtree_util.stype_of_typ ty in
       let te =
         check_expr env e (Expect_type stype) ~control_ctx ~tvar_env ~cenv
           ~global_env ~diagnostics
       in
       let pat_binders, tpat =
         Pattern_typer.check_pat pc stype ~tvar_env ~cenv ~global_env
           ~diagnostics
       in
       (pat_binders, tpat, te)
   | _ ->
       let te =
         infer_expr env e ~control_ctx ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let ty = Typedtree_util.type_of_typed_expr te in
       let pat_binders, tpat =
         Pattern_typer.check_pat p ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       (pat_binders, tpat, te)
    : Typedtree.pat_binders * Typedtree.pat * Typedtree.expr)

and typing_letmut (env : Local_env.t) (binder : Syntax.binder)
    (ty : Syntax.typ option) (e : Syntax.expr) ~control_ctx
    ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let binder = Typeutil.fresh_binder binder in
   let var_ty, expr, konstraint =
     match ty with
     | Some ty_expr ->
         let ty = typing_type ty_expr ~tvar_env ~global_env ~diagnostics in
         let stype = Typedtree_util.stype_of_typ ty in
         ( stype,
           check_expr env e (Expect_type stype) ~control_ctx ~tvar_env ~cenv
             ~global_env ~diagnostics,
           Some ty )
     | None ->
         let expr =
           infer_expr env e ~control_ctx ~tvar_env ~cenv ~global_env
             ~diagnostics
         in
         (Typedtree_util.type_of_typed_expr expr, expr, None)
   in
   let env = Typeutil.add_binder env binder ~typ:var_ty ~mut:true in
   (env, binder, expr, konstraint)
    : Local_env.t * Typedtree.binder * Typedtree.expr * Typedtree.typ option)

and check_loop (env : Local_env.t) (args : Syntax.expr list)
    (body : Syntax.multi_arg_case list) ~label ~(expect_ty : expect_ty)
    ~loop_loc_ ~loc ~control_ctx ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t)
    ~(global_env : Global_env.t) ~(diagnostics : Local_diagnostics.t) =
  (let targs, targ_tys, params, params_expr, targs_ty =
     let make_param_expr targ_ty =
       let param_id = Ident.fresh "*param" in
       let loc_ = Rloc.no_location in
       let binder : Typedtree.binder = { binder_id = param_id; loc_ } in
       let param : Typedtree.param =
         Param { binder; konstraint = None; ty = targ_ty; kind = Positional }
       in
       let id : Typedtree.var = { var_id = param_id; loc_ } in
       let param_expr : Typedtree.expr =
         Texpr_ident
           {
             id;
             ty_args_ = [||];
             arity_ = None;
             kind = Normal;
             ty = targ_ty;
             loc_;
           }
       in
       (param, param_expr)
         [@@inline]
     in
     match args with
     | [] -> assert false
     | arg :: [] ->
         let targ =
           infer_expr ~global_env ~tvar_env ~cenv ~diagnostics ~control_ctx env
             arg
         in
         let targ_ty = Typedtree_util.type_of_typed_expr targ in
         let param, param_expr = make_param_expr targ_ty in
         ([ targ ], [ targ_ty ], [ param ], param_expr, targ_ty)
     | args ->
         let rec go args =
           match args with
           | [] -> ([], [], [], [])
           | arg :: rest ->
               let targ =
                 infer_expr ~global_env ~tvar_env ~cenv ~diagnostics
                   ~control_ctx env arg
               in
               let targ_ty = Typedtree_util.type_of_typed_expr targ in
               let param, param_expr = make_param_expr targ_ty in
               let targs, params, targ_tys, param_exprs = go rest in
               ( targ :: targs,
                 param :: params,
                 targ_ty :: targ_tys,
                 param_expr :: param_exprs )
         in
         let targs, params, targ_tys, param_exprs = go args in
         let ty_tuple = Builtin.type_product targ_tys in
         let params_expr : Typedtree.expr =
           let loc_ =
             Rloc.merge
               (Syntax.loc_of_expression (List.hd args))
               (Syntax.loc_of_expression (Lst.last args))
           in
           Texpr_tuple { exprs = param_exprs; ty = ty_tuple; loc_ }
         in
         (targs, targ_tys, params, params_expr, ty_tuple)
   in
   let label : Typedtree.loop_label_binder option =
     match label with
     | None -> None
     | Some label ->
         Some { label = Label.fresh label.label_name; loc_ = label.loc_ }
   in
   let control_ctx =
     Control_ctx.with_loop control_ctx ~arg_typs:targ_tys ~label
       ~result_typ:(get_expected_type expect_ty)
       ~diagnostics
   in
   let cases =
     Lst.map body (fun { patterns = pats; guard; body = action } ->
         (let ty_pat =
            if Lst.same_length targs pats then targs_ty
            else
              let loc =
                Rloc.merge
                  (Syntax.loc_of_pattern (List.hd pats))
                  (Syntax.loc_of_pattern (Lst.last pats))
              in
              let expected = List.length args in
              let actual = List.length pats in
              add_error diagnostics
                (Errors.loop_pat_arity_mismatch ~expected ~actual ~loc);
              let tys =
                if expected < actual then
                  targ_tys @ Type.type_var_list (actual - expected) Tvar_error
                else Lst.take targ_tys actual
              in
              match tys with ty :: [] -> ty | tys -> Builtin.type_product tys
          in
          let pat : Syntax.pattern =
            match pats with
            | p :: [] -> p
            | pats -> Ppat_tuple { pats; loc_ = Rloc.no_location }
          in
          let pat_binders, tpat =
            Pattern_typer.check_pat pat ty_pat ~tvar_env ~cenv ~global_env
              ~diagnostics
          in
          let env1 = Typeutil.add_pat_binders env pat_binders in
          let guard, env2 =
            match guard with
            | None -> (None, env1)
            | Some guard ->
                let guard =
                  check_expr env1 guard (Expect_type Stype.bool) ~control_ctx
                    ~tvar_env ~cenv ~global_env ~diagnostics
                in
                ( Some guard,
                  Typeutil.add_pat_binders env1
                    (Typedtree_util.pat_binders_of_cond guard) )
          in
          let taction =
            check_expr ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
              env2 action expect_ty
          in
          { pat = tpat; pat_binders; action = taction; guard }
           : Typedtree.match_case))
   in
   if not (Control_ctx.loop_has_continue control_ctx) then
     Local_diagnostics.add_warning diagnostics
       { kind = Useless_loop; loc = loop_loc_ };
   let ty =
     match cases with
     | { action; _ } :: _ -> Typedtree_util.type_of_typed_expr action
     | [] -> get_expected_type expect_ty
   in
   let body : Typedtree.expr =
     Texpr_match
       { expr = params_expr; cases; ty; match_loc_ = loop_loc_; loc_ = loc }
   in
   Control_ctx.warn_if_label_unused label control_ctx ~diagnostics;
   Texpr_loop { params; body; args = targs; ty; loc_ = loc; label }
    : Typedtree.expr)

and check_while (env : Local_env.t) (cond : Syntax.expr) (body : Syntax.expr)
    (while_else : Syntax.expr option) ~(label : Syntax.label option)
    ~(expect_ty : expect_ty) ~loc ~control_ctx ~(tvar_env : Tvar_env.t)
    ~(cenv : Poly_type.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let typed_cond =
     check_expr ~global_env ~tvar_env ~cenv ~diagnostics
       ~control_ctx:(Control_ctx.with_ambiguous_position control_ctx)
       env cond expect_bool
   in
   let label : Typedtree.loop_label_binder option =
     match label with
     | None -> None
     | Some label ->
         Some { label = Label.fresh label.label_name; loc_ = label.loc_ }
   in
   let new_control_ctx =
     match while_else with
     | Some _ ->
         Control_ctx.with_while
           ~break_typ:(Some (get_expected_type expect_ty))
           control_ctx ~label ~diagnostics
     | None ->
         Control_ctx.with_while ~break_typ:None control_ctx ~label ~diagnostics
   in
   let pat_binders = Typedtree_util.pat_binders_of_cond typed_cond in
   let typed_body =
     check_expr ~global_env ~tvar_env ~cenv ~diagnostics
       ~control_ctx:new_control_ctx
       (Typeutil.add_pat_binders env pat_binders)
       body Ignored
   in
   let typed_while_else =
     match while_else with
     | None ->
         (match expect_ty with
         | Expect_type expect_ty when not (Ctype.try_unify expect_ty Stype.unit)
           ->
             add_error diagnostics
               (Errors.need_else_branch ~loop_kind:`While ~loc
                  ~ty:(Printer.type_to_string expect_ty))
         | Ignored | Expect_type _ -> ());
         None
     | Some while_else ->
         Some
           (check_expr ~global_env ~tvar_env ~cenv ~diagnostics ~control_ctx env
              while_else expect_ty)
   in
   Control_ctx.warn_if_label_unused label new_control_ctx ~diagnostics;
   Texpr_while
     {
       loop_cond = typed_cond;
       loop_body = typed_body;
       ty =
         (match typed_while_else with
         | Some expr -> Typedtree_util.type_of_typed_expr expr
         | None -> Stype.unit);
       while_else = typed_while_else;
       label;
       loc_ = loc;
     }
    : Typedtree.expr)

and check_for (env : Local_env.t) (binders : (Syntax.binder * Syntax.expr) list)
    (condition : Syntax.expr option)
    (steps : (Syntax.binder * Syntax.expr) list) (body : Syntax.expr)
    (for_else : Syntax.expr option) ~label ~(expect_ty : expect_ty) ~loc
    ~control_ctx ~(tvar_env : Tvar_env.t) ~(cenv : Poly_type.t)
    ~(global_env : Global_env.t) ~(diagnostics : Local_diagnostics.t) =
  (let is_relaxed_for = condition = None in
   let ambiguous_control_ctx =
     Control_ctx.with_ambiguous_position control_ctx
   in
   let typed_binders_rev =
     Lst.fold_left binders [] (fun acc ->
         fun (binder, init) ->
          let name = binder.binder_name in
          let binder_loc = binder.loc_ in
          (if
             Lst.exists_fst acc (fun b ->
                 Ident.base_name b.Typedtree.binder_id = name)
           then
             let error =
               Errors.duplicate_for_binder ~name:binder.binder_name
                 ~loc:binder_loc
             in
             add_error diagnostics error);
          let binder = Typeutil.fresh_binder binder in
          let init =
            infer_expr ~global_env ~tvar_env ~cenv ~diagnostics
              ~control_ctx:ambiguous_control_ctx env init
          in
          (binder, init) :: acc)
   in
   let typed_binders = List.rev typed_binders_rev in
   let env_with_binders =
     Lst.fold_left typed_binders env (fun env ->
         fun (binder, init) ->
          Typeutil.add_binder env binder
            ~typ:(Typedtree_util.type_of_typed_expr init)
            ~mut:false)
   in
   let typed_condition =
     match condition with
     | None -> None
     | Some cond ->
         Some
           (check_expr ~global_env ~tvar_env ~cenv
              ~control_ctx:ambiguous_control_ctx ~diagnostics env_with_binders
              cond expect_bool)
   in
   let typed_steps_rev =
     Lst.fold_left steps [] (fun acc ->
         fun (binder, step) ->
          (let name = binder.binder_name in
           let binder_loc = binder.loc_ in
           (if
              Lst.exists_fst acc (fun b ->
                  Ident.base_name b.Typedtree.var_id = name)
            then
              let error =
                Errors.duplicate_for_binder ~name:binder.binder_name
                  ~loc:binder_loc
              in
              add_error diagnostics error);
           match
             Lst.find_opt typed_binders (fun (binder, expr) ->
                 if Ident.base_name binder.binder_id = name then
                   Some (binder, Typedtree_util.type_of_typed_expr expr)
                 else None)
           with
           | Some (id, typ) ->
               let typed_step =
                 check_expr ~global_env ~tvar_env ~cenv ~diagnostics
                   ~control_ctx:ambiguous_control_ctx env_with_binders step
                   (Expect_type typ)
               in
               ({ var_id = id.binder_id; loc_ = binder_loc }, typed_step) :: acc
           | None ->
               let error =
                 Errors.unknown_binder_in_for_steps ~name ~loc:binder_loc
               in
               add_error diagnostics error;
               ( ({ var_id = Ident.fresh name; loc_ = binder_loc }
                   : Typedtree.var),
                 infer_expr env_with_binders step
                   ~control_ctx:ambiguous_control_ctx ~tvar_env ~cenv
                   ~global_env ~diagnostics )
               :: acc
            : (Typedtree.var * _) list))
   in
   let typed_steps = List.rev typed_steps_rev in
   let binder_types =
     Lst.map typed_binders (fun (_, expr) ->
         Typedtree_util.type_of_typed_expr expr)
   in
   let label : Typedtree.loop_label_binder option =
     match label with
     | None -> None
     | Some label ->
         Some { label = Label.fresh label.label_name; loc_ = label.loc_ }
   in
   let new_control_ctx =
     if for_else <> None || is_relaxed_for then
       Control_ctx.with_for ~arg_typs:binder_types
         ~break_typ:(Some (get_expected_type expect_ty))
         control_ctx ~label ~diagnostics
     else
       Control_ctx.with_for ~arg_typs:binder_types ~break_typ:None control_ctx
         ~label ~diagnostics
   in
   let typed_body =
     check_expr ~global_env ~tvar_env ~cenv ~control_ctx:new_control_ctx
       ~diagnostics env_with_binders body Ignored
   in
   let typed_for_else =
     match for_else with
     | None when is_relaxed_for -> None
     | None ->
         (match expect_ty with
         | Expect_type expect_ty when not (Ctype.try_unify expect_ty Stype.unit)
           ->
             add_error diagnostics
               (Errors.need_else_branch ~loop_kind:`For ~loc
                  ~ty:(Printer.type_to_string expect_ty))
         | Ignored | Expect_type _ -> ());
         None
     | Some for_else ->
         Some
           (check_expr ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
              env_with_binders for_else expect_ty)
   in
   Control_ctx.warn_if_label_unused label new_control_ctx ~diagnostics;
   Texpr_for
     {
       binders = typed_binders;
       condition = typed_condition;
       steps = typed_steps;
       body = typed_body;
       for_else = typed_for_else;
       ty = get_expected_type expect_ty;
       loc_ = loc;
       label;
     }
    : Typedtree.expr)

and typing_foreach env binders expr body else_block ~label ~loc ~expect_ty
    ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics =
  let loc_of_expr = Syntax.loc_of_expression expr in
  let n_binders = List.length binders in
  let expr =
    infer_expr env expr ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
  in
  if Type.is_array_like (Typedtree_util.type_of_typed_expr expr) then
    typing_foreach_transform_arraylike env binders expr body else_block ~label
      ~loc ~expect_ty ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
  else
    let expr, elem_tys =
      match Stype.type_repr (Typedtree_util.type_of_typed_expr expr) with
      | T_constr { type_constructor; tys }
        when Type_path.equal type_constructor Type_path.Builtin.type_path_iter
             || Type_path.equal type_constructor
                  Type_path.Builtin.type_path_iter2 ->
          let n_tys = List.length tys in
          if n_tys <> n_binders then (
            add_error diagnostics
              (Errors.foreach_loop_variable_count_mismatch ~actual:n_binders
                 ~expected:(Some n_tys) ~loc);
            let tys =
              if n_tys < n_binders then
                tys
                @ List.init (n_binders - n_tys) (fun _ ->
                      Stype.new_type_var Tvar_error)
              else Lst.take tys n_binders
            in
            (expr, tys))
          else (expr, tys)
      | ty ->
          let elem_tys =
            Lst.map binders (fun _ -> Stype.new_type_var Tvar_normal)
          in
          let method_name, iter_ty =
            match elem_tys with
            | [ ty1; ty2 ] -> ("iter2", Builtin.type_iter2 ty1 ty2)
            | ty :: rest ->
                if rest <> [] then
                  add_error diagnostics
                    (Errors.foreach_loop_variable_count_mismatch
                       ~actual:(List.length elem_tys) ~expected:None ~loc);
                ("iter", Builtin.type_iter ty)
            | [] -> ("iter", Builtin.type_iter (Stype.new_type_var Tvar_error))
          in
          let iter =
            typing_self_method ty
              { label_name = method_name; loc_ = Rloc.no_location }
              ~src:Dot_src_direct ~loc:loc_of_expr ~global_env ~tvar_env ~cenv
              ~diagnostics
          in
          let iter_method_ty = Typedtree_util.type_of_typed_expr iter in
          let actual_ty_str, has_correct_arity =
            match iter with
            | Texpr_ident { arity_ = Some arity; _ } ->
                ( (fun () ->
                    Printer.toplevel_function_type_to_string ~arity
                      iter_method_ty),
                  Fn_arity.equal arity (Fn_arity.simple 1) )
            | _ -> ((fun () -> Printer.type_to_string iter_method_ty), true)
          in
          let expected =
            Builtin.type_arrow [ ty ] iter_ty ~err_ty:None ~is_async:false
          in
          (if not (Ctype.try_unify expected iter_method_ty && has_correct_arity)
           then
             let expected = Printer.type_to_string expected in
             let actual = actual_ty_str () in
             add_error diagnostics
               (Errors.generic_type_mismatch
                  ~header:
                    (("`foreach` loop: `" ^ method_name
                      ^ "` method of iterated expression has wrong type"
                      : Stdlib.String.t)
                      [@merlin.hide])
                  ~expected ~actual ~loc:loc_of_expr));
          ( Texpr_apply
              {
                func = iter;
                args = [ { arg_value = expr; arg_kind = Positional } ];
                kind_ = Dot;
                ty = iter_ty;
                loc_ = Rloc.no_location;
              },
            elem_tys )
    in
    let else_block =
      match else_block with
      | None ->
          (match expect_ty with
          | Expect_type expect_ty
            when not (Ctype.try_unify expect_ty Stype.unit) ->
              add_error diagnostics
                (Errors.need_else_branch ~loop_kind:`For ~loc
                   ~ty:(Printer.type_to_string expect_ty))
          | Ignored | Expect_type _ -> ());
          None
      | Some else_block ->
          Some
            (check_expr env else_block expect_ty ~global_env ~tvar_env ~cenv
               ~control_ctx ~diagnostics)
    in
    let label : Typedtree.loop_label_binder option =
      match label with
      | None -> None
      | Some label ->
          Some { label = Label.fresh label.label_name; loc_ = label.loc_ }
    in
    let new_control_ctx =
      Control_ctx.with_foreach
        ~break_typ:(Option.map Typedtree_util.type_of_typed_expr else_block)
        ~label control_ctx ~diagnostics
    in
    let binders =
      Lst.map binders (fun binder ->
          (match binder with
           | None -> None
           | Some { binder_name; loc_ } ->
               Some { binder_id = Ident.fresh binder_name; loc_ }
            : Typedtree.binder option))
    in
    let new_env =
      Lst.fold_left2 binders elem_tys env (fun binder ->
          fun elem_ty ->
           fun env ->
            match binder with
            | None -> env
            | Some binder ->
                Typeutil.add_binder env binder ~typ:elem_ty ~mut:false)
    in
    let body =
      check_expr new_env body Ignored ~global_env ~tvar_env ~cenv
        ~control_ctx:new_control_ctx ~diagnostics
    in
    let ty =
      match else_block with
      | Some expr -> Typedtree_util.type_of_typed_expr expr
      | None -> Stype.unit
    in
    Control_ctx.warn_if_label_unused label new_control_ctx ~diagnostics;
    Texpr_foreach
      { binders; elem_tys; expr; body; else_block; ty; loc_ = loc; label }

and typing_foreach_transform_arraylike env binders expr_typed body else_block
    ~label ~loc ~expect_ty ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
    =
  let arr_name_raw = "*arr" in
  let arr_name = Ident.fresh arr_name_raw in
  let arr_binder : Typedtree.binder =
    { binder_id = arr_name; loc_ = Rloc.no_location }
  in
  let let_array body =
    let ty = Typedtree_util.type_of_typed_expr body in
    Typedtree.Texpr_let
      {
        pat = Tpat_var { binder = arr_binder; ty; loc_ = Rloc.no_location };
        pat_binders = [ { binder = arr_binder; binder_typ = ty } ];
        ty = Typedtree_util.type_of_typed_expr body;
        rhs = expr_typed;
        loc_ = Typedtree.loc_of_typed_expr expr_typed;
        body;
      }
  in
  let transformed_ast =
    let arr_len_name = "*len" in
    let arr_len_var : Syntax.var =
      { var_name = Lident arr_len_name; loc_ = Rloc.no_location }
    in
    let let_length body =
      Syntax.Pexpr_let
        {
          pattern =
            Syntax.Ppat_var
              { binder_name = arr_len_name; loc_ = Rloc.no_location };
          expr =
            Syntax.Pexpr_dot_apply
              {
                self =
                  Syntax.Pexpr_ident
                    {
                      id =
                        {
                          var_name = Longident.Lident arr_name_raw;
                          loc_ = Rloc.no_location;
                        };
                      loc_ = Rloc.no_location;
                    };
                method_name = { label_name = "length"; loc_ = Rloc.no_location };
                args = [];
                return_self = false;
                attr = No_attr;
                loc_ = Rloc.no_location;
              };
          body;
          loc_ = Rloc.no_location;
        }
    in
    let iter_var_name = "*i" in
    let iter_var : Syntax.var =
      { var_name = Lident iter_var_name; loc_ = Rloc.no_location }
    in
    let for_loop body =
      Syntax.Pexpr_for
        {
          binders =
            [
              ( { binder_name = iter_var_name; loc_ = Rloc.no_location },
                Syntax.Pexpr_constant
                  { c = Syntax.Const_int "0"; loc_ = Rloc.no_location } );
            ];
          condition =
            Some
              (Syntax.Pexpr_infix
                 {
                   op = { var_name = Lident "<"; loc_ = Rloc.no_location };
                   lhs =
                     Syntax.Pexpr_ident
                       { id = iter_var; loc_ = Rloc.no_location };
                   rhs =
                     Syntax.Pexpr_ident
                       { id = arr_len_var; loc_ = Rloc.no_location };
                   loc_ = Rloc.no_location;
                 });
          continue_block =
            [
              ( { binder_name = iter_var_name; loc_ = Rloc.no_location },
                Syntax.Pexpr_infix
                  {
                    op = { var_name = Lident "+"; loc_ = Rloc.no_location };
                    lhs =
                      Syntax.Pexpr_ident
                        { id = iter_var; loc_ = Rloc.no_location };
                    rhs =
                      Syntax.Pexpr_constant
                        { c = Const_int "1"; loc_ = Rloc.no_location };
                    loc_ = Rloc.no_location;
                  } );
            ];
          for_else = else_block;
          body;
          label;
          loc_ = loc;
        }
    in
    let el_binder, it_binder =
      match binders with
      | [] ->
          let fresh_binder_name = Ident.base_name (Ident.fresh "*el") in
          ( Some
              ({ binder_name = fresh_binder_name; loc_ = Rloc.no_location }
                : Syntax.binder),
            None )
      | el_binder :: [] -> (el_binder, None)
      | [ it_binder; el_binder ] -> (el_binder, Some it_binder)
      | it_binder :: el_binder :: _ as binders ->
          Local_diagnostics.add_error diagnostics
            (Errors.foreach_loop_variable_count_mismatch ~loc
               ~actual:(List.length binders) ~expected:None);
          (el_binder, Some it_binder)
    in
    let binder_to_pat : Syntax.binder option -> Syntax.pattern = function
      | None -> Syntax.Ppat_any { loc_ = Rloc.no_location }
      | Some { binder_name; loc_ } -> Syntax.Ppat_var { binder_name; loc_ }
    in
    let let_el body el_binder =
      Syntax.Pexpr_let
        {
          pattern = binder_to_pat el_binder;
          expr =
            Syntax.Pexpr_dot_apply
              {
                self =
                  Syntax.Pexpr_ident
                    {
                      id =
                        {
                          var_name = Longident.Lident arr_name_raw;
                          loc_ = Rloc.no_location;
                        };
                      loc_ = Rloc.no_location;
                    };
                method_name =
                  { label_name = "unsafe_get"; loc_ = Rloc.no_location };
                args =
                  [
                    {
                      arg_value =
                        Syntax.Pexpr_ident
                          { id = iter_var; loc_ = Rloc.no_location };
                      arg_kind = Positional;
                    };
                  ];
                return_self = false;
                attr = No_attr;
                loc_ = Rloc.no_location;
              };
          body;
          loc_ = Rloc.no_location;
        }
    in
    let let_iter body it_binder =
      match it_binder with
      | None -> body
      | Some it_binder ->
          Syntax.Pexpr_let
            {
              pattern = binder_to_pat it_binder;
              expr =
                Syntax.Pexpr_ident { id = iter_var; loc_ = Rloc.no_location };
              body;
              loc_ = Rloc.no_location;
            }
    in
    let for_loop_body = let_iter (let_el body el_binder) it_binder in
    let for_loop = for_loop for_loop_body in
    let_length for_loop
  in
  let tast =
    check_expr
      (Typeutil.add_binder env arr_binder
         ~typ:(Typedtree_util.type_of_typed_expr expr_typed)
         ~mut:false)
      transformed_ast expect_ty ~control_ctx ~tvar_env ~cenv ~global_env
      ~diagnostics
  in
  let_array tast

and typing_range_for_in env (binders : Syntax.binder option list)
    (lhs : Syntax.expr) (rhs : Syntax.expr) (body : Syntax.expr)
    (else_block : Syntax.expr option) ~label ~(inclusive : bool) ~operator_loc
    ~loc ~expect_ty ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics =
  (let tlhs =
     infer_expr env lhs ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
   in
   let lhs_ty = Stype.type_repr (Typedtree_util.type_of_typed_expr tlhs) in
   let trhs, elem_ty =
     let check_rhs expect_ty =
       check_expr env rhs (Expect_type expect_ty) ~global_env ~tvar_env ~cenv
         ~control_ctx ~diagnostics
         [@@inline]
     in
     let is_supported (b : Stype.builtin) =
       match b with
       | T_int | T_uint | T_int64 | T_uint64 -> true
       | T_unit | T_bool | T_byte | T_int16 | T_uint16 | T_char | T_float
       | T_double | T_string | T_bytes ->
           false
         [@@inline]
     in
     match lhs_ty with
     | T_builtin b when is_supported b -> (check_rhs lhs_ty, lhs_ty)
     | Tvar _ -> (
         let trhs = check_rhs lhs_ty in
         match Stype.type_repr (Typedtree_util.type_of_typed_expr trhs) with
         | T_builtin b when is_supported b -> (trhs, lhs_ty)
         | Tvar _ as rhs_ty ->
             Ctype.unify_exn rhs_ty Stype.int;
             (trhs, lhs_ty)
         | _ ->
             add_error diagnostics
               (Errors.range_operator_unsupported_type
                  ~actual_ty:(Printer.type_to_string lhs_ty)
                  ~loc:operator_loc);
             (trhs, Stype.int))
     | _ ->
         add_error diagnostics
           (Errors.range_operator_unsupported_type
              ~actual_ty:(Printer.type_to_string lhs_ty)
              ~loc:operator_loc);
         (check_rhs (Stype.new_type_var Tvar_error), Stype.int)
   in
   let lhs_var =
     ("*start" ^ Int.to_string (Basic_uuid.next ()) : Stdlib.String.t)
   in
   let rhs_var =
     ("*end" ^ Int.to_string (Basic_uuid.next ()) : Stdlib.String.t)
   in
   let hole () =
     (Pexpr_hole { kind = Synthesized; loc_ = Rloc.no_location } : Syntax.expr)
   in
   let loop_binders =
     match binders with
     | [] -> []
     | first_binder :: rest ->
         let first_binder =
           match first_binder with
           | Some binder -> binder
           | None -> { binder_name = "_"; loc_ = Rloc.no_location }
         in
         if rest <> [] then
           add_error diagnostics
             (Errors.foreach_loop_variable_count_mismatch
                ~actual:(List.length binders) ~expected:(Some 1) ~loc);
         ( first_binder,
           Syntax.Pexpr_ident
             {
               id = { var_name = Lident lhs_var; loc_ = Rloc.no_location };
               loc_ = Rloc.no_location;
             } )
         :: Lst.fold_right rest [] (fun binder_opt ->
                fun acc ->
                 match binder_opt with
                 | Some binder -> (binder, hole ()) :: acc
                 | None -> acc)
   in
   let loop_variable : Syntax.expr =
     match loop_binders with
     | (binder, _) :: _ ->
         Pexpr_ident
           {
             id =
               { var_name = Lident binder.binder_name; loc_ = Rloc.no_location };
             loc_ = Rloc.no_location;
           }
     | [] -> hole ()
   in
   let cond_op : Syntax.var =
     if inclusive then { var_name = Lident "<="; loc_ = Rloc.no_location }
     else { var_name = Lident "<"; loc_ = Rloc.no_location }
   in
   let condition : Syntax.expr =
     Pexpr_infix
       {
         op = cond_op;
         lhs = loop_variable;
         rhs =
           Pexpr_ident
             {
               id = { var_name = Lident rhs_var; loc_ = Rloc.no_location };
               loc_ = Rloc.no_location;
             };
         loc_ = Rloc.no_location;
       }
   in
   let loop_update : (Syntax.binder * Syntax.expr) list =
     match loop_binders with
     | (binder, _) :: _ ->
         [
           ( binder,
             Pexpr_infix
               {
                 op = { var_name = Lident "+"; loc_ = Rloc.no_location };
                 lhs = loop_variable;
                 rhs =
                   Pexpr_constant { c = Const_int "1"; loc_ = Rloc.no_location };
                 loc_ = Rloc.no_location;
               } );
         ]
     | [] -> []
   in
   let lhs_binder : Typedtree.binder =
     { binder_id = Ident.fresh lhs_var; loc_ = Rloc.no_location }
   in
   let rhs_binder : Typedtree.binder =
     { binder_id = Ident.fresh rhs_var; loc_ = Rloc.no_location }
   in
   let lhs_pat_binder : Typedtree.pat_binder =
     { binder = lhs_binder; binder_typ = elem_ty }
   in
   let rhs_pat_binder : Typedtree.pat_binder =
     { binder = rhs_binder; binder_typ = elem_ty }
   in
   let loop : Syntax.expr =
     Pexpr_for
       {
         binders = loop_binders;
         condition = Some condition;
         continue_block = loop_update;
         for_else = else_block;
         body;
         label;
         loc_ = loc;
       }
   in
   let tloop =
     check_expr
       (Typeutil.add_pat_binders env [ lhs_pat_binder; rhs_pat_binder ])
       loop expect_ty ~global_env ~tvar_env ~cenv ~control_ctx ~diagnostics
   in
   let ty = Stype.type_repr (Typedtree_util.type_of_typed_expr tloop) in
   Texpr_let
     {
       pat =
         Tpat_var { binder = lhs_binder; ty = elem_ty; loc_ = Rloc.no_location };
       pat_binders = [ lhs_pat_binder ];
       rhs = tlhs;
       ty;
       loc_ = Rloc.no_location;
       body =
         Texpr_let
           {
             pat =
               Tpat_var
                 { binder = rhs_binder; ty = elem_ty; loc_ = Rloc.no_location };
             pat_binders = [ rhs_pat_binder ];
             rhs = trhs;
             ty;
             loc_ = Rloc.no_location;
             body = tloop;
           };
     }
    : Typedtree.expr)

and check_break env arg ~label ~expect_ty ~loc ~global_env ~tvar_env ~cenv
    ~control_ctx ~diagnostics =
  (let check_break_arg break_arg arg label =
     let arg =
       check_expr env arg (Expect_type break_arg) ~global_env ~tvar_env ~cenv
         ~control_ctx ~diagnostics
     in
     Typedtree.Texpr_break { arg = Some arg; ty = expect_ty; loc_ = loc; label }
   in
   let fallback label =
     let arg =
       Option.map
         (fun arg ->
           infer_expr env arg ~global_env ~tvar_env ~cenv ~control_ctx
             ~diagnostics)
         arg
     in
     Typedtree.Texpr_break { arg; ty = expect_ty; loc_ = loc; label }
       [@@inline]
   in
   let check_with_control_info (control_info : Control_ctx.control_info)
       (label : Typedtree.loop_label option) =
     match control_info with
     | Not_in_loop ->
         add_error diagnostics (Errors.outside_loop ~msg:"break" ~loc);
         fallback label
     | Ambiguous_position ->
         add_error diagnostics (Errors.ambiguous_break loc);
         fallback label
     | In_while
     | In_for { break = None; continue = _ }
     | In_foreach { break = None } -> (
         match arg with
         | None -> Texpr_break { arg = None; ty = expect_ty; loc_ = loc; label }
         | Some arg ->
             let kind =
               match control_ctx.control_info with
               | In_while | In_while_with_else _ -> `While
               | In_for _ -> `For
               | In_foreach _ -> `For
               | _ -> assert false
             in
             add_error diagnostics (Errors.invalid_break ~loop_kind:kind ~loc);
             let arg =
               infer_expr env arg ~global_env ~tvar_env ~cenv ~control_ctx
                 ~diagnostics
             in
             Texpr_break { arg = Some arg; ty = expect_ty; loc_ = loc; label })
     | In_loop { break = break_arg; continue = _; has_continue = _ }
     | In_while_with_else { break = break_arg }
     | In_for { break = Some break_arg; continue = _ }
     | In_foreach { break = Some break_arg } -> (
         match arg with
         | None ->
             (try Ctype.unify_exn break_arg Stype.unit
              with _ ->
                add_error diagnostics
                  (Errors.break_type_mismatch
                     ~expected:(Printer.type_to_string break_arg)
                     ~actual:"no arguments" ~loc));
             Texpr_break { arg = None; ty = expect_ty; loc_ = loc; label }
         | Some arg -> check_break_arg break_arg arg label)
       [@@inline]
   in
   match label with
   | None -> check_with_control_info control_ctx.control_info None
   | Some label -> (
       match Lst.assoc_str control_ctx.labeled_controls label.label_name with
       | Some control_info ->
           let label : Typedtree.loop_label =
             { label = control_info.label; loc_ = label.loc_ }
           in
           control_info.used := true;
           check_with_control_info control_info.control_info (Some label)
       | None ->
           let error =
             Errors.use_undeclared_loop_label ~loc:label.loc_ label.label_name
           in
           add_error diagnostics error;
           let label : Typedtree.loop_label =
             { label = Label.fresh label.label_name; loc_ = label.loc_ }
           in
           fallback (Some label))
    : Typedtree.expr)

and check_continue env args ~label ~expect_ty ~loc ~global_env ~tvar_env ~cenv
    ~control_ctx ~diagnostics =
  (let check_continue_args continue_args label =
     let expected = List.length continue_args in
     let actual = List.length args in
     if expected <> actual then
       add_error diagnostics
         (Errors.continue_arity_mismatch ~expected ~actual ~loc);
     let rec check_args expected_tys args =
       match (expected_tys, args) with
       | [], args ->
           Lst.map args (fun arg ->
               infer_expr env arg ~global_env ~tvar_env ~cenv ~control_ctx
                 ~diagnostics)
       | _, [] -> []
       | expected_ty :: expected_tys, arg :: args ->
           check_expr env arg (Expect_type expected_ty) ~global_env ~tvar_env
             ~cenv ~control_ctx ~diagnostics
           :: check_args expected_tys args
     in
     let args = check_args continue_args args in
     Typedtree.Texpr_continue { args; ty = expect_ty; loc_ = loc; label }
       [@@inline]
   in
   let fallback label =
     let args =
       Lst.map args (fun arg ->
           infer_expr env arg ~global_env ~tvar_env ~cenv ~control_ctx
             ~diagnostics)
     in
     Typedtree.Texpr_continue { args; ty = expect_ty; label; loc_ = loc }
       [@@inline]
   in
   let check_with_control_info (control_info : Control_ctx.control_info) label =
     match control_info with
     | Not_in_loop ->
         add_error diagnostics (Errors.outside_loop ~msg:"continue" ~loc);
         fallback label
     | Ambiguous_position ->
         add_error diagnostics (Errors.ambiguous_continue loc);
         fallback label
     | In_while | In_while_with_else _ | In_foreach _ ->
         if args <> [] then
           add_error diagnostics
             (Errors.continue_arity_mismatch ~expected:0
                ~actual:(List.length args) ~loc);
         let args =
           Lst.map args (fun arg ->
               infer_expr env arg ~global_env ~tvar_env ~cenv ~control_ctx
                 ~diagnostics)
         in
         Texpr_continue { args; ty = expect_ty; loc_ = loc; label }
     | In_for { continue = continue_args; break = _ } ->
         if args = [] then
           Texpr_continue { args = []; ty = expect_ty; loc_ = loc; label }
         else check_continue_args continue_args label
     | In_loop { continue = continue_args; has_continue; _ } ->
         has_continue := true;
         check_continue_args continue_args label
       [@@inline]
   in
   match label with
   | None -> check_with_control_info control_ctx.control_info None
   | Some label -> (
       match Lst.assoc_str control_ctx.labeled_controls label.label_name with
       | Some control_info ->
           let label : Typedtree.loop_label =
             { label = control_info.label; loc_ = label.loc_ }
           in
           control_info.used := true;
           check_with_control_info control_info.control_info (Some label)
       | None ->
           let error =
             Errors.use_undeclared_loop_label ~loc:label.loc_ label.label_name
           in
           add_error diagnostics error;
           let label : Typedtree.loop_label =
             { label = Label.fresh label.label_name; loc_ = label.loc_ }
           in
           fallback (Some label))
    : Typedtree.expr)

let typing_impl_expr env expr ~(global_env : Global_env.t) ~diagnostics =
  let cenv = Poly_type.make () in
  let tvar_env = Tvar_env.empty in
  let te =
    check_expr env expr Ignored
      ~control_ctx:
        (Control_ctx.make_fn ~return:Stype.unit ~error_ctx:None ~is_async:false)
      ~tvar_env ~cenv ~global_env ~diagnostics
  in
  Type_lint.type_lint te ~diagnostics;
  Type_constraint.solve_constraints cenv ~tvar_env ~global_env ~diagnostics;
  te

let typing_impl_let (binder : Syntax.binder) (expr : Syntax.expr)
    ~(id : Ident.t) ~(typ : Stype.t) ~(global_env : Global_env.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let cenv = Poly_type.make () in
   let tvar_env = Tvar_env.empty in
   let te =
     check_expr Local_env.empty expr (Expect_type typ)
       ~control_ctx:Control_ctx.empty ~tvar_env ~cenv ~global_env ~diagnostics
   in
   Type_lint.type_lint te ~diagnostics;
   Type_constraint.solve_constraints cenv ~tvar_env ~global_env ~diagnostics;
   let binder : Typedtree.binder = { binder_id = id; loc_ = binder.loc_ } in
   (binder, te)
    : Typedtree.binder * Typedtree.expr)

let derive_by_ast ~global_env (directive : Syntax.deriving_directive)
    (decl : Syntax.type_decl) (method_name : string) ~trait_path
    ~(type_path : Type_path.t) ~(deriver : Ast_derive.deriver)
    ~(diagnostics : Local_diagnostics.t) =
  (let trait = directive.type_name_ in
   let tpath = type_path in
   let meth_info =
     Option.get
       (Ext_method_env.find_method
          (Global_env.get_ext_method_env global_env)
          ~trait:trait_path ~self_type:tpath ~method_name)
   in
   match[@warning "-fragile-match"] meth_info.typ with
   | (Tarrow { params_ty; ret_ty; err_ty; is_async } : Stype.t) ->
       let param_names, params =
         Lst.map_split params_ty (fun _ ->
             let param_name =
               (("*x_" ^ Int.to_string (Basic_uuid.next ())
                 : Stdlib.String.t)
                 [@merlin.hide])
             in
             let param : Syntax.parameter =
               Positional
                 {
                   binder =
                     { binder_name = param_name; loc_ = Rloc.no_location };
                   ty = None;
                 }
             in
             (param_name, param))
       in
       let assertions = Vec.empty () in
       let body =
         deriver directive decl ~params:param_names ~assertions ~diagnostics
       in
       Wl_top_funcdef
         {
           fun_binder = { binder_name = method_name; loc_ = trait.loc_ };
           decl_params = params;
           params_loc = Rloc.no_location;
           is_pub = meth_info.pub;
           doc = meth_info.doc_;
           attrs = [];
           decl_body =
             Decl_body
               {
                 expr =
                   Pexpr_static_assert
                     { asserts = Vec.to_list assertions; body };
                 local_types = [];
               };
           loc_ = decl.loc_;
           id = meth_info.id;
           kind = Fun_kind_method None;
           arity = meth_info.arity_;
           tvar_env = meth_info.ty_params_;
           typed_fn_annotation =
             {
               params_ty = Lst.map params_ty (fun ty -> (ty, None));
               ret_ty;
               err_ty;
               is_async;
               ret_annotation = No_annotation;
             };
           constraint_names = [];
         }
   | _ -> assert false
    : Local_typing_worklist.value)

let rec typing_worklist_item (item : Local_typing_worklist.value) ~global_env
    ~diagnostics ~(impls_acc : Typedtree.impls)
    ~(local_types_acc : Typedtree.type_decl list)
    ~(local_type_env : Local_type.env option) =
  (match item with
   | Wl_top_expr { expr; is_main; id; loc_; local_types } ->
       let local_diagnostics = Local_diagnostics.make ~base:loc_ in
       if local_type_env = None then
         Global_env.add_new_local_type_env global_env id loc_;
       let local_types_tast, derive_tasks =
         Typeutil.typing_local_type local_types id ~global_env
           ~tvar_env:Tvar_env.empty ~base:loc_ ~diagnostics:local_diagnostics
       in
       let derived_impls =
         Lst.concat_map derive_tasks
           (fun { decl; directive; trait_path; type_path } ->
             let trait_name = directive.type_name_ in
             match Type_path.Hash.find_opt Ast_derive.derivers trait_path with
             | Some derivers ->
                 Lst.concat_map derivers (fun (method_name, deriver) ->
                     let ast =
                       derive_by_ast ~type_path ~trait_path ~global_env
                         directive decl method_name ~deriver
                         ~diagnostics:local_diagnostics
                     in
                     fst
                       (typing_worklist_item ast ~global_env ~diagnostics
                          ~impls_acc:[] ~local_types_acc:[]
                          ~local_type_env:
                            (Global_env.get_cur_local_type_env global_env)))
             | None ->
                 Local_diagnostics.add_error local_diagnostics
                   (Errors.derive_unsupported_trait ~tycon:type_path
                      ~trait:trait_name.name ~loc:trait_name.loc_);
                 [])
       in
       let te =
         typing_impl_expr Local_env.empty expr ~global_env
           ~diagnostics:local_diagnostics
       in
       if local_type_env = None then
         Global_env.clear_cur_local_type_env global_env;
       Local_diagnostics.add_to_global local_diagnostics diagnostics;
       ( Typedtree.Timpl_expr
           { expr = te; is_main; expr_id = id; loc_; is_generated_ = false }
         :: derived_impls
         @ impls_acc,
         local_types_tast @ local_types_acc )
   | Wl_top_letdef
       { binder; expr; loc_; is_pub; doc_; konstraint; id; typ; attrs } ->
       let local_diagnostics = Local_diagnostics.make ~base:loc_ in
       let binder, te =
         typing_impl_let binder expr ~id:(Ident.of_qual_ident id) ~typ
           ~global_env ~diagnostics:local_diagnostics
       in
       Local_diagnostics.add_to_global local_diagnostics diagnostics;
       ( Typedtree.Timpl_letdef
           {
             binder;
             expr = te;
             is_pub;
             loc_;
             konstraint;
             doc_;
             attrs;
             is_generated_ = false;
           }
         :: impls_acc,
         local_types_acc )
   | Wl_top_funcdef
       {
         fun_binder;
         decl_params;
         params_loc;
         is_pub;
         doc;
         attrs;
         decl_body;
         loc_;
         id;
         kind;
         arity;
         tvar_env;
         constraint_names;
         typed_fn_annotation;
       } -> (
       let local_diagnostics = Local_diagnostics.make ~base:loc_ in
       let binder : Typedtree.binder =
         { binder_id = Ident.of_qual_ident id; loc_ = fun_binder.loc_ }
       in
       match decl_body with
       | Decl_stubs func_stubs ->
           let is_import_stub, language =
             match func_stubs with
             | Import _ -> (true, None)
             | Embedded { language; _ } -> (false, language)
           in
           let ({
                  params_ty;
                  ret_ty = _;
                  err_ty = _;
                  is_async = _;
                  ret_annotation;
                }
                 : Local_typing_worklist.typed_fn_annotation) =
             typed_fn_annotation
           in
           let stub_body : Typedtree.stub_body =
             if Syntax_util.is_intrinsic func_stubs then Intrinsic
             else
               let language =
                 Option.map (fun s -> s.Literal.string_val) language
               in
               let check_type ~allow_func ty =
                 store_error ~diagnostics:local_diagnostics
                   (Typeutil.check_stub_type ~language ty global_env ~allow_func
                      ~is_import_stub)
               in
               Lst.iter params_ty ~f:(fun (_, ty_opt) ->
                   match ty_opt with
                   | Some ty -> check_type ~allow_func:true ty
                   | None -> ());
               (match ret_annotation with
               | Annotated (ret_annot, _) ->
                   check_type ~allow_func:false ret_annot
               | No_annotation | Has_super_error _ -> ());
               let func_stubs =
                 Parsing_partial_info.take_info ~diagnostics
                   (Stub_type.from_syntax func_stubs ~loc:loc_ ~diagnostics)
               in
               Func_stub func_stubs
           in
           let cenv = Poly_type.make () in
           let _, params =
             check_function_params Local_env.empty decl_params params_ty
               ~is_global:true ~tvar_env ~cenv ~global_env
               ~diagnostics:local_diagnostics
           in
           Lst.iter params ~f:(fun p ->
               match p with
               | Discard_positional_param _ -> ()
               | Param { kind; _ } -> (
                   match kind with
                   | Optional default ->
                       Type_lint.type_lint default
                         ~diagnostics:local_diagnostics
                   | Positional | Labelled | Autofill | Question_optional -> ()));
           Type_constraint.solve_constraints cenv ~tvar_env ~global_env
             ~diagnostics:local_diagnostics;
           let ret =
             match ret_annotation with
             | Annotated (ret, _) -> Some ret
             | Has_super_error _ | No_annotation -> None
           in
           Local_diagnostics.add_to_global local_diagnostics diagnostics;
           ( Timpl_stub_decl
               {
                 binder;
                 params;
                 ret;
                 func_stubs = stub_body;
                 is_pub;
                 arity_ = arity;
                 kind_ = kind;
                 loc_;
                 doc_ = doc;
               }
             :: impls_acc,
             local_types_acc )
       | Decl_body { expr = decl_body; local_types } ->
           let cenv = Poly_type.make () in
           let is_in_test =
             String.starts_with ~prefix:"__test_" fun_binder.binder_name
           in
           let is_impl_method =
             match id with
             | Qext_method _ -> true
             | Qregular _ | Qmethod _ | Qregular_implicit_pkg _ -> false
           in
           if local_type_env = None then
             Global_env.add_new_local_type_env global_env id loc_;
           let local_types_tast, derive_tasks =
             Typeutil.typing_local_type local_types id ~global_env ~tvar_env
               ~base:loc_ ~diagnostics:local_diagnostics
           in
           let derived_impls =
             Lst.concat_map derive_tasks
               (fun { decl; directive; trait_path; type_path } ->
                 let trait_name = directive.type_name_ in
                 match
                   Type_path.Hash.find_opt Ast_derive.derivers trait_path
                 with
                 | Some derivers ->
                     Lst.concat_map derivers (fun (method_name, deriver) ->
                         let ast =
                           derive_by_ast ~type_path ~trait_path ~global_env
                             directive decl method_name ~deriver
                             ~diagnostics:local_diagnostics
                         in
                         fst
                           (typing_worklist_item ast ~global_env ~diagnostics
                              ~impls_acc:[] ~local_types_acc:[]
                              ~local_type_env:
                                (Global_env.get_cur_local_type_env global_env)))
                 | None ->
                     Local_diagnostics.add_error local_diagnostics
                       (Errors.derive_unsupported_trait ~tycon:type_path
                          ~trait:trait_name.name ~loc:trait_name.loc_);
                     [])
           in
           let tfuns, _ =
             check_function ~is_impl_method Local_env.empty decl_params
               params_loc decl_body typed_fn_annotation ~kind_:Lambda
               ~is_global:true ~is_in_test ~tvar_env ~cenv ~global_env
               ~diagnostics:local_diagnostics
           in
           if local_type_env = None then
             Global_env.clear_cur_local_type_env global_env;
           Type_lint.type_lint_fn tfuns ~diagnostics:local_diagnostics;
           Type_constraint.solve_constraints cenv ~tvar_env ~global_env
             ~diagnostics:local_diagnostics;
           Local_diagnostics.add_to_global local_diagnostics diagnostics;
           ( Timpl_fun_decl
               {
                 fun_decl =
                   {
                     kind;
                     fn_binder = binder;
                     fn = tfuns;
                     is_pub;
                     ty_params_ = tvar_env;
                     doc_ = doc;
                     attrs;
                     constraint_names;
                   };
                 arity_ = arity;
                 loc_;
                 is_generated_ = false;
               }
             :: (derived_impls @ impls_acc),
             local_types_tast @ local_types_acc ))
   | Wl_derive { ty_decl; syn_decl; trait_path; directive } -> (
       let local_diagnostics = Local_diagnostics.make ~base:syn_decl.loc_ in
       match Type_path.Hash.find_opt Ast_derive.derivers trait_path with
       | Some derivers ->
           let type_path =
             Type_path.toplevel_type
               ~pkg:!Basic_config.current_package
               syn_decl.tycon
           in
           let derived_impls =
             Lst.fold_right derivers impls_acc (fun (method_name, deriver) ->
                 fun impls_acc ->
                  fst
                    (typing_worklist_item ~impls_acc ~local_types_acc:[]
                       ~global_env ~diagnostics ~local_type_env:None
                       (derive_by_ast ~type_path ~global_env directive syn_decl
                          method_name ~trait_path ~deriver
                          ~diagnostics:local_diagnostics)))
           in
           Local_diagnostics.add_to_global local_diagnostics diagnostics;
           (derived_impls, local_types_acc)
       | None ->
           Local_diagnostics.add_error local_diagnostics
             (Errors.derive_unsupported_trait ~tycon:ty_decl.ty_constr
                ~trait:directive.type_name_.name ~loc:directive.loc_);
           Local_diagnostics.add_to_global local_diagnostics diagnostics;
           (impls_acc, local_types_acc))
    : Typedtree.impls * Typedtree.type_decl list)

let type_check ~diagnostics (input : Local_typing_worklist.t) =
  (let global_env = input.global_env in
   let value_defs, local_types =
     Vec.fold_right input.values ([], []) ~f:(fun item ->
         fun (impls_acc, local_types_acc) ->
          typing_worklist_item ~global_env ~diagnostics item ~impls_acc
            ~local_types_acc ~local_type_env:None)
   in
   Typedtree.Output
     {
       value_defs = input.const_decls @ value_defs;
       type_defs = local_types @ input.type_decls;
       trait_defs = input.trait_decls;
       trait_alias = input.trait_alias;
     }
    : Typedtree.output)
