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


module Qual_ident = Basic_qual_ident
module Config = Basic_config
module Constr_info = Basic_constr_info
module Lst = Basic_lst
module Vec = Basic_vec
module Arr = Basic_arr
module Type_path = Basic_type_path
module Hash_string = Basic_hash_string
module Hash_int = Basic_hash_int
module Strutil = Basic_strutil
module Syntax = Parsing_syntax
module Operators = Parsing_operators

type docstring = Docstring.t

let trait_not_implemented ~trait ~type_name ~failure_reasons ~loc =
  let failure_reasons =
    Lst.map failure_reasons (fun reason ->
        match reason with
        | `Method_missing name ->
            ("method " ^ name ^ " is missing" : Stdlib.String.t) [@merlin.hide]
        | `Private_method name ->
            ("implementation of method " ^ name ^ " is private"
              : Stdlib.String.t)
              [@merlin.hide]
        | `Type_mismatch (name, expected, actual) ->
            Stdlib.String.concat ""
              [
                "method ";
                name;
                " is expected to have type ";
                expected;
                ", but has type ";
                actual;
              ] [@merlin.hide]
        | `Method_constraint_not_satisfied name ->
            ("constraints of method " ^ name ^ " are not satisfied"
              : Stdlib.String.t)
              [@merlin.hide]
        | `Method_type_params_unsolved name ->
            ("cannot decide type parameters of method " ^ name
              : Stdlib.String.t)
              [@merlin.hide])
  in
  Errors.trait_not_implemented ~trait ~type_name ~failure_reasons ~loc

let add_error = Local_diagnostics.add_error
let add_global_error = Diagnostics.add_error

let take_info_partial partial ~diagnostics =
  Local_diagnostics.take_partial_info partial ~diagnostics
[@@inline]

let rec check_duplicate (xs : 'a list) (f : 'a -> 'a -> bool) =
  match xs with
  | [] -> None
  | x :: rest ->
      if Lst.exists rest (f x) then Some x else check_duplicate rest f

let duplicate_import_help_message kind =
  ("This " ^ kind
   ^ " is imported implicitly because \"test-import-all\" is set to true \
      implicitly. If you want to avoid this, set \"test-import-all\" to false \
      in moon.pkg.json."
    : Stdlib.String.t)

let get_duplicate_loc_info (v : Value_info.toplevel) =
  match v.direct_use_loc_ with
  | Not_direct_use -> (v.loc_, None)
  | Explicit_import loc_ -> (loc_, None)
  | Implicit_import_all loc_ ->
      (loc_, Some (duplicate_import_help_message "value"))

let typing_type ~(allow_private : bool)
    ?(placeholder_env : Placeholder_env.t option) (te : Syntax.typ)
    ~(tvar_env : Tvar_env.t) ~(types : Global_env.All_types.t) ~diagnostics =
  (Typeutil.typing_type ~allow_private ~allow_partial:false ?placeholder_env
     ~tvar_env ~types ~local_type_env:None te ~diagnostics
    : Typedtree.typ)

let typing_error_type ~(allow_private : bool)
    ?(placeholder_env : Placeholder_env.t option) (err_ty : Syntax.error_typ)
    ~(tvar_env : Tvar_env.t) ~(types : Global_env.All_types.t) ~has_error
    ~header_loc ~diagnostics =
  (match err_ty with
   | Error_typ { ty } ->
       let typ =
         typing_type ~allow_private ?placeholder_env ~tvar_env ~types
           ~diagnostics ty
       in
       let stype = Typedtree_util.stype_of_typ typ in
       if not (Type.is_error_type ~tvar_env stype) then
         add_error diagnostics
           (Errors.not_error_subtype
              (Printer.type_to_string stype)
              (Typedtree.loc_of_typ typ));
       (Some stype, Error_typ { ty = typ })
   | Default_error_typ { loc_ } ->
       (Some Typeutil.default_err_type, Default_error_typ { loc_ })
   | No_error_typ when has_error ->
       (Some Typeutil.default_err_type, Default_error_typ { loc_ = header_loc })
   | No_error_typ -> (None, No_error_typ)
    : Stype.t option * Typedtree.error_typ)

let typing_trait_name ~types ~allow_private trait ~diagnostics =
  Typeutil.typing_trait_name ~types ~allow_private trait ~diagnostics

let collect_types_and_traits (impls : Syntax.impls) ~foreign_types
    ~(diagnostics : Diagnostics.t) =
  (let type_defs : Syntax.type_decl Hash_string.t = Hash_string.create 17 in
   let trait_defs : Syntax.trait_decl Hash_string.t = Hash_string.create 17 in
   let trait_alias : Syntax.impl Hash_string.t = Hash_string.create 17 in
   let find_declared_type name =
     match Hash_string.find_opt type_defs name with
     | Some { loc_; _ } -> Some (loc_, None)
     | None -> (
         match
           Global_env.All_types.find_type_alias foreign_types
             (Basic_longident.Lident name)
         with
         | Some alias ->
             Some (alias.loc_, Some (duplicate_import_help_message "type"))
         | None -> None)
   in
   let go (impl : Syntax.impl) =
     match impl with
     | Ptop_expr _ | Ptop_test _ | Ptop_letdef _ | Ptop_funcdef _ | Ptop_impl _
     | Ptop_impl_relation _ ->
         ()
     | Ptop_typedef decl -> (
         let base = decl.loc_ in
         let tycon_loc_ = Rloc.to_loc ~base decl.tycon_loc_ in
         let name = decl.tycon in
         match find_declared_type name with
         | Some (first_loc, extra_message) ->
             add_global_error diagnostics
               (Errors.type_redeclare ~name ~first_loc ~second_loc:tycon_loc_
                  ~extra_message);
             if extra_message <> None then Hash_string.add type_defs name decl
         | None -> (
             Hash_string.add type_defs name decl;
             match Hash_string.find_opt trait_defs name with
             | Some i ->
                 add_global_error diagnostics
                   (Errors.type_trait_duplicate ~name ~first_kind:"trait"
                      ~first_loc:i.trait_loc_ ~second_kind:"type"
                      ~second_loc:tycon_loc_ ~extra_message:None)
             | None -> ()))
     | Ptop_trait trait_decl -> (
         let name = trait_decl.trait_name.binder_name in
         let trait_loc_ = trait_decl.trait_loc_ in
         match Hash_string.find_opt trait_alias name with
         | Some first_def -> (
             match[@warning "-fragile-match"] first_def with
             | Ptop_trait_alias first_def ->
                 add_global_error diagnostics
                   (Errors.trait_redeclare ~name ~first_loc:first_def.loc_
                      ~second_loc:trait_loc_)
             | _ -> assert false)
         | None -> (
             match Hash_string.find_opt trait_defs name with
             | Some i ->
                 add_global_error diagnostics
                   (Errors.trait_redeclare ~name ~first_loc:i.trait_loc_
                      ~second_loc:trait_loc_)
             | None -> (
                 Hash_string.add trait_defs name trait_decl;
                 match find_declared_type name with
                 | Some (loc, extra_message) ->
                     add_global_error diagnostics
                       (Errors.type_trait_duplicate ~name ~first_kind:"type"
                          ~first_loc:loc ~second_kind:"trait"
                          ~second_loc:trait_loc_ ~extra_message)
                 | None -> ())))
     | Ptop_trait_alias { binder; loc_; _ } -> (
         let name = binder.binder_name in
         match Hash_string.find_opt trait_alias name with
         | Some first_def -> (
             match[@warning "-fragile-match"] first_def with
             | Ptop_trait_alias first_def ->
                 add_global_error diagnostics
                   (Errors.trait_redeclare ~name ~first_loc:first_def.loc_
                      ~second_loc:loc_)
             | _ -> assert false)
         | None -> (
             match Hash_string.find_opt trait_defs name with
             | Some first_def ->
                 add_global_error diagnostics
                   (Errors.trait_redeclare ~name ~first_loc:first_def.trait_loc_
                      ~second_loc:loc_)
             | None -> (
                 Hash_string.add trait_alias name impl;
                 match find_declared_type name with
                 | Some (loc, extra_message) ->
                     add_global_error diagnostics
                       (Errors.type_trait_duplicate ~name ~first_kind:"type"
                          ~first_loc:loc ~second_kind:"trait" ~second_loc:loc_
                          ~extra_message)
                 | None -> ())))
   in
   Lst.iter impls ~f:go;
   Placeholder_env.make ~type_defs ~trait_defs ~trait_alias
    : Placeholder_env.t)

let check_operator_decl ~(diagnostics : Local_diagnostics.t) ~(loc : Rloc.t)
    method_name ty =
  match Operators.find_by_method_opt method_name with
  | Some op -> (
      let expected_arity =
        match op.kind with
        | Prefix -> 1
        | Infix _ -> 2
        | Mixfix { arity; _ } -> arity
      in
      let actual_arity = Stype.arity_of_typ ty in
      if expected_arity <> actual_arity then
        add_error diagnostics
          (Errors.bad_operator_arity ~method_name ~expected:expected_arity
             ~actual:actual_arity ~loc)
      else
        match op.kind with
        | Infix { should_have_equal_param_type = true } -> (
            match ty with
            | Tarrow { params_ty = [ param_typ1; param_typ2 ]; _ } ->
                if not (Type.same_type param_typ1 param_typ2) then
                  add_error diagnostics
                    (Errors.bad_operator_type ~method_name
                       ~first:(Printer.type_to_string param_typ1)
                       ~second:(Printer.type_to_string param_typ2)
                       ~loc)
            | _ -> assert false)
        | _ -> ())
  | None -> ()

let typing_trait_method_decl ~is_pub (method_decl : Syntax.trait_method_decl)
    ~(placeholder_env : Placeholder_env.t) ~(types : Global_env.All_types.t)
    ~(base : Loc.t) ~diagnostics =
  (let (Trait_method
          { name; has_error; quantifiers = _; params; return_type; loc_ }) =
     method_decl
   in
   let meth_loc = Rloc.to_loc ~base loc_ in
   let tvar_env = Tvar_env.tvar_env_self in
   let params_ty, tast_params =
     Lst.map_split params (fun p ->
         let typ =
           typing_type ~allow_private:(not is_pub) ~placeholder_env ~types
             ~tvar_env p.tmparam_typ ~diagnostics
         in
         (Typedtree_util.stype_of_typ typ, (p.tmparam_label, typ)))
   in
   let ret_ty, ret_typ, err_ty, err_typ =
     match return_type with
     | Some (return_type, err_type) ->
         let typ =
           typing_type ~allow_private:(not is_pub) ~placeholder_env ~types
             ~tvar_env return_type ~diagnostics
         in
         let error_stype, error_typ =
           typing_error_type ~allow_private:(not is_pub) ~placeholder_env ~types
             ~tvar_env ~diagnostics ~has_error ~header_loc:name.loc_ err_type
         in
         let error_typ =
           match error_typ with
           | Error_typ { ty } -> Some ty
           | Default_error_typ _ -> Some Typeutil.default_err_typedtree_type
           | No_error_typ -> None
         in
         (Typedtree_util.stype_of_typ typ, Some typ, error_stype, error_typ)
     | None ->
         add_error diagnostics (Errors.missing_return_annot name.loc_);
         (Stype.blackhole, None, None, None)
   in
   let method_arity =
     take_info_partial ~diagnostics
       (Fn_arity.from_trait_method_params params ~base)
   in
   let method_name = name.binder_name in
   let method_typ : Stype.t =
     Tarrow { params_ty; ret_ty; err_ty; generic_ = true; is_async = false }
   in
   check_operator_decl ~diagnostics ~loc:name.loc_ method_name method_typ;
   let method_decl : Trait_decl.method_decl =
     {
       method_name = name.binder_name;
       method_typ;
       method_arity;
       method_loc_ = meth_loc;
     }
   in
   let tast_decl : Typedtree.method_decl =
     {
       method_name = name;
       method_params = tast_params;
       method_ret = ret_typ;
       method_err = err_typ;
       method_loc_ = loc_;
     }
   in
   (method_decl, tast_decl)
    : Trait_decl.method_decl * Typedtree.method_decl)

type toplevel_env = Typing_info.values

let transl_type_vis ~diagnostics (vis : Syntax.visibility) =
  (match vis with
   | Vis_priv _ -> Vis_priv
   | Vis_default -> Vis_default
   | Vis_pub { attr = None } -> Vis_readonly
   | Vis_pub { attr = Some "all" } -> Vis_fully_pub
   | Vis_pub { attr = Some "" } -> Vis_fully_pub
   | Vis_pub { attr = Some attr; loc_ } ->
       add_error diagnostics
         (Errors.unsupported_modifier
            ~modifier:(("pub(" ^ attr ^ ")" : Stdlib.String.t) [@merlin.hide])
            ~loc:loc_);
       Vis_fully_pub
    : Typedecl_info.visibility)

let transl_trait_vis ~diagnostics (vis : Syntax.visibility) =
  (match vis with
   | Vis_priv _ -> Vis_priv
   | Vis_default -> Vis_default
   | Vis_pub { attr = None } -> Vis_readonly
   | Vis_pub { attr = Some "" } -> Vis_fully_pub
   | Vis_pub { attr = Some "open" } -> Vis_fully_pub
   | Vis_pub { attr = Some attr; loc_ } ->
       add_error diagnostics
         (Errors.unsupported_modifier
            ~modifier:(("pub(" ^ attr ^ ")" : Stdlib.String.t) [@merlin.hide])
            ~loc:loc_);
       Vis_fully_pub
    : Typedecl_info.visibility)

let transl_value_visibility ~local_diagnostics ~entity (vis : Syntax.visibility)
    =
  match vis with
  | Vis_default -> false
  | Vis_pub { attr = None } -> true
  | Vis_priv { loc_ } | Vis_pub { attr = Some _; loc_ } ->
      add_error local_diagnostics
        (Errors.invalid_visibility ~entity ~vis:(Syntax.string_of_vis vis)
           ~loc:loc_);
      false

let transl_component_vis = function
  | Typedecl_info.Vis_default | Vis_priv -> Typedecl_info.Invisible
  | Vis_readonly -> Readable
  | Vis_fully_pub -> Read_write

let submit_variant (ty_res : Stype.t) (ty_vis : Typedecl_info.visibility)
    (cs : Syntax.constr_decl list) (tvar_env : Tvar_env.t)
    ~(placeholder_env : Placeholder_env.t) ~(types : Global_env.All_types.t)
    (toplevel : toplevel_env) ~(is_exn : bool) ~(is_only_tag_enum : bool)
    ~(type_name : string) ~(base : Loc.t) ~diagnostics =
  (let total =
     let n = List.length cs in
     if n = 0 then Constr_info.Index_set.empty
     else Constr_info.Index_set.singleton 0 (n - 1)
   in
   let allow_private =
     match ty_vis with
     | Vis_default | Vis_priv -> true
     | Vis_readonly | Vis_fully_pub -> false
   in
   let curr_repr = ref 0 in
   let repr_dedup_map = Hash_int.create 7 in
   let cds, tast_cds =
     List.split
       (Lst.mapi cs (fun i ->
            fun constr ->
             let ({
                    constr_name;
                    constr_args = args;
                    constr_tag;
                    constr_loc_ = cs_loc_;
                  }
                   : Syntax.constr_decl) =
               constr
             in
             let name = constr_name.name in
             let args, cs_arity_ =
               match args with
               | None -> ([], Fn_arity.simple 0)
               | Some args ->
                   ( args,
                     take_info_partial ~diagnostics
                       (Fn_arity.from_constr_params ~base args) )
             in
             let cs_args, tast_args =
               Lst.map_split args (fun arg ->
                   let typ =
                     typing_type ~allow_private ~tvar_env ~placeholder_env
                       ~types ~diagnostics arg.cparam_typ
                   in
                   let tast_arg : Typedtree.constr_decl_arg =
                     {
                       carg_typ = typ;
                       carg_mut = arg.cparam_mut;
                       carg_label = arg.cparam_label;
                     }
                   in
                   (Typedtree_util.stype_of_typ typ, tast_arg))
             in
             let cs_vis = transl_component_vis ty_vis in
             let repr_ : Constr_info.constr_repr =
               if is_only_tag_enum then
                 match constr_tag with
                 | None ->
                     let value = !curr_repr in
                     curr_repr := value + 1;
                     (match Hash_int.find_opt repr_dedup_map value with
                     | None ->
                         Hash_int.add repr_dedup_map value constr_name.name
                     | Some first ->
                         Local_diagnostics.add_error diagnostics
                           (Errors.enum_tag_duplicate ~first
                              ~tag:(string_of_int value) ~loc:constr_name.loc_));
                     Integer value
                 | Some (tag_repr, tag_loc) -> (
                     match Int32.of_string_opt tag_repr with
                     | None ->
                         Local_diagnostics.add_error diagnostics
                           (Errors.overflow ~value:tag_repr ~loc:tag_loc);
                         Constant
                     | Some value ->
                         let value = Int32.to_int value in
                         curr_repr := value + 1;
                         (match Hash_int.find_opt repr_dedup_map value with
                         | None ->
                             Hash_int.add repr_dedup_map value constr_name.name
                         | Some first ->
                             Local_diagnostics.add_error diagnostics
                               (Errors.enum_tag_duplicate ~first ~tag:tag_repr
                                  ~loc:tag_loc));
                         Integer value)
               else (
                 (match constr_tag with
                 | Some (_, tag_loc) ->
                     Local_diagnostics.add_error diagnostics
                       (Errors.non_constant_enum_no_custom_tag tag_loc)
                 | _ -> ());
                 match cs_args with [] -> Constant | _ -> Non_constant)
             in
             let cs_tag : Constr_info.constr_tag =
               if is_exn then
                 Extensible_tag
                   {
                     pkg = !Config.current_package;
                     type_name;
                     name;
                     total;
                     index = i;
                   }
               else Constr_tag_regular { total; index = i; name_ = name; repr_ }
             in
             let constr : Typedecl_info.constructor =
               let cs_constr_loc_ = Rloc.to_loc ~base constr_name.loc_ in
               let cs_loc_ = Rloc.to_loc ~base cs_loc_ in
               {
                 constr_name = name;
                 cs_res = ty_res;
                 cs_args;
                 cs_tag;
                 cs_vis;
                 cs_ty_params_ = tvar_env;
                 cs_arity_;
                 cs_constr_loc_;
                 cs_loc_;
               }
             in
             let constr_decl : Typedtree.constr_decl =
               {
                 constr_name = { label_name = name; loc_ = constr_name.loc_ };
                 constr_tag = cs_tag;
                 constr_args = tast_args;
                 constr_arity_ = cs_arity_;
                 constr_loc_ = cs_loc_;
               }
             in
             Typing_info.add_constructor toplevel constr;
             (constr, constr_decl)))
   in
   if is_exn then (ErrorEnum_type cds, Td_error (Enum_payload tast_cds))
   else (Variant_type cds, Td_variant tast_cds)
    : Typedecl_info.type_components * Typedtree.type_desc)

let submit_field (ty_record : Stype.t) (ty_vis : Typedecl_info.visibility)
    (fs : Syntax.field_decl list) (tvar_env : Tvar_env.t)
    ~(placeholder_env : Placeholder_env.t) ~(types : Global_env.All_types.t)
    (toplevel : toplevel_env) ~diagnostics ~base =
  (let all_labels = Lst.map fs (fun f -> f.field_name.label) in
   let has_private_field = ref false in
   let fields, tast_fields =
     List.split
       (Lst.mapi fs (fun i ->
            fun f ->
             let ({
                    field_name = name;
                    field_ty = ty_expr;
                    field_mut = mut;
                    field_loc_ = loc_;
                    field_vis;
                  }
                   : Syntax.field_decl) =
               f
             in
             let vis : Typedecl_info.type_component_visibility =
               match field_vis with
               | Vis_priv _ ->
                   has_private_field := true;
                   Invisible
               | Vis_pub _ | Vis_default -> transl_component_vis ty_vis
             in
             let allow_private =
               match vis with
               | Read_write | Readable -> false
               | Invisible -> true
             in
             let field_typ =
               typing_type ty_expr ~allow_private ~placeholder_env ~tvar_env
                 ~types ~diagnostics
             in
             let field : Typedecl_info.field =
               {
                 field_name = name.label;
                 all_labels;
                 pos = i;
                 ty_field = Typedtree_util.stype_of_typ field_typ;
                 mut;
                 vis;
                 ty_record;
                 ty_params_ = tvar_env;
                 label_loc_ = Rloc.to_loc ~base name.loc_;
                 loc_ = Rloc.to_loc ~base loc_;
               }
             in
             let field_decl : Typedtree.field_decl =
               {
                 field_label = { label_name = name.label; loc_ = name.loc_ };
                 field_typ;
                 field_mut = mut;
                 field_vis = vis;
                 field_loc_ = loc_;
               }
             in
             Typing_info.add_field toplevel ~field;
             (field, field_decl)))
   in
   let has_private_field_ = !has_private_field in
   (Record_type { fields; has_private_field_ }, Td_record tast_fields)
    : Typedecl_info.type_components * Typedtree.type_desc)

let typing_typedef (decl : Syntax.type_decl)
    ~(placeholder_env : Placeholder_env.t) (types : Global_env.All_types.t)
    (toplevel : toplevel_env) ~ext_method_env ~trait_impls
    ~(value_worklist : Local_typing_worklist.value Vec.t)
    ~(diagnostics : Local_diagnostics.t) =
  (let name = decl.tycon in
   let tvar_env = Typeutil.typing_type_decl_binders decl.params in
   let base = decl.loc_ in
   let ty_vis = transl_type_vis ~diagnostics decl.type_vis in
   let tycon_loc_ = Rloc.to_loc ~base decl.tycon_loc_ in
   let type_constructor =
     Type_path.toplevel_type ~pkg:!Basic_config.current_package decl.tycon
   in
   let ty_args = Tvar_env.get_types tvar_env in
   let only_tag_enum_ =
     match decl.components with
     | Ptd_variant cs ->
         Lst.for_all cs (fun { constr_args; _ } -> constr_args = None)
     | _ -> false
   in
   let is_suberror_ =
     match decl.components with Ptd_error _ -> true | _ -> false
   in
   let ty_res : Stype.t =
     T_constr
       {
         type_constructor;
         tys = ty_args;
         generic_ = ty_args <> [];
         is_suberror_;
       }
   in
   let ty_comp, ty_desc =
     match decl.components with
     | Ptd_variant cs ->
         (match
            check_duplicate cs (fun c1 ->
                fun c2 -> c1.constr_name.name = c2.constr_name.name)
          with
         | Some { constr_name = { name; loc_ }; _ } ->
             add_error diagnostics
               (Errors.constructor_duplicate ~name ~loc:loc_)
         | None -> ());
         submit_variant ty_res ty_vis cs tvar_env ~placeholder_env ~types
           toplevel ~is_exn:false ~is_only_tag_enum:only_tag_enum_
           ~type_name:name ~diagnostics ~base
     | Ptd_error ty -> (
         let allow_private =
           match ty_vis with
           | Vis_priv | Vis_default -> true
           | Vis_readonly | Vis_fully_pub -> false
         in
         match ty with
         | No_payload ->
             let constr : Typedecl_info.constructor =
               {
                 constr_name = decl.tycon;
                 cs_res = ty_res;
                 cs_args = [];
                 cs_tag =
                   Extensible_tag
                     {
                       pkg = !Config.current_package;
                       type_name = name;
                       name;
                       total = Constr_info.Index_set.singleton 0 0;
                       index = 0;
                     };
                 cs_vis = transl_component_vis ty_vis;
                 cs_ty_params_ = tvar_env;
                 cs_arity_ = Fn_arity.simple 0;
                 cs_constr_loc_ = tycon_loc_;
                 cs_loc_ = tycon_loc_;
               }
             in
             Typing_info.add_constructor toplevel constr;
             (Error_type constr, Td_error No_payload)
         | Single_payload ty ->
             let typ =
               typing_type ty ~allow_private ~placeholder_env ~tvar_env ~types
                 ~diagnostics
             in
             let constr : Typedecl_info.constructor =
               {
                 constr_name = decl.tycon;
                 cs_res = ty_res;
                 cs_args = [ Typedtree_util.stype_of_typ typ ];
                 cs_tag =
                   Extensible_tag
                     {
                       pkg = !Config.current_package;
                       type_name = name;
                       name;
                       total = Constr_info.Index_set.singleton 0 0;
                       index = 0;
                     };
                 cs_vis = transl_component_vis ty_vis;
                 cs_ty_params_ = tvar_env;
                 cs_arity_ = Fn_arity.simple 1;
                 cs_constr_loc_ = tycon_loc_;
                 cs_loc_ = tycon_loc_;
               }
             in
             Typing_info.add_constructor toplevel constr;
             (Error_type constr, Td_error (Single_payload typ))
         | Enum_payload cs ->
             (match
                check_duplicate cs (fun c1 ->
                    fun c2 -> c1.constr_name.name = c2.constr_name.name)
              with
             | Some { constr_name = { name; loc_ }; _ } ->
                 add_error diagnostics
                   (Errors.constructor_duplicate ~name ~loc:loc_)
             | None -> ());
             submit_variant ty_res ty_vis cs tvar_env ~placeholder_env ~types
               toplevel ~is_exn:true ~is_only_tag_enum:false ~type_name:name
               ~diagnostics ~base)
     | Ptd_newtype ty ->
         let allow_private =
           match ty_vis with
           | Vis_priv | Vis_default -> true
           | Vis_readonly | Vis_fully_pub -> false
         in
         let typ =
           typing_type ty ~allow_private ~placeholder_env ~tvar_env ~types
             ~diagnostics
         in
         let sty = Typedtree_util.stype_of_typ typ in
         let name = decl.tycon in
         let constr : Typedecl_info.constructor =
           {
             constr_name = name;
             cs_res = ty_res;
             cs_args = [ sty ];
             cs_tag =
               Constr_tag_regular
                 {
                   total = Constr_info.Index_set.singleton 0 0;
                   index = 0;
                   name_ = name;
                   repr_ = Non_constant;
                 };
             cs_vis = transl_component_vis ty_vis;
             cs_ty_params_ = tvar_env;
             cs_arity_ = Fn_arity.simple 1;
             cs_constr_loc_ = tycon_loc_;
             cs_loc_ = tycon_loc_;
           }
         in
         Typing_info.add_constructor toplevel constr;
         let recursive =
           Placeholder_env.newtype_in_cycle placeholder_env name
         in
         ( New_type { newtype_constr = constr; underlying_typ = sty; recursive },
           Td_newtype typ )
     | Ptd_record fs ->
         (match
            check_duplicate fs (fun f1 ->
                fun f2 -> f1.field_name.label = f2.field_name.label)
          with
         | Some { field_name = { label = name; loc_ }; _ } ->
             add_error diagnostics (Errors.field_duplicate ~name ~loc:loc_)
         | None -> ());
         submit_field ty_res ty_vis fs tvar_env ~placeholder_env ~types toplevel
           ~diagnostics ~base
     | Ptd_abstract ->
         Local_diagnostics.add_warning diagnostics
           {
             kind =
               Deprecated_syntax
                 {
                   old_usage =
                     ("`type " ^ decl.tycon ^ "`"
                       : Stdlib.String.t)
                       [@merlin.hide];
                   purpose = "declare an external type for FFI use";
                   new_usage =
                     Some
                       (("`extern type " ^ decl.tycon ^ "`"
                         : Stdlib.String.t)
                         [@merlin.hide]);
                 };
             loc = Rloc.of_loc ~base:(Loc.get_start base) base;
           };
         (Extern_type, Td_abstract)
     | Ptd_extern -> (Extern_type, Td_abstract)
     | Ptd_alias _ -> assert false
   in
   let ty_constr =
     Type_path.toplevel_type ~pkg:!Basic_config.current_package name
   in
   let typedecl_info : Typedecl_info.t =
     {
       ty_constr;
       ty_arity = List.length decl.params;
       ty_desc = ty_comp;
       ty_vis;
       ty_params_ = tvar_env;
       ty_loc_ = tycon_loc_;
       ty_doc_ = decl.doc_;
       ty_attrs =
         Checked_attributes.check ~local_diagnostics:diagnostics
           ~context:`TopTypeDecl decl.attrs;
       ty_is_only_tag_enum_ = only_tag_enum_;
       ty_is_suberror_ = is_suberror_;
     }
   in
   Global_env.All_types.add_type types name typedecl_info;
   let td_deriving_ =
     Lst.map decl.deriving_ (fun directive ->
         let trait = directive.type_name_ in
         let trait =
           { trait with name = Typeutil.resolve_derive_alias trait.name }
         in
         let trait_loc = Rloc.to_loc ~base:decl.loc_ trait.loc_ in
         let trait_decl, tast_trait_name =
           typing_trait_name trait ~types
             ~allow_private:(Typedecl_info.vis_is_pub ty_vis)
             ~diagnostics
         in
         (match trait_decl with
         | Some trait_decl ->
             Derive.generate_signatures ~types ~ext_method_env ~trait_impls
               ~diagnostics ~loc:trait_loc typedecl_info trait trait_decl;
             Vec.push value_worklist
               (Wl_derive
                  {
                    directive = { directive with type_name_ = trait };
                    ty_decl = typedecl_info;
                    syn_decl = decl;
                    trait_path = trait_decl.name;
                    loc_ = trait_loc;
                  })
         | None -> ());
         tast_trait_name)
   in
   {
     td_binder = { name = ty_constr; kind = Type; loc_ = decl.tycon_loc_ };
     td_params = tvar_env;
     td_desc = ty_desc;
     td_vis = ty_vis;
     td_loc_ = decl.loc_;
     td_doc_ = decl.doc_;
     td_deriving_;
   }
    : Typedtree.type_decl)

let typing_alias ~(placeholder_env : Placeholder_env.t)
    ~(types : Global_env.All_types.t) ~(diagnostics : Diagnostics.t) =
  (let trait_tast_nodes = Hash_string.create 17 in
   let rec go_trait ~visiting name (decl : Syntax.impl) =
     match[@warning "-fragile-match"] decl with
     | Ptop_trait_alias { binder; target; vis; doc_; loc_; _ } ->
         let base = loc_ in
         let local_diagnostics = Local_diagnostics.make ~base in
         if Hash_string.mem trait_tast_nodes name then ()
         else if Lst.mem_string visiting name then (
           add_error local_diagnostics
             (Errors.cycle_in_type_alias
                ~cycle:(List.rev (name :: visiting))
                ~kind:`Trait ~loc:binder.loc_);
           Local_diagnostics.add_to_global local_diagnostics diagnostics)
         else
           let is_pub =
             transl_value_visibility ~local_diagnostics ~entity:"trait alias"
               vis
           in
           let path_of_alias =
             Type_path.toplevel_type ~pkg:!Basic_config.current_package name
           in
           let result : Type_alias.t =
             {
               name;
               arity = 0;
               ty_params = Tvar_env.empty;
               target = Trait_alias { trait = path_of_alias };
               is_pub;
               doc_;
               loc_;
             }
           in
           Global_env.All_types.add_type_alias types name result;
           (match target.name with
           | Lident target -> (
               match
                 Placeholder_env.find_trait_alias placeholder_env target
               with
               | Some target_decl ->
                   go_trait ~visiting:(name :: visiting) target target_decl
               | None -> ())
           | Ldot _ -> ());
           let target : Typedtree.type_name =
             match
               Placeholder_env.typing_trait_name ~types placeholder_env target
             with
             | Error err ->
                 Local_diagnostics.add_error local_diagnostics err;
                 Tname_path
                   {
                     name =
                       Type_path.toplevel_type
                         ~pkg:!Basic_config.current_package
                         "";
                     kind = Trait;
                     loc_ = target.loc_;
                   }
             | Ok
                 ( (Local_trait (path, _) | Foreign_trait { name = path; _ }),
                   type_name ) ->
                 Global_env.All_types.add_type_alias types name
                   { result with target = Trait_alias { trait = path } };
                 type_name
           in
           let tast_node : Typedtree.trait_alias_decl =
             {
               trait_alias_name =
                 { name = path_of_alias; kind = Trait; loc_ = binder.loc_ };
               trait_alias_target = target;
               trait_alias_is_pub = is_pub;
               trait_alias_loc_ = loc_;
               trait_alias_doc_ = doc_;
             }
           in
           Hash_string.add trait_tast_nodes name tast_node
     | _ -> assert false
   in
   Placeholder_env.iter_trait_alias placeholder_env (go_trait ~visiting:[]);
   let type_tast_nodes = Hash_string.create 17 in
   let rec go_type ~visiting name (decl : Syntax.type_decl) =
     let base = decl.loc_ in
     let local_diagnostics = Local_diagnostics.make ~base in
     match decl.components with
     | Ptd_alias _ when Hash_string.mem type_tast_nodes name -> ()
     | Ptd_alias _ when Lst.mem_string visiting name ->
         add_error local_diagnostics
           (Errors.cycle_in_type_alias
              ~cycle:(List.rev (name :: visiting))
              ~kind:`Type ~loc:decl.tycon_loc_);
         Local_diagnostics.add_to_global local_diagnostics diagnostics
     | Ptd_alias typ ->
         let tvar_env = Typeutil.typing_type_decl_binders decl.params in
         let is_pub =
           match decl.type_vis with
           | Vis_pub _ -> true
           | Vis_default -> false
           | Vis_priv { loc_ = vis_loc } ->
               add_error local_diagnostics
                 (Errors.invalid_visibility ~vis:"priv" ~entity:"type alias"
                    ~loc:vis_loc);
               false
         in
         let result : Type_alias.t =
           {
             name;
             arity = List.length decl.params;
             ty_params = tvar_env;
             target = Type_alias T_blackhole;
             is_pub;
             doc_ = decl.doc_;
             loc_ = decl.loc_;
           }
         in
         Global_env.All_types.add_type_alias types name result;
         (match
            Placeholder_env.find_type_alias_deps placeholder_env decl.tycon
          with
         | None -> ()
         | Some deps ->
             Lst.iter deps ~f:(fun dep ->
                 match Placeholder_env.find_type_opt placeholder_env dep with
                 | None -> ()
                 | Some dep_decl ->
                     go_type ~visiting:(name :: visiting) dep dep_decl));
         let typ =
           typing_type ~allow_private:(not is_pub) ~placeholder_env ~tvar_env
             ~types ~diagnostics:local_diagnostics typ
         in
         let stype = Stype.type_repr (Typedtree_util.stype_of_typ typ) in
         (match stype with
         | Tparam _ ->
             add_error local_diagnostics
               (Errors.invalid_type_alias_target (Typedtree.loc_of_typ typ))
         | _ -> ());
         (match decl.deriving_ with
         | [] -> ()
         | trait1 :: rest ->
             let loc =
               Lst.fold_left rest trait1.loc_ (fun l ->
                   fun r -> Rloc.merge l r.loc_)
             in
             add_error local_diagnostics (Errors.type_alias_cannot_derive loc));
         Global_env.All_types.add_type_alias types name
           { result with target = Type_alias stype };
         let tast_node : Typedtree.type_decl =
           {
             td_binder =
               {
                 name =
                   Type_path.toplevel_type
                     ~pkg:!Basic_config.current_package
                     name;
                 kind = Type;
                 loc_ = decl.tycon_loc_;
               };
             td_params = tvar_env;
             td_desc = Td_alias typ;
             td_vis = (if is_pub then Vis_fully_pub else Vis_priv);
             td_loc_ = decl.loc_;
             td_doc_ = decl.doc_;
             td_deriving_ = [];
           }
         in
         Hash_string.add type_tast_nodes name tast_node;
         Local_diagnostics.add_to_global local_diagnostics diagnostics
     | _ -> ()
   in
   Placeholder_env.iter_types placeholder_env (go_type ~visiting:[]);
   let trait_alias_decls = Vec.empty () in
   Placeholder_env.iter_trait_alias placeholder_env (fun name ->
       fun _ ->
        match Hash_string.find_opt trait_tast_nodes name with
        | Some decl -> Vec.push trait_alias_decls decl
        | _ -> ());
   (type_tast_nodes, Vec.to_list trait_alias_decls)
    : Typedtree.type_decl Hash_string.t * Typedtree.trait_alias_decl list)

let rec check_object_safety_in_typ ~types (typ : Typedtree.typ) ~diagnostics =
  let go typ = check_object_safety_in_typ ~types typ ~diagnostics [@@inline] in
  let check_stype sty ~loc =
    match Stype.type_repr sty with
    | T_trait trait when Type_path.get_pkg trait = !Basic_config.current_package
      -> (
        match Global_env.All_types.find_trait_by_path types trait with
        | Some decl -> (
            match
              Trait_decl.check_object_safety
                ~name:(Type_path_util.name trait)
                ~loc decl.object_safety_
            with
            | Some err -> Local_diagnostics.add_error diagnostics err
            | None -> ())
        | None -> ())
    | _ -> ()
  in
  match typ with
  | Tany _ -> ()
  | Tarrow { params; return; err_ty; ty = _ } -> (
      Lst.iter params ~f:go;
      go return;
      match err_ty with
      | Error_typ { ty } -> go ty
      | Default_error_typ _ | No_error_typ -> ())
  | T_tuple { params; ty = _ } -> Lst.iter params ~f:go
  | Tname { constr; params; ty } ->
      check_stype ty ~loc:constr.loc_;
      Lst.iter params ~f:go
  | Tobject { constr; ty } -> check_stype ty ~loc:constr.loc_

type partial_trait_info = {
  method_decls : Trait_decl.method_decl list;
  method_tast_nodes : Typedtree.method_decl list;
  self_object_safety : Trait_decl.object_safety_status;
}

let typing_types_and_traits (impls : Syntax.impl list)
    (types : Global_env.All_types.t) (toplevel_env : toplevel_env)
    ~ext_method_env ~trait_impls ~value_worklist ~(diagnostics : Diagnostics.t)
    =
  let placeholder_env =
    collect_types_and_traits ~foreign_types:types impls ~diagnostics
  in
  let tast_of_type_alias, tast_of_trait_alias =
    typing_alias ~placeholder_env ~types ~diagnostics
  in
  let partial_trait_info = Hash_string.create 17 in
  Placeholder_env.iter_traits placeholder_env (fun name ->
      fun decl ->
       let base = decl.trait_loc_ in
       let local_diagnostics = Local_diagnostics.make ~base in
       let method_decls, method_tast_nodes =
         let is_pub =
           match decl.trait_vis with
           | Vis_pub _ -> true
           | Vis_priv _ | Vis_default -> false
         in
         Lst.map_split decl.trait_methods
           (typing_trait_method_decl ~is_pub ~placeholder_env ~types ~base
              ~diagnostics:local_diagnostics)
       in
       Local_diagnostics.add_to_global local_diagnostics diagnostics;
       let self_object_safety = Trait_decl.get_methods_object_safety decl in
       Hash_string.add partial_trait_info name
         { method_decls; method_tast_nodes; self_object_safety });
  let trait_decls =
    Placeholder_env.traits_to_list_map placeholder_env (fun name ->
        fun decl ->
         (let local_diagnostics =
            Local_diagnostics.make ~base:decl.trait_loc_
          in
          let trait_vis =
            transl_trait_vis ~diagnostics:local_diagnostics decl.trait_vis
          in
          let path =
            Type_path.toplevel_type ~pkg:!Basic_config.current_package name
          in
          let trait_supers, closure =
            Typeutil.typing_super_traits ~types ~placeholder_env
              ~allow_private:(not (Typedecl_info.vis_is_pub trait_vis))
              ~self:path decl.trait_supers ~diagnostics:local_diagnostics
          in
          let supers =
            Lst.map trait_supers (fun tast_trait_name ->
                match tast_trait_name with
                | Tname_path { name; _ } | Tname_alias { name; _ } -> name
                | Tname_tvar _ -> assert false)
          in
          let { method_decls; method_tast_nodes; self_object_safety } =
            Hash_string.find_exn partial_trait_info name
          in
          let closure_methods =
            Lst.concat_map closure (fun trait ->
                match trait with
                | Toplevel { pkg; id } when pkg = !Basic_config.current_package
                  ->
                    Lst.map
                      (Hash_string.find_exn partial_trait_info id).method_decls
                      (fun meth_decl -> (trait, meth_decl))
                | _ -> (
                    match
                      Global_env.All_types.find_trait_by_path types trait
                    with
                    | None -> []
                    | Some trait_decl ->
                        Lst.map trait_decl.methods (fun meth_decl ->
                            (trait, meth_decl))))
          in
          let object_safety_ =
            self_object_safety
            @ Lst.fold_right supers [] (fun super ->
                  fun acc ->
                   match super with
                   | Toplevel { pkg; id }
                     when pkg = !Basic_config.current_package -> (
                       match
                         (Hash_string.find_exn partial_trait_info id)
                           .self_object_safety
                       with
                       | [] -> acc
                       | _ :: _ -> Trait_decl.Bad_super_trait super :: acc)
                   | _ -> (
                       match
                         Global_env.All_types.find_trait_by_path types super
                       with
                       | Some { object_safety_ = _ :: _; _ } ->
                           Trait_decl.Bad_super_trait super :: acc
                       | Some { object_safety_ = []; _ } | None -> acc))
          in
          let attrs =
            Checked_attributes.check ~local_diagnostics ~context:`Trait
              decl.trait_attrs
          in
          Local_diagnostics.add_to_global local_diagnostics diagnostics;
          let trait_decl : Trait_decl.t =
            {
              name = path;
              supers;
              closure;
              closure_methods;
              methods = method_decls;
              vis_ = trait_vis;
              loc_ = decl.trait_loc_;
              doc_ = decl.trait_doc_;
              attrs;
              object_safety_;
            }
          in
          Global_env.All_types.add_trait types name trait_decl;
          {
            trait_name =
              { name = path; kind = Trait; loc_ = decl.trait_name.loc_ };
            trait_supers;
            trait_methods = method_tast_nodes;
            trait_vis;
            trait_doc_ = decl.trait_doc_;
            trait_attrs = attrs;
            trait_loc_ = decl.trait_loc_;
          }
           : Typedtree.trait_decl))
  in
  let type_decls =
    Placeholder_env.types_to_list_map placeholder_env (fun name ->
        fun decl ->
         let local_diagnostics = Local_diagnostics.make ~base:decl.loc_ in
         let res =
           match decl.components with
           | Ptd_alias _ -> Hash_string.find_exn tast_of_type_alias name
           | _ ->
               typing_typedef decl ~placeholder_env types toplevel_env
                 ~ext_method_env ~trait_impls ~value_worklist
                 ~diagnostics:local_diagnostics
         in
         Local_diagnostics.add_to_global local_diagnostics diagnostics;
         res)
  in
  Lst.iter type_decls ~f:(fun decl ->
      let local_diagnostics = Local_diagnostics.make ~base:decl.td_loc_ in
      (match decl.td_desc with
      | Td_error (Single_payload typ) | Td_newtype typ | Td_alias typ ->
          check_object_safety_in_typ ~types typ ~diagnostics:local_diagnostics
      | Td_variant constrs | Td_error (Enum_payload constrs) ->
          Lst.iter constrs ~f:(fun constr ->
              Lst.iter constr.constr_args ~f:(fun constr_arg ->
                  check_object_safety_in_typ ~types constr_arg.carg_typ
                    ~diagnostics:local_diagnostics))
      | Td_record fields ->
          Lst.iter fields ~f:(fun field ->
              check_object_safety_in_typ ~types field.field_typ
                ~diagnostics:local_diagnostics)
      | Td_abstract | Td_error No_payload -> ());
      Local_diagnostics.add_to_global local_diagnostics diagnostics);
  Lst.iter trait_decls ~f:(fun decl ->
      let local_diagnostics = Local_diagnostics.make ~base:decl.trait_loc_ in
      Lst.iter decl.trait_methods ~f:(fun meth ->
          Lst.iter meth.method_params ~f:(fun (_, typ) ->
              check_object_safety_in_typ ~types typ
                ~diagnostics:local_diagnostics);
          (match meth.method_ret with
          | None -> ()
          | Some typ ->
              check_object_safety_in_typ ~types typ
                ~diagnostics:local_diagnostics);
          match meth.method_err with
          | None -> ()
          | Some typ ->
              check_object_safety_in_typ ~types typ
                ~diagnostics:local_diagnostics);
      Local_diagnostics.add_to_global local_diagnostics diagnostics);
  (type_decls, trait_decls, tast_of_trait_alias)

let add_method (type_name : Type_path.t) (fun_type : Stype.t)
    (meth : Syntax.binder) is_pub ~is_trait ~(doc : Docstring.t)
    ~(attrs : Typedtree.attributes) ~(types : Global_env.All_types.t) ~tvar_env
    ~method_env ~arity ~param_names ~prim ~(kind : Method_env.method_kind)
    ~(diagnostics : Local_diagnostics.t) ~(meth_loc : Loc.t) =
  (let method_name = meth.binder_name in
   let check_duplicate_method type_name =
     (if is_trait then
        match Global_env.All_types.find_trait_by_path types type_name with
        | None -> ()
        | Some trait_decl -> (
            match
              Lst.find_first trait_decl.methods (fun meth_decl ->
                  meth_decl.method_name = method_name)
            with
            | Some meth_decl ->
                add_error diagnostics
                  (Errors.method_duplicate ~method_name ~type_name
                     ~first_loc:meth_decl.method_loc_ ~second_loc:meth.loc_)
            | None -> ()));
     let report_duplicate (method_info : Method_env.method_info) =
       add_error diagnostics
         (Errors.method_duplicate ~method_name ~type_name
            ~first_loc:method_info.loc ~second_loc:meth.loc_)
         [@@inline]
     in
     (match
        Method_env.find_regular_method method_env ~type_name ~method_name
      with
     | Some x -> report_duplicate x
     | _ -> ());
     (match type_name with
     | Toplevel { pkg; id = _ } when pkg <> !Config.current_package -> (
         match
           Pkg.find_regular_method
             (Global_env.All_types.get_pkg_tbl types)
             ~pkg ~type_name ~method_name
         with
         | Some x -> report_duplicate x
         | _ -> ())
     | _ -> ());
     if Type_path.can_be_extended_from_builtin type_name then
       match
         Pkg.find_regular_method
           (Global_env.All_types.get_pkg_tbl types)
           ~pkg:Config.builtin_package ~type_name ~method_name
       with
       | Some x -> report_duplicate x
       | _ -> ()
   in
   let check_self_type () =
     match type_name with
     | _ when not (Type_path_util.is_foreign type_name) -> true
     | _ when !Basic_config.current_package = Basic_config.builtin_package ->
         true
     | _ ->
         add_error diagnostics
           (Errors.method_on_foreign_type ~method_name:meth.binder_name
              ~type_name ~loc:meth.loc_);
         false
   in
   check_operator_decl ~diagnostics ~loc:meth.loc_ method_name fun_type;
   check_duplicate_method type_name;
   let id : Basic_qual_ident.t =
     Qual_ident.meth ~self_typ:type_name ~name:method_name
   in
   (if check_self_type () then
      let method_info : Method_env.method_info =
        {
          id;
          prim;
          typ = fun_type;
          pub = is_pub;
          loc = meth_loc;
          doc_ = doc;
          attrs;
          ty_params_ = tvar_env;
          kind_ = kind;
          arity_ = arity;
          param_names_ = param_names;
        }
      in
      Method_env.add_method method_env ~type_name ~method_name ~method_info);
   id
    : Qual_ident.t)

let check_method_self_type ~(diagnostics : Local_diagnostics.t) ~(loc : Rloc.t)
    (self_type : Stype.t) =
  match Stype.type_repr self_type with
  | T_constr { type_constructor = p; tys = _ } -> Some (false, p)
  | T_builtin b -> Some (false, Stype.tpath_of_builtin b)
  | Tvar { contents = Tnolink Tvar_error } | T_blackhole -> None
  | Tvar { contents = Tnolink Tvar_normal } ->
      add_error diagnostics (Errors.cannot_determine_self_type loc);
      None
  | T_trait trait -> Some (true, trait)
  | Tarrow _ | Tparam _ ->
      add_error diagnostics (Errors.invalid_self_type loc);
      None
  | Tvar _ -> assert false

let add_self_method (self_type : Stype.t) (fun_type : Stype.t)
    (meth : Syntax.binder) is_pub ~(doc : docstring)
    ~(attrs : Typedtree.attributes) ~(types : Global_env.All_types.t) ~tvar_env
    ~toplevel ~method_env ~arity ~param_names ~prim
    ~(diagnostics : Diagnostics.t) ~(local_diagnostics : Local_diagnostics.t)
    ~meth_loc =
  (match
     check_method_self_type ~diagnostics:local_diagnostics ~loc:meth.loc_
       self_type
   with
   | Some (is_trait, type_name) ->
       let method_name = meth.binder_name in
       let qid =
         add_method type_name fun_type meth is_pub ~is_trait ~doc ~attrs ~types
           ~tvar_env ~method_env ~arity ~param_names ~prim ~kind:Regular_method
           ~diagnostics:local_diagnostics ~meth_loc
       in
       (match Typing_info.find_value toplevel method_name with
       | Some v ->
           let first_loc, extra_message = get_duplicate_loc_info v in
           add_global_error diagnostics
             (Errors.value_redeclare ~name:method_name ~first_loc
                ~second_loc:meth_loc ~extra_message)
       | None ->
           Typing_info.add_value toplevel
             {
               id = qid;
               typ = fun_type;
               pub = is_pub;
               kind =
                 (match prim with None -> Normal | Some prim -> Prim prim);
               loc_ = meth_loc;
               doc_ = doc;
               attrs;
               ty_params_ = tvar_env;
               arity_ = Some arity;
               param_names_ = param_names;
               direct_use_loc_ = Not_direct_use;
             });
       qid
   | None -> Qual_ident.toplevel_value ~name:meth.binder_name
    : Qual_ident.t)

let add_ext_method (trait_decl : Trait_decl.t) ~(self_ty : Stype.t)
    (fun_type : Stype.t) (meth : Syntax.binder) is_pub ~(doc : Docstring.t)
    ~(attrs : Checked_attributes.t) ~types ~tvar_env ~ext_method_env
    ~trait_impls ~arity ~param_names ~prim ~(diagnostics : Local_diagnostics.t)
    ~(global_diagnostics : Diagnostics.t) ~header_loc ~meth_loc =
  (let trait = trait_decl.name in
   let method_name = meth.binder_name in
   match Trait_decl.find_method trait_decl method_name ~loc:meth.loc_ with
   | Error err ->
       add_error diagnostics err;
       Qual_ident.toplevel_value ~name:method_name
   | Ok meth_decl -> (
       let check_type () =
         let exception Arity_mismatch in
         let expected_ty =
           Poly_type.instantiate_method_decl meth_decl ~self:self_ty
         in
         try
           Ctype.unify_exn expected_ty fun_type;
           if not (Fn_arity.equal arity meth_decl.method_arity) then
             raise_notrace Arity_mismatch
         with _ ->
           add_error diagnostics
             (Errors.ext_method_type_mismatch ~trait ~method_name
                ~expected:
                  (Printer.toplevel_function_type_to_string
                     ~arity:meth_decl.method_arity expected_ty)
                ~actual:
                  (Printer.toplevel_function_type_to_string ~arity fun_type)
                ~loc:meth.loc_)
           [@@inline]
       in
       check_type ();
       match check_method_self_type ~diagnostics ~loc:meth.loc_ self_ty with
       | None -> Qual_ident.toplevel_value ~name:method_name
       | Some (is_trait, type_name) ->
           let check_duplication () =
             if is_trait && Type_path.equal type_name trait then
               add_error diagnostics
                 (Errors.method_duplicate ~method_name ~type_name
                    ~first_loc:meth_decl.method_loc_ ~second_loc:meth.loc_);
             let prev_def =
               match
                 Ext_method_env.find_method ext_method_env ~trait
                   ~self_type:type_name ~method_name
               with
               | Some _ as prev_def -> prev_def
               | None ->
                   Pkg.find_ext_method_opt
                     (Global_env.All_types.get_pkg_tbl types)
                     ~pkg:Config.builtin_package ~trait ~self_type:type_name
                     ~method_name
             in
             match prev_def with
             | None -> ()
             | Some prev_def ->
                 let trait_name =
                   Type_path.short_name
                     ~cur_pkg_name:(Some !Basic_config.current_package) trait
                 in
                 add_error diagnostics
                   (Errors.method_duplicate
                      ~method_name:
                        ((trait_name ^ "::" ^ method_name
                          : Stdlib.String.t)
                          [@merlin.hide])
                      ~type_name ~first_loc:prev_def.loc ~second_loc:meth.loc_)
               [@@inline]
           in
           check_duplication ();
           let id : Basic_qual_ident.t =
             Qual_ident.ext_meth ~trait ~self_typ:type_name ~name:method_name
           in
           let method_info : Ext_method_env.method_info =
             {
               id;
               prim;
               typ = fun_type;
               pub = is_pub;
               loc = meth_loc;
               doc_ = doc;
               attrs;
               ty_params_ = tvar_env;
               kind_ = Method_explicit_self { self_ty };
               arity_ = arity;
               param_names_ = param_names;
             }
           in
           if
             Type_path_util.is_foreign trait
             && Type_path_util.is_foreign type_name
           then
             add_error diagnostics
               (Errors.ext_method_foreign_trait_foreign_type ~trait ~type_name
                  ~method_name ~loc:meth.loc_)
           else (
             (match Trait_impl.find_impl trait_impls ~trait ~type_name with
             | None ->
                 let impl : Trait_impl.impl =
                   {
                     trait;
                     self_ty;
                     ty_params = tvar_env;
                     is_pub;
                     is_implicit_ = false;
                     doc_ = Docstring.empty;
                     loc_ = header_loc;
                   }
                 in
                 Trait_impl.add_impl trait_impls ~trait ~type_name impl
             | Some impl ->
                 if not (Type.same_type impl.self_ty self_ty) then
                   add_global_error global_diagnostics
                     (Errors.inconsistent_impl
                        ~trait:(Type_path_util.name trait)
                        ~type_name:(Type_path_util.name type_name)
                        ~reason:
                          (`Self_type_mismatch
                             ( Printer.type_to_string impl.self_ty,
                               Printer.type_to_string self_ty ))
                        ~loc1:impl.loc_ ~loc2:header_loc)
                 else if not (Tvar_env.equal impl.ty_params tvar_env) then
                   add_global_error global_diagnostics
                     (Errors.inconsistent_impl
                        ~trait:(Type_path_util.name trait)
                        ~type_name:(Type_path_util.name type_name)
                        ~reason:`Type_parameter_bound ~loc1:impl.loc_
                        ~loc2:header_loc)
                 else if is_pub && not impl.is_pub then
                   Trait_impl.update trait_impls ~trait ~type_name (fun impl ->
                       { impl with is_pub = true; loc_ = header_loc }));
             Ext_method_env.add_method ext_method_env ~trait
               ~self_type:type_name ~method_name method_info);
           id)
    : Qual_ident.t)

let try_infer_expr ~types expr =
  (let exception Cannot_infer in
   let is_literal = ref true in
   let rec go (expr : Syntax.expr) =
     match expr with
     | Pexpr_constant { c } -> Typeutil.type_of_constant c
     | Pexpr_interp _ | Pexpr_multiline_string _ -> Stype.string
     | Pexpr_array { exprs = [] } -> raise_notrace Cannot_infer
     | Pexpr_array { exprs = expr0 :: exprs } ->
         let ty0 = go expr0 in
         Lst.iter exprs ~f:(fun expr ->
             let ty = go expr in
             ignore (Ctype.try_unify ty0 ty));
         Builtin.type_array ty0
     | Pexpr_tuple { exprs } -> Builtin.type_product (Lst.map exprs go)
     | Pexpr_ident { id = { var_name = Ldot { pkg; id } } } -> (
         match Global_env.All_types.find_foreign_value types ~pkg ~name:id with
         | None -> raise_notrace Cannot_infer
         | Some (Local_imm _ | Local_mut _) -> assert false
         | Some (Toplevel_value { ty_params_; _ })
           when not (Tvar_env.is_empty ty_params_) ->
             raise_notrace Cannot_infer
         | Some (Toplevel_value { typ; _ }) ->
             is_literal := false;
             typ)
     | _ -> raise_notrace Cannot_infer
   in
   try
     let ty = go expr in
     Some (ty, !is_literal)
   with Cannot_infer -> None
    : (Stype.t * bool) option)

let prim_of_decl (decl : Syntax.decl_body) (loc : Loc.t) ~(doc : Docstring.t)
    ~(diagnostics : Diagnostics.t) =
  match decl with
  | Decl_stubs (Embedded { language = None; code = Code_string s }) ->
      if Strutil.first_char_is s.string_val '%' then (
        let prim = Primitive.find_prim s.string_val in
        if prim = None then
          add_global_error diagnostics
            (Errors.unknown_intrinsic ~name:s.string_val ~loc);
        prim)
      else None
  | Decl_body _ -> (
      match
        Lst.fold_right (Docstring.pragmas doc) [] (fun pragma ->
            fun acc ->
             match pragma with
             | Pragma_alert _ -> acc
             | Pragma_gen_js _ -> acc
             | Pragma_intrinsic intrinsic -> intrinsic :: acc
             | Pragma_coverage_skip -> acc)
      with
      | [] -> None
      | intrinsic :: [] ->
          let prim = Primitive.find_prim intrinsic in
          if prim = None then
            add_global_error diagnostics
              (Errors.unknown_intrinsic ~name:intrinsic ~loc);
          prim
      | _ :: _ ->
          add_global_error diagnostics (Errors.multiple_intrinsic loc);
          None)
  | Decl_stubs _ -> None

let check_toplevel_decl ~(toplevel : toplevel_env) (impls : Syntax.impls)
    (types : Global_env.All_types.t) ~(method_env : Method_env.t)
    ~(ext_method_env : Ext_method_env.t) ~(trait_impls : Trait_impl.t)
    ~(worklist : Local_typing_worklist.value Vec.t)
    ~(constant_worklist : Local_typing_worklist.const_decl Vec.t) ~diagnostics
    ~(build_context : Typeutil.build_context) =
  (let add_symbol (toplevel : toplevel_env) (binder : Syntax.binder) ty is_pub
       doc (tvar_env : Tvar_env.t) ~(attrs : Checked_attributes.t) ~kind ~arity
       ~param_names ~(binder_loc : Loc.t) =
     (let name = binder.binder_name in
      (match Typing_info.find_value toplevel name with
      | Some v ->
          let first_loc, extra_message = get_duplicate_loc_info v in
          add_global_error diagnostics
            (Errors.value_redeclare ~name ~first_loc ~second_loc:binder_loc
               ~extra_message)
      | None -> ());
      let qid : Qual_ident.t = Qual_ident.toplevel_value ~name in
      Typing_info.add_value toplevel
        {
          id = qid;
          typ = ty;
          pub = is_pub;
          kind;
          loc_ = binder_loc;
          doc_ = doc;
          attrs;
          ty_params_ = tvar_env;
          arity_ = arity;
          param_names_ = param_names;
          direct_use_loc_ = Not_direct_use;
        };
      qid
       : Qual_ident.t)
   in
   let main_loc = ref None in
   let go (impl : Syntax.impl) =
     (match impl with
      | Ptop_expr { expr; is_main = true; local_types; loc_ } ->
          if build_context = Lib then
            add_global_error diagnostics (Errors.unexpected_main loc_);
          if !main_loc <> None then
            add_global_error diagnostics
              (Errors.multiple_main ~first_loc:(Option.get !main_loc)
                 ~second_loc:loc_)
          else main_loc := Some loc_;
          let id =
            Basic_qual_ident.toplevel_value
              ~name:(("*main" : Stdlib.String.t) [@merlin.hide])
          in
          Vec.push worklist
            (Wl_top_expr { expr; is_main = true; id; loc_; local_types })
      | Ptop_expr { expr; is_main = false; local_types; loc_ } ->
          let id =
            Basic_qual_ident.toplevel_value
              ~name:
                (("*init" ^ Int.to_string (Basic_uuid.next ())
                  : Stdlib.String.t)
                  [@merlin.hide])
          in
          Vec.push worklist
            (Wl_top_expr { expr; is_main = false; id; loc_; local_types })
      | Ptop_test _ -> assert false
      | Ptop_typedef _ -> ()
      | Ptop_trait _ -> ()
      | Ptop_trait_alias _ -> ()
      | Ptop_letdef { binder; ty; expr; is_constant; vis; loc_; attrs; doc_ } ->
          let local_diagnostics = Local_diagnostics.make ~base:loc_ in
          let binder_loc = Rloc.to_loc ~base:loc_ binder.loc_ in
          let is_pub =
            transl_value_visibility ~local_diagnostics ~entity:"value" vis
          in
          let check_toplevel_let (binder : Syntax.binder) (expr : Syntax.expr)
              (ty_opt : Syntax.typ option) (toplevel : toplevel_env) =
            (let add_letdef konstraint typ =
               let attrs =
                 Checked_attributes.check ~local_diagnostics ~context:`TopLet
                   attrs
               in
               if is_constant then
                 Vec.push constant_worklist
                   {
                     binder;
                     expr;
                     is_pub;
                     loc_;
                     doc_;
                     attrs;
                     konstraint;
                     id = Qual_ident.toplevel_value ~name:binder.binder_name;
                     typ;
                   }
               else
                 let id =
                   add_symbol toplevel binder typ is_pub doc_ Tvar_env.empty
                     ~attrs ~kind:Normal ~arity:None ~param_names:[] ~binder_loc
                 in
                 Vec.push worklist
                   (Wl_top_letdef
                      {
                        binder;
                        expr;
                        is_pub;
                        loc_;
                        doc_;
                        attrs;
                        konstraint;
                        id;
                        typ;
                      })
                 [@@inline]
             in
             match (ty_opt, expr) with
             | Some ty_expr, _ | None, Pexpr_constraint { ty = ty_expr; _ } ->
                 let konstraint =
                   typing_type ~allow_private:(not is_pub) ty_expr
                     ~tvar_env:Tvar_env.empty ~types
                     ~diagnostics:local_diagnostics
                 in
                 add_letdef (Some konstraint)
                   (Typedtree_util.stype_of_typ konstraint)
             | None, expr -> (
                 match try_infer_expr ~types expr with
                 | Some (ty, is_literal) ->
                     if (not is_literal) && is_pub then
                       add_error local_diagnostics
                         (Errors.let_missing_annot ~name:binder.binder_name
                            ~loc:binder.loc_ ~reason:`Pub_not_literal);
                     add_letdef None ty
                 | None ->
                     add_error local_diagnostics
                       (Errors.let_missing_annot ~name:binder.binder_name
                          ~loc:binder.loc_ ~reason:`Cannot_infer);
                     add_letdef None Stype.blackhole)
              : unit)
          in
          let res = check_toplevel_let binder expr ty toplevel in
          Local_diagnostics.add_to_global local_diagnostics diagnostics;
          res
      | Ptop_funcdef { fun_decl; decl_body; loc_ } ->
          let local_diagnostics = Local_diagnostics.make ~base:loc_ in
          let check_toplevel_fun (fun_decl : Syntax.fun_decl)
              (toplevel : toplevel_env) =
            let ({
                   type_name;
                   name;
                   has_error;
                   is_async;
                   quantifiers;
                   return_type;
                   decl_params;
                   params_loc_;
                   vis;
                   doc_;
                   attrs;
                 }
                  : Syntax.fun_decl) =
              fun_decl
            in
            let attrs =
              Checked_attributes.check ~local_diagnostics ~context:`TopFun attrs
            in
            let is_pub =
              transl_value_visibility ~local_diagnostics ~entity:"function" vis
            in
            let tvar_env, constraint_names =
              Typeutil.typing_func_def_tparam_binders
                ~allow_private:(not is_pub) ~types quantifiers
                ~diagnostics:local_diagnostics
            in
            let decl_params = Option.value ~default:[] decl_params in
            let params_typ =
              Lst.map decl_params (fun (p : Syntax.parameter) ->
                  match Syntax.ty_of_param p with
                  | None ->
                      let binder_name = Syntax.binder_name_of_param p in
                      let loc = Syntax.loc_of_param_binder p in
                      add_error local_diagnostics
                        (Errors.missing_param_annot ~name:binder_name ~loc);
                      (Stype.blackhole, None)
                  | Some ty ->
                      let typ =
                        typing_type ~allow_private:(not is_pub) ty ~tvar_env
                          ~types ~diagnostics:local_diagnostics
                      in
                      (Typedtree_util.stype_of_typ typ, Some typ))
            in
            let ret_sty, err_sty, (ret_annotation : Typeutil.ret_annotation) =
              match return_type with
              | None -> (
                  match decl_body with
                  | Decl_body _ ->
                      add_error local_diagnostics
                        (Errors.missing_return_annot fun_decl.name.loc_);
                      (Stype.blackhole, None, No_annotation)
                  | Decl_stubs _ -> (Stype.unit, None, No_annotation))
              | Some (res_ty, err_ty) ->
                  let typ =
                    typing_type ~allow_private:(not is_pub) res_ty ~tvar_env
                      ~types ~diagnostics:local_diagnostics
                  in
                  let err_sty, err_typ =
                    typing_error_type ~allow_private:(not is_pub) ~types
                      ~tvar_env err_ty ~diagnostics:local_diagnostics
                      ~has_error:
                        (match has_error with Some _ -> true | _ -> false)
                      ~header_loc:name.loc_
                  in
                  ( Typedtree_util.stype_of_typ typ,
                    err_sty,
                    Annotated (typ, err_typ) )
            in
            let typ_generic = not (quantifiers = []) in
            let fun_type : Stype.t =
              Tarrow
                {
                  params_ty = Lst.map params_typ fst;
                  ret_ty = ret_sty;
                  err_ty = err_sty;
                  generic_ = typ_generic;
                  is_async;
                }
            in
            let arity =
              take_info_partial ~diagnostics:local_diagnostics
                (Fn_arity.from_params decl_params ~base:loc_)
            in
            let param_names = Lst.map decl_params Syntax.binder_name_of_param in
            let prim = prim_of_decl decl_body loc_ ~doc:doc_ ~diagnostics in
            let kind, id =
              match type_name with
              | Some type_name -> (
                  match
                    Typeutil.typing_type_name type_name
                      ~allow_private:(not is_pub) ~tvar_env:Tvar_env.empty
                      ~types ~local_type_env:None ~diagnostics:local_diagnostics
                  with
                  | Some ((Tname_param _ | Tname_local_type _), _) ->
                      assert false
                  | Some
                      ( (Tname_predef ty_constr | Tname_defined { ty_constr; _ }),
                        type_name ) ->
                      let meth_loc = Rloc.to_loc ~base:loc_ name.loc_ in
                      ( Typedtree.Fun_kind_method (Some type_name),
                        add_method ty_constr fun_type name is_pub
                          ~is_trait:false ~doc:doc_ ~attrs ~types ~tvar_env
                          ~method_env ~diagnostics:local_diagnostics ~arity
                          ~param_names ~prim ~kind:Regular_method_qualified
                          ~meth_loc )
                  | Some (Tname_trait_object trait_decl, type_name) ->
                      let meth_loc = Rloc.to_loc ~base:loc_ name.loc_ in
                      ( Typedtree.Fun_kind_method (Some type_name),
                        add_method trait_decl.name fun_type name is_pub
                          ~is_trait:true ~doc:doc_ ~attrs ~types ~tvar_env
                          ~method_env ~diagnostics:local_diagnostics ~arity
                          ~param_names ~prim ~kind:Regular_method_qualified
                          ~meth_loc )
                  | Some (Tname_trait _, tast_type_name) ->
                      Local_diagnostics.add_error local_diagnostics
                        (Errors.type_not_found
                           ~tycon:(Basic_longident.to_string type_name.name)
                           ~loc:type_name.loc_);
                      ( Typedtree.Fun_kind_method (Some tast_type_name),
                        Qual_ident.toplevel_value ~name:name.binder_name )
                  | None ->
                      ( Typedtree.Fun_kind_regular,
                        Qual_ident.toplevel_value ~name:name.binder_name ))
              | None -> (
                  match Typeutil.classify_func decl_params with
                  | Method _ ->
                      let meth_loc = Rloc.to_loc ~base:loc_ name.loc_ in
                      let self_ty, _ = List.hd params_typ in
                      ( Typedtree.Fun_kind_method None,
                        add_self_method self_ty fun_type name is_pub ~doc:doc_
                          ~attrs ~types ~tvar_env ~method_env ~toplevel
                          ~diagnostics ~local_diagnostics ~arity ~param_names
                          ~prim ~meth_loc )
                  | Regular_func ->
                      let binder_loc = Rloc.to_loc ~base:loc_ name.loc_ in
                      ( Typedtree.Fun_kind_regular,
                        add_symbol toplevel name fun_type is_pub doc_ tvar_env
                          ~attrs ~arity:(Some arity)
                          ~kind:
                            (match prim with
                            | None -> Normal
                            | Some p -> Prim p)
                          ~param_names ~binder_loc ))
            in
            let typed_fn_annotation : Local_typing_worklist.typed_fn_annotation
                =
              {
                params_ty = params_typ;
                ret_ty = ret_sty;
                err_ty = err_sty;
                is_async;
                ret_annotation;
              }
            in
            Vec.push worklist
              (Wl_top_funcdef
                 {
                   fun_binder = name;
                   decl_params;
                   params_loc = params_loc_;
                   is_pub;
                   doc = doc_;
                   attrs;
                   decl_body;
                   loc_;
                   id;
                   kind;
                   arity;
                   tvar_env;
                   constraint_names;
                   typed_fn_annotation;
                 })
          in
          let res = check_toplevel_fun fun_decl toplevel in
          Local_diagnostics.add_to_global local_diagnostics diagnostics;
          res
      | Ptop_impl
          {
            self_ty = None;
            trait;
            method_name = method_binder;
            has_error;
            quantifiers;
            params;
            ret_ty;
            body;
            vis;
            header_loc_;
            loc_;
            attrs;
            doc_;
          } ->
          let local_diagnostics = Local_diagnostics.make ~base:loc_ in
          let method_name = method_binder.binder_name in
          let is_partial = method_name = "*" in
          (match vis with
          | Vis_default -> ()
          | Vis_pub { loc_; attr = _ } | Vis_priv { loc_ } ->
              add_error local_diagnostics (Errors.no_vis_on_default_impl loc_));
          let is_pub, trait_decl, type_name =
            match
              typing_trait_name ~types ~allow_private:true trait
                ~diagnostics:local_diagnostics
            with
            | Some trait_decl, type_name ->
                let is_pub =
                  match trait_decl.vis_ with
                  | Vis_fully_pub -> true
                  | Vis_priv | Vis_default | Vis_readonly -> false
                in
                (is_pub, Some trait_decl, type_name)
            | None, type_name -> (false, None, type_name)
          in
          let method_decl =
            match trait_decl with
            | None -> None
            | Some trait_decl -> (
                match
                  Trait_decl.find_method trait_decl method_name
                    ~loc:method_binder.loc_
                with
                | Ok meth_decl -> Some meth_decl
                | Error err ->
                    if not is_partial then add_error local_diagnostics err;
                    None)
          in
          let tvar_env, constraint_names =
            Typeutil.typing_func_def_tparam_binders ~allow_private:(not is_pub)
              ~types
              ({
                 tvar_name = "Self";
                 tvar_constraints =
                   (match trait_decl with
                   | Some _ ->
                       [ { tvc_trait = trait.name; loc_ = Rloc.no_location } ]
                   | _ -> []);
                 loc_ = Rloc.no_location;
               }
              :: quantifiers)
              ~diagnostics:local_diagnostics
          in
          let arity =
            take_info_partial ~diagnostics:local_diagnostics
              (Fn_arity.from_params params ~base:loc_)
          in
          let params_typ =
            Lst.map params (fun p ->
                match Syntax.ty_of_param p with
                | None -> (Stype.new_type_var Tvar_normal, None)
                | Some ty ->
                    let typ =
                      typing_type ~allow_private:(not is_pub) ty ~tvar_env
                        ~types ~diagnostics:local_diagnostics
                    in
                    (Typedtree_util.stype_of_typ typ, Some typ))
          in
          let ret_sty, err_sty, (ret_annotation : Typeutil.ret_annotation) =
            match ret_ty with
            | None ->
                let has_error_type =
                  match method_decl with
                  | Some method_decl -> (
                      match method_decl.method_typ with
                      | Tarrow { err_ty = Some _; _ } -> true
                      | _ -> false)
                  | None -> false
                in
                ( Stype.new_type_var Tvar_normal,
                  (if has_error_type then Some (Stype.new_type_var Tvar_normal)
                   else None),
                  if has_error_type then Has_super_error header_loc_
                  else No_annotation )
            | Some (res_ty, err_ty) ->
                let typ =
                  typing_type ~allow_private:(not is_pub) res_ty ~tvar_env
                    ~types ~diagnostics:local_diagnostics
                in
                let err_sty, err_typ =
                  typing_error_type ~allow_private:(not is_pub) err_ty ~tvar_env
                    ~types ~diagnostics:local_diagnostics ~has_error
                    ~header_loc:method_binder.loc_
                in
                ( Typedtree_util.stype_of_typ typ,
                  err_sty,
                  Annotated (typ, err_typ) )
          in
          let ty_func : Stype.t =
            Tarrow
              {
                params_ty = Lst.map params_typ fst;
                ret_ty = ret_sty;
                err_ty = err_sty;
                generic_ = true;
                is_async = false;
              }
          in
          let attrs =
            Checked_attributes.check ~local_diagnostics ~context:`Impl attrs
          in
          let typed_fn_annotation : Local_typing_worklist.typed_fn_annotation =
            {
              params_ty = params_typ;
              ret_ty = ret_sty;
              err_ty = err_sty;
              is_async = false;
              ret_annotation;
            }
          in
          let add_to_worklist id =
            Vec.push worklist
              (Wl_top_funcdef
                 {
                   fun_binder = method_binder;
                   decl_params = params;
                   params_loc = Rloc.no_location;
                   is_pub;
                   doc = doc_;
                   attrs;
                   decl_body = body;
                   loc_;
                   id;
                   kind = Fun_kind_default_impl type_name;
                   arity;
                   tvar_env;
                   constraint_names;
                   typed_fn_annotation;
                 })
              [@@inline]
          in
          let res =
            match trait_decl with
            | None ->
                add_to_worklist (Qual_ident.toplevel_value ~name:method_name)
            | Some trait_decl -> (
                let trait = trait_decl.name in
                let id =
                  Qual_ident.ext_meth ~trait
                    ~self_typ:Type_path.Builtin.default_impl_placeholder
                    ~name:method_name
                in
                add_to_worklist id;
                match
                  Trait_decl.find_method trait_decl method_name
                    ~loc:method_binder.loc_
                with
                | Error err -> add_error local_diagnostics err
                | Ok meth_decl -> (
                    let exception Arity_mismatch in
                    (try
                       Ctype.unify_exn meth_decl.method_typ ty_func;
                       if not (Fn_arity.equal arity meth_decl.method_arity) then
                         raise_notrace Arity_mismatch
                     with _ ->
                       add_error local_diagnostics
                         (Errors.ext_method_type_mismatch ~trait ~method_name
                            ~expected:
                              (Printer.toplevel_function_type_to_string
                                 ~arity:meth_decl.method_arity
                                 meth_decl.method_typ)
                            ~actual:
                              (Printer.toplevel_function_type_to_string ~arity
                                 ty_func)
                            ~loc:method_binder.loc_));
                    if Type_path_util.is_foreign trait then
                      add_error local_diagnostics
                        (Errors.default_method_on_foreign ~trait
                           ~loc:method_binder.loc_)
                    else
                      match
                        Ext_method_env.find_method ext_method_env ~trait
                          ~self_type:Type_path.Builtin.default_impl_placeholder
                          ~method_name
                      with
                      | Some mi ->
                          add_error local_diagnostics
                            (Errors.default_method_duplicate ~trait ~method_name
                               ~first_loc:mi.loc ~second_loc:method_binder.loc_)
                      | None ->
                          let method_info : Ext_method_env.method_info =
                            {
                              id;
                              prim =
                                prim_of_decl body loc_ ~doc:doc_ ~diagnostics;
                              typ = ty_func;
                              pub = is_pub;
                              loc = loc_;
                              doc_;
                              attrs;
                              ty_params_ = tvar_env;
                              kind_ =
                                Method_explicit_self
                                  {
                                    self_ty =
                                      (Tvar_env.find_by_index_exn tvar_env 0)
                                        .typ;
                                  };
                              arity_ = arity;
                              param_names_ =
                                Lst.map params Syntax.binder_name_of_param;
                            }
                          in
                          Ext_method_env.add_method ext_method_env ~trait
                            ~self_type:
                              Type_path.Builtin.default_impl_placeholder
                            ~method_name method_info))
          in
          Local_diagnostics.add_to_global local_diagnostics diagnostics;
          res
      | Ptop_impl
          {
            self_ty = Some self_ty;
            trait;
            method_name = method_binder;
            has_error;
            quantifiers;
            params;
            ret_ty;
            body;
            vis;
            header_loc_;
            loc_;
            attrs;
            doc_;
          } ->
          let local_diagnostics = Local_diagnostics.make ~base:loc_ in
          let method_name = method_binder.binder_name in
          let is_pub =
            transl_value_visibility ~local_diagnostics ~entity:"impl" vis
          in
          let tvar_env, constraint_names =
            Typeutil.typing_func_def_tparam_binders ~allow_private:(not is_pub)
              ~types quantifiers ~diagnostics:local_diagnostics
          in
          let self_typ =
            typing_type ~allow_private:(not is_pub) self_ty ~tvar_env ~types
              ~diagnostics:local_diagnostics
          in
          let arity =
            take_info_partial ~diagnostics:local_diagnostics
              (Fn_arity.from_params params ~base:loc_)
          in
          let params_typ =
            Lst.map params (fun p ->
                match Syntax.ty_of_param p with
                | None -> (Stype.new_type_var Tvar_normal, None)
                | Some ty ->
                    let typ =
                      typing_type ~allow_private:(not is_pub) ty ~tvar_env
                        ~types ~diagnostics:local_diagnostics
                    in
                    (Typedtree_util.stype_of_typ typ, Some typ))
          in
          let ret_sty, err_sty, (ret_annotation : Typeutil.ret_annotation) =
            match ret_ty with
            | None ->
                let has_error_typ =
                  match
                    Global_env.All_types.find_trait types trait.name
                      ~loc:trait.loc_
                  with
                  | Error _ -> false
                  | Ok trait_decl -> (
                      match
                        Trait_decl.find_method trait_decl method_name
                          ~loc:method_binder.loc_
                      with
                      | Ok method_decl -> (
                          match method_decl.method_typ with
                          | Tarrow { err_ty = Some _; _ } -> true
                          | _ -> false)
                      | Error _ -> false)
                in
                ( Stype.new_type_var Tvar_normal,
                  (if has_error_typ then Some (Stype.new_type_var Tvar_normal)
                   else None),
                  if has_error_typ then Has_super_error header_loc_
                  else No_annotation )
            | Some (res_ty, err_ty) ->
                let typ =
                  typing_type ~allow_private:(not is_pub) res_ty ~tvar_env
                    ~types ~diagnostics:local_diagnostics
                in
                let err_sty, err_typ =
                  typing_error_type ~allow_private:(not is_pub) ~types ~tvar_env
                    err_ty ~diagnostics:local_diagnostics ~has_error
                    ~header_loc:method_binder.loc_
                in
                ( Typedtree_util.stype_of_typ typ,
                  err_sty,
                  Annotated (typ, err_typ) )
          in
          let ty_func : Stype.t =
            Tarrow
              {
                params_ty = Lst.map params_typ fst;
                ret_ty = ret_sty;
                err_ty = err_sty;
                is_async = false;
                generic_ = true;
              }
          in
          let typed_fn_annotation : Local_typing_worklist.typed_fn_annotation =
            {
              params_ty = params_typ;
              ret_ty = ret_sty;
              err_ty = err_sty;
              is_async = false;
              ret_annotation;
            }
          in
          let attrs =
            Checked_attributes.check ~local_diagnostics ~context:`Impl attrs
          in
          let add_to_worklist id type_name =
            Vec.push worklist
              (Wl_top_funcdef
                 {
                   fun_binder = method_binder;
                   decl_params = params;
                   params_loc = Rloc.no_location;
                   is_pub;
                   doc = doc_;
                   attrs;
                   decl_body = body;
                   loc_;
                   id;
                   kind =
                     Fun_kind_impl { self_ty = self_typ; trait = type_name };
                   arity;
                   tvar_env;
                   constraint_names;
                   typed_fn_annotation;
                 })
              [@@inline]
          in
          let res =
            match
              typing_trait_name ~types ~allow_private:(not is_pub) trait
                ~diagnostics:local_diagnostics
            with
            | None, type_name ->
                add_to_worklist
                  (Qual_ident.toplevel_value ~name:method_name)
                  type_name
            | ( Some ({ vis_ = Vis_default | Vis_readonly; _ } as trait_decl),
                type_name )
              when Type_path_util.is_foreign trait_decl.name ->
                let trait_vis =
                  match trait_decl.vis_ with
                  | Vis_default -> "abstract"
                  | Vis_readonly -> "readonly"
                  | Vis_priv | Vis_fully_pub -> assert false
                in
                add_error local_diagnostics
                  (Errors.cannot_implement_sealed_trait
                     ~trait:(Type_path_util.name trait_decl.name)
                     ~trait_vis ~loc:trait.loc_);
                add_to_worklist
                  (Qual_ident.toplevel_value ~name:method_name)
                  type_name
            | Some trait_decl, type_name ->
                let meth_loc = Rloc.to_loc ~base:loc_ method_binder.loc_ in
                let header_loc =
                  let self_typ =
                    Rloc.to_loc ~base:loc_ (Typedtree.loc_of_typ self_typ)
                  in
                  Loc.of_menhir (Loc.get_start loc_, Loc.get_end self_typ)
                in
                let prim = prim_of_decl body loc_ ~doc:doc_ ~diagnostics in
                let id =
                  add_ext_method trait_decl ty_func method_binder is_pub
                    ~self_ty:(Typedtree_util.stype_of_typ self_typ)
                    ~doc:doc_ ~attrs ~types ~tvar_env ~ext_method_env
                    ~trait_impls ~arity
                    ~param_names:(Lst.map params Syntax.binder_name_of_param)
                    ~prim ~diagnostics:local_diagnostics
                    ~global_diagnostics:diagnostics ~meth_loc ~header_loc
                in
                add_to_worklist id type_name
          in
          Local_diagnostics.add_to_global local_diagnostics diagnostics;
          res
      | Ptop_impl_relation _ -> assert false
       : unit)
   in
   Lst.iter impls ~f:go;
   match build_context with
   | Exec { is_main_loc } when !main_loc = None ->
       add_global_error diagnostics (Errors.missing_main ~loc:is_main_loc)
   | _ -> ()
    : unit)

let load_trait_object_methods ~global_env =
  let method_env = Global_env.get_method_env global_env in
  let ext_method_env = Global_env.get_ext_method_env global_env in
  let trait_impls = Global_env.get_trait_impls global_env in
  Typing_info.iter_traits (Global_env.get_toplevel_types global_env)
    (fun (_, trait_decl) ->
      let trait = trait_decl.name in
      let self_type : Stype.t = T_trait trait in
      Lst.iteri trait_decl.closure_methods ~f:(fun index ->
          fun (actual_trait, method_decl) ->
           let method_name = method_decl.method_name in
           let method_info : Method_env.method_info =
             {
               id =
                 Qual_ident.ext_meth ~trait:actual_trait ~self_typ:trait
                   ~name:method_name;
               prim =
                 Some
                   (Pcall_object_method { method_index = index; method_name });
               typ =
                 Poly_type.instantiate_method_decl method_decl ~self:self_type;
               pub = Typedecl_info.vis_is_pub trait_decl.vis_;
               loc = method_decl.method_loc_;
               doc_ = Docstring.empty;
               attrs = [];
               ty_params_ = Tvar_env.empty;
               kind_ = Method_explicit_self { self_ty = self_type };
               arity_ = method_decl.method_arity;
               param_names_ = [];
             }
           in
           Ext_method_env.add_method ext_method_env ~trait:actual_trait
             ~self_type:trait ~method_name method_info;
           if Type_path.equal trait actual_trait then
             Method_env.add_method method_env ~type_name:trait ~method_name
               ~method_info
           else
             Method_env.add_impl method_env ~type_name:trait ~method_name
               ~method_info);
      Lst.iter trait_decl.closure ~f:(fun actual_trait ->
          let impl : Trait_impl.impl =
            {
              trait = actual_trait;
              self_ty = self_type;
              ty_params = Tvar_env.empty;
              is_pub = Typedecl_info.vis_is_pub trait_decl.vis_;
              is_implicit_ = true;
              doc_ = Docstring.empty;
              loc_ = Loc.no_location;
            }
          in
          Trait_impl.add_impl trait_impls ~trait:actual_trait ~type_name:trait
            impl))

let check_traits_implemented ~global_env ~diagnostics =
  let check_method ~trait ~(trait_vis : Typedecl_info.visibility) ~self_typ
      ~(request : Trait_impl.impl) (method_decl : Trait_decl.method_decl) =
    let method_name = method_decl.method_name in
    let method_impl =
      Global_env.find_trait_method global_env ~trait ~type_name:self_typ
        ~method_name
    in
    (match method_impl with
    | Some ({ id = Qext_method _; _ } as method_info) ->
        Method_env.add_impl
          (Global_env.get_method_env global_env)
          ~type_name:self_typ ~method_name ~method_info
    | _ -> ());
    match method_impl with
    | None -> Some (`Method_missing method_name)
    | Some { id = Qext_method { self_typ; _ }; _ }
      when Type_path.equal self_typ Type_path.Builtin.default_impl_placeholder
      ->
        None
    | Some { pub = false; _ } when request.is_pub ->
        Some (`Private_method method_name)
    | Some { id = Qext_method _; pub; loc; _ } ->
        (match trait_vis with
        | Vis_fully_pub when not pub -> (
            match
              Global_env.find_regular_method global_env ~type_name:self_typ
                ~method_name
            with
            | Some { pub = true; loc = prev_loc; _ } ->
                add_global_error diagnostics
                  (Errors.priv_ext_shadows_pub_method ~method_name ~trait
                     ~type_name:self_typ ~prev_loc ~loc)
            | _ -> ())
        | _ -> ());
        None
    | Some method_info ->
        let expected =
          Poly_type.instantiate_method_decl method_decl ~self:request.self_ty
        in
        let cenv = Poly_type.make () in
        let actual, ty_args = Poly_type.instantiate_method ~cenv method_info in
        if Ctype.try_unify expected actual then (
          if
            Arr.exists ty_args (fun ty ->
                match Stype.type_repr ty with Tvar _ -> true | _ -> false)
          then Some (`Method_type_params_unsolved method_name)
          else
            let aux_diagnostics =
              Local_diagnostics.make ~base:Loc.no_location
            in
            Type_constraint.solve_constraints cenv ~tvar_env:request.ty_params
              ~global_env ~diagnostics:aux_diagnostics;
            if Local_diagnostics.has_fatal_errors aux_diagnostics then
              Some (`Method_constraint_not_satisfied method_name)
            else (
              Diagnostics.add_warning diagnostics
                {
                  kind =
                    Implement_trait_with_method
                      {
                        trait = Type_path_util.name trait;
                        typ = Printer.type_to_string request.self_ty;
                        methods = [ method_name ];
                      };
                  loc = request.loc_;
                };
              None))
        else
          Some
            (`Type_mismatch
               ( method_name,
                 Printer.type_to_string expected,
                 Printer.type_to_string method_info.typ ))
      [@@inline]
  in
  Trait_impl.iter (Global_env.get_trait_impls global_env) (fun ~trait ->
      fun ~type_name ->
       fun request ->
        match Global_env.find_trait_by_path global_env trait with
        | None -> ()
        | Some trait_decl -> (
            let failure_reasons =
              Lst.fold_right trait_decl.methods [] (fun meth_decl ->
                  fun acc ->
                   match
                     check_method ~trait ~trait_vis:trait_decl.vis_
                       ~self_typ:type_name ~request meth_decl
                   with
                   | Some err -> err :: acc
                   | None -> acc)
            in
            match failure_reasons with
            | [] -> ()
            | failure_reasons ->
                add_global_error diagnostics
                  (trait_not_implemented ~trait ~type_name ~failure_reasons
                     ~loc:request.loc_)))

let check_toplevel ?(pkgs : Pkg.pkg_tbl option) ~diagnostics
    ?(build_context : Typeutil.build_context = SingleFile) list_of_impls =
  (let pkgs = match pkgs with Some pkgs -> pkgs | None -> Pkg.create_tbl () in
   let impls = Lst.concat list_of_impls in
   let builtin_types = Builtin.builtin_types in
   let toplevel_types = Typing_info.make_types () in
   let type_alias = Hash_string.create 17 in
   let types =
     Global_env.All_types.make ~toplevel:toplevel_types ~builtin:builtin_types
       ~type_alias ~pkgs
   in
   let toplevel_values = Typing_info.make_values () in
   Pkg.load_direct_uses pkgs toplevel_values type_alias ~impls ~diagnostics;
   let method_env = Method_env.empty () in
   let ext_method_env = Ext_method_env.empty () in
   let trait_impls = Trait_impl.make () in
   let worklist = Vec.empty () in
   let constant_worklist = Vec.empty () in
   let type_decls, trait_decls, trait_alias =
     typing_types_and_traits impls types toplevel_values ~ext_method_env
       ~trait_impls ~value_worklist:worklist ~diagnostics
   in
   check_toplevel_decl impls types ~method_env ~toplevel:toplevel_values
     ~ext_method_env ~trait_impls ~worklist ~constant_worklist ~diagnostics
     ~build_context;
   let builtin_values = Builtin.builtin_values in
   let global_env =
     Global_env.make ~types ~builtin:builtin_values ~toplevel:toplevel_values
       ~method_env ~ext_method_env ~trait_impls
   in
   let const_decls =
     Const_util.typing_const_decls constant_worklist ~global_env ~diagnostics
   in
   check_traits_implemented ~global_env ~diagnostics;
   load_trait_object_methods ~global_env;
   {
     global_env;
     values = worklist;
     const_decls;
     type_decls;
     trait_decls;
     trait_alias;
   }
    : Local_typing_worklist.t)
