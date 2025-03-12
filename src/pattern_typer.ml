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
module I = Basic_ident
module Constr_info = Basic_constr_info
module Syntax = Parsing_syntax
module Operators = Parsing_operators
module Type_path = Basic_type_path

let add_error = Typeutil.add_local_typing_error
let store_error = Typeutil.store_error
let take_info_partial = Typeutil.take_info_partial

let add_pat_binder (b : Typedtree.pat_binder) (binders : Typedtree.pat_binders)
    =
  (if
     Lst.exists binders (fun a ->
         I.same_local_name a.binder.binder_id b.binder.binder_id)
   then
     let error =
       Errors.non_linear_pattern
         ~name:(I.base_name b.binder.binder_id)
         ~loc:b.binder.loc_
     in
     Partial (binders, [ error ])
   else Ok (b :: binders)
    : _ Local_diagnostics.partial_info)

let combine_pat_binders ~(new_binders : Typedtree.pat_binders)
    ~(binders : Typedtree.pat_binders) =
  (let duplicated = ref [] in
   let add_binder (acc : Typedtree.pat_binders) (b : Typedtree.pat_binder) =
     if
       Lst.exists acc (fun a ->
           I.same_local_name a.binder.binder_id b.binder.binder_id)
     then (
       duplicated := b :: !duplicated;
       acc)
     else b :: acc
   in
   let res = List.fold_left add_binder new_binders binders in
   match !duplicated with
   | [] -> Ok res
   | dups ->
       let errors =
         Lst.map dups (fun b ->
             Errors.non_linear_pattern
               ~name:(I.base_name b.binder.binder_id)
               ~loc:b.binder.loc_)
       in
       Partial (res, errors)
    : _ Local_diagnostics.partial_info)

let merge_pat_binders (binders1 : Typedtree.pat_binders)
    (binders2 : Typedtree.pat_binders) =
  (let binder_compare (b1 : Typedtree.pat_binder) (b2 : Typedtree.pat_binder) =
     String.compare
       (I.base_name b1.binder.binder_id)
       (I.base_name b2.binder.binder_id)
   in
   let sorted_binders1 = Lst.stable_sort binders1 binder_compare in
   let sorted_binders2 = Lst.stable_sort binders2 binder_compare in
   let errors = ref [] in
   let rec go (bs1 : Typedtree.pat_binders) bs2 =
     (match (bs1, bs2) with
      | [], [] -> []
      | [], b :: _ | b :: _, [] ->
          let name = I.base_name b.binder.binder_id in
          errors :=
            Errors.inconsistent_or_pattern ~name ~loc:b.binder.loc_ :: !errors;
          bs1
      | b1 :: rest1, b2 :: rest2 ->
          let name1 = I.base_name b1.binder.binder_id in
          let name2 = I.base_name b2.binder.binder_id in
          if name1 = name2 then
            let ty1, tag1 = Type.deref_constr_type b1.binder_typ in
            let ty2, tag2 = Type.deref_constr_type b2.binder_typ in
            match
              Ctype.unify_pat ~expect_ty:ty1 ~actual_ty:ty2 b2.binder.loc_
            with
            | Some err ->
                errors := err :: !errors;
                { b1 with binder_typ = ty1 } :: go rest1 rest2
            | None ->
                (match (tag1, tag2) with
                | Some tag1, Some tag2 when Constr_info.equal tag1 tag2 -> b1
                | Some _, _ -> { b1 with binder_typ = ty1 }
                | None, _ -> b1)
                :: go rest1 rest2
          else if name1 < name2 then (
            errors :=
              Errors.inconsistent_or_pattern ~name:name1 ~loc:b1.binder.loc_
              :: !errors;
            bs1)
          else (
            errors :=
              Errors.inconsistent_or_pattern ~name:name2 ~loc:b2.binder.loc_
              :: !errors;
            bs1)
       : Typedtree.pat_binders)
   in
   let binders = go sorted_binders1 sorted_binders2 in
   match !errors with [] -> Ok binders | errs -> Partial (binders, errs)
    : Typedtree.pat_binders Local_diagnostics.partial_info)

let get_view_type (env : Global_env.t) (ty : Stype.t) =
  (let find_by_path p =
     match
       Global_env.find_dot_method env ~type_name:p ~method_name:"op_as_view"
     with
     | method_ :: [] -> (
         let func_ty, _, _ =
           Poly_type.instantiate_method_no_constraint method_
         in
         match func_ty with
         | Tarrow { params_ty = ty_self :: _; ret_ty; err_ty = _; is_async = _ }
           ->
             Ctype.unify_exn ty_self ty;
             Some ret_ty
         | _ -> None)
     | _ -> None
       [@@inline]
   in
   match Stype.type_repr ty with
   | T_constr { type_constructor = p; _ } -> find_by_path p
   | T_builtin T_string -> find_by_path Basic_type_path.Builtin.type_path_string
   | _ -> None
    : Stype.t option)

let get_key_value_type ~global_env ~cenv ~tvar_env (ty : Stype.t) ~loc =
  (match
     Type_constraint.resolve_method_by_type ty Operators.op_get_info.method_name
       ~loc ~src:Dot_src_direct ~tvar_env ~global_env
   with
   | Error _ ->
       Error
         (Errors.cannot_use_map_pattern_no_method
            ~ty:(Printer.type_to_string ty)
            ~loc)
   | Ok method_info -> (
       let check_method_ty method_id method_ty method_arity ~ty_args =
         (let ty_key = Stype.new_type_var Tvar_normal in
          let ty_value = Builtin.type_option (Stype.new_type_var Tvar_normal) in
          if
            Ctype.try_unify method_ty
              (Builtin.type_arrow [ ty; ty_key ] ty_value ~err_ty:None
                 ~is_async:false)
            && Fn_arity.equal method_arity (Fn_arity.simple 2)
          then Ok (ty_key, ty_value, (method_id, method_ty, ty_args))
          else
            Error
              (Errors.cannot_use_map_pattern_method_type_mismatch
                 ~ty:(Printer.type_to_string ty)
                 ~actual_ty:
                   (Printer.toplevel_function_type_to_string ~arity:method_arity
                      method_ty)
                 ~loc)
           : _ Local_diagnostics.info)
           [@@local]
       in
       match method_info with
       | Promised_method { method_ty; method_arity; method_id; prim = _ } ->
           check_method_ty method_id method_ty method_arity ~ty_args:[||]
       | Known_method mi ->
           let method_ty, ty_args =
             Poly_type.instantiate_method ~cenv ~loc mi
           in
           check_method_ty (I.of_qual_ident mi.id) method_ty mi.arity_ ~ty_args)
    : (Stype.t * Stype.t * (I.t * Stype.t * Stype.t array))
      Local_diagnostics.info)

let fields_of_struct ~global_env p =
  match Global_env.find_type_by_path global_env p with
  | None -> None
  | Some info -> (
      match info.ty_desc with
      | Record_type { fields; _ } ->
          Some (Lst.map fields (fun f -> f.field_name))
      | _ -> None)

let rec type_guided_record_pat_check (fields : Syntax.field_pat list)
    (labels : Typedecl_info.field list) is_closed record_ty ~tvar_env ~cenv
    ~global_env ~diagnostics ~loc =
  (let _ =
     Typeutil.validate_record ~context:`Pattern ~expected:labels
       (Lst.map fields (fun (Field_pat { label; _ }) -> label))
       ~record_ty ~is_strict:is_closed ~loc ~diagnostics
   in
   let binders = ref [] in
   let check_field_pat (Field_pat { label; pattern; is_pun } : Syntax.field_pat)
       =
     (let ty, pos =
        match
          Lst.find_first labels (fun { field_name; _ } ->
              field_name = label.label_name)
        with
        | None -> (Stype.new_type_var Tvar_error, Typeutil.unknown_pos)
        | Some field_info -> (field_info.ty_field, field_info.pos)
      in
      let new_binders, pat =
        check_pat pattern ty ~tvar_env ~cenv ~global_env ~diagnostics
      in
      binders :=
        take_info_partial ~diagnostics
          (combine_pat_binders ~new_binders ~binders:!binders);
      Field_pat { label; pat; is_pun; pos }
       : Typedtree.field_pat)
       [@@inline]
   in
   let fields = Lst.map fields check_field_pat in
   (!binders, Tpat_record { fields; ty = record_ty; loc_ = loc; is_closed })
    : Typedtree.pat_binders * Typedtree.pat)

and infer_record_pat (fields : Syntax.field_pat list) (ty : Stype.t) is_closed
    ~tvar_env ~cenv ~global_env ~diagnostics ~loc =
  (let handle_error err =
     (add_error diagnostics err;
      let go (pat_binders_acc, pat_acc)
          (Field_pat { label; pattern; is_pun } : Syntax.field_pat) =
        let ty = Stype.new_type_var Tvar_error in
        let pat_binders, pat =
          check_pat pattern ty ~tvar_env ~cenv ~global_env ~diagnostics
        in
        ( pat_binders @ pat_binders_acc,
          Typedtree.Field_pat { label; pat; is_pun; pos = Typeutil.unknown_pos }
          :: pat_acc )
      in
      let pat_binders, fields = List.fold_left go ([], []) fields in
      ( pat_binders,
        Tpat_record { fields = List.rev fields; ty; loc_ = loc; is_closed } )
       : Typedtree.pat_binders * Typedtree.pat)
   in
   if fields = [] then handle_error (Errors.record_type_missing loc)
   else
     let labels = Lst.map fields (fun (Field_pat { label; _ }) -> label) in
     match
       Basic_duplicate_check.check_duplicate_by labels (fun x -> x.label_name)
     with
     | Some { label_name = label; loc_ = loc } ->
         handle_error
           (Errors.duplicate_record_field ~context:`Pattern ~label ~loc)
     | None -> (
         match Global_env.resolve_record global_env ~labels ~loc with
         | Error err -> handle_error err
         | Ok (ty_params, ty_record, labels) ->
             let ty, labels =
               Poly_type.instantiate_record
                 ~ty_record:(`Generic (ty_params, ty_record))
                 labels
             in
             type_guided_record_pat_check fields labels is_closed ty ~tvar_env
               ~cenv ~global_env ~diagnostics ~loc)
    : Typedtree.pat_binders * Typedtree.pat)

and check_constr_pat (constr : Syntax.constructor)
    (args : Syntax.constr_pat_arg list option)
    (constr_desc : Typedecl_info.constructor) (expect_ty : Stype.t)
    ~(is_open : bool) ~(type_name : Typedtree.type_name option) ~tvar_env ~cenv
    ~global_env ~diagnostics ~loc =
  (let name = constr.constr_name.name in
   let cs_tag = constr_desc.cs_tag in
   let ty_res, ty_args = Poly_type.instantiate_constr constr_desc in
   (if Typeutil.is_tvar (Stype.type_repr expect_ty) then
      if Type.is_suberror ty_res then Ctype.unify_exn expect_ty Stype.error
      else Ctype.unify_exn expect_ty ty_res
    else
      try Ctype.unify_exn expect_ty ty_res
      with _ ->
        if not (Type.is_super_error expect_ty && Type.is_suberror ty_res) then
          let expected = Printer.type_to_string expect_ty in
          let actual = Printer.type_to_string ty_res in
          add_error diagnostics
            (Errors.constr_unify ~name ~expected ~actual ~loc));
   let is_super_error = Type.is_super_error expect_ty in
   let do_check args =
     let arity = constr_desc.cs_arity_ in
     let typ_of_args =
       let pos = ref (-1) in
       Fn_arity.to_hashtbl arity ty_args (fun _ ->
           fun ty ->
            incr pos;
            (!pos, ty))
     in
     let seen_labels = Basic_hash_string.create 17 in
     let last_positional_index = ref (-1) in
     let lookup_positional_arg () =
       incr last_positional_index;
       match
         Fn_arity.Hash.find_opt typ_of_args (Positional !last_positional_index)
       with
       | Some info -> info
       | None -> (Typeutil.unknown_pos, Stype.new_type_var Tvar_error)
     in
     let lookup_labelled_arg (label : Syntax.label) =
       (match Basic_hash_string.find_opt seen_labels label.label_name with
       | Some _first_loc ->
           add_error diagnostics
             (Errors.duplicated_fn_label ~label:label.label_name
                ~second_loc:label.loc_)
       | None -> Basic_hash_string.add seen_labels label.label_name label.loc_);
       match Fn_arity.Hash.find_opt typ_of_args (Labelled label.label_name) with
       | Some typ -> typ
       | None ->
           add_error diagnostics
             (Errors.superfluous_arg_label ~label:label.label_name
                ~kind:"constructor" ~loc:label.loc_);
           (Typeutil.unknown_pos, Stype.new_type_var Tvar_error)
     in
     let rec check_args ~pat_binders ~targs_rev
         (args : Syntax.constr_pat_arg list) =
       match args with
       | [] ->
           ( pat_binders,
             Typedtree.Tpat_constr
               {
                 type_name;
                 constr = constr.constr_name;
                 args = List.rev targs_rev;
                 tag = cs_tag;
                 ty = ty_res;
                 used_error_subtyping = is_super_error;
                 arity_ = constr_desc.cs_arity_;
                 all_args_ = ty_args;
                 loc_ = loc;
               } )
       | Constr_pat_arg { pat; kind } :: args ->
           let pos, ty =
             match kind with
             | Positional -> lookup_positional_arg ()
             | Labelled label
             | Labelled_pun label
             | Labelled_option { label; _ }
             | Labelled_option_pun { label; _ } ->
                 lookup_labelled_arg label
           in
           let new_binders, tpat =
             check_pat pat ty ~tvar_env ~cenv ~global_env ~diagnostics
           in
           let pat_binders =
             take_info_partial ~diagnostics
               (combine_pat_binders ~binders:pat_binders ~new_binders)
           in
           let targ : Typedtree.constr_pat_arg =
             Constr_pat_arg { pat = tpat; kind; pos }
           in
           check_args ~pat_binders ~targs_rev:(targ :: targs_rev) args
     in
     let result = check_args ~pat_binders:[] ~targs_rev:[] args in
     let () =
       let actual = !last_positional_index + 1 in
       let expected = Fn_arity.count_positional arity in
       if actual <> expected then
         add_error diagnostics
           (Errors.constr_arity_mismatch ~name:constr.constr_name.name ~expected
              ~actual
              ~has_label:
                ((not (Fn_arity.is_simple arity))
                || Lst.exists args (fun (Constr_pat_arg { pat = _; kind }) ->
                       match kind with
                       | Positional -> false
                       | Labelled_pun _ | Labelled _ | Labelled_option _
                       | Labelled_option_pun _ ->
                           true))
              ~loc)
     in
     if not is_open then (
       let missing = Vec.empty () in
       Fn_arity.iter arity (fun param_kind ->
           match param_kind with
           | Positional _ | Optional _ | Autofill _ | Question_optional _ -> ()
           | Labelled { label; _ } ->
               if not (Basic_hash_string.mem seen_labels label) then
                 Vec.push missing label);
       if not (Vec.is_empty missing) then
         Local_diagnostics.add_warning diagnostics
           {
             kind =
               Omitted_constr_argument
                 {
                   labels = Vec.to_list missing;
                   constr = constr.constr_name.name;
                 };
             loc;
           });
     result
       [@@local]
   in
   match args with
   | Some (Constr_pat_arg { pat = Ppat_any _; kind = Positional } :: []) ->
       ( [],
         Tpat_constr
           {
             type_name;
             constr = constr.constr_name;
             args = [];
             tag = cs_tag;
             ty = ty_res;
             used_error_subtyping = is_super_error;
             arity_ = constr_desc.cs_arity_;
             all_args_ = ty_args;
             loc_ = loc;
           } )
   | None -> do_check []
   | Some args -> do_check args
    : Typedtree.pat_binders * Typedtree.pat)

and check_pat (pat : Syntax.pattern) (ty : Stype.t) ~(tvar_env : Tvar_env.t)
    ~(cenv : Poly_type.t) ~(global_env : Global_env.t) ~diagnostics =
  (let make_json_pat constr args ~loc =
     Json_literal.make_json_pat ~global_env ~diagnostics constr args ~loc
       [@@inline]
   in
   match pat with
   | Ppat_alias { pat = p; alias = a; loc_ } ->
       let alias = Typeutil.fresh_binder a in
       let binders, tp =
         check_pat p ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let binder_typ : Stype.t =
         match tp with
         | Tpat_constr { tag; ty = pat_ty; _ }
           when not (Global_env.is_newtype global_env pat_ty) ->
             Type.make_constr_type pat_ty ~tag
         | _ -> ty
       in
       let new_binder : Typedtree.pat_binder = { binder = alias; binder_typ } in
       let binders =
         take_info_partial ~diagnostics (add_pat_binder new_binder binders)
       in
       (binders, Tpat_alias { pat = tp; alias; ty; loc_ })
   | Ppat_any { loc_ } -> ([], Tpat_any { ty; loc_ })
   | Ppat_array { pats; loc_; _ } ->
       let is_json, ty_array =
         if Type.same_type ty Stype.json then
           (true, Builtin.type_array Stype.json)
         else (false, ty)
       in
       let ty_elem =
         take_info_partial ~diagnostics
           (Type.filter_array_like_pattern ty_array loc_)
       in
       let go (binders, pat_acc) (pat : Syntax.array_pattern) =
         let break_string_literal str name_ =
           let v = Basic_utf8.from_string str in
           Basic_vec_int.map_into_list
             ~unorder:(fun i ->
               Typedtree.Tpat_constant
                 { c = C_char (Uchar.of_int i); ty = Stype.char; name_; loc_ })
             v
         in
         match pat with
         | Pattern pat ->
             let new_binders, tp =
               check_pat pat ty_elem ~tvar_env ~cenv ~global_env ~diagnostics
             in
             let pat_binders =
               take_info_partial ~diagnostics
                 (combine_pat_binders ~new_binders ~binders)
             in
             (pat_binders, tp :: pat_acc)
         | String_spread str ->
             store_error ~diagnostics
               (Ctype.unify_pat ~expect_ty:Stype.char ~actual_ty:ty_elem loc_);
             let ps = break_string_literal str.string_val None in
             (binders, List.rev_append ps pat_acc)
         | String_spread_const { binder; pkg; loc_ } -> (
             let name_ : Typedtree.var option =
               match pkg with
               | None ->
                   Some
                     {
                       var_id =
                         I.of_qual_ident
                           (Basic_qual_ident.make
                              ~pkg:!Basic_config.current_package
                              ~name:binder.binder_name);
                       loc_;
                     }
               | Some _ -> None
             in
             match
               Typeutil.resolve_constant ~global_env ~pkg ~constant_name:binder
                 ~loc:loc_ ~diagnostics
             with
             | Some c -> (
                 match c.kind with
                 | Const (C_string str) ->
                     let ps = break_string_literal str name_ in
                     (binders, List.rev_append ps pat_acc)
                 | Const const ->
                     Local_diagnostics.add_error diagnostics
                       (Errors.type_mismatch ~expected:"String"
                          ~actual:(Printer.type_to_string c.typ)
                          ~loc:loc_);
                     ( binders,
                       Tpat_constant { c = const; ty = c.typ; name_; loc_ }
                       :: pat_acc )
                 | Normal | Prim _ -> assert false)
             | None -> (binders, pat_acc))
       in
       let pat_binders, pats =
         match pats with
         | Closed pats ->
             let pat_binders, pats = Lst.fold_left pats ([], []) go in
             (pat_binders, Typedtree.Closed (List.rev pats))
         | Open (pats1, pats2, dotdot_binder) ->
             let pat_binders, pats1 = Lst.fold_left pats1 ([], []) go in
             let pat_binders, pats2 =
               Lst.fold_left pats2 (pat_binders, []) go
             in
             let pat_binders, dotdot_binder =
               match dotdot_binder with
               | Some binder ->
                   let binder = Typeutil.fresh_binder binder in
                   let ty_view =
                     match get_view_type global_env ty_array with
                     | Some ty_view -> ty_view
                     | None ->
                         let error =
                           Errors.no_op_as_view
                             ~ty:(Printer.type_to_string ty_array)
                             ~loc:(Syntax.loc_of_pattern pat)
                         in
                         add_error diagnostics error;
                         Stype.new_type_var Tvar_error
                   in
                   let new_binder : Typedtree.pat_binder =
                     { binder; binder_typ = ty_view }
                   in
                   let pat_binders =
                     take_info_partial ~diagnostics
                       (add_pat_binder new_binder pat_binders)
                   in
                   (pat_binders, Some (binder, ty_view))
               | None -> (pat_binders, None)
             in
             (pat_binders, Open (List.rev pats1, List.rev pats2, dotdot_binder))
       in
       let array_pat : Typedtree.pat =
         Tpat_array { pats; ty = ty_array; loc_ }
       in
       let result : Typedtree.pat =
         if is_json then
           make_json_pat Json_literal.array [ array_pat ] ~loc:loc_
         else array_pat
       in
       (pat_binders, result)
   | Ppat_constant { c = Const_bool b; loc_ } when Type.same_type ty Stype.json
     ->
       ( [],
         make_json_pat
           (if b then Json_literal.true_ else Json_literal.false_)
           [] ~loc:loc_ )
   | Ppat_constant { c = Const_string lit; loc_ }
     when Type.same_type ty Stype.json ->
       let const_pat : Typedtree.pat =
         Tpat_constant
           {
             c = C_string lit.string_val;
             ty = Stype.string;
             name_ = None;
             loc_;
           }
       in
       ([], make_json_pat Json_literal.string [ const_pat ] ~loc:loc_)
   | Ppat_constant { c = Const_int rep | Const_double rep; loc_ }
     when Type.same_type ty Stype.json ->
       let const_pat : Typedtree.pat =
         Tpat_constant
           {
             c =
               snd
                 (Typeutil.typing_constant (Const_double rep) ~expect_ty:None
                    ~loc:loc_ ~diagnostics);
             ty = Stype.double;
             name_ = None;
             loc_;
           }
       in
       ([], make_json_pat Json_literal.number [ const_pat ] ~loc:loc_)
   | Ppat_constant { c; loc_ } ->
       let actual_ty, c =
         Typeutil.typing_constant ~expect_ty:(Some ty) c ~loc:loc_ ~diagnostics
       in
       store_error ~diagnostics (Ctype.unify_pat ~expect_ty:ty ~actual_ty loc_);
       ([], Tpat_constant { c; ty; name_ = None; loc_ })
   | Ppat_constr { constr; args; is_open; loc_ } -> (
       let handle_error type_name =
         let tag = Typeutil.unknown_tag in
         let rec infer_args ~type_name ~pat_binders ~targs_rev args =
           match args with
           | [] ->
               let args = List.rev targs_rev in
               ( pat_binders,
                 Typedtree.Tpat_constr
                   {
                     type_name;
                     constr = constr.constr_name;
                     args;
                     tag;
                     ty;
                     used_error_subtyping = false;
                     arity_ = Fn_arity.simple (List.length args);
                     all_args_ =
                       Lst.map args (fun (Constr_pat_arg { pat; _ }) ->
                           Typedtree_util.type_of_pat pat);
                     loc_;
                   } )
           | Syntax.Constr_pat_arg { pat; kind } :: args ->
               let new_binders, tpat =
                 check_pat pat
                   (Stype.new_type_var Tvar_error)
                   ~tvar_env ~cenv ~global_env ~diagnostics
               in
               let targ : Typedtree.constr_pat_arg =
                 Constr_pat_arg { pat = tpat; kind; pos = Typeutil.unknown_pos }
               in
               let pat_binders =
                 take_info_partial ~diagnostics
                   (combine_pat_binders ~binders:pat_binders ~new_binders)
               in
               infer_args ~type_name ~pat_binders ~targs_rev:(targ :: targs_rev)
                 args
         in
         let args = match args with None -> [] | Some args -> args in
         infer_args ~type_name ~pat_binders:[] ~targs_rev:[] args
           [@@local]
       in
       match
         Typeutil.resolve_constr_or_constant ~global_env ~tvar_env
           ~expect_ty:(Some ty) ~constr ~creating_value:false ~diagnostics
       with
       | Some (`Constr constr_desc), type_name ->
           check_constr_pat constr args constr_desc ty ~is_open ~type_name
             ~tvar_env ~cenv ~global_env ~diagnostics ~loc:loc_
       | Some (`Constant _), type_name
         when match args with Some _ -> true | _ -> false ->
           (match Stype.type_repr ty with
           | Tvar { contents = Tnolink Tvar_error } -> ()
           | _ ->
               add_error diagnostics
                 (Errors.constant_pat_with_args ~name:constr.constr_name.name
                    ~loc:constr.loc_));
           handle_error type_name
       | Some (`Constant { id; kind; typ; _ }), _ -> (
           store_error ~diagnostics
             (Ctype.unify_pat ~expect_ty:ty ~actual_ty:typ loc_);
           match kind with
           | Const c ->
               ( [],
                 Tpat_constant
                   {
                     c;
                     ty;
                     name_ = Some { var_id = I.of_qual_ident id; loc_ };
                     loc_;
                   } )
           | Normal | Prim _ -> ([], Tpat_any { ty; loc_ }))
       | None, type_name -> handle_error type_name)
   | Ppat_constraint { pat = p; ty = te; loc_ } ->
       let ty' =
         Typeutil.typing_type ~allow_private:true te ~tvar_env
           ~allow_partial:true
           ~types:(Global_env.get_all_types global_env)
           ~local_type_env:(Global_env.get_cur_local_type_env global_env)
           ~diagnostics
       in
       let stype' = Typedtree_util.stype_of_typ ty' in
       let pat_binders, tp =
         check_pat p stype' ~tvar_env ~cenv ~global_env ~diagnostics
       in
       store_error ~diagnostics
         (Ctype.unify_pat ~expect_ty:ty ~actual_ty:stype' loc_);
       ( pat_binders,
         Tpat_constraint { pat = tp; konstraint = ty'; ty = stype'; loc_ } )
   | Ppat_or { pat1; pat2; loc_ } ->
       let pat_binders1, tp1 =
         check_pat pat1 ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let pat_binders2, tp2 =
         check_pat pat2 ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let pat_binders =
         take_info_partial ~diagnostics
           (merge_pat_binders pat_binders1 pat_binders2)
       in
       ( pat_binders,
         Tpat_or
           { pat1 = tp1; pat2 = Or_pat.rename_pat tp2 pat_binders1; ty; loc_ }
       )
   | Ppat_tuple { pats; loc_ } ->
       let arity = List.length pats in
       let go (binders, pat_acc) pat pat_ty =
         let new_binders, tp =
           check_pat pat pat_ty ~tvar_env ~cenv ~global_env ~diagnostics
         in
         let pat_binders =
           take_info_partial ~diagnostics
             (combine_pat_binders ~new_binders ~binders)
         in
         (pat_binders, tp :: pat_acc)
       in
       let ty_tuple =
         take_info_partial ~diagnostics
           (Type.filter_product ~blame:Filter_itself ~arity:(Some arity) ty loc_)
       in
       let pat_binders, tps = List.fold_left2 go ([], []) pats ty_tuple in
       (pat_binders, Tpat_tuple { pats = List.rev tps; ty; loc_ })
   | Ppat_var v ->
       let binder = Typeutil.fresh_binder v in
       let new_binder : Typedtree.pat_binder = { binder; binder_typ = ty } in
       ([ new_binder ], Tpat_var { binder; ty; loc_ = binder.loc_ })
   | Ppat_record { fields = []; is_closed; loc_ } -> (
       let default () =
         ((match
             get_key_value_type ~global_env ~cenv ~tvar_env ty ~loc:loc_
           with
          | Error e -> add_error diagnostics e
          | Ok _ ->
              if is_closed then
                Local_diagnostics.add_warning diagnostics
                  { kind = Closed_map_pattern; loc = loc_ });
          ([], Tpat_any { ty; loc_ })
           : Typedtree.pat_binders * Typedtree.pat)
       in
       match Stype.type_repr ty with
       | T_constr { type_constructor; tys = []; _ }
         when Type_path.equal type_constructor Type_path.Builtin.type_path_json
         ->
           ([], make_json_pat Json_literal.object_ [] ~loc:loc_)
       | T_constr { type_constructor; _ } -> (
           match fields_of_struct ~global_env type_constructor with
           | None -> default ()
           | Some [] ->
               if not is_closed then
                 add_error diagnostics (Errors.record_pattern_only_dotdot loc_);
               ([], Tpat_record { fields = []; ty; loc_; is_closed = true })
           | Some labels ->
               if is_closed then
                 Local_diagnostics.add_error diagnostics
                   (Errors.missing_fields_in_record ~labels
                      ~ty:(Printer.type_to_string ty)
                      ~context:`Pattern ~loc:loc_)
               else
                 add_error diagnostics (Errors.record_pattern_only_dotdot loc_);
               ([], Tpat_record { fields = []; ty; loc_; is_closed = true }))
       | _ -> default ())
   | Ppat_record { fields; loc_; is_closed } -> (
       if Typeutil.is_tvar ty then
         infer_record_pat fields ty is_closed ~tvar_env ~cenv ~global_env
           ~diagnostics ~loc:loc_
       else
         match
           Global_env.labels_of_record global_env ty ~loc:loc_ ~context:`Pattern
         with
         | Ok labels ->
             let _, labels =
               Poly_type.instantiate_record ~ty_record:(`Known ty) labels
             in
             type_guided_record_pat_check fields labels is_closed ty ~tvar_env
               ~cenv ~global_env ~diagnostics ~loc:loc_
         | Error error ->
             let ty = Stype.new_type_var Tvar_error in
             add_error diagnostics error;
             let pat_binders, fields_rev =
               Lst.fold_left fields ([], [])
                 (fun (pat_binders_acc, fields_rev) ->
                   fun (Field_pat { label; pattern; is_pun }) ->
                    let ty = Stype.new_type_var Tvar_error in
                    let pat_binders, pat =
                      check_pat pattern ty ~tvar_env ~cenv ~global_env
                        ~diagnostics
                    in
                    ( pat_binders @ pat_binders_acc,
                      Typedtree.Field_pat
                        { label; pat; is_pun; pos = Typeutil.unknown_pos }
                      :: fields_rev ))
             in
             ( pat_binders,
               Tpat_record { fields = List.rev fields_rev; ty; loc_; is_closed }
             ))
   | Ppat_map { elems; is_closed; loc_ } ->
       if is_closed then
         Local_diagnostics.add_warning diagnostics
           { kind = Closed_map_pattern; loc = loc_ };
       let is_json, ty_map =
         if Type.same_type ty Stype.json then
           (true, Builtin.type_map Stype.string Stype.json)
         else (false, ty)
       in
       let ty_key, ty_value, op_get_info_ =
         match
           get_key_value_type ~global_env ~cenv ~tvar_env ty_map ~loc:loc_
         with
         | Ok result -> result
         | Error err ->
             add_error diagnostics err;
             ( Stype.new_type_var Tvar_error,
               Builtin.type_option (Stype.new_type_var Tvar_error),
               (I.fresh "op_get", Stype.new_type_var Tvar_error, [||]) )
       in
       let rec check_elems pat_binders (elems : Syntax.map_pat_elem list) =
         match elems with
         | [] -> (pat_binders, [])
         | Map_pat_elem { key; pat; match_absent; key_loc_; loc_ } :: elems ->
             let pat : Syntax.pattern =
               if match_absent then pat
               else
                 Ppat_constr
                   {
                     constr =
                       {
                         constr_name =
                           { name = "Some"; loc_ = Rloc.no_location };
                         extra_info = No_extra_info;
                         loc_ = Rloc.no_location;
                       };
                     args = Some [ Constr_pat_arg { pat; kind = Positional } ];
                     is_open = false;
                     loc_;
                   }
             in
             let actual_ty_key, key =
               Typeutil.typing_constant ~expect_ty:(Some ty_key) ~loc:key_loc_
                 key ~diagnostics
             in
             store_error ~diagnostics
               (Ctype.unify_pat ~expect_ty:ty_key ~actual_ty:actual_ty_key
                  key_loc_);
             let new_binders, tpat =
               check_pat pat ty_value ~tvar_env ~cenv ~global_env ~diagnostics
             in
             let pat_binders =
               take_info_partial ~diagnostics
                 (combine_pat_binders ~new_binders ~binders:pat_binders)
             in
             let pat_binders, elems = check_elems pat_binders elems in
             (pat_binders, (key, tpat) :: elems)
       in
       let pat_binders, elems = check_elems [] elems in
       let result : Typedtree.pat =
         let map_pat : Typedtree.pat =
           Tpat_map { elems; ty = ty_map; op_get_info_; loc_ }
         in
         if is_json then
           make_json_pat Json_literal.object_ [ map_pat ] ~loc:loc_
         else map_pat
       in
       (pat_binders, result)
   | Ppat_range { lhs; rhs; inclusive; loc_ } ->
       let _, tlhs =
         check_pat lhs ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let _, trhs =
         check_pat rhs ty ~tvar_env ~cenv ~global_env ~diagnostics
       in
       let invalid_type () =
         add_error diagnostics
           (Errors.range_pattern_unsupported_type
              ~ty:(Printer.type_to_string ty)
              ~loc:loc_)
           [@@local]
       in
       (match Stype.type_repr ty with
       | T_builtin b -> (
           match b with
           | T_byte | T_int16 | T_uint16 | T_char | T_int | T_int64 | T_uint
           | T_uint64 ->
               ()
           | T_float | T_double | T_unit | T_bool | T_bytes | T_string ->
               invalid_type ())
       | _ -> invalid_type ());
       (match (tlhs, trhs) with
       | Tpat_constant { c = c1; _ }, Tpat_constant { c = c2; _ } -> (
           match Constant.eval_compare c1 c2 with
           | Some compare_result ->
               if compare_result > 0 || (compare_result = 0 && not inclusive)
               then
                 add_error diagnostics
                   (Errors.range_pattern_invalid_range ~inclusive ~loc:loc_)
           | None -> ())
       | _ -> ());
       ([], Tpat_range { lhs = tlhs; rhs = trhs; inclusive; ty; loc_ })
    : Typedtree.pat_binders * Typedtree.pat)
