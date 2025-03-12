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


open Basic_unsafe_external
module Lst = Basic_lst
module Map_string = Basic_map_string
module Syntax = Parsing_syntax
module Parser_util = Parsing_util
module Syntax_util = Parsing_syntax_util
module Longident = Basic_longident

let add_error = Local_diagnostics.add_error
let add_global_error = Diagnostics.add_error
let add_warning = Local_diagnostics.add_warning

let warn_unused_tvars diagnostics ~is_error map =
  Map_string.iter map (fun name ->
      fun loc ->
       if is_error then
         Local_diagnostics.add_error diagnostics (Errors.unused_tvar ~name ~loc)
       else
         Local_diagnostics.add_warning diagnostics
           { loc; kind = Warnings.Unused_tvar name })

type ctx = {
  type_vis : Syntax.visibility;
  tvars : string list;
  diagnostics : Local_diagnostics.t;
  base : Loc.t;
}

let tvar_list_to_map ~diagnostics (l : Syntax.tvar_binder list) =
  Lst.fold_left l Map_string.empty (fun map ->
      fun { tvar_name; loc_; _ } ->
       if Map_string.mem map tvar_name then (
         add_error diagnostics (Errors.duplicate_tvar ~name:tvar_name ~loc:loc_);
         map)
       else Map_string.add map tvar_name loc_)

let tvar_option_list_to_map ~diagnostics (l : Syntax.type_decl_binder list) =
  Lst.fold_left l Map_string.empty (fun map ->
      fun { tvar_name; loc_; _ } ->
       match tvar_name with
       | Some tvar_name ->
           if Map_string.mem map tvar_name then (
             add_error diagnostics
               (Errors.duplicate_tvar ~name:tvar_name ~loc:loc_);
             map)
           else Map_string.add map tvar_name loc_
       | None -> map)

let type_name_is_reserved = function "Self" | "Error" -> true | _ -> false

let syntax_check ~diagnostics impls =
  let unused_tvars = ref Map_string.empty in
  let obj =
    object (self)
      inherit [_] Syntax.iter as super

      method! visit_Pexpr_sequence ctx exprs last_expr _ =
        Lst.iter exprs ~f:(self#visit_expr ctx);
        self#visit_expr ctx last_expr;
        let rec report_unreachable_expr = function
          | (Syntax.Pexpr_return _ | Pexpr_break _ | Pexpr_continue _) :: exprs
            ->
              let loc =
                match exprs with
                | e :: _ ->
                    Rloc.merge
                      (Syntax.loc_of_expression e)
                      (Syntax.loc_of_expression last_expr)
                | [] -> Syntax.loc_of_expression last_expr
              in
              Local_diagnostics.add_warning ctx.diagnostics
                { kind = Warnings.Unreachable; loc }
          | _ :: exprs -> report_unreachable_expr exprs
          | [] -> ()
        in
        report_unreachable_expr exprs

      method! visit_Pexpr_letrec ctx bindings body loc =
        let vis = ref Map_string.empty in
        Lst.iter bindings ~f:(fun (binder, _) ->
            let name = binder.binder_name in
            vis :=
              Map_string.adjust !vis name (function
                | Some prev_loc ->
                    add_error ctx.diagnostics
                      (Errors.duplicate_local_fns ~name ~loc:binder.loc_
                         ~prev_loc);
                    prev_loc
                | None -> Rloc.to_loc ~base:ctx.base binder.loc_));
        super#visit_Pexpr_letrec ctx bindings body loc

      method! visit_Ppat_range ctx lhs rhs inclusive loc =
        super#visit_Ppat_range ctx lhs rhs inclusive loc;
        (match lhs with
        | Ppat_constr { args = None; _ } | Ppat_constant _ | Ppat_any _ -> ()
        | _ ->
            add_error ctx.diagnostics
              (Errors.bad_range_pattern_operand (Syntax.loc_of_pattern lhs)));
        match rhs with
        | Ppat_constr { args = None; _ } | Ppat_constant _ -> ()
        | Ppat_any { loc_ } ->
            if inclusive then
              add_error ctx.diagnostics
                (Errors.inclusive_range_pattern_no_upper_bound loc_)
        | _ ->
            add_error ctx.diagnostics
              (Errors.bad_range_pattern_operand (Syntax.loc_of_pattern rhs))

      method! visit_Ptop_funcdef ctx fun_decl decl_body loc =
        let binder_name = fun_decl.name.binder_name in
        if
          fun_decl.type_name = None
          && (binder_name = "init" || binder_name = "main")
        then (
          match (fun_decl, decl_body) with
          | ( {
                quantifiers = [];
                return_type = None;
                decl_params = None;
                type_name = _;
                name = _;
                has_error = None;
                is_async = false;
                vis = Vis_default;
                params_loc_ = _;
                doc_ = _;
                attrs = _;
              },
              Decl_body { expr = body; local_types = _ } ) ->
              self#visit_expr ctx body
          | _, _ ->
              let kind = if binder_name = "init" then `Init else `Main in
              add_global_error diagnostics
                (Errors.invalid_init_or_main ~kind ~loc);
              self#visit_decl_body ctx decl_body)
        else
          let ctx =
            {
              ctx with
              tvars = Lst.map fun_decl.quantifiers (fun x -> x.tvar_name);
            }
          in
          Basic_lst.iter fun_decl.quantifiers ~f:(self#visit_tvar_binder ctx);
          unused_tvars :=
            tvar_list_to_map ~diagnostics:ctx.diagnostics fun_decl.quantifiers;
          (match fun_decl.decl_params with
          | None ->
              add_error ctx.diagnostics
                (Errors.missing_parameter_list ~name:binder_name
                   ~loc:fun_decl.name.loc_)
          | Some params -> self#visit_parameters ctx params);
          (match fun_decl.return_type with
          | None -> ()
          | Some (ty, (No_error_typ | Default_error_typ _)) ->
              self#visit_typ ctx ty
          | Some (ty1, Error_typ { ty }) ->
              self#visit_typ ctx ty1;
              self#visit_typ ctx ty);
          Lst.iter fun_decl.attrs ~f:(self#visit_attribute ctx);
          self#visit_decl_body ctx decl_body;
          warn_unused_tvars ctx.diagnostics ~is_error:true !unused_tvars;
          match decl_body with
          | Decl_stubs decl
            when fun_decl.quantifiers <> []
                 && not (Syntax_util.is_intrinsic decl) ->
              add_global_error diagnostics (Errors.ffi_cannot_poly loc)
          | _ -> ()

      method! visit_tvar_binder ctx { tvar_name; tvar_constraints = _; loc_ } =
        if type_name_is_reserved tvar_name then
          add_error ctx.diagnostics
            (Errors.reserved_type_name ~decl_kind:`Tvar ~name:tvar_name
               ~loc:loc_)

      method! visit_Ptop_typedef ctx
          { params; components; type_vis; tycon; tycon_loc_; doc_ = _; attrs } =
        if type_name_is_reserved tycon then
          add_error ctx.diagnostics
            (Errors.reserved_type_name ~decl_kind:`Type ~name:tycon
               ~loc:tycon_loc_);
        Lst.iter params ~f:(fun tvar_binder ->
            match tvar_binder.tvar_name with
            | Some name when type_name_is_reserved name ->
                add_error ctx.diagnostics
                  (Errors.reserved_type_name ~decl_kind:`Tvar ~name
                     ~loc:tvar_binder.loc_)
            | _ -> ());
        let tvars =
          Lst.fold_right params [] (fun x ->
              fun acc ->
               match x.tvar_name with Some name -> name :: acc | None -> acc)
        in
        unused_tvars :=
          tvar_option_list_to_map ~diagnostics:ctx.diagnostics params;
        Lst.iter attrs ~f:(fun attr -> self#visit_attribute ctx attr);
        self#visit_type_desc
          { type_vis; tvars; diagnostics = ctx.diagnostics; base = ctx.base }
          components;
        (match components with
        | Ptd_abstract | Ptd_extern -> ()
        | _ -> warn_unused_tvars ctx.diagnostics ~is_error:false !unused_tvars);
        if Basic_strutil.starts_with_lower_case tycon.![0] then
          Local_diagnostics.add_warning ctx.diagnostics
            { kind = Lowercase_type_name tycon; loc = tycon_loc_ }

      method! visit_field_decl ctx
          { field_vis; field_ty; field_name; field_mut = _ } =
        (if field_vis <> Vis_default then
           match (field_vis, ctx.type_vis) with
           | Vis_pub { attr = Some _; loc_ = vis_loc }, _ ->
               add_error ctx.diagnostics
                 (Errors.unsupported_modifier
                    ~modifier:(Syntax.string_of_vis field_vis)
                    ~loc:vis_loc)
           | Vis_priv { loc_ = vis_loc }, (Vis_priv _ | Vis_default)
           | Vis_pub { attr = None; loc_ = vis_loc }, Vis_pub { attr = _ } ->
               Local_diagnostics.add_warning ctx.diagnostics
                 {
                   loc = vis_loc;
                   kind =
                     Warnings.Redundant_modifier
                       {
                         modifier = Syntax.string_of_vis field_vis;
                         field = field_name.label;
                       };
                 }
           | Vis_pub { attr = None; loc_ = vis_loc }, (Vis_default | Vis_priv _)
             ->
               add_error ctx.diagnostics
                 (Errors.field_visibility
                    ~field_vis:(Syntax.string_of_vis field_vis)
                    ~type_vis:(Syntax.string_of_vis ctx.type_vis)
                    ~loc:vis_loc)
           | _ -> ());
        self#visit_typ ctx field_ty

      method! visit_constr_param ctx p =
        super#visit_constr_param ctx p;
        match p.cparam_label with
        | None when p.cparam_mut ->
            add_error ctx.diagnostics
              (Errors.constr_no_mut_positional_field
                 (Syntax.loc_of_type_expression p.cparam_typ))
        | _ -> ()

      method! visit_Trait_method ctx name has_error quantifiers param_typs
          return_type loc_ =
        if quantifiers <> [] then
          add_error ctx.diagnostics (Errors.trait_method_cannot_poly loc_);
        super#visit_Trait_method ctx name has_error quantifiers param_typs
          return_type loc_

      method! visit_trait_decl ctx decl =
        let name = decl.trait_name.binder_name in
        if type_name_is_reserved name then
          add_error ctx.diagnostics
            (Errors.reserved_type_name ~decl_kind:`Trait ~name
               ~loc:decl.trait_name.loc_);
        let _ =
          Lst.fold_left decl.trait_methods Map_string.empty (fun seen_methods ->
              fun meth ->
               let (Trait_method { name; loc_; _ }) = meth in
               let name = name.binder_name in
               Map_string.adjust seen_methods name (fun prev ->
                   match prev with
                   | Some prev_loc ->
                       add_error ctx.diagnostics
                         (Errors.trait_duplicate_method
                            ~trait:decl.trait_name.binder_name ~name
                            ~first:prev_loc ~second:loc_);
                       prev_loc
                   | None -> Rloc.to_loc ~base:ctx.base loc_))
        in
        super#visit_trait_decl ctx decl

      method! visit_Ptype_name ctx constr_id tys _loc =
        match (constr_id, tys) with
        | { lid = Lident id; loc_ = _ }, [] when Lst.has_string ctx.tvars id ->
            if Map_string.mem !unused_tvars id then
              unused_tvars := Map_string.remove !unused_tvars id
        | _ -> Lst.iter tys ~f:(self#visit_typ ctx)

      method! visit_Ptop_impl ctx self_ty trait method_name has_error
          quantifiers params ret_ty body vis loc_ header_loc_ attrs doc_ =
        (match self_ty with
        | None ->
            if quantifiers <> [] then
              add_error ctx.diagnostics
                (Errors.no_quantifiers_on_default_impl method_name.loc_)
        | _ -> ());
        let ctx =
          { ctx with tvars = Lst.map quantifiers (fun x -> x.tvar_name) }
        in
        unused_tvars :=
          tvar_list_to_map ~diagnostics:ctx.diagnostics quantifiers;
        super#visit_Ptop_impl ctx self_ty trait method_name has_error
          quantifiers params ret_ty body vis loc_ header_loc_ attrs doc_;
        warn_unused_tvars ctx.diagnostics ~is_error:true !unused_tvars

      method! visit_attribute ctx attr =
        match attr.parsed with
        | None ->
            add_error ctx.diagnostics
              (Errors.attribute_parse_error ~loc:attr.loc_
                 "syntax error in attribute")
        | Some expr -> (
            let warn_invalid_attribute message =
              add_warning ctx.diagnostics
                {
                  Local_diagnostics.loc = attr.loc_;
                  kind = Warnings.Invalid_attribute message;
                }
            in
            match expr with
            | Attribute.Apply ({ qual = None; name = "deprecated" }, props) -> (
                match props with
                | Expr (String _) :: [] -> ()
                | _ ->
                    warn_invalid_attribute
                      "invalid properties in deprecated attribute")
            | Attribute.Apply ({ qual = None; name = "alert" }, props) -> (
                match props with
                | [ Expr (Ident _); Expr (String _) ] -> ()
                | _ ->
                    warn_invalid_attribute
                      "invalid properties in alert attribute")
            | Apply ({ qual = None; name = "intrinsic" }, props) -> (
                match props with
                | Expr (String _) :: [] -> ()
                | _ ->
                    warn_invalid_attribute
                      "invalid properties in intrinsic attribute")
            | Ident { qual = Some _; name = _ }
            | Apply ({ qual = Some _; name = _ }, _) ->
                ()
            | Ident id | Apply (id, _) ->
                let id = Attribute.string_of_id id in
                warn_invalid_attribute
                  (("unknown attribute " ^ id : Stdlib.String.t) [@merlin.hide])
            | _ -> warn_invalid_attribute "invalid attribute")
    end
  in
  Lst.iter impls ~f:(fun impl ->
      let base = Syntax.loc_of_impl impl in
      let local_diagnostics = Local_diagnostics.make ~base in
      let init_ctx =
        {
          type_vis = Vis_default;
          tvars = [];
          diagnostics = local_diagnostics;
          base;
        }
      in
      obj#visit_impl init_ctx impl;
      Local_diagnostics.add_to_global local_diagnostics diagnostics)

let desugar_matchfn (cases : Syntax.multi_arg_case list) ~has_error ~is_async
    fn_loc_ loc_ ~diagnostics =
  (let only_variable_pattern pats =
     let is_variable_pattern (pat : Syntax.pattern) =
       match pat with
       | Ppat_var _ | Ppat_constraint { pat = Ppat_var _; _ } -> true
       | _ -> false
     in
     Lst.for_all pats is_variable_pattern
   in
   let loc_of_pats pats =
     Rloc.merge
       (Syntax.loc_of_pattern (List.hd pats))
       (Syntax.loc_of_pattern (Lst.last pats))
       [@@inline]
   in
   match cases with
   | [] ->
       Lambda
         {
           parameters = [];
           params_loc_ = Rloc.no_location;
           body = Pexpr_unit { faked = false; loc_ };
           return_type = None;
           kind_ = Matrix;
           has_error;
           is_async;
         }
   | { patterns = pats; guard = None; body } :: []
     when only_variable_pattern pats ->
       let parameters_rev =
         Lst.fold_left pats [] (fun acc ->
             fun pat ->
              (match pat with
               | Ppat_var binder -> Positional { ty = None; binder } :: acc
               | Ppat_constraint { pat = Ppat_var binder; ty; _ } ->
                   Positional { ty = Some ty; binder } :: acc
               | _ -> assert false
                : Syntax.parameter list))
       in
       let parameters = List.rev parameters_rev in
       Lambda
         {
           parameters;
           params_loc_ = loc_of_pats pats;
           body;
           return_type = None;
           kind_ = Matrix;
           has_error;
           is_async;
         }
   | { patterns = pats; _ } :: _ -> (
       let fresh_param_name () =
         let i = Basic_uuid.next () in
         (("*param" ^ Int.to_string i : Stdlib.String.t) [@merlin.hide])
       in
       let to_parameter p_name loc_ =
         (Positional { ty = None; binder = { binder_name = p_name; loc_ } }
           : Syntax.parameter)
           [@@inline]
       in
       let to_var p_name loc_ =
         Syntax.Pexpr_ident { id = { var_name = Lident p_name; loc_ }; loc_ }
           [@@inline]
       in
       let pat_tuple pats =
         let loc_ = loc_of_pats pats in
         Syntax.Ppat_tuple { pats; loc_ }
           [@@inline]
       in
       let add_error ~loc ~expect ~actual =
         add_error diagnostics
           (Errors.matchfn_arity_mismatch ~loc ~expected:expect ~actual)
           [@@inline]
       in
       match pats with
       | pat :: [] ->
           let param_loc_ = Syntax.loc_of_pattern pat in
           let param_name = fresh_param_name () in
           let param_with_loc = to_parameter param_name param_loc_ in
           let cases =
             Lst.map cases (fun { patterns = pats; guard; body } ->
                 (match pats with
                  | pat :: [] -> { pattern = pat; guard; body }
                  | _ ->
                      let loc = loc_of_pats pats in
                      add_error ~loc ~expect:1 ~actual:(List.length pats);
                      { pattern = pat_tuple pats; guard; body }
                   : Syntax.case))
           in
           let matchee = to_var param_name param_loc_ in
           Lambda
             {
               parameters = [ param_with_loc ];
               params_loc_ = Rloc.no_location;
               body =
                 Syntax.Pexpr_match
                   { expr = matchee; cases; match_loc_ = fn_loc_; loc_ };
               return_type = None;
               kind_ = Matrix;
               has_error;
               is_async;
             }
       | pats ->
           let param_names =
             Lst.map pats (fun p ->
                 (fresh_param_name (), Syntax.loc_of_pattern p))
           in
           let param_names_with_loc =
             Lst.map param_names (fun (p, loc) -> to_parameter p loc)
           in
           let cases =
             Lst.map cases (fun { patterns = pats; guard; body } ->
                 ({ pattern = pat_tuple pats; guard; body } : Syntax.case))
           in
           let variables = Lst.map param_names (fun (p, loc) -> to_var p loc) in
           let matchee =
             Syntax.Pexpr_tuple { exprs = variables; loc_ = Rloc.no_location }
           in
           Lambda
             {
               parameters = param_names_with_loc;
               params_loc_ = Rloc.no_location;
               body =
                 Syntax.Pexpr_match
                   { expr = matchee; cases; match_loc_ = fn_loc_; loc_ };
               return_type = None;
               kind_ = Matrix;
               has_error;
               is_async;
             })
    : Syntax.func)

let test_to_func ~diagnostics i expr params local_types loc_ =
  let test_name_gen = Test_util.gen_test_name (Loc.filename loc_) i in
  let unit_type : Syntax.typ =
    Ptype_name
      {
        constr_id = { lid = Lident "Unit"; loc_ = Rloc.no_location };
        tys = [];
        loc_ = Rloc.no_location;
      }
  in
  let string_type : Syntax.typ =
    Ptype_name
      {
        constr_id = { lid = Lident "Error"; loc_ = Rloc.no_location };
        tys = [];
        loc_ = Rloc.no_location;
      }
  in
  let params =
    match params with
    | None -> Some []
    | Some
        (Syntax.Positional
           {
             ty =
               Some
                 (Ptype_name
                    {
                      constr_id =
                        {
                          lid =
                            Longident.Ldot
                              {
                                pkg = "test" | "moonbitlang/core/test";
                                id = "T";
                              };
                          _;
                        };
                      tys = [];
                      _;
                    });
             _;
           }
        :: []) ->
        params
    | _ ->
        Diagnostics.add_error diagnostics (Errors.invalid_test_parameter loc_);
        Some []
  in
  Syntax.Ptop_funcdef
    {
      fun_decl =
        {
          type_name = None;
          has_error = None;
          is_async = false;
          name = { binder_name = test_name_gen; loc_ = Rloc.no_location };
          decl_params = params;
          params_loc_ = Rloc.no_location;
          quantifiers = [];
          return_type = Some (unit_type, Error_typ { ty = string_type });
          vis = Vis_default;
          attrs = [];
          doc_ = Docstring.empty;
        };
      decl_body = Decl_body { expr; local_types };
      loc_;
    }

let post_process ~diagnostics (impls : Syntax.impls) =
  (syntax_check ~diagnostics impls;
   let map_top impl =
     let local_diagnostics =
       Local_diagnostics.make ~base:(Syntax.loc_of_impl impl)
     in
     let impl =
       (object (self)
          inherit [_] Syntax.map as super

          method! visit_Pexpr_array_get env array index loc_ =
            let array' = self#visit_expr env array in
            let index' = self#visit_expr env index in
            Parser_util.desugar_array_get array' index' ~loc_

          method! visit_Pexpr_array_set env array index value loc_ =
            let array' = self#visit_expr env array in
            let index' = self#visit_expr env index in
            let value' = self#visit_expr env value in
            Parser_util.desugar_array_set array' index' value' ~loc_

          method! visit_Pexpr_array_augmented_set env op array index value loc_
              =
            let op' = self#visit_var env op in
            let array' = self#visit_expr env array in
            let index' = self#visit_expr env index in
            let value' = self#visit_expr env value in
            Parser_util.desugar_array_augmented_set op' array' index' value'
              ~loc_

          method! visit_func _ func =
            match func with
            | Lambda _ -> super#visit_func () func
            | Match { cases; has_error; is_async; fn_loc_; loc_ } ->
                super#visit_func ()
                  (desugar_matchfn cases ~has_error ~is_async fn_loc_ loc_
                     ~diagnostics:local_diagnostics)

          method! visit_Pexpr_apply env func args bang loc_ =
            (match (func, args) with
            | Pexpr_constr _, [] ->
                add_error local_diagnostics (Errors.illform_constr_arg loc_)
            | _ -> ());
            super#visit_Pexpr_apply env func args bang loc_

          method! visit_Pexpr_record env type_name fields trailing loc =
            (match (type_name, fields, trailing) with
            | ( None,
                Field_def { is_pun = true; _ } :: [],
                (Syntax.Trailing_semi | Trailing_none) ) ->
                Local_diagnostics.add_warning local_diagnostics
                  { loc; kind = Warnings.Ambiguous_block }
            | _ -> ());
            super#visit_Pexpr_record env type_name fields trailing loc

          method! visit_Pexpr_group env expr _ _ = super#visit_expr env expr

          method! visit_Pexpr_unary env op expr loc_ =
            match op.var_name with
            | Lident "-" ->
                Pexpr_dot_apply
                  {
                    loc_;
                    self = self#visit_expr env expr;
                    method_name =
                      {
                        label_name =
                          (Parsing_operators.find_exn "~-").method_name;
                        loc_;
                      };
                    return_self = false;
                    attr = No_attr;
                    args = [];
                  }
            | _ -> assert false

          method! visit_Interp_source env interp_source =
            let base = Loc.get_start (Syntax.loc_of_impl impl) in
            let expr =
              Parsing_interp.expr_of_interp ~diagnostics ~base interp_source
            in
            let expr = self#visit_expr env expr in
            let loc_ =
              Rloc.of_loc
                ~base:(Loc.get_start (Syntax.loc_of_impl impl))
                interp_source.loc_
            in
            Syntax.Interp_expr { expr; loc_ }
       end)
         #visit_impl
         () impl
     in
     Local_diagnostics.add_to_global local_diagnostics diagnostics;
     impl
   in
   let rec aux i impls =
     match impls with
     | [] -> []
     | hd :: tl -> (
         (let loc = Syntax.loc_of_impl hd in
          let loca = Loc.get_start loc in
          let not_left_aligned =
            loca.pos_cnum <> loca.pos_bol && not (Loc.is_no_location loc)
          in
          if not_left_aligned then
            Diagnostics.add_warning diagnostics
              { loc; kind = Warnings.Toplevel_not_left_aligned });
         match map_top hd with
         | Syntax.Ptop_test
             { expr; name = _; params; local_types; loc_; attrs = _ } ->
             test_to_func ~diagnostics i expr params local_types loc_
             :: aux (i + 1) tl
         | Syntax.Ptop_funcdef
             {
               fun_decl =
                 { name = { binder_name = "init"; _ }; type_name = None; _ };
               decl_body = Decl_body { expr = body; local_types };
               loc_;
             } ->
             Syntax.Ptop_expr
               { expr = body; is_main = false; local_types; loc_ }
             :: aux i tl
         | Syntax.Ptop_funcdef
             {
               fun_decl =
                 { name = { binder_name = "main"; _ }; type_name = None; _ };
               decl_body = Decl_body { expr = body; local_types };
               loc_;
             } ->
             Syntax.Ptop_expr { expr = body; is_main = true; local_types; loc_ }
             :: aux i tl
         | top -> top :: aux i tl)
       [@@tail_mod_cons]
   in
   aux 0 impls
    : Syntax.impls)
