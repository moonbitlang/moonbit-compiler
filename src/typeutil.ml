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


type typ = Stype.t

module Ident = Basic_ident
module Lst = Basic_lst
module Vec = Basic_vec
module Hashset_string = Basic_hashset_string
module Longident = Basic_longident
module Type_path = Basic_type_path
module Syntax = Parsing_syntax
module UInt32 = Basic_uint32
module UInt64 = Basic_uint64
module Bigint = Basic_bigint
module Constr_info = Basic_constr_info

type build_context = Exec of { is_main_loc : Loc.t } | Lib | SingleFile

module Loc = Rloc

let type_of_constant (c : Syntax.constant) =
  match c with
  | Const_bool _ -> Stype.bool
  | Const_byte _ -> Stype.byte
  | Const_bytes _ -> Stype.bytes
  | Const_char _ -> Stype.char
  | Const_int _ -> Stype.int
  | Const_int64 _ -> Stype.int64
  | Const_double _ -> Stype.double
  | Const_string _ -> Stype.string
  | Const_uint _ -> Stype.uint
  | Const_uint64 _ -> Stype.uint64
  | Const_bigint _ -> Stype.bigint

let unknown_tag : Typedtree.constr_tag =
  Constr_tag_regular
    {
      total = Constr_info.Index_set.empty;
      index = -1;
      name_ = "";
      repr_ = Non_constant;
    }

let unknown_pos : int = -1
let default_err_type = Stype.error

let default_err_typedtree_type =
  Typedtree.Tname
    {
      constr = { lid = Lident "string"; loc_ = Loc.no_location };
      params = [];
      ty = default_err_type;
      is_alias_ = false;
      loc_ = Loc.no_location;
    }

let typing_type ~(allow_partial : bool) ~(allow_private : bool)
    ?(placeholder_env : Placeholder_env.t option) (te : Syntax.typ)
    ~(tvar_env : Tvar_env.t) ~(types : Global_env.All_types.t)
    ~(local_type_env : Local_type.env option)
    ~(diagnostics : Local_diagnostics.t) =
  (let add_error err = Local_diagnostics.add_error diagnostics err [@@inline] in
   let check_arity ~kind id expected ~loc
       (tys : [ `Wildcard | `Fixed of Stype.t list ]) =
     match tys with
     | `Wildcard ->
         if expected = 0 then
           add_error
             (Errors.type_constr_arity_mismatch ~kind ~id ~expected ~actual:1
                ~loc);
         if allow_partial then Type.type_var_list expected Tvar_normal
         else List.init expected (fun _ -> Stype.blackhole)
     | `Fixed tys ->
         let actual = List.length tys in
         if expected <> actual then
           add_error
             (Errors.type_constr_arity_mismatch ~kind ~id ~expected ~actual ~loc);
         if expected = actual then tys
         else if expected < actual then Lst.take tys expected
         else if allow_partial then
           tys @ Type.type_var_list (expected - actual) Tvar_error
         else tys @ List.init (expected - actual) (fun _ -> Stype.blackhole)
       [@@inline]
   in
   let rec go (te : Syntax.typ) =
     (match te with
      | Ptype_any { loc_ } ->
          if not allow_partial then
            add_error (Errors.unexpected_partial_type loc_);
          let ty =
            if allow_partial then Stype.new_type_var Tvar_normal
            else Stype.blackhole
          in
          (ty, Tany { ty; loc_ }, false)
      | Ptype_arrow { ty_arg; ty_res; ty_err; is_async; loc_ } ->
          let params_ty, params, params_is_generic = go_list ty_arg in
          let ret_ty, return, ret_is_generic = go ty_res in
          let err_ty, err, err_is_generic =
            match ty_err with
            | Error_typ { ty = ty_err } ->
                let err_ty, err, err_is_generic = go ty_err in
                if not (Type.is_error_type ~tvar_env err_ty) then
                  add_error
                    (Errors.not_error_subtype
                       (Printer.type_to_string err_ty)
                       loc_);
                (Some err_ty, Typedtree.Error_typ { ty = err }, err_is_generic)
            | Default_error_typ { loc_ } ->
                (Some default_err_type, Default_error_typ { loc_ }, false)
            | No_error_typ -> (None, No_error_typ, false)
          in
          let generic_ =
            params_is_generic || ret_is_generic || err_is_generic
          in
          let ty : Stype.t =
            Tarrow { params_ty; ret_ty; err_ty; is_async; generic_ }
          in
          (ty, Tarrow { ty; params; return; err_ty = err; loc_ }, generic_)
      | Ptype_name { constr_id; tys; loc_ } -> (
          let tys, params, generic_ = go_list tys in
          let make_typ ty ~is_alias ~generic_ =
            ( ty,
              Typedtree.Tname
                { ty; constr = constr_id; params; is_alias_ = is_alias; loc_ },
              generic_ )
              [@@inline]
          in
          let tys =
            match params with Tany _ :: [] -> `Wildcard | _ -> `Fixed tys
          in
          let find_tvar_env id =
            match Tvar_env.find_by_name tvar_env id with
            | Some tvar_info ->
                let _ =
                  check_arity ~kind:"type parameter" constr_id.lid 0 tys
                    ~loc:loc_
                in
                make_typ ~generic_:true ~is_alias:false tvar_info.typ
            | None ->
                let ty, is_alias = go_constr ~generic_ constr_id.lid tys loc_ in
                make_typ ~generic_ ~is_alias ty
              [@@inline]
          in
          match (local_type_env, constr_id.lid) with
          | Some local_type_env, Lident name -> (
              match Local_type.find_type local_type_env name with
              | Some { name; toplevel_id; _ } ->
                  let ty : Stype.t =
                    T_constr
                      {
                        type_constructor = Type_path.local_type toplevel_id name;
                        tys = Tvar_env.get_types tvar_env;
                        generic_ = false;
                        is_suberror_ = false;
                      }
                  in
                  let _ =
                    check_arity ~kind:"type constructor" constr_id.lid 0 tys
                      ~loc:loc_
                  in
                  make_typ ~generic_:false ~is_alias:false ty
              | None -> find_tvar_env name)
          | None, Lident id -> find_tvar_env id
          | _ ->
              let ty, is_alias = go_constr ~generic_ constr_id.lid tys loc_ in
              make_typ ~generic_ ~is_alias ty)
      | Ptype_tuple { tys; loc_ } ->
          let tys, params, generic_ = go_list tys in
          let ty : Stype.t =
            T_constr
              {
                type_constructor = Type_path.tuple (List.length tys);
                tys;
                generic_;
                is_suberror_ = false;
              }
          in
          (ty, T_tuple { ty; params; loc_ }, generic_)
      | Ptype_option { ty; loc_; question_loc } ->
          let tys, params, generic_ = go_list [ ty ] in
          let ty : Stype.t =
            T_constr
              {
                type_constructor = Type_path.Builtin.type_path_option;
                tys;
                generic_;
                is_suberror_ = false;
              }
          in
          ( ty,
            Tname
              {
                params;
                ty;
                loc_;
                constr = { lid = Lident "Option"; loc_ = question_loc };
                is_alias_ = false;
              },
            generic_ )
      | Ptype_object ({ lid; loc_ } as constr) -> (
          let check_object_safety ~name object_safety =
            match
              Trait_decl.check_object_safety ~name:(Type_path_util.name name)
                ~loc:loc_ object_safety
            with
            | Some err -> add_error err
            | None -> ()
              [@@inline]
          in
          match Global_env.All_types.find_type_alias types lid with
          | Some { target = Trait_alias { trait }; _ } ->
              (match Global_env.All_types.find_trait_by_path types trait with
              | Some decl -> check_object_safety ~name:trait decl.object_safety_
              | None -> ());
              let ty : Stype.t = T_trait trait in
              (ty, Tobject { constr; ty; is_alias_ = true; loc_ }, false)
          | Some { target = Type_alias _; name; _ } ->
              add_error (Errors.not_a_trait ~name ~loc:loc_);
              (Stype.blackhole, Tany { ty = Stype.blackhole; loc_ }, false)
          | None -> (
              let resolve_in_types () =
                (match Global_env.All_types.find_trait types lid ~loc:loc_ with
                 | Ok { name; object_safety_; _ } ->
                     check_object_safety ~name object_safety_;
                     let ty : Stype.t = T_trait name in
                     (ty, Tobject { constr; ty; is_alias_ = false; loc_ }, false)
                 | Error err ->
                     add_error err;
                     ( Stype.blackhole,
                       Tany { ty = Stype.blackhole; loc_ },
                       false )
                  : Stype.t * Typedtree.typ * bool)
                  [@@local]
              in
              match (lid, placeholder_env) with
              | Lident name, Some placeholder_env -> (
                  match Placeholder_env.find_trait_opt placeholder_env name with
                  | None -> resolve_in_types ()
                  | Some decl ->
                      let name =
                        Type_path.toplevel_type
                          ~pkg:!Basic_config.current_package
                          decl.trait_name.binder_name
                      in
                      let ty : Stype.t = T_trait name in
                      ( ty,
                        Tobject { constr; ty; is_alias_ = false; loc_ },
                        false ))
              | _ -> resolve_in_types ()))
       : typ * Typedtree.typ * bool)
   and go_list tys =
     match tys with
     | [] -> ([], [], false)
     | ty :: tys ->
         let sty, typ, ty_is_generic = go ty in
         let stys, typs, tys_is_generic = go_list tys in
         (sty :: stys, typ :: typs, ty_is_generic || tys_is_generic)
   and go_constr ~generic_ (constr_id : Longident.t) tys (loc_ : Loc.t) =
     (let exception
        Found_type of {
          arity : int;
          type_constructor : Type_path.t;
          is_private : bool;
          is_suberror_ : bool;
        }
      in
      let exception Found_predef of Stype.t in
      let exception Found_alias of Type_alias.t in
      try
        (match Global_env.All_types.find_type_alias types constr_id with
        | Some ({ target = Type_alias _; _ } as alias) ->
            raise_notrace (Found_alias alias)
        | None | Some { target = Trait_alias _; _ } -> ());
        (match (placeholder_env, constr_id) with
        | Some placeholder_env, Lident name -> (
            match Placeholder_env.find_type_opt placeholder_env name with
            | Some decl ->
                let type_constructor =
                  Type_path.toplevel_type
                    ~pkg:!Basic_config.current_package
                    decl.tycon
                in
                let is_suberror_ =
                  match decl.components with Ptd_error _ -> true | _ -> false
                in
                let arity = List.length decl.params in
                raise_notrace
                  (Found_type
                     {
                       arity;
                       type_constructor;
                       is_private =
                         (match decl.type_vis with
                         | Vis_priv _ -> true
                         | _ -> false);
                       is_suberror_;
                     })
            | None -> ())
        | _ -> ());
        match
          Global_env.All_types.find_type_or_trait types constr_id ~loc:loc_
        with
        | `Type { ty_arity; ty_constr; ty_vis; ty_is_suberror_; _ } ->
            raise_notrace
              (Found_type
                 {
                   arity = ty_arity;
                   type_constructor = ty_constr;
                   is_private = ty_vis = Vis_priv;
                   is_suberror_ = ty_is_suberror_;
                 })
        | `Predef ty -> raise_notrace (Found_predef ty)
        | `Trait _ ->
            add_error
              (Errors.not_a_type
                 ~name:(Longident.to_string constr_id)
                 ~loc:loc_);
            ( (if allow_partial then Stype.new_type_var Tvar_error
               else Stype.blackhole),
              false )
        | `Error error_message ->
            add_error error_message;
            ( (if allow_partial then Stype.new_type_var Tvar_error
               else Stype.blackhole),
              false )
      with
      | Found_type { arity; type_constructor; is_private; is_suberror_ } ->
          if (not allow_private) && is_private then
            add_error (Errors.cannot_depend_private ~entity:"type" ~loc:loc_);
          let tys =
            check_arity ~kind:"type constructor" constr_id arity tys ~loc:loc_
          in
          (T_constr { type_constructor; tys; generic_; is_suberror_ }, false)
      | Found_predef ty ->
          let _ =
            check_arity ~kind:"type constructor" constr_id 0 tys ~loc:loc_
          in
          (ty, false)
      | Found_alias ({ arity; _ } as alias) ->
          let tys =
            check_arity ~kind:"type alias" constr_id arity tys ~loc:loc_
          in
          let exception Alias_mentions_priv_type of Type_path.t in
          let rec check_priv (ty : Stype.t) =
            let check_path (p : Type_path.t) =
              match p with
              | Toplevel { pkg; id } when pkg = !Basic_config.current_package
                -> (
                  match placeholder_env with
                  | Some placeholder_env -> (
                      match
                        Placeholder_env.find_type_opt placeholder_env id
                      with
                      | None -> ()
                      | Some decl -> (
                          match decl.type_vis with
                          | Vis_priv _ ->
                              raise_notrace (Alias_mentions_priv_type p)
                          | Vis_pub _ | Vis_default -> ()))
                  | None -> (
                      match Global_env.All_types.find_type_by_path types p with
                      | None -> ()
                      | Some decl ->
                          if decl.ty_vis = Vis_priv then
                            raise_notrace (Alias_mentions_priv_type p)))
              | _ -> ()
                [@@inline]
            in
            match Stype.type_repr ty with
            | T_constr { type_constructor = p; tys } ->
                check_path p;
                Lst.iter tys ~f:check_priv
            | T_trait trait -> check_path trait
            | Tarrow { params_ty; ret_ty; err_ty; is_async = _ } -> (
                Lst.iter params_ty ~f:check_priv;
                check_priv ret_ty;
                match err_ty with
                | None -> ()
                | Some err_ty -> check_priv err_ty)
            | Tvar _ | Tparam _ | T_blackhole | T_builtin _ -> ()
          in
          (match constr_id with
          | Lident name -> (
              if (not allow_private) && not alias.is_pub then
                match[@warning "-fragile-match"] alias.target with
                | Type_alias ty -> (
                    try check_priv ty
                    with Alias_mentions_priv_type p ->
                      add_error
                        (Errors.alias_with_priv_target_in_pub_sig ~alias:name
                           ~priv_type:(Type_path_util.name p) ~loc:loc_))
                | _ -> assert false)
          | Ldot _ -> ());
          (Poly_type.instantiate_alias alias tys, true)
       : Stype.t * bool)
   in
   let _, typ, _ = go te in
   typ
    : Typedtree.typ)

type type_name =
  | Tname_param of { index : int; name_ : string }
  | Tname_predef of Type_path.t
  | Tname_defined of Typedecl_info.t
  | Tname_trait of Trait_decl.t
  | Tname_local_type of Local_type.t
  | Tname_trait_object of Trait_decl.t

let typing_type_name_not_object ~(allow_private : bool)
    (type_name : Syntax.type_name) ~(tvar_env : Tvar_env.t)
    ~(types : Global_env.All_types.t) ~(local_type_env : Local_type.env option)
    ~(diagnostics : Local_diagnostics.t) =
  (let loc = type_name.loc_ in
   let resolve_as_constr () =
     match
       Global_env.All_types.find_type_or_trait types type_name.name ~loc
     with
     | `Type ty_decl ->
         let type_name : Typedtree.type_name =
           Tname_path
             { name = ty_decl.ty_constr; kind = Type; loc_ = type_name.loc_ }
         in
         if (not allow_private) && ty_decl.ty_vis = Vis_priv then
           Local_diagnostics.add_error diagnostics
             (Errors.cannot_depend_private ~entity:"type" ~loc);
         Some (Tname_defined ty_decl, type_name)
     | `Trait trait ->
         let type_name : Typedtree.type_name =
           Tname_path { name = trait.name; kind = Trait; loc_ = type_name.loc_ }
         in
         if (not allow_private) && trait.vis_ = Vis_priv then
           Local_diagnostics.add_error diagnostics
             (Errors.cannot_depend_private ~entity:"trait" ~loc);
         Some (Tname_trait trait, type_name)
     | `Predef ty -> (
         match[@warning "-fragile-match"] ty with
         | T_builtin builtin ->
             let path = Stype.tpath_of_builtin builtin in
             let type_name : Typedtree.type_name =
               Tname_path { name = path; kind = Type; loc_ = type_name.loc_ }
             in
             Some (Tname_predef path, type_name)
         | _ -> assert false)
     | `Error err ->
         Local_diagnostics.add_error diagnostics err;
         None
   in
   let local_type : (type_name * Typedtree.type_name) option =
     match (type_name.name, local_type_env) with
     | Lident name, Some local_type_env -> (
         match Local_type.find_type local_type_env name with
         | Some type_info ->
             Some
               ( Tname_local_type type_info,
                 Tname_path
                   {
                     name = Type_path.local_type type_info.toplevel_id name;
                     kind = Type;
                     loc_ = type_name.loc_;
                   } )
         | None -> None)
     | _ -> None
   in
   match local_type with
   | Some local_type -> Some local_type
   | None -> (
       match Global_env.All_types.find_type_alias types type_name.name with
       | Some { target = Type_alias alias; _ } -> (
           match Stype.type_repr alias with
           | T_constr { type_constructor = Tuple _ as p; _ } ->
               Some
                 ( Tname_predef p,
                   Tname_alias
                     {
                       name = p;
                       kind = Type;
                       alias_ = type_name.name;
                       loc_ = type_name.loc_;
                     } )
           | T_constr { type_constructor = p; _ } -> (
               match Global_env.All_types.find_type_by_path types p with
               | Some decl ->
                   let type_name : Typedtree.type_name =
                     Tname_alias
                       {
                         name = p;
                         kind = Type;
                         alias_ = type_name.name;
                         loc_ = type_name.loc_;
                       }
                   in
                   if (not allow_private) && decl.ty_vis = Vis_priv then
                     Local_diagnostics.add_error diagnostics
                       (Errors.cannot_depend_private ~entity:"type" ~loc);
                   Some (Tname_defined decl, type_name)
               | None -> None)
           | T_builtin predef ->
               let p = Stype.tpath_of_builtin predef in
               Some
                 ( Tname_predef p,
                   Tname_alias
                     {
                       name = p;
                       kind = Type;
                       alias_ = type_name.name;
                       loc_ = type_name.loc_;
                     } )
           | Tarrow _ ->
               Local_diagnostics.add_error diagnostics
                 (Errors.type_alias_not_a_constructor
                    ~alias_name:(Longident.to_string type_name.name)
                    ~loc:type_name.loc_);
               None
           | T_blackhole -> None
           | Tparam _ | Tvar _ | T_trait _ -> assert false)
       | Some { target = Trait_alias { trait }; _ } -> (
           match Global_env.All_types.find_trait_by_path types trait with
           | Some decl ->
               let type_name : Typedtree.type_name =
                 Tname_alias
                   {
                     name = trait;
                     kind = Trait;
                     alias_ = type_name.name;
                     loc_ = type_name.loc_;
                   }
               in
               if (not allow_private) && decl.vis_ = Vis_priv then
                 Local_diagnostics.add_error diagnostics
                   (Errors.cannot_depend_private ~entity:"trait" ~loc);
               Some (Tname_trait decl, type_name)
           | None -> None)
       | None -> (
           match type_name.name with
           | Lident name -> (
               match Tvar_env.find_by_name tvar_env name with
               | Some tvar_info -> (
                   match tvar_info.typ with
                   | Tparam { index; name_ } ->
                       let type_name : Typedtree.type_name =
                         Tname_tvar { index; name_; loc_ = type_name.loc_ }
                       in
                       Some (Tname_param { index; name_ }, type_name)
                   | _ -> assert false)
               | None -> resolve_as_constr ())
           | Ldot _ -> resolve_as_constr ()))
    : (type_name * Typedtree.type_name) option)

let typing_trait_name ~types ~allow_private (trait : Syntax.type_name)
    ~diagnostics =
  let fallback_type_name () =
    (Tname_path
       {
         name = Type_path.toplevel_type ~pkg:!Basic_config.current_package "";
         kind = Trait;
         loc_ = trait.loc_;
       }
      : Typedtree.type_name)
  in
  let error ?type_name err =
    let type_name =
      match type_name with
      | None -> fallback_type_name ()
      | Some type_name -> type_name
    in
    Local_diagnostics.add_error diagnostics err;
    (None, type_name)
      [@@local]
  in
  match Global_env.All_types.find_type_alias types trait.name with
  | Some { target = Trait_alias { trait = trait_path }; _ } -> (
      match Global_env.All_types.find_trait_by_path types trait_path with
      | None -> (None, fallback_type_name ())
      | Some decl ->
          let type_name : Typedtree.type_name =
            Tname_alias
              {
                name = trait_path;
                kind = Trait;
                alias_ = trait.name;
                loc_ = trait.loc_;
              }
          in
          if (not allow_private) && decl.vis_ = Vis_priv then
            Local_diagnostics.add_error diagnostics
              (Errors.cannot_depend_private ~entity:"trait" ~loc:trait.loc_)
          else if trait.is_object then
            Local_diagnostics.add_error diagnostics
              (Errors.not_a_trait
                 ~name:
                   (("&" ^ Longident.to_string trait.name
                     : Stdlib.String.t)
                     [@merlin.hide])
                 ~loc:trait.loc_);
          (Some decl, type_name))
  | Some { target = Type_alias alias; _ } -> (
      match Stype.type_repr alias with
      | T_constr { type_constructor = p; _ } ->
          let type_name : Typedtree.type_name =
            Tname_alias
              { name = p; kind = Type; alias_ = trait.name; loc_ = trait.loc_ }
          in
          error ~type_name
            (Errors.not_a_trait
               ~name:(Basic_longident.to_string trait.name)
               ~loc:trait.loc_)
      | _ ->
          error
            (Errors.not_a_trait
               ~name:(Basic_longident.to_string trait.name)
               ~loc:trait.loc_))
  | None -> (
      match
        Global_env.All_types.find_trait types trait.name ~loc:trait.loc_
      with
      | Ok decl ->
          let type_name : Typedtree.type_name =
            Tname_path { name = decl.name; kind = Trait; loc_ = trait.loc_ }
          in
          if (not allow_private) && decl.vis_ = Vis_priv then
            Local_diagnostics.add_error diagnostics
              (Errors.cannot_depend_private ~entity:"trait" ~loc:trait.loc_)
          else if trait.is_object then
            Local_diagnostics.add_error diagnostics
              (Errors.not_a_trait
                 ~name:
                   (("&" ^ Longident.to_string trait.name
                     : Stdlib.String.t)
                     [@merlin.hide])
                 ~loc:trait.loc_);
          (Some decl, type_name)
      | Error err -> error err)

let typing_type_name ~(allow_private : bool) (type_name : Syntax.type_name)
    ~(tvar_env : Tvar_env.t) ~(types : Global_env.All_types.t)
    ~(local_type_env : Local_type.env option) ~diagnostics =
  (if type_name.is_object then
     match
       typing_trait_name ~types ~allow_private
         { type_name with is_object = false }
         ~diagnostics
     with
     | Some trait_decl, type_name ->
         Some (Tname_trait_object trait_decl, type_name)
     | None, _ -> None
   else
     typing_type_name_not_object ~allow_private ~tvar_env ~types ~local_type_env
       type_name ~diagnostics
    : (type_name * Typedtree.type_name) option)

let typing_type_decl_binders (binders : Syntax.type_decl_binder list) =
  (let go index (tvb : Syntax.type_decl_binder) =
     let name =
       match tvb.tvar_name with Some tvar_name -> tvar_name | None -> "_"
     in
     let typ : Stype.t = Tparam { index; name_ = name } in
     Tvar_env.tparam_info ~name ~typ ~constraints:[] ~loc:tvb.loc_
   in
   Tvar_env.of_list_mapi binders go
    : Tvar_env.t)

let typing_super_traits ~types ~placeholder_env ~allow_private ~self supers
    ~diagnostics =
  (match supers with
   | [] -> ([], [ self ])
   | supers ->
       let visited = Type_path.Hashset.create 17 in
       let direct_supers = Vec.empty () in
       let closure = Vec.empty () in
       Type_path.Hashset.add visited self;
       Vec.push closure self;
       let rec add_super_trait ~is_direct
           ({ tvc_trait; loc_ } : Syntax.tvar_constraint) =
         match
           Placeholder_env.typing_trait_name ~types placeholder_env
             { name = tvc_trait; is_object = false; loc_ }
         with
         | Ok (Local_trait (path, decl), trait_name) ->
             if is_direct then (
               Vec.push direct_supers trait_name;
               match decl.trait_vis with
               | Vis_priv _ when not allow_private ->
                   Local_diagnostics.add_error diagnostics
                     (Errors.cannot_depend_private ~entity:"trait" ~loc:loc_)
               | _ -> ());
             if not (Type_path.Hashset.mem visited path) then (
               Type_path.Hashset.add visited path;
               Vec.push closure path;
               Lst.iter decl.trait_supers ~f:(add_super_trait ~is_direct:false))
         | Ok (Foreign_trait decl, trait_name) ->
             if is_direct then Vec.push direct_supers trait_name;
             Lst.iter decl.closure ~f:(fun path ->
                 if not (Type_path.Hashset.mem visited path) then (
                   Type_path.Hashset.add visited path;
                   Vec.push closure path))
         | Error err -> Local_diagnostics.add_error diagnostics err
       in
       Lst.iter supers ~f:(add_super_trait ~is_direct:true);
       (Vec.to_list direct_supers, Vec.to_list closure)
    : Typedtree.type_name list * Type_path.t list)

let typing_func_def_tparam_binders ~(allow_private : bool)
    ~(types : Global_env.All_types.t) (binders : Syntax.tvar_binder list)
    ~diagnostics =
  (let constraint_names : Typedtree.type_name list ref = ref [] in
   let go index tvar_name tvar_constraints loc =
     (let name = tvar_name in
      let typ : Stype.t = Tparam { index; name_ = name } in
      let constraints =
        Lst.map tvar_constraints (fun tvc ->
            (let Syntax.{ tvc_trait; loc_ } = tvc in
             match tvc_trait with
             | Lident "Error" ->
                 {
                   trait = Type_path.Builtin.type_path_error;
                   loc_;
                   src_ = Direct;
                 }
             | _ -> (
                 let trait_decl, constraint_name =
                   typing_trait_name ~types ~allow_private
                     { name = tvc_trait; is_object = false; loc_ }
                     ~diagnostics
                 in
                 constraint_names := constraint_name :: !constraint_names;
                 match trait_decl with
                 | Some trait -> { trait = trait.name; loc_; src_ = Direct }
                 | None ->
                     let trait =
                       match tvc_trait with
                       | Lident name ->
                           Type_path.toplevel_type
                             ~pkg:!Basic_config.current_package
                             name
                       | Ldot { pkg; id } -> Type_path.toplevel_type ~pkg id
                     in
                     { trait; loc_; src_ = Direct })
              : Tvar_env.type_constraint))
      in
      let constraints = Trait_closure.compute_closure ~types constraints in
      Tvar_env.tparam_info ~name ~typ ~constraints ~loc
       : Tvar_env.tparam_info)
   in
   let go_with_constraints index (tvb : Syntax.tvar_binder) =
     go index tvb.tvar_name tvb.tvar_constraints tvb.loc_
   in
   let tvar_env = Tvar_env.of_list_mapi binders go_with_constraints in
   (tvar_env, !constraint_names)
    : Tvar_env.t * Typedtree.type_name list)

let typing_constant ~(expect_ty : Stype.t option) (c : Syntax.constant) ~loc
    ~diagnostics =
  (let overflow c =
     Local_diagnostics.add_error diagnostics (Errors.overflow ~value:c ~loc)
       [@@inline]
   in
   let check_byte_literal v ~repr =
     (if v < 0 || v > 255 then (
        overflow repr;
        (Stype.byte, C_byte { v = 0; repr = Some repr }))
      else (Stype.byte, C_byte { v; repr = Some repr })
       : Stype.t * Constant.t)
   in
   let check_short_literal v ~repr ~signed =
     (if signed then
        if v < -32768 || v > 32767 then (
          overflow repr;
          (Stype.int16, C_int { v = 0l; repr = Some repr }))
        else (Stype.int16, C_int { v = Int32.of_int v; repr = Some repr })
      else if v < 0 || v > 65536 then (
        overflow repr;
        (Stype.uint16, C_int { v = 0l; repr = Some repr }))
      else (Stype.uint16, C_int { v = Int32.of_int v; repr = Some repr })
       : Stype.t * Constant.t)
   in
   match expect_ty with
   | None -> (
       match c with
       | Const_bool b -> (Stype.bool, C_bool b)
       | Const_char lit -> (Stype.char, C_char lit.char_val)
       | Const_string lit -> (Stype.string, C_string lit.string_val)
       | Const_bytes lit ->
           ( Stype.bytes,
             C_bytes { v = lit.bytes_val; repr = Some lit.bytes_repr } )
       | Const_int c -> (
           match Int32.of_string_opt c with
           | Some v -> (Stype.int, C_int { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.int, C_int { v = 0l; repr = Some c }))
       | Const_byte c -> check_byte_literal c.byte_val ~repr:c.byte_repr
       | Const_uint c -> (
           match UInt32.of_string_opt c with
           | Some v -> (Stype.uint, C_uint { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.uint, C_uint { v = UInt32.min_int; repr = Some c }))
       | Const_int64 c -> (
           match Int64.of_string_opt c with
           | Some v -> (Stype.int64, C_int64 { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.int64, C_int64 { v = 0L; repr = Some c }))
       | Const_uint64 c -> (
           match UInt64.of_string_opt c with
           | Some v -> (Stype.uint64, C_uint64 { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.uint64, C_uint64 { v = UInt64.min_int; repr = Some c }))
       | Const_bigint c ->
           (Stype.bigint, C_bigint { v = Bigint.of_string c; repr = Some c })
       | Const_double c ->
           (Stype.double, C_double { v = float_of_string c; repr = Some c }))
   | Some expect_ty -> (
       match (Stype.type_repr expect_ty, c) with
       | _, Const_bool b -> (Stype.bool, C_bool b)
       | _, Const_char lit -> (Stype.char, C_char lit.char_val)
       | T_builtin T_bytes, Const_string { string_val; string_repr = _ } ->
           (Stype.bytes, C_bytes { v = string_val; repr = Some string_val })
       | _, Const_string lit -> (Stype.string, C_string lit.string_val)
       | _, Const_bytes lit ->
           ( Stype.bytes,
             C_bytes { v = lit.bytes_val; repr = Some lit.bytes_repr } )
       | T_builtin T_byte, Const_int c -> (
           match int_of_string_opt c with
           | Some v -> check_byte_literal v ~repr:c
           | None ->
               overflow c;
               (Stype.byte, C_int { v = 0l; repr = Some c }))
       | T_builtin T_int16, Const_int c -> (
           match int_of_string_opt c with
           | Some v -> check_short_literal v ~repr:c ~signed:true
           | None ->
               overflow c;
               (Stype.int16, C_int { v = 0l; repr = Some c }))
       | T_builtin T_uint16, Const_int c -> (
           match int_of_string_opt c with
           | Some v -> check_short_literal v ~repr:c ~signed:false
           | None ->
               overflow c;
               (Stype.uint16, C_int { v = 0l; repr = Some c }))
       | _, Const_byte c -> check_byte_literal c.byte_val ~repr:c.byte_repr
       | T_builtin T_uint, Const_int c | _, Const_uint c -> (
           match UInt32.of_string_opt c with
           | Some v -> (Stype.uint, C_uint { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.uint, C_uint { v = UInt32.min_int; repr = Some c }))
       | T_builtin T_int64, Const_int c | _, Const_int64 c -> (
           match Int64.of_string_opt c with
           | Some v -> (Stype.int64, C_int64 { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.int64, C_int64 { v = 0L; repr = Some c }))
       | T_builtin T_uint64, Const_int c | _, Const_uint64 c -> (
           match UInt64.of_string_opt c with
           | Some v -> (Stype.uint64, C_uint64 { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.uint64, C_uint64 { v = UInt64.min_int; repr = Some c }))
       | ty, Const_int c when Type.same_type ty Stype.bigint ->
           (Stype.bigint, C_bigint { v = Bigint.of_string c; repr = Some c })
       | _, Const_bigint c ->
           (Stype.bigint, C_bigint { v = Bigint.of_string c; repr = Some c })
       | T_builtin T_float, (Const_double c | Const_int c) ->
           (Stype.float, C_float { v = float_of_string c; repr = Some c })
       | T_builtin T_double, Const_int c | _, Const_double c ->
           (Stype.double, C_double { v = float_of_string c; repr = Some c })
       | _, Const_int c -> (
           match Int32.of_string_opt c with
           | Some v -> (Stype.int, C_int { v; repr = Some c })
           | None ->
               overflow c;
               (Stype.int, C_int { v = 0l; repr = Some c })))
    : Stype.t * Constant.t)

let typed_constant_to_syntax_constant = function
  | Constant.C_bool b -> Syntax.Const_bool b
  | C_char c ->
      Const_char
        { char_val = c; char_repr = Basic_uchar_utils.uchar_to_string c }
  | C_int { repr; v } -> (
      match repr with
      | Some repr -> Const_int repr
      | None -> Const_int (Int32.to_string v))
  | C_byte { repr; v } -> (
      match repr with
      | Some repr -> Const_byte { byte_val = v; byte_repr = repr }
      | None -> Const_byte { byte_val = v; byte_repr = Int.to_string v })
  | C_int64 { repr; v } -> (
      match repr with
      | Some repr -> Const_int repr
      | None -> Const_int (Int64.to_string v))
  | C_uint { repr; v } -> (
      match repr with
      | Some repr -> Const_int repr
      | None -> Const_uint (UInt32.to_string v))
  | C_uint64 { repr; v } -> (
      match repr with
      | Some repr -> Const_int repr
      | None -> Const_uint (UInt64.to_string v))
  | C_float { repr; v } ->
      Const_double
        (match repr with Some repr -> repr | None -> string_of_float v)
  | C_double { repr; v } ->
      Const_double
        (match repr with Some repr -> repr | None -> string_of_float v)
  | C_string c -> Const_string { string_val = c; string_repr = c }
  | C_bytes { v; repr } ->
      Const_bytes
        {
          bytes_val = v;
          bytes_repr = (match repr with Some x -> x | None -> v);
        }
  | C_bigint { v; repr } ->
      Const_bigint
        (match repr with Some repr -> repr | None -> Bigint.to_string v)

type func_type = Method of Syntax.typ option | Regular_func

let classify_func (ps : Syntax.parameters) =
  (match ps with
   | Positional { binder = { binder_name = "self"; _ }; ty } :: _ -> Method ty
   | _ -> Regular_func
    : func_type)

let check_stub_type ~(language : string option) (typ : Typedtree.typ)
    (global_env : Global_env.t) ~(allow_func : bool) ~(is_import_stub : bool) =
  (match language with
   | Some ("js" | "c" | "C") -> None
   | _ -> (
       let loc = Typedtree.loc_of_typ typ in
       let ty = Stype.type_repr (Typedtree_util.stype_of_typ typ) in
       let ty =
         match ty with
         | T_constr { type_constructor; tys = ty :: [] }
           when Type_path.equal type_constructor
                  Type_path.Builtin.type_path_func_ref ->
             Stype.type_repr ty
         | _ -> ty
       in
       match ty with
       | T_builtin T_unit
       | T_builtin T_int
       | T_builtin T_bool
       | T_builtin T_byte
       | T_builtin T_int16
       | T_builtin T_uint16
       | T_builtin T_char
       | T_builtin T_int64
       | T_builtin T_float
       | T_builtin T_double
       | T_builtin T_uint
       | T_builtin T_uint64 ->
           None
       | T_builtin T_string ->
           if is_import_stub && !Basic_config.target <> Wasm_gc then
             Some (Errors.invalid_stub_type loc)
           else None
       | T_builtin T_bytes ->
           if is_import_stub then Some (Errors.invalid_stub_type loc) else None
       | T_constr { type_constructor; tys = ty_elem :: [] }
         when Type_path.equal type_constructor
                Type_path.Builtin.type_path_fixedarray -> (
           if is_import_stub then Some (Errors.invalid_stub_type loc)
           else
             match language with
             | Some "wasm" -> (
                 match ty_elem with
                 | T_builtin _ -> None
                 | _ -> Some (Errors.invalid_stub_type loc))
             | _ -> (
                 match ty_elem with
                 | T_builtin builtin -> (
                     match builtin with
                     | T_int | T_int64 | T_double -> None
                     | T_unit | T_byte | T_int16 | T_uint16 | T_bytes | T_uint64
                     | T_string | T_uint | T_bool | T_char | T_float ->
                         Some (Errors.invalid_stub_type loc))
                 | _ -> Some (Errors.invalid_stub_type loc)))
       | T_constr { type_constructor = p; tys = _ } -> (
           match Global_env.find_type_by_path global_env p with
           | Some { ty_desc = Extern_type; _ } -> None
           | Some { ty_desc = Variant_type constrs; _ }
             when Lst.for_all constrs (fun constr -> constr.cs_args = []) ->
               None
           | _ -> Some (Errors.invalid_stub_type loc))
       | Tarrow { is_async; _ } when (not allow_func) || is_async ->
           Some (Errors.invalid_stub_type loc)
       | Tarrow { params_ty; ret_ty; err_ty; is_async = _ } ->
           let is_simple_stub (ty : Stype.t) =
             match Stype.type_repr ty with
             | T_builtin T_unit
             | T_builtin T_int
             | T_builtin T_bool
             | T_builtin T_byte
             | T_builtin T_int16
             | T_builtin T_uint16
             | T_builtin T_char
             | T_builtin T_int64
             | T_builtin T_uint
             | T_builtin T_uint64
             | T_builtin T_float
             | T_builtin T_double ->
                 true
             | T_builtin T_string | T_builtin T_bytes -> not is_import_stub
             | T_constr { type_constructor; tys = _ }
               when Type_path.equal type_constructor
                      Type_path.Builtin.type_path_fixedarray ->
                 not is_import_stub
             | T_constr { type_constructor = p; tys = _ } -> (
                 match Global_env.find_type_by_path global_env p with
                 | Some { ty_desc = Extern_type; _ } -> true
                 | _ -> false)
             | T_blackhole -> true
             | Tarrow _ | Tparam _ | T_trait _ -> false
             | Tvar _ -> assert false
               [@@inline]
           in
           if
             Lst.for_all params_ty is_simple_stub
             && is_simple_stub ret_ty && err_ty = None
           then None
           else Some (Errors.invalid_stub_type loc)
       | Tparam _ | T_trait _ | T_blackhole ->
           Some (Errors.invalid_stub_type loc)
       | Tvar _ -> assert false)
    : Local_diagnostics.error option)

let is_raw_string = function Syntax.Multiline_string _ -> true | _ -> false

let is_tvar (typ : Stype.t) =
  (let typ = Stype.type_repr typ in
   match typ with Tvar _ -> true | _ -> false
    : bool)

let is_trait (typ : Stype.t) =
  (let typ = Stype.type_repr typ in
   match typ with T_trait _ -> true | _ -> false
    : bool)

let validate_record ~(context : [ `Pattern | `Creation ])
    ~(expected : Typedecl_info.fields) (fields : Syntax.label list)
    ~(record_ty : Stype.t) ~(is_strict : bool) ~loc ~diagnostics =
  (let seen_labels = Hashset_string.create 17 in
   let superfluous = ref [] in
   Lst.iter fields ~f:(fun { label_name; loc_ = label_loc } ->
       if not (Hashset_string.check_add seen_labels label_name) then
         Local_diagnostics.add_error diagnostics
           (Errors.duplicate_record_field ~label:label_name ~context
              ~loc:label_loc);
       if
         not
           (Lst.exists expected (fun { field_name; _ } ->
                field_name = label_name))
       then (
         superfluous := label_name :: !superfluous;
         Local_diagnostics.add_error diagnostics
           (Errors.superfluous_field ~label:label_name
              ~ty:(Printer.type_to_string record_ty)
              ~loc:label_loc)));
   let missing =
     Lst.fold_right expected [] (fun { field_name; _ } ->
         fun acc ->
          if Hashset_string.mem seen_labels field_name then acc
          else field_name :: acc)
   in
   if missing <> [] && is_strict then
     Local_diagnostics.add_error diagnostics
       (Errors.missing_fields_in_record ~labels:missing
          ~ty:(Printer.type_to_string record_ty)
          ~context ~loc);
   !superfluous
    : string list)

let resolve_constr_or_constant ~(global_env : Global_env.t) ~tvar_env
    ~(expect_ty : Stype.t option) ~(constr : Syntax.constructor)
    ~(creating_value : bool) ~diagnostics =
  (let ({ constr_name; extra_info; loc_ = loc } : Syntax.constructor) =
     constr
   in
   let find_constr (constrs : Typedecl_info.constructor list) ~type_name =
     match
       Lst.find_first constrs (fun c -> c.constr_name = constr_name.name)
     with
     | Some constr ->
         (match constr.cs_res with
         | T_constr { type_constructor = p; _ }
           when creating_value
                && Type_path_util.is_foreign p
                && constr.cs_vis <> Read_write ->
             Local_diagnostics.add_error diagnostics
               (Errors.readonly_type ~name:constr.constr_name ~loc)
         | _ -> ());
         (Some (`Constr constr), type_name)
     | None ->
         let ty =
           match extra_info with
           | Type_name type_name -> Some (Longident.to_string type_name.name)
           | Package _ | No_extra_info -> (
               match expect_ty with
               | Some expect_ty -> Some (Printer.type_to_string expect_ty)
               | None -> None)
         in
         Local_diagnostics.add_error diagnostics
           (Errors.constr_not_found ~constr:constr_name.name ~ty
              ~loc:constr_name.loc_);
         (None, type_name)
       [@@local]
   in
   match extra_info with
   | Type_name type_name -> (
       match
         typing_type_name ~allow_private:true
           ~types:(Global_env.get_all_types global_env)
           ~tvar_env
           ~local_type_env:(Global_env.get_cur_local_type_env global_env)
           ~diagnostics type_name
       with
       | None -> (None, None)
       | Some (type_info, tast_type_name) -> (
           let not_a_variant kind =
             Local_diagnostics.add_error diagnostics
               (Errors.not_a_variant
                  ~ty:(Longident.to_string type_name.name)
                  ~kind ~loc:type_name.loc_);
             (None, Some tast_type_name)
               [@@local]
           in
           match type_info with
           | Tname_defined { ty_desc = Variant_type constrs; _ }
           | Tname_defined { ty_desc = ErrorEnum_type constrs; _ }
           | Tname_local_type { kind = Enum constrs; _ } ->
               find_constr constrs ~type_name:(Some tast_type_name)
           | Tname_defined
               { ty_desc = New_type { newtype_constr = constr; _ }; _ }
           | Tname_defined { ty_desc = Error_type constr; _ }
           | Tname_local_type
               { kind = Newtype { newtype_constr = constr; _ }; _ } ->
               find_constr [ constr ] ~type_name:(Some tast_type_name)
           | Tname_defined { ty_desc = Abstract_type | Extern_type; _ } ->
               not_a_variant "abstract"
           | Tname_defined { ty_desc = Record_type _; _ }
           | Tname_local_type { kind = Struct _; _ } ->
               not_a_variant "struct type"
           | Tname_local_type { kind = Placeholder; _ } -> assert false
           | Tname_param _ -> not_a_variant "type parameter"
           | Tname_predef (Tuple _) -> not_a_variant "tuple"
           | Tname_predef _ -> not_a_variant "predefined type"
           | Tname_trait _ -> not_a_variant "trait"
           | Tname_trait_object _ -> not_a_variant "trait object type"))
   | No_extra_info -> (
       let expect_ty = Option.map Stype.type_repr expect_ty in
       let take_info info =
         match info with
         | Ok result -> (Some result, None)
         | Error err ->
             (match expect_ty with
             | Some (Tvar { contents = Tnolink Tvar_error }) | Some T_blackhole
               ->
                 ()
             | _ -> Local_diagnostics.add_error diagnostics err);
             (None, None)
           [@@local]
       in
       match expect_ty with
       | None | Some (Tvar _) ->
           take_info
             (Global_env.find_constructor_or_constant global_env
                constr_name.name ~loc)
       | Some ty when Type.is_super_error ty ->
           take_info
             (Global_env.find_constructor_or_constant global_env
                constr_name.name ~loc)
       | Some (T_builtin _) ->
           take_info
             (Global_env.find_constructor_or_constant global_env
                constr_name.name ~loc)
       | Some ty -> (
           match
             Global_env.constrs_of_variant global_env ty ~loc ~creating_value
           with
           | Ok constrs -> find_constr constrs ~type_name:None
           | Error err ->
               Local_diagnostics.add_error diagnostics err;
               (None, None)))
   | Package pkg -> (
       match
         Pkg.find_constructor_or_constant
           (Global_env.get_pkg_tbl global_env)
           ~pkg constr_name.name ~loc
       with
       | Ok result -> (Some result, None)
       | Error err ->
           Local_diagnostics.add_error diagnostics err;
           (None, None))
    : [ `Constr of Typedecl_info.constructor | `Constant of Value_info.toplevel ]
      option
      * Typedtree.type_name option)

let resolve_constant ~(global_env : Global_env.t) ~(pkg : string option)
    ~(constant_name : Syntax.binder) ~diagnostics ~loc =
  match pkg with
  | None -> (
      match
        Typing_info.find_value
          (Global_env.get_toplevel_values global_env)
          constant_name.binder_name
      with
      | Some c -> Some c
      | None ->
          Local_diagnostics.add_error diagnostics
            (Errors.constant_not_found ~name:constant_name.binder_name
               ~loc:constant_name.loc_);
          None)
  | Some pkg ->
      Pkg.find_constant
        (Global_env.get_pkg_tbl global_env)
        ~pkg ~constant_name:constant_name.binder_name ~loc ~diagnostics

let add_binder (env : Local_env.t) (binder : Typedtree.binder) ~typ ~mut =
  (Local_env.add env binder.binder_id ~typ ~mut ~loc:binder.loc_ : Local_env.t)

let fresh_binder (b : Syntax.binder) =
  ({ binder_id = Ident.fresh b.binder_name; loc_ = b.loc_ } : Typedtree.binder)

let add_pat_binder (env : Local_env.t)
    ({ binder; binder_typ } : Typedtree.pat_binder) =
  (Local_env.add env binder.binder_id ~typ:binder_typ ~mut:false
     ~loc:binder.loc_
    : Local_env.t)

let rec add_pat_binders (env : Local_env.t) (bs : Typedtree.pat_binders) =
  (match bs with
   | [] -> env
   | b :: [] -> add_pat_binder env b
   | b1 :: b2 :: bs ->
       let env = add_pat_binder env b1 in
       let env = add_pat_binder env b2 in
       add_pat_binders env bs
    : Local_env.t)

let add_local_typing_error = Local_diagnostics.add_error

let store_error (error_option : Local_diagnostics.error option) ~diagnostics =
  match error_option with
  | None -> ()
  | Some err -> add_local_typing_error diagnostics err

let take_info_partial (x : 'a Local_diagnostics.partial_info) ~diagnostics =
  (match x with
   | Ok a -> a
   | Partial (a, errors) ->
       Basic_lst.iter errors ~f:(add_local_typing_error diagnostics);
       a
    : 'a)

let resolve_derive_alias (trait : Longident.t) =
  (match trait with
   | Lident "FromJson" ->
       Ldot { pkg = "moonbitlang/core/json"; id = "FromJson" }
   | Lident "Arbitrary" ->
       Ldot { pkg = "moonbitlang/core/quickcheck"; id = "Arbitrary" }
   | trait -> trait
    : Longident.t)

let generate_signatures_local_type_derive ~(global_env : Global_env.t)
    ~(self_typ_path : Type_path.t) ~(self_typ : Stype.t) ~(trait : Type_path.t)
    ~(tvar_env : Tvar_env.t) ~loc =
  match Global_env.find_trait_by_path global_env trait with
  | None -> ()
  | Some trait_decl ->
      let impl_params = tvar_env in
      Lst.iter trait_decl.methods ~f:(fun method_ ->
          let impl_ty =
            Poly_type.instantiate_method_decl method_ ~self:self_typ
          in
          let method_name = method_.method_name in
          if
            (not
               (Type_path.equal trait_decl.name Type_path.Builtin.trait_hash
               && method_name = "hash"))
            && not
                 (Type_path.equal trait_decl.name Type_path.Builtin.trait_show
                 && method_name = "to_string")
          then (
            let id =
              Basic_qual_ident.ext_meth ~trait:trait_decl.name
                ~self_typ:self_typ_path ~name:method_name
            in
            let method_info : Method_env.method_info =
              {
                id;
                prim = None;
                typ = impl_ty;
                pub = false;
                loc;
                doc_ = Docstring.empty;
                attrs = [];
                ty_params_ = impl_params;
                kind_ = Method_explicit_self { self_ty = self_typ };
                arity_ = method_.method_arity;
                param_names_ = [];
              }
            in
            Ext_method_env.add_method
              (Global_env.get_ext_method_env global_env)
              ~trait ~self_type:self_typ_path ~method_name method_info;
            Method_env.add_impl
              (Global_env.get_method_env global_env)
              ~type_name:self_typ_path ~method_name ~method_info)
          else
            match[@warning "-fragile-match"]
              Global_env.find_trait_method global_env ~trait
                ~type_name:self_typ_path ~method_name
            with
            | Some method_info ->
                Method_env.add_impl
                  (Global_env.get_method_env global_env)
                  ~type_name:self_typ_path ~method_name ~method_info
            | _ -> assert false)

let local_type_decl_to_type_decl (local_decl : Parsing_syntax.local_type_decl)
    base =
  ({
     tycon = local_decl.local_tycon;
     tycon_loc_ = local_decl.local_tycon_loc_;
     params = [];
     components = local_decl.local_components;
     doc_ = Docstring.empty;
     type_vis = Vis_default;
     deriving_ = [];
     loc_ = base;
     attrs = [];
   }
    : Parsing_syntax.type_decl)

let add_newtype_dep (newtype_deps : string list Basic_hash_string.t)
    (name : string) (underlying_type : Syntax.typ) =
  let result = ref [] in
  let rec go (typ : Syntax.typ) =
    match typ with
    | Ptype_name { constr_id; tys } -> (
        Lst.iter tys ~f:go;
        match constr_id.lid with
        | Lident name ->
            if not (Lst.exists !result (fun x -> x = name)) then
              result := name :: !result
        | Ldot _ -> ())
    | Ptype_arrow { ty_arg; ty_res; ty_err; is_async = _ } -> (
        Lst.iter ty_arg ~f:go;
        go ty_res;
        match ty_err with
        | Error_typ { ty = ty_err } -> go ty_err
        | No_error_typ | Default_error_typ _ -> ())
    | Ptype_tuple { tys } -> Lst.iter tys ~f:go
    | Ptype_option { ty; _ } -> go ty
    | Ptype_object _ -> ()
    | Ptype_any _ -> ()
  in
  go underlying_type;
  Basic_hash_string.add newtype_deps name !result

let check_newtype_cycle (newtype_deps : string list Basic_hash_string.t)
    (name : string) =
  (let visiting = Hashset_string.create 17 in
   let rec go (name : string) =
     if Hashset_string.mem visiting name then true
     else
       match Basic_hash_string.find_opt newtype_deps name with
       | None -> false
       | Some deps ->
           Hashset_string.add visiting name;
           let result = Lst.exists deps (fun dep -> go dep) in
           Hashset_string.remove visiting name;
           result
   in
   go name
    : bool)

let typing_local_type (local_types : Parsing_syntax.local_type_decl list)
    (toplevel_id : Basic_qual_ident.t) ~global_env ~tvar_env ~base ~diagnostics
    =
  (let newtype_deps = Basic_hash_string.create 17 in
   let typing_type typ =
     typing_type typ
       ~types:(Global_env.get_all_types global_env)
       ~tvar_env ~allow_partial:false ~allow_private:true
       ~local_type_env:(Global_env.get_cur_local_type_env global_env)
       ~diagnostics
       [@@inline]
   in
   let typing_fields (struct_ty : Stype.t) (fs : Syntax.field_decl list) =
     (let all_labels = Lst.map fs (fun f -> f.field_name.label) in
      List.split
        (Lst.mapi fs (fun i ->
             fun f ->
              let field_ty = typing_type f.field_ty in
              let field : Typedecl_info.field =
                {
                  field_name = f.field_name.label;
                  pos = i;
                  ty_field = Typedtree_util.stype_of_typ field_ty;
                  ty_record = struct_ty;
                  all_labels;
                  mut = f.field_mut;
                  label_loc_ = Rloc.to_loc ~base f.field_name.loc_;
                  loc_ = Rloc.to_loc ~base f.field_loc_;
                  ty_params_ = Tvar_env.empty;
                  vis = Read_write;
                }
              in
              let tast_field : Typedtree.field_decl =
                {
                  field_label =
                    {
                      label_name = f.field_name.label;
                      loc_ = f.field_name.loc_;
                    };
                  field_typ = field_ty;
                  field_mut = f.field_mut;
                  field_vis = Invisible;
                  field_loc_ = f.field_loc_;
                }
              in
              Global_env.add_local_field global_env field;
              (tast_field, field)))
       : Typedtree.field_decl list * Typedecl_info.field list)
   in
   let typing_constructors ~is_only_tag_enum (enum_ty : Stype.t)
       (cs : Syntax.constr_decl list) =
     (let total =
        let n = List.length cs in
        if n = 0 then Constr_info.Index_set.empty
        else Constr_info.Index_set.singleton 0 (n - 1)
      in
      List.split
        (Lst.mapi cs (fun i ->
             fun c ->
              let args, arity =
                match c.constr_args with
                | None -> ([], Fn_arity.simple 0)
                | Some args ->
                    ( args,
                      take_info_partial ~diagnostics
                        (Fn_arity.from_constr_params ~base args) )
              in
              let args_ty, tast_args =
                Lst.map_split args (fun arg ->
                    let typ = typing_type arg.cparam_typ in
                    let tast_arg : Typedtree.constr_decl_arg =
                      {
                        carg_typ = typ;
                        carg_mut = arg.cparam_mut;
                        carg_label = arg.cparam_label;
                      }
                    in
                    (Typedtree_util.stype_of_typ typ, tast_arg))
              in
              let cs_tag : Constr_info.constr_tag =
                Constr_tag_regular
                  {
                    total;
                    index = i;
                    name_ = c.constr_name.name;
                    repr_ =
                      (if is_only_tag_enum then Integer i
                       else
                         match c.constr_args with
                         | None -> Constant
                         | _ -> Non_constant);
                  }
              in
              let constructor : Typedecl_info.constructor =
                {
                  constr_name = c.constr_name.name;
                  cs_args = args_ty;
                  cs_res = enum_ty;
                  cs_tag;
                  cs_arity_ = arity;
                  cs_constr_loc_ = Rloc.to_loc ~base c.constr_name.loc_;
                  cs_loc_ = Rloc.to_loc ~base c.constr_loc_;
                  cs_vis = Read_write;
                  cs_ty_params_ = Tvar_env.empty;
                }
              in
              let tast_constructor : Typedtree.constr_decl =
                {
                  constr_name =
                    {
                      label_name = c.constr_name.name;
                      loc_ = c.constr_name.loc_;
                    };
                  constr_tag = constructor.cs_tag;
                  constr_args = tast_args;
                  constr_arity_ = arity;
                  constr_loc_ = c.constr_loc_;
                }
              in
              Global_env.add_local_constr global_env constructor;
              (tast_constructor, constructor)))
       : Typedtree.constr_decl list * Typedecl_info.constructor list)
   in
   let typing_decl (decl : Syntax.local_type_decl)
       (type_constructor : Type_path.t) =
     (match decl.local_components with
      | Ptd_record fs ->
          let struct_ty : Stype.t =
            T_constr
              {
                type_constructor;
                tys = Tvar_env.get_types tvar_env;
                generic_ = false;
                is_suberror_ = false;
              }
          in
          (match
             Basic_duplicate_check.check_duplicate_by fs (fun f ->
                 f.field_name.label)
           with
          | Some { field_name = { label = name; loc_ }; _ } ->
              Local_diagnostics.add_error diagnostics
                (Errors.field_duplicate ~name ~loc:loc_)
          | None -> ());
          let fields, fields_info = typing_fields struct_ty fs in
          (Td_record fields, Struct fields_info, false)
      | Ptd_variant cs ->
          let only_tag_enum_ = Lst.for_all cs (fun c -> c.constr_args = None) in
          let enum_ty : Stype.t =
            T_constr
              {
                type_constructor;
                tys = Tvar_env.get_types tvar_env;
                generic_ = false;
                is_suberror_ = false;
              }
          in
          (match
             Basic_duplicate_check.check_duplicate_by cs (fun c ->
                 c.constr_name.name)
           with
          | Some { constr_name = { name; loc_ }; _ } ->
              Local_diagnostics.add_error diagnostics
                (Errors.constructor_duplicate ~name ~loc:loc_)
          | None -> ());
          let constructors, constructors_info =
            typing_constructors ~is_only_tag_enum:only_tag_enum_ enum_ty cs
          in
          (Td_variant constructors, Enum constructors_info, only_tag_enum_)
      | Ptd_newtype ty ->
          let underlying_typ = typing_type ty in
          let underlying_sty = Typedtree_util.stype_of_typ underlying_typ in
          let newtype_ty : Stype.t =
            T_constr
              {
                type_constructor;
                tys = Tvar_env.get_types tvar_env;
                generic_ = false;
                is_suberror_ = false;
              }
          in
          let constr : Typedecl_info.constructor =
            {
              constr_name = decl.local_tycon;
              cs_res = newtype_ty;
              cs_args = [ underlying_sty ];
              cs_tag =
                Constr_tag_regular
                  {
                    total = Constr_info.Index_set.singleton 0 0;
                    index = 0;
                    name_ = decl.local_tycon;
                    repr_ = Non_constant;
                  };
              cs_arity_ = Fn_arity.simple 1;
              cs_constr_loc_ = Rloc.to_loc ~base decl.local_tycon_loc_;
              cs_loc_ = Rloc.to_loc ~base decl.local_tycon_loc_;
              cs_vis = Read_write;
              cs_ty_params_ = Tvar_env.empty;
            }
          in
          Global_env.add_local_constr global_env constr;
          let recursive = check_newtype_cycle newtype_deps decl.local_tycon in
          ( Td_newtype underlying_typ,
            Newtype
              {
                newtype_constr = constr;
                underlying_typ = underlying_sty;
                recursive;
              },
            false )
      | _ -> assert false
       : Typedtree.type_desc * Local_type.local_type_kind * bool)
   in
   Lst.iter local_types ~f:(fun decl ->
       match Global_env.find_local_type global_env decl.local_tycon with
       | Some t ->
           let error =
             Errors.local_type_redeclare ~name:decl.local_tycon
               ~first_loc:(Rloc.to_loc ~base t.loc_)
               ~second_loc:decl.local_tycon_loc_
           in
           Local_diagnostics.add_error diagnostics error
       | None ->
           (match decl.local_components with
           | Ptd_newtype typ ->
               add_newtype_dep newtype_deps decl.local_tycon typ
           | _ -> ());
           let placeholder : Local_type.t =
             {
               name = decl.local_tycon;
               toplevel_id = Basic_qual_ident.to_toplevel_id toplevel_id;
               kind = Placeholder;
               loc_ = decl.local_tycon_loc_;
               is_only_tag_enum = false;
               ty_params_ = Tvar_env.empty;
             }
           in
           Global_env.add_local_type global_env placeholder);
   let derive_tasks = Basic_vec.empty () in
   let type_decl_tast =
     Lst.map local_types (fun decl ->
         (let type_path =
            Type_path.local_type
              (Basic_qual_ident.to_toplevel_id toplevel_id)
              decl.local_tycon
          in
          let desc, kind, is_only_tag_enum = typing_decl decl type_path in
          let self_typ : Stype.t =
            T_constr
              {
                type_constructor = type_path;
                tys = Tvar_env.get_types tvar_env;
                generic_ = not (Tvar_env.is_empty tvar_env);
                is_suberror_ = false;
              }
          in
          Lst.iter decl.deriving_ ~f:(fun d ->
              let name = resolve_derive_alias d.type_name_.name in
              match
                Global_env.All_types.find_trait
                  (Global_env.get_all_types global_env)
                  name ~loc:d.loc_
              with
              | Ok { name = trait_path; _ } ->
                  generate_signatures_local_type_derive ~global_env
                    ~self_typ_path:type_path ~self_typ ~trait:trait_path
                    ~tvar_env
                    ~loc:(Rloc.to_loc ~base decl.local_tycon_loc_);
                  let decl = local_type_decl_to_type_decl decl base in
                  Basic_vec.push derive_tasks
                    ({ decl; type_path; directive = d; trait_path }
                      : Local_type.derive_task)
              | Error err -> Local_diagnostics.add_error diagnostics err);
          let type_info : Local_type.t =
            {
              name = decl.local_tycon;
              toplevel_id = Basic_qual_ident.to_toplevel_id toplevel_id;
              kind;
              loc_ = decl.local_tycon_loc_;
              is_only_tag_enum;
              ty_params_ = tvar_env;
            }
          in
          Global_env.update_local_type global_env type_info;
          {
            td_binder =
              { name = type_path; kind = Type; loc_ = decl.local_tycon_loc_ };
            td_params = Tvar_env.empty;
            td_desc = desc;
            td_vis = Vis_priv;
            td_loc_ = base;
            td_doc_ = Docstring.empty;
            td_deriving_ = [];
          }
           : Typedtree.type_decl))
   in
   (type_decl_tast, Basic_vec.to_list derive_tasks)
    : Typedtree.type_decl list * Local_type.derive_tasks)

type ret_annotation =
  | Annotated of (Typedtree.typ * Typedtree.error_typ)
  | Has_super_error of Rloc.t
  | No_annotation

type ret_info = {
  ret_sty : Stype.t;
  err_sty : Stype.t option;
  annotation : ret_annotation;
}

let handle_return_annotation
    (return_type : (Syntax.typ * Syntax.error_typ) option)
    ~(has_error : Rloc.t option) ~(typing_type : Syntax.typ -> Typedtree.typ) =
  (match return_type with
   | None -> (
       let ret_sty = Stype.new_type_var Tvar_normal in
       match has_error with
       | None -> { ret_sty; err_sty = None; annotation = No_annotation }
       | Some loc_ ->
           {
             ret_sty;
             err_sty = Some default_err_type;
             annotation = Has_super_error loc_;
           })
   | Some (ret_ty, err_ty) -> (
       let ret_ty = typing_type ret_ty in
       let ret_sty = Typedtree_util.stype_of_typ ret_ty in
       match err_ty with
       | Error_typ { ty = err_ty } ->
           let err_ty = typing_type err_ty in
           let err_sty = Typedtree_util.stype_of_typ err_ty in
           {
             ret_sty;
             err_sty = Some err_sty;
             annotation = Annotated (ret_ty, Error_typ { ty = err_ty });
           }
       | Default_error_typ { loc_ } ->
           {
             ret_sty;
             err_sty = Some default_err_type;
             annotation = Annotated (ret_ty, Default_error_typ { loc_ });
           }
       | No_error_typ -> (
           match has_error with
           | None ->
               {
                 ret_sty;
                 err_sty = None;
                 annotation = Annotated (ret_ty, No_error_typ);
               }
           | Some loc_ ->
               {
                 ret_sty;
                 err_sty = Some default_err_type;
                 annotation = Annotated (ret_ty, Default_error_typ { loc_ });
               }))
    : ret_info)

let report_unused_error_annotation (annotation : ret_annotation)
    (diagnostics : Local_diagnostics.t) =
  match annotation with
  | Annotated (_, Error_typ { ty }) ->
      let loc = Typedtree.loc_of_typ ty in
      Local_diagnostics.add_warning diagnostics
        { kind = Useless_error_type; loc }
  | Annotated (_, Default_error_typ { loc_ = loc }) | Has_super_error loc ->
      Local_diagnostics.add_warning diagnostics
        { kind = Useless_error_type; loc }
  | Annotated (_, No_error_typ) | No_annotation -> ()
