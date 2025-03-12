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
module Type_path = Basic_type_path
module Lst = Basic_lst
module Syntax = Parsing_syntax

let add_error = Local_diagnostics.add_error

let generate_signatures ~(types : Global_env.All_types.t)
    ~(ext_method_env : Ext_method_env.t) ~(trait_impls : Trait_impl.t)
    (decl : Typedecl_info.t) (trait : Syntax.type_name)
    (trait_decl : Trait_decl.t) ~(diagnostics : Local_diagnostics.t) ~loc =
  let impl_params =
    Tvar_env.map decl.ty_params_ (fun tvar ->
        assert (tvar.constraints = []);
        let constraints =
          Trait_closure.compute_closure ~types
            [ { trait = trait_decl.name; loc_ = trait.loc_; src_ = Direct } ]
        in
        Tvar_env.tparam_info ~name:tvar.name ~typ:tvar.typ ~constraints
          ~loc:Rloc.no_location)
  in
  let is_suberror_ =
    match decl.ty_desc with
    | Error_type _ | ErrorEnum_type _ -> true
    | _ -> false
  in
  let self_typ : Stype.t =
    T_constr
      {
        type_constructor = decl.ty_constr;
        tys = Tvar_env.get_types impl_params;
        generic_ = not (Tvar_env.is_empty impl_params);
        is_suberror_;
      }
  in
  let pub =
    match decl.ty_vis with
    | Vis_fully_pub | Vis_readonly | Vis_default -> true
    | Vis_priv -> false
  in
  let has_error = ref false in
  let check_duplicate_impl self_type method_name =
    match
      Ext_method_env.find_method ext_method_env ~trait:trait_decl.name
        ~self_type ~method_name
    with
    | None -> true
    | Some method_info ->
        has_error := true;
        add_error diagnostics
          (Errors.derive_method_exists ~trait:trait_decl.name
             ~type_name:self_type ~method_name ~prev_loc:method_info.loc
             ~loc:trait.loc_);
        false
      [@@inline]
  in
  let add_method (method_info : Method_env.method_info) =
    match[@warning "-fragile-match"] method_info.id with
    | Qext_method { trait; self_typ; name } ->
        if check_duplicate_impl self_typ name then
          Ext_method_env.add_method ext_method_env ~trait ~self_type:self_typ
            ~method_name:name method_info
    | _ -> assert false
      [@@inline]
  in
  Lst.iter trait_decl.methods ~f:(fun meth ->
      let impl_ty = Poly_type.instantiate_method_decl meth ~self:self_typ in
      let method_name = meth.method_name in
      if
        (not
           (Type_path.equal trait_decl.name Type_path.Builtin.trait_hash
           && method_name = "hash"))
        && not
             (Type_path.equal trait_decl.name Type_path.Builtin.trait_show
             && method_name = "to_string")
      then
        add_method
          {
            id =
              Qual_ident.ext_meth ~trait:trait_decl.name
                ~self_typ:decl.ty_constr ~name:method_name;
            prim = None;
            typ = impl_ty;
            pub;
            loc;
            doc_ =
              Docstring.make ~pragmas:[] ~loc:Loc.no_location
                [ "automatically derived" ];
            attrs = [];
            ty_params_ = impl_params;
            kind_ = Method_explicit_self { self_ty = self_typ };
            arity_ = meth.method_arity;
            param_names_ = [];
          });
  if not !has_error then
    let trait = trait_decl.name in
    let type_name = decl.ty_constr in
    let impl : Trait_impl.impl =
      {
        trait;
        self_ty = self_typ;
        ty_params = impl_params;
        is_pub = pub;
        is_implicit_ = false;
        doc_ =
          Docstring.make ~pragmas:[] ~loc:Loc.no_location
            [ "automatically derived" ];
        loc_ = loc;
      }
    in
    Trait_impl.add_impl trait_impls ~trait ~type_name impl
