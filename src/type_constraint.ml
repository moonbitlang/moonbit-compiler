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


module Type_path = Basic_type_path
module Ident = Basic_ident
module Lst = Basic_lst
module Cache = Constraint_cache
module Operators = Parsing_operators
module Vec = Basic_vec

type dot_source = Dot_src_infix of Operators.operator_info | Dot_src_direct

type trait_resolution_failure_reason =
  | Method_missing of string
  | Method_type_mismatch of {
      method_name : string;
      expected : string;
      actual : string;
    }
  | Impl_self_type_mismatch of { expected : string; actual : string }
  | No_trait_bound
  | Pkg_not_imported of { pkg : string }
  | Not_an_error_type
  | Trait_not_imported
  | Readonly_trait of { trait_vis : Typedecl_info.visibility }
  | Unsolved_tvar of { impl_loc : Loc.t }

type indirect_trait_source =
  | Poly_method of (string * Stype.t * Loc.t)
  | Impl of { type_name : Type_path.t; trait : Type_path.t; loc : Loc.t }
  | Super_trait of Type_path.t
  | Static_assert of string

type trait_resolution_error = {
  self_ty : Stype.t;
  trait : Type_path.t;
  reasons : trait_resolution_failure_reason list;
  required_by : indirect_trait_source list;
}

let cannot_resolve_method ~ty ~name ~src ~hint ~loc =
  match src with
  | Dot_src_direct -> Errors.cannot_resolve_method ~ty ~name ~hint ~loc
  | Dot_src_infix op ->
      Errors.cannot_resolve_infix_op ~method_name:op.method_name
        ~op_name:(Operators.display_name_of_op op)
        ~ty ~loc

let ambiguous_method ~ty ~method_name ~src ~first ~second ~loc =
  let first = Type_path_util.name first in
  let second = Type_path_util.name second in
  let label =
    match src with
    | Dot_src_infix op ->
        ("Operator \"" ^ op.op_name ^ "\"" : Stdlib.String.t) [@merlin.hide]
    | Dot_src_direct ->
        ("Method " ^ method_name : Stdlib.String.t) [@merlin.hide]
  in
  Errors.ambiguous_trait_method ~label ~ty ~first ~second ~loc

let cannot_resolve_trait (err : trait_resolution_error) ~loc =
  let print_require_list buf reqs =
    let print_requirement buf req =
      match req with
      | Poly_method (method_name, ty, loc)
        when Loc.compare loc Loc.no_location = 0 ->
          Printf.bprintf buf "required by builtin method %s of type %s"
            method_name
            (Printer.type_to_string ty)
      | Poly_method (method_name, ty, loc) ->
          Printf.bprintf buf "required by method %s of type %s at %s"
            method_name
            (Printer.type_to_string ty)
            (Loc.to_string loc)
      | Impl { trait; type_name; loc } ->
          Printf.bprintf buf "required by `impl` of %s for type %s at %s"
            (Type_path_util.name trait)
            (Type_path_util.name type_name)
            (Loc.to_string loc)
      | Super_trait trait ->
          Printf.bprintf buf "required by trait %s" (Type_path_util.name trait)
      | Static_assert msg -> Printf.bprintf buf "required by %s" msg
        [@@inline]
    in
    match List.rev reqs with
    | [] -> ()
    | first :: rest ->
        Printf.bprintf buf "\n  note: this constraint is ";
        print_requirement buf first;
        Lst.iter rest ~f:(fun req ->
            Printf.bprintf buf "\n  - which is ";
            print_requirement buf req)
  in
  let { self_ty; reasons; trait = trait_path; required_by } = err in
  let buf = Buffer.create 150 in
  (if Type_path.equal trait_path Type_path.Builtin.type_path_error then
     Printf.bprintf buf "Type %s is not an error type"
       (Printer.type_to_string self_ty)
   else
     let trait = Type_path_util.name trait_path in
     Printf.bprintf buf "Type %s does not implement trait %s"
       (Printer.type_to_string self_ty)
       trait;
     let print_reason buf reason =
       match reason with
       | Method_missing m -> Printf.bprintf buf "method %s is missing" m
       | Method_type_mismatch { method_name; expected; actual } ->
           Printf.bprintf buf "method %s has incorrect type\n" method_name;
           Printf.bprintf buf "        has type : %s\n        wanted   : %s"
             actual expected
       | Impl_self_type_mismatch { expected; actual } ->
           Buffer.add_string buf "impl of the trait is for a different type\n";
           Printf.bprintf buf "        impl is for: %s\n        wanted     : %s"
             actual expected
       | No_trait_bound ->
           Printf.bprintf buf "%s is missing from its declaration" trait
       | Pkg_not_imported { pkg } ->
           Printf.bprintf buf
             "package @%s is not imported, so its methods cannot be used." pkg
       | Not_an_error_type -> ()
       | Readonly_trait { trait_vis } ->
           let vis =
             match trait_vis with
             | Vis_default -> "abstract"
             | Vis_readonly -> "readonly"
             | Vis_priv | Vis_fully_pub -> assert false
           in
           Printf.bprintf buf
             "this trait is %s, it can only be implemented explicitly in @%s."
             vis
             (Type_path.get_pkg trait_path)
       | Trait_not_imported ->
           Buffer.add_string buf
             "definition of the trait is unknown, due to its package not \
              imported."
       | Unsolved_tvar { impl_loc } ->
           Printf.bprintf buf
             "cannot decide value for type parameters of implementation at %s."
             (Loc.to_string impl_loc)
     in
     (match reasons with
     | only_reason :: [] ->
         Printf.bprintf buf ": ";
         print_reason buf only_reason
     | _ ->
         Printf.bprintf buf ":";
         Lst.iter reasons ~f:(fun reason ->
             Printf.bprintf buf "\n  - ";
             print_reason buf reason));
     print_require_list buf required_by);
  let message = Buffer.contents buf in
  Errors.cannot_resolve_trait ~message ~loc

type method_info =
  | Known_method of Method_env.method_info
  | Promised_method of {
      method_id : Ident.t;
      method_ty : Stype.t;
      method_arity : Fn_arity.t;
      prim : Primitive.prim option;
    }

let find_method_from_constraints ~global_env
    (cs : Tvar_env.type_constraint list) method_name =
  (Lst.fold_right cs [] (fun { trait } ->
       fun acc ->
        match Global_env.find_trait_by_path global_env trait with
        | None -> acc
        | Some { vis_ = Vis_default; _ } when Type_path_util.is_foreign trait ->
            acc
        | Some trait_decl -> (
            match
              Lst.find_opt trait_decl.methods (fun meth_decl ->
                  if meth_decl.method_name = method_name then
                    Some (trait, meth_decl)
                  else None)
            with
            | Some x -> x :: acc
            | None -> acc))
    : (Type_path.t * Trait_decl.method_decl) list)

let resolve_method_by_type_name (type_name : Typedtree.type_name)
    (method_name : string) ~loc ~tvar_env ~global_env =
  let type_name_to_string type_name =
    match (type_name : Typedtree.type_name) with
    | Tname_tvar { index = _; name_ } -> name_
    | Tname_path { name = p; kind = Trait } ->
        "&"
        ^ Type_path.short_name
            ~cur_pkg_name:(Some !Basic_config.current_package) p
    | Tname_path { name = p; kind = _ } | Tname_alias { name = p; kind = _ } ->
        Type_path.short_name ~cur_pkg_name:(Some !Basic_config.current_package)
          p
  in
  let error () =
    Error
      (cannot_resolve_method
         ~ty:(type_name_to_string type_name)
         ~name:method_name ~src:Dot_src_direct ~hint:`No_hint ~loc)
  in
  match type_name with
  | Tname_tvar { index; name_ } -> (
      let constraints =
        (Tvar_env.find_by_index_exn tvar_env index).constraints
      in
      match
        find_method_from_constraints ~global_env constraints method_name
      with
      | (trait, method_info) :: [] ->
          let method_ty =
            Poly_type.instantiate_method_decl method_info
              ~self:(Tparam { index; name_ })
          in
          Ok
            (Promised_method
               {
                 method_id =
                   Ident.of_local_method ~index ~tvar_name:name_ ~trait
                     method_name;
                 method_ty;
                 method_arity = method_info.method_arity;
                 prim = None;
               })
      | (trait1, _) :: (trait2, _) :: _ ->
          Error
            (ambiguous_method ~ty:name_ ~method_name ~src:Dot_src_direct
               ~first:trait1 ~second:trait2 ~loc)
      | _ -> error ())
  | Tname_path { name = p; kind = _ } | Tname_alias { name = p; kind = _ } -> (
      match Global_env.find_dot_method global_env ~type_name:p ~method_name with
      | method_info :: [] -> Ok (Known_method method_info)
      | method1 :: method2 :: _ -> (
          match[@warning "-fragile-match"] method1.id with
          | Qext_method { trait = trait1; _ } -> (
              match[@warning "-fragile-match"] method2.id with
              | Qext_method { trait = trait2; _ } ->
                  Error
                    (ambiguous_method ~ty:(Type_path_util.name p) ~method_name
                       ~src:Dot_src_direct ~first:trait1 ~second:trait2 ~loc)
              | _ -> assert false)
          | _ -> assert false)
      | [] -> error ())

let resolve_method_by_type (ty : Stype.t) (method_name : string) ~loc ~src
    ~tvar_env ~global_env =
  let error ~hint =
    Error
      (cannot_resolve_method
         ~ty:(Printer.type_to_string ty)
         ~name:method_name ~src ~hint ~loc)
  in
  let resolve_by_type_path ~is_trait p =
    match Global_env.find_dot_method global_env ~type_name:p ~method_name with
    | method_info :: [] -> Ok (Known_method method_info)
    | method1 :: method2 :: _ -> (
        match[@warning "-fragile-match"] method1.id with
        | Qext_method { trait = trait1; _ } -> (
            match[@warning "-fragile-match"] method2.id with
            | Qext_method { trait = trait2; _ } ->
                Error
                  (ambiguous_method ~ty:(Type_path_util.name p) ~method_name
                     ~src:Dot_src_direct ~first:trait1 ~second:trait2 ~loc)
            | _ -> assert false)
        | _ -> assert false)
    | [] ->
        let pkg = Type_path.get_pkg p in
        if
          pkg = !Basic_config.current_package
          ||
          match Pkg.find_pkg_opt (Global_env.get_pkg_tbl global_env) ~pkg with
          | Some _ -> true
          | _ -> false
        then
          if is_trait then
            match Global_env.find_trait_by_path global_env p with
            | Some { vis_ = Vis_default; _ } when Type_path_util.is_foreign p ->
                Error
                  (Errors.cannot_use_method_of_abstract_trait ~trait:p
                     ~method_name ~loc)
            | _ -> error ~hint:`Trait
          else
            match Global_env.find_type_by_path global_env p with
            | Some { ty_desc = Record_type { fields }; _ }
              when Lst.exists fields (fun { field_name; _ } ->
                       field_name = method_name) ->
                error ~hint:`Record
            | _ -> error ~hint:`No_hint
        else
          let type_name =
            (if is_trait then "&" else "") ^ Type_path_util.name p
          in
          Error
            (Errors.pkg_not_imported ~name:pkg
               ~action:
                 (("call method of type " ^ type_name
                   : Stdlib.String.t)
                   [@merlin.hide])
               ~loc)
  in
  let ty = Stype.type_repr ty in
  match ty with
  | Tparam { index; name_ = ty_name } -> (
      let constraints =
        (Tvar_env.find_by_index_exn tvar_env index).constraints
      in
      match
        find_method_from_constraints ~global_env constraints method_name
      with
      | (trait, method_info) :: [] ->
          Ok
            (Promised_method
               {
                 method_id =
                   Ident.of_local_method ~index ~tvar_name:ty_name ~trait
                     method_name;
                 method_ty =
                   Poly_type.instantiate_method_decl method_info ~self:ty;
                 method_arity = method_info.method_arity;
                 prim = None;
               })
      | (trait1, _) :: (trait2, _) :: _ ->
          Error
            (ambiguous_method ~ty:ty_name ~method_name ~src:Dot_src_direct
               ~first:trait1 ~second:trait2 ~loc)
      | _ -> error ~hint:`No_hint)
  | T_trait trait -> resolve_by_type_path ~is_trait:true trait
  | T_constr { type_constructor = p; _ } ->
      resolve_by_type_path ~is_trait:false p
  | Tvar { contents = Tnolink Tvar_error } | T_blackhole ->
      Error Errors.swallow_error
  | Tvar _ | Tarrow _ -> error ~hint:`No_hint
  | T_builtin b ->
      resolve_by_type_path ~is_trait:false (Stype.tpath_of_builtin b)

let solve_constraints (cenv : Poly_type.t) ~tvar_env ~global_env ~diagnostics =
  let hty_cache = Hashed_type.make_cache () in
  let cache : trait_resolution_error Cache.t = Cache.create 17 in
  let exception Resolve_failure of trait_resolution_error in
  let solve_with_regular_methods ~self_ty ~type_name ~trait ~root_loc =
    match Global_env.find_trait_by_path global_env trait with
    | None -> ([ Trait_not_imported ], [])
    | Some { vis_ = (Vis_default | Vis_readonly) as trait_vis; _ }
      when Type_path_util.is_foreign trait ->
        ([ Readonly_trait { trait_vis } ], [])
    | Some trait_decl -> (
        let new_constraints = ref [] in
        let failures = Vec.empty () in
        let regular_impls = Vec.empty () in
        let () =
          Lst.iter trait_decl.methods ~f:(fun method_decl ->
              let method_name = method_decl.method_name in
              let check_method_type actual_arity actual =
                let exception Arity_mismatch in
                let expected =
                  Poly_type.instantiate_method_decl method_decl ~self:self_ty
                in
                try
                  Ctype.unify_exn expected actual;
                  if not (Fn_arity.equal actual_arity method_decl.method_arity)
                  then raise_notrace Arity_mismatch;
                  None
                with _ ->
                  Some
                    (Method_type_mismatch
                       {
                         method_name;
                         expected =
                           Printer.toplevel_function_type_to_string
                             ~arity:method_decl.method_arity expected;
                         actual =
                           Printer.toplevel_function_type_to_string
                             ~arity:actual_arity actual;
                       })
                  [@@inline]
              in
              match
                Global_env.find_trait_method global_env ~trait ~type_name
                  ~method_name
              with
              | Some method_info -> (
                  let aux_cenv = Poly_type.make () in
                  let err_opt =
                    let method_ty', _ =
                      Poly_type.instantiate_method ~cenv:aux_cenv method_info
                    in
                    check_method_type method_info.arity_ method_ty'
                  in
                  match err_opt with
                  | Some err -> Vec.push failures err
                  | None ->
                      (match method_info.id with
                      | Qmethod _ -> Vec.push regular_impls method_name
                      | Qregular _ | Qregular_implicit_pkg _ | Qext_method _ ->
                          ());
                      Poly_type.iter aux_cenv ~f:(fun a ->
                          fun b ->
                           let new_constraint_source =
                             Poly_method (method_name, self_ty, method_info.loc)
                           in
                           new_constraints :=
                             (new_constraint_source, a, b) :: !new_constraints))
              | None -> Vec.push failures (Method_missing method_name))
        in
        if not (Vec.is_empty regular_impls) then
          Local_diagnostics.add_warning diagnostics
            {
              kind =
                Implement_trait_with_method
                  {
                    trait = Type_path_util.name trait;
                    typ = Printer.type_to_string self_ty;
                    methods = Vec.to_list regular_impls;
                  };
              loc = root_loc;
            };
        let failures = Vec.to_list failures in
        let pkg = Type_path.get_pkg type_name in
        match failures with
        | _ :: _
          when pkg <> !Basic_config.current_package
               &&
               match
                 Pkg.find_pkg_opt (Global_env.get_pkg_tbl global_env) ~pkg
               with
               | None -> true
               | _ -> false ->
            ([ Pkg_not_imported { pkg } ], !new_constraints)
        | _ -> (failures, !new_constraints))
  in
  let do_solve (self_ty : Stype.t) ({ trait } : Tvar_env.type_constraint)
      ~root_loc =
    let self_ty = Stype.type_repr self_ty in
    let solve_type_path type_name =
      match Global_env.find_trait_impl global_env ~trait ~type_name with
      | None -> solve_with_regular_methods ~self_ty ~type_name ~trait ~root_loc
      | Some impl ->
          let aux_cenv = Poly_type.make () in
          let actual_self_ty =
            Poly_type.instantiate_impl_self_type ~cenv:aux_cenv
              ~ty_params:impl.ty_params impl.self_ty
          in
          if Ctype.try_unify self_ty actual_self_ty then (
            let new_constraints = ref [] in
            Poly_type.iter aux_cenv ~f:(fun a ->
                fun b ->
                 let new_constraint_source =
                   Impl { trait; type_name; loc = impl.loc_ }
                 in
                 new_constraints :=
                   (new_constraint_source, a, b) :: !new_constraints);
            ([], !new_constraints))
          else
            let err =
              Impl_self_type_mismatch
                {
                  expected = Printer.type_to_string self_ty;
                  actual = Printer.type_to_string actual_self_ty;
                }
            in
            ([ err ], [])
    in
    match self_ty with
    | _ when Type_path.equal trait Type_path.Builtin.type_path_error ->
        if Type.is_error_type ~tvar_env self_ty then ([], [])
        else ([ Not_an_error_type ], [])
    | Tparam { index } -> (
        let constraints =
          (Tvar_env.find_by_index_exn tvar_env index).constraints
        in
        if
          Lst.exists constraints (fun { trait = trait' } ->
              Type_path.equal trait' trait)
        then ([], [])
        else
          match Global_env.find_trait_by_path global_env trait with
          | None | Some { methods = []; _ } -> ([], [])
          | Some _ -> ([ No_trait_bound ], []))
    | T_trait obj_trait when Type_path.equal obj_trait trait -> ([], [])
    | T_trait obj_trait -> solve_type_path obj_trait
    | T_constr { type_constructor = p; tys = _ } -> solve_type_path p
    | T_builtin b -> solve_type_path (Stype.tpath_of_builtin b)
    | _ -> (
        match Global_env.find_trait_by_path global_env trait with
        | None -> ([], [])
        | Some trait_decl ->
            ( Lst.map trait_decl.methods (fun { method_name; _ } ->
                  Method_missing method_name),
              [] ))
  in
  let rec solve self_ty ({ trait; src_ } as c : Tvar_env.type_constraint)
      ~root_loc =
    let hty = Hashed_type.of_stype hty_cache self_ty in
    match Cache.find_opt cache (hty, trait) with
    | Some Success -> ()
    | Some (Failure err) -> raise_notrace (Resolve_failure err)
    | None -> (
        Cache.add cache (hty, trait) Success;
        match do_solve self_ty c ~root_loc with
        | (_ :: _ as reasons), _ ->
            let err =
              {
                self_ty;
                trait;
                reasons;
                required_by =
                  (match src_ with
                  | Super_traits supers ->
                      List.rev_map (fun t -> Super_trait t) supers
                  | Static_assert msg -> [ Static_assert msg ]
                  | Direct -> []);
              }
            in
            Cache.add cache (hty, trait) (Failure err);
            raise_notrace (Resolve_failure err)
        | [], new_constraints ->
            let parent_self_ty = self_ty in
            let parent_trait = trait in
            Lst.iter new_constraints ~f:(fun (new_src, self_ty, c) ->
                try solve self_ty c ~root_loc with
                | Hashed_type.Unresolved_type_variable -> (
                    match[@warning "-fragile-match"] new_src with
                    | Poly_method (_, _, loc) | Impl { loc; _ } ->
                        raise_notrace
                          (Resolve_failure
                             {
                               self_ty = parent_self_ty;
                               trait = parent_trait;
                               reasons = [ Unsolved_tvar { impl_loc = loc } ];
                               required_by = [];
                             })
                    | _ -> assert false)
                | Resolve_failure err ->
                    let err =
                      let required_by =
                        match src_ with
                        | Direct -> new_src :: err.required_by
                        | Static_assert msg ->
                            Static_assert msg :: new_src :: err.required_by
                        | Super_traits supers ->
                            Lst.rev_map_append supers
                              (Super_trait trait :: new_src :: err.required_by)
                              (fun t -> Super_trait t)
                      in
                      { err with required_by }
                    in
                    Cache.add cache (hty, trait) (Failure err);
                    raise_notrace (Resolve_failure err));
            Cache.add cache (hty, trait) Success)
  in
  Poly_type.iter cenv ~f:(fun self_ty ->
      fun c ->
       try solve self_ty c ~root_loc:c.loc_
       with Resolve_failure err ->
         Local_diagnostics.add_error diagnostics
           (cannot_resolve_trait err ~loc:c.loc_))
