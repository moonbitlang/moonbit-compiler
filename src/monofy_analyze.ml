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
module Arr = Basic_arr
module Ident = Basic_core_ident
module Worklist = Monofy_worklist
module Q = Qual_ident_tbl
module Type_path = Basic_type_path

let add_dependency (wl : Worklist.t) (src : Ident.t option) (tgt : Ident.t) =
  match src with None -> () | Some src -> Worklist.add_dependency wl src tgt

let monofy_analyze (prog : Core.program) (monofy_env : Monofy_env.t) ~stype_defs
    ~mtype_defs ~exported_functions =
  (let (wl : Worklist.t) = Monofy_worklist.make () in
   let analyze_ident ~(binder : Ident.t option) (env : Type_subst.t)
       (ty_args : Type_args.t) (ty : Stype.t) (id : Ident.t) =
     (match id with
      | Pident _ | Pmutable_ident _ -> ()
      | Pdot qual_name ->
          let tys = Arr.map ty_args (Type_subst.monofy_typ env) in
          if not (Core_util.specializable qual_name tys) then
            let new_id = Worklist.add_value_if_not_exist wl qual_name tys in
            add_dependency wl binder new_id
      | Plocal_method { index; name_; trait; method_name } -> (
          let type_name = Type_subst.monofy_param env ~index in
          let method_type = Type_subst.monofy_typ env ty in
          match
            Monofy_env.find_method_opt monofy_env ~type_name ~method_name ~trait
          with
          | None -> ()
          | Some meth ->
              let method_type', kind, ty_args =
                Poly_type.instantiate_method_no_constraint meth
              in
              (match kind with
              | Regular_method | Regular_method_qualified ->
                  Ctype.unify_exn method_type method_type'
              | Method_explicit_self { self_ty } ->
                  Ctype.unify_exn self_ty
                    (Type_subst.monofy_typ env (Tparam { index; name_ })));
              let qual_name = meth.id in
              let tys = Arr.map ty_args (Type_subst.monofy_typ env) in
              let new_id = Worklist.add_value_if_not_exist wl qual_name tys in
              add_dependency wl binder new_id)
       : unit)
   in
   let analyze_object ~(binder : Ident.t option) ~(self_typ : Stype.t)
       ~(trait : Type_path.t) =
     (let self_typ = Stype.type_repr self_typ in
      let type_name = Stype.extract_tpath_exn self_typ in
      let type_ = Type_args.mangle_ty self_typ in
      if not (Worklist.has_object wl ~trait ~type_) then
        let trait_def =
          match trait with
          | Toplevel { pkg; id } ->
              let types = Basic_hash_string.find_exn stype_defs pkg in
              Typing_info.find_trait_exn types id
          | _ -> assert false
        in
        let methods =
          Lst.map trait_def.closure_methods (fun (actual_trait, meth_decl) ->
              (let method_name = meth_decl.method_name in
               let method_type =
                 Poly_type.instantiate_method_decl meth_decl ~self:self_typ
               in
               let method_mty =
                 Mtype.from_stype method_type ~stype_defs ~mtype_defs
               in
               match[@warning "-fragile-match"]
                 Monofy_env.find_method_opt monofy_env ~type_name
                   ~trait:actual_trait ~method_name
               with
               | Some method_info ->
                   let method_type', kind, ty_args =
                     Poly_type.instantiate_method_no_constraint method_info
                   in
                   (match kind with
                   | Regular_method | Regular_method_qualified ->
                       Ctype.unify_exn method_type method_type'
                   | Method_explicit_self { self_ty } ->
                       Ctype.unify_exn self_ty self_typ);
                   Ctype.unify_exn method_type method_type';
                   let method_id =
                     match method_info.prim with
                     | Some (Pintrinsic _) | None ->
                         let method_id =
                           Worklist.add_value_if_not_exist wl method_info.id
                             ty_args
                         in
                         add_dependency wl binder method_id;
                         method_id
                     | Some _ -> Ident.of_qual_ident method_info.id
                   in
                   {
                     method_id;
                     method_prim = method_info.prim;
                     method_ty = method_mty;
                   }
               | _ -> assert false
                : Object_util.object_method_item))
        in
        let self_ty = Mtype.from_stype self_typ ~stype_defs ~mtype_defs in
        Worklist.add_object_methods wl ~trait ~type_ ~self_ty ~methods
       : unit)
   in
   let analyze_expr ~(binder : Ident.t option) (env : Type_subst.t)
       (e : Core.expr) =
     let obj =
       object (self)
         inherit [_] Core.Iter.iter as super

         method! visit_Cexpr_var () id ty ty_args _prim _loc_ =
           analyze_ident ~binder env ty_args ty id

         method! visit_Cexpr_apply () func args kind ty ty_args prim loc_ =
           (match kind with
           | Normal { func_ty } | Async { func_ty } ->
               analyze_ident ~binder env ty_args func_ty func
           | Join -> ());
           super#visit_Cexpr_apply () func args kind ty ty_args prim loc_

         method! visit_Cexpr_as () expr trait obj_ty _loc_ =
           self#visit_expr () expr;
           let monofied_obj_ty = Type_subst.monofy_typ env obj_ty in
           analyze_object ~binder ~self_typ:monofied_obj_ty ~trait

         method! visit_Cexpr_prim () prim args ty loc_ =
           (match prim with
           | Perror_to_string -> Worklist.set_used_error_to_string wl
           | Pany_to_string -> (
               match[@warning "-fragile-match"] args with
               | arg :: [] ->
                   let arg_ty =
                     Type_subst.monofy_typ env (Core.type_of_expr arg)
                   in
                   Worklist.add_any_to_string wl monofy_env arg_ty
               | _ -> assert false)
           | _ -> ());
           super#visit_Cexpr_prim () prim args ty loc_

         method! visit_constr_tag () tag =
           match tag with
           | Constr_tag_regular _ -> ()
           | Extensible_tag _ -> Worklist.add_error_type wl tag
       end
     in
     obj#visit_expr () e
   in
   let def_tbl : Core.expr list Q.t = Q.create 17 in
   let rec analyze_loop () =
     let todo_items = Worklist.get_todo_items_and_mark_as_analyzed wl in
     match todo_items with
     | [] -> ()
     | items ->
         Lst.iter items ~f:(fun { types; binder; old_binder } ->
             match Q.find_opt def_tbl old_binder with
             | Some expr ->
                 let env = Type_subst.make types in
                 Lst.iter expr ~f:(analyze_expr ~binder:(Some binder) env)
             | None -> ());
         analyze_loop ()
   in
   let collect (item : Core.top_item) =
     match item with
     | Ctop_expr { expr; is_main = _ } ->
         analyze_expr ~binder:None (Type_subst.empty ()) expr
     | Ctop_fn
         {
           binder;
           func = { body = expr; _ };
           ty_params_;
           is_pub_ = true;
           subtops;
         }
       when Exported_functions.is_exported_ident exported_functions binder
            && Tvar_env.is_empty ty_params_ ->
         assert (subtops = []);
         analyze_expr ~binder:(Some binder) (Type_subst.empty ()) expr
     | Ctop_let { binder; expr } -> (
         match binder with
         | Pdot qual_name -> Q.add def_tbl qual_name [ expr ]
         | _ -> assert false)
     | Ctop_fn { binder; func = { body = expr; _ }; subtops } -> (
         match binder with
         | Pdot qual_name ->
             Q.add def_tbl qual_name
               (expr :: Lst.map subtops (fun subtop -> subtop.fn.body))
         | _ -> assert false)
     | Ctop_stub _ -> ()
   in
   Lst.iter prog ~f:collect;
   analyze_loop ();
   if Worklist.get_used_error_to_string wl then (
     Worklist.add_error_to_string wl ~monofy_env;
     analyze_loop ());
   wl
    : Worklist.t)
