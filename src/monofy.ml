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
module Type_path = Basic_type_path
module Worklist = Monofy_worklist
module H = Ident.Hash
module Qual_ident = Basic_qual_ident
module Vec = Basic_vec

let monofy_typ = Type_subst.monofy_typ
let monofy_param = Type_subst.monofy_param

type stype_defs = Typing_info.stype_defs

type rename_ctx = {
  ident_table : Ident.t H.t;
  label_table : Label.t Label.Hash.t;
}

let create_rename_ctx () =
  ({ ident_table = H.create 17; label_table = Label.Hash.create 17 }
    : rename_ctx)

type monofy_ctx = {
  subst_env : Type_subst.t;
  stype_defs : Typing_info.stype_defs;
  mtype_defs : Mtype.defs;
  rename_ctx : rename_ctx;
  worklist : Worklist.t;
  monofy_env : Monofy_env.t;
}

let update_ident ident_table (b : Ident.t) =
  (match b with
   | Pident _ | Pmutable_ident _ ->
       H.find_or_update ident_table b ~update:Ident.rename
   | Pdot _ -> b
   | Plocal_method _ -> assert false
    : Ident.t)

let generate_trait_method (type_name : Type_path.t) (method_type : Stype.t)
    (method_name : string) (env : Type_subst.t) ~tparam_index
    ~(worklist : Worklist.t) ~(monofy_env : Monofy_env.t) ~(trait : Type_path.t)
    =
  match
    Monofy_env.find_method_opt monofy_env ~type_name ~method_name ~trait
  with
  | Some { prim = Some p; _ } when not (Primitive.is_intrinsic p) -> `Prim p
  | Some meth ->
      if not (Tvar_env.is_empty meth.ty_params_) then (
        let method_type', kind, ty_args =
          Poly_type.instantiate_method_no_constraint meth
        in
        (match kind with
        | Regular_method | Regular_method_qualified ->
            Ctype.unify_exn method_type method_type'
        | Method_explicit_self { self_ty } ->
            Ctype.unify_exn self_ty
              (Type_subst.monofy_typ env
                 (Tparam { index = tparam_index; name_ = "" })));
        let qual_name = meth.id in
        let tys = Arr.map ty_args (monofy_typ env) in
        let new_binder = Worklist.find_new_binder_exn worklist qual_name tys in
        `Method (new_binder, meth.prim))
      else `Method (Ident.of_qual_ident meth.id, meth.prim)
  | None -> assert false

let monofy_obj =
  object (self)
    inherit [_] Mcore.Map_core.map

    method! visit_binder ctx binder =
      update_ident ctx.rename_ctx.ident_table binder

    method visit_typ ctx typ =
      let stype = monofy_typ ctx.subst_env typ in
      Mtype.from_stype stype ~stype_defs:ctx.stype_defs
        ~mtype_defs:ctx.mtype_defs

    method visit_tag ctx tag = Tag.of_core_tag ctx.mtype_defs.ext_tags tag
    method! visit_var ctx var = update_ident ctx.rename_ctx.ident_table var

    method! visit_loop_label ctx label =
      Label.Hash.find_or_update ctx.rename_ctx.label_table label
        ~update:Label.rename

    method visit_Cexpr_var ctx id ty ty_args prim loc_ =
      let stype = monofy_typ ctx.subst_env ty in
      let mtype =
        Mtype.from_stype stype ~stype_defs:ctx.stype_defs
          ~mtype_defs:ctx.mtype_defs
      in
      match id with
      | Pident _ | Pmutable_ident _ ->
          Mcore.var ~ty:mtype (self#visit_var ctx id) ~prim
      | Pdot qual_name ->
          if Arr.is_empty ty_args then Mcore.var ~loc:loc_ ~ty:mtype id ~prim
          else
            let tys = Arr.map ty_args (monofy_typ ctx.subst_env) in
            let new_binder =
              Worklist.find_new_binder_exn ctx.worklist qual_name tys
            in
            Mcore.var ~loc:loc_ ~ty:mtype new_binder ~prim
      | Plocal_method { index; trait; method_name } -> (
          let type_name = Type_subst.monofy_param ctx.subst_env ~index in
          match
            generate_trait_method type_name stype method_name ctx.subst_env
              ~tparam_index:index ~trait ~worklist:ctx.worklist
              ~monofy_env:ctx.monofy_env
          with
          | `Prim prim -> Mcore.unsaturated_prim ~ty:mtype ~loc:loc_ prim
          | `Method (meth, prim) -> Mcore.var ~loc:loc_ ~ty:mtype meth ~prim)

    method visit_Cexpr_as ctx obj trait obj_ty loc_ =
      let monofied_obj_ty = Type_subst.monofy_typ ctx.subst_env obj_ty in
      let trait_mty =
        Mtype.from_stype (T_trait trait) ~stype_defs:ctx.stype_defs
          ~mtype_defs:ctx.mtype_defs
      in
      let type_ = Type_args.mangle_ty monofied_obj_ty in
      let self = self#visit_expr ctx obj in
      Mcore.make_object ~loc:loc_ ~methods_key:{ trait; type_ } ~ty:trait_mty
        self

    method visit_Cexpr_apply ctx func args kind ty ty_args prim loc_ =
      match func with
      | Pdot qual_name when Array.length ty_args > 0 -> (
          let ty = self#visit_typ ctx ty in
          let tys = Arr.map ty_args (monofy_typ ctx.subst_env) in
          let args = Lst.map args (self#visit_expr ctx) in
          match Core_util.specialize qual_name tys with
          | Some prim -> Mcore.prim ~loc:loc_ ~ty prim args
          | None ->
              let kind = self#visit_apply_kind ctx kind in
              let func =
                Worklist.find_new_binder_exn ctx.worklist qual_name tys
              in
              Mcore.apply ~loc:loc_ ~ty ~kind func args ~prim)
      | Plocal_method { index; trait; method_name } -> (
          let func_ty =
            match kind with
            | Normal { func_ty } | Async { func_ty } -> func_ty
            | Join -> assert false
          in
          let func_sty = monofy_typ ctx.subst_env func_ty in
          let args = Lst.map args (self#visit_expr ctx) in
          let ret_ty = self#visit_typ ctx ty in
          let type_name = monofy_param ctx.subst_env ~index in
          match
            generate_trait_method type_name func_sty method_name ctx.subst_env
              ~tparam_index:index ~trait ~worklist:ctx.worklist
              ~monofy_env:ctx.monofy_env
          with
          | `Prim prim -> Mcore.prim ~loc:loc_ ~ty:ret_ty prim args
          | `Method (func, prim) ->
              let kind = self#visit_apply_kind ctx kind in
              Mcore.apply ~loc:loc_ ~ty:ret_ty ~kind func args ~prim)
      | _ ->
          let func = self#visit_var ctx func in
          let args = Lst.map args (self#visit_expr ctx) in
          let kind = self#visit_apply_kind ctx kind in
          let ty = self#visit_typ ctx ty in
          Mcore.apply ~loc:loc_ ~ty ~kind func args ~prim

    method! visit_apply_kind ctx kind =
      match kind with
      | Join -> Join
      | Normal { func_ty } | Async { func_ty } ->
          Normal { func_ty = self#visit_typ ctx func_ty }

    method! visit_Cexpr_prim ctx prim args ty loc_ =
      let core_args = args in
      let args = Lst.map args (self#visit_expr ctx) in
      let ty = self#visit_typ ctx ty in
      match prim with
      | Perror_to_string ->
          let func_ty : Mtype.t =
            T_func { params = Lst.map args Mcore.type_of_expr; return = ty }
          in
          Mcore.apply ~loc:loc_ ~prim:None ~ty Worklist.error_to_string_binder
            args
            ~kind:(Normal { func_ty })
      | Pany_to_string -> (
          match[@warning "-fragile-match"] core_args with
          | arg :: [] -> (
              let arg_ty =
                Type_subst.monofy_typ ctx.subst_env (Core.type_of_expr arg)
              in
              match Worklist.transl_any_to_string ctx.worklist arg_ty with
              | None ->
                  let ty_string = Printer.type_to_string arg_ty in
                  Mcore.const ~loc:loc_
                    (C_string
                       (("<obj:" ^ ty_string ^ ">"
                         : Stdlib.String.t)
                         [@merlin.hide]))
              | Some (id, prim) ->
                  let func_ty : Mtype.t =
                    T_func
                      { params = Lst.map args Mcore.type_of_expr; return = ty }
                  in
                  Mcore.apply ~loc:loc_ ~prim ~ty id args
                    ~kind:(Normal { func_ty }))
          | _ -> assert false)
      | _ -> Mcore.prim ~loc:loc_ ~ty prim args
  end

let monofy_generate (prog : Core.program) (wl : Worklist.t)
    ~(mtype_defs : Mtype.defs) ~(stype_defs : stype_defs)
    ~(monofy_env : Monofy_env.t) ~(exported_functions : Exported_functions.t) =
  let generate_expr ~(rename_ctx : rename_ctx) (env : Type_subst.t)
      (expr : Core.expr) =
    (let ctx =
       {
         subst_env = env;
         stype_defs;
         mtype_defs;
         rename_ctx;
         worklist = wl;
         monofy_env;
       }
     in
     monofy_obj#visit_expr ctx expr
      : Mcore.expr)
  in
  let setup_rename_table (subtops : Core.subtop_fun_decl list) rename_table
      ~(hint_binder : Ident.t) =
    let pkg, name_hint =
      match hint_binder with
      | Pdot qual_name ->
          (Qual_ident.get_pkg qual_name, Qual_ident.base_name qual_name)
      | _ -> assert false
    in
    Lst.iter subtops ~f:(fun subtop ->
        let name =
          (name_hint ^ "." ^ Ident.to_string (Ident.rename subtop.binder)
            : Stdlib.String.t)
        in
        let new_binder =
          Ident.of_qual_ident (Basic_qual_ident.make ~pkg ~name)
        in
        H.add rename_table subtop.binder new_binder)
  in
  let generate_fn ~(rename_ctx : rename_ctx) (fn : Core.fn) (env : Type_subst.t)
      =
    ({
       params =
         Lst.map fn.params (fun { binder; ty; loc_ } ->
             (let stype = monofy_typ env ty in
              let mtype = Mtype.from_stype stype ~stype_defs ~mtype_defs in
              {
                binder = update_ident rename_ctx.ident_table binder;
                ty = mtype;
                loc_;
              }
               : Mcore.param));
       body = generate_expr env fn.body ~rename_ctx;
     }
      : Mcore.fn)
  in
  let generate_subtops (subtops : Core.subtop_fun_decl list) rename_ctx env =
    Lst.map subtops (fun subtop ->
        let func = generate_fn subtop.fn env ~rename_ctx in
        let new_binder = H.find_exn rename_ctx.ident_table subtop.binder in
        Mcore.Ctop_fn
          {
            binder = new_binder;
            func;
            export_info_ = None;
            loc_ = Loc.no_location;
          })
  in
  let generate_fun_decl (fd : Core.top_fun_decl) =
    (match fd.binder with
     | Pmutable_ident _ -> assert false
     | Pident _ -> assert false
     | Plocal_method _ -> assert false
     | Pdot qual_name -> (
         match Worklist.find_analyzed_items wl qual_name with
         | [] -> []
         | items ->
             Lst.concat_map items (fun item ->
                 (let env = Type_subst.make item.types in
                  let rename_ctx = create_rename_ctx () in
                  setup_rename_table fd.subtops rename_ctx.ident_table
                    ~hint_binder:fd.binder;
                  let subtops = generate_subtops fd.subtops rename_ctx env in
                  let func = generate_fn fd.func env ~rename_ctx in
                  Ctop_fn
                    {
                      binder = item.binder;
                      func;
                      export_info_ = None;
                      loc_ = fd.loc_;
                    }
                  :: subtops
                   : Mcore.top_item list)))
      : Mcore.top_item list)
  in
  let main = ref None in
  let globals = Vec.empty () in
  let generate (item : Core.top_item) =
    (match item with
     | Ctop_expr { expr; is_main; loc_ } ->
         let rename_ctx = create_rename_ctx () in
         let expr = generate_expr (Type_subst.empty ()) expr ~rename_ctx in
         if is_main then (
           main := Some (expr, loc_);
           [])
         else [ Ctop_expr { expr; loc_ } ]
     | Ctop_fn
         ({ binder; func; ty_params_; is_pub_ = true; loc_; subtops } as
          fun_decl)
       when Tvar_env.is_empty ty_params_ -> (
         match
           Exported_functions.get_exported_name exported_functions binder
         with
         | Some export_name ->
             assert (subtops = []);
             let rename_ctx = create_rename_ctx () in
             let func = generate_fn func (Type_subst.empty ()) ~rename_ctx in
             [ Ctop_fn { binder; func; export_info_ = Some export_name; loc_ } ]
         | None -> generate_fun_decl fun_decl)
     | Ctop_let { binder; expr; is_pub_; loc_ } -> (
         match binder with
         | Pdot qual_name -> (
             match Worklist.find_analyzed_items wl qual_name with
             | [] -> []
             | _ ->
                 let rename_ctx = create_rename_ctx () in
                 let expr =
                   generate_expr (Type_subst.empty ()) expr ~rename_ctx
                 in
                 Vec.push globals (binder, expr, is_pub_, loc_);
                 [])
         | _ -> assert false)
     | Ctop_fn fun_decl -> generate_fun_decl fun_decl
     | Ctop_stub { binder; func_stubs; params_ty; return_ty; is_pub_; loc_ } ->
         let transl_type (env : Type_subst.t) (t : Stype.t) =
           (let ty = monofy_typ env t in
            Mtype.from_stype ~stype_defs ~mtype_defs ty
             : Mtype.t)
         in
         let add_if_used () =
           match binder with
           | Pdot qual_name -> (
               match Worklist.find_analyzed_items wl qual_name with
               | [] -> []
               | instances ->
                   Lst.map instances (fun i ->
                       (let env = Type_subst.make i.types in
                        let params_ty = Lst.map params_ty (transl_type env) in
                        let return_ty =
                          Option.map (transl_type env) return_ty
                        in
                        Ctop_stub
                          {
                            binder = i.binder;
                            func_stubs;
                            params_ty;
                            return_ty;
                            export_info_ = None;
                            loc_;
                          }
                         : Mcore.top_item)))
           | _ -> assert false
             [@@inline]
         in
         if is_pub_ then
           let export_info_ =
             Exported_functions.get_exported_name exported_functions binder
           in
           if export_info_ <> None then
             let env = Type_subst.empty () in
             let params_ty = Lst.map params_ty (transl_type env) in
             let return_ty = Option.map (transl_type env) return_ty in
             [
               Ctop_stub
                 {
                   binder;
                   func_stubs;
                   export_info_;
                   params_ty;
                   return_ty;
                   loc_;
                 };
             ]
           else add_if_used ()
         else add_if_used ()
      : Mcore.top_item list)
  in
  let result = Lst.flat_map_append prog ~init:[] ~f:generate in
  let toplevels =
    Vec.map_into_list_and_append
      (Worklist.order_globals wl globals)
      ~f:(fun (binder, expr, is_pub_, loc_) ->
        (Ctop_let { binder; expr; is_pub_; loc_ } : Mcore.top_item))
      result
  in
  (toplevels, !main)

let monofy (prog : Core.program) ~(monofy_env : Monofy_env.t)
    ~(stype_defs : stype_defs) ~exported_functions =
  (let mtype_defs : Mtype.defs =
     { defs = Mtype.Id_hash.create 17; ext_tags = Basic_hash_string.create 17 }
   in
   let wl =
     Monofy_analyze.monofy_analyze prog monofy_env ~stype_defs ~mtype_defs
       ~exported_functions
   in
   let body, main =
     monofy_generate prog wl ~monofy_env ~mtype_defs ~stype_defs
       ~exported_functions
   in
   let body =
     if Worklist.get_used_error_to_string wl then
       Worklist.make_error_to_string wl ~tags:mtype_defs.ext_tags :: body
     else body
   in
   let object_methods = Monofy_worklist.get_all_object_methods wl in
   { body; main; types = mtype_defs; object_methods }
    : Mcore.t)
