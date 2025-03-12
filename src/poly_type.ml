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

type typ = Stype.t

module Lst = Basic_lst
module Alist = Basic_alist

type t = { mutable constraints : (Stype.t, Tvar_env.type_constraint) Alist.t }

let make () = { constraints = Alist.nil }

let add_constraint (cenv : t) (typ : typ) (c : Tvar_env.type_constraint) =
  cenv.constraints <- Alist.cons typ c cenv.constraints

let iter (c : t) ~f = Alist.iter c.constraints f

let rec instantiate_type_aux ~(generic : bool) (type_subst : Stype.t array)
    (ty : typ) =
  (let go ty = instantiate_type_aux ~generic type_subst ty [@@inline] in
   match Stype.type_repr ty with
   | Tarrow { params_ty = ty1; ret_ty = ty2; err_ty = ty3; is_async; generic_ }
     ->
       if generic_ then
         let err_ty =
           match ty3 with None -> None | Some ty3 -> Some (go ty3)
         in
         Tarrow
           {
             params_ty = Lst.map ty1 go;
             ret_ty = go ty2;
             err_ty;
             is_async;
             generic_ = generic;
           }
       else ty
   | T_constr { type_constructor = c; tys; generic_; is_suberror_ } ->
       if generic_ then
         T_constr
           {
             type_constructor = c;
             tys = Lst.map tys go;
             generic_ = generic;
             is_suberror_;
           }
       else ty
   | Tvar _ as ty -> ty
   | Tparam { index } -> type_subst.!(index)
   | (T_trait _ | T_builtin _ | T_blackhole) as ty -> ty
    : Stype.t)

let instantiate_type type_subst ty =
  instantiate_type_aux ~generic:false type_subst ty
[@@inline]

let gen_instantiate_type type_subst ty =
  instantiate_type_aux ~generic:true type_subst ty
[@@inline]

let add_constraints ~loc ~cenv (type_subst : Stype.t array) (env : Tvar_env.t) =
  Tvar_env.iteri env (fun index ->
      fun tvar_info ->
       let ty = type_subst.!(index) in
       Lst.iter tvar_info.constraints ~f:(fun c ->
           let c' = match loc with None -> c | Some loc_ -> { c with loc_ } in
           add_constraint cenv ty c'))
[@@inline]

let instantiate_value ~(cenv : t) ?(loc : Rloc.t option)
    (ty_params : Tvar_env.t) ty =
  let type_subst = Tvar_env.make_type_subst ty_params in
  add_constraints ~loc ~cenv type_subst ty_params;
  (instantiate_type type_subst ty, type_subst)

let instantiate_method ~(cenv : t) ?(loc : Rloc.t option)
    (method_info : Method_env.method_info) =
  let ty_params = method_info.ty_params_ in
  if Tvar_env.is_empty ty_params then (method_info.typ, [||])
  else
    let type_subst = Tvar_env.make_type_subst ty_params in
    add_constraints ~loc ~cenv type_subst ty_params;
    (instantiate_type type_subst method_info.typ, type_subst)

let instantiate_impl_self_type ~(cenv : t) ~(ty_params : Tvar_env.t)
    (self_ty : Stype.t) =
  if Tvar_env.is_empty ty_params then self_ty
  else
    let type_subst = Tvar_env.make_type_subst ty_params in
    add_constraints ~loc:None ~cenv type_subst ty_params;
    instantiate_type type_subst self_ty

let instantiate_method_no_constraint (method_info : Method_env.method_info) =
  (let ty_params = method_info.ty_params_ in
   if Tvar_env.is_empty ty_params then (method_info.typ, method_info.kind_, [||])
   else
     let type_subst = Tvar_env.make_type_subst ty_params in
     let typ' = instantiate_type type_subst method_info.typ in
     let kind : Method_env.method_kind =
       match method_info.kind_ with
       | Regular_method | Regular_method_qualified -> method_info.kind_
       | Method_explicit_self { self_ty } ->
           Method_explicit_self
             { self_ty = instantiate_type type_subst self_ty }
     in
     (typ', kind, type_subst)
    : Stype.t * Method_env.method_kind * Stype.t array)

let instantiate_method_decl (md : Trait_decl.method_decl) ~(self : Stype.t) =
  (let type_subst = [| self |] in
   gen_instantiate_type type_subst md.method_typ
    : Stype.t)

let instantiate_record
    ~(ty_record : [ `Known of Stype.t | `Generic of Tvar_env.t * Stype.t ])
    (fields : Typedecl_info.fields) =
  let instantiate_field type_subst (field_info : Typedecl_info.field) =
    ({
       field_info with
       ty_record = instantiate_type type_subst field_info.ty_record;
       ty_field = instantiate_type type_subst field_info.ty_field;
     }
      : Typedecl_info.field)
  in
  match ty_record with
  | `Known ty_expect -> (
      match fields with
      | [] -> (ty_expect, [])
      | field0 :: _ when Tvar_env.is_empty field0.ty_params_ ->
          (ty_expect, fields)
      | field0 :: fields ->
          let type_subst = Tvar_env.make_type_subst field0.ty_params_ in
          let field0' = instantiate_field type_subst field0 in
          let fields' = Lst.map fields (instantiate_field type_subst) in
          Ctype.unify_exn field0'.ty_record ty_expect;
          (ty_expect, field0' :: fields'))
  | `Generic (ty_params, ty_record) ->
      if Tvar_env.is_empty ty_params then (ty_record, fields)
      else
        let type_subst = Tvar_env.make_type_subst ty_params in
        let ty_record' = instantiate_type type_subst ty_record in
        let fields' = Lst.map fields (instantiate_field type_subst) in
        (ty_record', fields')

let instantiate_field (field : Typedecl_info.field) =
  if Tvar_env.is_empty field.ty_params_ then (field.ty_record, field.ty_field)
  else
    let type_subst = Tvar_env.make_type_subst field.ty_params_ in
    let ty_record' = instantiate_type type_subst field.ty_record in
    let ty_field' = instantiate_type type_subst field.ty_field in
    (ty_record', ty_field')

let instantiate_constr (constr : Typedecl_info.constructor) =
  if Tvar_env.is_empty constr.cs_ty_params_ then (constr.cs_res, constr.cs_args)
  else
    let type_subst = Tvar_env.make_type_subst constr.cs_ty_params_ in
    let ty_res = instantiate_type type_subst constr.cs_res in
    let ty_args =
      Lst.map constr.cs_args (fun ty -> instantiate_type type_subst ty)
    in
    (ty_res, ty_args)

let instantiate_alias (alias : Type_alias.t) (ty_args : Stype.t list) =
  let ty =
    match alias.target with
    | Type_alias ty -> ty
    | Trait_alias { trait } -> T_trait trait
  in
  if Tvar_env.is_empty alias.ty_params then ty
  else gen_instantiate_type (Array.of_list ty_args) ty
