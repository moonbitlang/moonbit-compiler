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
module Type_path = Basic_type_path

type typ = Stype.t

include struct
  let _ = fun (_ : typ) -> ()
  let sexp_of_typ = (Stype.sexp_of_t : typ -> S.t)
  let _ = sexp_of_typ
end

let type_repr = Stype.type_repr
let type_var_list arity kind = Lst.init arity (fun _ -> Stype.new_type_var kind)

let check_occur (v : typ) (ty : typ) =
  let rec go (ty : typ) =
    match ty with
    | Tarrow { params_ty = ty1; ret_ty = ty2; err_ty = ty3; is_async = _ } -> (
        List.exists go ty1 || go ty2
        || match ty3 with Some ty3 -> go ty3 | None -> false)
    | T_constr { tys; _ } -> List.exists go tys
    | Tvar { contents = Tlink ty' } -> go ty'
    | Tvar _ -> Basic_prelude.phys_equal v ty
    | Tparam _ | T_builtin _ | T_trait _ | T_blackhole -> false
  in
  go ty

type filter_blame_target = Filtered_type | Filter_itself

let filter_arrow ~blame ~(has_error : bool) ~is_async (arity : int) (ty : typ)
    (loc : Rloc.t) =
  (let ty = type_repr ty in
   match ty with
   | Tarrow { params_ty; ret_ty; err_ty; is_async = is_async' } ->
       let len = List.length params_ty in
       if arity = len && is_async' = is_async then Ok (params_ty, ret_ty, err_ty)
       else
         let error =
           match blame with
           | Filtered_type ->
               Errors.func_param_num_mismatch ~expected:len ~actual:arity
                 ~ty:(Printer.type_to_string ty)
                 ~loc
           | Filter_itself ->
               let expected =
                 (("function with " ^ Int.to_string len ^ " argument(s)"
                   : Stdlib.String.t)
                   [@merlin.hide])
               in
               let actual =
                 (("function with " ^ Int.to_string arity ^ " argument(s)"
                   : Stdlib.String.t)
                   [@merlin.hide])
               in
               Errors.type_mismatch ~expected ~actual ~loc
         in
         let params_ty =
           if arity < len then Basic_lst.take params_ty arity
           else params_ty @ type_var_list (arity - len) Tvar_error
         in
         Partial ((params_ty, ret_ty, err_ty), [ error ])
   | Tvar ({ contents = Tnolink tvar_kind } as link) ->
       let params_ty = type_var_list arity tvar_kind in
       let ret_ty = Stype.new_type_var tvar_kind in
       let err_ty =
         if has_error then Some (Stype.new_type_var tvar_kind) else None
       in
       link :=
         Tlink
           (Tarrow { params_ty; ret_ty; err_ty; generic_ = false; is_async });
       Ok (params_ty, ret_ty, err_ty)
   | _ -> (
       let ty_params = type_var_list arity Tvar_error in
       let ty_res = Stype.new_type_var Tvar_error in
       let ty_err =
         if has_error then Some (Stype.new_type_var Tvar_error) else None
       in
       match ty with
       | T_blackhole -> Ok (ty_params, ty_res, ty_err)
       | _ ->
           let expected, actual =
             match blame with
             | Filtered_type -> ("function type", Printer.type_to_string ty)
             | Filter_itself -> (Printer.type_to_string ty, "function type")
           in
           Partial
             ( (ty_params, ty_res, ty_err),
               [ Errors.type_mismatch ~expected ~actual ~loc ] ))
    : (typ list * typ * typ option) Local_diagnostics.partial_info)

let filter_product ?src_ty ~blame ~(arity : int option) (ty : typ)
    (loc : Rloc.t) =
  (match (type_repr ty, arity) with
   | Tvar ({ contents = Tnolink tvar_kind } as link), Some n ->
       let tys = type_var_list n tvar_kind in
       link := Tlink (Builtin.type_product tys);
       Ok tys
   | T_constr { type_constructor = Tuple len; tys }, Some n ->
       if len = n then Ok tys
       else
         let expected, actual =
           match blame with
           | Filtered_type -> (n, len)
           | Filter_itself -> (len, n)
         in
         let error =
           Errors.type_mismatch
             ~expected:
               ((Int.to_string expected ^ "-tuple"
                 : Stdlib.String.t)
                 [@merlin.hide])
             ~actual:
               ((Int.to_string actual ^ "-tuple"
                 : Stdlib.String.t)
                 [@merlin.hide])
             ~loc
         in
         let tys =
           if n < len then Basic_lst.take tys n
           else tys @ type_var_list (n - len) Tvar_error
         in
         Partial (tys, [ error ])
   | T_constr { type_constructor = Tuple _; tys }, None -> Ok tys
   | ty, Some n -> (
       let tys = type_var_list n Tvar_error in
       match ty with
       | T_blackhole -> Ok tys
       | _ ->
           let ty = Option.value src_ty ~default:ty in
           let expected, actual =
             match blame with
             | Filtered_type ->
                 ( ((Int.to_string n ^ "-tuple"
                     : Stdlib.String.t)
                     [@merlin.hide]),
                   ((Printer.type_to_string ty
                     : Stdlib.String.t)
                     [@merlin.hide]) )
             | Filter_itself ->
                 ( ((Printer.type_to_string ty : Stdlib.String.t) [@merlin.hide]),
                   ((Int.to_string n ^ "-tuple"
                     : Stdlib.String.t)
                     [@merlin.hide]) )
           in
           Partial (tys, [ Errors.type_mismatch ~expected ~actual ~loc ]))
   | Tvar { contents = Tnolink Tvar_error }, None -> Partial ([], [])
   | T_blackhole, None -> Ok []
   | ty, None ->
       let ty = Option.value src_ty ~default:ty in
       let expected, actual =
         match blame with
         | Filtered_type ->
             ( "tuple",
               ((Printer.type_to_string ty : Stdlib.String.t) [@merlin.hide]) )
         | Filter_itself ->
             ( ((Printer.type_to_string ty : Stdlib.String.t) [@merlin.hide]),
               "tuple" )
       in
       Partial ([], [ Errors.type_mismatch ~expected ~actual ~loc ])
    : typ list Local_diagnostics.partial_info)

let filter_array_like_pattern (ty : typ) (loc : Rloc.t) =
  (match type_repr ty with
   | Tvar { contents = Tnolink Tvar_error } | T_blackhole ->
       Ok (Stype.new_type_var Tvar_error)
   | T_constr { type_constructor = p; tys = ty_arg :: [] }
     when Type_path.can_use_array_pattern p ->
       Ok ty_arg
   | T_builtin T_string -> Ok Stype.char
   | T_constr { type_constructor; _ }
     when Type_path.is_bytesview type_constructor ->
       Ok Stype.byte
   | T_constr { type_constructor = p; _ }
     when Type_path.equal p Type_path.Builtin.type_path_stringview ->
       Ok Stype.char
   | _ ->
       Partial
         ( Stype.new_type_var Tvar_error,
           [
             Errors.type_mismatch
               ~actual:
                 "Array Like(Array, FixedArray, @array.View, @bytes.View, \
                  String, or @string.View)"
               ~expected:(Printer.type_to_string ty)
               ~loc;
           ] )
    : typ Local_diagnostics.partial_info)

let rec same_type (ty1 : typ) (ty2 : typ) =
  (let ty1 = type_repr ty1 in
   let ty2 = type_repr ty2 in
   Basic_prelude.phys_equal ty1 ty2
   ||
   match ty1 with
   | Tvar _ -> false
   | Tarrow { params_ty = ps1; ret_ty = r1; err_ty = e1; is_async = a1 } -> (
       match ty2 with
       | Tarrow { params_ty = ps2; ret_ty = r2; err_ty = e2; is_async = a2 }
         -> (
           Lst.for_all2_no_exn ps1 ps2 same_type
           && same_type r1 r2 && a1 = a2
           &&
           match (e1, e2) with
           | Some e1, Some e2 -> same_type e1 e2
           | None, None -> true
           | _ -> false)
       | _ -> false)
   | T_constr { type_constructor = p1; tys = tys1 } -> (
       match ty2 with
       | T_constr { type_constructor = p2; tys = tys2 } ->
           Type_path.equal p1 p2 && Lst.for_all2_no_exn tys1 tys2 same_type
       | _ -> false)
   | Tparam { index = i1 } -> (
       match ty2 with Tparam { index = i2 } -> i1 = i2 | _ -> false)
   | T_trait t1 -> (
       match ty2 with T_trait t2 -> Type_path.equal t1 t2 | _ -> false)
   | T_builtin x -> (
       match ty2 with T_builtin y -> Stype.equal_builtin x y | _ -> false)
   | T_blackhole -> ( match ty2 with T_blackhole -> true | _ -> false)
    : bool)

let classify_as_builtin (t : Stype.t) =
  match type_repr t with
  | T_builtin builtin -> (
      match builtin with
      | T_int -> `Int
      | T_int64 -> `Int64
      | T_uint -> `UInt
      | T_uint64 -> `UInt64
      | T_float -> `Float
      | T_double -> `Double
      | T_char -> `Char
      | T_byte -> `Byte
      | T_int16 -> `Int16
      | T_uint16 -> `UInt16
      | T_string | T_unit | T_bool | T_bytes -> `Other)
  | _ -> `Other

let deref_constr_type (t : Stype.t) =
  (match type_repr t with
   | T_constr
       { type_constructor = Constr { ty; tag }; tys; generic_; is_suberror_ } ->
       ( T_constr { type_constructor = ty; tys; generic_; is_suberror_ },
         Some tag )
   | ty -> (ty, None)
    : Stype.t * Basic_constr_info.constr_tag option)

let deref_constr_type_to_local_value (t : Stype.t) =
  (match type_repr t with
   | T_constr
       { type_constructor = Constr { ty; tag }; tys; generic_; is_suberror_ } ->
       ( T_constr { type_constructor = ty; tys; generic_; is_suberror_ },
         Value_constr tag )
   | ty -> (ty, Normal)
    : Stype.t * Typedtree.value_kind)

let make_constr_type (t : Stype.t) ~(tag : Basic_constr_info.constr_tag) =
  (match type_repr t with
   | T_constr { type_constructor; tys; generic_; is_suberror_ } ->
       T_constr
         {
           type_constructor = Type_path.constr ~ty:type_constructor ~tag;
           tys;
           generic_;
           is_suberror_;
         }
   | ty -> ty
    : Stype.t)

let is_super_error (ty : Stype.t) =
  (match type_repr ty with
   | T_constr { type_constructor = Basic_type_path.T_error; _ } -> true
   | _ -> false
    : bool)

let is_suberror (ty : Stype.t) =
  (match type_repr ty with
   | T_constr { is_suberror_; _ } -> is_suberror_
   | _ -> false
    : bool)

let is_error_type ~tvar_env (ty : Stype.t) =
  (match type_repr ty with
   | T_constr { type_constructor = T_error; _ } -> true
   | T_constr { is_suberror_; _ } -> is_suberror_
   | Tparam { index } ->
       let tparam_info = Tvar_env.find_by_index_exn tvar_env index in
       Lst.exists tparam_info.constraints (fun c ->
           Type_path.equal c.trait Type_path.Builtin.type_path_error)
   | Tvar _ -> true
   | T_blackhole -> true
   | T_builtin _ | Tarrow _ | T_trait _ -> false
    : bool)

let is_array_like ty =
  match type_repr ty with
  | T_constr { type_constructor; _ } ->
      Type_path.equal type_constructor Type_path.Builtin.type_path_fixedarray
      || Type_path.equal type_constructor Type_path.Builtin.type_path_array
      || Type_path.equal type_constructor Type_path.Builtin.type_path_arrayview
  | T_builtin T_bytes -> true
  | T_builtin T_string
  | T_builtin T_byte
  | T_builtin T_char
  | T_builtin T_int
  | T_builtin T_int64
  | T_builtin T_uint
  | T_builtin T_uint64
  | T_builtin T_float
  | T_builtin T_double
  | T_builtin T_unit
  | T_builtin T_bool
  | T_builtin T_int16
  | T_builtin T_uint16
  | Tparam _ | Tvar _ | T_blackhole | Tarrow _ | T_trait _ ->
      false
