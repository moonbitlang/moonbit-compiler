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

type typ = Stype.t

exception Unify

let type_repr = Stype.type_repr
let check_occur = Type.check_occur

let rec unify (ty1 : typ) (ty2 : typ) =
  (let ty1' = type_repr ty1 and ty2' = type_repr ty2 in
   if Basic_prelude.phys_not_equal ty1' ty2' then (
     (match (ty1', ty2') with
     | Tvar link1, _ -> (
         match ty2' with
         | Tvar link2 -> (
             match !link1 with
             | Tnolink Tvar_error -> link2 := Tlink ty1'
             | _ -> link1 := Tlink ty2')
         | _ ->
             if check_occur ty1' ty2' then raise_notrace Unify
             else link1 := Tlink ty2')
     | _, Tvar link ->
         if check_occur ty2' ty1' then raise_notrace Unify
         else link := Tlink ty1'
     | T_blackhole, _ | _, T_blackhole -> ()
     | ( Tarrow { params_ty = t1x; ret_ty = t1y; err_ty = t1z; is_async = a1 },
         Tarrow { params_ty = t2x; ret_ty = t2y; err_ty = t2z; is_async = a2 } )
       -> (
         if a1 <> a2 then raise_notrace Unify;
         unify_list t1x t2x;
         unify t1y t2y;
         match (t1z, t2z) with
         | None, None -> ()
         | Some t1z, Some t2z -> unify t1z t2z
         | _ -> raise_notrace Unify)
     | ( T_constr { type_constructor = c1; tys = tys1 },
         T_constr { type_constructor = c2; tys = tys2 } ) ->
         if Type_path.equal c1 c2 then unify_list tys1 tys2
         else raise_notrace Unify
     | Tparam { index = i1 }, Tparam { index = i2 } ->
         if i1 <> i2 then raise_notrace Unify
     | T_trait t1, T_trait t2 ->
         if not (Type_path.equal t1 t2) then raise_notrace Unify
     | T_builtin a, T_builtin b ->
         if not (Stype.equal_builtin a b) then raise_notrace Unify
     | Tarrow _, (T_constr _ | Tparam _ | T_trait _ | T_builtin _)
     | T_constr _, (Tarrow _ | Tparam _ | T_trait _ | T_builtin _)
     | Tparam _, (T_constr _ | Tarrow _ | T_trait _ | T_builtin _)
     | T_trait _, (T_constr _ | Tarrow _ | Tparam _ | T_builtin _)
     | T_builtin _, (T_constr _ | Tarrow _ | Tparam _ | T_trait _) ->
         raise_notrace Unify);
     ignore (type_repr ty1);
     ignore (type_repr ty2))
    : unit)

and unify_list t1s t2s =
  match (t1s, t2s) with
  | [], [] -> ()
  | t1 :: t1s, t2 :: t2s ->
      unify t1 t2;
      unify_list t1s t2s
  | [], _ :: _ | _ :: _, [] -> raise_notrace Unify

let unify_expr ~expect_ty ~actual_ty loc =
  (try
     unify expect_ty actual_ty;
     None
   with Unify ->
     let expected, actual = Printer.type_pair_to_string expect_ty actual_ty in
     Some (Errors.expr_unify ~expected ~actual ~loc)
    : Local_diagnostics.error option)

let unify_expr_and_store_error ~expect_ty ~actual_ty ~diagnostics loc =
  try unify expect_ty actual_ty
  with Unify ->
    let expected, actual = Printer.type_pair_to_string expect_ty actual_ty in
    Local_diagnostics.add_error diagnostics
      (Errors.expr_unify ~expected ~actual ~loc)

let unify_pat ~expect_ty ~actual_ty loc =
  (try
     unify expect_ty actual_ty;
     None
   with Unify ->
     let expected, actual = Printer.type_pair_to_string expect_ty actual_ty in
     Some (Errors.pat_unify ~expected ~actual ~loc)
    : Local_diagnostics.error option)

let unify_param name ~expect_ty ~actual_ty loc =
  (try
     unify expect_ty actual_ty;
     None
   with Unify ->
     let expected, actual = Printer.type_pair_to_string expect_ty actual_ty in
     Some (Errors.param_unify ~name ~expected ~actual ~loc)
    : Local_diagnostics.error option)

let unify_constr name ~expect_ty ~actual_ty loc =
  (try
     unify expect_ty actual_ty;
     None
   with Unify ->
     let expected, actual = Printer.type_pair_to_string expect_ty actual_ty in
     Some (Errors.constr_unify ~name ~expected ~actual ~loc)
    : Local_diagnostics.error option)

let unify_exn = unify

let try_unify ty1 ty2 =
  try
    unify ty1 ty2;
    true
  with Unify -> false
