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
module Type_path = Basic_type_path

type t = Stype.t array

let make typ_instances = typ_instances
let empty () = [||]

let monofy_param (env : t) ~(index : int) =
  (match Stype.type_repr env.!(index) with
   | T_constr { type_constructor = p; tys = _ } -> p
   | T_trait t -> t
   | T_builtin b -> Stype.tpath_of_builtin b
   | Tparam _ -> assert false
   | Tarrow _ -> assert false
   | Tvar _ -> assert false
   | T_blackhole -> assert false
    : Type_path.t)

let monofy_typ (env : t) (t : Stype.t) =
  (let rec go (t : Stype.t) =
     (match Stype.type_repr t with
      | Tarrow { params_ty; ret_ty; err_ty; is_async } ->
          Tarrow
            {
              params_ty = gos params_ty;
              ret_ty = go ret_ty;
              generic_ = false;
              err_ty = Option.map go err_ty;
              is_async;
            }
      | Tvar _ -> assert false
      | T_blackhole -> assert false
      | T_constr { type_constructor; tys; is_suberror_ } ->
          let tys = gos tys in
          T_constr { type_constructor; tys; generic_ = false; is_suberror_ }
      | T_trait _ as t -> t
      | Tparam { index } -> env.!(index)
      | T_builtin _ as t -> t
       : Stype.t)
   and gos (ts : Stype.t list) = Lst.map ts go in
   go t
    : Stype.t)
