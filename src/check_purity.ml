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

let rec check (lam : Clam.lambda) =
  (match lam with
   | Levent { expr; loc_ = _ } -> check expr
   | Lvar _ | Lconst _ | Lallocate _ | Lclosure _ | Lget_raw_func _
   | Lget_field _ | Lclosure_field _ | Lmake_array _ | Larray_get_item _
   | Lcast _ ->
       true
   | Lloop _ | Lapply _ | Lstub_call _ | Ljoinlet _ | Ljoinapply _ | Lbreak _
   | Lcontinue _ | Lreturn _ | Lcatch _ | Lset_field _ | Larray_set_item _
   | Lassign _ ->
       false
   | Lif { pred; ifso; ifnot } -> check pred && check ifso && check ifnot
   | Llet { name = _; e; body } -> check e && check body
   | Lletrec { body; _ } -> check body
   | Lprim { fn; args } -> Primitive.is_pure fn && List.for_all check args
   | Lsequence { exprs; last_expr } ->
       Lst.for_all exprs check && check last_expr
   | Lswitch { obj = _; cases; default } ->
       List.for_all (fun (_, arm) -> check arm) cases && check default
   | Lswitchstring { obj; cases; default } ->
       check obj
       && List.for_all (fun (_, arm) -> check arm) cases
       && check default
   | Lswitchint { obj = _; cases; default } ->
       List.for_all (fun (_, arm) -> check arm) cases && check default
    : bool)
