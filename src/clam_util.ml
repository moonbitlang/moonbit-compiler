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


let loc_of_lambda : Clam.lambda -> Loc.t option = function
  | Levent { loc_; _ } -> Some loc_
  | Lallocate _ | Lclosure _ | Lget_raw_func _ | Lget_field _ | Lclosure_field _
  | Lset_field _ | Lmake_array _ | Larray_get_item _ | Larray_set_item _
  | Lapply _ | Lstub_call _ | Lconst _ | Lif _ | Llet _ | Lletrec _ | Lprim _
  | Lsequence _ | Ljoinlet _ | Ljoinapply _ | Lbreak _ | Lcontinue _ | Lswitch _
  | Lswitchint _ | Lswitchstring _ | Lvar _ | Lassign _ | Lcatch _ | Lcast _
  | Lloop _ | Lreturn _ ->
      None

let rec no_located : Clam.lambda -> Clam.lambda = function
  | Levent { expr; _ } -> no_located expr
  | expr -> expr
