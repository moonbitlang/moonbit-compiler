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


module Ltype = Ltype_gc
module Tid = Basic_ty_ident
module Hash_tid = Basic_ty_ident.Hash

let get_arr_elem (tid : Tid.t) (type_defs : Ltype.type_defs) =
  (if Tid.equal tid Ltype.tid_bytes then Ltype.i32_byte
   else
     match Hash_tid.find_exn type_defs tid with
     | Ref_array { elem } -> elem
     | Ref_concrete_object _ -> assert false
     | Ref_closure_abstract _ -> assert false
     | Ref_late_init_struct _ -> assert false
     | Ref_struct _ -> assert false
     | Ref_closure _ -> assert false
     | Ref_object _ -> assert false
     | Ref_constructor _ -> assert false
    : Ltype.t)

let is_non_nullable_ref_type (ty : Ltype.t) =
  match ty with
  | I32 _ | I64 | F32 | F64 -> false
  | Ref_nullable _ -> false
  | Ref_lazy_init _ | Ref _ | Ref_extern | Ref_string | Ref_bytes | Ref_func
  | Ref_any ->
      true
