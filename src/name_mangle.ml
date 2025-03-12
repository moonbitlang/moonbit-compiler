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


module Tid = Basic_ty_ident

let make_type_name (t : Mtype.t) =
  (let buf = Buffer.create 16 in
   let ( +> ) = Buffer.add_char in
   let ( +>> ) = Buffer.add_string in
   let rec go (ty : Mtype.t) =
     match ty with
     | T_int -> buf +>> "Int"
     | T_char -> buf +>> "Char"
     | T_bool -> buf +>> "Bool"
     | T_unit -> buf +>> "Unit"
     | T_byte -> buf +>> "Byte"
     | T_int16 -> buf +>> "Int16"
     | T_uint16 -> buf +>> "UInt16"
     | T_int64 -> buf +>> "Int64"
     | T_uint -> buf +>> "UInt"
     | T_uint64 -> buf +>> "UInt64"
     | T_float -> buf +>> "Float"
     | T_double -> buf +>> "Double"
     | T_string -> buf +>> "String"
     | T_bytes -> buf +>> "Bytes"
     | T_optimized_option { elem = T_char } -> buf +>> "Option<Char>"
     | T_optimized_option { elem } ->
         buf +>> "Option<";
         go elem;
         buf +>> ">"
     | T_func { params; return } ->
         buf +> '<';
         (match params with [] -> () | ty :: [] -> go ty | tys -> gos '*' tys);
         buf +> '>';
         buf +>> "=>";
         go return
     | T_raw_func { params; return } ->
         buf +>> "RawFunction<";
         (match params with [] -> () | ty :: [] -> go ty | tys -> gos '*' tys);
         buf +> '>';
         buf +>> "=>";
         go return
     | T_tuple { tys } ->
         buf +> '<';
         gos '*' tys;
         buf +> '>'
     | T_fixedarray { elem } ->
         buf +>> "FixedArray<";
         go elem;
         buf +>> ">"
     | T_constr id -> buf +>> Mtype.id_to_string id
     | T_trait id -> buf +>> Mtype.id_to_string id
     | T_any { name } -> buf +>> Mtype.id_to_string name
     | T_maybe_uninit t ->
         buf +>> "UnsafeMaybeUninit<";
         go t;
         buf +>> ">"
     | T_error_value_result { id; _ } -> (
         match !Basic_config.target with
         | Wasm_gc -> buf +>> Mtype.id_to_string id)
   and gos (sep : char) = function
     | [] -> ()
     | ty :: [] -> go ty
     | ty :: tys ->
         go ty;
         buf +> sep;
         gos sep tys
   in
   go t;
   Tid.of_string (Buffer.contents buf)
    : Tid.t)

let make_constr_name (enum_name : Tid.t) (tag : Tag.t) =
  (Tid.of_string (Tid.to_string enum_name ^ "." ^ tag.name_) : Tid.t)
