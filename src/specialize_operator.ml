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


module Operators = Parsing_operators
module Ident = Basic_ident
module Qual_ident = Basic_qual_ident
module Type_path = Basic_type_path

let try_specialize_op ~(op_info : Operators.operator_info) ~(ty_lhs : Stype.t)
    ~loc =
  let return ~ty_args_ ~ret_ty id ~kind =
    let ty_rhs =
      match op_info.op_name with "<<" | ">>" -> Stype.int | _ -> ty_lhs
    in
    let tmethod : Typedtree.expr =
      Texpr_ident
        {
          id = { var_id = Ident.of_qual_ident id; loc_ = loc };
          ty_args_;
          arity_ = Some (Fn_arity.simple 2);
          kind;
          ty =
            Builtin.type_arrow [ ty_lhs; ty_rhs ] ret_ty ~err_ty:None
              ~is_async:false;
          loc_ = loc;
        }
    in
    Some tmethod
  in
  let return_method ~ret_ty ~self_typ ~kind =
    return ~ty_args_:[||] ~ret_ty ~kind
      (Qual_ident.meth ~self_typ ~name:op_info.method_name)
      [@@inline]
  in
  let return_impl ~ret_ty ~trait ~self_typ ~kind =
    return ~ty_args_:[||] ~ret_ty ~kind
      (Qual_ident.ext_meth ~trait ~self_typ ~name:op_info.method_name)
      [@@inline]
  in
  match (op_info.op_name, Stype.type_repr ty_lhs) with
  | "+", T_builtin T_string ->
      return_method ~self_typ:Type_path.Builtin.type_path_string
        ~ret_ty:Stype.string ~kind:(Prim Primitive.add_string)
  | ("+" | "-" | "*" | "/" | "%"), T_builtin b -> (
      let operator : Primitive.arith_operator =
        match op_info.op_name with
        | "+" -> Add
        | "-" -> Sub
        | "*" -> Mul
        | "/" -> Div
        | "%" -> Mod
        | _ -> assert false
      in
      let return kind =
        return_method ~self_typ:(Stype.tpath_of_builtin b) ~ret_ty:ty_lhs ~kind
          [@@local]
      in
      match b with
      | T_int -> return (Prim (Parith { operator; operand_type = I32 }))
      | T_uint -> return (Prim (Parith { operator; operand_type = U32 }))
      | T_float when operator <> Mod ->
          return (Prim (Parith { operator; operand_type = F32 }))
      | T_double when operator <> Mod ->
          return (Prim (Parith { operator; operand_type = F64 }))
      | T_string -> return (Prim Primitive.add_string)
      | T_byte | T_int16 | T_uint16 -> return Normal
      | T_unit | T_bool | T_char | T_bytes | T_float | T_double | T_int64
      | T_uint64 ->
          None)
  | "==", T_builtin b -> (
      let return kind =
        return_impl ~trait:Type_path.Builtin.trait_eq
          ~self_typ:(Stype.tpath_of_builtin b) ~ret_ty:Stype.bool ~kind
          [@@local]
      in
      match b with
      | T_int | T_bool | T_char ->
          return (Prim (Pcomparison { operator = Eq; operand_type = I32 }))
      | T_uint ->
          return (Prim (Pcomparison { operator = Eq; operand_type = U32 }))
      | T_float ->
          return (Prim (Pcomparison { operator = Eq; operand_type = F32 }))
      | T_double ->
          return (Prim (Pcomparison { operator = Eq; operand_type = F64 }))
      | T_string -> return (Prim Pstringequal)
      | T_bytes -> return (Prim Pbytesequal)
      | T_unit | T_byte | T_int16 | T_uint16 -> return Normal
      | T_int64 | T_uint64 -> None)
  | ("<" | ">" | "<=" | ">=" | "!="), T_builtin b -> (
      let return kind =
        return ~ret_ty:Stype.bool ~kind ~ty_args_:[| ty_lhs |]
          (Qual_ident.make ~pkg:Basic_config.builtin_package
             ~name:op_info.method_name)
          [@@local]
      in
      let operator : Primitive.comparison =
        match op_info.op_name with
        | "<" -> Lt
        | ">" -> Gt
        | "<=" -> Le
        | ">=" -> Ge
        | "!=" -> Ne
        | _ -> assert false
      in
      match b with
      | T_int | T_char | T_byte | T_int16 | T_uint16 ->
          return (Prim (Pcomparison { operator; operand_type = I32 }))
      | T_uint -> return (Prim (Pcomparison { operator; operand_type = U32 }))
      | T_float -> return (Prim (Pcomparison { operator; operand_type = F32 }))
      | T_double -> return (Prim (Pcomparison { operator; operand_type = F64 }))
      | T_unit | T_bool | T_string | T_bytes | T_int64 | T_uint64 -> None)
  | ("&" | "|" | "^" | "<<" | ">>"), T_builtin b -> (
      let return kind =
        return_method ~self_typ:(Stype.tpath_of_builtin b) ~ret_ty:ty_lhs ~kind
          [@@local]
      in
      let operator : Primitive.bitwise_operator =
        match op_info.op_name with
        | "&" -> And
        | "|" -> Or
        | "^" -> Xor
        | "<<" -> Shl
        | ">>" -> Shr
        | _ -> assert false
      in
      match b with
      | T_int -> return (Prim (Pbitwise { operator; operand_type = I32 }))
      | T_uint -> return (Prim (Pbitwise { operator; operand_type = U32 }))
      | T_byte | T_int16 | T_uint16 -> return Normal
      | T_float | T_double | T_unit | T_bool | T_char | T_string | T_bytes
      | T_int64 | T_uint64 ->
          None)
  | _ -> None
