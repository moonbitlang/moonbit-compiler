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


module Constr_info = Basic_constr_info
module Hash_string = Basic_hash_string

type operand_type = I32 | I64 | U32 | U64 | F64 | F32 | U8 | U16 | I16
and convert_kind = Convert | Saturate | Reinterpret
and arith_operator = Add | Sub | Mul | Div | Mod | Neg | Sqrt | Abs
and bitwise_operator = Not | And | Or | Xor | Shl | Shr | Ctz | Clz | Popcnt
and comparison = Lt | Le | Gt | Ge | Eq | Ne

and cast_kind =
  | Constr_to_enum
  | Enum_to_constr
  | Unfold_rec_newtype
  | Make_newtype

and prim =
  | Pfixedarray_length
  | Pccall of { arity : int; func_name : string }
  | Pintrinsic of Moon_intrinsic.t
  | Pgetstringitem of { safe : bool }
  | Pignore
  | Pidentity
  | Pmakebytes
  | Pbyteslength
  | Pgetbytesitem of { safe : bool }
  | Psetbytesitem of { safe : bool }
  | Pnot
  | Ppanic
  | Punreachable
  | Pcatch
  | Pstringlength
  | Pstringequal
  | Pbytesequal
  | Pcast of { kind : cast_kind }
  | Pconvert of { kind : convert_kind; from : operand_type; to_ : operand_type }
  | Parith of { operand_type : operand_type; operator : arith_operator }
  | Pbitwise of { operand_type : operand_type; operator : bitwise_operator }
  | Pcomparison of { operand_type : operand_type; operator : comparison }
  | Pcompare of operand_type
  | Pfixedarray_make of { kind : make_array_kind }
  | Pfixedarray_get_item of { kind : array_get_kind }
  | Pfixedarray_set_item of { set_kind : array_set_kind }
  | Penum_field of { index : int; tag : Constr_info.constr_tag }
  | Pset_enum_field of { index : int; tag : Constr_info.constr_tag }
  | Pclosure_to_extern_ref
  | Prefeq
  | Parray_make
  | Pnull
  | Pnull_string_extern
  | Pis_null
  | Pas_non_null
  | Pmake_value_or_error of { tag : Basic_constr_info.constr_tag }
  | Pprintln
  | Perror_to_string
  | Pcall_object_method of { method_index : int; method_name : string }
  | Pany_to_string
  | Pget_current_continuation
  | Prun_async
  | Praw_func_to_func_ref

and make_array_kind = LenAndInit | EverySingleElem | Uninit
and array_get_kind = Safe | Unsafe | Rev_unsafe
and array_set_kind = Null | Default | Value | Unsafe

include struct
  let _ = fun (_ : operand_type) -> ()
  let _ = fun (_ : convert_kind) -> ()
  let _ = fun (_ : arith_operator) -> ()
  let _ = fun (_ : bitwise_operator) -> ()
  let _ = fun (_ : comparison) -> ()
  let _ = fun (_ : cast_kind) -> ()
  let _ = fun (_ : prim) -> ()
  let _ = fun (_ : make_array_kind) -> ()
  let _ = fun (_ : array_get_kind) -> ()
  let _ = fun (_ : array_set_kind) -> ()

  let rec sexp_of_operand_type =
    (function
     | I32 -> S.Atom "I32"
     | I64 -> S.Atom "I64"
     | U32 -> S.Atom "U32"
     | U64 -> S.Atom "U64"
     | F64 -> S.Atom "F64"
     | F32 -> S.Atom "F32"
     | U8 -> S.Atom "U8"
     | U16 -> S.Atom "U16"
     | I16 -> S.Atom "I16"
      : operand_type -> S.t)

  and sexp_of_convert_kind =
    (function
     | Convert -> S.Atom "Convert"
     | Saturate -> S.Atom "Saturate"
     | Reinterpret -> S.Atom "Reinterpret"
      : convert_kind -> S.t)

  and sexp_of_arith_operator =
    (function
     | Add -> S.Atom "Add"
     | Sub -> S.Atom "Sub"
     | Mul -> S.Atom "Mul"
     | Div -> S.Atom "Div"
     | Mod -> S.Atom "Mod"
     | Neg -> S.Atom "Neg"
     | Sqrt -> S.Atom "Sqrt"
     | Abs -> S.Atom "Abs"
      : arith_operator -> S.t)

  and sexp_of_bitwise_operator =
    (function
     | Not -> S.Atom "Not"
     | And -> S.Atom "And"
     | Or -> S.Atom "Or"
     | Xor -> S.Atom "Xor"
     | Shl -> S.Atom "Shl"
     | Shr -> S.Atom "Shr"
     | Ctz -> S.Atom "Ctz"
     | Clz -> S.Atom "Clz"
     | Popcnt -> S.Atom "Popcnt"
      : bitwise_operator -> S.t)

  and sexp_of_comparison =
    (function
     | Lt -> S.Atom "Lt"
     | Le -> S.Atom "Le"
     | Gt -> S.Atom "Gt"
     | Ge -> S.Atom "Ge"
     | Eq -> S.Atom "Eq"
     | Ne -> S.Atom "Ne"
      : comparison -> S.t)

  and sexp_of_cast_kind =
    (function
     | Constr_to_enum -> S.Atom "Constr_to_enum"
     | Enum_to_constr -> S.Atom "Enum_to_constr"
     | Unfold_rec_newtype -> S.Atom "Unfold_rec_newtype"
     | Make_newtype -> S.Atom "Make_newtype"
      : cast_kind -> S.t)

  and sexp_of_prim =
    (function
     | Pfixedarray_length -> S.Atom "Pfixedarray_length"
     | Pccall { arity = arity__002_; func_name = func_name__004_ } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_string func_name__004_ in
           (S.List [ S.Atom "func_name"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_int arity__002_ in
           (S.List [ S.Atom "arity"; arg__003_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pccall" :: bnds__001_)
     | Pintrinsic arg0__006_ ->
         let res0__007_ = Moon_intrinsic.sexp_of_t arg0__006_ in
         S.List [ S.Atom "Pintrinsic"; res0__007_ ]
     | Pgetstringitem { safe = safe__009_ } ->
         let bnds__008_ = ([] : _ Stdlib.List.t) in
         let bnds__008_ =
           let arg__010_ = Moon_sexp_conv.sexp_of_bool safe__009_ in
           (S.List [ S.Atom "safe"; arg__010_ ] :: bnds__008_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pgetstringitem" :: bnds__008_)
     | Pignore -> S.Atom "Pignore"
     | Pidentity -> S.Atom "Pidentity"
     | Pmakebytes -> S.Atom "Pmakebytes"
     | Pbyteslength -> S.Atom "Pbyteslength"
     | Pgetbytesitem { safe = safe__012_ } ->
         let bnds__011_ = ([] : _ Stdlib.List.t) in
         let bnds__011_ =
           let arg__013_ = Moon_sexp_conv.sexp_of_bool safe__012_ in
           (S.List [ S.Atom "safe"; arg__013_ ] :: bnds__011_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pgetbytesitem" :: bnds__011_)
     | Psetbytesitem { safe = safe__015_ } ->
         let bnds__014_ = ([] : _ Stdlib.List.t) in
         let bnds__014_ =
           let arg__016_ = Moon_sexp_conv.sexp_of_bool safe__015_ in
           (S.List [ S.Atom "safe"; arg__016_ ] :: bnds__014_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Psetbytesitem" :: bnds__014_)
     | Pnot -> S.Atom "Pnot"
     | Ppanic -> S.Atom "Ppanic"
     | Punreachable -> S.Atom "Punreachable"
     | Pcatch -> S.Atom "Pcatch"
     | Pstringlength -> S.Atom "Pstringlength"
     | Pstringequal -> S.Atom "Pstringequal"
     | Pbytesequal -> S.Atom "Pbytesequal"
     | Pcast { kind = kind__018_ } ->
         let bnds__017_ = ([] : _ Stdlib.List.t) in
         let bnds__017_ =
           let arg__019_ = sexp_of_cast_kind kind__018_ in
           (S.List [ S.Atom "kind"; arg__019_ ] :: bnds__017_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pcast" :: bnds__017_)
     | Pconvert { kind = kind__021_; from = from__023_; to_ = to___025_ } ->
         let bnds__020_ = ([] : _ Stdlib.List.t) in
         let bnds__020_ =
           let arg__026_ = sexp_of_operand_type to___025_ in
           (S.List [ S.Atom "to_"; arg__026_ ] :: bnds__020_ : _ Stdlib.List.t)
         in
         let bnds__020_ =
           let arg__024_ = sexp_of_operand_type from__023_ in
           (S.List [ S.Atom "from"; arg__024_ ] :: bnds__020_ : _ Stdlib.List.t)
         in
         let bnds__020_ =
           let arg__022_ = sexp_of_convert_kind kind__021_ in
           (S.List [ S.Atom "kind"; arg__022_ ] :: bnds__020_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pconvert" :: bnds__020_)
     | Parith { operand_type = operand_type__028_; operator = operator__030_ }
       ->
         let bnds__027_ = ([] : _ Stdlib.List.t) in
         let bnds__027_ =
           let arg__031_ = sexp_of_arith_operator operator__030_ in
           (S.List [ S.Atom "operator"; arg__031_ ] :: bnds__027_
             : _ Stdlib.List.t)
         in
         let bnds__027_ =
           let arg__029_ = sexp_of_operand_type operand_type__028_ in
           (S.List [ S.Atom "operand_type"; arg__029_ ] :: bnds__027_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Parith" :: bnds__027_)
     | Pbitwise { operand_type = operand_type__033_; operator = operator__035_ }
       ->
         let bnds__032_ = ([] : _ Stdlib.List.t) in
         let bnds__032_ =
           let arg__036_ = sexp_of_bitwise_operator operator__035_ in
           (S.List [ S.Atom "operator"; arg__036_ ] :: bnds__032_
             : _ Stdlib.List.t)
         in
         let bnds__032_ =
           let arg__034_ = sexp_of_operand_type operand_type__033_ in
           (S.List [ S.Atom "operand_type"; arg__034_ ] :: bnds__032_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pbitwise" :: bnds__032_)
     | Pcomparison
         { operand_type = operand_type__038_; operator = operator__040_ } ->
         let bnds__037_ = ([] : _ Stdlib.List.t) in
         let bnds__037_ =
           let arg__041_ = sexp_of_comparison operator__040_ in
           (S.List [ S.Atom "operator"; arg__041_ ] :: bnds__037_
             : _ Stdlib.List.t)
         in
         let bnds__037_ =
           let arg__039_ = sexp_of_operand_type operand_type__038_ in
           (S.List [ S.Atom "operand_type"; arg__039_ ] :: bnds__037_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pcomparison" :: bnds__037_)
     | Pcompare arg0__042_ ->
         let res0__043_ = sexp_of_operand_type arg0__042_ in
         S.List [ S.Atom "Pcompare"; res0__043_ ]
     | Pfixedarray_make { kind = kind__045_ } ->
         let bnds__044_ = ([] : _ Stdlib.List.t) in
         let bnds__044_ =
           let arg__046_ = sexp_of_make_array_kind kind__045_ in
           (S.List [ S.Atom "kind"; arg__046_ ] :: bnds__044_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pfixedarray_make" :: bnds__044_)
     | Pfixedarray_get_item { kind = kind__048_ } ->
         let bnds__047_ = ([] : _ Stdlib.List.t) in
         let bnds__047_ =
           let arg__049_ = sexp_of_array_get_kind kind__048_ in
           (S.List [ S.Atom "kind"; arg__049_ ] :: bnds__047_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pfixedarray_get_item" :: bnds__047_)
     | Pfixedarray_set_item { set_kind = set_kind__051_ } ->
         let bnds__050_ = ([] : _ Stdlib.List.t) in
         let bnds__050_ =
           let arg__052_ = sexp_of_array_set_kind set_kind__051_ in
           (S.List [ S.Atom "set_kind"; arg__052_ ] :: bnds__050_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pfixedarray_set_item" :: bnds__050_)
     | Penum_field { index = index__054_; tag = tag__056_ } ->
         let bnds__053_ = ([] : _ Stdlib.List.t) in
         let bnds__053_ =
           let arg__057_ = Constr_info.sexp_of_constr_tag tag__056_ in
           (S.List [ S.Atom "tag"; arg__057_ ] :: bnds__053_ : _ Stdlib.List.t)
         in
         let bnds__053_ =
           let arg__055_ = Moon_sexp_conv.sexp_of_int index__054_ in
           (S.List [ S.Atom "index"; arg__055_ ] :: bnds__053_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Penum_field" :: bnds__053_)
     | Pset_enum_field { index = index__059_; tag = tag__061_ } ->
         let bnds__058_ = ([] : _ Stdlib.List.t) in
         let bnds__058_ =
           let arg__062_ = Constr_info.sexp_of_constr_tag tag__061_ in
           (S.List [ S.Atom "tag"; arg__062_ ] :: bnds__058_ : _ Stdlib.List.t)
         in
         let bnds__058_ =
           let arg__060_ = Moon_sexp_conv.sexp_of_int index__059_ in
           (S.List [ S.Atom "index"; arg__060_ ] :: bnds__058_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pset_enum_field" :: bnds__058_)
     | Pclosure_to_extern_ref -> S.Atom "Pclosure_to_extern_ref"
     | Prefeq -> S.Atom "Prefeq"
     | Parray_make -> S.Atom "Parray_make"
     | Pnull -> S.Atom "Pnull"
     | Pnull_string_extern -> S.Atom "Pnull_string_extern"
     | Pis_null -> S.Atom "Pis_null"
     | Pas_non_null -> S.Atom "Pas_non_null"
     | Pmake_value_or_error { tag = tag__064_ } ->
         let bnds__063_ = ([] : _ Stdlib.List.t) in
         let bnds__063_ =
           let arg__065_ = Basic_constr_info.sexp_of_constr_tag tag__064_ in
           (S.List [ S.Atom "tag"; arg__065_ ] :: bnds__063_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pmake_value_or_error" :: bnds__063_)
     | Pprintln -> S.Atom "Pprintln"
     | Perror_to_string -> S.Atom "Perror_to_string"
     | Pcall_object_method
         { method_index = method_index__067_; method_name = method_name__069_ }
       ->
         let bnds__066_ = ([] : _ Stdlib.List.t) in
         let bnds__066_ =
           let arg__070_ = Moon_sexp_conv.sexp_of_string method_name__069_ in
           (S.List [ S.Atom "method_name"; arg__070_ ] :: bnds__066_
             : _ Stdlib.List.t)
         in
         let bnds__066_ =
           let arg__068_ = Moon_sexp_conv.sexp_of_int method_index__067_ in
           (S.List [ S.Atom "method_index"; arg__068_ ] :: bnds__066_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pcall_object_method" :: bnds__066_)
     | Pany_to_string -> S.Atom "Pany_to_string"
     | Pget_current_continuation -> S.Atom "Pget_current_continuation"
     | Prun_async -> S.Atom "Prun_async"
     | Praw_func_to_func_ref -> S.Atom "Praw_func_to_func_ref"
      : prim -> S.t)

  and sexp_of_make_array_kind =
    (function
     | LenAndInit -> S.Atom "LenAndInit"
     | EverySingleElem -> S.Atom "EverySingleElem"
     | Uninit -> S.Atom "Uninit"
      : make_array_kind -> S.t)

  and sexp_of_array_get_kind =
    (function
     | Safe -> S.Atom "Safe"
     | Unsafe -> S.Atom "Unsafe"
     | Rev_unsafe -> S.Atom "Rev_unsafe"
      : array_get_kind -> S.t)

  and sexp_of_array_set_kind =
    (function
     | Null -> S.Atom "Null"
     | Default -> S.Atom "Default"
     | Value -> S.Atom "Value"
     | Unsafe -> S.Atom "Unsafe"
      : array_set_kind -> S.t)

  let _ = sexp_of_operand_type
  and _ = sexp_of_convert_kind
  and _ = sexp_of_arith_operator
  and _ = sexp_of_bitwise_operator
  and _ = sexp_of_comparison
  and _ = sexp_of_cast_kind
  and _ = sexp_of_prim
  and _ = sexp_of_make_array_kind
  and _ = sexp_of_array_get_kind
  and _ = sexp_of_array_set_kind

  let rec equal_operand_type =
    (fun a__071_ -> fun b__072_ -> Stdlib.( = ) a__071_ b__072_
      : operand_type -> operand_type -> bool)

  and equal_convert_kind =
    (fun a__073_ -> fun b__074_ -> Stdlib.( = ) a__073_ b__074_
      : convert_kind -> convert_kind -> bool)

  and equal_arith_operator =
    (fun a__075_ -> fun b__076_ -> Stdlib.( = ) a__075_ b__076_
      : arith_operator -> arith_operator -> bool)

  and equal_bitwise_operator =
    (fun a__077_ -> fun b__078_ -> Stdlib.( = ) a__077_ b__078_
      : bitwise_operator -> bitwise_operator -> bool)

  and equal_comparison =
    (fun a__079_ -> fun b__080_ -> Stdlib.( = ) a__079_ b__080_
      : comparison -> comparison -> bool)

  and equal_cast_kind =
    (fun a__081_ -> fun b__082_ -> Stdlib.( = ) a__081_ b__082_
      : cast_kind -> cast_kind -> bool)

  and equal_prim =
    (fun a__083_ ->
       fun b__084_ ->
        if Stdlib.( == ) a__083_ b__084_ then true
        else
          match (a__083_, b__084_) with
          | Pfixedarray_length, Pfixedarray_length -> true
          | Pfixedarray_length, _ -> false
          | _, Pfixedarray_length -> false
          | Pccall _a__085_, Pccall _b__086_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__085_.arity : int) _b__086_.arity)
                (Stdlib.( = ) (_a__085_.func_name : string) _b__086_.func_name)
          | Pccall _, _ -> false
          | _, Pccall _ -> false
          | Pintrinsic _a__087_, Pintrinsic _b__088_ ->
              Moon_intrinsic.equal _a__087_ _b__088_
          | Pintrinsic _, _ -> false
          | _, Pintrinsic _ -> false
          | Pgetstringitem _a__089_, Pgetstringitem _b__090_ ->
              Stdlib.( = ) (_a__089_.safe : bool) _b__090_.safe
          | Pgetstringitem _, _ -> false
          | _, Pgetstringitem _ -> false
          | Pignore, Pignore -> true
          | Pignore, _ -> false
          | _, Pignore -> false
          | Pidentity, Pidentity -> true
          | Pidentity, _ -> false
          | _, Pidentity -> false
          | Pmakebytes, Pmakebytes -> true
          | Pmakebytes, _ -> false
          | _, Pmakebytes -> false
          | Pbyteslength, Pbyteslength -> true
          | Pbyteslength, _ -> false
          | _, Pbyteslength -> false
          | Pgetbytesitem _a__091_, Pgetbytesitem _b__092_ ->
              Stdlib.( = ) (_a__091_.safe : bool) _b__092_.safe
          | Pgetbytesitem _, _ -> false
          | _, Pgetbytesitem _ -> false
          | Psetbytesitem _a__093_, Psetbytesitem _b__094_ ->
              Stdlib.( = ) (_a__093_.safe : bool) _b__094_.safe
          | Psetbytesitem _, _ -> false
          | _, Psetbytesitem _ -> false
          | Pnot, Pnot -> true
          | Pnot, _ -> false
          | _, Pnot -> false
          | Ppanic, Ppanic -> true
          | Ppanic, _ -> false
          | _, Ppanic -> false
          | Punreachable, Punreachable -> true
          | Punreachable, _ -> false
          | _, Punreachable -> false
          | Pcatch, Pcatch -> true
          | Pcatch, _ -> false
          | _, Pcatch -> false
          | Pstringlength, Pstringlength -> true
          | Pstringlength, _ -> false
          | _, Pstringlength -> false
          | Pstringequal, Pstringequal -> true
          | Pstringequal, _ -> false
          | _, Pstringequal -> false
          | Pbytesequal, Pbytesequal -> true
          | Pbytesequal, _ -> false
          | _, Pbytesequal -> false
          | Pcast _a__095_, Pcast _b__096_ ->
              equal_cast_kind _a__095_.kind _b__096_.kind
          | Pcast _, _ -> false
          | _, Pcast _ -> false
          | Pconvert _a__097_, Pconvert _b__098_ ->
              Stdlib.( && )
                (equal_convert_kind _a__097_.kind _b__098_.kind)
                (Stdlib.( && )
                   (equal_operand_type _a__097_.from _b__098_.from)
                   (equal_operand_type _a__097_.to_ _b__098_.to_))
          | Pconvert _, _ -> false
          | _, Pconvert _ -> false
          | Parith _a__099_, Parith _b__100_ ->
              Stdlib.( && )
                (equal_operand_type _a__099_.operand_type _b__100_.operand_type)
                (equal_arith_operator _a__099_.operator _b__100_.operator)
          | Parith _, _ -> false
          | _, Parith _ -> false
          | Pbitwise _a__101_, Pbitwise _b__102_ ->
              Stdlib.( && )
                (equal_operand_type _a__101_.operand_type _b__102_.operand_type)
                (equal_bitwise_operator _a__101_.operator _b__102_.operator)
          | Pbitwise _, _ -> false
          | _, Pbitwise _ -> false
          | Pcomparison _a__103_, Pcomparison _b__104_ ->
              Stdlib.( && )
                (equal_operand_type _a__103_.operand_type _b__104_.operand_type)
                (equal_comparison _a__103_.operator _b__104_.operator)
          | Pcomparison _, _ -> false
          | _, Pcomparison _ -> false
          | Pcompare _a__105_, Pcompare _b__106_ ->
              equal_operand_type _a__105_ _b__106_
          | Pcompare _, _ -> false
          | _, Pcompare _ -> false
          | Pfixedarray_make _a__107_, Pfixedarray_make _b__108_ ->
              equal_make_array_kind _a__107_.kind _b__108_.kind
          | Pfixedarray_make _, _ -> false
          | _, Pfixedarray_make _ -> false
          | Pfixedarray_get_item _a__109_, Pfixedarray_get_item _b__110_ ->
              equal_array_get_kind _a__109_.kind _b__110_.kind
          | Pfixedarray_get_item _, _ -> false
          | _, Pfixedarray_get_item _ -> false
          | Pfixedarray_set_item _a__111_, Pfixedarray_set_item _b__112_ ->
              equal_array_set_kind _a__111_.set_kind _b__112_.set_kind
          | Pfixedarray_set_item _, _ -> false
          | _, Pfixedarray_set_item _ -> false
          | Penum_field _a__113_, Penum_field _b__114_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__113_.index : int) _b__114_.index)
                (Constr_info.equal_constr_tag _a__113_.tag _b__114_.tag)
          | Penum_field _, _ -> false
          | _, Penum_field _ -> false
          | Pset_enum_field _a__115_, Pset_enum_field _b__116_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__115_.index : int) _b__116_.index)
                (Constr_info.equal_constr_tag _a__115_.tag _b__116_.tag)
          | Pset_enum_field _, _ -> false
          | _, Pset_enum_field _ -> false
          | Pclosure_to_extern_ref, Pclosure_to_extern_ref -> true
          | Pclosure_to_extern_ref, _ -> false
          | _, Pclosure_to_extern_ref -> false
          | Prefeq, Prefeq -> true
          | Prefeq, _ -> false
          | _, Prefeq -> false
          | Parray_make, Parray_make -> true
          | Parray_make, _ -> false
          | _, Parray_make -> false
          | Pnull, Pnull -> true
          | Pnull, _ -> false
          | _, Pnull -> false
          | Pnull_string_extern, Pnull_string_extern -> true
          | Pnull_string_extern, _ -> false
          | _, Pnull_string_extern -> false
          | Pis_null, Pis_null -> true
          | Pis_null, _ -> false
          | _, Pis_null -> false
          | Pas_non_null, Pas_non_null -> true
          | Pas_non_null, _ -> false
          | _, Pas_non_null -> false
          | Pmake_value_or_error _a__117_, Pmake_value_or_error _b__118_ ->
              Basic_constr_info.equal_constr_tag _a__117_.tag _b__118_.tag
          | Pmake_value_or_error _, _ -> false
          | _, Pmake_value_or_error _ -> false
          | Pprintln, Pprintln -> true
          | Pprintln, _ -> false
          | _, Pprintln -> false
          | Perror_to_string, Perror_to_string -> true
          | Perror_to_string, _ -> false
          | _, Perror_to_string -> false
          | Pcall_object_method _a__119_, Pcall_object_method _b__120_ ->
              Stdlib.( && )
                (Stdlib.( = )
                   (_a__119_.method_index : int)
                   _b__120_.method_index)
                (Stdlib.( = )
                   (_a__119_.method_name : string)
                   _b__120_.method_name)
          | Pcall_object_method _, _ -> false
          | _, Pcall_object_method _ -> false
          | Pany_to_string, Pany_to_string -> true
          | Pany_to_string, _ -> false
          | _, Pany_to_string -> false
          | Pget_current_continuation, Pget_current_continuation -> true
          | Pget_current_continuation, _ -> false
          | _, Pget_current_continuation -> false
          | Prun_async, Prun_async -> true
          | Prun_async, _ -> false
          | _, Prun_async -> false
          | Praw_func_to_func_ref, Praw_func_to_func_ref -> true
      : prim -> prim -> bool)

  and equal_make_array_kind =
    (fun a__121_ -> fun b__122_ -> Stdlib.( = ) a__121_ b__122_
      : make_array_kind -> make_array_kind -> bool)

  and equal_array_get_kind =
    (fun a__123_ -> fun b__124_ -> Stdlib.( = ) a__123_ b__124_
      : array_get_kind -> array_get_kind -> bool)

  and equal_array_set_kind =
    (fun a__125_ -> fun b__126_ -> Stdlib.( = ) a__125_ b__126_
      : array_set_kind -> array_set_kind -> bool)

  let _ = equal_operand_type
  and _ = equal_convert_kind
  and _ = equal_arith_operator
  and _ = equal_bitwise_operator
  and _ = equal_comparison
  and _ = equal_cast_kind
  and _ = equal_prim
  and _ = equal_make_array_kind
  and _ = equal_array_get_kind
  and _ = equal_array_set_kind
end

let sexp_of_prim (x : prim) =
  (match x with
   | Pccall { func_name; _ } -> Atom ("@" ^ func_name)
   | Parith { operand_type; operator } ->
       (List
          (List.cons
             (Atom "Parith" : S.t)
             (List.cons
                (sexp_of_operand_type operand_type : S.t)
                ([ sexp_of_arith_operator operator ] : S.t list)))
         : S.t)
   | Pcomparison { operand_type; operator } ->
       (List
          (List.cons
             (Atom "Pcomparison" : S.t)
             (List.cons
                (sexp_of_operand_type operand_type : S.t)
                ([ sexp_of_comparison operator ] : S.t list)))
         : S.t)
   | Pgetbytesitem { safe = true } -> (Atom "Pgetbytesitem" : S.t)
   | Psetbytesitem { safe = true } -> (Atom "Psetbytesitem" : S.t)
   | _ -> sexp_of_prim x
    : S.t)

let compare_int = Pcompare I32
let compare_int64 = Pcompare I64
let compare_uint = Pcompare U32
let compare_uint64 = Pcompare U64
let compare_bool = compare_int
let compare_char = compare_int
let compare_float = Pcompare F64
let compare_float32 = Pcompare F32
let equal_int = Pcomparison { operand_type = I32; operator = Eq }
let equal_int64 = Pcomparison { operand_type = I64; operator = Eq }
let equal_uint = equal_int
let equal_uint64 = equal_int64
let equal_bool = equal_int
let equal_char = equal_int
let equal_float = Pcomparison { operand_type = F32; operator = Eq }
let equal_float64 = Pcomparison { operand_type = F64; operator = Eq }
let equal_string = Pstringequal
let ge_int = Pcomparison { operand_type = I32; operator = Ge }
let le_int = Pcomparison { operand_type = I32; operator = Le }
let gt_int = Pcomparison { operand_type = I32; operator = Gt }
let lt_int = Pcomparison { operand_type = I32; operator = Lt }
let ne_int = Pcomparison { operand_type = I32; operator = Ne }
let eq_int = Pcomparison { operand_type = I32; operator = Eq }
let ge_uint = Pcomparison { operand_type = U32; operator = Ge }
let le_uint = Pcomparison { operand_type = U32; operator = Le }
let gt_uint = Pcomparison { operand_type = U32; operator = Gt }
let lt_uint = Pcomparison { operand_type = U32; operator = Lt }
let ne_uint = Pcomparison { operand_type = U32; operator = Ne }
let ge_int64 = Pcomparison { operand_type = I64; operator = Ge }
let le_int64 = Pcomparison { operand_type = I64; operator = Le }
let gt_int64 = Pcomparison { operand_type = I64; operator = Gt }
let lt_int64 = Pcomparison { operand_type = I64; operator = Lt }
let ne_int64 = Pcomparison { operand_type = I64; operator = Ne }
let eq_int64 = Pcomparison { operand_type = I64; operator = Eq }
let ge_uint64 = Pcomparison { operand_type = U64; operator = Ge }
let le_uint64 = Pcomparison { operand_type = U64; operator = Le }
let gt_uint64 = Pcomparison { operand_type = U64; operator = Gt }
let lt_uint64 = Pcomparison { operand_type = U64; operator = Lt }
let ne_uint64 = Pcomparison { operand_type = U64; operator = Ne }
let ge_float = Pcomparison { operand_type = F32; operator = Ge }
let le_float = Pcomparison { operand_type = F32; operator = Le }
let gt_float = Pcomparison { operand_type = F32; operator = Gt }
let lt_float = Pcomparison { operand_type = F32; operator = Lt }
let ne_float = Pcomparison { operand_type = F32; operator = Ne }
let ge_double = Pcomparison { operand_type = F64; operator = Ge }
let le_double = Pcomparison { operand_type = F64; operator = Le }
let gt_double = Pcomparison { operand_type = F64; operator = Gt }
let lt_double = Pcomparison { operand_type = F64; operator = Lt }
let ne_double = Pcomparison { operand_type = F64; operator = Ne }
let add_string = Pccall { func_name = "add_string"; arity = 2 }
let incref = Pccall { func_name = "incref"; arity = 1 }
let incref_nullable = Pccall { func_name = "incref_nullable"; arity = 1 }
let decref = Pccall { func_name = "decref"; arity = 1 }
let decref_nullable = Pccall { func_name = "decref_nullable"; arity = 1 }

let prim_table =
  Hash_string.of_list
    [
      ("%ignore", Pignore);
      ("%identity", Pidentity);
      ("%refeq", Prefeq);
      ("%loc_to_string", Pidentity);
      ( "%unsafe_obj_block_tag",
        Pccall { func_name = "unsafe_obj_block_tag"; arity = 1 } );
      ( "%unsafe_obj_block_length",
        Pccall { func_name = "unsafe_obj_block_length"; arity = 1 } );
      ( "%unsafe_obj_get_field",
        Pccall { func_name = "unsafe_obj_get_field"; arity = 2 } );
      ("%println", Pprintln);
      ("%panic", Ppanic);
      ("%unreachable", Punreachable);
      ("%control.catch", Pcatch);
      ("%bool_not", Pnot);
      ("%bool_eq", equal_bool);
      ("%bool_compare", compare_bool);
      ("%bool_default", Pccall { func_name = "default_bool"; arity = 0 });
      ("%loc_ghost", Pccall { arity = 0; func_name = "loc_ghost" });
      ("%loc_to_string", Pidentity);
      ("%string_length", Pstringlength);
      ("%string_get", Pgetstringitem { safe = true });
      ("%string.unsafe_get", Pgetstringitem { safe = false });
      ("%string_add", add_string);
      ("%string_eq", equal_string);
      ("%string_to_string", Pidentity);
      ("%fixedarray.get", Pfixedarray_get_item { kind = Safe });
      ("%fixedarray.unsafe_get", Pfixedarray_get_item { kind = Unsafe });
      ("%fixedarray.set", Pfixedarray_set_item { set_kind = Value });
      ("%fixedarray.unsafe_set", Pfixedarray_set_item { set_kind = Unsafe });
      ("%fixedarray.set_default", Pfixedarray_set_item { set_kind = Default });
      ("%fixedarray.set_null", Pfixedarray_set_item { set_kind = Null });
      ("%fixedarray.length", Pfixedarray_length);
      ("%fixedarray.make", Pfixedarray_make { kind = LenAndInit });
      ("%fixedarray.make_uninit", Pfixedarray_make { kind = Uninit });
      ("%bytes_get", Pgetbytesitem { safe = true });
      ("%bytes_set", Psetbytesitem { safe = true });
      ("%bytes_make", Pmakebytes);
      ("%bytes_length", Pbyteslength);
      ("%bytes.unsafe_get", Pgetbytesitem { safe = false });
      ("%bytes.equal", Pbytesequal);
      ("%i32_neg", Parith { operand_type = I32; operator = Neg });
      ("%i32_add", Parith { operand_type = I32; operator = Add });
      ("%i32_sub", Parith { operand_type = I32; operator = Sub });
      ("%i32_mul", Parith { operand_type = I32; operator = Mul });
      ("%i32_div", Parith { operand_type = I32; operator = Div });
      ("%i32_mod", Parith { operand_type = I32; operator = Mod });
      ("%i32_lnot", Pbitwise { operand_type = I32; operator = Not });
      ("%i32_land", Pbitwise { operand_type = I32; operator = And });
      ("%i32_lor", Pbitwise { operand_type = I32; operator = Or });
      ("%i32_lxor", Pbitwise { operand_type = I32; operator = Xor });
      ("%i32_shl", Pbitwise { operand_type = I32; operator = Shl });
      ("%i32_shr", Pbitwise { operand_type = I32; operator = Shr });
      ("%i32_ctz", Pbitwise { operand_type = I32; operator = Ctz });
      ("%i32_clz", Pbitwise { operand_type = I32; operator = Clz });
      ("%i32_popcnt", Pbitwise { operand_type = I32; operator = Popcnt });
      ("%i32_eq", Pcomparison { operand_type = I32; operator = Eq });
      ("%i32_ne", Pcomparison { operand_type = I32; operator = Ne });
      ("%i32_compare", compare_int);
      ("%i32_is_pos", Pccall { func_name = "int_is_pos"; arity = 1 });
      ("%i32_is_neg", Pccall { func_name = "int_is_neg"; arity = 1 });
      ("%i32_is_non_pos", Pccall { func_name = "int_is_non_pos"; arity = 1 });
      ("%i32_is_non_neg", Pccall { func_name = "int_is_non_neg"; arity = 1 });
      ("%i32_default", Pccall { func_name = "default_int"; arity = 0 });
      ("%u32.add", Parith { operand_type = U32; operator = Add });
      ("%u32.sub", Parith { operand_type = U32; operator = Sub });
      ("%u32.mul", Parith { operand_type = U32; operator = Mul });
      ("%u32.div", Parith { operand_type = U32; operator = Div });
      ("%u32.mod", Parith { operand_type = U32; operator = Mod });
      ("%u32.eq", Pcomparison { operand_type = U32; operator = Eq });
      ("%u32.ne", Pcomparison { operand_type = U32; operator = Ne });
      ("%u32.compare", compare_uint);
      ("%u32.bitand", Pbitwise { operand_type = U32; operator = And });
      ("%u32.bitnot", Pbitwise { operand_type = U32; operator = Not });
      ("%u32.bitor", Pbitwise { operand_type = U32; operator = Or });
      ("%u32.bitxor", Pbitwise { operand_type = U32; operator = Xor });
      ("%u32.shl", Pbitwise { operand_type = U32; operator = Shl });
      ("%u32.shr", Pbitwise { operand_type = U32; operator = Shr });
      ("%u32.clz", Pbitwise { operand_type = U32; operator = Clz });
      ("%u32.ctz", Pbitwise { operand_type = U32; operator = Ctz });
      ("%u32.popcnt", Pbitwise { operand_type = U32; operator = Popcnt });
      ("%u64.add", Parith { operand_type = U64; operator = Add });
      ("%u64.sub", Parith { operand_type = U64; operator = Sub });
      ("%u64.mul", Parith { operand_type = U64; operator = Mul });
      ("%u64.div", Parith { operand_type = U64; operator = Div });
      ("%u64.mod", Parith { operand_type = U64; operator = Mod });
      ("%u64.eq", Pcomparison { operand_type = U64; operator = Eq });
      ("%u64.ne", Pcomparison { operand_type = U64; operator = Ne });
      ("%u64.compare", compare_uint64);
      ("%u64.bitand", Pbitwise { operand_type = U64; operator = And });
      ("%u64.bitnot", Pbitwise { operand_type = U64; operator = Not });
      ("%u64.bitor", Pbitwise { operand_type = U64; operator = Or });
      ("%u64.bitxor", Pbitwise { operand_type = U64; operator = Xor });
      ("%u64.shl", Pbitwise { operand_type = U64; operator = Shl });
      ("%u64.shr", Pbitwise { operand_type = U64; operator = Shr });
      ("%u64.clz", Pbitwise { operand_type = U64; operator = Clz });
      ("%u64.ctz", Pbitwise { operand_type = U64; operator = Ctz });
      ("%u64.popcnt", Pbitwise { operand_type = U64; operator = Popcnt });
      ("%i64_neg", Parith { operand_type = I64; operator = Neg });
      ("%i64_add", Parith { operand_type = I64; operator = Add });
      ("%i64_sub", Parith { operand_type = I64; operator = Sub });
      ("%i64_mul", Parith { operand_type = I64; operator = Mul });
      ("%i64_div", Parith { operand_type = I64; operator = Div });
      ("%i64_mod", Parith { operand_type = I64; operator = Mod });
      ("%i64_lnot", Pbitwise { operand_type = I64; operator = Not });
      ("%i64_land", Pbitwise { operand_type = I64; operator = And });
      ("%i64_lor", Pbitwise { operand_type = I64; operator = Or });
      ("%i64_lxor", Pbitwise { operand_type = I64; operator = Xor });
      ("%i64_shl", Pbitwise { operand_type = I64; operator = Shl });
      ("%i64_shr", Pbitwise { operand_type = I64; operator = Shr });
      ("%i64_ctz", Pbitwise { operand_type = I64; operator = Ctz });
      ("%i64_clz", Pbitwise { operand_type = I64; operator = Clz });
      ("%i64_popcnt", Pbitwise { operand_type = I64; operator = Popcnt });
      ("%i64_eq", Pcomparison { operand_type = I64; operator = Eq });
      ("%i64_ne", Pcomparison { operand_type = I64; operator = Ne });
      ("%i64_compare", compare_int64);
      ("%i64_default", Pccall { func_name = "default_int64"; arity = 0 });
      ("%f32.neg", Parith { operand_type = F32; operator = Neg });
      ("%f32.add", Parith { operand_type = F32; operator = Add });
      ("%f32.sub", Parith { operand_type = F32; operator = Sub });
      ("%f32.mul", Parith { operand_type = F32; operator = Mul });
      ("%f32.div", Parith { operand_type = F32; operator = Div });
      ("%f32.sqrt", Parith { operand_type = F32; operator = Sqrt });
      ("%f32.abs", Parith { operand_type = F32; operator = Abs });
      ("%f32.eq", Pcomparison { operand_type = F32; operator = Eq });
      ("%f32.ne", Pcomparison { operand_type = F32; operator = Ne });
      ("%f32.compare", compare_float32);
      ("%i32.to_f32", Pconvert { from = I32; to_ = F32; kind = Convert });
      ("%i64.to_f32", Pconvert { from = I64; to_ = F32; kind = Convert });
      ("%f64.to_f32", Pconvert { from = F64; to_ = F32; kind = Convert });
      ("%f32.to_f64", Pconvert { from = F32; to_ = F64; kind = Convert });
      ("%byte.to_f32", Pconvert { from = U8; to_ = F32; kind = Convert });
      ("%u32.to_f32", Pconvert { from = U32; to_ = F32; kind = Convert });
      ("%u64.to_f32", Pconvert { from = U64; to_ = F32; kind = Convert });
      ("%f32.to_i32", Pconvert { from = F32; to_ = I32; kind = Convert });
      ("%f32.to_u32", Pconvert { from = F32; to_ = U32; kind = Convert });
      ("%f32.to_i64", Pconvert { from = F32; to_ = I64; kind = Convert });
      ("%f32.to_u64", Pconvert { from = F32; to_ = U64; kind = Convert });
      ( "%f32.to_i32_saturate",
        Pconvert { from = F32; to_ = I32; kind = Saturate } );
      ( "%f32.to_u32_saturate",
        Pconvert { from = F32; to_ = U32; kind = Saturate } );
      ( "%f32.to_i64_saturate",
        Pconvert { from = F32; to_ = I64; kind = Saturate } );
      ( "%f32.to_u64_saturate",
        Pconvert { from = F32; to_ = U64; kind = Saturate } );
      ( "%f32.to_i32_reinterpret",
        Pconvert { from = F32; to_ = I32; kind = Reinterpret } );
      ( "%i32.to_f32_reinterpret",
        Pconvert { from = I32; to_ = F32; kind = Reinterpret } );
      ("%f64_neg", Parith { operand_type = F64; operator = Neg });
      ("%f64_add", Parith { operand_type = F64; operator = Add });
      ("%f64_sub", Parith { operand_type = F64; operator = Sub });
      ("%f64_mul", Parith { operand_type = F64; operator = Mul });
      ("%f64_div", Parith { operand_type = F64; operator = Div });
      ("%f64_sqrt", Parith { operand_type = F64; operator = Sqrt });
      ("%f64.abs", Parith { operand_type = F64; operator = Abs });
      ("%f64_eq", Pcomparison { operand_type = F64; operator = Eq });
      ("%f64_ne", Pcomparison { operand_type = F64; operator = Ne });
      ("%f64_compare", compare_float);
      ("%f64_default", Pccall { func_name = "default_float"; arity = 0 });
      ("%char_to_int", Pidentity);
      ("%char_from_int", Pidentity);
      ("%char_eq", Pcomparison { operand_type = I32; operator = Eq });
      ("%char_compare", compare_char);
      ("%char_default", Pccall { func_name = "default_char"; arity = 0 });
      ("%u16_to_i32", Pconvert { from = U16; to_ = I32; kind = Convert });
      ("%u16_to_i64", Pconvert { from = U16; to_ = I64; kind = Convert });
      ("%u16_to_byte", Pconvert { from = U16; to_ = U8; kind = Convert });
      ("%i32_to_u16", Pconvert { from = I32; to_ = U16; kind = Convert });
      ("%i64_to_u16", Pconvert { from = I64; to_ = U16; kind = Convert });
      ("%byte_to_u16", Pconvert { from = U8; to_ = U16; kind = Convert });
      ("%i16_to_i32", Pconvert { from = I16; to_ = I32; kind = Convert });
      ("%i16_to_i64", Pconvert { from = I16; to_ = I64; kind = Convert });
      ("%i16_to_byte", Pconvert { from = I16; to_ = U8; kind = Convert });
      ("%i32_to_i16", Pconvert { from = I32; to_ = I16; kind = Convert });
      ("%i64_to_i16", Pconvert { from = I64; to_ = I16; kind = Convert });
      ("%byte_to_i16", Pconvert { from = U8; to_ = I16; kind = Convert });
      ("%i32_to_i64", Pconvert { from = I32; to_ = I64; kind = Convert });
      ("%i32_to_f64", Pconvert { from = I32; to_ = F64; kind = Convert });
      ("%i64_to_i32", Pconvert { from = I64; to_ = I32; kind = Convert });
      ("%i64_to_f64", Pconvert { from = I64; to_ = F64; kind = Convert });
      ( "%i64_to_f64_reinterpret",
        Pconvert { from = I64; to_ = F64; kind = Reinterpret } );
      ("%f64_default", Pccall { func_name = "default_float"; arity = 0 });
      ("%f64_to_i32", Pconvert { from = F64; to_ = I32; kind = Convert });
      ( "%f64_to_i32_saturate",
        Pconvert { from = F64; to_ = I32; kind = Saturate } );
      ("%f64_to_i64", Pconvert { from = F64; to_ = I64; kind = Convert });
      ( "%f64_to_i64_saturate",
        Pconvert { from = F64; to_ = I64; kind = Saturate } );
      ( "%f64_to_i64_reinterpret",
        Pconvert { from = F64; to_ = I64; kind = Reinterpret } );
      ("%byte_to_int", Pidentity);
      ("%byte_to_i64", Pconvert { from = U8; to_ = I64; kind = Convert });
      ("%byte_to_f64", Pconvert { from = U8; to_ = F64; kind = Convert });
      ("%i32_to_byte", Pconvert { from = I32; to_ = U8; kind = Convert });
      ("%i64_to_byte", Pconvert { from = I64; to_ = U8; kind = Convert });
      ("%f64_to_byte", Pconvert { from = F64; to_ = U8; kind = Convert });
      ( "%u32.to_i32_reinterpret",
        Pconvert { from = U32; to_ = I32; kind = Reinterpret } );
      ( "%i32.to_u32_reinterpret",
        Pconvert { from = I32; to_ = U32; kind = Reinterpret } );
      ( "%u64.to_i64_reinterpret",
        Pconvert { from = U64; to_ = I64; kind = Reinterpret } );
      ( "%i64.to_u64_reinterpret",
        Pconvert { from = I64; to_ = U64; kind = Reinterpret } );
      ("%f64.to_u32", Pconvert { from = F64; to_ = U32; kind = Convert });
      ( "%f64.to_u32_saturate",
        Pconvert { from = F64; to_ = U32; kind = Saturate } );
      ("%u32.to_u64", Pconvert { from = U32; to_ = U64; kind = Convert });
      ("%f64.to_u64", Pconvert { from = F64; to_ = U64; kind = Convert });
      ( "%f64.to_u64_saturate",
        Pconvert { from = F64; to_ = U64; kind = Saturate } );
      ("%u32.to_f64", Pconvert { from = U32; to_ = F64; kind = Convert });
      ("%u64.to_u32", Pconvert { from = U64; to_ = U32; kind = Convert });
      ("%u64.to_i32", Pconvert { from = U64; to_ = I32; kind = Convert });
      ("%u64.to_f64", Pconvert { from = U64; to_ = F64; kind = Convert });
      ("%error.to_string", Perror_to_string);
      ("%any.to_string", Pany_to_string);
      ("%char.to_string", Pintrinsic Char_to_string);
      ("%f64.to_string", Pintrinsic F64_to_string);
      ("%string.substring", Pintrinsic String_substring);
      ("%fixedarray.join", Pintrinsic FixedArray_join);
      ("%fixedarray.iter", Pintrinsic FixedArray_iter);
      ("%fixedarray.iteri", Pintrinsic FixedArray_iteri);
      ("%fixedarray.map", Pintrinsic FixedArray_map);
      ("%fixedarray.fold_left", Pintrinsic FixedArray_fold_left);
      ("%fixedarray.copy", Pintrinsic FixedArray_copy);
      ("%fixedarray.fill", Pintrinsic FixedArray_fill);
      ("%iter.map", Pintrinsic Iter_map);
      ("%iter.filter", Pintrinsic Iter_filter);
      ("%iter.iter", Pintrinsic Iter_iter);
      ("%iter.take", Pintrinsic Iter_take);
      ("%iter.reduce", Pintrinsic Iter_reduce);
      ("%iter.from_array", Pintrinsic Iter_from_array);
      ("%iter.flat_map", Pintrinsic Iter_flat_map);
      ("%iter.repeat", Pintrinsic Iter_repeat);
      ("%iter.concat", Pintrinsic Iter_concat);
      ("%array.length", Pintrinsic Array_length);
      ("%array.get", Pintrinsic Array_get);
      ("%array.unsafe_get", Pintrinsic Array_unsafe_get);
      ("%array.set", Pintrinsic Array_set);
      ("%array.unsafe_set", Pintrinsic Array_unsafe_set);
      ("%arrayview.length", Pintrinsic ArrayView_length);
      ("%arrayview.unsafe_get", Pintrinsic ArrayView_unsafe_get);
      ("%arrayview.unsafe_set", Pintrinsic ArrayView_unsafe_set);
      ("%arrayview.unsafe_as_view", Pintrinsic ArrayView_unsafe_as_view);
      ("%async.suspend", Pget_current_continuation);
      ("%async.run", Prun_async);
    ]

let find_prim (name : string) =
  (Hash_string.find_opt prim_table name : prim option)

let is_intrinsic (p : prim) = match p with Pintrinsic _ -> true | _ -> false

let is_pure (prim : prim) =
  match prim with
  | Pfixedarray_length | Penum_field _ | Pnot | Pstringlength | Pstringequal
  | Pbytesequal | Pignore | Pidentity | Pmakebytes | Pbyteslength
  | Pfixedarray_make _
  | Pfixedarray_get_item { kind = Unsafe }
  | Parray_make | Parith _ | Pbitwise _ | Pconvert _ | Pcast _ | Pcomparison _
  | Pcompare _ | Pclosure_to_extern_ref | Prefeq | Pnull | Pnull_string_extern
  | Pis_null | Pas_non_null | Pmake_value_or_error _ | Perror_to_string
  | Praw_func_to_func_ref | Pany_to_string ->
      true
  | Pfixedarray_get_item _ | Pgetstringitem _ | Pgetbytesitem _ | Pccall _
  | Pintrinsic _ | Ppanic | Punreachable | Pcatch | Pfixedarray_set_item _
  | Psetbytesitem _ | Pset_enum_field _ | Pprintln | Pget_current_continuation
  | Prun_async | Pcall_object_method _ ->
      false
