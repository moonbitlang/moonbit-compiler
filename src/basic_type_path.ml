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


module Config = Basic_config
module Map_string = Basic_map_string
module Hashf = Basic_hashf
module Hashsetf = Basic_hashsetf
module Constr_info = Basic_constr_info

type t =
  | T_unit
  | T_bool
  | T_byte
  | T_char
  | T_int
  | T_int16
  | T_int64
  | T_uint
  | T_uint16
  | T_uint64
  | T_float
  | T_double
  | T_string
  | T_option
  | T_result
  | T_error_value_result
  | T_fixedarray
  | T_bytes
  | T_ref
  | T_error
  | Toplevel of { pkg : string; id : string }
  | T_local of { toplevel_id : toplevel_id; name : string }
  | Tuple of int
  | Constr of { ty : t; tag : Constr_info.constr_tag }

and toplevel_id =
  | T_regular of { pkg : string; name : string }
  | T_method of { self_typ : t; name : string }
  | T_ext_method of { trait : t; self_typ : t; name : string }

include struct
  let _ = fun (_ : t) -> ()
  let _ = fun (_ : toplevel_id) -> ()

  let rec compare =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then 0
        else
          match (a__001_, b__002_) with
          | T_unit, T_unit -> 0
          | T_unit, _ -> -1
          | _, T_unit -> 1
          | T_bool, T_bool -> 0
          | T_bool, _ -> -1
          | _, T_bool -> 1
          | T_byte, T_byte -> 0
          | T_byte, _ -> -1
          | _, T_byte -> 1
          | T_char, T_char -> 0
          | T_char, _ -> -1
          | _, T_char -> 1
          | T_int, T_int -> 0
          | T_int, _ -> -1
          | _, T_int -> 1
          | T_int16, T_int16 -> 0
          | T_int16, _ -> -1
          | _, T_int16 -> 1
          | T_int64, T_int64 -> 0
          | T_int64, _ -> -1
          | _, T_int64 -> 1
          | T_uint, T_uint -> 0
          | T_uint, _ -> -1
          | _, T_uint -> 1
          | T_uint16, T_uint16 -> 0
          | T_uint16, _ -> -1
          | _, T_uint16 -> 1
          | T_uint64, T_uint64 -> 0
          | T_uint64, _ -> -1
          | _, T_uint64 -> 1
          | T_float, T_float -> 0
          | T_float, _ -> -1
          | _, T_float -> 1
          | T_double, T_double -> 0
          | T_double, _ -> -1
          | _, T_double -> 1
          | T_string, T_string -> 0
          | T_string, _ -> -1
          | _, T_string -> 1
          | T_option, T_option -> 0
          | T_option, _ -> -1
          | _, T_option -> 1
          | T_result, T_result -> 0
          | T_result, _ -> -1
          | _, T_result -> 1
          | T_error_value_result, T_error_value_result -> 0
          | T_error_value_result, _ -> -1
          | _, T_error_value_result -> 1
          | T_fixedarray, T_fixedarray -> 0
          | T_fixedarray, _ -> -1
          | _, T_fixedarray -> 1
          | T_bytes, T_bytes -> 0
          | T_bytes, _ -> -1
          | _, T_bytes -> 1
          | T_ref, T_ref -> 0
          | T_ref, _ -> -1
          | _, T_ref -> 1
          | T_error, T_error -> 0
          | T_error, _ -> -1
          | _, T_error -> 1
          | Toplevel _a__003_, Toplevel _b__004_ -> (
              match Stdlib.compare (_a__003_.pkg : string) _b__004_.pkg with
              | 0 -> Stdlib.compare (_a__003_.id : string) _b__004_.id
              | n -> n)
          | Toplevel _, _ -> -1
          | _, Toplevel _ -> 1
          | T_local _a__005_, T_local _b__006_ -> (
              match
                compare_toplevel_id _a__005_.toplevel_id _b__006_.toplevel_id
              with
              | 0 -> Stdlib.compare (_a__005_.name : string) _b__006_.name
              | n -> n)
          | T_local _, _ -> -1
          | _, T_local _ -> 1
          | Tuple _a__007_, Tuple _b__008_ ->
              Stdlib.compare (_a__007_ : int) _b__008_
          | Tuple _, _ -> -1
          | _, Tuple _ -> 1
          | Constr _a__009_, Constr _b__010_ -> (
              match compare _a__009_.ty _b__010_.ty with
              | 0 -> Constr_info.compare_constr_tag _a__009_.tag _b__010_.tag
              | n -> n)
      : t -> t -> int)

  and compare_toplevel_id =
    (fun a__011_ ->
       fun b__012_ ->
        if Stdlib.( == ) a__011_ b__012_ then 0
        else
          match (a__011_, b__012_) with
          | T_regular _a__013_, T_regular _b__014_ -> (
              match Stdlib.compare (_a__013_.pkg : string) _b__014_.pkg with
              | 0 -> Stdlib.compare (_a__013_.name : string) _b__014_.name
              | n -> n)
          | T_regular _, _ -> -1
          | _, T_regular _ -> 1
          | T_method _a__015_, T_method _b__016_ -> (
              match compare _a__015_.self_typ _b__016_.self_typ with
              | 0 -> Stdlib.compare (_a__015_.name : string) _b__016_.name
              | n -> n)
          | T_method _, _ -> -1
          | _, T_method _ -> 1
          | T_ext_method _a__017_, T_ext_method _b__018_ -> (
              match compare _a__017_.trait _b__018_.trait with
              | 0 -> (
                  match compare _a__017_.self_typ _b__018_.self_typ with
                  | 0 -> Stdlib.compare (_a__017_.name : string) _b__018_.name
                  | n -> n)
              | n -> n)
      : toplevel_id -> toplevel_id -> int)

  let _ = compare
  and _ = compare_toplevel_id

  let rec (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | T_unit -> Ppx_base.hash_fold_int hsv 0
        | T_bool -> Ppx_base.hash_fold_int hsv 1
        | T_byte -> Ppx_base.hash_fold_int hsv 2
        | T_char -> Ppx_base.hash_fold_int hsv 3
        | T_int -> Ppx_base.hash_fold_int hsv 4
        | T_int16 -> Ppx_base.hash_fold_int hsv 5
        | T_int64 -> Ppx_base.hash_fold_int hsv 6
        | T_uint -> Ppx_base.hash_fold_int hsv 7
        | T_uint16 -> Ppx_base.hash_fold_int hsv 8
        | T_uint64 -> Ppx_base.hash_fold_int hsv 9
        | T_float -> Ppx_base.hash_fold_int hsv 10
        | T_double -> Ppx_base.hash_fold_int hsv 11
        | T_string -> Ppx_base.hash_fold_int hsv 12
        | T_option -> Ppx_base.hash_fold_int hsv 13
        | T_result -> Ppx_base.hash_fold_int hsv 14
        | T_error_value_result -> Ppx_base.hash_fold_int hsv 15
        | T_fixedarray -> Ppx_base.hash_fold_int hsv 16
        | T_bytes -> Ppx_base.hash_fold_int hsv 17
        | T_ref -> Ppx_base.hash_fold_int hsv 18
        | T_error -> Ppx_base.hash_fold_int hsv 19
        | Toplevel _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 20 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_string hsv _ir.pkg
            in
            Ppx_base.hash_fold_string hsv _ir.id
        | T_local _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 21 in
            let hsv =
              let hsv = hsv in
              hash_fold_toplevel_id hsv _ir.toplevel_id
            in
            Ppx_base.hash_fold_string hsv _ir.name
        | Tuple _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 22 in
            let hsv = hsv in
            Ppx_base.hash_fold_int hsv _a0
        | Constr _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 23 in
            let hsv =
              let hsv = hsv in
              hash_fold_t hsv _ir.ty
            in
            Constr_info.hash_fold_constr_tag hsv _ir.tag
      : Ppx_base.state -> t -> Ppx_base.state)

  and (hash_fold_toplevel_id : Ppx_base.state -> toplevel_id -> Ppx_base.state)
      =
    (fun hsv ->
       fun arg ->
        match arg with
        | T_regular _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_string hsv _ir.pkg
            in
            Ppx_base.hash_fold_string hsv _ir.name
        | T_method _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv =
              let hsv = hsv in
              hash_fold_t hsv _ir.self_typ
            in
            Ppx_base.hash_fold_string hsv _ir.name
        | T_ext_method _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv =
              let hsv =
                let hsv = hsv in
                hash_fold_t hsv _ir.trait
              in
              hash_fold_t hsv _ir.self_typ
            in
            Ppx_base.hash_fold_string hsv _ir.name
      : Ppx_base.state -> toplevel_id -> Ppx_base.state)

  and (hash : t -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_t hsv arg)
    in
    fun x -> func x

  and (hash_toplevel_id : toplevel_id -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_toplevel_id hsv arg)
    in
    fun x -> func x

  let _ = hash_fold_t
  and _ = hash_fold_toplevel_id
  and _ = hash
  and _ = hash_toplevel_id

  let rec equal =
    (fun a__019_ ->
       fun b__020_ ->
        if Stdlib.( == ) a__019_ b__020_ then true
        else
          match (a__019_, b__020_) with
          | T_unit, T_unit -> true
          | T_unit, _ -> false
          | _, T_unit -> false
          | T_bool, T_bool -> true
          | T_bool, _ -> false
          | _, T_bool -> false
          | T_byte, T_byte -> true
          | T_byte, _ -> false
          | _, T_byte -> false
          | T_char, T_char -> true
          | T_char, _ -> false
          | _, T_char -> false
          | T_int, T_int -> true
          | T_int, _ -> false
          | _, T_int -> false
          | T_int16, T_int16 -> true
          | T_int16, _ -> false
          | _, T_int16 -> false
          | T_int64, T_int64 -> true
          | T_int64, _ -> false
          | _, T_int64 -> false
          | T_uint, T_uint -> true
          | T_uint, _ -> false
          | _, T_uint -> false
          | T_uint16, T_uint16 -> true
          | T_uint16, _ -> false
          | _, T_uint16 -> false
          | T_uint64, T_uint64 -> true
          | T_uint64, _ -> false
          | _, T_uint64 -> false
          | T_float, T_float -> true
          | T_float, _ -> false
          | _, T_float -> false
          | T_double, T_double -> true
          | T_double, _ -> false
          | _, T_double -> false
          | T_string, T_string -> true
          | T_string, _ -> false
          | _, T_string -> false
          | T_option, T_option -> true
          | T_option, _ -> false
          | _, T_option -> false
          | T_result, T_result -> true
          | T_result, _ -> false
          | _, T_result -> false
          | T_error_value_result, T_error_value_result -> true
          | T_error_value_result, _ -> false
          | _, T_error_value_result -> false
          | T_fixedarray, T_fixedarray -> true
          | T_fixedarray, _ -> false
          | _, T_fixedarray -> false
          | T_bytes, T_bytes -> true
          | T_bytes, _ -> false
          | _, T_bytes -> false
          | T_ref, T_ref -> true
          | T_ref, _ -> false
          | _, T_ref -> false
          | T_error, T_error -> true
          | T_error, _ -> false
          | _, T_error -> false
          | Toplevel _a__021_, Toplevel _b__022_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__021_.pkg : string) _b__022_.pkg)
                (Stdlib.( = ) (_a__021_.id : string) _b__022_.id)
          | Toplevel _, _ -> false
          | _, Toplevel _ -> false
          | T_local _a__023_, T_local _b__024_ ->
              Stdlib.( && )
                (equal_toplevel_id _a__023_.toplevel_id _b__024_.toplevel_id)
                (Stdlib.( = ) (_a__023_.name : string) _b__024_.name)
          | T_local _, _ -> false
          | _, T_local _ -> false
          | Tuple _a__025_, Tuple _b__026_ ->
              Stdlib.( = ) (_a__025_ : int) _b__026_
          | Tuple _, _ -> false
          | _, Tuple _ -> false
          | Constr _a__027_, Constr _b__028_ ->
              Stdlib.( && )
                (equal _a__027_.ty _b__028_.ty)
                (Constr_info.equal_constr_tag _a__027_.tag _b__028_.tag)
      : t -> t -> bool)

  and equal_toplevel_id =
    (fun a__029_ ->
       fun b__030_ ->
        if Stdlib.( == ) a__029_ b__030_ then true
        else
          match (a__029_, b__030_) with
          | T_regular _a__031_, T_regular _b__032_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__031_.pkg : string) _b__032_.pkg)
                (Stdlib.( = ) (_a__031_.name : string) _b__032_.name)
          | T_regular _, _ -> false
          | _, T_regular _ -> false
          | T_method _a__033_, T_method _b__034_ ->
              Stdlib.( && )
                (equal _a__033_.self_typ _b__034_.self_typ)
                (Stdlib.( = ) (_a__033_.name : string) _b__034_.name)
          | T_method _, _ -> false
          | _, T_method _ -> false
          | T_ext_method _a__035_, T_ext_method _b__036_ ->
              Stdlib.( && )
                (equal _a__035_.trait _b__036_.trait)
                (Stdlib.( && )
                   (equal _a__035_.self_typ _b__036_.self_typ)
                   (Stdlib.( = ) (_a__035_.name : string) _b__036_.name))
      : toplevel_id -> toplevel_id -> bool)

  let _ = equal
  and _ = equal_toplevel_id
end

let equal t1 t2 =
  match (t1, t2) with
  | T_result, T_error_value_result | T_error_value_result, T_result -> true
  | _ -> equal t1 t2

let rec name_aux ~compress ~cur_pkg_name = function
  | T_unit -> "Unit"
  | T_bool -> "Bool"
  | T_byte -> "Byte"
  | T_char -> "Char"
  | T_int -> "Int"
  | T_int16 -> "Int16"
  | T_int64 -> "Int64"
  | T_uint -> "UInt"
  | T_uint16 -> "UInt16"
  | T_uint64 -> "UInt64"
  | T_float -> "Float"
  | T_double -> "Double"
  | T_string -> "String"
  | T_option -> "Option"
  | T_result -> "Result"
  | T_error_value_result -> "Result"
  | T_fixedarray -> "FixedArray"
  | T_bytes -> "Bytes"
  | T_ref -> "Ref"
  | T_error -> "Error"
  | Toplevel { pkg; id } -> (
      if pkg = "" then id
      else
        match cur_pkg_name with
        | Some pkg_name when pkg = pkg_name -> id
        | _ when compress ->
            if pkg = Config.builtin_package then id
            else
              let pkg =
                Map_string.find_default !Config.current_import_map pkg pkg
              in
              (Stdlib.String.concat "" [ "@"; pkg; "."; id ] [@merlin.hide])
        | _ -> Stdlib.String.concat "" [ "@"; pkg; "."; id ] [@merlin.hide])
  | Tuple n -> ("Tuple" ^ Int.to_string n : Stdlib.String.t) [@merlin.hide]
  | Constr
      {
        ty;
        tag =
          Constr_tag_regular { name_; _ } | Extensible_tag { name = name_; _ };
      } ->
      (name_aux ~compress ~cur_pkg_name ty ^ "." ^ name_
        : Stdlib.String.t)
        [@merlin.hide]
  | T_local { toplevel_id; name } ->
      if compress then name
      else
        let toplevel_id = toplevel_id_to_string toplevel_id in
        ((toplevel_id ^ "." ^ name : Stdlib.String.t) [@merlin.hide])

and toplevel_id_to_string (id : toplevel_id) =
  (match id with
   | T_regular { pkg; name } ->
       if pkg = "" then name
       else if pkg = Config.builtin_package then "$builtin." ^ name
       else Stdlib.String.concat "" [ "$"; pkg; "."; name ] [@merlin.hide]
   | T_method { self_typ; name } ->
       let typ_str = name_aux ~cur_pkg_name:None ~compress:false self_typ in
       ((typ_str ^ "::" ^ name : Stdlib.String.t) [@merlin.hide])
   | T_ext_method { trait; self_typ; name } ->
       let trait_str = name_aux ~cur_pkg_name:None ~compress:false trait in
       let typ_str = name_aux ~cur_pkg_name:None ~compress:false self_typ in
       (Stdlib.String.concat ""
          [ trait_str; "::"; name; "|"; typ_str; "|" ] [@merlin.hide])
    : string)

let map_toplevel_id id f =
  match id with
  | T_regular { pkg; name } -> T_regular { pkg; name = f name }
  | T_method { self_typ; name } -> T_method { self_typ; name = f name }
  | T_ext_method { trait; self_typ; name } ->
      T_ext_method { trait; self_typ; name = f name }

let export_name ?cur_pkg_name t = name_aux ~compress:false ~cur_pkg_name t
let short_name ~cur_pkg_name t = name_aux ~compress:true ~cur_pkg_name t

let sexp_of_t t =
  S.Atom (short_name ~cur_pkg_name:(Some !Config.current_package) t)

let sexp_of_toplevel_id id = S.Atom (toplevel_id_to_string id)
let toplevel_type ~pkg t = Toplevel { pkg; id = t }
let local_type toplevel_id name = T_local { toplevel_id; name }
let constr ~ty ~tag = Constr { ty; tag }
let tuple_2 = Tuple 2
let tuple_3 = Tuple 3
let tuple_4 = Tuple 4
let tuple_5 = Tuple 5
let tuple_6 = Tuple 6

let tuple n =
  match n with
  | 2 -> tuple_2
  | 3 -> tuple_3
  | 4 -> tuple_4
  | 5 -> tuple_5
  | 6 -> tuple_6
  | _ -> Tuple n

let rec get_pkg t =
  match t with
  | Toplevel { pkg; id }
    when pkg = Config.builtin_package && (id = "Array" || id = "ArrayView") ->
      "moonbitlang/core/array"
  | Toplevel { pkg; id = "Json" } when pkg = Config.builtin_package ->
      "moonbitlang/core/json"
  | Toplevel { pkg; id } when pkg = Config.builtin_package && id = "BigInt" ->
      "moonbitlang/core/bigint"
  | T_fixedarray -> "moonbitlang/core/array"
  | T_local { toplevel_id; _ } -> (
      match toplevel_id with
      | T_regular _ -> !Config.current_package
      | T_method { self_typ; _ } -> get_pkg self_typ
      | T_ext_method { trait; _ } -> get_pkg trait)
  | Toplevel { pkg; id = _ } -> pkg
  | Constr { ty; tag = _ } -> get_pkg ty
  | Tuple _ -> "moonbitlang/core/tuple"
  | T_unit -> "moonbitlang/core/unit"
  | T_bool -> "moonbitlang/core/bool"
  | T_byte -> "moonbitlang/core/byte"
  | T_char -> "moonbitlang/core/char"
  | T_int -> "moonbitlang/core/int"
  | T_int16 -> "moonbitlang/core/int16"
  | T_int64 -> "moonbitlang/core/int64"
  | T_uint -> "moonbitlang/core/uint"
  | T_uint16 -> "moonbitlang/core/uint16"
  | T_uint64 -> "moonbitlang/core/uint64"
  | T_float -> "moonbitlang/core/float"
  | T_double -> "moonbitlang/core/double"
  | T_string -> "moonbitlang/core/string"
  | T_option -> "moonbitlang/core/option"
  | T_result -> "moonbitlang/core/result"
  | T_bytes -> "moonbitlang/core/bytes"
  | T_ref -> "moonbitlang/core/ref"
  | T_error -> "moonbitlang/core/error"
  | T_error_value_result -> "moonbitlang/core/result"

let rec get_base_id t =
  match t with
  | Toplevel { id; _ } -> id
  | T_fixedarray -> "FixedArray"
  | T_local { name; _ } -> name
  | Constr { ty; tag = _ } -> get_base_id ty
  | Tuple i -> "Tuple" ^ string_of_int i
  | T_unit -> "Unit"
  | T_bool -> "Bool"
  | T_byte -> "Byte"
  | T_char -> "Char"
  | T_int16 -> "Int16"
  | T_uint16 -> "UInt16"
  | T_int -> "Int"
  | T_int64 -> "Int64"
  | T_uint -> "UInt"
  | T_uint64 -> "UInt64"
  | T_float -> "Float"
  | T_double -> "Double"
  | T_string -> "String"
  | T_option -> "Option"
  | T_result -> "Result"
  | T_bytes -> "Bytes"
  | T_ref -> "Ref"
  | T_error -> "Error"
  | T_error_value_result -> "Result"

let pkg_of_toplevel_id (id : toplevel_id) =
  match id with
  | T_regular { pkg; _ } -> pkg
  | T_method { self_typ; _ } -> get_pkg self_typ
  | T_ext_method { trait; _ } -> get_pkg trait

let can_be_extended_from_builtin t =
  match t with
  | Tuple _ | T_unit | T_bool | T_byte | T_char | T_int | T_int16 | T_int64
  | T_uint | T_uint16 | T_uint64 | T_float | T_double | T_string | T_option
  | T_result | T_bytes | T_fixedarray | T_ref | T_error | T_error_value_result
    ->
      true
  | Toplevel { pkg; id = _ } -> pkg = Config.builtin_package
  | T_local _ -> false
  | Constr _ -> false

module Builtin = struct
  let type_path_unit = T_unit
  let type_path_bool = T_bool
  let type_path_byte = T_byte
  let type_path_char = T_char
  let type_path_int = T_int
  let type_path_int16 = T_int16
  let type_path_int64 = T_int64
  let type_path_uint = T_uint
  let type_path_uint16 = T_uint16
  let type_path_uint64 = T_uint64
  let type_path_float = T_float
  let type_path_double = T_double
  let type_path_string = T_string
  let type_path_option = T_option
  let type_path_result = T_result
  let type_path_multi_value_result = T_error_value_result
  let type_path_fixedarray = T_fixedarray
  let type_path_bytes = T_bytes
  let type_path_ref = T_ref
  let type_path_error = T_error

  let type_path_js_string =
    Toplevel { pkg = Config.builtin_package; id = "Js_string" }

  let type_path_sourceloc =
    Toplevel { pkg = Config.builtin_package; id = "SourceLoc" }

  let type_path_argsloc =
    Toplevel { pkg = Config.builtin_package; id = "ArgsLoc" }

  let type_path_array = Toplevel { pkg = Config.builtin_package; id = "Array" }

  let type_path_arrayview =
    Toplevel { pkg = Config.builtin_package; id = "ArrayView" }

  let type_path_map = Toplevel { pkg = Config.builtin_package; id = "Map" }
  let type_path_json = Toplevel { pkg = Config.builtin_package; id = "Json" }
  let type_path_iter = Toplevel { pkg = Config.builtin_package; id = "Iter" }
  let type_path_iter2 = Toplevel { pkg = Config.builtin_package; id = "Iter2" }

  let type_path_iter_result =
    Toplevel { pkg = Config.builtin_package; id = "IterResult" }

  let type_path_bytesview =
    Toplevel { pkg = Config.builtin_package; id = "BytesView" }

  let type_path_bytes_view =
    Toplevel { pkg = "moonbitlang/core/bytes"; id = "View" }

  let type_path_stringview =
    Toplevel { pkg = "moonbitlang/core/string"; id = "StringView" }

  let type_path_foreach_result =
    Toplevel { pkg = Config.builtin_package; id = "ForeachResult" }

  let type_path_bigint =
    Toplevel { pkg = Config.builtin_package; id = "BigInt" }

  let type_path_maybe_uninit =
    Toplevel { pkg = Config.builtin_package; id = "UnsafeMaybeUninit" }

  let type_path_func_ref =
    Toplevel { pkg = Config.builtin_package; id = "FuncRef" }

  let trait_eq = Toplevel { pkg = Config.builtin_package; id = "Eq" }
  let trait_compare = Toplevel { pkg = Config.builtin_package; id = "Compare" }
  let trait_hash = Toplevel { pkg = Config.builtin_package; id = "Hash" }
  let trait_show = Toplevel { pkg = Config.builtin_package; id = "Show" }
  let trait_default = Toplevel { pkg = Config.builtin_package; id = "Default" }
  let default_impl_placeholder = Toplevel { pkg = ""; id = "$default_impl" }
end

module Hash = Hashf.Make (struct
  type nonrec t = t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_t

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

module Hashset = Hashsetf.Make (struct
  type nonrec t = t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_t

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

let can_use_array_pattern (p : t) =
  equal p Builtin.type_path_fixedarray
  || equal p Builtin.type_path_array
  || equal p Builtin.type_path_arrayview

let is_multi_value_result (p : t) =
  match p with T_error_value_result -> true | _ -> false

let is_bytesview t =
  equal t Builtin.type_path_bytesview || equal t Builtin.type_path_bytes_view
