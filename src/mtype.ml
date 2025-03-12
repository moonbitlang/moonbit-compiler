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


module Hash_string = Basic_hash_string
module Type_path = Basic_type_path
module Lst = Basic_lst

type id = string

include struct
  let _ = fun (_ : id) -> ()
  let sexp_of_id = (Moon_sexp_conv.sexp_of_string : id -> S.t)
  let _ = sexp_of_id
end

module Id_hash : Basic_hash_intf.S with type key = id = Basic_hash_string

let id_to_string (s : id) = s
let id_to_tid (s : id) = Basic_ty_ident.of_string s

type t =
  | T_int
  | T_char
  | T_bool
  | T_unit
  | T_byte
  | T_int16
  | T_uint16
  | T_int64
  | T_uint
  | T_uint64
  | T_float
  | T_double
  | T_string
  | T_bytes
  | T_optimized_option of { elem : t }
  | T_func of { params : t list; return : t }
  | T_raw_func of { params : t list; return : t }
  | T_tuple of { tys : t list }
  | T_fixedarray of { elem : t }
  | T_constr of id
  | T_trait of id
  | T_any of { name : id }
  | T_maybe_uninit of t
  | T_error_value_result of { ok : t; err : t; id : id }
[@@warning "+4"]

include struct
  let _ = fun (_ : t) -> ()

  let rec sexp_of_t =
    (function
     | T_int -> S.Atom "T_int"
     | T_char -> S.Atom "T_char"
     | T_bool -> S.Atom "T_bool"
     | T_unit -> S.Atom "T_unit"
     | T_byte -> S.Atom "T_byte"
     | T_int16 -> S.Atom "T_int16"
     | T_uint16 -> S.Atom "T_uint16"
     | T_int64 -> S.Atom "T_int64"
     | T_uint -> S.Atom "T_uint"
     | T_uint64 -> S.Atom "T_uint64"
     | T_float -> S.Atom "T_float"
     | T_double -> S.Atom "T_double"
     | T_string -> S.Atom "T_string"
     | T_bytes -> S.Atom "T_bytes"
     | T_optimized_option { elem = elem__002_ } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__003_ = sexp_of_t elem__002_ in
           (S.List [ S.Atom "elem"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_optimized_option" :: bnds__001_)
     | T_func { params = params__005_; return = return__007_ } ->
         let bnds__004_ = ([] : _ Stdlib.List.t) in
         let bnds__004_ =
           let arg__008_ = sexp_of_t return__007_ in
           (S.List [ S.Atom "return"; arg__008_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         let bnds__004_ =
           let arg__006_ = Moon_sexp_conv.sexp_of_list sexp_of_t params__005_ in
           (S.List [ S.Atom "params"; arg__006_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_func" :: bnds__004_)
     | T_raw_func { params = params__010_; return = return__012_ } ->
         let bnds__009_ = ([] : _ Stdlib.List.t) in
         let bnds__009_ =
           let arg__013_ = sexp_of_t return__012_ in
           (S.List [ S.Atom "return"; arg__013_ ] :: bnds__009_
             : _ Stdlib.List.t)
         in
         let bnds__009_ =
           let arg__011_ = Moon_sexp_conv.sexp_of_list sexp_of_t params__010_ in
           (S.List [ S.Atom "params"; arg__011_ ] :: bnds__009_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_raw_func" :: bnds__009_)
     | T_tuple { tys = tys__015_ } ->
         let bnds__014_ = ([] : _ Stdlib.List.t) in
         let bnds__014_ =
           let arg__016_ = Moon_sexp_conv.sexp_of_list sexp_of_t tys__015_ in
           (S.List [ S.Atom "tys"; arg__016_ ] :: bnds__014_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_tuple" :: bnds__014_)
     | T_fixedarray { elem = elem__018_ } ->
         let bnds__017_ = ([] : _ Stdlib.List.t) in
         let bnds__017_ =
           let arg__019_ = sexp_of_t elem__018_ in
           (S.List [ S.Atom "elem"; arg__019_ ] :: bnds__017_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_fixedarray" :: bnds__017_)
     | T_constr arg0__020_ ->
         let res0__021_ = sexp_of_id arg0__020_ in
         S.List [ S.Atom "T_constr"; res0__021_ ]
     | T_trait arg0__022_ ->
         let res0__023_ = sexp_of_id arg0__022_ in
         S.List [ S.Atom "T_trait"; res0__023_ ]
     | T_any { name = name__025_ } ->
         let bnds__024_ = ([] : _ Stdlib.List.t) in
         let bnds__024_ =
           let arg__026_ = sexp_of_id name__025_ in
           (S.List [ S.Atom "name"; arg__026_ ] :: bnds__024_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_any" :: bnds__024_)
     | T_maybe_uninit arg0__027_ ->
         let res0__028_ = sexp_of_t arg0__027_ in
         S.List [ S.Atom "T_maybe_uninit"; res0__028_ ]
     | T_error_value_result { ok = ok__030_; err = err__032_; id = id__034_ } ->
         let bnds__029_ = ([] : _ Stdlib.List.t) in
         let bnds__029_ =
           let arg__035_ = sexp_of_id id__034_ in
           (S.List [ S.Atom "id"; arg__035_ ] :: bnds__029_ : _ Stdlib.List.t)
         in
         let bnds__029_ =
           let arg__033_ = sexp_of_t err__032_ in
           (S.List [ S.Atom "err"; arg__033_ ] :: bnds__029_ : _ Stdlib.List.t)
         in
         let bnds__029_ =
           let arg__031_ = sexp_of_t ok__030_ in
           (S.List [ S.Atom "ok"; arg__031_ ] :: bnds__029_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "T_error_value_result" :: bnds__029_)
      : t -> S.t)

  let _ = sexp_of_t
end

let is_numeric (t : t) =
  match t with
  | T_unit | T_int | T_uint | T_char | T_bool | T_byte | T_int16 | T_uint16
  | T_int64 | T_uint64 | T_float | T_double ->
      true
  | T_optimized_option { elem } -> (
      match elem with
      | T_char | T_bool | T_byte | T_unit | T_int | T_uint | T_int16 | T_uint16
        ->
          true
      | T_int64 | T_uint64 | T_float | T_double | T_optimized_option _ | T_bytes
      | T_string | T_func _ | T_raw_func _ | T_tuple _ | T_fixedarray _
      | T_maybe_uninit _ | T_constr _ | T_trait _ | T_any _
      | T_error_value_result _ ->
          false)
  | T_bytes | T_string | T_func _ | T_raw_func _ | T_tuple _ | T_fixedarray _
  | T_maybe_uninit _ | T_constr _ | T_trait _ | T_any _ | T_error_value_result _
    ->
      false

type field_name = Named of string | Indexed of int

include struct
  let _ = fun (_ : field_name) -> ()

  let sexp_of_field_name =
    (function
     | Named arg0__036_ ->
         let res0__037_ = Moon_sexp_conv.sexp_of_string arg0__036_ in
         S.List [ S.Atom "Named"; res0__037_ ]
     | Indexed arg0__038_ ->
         let res0__039_ = Moon_sexp_conv.sexp_of_int arg0__038_ in
         S.List [ S.Atom "Indexed"; res0__039_ ]
      : field_name -> S.t)

  let _ = sexp_of_field_name
end

let field_index0 = Indexed 0
let field_index1 = Indexed 1
let field_index2 = Indexed 2
let field_index3 = Indexed 3

let field_indexed i =
  match i with
  | 0 -> field_index0
  | 1 -> field_index1
  | 2 -> field_index2
  | 3 -> field_index3
  | n -> Indexed n

type field_info = { field_type : t; name : field_name; mut : bool }

include struct
  let _ = fun (_ : field_info) -> ()

  let sexp_of_field_info =
    (fun { field_type = field_type__041_; name = name__043_; mut = mut__045_ }
     ->
       let bnds__040_ = ([] : _ Stdlib.List.t) in
       let bnds__040_ =
         let arg__046_ = Moon_sexp_conv.sexp_of_bool mut__045_ in
         (S.List [ S.Atom "mut"; arg__046_ ] :: bnds__040_ : _ Stdlib.List.t)
       in
       let bnds__040_ =
         let arg__044_ = sexp_of_field_name name__043_ in
         (S.List [ S.Atom "name"; arg__044_ ] :: bnds__040_ : _ Stdlib.List.t)
       in
       let bnds__040_ =
         let arg__042_ = sexp_of_t field_type__041_ in
         (S.List [ S.Atom "field_type"; arg__042_ ] :: bnds__040_
           : _ Stdlib.List.t)
       in
       S.List bnds__040_
      : field_info -> S.t)

  let _ = sexp_of_field_info
end

type constr_info = { payload : field_info list; tag : Tag.t }

include struct
  let _ = fun (_ : constr_info) -> ()

  let sexp_of_constr_info =
    (fun { payload = payload__048_; tag = tag__050_ } ->
       let bnds__047_ = ([] : _ Stdlib.List.t) in
       let bnds__047_ =
         let arg__051_ = Tag.sexp_of_t tag__050_ in
         (S.List [ S.Atom "tag"; arg__051_ ] :: bnds__047_ : _ Stdlib.List.t)
       in
       let bnds__047_ =
         let arg__049_ =
           Moon_sexp_conv.sexp_of_list sexp_of_field_info payload__048_
         in
         (S.List [ S.Atom "payload"; arg__049_ ] :: bnds__047_
           : _ Stdlib.List.t)
       in
       S.List bnds__047_
      : constr_info -> S.t)

  let _ = sexp_of_constr_info
end

type method_info = {
  params_ty : t list;
  return_ty : t;
  index : int;
  name : string;
}

include struct
  let _ = fun (_ : method_info) -> ()

  let sexp_of_method_info =
    (fun {
           params_ty = params_ty__053_;
           return_ty = return_ty__055_;
           index = index__057_;
           name = name__059_;
         }
     ->
       let bnds__052_ = ([] : _ Stdlib.List.t) in
       let bnds__052_ =
         let arg__060_ = Moon_sexp_conv.sexp_of_string name__059_ in
         (S.List [ S.Atom "name"; arg__060_ ] :: bnds__052_ : _ Stdlib.List.t)
       in
       let bnds__052_ =
         let arg__058_ = Moon_sexp_conv.sexp_of_int index__057_ in
         (S.List [ S.Atom "index"; arg__058_ ] :: bnds__052_ : _ Stdlib.List.t)
       in
       let bnds__052_ =
         let arg__056_ = sexp_of_t return_ty__055_ in
         (S.List [ S.Atom "return_ty"; arg__056_ ] :: bnds__052_
           : _ Stdlib.List.t)
       in
       let bnds__052_ =
         let arg__054_ =
           Moon_sexp_conv.sexp_of_list sexp_of_t params_ty__053_
         in
         (S.List [ S.Atom "params_ty"; arg__054_ ] :: bnds__052_
           : _ Stdlib.List.t)
       in
       S.List bnds__052_
      : method_info -> S.t)

  let _ = sexp_of_method_info
end

type info =
  | Placeholder
  | Externref
  | Variant of { constrs : constr_info list }
  | Variant_constr
  | Constant_variant_constr
  | Record of { fields : field_info list }
  | Trait of { methods : method_info list }

include struct
  let _ = fun (_ : info) -> ()

  let sexp_of_info =
    (function
     | Placeholder -> S.Atom "Placeholder"
     | Externref -> S.Atom "Externref"
     | Variant { constrs = constrs__062_ } ->
         let bnds__061_ = ([] : _ Stdlib.List.t) in
         let bnds__061_ =
           let arg__063_ =
             Moon_sexp_conv.sexp_of_list sexp_of_constr_info constrs__062_
           in
           (S.List [ S.Atom "constrs"; arg__063_ ] :: bnds__061_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Variant" :: bnds__061_)
     | Variant_constr -> S.Atom "Variant_constr"
     | Constant_variant_constr -> S.Atom "Constant_variant_constr"
     | Record { fields = fields__065_ } ->
         let bnds__064_ = ([] : _ Stdlib.List.t) in
         let bnds__064_ =
           let arg__066_ =
             Moon_sexp_conv.sexp_of_list sexp_of_field_info fields__065_
           in
           (S.List [ S.Atom "fields"; arg__066_ ] :: bnds__064_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Record" :: bnds__064_)
     | Trait { methods = methods__068_ } ->
         let bnds__067_ = ([] : _ Stdlib.List.t) in
         let bnds__067_ =
           let arg__069_ =
             Moon_sexp_conv.sexp_of_list sexp_of_method_info methods__068_
           in
           (S.List [ S.Atom "methods"; arg__069_ ] :: bnds__067_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Trait" :: bnds__067_)
      : info -> S.t)

  let _ = sexp_of_info
end

type defs = { defs : info Id_hash.t; ext_tags : int Hash_string.t }

include struct
  let _ = fun (_ : defs) -> ()

  let sexp_of_defs =
    (fun { defs = defs__071_; ext_tags = ext_tags__073_ } ->
       let bnds__070_ = ([] : _ Stdlib.List.t) in
       let bnds__070_ =
         let arg__074_ =
           Hash_string.sexp_of_t Moon_sexp_conv.sexp_of_int ext_tags__073_
         in
         (S.List [ S.Atom "ext_tags"; arg__074_ ] :: bnds__070_
           : _ Stdlib.List.t)
       in
       let bnds__070_ =
         let arg__072_ = Id_hash.sexp_of_t sexp_of_info defs__071_ in
         (S.List [ S.Atom "defs"; arg__072_ ] :: bnds__070_ : _ Stdlib.List.t)
       in
       S.List bnds__070_
      : defs -> S.t)

  let _ = sexp_of_defs
end

let sexp_of_defs defs =
  if Hash_string.length defs.ext_tags = 0 then
    (fun x__075_ -> Id_hash.sexp_of_t sexp_of_info x__075_) defs.defs
  else sexp_of_defs defs

let find_stype_exn (stype_defs : Typing_info.stype_defs) (pkg : string)
    (id : string) =
  (let types = Hash_string.find_exn stype_defs pkg in
   Typing_info.find_type_exn types id
    : Typedecl_info.t)

let find_trait_exn (stype_defs : Typing_info.stype_defs) (pkg : string)
    (id : string) =
  (let types = Hash_string.find_exn stype_defs pkg in
   Typing_info.find_trait_exn types id
    : Trait_decl.t)

let is_optimizable_option_elem t mtype_defs =
  match t with
  | T_char | T_bool | T_int | T_uint | T_unit | T_byte | T_int16 | T_uint16
  | T_string | T_bytes | T_tuple _ | T_fixedarray _ | T_trait _ ->
      true
  | T_constr id -> (
      match Id_hash.find_opt mtype_defs id with
      | Some Externref -> false
      | None | Some _ -> true)
  | T_int64 | T_uint64 | T_func _ | T_raw_func _ -> false
  | T_float | T_double | T_any _ | T_optimized_option _ | T_maybe_uninit _ ->
      false
  | T_error_value_result _ -> false

let error_mid = Type_args.mangle_ty Stype.error

let make_suberror_mid pkg name =
  if pkg = "" then (error_mid ^ "." ^ name : Stdlib.String.t)
  else Stdlib.String.concat "" [ error_mid; "."; pkg; "."; name ]

let from_stype (stype : Stype.t) ~(stype_defs : Typing_info.stype_defs)
    ~(mtype_defs : defs) =
  (let rec go (stype : Stype.t) =
     (let stype = Stype.type_repr stype in
      match stype with
      | T_builtin T_unit -> T_unit
      | T_builtin T_bool -> T_bool
      | T_builtin T_byte -> T_byte
      | T_builtin T_char -> T_char
      | T_builtin T_int16 -> T_int16
      | T_builtin T_uint16 -> T_uint16
      | T_builtin T_int -> T_int
      | T_builtin T_int64 -> T_int64
      | T_builtin T_uint -> T_uint
      | T_builtin T_uint64 -> T_uint64
      | T_builtin T_float -> T_float
      | T_builtin T_double -> T_double
      | T_builtin T_string -> T_string
      | T_builtin T_bytes -> T_bytes
      | Tarrow { params_ty; ret_ty; err_ty; is_async = true } ->
          let ret_ty = go ret_ty in
          let cont_params =
            match err_ty with
            | None -> [ T_func { params = [ ret_ty ]; return = T_unit } ]
            | Some err_ty ->
                [
                  T_func { params = [ ret_ty ]; return = T_unit };
                  T_func { params = [ go err_ty ]; return = T_unit };
                ]
          in
          T_func
            {
              params = Lst.map_append params_ty cont_params go;
              return = T_unit;
            }
      | Tarrow { params_ty; ret_ty; err_ty; is_async = false } ->
          let return =
            match err_ty with
            | None -> go ret_ty
            | Some err_ty ->
                go (Stype.make_multi_value_result_ty ~ok_ty:ret_ty ~err_ty)
          in
          T_func { params = Lst.map params_ty go; return }
      | T_constr
          {
            type_constructor = Toplevel { pkg; id };
            tys = [];
            is_suberror_ = true;
          } ->
          let mid = make_suberror_mid pkg id in
          (if not (Id_hash.mem mtype_defs.defs mid) then
             let typedecl_info = find_stype_exn stype_defs pkg id in
             add_type_def stype mid typedecl_info);
          T_constr error_mid
      | T_constr { type_constructor = T_error; _ } ->
          (match Id_hash.find_opt mtype_defs.defs error_mid with
          | None ->
              Id_hash.add mtype_defs.defs error_mid (Variant { constrs = [] })
          | Some _ -> ());
          T_constr error_mid
      | T_constr { type_constructor = T_option; tys = t :: [] } ->
          let elem_ty = go t in
          if is_optimizable_option_elem elem_ty mtype_defs.defs then
            T_optimized_option { elem = elem_ty }
          else
            let s = Type_args.mangle_ty stype in
            if not (Id_hash.mem mtype_defs.defs s) then
              add_type_def stype s Builtin.ty_constr_option;
            T_constr s
      | T_constr { type_constructor; tys = _ }
        when Type_path.equal type_constructor
               Type_path.Builtin.type_path_foreach_result ->
          let s = Type_args.mangle_ty stype in
          if not (Id_hash.mem mtype_defs.defs s) then
            add_type_def stype s Foreach_util.foreach_result;
          T_constr s
      | T_constr { type_constructor; tys }
        when Type_path.equal type_constructor
               Type_path.Builtin.type_path_func_ref -> (
          match[@warning "-fragile-match"] tys with
          | func_ty :: [] -> (
              match go func_ty with
              | T_func { params; return } -> T_raw_func { params; return }
              | ( T_raw_func _ | T_int | T_char | T_bool | T_unit | T_byte
                | T_int16 | T_uint16 | T_int64 | T_uint | T_uint64 | T_float
                | T_double | T_string | T_bytes | T_constr _
                | T_optimized_option _ | T_tuple _ | T_fixedarray _ | T_trait _
                | T_any _ | T_maybe_uninit _ | T_error_value_result _ ) as ty ->
                  ty)
          | _ -> assert false)
      | T_constr { type_constructor; tys; is_suberror_ } -> (
          let handle_normal_type ~(pkg : string) id =
            (let typedecl_info = find_stype_exn stype_defs pkg id in
             match typedecl_info.ty_desc with
             | _ when typedecl_info.ty_is_only_tag_enum_ -> T_int
             | Record_type { fields = [] } -> T_unit
             | Extern_type | Abstract_type | Record_type _ | Variant_type _
             | Error_type _ | ErrorEnum_type _ ->
                 let s = Type_args.mangle_ty stype in
                 if not (Id_hash.mem mtype_defs.defs s) then (
                   add_type_def stype s typedecl_info;
                   T_constr s)
                 else T_constr s
             | New_type { newtype_constr; recursive; underlying_typ = _ } -> (
                 if recursive then T_any { name = Type_args.mangle_ty stype }
                 else
                   let newtype_ty, newtype_ty_field =
                     Poly_type.instantiate_constr newtype_constr
                   in
                   Ctype.unify_exn newtype_ty stype;
                   match[@warning "-fragile-match"] newtype_ty_field with
                   | t :: [] -> go t
                   | _ -> assert false)
              : t)
              [@@local]
          in
          match type_constructor with
          | T_int | T_char | T_bool | T_byte | T_unit | T_int16 | T_uint16
          | T_int64 | T_uint | T_uint64 | T_float | T_double | T_bytes
          | T_string ->
              assert false
          | T_error -> T_constr error_mid
          | T_fixedarray -> (
              match[@warning "-fragile-match"] tys with
              | elem_ty :: [] ->
                  let elem = go elem_ty in
                  if elem = T_byte then T_bytes else T_fixedarray { elem }
              | _ -> assert false)
          | T_option | T_ref | T_result | T_error_value_result ->
              let type_name =
                Type_path.export_name
                  ~cur_pkg_name:!Basic_config.current_package
                  type_constructor
              in
              let s = Type_args.mangle_ty stype in
              (if not (Id_hash.mem mtype_defs.defs s) then
                 match[@warning "-fragile-match"]
                   Typing_info.find_type Builtin.builtin_types type_name
                 with
                 | Some typedecl_info -> add_type_def stype s typedecl_info
                 | _ -> assert false);
              if Type_path.is_multi_value_result type_constructor then
                match[@warning "-fragile-match"] tys with
                | [ ok_ty; err_ty ] ->
                    T_error_value_result
                      { ok = go ok_ty; err = go err_ty; id = s }
                | _ -> assert false
              else T_constr s
          | Tuple _ -> T_tuple { tys = Lst.map tys go }
          | _
            when Type_path.equal type_constructor
                   Type_path.Builtin.type_path_sourceloc ->
              T_string
          | Toplevel { pkg; id = "UnsafeMaybeUninit" }
            when pkg = Basic_config.builtin_package -> (
              match[@warning "-fragile-match"] tys with
              | t :: [] ->
                  let t = go t in
                  if is_numeric t then t else T_maybe_uninit t
              | _ -> assert false)
          | T_local { toplevel_id; name } ->
              handle_normal_type
                ~pkg:(Type_path.pkg_of_toplevel_id toplevel_id)
                (Local_type.mangle_name toplevel_id name)
          | Toplevel { pkg; id } -> handle_normal_type ~pkg id
          | Constr
              {
                tag =
                  Extensible_tag { pkg; type_name; name; total = _; index = _ };
                _;
              } ->
              let tag_str =
                Basic_constr_info.ext_tag_to_str ~pkg ~type_name ~name
              in
              let mid =
                ((error_mid ^ "." ^ tag_str : Stdlib.String.t) [@merlin.hide])
              in
              if not (Id_hash.mem mtype_defs.defs mid) then
                Id_hash.add mtype_defs.defs mid Variant_constr;
              let error_type_mid = make_suberror_mid pkg type_name in
              (if not (Id_hash.mem mtype_defs.defs error_type_mid) then
                 let typedecl_info = find_stype_exn stype_defs pkg type_name in
                 add_type_def stype error_type_mid typedecl_info);
              T_constr mid
          | Constr { ty; tag = Constr_tag_regular { name_; repr_; _ } } -> (
              let enum_ty : Stype.t =
                T_constr
                  { type_constructor = ty; tys; generic_ = false; is_suberror_ }
              in
              match go enum_ty with
              | T_constr id ->
                  let id = id ^ "." ^ name_ in
                  Id_hash.replace mtype_defs.defs id
                    (match repr_ with
                    | Constant -> Constant_variant_constr
                    | Non_constant -> Variant_constr
                    | Integer _ -> assert false);
                  T_constr id
              | T_error_value_result { id; ok; err } ->
                  let id = id ^ "." ^ name_ in
                  Id_hash.replace mtype_defs.defs id Variant_constr;
                  T_error_value_result { id; ok; err }
              | ( T_int | T_char | T_bool | T_unit | T_byte | T_int16 | T_uint16
                | T_int64 | T_uint | T_uint64 | T_float | T_double | T_string
                | T_bytes | T_optimized_option _ | T_func _ | T_raw_func _
                | T_tuple _ | T_fixedarray _ | T_trait _ | T_any _
                | T_maybe_uninit _ ) as ty ->
                  ty))
      | Tparam _ -> assert false
      | Tvar _ -> assert false
      | T_blackhole -> assert false
      | T_trait type_path ->
          let s = Type_args.mangle_ty stype in
          (if not (Id_hash.mem mtype_defs.defs s) then
             match[@warning "-fragile-match"] type_path with
             | Toplevel { pkg; id } ->
                 let trait_info = find_trait_exn stype_defs pkg id in
                 add_trait_def s trait_info
             | _ -> assert false);
          T_trait s
       : t)
   and add_type_def (stype : Stype.t) (mid : id) (decl : Typedecl_info.t) =
     Id_hash.add mtype_defs.defs mid Placeholder;
     match decl.ty_desc with
     | Extern_type -> Id_hash.replace mtype_defs.defs mid Externref
     | Abstract_type | New_type _ -> ()
     | Error_type c -> (
         Id_hash.replace mtype_defs.defs mid Variant_constr;
         let constr =
           let tag = Tag.of_core_tag mtype_defs.ext_tags c.cs_tag in
           let payload =
             Lst.mapi c.cs_args (fun i ->
                 fun ty ->
                  let field_type = go ty in
                  { field_type; name = field_indexed i; mut = false })
           in
           { payload; tag }
         in
         match Id_hash.find_opt mtype_defs.defs error_mid with
         | None ->
             Id_hash.add mtype_defs.defs error_mid
               (Variant { constrs = [ constr ] })
         | Some (Variant { constrs = cs }) ->
             Id_hash.replace mtype_defs.defs error_mid
               (Variant { constrs = constr :: cs })
         | _ -> assert false)
     | ErrorEnum_type cs -> (
         Id_hash.replace mtype_defs.defs mid Variant_constr;
         let constrs =
           Lst.map cs (fun c ->
               let tag = Tag.of_core_tag mtype_defs.ext_tags c.cs_tag in
               let arity = c.cs_arity_ in
               let payload =
                 Fn_arity.to_list_map2 arity c.cs_args (fun param_kind ->
                     fun ty ->
                      let field_type = go ty in
                      match param_kind with
                      | Labelled { label; is_mut = mut; _ } ->
                          { field_type; name = Named label; mut }
                      | Positional index ->
                          {
                            field_type;
                            name = field_indexed index;
                            mut = false;
                          }
                      | Optional _ | Autofill _ | Question_optional _ ->
                          assert false)
               in
               { payload; tag })
         in
         match Id_hash.find_opt mtype_defs.defs error_mid with
         | None -> Id_hash.add mtype_defs.defs error_mid (Variant { constrs })
         | Some (Variant { constrs = cs }) ->
             Id_hash.replace mtype_defs.defs error_mid
               (Variant { constrs = constrs @ cs })
         | _ -> assert false)
     | Record_type { fields = fs } ->
         let _, fs' =
           Poly_type.instantiate_record ~ty_record:(`Known stype) fs
         in
         let fields =
           Lst.map fs' (fun f ->
               {
                 field_type = go f.ty_field;
                 name = Named f.field_name;
                 mut = f.mut;
               })
         in
         Id_hash.replace mtype_defs.defs mid (Record { fields })
     | Variant_type cs ->
         let constrs =
           Lst.map cs (fun c ->
               let tag = Tag.of_core_tag_no_ext c.cs_tag in
               let arity = c.cs_arity_ in
               let enum_ty, constr_tys = Poly_type.instantiate_constr c in
               Ctype.unify_exn stype enum_ty;
               let payload =
                 Fn_arity.to_list_map2 arity constr_tys (fun param_kind ->
                     fun ty ->
                      let field_type = go ty in
                      match param_kind with
                      | Labelled { label; is_mut = mut; _ } ->
                          { field_type; name = Named label; mut }
                      | Positional index ->
                          {
                            field_type;
                            name = field_indexed index;
                            mut = false;
                          }
                      | Optional _ | Autofill _ | Question_optional _ ->
                          assert false)
               in
               { payload; tag })
         in
         Id_hash.replace mtype_defs.defs mid (Variant { constrs })
   and add_trait_def (mid : id) (decl : Trait_decl.t) =
     Id_hash.add mtype_defs.defs mid Placeholder;
     let methods =
       Lst.mapi decl.closure_methods (fun i ->
           fun (_, m) ->
            match m.method_typ with
            | Stype.Tarrow
                { params_ty = _ :: params_ty; ret_ty; err_ty; is_async = false }
              ->
                let params_ty = Lst.map params_ty go in
                let return_ty =
                  match err_ty with
                  | None -> go ret_ty
                  | Some err_ty ->
                      go
                        (Stype.make_multi_value_result_ty ~ok_ty:ret_ty ~err_ty)
                in
                { params_ty; return_ty; index = i; name = m.method_name }
            | _ -> assert false)
     in
     Id_hash.replace mtype_defs.defs mid (Trait { methods })
   in
   go stype
    : t)

let is_func (t : t) =
  (match t with
   | T_func _ -> true
   | T_raw_func _ | T_int | T_char | T_bool | T_unit | T_byte | T_int16
   | T_uint16 | T_int64 | T_uint | T_uint64 | T_float | T_double | T_string
   | T_bytes | T_constr _ | T_optimized_option _ | T_tuple _ | T_fixedarray _
   | T_trait _ | T_any _ | T_maybe_uninit _ | T_error_value_result _ ->
       false
    : bool)

let get_fixedarray_elem_exn (t : t) =
  (match t with
   | T_fixedarray { elem } -> elem
   | T_maybe_uninit _ -> assert false
   | T_raw_func _ -> assert false
   | T_trait _ -> assert false
   | T_optimized_option _ -> assert false
   | T_bytes -> assert false
   | T_double -> assert false
   | T_uint64 -> assert false
   | T_int64 -> assert false
   | T_int16 -> assert false
   | T_unit -> assert false
   | T_char -> assert false
   | T_int -> assert false
   | T_bool -> assert false
   | T_byte -> assert false
   | T_uint16 -> assert false
   | T_uint -> assert false
   | T_float -> assert false
   | T_string -> assert false
   | T_constr _ -> assert false
   | T_tuple _ -> assert false
   | T_func _ -> assert false
   | T_any _ -> assert false
   | T_error_value_result _ -> assert false
    : t)

let is_uninit (t : t) =
  (match t with
   | T_maybe_uninit _ -> true
   | T_func _ | T_raw_func _ | T_int | T_char | T_bool | T_unit | T_byte
   | T_int16 | T_uint16 | T_int64 | T_uint | T_uint64 | T_float | T_double
   | T_string | T_bytes | T_constr _ | T_optimized_option _ | T_tuple _
   | T_fixedarray _ | T_trait _ | T_any _ | T_error_value_result _ ->
       false
    : bool)

type optimized_option_kind =
  | Not_optimized
  | Optimized_to_int
  | Optimized_to_int64
  | Optimized_ref

let get_optimized_option_kind (t : t) =
  (match t with
   | T_optimized_option { elem } -> (
       match elem with
       | T_char | T_byte | T_bool | T_unit | T_int16 | T_uint16 ->
           Optimized_to_int
       | T_int | T_uint -> Optimized_to_int64
       | T_int64 | T_uint64 | T_func _ | T_raw_func _ -> assert false
       | T_float | T_double | T_any _ | T_optimized_option _ | T_maybe_uninit _
       | T_error_value_result _ ->
           assert false
       | T_string | T_bytes | T_tuple _ | T_fixedarray _ | T_trait _
       | T_constr _ ->
           Optimized_ref)
   | T_char | T_byte | T_int16 | T_uint16 | T_bool | T_unit | T_int | T_uint
   | T_int64 | T_uint64 | T_float | T_double | T_string | T_bytes | T_func _
   | T_raw_func _ | T_tuple _ | T_fixedarray _ | T_constr _ | T_trait _
   | T_any _ | T_maybe_uninit _ | T_error_value_result _ ->
       Not_optimized
    : optimized_option_kind)
