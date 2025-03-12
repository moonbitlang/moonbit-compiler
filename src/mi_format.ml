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


module Iter_utils = Basic_iter_utils
module Hash_string = Basic_hash_string
module Arr = Basic_arr
module Lst = Basic_lst

type method_array = Method_env.method_array

include struct
  let _ = fun (_ : method_array) -> ()

  let sexp_of_method_array =
    (Method_env.sexp_of_method_array : method_array -> S.t)

  let _ = sexp_of_method_array
end

type ext_method_array = (Ext_method_env.key * Method_env.method_info) array

include struct
  let _ = fun (_ : ext_method_array) -> ()

  let sexp_of_ext_method_array =
    (fun x__005_ ->
       Moon_sexp_conv.sexp_of_array
         (fun (arg0__001_, arg1__002_) ->
           let res0__003_ = Ext_method_env.sexp_of_key arg0__001_
           and res1__004_ = Method_env.sexp_of_method_info arg1__002_ in
           S.List [ res0__003_; res1__004_ ])
         x__005_
      : ext_method_array -> S.t)

  let _ = sexp_of_ext_method_array
end

module Serialize = struct
  type serialized = {
    export_values : (string * Value_info.toplevel) array;
    export_types : (string * Typedecl_info.t) array;
    export_traits : (string * Trait_decl.t) array;
    export_type_alias : (string * Type_alias.t) array;
    export_method_env : method_array;
    export_ext_method_env : ext_method_array;
    export_trait_impls : Trait_impl.impl array;
    name : string;
  }

  include struct
    let _ = fun (_ : serialized) -> ()

    let sexp_of_serialized =
      (fun {
             export_values = export_values__007_;
             export_types = export_types__013_;
             export_traits = export_traits__019_;
             export_type_alias = export_type_alias__025_;
             export_method_env = export_method_env__031_;
             export_ext_method_env = export_ext_method_env__033_;
             export_trait_impls = export_trait_impls__035_;
             name = name__037_;
           }
       ->
         let bnds__006_ = ([] : _ Stdlib.List.t) in
         let bnds__006_ =
           let arg__038_ = Moon_sexp_conv.sexp_of_string name__037_ in
           (S.List [ S.Atom "name"; arg__038_ ] :: bnds__006_ : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__036_ =
             Moon_sexp_conv.sexp_of_array Trait_impl.sexp_of_impl
               export_trait_impls__035_
           in
           (S.List [ S.Atom "export_trait_impls"; arg__036_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__034_ =
             sexp_of_ext_method_array export_ext_method_env__033_
           in
           (S.List [ S.Atom "export_ext_method_env"; arg__034_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__032_ = sexp_of_method_array export_method_env__031_ in
           (S.List [ S.Atom "export_method_env"; arg__032_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__026_ =
             Moon_sexp_conv.sexp_of_array
               (fun (arg0__027_, arg1__028_) ->
                 let res0__029_ = Moon_sexp_conv.sexp_of_string arg0__027_
                 and res1__030_ = Type_alias.sexp_of_t arg1__028_ in
                 S.List [ res0__029_; res1__030_ ])
               export_type_alias__025_
           in
           (S.List [ S.Atom "export_type_alias"; arg__026_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__020_ =
             Moon_sexp_conv.sexp_of_array
               (fun (arg0__021_, arg1__022_) ->
                 let res0__023_ = Moon_sexp_conv.sexp_of_string arg0__021_
                 and res1__024_ = Trait_decl.sexp_of_t arg1__022_ in
                 S.List [ res0__023_; res1__024_ ])
               export_traits__019_
           in
           (S.List [ S.Atom "export_traits"; arg__020_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__014_ =
             Moon_sexp_conv.sexp_of_array
               (fun (arg0__015_, arg1__016_) ->
                 let res0__017_ = Moon_sexp_conv.sexp_of_string arg0__015_
                 and res1__018_ = Typedecl_info.sexp_of_t arg1__016_ in
                 S.List [ res0__017_; res1__018_ ])
               export_types__013_
           in
           (S.List [ S.Atom "export_types"; arg__014_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__008_ =
             Moon_sexp_conv.sexp_of_array
               (fun (arg0__009_, arg1__010_) ->
                 let res0__011_ = Moon_sexp_conv.sexp_of_string arg0__009_
                 and res1__012_ = Value_info.sexp_of_toplevel arg1__010_ in
                 S.List [ res0__011_; res1__012_ ])
               export_values__007_
           in
           (S.List [ S.Atom "export_values"; arg__008_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         S.List bnds__006_
        : serialized -> S.t)

    let _ = sexp_of_serialized
  end

  let magic_str = Basic_config.mi_magic_str
  let digest_len = 16

  let output_mi (type t) (action : t Action.t) (mi_content : serialized) =
    (let content = Marshal.to_bytes mi_content [] in
     let digest = Digest.bytes content in
     match action with
     | Write_file path ->
         let same_file_exist =
           if Sys.file_exists path then
             In_channel.with_open_bin path (fun ic ->
                 let magic_len = String.length magic_str in
                 let header = Bytes.make (magic_len + digest_len) '\000' in
                 if In_channel.input ic header 0 (magic_len + digest_len) = 0
                 then false
                 else
                   String.equal digest
                     (Bytes.sub_string header magic_len digest_len))
           else false
         in
         if not same_file_exist then
           Out_channel.with_open_bin path (fun oc ->
               output_string oc magic_str;
               output_string oc digest;
               output_bytes oc content)
     | Return_bytes ->
         let magic_bytes = Bytes.of_string magic_str in
         let digest_bytes = Bytes.of_string digest in
         Bytes.cat (Bytes.cat magic_bytes digest_bytes) content
      : t)

  let dummy_mi ~(pkg_name : string) =
    let mi_content : serialized =
      {
        export_values = [||];
        export_types = [||];
        export_type_alias = [||];
        export_traits = [||];
        export_method_env = [||];
        export_ext_method_env = [||];
        export_trait_impls = [||];
        name = pkg_name;
      }
    in
    let magic_bytes = Bytes.of_string magic_str in
    let mi_bytes = Marshal.to_bytes mi_content [] in
    Bytes.cat magic_bytes mi_bytes

  let input_raw ~path bin =
    try
      let magic_len = String.length Basic_config.mi_magic_str in
      let magic = String.sub bin 0 magic_len in
      if String.equal magic Basic_config.mi_magic_str then
        Ok (Marshal.from_string bin (magic_len + digest_len) : serialized)
      else if String.length magic <> magic_len then
        Error (Errors.pkg_wrong_format ~pkg:path)
      else Error (Errors.pkg_magic_mismatch ~pkg:path)
    with _ -> Error (Errors.pkg_not_found ~pkg:path)
end

let export_mi (type t) ~(action : t Action.t) ~(pkg_name : string)
    ~(types : Typing_info.types) ~(type_alias : Type_alias.t Hash_string.t)
    ~(values : Typing_info.values) ~method_env ~ext_method_env ~trait_impls =
  (let pub_values = Typing_info.get_pub_values values in
   let pub_types = Typing_info.get_pub_types types in
   let pub_type_alias =
     Hash_string.to_array_filter_map type_alias (fun (name, alias) ->
         if alias.is_pub then Some (name, alias) else None)
   in
   let pub_traits = Typing_info.get_pub_traits types in
   let mi_content : Serialize.serialized =
     {
       export_values = pub_values;
       export_types = pub_types;
       export_type_alias = pub_type_alias;
       export_traits = pub_traits;
       export_method_env = Method_env.export ~export_private:false method_env;
       export_ext_method_env =
         Iter_utils.to_array (Ext_method_env.iter_pub ext_method_env);
       export_trait_impls = Trait_impl.get_pub_impls trait_impls;
       name = pkg_name;
     }
   in
   Serialize.output_mi action mi_content
    : t)

let serialized_of_pkg_info (mi_view : Pkg_info.mi_view) =
  ({
     export_values = Hash_string.to_array mi_view.values;
     export_types = Hash_string.to_array mi_view.external_types;
     export_type_alias = Hash_string.to_array mi_view.external_type_alias;
     export_traits = Hash_string.to_array mi_view.external_traits;
     export_method_env =
       Method_env.export ~export_private:false mi_view.method_env;
     export_ext_method_env =
       Iter_utils.to_array (Ext_method_env.iter_pub mi_view.ext_method_env);
     export_trait_impls = Trait_impl.get_pub_impls mi_view.trait_impls;
     name = mi_view.name;
   }
    : Serialize.serialized)

let pkg_info_of_serialized (pkg_info : Serialize.serialized) =
  (let values = Hash_string.create 17 in
   Arr.iter pkg_info.export_values (fun (name, value) ->
       Hash_string.add values name value);
   let external_types = Hash_string.create 17 in
   let external_constrs = Hash_string.create 17 in
   Arr.iter pkg_info.export_types (fun (name, ty_decl) ->
       Hash_string.add external_types name ty_decl;
       match ty_decl.ty_desc with
       | Variant_type constrs | ErrorEnum_type constrs ->
           Lst.iter constrs ~f:(fun ({ constr_name; _ } as constr_info) ->
               ignore
                 (Hash_string.add_or_update external_constrs constr_name
                    [ constr_info ] ~update:(fun cs -> constr_info :: cs)))
       | New_type { newtype_constr = { constr_name; _ } as constr_info; _ }
       | Error_type ({ constr_name; _ } as constr_info) ->
           ignore
             (Hash_string.add_or_update external_constrs constr_name
                [ constr_info ] ~update:(fun cs -> constr_info :: cs))
       | Abstract_type | Extern_type | Record_type _ -> ());
   let external_traits = Hash_string.create 17 in
   Arr.iter pkg_info.export_traits (fun (name, decl) ->
       Hash_string.add external_traits name decl);
   let external_type_alias = Hash_string.create 17 in
   Arr.iter pkg_info.export_type_alias (fun (name, alias) ->
       Hash_string.add external_type_alias name alias);
   let trait_impls = Trait_impl.make () in
   Arr.iter pkg_info.export_trait_impls (fun impl ->
       let type_name = Stype.extract_tpath_exn (Stype.type_repr impl.self_ty) in
       Trait_impl.add_impl trait_impls ~trait:impl.trait ~type_name impl);
   {
     values;
     external_types;
     external_type_alias;
     external_traits;
     external_constrs;
     method_env = Method_env.import pkg_info.export_method_env;
     ext_method_env = Ext_method_env.of_array pkg_info.export_ext_method_env;
     trait_impls;
     name = pkg_info.name;
   }
    : Pkg_info.mi_view)

let input_mi ~path bin =
  (Result.map pkg_info_of_serialized (Serialize.input_raw ~path bin)
    : Pkg_info.mi_view Info.t)

let dummy_mi = Serialize.dummy_mi

let dump_serialized_from_pkg_info (pkg_info : Pkg_info.mi_view) =
  Basic_ref.protect Basic_config.current_package pkg_info.name (fun () ->
      Serialize.sexp_of_serialized (serialized_of_pkg_info pkg_info))
