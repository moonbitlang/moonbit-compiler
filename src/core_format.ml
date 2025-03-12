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

type serialized = {
  program : Core.program;
  types : (string * Typedecl_info.t) array;
  traits : (string * Trait_decl.t) array;
  methods : method_array;
  ext_methods : ext_method_array;
  pkg_name : string;
}

include struct
  let _ = fun (_ : serialized) -> ()

  let sexp_of_serialized =
    (fun {
           program = program__007_;
           types = types__009_;
           traits = traits__015_;
           methods = methods__021_;
           ext_methods = ext_methods__023_;
           pkg_name = pkg_name__025_;
         }
     ->
       let bnds__006_ = ([] : _ Stdlib.List.t) in
       let bnds__006_ =
         let arg__026_ = Moon_sexp_conv.sexp_of_string pkg_name__025_ in
         (S.List [ S.Atom "pkg_name"; arg__026_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__024_ = sexp_of_ext_method_array ext_methods__023_ in
         (S.List [ S.Atom "ext_methods"; arg__024_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__022_ = sexp_of_method_array methods__021_ in
         (S.List [ S.Atom "methods"; arg__022_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__016_ =
           Moon_sexp_conv.sexp_of_array
             (fun (arg0__017_, arg1__018_) ->
               let res0__019_ = Moon_sexp_conv.sexp_of_string arg0__017_
               and res1__020_ = Trait_decl.sexp_of_t arg1__018_ in
               S.List [ res0__019_; res1__020_ ])
             traits__015_
         in
         (S.List [ S.Atom "traits"; arg__016_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__010_ =
           Moon_sexp_conv.sexp_of_array
             (fun (arg0__011_, arg1__012_) ->
               let res0__013_ = Moon_sexp_conv.sexp_of_string arg0__011_
               and res1__014_ = Typedecl_info.sexp_of_t arg1__012_ in
               S.List [ res0__013_; res1__014_ ])
             types__009_
         in
         (S.List [ S.Atom "types"; arg__010_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__008_ = Core.sexp_of_program program__007_ in
         (S.List [ S.Atom "program"; arg__008_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       S.List bnds__006_
      : serialized -> S.t)

  let _ = sexp_of_serialized
end

type t = {
  program : Core.program;
  types : (string * Typedecl_info.t) array;
  traits : (string * Trait_decl.t) array;
  methods : Method_env.t;
  ext_methods : Ext_method_env.t;
  pkg_name : string;
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun {
           program = program__028_;
           types = types__030_;
           traits = traits__036_;
           methods = methods__042_;
           ext_methods = ext_methods__044_;
           pkg_name = pkg_name__046_;
         }
     ->
       let bnds__027_ = ([] : _ Stdlib.List.t) in
       let bnds__027_ =
         let arg__047_ = Moon_sexp_conv.sexp_of_string pkg_name__046_ in
         (S.List [ S.Atom "pkg_name"; arg__047_ ] :: bnds__027_
           : _ Stdlib.List.t)
       in
       let bnds__027_ =
         let arg__045_ = Ext_method_env.sexp_of_t ext_methods__044_ in
         (S.List [ S.Atom "ext_methods"; arg__045_ ] :: bnds__027_
           : _ Stdlib.List.t)
       in
       let bnds__027_ =
         let arg__043_ = Method_env.sexp_of_t methods__042_ in
         (S.List [ S.Atom "methods"; arg__043_ ] :: bnds__027_
           : _ Stdlib.List.t)
       in
       let bnds__027_ =
         let arg__037_ =
           Moon_sexp_conv.sexp_of_array
             (fun (arg0__038_, arg1__039_) ->
               let res0__040_ = Moon_sexp_conv.sexp_of_string arg0__038_
               and res1__041_ = Trait_decl.sexp_of_t arg1__039_ in
               S.List [ res0__040_; res1__041_ ])
             traits__036_
         in
         (S.List [ S.Atom "traits"; arg__037_ ] :: bnds__027_ : _ Stdlib.List.t)
       in
       let bnds__027_ =
         let arg__031_ =
           Moon_sexp_conv.sexp_of_array
             (fun (arg0__032_, arg1__033_) ->
               let res0__034_ = Moon_sexp_conv.sexp_of_string arg0__032_
               and res1__035_ = Typedecl_info.sexp_of_t arg1__033_ in
               S.List [ res0__034_; res1__035_ ])
             types__030_
         in
         (S.List [ S.Atom "types"; arg__031_ ] :: bnds__027_ : _ Stdlib.List.t)
       in
       let bnds__027_ =
         let arg__029_ = Core.sexp_of_program program__028_ in
         (S.List [ S.Atom "program"; arg__029_ ] :: bnds__027_
           : _ Stdlib.List.t)
       in
       S.List bnds__027_
      : t -> S.t)

  let _ = sexp_of_t
end

let magic_str = Basic_config.core_magic_str

let export (type t) ~(action : t Action.t) ~(pkg_name : string)
    ~(program : Core.program) ~(genv : Global_env.t) =
  (let methods =
     Method_env.export ~export_private:true (Global_env.get_method_env genv)
   in
   let ext_methods =
     Iter_utils.to_array
       (Ext_method_env.iter (Global_env.get_ext_method_env genv))
   in
   let types = Typing_info.get_all_types (Global_env.get_toplevel_types genv) in
   let local_types = Global_env.all_local_types genv in
   let traits =
     Typing_info.get_all_traits (Global_env.get_toplevel_types genv)
   in
   let serialized : serialized =
     {
       program;
       types = Array.append local_types types;
       traits;
       pkg_name;
       methods;
       ext_methods;
     }
   in
   match action with
   | Write_file path ->
       Out_channel.with_open_bin path (fun oc ->
           output_string oc magic_str;
           Marshal.to_channel oc [| serialized |] [])
   | Return_bytes ->
       let magic_bytes = Bytes.of_string magic_str in
       let serialized_bytes = Marshal.to_bytes [| serialized |] [] in
       Bytes.cat magic_bytes serialized_bytes
    : t)

let of_string (bin : string) =
  (if String.starts_with ~prefix:magic_str bin then
     let magic_str_len = String.length Basic_config.core_magic_str in
     Arr.map
       (Marshal.from_string bin magic_str_len : serialized array)
       (fun serialized ->
         let methods = Method_env.import serialized.methods in
         {
           program = serialized.program;
           types = serialized.types;
           traits = serialized.traits;
           methods;
           ext_methods = Ext_method_env.of_array serialized.ext_methods;
           pkg_name = serialized.pkg_name;
         })
   else assert false
    : t array)

let import ~(path : string) =
  (if Sys.file_exists path then
     In_channel.with_open_bin path (fun ic ->
         of_string (Stdlib.In_channel.input_all ic))
   else failwith (path ^ " not found")
    : t array)

let dump_serialized_from_t (t : t array) =
  (let t_to_serialized (t : t) =
     ({
        program = t.program;
        types = t.types;
        traits = t.traits;
        methods = Method_env.export ~export_private:true t.methods;
        ext_methods = Iter_utils.to_array (Ext_method_env.iter t.ext_methods);
        pkg_name = t.pkg_name;
      }
       : serialized)
   in
   Moon_sexp_conv.sexp_of_array sexp_of_serialized (Arr.map t t_to_serialized)
    : S.t)

let bundle ~inputs ~path =
  let pkgs =
    Array.concat
      (Lst.map inputs (fun path ->
           In_channel.with_open_bin path (fun ic ->
               match Basic_config.input_magic_str ic with
               | Some s when String.equal s magic_str ->
                   (Marshal.from_channel ic : serialized array)
               | _ -> failwith "invalid MoonBit object file")))
  in
  Out_channel.with_open_bin path (fun oc ->
      output_string oc magic_str;
      Marshal.to_channel oc pkgs [])
