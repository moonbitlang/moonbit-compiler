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
module Vec = Basic_vec

type output = {
  linked_program : Core.program;
  methods : Method_env.t Hash_string.t;
  ext_methods : Ext_method_env.t Hash_string.t;
  types : Typing_info.types Hash_string.t;
}

type linking_target = File_path of string | Core_format of Core_format.t array

let link ~(targets : linking_target Vec.t) =
  (let types = Hash_string.create 17 in
   let methods = Hash_string.create 17 in
   let ext_methods = Hash_string.create 17 in
   let append_core (serialized : Core_format.t) acc =
     Hash_string.add types serialized.pkg_name
       (Typing_info.init_types serialized.types serialized.traits);
     Hash_string.add methods serialized.pkg_name serialized.methods;
     Hash_string.add ext_methods serialized.pkg_name serialized.ext_methods;
     List.rev_append serialized.program acc
   in
   let items_rev =
     Vec.fold_left
       ~f:(fun items_acc ->
         fun target ->
          let pkgs =
            match target with
            | File_path path -> Core_format.import ~path
            | Core_format pkgs -> pkgs
          in
          Array.fold_left
            (fun items_acc -> fun pkg -> append_core pkg items_acc)
            items_acc pkgs)
       [] targets
   in
   { linked_program = List.rev items_rev; types; methods; ext_methods }
    : output)
