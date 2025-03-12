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

type t = {
  regular_methods : Method_env.t Hash_string.t;
  extension_methods : Ext_method_env.t Hash_string.t;
}

let make ~regular_methods ~extension_methods =
  ({ regular_methods; extension_methods } : t)

let find_method_opt (env : t) ~(type_name : Method_env.type_name)
    ~(method_name : Method_env.method_name) ~(trait : Type_path.t) =
  let exception Found of Method_env.method_info in
  let find_ext_method pkg ~type_name ~method_name =
    match Hash_string.find_opt env.extension_methods pkg with
    | None -> ()
    | Some methods -> (
        match
          Ext_method_env.find_method methods ~trait ~self_type:type_name
            ~method_name
        with
        | Some mi -> raise_notrace (Found mi)
        | None -> ())
      [@@inline]
  in
  let find_regular_method pkg =
    match Hash_string.find_opt env.regular_methods pkg with
    | None -> ()
    | Some methods -> (
        match
          Method_env.find_regular_method methods ~type_name ~method_name
        with
        | Some mi -> raise_notrace (Found mi)
        | None -> ())
      [@@inline]
  in
  try
    let pkg_of_trait = Type_path.get_pkg trait in
    find_ext_method pkg_of_trait ~type_name ~method_name;
    let pkg_of_type = Type_path.get_pkg type_name in
    if pkg_of_type <> pkg_of_trait then
      find_ext_method pkg_of_type ~type_name ~method_name;
    find_ext_method pkg_of_trait
      ~type_name:Type_path.Builtin.default_impl_placeholder ~method_name;
    find_regular_method pkg_of_type;
    if pkg_of_type <> Basic_config.builtin_package then
      find_regular_method Basic_config.builtin_package;
    None
  with Found mi -> Some mi
