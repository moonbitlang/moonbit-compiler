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


module Ident = Basic_core_ident
module Lst = Basic_lst
module Hash_string = Basic_hash_string

type t = No_export | Export_all_pubs | Export_selected of string Hash_string.t

let parse_exported_functions (str : string)
    (exported_functions : string Hash_string.t) =
  Lst.iter (String.split_on_char ',' str) ~f:(fun s ->
      let parts = Basic_strutil.split_on_first ':' s in
      match parts with
      | name, "" -> Hash_string.add exported_functions name name
      | name, export_name -> Hash_string.add exported_functions name export_name)

let is_exported_ident (exported : t) (id : Ident.t) =
  (match exported with
   | No_export -> false
   | Export_all_pubs -> not (Ident.is_foreign id)
   | Export_selected id_map -> (
       match id with
       | Pdot qual_name ->
           Hash_string.mem id_map (Basic_qual_ident.base_name qual_name)
           && not (Ident.is_foreign id)
       | Plocal_method _ | Pident _ | Pmutable_ident _ -> false)
    : bool)

let get_exported_name (exported : t) (id : Ident.t) =
  (match exported with
   | No_export -> None
   | Export_all_pubs ->
       if not (Ident.is_foreign id) then Some (Ident.as_export_name id)
       else None
   | Export_selected id_map -> (
       match id with
       | Pdot qual_name ->
           if not (Ident.is_foreign id) then
             Hash_string.find_opt id_map (Basic_qual_ident.base_name qual_name)
           else None
       | Plocal_method _ | Pident _ | Pmutable_ident _ -> None)
    : string option)
