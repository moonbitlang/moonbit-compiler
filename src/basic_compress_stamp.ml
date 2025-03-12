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


type name_kind = Local of string | Global of string | None

let basename_without_stamp s =
  let ubound = String.length s - 1 in
  let rec aux i s =
    if i < 0 then None
    else
      match s.[i] with
      | '0' .. '9' -> aux (i - 1) s
      | '/' when i <> ubound -> Local (String.sub s 0 i)
      | '|' when i <> ubound -> Global (String.sub s 0 i)
      | _ -> None
  in
  aux ubound s

let normalize ?global_stamps (impl_sexp : S.t) =
  (let tbl = Hashtbl.create 20 in
   let rec mapper = function
     | S.List xs -> S.List (Basic_lst.map xs mapper)
     | S.Atom s ->
         let s =
           match basename_without_stamp s with
           | None -> s
           | Local base_name -> (
               match Hashtbl.find_opt tbl s with
               | Some n -> n
               | None ->
                   let new_name =
                     base_name ^ "/" ^ Int.to_string (Hashtbl.length tbl)
                   in
                   Hashtbl.add tbl s new_name;
                   new_name)
           | Global base_name -> (
               match global_stamps with
               | None ->
                   failwith
                     (("ident '" ^ s
                       ^ "' occured but global_stamps is not provided"
                       : Stdlib.String.t)
                       [@merlin.hide])
               | Some global_stamps -> (
                   match Hashtbl.find_opt global_stamps s with
                   | Some n -> n
                   | None ->
                       let new_name =
                         base_name ^ "|"
                         ^ Int.to_string (Hashtbl.length global_stamps)
                       in
                       Hashtbl.add global_stamps s new_name;
                       new_name))
         in
         S.Atom s
   in
   mapper impl_sexp
    : S.t)
