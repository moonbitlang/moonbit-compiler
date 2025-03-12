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


module H = Basic_hash_string

type t = string H.t

let create () = H.create 4
let add_pkg self name src_dir = H.add self name src_dir
let find_pkg = H.find_opt

let resolve_source self ~pkg ~file:path =
  let rec find rev_parts path =
    match rev_parts with
    | [] -> path
    | last :: rest -> (
        let pkg = String.concat "/" (List.rev rev_parts) in
        match H.find_opt self pkg with
        | Some src_dir -> Filename.concat src_dir path
        | None -> find rest (Filename.concat last path))
  in
  let rev_parts = List.rev (String.split_on_char '/' pkg) in
  find rev_parts path
