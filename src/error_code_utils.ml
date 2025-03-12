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


let to_json t = (`Int t : Json.t)

let to_string code =
  let padding len x =
    let m = string_of_int x in
    String.make (Int.max 0 (len - String.length m)) '0' ^ m
  in
  let s = padding 4 code in
  ("E" ^ s : Stdlib.String.t)

let is_non_fatal_parse_error error_code =
  error_code >= 3800 && error_code < 4000

let warning warning_id = 1000 + warning_id
