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


module Strutil = Basic_strutil
module Syntax = Parsing_syntax

let is_intrinsic (decl : Syntax.func_stubs) =
  match decl with
  | Embedded { language = None; code = Code_string s } ->
      Strutil.first_char_is s.string_val '%'
  | Import _ | Embedded _ -> false
