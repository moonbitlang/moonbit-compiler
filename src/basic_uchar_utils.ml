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


type uchar = Uchar.t

let uchar_to_string (c : Uchar.t) =
  (let buf = Buffer.create 4 in
   Buffer.add_utf_8_uchar buf c;
   Buffer.contents buf
    : string)

let to_string (t : Uchar.t) = Printf.sprintf "U+%04X" (Uchar.to_int t)

let sexp_of_uchar c =
  if Uchar.to_int c <= 255 then Moon_sexp_conv.sexp_of_char (Uchar.to_char c)
  else S.Atom (to_string c)
