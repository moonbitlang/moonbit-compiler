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


module Lst = Basic_lst

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

let escaped_quote s = "\"" ^ Basic_strutil.escaped_string s ^ "\""

let rec to_string (t : t) =
  match t with
  | `Null -> "null"
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> escaped_quote s
  | `Assoc l ->
      "{"
      ^ String.concat ","
          (Lst.map l (fun (k, v) -> escaped_quote k ^ ":" ^ to_string v))
      ^ "}"
  | `List l -> "[" ^ String.concat "," (Lst.map l to_string) ^ "]"
