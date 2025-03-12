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


type t =
  | Int
  | Uint
  | Char
  | Bool
  | Unit
  | Byte
  | Int16
  | UInt16
  | Int64
  | UInt64
  | Float
  | Double

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (function
     | Int -> S.Atom "Int"
     | Uint -> S.Atom "Uint"
     | Char -> S.Atom "Char"
     | Bool -> S.Atom "Bool"
     | Unit -> S.Atom "Unit"
     | Byte -> S.Atom "Byte"
     | Int16 -> S.Atom "Int16"
     | UInt16 -> S.Atom "UInt16"
     | Int64 -> S.Atom "Int64"
     | UInt64 -> S.Atom "UInt64"
     | Float -> S.Atom "Float"
     | Double -> S.Atom "Double"
      : t -> S.t)

  let _ = sexp_of_t
end
