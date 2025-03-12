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


type t = int32

let to_string x = Printf.sprintf "%lu" x
let min_int = 0l
let max_int = -1l
let pred = Int32.pred
let succ = Int32.succ
let add = Int32.add
let sub = Int32.sub
let mul = Int32.mul
let div = Int32.unsigned_div
let rem = Int32.unsigned_rem
let compare = Int32.unsigned_compare
let equal = Int32.equal
let lognot = Int32.lognot
let logand = Int32.logand
let logor = Int32.logor
let logxor = Int32.logxor
let shift_left = Int32.shift_left
let shift_right = Int32.shift_right_logical
let to_int32 x = x
let of_int32 (x : int32) = (x : t)
let min x y = match compare x y with -1 -> x | _ -> y
let max x y = match compare x y with 1 -> x | _ -> y

let parse_digit chr =
  let c = Char.code chr in
  if c >= 48 && c <= 57 then c - 48
  else if c >= 65 && c <= 90 then c - 55
  else if c >= 97 && c <= 122 then c - 87
  else raise (Failure "parse_digit")

let of_string_unsigned i s =
  let rec aux i (acc : t) =
    if i < String.length s then (
      match s.[i] with
      | '_' -> aux (i + 1) acc
      | c ->
          let d = parse_digit c in
          if d < 0 || d >= 10 then raise (Failure "of_string_aux");
          let n = add (mul acc 10l) (Int32.of_int d) in
          if compare n acc < 0 then raise (Failure "of_string_aux");
          aux (i + 1) n)
    else acc
  in
  let r = aux i 0l in
  if compare r Int32.max_int > 0 then
    sub (sub (Int32.add Int32.min_int r) Int32.max_int) 1l
  else r

let of_string s =
  if String.length s > 2 then
    match String.sub s 0 2 with
    | "0x" | "0X" | "0o" | "0O" | "0b" | "0B" -> Int32.of_string s
    | "0u" | "0U" -> of_string_unsigned 2 s
    | _ -> of_string_unsigned 0 s
  else of_string_unsigned 0 s

let of_string_opt s = try Some (of_string s) with Failure _ -> None
