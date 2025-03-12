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


type t = int64

let to_string x = Printf.sprintf "%Lu" x
let min_int = 0L
let max_int = -1L
let pred = Int64.pred
let succ = Int64.succ
let add = Int64.add
let sub = Int64.sub
let mul = Int64.mul
let div = Int64.unsigned_div
let rem = Int64.unsigned_rem
let compare = Int64.unsigned_compare
let equal = Int64.equal
let lognot = Int64.lognot
let logand = Int64.logand
let logor = Int64.logor
let logxor = Int64.logxor
let shift_left = Int64.shift_left
let shift_right = Int64.shift_right_logical
let of_int64 (x : int64) = (x : t)
let to_int64 x = x
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
          let n = add (mul acc 10L) (Int64.of_int d) in
          if compare n acc < 0 then raise (Failure "of_string_aux");
          aux (i + 1) n)
    else acc
  in
  let r = aux i 0L in
  if Int64.compare r Int64.max_int = 1 then
    sub (sub (Int64.add Int64.min_int r) Int64.max_int) 1L
  else r

let of_string s =
  if String.length s > 2 then
    match String.sub s 0 2 with
    | "0x" | "0X" | "0o" | "0O" | "0b" | "0B" -> Int64.of_string s
    | "0u" | "0U" -> of_string_unsigned 2 s
    | _ -> of_string_unsigned 0 s
  else of_string_unsigned 0 s

let of_string_opt s = try Some (of_string s) with Failure _ -> None
