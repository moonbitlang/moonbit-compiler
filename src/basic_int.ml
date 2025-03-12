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


let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let string_of_int n =
  if n = 0 then "0"
  else if n = Int.min_int then Stdlib.string_of_int n
  else
    let rec count_digits_aux n acc =
      if n = 0 then acc else count_digits_aux (n / 10) (acc + 1)
    in
    let count_digits n = count_digits_aux n 0 in
    let negative = n < 0 in
    let n = if negative then -n else n in
    let len = count_digits n in
    let total_len = if negative then len + 1 else len in
    let bytes = Bytes.create total_len in
    let n = ref n in
    let index =
      if negative then (
        Bytes.unsafe_set bytes 0 '-';
        1)
      else 0
    in
    for pos = total_len - 1 downto index do
      Bytes.unsafe_set bytes pos (Char.unsafe_chr (48 + (!n mod 10)));
      n := !n / 10
    done;
    Bytes.unsafe_to_string bytes
