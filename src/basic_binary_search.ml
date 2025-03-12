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


open Basic_unsafe_external

let search_range (x : int) (a : int array) =
  (let rec go lower_bound upper_bound =
     if lower_bound > upper_bound then -1
     else
       let m = lower_bound + ((upper_bound - lower_bound) / 2) in
       let x0 = a.!(2 * m) in
       let x1 = a.!((2 * m) + 1) in
       if x < x0 then go lower_bound (m - 1)
       else if x > x1 then go (m + 1) upper_bound
       else m
   in
   go 0 ((Array.length a / 2) - 1)
    : int)

type binary_search_result = Found of int | Insert_at of int

let binary_search_by (cmp : 'a -> int) (arr : 'a array) =
  (let rec aux lo sz =
     if sz <= 1 then lo
     else
       let half = sz / 2 in
       let mid = lo + half in
       let new_sz = sz - half in
       let new_lo = if cmp arr.!(mid) > 0 then lo else mid in
       aux new_lo new_sz
   in
   let pp = aux 0 (Array.length arr) in
   let cmp_pp = cmp arr.!(pp) in
   if cmp_pp = 0 then Found pp
   else Insert_at (if cmp_pp > 0 then pp else pp + 1)
    : binary_search_result)

let binary_search cmp arr v = binary_search_by (fun x -> cmp x v) arr

let partition_point (pred : 'a -> bool) (arr : 'a array) =
  let res = binary_search_by (fun x -> if pred x then -1 else 1) arr in
  match res with Found i | Insert_at i -> i
