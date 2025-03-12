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


let rec compare_list compare_elt a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else compare_list compare_elt xs ys

let rec equal_list equal_elt a b =
  match (a, b) with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x :: xs, y :: ys -> equal_elt x y && equal_list equal_elt xs ys

type state = int
type hash_value = int

external hash_fold_int : state -> int -> state = "Ppx_fold_int" [@@noalloc]

external hash_fold_string : state -> string -> state = "Ppx_fold_string"
[@@noalloc]

external hash_string : string -> int = "Ppx_hash_string" [@@noalloc]
external get_hash_value : state -> hash_value = "Ppx_get_hash_value" [@@noalloc]

let hash_fold_option hash_fold_elem s = function
  | None -> hash_fold_int s 0
  | Some x -> hash_fold_elem (hash_fold_int s 1) x

let hash_int (t : int) =
  let t = lnot t + (t lsl 21) in
  let t = t lxor (t lsr 24) in
  let t = t + (t lsl 3) + (t lsl 8) in
  let t = t lxor (t lsr 14) in
  let t = t + (t lsl 2) + (t lsl 4) in
  let t = t lxor (t lsr 28) in
  t + (t lsl 31)
[@@inline always]

let rec hash_fold_list_body hash_fold_elem s list =
  match list with
  | [] -> s
  | x :: xs -> hash_fold_list_body hash_fold_elem (hash_fold_elem s x) xs

let hash_fold_list hash_fold_elem s list =
  let s = hash_fold_int s (List.length list) in
  let s = hash_fold_list_body hash_fold_elem s list in
  s

let hash_fold_char s c = hash_fold_int s (Char.code c)
let hash_fold_bool s b = hash_fold_int s (if b then 1 else 0)
let create () = (0 : state)
