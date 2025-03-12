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


module Hashset_gen = Basic_hashset_gen

type key = string

include struct
  let _ = fun (_ : key) -> ()

  let equal_key =
    (fun a__001_ -> fun b__002_ -> Stdlib.( = ) (a__001_ : string) b__002_
      : key -> key -> bool)

  let _ = equal_key
end

let key_index (h : _ Hashset_gen.t) (key : key) =
  Ppx_base.hash_string key land (Array.length h.data - 1)

type t = key Hashset_gen.t

module Unsafe_external = Basic_unsafe_external
open Unsafe_external

let create = Hashset_gen.create
let clear = Hashset_gen.clear
let reset = Hashset_gen.reset
let iter = Hashset_gen.iter
let to_iter = Hashset_gen.to_iter
let fold = Hashset_gen.fold
let length = Hashset_gen.length
let to_list = Hashset_gen.to_list

let remove (h : _ Hashset_gen.t) key =
  let i = key_index h key in
  let h_data = h.data in
  Hashset_gen.remove_bucket h i key ~prec:Empty h_data.!(i) equal_key

let add (h : _ Hashset_gen.t) key =
  let i = key_index h key in
  let h_data = h.data in
  let old_bucket = h_data.!(i) in
  if not (Hashset_gen.small_bucket_mem equal_key key old_bucket) then (
    h_data.!(i) <- Cons { key; next = old_bucket };
    h.size <- h.size + 1;
    if h.size > Array.length h_data lsl 1 then Hashset_gen.resize key_index h)

let of_array arr =
  let len = Array.length arr in
  let tbl = create len in
  for i = 0 to len - 1 do
    add tbl arr.!(i)
  done;
  tbl

let check_add (h : _ Hashset_gen.t) key =
  (let i = key_index h key in
   let h_data = h.data in
   let old_bucket = h_data.!(i) in
   if not (Hashset_gen.small_bucket_mem equal_key key old_bucket) then (
     h_data.!(i) <- Cons { key; next = old_bucket };
     h.size <- h.size + 1;
     if h.size > Array.length h_data lsl 1 then Hashset_gen.resize key_index h;
     true)
   else false
    : bool)

let find_or_add (h : _ Hashset_gen.t) key =
  (let i = key_index h key in
   let h_data = h.data in
   let old_bucket = h_data.!(i) in
   match Hashset_gen.small_bucket_find equal_key key old_bucket with
   | Some key0 -> key0
   | None ->
       h_data.!(i) <- Cons { key; next = old_bucket };
       h.size <- h.size + 1;
       if h.size > Array.length h_data lsl 1 then Hashset_gen.resize key_index h;
       key
    : key)

let mem (h : _ Hashset_gen.t) key =
  Hashset_gen.small_bucket_mem equal_key key h.data.!(key_index h key)
