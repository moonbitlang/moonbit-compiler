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


module Int_util = Basic_int
module Unsafe_external = Basic_unsafe_external
open Unsafe_external

type ('a, 'b) bucket =
  | Empty
  | Cons of { key : 'a; ord : int; data : 'b; next : ('a, 'b) bucket }

type ('a, 'b) t = {
  mutable size : int;
  mutable data : ('a, 'b) bucket array;
  initial_size : int;
}

let create initial_size =
  let s = Int_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  Array.fill h.data 0 len Empty

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then (
    let ndata = Array.make nsize Empty in
    h.data <- ndata;
    let rec insert_bucket = function
      | Empty -> ()
      | Cons { key; ord; data; next } ->
          let nidx = indexfun h key in
          ndata.!(nidx) <- Cons { key; ord; data; next = ndata.!(nidx) };
          insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.!(i)
    done)

let iter h f =
  let rec do_bucket = function
    | Empty -> ()
    | Cons { key; ord; data; next } ->
        f key data ord;
        do_bucket next
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.!(i)
  done

let choose h =
  let rec aux arr offset len =
    if offset >= len then raise Not_found
    else
      match arr.!(offset) with
      | Empty -> aux arr (offset + 1) len
      | Cons { key = k; _ } -> k
  in
  aux h.data 0 (Array.length h.data)

let to_sorted_array h =
  if h.size = 0 then [||]
  else
    let v = choose h in
    let arr = Array.make h.size v in
    iter h (fun k -> fun _ -> fun i -> arr.!(i) <- k);
    arr

let fold h init f =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons { key; ord; data; next } -> do_bucket next (f key data ord accu)
  in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.!(i) !accu
  done;
  !accu

let elements set = fold set [] (fun k -> fun _ -> fun _ -> fun acc -> k :: acc)

let rec bucket_length acc (x : _ bucket) =
  match x with Empty -> 0 | Cons rhs -> bucket_length (acc + 1) rhs.next
