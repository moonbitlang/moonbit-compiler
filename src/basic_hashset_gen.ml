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
open Basic_unsafe_external

type 'a bucket =
  | Empty
  | Cons of { mutable key : 'a; mutable next : 'a bucket }

type 'a t = {
  mutable size : int;
  mutable data : 'a bucket array;
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
    let ndata_tail = Array.make nsize Empty in
    h.data <- ndata;
    let rec insert_bucket = function
      | Empty -> ()
      | Cons { key; next } as cell ->
          let nidx = indexfun h key in
          (match ndata_tail.!(nidx) with
          | Empty -> ndata.!(nidx) <- cell
          | Cons tail -> tail.next <- cell);
          ndata_tail.!(nidx) <- cell;
          insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.!(i)
    done;
    for i = 0 to nsize - 1 do
      match ndata_tail.!(i) with Empty -> () | Cons tail -> tail.next <- Empty
    done)

let iter h f =
  let rec do_bucket = function
    | Empty -> ()
    | Cons l ->
        f l.key;
        do_bucket l.next
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.!(i)
  done

let to_iter h =
  Basic_iter.make (fun f ->
      let rec do_bucket = function
        | Empty -> ()
        | Cons l ->
            f l.key;
            do_bucket l.next
      in
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        do_bucket d.!(i)
      done)

let fold h init f =
  let rec do_bucket b accu =
    match b with Empty -> accu | Cons l -> do_bucket l.next (f l.key accu)
  in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.!(i) !accu
  done;
  !accu

let to_list set = fold set [] List.cons

let rec small_bucket_mem eq key lst =
  match lst with
  | Empty -> false
  | Cons lst -> (
      eq key lst.key
      ||
      match lst.next with
      | Empty -> false
      | Cons lst -> (
          eq key lst.key
          ||
          match lst.next with
          | Empty -> false
          | Cons lst -> eq key lst.key || small_bucket_mem eq key lst.next))

let rec small_bucket_find eq key lst =
  match lst with
  | Empty -> None
  | Cons lst -> (
      if eq key lst.key then Some lst.key
      else
        match lst.next with
        | Empty -> None
        | Cons lst -> (
            if eq key lst.key then Some lst.key
            else
              match lst.next with
              | Empty -> None
              | Cons lst ->
                  if eq key lst.key then Some lst.key
                  else small_bucket_find eq key lst.next))

let rec remove_bucket (h : _ t) (i : int) key ~(prec : _ bucket)
    (buck : _ bucket) eq_key =
  match buck with
  | Empty -> ()
  | Cons { key = k; next } ->
      if eq_key k key then (
        h.size <- h.size - 1;
        match prec with
        | Empty -> h.data.!(i) <- next
        | Cons c -> c.next <- next)
      else remove_bucket h i key ~prec:buck next eq_key
