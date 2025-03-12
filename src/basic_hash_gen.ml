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
module Vec = Basic_vec
open Basic_unsafe_external

type ('a, 'b) bucket =
  | Empty
  | Cons of {
      mutable key : 'a;
      mutable data : 'b;
      mutable next : ('a, 'b) bucket;
    }

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
    let ndata_tail = Array.make nsize Empty in
    h.data <- ndata;
    let rec insert_bucket = function
      | Empty -> ()
      | Cons { key; next; _ } as cell ->
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

let iter2 h f =
  let rec do_bucket = function
    | Empty -> ()
    | Cons l ->
        f l.key l.data;
        do_bucket l.next
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.!(i)
  done

let iter h f = iter2 h (fun k -> fun v -> f (k, v)) [@@inline]
let to_iter h = Basic_iter.make (fun f -> iter2 h (fun k -> fun v -> f (k, v)))

let fold h init f =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons l -> do_bucket l.next (f l.key l.data accu)
  in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.!(i) !accu
  done;
  !accu

let to_list_with h f =
  fold h [] (fun k -> fun data -> fun acc -> f k data :: acc)

let to_list h = fold h [] (fun k -> fun data -> fun acc -> (k, data) :: acc)

let rec small_bucket_mem (lst : _ bucket) eq key =
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
          | Cons lst -> eq key lst.key || small_bucket_mem lst.next eq key))

let rec small_bucket_opt eq key (lst : _ bucket) =
  (match lst with
   | Empty -> None
   | Cons lst -> (
       if eq key lst.key then Some lst.data
       else
         match lst.next with
         | Empty -> None
         | Cons lst -> (
             if eq key lst.key then Some lst.data
             else
               match lst.next with
               | Empty -> None
               | Cons lst ->
                   if eq key lst.key then Some lst.data
                   else small_bucket_opt eq key lst.next))
    : _ option)

let rec small_bucket_key_opt eq key (lst : _ bucket) =
  (match lst with
   | Empty -> None
   | Cons { key = k; next; _ } -> (
       if eq key k then Some k
       else
         match next with
         | Empty -> None
         | Cons { key = k; next; _ } -> (
             if eq key k then Some k
             else
               match next with
               | Empty -> None
               | Cons { key = k; next; _ } ->
                   if eq key k then Some k else small_bucket_key_opt eq key next
             ))
    : _ option)

let rec small_bucket_default eq key default (lst : _ bucket) =
  match lst with
  | Empty -> default
  | Cons lst -> (
      if eq key lst.key then lst.data
      else
        match lst.next with
        | Empty -> default
        | Cons lst -> (
            if eq key lst.key then lst.data
            else
              match lst.next with
              | Empty -> default
              | Cons lst ->
                  if eq key lst.key then lst.data
                  else small_bucket_default eq key default lst.next))

let rec remove_bucket h (i : int) key ~(prec : _ bucket) (buck : _ bucket)
    eq_key =
  match buck with
  | Empty -> ()
  | Cons { key = k; next; _ } ->
      if eq_key k key then (
        h.size <- h.size - 1;
        match prec with
        | Empty -> h.data.!(i) <- next
        | Cons c -> c.next <- next)
      else remove_bucket h i key ~prec:buck next eq_key

let rec replace_bucket key data (buck : _ bucket) eq_key =
  match buck with
  | Empty -> true
  | Cons slot ->
      if eq_key slot.key key then (
        slot.key <- key;
        slot.data <- data;
        false)
      else replace_bucket key data slot.next eq_key

let to_array (type key) (type a) (x : (key, a) t) =
  (let vec = Vec.empty () in
   iter x (fun entry -> Vec.push vec entry);
   Vec.to_array vec
    : (key * a) array)

let to_array_map (type key) (type a) (type b) (x : (key, a) t)
    (f : key * a -> b) =
  (let vec = Vec.empty () in
   iter x (fun entry -> Vec.push vec (f entry));
   Vec.to_array vec
    : b array)

let to_array_filter_map (type key) (type a) (type b) (x : (key, a) t)
    (f : key * a -> b option) =
  (let vec = Vec.empty () in
   iter x (fun entry ->
       match f entry with Some v -> Vec.push vec v | None -> ());
   Vec.to_array vec
    : b array)
