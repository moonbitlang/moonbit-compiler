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


module Hash_intf = Basic_hash_intf
module Hash_gen = Basic_hash_gen

module Make (Key : Hash_intf.HashedType) = struct
  type key = Key.t

  include struct
    let _ = fun (_ : key) -> ()
    let sexp_of_key = (Key.sexp_of_t : key -> S.t)
    let _ = sexp_of_key
    let equal_key = (Key.equal : key -> key -> bool)
    let _ = equal_key
  end

  type 'a t = (key, 'a) Hash_gen.t

  let key_index (h : _ t) (key : key) =
    Key.hash key land (Array.length h.data - 1)

  module Lst = Basic_lst
  module Unsafe_external = Basic_unsafe_external
  open Unsafe_external

  exception Key_not_found of key

  type ('a, 'b) bucket = ('a, 'b) Hash_gen.bucket

  let create = Hash_gen.create
  let clear = Hash_gen.clear
  let reset = Hash_gen.reset
  let iter = Hash_gen.iter
  let to_iter = Hash_gen.to_iter
  let iter2 = Hash_gen.iter2
  let to_list_with = Hash_gen.to_list_with
  let to_list = Hash_gen.to_list
  let to_array = Hash_gen.to_array
  let to_array_map = Hash_gen.to_array_map
  let to_array_filter_map = Hash_gen.to_array_filter_map
  let fold = Hash_gen.fold
  let length = Hash_gen.length

  let add (h : _ t) key data =
    let i = key_index h key in
    let h_data = h.data in
    h_data.!(i) <- Cons { key; data; next = h_data.!(i) };
    h.size <- h.size + 1;
    if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h

  let add_or_update (h : 'a t) (key : key) ~update:(modf : 'a -> 'a)
      (default : 'a) =
    (let rec find_bucket (bucketlist : _ bucket) =
       (match bucketlist with
        | Cons rhs ->
            if equal_key rhs.key key then (
              let data = modf rhs.data in
              rhs.data <- data;
              Some data)
            else find_bucket rhs.next
        | Empty -> None
         : 'a option)
     in
     let i = key_index h key in
     let h_data = h.data in
     match find_bucket h_data.!(i) with
     | Some data -> data
     | None ->
         h_data.!(i) <- Cons { key; data = default; next = h_data.!(i) };
         h.size <- h.size + 1;
         if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h;
         default
      : 'a)

  let remove (h : _ t) key =
    let i = key_index h key in
    let h_data = h.data in
    Hash_gen.remove_bucket h i key ~prec:Empty h_data.!(i) equal_key

  let rec find_rec key (bucketlist : _ bucket) =
    match bucketlist with
    | Empty -> raise (Key_not_found key)
    | Cons rhs ->
        if equal_key key rhs.key then rhs.data else find_rec key rhs.next

  let find_exn (h : _ t) key =
    match h.data.!(key_index h key) with
    | Empty -> raise (Key_not_found key)
    | Cons rhs -> (
        if equal_key key rhs.key then rhs.data
        else
          match rhs.next with
          | Empty -> raise (Key_not_found key)
          | Cons rhs -> (
              if equal_key key rhs.key then rhs.data
              else
                match rhs.next with
                | Empty -> raise (Key_not_found key)
                | Cons rhs ->
                    if equal_key key rhs.key then rhs.data
                    else find_rec key rhs.next))

  let find_opt (h : _ t) key =
    Hash_gen.small_bucket_opt equal_key key h.data.!(key_index h key)

  let find_key_opt (h : _ t) key =
    Hash_gen.small_bucket_key_opt equal_key key h.data.!(key_index h key)

  let find_default (h : _ t) key default =
    Hash_gen.small_bucket_default equal_key key default
      h.data.!(key_index h key)

  let find_or_update (type v) (h : v t) (key : key) ~(update : key -> v) =
    (let rec find_bucket h_data i (bucketlist : _ bucket) =
       match bucketlist with
       | Cons rhs ->
           if equal_key rhs.key key then rhs.data
           else find_bucket h_data i rhs.next
       | Empty ->
           let data = update key in
           h_data.!(i) <- Hash_gen.Cons { key; data; next = h_data.!(i) };
           h.size <- h.size + 1;
           if h.size > Array.length h_data lsl 1 then
             Hash_gen.resize key_index h;
           data
     in
     let i = key_index h key in
     let h_data = h.data in
     find_bucket h_data i h_data.!(i)
      : v)

  let find_all (h : _ t) key =
    let rec find_in_bucket (bucketlist : _ bucket) =
      match bucketlist with
      | Empty -> []
      | Cons rhs ->
          if equal_key key rhs.key then rhs.data :: find_in_bucket rhs.next
          else find_in_bucket rhs.next
        [@@tail_mod_cons]
    in
    find_in_bucket h.data.!(key_index h key)

  let replace h key data =
    let i = key_index h key in
    let h_data = h.data in
    let l = h_data.!(i) in
    if Hash_gen.replace_bucket key data l equal_key then (
      h_data.!(i) <- Cons { key; data; next = l };
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h)

  let update_if_exists h key f =
    let i = key_index h key in
    let h_data = h.data in
    let rec mutate_bucket h_data (bucketlist : _ bucket) =
      match bucketlist with
      | Cons rhs ->
          if equal_key rhs.key key then rhs.data <- f rhs.data
          else mutate_bucket h_data rhs.next
      | Empty -> ()
    in
    mutate_bucket h_data h_data.!(i)

  let mem (h : _ t) key =
    Hash_gen.small_bucket_mem h.data.!(key_index h key) equal_key key

  let of_list2 ks vs =
    let len = List.length ks in
    let map = create len in
    List.iter2 (fun k -> fun v -> add map k v) ks vs;
    map

  let of_list_map kvs f =
    let len = List.length kvs in
    let map = create len in
    Lst.iter kvs ~f:(fun kv ->
        let k, v = f kv in
        add map k v);
    map

  let of_list kvs =
    let len = List.length kvs in
    let map = create len in
    Lst.iter kvs ~f:(fun (k, v) -> add map k v);
    map

  let sexp_of_t (type a) (cb : a -> _) (x : a t) =
    Moon_sexp_conv.sexp_of_list
      (fun (k, v) -> S.List [ sexp_of_key k; cb v ])
      (to_list x)
end
