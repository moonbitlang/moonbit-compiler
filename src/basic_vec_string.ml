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


module Unsafe_external = Basic_unsafe_external
open Unsafe_external

type elt = string

let min (x : int) y = if x < y then x else y [@@inline]
let unsafe_blit = Array.blit

type t = { mutable arr : string array; mutable len : int }

let sexp_of_t x =
  Moon_sexp_conv.sexp_of_array Moon_sexp_conv.sexp_of_string
    (Array.sub x.arr 0 x.len)

let length d = d.len
let empty () = { len = 0; arr = [||] }

let of_list lst =
  let arr = Array.of_list lst in
  { arr; len = Array.length arr }

let of_array src = { len = Array.length src; arr = Array.copy src }

let iter d f =
  let arr = d.arr in
  for i = 0 to d.len - 1 do
    f arr.!(i)
  done

let iteri d f =
  let arr = d.arr in
  for i = 0 to d.len - 1 do
    f i arr.!(i)
  done

let get d i =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i)

let last d = if d.len <= 0 then invalid_arg __FUNCTION__ else d.arr.!(d.len - 1)

let set d i v =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i) <- v

let map f src =
  let src_len = src.len in
  let src_arr = src.arr in
  let arr = Array.make src_len "" in
  for i = 0 to src_len - 1 do
    arr.!(i) <- f src_arr.!(i)
  done;
  { len = src_len; arr }

let make initsize =
  (if initsize < 0 then invalid_arg __FUNCTION__;
   { len = 0; arr = Array.make initsize "" }
    : t)

let push (d : t) v =
  let d_len = d.len in
  let d_arr = d.arr in
  let d_arr_len = Array.length d_arr in
  if d_arr_len = 0 then (
    d.len <- 1;
    d.arr <- [| v |])
  else (
    if d_len = d_arr_len then (
      if d_len >= Sys.max_array_length then failwith "exceeds max_array_length";
      let new_capacity = min Sys.max_array_length d_len * 2 in
      let new_d_arr = Array.make new_capacity "" in
      d.arr <- new_d_arr;
      unsafe_blit d_arr 0 new_d_arr 0 d_len);
    d.len <- d_len + 1;
    d.arr.!(d_len) <- v)

let map_into_list src ~unorder:f =
  let src_len = src.len in
  let src_arr = src.arr in
  if src_len = 0 then []
  else
    let acc = ref [] in
    for i = src_len - 1 downto 0 do
      acc := f src_arr.!(i) :: !acc
    done;
    !acc
