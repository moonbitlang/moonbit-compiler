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
module Arr = Basic_arr
open Unsafe_external

let min (x : int) y = if x < y then x else y [@@inline]
let unsafe_blit = Array.blit

type elt = int

include struct
  let _ = fun (_ : elt) -> ()
  let sexp_of_elt = (Moon_sexp_conv.sexp_of_int : elt -> S.t)
  let _ = sexp_of_elt
end

external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"

type t = { mutable arr : elt array; mutable len : int }

let sexp_of_t (x : t) =
  (fun x__001_ -> Moon_sexp_conv.sexp_of_array sexp_of_elt x__001_)
    (Array.sub x.arr 0 x.len)

let length d = d.len
let empty () = { len = 0; arr = [||] }

let of_list lst =
  let arr = Array.of_list lst in
  { arr; len = Array.length arr }

let of_array src = { len = Array.length src; arr = Array.copy src }
let of_sub_array arr off len = { len; arr = Array.sub arr off len }
let reverse_in_place src = Arr.reverse_range src.arr 0 src.len

let sub (src : t) start len =
  let src_len = src.len in
  if len < 0 || start > src_len - len then invalid_arg __FUNCTION__
  else { len; arr = unsafe_sub src.arr start len }

let iter d f =
  let arr = d.arr in
  for i = 0 to d.len - 1 do
    f arr.!(i)
  done

let map_into_array f src =
  let src_len = src.len in
  let src_arr = src.arr in
  if src_len = 0 then [||]
  else
    let first_one = f src_arr.!(0) in
    let arr = Array.make src_len first_one in
    for i = 1 to src_len - 1 do
      arr.!(i) <- f src_arr.!(i)
    done;
    arr

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

let mapi f src =
  let len = src.len in
  if len = 0 then { len; arr = [||] }
  else
    let src_arr = src.arr in
    let arr = Array.make len src_arr.!(0) in
    for i = 0 to len - 1 do
      arr.!(i) <- f i src_arr.!(i)
    done;
    { len; arr }

let equal eq x y =
  (if x.len <> y.len then false
   else
     let rec aux x_arr y_arr i =
       if i < 0 then true
       else if eq x_arr.!(i) y_arr.!(i) then aux x_arr y_arr (i - 1)
       else false
     in
     aux x.arr y.arr (x.len - 1)
    : bool)

let unsafe_get d i = d.arr.!(i)
let last d = if d.len <= 0 then invalid_arg __FUNCTION__ else d.arr.!(d.len - 1)

let exists p d =
  let a = d.arr in
  let n = d.len in
  let rec loop i =
    if i = n then false else if p a.!(i) then true else loop (succ i)
  in
  loop 0

let map f src =
  let src_len = src.len in
  if src_len = 0 then { len = 0; arr = [||] }
  else
    let src_arr = src.arr in
    let first = f src_arr.!(0) in
    let arr = Array.make src_len first in
    for i = 1 to src_len - 1 do
      arr.!(i) <- f src_arr.!(i)
    done;
    { len = src_len; arr }

let init len f =
  if len < 0 then invalid_arg __FUNCTION__
  else if len = 0 then { len = 0; arr = [||] }
  else
    let first = f 0 in
    let arr = Array.make len first in
    for i = 1 to len - 1 do
      arr.!(i) <- f i
    done;
    { len; arr }

let make initsize =
  (if initsize < 0 then invalid_arg __FUNCTION__;
   { len = 0; arr = Array.make initsize 0 }
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
      let new_d_arr = Array.make new_capacity 0 in
      d.arr <- new_d_arr;
      unsafe_blit d_arr 0 new_d_arr 0 d_len);
    d.len <- d_len + 1;
    d.arr.!(d_len) <- v)

let get_and_delete_range (d : t) idx len =
  (let d_len = d.len in
   if len < 0 || idx < 0 || idx + len > d_len then invalid_arg __FUNCTION__;
   let arr = d.arr in
   let value = unsafe_sub arr idx len in
   unsafe_blit arr (idx + len) arr idx (d_len - idx - len);
   d.len <- d_len - len;
   { len; arr = value }
    : t)

let get d i =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i)

let reset d =
  d.len <- 0;
  d.arr <- [||]

let set d i v =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i) <- v

let delete d i =
  let d_len = d.len in
  if i < 0 || i >= d_len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else
    let arr = d.arr in
    unsafe_blit arr (i + 1) arr i (d_len - i - 1);
    d.len <- d_len - 1

let arg_min (d : t) =
  let value = ref Int.max_int in
  let index = ref (-1) in
  for i = 0 to length d - 1 do
    let v = d.arr.!(i) in
    if v < !value then (
      value := v;
      index := i)
  done;
  !index

let iteri d f =
  let arr = d.arr in
  for i = 0 to d.len - 1 do
    f i arr.!(i)
  done
