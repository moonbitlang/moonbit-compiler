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

open struct
  open struct
    external as_float_arr : 'a array -> float array = "%identity"
    external as_obj_arr : 'a array -> Obj.t array = "%identity"
  end

  let tag = Obj.tag
  and repr = Obj.repr
  and double_array_tag = Obj.double_array_tag

  let fill_with_junk_ (a : _ array) i len =
    (if tag (repr a) = double_array_tag then
       Array.fill (as_float_arr a) i len 0.
     else Array.fill (as_obj_arr a) i len (Obj.repr ())
      : unit)
end

external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"

type 'a t = { mutable arr : 'a array; mutable len : int }

let sexp_of_t sexp_of_a x =
  Moon_sexp_conv.sexp_of_array sexp_of_a (Array.sub x.arr 0 x.len)

let length d = d.len
let unsafe_internal_array d = d.arr
let empty () = { len = 0; arr = [||] }
let is_empty d = d.len = 0

let to_list d =
  let rec loop (d_arr : 'a array) idx accum =
    if idx < 0 then accum else loop d_arr (idx - 1) (d_arr.!(idx) :: accum)
  in
  loop d.arr (d.len - 1) []

let of_list lst =
  let arr = Array.of_list lst in
  { arr; len = Array.length arr }

let to_array d = unsafe_sub d.arr 0 d.len

let sort d cmp =
  let arr = to_array d in
  Array.fast_sort cmp arr;
  d.arr <- arr;
  d.len <- Array.length arr

let of_array src = { len = Array.length src; arr = Array.copy src }
let reverse_in_place src = Arr.reverse_range src.arr 0 src.len

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

let map_into_list_and_append src ~f acc =
  let src_len = src.len in
  let src_arr = src.arr in
  if src_len = 0 then acc
  else
    let acc = ref acc in
    for i = src_len - 1 downto 0 do
      acc := f src_arr.!(i) :: !acc
    done;
    !acc

let fold_left ~f x a =
  let rec loop a_len (a_arr : 'a array) idx x =
    if idx >= a_len then x else loop a_len a_arr (idx + 1) (f x a_arr.!(idx))
  in
  loop a.len a.arr 0 x

let fold_right ~f a x =
  let rec loop (a_arr : 'a array) idx x =
    if idx < 0 then x else loop a_arr (idx - 1) (f a_arr.!(idx) x)
  in
  loop a.arr (a.len - 1) x

let fold_lefti ~f x a =
  let rec loop a_len (a_arr : 'a array) idx x =
    if idx >= a_len then x
    else loop a_len a_arr (idx + 1) (f idx x a_arr.!(idx))
  in
  loop a.len a.arr 0 x

let get d i =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i)

let get_opt d i = if i < 0 || i >= d.len then None else Some d.arr.!(i)
let last d = if d.len <= 0 then invalid_arg __FUNCTION__ else d.arr.!(d.len - 1)

let set_last d v =
  if d.len <= 0 then invalid_arg __FUNCTION__ else d.arr.!(d.len - 1) <- v

let set d i v =
  if i < 0 || i >= d.len then invalid_arg (__FUNCTION__ ^ " " ^ string_of_int i)
  else d.arr.!(i) <- v

let capacity d = Array.length d.arr [@@inline]

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

let make ~dummy initsize =
  (if initsize < 0 then invalid_arg __FUNCTION__;
   { len = 0; arr = Array.make initsize dummy }
    : 'a t)

let push (d : 'a t) v =
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
      let new_d_arr = Array.make new_capacity d.arr.!(0) in
      fill_with_junk_ new_d_arr 0 new_capacity;
      d.arr <- new_d_arr;
      unsafe_blit d_arr 0 new_d_arr 0 d_len);
    d.len <- d_len + 1;
    d.arr.!(d_len) <- v)

let insert (d : 'a t) idx elt =
  let enlarge size =
    if size >= Sys.max_array_length then failwith "exceeds max_array_length";
    let new_arr = Array.make size elt in
    fill_with_junk_ new_arr 0 size;
    unsafe_blit d.arr 0 new_arr 0 (length d);
    d.arr <- new_arr
  in
  if idx < 0 || idx > length d then failwith "index out of range";
  if length d + 1 > capacity d then enlarge (capacity d * 2);
  unsafe_blit d.arr idx d.arr (idx + 1) (length d - idx);
  d.arr.(idx) <- elt;
  d.len <- d.len + 1

let pop_no_compact (d : 'a t) =
  (let d_len = d.len in
   if d_len = 0 then None
   else
     let d_arr = d.arr in
     let last_index = d_len - 1 in
     let last = d_arr.!(last_index) in
     fill_with_junk_ d_arr last_index 1;
     d.len <- last_index;
     Some last
    : 'a option)
