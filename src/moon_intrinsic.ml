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


module Tid = Basic_ty_ident

type t =
  | Char_to_string
  | F64_to_string
  | String_substring
  | FixedArray_join
  | FixedArray_iter
  | FixedArray_iteri
  | FixedArray_map
  | FixedArray_fold_left
  | FixedArray_copy
  | FixedArray_fill
  | Iter_map
  | Iter_iter
  | Iter_from_array
  | Iter_take
  | Iter_reduce
  | Iter_flat_map
  | Iter_repeat
  | Iter_filter
  | Iter_concat
  | Array_length
  | Array_get
  | Array_unsafe_get
  | Array_set
  | Array_unsafe_set
  | ArrayView_length
  | ArrayView_unsafe_set
  | ArrayView_unsafe_get
  | ArrayView_unsafe_as_view
  | BytesView_length
  | BytesView_unsafe_get
  | BytesView_unsafe_as_view

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (function
     | Char_to_string -> S.Atom "Char_to_string"
     | F64_to_string -> S.Atom "F64_to_string"
     | String_substring -> S.Atom "String_substring"
     | FixedArray_join -> S.Atom "FixedArray_join"
     | FixedArray_iter -> S.Atom "FixedArray_iter"
     | FixedArray_iteri -> S.Atom "FixedArray_iteri"
     | FixedArray_map -> S.Atom "FixedArray_map"
     | FixedArray_fold_left -> S.Atom "FixedArray_fold_left"
     | FixedArray_copy -> S.Atom "FixedArray_copy"
     | FixedArray_fill -> S.Atom "FixedArray_fill"
     | Iter_map -> S.Atom "Iter_map"
     | Iter_iter -> S.Atom "Iter_iter"
     | Iter_from_array -> S.Atom "Iter_from_array"
     | Iter_take -> S.Atom "Iter_take"
     | Iter_reduce -> S.Atom "Iter_reduce"
     | Iter_flat_map -> S.Atom "Iter_flat_map"
     | Iter_repeat -> S.Atom "Iter_repeat"
     | Iter_filter -> S.Atom "Iter_filter"
     | Iter_concat -> S.Atom "Iter_concat"
     | Array_length -> S.Atom "Array_length"
     | Array_get -> S.Atom "Array_get"
     | Array_unsafe_get -> S.Atom "Array_unsafe_get"
     | Array_set -> S.Atom "Array_set"
     | Array_unsafe_set -> S.Atom "Array_unsafe_set"
     | ArrayView_length -> S.Atom "ArrayView_length"
     | ArrayView_unsafe_set -> S.Atom "ArrayView_unsafe_set"
     | ArrayView_unsafe_get -> S.Atom "ArrayView_unsafe_get"
     | ArrayView_unsafe_as_view -> S.Atom "ArrayView_unsafe_as_view"
     | BytesView_length -> S.Atom "BytesView_length"
     | BytesView_unsafe_get -> S.Atom "BytesView_unsafe_get"
     | BytesView_unsafe_as_view -> S.Atom "BytesView_unsafe_as_view"
      : t -> S.t)

  let _ = sexp_of_t
  let equal = (Stdlib.( = ) : t -> t -> bool)
  let _ = equal
end

let string_of_t (i : t) =
  (match i with
   | Char_to_string -> "%char.to_string"
   | F64_to_string -> "%f64.to_string"
   | String_substring -> "%string.substring"
   | FixedArray_join -> "%fixedarray.join"
   | FixedArray_iter -> "%fixedarray.iter"
   | FixedArray_iteri -> "%fixedarray.iteri"
   | FixedArray_map -> "%fixedarray.map"
   | FixedArray_fold_left -> "%fixedarray.fold_left"
   | FixedArray_copy -> "%fixedarray.copy"
   | FixedArray_fill -> "%fixedarray.fill"
   | Iter_map -> "%iter.map"
   | Iter_iter -> "%iter.iter"
   | Iter_take -> "%iter.take"
   | Iter_reduce -> "%iter.reduce"
   | Iter_from_array -> "%iter.from_array"
   | Iter_flat_map -> "%iter.flat_map"
   | Iter_repeat -> "%iter.repeat"
   | Iter_filter -> "%iter.filter"
   | Iter_concat -> "%iter.concat"
   | Array_length -> "%array.length"
   | Array_get -> "%array.get"
   | Array_unsafe_get -> "%array.unsafe_get"
   | Array_set -> "%array.set"
   | Array_unsafe_set -> "%array.unsafe_set"
   | ArrayView_length -> "%arrayview.length"
   | ArrayView_unsafe_set -> "%arrayview.unsafe_set"
   | ArrayView_unsafe_get -> "%arrayview.unsafe_get"
   | ArrayView_unsafe_as_view -> "%arrayview.unsafe_as_view"
   | BytesView_length -> "%bytesview.length"
   | BytesView_unsafe_get -> "%bytesview.unsafe_get"
   | BytesView_unsafe_as_view -> "%bytesview.unsafe_as_view"
    : string)
