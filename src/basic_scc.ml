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
module Vec_int = Basic_vec_int
module Vec = Basic_vec
open Unsafe_external

type node = Vec_int.t

let min_int (x : int) y = if x < y then x else y

let graph e =
  let index = ref 0 in
  let s = Vec_int.empty () in
  let output = Vec.empty () in
  let node_numes = Array.length e in
  let on_stack_array = Array.make node_numes false in
  let index_array = Array.make node_numes (-1) in
  let lowlink_array = Array.make node_numes (-1) in
  let rec scc v_data =
    let new_index = !index + 1 in
    index := new_index;
    Vec_int.push s v_data;
    index_array.(v_data) <- new_index;
    lowlink_array.(v_data) <- new_index;
    on_stack_array.(v_data) <- true;
    let v = e.(v_data) in
    Vec_int.iter v (fun w_data ->
        if index_array.!(w_data) < 0 then (
          scc w_data;
          lowlink_array.!(v_data) <-
            min_int lowlink_array.!(v_data) lowlink_array.!(w_data))
        else if on_stack_array.!(w_data) then
          lowlink_array.!(v_data) <-
            min_int lowlink_array.!(v_data) lowlink_array.!(w_data));
    if lowlink_array.!(v_data) = index_array.!(v_data) then (
      let s_len = Vec_int.length s in
      let last_index = ref (s_len - 1) in
      let u = ref (Vec_int.unsafe_get s !last_index) in
      while !u <> v_data do
        on_stack_array.!(!u) <- false;
        decr last_index;
        u := Vec_int.unsafe_get s !last_index
      done;
      on_stack_array.(v_data) <- false;
      Vec.push output
        (Vec_int.get_and_delete_range s !last_index (s_len - !last_index)))
  in
  for i = 0 to node_numes - 1 do
    if index_array.!(i) < 0 then scc i
  done;
  output

let graph_check v =
  let v = graph v in
  ( Vec.length v,
    Vec.fold_left ~f:(fun acc -> fun x -> Vec_int.length x :: acc) [] v )
