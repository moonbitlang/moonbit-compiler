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


module Lst = Basic_lst

let non_exn_protect r v body =
  let old = !r in
  r := v;
  let res = body () in
  r := old;
  res

let protect r v body =
  let old = !r in
  try
    r := v;
    let res = body () in
    r := old;
    res
  with x ->
    r := old;
    raise x

let non_exn_protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in
  r1 := v1;
  r2 := v2;
  let res = body () in
  r1 := old1;
  r2 := old2;
  res

let protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in
  try
    r1 := v1;
    r2 := v2;
    let res = body () in
    r1 := old1;
    r2 := old2;
    res
  with x ->
    r1 := old1;
    r2 := old2;
    raise x

let protect_list rvs body =
  let olds = Lst.map rvs (fun (x, _) -> !x) in
  let () = Lst.iter rvs ~f:(fun (x, y) -> x := y) in
  try
    let res = body () in
    List.iter2 (fun (x, _) -> fun old -> x := old) rvs olds;
    res
  with e ->
    List.iter2 (fun (x, _) -> fun old -> x := old) rvs olds;
    raise e
