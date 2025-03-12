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


module Key = struct
  type t = string

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (Moon_sexp_conv.sexp_of_string : t -> S.t)
    let _ = sexp_of_t

    let compare =
      (fun a__001_ -> fun b__002_ -> Stdlib.compare (a__001_ : string) b__002_
        : t -> t -> int)

    let _ = compare

    let equal =
      (fun a__003_ -> fun b__004_ -> Stdlib.( = ) (a__003_ : string) b__004_
        : t -> t -> bool)

    let _ = equal
  end
end

module Set_gen = Basic_set_gen
module Lst = Basic_lst

type elt = Key.t
type 'a t0 = 'a Set_gen.t
type t = elt t0

let empty = Set_gen.empty
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all
let exists = Set_gen.exists
let singleton = Set_gen.singleton
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let choose = Set_gen.choose
let to_list = Set_gen.to_list
let map_to_list = Set_gen.map_to_list
let of_sorted_array = Set_gen.of_sorted_array

let rec mem (tree : t) (x : elt) =
  match tree with
  | Empty -> false
  | Leaf v -> Key.equal x v
  | Node { l; v; r; _ } ->
      let c = Key.compare x v in
      c = 0 || mem (if c < 0 then l else r) x

type split = Yes of { l : t; r : t } | No of { l : t; r : t }

let split_l (x : split) = match x with Yes { l; _ } | No { l; _ } -> l
[@@inline]

let split_r (x : split) = match x with Yes { r; _ } | No { r; _ } -> r
[@@inline]

let split_pres (x : split) = match x with Yes _ -> true | No _ -> false
[@@inline]

let rec split (tree : t) x =
  (match tree with
   | Empty -> No { l = empty; r = empty }
   | Leaf v ->
       let c = Key.compare x v in
       if c = 0 then Yes { l = empty; r = empty }
       else if c < 0 then No { l = empty; r = tree }
       else No { l = tree; r = empty }
   | Node { l; v; r; _ } -> (
       let c = Key.compare x v in
       if c = 0 then Yes { l; r }
       else if c < 0 then
         match split l x with
         | Yes result ->
             Yes { result with r = Set_gen.internal_join result.r v r }
         | No result ->
             No { result with r = Set_gen.internal_join result.r v r }
       else
         match split r x with
         | Yes result ->
             Yes { result with l = Set_gen.internal_join l v result.l }
         | No result ->
             No { result with l = Set_gen.internal_join l v result.l })
    : split)

let rec add (tree : t) x =
  (match tree with
   | Empty -> singleton x
   | Leaf v ->
       let c = Key.compare x v in
       if c = 0 then tree
       else if c < 0 then Set_gen.unsafe_two_elements x v
       else Set_gen.unsafe_two_elements v x
   | Node { l; v; r; _ } as t ->
       let c = Key.compare x v in
       if c = 0 then t
       else if c < 0 then Set_gen.bal (add l x) v r
       else Set_gen.bal l v (add r x)
    : t)

let rec check_add (tree : t) ~(duplicate_flag : bool ref) (x : elt) =
  (match tree with
   | Empty -> singleton x
   | Leaf v ->
       let c = Key.compare x v in
       if c = 0 then (
         duplicate_flag := true;
         tree)
       else if c < 0 then Set_gen.unsafe_two_elements x v
       else Set_gen.unsafe_two_elements v x
   | Node { l; v; r; _ } ->
       let c = Key.compare x v in
       if c = 0 then tree
       else if c < 0 then Set_gen.bal (check_add l ~duplicate_flag x) v r
       else Set_gen.bal l v (check_add r ~duplicate_flag x)
    : t)

let rec union (s1 : t) (s2 : t) =
  (match (s1, s2) with
   | Empty, t | t, Empty -> t
   | Node _, Leaf v2 -> add s1 v2
   | Leaf v1, Node _ -> add s2 v1
   | Leaf x, Leaf v ->
       let c = Key.compare x v in
       if c = 0 then s1
       else if c < 0 then Set_gen.unsafe_two_elements x v
       else Set_gen.unsafe_two_elements v x
   | ( Node { l = l1; v = v1; r = r1; h = h1 },
       Node { l = l2; v = v2; r = r2; h = h2 } ) ->
       if h1 >= h2 then
         let split_result = split s2 v1 in
         Set_gen.internal_join
           (union l1 (split_l split_result))
           v1
           (union r1 (split_r split_result))
       else
         let split_result = split s1 v2 in
         Set_gen.internal_join
           (union (split_l split_result) l2)
           v2
           (union (split_r split_result) r2)
    : t)

let rec inter (s1 : t) (s2 : t) =
  (match (s1, s2) with
   | Empty, _ | _, Empty -> empty
   | Leaf v, _ -> if mem s2 v then s1 else empty
   | Node ({ v; _ } as s1), _ ->
       let result = split s2 v in
       if split_pres result then
         Set_gen.internal_join
           (inter s1.l (split_l result))
           v
           (inter s1.r (split_r result))
       else
         Set_gen.internal_concat
           (inter s1.l (split_l result))
           (inter s1.r (split_r result))
    : t)

let rec diff (s1 : t) (s2 : t) =
  (match (s1, s2) with
   | Empty, _ -> empty
   | t1, Empty -> t1
   | Leaf v, _ -> if mem s2 v then empty else s1
   | Node ({ v; _ } as s1), _ ->
       let result = split s2 v in
       if split_pres result then
         Set_gen.internal_concat
           (diff s1.l (split_l result))
           (diff s1.r (split_r result))
       else
         Set_gen.internal_join
           (diff s1.l (split_l result))
           v
           (diff s1.r (split_r result))
    : t)

let rec remove (tree : t) (x : elt) =
  (match tree with
   | Empty -> empty
   | Leaf v -> if Key.equal x v then empty else tree
   | Node { l; v; r; _ } ->
       let c = Key.compare x v in
       if c = 0 then Set_gen.internal_merge l r
       else if c < 0 then Set_gen.bal (remove l x) v r
       else Set_gen.bal l v (remove r x)
    : t)

let of_list l =
  match l with
  | [] -> empty
  | x0 :: [] -> singleton x0
  | [ x0; x1 ] -> add (singleton x0) x1
  | [ x0; x1; x2 ] -> add (add (singleton x0) x1) x2
  | [ x0; x1; x2; x3 ] -> add (add (add (singleton x0) x1) x2) x3
  | [ x0; x1; x2; x3; x4 ] -> add (add (add (add (singleton x0) x1) x2) x3) x4
  | x0 :: x1 :: x2 :: x3 :: x4 :: rest ->
      let init = add (add (add (add (singleton x0) x1) x2) x3) x4 in
      Lst.fold_left rest init add

let invariant t =
  Set_gen.check t;
  Set_gen.is_ordered ~cmp:Key.compare t

let add_list (env : t) params =
  (List.fold_left (fun env -> fun e -> add env e) env params : t)

let sexp_of_t t = Moon_sexp_conv.sexp_of_list Key.sexp_of_t (to_list t)

let filter t f =
  let nt = ref empty in
  iter t (fun e -> if f e then nt := add !nt e);
  !nt
