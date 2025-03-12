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


[@@@warnerror "+55"]

module Unsafe_external = Basic_unsafe_external
module Iter = Basic_iter
open Unsafe_external

type 'a t0 =
  | Empty
  | Leaf of 'a
  | Node of { l : 'a t0; v : 'a; r : 'a t0; h : int }

type 'a partial_node = { l : 'a t0; v : 'a; r : 'a t0; h : int }

external ( ~! ) : 'a t0 -> 'a partial_node = "%identity"

let empty = Empty
let height = function Empty -> 0 | Leaf _ -> 1 | Node { h; _ } -> h [@@inline]
let calc_height a b = (if a >= b then a else b) + 1 [@@inline]
let unsafe_node v l r h = Node { l; v; r; h } [@@inline]

let unsafe_node_maybe_leaf v l r h =
  if h = 1 then Leaf v else Node { l; v; r; h }
[@@inline]

let singleton x = Leaf x [@@inline]
let unsafe_two_elements x v = unsafe_node v (singleton x) empty 2 [@@inline]

type 'a t = 'a t0 = private
  | Empty
  | Leaf of 'a
  | Node of { l : 'a t0; v : 'a; r : 'a t0; h : int }

let rec min_exn = function
  | Empty -> raise Not_found
  | Leaf v -> v
  | Node { l; v; _ } -> (
      match l with Empty -> v | Leaf _ | Node _ -> min_exn l)

let is_empty = function Empty -> true | _ -> false [@@inline]

let rec cardinal_aux acc = function
  | Empty -> acc
  | Leaf _ -> acc + 1
  | Node { l; r; _ } -> cardinal_aux (cardinal_aux (acc + 1) r) l

let cardinal s = cardinal_aux 0 s

let rec elements_aux accu = function
  | Empty -> accu
  | Leaf v -> v :: accu
  | Node { l; v; r; _ } -> elements_aux (v :: elements_aux accu r) l

let elements s = elements_aux [] s
let choose = min_exn

let rec iter x f =
  match x with
  | Empty -> ()
  | Leaf v -> f v
  | Node { l; v; r; _ } ->
      iter l f;
      f v;
      iter r f

let rec fold s accu f =
  match s with
  | Empty -> accu
  | Leaf v -> f v accu
  | Node { l; v; r; _ } -> fold r (f v (fold l accu f)) f

let rec for_all x p =
  match x with
  | Empty -> true
  | Leaf v -> p v
  | Node { l; v; r; _ } -> p v && for_all l p && for_all r p

let rec exists x p =
  match x with
  | Empty -> false
  | Leaf v -> p v
  | Node { l; v; r; _ } -> p v || exists l p || exists r p

exception Height_invariant_broken
exception Height_diff_borken

let rec check_height_and_diff = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { l; r; h; _ } ->
      let hl = check_height_and_diff l in
      let hr = check_height_and_diff r in
      if h <> calc_height hl hr then raise Height_invariant_broken
      else
        let diff = abs (hl - hr) in
        if diff > 2 then raise Height_diff_borken else h

let check tree = ignore (check_height_and_diff tree)

let bal l v r =
  (let hl = height l in
   let hr = height r in
   if hl > hr + 2 then
     let { l = ll; r = lr; v = lv; h = _ } = ~!l in
     let hll = height ll in
     let hlr = height lr in
     if hll >= hlr then
       let hnode = calc_height hlr hr in
       unsafe_node lv ll
         (unsafe_node_maybe_leaf v lr r hnode)
         (calc_height hll hnode)
     else
       let { l = lrl; r = lrr; v = lrv; _ } = ~!lr in
       let hlrl = height lrl in
       let hlrr = height lrr in
       let hlnode = calc_height hll hlrl in
       let hrnode = calc_height hlrr hr in
       unsafe_node lrv
         (unsafe_node_maybe_leaf lv ll lrl hlnode)
         (unsafe_node_maybe_leaf v lrr r hrnode)
         (calc_height hlnode hrnode)
   else if hr > hl + 2 then
     let { l = rl; r = rr; v = rv; _ } = ~!r in
     let hrr = height rr in
     let hrl = height rl in
     if hrr >= hrl then
       let hnode = calc_height hl hrl in
       unsafe_node rv
         (unsafe_node_maybe_leaf v l rl hnode)
         rr (calc_height hnode hrr)
     else
       let { l = rll; r = rlr; v = rlv; _ } = ~!rl in
       let hrll = height rll in
       let hrlr = height rlr in
       let hlnode = calc_height hl hrll in
       let hrnode = calc_height hrlr hrr in
       unsafe_node rlv
         (unsafe_node_maybe_leaf v l rll hlnode)
         (unsafe_node_maybe_leaf rv rlr rr hrnode)
         (calc_height hlnode hrnode)
   else unsafe_node_maybe_leaf v l r (calc_height hl hr)
    : _ t)

let rec remove_min_elt = function
  | Empty -> invalid_arg __FUNCTION__
  | Leaf _ -> empty
  | Node { l = Empty; r; _ } -> r
  | Node { l; v; r; _ } -> bal (remove_min_elt l) v r

let internal_merge l r =
  match (l, r) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ -> bal l (min_exn r) (remove_min_elt r)

let rec add_min v = function
  | Empty -> singleton v
  | Leaf x -> unsafe_two_elements v x
  | Node n -> bal (add_min v n.l) n.v n.r

let rec add_max v = function
  | Empty -> singleton v
  | Leaf x -> unsafe_two_elements x v
  | Node n -> bal n.l n.v (add_max v n.r)

let rec internal_join l v r =
  match (l, r) with
  | Empty, _ -> add_min v r
  | _, Empty -> add_max v l
  | Leaf lv, Node { h = rh; _ } ->
      if rh > 3 then add_min lv (add_min v r) else unsafe_node v l r (rh + 1)
  | Leaf _, Leaf _ -> unsafe_node v l r 2
  | Node { h = lh; _ }, Leaf rv ->
      if lh > 3 then add_max rv (add_max v l) else unsafe_node v l r (lh + 1)
  | ( Node { l = ll; v = lv; r = lr; h = lh },
      Node { l = rl; v = rv; r = rr; h = rh } ) ->
      if lh > rh + 2 then bal ll lv (internal_join lr v r)
      else if rh > lh + 2 then bal (internal_join l v rl) rv rr
      else unsafe_node v l r (calc_height lh rh)

let internal_concat t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ -> internal_join t1 (min_exn t2) (remove_min_elt t2)

let rec partition x p =
  match x with
  | Empty -> (empty, empty)
  | Leaf v ->
      let pv = p v in
      if pv then (x, empty) else (empty, x)
  | Node { l; v; r; _ } ->
      let lt, lf = partition l p in
      let pv = p v in
      let rt, rf = partition r p in
      if pv then (internal_join lt v rt, internal_concat lf rf)
      else (internal_concat lt rt, internal_join lf v rf)

let of_sorted_array l =
  let rec sub start n l =
    if n = 0 then empty
    else if n = 1 then
      let x0 = l.!(start) in
      singleton x0
    else if n = 2 then
      let x0 = l.!(start) in
      let x1 = l.!(start + 1) in
      unsafe_node x1 (singleton x0) empty 2
    else if n = 3 then
      let x0 = l.!(start) in
      let x1 = l.!(start + 1) in
      let x2 = l.!(start + 2) in
      unsafe_node x1 (singleton x0) (singleton x2) 2
    else
      let nl = n / 2 in
      let left = sub start nl l in
      let mid = start + nl in
      let v = l.!(mid) in
      let right = sub (mid + 1) (n - nl - 1) l in
      unsafe_node v left right (calc_height (height left) (height right))
  in
  sub 0 (Array.length l) l

let is_ordered ~cmp tree =
  let rec is_ordered_min_max tree =
    match tree with
    | Empty -> `Empty
    | Leaf v -> `V (v, v)
    | Node { l; v; r; _ } -> (
        match is_ordered_min_max l with
        | `No -> `No
        | `Empty -> (
            match is_ordered_min_max r with
            | `No -> `No
            | `Empty -> `V (v, v)
            | `V (l, r) -> if cmp v l < 0 then `V (v, r) else `No)
        | `V (min_v, max_v) -> (
            match is_ordered_min_max r with
            | `No -> `No
            | `Empty -> if cmp max_v v < 0 then `V (min_v, v) else `No
            | `V (min_v_r, max_v_r) ->
                if cmp max_v min_v_r < 0 then `V (min_v, max_v_r) else `No))
  in
  is_ordered_min_max tree <> `No

let invariant ~cmp t =
  check t;
  is_ordered ~cmp t

let to_list (t : 'a t0) =
  let v = ref [] in
  iter t (fun x -> v := x :: !v);
  List.rev !v

let map_to_list s f =
  let v = ref [] in
  iter s (fun x -> v := f x :: !v);
  List.rev !v

let of_iter t =
  let s = ref empty in
  t (fun x -> s := internal_merge !s (singleton x));
  !s
