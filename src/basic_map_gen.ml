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

type ('key, 'a) t0 =
  | Empty
  | Leaf of { k : 'key; v : 'a }
  | Node of { l : ('key, 'a) t0; k : 'key; v : 'a; r : ('key, 'a) t0; h : int }

type ('key, 'a) parital_node = {
  l : ('key, 'a) t0;
  k : 'key;
  v : 'a;
  r : ('key, 'a) t0;
  h : int;
}

external ( ~! ) : ('key, 'a) t0 -> ('key, 'a) parital_node = "%identity"

let empty = Empty

let rec map x f =
  match x with
  | Empty -> Empty
  | Leaf { k; v } -> Leaf { k; v = f v }
  | Node ({ l; v; r; _ } as x) ->
      let l' = map l f in
      let d' = f v in
      let r' = map r f in
      Node { x with l = l'; v = d'; r = r' }

let rec mapi x f =
  match x with
  | Empty -> Empty
  | Leaf { k; v } -> Leaf { k; v = f k v }
  | Node ({ l; k; v; r; _ } as x) ->
      let l' = mapi l f in
      let v' = f k v in
      let r' = mapi r f in
      Node { x with l = l'; v = v'; r = r' }

let calc_height a b = (if a >= b then a else b) + 1 [@@inline]
let singleton k v = Leaf { k; v } [@@inline]
let height = function Empty -> 0 | Leaf _ -> 1 | Node { h; _ } -> h [@@inline]
let unsafe_node k v l r h = Node { l; k; v; r; h } [@@inline]

let unsafe_two_elements k1 v1 k2 v2 =
  unsafe_node k2 v2 (singleton k1 v1) empty 2
[@@inline]

let unsafe_node_maybe_leaf k v l r h =
  if h = 1 then Leaf { k; v } else Node { l; k; v; r; h }
[@@inline]

type ('key, +'a) t = ('key, 'a) t0 = private
  | Empty
  | Leaf of { k : 'key; v : 'a }
  | Node of { l : ('key, 'a) t; k : 'key; v : 'a; r : ('key, 'a) t; h : int }

let rec cardinal_aux acc = function
  | Empty -> acc
  | Leaf _ -> acc + 1
  | Node { l; r; _ } -> cardinal_aux (cardinal_aux (acc + 1) r) l

let cardinal s = cardinal_aux 0 s

let rec bindings_aux accu = function
  | Empty -> accu
  | Leaf { k; v } -> (k, v) :: accu
  | Node { l; k; v; r; _ } -> bindings_aux ((k, v) :: bindings_aux accu r) l

let bindings s = bindings_aux [] s

let rec fill_array_with_f (s : _ t) i arr f =
  (match s with
   | Empty -> i
   | Leaf { k; v } ->
       arr.!(i) <- f k v;
       i + 1
   | Node { l; k; v; r; _ } ->
       let inext = fill_array_with_f l i arr f in
       arr.!(inext) <- f k v;
       fill_array_with_f r (inext + 1) arr f
    : int)

let rec fill_array_aux (s : _ t) i arr =
  (match s with
   | Empty -> i
   | Leaf { k; v } ->
       arr.!(i) <- (k, v);
       i + 1
   | Node { l; k; v; r; _ } ->
       let inext = fill_array_aux l i arr in
       arr.!(inext) <- (k, v);
       fill_array_aux r (inext + 1) arr
    : int)

let to_sorted_array (s : ('key, 'a) t) =
  (match s with
   | Empty -> [||]
   | Leaf { k; v } -> [| (k, v) |]
   | Node { l; k; v; r; _ } ->
       let len = cardinal_aux (cardinal_aux 1 r) l in
       let arr = Array.make len (k, v) in
       ignore (fill_array_aux s 0 arr : int);
       arr
    : ('key * 'a) array)

let to_sorted_array_with_f (type key) (type a) (type b) (s : (key, a) t)
    (f : key -> a -> b) =
  (match s with
   | Empty -> [||]
   | Leaf { k; v } -> [| f k v |]
   | Node { l; k; v; r; _ } ->
       let len = cardinal_aux (cardinal_aux 1 r) l in
       let arr = Array.make len (f k v) in
       ignore (fill_array_with_f s 0 arr f : int);
       arr
    : b array)

let rec keys_aux accu = function
  | Empty -> accu
  | Leaf { k; _ } -> k :: accu
  | Node { l; k; r; _ } -> keys_aux (k :: keys_aux accu r) l

let keys s = keys_aux [] s

let bal l x d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    let { l = ll; r = lr; v = lv; k = lk; h = _ } = ~!l in
    let hll = height ll in
    let hlr = height lr in
    if hll >= hlr then
      let hnode = calc_height hlr hr in
      unsafe_node lk lv ll
        (unsafe_node_maybe_leaf x d lr r hnode)
        (calc_height hll hnode)
    else
      let { l = lrl; r = lrr; k = lrk; v = lrv; _ } = ~!lr in
      let hlrl = height lrl in
      let hlrr = height lrr in
      let hlnode = calc_height hll hlrl in
      let hrnode = calc_height hlrr hr in
      unsafe_node lrk lrv
        (unsafe_node_maybe_leaf lk lv ll lrl hlnode)
        (unsafe_node_maybe_leaf x d lrr r hrnode)
        (calc_height hlnode hrnode)
  else if hr > hl + 2 then
    let { l = rl; r = rr; k = rk; v = rv; _ } = ~!r in
    let hrr = height rr in
    let hrl = height rl in
    if hrr >= hrl then
      let hnode = calc_height hl hrl in
      unsafe_node rk rv
        (unsafe_node_maybe_leaf x d l rl hnode)
        rr (calc_height hnode hrr)
    else
      let { l = rll; r = rlr; k = rlk; v = rlv; _ } = ~!rl in
      let hrll = height rll in
      let hrlr = height rlr in
      let hlnode = calc_height hl hrll in
      let hrnode = calc_height hrlr hrr in
      unsafe_node rlk rlv
        (unsafe_node_maybe_leaf x d l rll hlnode)
        (unsafe_node_maybe_leaf rk rv rlr rr hrnode)
        (calc_height hlnode hrnode)
  else unsafe_node_maybe_leaf x d l r (calc_height hl hr)

let is_empty = function Empty -> true | _ -> false [@@inline]

let rec min_binding_exn = function
  | Empty -> raise Not_found
  | Leaf { k; v } -> (k, v)
  | Node { l; k; v; _ } -> (
      match l with Empty -> (k, v) | Leaf _ | Node _ -> min_binding_exn l)

let rec remove_min_binding = function
  | Empty -> invalid_arg __FUNCTION__
  | Leaf _ -> empty
  | Node { l = Empty; r; _ } -> r
  | Node { l; k; v; r; _ } -> bal (remove_min_binding l) k v r

let merge t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
      let x, d = min_binding_exn t2 in
      bal t1 x d (remove_min_binding t2)

let rec iter x f =
  match x with
  | Empty -> ()
  | Leaf { k; v } -> (f k v : unit)
  | Node { l; k; v; r; _ } ->
      iter l f;
      f k v;
      iter r f

let rec fold m accu f =
  match m with
  | Empty -> accu
  | Leaf { k; v } -> f k v accu
  | Node { l; k; v; r; _ } -> fold r (f k v (fold l accu f)) f

let rec for_all x p =
  match x with
  | Empty -> true
  | Leaf { k; v } -> p k v
  | Node { l; k; v; r; _ } -> p k v && for_all l p && for_all r p

let rec exists x p =
  match x with
  | Empty -> false
  | Leaf { k; v } -> p k v
  | Node { l; k; v; r; _ } -> p k v || exists l p || exists r p

let rec add_min k v = function
  | Empty -> singleton k v
  | Leaf l -> unsafe_two_elements k v l.k l.v
  | Node tree -> bal (add_min k v tree.l) tree.k tree.v tree.r

let rec add_max k v = function
  | Empty -> singleton k v
  | Leaf l -> unsafe_two_elements l.k l.v k v
  | Node tree -> bal tree.l tree.k tree.v (add_max k v tree.r)

let rec join l v d r =
  match l with
  | Empty -> add_min v d r
  | Leaf leaf -> add_min leaf.k leaf.v (add_min v d r)
  | Node xl -> (
      match r with
      | Empty -> add_max v d l
      | Leaf leaf -> add_max leaf.k leaf.v (add_max v d l)
      | Node xr ->
          let lh = xl.h in
          let rh = xr.h in
          if lh > rh + 2 then bal xl.l xl.k xl.v (join xl.r v d r)
          else if rh > lh + 2 then bal (join l v d xr.l) xr.k xr.v xr.r
          else unsafe_node v d l r (calc_height lh rh))

let concat t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
      let x, d = min_binding_exn t2 in
      join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with Some d -> join t1 v d t2 | None -> concat t1 t2
