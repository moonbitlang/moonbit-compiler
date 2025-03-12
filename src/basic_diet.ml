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


module Diet_intf = Basic_diet_intf

type 'a t = 'a Basic_diet_gen.t

let create = Basic_diet_gen.create
and empty = Basic_diet_gen.empty
and is_empty = Basic_diet_gen.is_empty
and splitMax = Basic_diet_gen.splitMax
and bal = Basic_diet_gen.bal
and fold = Basic_diet_gen.fold
and iter_range = Basic_diet_gen.iter_range

module Make (Elt : Diet_intf.ELT) = struct
  type elt = Elt.t

  let ( >~ ) x y = Elt.compare x y > 0
  let ( >=~ ) x y = Elt.compare x y >= 0
  let ( <~ ) x y = Elt.compare x y < 0
  let ( <=~ ) x y = Elt.compare x y <= 0

  type nonrec t = elt t

  let has_single_element (set : t) =
    match set with
    | Node { l = Empty; r = Empty; h = _; x; y } -> Elt.equal x y
    | _ -> false

  let singleton (x : elt) (y : elt) =
    if x >~ y then invalid_arg __FUNCTION__;
    create x y Empty Empty

  let rec mem elt (diet : t) =
    match diet with
    | Empty -> false
    | Node n ->
        (elt >=~ n.x && elt <=~ n.y)
        || if elt <~ n.x then mem elt n.l else mem elt n.r

  let empty = empty
  let is_empty = is_empty

  let merge (l : t) (r : t) =
    match (l, r) with
    | l, Empty -> l
    | Empty, r -> r
    | (Node _ as l), r ->
        let x, y, l' = splitMax l in
        bal x y l' r

  let rec removeAux (t : t) ~lo ~hi =
    (match t with
     | Empty -> Empty
     | Node n ->
         if hi <~ n.x then
           let l = removeAux ~lo ~hi n.l in
           bal n.x n.y l n.r
         else if n.y <~ lo then
           let r = removeAux ~lo ~hi n.r in
           bal n.x n.y n.l r
         else if lo <~ n.x && hi <~ n.y then
           let n' = bal (Elt.succ hi) n.y n.l n.r in
           removeAux ~lo ~hi:(Elt.pred n.x) n'
         else if hi >~ n.y && lo >~ n.x then
           let n' = bal n.x (Elt.pred lo) n.l n.r in
           removeAux ~lo:(Elt.succ n.y) ~hi n'
         else if lo <=~ n.x && hi >=~ n.y then
           let l = removeAux ~lo ~hi:n.x n.l in
           let r = removeAux ~lo:n.y ~hi n.r in
           merge l r
         else if Elt.equal hi n.y then bal n.x (Elt.pred lo) n.l n.r
         else if Elt.equal lo n.x then bal (Elt.succ hi) n.y n.l n.r
         else (
           assert (n.x <~ lo);
           assert (hi <~ n.y);
           let r = bal (Elt.succ hi) n.y Empty n.r in
           bal n.x (Elt.pred lo) n.l r)
      : t)

  let diff a b = fold removeAux b a
  let iter_range = iter_range

  let iter t f =
    iter_range t (fun (min, max) ->
        let cur = ref min in
        while not (Elt.equal !cur max) do
          f !cur;
          cur := Elt.succ !cur
        done;
        f max)
end
