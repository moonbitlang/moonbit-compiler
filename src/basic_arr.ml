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


open Basic_unsafe_external

let reverse_range a i len =
  if len = 0 then ()
  else
    for k = 0 to (len - 1) / 2 do
      let t = a.!(i + k) in
      a.!(i + k) <- a.!(i + len - 1 - k);
      a.!(i + len - 1 - k) <- t
    done

let reverse_in_place a = reverse_range a 0 (Array.length a)

let reverse a =
  let b_len = Array.length a in
  if b_len = 0 then [||]
  else
    let b = Array.copy a in
    for i = 0 to b_len - 1 do
      b.!(i) <- a.!(b_len - 1 - i)
    done;
    b

let reverse_of_list = function
  | [] -> [||]
  | hd :: tl ->
      let len = List.length tl in
      let a = Array.make (len + 1) hd in
      let rec fill i = function
        | [] -> a
        | hd :: tl ->
            a.!(i) <- hd;
            fill (i - 1) tl
      in
      fill (len - 1) tl

let filter a f =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len then reverse_of_list acc
    else
      let v = a.!(i) in
      if f v then aux (v :: acc) (i + 1) else aux acc (i + 1)
  in
  aux [] 0

let filter_map a (f : _ -> _ option) =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len then reverse_of_list acc
    else
      let v = a.!(i) in
      match f v with Some v -> aux (v :: acc) (i + 1) | None -> aux acc (i + 1)
  in
  aux [] 0

let range from to_ =
  if from > to_ then invalid_arg __FUNCTION__
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b =
  let len = Array.length a in
  if len <> Array.length b then invalid_arg __FUNCTION__
  else Array.mapi (fun i -> fun a -> f i a b.!(i)) a

let rec tolist_f_aux a f i res =
  if i < 0 then res
  else
    let v = a.!(i) in
    tolist_f_aux a f (i - 1) (f v :: res)

let to_list_f a f = tolist_f_aux a f (Array.length a - 1) []

let rec tolist_aux a f i res =
  if i < 0 then res
  else
    tolist_aux a f (i - 1)
      (match f a.!(i) with Some v -> v :: res | None -> res)

let to_list_map a f = tolist_aux a f (Array.length a - 1) []
let to_list_map_acc a acc f = tolist_aux a f (Array.length a - 1) acc

let of_list_map a f =
  match a with
  | [] -> [||]
  | a0 :: [] ->
      let b0 = f a0 in
      [| b0 |]
  | [ a0; a1 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      [| b0; b1 |]
  | [ a0; a1; a2 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      [| b0; b1; b2 |]
  | [ a0; a1; a2; a3 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      [| b0; b1; b2; b3 |]
  | [ a0; a1; a2; a3; a4 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      let b4 = f a4 in
      [| b0; b1; b2; b3; b4 |]
  | a0 :: a1 :: a2 :: a3 :: a4 :: tl ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      let b4 = f a4 in
      let len = List.length tl + 5 in
      let arr = Array.make len b0 in
      arr.!(1) <- b1;
      arr.!(2) <- b2;
      arr.!(3) <- b3;
      arr.!(4) <- b4;
      let rec fill i = function
        | [] -> arr
        | hd :: tl ->
            arr.!(i) <- f hd;
            fill (i + 1) tl
      in
      fill 5 tl

let of_list_mapi a f =
  match a with
  | [] -> [||]
  | a0 :: [] ->
      let b0 = f 0 a0 in
      [| b0 |]
  | [ a0; a1 ] ->
      let b0 = f 0 a0 in
      let b1 = f 1 a1 in
      [| b0; b1 |]
  | [ a0; a1; a2 ] ->
      let b0 = f 0 a0 in
      let b1 = f 1 a1 in
      let b2 = f 2 a2 in
      [| b0; b1; b2 |]
  | [ a0; a1; a2; a3 ] ->
      let b0 = f 0 a0 in
      let b1 = f 1 a1 in
      let b2 = f 2 a2 in
      let b3 = f 3 a3 in
      [| b0; b1; b2; b3 |]
  | [ a0; a1; a2; a3; a4 ] ->
      let b0 = f 0 a0 in
      let b1 = f 1 a1 in
      let b2 = f 2 a2 in
      let b3 = f 3 a3 in
      let b4 = f 4 a4 in
      [| b0; b1; b2; b3; b4 |]
  | a0 :: a1 :: a2 :: a3 :: a4 :: tl ->
      let b0 = f 0 a0 in
      let b1 = f 1 a1 in
      let b2 = f 2 a2 in
      let b3 = f 3 a3 in
      let b4 = f 4 a4 in
      let len = List.length tl + 5 in
      let arr = Array.make len b0 in
      arr.!(1) <- b1;
      arr.!(2) <- b2;
      arr.!(3) <- b3;
      arr.!(4) <- b4;
      let rec fill i = function
        | [] -> arr
        | hd :: tl ->
            arr.!(i) <- f i hd;
            fill (i + 1) tl
      in
      fill 5 tl

let rfind_with_index arr cmp v =
  let len = Array.length arr in
  let rec aux i =
    if i < 0 then i else if cmp arr.!(i) v then i else aux (i - 1)
  in
  aux (len - 1)

let find arr f =
  let len = Array.length arr in
  let rec aux i =
    if i >= len then None
    else
      let elem = arr.(i) in
      if f elem then Some elem else aux (i + 1)
  in
  aux 0

let find_map a f =
  let n = Array.length a in
  let rec loop i =
    if i = n then None
    else match f a.!(i) with None -> loop (succ i) | Some _ as r -> r
  in
  loop 0

type 'a split = No_split | Split of 'a array * 'a array

let find_with_index arr cmp v =
  let len = Array.length arr in
  let rec aux i len =
    if i >= len then -1 else if cmp arr.!(i) v then i else aux (i + 1) len
  in
  aux 0 len

let find_and_split arr cmp v =
  (let i = find_with_index arr cmp v in
   if i < 0 then No_split
   else
     Split (Array.sub arr 0 i, Array.sub arr (i + 1) (Array.length arr - i - 1))
    : _ split)

let exists a p =
  let n = Array.length a in
  let rec loop i =
    if i = n then false else if p a.!(i) then true else loop (succ i)
  in
  loop 0

let is_empty arr = Array.length arr = 0

let rec unsafe_loop index len p xs ys =
  if index >= len then true
  else p xs.!(index) ys.!(index) && unsafe_loop (succ index) len p xs ys

let for_alli a p =
  let n = Array.length a in
  let rec loop i =
    if i = n then true else if p i a.!(i) then loop (succ i) else false
  in
  loop 0

let for_all2_no_exn xs ys p =
  let len_xs = Array.length xs in
  let len_ys = Array.length ys in
  len_xs = len_ys && unsafe_loop 0 len_xs p xs ys

let map a f =
  let l = Array.length a in
  if l = 0 then [||]
  else
    let r = Array.make l (f a.!(0)) in
    for i = 1 to l - 1 do
      r.!(i) <- f a.!(i)
    done;
    r

let iter a f =
  for i = 0 to Array.length a - 1 do
    f a.!(i)
  done

let fold_left a x f =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f !r a.!(i)
  done;
  !r

let get_or arr i cb = if i >= 0 && i < Array.length arr then arr.!(i) else cb ()

let get_opt arr i =
  if i >= 0 && i < Array.length arr then Some arr.!(i) else None
