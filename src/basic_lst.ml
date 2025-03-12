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


module Arr = Basic_arr

let rec map l f =
  match l with
  | [] -> []
  | x1 :: [] ->
      let y1 = f x1 in
      [ y1 ]
  | [ x1; x2 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      [ y1; y2 ]
  | [ x1; x2; x3 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      [ y1; y2; y3 ]
  | [ x1; x2; x3; x4 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      [ y1; y2; y3; y4 ]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      let y5 = f x5 in
      y1 :: y2 :: y3 :: y4 :: y5 :: map tail f
[@@tail_mod_cons]

let rec map_and_check_tail l f =
  match l with
  | [] -> []
  | x :: [] -> [ f true x ]
  | x1 :: (_ :: _ as tail) ->
      let y1 = f false x1 in
      y1 :: map_and_check_tail tail f
[@@tail_mod_cons]

let rec iter_and_check_tail l f =
  match l with
  | [] -> ()
  | x :: [] -> f true x
  | x1 :: (_ :: _ as tail) ->
      f false x1;
      iter_and_check_tail tail f

let rec has_string (l : string list) f =
  match l with
  | [] -> false
  | x1 :: [] -> x1 = f
  | [ x1; x2 ] -> x1 = f || x2 = f
  | [ x1; x2; x3 ] -> x1 = f || x2 = f || x3 = f
  | x1 :: x2 :: x3 :: x4 -> x1 = f || x2 = f || x3 = f || has_string x4 f

let rec map_split (xs : 'a list) (f : 'a -> 'b * 'c) =
  (match xs with
   | [] -> ([], [])
   | x :: xs ->
       let c, d = f x in
       let cs, ds = map_split xs f in
       (c :: cs, d :: ds)
    : 'b list * 'c list)

let rec mapi_aux lst i f tail =
  match lst with
  | [] -> tail
  | x0 :: [] -> f i x0 :: tail
  | [ x0; x1 ] ->
      let r0 = f i x0 in
      let r1 = f (i + 1) x1 in
      r0 :: r1 :: tail
  | [ x0; x1; x2 ] ->
      let r0 = f i x0 in
      let r1 = f (i + 1) x1 in
      let r2 = f (i + 2) x2 in
      r0 :: r1 :: r2 :: tail
  | [ x0; x1; x2; x3 ] ->
      let r0 = f i x0 in
      let r1 = f (i + 1) x1 in
      let r2 = f (i + 2) x2 in
      let r3 = f (i + 3) x3 in
      r0 :: r1 :: r2 :: r3 :: tail
  | a :: l ->
      let r = f i a in
      r :: mapi_aux l (i + 1) f tail
[@@tail_mod_cons]

let mapi lst f = mapi_aux lst 0 f []
let mapi_append lst f tail = mapi_aux lst 0 f tail

let rec last xs =
  match xs with
  | x :: [] -> x
  | _ :: tl -> last tl
  | [] -> invalid_arg __FUNCTION__

let rec append_aux l1 l2 =
  match l1 with
  | [] -> l2
  | a0 :: [] -> a0 :: l2
  | [ a0; a1 ] -> a0 :: a1 :: l2
  | [ a0; a1; a2 ] -> a0 :: a1 :: a2 :: l2
  | [ a0; a1; a2; a3 ] -> a0 :: a1 :: a2 :: a3 :: l2
  | [ a0; a1; a2; a3; a4 ] -> a0 :: a1 :: a2 :: a3 :: a4 :: l2
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
      a0 :: a1 :: a2 :: a3 :: a4 :: append_aux rest l2
[@@tail_mod_cons]

let append l1 l2 = match l2 with [] -> l1 | _ -> append_aux l1 l2

let rec map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | a0 :: [] -> f a0 :: l2
  | [ a0; a1 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      b0 :: b1 :: l2
  | [ a0; a1; a2 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      b0 :: b1 :: b2 :: l2
  | [ a0; a1; a2; a3 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      b0 :: b1 :: b2 :: b3 :: l2
  | [ a0; a1; a2; a3; a4 ] ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      let b4 = f a4 in
      b0 :: b1 :: b2 :: b3 :: b4 :: l2
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
      let b0 = f a0 in
      let b1 = f a1 in
      let b2 = f a2 in
      let b3 = f a3 in
      let b4 = f a4 in
      b0 :: b1 :: b2 :: b3 :: b4 :: map_append rest l2 f
[@@tail_mod_cons]

let rec rev_split chunks_acc input_tail =
  (match input_tail with
   | _x0 :: _x1 :: _x2 :: _x3 :: _x4 :: _x5 :: _x6 :: _x7 :: _x8 :: _x9 :: tail
     as chunk_start ->
       rev_split (chunk_start :: chunks_acc) tail
   | [] -> chunks_acc
   | remaining -> remaining :: chunks_acc
    : 'a list list)

let fold_right_chunk l acc f =
  match l with
  | [] -> acc
  | a0 :: [] -> f a0 acc
  | [ a0; a1 ] -> f a0 (f a1 acc)
  | [ a0; a1; a2 ] -> f a0 (f a1 (f a2 acc))
  | [ a0; a1; a2; a3 ] -> f a0 (f a1 (f a2 (f a3 acc)))
  | [ a0; a1; a2; a3; a4 ] -> f a0 (f a1 (f a2 (f a3 (f a4 acc))))
  | [ a0; a1; a2; a3; a4; a5 ] -> f a0 (f a1 (f a2 (f a3 (f a4 (f a5 acc)))))
  | [ a0; a1; a2; a3; a4; a5; a6 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 acc))))))
  | [ a0; a1; a2; a3; a4; a5; a6; a7 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 acc)))))))
  | [ a0; a1; a2; a3; a4; a5; a6; a7; a8 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 (f a8 acc))))))))
  | a0 :: a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: _rest ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 (f a8 (f a9 acc)))))))))

let fold_right l acc f =
  match l with
  | [] -> acc
  | a0 :: [] -> f a0 acc
  | [ a0; a1 ] -> f a0 (f a1 acc)
  | [ a0; a1; a2 ] -> f a0 (f a1 (f a2 acc))
  | [ a0; a1; a2; a3 ] -> f a0 (f a1 (f a2 (f a3 acc)))
  | [ a0; a1; a2; a3; a4 ] -> f a0 (f a1 (f a2 (f a3 (f a4 acc))))
  | [ a0; a1; a2; a3; a4; a5 ] -> f a0 (f a1 (f a2 (f a3 (f a4 (f a5 acc)))))
  | [ a0; a1; a2; a3; a4; a5; a6 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 acc))))))
  | [ a0; a1; a2; a3; a4; a5; a6; a7 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 acc)))))))
  | [ a0; a1; a2; a3; a4; a5; a6; a7; a8 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 (f a8 acc))))))))
  | [ a0; a1; a2; a3; a4; a5; a6; a7; a8; a9 ] ->
      f a0 (f a1 (f a2 (f a3 (f a4 (f a5 (f a6 (f a7 (f a8 (f a9 acc)))))))))
  | _a0 :: _a1 :: _a2 :: _a3 :: _a4 :: _a5 :: _a6 :: _a7 :: _a8 :: _a9 :: rest
    ->
      let chunks = rev_split [ l ] rest in
      let rec fold_chunks chunks acc ~f =
        match chunks with
        | [] -> acc
        | chunk :: preceding_chunks ->
            let new_acc = fold_right_chunk chunk acc f in
            fold_chunks preceding_chunks new_acc ~f
      in
      fold_chunks chunks acc ~f

let rec fold_right2 l r acc f =
  match (l, r) with
  | [], [] -> acc
  | a0 :: [], b0 :: [] -> f a0 b0 acc
  | [ a0; a1 ], [ b0; b1 ] -> f a0 b0 (f a1 b1 acc)
  | [ a0; a1; a2 ], [ b0; b1; b2 ] -> f a0 b0 (f a1 b1 (f a2 b2 acc))
  | [ a0; a1; a2; a3 ], [ b0; b1; b2; b3 ] ->
      f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 acc)))
  | [ a0; a1; a2; a3; a4 ], [ b0; b1; b2; b3; b4 ] ->
      f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 acc))))
  | a0 :: a1 :: a2 :: a3 :: a4 :: arest, b0 :: b1 :: b2 :: b3 :: b4 :: brest ->
      f a0 b0
        (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 (fold_right2 arest brest acc f)))))
  | _, _ -> invalid_arg __FUNCTION__

let rec map2i_aux l r f i =
  match (l, r) with
  | [], [] -> []
  | a0 :: [], b0 :: [] -> [ f i a0 b0 ]
  | [ a0; a1 ], [ b0; b1 ] ->
      let c0 = f i a0 b0 in
      let c1 = f (i + 1) a1 b1 in
      [ c0; c1 ]
  | [ a0; a1; a2 ], [ b0; b1; b2 ] ->
      let c0 = f i a0 b0 in
      let c1 = f (i + 1) a1 b1 in
      let c2 = f (i + 2) a2 b2 in
      [ c0; c1; c2 ]
  | [ a0; a1; a2; a3 ], [ b0; b1; b2; b3 ] ->
      let c0 = f i a0 b0 in
      let c1 = f (i + 1) a1 b1 in
      let c2 = f (i + 2) a2 b2 in
      let c3 = f (i + 3) a3 b3 in
      [ c0; c1; c2; c3 ]
  | [ a0; a1; a2; a3; a4 ], [ b0; b1; b2; b3; b4 ] ->
      let c0 = f i a0 b0 in
      let c1 = f (i + 1) a1 b1 in
      let c2 = f (i + 2) a2 b2 in
      let c3 = f (i + 3) a3 b3 in
      let c4 = f (i + 4) a4 b4 in
      [ c0; c1; c2; c3; c4 ]
  | a0 :: a1 :: a2 :: a3 :: a4 :: arest, b0 :: b1 :: b2 :: b3 :: b4 :: brest ->
      let c0 = f i a0 b0 in
      let c1 = f (i + 1) a1 b1 in
      let c2 = f (i + 2) a2 b2 in
      let c3 = f (i + 3) a3 b3 in
      let c4 = f (i + 4) a4 b4 in
      c0 :: c1 :: c2 :: c3 :: c4 :: map2i_aux arest brest f (i + 5)
  | _, _ -> invalid_arg __FUNCTION__
[@@tail_mod_cons]

let map2i l r f = map2i_aux l r f 0

let rec map2 l r f =
  match (l, r) with
  | [], [] -> []
  | a0 :: [], b0 :: [] -> [ f a0 b0 ]
  | [ a0; a1 ], [ b0; b1 ] ->
      let c0 = f a0 b0 in
      let c1 = f a1 b1 in
      [ c0; c1 ]
  | [ a0; a1; a2 ], [ b0; b1; b2 ] ->
      let c0 = f a0 b0 in
      let c1 = f a1 b1 in
      let c2 = f a2 b2 in
      [ c0; c1; c2 ]
  | [ a0; a1; a2; a3 ], [ b0; b1; b2; b3 ] ->
      let c0 = f a0 b0 in
      let c1 = f a1 b1 in
      let c2 = f a2 b2 in
      let c3 = f a3 b3 in
      [ c0; c1; c2; c3 ]
  | [ a0; a1; a2; a3; a4 ], [ b0; b1; b2; b3; b4 ] ->
      let c0 = f a0 b0 in
      let c1 = f a1 b1 in
      let c2 = f a2 b2 in
      let c3 = f a3 b3 in
      let c4 = f a4 b4 in
      [ c0; c1; c2; c3; c4 ]
  | a0 :: a1 :: a2 :: a3 :: a4 :: arest, b0 :: b1 :: b2 :: b3 :: b4 :: brest ->
      let c0 = f a0 b0 in
      let c1 = f a1 b1 in
      let c2 = f a2 b2 in
      let c3 = f a3 b3 in
      let c4 = f a4 b4 in
      c0 :: c1 :: c2 :: c3 :: c4 :: map2 arest brest f
  | _, _ -> invalid_arg __FUNCTION__
[@@tail_mod_cons]

let rec fold_left_with_offset l accu i f =
  match l with
  | [] -> accu
  | a :: l -> fold_left_with_offset l (f a accu i) (i + 1) f

let rec filter_map xs (f : 'a -> 'b option) =
  match xs with
  | [] -> []
  | y :: ys -> (
      match f y with None -> filter_map ys f | Some z -> z :: filter_map ys f)
[@@tail_mod_cons]

let rec same_length xs ys =
  match (xs, ys) with
  | [], [] -> true
  | _ :: xs, _ :: ys -> same_length xs ys
  | _, _ -> false

let init n f =
  match n with
  | 0 -> []
  | 1 ->
      let a0 = f 0 in
      [ a0 ]
  | 2 ->
      let a0 = f 0 in
      let a1 = f 1 in
      [ a0; a1 ]
  | 3 ->
      let a0 = f 0 in
      let a1 = f 1 in
      let a2 = f 2 in
      [ a0; a1; a2 ]
  | 4 ->
      let a0 = f 0 in
      let a1 = f 1 in
      let a2 = f 2 in
      let a3 = f 3 in
      [ a0; a1; a2; a3 ]
  | 5 ->
      let a0 = f 0 in
      let a1 = f 1 in
      let a2 = f 2 in
      let a3 = f 3 in
      let a4 = f 4 in
      [ a0; a1; a2; a3; a4 ]
  | _ -> Array.to_list (Array.init n f)

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | a0 :: [] -> a0 :: l2
  | [ a0; a1 ] -> a1 :: a0 :: l2
  | a0 :: a1 :: a2 :: rest -> rev_append rest (a2 :: a1 :: a0 :: l2)

let rev l = rev_append l []

let rec split_at_last_aux acc x =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | x :: [] -> (rev acc, x)
  | y0 :: ys -> split_at_last_aux (y0 :: acc) ys

let split_at_last (x : 'a list) =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | a0 :: [] -> ([], a0)
  | [ a0; a1 ] -> ([ a0 ], a1)
  | [ a0; a1; a2 ] -> ([ a0; a1 ], a2)
  | [ a0; a1; a2; a3 ] -> ([ a0; a1; a2 ], a3)
  | [ a0; a1; a2; a3; a4 ] -> ([ a0; a1; a2; a3 ], a4)
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
      let rev, last = split_at_last_aux [] rest in
      (a0 :: a1 :: a2 :: a3 :: a4 :: rev, last)

let filter_mapi xs f =
  let rec aux i xs =
    match xs with
    | [] -> []
    | y :: ys -> (
        match f y i with
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys)
      [@@tail_mod_cons]
  in
  aux 0 xs

let rec filter_map2 xs ys (f : 'a -> 'b -> 'c option) =
  match (xs, ys) with
  | [], [] -> []
  | u :: us, v :: vs -> (
      match f u v with
      | None -> filter_map2 us vs f
      | Some z -> z :: filter_map2 us vs f)
  | _ -> invalid_arg __FUNCTION__
[@@tail_mod_cons]

let rec rev_map_append l1 l2 f =
  match l1 with [] -> l2 | a :: l -> rev_map_append l (f a :: l2) f

let rec flat_map_aux f acc append lx =
  match lx with
  | [] -> rev_append acc append
  | a0 :: rest ->
      let new_acc =
        match f a0 with
        | [] -> acc
        | a0 :: [] -> a0 :: acc
        | [ a0; a1 ] -> a1 :: a0 :: acc
        | a0 :: a1 :: a2 :: rest -> rev_append rest (a2 :: a1 :: a0 :: acc)
      in
      flat_map_aux f new_acc append rest

let concat_map lx f = flat_map_aux f [] [] lx
let flat_map_append lx ~init:append ~f = flat_map_aux f [] append lx

let rec flat_map_auxi f acc append lx index =
  match lx with
  | [] -> rev_append acc append
  | a0 :: rest ->
      let new_acc =
        match f index a0 with
        | [] -> acc
        | a0 :: [] -> a0 :: acc
        | [ a0; a1 ] -> a1 :: a0 :: acc
        | a0 :: a1 :: a2 :: rest -> rev_append rest (a2 :: a1 :: a0 :: acc)
      in
      flat_map_auxi f new_acc append rest (index + 1)

let flat_mapi_append lx ~init:append ~f = flat_map_auxi f [] append lx 0

let rec drop h n =
  if n < 0 then invalid_arg __FUNCTION__
  else if n = 0 then h
  else match h with [] -> invalid_arg __FUNCTION__ | _ :: tl -> drop tl (n - 1)

let rec find_first x p =
  match x with [] -> None | x :: l -> if p x then Some x else find_first l p

let rec find_exn x p =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | x :: l -> if p x then x else find_exn l p

let rec iter l ~f =
  match l with
  | [] -> ()
  | x1 :: [] -> f x1
  | [ x1; x2 ] ->
      f x1;
      f x2
  | [ x1; x2; x3 ] ->
      f x1;
      f x2;
      f x3
  | [ x1; x2; x3; x4 ] ->
      f x1;
      f x2;
      f x3;
      f x4
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      f x1;
      f x2;
      f x3;
      f x4;
      f x5;
      iter tail ~f

let rec iteri_aux l f i =
  match l with
  | [] -> ()
  | x1 :: [] -> f i x1
  | [ x1; x2 ] ->
      f i x1;
      f (i + 1) x2
  | [ x1; x2; x3 ] ->
      f i x1;
      f (i + 1) x2;
      f (i + 2) x3
  | [ x1; x2; x3; x4 ] ->
      f i x1;
      f (i + 1) x2;
      f (i + 2) x3;
      f (i + 3) x4
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      f i x1;
      f (i + 1) x2;
      f (i + 2) x3;
      f (i + 3) x4;
      f (i + 4) x5;
      iteri_aux tail f (i + 5)

let iteri l ~f = iteri_aux l f 0

let rec iter2 l1 l2 f =
  match (l1, l2) with
  | [], [] -> ()
  | a1 :: l1, a2 :: l2 ->
      f a1 a2;
      iter2 l1 l2 f
  | _, _ -> invalid_arg __FUNCTION__

let rec for_all lst p =
  match lst with [] -> true | a :: l -> p a && for_all l p

let rec for_all2_no_exn l1 l2 p =
  match (l1, l2) with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> p a1 a2 && for_all2_no_exn l1 l2 p
  | _, _ -> false

let rec find_opt xs p =
  match xs with
  | [] -> None
  | x :: l -> ( match p x with Some _ as v -> v | None -> find_opt l p)

let rec find_def xs p def =
  match xs with
  | [] -> def
  | x :: l -> ( match p x with Some v -> v | None -> find_def l p def)

let rec split_map2 l r f =
  match (l, r) with
  | [], [] -> ([], [])
  | x1 :: [], y1 :: [] ->
      let a0, b0 = f x1 y1 in
      ([ a0 ], [ b0 ])
  | [ x1; x2 ], [ y1; y2 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      ([ a1; a2 ], [ b1; b2 ])
  | [ x1; x2; x3 ], [ y1; y2; y3 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      ([ a1; a2; a3 ], [ b1; b2; b3 ])
  | [ x1; x2; x3; x4 ], [ y1; y2; y3; y4 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      let a4, b4 = f x4 y4 in
      ([ a1; a2; a3; a4 ], [ b1; b2; b3; b4 ])
  | x1 :: x2 :: x3 :: x4 :: x5 :: tailx, y1 :: y2 :: y3 :: y4 :: y5 :: taily ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      let a4, b4 = f x4 y4 in
      let a5, b5 = f x5 y5 in
      let ass, bss = split_map2 tailx taily f in
      (a1 :: a2 :: a3 :: a4 :: a5 :: ass, b1 :: b2 :: b3 :: b4 :: b5 :: bss)
  | _, _ -> invalid_arg __FUNCTION__

let sort_via_arrayf lst cmp f =
  let arr = Array.of_list lst in
  Array.sort cmp arr;
  Arr.to_list_f arr f

let rec assoc_by_string lst (k : string) def =
  match lst with
  | [] -> ( match def with None -> assert false | Some x -> x)
  | (k1, v1) :: rest -> if k1 = k then v1 else assoc_by_string rest k def

let rec assoc_by_opt lst comp k =
  match lst with
  | [] -> None
  | (k1, v1) :: rest -> if comp k1 k then Some v1 else assoc_by_opt rest comp k

let assoc_str lst str = assoc_by_opt lst String.equal str
let assoc_str_exn lst str = assoc_by_string lst str None

let rec nth_aux l n =
  match l with
  | [] -> None
  | a :: l -> if n = 0 then Some a else nth_aux l (n - 1)

let nth_opt l n = if n < 0 then None else nth_aux l n
let rec exists l p = match l with [] -> false | x :: xs -> p x || exists xs p

let rec exists_fst l p =
  match l with [] -> false | (a, _) :: l -> p a || exists_fst l p

let rec fold_left l accu f =
  match l with [] -> accu | a :: l -> fold_left l (f accu a) f

let rec fold_left2 l1 l2 accu f =
  match (l1, l2) with
  | [], [] -> accu
  | a1 :: l1, a2 :: l2 -> fold_left2 l1 l2 (f a1 a2 accu) f
  | _, _ -> invalid_arg __FUNCTION__

let rec mem_string (xs : string list) (x : string) =
  match xs with [] -> false | a :: l -> a = x || mem_string l x

let filter lst p =
  let rec find ~p accu lst =
    match lst with
    | [] -> rev accu
    | x :: l -> if p x then find (x :: accu) l ~p else find accu l ~p
  in
  find [] lst ~p

let stable_sort l cmp =
  match l with
  | [] -> []
  | x :: [] -> [ x ]
  | [ x1; x2 ] -> if cmp x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ]
  | [ x1; x2; x3 ] ->
      if cmp x1 x2 <= 0 then
        if cmp x2 x3 <= 0 then [ x1; x2; x3 ]
        else if cmp x1 x3 <= 0 then [ x1; x3; x2 ]
        else [ x3; x1; x2 ]
      else if cmp x1 x3 <= 0 then [ x2; x1; x3 ]
      else if cmp x2 x3 <= 0 then [ x2; x3; x1 ]
      else [ x3; x2; x1 ]
  | l ->
      let arr = Array.of_list l in
      Array.stable_sort cmp arr;
      Array.to_list arr

let rec unsafe_take n xs =
  match xs with
  | [] -> []
  | x :: rest -> if n = 1 then [ x ] else x :: unsafe_take (n - 1) rest

let take xs n = if n <= 0 then [] else unsafe_take n xs

let rec concat (xss : 'a list list) =
  (match xss with
   | [] -> []
   | xs :: [] -> xs
   | xs :: yss -> append_concat xs yss
    : 'a list)
[@@tail_mod_cons]

and append_concat xs yss =
  match xs with
  | [] -> concat yss
  | x :: [] -> x :: concat yss
  | [ x1; x2 ] -> x1 :: x2 :: concat yss
  | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: concat yss
  | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: concat yss
  | x1 :: x2 :: x3 :: x4 :: x5 :: rest ->
      x1 :: x2 :: x3 :: x4 :: x5 :: append_concat rest yss
[@@tail_mod_cons]

let rec combine xs ys =
  match (xs, ys) with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine xs ys
  | _, _ -> invalid_arg __FUNCTION__
[@@tail_mod_cons]

let split xs =
  let rec aux accu1 accu2 = function
    | [] -> (List.rev accu1, List.rev accu2)
    | (x, y) :: rest -> aux (x :: accu1) (y :: accu2) rest
  in
  aux [] [] xs
