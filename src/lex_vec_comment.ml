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


module Vec = Basic_vec
module Comment = Lex_comment

type t = Comment.with_loc Vec.t

let search ~last ~loc_ (t : Comment.with_loc Vec.t) =
  (let line = Loc.line_number loc_ in
   let rec go l r =
     if l >= r then l
     else
       let m = l + ((r - l) / 2) in
       let locs = Vec.get t m in
       match locs with
       | [] -> assert false
       | (x, _) :: _ ->
           if Loc.line_number x <= line then go l m else go (m + 1) r
   in
   let result =
     let pos = go 0 (Vec.length t - 1) in
     if pos >= Vec.length t then []
     else
       match Vec.get t pos with
       | [] -> assert false
       | (x, _) :: _ as xs ->
           let lx = Loc.line_number x in
           if Loc.line_number_end last < lx && lx <= line then xs else []
   in
   Basic_lst.iter result ~f:(fun (_, (comment : Comment.t)) ->
       comment.consumed_by_docstring := true);
   result
    : Comment.with_loc)
