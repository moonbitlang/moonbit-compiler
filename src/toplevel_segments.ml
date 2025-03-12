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
module Vec_token = Lex_vec_token

type t = Parsing_segment.t

let toplevel_segments (tokens : Vec_token.t) =
  (let start = 0 in
   let rec get_slice_points index (topmost_related_token : int option) acc =
     let tok, loca, _ = Vec.get tokens index in
     let left_aligned = loca.pos_cnum = loca.pos_bol in
     match tok with
     | PUB | PRIV | TYPE | STRUCT | ENUM | ASYNC | FN | LET | CONST | TRAIT
     | TRAITALIAS | TEST | IMPL | EXTERN | TYPEALIAS
       when left_aligned ->
         let point =
           match topmost_related_token with
           | Some top_index -> top_index
           | None -> index
         in
         get_slice_points (index + 1) None (point :: acc)
     | EOF -> List.rev (index :: acc)
     | ATTRIBUTE _ ->
         let topmost_related_token =
           match topmost_related_token with
           | None -> Some index
           | _ -> topmost_related_token
         in
         let next =
           if index + 2 < Vec.length tokens then index + 2 else index + 1
         in
         get_slice_points next topmost_related_token acc
     | COMMENT _ ->
         let topmost_related_token =
           match topmost_related_token with
           | None when left_aligned -> Some index
           | Some _ when not left_aligned -> None
           | _ -> topmost_related_token
         in
         let next =
           if index + 2 < Vec.length tokens then index + 2 else index + 1
         in
         get_slice_points next topmost_related_token acc
     | _ -> get_slice_points (index + 1) None acc
   in
   let rec slice ps =
     match ps with
     | l :: (r :: _ as remain) ->
         if r - l > 0 then
           let seg = Parsing_segment.from ~start:l ~len:(r - l) tokens in
           seg :: slice remain
         else slice remain
     | _ -> []
       [@@tail_mod_cons]
   in
   slice (get_slice_points start None [ start ])
    : t list)
