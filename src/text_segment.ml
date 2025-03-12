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


type t = {
  text_start_line : int;
  text_name : string;
  text_segment_code : string;
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun {
           text_start_line = text_start_line__002_;
           text_name = text_name__004_;
           text_segment_code = text_segment_code__006_;
         }
     ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__007_ =
           Moon_sexp_conv.sexp_of_string text_segment_code__006_
         in
         (S.List [ S.Atom "text_segment_code"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_string text_name__004_ in
         (S.List [ S.Atom "text_name"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Moon_sexp_conv.sexp_of_int text_start_line__002_ in
         (S.List [ S.Atom "text_start_line"; arg__003_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : t -> S.t)

  let _ = sexp_of_t
end

let of_string ?(name = "") str =
  (let get i = if i >= String.length str then '\000' else str.[i] in
   let rec loop i lnum (bol : int) acc =
     match get i with
     | '\n' -> match_block_line (i + 1) (lnum + 1) (i + 1) acc
     | '\r' -> (
         match get (i + 1) with
         | '\n' -> match_block_line (i + 2) (lnum + 1) (i + 2) acc
         | _ -> loop (i + 1) lnum bol acc)
     | '\000' -> List.rev ((lnum, String.length str) :: acc)
     | _ -> loop (i + 1) lnum bol acc
   and match_block_line i lnum bol acc =
     if
       get i = '/'
       && get (i + 1) = '/'
       && get (i + 2) = '/'
       && get (i + 3) = '|'
     then loop (i + 4) lnum bol ((lnum, bol) :: acc)
     else loop i lnum bol acc
   in
   let points = loop 0 1 0 [ (1, 0) ] in
   let rec slice points acc =
     match points with
     | (line, start_offset) :: ((_, end_offset) :: _ as remain) ->
         let offset = start_offset in
         let length = end_offset - start_offset in
         let buf = String.sub str offset length in
         slice remain
           ({
              text_segment_code = buf;
              text_start_line = line;
              text_name = name;
            }
           :: acc)
     | _ -> List.rev acc
   in
   slice points []
    : t list)
