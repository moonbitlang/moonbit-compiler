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


type directive = (string * string) list

let parse_directive segment =
  (let parse_entry start s =
     Basic_strutil.split_on_first ':'
       (String.sub s start (String.length s - start))
   in
   let rec go acc =
     match Parsing_segment.peek ~comment:true segment with
     | COMMENT s, _, _ when String.starts_with s.content ~prefix:"//!" ->
         Parsing_segment.skip ~comment:true segment;
         go
           (let k, v = parse_entry 3 s.content in
            (k, v) :: acc)
     | _ -> acc
   in
   List.rev (go [])
    : directive)

let parse (segment : Parsing_segment.t) = (parse_directive segment : directive)
