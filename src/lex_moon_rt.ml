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

type lexbuf = {
  buf : int array;
  len : int;
  mutable pos : int;
  mutable start_pos : int;
  mutable marked_pos : int;
  mutable marked_val : int;
}

let empty_lexbuf ~buf ~len =
  { buf; len; pos = 0; start_pos = 0; marked_pos = 0; marked_val = 0 }
[@@inline]

let from_int_vec (a : Basic_vec_int.t) =
  let len = Basic_vec_int.length a in
  empty_lexbuf ~buf:a.arr ~len

let __private__next_int lexbuf =
  (if lexbuf.pos = lexbuf.len then -1
   else
     let ret = lexbuf.buf.!(lexbuf.pos) in
     lexbuf.pos <- lexbuf.pos + 1;
     ret
    : int)

let peek_next_int lexbuf =
  (if lexbuf.pos = lexbuf.len then -1 else lexbuf.buf.!(lexbuf.pos) : int)

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  mark lexbuf (-1)

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.marked_val

let backoff lexbuf i = lexbuf.pos <- lexbuf.pos - i
let set_start lexbuf i = lexbuf.start_pos <- i
let lexeme_start lexbuf = lexbuf.start_pos
let lexeme_end lexbuf = lexbuf.pos
let current_code_point lexbuf = lexbuf.buf.(lexbuf.pos - 1)
