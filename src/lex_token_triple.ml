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


module Menhir_token = Lex_menhir_token

type t = Menhir_token.token * Lexing.position * Lexing.position

let sexp_of_t ((tok, a, b) : t) =
  (S.List
     [
       Menhir_token.sexp_of_token tok;
       Atom
         (Printf.sprintf "%d:%d-%d:%d" a.pos_lnum
            (a.pos_cnum - a.pos_bol + 1)
            b.pos_lnum
            (b.pos_cnum - b.pos_bol + 1));
     ]
    : S.t)

let null : t = (Menhir_token.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
