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


type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type lexbuf = { mutable lex_start_p : position; mutable lex_curr_p : position }

let dummy_pos = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 }

let of_stdlib_position (pos : Stdlib.Lexing.position) =
  ({
     pos_fname = pos.pos_fname;
     pos_lnum = pos.pos_lnum;
     pos_bol = pos.pos_bol;
     pos_cnum = pos.pos_cnum;
   }
    : position)

let from_string (_ : string) =
  { lex_start_p = dummy_pos; lex_curr_p = dummy_pos }
