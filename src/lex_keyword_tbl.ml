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


module KeyTbl = Basic_hash_string
module Menhir_token = Lex_menhir_token
module Hashset_string = Basic_hashset_string

let keyword_tbl =
  KeyTbl.of_list
    [
      ("as", Menhir_token.AS);
      ("else", ELSE);
      ("extern", EXTERN);
      ("fn", FN);
      ("if", IF);
      ("let", LET);
      ("const", CONST);
      ("match", MATCH);
      ("mut", MUTABLE);
      ("type", TYPE);
      ("typealias", TYPEALIAS);
      ("struct", STRUCT);
      ("enum", ENUM);
      ("trait", TRAIT);
      ("traitalias", TRAITALIAS);
      ("derive", DERIVE);
      ("while", WHILE);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("import", IMPORT);
      ("return", RETURN);
      ("throw", THROW);
      ("raise", RAISE);
      ("try", TRY);
      ("catch", CATCH);
      ("pub", PUB);
      ("priv", PRIV);
      ("readonly", READONLY);
      ("true", TRUE);
      ("false", FALSE);
      ("_", UNDERSCORE);
      ("test", TEST);
      ("loop", LOOP);
      ("for", FOR);
      ("in", IN);
      ("impl", IMPL);
      ("with", WITH);
      ("guard", GUARD);
      ("async", ASYNC);
      ("is", IS);
    ]

let reserved =
  Hashset_string.of_array
    [|
      "module";
      "move";
      "ref";
      "static";
      "super";
      "unsafe";
      "use";
      "where";
      "await";
      "dyn";
      "abstract";
      "do";
      "final";
      "macro";
      "override";
      "typeof";
      "virtual";
      "yield";
      "local";
      "method";
      "alias";
      "assert";
    |]

let find_opt lexeme = KeyTbl.find_opt keyword_tbl lexeme
let is_reserved lexeme = Hashset_string.mem reserved lexeme
