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
module Menhir_token = Lex_menhir_token
module Token_triple = Lex_token_triple

let can_occur_before_semi (token : Menhir_token.token) =
  match token with
  | UIDENT _ | LIDENT _ | DOT_UIDENT _ | DOT_LIDENT _ | DOT_INT _ | FLOAT _
  | INT _ | BYTE _ | BYTES _ | TRUE | FALSE | STRING _ | MULTILINE_STRING _
  | MULTILINE_INTERP _ | INTERP _ | CHAR _ | RBRACE | RPAREN | RBRACKET
  | UNDERSCORE | BREAK | CONTINUE | RETURN | THROW | QUESTION | EXCLAMATION
  | RANGE_INCLUSIVE | RANGE_EXCLUSIVE | PIPE | ELLIPSIS | POST_LABEL _ ->
      true
  | TYPE | TYPEALIAS | STRUCT | SEMI _ | PLUS | PACKAGE_NAME _ | NEWLINE
  | MUTABLE | MINUS | MATCH | TRY | CATCH | LPAREN | LET | CONST | LBRACKET
  | LBRACE | INFIX1 _ | INFIX4 _ | INFIX3 _ | INFIX2 _ | IMPL | WITH | IMPORT
  | IF | GUARD | WHILE | ASYNC | FN | FAT_ARROW | THIN_ARROW | IN | EQUAL
  | AUGMENTED_ASSIGNMENT _ | EOF | ENUM | ELSE | EXTERN | COMMENT _ | COMMA
  | COLON | BARBAR | BAR | AS | IS | AMPERAMPER | DOTDOT | PUB | PRIV | READONLY
  | TRAIT | TRAITALIAS | DERIVE | COLONCOLON | TEST | LOOP | FOR | AMPER | CARET
  | RAISE | ATTRIBUTE _ ->
      false
[@@inline]

let can_occur_after_semi (token : Menhir_token.token) =
  match token with
  | UIDENT _ | LIDENT _ | FLOAT _ | INT _ | BYTE _ | BYTES _ | TRUE | FALSE
  | STRING _ | MULTILINE_STRING _ | MULTILINE_INTERP _ | INTERP _ | CHAR _
  | LBRACE | LPAREN | LBRACKET | UNDERSCORE | BREAK | CONTINUE | RETURN | THROW
  | RAISE | TYPE | TYPEALIAS | STRUCT | TRAIT | TRAITALIAS | PACKAGE_NAME _
  | MUTABLE | MATCH | TRY | LET | CONST | IMPL | IMPORT | EXTERN | IF | GUARD
  | WHILE | ASYNC | FN | EOF | ENUM | PUB | PRIV | READONLY | TEST | LOOP | FOR
  | ELLIPSIS | POST_LABEL _ | ATTRIBUTE _ ->
      true
  | COMMENT _ | NEWLINE -> true
  | MINUS -> true
  | PLUS | INFIX1 _ | INFIX2 _ | INFIX3 _ | INFIX4 _ | AMPERAMPER | BARBAR
  | CARET | AMPER ->
      true
  | RBRACE -> false
  | DOT_UIDENT _ | DOT_LIDENT _ | DOT_INT _ | COLONCOLON | RPAREN | RBRACKET
  | SEMI _ | FAT_ARROW | THIN_ARROW | IN | PIPE | EQUAL | AUGMENTED_ASSIGNMENT _
  | ELSE | CATCH | COMMA | COLON | BAR | AS | IS | DOTDOT | DERIVE | WITH
  | QUESTION | RANGE_INCLUSIVE | RANGE_EXCLUSIVE | EXCLAMATION ->
      false
[@@inline]

type asi_context = { mutable last_unhandled_newline : int }

let make_asi_context () = { last_unhandled_newline = -1 }

let add_token ctx ~(tokens : Token_triple.t Vec.t)
    (next_token : Menhir_token.token)
    ~(last_unhandled_comment : (_ * int) option ref) =
  match next_token with
  | COMMENT _ -> ()
  | NEWLINE ->
      if ctx.last_unhandled_newline < 0 then
        ctx.last_unhandled_newline <- Vec.length tokens
  | _ when ctx.last_unhandled_newline >= 0 ->
      let rec try_insert top =
        if top >= 0 then
          match Vec.get tokens top with
          | COMMENT _, _, _ -> try_insert (top - 1)
          | last_token, _, endp -> (
              if
                can_occur_before_semi last_token
                && can_occur_after_semi next_token
              then
                match (last_token, next_token) with
                | ( (MULTILINE_STRING _ | MULTILINE_INTERP _),
                    (MULTILINE_STRING _ | MULTILINE_INTERP _) ) ->
                    ()
                | _ -> (
                    Vec.insert tokens (top + 1)
                      (Menhir_token.faked_semi, endp, endp);
                    match !last_unhandled_comment with
                    | Some (c, i) when i >= top + 1 ->
                        last_unhandled_comment := Some (c, i + 1)
                    | _ -> ()))
      in
      try_insert (ctx.last_unhandled_newline - 1);
      ctx.last_unhandled_newline <- -1
  | _ -> ()
