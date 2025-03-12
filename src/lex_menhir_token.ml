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


module Literal = Lex_literal
module Comment = Lex_comment

type token =
  | WITH
  | WHILE
  | UNDERSCORE
  | UIDENT of string
  | TYPEALIAS
  | TYPE
  | TRY
  | TRUE
  | TRAITALIAS
  | TRAIT
  | THROW
  | THIN_ARROW
  | TEST
  | STRUCT
  | STRING of Lex_literal.string_literal
  | SEMI of bool
  | RPAREN
  | RETURN
  | READONLY
  | RBRACKET
  | RBRACE
  | RANGE_INCLUSIVE
  | RANGE_EXCLUSIVE
  | RAISE
  | QUESTION
  | PUB
  | PRIV
  | POST_LABEL of string
  | PLUS
  | PIPE
  | PACKAGE_NAME of string
  | NEWLINE
  | MUTABLE
  | MULTILINE_STRING of string
  | MULTILINE_INTERP of Lex_literal.interp_literal
  | MINUS
  | MATCH
  | LPAREN
  | LOOP
  | LIDENT of string
  | LET
  | LBRACKET
  | LBRACE
  | IS
  | INTERP of Lex_literal.interp_literal
  | INT of string
  | INFIX4 of string
  | INFIX3 of string
  | INFIX2 of string
  | INFIX1 of string
  | IN
  | IMPORT
  | IMPL
  | IF
  | GUARD
  | FOR
  | FN
  | FLOAT of string
  | FAT_ARROW
  | FALSE
  | EXTERN
  | EXCLAMATION
  | EQUAL
  | EOF
  | ENUM
  | ELSE
  | ELLIPSIS
  | DOT_UIDENT of string
  | DOT_LIDENT of string
  | DOT_INT of int
  | DOTDOT
  | DERIVE
  | CONTINUE
  | CONST
  | COMMENT of Comment.t
  | COMMA
  | COLONCOLON
  | COLON
  | CHAR of Lex_literal.char_literal
  | CATCH
  | CARET
  | BYTES of Lex_literal.bytes_literal
  | BYTE of Lex_literal.byte_literal
  | BREAK
  | BARBAR
  | BAR
  | AUGMENTED_ASSIGNMENT of string
  | ATTRIBUTE of string
  | ASYNC
  | AS
  | AMPERAMPER
  | AMPER

include struct
  let _ = fun (_ : token) -> ()

  let sexp_of_token =
    (function
     | WITH -> S.Atom "WITH"
     | WHILE -> S.Atom "WHILE"
     | UNDERSCORE -> S.Atom "UNDERSCORE"
     | UIDENT arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_string arg0__001_ in
         S.List [ S.Atom "UIDENT"; res0__002_ ]
     | TYPEALIAS -> S.Atom "TYPEALIAS"
     | TYPE -> S.Atom "TYPE"
     | TRY -> S.Atom "TRY"
     | TRUE -> S.Atom "TRUE"
     | TRAITALIAS -> S.Atom "TRAITALIAS"
     | TRAIT -> S.Atom "TRAIT"
     | THROW -> S.Atom "THROW"
     | THIN_ARROW -> S.Atom "THIN_ARROW"
     | TEST -> S.Atom "TEST"
     | STRUCT -> S.Atom "STRUCT"
     | STRING arg0__003_ ->
         let res0__004_ = Lex_literal.sexp_of_string_literal arg0__003_ in
         S.List [ S.Atom "STRING"; res0__004_ ]
     | SEMI arg0__005_ ->
         let res0__006_ = Moon_sexp_conv.sexp_of_bool arg0__005_ in
         S.List [ S.Atom "SEMI"; res0__006_ ]
     | RPAREN -> S.Atom "RPAREN"
     | RETURN -> S.Atom "RETURN"
     | READONLY -> S.Atom "READONLY"
     | RBRACKET -> S.Atom "RBRACKET"
     | RBRACE -> S.Atom "RBRACE"
     | RANGE_INCLUSIVE -> S.Atom "RANGE_INCLUSIVE"
     | RANGE_EXCLUSIVE -> S.Atom "RANGE_EXCLUSIVE"
     | RAISE -> S.Atom "RAISE"
     | QUESTION -> S.Atom "QUESTION"
     | PUB -> S.Atom "PUB"
     | PRIV -> S.Atom "PRIV"
     | POST_LABEL arg0__007_ ->
         let res0__008_ = Moon_sexp_conv.sexp_of_string arg0__007_ in
         S.List [ S.Atom "POST_LABEL"; res0__008_ ]
     | PLUS -> S.Atom "PLUS"
     | PIPE -> S.Atom "PIPE"
     | PACKAGE_NAME arg0__009_ ->
         let res0__010_ = Moon_sexp_conv.sexp_of_string arg0__009_ in
         S.List [ S.Atom "PACKAGE_NAME"; res0__010_ ]
     | NEWLINE -> S.Atom "NEWLINE"
     | MUTABLE -> S.Atom "MUTABLE"
     | MULTILINE_STRING arg0__011_ ->
         let res0__012_ = Moon_sexp_conv.sexp_of_string arg0__011_ in
         S.List [ S.Atom "MULTILINE_STRING"; res0__012_ ]
     | MULTILINE_INTERP arg0__013_ ->
         let res0__014_ = Lex_literal.sexp_of_interp_literal arg0__013_ in
         S.List [ S.Atom "MULTILINE_INTERP"; res0__014_ ]
     | MINUS -> S.Atom "MINUS"
     | MATCH -> S.Atom "MATCH"
     | LPAREN -> S.Atom "LPAREN"
     | LOOP -> S.Atom "LOOP"
     | LIDENT arg0__015_ ->
         let res0__016_ = Moon_sexp_conv.sexp_of_string arg0__015_ in
         S.List [ S.Atom "LIDENT"; res0__016_ ]
     | LET -> S.Atom "LET"
     | LBRACKET -> S.Atom "LBRACKET"
     | LBRACE -> S.Atom "LBRACE"
     | IS -> S.Atom "IS"
     | INTERP arg0__017_ ->
         let res0__018_ = Lex_literal.sexp_of_interp_literal arg0__017_ in
         S.List [ S.Atom "INTERP"; res0__018_ ]
     | INT arg0__019_ ->
         let res0__020_ = Moon_sexp_conv.sexp_of_string arg0__019_ in
         S.List [ S.Atom "INT"; res0__020_ ]
     | INFIX4 arg0__021_ ->
         let res0__022_ = Moon_sexp_conv.sexp_of_string arg0__021_ in
         S.List [ S.Atom "INFIX4"; res0__022_ ]
     | INFIX3 arg0__023_ ->
         let res0__024_ = Moon_sexp_conv.sexp_of_string arg0__023_ in
         S.List [ S.Atom "INFIX3"; res0__024_ ]
     | INFIX2 arg0__025_ ->
         let res0__026_ = Moon_sexp_conv.sexp_of_string arg0__025_ in
         S.List [ S.Atom "INFIX2"; res0__026_ ]
     | INFIX1 arg0__027_ ->
         let res0__028_ = Moon_sexp_conv.sexp_of_string arg0__027_ in
         S.List [ S.Atom "INFIX1"; res0__028_ ]
     | IN -> S.Atom "IN"
     | IMPORT -> S.Atom "IMPORT"
     | IMPL -> S.Atom "IMPL"
     | IF -> S.Atom "IF"
     | GUARD -> S.Atom "GUARD"
     | FOR -> S.Atom "FOR"
     | FN -> S.Atom "FN"
     | FLOAT arg0__029_ ->
         let res0__030_ = Moon_sexp_conv.sexp_of_string arg0__029_ in
         S.List [ S.Atom "FLOAT"; res0__030_ ]
     | FAT_ARROW -> S.Atom "FAT_ARROW"
     | FALSE -> S.Atom "FALSE"
     | EXTERN -> S.Atom "EXTERN"
     | EXCLAMATION -> S.Atom "EXCLAMATION"
     | EQUAL -> S.Atom "EQUAL"
     | EOF -> S.Atom "EOF"
     | ENUM -> S.Atom "ENUM"
     | ELSE -> S.Atom "ELSE"
     | ELLIPSIS -> S.Atom "ELLIPSIS"
     | DOT_UIDENT arg0__031_ ->
         let res0__032_ = Moon_sexp_conv.sexp_of_string arg0__031_ in
         S.List [ S.Atom "DOT_UIDENT"; res0__032_ ]
     | DOT_LIDENT arg0__033_ ->
         let res0__034_ = Moon_sexp_conv.sexp_of_string arg0__033_ in
         S.List [ S.Atom "DOT_LIDENT"; res0__034_ ]
     | DOT_INT arg0__035_ ->
         let res0__036_ = Moon_sexp_conv.sexp_of_int arg0__035_ in
         S.List [ S.Atom "DOT_INT"; res0__036_ ]
     | DOTDOT -> S.Atom "DOTDOT"
     | DERIVE -> S.Atom "DERIVE"
     | CONTINUE -> S.Atom "CONTINUE"
     | CONST -> S.Atom "CONST"
     | COMMENT arg0__037_ ->
         let res0__038_ = Comment.sexp_of_t arg0__037_ in
         S.List [ S.Atom "COMMENT"; res0__038_ ]
     | COMMA -> S.Atom "COMMA"
     | COLONCOLON -> S.Atom "COLONCOLON"
     | COLON -> S.Atom "COLON"
     | CHAR arg0__039_ ->
         let res0__040_ = Lex_literal.sexp_of_char_literal arg0__039_ in
         S.List [ S.Atom "CHAR"; res0__040_ ]
     | CATCH -> S.Atom "CATCH"
     | CARET -> S.Atom "CARET"
     | BYTES arg0__041_ ->
         let res0__042_ = Lex_literal.sexp_of_bytes_literal arg0__041_ in
         S.List [ S.Atom "BYTES"; res0__042_ ]
     | BYTE arg0__043_ ->
         let res0__044_ = Lex_literal.sexp_of_byte_literal arg0__043_ in
         S.List [ S.Atom "BYTE"; res0__044_ ]
     | BREAK -> S.Atom "BREAK"
     | BARBAR -> S.Atom "BARBAR"
     | BAR -> S.Atom "BAR"
     | AUGMENTED_ASSIGNMENT arg0__045_ ->
         let res0__046_ = Moon_sexp_conv.sexp_of_string arg0__045_ in
         S.List [ S.Atom "AUGMENTED_ASSIGNMENT"; res0__046_ ]
     | ATTRIBUTE arg0__047_ ->
         let res0__048_ = Moon_sexp_conv.sexp_of_string arg0__047_ in
         S.List [ S.Atom "ATTRIBUTE"; res0__048_ ]
     | ASYNC -> S.Atom "ASYNC"
     | AS -> S.Atom "AS"
     | AMPERAMPER -> S.Atom "AMPERAMPER"
     | AMPER -> S.Atom "AMPER"
      : token -> S.t)

  let _ = sexp_of_token
end

type 'a terminal =
  | T_error : unit terminal
  | T_WITH : unit terminal
  | T_WHILE : unit terminal
  | T_UNDERSCORE : unit terminal
  | T_UIDENT : string terminal
  | T_TYPEALIAS : unit terminal
  | T_TYPE : unit terminal
  | T_TRY : unit terminal
  | T_TRUE : unit terminal
  | T_TRAITALIAS : unit terminal
  | T_TRAIT : unit terminal
  | T_THROW : unit terminal
  | T_THIN_ARROW : unit terminal
  | T_TEST : unit terminal
  | T_STRUCT : unit terminal
  | T_STRING : Lex_literal.string_literal terminal
  | T_SEMI : bool terminal
  | T_RPAREN : unit terminal
  | T_RETURN : unit terminal
  | T_READONLY : unit terminal
  | T_RBRACKET : unit terminal
  | T_RBRACE : unit terminal
  | T_RANGE_INCLUSIVE : unit terminal
  | T_RANGE_EXCLUSIVE : unit terminal
  | T_RAISE : unit terminal
  | T_QUESTION : unit terminal
  | T_PUB : unit terminal
  | T_PRIV : unit terminal
  | T_POST_LABEL : string terminal
  | T_PLUS : unit terminal
  | T_PIPE : unit terminal
  | T_PACKAGE_NAME : string terminal
  | T_NEWLINE : unit terminal
  | T_MUTABLE : unit terminal
  | T_MULTILINE_STRING : string terminal
  | T_MULTILINE_INTERP : Lex_literal.interp_literal terminal
  | T_MINUS : unit terminal
  | T_MATCH : unit terminal
  | T_LPAREN : unit terminal
  | T_LOOP : unit terminal
  | T_LIDENT : string terminal
  | T_LET : unit terminal
  | T_LBRACKET : unit terminal
  | T_LBRACE : unit terminal
  | T_IS : unit terminal
  | T_INTERP : Lex_literal.interp_literal terminal
  | T_INT : string terminal
  | T_INFIX4 : string terminal
  | T_INFIX3 : string terminal
  | T_INFIX2 : string terminal
  | T_INFIX1 : string terminal
  | T_IN : unit terminal
  | T_IMPORT : unit terminal
  | T_IMPL : unit terminal
  | T_IF : unit terminal
  | T_GUARD : unit terminal
  | T_FOR : unit terminal
  | T_FN : unit terminal
  | T_FLOAT : string terminal
  | T_FAT_ARROW : unit terminal
  | T_FALSE : unit terminal
  | T_EXTERN : unit terminal
  | T_EXCLAMATION : unit terminal
  | T_EQUAL : unit terminal
  | T_EOF : unit terminal
  | T_ENUM : unit terminal
  | T_ELSE : unit terminal
  | T_ELLIPSIS : unit terminal
  | T_DOT_UIDENT : string terminal
  | T_DOT_LIDENT : string terminal
  | T_DOT_INT : int terminal
  | T_DOTDOT : unit terminal
  | T_DERIVE : unit terminal
  | T_CONTINUE : unit terminal
  | T_CONST : unit terminal
  | T_COMMENT : Comment.t terminal
  | T_COMMA : unit terminal
  | T_COLONCOLON : unit terminal
  | T_COLON : unit terminal
  | T_CHAR : Lex_literal.char_literal terminal
  | T_CATCH : unit terminal
  | T_CARET : unit terminal
  | T_BYTES : Lex_literal.bytes_literal terminal
  | T_BYTE : Lex_literal.byte_literal terminal
  | T_BREAK : unit terminal
  | T_BARBAR : unit terminal
  | T_BAR : unit terminal
  | T_AUGMENTED_ASSIGNMENT : string terminal
  | T_ATTRIBUTE : string terminal
  | T_ASYNC : unit terminal
  | T_AS : unit terminal
  | T_AMPERAMPER : unit terminal
  | T_AMPER : unit terminal

include struct
  let _ = fun (_ : 'a terminal) -> ()

  let sexp_of_terminal : 'a. ('a -> S.t) -> 'a terminal -> S.t =
   fun (type a__050_) ->
    (fun _of_a__049_ -> function
       | T_error -> S.Atom "T_error"
       | T_WITH -> S.Atom "T_WITH"
       | T_WHILE -> S.Atom "T_WHILE"
       | T_UNDERSCORE -> S.Atom "T_UNDERSCORE"
       | T_UIDENT -> S.Atom "T_UIDENT"
       | T_TYPEALIAS -> S.Atom "T_TYPEALIAS"
       | T_TYPE -> S.Atom "T_TYPE"
       | T_TRY -> S.Atom "T_TRY"
       | T_TRUE -> S.Atom "T_TRUE"
       | T_TRAITALIAS -> S.Atom "T_TRAITALIAS"
       | T_TRAIT -> S.Atom "T_TRAIT"
       | T_THROW -> S.Atom "T_THROW"
       | T_THIN_ARROW -> S.Atom "T_THIN_ARROW"
       | T_TEST -> S.Atom "T_TEST"
       | T_STRUCT -> S.Atom "T_STRUCT"
       | T_STRING -> S.Atom "T_STRING"
       | T_SEMI -> S.Atom "T_SEMI"
       | T_RPAREN -> S.Atom "T_RPAREN"
       | T_RETURN -> S.Atom "T_RETURN"
       | T_READONLY -> S.Atom "T_READONLY"
       | T_RBRACKET -> S.Atom "T_RBRACKET"
       | T_RBRACE -> S.Atom "T_RBRACE"
       | T_RANGE_INCLUSIVE -> S.Atom "T_RANGE_INCLUSIVE"
       | T_RANGE_EXCLUSIVE -> S.Atom "T_RANGE_EXCLUSIVE"
       | T_RAISE -> S.Atom "T_RAISE"
       | T_QUESTION -> S.Atom "T_QUESTION"
       | T_PUB -> S.Atom "T_PUB"
       | T_PRIV -> S.Atom "T_PRIV"
       | T_POST_LABEL -> S.Atom "T_POST_LABEL"
       | T_PLUS -> S.Atom "T_PLUS"
       | T_PIPE -> S.Atom "T_PIPE"
       | T_PACKAGE_NAME -> S.Atom "T_PACKAGE_NAME"
       | T_NEWLINE -> S.Atom "T_NEWLINE"
       | T_MUTABLE -> S.Atom "T_MUTABLE"
       | T_MULTILINE_STRING -> S.Atom "T_MULTILINE_STRING"
       | T_MULTILINE_INTERP -> S.Atom "T_MULTILINE_INTERP"
       | T_MINUS -> S.Atom "T_MINUS"
       | T_MATCH -> S.Atom "T_MATCH"
       | T_LPAREN -> S.Atom "T_LPAREN"
       | T_LOOP -> S.Atom "T_LOOP"
       | T_LIDENT -> S.Atom "T_LIDENT"
       | T_LET -> S.Atom "T_LET"
       | T_LBRACKET -> S.Atom "T_LBRACKET"
       | T_LBRACE -> S.Atom "T_LBRACE"
       | T_IS -> S.Atom "T_IS"
       | T_INTERP -> S.Atom "T_INTERP"
       | T_INT -> S.Atom "T_INT"
       | T_INFIX4 -> S.Atom "T_INFIX4"
       | T_INFIX3 -> S.Atom "T_INFIX3"
       | T_INFIX2 -> S.Atom "T_INFIX2"
       | T_INFIX1 -> S.Atom "T_INFIX1"
       | T_IN -> S.Atom "T_IN"
       | T_IMPORT -> S.Atom "T_IMPORT"
       | T_IMPL -> S.Atom "T_IMPL"
       | T_IF -> S.Atom "T_IF"
       | T_GUARD -> S.Atom "T_GUARD"
       | T_FOR -> S.Atom "T_FOR"
       | T_FN -> S.Atom "T_FN"
       | T_FLOAT -> S.Atom "T_FLOAT"
       | T_FAT_ARROW -> S.Atom "T_FAT_ARROW"
       | T_FALSE -> S.Atom "T_FALSE"
       | T_EXTERN -> S.Atom "T_EXTERN"
       | T_EXCLAMATION -> S.Atom "T_EXCLAMATION"
       | T_EQUAL -> S.Atom "T_EQUAL"
       | T_EOF -> S.Atom "T_EOF"
       | T_ENUM -> S.Atom "T_ENUM"
       | T_ELSE -> S.Atom "T_ELSE"
       | T_ELLIPSIS -> S.Atom "T_ELLIPSIS"
       | T_DOT_UIDENT -> S.Atom "T_DOT_UIDENT"
       | T_DOT_LIDENT -> S.Atom "T_DOT_LIDENT"
       | T_DOT_INT -> S.Atom "T_DOT_INT"
       | T_DOTDOT -> S.Atom "T_DOTDOT"
       | T_DERIVE -> S.Atom "T_DERIVE"
       | T_CONTINUE -> S.Atom "T_CONTINUE"
       | T_CONST -> S.Atom "T_CONST"
       | T_COMMENT -> S.Atom "T_COMMENT"
       | T_COMMA -> S.Atom "T_COMMA"
       | T_COLONCOLON -> S.Atom "T_COLONCOLON"
       | T_COLON -> S.Atom "T_COLON"
       | T_CHAR -> S.Atom "T_CHAR"
       | T_CATCH -> S.Atom "T_CATCH"
       | T_CARET -> S.Atom "T_CARET"
       | T_BYTES -> S.Atom "T_BYTES"
       | T_BYTE -> S.Atom "T_BYTE"
       | T_BREAK -> S.Atom "T_BREAK"
       | T_BARBAR -> S.Atom "T_BARBAR"
       | T_BAR -> S.Atom "T_BAR"
       | T_AUGMENTED_ASSIGNMENT -> S.Atom "T_AUGMENTED_ASSIGNMENT"
       | T_ATTRIBUTE -> S.Atom "T_ATTRIBUTE"
       | T_ASYNC -> S.Atom "T_ASYNC"
       | T_AS -> S.Atom "T_AS"
       | T_AMPERAMPER -> S.Atom "T_AMPERAMPER"
       | T_AMPER -> S.Atom "T_AMPER"
      : (a__050_ -> S.t) -> a__050_ terminal -> S.t)

  let _ = sexp_of_terminal
end

type token_kind = Token_kind : _ terminal -> token_kind [@@unboxed]

let kind_of_token = function
  | WITH -> Token_kind T_WITH
  | WHILE -> Token_kind T_WHILE
  | UNDERSCORE -> Token_kind T_UNDERSCORE
  | UIDENT _ -> Token_kind T_UIDENT
  | TYPEALIAS -> Token_kind T_TYPEALIAS
  | TYPE -> Token_kind T_TYPE
  | TRY -> Token_kind T_TRY
  | TRUE -> Token_kind T_TRUE
  | TRAITALIAS -> Token_kind T_TRAITALIAS
  | TRAIT -> Token_kind T_TRAIT
  | THROW -> Token_kind T_THROW
  | THIN_ARROW -> Token_kind T_THIN_ARROW
  | TEST -> Token_kind T_TEST
  | STRUCT -> Token_kind T_STRUCT
  | STRING _ -> Token_kind T_STRING
  | SEMI _ -> Token_kind T_SEMI
  | RPAREN -> Token_kind T_RPAREN
  | RETURN -> Token_kind T_RETURN
  | READONLY -> Token_kind T_READONLY
  | RBRACKET -> Token_kind T_RBRACKET
  | RBRACE -> Token_kind T_RBRACE
  | RANGE_INCLUSIVE -> Token_kind T_RANGE_INCLUSIVE
  | RANGE_EXCLUSIVE -> Token_kind T_RANGE_EXCLUSIVE
  | RAISE -> Token_kind T_RAISE
  | QUESTION -> Token_kind T_QUESTION
  | PUB -> Token_kind T_PUB
  | PRIV -> Token_kind T_PRIV
  | POST_LABEL _ -> Token_kind T_POST_LABEL
  | PLUS -> Token_kind T_PLUS
  | PIPE -> Token_kind T_PIPE
  | PACKAGE_NAME _ -> Token_kind T_PACKAGE_NAME
  | NEWLINE -> Token_kind T_NEWLINE
  | MUTABLE -> Token_kind T_MUTABLE
  | MULTILINE_STRING _ -> Token_kind T_MULTILINE_STRING
  | MULTILINE_INTERP _ -> Token_kind T_MULTILINE_INTERP
  | MINUS -> Token_kind T_MINUS
  | MATCH -> Token_kind T_MATCH
  | LPAREN -> Token_kind T_LPAREN
  | LOOP -> Token_kind T_LOOP
  | LIDENT _ -> Token_kind T_LIDENT
  | LET -> Token_kind T_LET
  | LBRACKET -> Token_kind T_LBRACKET
  | LBRACE -> Token_kind T_LBRACE
  | IS -> Token_kind T_IS
  | INTERP _ -> Token_kind T_INTERP
  | INT _ -> Token_kind T_INT
  | INFIX4 _ -> Token_kind T_INFIX4
  | INFIX3 _ -> Token_kind T_INFIX3
  | INFIX2 _ -> Token_kind T_INFIX2
  | INFIX1 _ -> Token_kind T_INFIX1
  | IN -> Token_kind T_IN
  | IMPORT -> Token_kind T_IMPORT
  | IMPL -> Token_kind T_IMPL
  | IF -> Token_kind T_IF
  | GUARD -> Token_kind T_GUARD
  | FOR -> Token_kind T_FOR
  | FN -> Token_kind T_FN
  | FLOAT _ -> Token_kind T_FLOAT
  | FAT_ARROW -> Token_kind T_FAT_ARROW
  | FALSE -> Token_kind T_FALSE
  | EXTERN -> Token_kind T_EXTERN
  | EXCLAMATION -> Token_kind T_EXCLAMATION
  | EQUAL -> Token_kind T_EQUAL
  | EOF -> Token_kind T_EOF
  | ENUM -> Token_kind T_ENUM
  | ELSE -> Token_kind T_ELSE
  | ELLIPSIS -> Token_kind T_ELLIPSIS
  | DOT_UIDENT _ -> Token_kind T_DOT_UIDENT
  | DOT_LIDENT _ -> Token_kind T_DOT_LIDENT
  | DOT_INT _ -> Token_kind T_DOT_INT
  | DOTDOT -> Token_kind T_DOTDOT
  | DERIVE -> Token_kind T_DERIVE
  | CONTINUE -> Token_kind T_CONTINUE
  | CONST -> Token_kind T_CONST
  | COMMENT _ -> Token_kind T_COMMENT
  | COMMA -> Token_kind T_COMMA
  | COLONCOLON -> Token_kind T_COLONCOLON
  | COLON -> Token_kind T_COLON
  | CHAR _ -> Token_kind T_CHAR
  | CATCH -> Token_kind T_CATCH
  | CARET -> Token_kind T_CARET
  | BYTES _ -> Token_kind T_BYTES
  | BYTE _ -> Token_kind T_BYTE
  | BREAK -> Token_kind T_BREAK
  | BARBAR -> Token_kind T_BARBAR
  | BAR -> Token_kind T_BAR
  | AUGMENTED_ASSIGNMENT _ -> Token_kind T_AUGMENTED_ASSIGNMENT
  | ATTRIBUTE _ -> Token_kind T_ATTRIBUTE
  | ASYNC -> Token_kind T_ASYNC
  | AS -> Token_kind T_AS
  | AMPERAMPER -> Token_kind T_AMPERAMPER
  | AMPER -> Token_kind T_AMPER

let real_semi = SEMI true
let faked_semi = SEMI false
