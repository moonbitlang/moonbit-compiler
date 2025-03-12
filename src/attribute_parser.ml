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


module MenhirBasics = struct
  exception Error

  let _eRR _s = raise Error

  type token = Lex_menhir_token.token
end

include MenhirBasics

type ('s, 'r) _menhir_state =
  | MenhirState00 : ('s, _menhir_box_attribute) _menhir_state
  | MenhirState02 :
      ( ('s, _menhir_box_attribute) _menhir_cell1_LIDENT,
        _menhir_box_attribute )
      _menhir_state
  | MenhirState03 :
      ( ('s, _menhir_box_attribute) _menhir_cell1_LPAREN,
        _menhir_box_attribute )
      _menhir_state
  | MenhirState05 :
      ( ('s, _menhir_box_attribute) _menhir_cell1_LIDENT,
        _menhir_box_attribute )
      _menhir_state
  | MenhirState06 :
      ( ( ('s, _menhir_box_attribute) _menhir_cell1_LIDENT,
          _menhir_box_attribute )
        _menhir_cell1_EQUAL,
        _menhir_box_attribute )
      _menhir_state
  | MenhirState08 :
      ( ( ('s, _menhir_box_attribute) _menhir_cell1_LIDENT,
          _menhir_box_attribute )
        _menhir_cell1_DOT_LIDENT,
        _menhir_box_attribute )
      _menhir_state
  | MenhirState12 :
      ( ('s, _menhir_box_attribute) _menhir_cell1_property,
        _menhir_box_attribute )
      _menhir_state

and ('s, 'r) _menhir_cell1_property =
  | MenhirCell1_property of 's * ('s, 'r) _menhir_state * Attribute.attr_prop

and ('s, 'r) _menhir_cell1_DOT_LIDENT =
  | MenhirCell1_DOT_LIDENT of 's * ('s, 'r) _menhir_state * string

and ('s, 'r) _menhir_cell1_EQUAL =
  | MenhirCell1_EQUAL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LIDENT =
  | MenhirCell1_LIDENT of 's * ('s, 'r) _menhir_state * string

and ('s, 'r) _menhir_cell1_LPAREN =
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and _menhir_box_attribute = MenhirBox_attribute of Attribute.attr_expr
[@@unboxed]

let _menhir_action_01 _1 = (_1 : Attribute.attr_expr)

let _menhir_action_02 name =
  (Attribute.Ident { qual = None; name } : Attribute.attr_expr)

let _menhir_action_03 name qual =
  (Attribute.Ident { qual = Some qual; name } : Attribute.attr_expr)

let _menhir_action_04 name props =
  (Apply ({ qual = None; name }, props) : Attribute.attr_expr)

let _menhir_action_05 name props qual =
  (Apply ({ qual = Some qual; name }, props) : Attribute.attr_expr)

let _menhir_action_06 _1 = (String _1 : Attribute.attr_expr)
let _menhir_action_07 _1 = ([ _1 ] : Attribute.attr_prop list)
let _menhir_action_08 x xs = (x :: xs : Attribute.attr_prop list)
let _menhir_action_09 () = ([] : Attribute.attr_prop list)
let _menhir_action_10 _2 = (_2 : Attribute.attr_prop list)

let _menhir_action_11 _3 name =
  (Attribute.Labeled (name, _3) : Attribute.attr_prop)

let _menhir_action_12 _1 = (Attribute.Expr _1 : Attribute.attr_prop)

let _menhir_print_token : token -> string =
 fun _tok ->
  match _tok with
  | Lex_menhir_token.AMPER -> "AMPER"
  | Lex_menhir_token.AMPERAMPER -> "AMPERAMPER"
  | Lex_menhir_token.AS -> "AS"
  | Lex_menhir_token.ASYNC -> "ASYNC"
  | Lex_menhir_token.ATTRIBUTE _ -> "ATTRIBUTE"
  | Lex_menhir_token.AUGMENTED_ASSIGNMENT _ -> "AUGMENTED_ASSIGNMENT"
  | Lex_menhir_token.BAR -> "BAR"
  | Lex_menhir_token.BARBAR -> "BARBAR"
  | Lex_menhir_token.BREAK -> "BREAK"
  | Lex_menhir_token.BYTE _ -> "BYTE"
  | Lex_menhir_token.BYTES _ -> "BYTES"
  | Lex_menhir_token.CARET -> "CARET"
  | Lex_menhir_token.CATCH -> "CATCH"
  | Lex_menhir_token.CHAR _ -> "CHAR"
  | Lex_menhir_token.COLON -> "COLON"
  | Lex_menhir_token.COLONCOLON -> "COLONCOLON"
  | Lex_menhir_token.COMMA -> "COMMA"
  | Lex_menhir_token.COMMENT _ -> "COMMENT"
  | Lex_menhir_token.CONST -> "CONST"
  | Lex_menhir_token.CONTINUE -> "CONTINUE"
  | Lex_menhir_token.DERIVE -> "DERIVE"
  | Lex_menhir_token.DOTDOT -> "DOTDOT"
  | Lex_menhir_token.DOT_INT _ -> "DOT_INT"
  | Lex_menhir_token.DOT_LIDENT _ -> "DOT_LIDENT"
  | Lex_menhir_token.DOT_UIDENT _ -> "DOT_UIDENT"
  | Lex_menhir_token.ELLIPSIS -> "ELLIPSIS"
  | Lex_menhir_token.ELSE -> "ELSE"
  | Lex_menhir_token.ENUM -> "ENUM"
  | Lex_menhir_token.EOF -> "EOF"
  | Lex_menhir_token.EQUAL -> "EQUAL"
  | Lex_menhir_token.EXCLAMATION -> "EXCLAMATION"
  | Lex_menhir_token.EXTERN -> "EXTERN"
  | Lex_menhir_token.FALSE -> "FALSE"
  | Lex_menhir_token.FAT_ARROW -> "FAT_ARROW"
  | Lex_menhir_token.FLOAT _ -> "FLOAT"
  | Lex_menhir_token.FN -> "FN"
  | Lex_menhir_token.FOR -> "FOR"
  | Lex_menhir_token.GUARD -> "GUARD"
  | Lex_menhir_token.IF -> "IF"
  | Lex_menhir_token.IMPL -> "IMPL"
  | Lex_menhir_token.IMPORT -> "IMPORT"
  | Lex_menhir_token.IN -> "IN"
  | Lex_menhir_token.INFIX1 _ -> "INFIX1"
  | Lex_menhir_token.INFIX2 _ -> "INFIX2"
  | Lex_menhir_token.INFIX3 _ -> "INFIX3"
  | Lex_menhir_token.INFIX4 _ -> "INFIX4"
  | Lex_menhir_token.INT _ -> "INT"
  | Lex_menhir_token.INTERP _ -> "INTERP"
  | Lex_menhir_token.IS -> "IS"
  | Lex_menhir_token.LBRACE -> "LBRACE"
  | Lex_menhir_token.LBRACKET -> "LBRACKET"
  | Lex_menhir_token.LET -> "LET"
  | Lex_menhir_token.LIDENT _ -> "LIDENT"
  | Lex_menhir_token.LOOP -> "LOOP"
  | Lex_menhir_token.LPAREN -> "LPAREN"
  | Lex_menhir_token.MATCH -> "MATCH"
  | Lex_menhir_token.MINUS -> "MINUS"
  | Lex_menhir_token.MULTILINE_INTERP _ -> "MULTILINE_INTERP"
  | Lex_menhir_token.MULTILINE_STRING _ -> "MULTILINE_STRING"
  | Lex_menhir_token.MUTABLE -> "MUTABLE"
  | Lex_menhir_token.NEWLINE -> "NEWLINE"
  | Lex_menhir_token.PACKAGE_NAME _ -> "PACKAGE_NAME"
  | Lex_menhir_token.PIPE -> "PIPE"
  | Lex_menhir_token.PLUS -> "PLUS"
  | Lex_menhir_token.POST_LABEL _ -> "POST_LABEL"
  | Lex_menhir_token.PRIV -> "PRIV"
  | Lex_menhir_token.PUB -> "PUB"
  | Lex_menhir_token.QUESTION -> "QUESTION"
  | Lex_menhir_token.RAISE -> "RAISE"
  | Lex_menhir_token.RANGE_EXCLUSIVE -> "RANGE_EXCLUSIVE"
  | Lex_menhir_token.RANGE_INCLUSIVE -> "RANGE_INCLUSIVE"
  | Lex_menhir_token.RBRACE -> "RBRACE"
  | Lex_menhir_token.RBRACKET -> "RBRACKET"
  | Lex_menhir_token.READONLY -> "READONLY"
  | Lex_menhir_token.RETURN -> "RETURN"
  | Lex_menhir_token.RPAREN -> "RPAREN"
  | Lex_menhir_token.SEMI _ -> "SEMI"
  | Lex_menhir_token.STRING _ -> "STRING"
  | Lex_menhir_token.STRUCT -> "STRUCT"
  | Lex_menhir_token.TEST -> "TEST"
  | Lex_menhir_token.THIN_ARROW -> "THIN_ARROW"
  | Lex_menhir_token.THROW -> "THROW"
  | Lex_menhir_token.TRAIT -> "TRAIT"
  | Lex_menhir_token.TRAITALIAS -> "TRAITALIAS"
  | Lex_menhir_token.TRUE -> "TRUE"
  | Lex_menhir_token.TRY -> "TRY"
  | Lex_menhir_token.TYPE -> "TYPE"
  | Lex_menhir_token.TYPEALIAS -> "TYPEALIAS"
  | Lex_menhir_token.UIDENT _ -> "UIDENT"
  | Lex_menhir_token.UNDERSCORE -> "UNDERSCORE"
  | Lex_menhir_token.WHILE -> "WHILE"
  | Lex_menhir_token.WITH -> "WITH"

let _menhir_fail : unit -> 'a =
 fun () ->
  Printf.eprintf
    "Internal failure -- please contact the parser generator's developers.\n%!";
  assert false

include struct
  [@@@ocaml.warning "-4-37-39"]

  let rec _menhir_run_17 : type ttv_stack.
      ttv_stack -> _ -> _ -> _menhir_box_attribute =
   fun _menhir_stack ->
    fun _v ->
     fun _tok ->
      match (_tok : MenhirBasics.token) with
      | Lex_menhir_token.EOF ->
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          MenhirBox_attribute _v
      | _ -> _eRR ()

  let rec _menhir_run_01 : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        let _tok = _menhir_lexer _menhir_lexbuf in
        let _1 = _v in
        let _v = _menhir_action_06 _1 in
        _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
          _menhir_s _tok

  and _menhir_goto_expr : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        fun _tok ->
         match _menhir_s with
         | MenhirState00 -> _menhir_run_17 _menhir_stack _v _tok
         | MenhirState03 ->
             _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v
               _menhir_s _tok
         | MenhirState12 ->
             _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v
               _menhir_s _tok
         | MenhirState06 ->
             _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
         | _ -> _menhir_fail ()

  and _menhir_run_14 : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        fun _tok ->
         let _1 = _v in
         let _v = _menhir_action_12 _1 in
         _menhir_goto_property _menhir_stack _menhir_lexbuf _menhir_lexer _v
           _menhir_s _tok

  and _menhir_goto_property : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        fun _tok ->
         match (_tok : MenhirBasics.token) with
         | Lex_menhir_token.COMMA -> (
             let _menhir_stack =
               MenhirCell1_property (_menhir_stack, _menhir_s, _v)
             in
             let _menhir_s = MenhirState12 in
             let _tok = _menhir_lexer _menhir_lexbuf in
             match (_tok : MenhirBasics.token) with
             | Lex_menhir_token.STRING _v ->
                 _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v
                   _menhir_s
             | Lex_menhir_token.LIDENT _v ->
                 _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v
                   _menhir_s
             | _ -> _eRR ())
         | Lex_menhir_token.RPAREN ->
             let _1 = _v in
             let _v = _menhir_action_07 _1 in
             _menhir_goto_non_empty_properties _menhir_stack _menhir_lexbuf
               _menhir_lexer _v _menhir_s
         | _ -> _eRR ()

  and _menhir_run_05 : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        let _tok = _menhir_lexer _menhir_lexbuf in
        match (_tok : MenhirBasics.token) with
        | Lex_menhir_token.LPAREN ->
            let _menhir_stack =
              MenhirCell1_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer
              MenhirState05
        | Lex_menhir_token.EQUAL -> (
            let _menhir_stack =
              MenhirCell1_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            let _menhir_stack =
              MenhirCell1_EQUAL (_menhir_stack, MenhirState05)
            in
            let _menhir_s = MenhirState06 in
            let _tok = _menhir_lexer _menhir_lexbuf in
            match (_tok : MenhirBasics.token) with
            | Lex_menhir_token.STRING _v ->
                _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  _menhir_s
            | Lex_menhir_token.LIDENT _v ->
                _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  _menhir_s
            | _ -> _eRR ())
        | Lex_menhir_token.DOT_LIDENT _v_2 ->
            let _menhir_stack =
              MenhirCell1_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
              MenhirState05
        | Lex_menhir_token.COMMA | Lex_menhir_token.RPAREN ->
            let name = _v in
            let _v = _menhir_action_02 name in
            _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
              _menhir_s _tok
        | _ -> _eRR ()

  and _menhir_run_03 : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _menhir_s ->
       let _tok = _menhir_lexer _menhir_lexbuf in
       match (_tok : MenhirBasics.token) with
       | Lex_menhir_token.STRING _v ->
           let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
           _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v
             MenhirState03
       | Lex_menhir_token.RPAREN ->
           let _tok = _menhir_lexer _menhir_lexbuf in
           let _v = _menhir_action_09 () in
           _menhir_goto_properties _menhir_stack _menhir_lexbuf _menhir_lexer _v
             _menhir_s _tok
       | Lex_menhir_token.LIDENT _v ->
           let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
           _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v
             MenhirState03
       | _ -> _eRR ()

  and _menhir_goto_properties : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        fun _tok ->
         match _menhir_s with
         | MenhirState02 ->
             _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
         | MenhirState05 ->
             _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
         | MenhirState08 ->
             _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
         | _ -> _menhir_fail ()

  and _menhir_run_10 : type ttv_stack.
      (ttv_stack, _menhir_box_attribute) _menhir_cell1_LIDENT ->
      _ ->
      _ ->
      _ ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _tok ->
        let (MenhirCell1_LIDENT (_menhir_stack, _menhir_s, name)) =
          _menhir_stack
        in
        let props = _v in
        let _v = _menhir_action_04 name props in
        _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
          _menhir_s _tok

  and _menhir_run_09 : type ttv_stack.
      ( (ttv_stack, _menhir_box_attribute) _menhir_cell1_LIDENT,
        _menhir_box_attribute )
      _menhir_cell1_DOT_LIDENT ->
      _ ->
      _ ->
      _ ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _tok ->
        let (MenhirCell1_DOT_LIDENT (_menhir_stack, _, name)) = _menhir_stack in
        let (MenhirCell1_LIDENT (_menhir_stack, _menhir_s, qual)) =
          _menhir_stack
        in
        let props = _v in
        let _v = _menhir_action_05 name props qual in
        _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
          _menhir_s _tok

  and _menhir_run_02 : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        let _tok = _menhir_lexer _menhir_lexbuf in
        match (_tok : MenhirBasics.token) with
        | Lex_menhir_token.LPAREN ->
            let _menhir_stack =
              MenhirCell1_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer
              MenhirState02
        | Lex_menhir_token.DOT_LIDENT _v_0 ->
            let _menhir_stack =
              MenhirCell1_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
              MenhirState02
        | Lex_menhir_token.COMMA | Lex_menhir_token.EOF
        | Lex_menhir_token.RPAREN ->
            let name = _v in
            let _v = _menhir_action_02 name in
            _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
              _menhir_s _tok
        | _ -> _eRR ()

  and _menhir_run_08 : type ttv_stack.
      ((ttv_stack, _menhir_box_attribute) _menhir_cell1_LIDENT as 'stack) ->
      _ ->
      _ ->
      _ ->
      ('stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        let _tok = _menhir_lexer _menhir_lexbuf in
        match (_tok : MenhirBasics.token) with
        | Lex_menhir_token.LPAREN ->
            let _menhir_stack =
              MenhirCell1_DOT_LIDENT (_menhir_stack, _menhir_s, _v)
            in
            _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer
              MenhirState08
        | Lex_menhir_token.COMMA | Lex_menhir_token.EOF
        | Lex_menhir_token.RPAREN ->
            let (MenhirCell1_LIDENT (_menhir_stack, _menhir_s, qual)) =
              _menhir_stack
            in
            let name = _v in
            let _v = _menhir_action_03 name qual in
            _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v
              _menhir_s _tok
        | _ -> _eRR ()

  and _menhir_goto_non_empty_properties : type ttv_stack.
      ttv_stack ->
      _ ->
      _ ->
      _ ->
      (ttv_stack, _menhir_box_attribute) _menhir_state ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _menhir_s ->
        match _menhir_s with
        | MenhirState03 ->
            _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v
        | MenhirState12 ->
            _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v
        | _ -> _menhir_fail ()

  and _menhir_run_15 : type ttv_stack.
      (ttv_stack, _menhir_box_attribute) _menhir_cell1_LPAREN ->
      _ ->
      _ ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       let _tok = _menhir_lexer _menhir_lexbuf in
       let (MenhirCell1_LPAREN (_menhir_stack, _menhir_s)) = _menhir_stack in
       let _2 = _v in
       let _v = _menhir_action_10 _2 in
       _menhir_goto_properties _menhir_stack _menhir_lexbuf _menhir_lexer _v
         _menhir_s _tok

  and _menhir_run_13 : type ttv_stack.
      (ttv_stack, _menhir_box_attribute) _menhir_cell1_property ->
      _ ->
      _ ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       let (MenhirCell1_property (_menhir_stack, _menhir_s, x)) =
         _menhir_stack
       in
       let xs = _v in
       let _v = _menhir_action_08 x xs in
       _menhir_goto_non_empty_properties _menhir_stack _menhir_lexbuf
         _menhir_lexer _v _menhir_s

  and _menhir_run_07 : type ttv_stack.
      ( (ttv_stack, _menhir_box_attribute) _menhir_cell1_LIDENT,
        _menhir_box_attribute )
      _menhir_cell1_EQUAL ->
      _ ->
      _ ->
      _ ->
      _ ->
      _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      fun _v ->
       fun _tok ->
        let (MenhirCell1_EQUAL (_menhir_stack, _)) = _menhir_stack in
        let (MenhirCell1_LIDENT (_menhir_stack, _menhir_s, name)) =
          _menhir_stack
        in
        let _3 = _v in
        let _v = _menhir_action_11 _3 name in
        _menhir_goto_property _menhir_stack _menhir_lexbuf _menhir_lexer _v
          _menhir_s _tok

  let rec _menhir_run_00 : type ttv_stack.
      ttv_stack -> _ -> _ -> _menhir_box_attribute =
   fun _menhir_stack ->
    fun _menhir_lexbuf ->
     fun _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lex_menhir_token.STRING _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lex_menhir_token.LIDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ -> _eRR ()
end

let attribute _menhir_lexer _menhir_lexbuf =
  let _menhir_stack = () in
  let (MenhirBox_attribute v) =
    _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer
  in
  v
