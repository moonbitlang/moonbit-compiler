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
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = Sexp_token.token
  
end

include MenhirBasics

# 1 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
  
[@@@ocamlformat "disable"]

# 21 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState01 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 01.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState02 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 02.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState07 : (('s, 'r) _menhir_cell1_sexp, 'r) _menhir_state
    (** State 07.
        Stack shape : sexp.
        Start symbol: <undetermined>. *)

  | MenhirState16 : ('s, _menhir_box_sexps) _menhir_state
    (** State 16.
        Stack shape : .
        Start symbol: sexps. *)

  | MenhirState20 : (('s, _menhir_box_sexps) _menhir_cell1_sexp, _menhir_box_sexps) _menhir_state
    (** State 20.
        Stack shape : sexp.
        Start symbol: sexps. *)


and ('s, 'r) _menhir_cell1_sexp = 
  | MenhirCell1_sexp of 's * ('s, 'r) _menhir_state * (W.t)

and ('s, 'r) _menhir_cell1_sexp_list = 
  | MenhirCell1_sexp_list of 's * ('s, 'r) _menhir_state * (W.t list)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_sexps = 
  | MenhirBox_sexps of (W.t list) [@@unboxed]

and _menhir_box_main = 
  | MenhirBox_main of (W.t) [@@unboxed]

let _menhir_action_2 =
  fun s ->
    (
# 16 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( s )
# 75 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t))

let _menhir_action_3 =
  fun s ->
    (
# 25 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( W.List s )
# 83 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t))

let _menhir_action_4 =
  fun _1 ->
    let a = 
# 22 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( _1)
# 91 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     in
    (
# 26 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( W.Atom a )
# 96 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t))

let _menhir_action_5 =
  fun _endpos__3_ _startpos__1_ ->
    let _ = let _endpos = _endpos__3_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 27 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( raise (Dwarfsm_sexp_parse_error.Unmatched_parenthesis _sloc) )
# 107 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t)) in
    prerr_string "Menhir: misuse: the semantic action associated with the production\nsexp -> LPAREN sexp_list error\nis expected to abort the parser, but does not do so.\n";
    assert false

let _menhir_action_6 =
  fun () ->
    (
# 30 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( [] )
# 117 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t list))

let _menhir_action_7 =
  fun l s ->
    (
# 31 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( s :: l )
# 125 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t list))

let _menhir_action_8 =
  fun s ->
    (
# 19 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.mly"
                                      ( s )
# 133 "lib/dwarfsm/sexp/dwarfsm_sexp_parser.ml"
     : (W.t list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | Sexp_token.ANTIQUOT _ ->
        "ANTIQUOT"
    | Sexp_token.ANTIQUOT_SEXP _ ->
        "ANTIQUOT_SEXP"
    | Sexp_token.Atom _ ->
        "Atom"
    | Sexp_token.EOF ->
        "EOF"
    | Sexp_token.LPAREN ->
        "LPAREN"
    | Sexp_token.RPAREN ->
        "RPAREN"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_sexps =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Sexp_token.EOF ->
          let s = _v in
          let _v = _menhir_action_8 s in
          MenhirBox_sexps _v
      | _ ->
          _eRR ()
  
  let rec _menhir_error_run_20 : type  ttv_stack. ttv_stack -> _menhir_box_sexps =
    fun _menhir_stack ->
      _eRR ()
  
  let rec _menhir_error_run_13 : type  ttv_stack. ttv_stack -> _menhir_box_main =
    fun _menhir_stack ->
      _eRR ()
  
  let rec _menhir_error_run_18 : type  ttv_stack. ttv_stack -> _menhir_box_sexps =
    fun _menhir_stack ->
      _eRR ()
  
  let rec _menhir_error_run_10 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_sexp_list -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let MenhirCell1_sexp_list (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_5 _endpos__3_ _startpos__1_ in
      _menhir_error_goto_sexp _menhir_stack _menhir_lexbuf _v _menhir_s
  
  and _menhir_error_goto_sexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_error_run_20 _menhir_stack
      | MenhirState16 ->
          _menhir_error_run_20 _menhir_stack
      | MenhirState00 ->
          _menhir_error_run_13 _menhir_stack
      | MenhirState01 ->
          _menhir_error_run_07 _menhir_stack _menhir_lexbuf _v _menhir_s
      | MenhirState07 ->
          _menhir_error_run_07 _menhir_stack _menhir_lexbuf _v _menhir_s
      | MenhirState02 ->
          _menhir_error_run_07 _menhir_stack _menhir_lexbuf _v _menhir_s
  
  and _menhir_error_run_07 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_6 () in
      _menhir_error_run_08 _menhir_stack _menhir_lexbuf _v_0
  
  and _menhir_error_run_08 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_sexp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_7 l s in
      _menhir_error_goto_sexp_list _menhir_stack _menhir_lexbuf _v _menhir_s
  
  and _menhir_error_goto_sexp_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_error_run_21 _menhir_stack _menhir_lexbuf _v
      | MenhirState16 ->
          _menhir_error_run_18 _menhir_stack
      | MenhirState01 ->
          _menhir_error_run_09 _menhir_stack _menhir_lexbuf _v _menhir_s
      | MenhirState07 ->
          _menhir_error_run_08 _menhir_stack _menhir_lexbuf _v
      | MenhirState02 ->
          _menhir_error_run_04 _menhir_stack _menhir_lexbuf _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_error_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_sexps) _menhir_cell1_sexp -> _ -> _ -> _menhir_box_sexps =
    fun _menhir_stack _menhir_lexbuf _v ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_7 l s in
      _menhir_error_goto_sexp_list _menhir_stack _menhir_lexbuf _v _menhir_s
  
  and _menhir_error_run_09 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      let _menhir_stack = MenhirCell1_sexp_list (_menhir_stack, _menhir_s, _v) in
      _menhir_error_run_10 _menhir_stack _menhir_lexbuf
  
  and _menhir_error_run_04 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      let _menhir_stack = MenhirCell1_sexp_list (_menhir_stack, _menhir_s, _v) in
      _menhir_error_run_05 _menhir_stack _menhir_lexbuf
  
  and _menhir_error_run_05 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_sexp_list -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let MenhirCell1_sexp_list (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_5 _endpos__3_ _startpos__1_ in
      _menhir_error_goto_sexp _menhir_stack _menhir_lexbuf _v _menhir_s
  
  let rec _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Sexp_token.EOF ->
          let s = _v in
          let _v = _menhir_action_2 s in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | Sexp_token.Atom _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01
      | Sexp_token.RPAREN ->
          let _v = _menhir_action_6 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
      | _ ->
          let _v = _menhir_action_6 () in
          _menhir_error_run_09 _menhir_stack _menhir_lexbuf _v MenhirState01
  
  and _menhir_run_02 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | Sexp_token.Atom _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02
      | Sexp_token.RPAREN ->
          let _v = _menhir_action_6 () in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
      | _ ->
          let _v = _menhir_action_6 () in
          _menhir_error_run_04 _menhir_stack _menhir_lexbuf _v MenhirState02
  
  and _menhir_run_03 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_4 _1 in
      _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_sexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_13 _menhir_stack _v _tok
      | MenhirState01 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_sexps) _menhir_state -> _ -> _menhir_box_sexps =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | Sexp_token.Atom _v_0 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState20
      | Sexp_token.EOF ->
          let _v_1 = _menhir_action_6 () in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_4 _1 in
      _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_sexps) _menhir_cell1_sexp -> _ -> _ -> _ -> _ -> _menhir_box_sexps =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_7 l s in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_sexp_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_18 _menhir_stack _v _tok
      | MenhirState01 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState02 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_09 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Sexp_token.RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let s = _v in
          let _v = _menhir_action_3 s in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          let _menhir_stack = MenhirCell1_sexp_list (_menhir_stack, _menhir_s, _v) in
          _menhir_error_run_10 _menhir_stack _menhir_lexbuf
  
  and _menhir_run_08 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_sexp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_7 l s in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_04 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Sexp_token.RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let s = _v in
          let _v = _menhir_action_3 s in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          let _menhir_stack = MenhirCell1_sexp_list (_menhir_stack, _menhir_s, _v) in
          _menhir_error_run_05 _menhir_stack _menhir_lexbuf
  
  and _menhir_run_07 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | Sexp_token.Atom _v_0 ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState07
      | Sexp_token.RPAREN ->
          let _v_1 = _menhir_action_6 () in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          let _v_2 = _menhir_action_6 () in
          _menhir_error_run_08 _menhir_stack _menhir_lexbuf _v_2
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Sexp_token.Atom _v ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  let rec _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_sexps =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Sexp_token.LPAREN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | Sexp_token.Atom _v ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16
      | Sexp_token.EOF ->
          let _v = _menhir_action_6 () in
          _menhir_run_18 _menhir_stack _v _tok
      | _ ->
          _eRR ()
  
end

let sexps =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_sexps v = _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
