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
module Menhir_token_util = Lex_menhir_token_util
module Token_triple = Lex_token_triple
module Vec = Basic_vec
module Lst = Basic_lst
module Syntax = Parsing_syntax
module Segment = Parsing_segment

type token = Menhir_token.token
type triple = Token_triple.t
type absolute_pos = Lexing.position
type position = Rloc.rpos
type location = Syntax.location
type string_literal = Menhir_token.Literal.string_literal

type token_kind = Menhir_token.token_kind =
  | Token_kind : _ Menhir_token.terminal -> token_kind
[@@unboxed]

let kind_of_token = Menhir_token.kind_of_token
and sexp_of_token = Menhir_token.sexp_of_token

let token_kind_to_expect_string = Menhir_token_util.token_kind_to_expect_string

type parse_mode =
  | Normal
  | Panic of { fail_at : token_kind; sync_at : int; loc : location }

type parse_state = {
  segment : Segment.t;
  diagnostics : Diagnostics.t;
  mutable previous_triple : triple;
  mutable parsed_position : absolute_pos;
  syncs : token_kind Vec.t;
  mutable mode : parse_mode;
  mutable base : absolute_pos;
}

let index_of_token_kind (xs : token_kind Vec.t) (x : token_kind) =
  let rec loop (xs : token_kind Vec.t) x i =
    match Vec.get_opt xs i with
    | None -> None
    | Some x' -> if x = x' then Some i else loop xs x (i - 1)
  in
  loop xs x (Vec.length xs - 1)

let rec mem_token_kind (xs : token_kind list) (x : token_kind) =
  match xs with [] -> false | a :: l -> a = x || mem_token_kind l x

let skip_this (state : parse_state) =
  state.previous_triple <- Segment.next state.segment;
  let _, _, e = state.previous_triple in
  state.parsed_position <- e

let init_state ~diagnostics (segment : Segment.t) =
  (let syncs = Vec.empty () in
   let () = Vec.push syncs (Menhir_token.Token_kind T_EOF) in
   let ((_, s, _) as first_triple) = Segment.peek segment in
   {
     segment;
     previous_triple = first_triple;
     parsed_position = s;
     diagnostics;
     syncs;
     mode = Normal;
     base = s;
   }
    : parse_state)

let get_mode (state : parse_state) = (state.mode : parse_mode)
let peek_triple (state : parse_state) = (Segment.peek state.segment : triple)

let peek_token (state : parse_state) =
  (let tok, _, _ = peek_triple state in
   tok
    : token)

let peek_token_kind (state : parse_state) =
  (kind_of_token (peek_token state) : token_kind)

let peek_nth_token (state : parse_state) (n : int) =
  (let tok, _, _ = Segment.peek_nth state.segment n in
   tok
    : token)

let peek_nth_token_kind (state : parse_state) (n : int) =
  (kind_of_token (peek_nth_token state n) : token_kind)

let peek_loc_start (state : parse_state) =
  (let _, s, _ = peek_triple state in
   Rloc.lex_pos_to_pos ~base:state.base s
    : position)

let peek_loc_end (state : parse_state) =
  (let _, _, e = peek_triple state in
   Rloc.lex_pos_to_pos ~base:state.base e
    : position)

let loc_start_with (state : parse_state) (loc_start : position) =
  (let e = state.parsed_position in
   let loc_end = Rloc.lex_pos_to_pos ~base:state.base e in
   Rloc.of_pos (loc_start, loc_end)
    : location)

let peek_location (state : parse_state) =
  (let _, s, e = peek_triple state in
   let loc = (s, e) in
   Rloc.of_menhir ~base:state.base loc
    : location)

let peek_absolute_location (state : parse_state) =
  (let _, s, e = peek_triple state in
   let loc = (s, e) in
   Loc.of_menhir loc
    : Loc.t)

let debug_mode : bool = false
let debug_indent : int ref = ref 0

let parser_debug_print (str : string) =
  if debug_mode then Printf.printf "%*s%s\n" !debug_indent " " str

let debug_show_rest_tokens (state : parse_state) (n : int) =
  if debug_mode then
    for i = 1 to n do
      parser_debug_print (S.to_string (sexp_of_token (peek_nth_token state i)))
    done

let debugged (str : string) (state : parse_state) (parser : parse_state -> 'a) =
  (if debug_mode then (
     if get_mode state = Normal then
       parser_debug_print
         ("# " ^ str ^ " at "
         ^ S.to_string (sexp_of_token (peek_token state))
         ^ " ("
         ^ Loc.to_string (peek_absolute_location state)
         ^ ")")
     else parser_debug_print ("!!! " ^ str ^ " skipped.");
     debug_indent := !debug_indent + 2;
     let res = parser state in
     debug_indent := !debug_indent - 2;
     res)
   else parser state
    : 'a)

let parser_panic (state : parse_state) =
  if debug_mode then (
    parser_debug_print
      ("[!] parser paniced at " ^ S.to_string (sexp_of_token (peek_token state)));
    debug_show_rest_tokens state 8);
  match get_mode state with
  | Normal ->
      let _, _, prev_token_end = state.previous_triple in
      let fail_at, first_token_start, _ = peek_triple state in
      let rec find_recovery_point n_skipped =
        let _, _, last_token_end = state.previous_triple in
        let next_token, next_token_start, _ = peek_triple state in
        match index_of_token_kind state.syncs (kind_of_token next_token) with
        | Some index ->
            let loc =
              if n_skipped > 0 then
                Rloc.of_menhir ~base:state.base
                  (first_token_start, last_token_end)
              else (
                state.parsed_position <- next_token_start;
                Rloc.of_menhir ~base:state.base
                  (prev_token_end, next_token_start))
            in
            state.mode <-
              Panic { fail_at = kind_of_token fail_at; sync_at = index; loc };
            loc
        | None ->
            skip_this state;
            find_recovery_point (n_skipped + 1)
      in
      find_recovery_point 0
  | Panic { loc; _ } -> loc

let parser_recover (state : parse_state) =
  if get_mode state <> Normal then (
    if debug_mode then parser_debug_print "!!! parser recovered here!")
  else failwith "you can't recover from normal mode!";
  state.mode <- Normal

let add_error_failed_to_parse (state : parse_state) (found : token)
    (expected : string) (loc : Rloc.t) =
  let loc = Rloc.to_loc_base_pos ~base:state.base loc in
  let error =
    let fallback found =
      Errors.unexpected_token
        ~found:(token_kind_to_expect_string (kind_of_token found))
        ~expected ~loc
    in
    match found with
    | SEMI false -> Errors.unexpected_line_break ~expected ~loc
    | EOF -> (
        match Segment.next_segment_head state.segment with
        | Some (tok, _, _) -> (
            match tok with
            | FN | LET ->
                Errors.unexpected_token_maybe_forget_indent ~expected
                  ~next:(token_kind_to_expect_string (kind_of_token tok))
                  ~loc
            | _ -> fallback tok)
        | _ -> fallback found)
    | _ -> fallback found
  in
  parser_debug_print error.message;
  Diagnostics.add_error state.diagnostics error

let add_error_unexpected (state : parse_state) (found : token)
    (expect : token_kind list) (loc : location) =
  let expect =
    String.concat " or " (Lst.map expect token_kind_to_expect_string)
  in
  add_error_failed_to_parse state found expect loc

let add_error_skipped (_state : parse_state) (expect : string) (_loc : Rloc.t) =
  let message = "Skipped: " ^ expect in
  parser_debug_print message

let add_error (state : parse_state) (error : Local_diagnostics.error) =
  parser_debug_print error.message;
  let loc = Rloc.to_loc_base_pos ~base:state.base error.loc in
  let error : Diagnostics.report =
    { loc; message = error.message; error_code = error.error_code }
  in
  Diagnostics.add_error state.diagnostics error

let push_sync (state : parse_state) (sync : token_kind) =
  Vec.push state.syncs sync

let pop_sync (state : parse_state) (sync : token_kind) =
  match Vec.pop_no_compact state.syncs with
  | Some x -> (
      if x <> sync then failwith "unmatched sync token!";
      match state.mode with
      | Panic { sync_at; _ } when sync_at = Vec.length state.syncs ->
          parser_recover state
      | _ -> ())
  | None -> failwith "pop from an empty stack!"

let push_sync_list (state : parse_state) (syncs : token_kind list) =
  Basic_lst.iter syncs ~f:(fun sync -> push_sync state sync)

let pop_sync_list (state : parse_state) (syncs : token_kind list) =
  Basic_lst.iter (List.rev syncs) ~f:(fun sync -> pop_sync state sync)

let with_sync (state : parse_state) (sync : token_kind) f =
  push_sync state sync;
  let r = f state in
  pop_sync state sync;
  r

let with_syncs (state : parse_state) (syncs : token_kind list) f =
  push_sync_list state syncs;
  let r = f state in
  pop_sync_list state syncs;
  r

let expected_token (expect : token_kind) (state : parse_state) =
  (match state.mode with
   | Normal ->
       let tok, loc_start, loc_end = peek_triple state in
       let found = kind_of_token tok in
       if found = expect then (
         if debug_mode then
           parser_debug_print ("  consumed " ^ S.to_string (sexp_of_token tok));
         skip_this state)
       else
         let _ = parser_panic state in
         add_error_unexpected state tok [ expect ]
           (Rloc.of_menhir ~base:state.base (loc_start, loc_end))
   | Panic { loc; _ } ->
       add_error_skipped state (token_kind_to_expect_string expect) loc
    : unit)

let expected_template (expect : token_kind) (default : 'a)
    (func : token -> 'a option) (state : parse_state) =
  match state.mode with
  | Normal -> (
      let found, loc_start, loc_end = peek_triple state in
      match func found with
      | Some res ->
          skip_this state;
          res
      | None ->
          let _ = parser_panic state in
          add_error_unexpected state found [ expect ]
            (Rloc.of_menhir ~base:state.base (loc_start, loc_end));
          default)
  | Panic { loc; _ } ->
      add_error_skipped state (token_kind_to_expect_string expect) loc;
      default

let expected_lident (state : parse_state) =
  (expected_template (Token_kind T_LIDENT) "?LIDENT"
     (fun tok -> match tok with LIDENT res -> Some res | _ -> None)
     state
    : string)

let expected_uident ?(default = "?UIDENT") (state : parse_state) =
  (expected_template (Token_kind T_UIDENT) default
     (fun tok -> match tok with UIDENT res -> Some res | _ -> None)
     state
    : string)

let expected_string (state : parse_state) =
  (expected_template (Token_kind T_STRING)
     ({ string_val = "?STRING"; string_repr = "?STRING" } : string_literal)
     (fun tok -> match tok with STRING res -> Some res | _ -> None)
     state
    : string_literal)

let expected_pkg_name (state : parse_state) =
  (expected_template (Token_kind T_PACKAGE_NAME) "?PACKAGE_NAME"
     (fun tok -> match tok with PACKAGE_NAME res -> Some res | _ -> None)
     state
    : string)

let expected_dot_lident (state : parse_state) =
  (expected_template (Token_kind T_DOT_LIDENT) "?DOT_IDENT"
     (fun tok -> match tok with DOT_LIDENT res -> Some res | _ -> None)
     state
    : string)

let expected_dot_uident (state : parse_state) =
  (expected_template (Token_kind T_DOT_UIDENT) "?DOT_IDENT"
     (fun tok -> match tok with DOT_UIDENT res -> Some res | _ -> None)
     state
    : string)

let expected_dot_int (state : parse_state) =
  (expected_template (Token_kind T_DOT_INT) 0
     (fun tok -> match tok with DOT_INT res -> Some res | _ -> None)
     state
    : int)

let expected_int (state : parse_state) =
  (expected_template (Token_kind T_INT) ""
     (fun tok -> match tok with INT res -> Some res | _ -> None)
     state
    : string)

let option (first : token_kind list) (parser : parse_state -> 'a)
    (state : parse_state) =
  (match state.mode with
   | Normal ->
       if mem_token_kind first (kind_of_token (peek_token state)) then
         Some (parser state)
       else None
   | Panic { loc; _ } ->
       add_error_skipped state "option .." loc;
       None
    : 'a option)

let sepby (delim : token_kind) (first : token_kind list)
    (parser : parse_state -> 'a) (state : parse_state) =
  (match state.mode with
   | Normal ->
       push_sync state delim;
       if mem_token_kind first (kind_of_token (peek_token state)) then (
         let elem1 = parser state in
         let res = ref [ elem1 ] in
         while kind_of_token (peek_token state) = delim do
           pop_sync state delim;
           expected_token delim state;
           push_sync state delim;
           res := parser state :: !res
         done;
         pop_sync state delim;
         List.rev !res)
       else (
         pop_sync state delim;
         [])
   | Panic { loc; _ } ->
       add_error_skipped state "sepby .." loc;
       []
    : 'a list)

let sepby1 (delim : token_kind) (parser : parse_state -> 'a)
    (state : parse_state) =
  (match state.mode with
   | Normal ->
       push_sync state delim;
       let elem1 = parser state in
       let res = ref [ elem1 ] in
       while kind_of_token (peek_token state) = delim do
         pop_sync state delim;
         expected_token delim state;
         push_sync state delim;
         res := parser state :: !res
       done;
       pop_sync state delim;
       List.rev !res
   | Panic { loc; _ } ->
       add_error_skipped state "sepby1 .." loc;
       []
    : 'a list)

let surround (left : token_kind) (right : token_kind)
    (parser : parse_state -> 'a) (state : parse_state) =
  match state.mode with
  | Normal ->
      expected_token left state;
      push_sync state right;
      let res = parser state in
      pop_sync state right;
      expected_token right state;
      res
  | Panic { loc; _ } ->
      add_error_skipped state "surround .." loc;
      parser state

let with_follow (follow : token_kind list) (parser : parse_state -> 'a)
    (state : parse_state) =
  (match state.mode with
   | Normal ->
       push_sync_list state follow;
       let result = parser state in
       if not (mem_token_kind follow (peek_token_kind state)) then (
         add_error_unexpected state (peek_token state) follow
           (peek_location state);
         ignore (parser_panic state));
       pop_sync_list state follow;
       result
   | Panic { loc; _ } ->
       add_error_skipped state "with follow " loc;
       parser state
    : 'a)

let sepby_with_follow ?(invalid_delims = []) (delim : token_kind)
    (follow : token_kind list) (parser : parse_state -> 'a)
    (state : parse_state) =
  (let try_recover_to_delim () =
     let loc = peek_location state in
     let tok = peek_token state in
     push_sync state delim;
     let _ = parser_panic state in
     add_error_unexpected state tok (delim :: follow) loc;
     pop_sync state delim;
     if peek_token_kind state = delim then expected_token delim state
   in
   match state.mode with
   | Normal ->
       let res = ref [] in
       push_sync_list state follow;
       let rec loop () =
         match state.mode with
         | Panic _ -> ()
         | Normal when mem_token_kind follow (peek_token_kind state) -> ()
         | Normal -> (
             push_sync state delim;
             res := parser state :: !res;
             pop_sync state delim;
             match state.mode with
             | Panic _ -> ()
             | Normal when mem_token_kind follow (peek_token_kind state) -> ()
             | Normal when peek_token_kind state = delim ->
                 expected_token delim state;
                 loop ()
             | Normal when mem_token_kind invalid_delims (peek_token_kind state)
               ->
                 let error =
                   Errors.invalid_extra_delimiter
                     ~loc:
                       (Rloc.to_loc_base_pos ~base:state.base
                          (peek_location state))
                     ~delimiter:
                       (Menhir_token_util.token_kind_to_expect_string
                          (peek_token_kind state))
                 in
                 Diagnostics.add_error state.diagnostics error;
                 skip_this state;
                 loop ()
             | Normal ->
                 try_recover_to_delim ();
                 loop ())
       in
       loop ();
       pop_sync_list state follow;
       let prv_tok, _, _ = state.previous_triple in
       let trailing_delim = kind_of_token prv_tok = delim in
       (List.rev !res, trailing_delim)
   | Panic { loc; _ } ->
       add_error_skipped state "sepby with right .." loc;
       ([], false)
    : 'a list * bool)

let surround_sepby ?(invalid_delims = []) (left : token_kind)
    (right : token_kind) (delim : token_kind) (parser : parse_state -> 'a)
    (state : parse_state) =
  (match state.mode with
   | Normal ->
       Stdlib.fst
         (surround left right
            (sepby_with_follow ~invalid_delims delim [ right ] parser)
            state)
   | Panic { loc; _ } ->
       add_error_skipped state "surround sepby .." loc;
       []
    : 'a list)

let located (state : parse_state) (parser : parse_state -> location -> 'a) =
  (let loc_start = peek_loc_start state in
   let func = parser state in
   let loc = loc_start_with state loc_start in
   func loc
    : 'a)

let get_absolute_loc (state : parse_state) (loc : location) =
  (Rloc.to_loc_base_pos ~base:state.base loc : Loc.t)

let update_base (state : parse_state) =
  (let _, s, _ = peek_triple state in
   state.base <- s
    : unit)
