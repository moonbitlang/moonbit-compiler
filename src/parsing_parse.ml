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


module Syntax = Parsing_syntax
module Segment = Parsing_segment
module Parser = Parsing_parser
module Parser_main = Parsing_main
module Parser_core = Parsing_core
module Header_parser = Parsing_header_parser
module Ast_lint = Parsing_ast_lint
module Vec_token = Lex_vec_token
module Menhir_token = Lex_menhir_token
module Vec_comment = Lex_vec_comment
module Unicode_lex = Lex_unicode_lex
module Lst = Basic_lst

type output = {
  ast : Syntax.impls;
  directive : (string * string) list;
  tokens_list : Vec_token.t list;
  name : string;
  pkg : string;
}

let sexp_of_output (x : output) =
  (let impls = Basic_lst.map x.ast Syntax.sexp_of_impl in
   let directive =
     match Basic_lst.assoc_str x.directive "build" with
     | None -> []
     | Some d ->
         let msg = "//!build:" ^ d in
         ([
            List (List.cons (Atom "directive" : S.t) ([ Atom msg ] : S.t list));
          ]
           : S.t list)
   in
   (List (List.append (directive : S.t list) (impls : S.t list)) : S.t)
    : S.t)

let menhir_parse_toplevel ~diagnostics segment =
  (let lexbuf : Lexing.lexbuf = Lexing.from_string "" in
   let lexer (lexbuf : Lexing.lexbuf) =
     (Segment.next_with_lexbuf_update segment lexbuf : Menhir_token.token)
   in
   Parsing_menhir_state.initialize_state segment;
   try Parser.structure lexer lexbuf
   with Parser.Error ->
     Diagnostics.add_error diagnostics
       (Errors.parse_error ~loc_start:lexbuf.lex_start_p
          ~loc_end:lexbuf.lex_curr_p "parse error");
     []
    : Syntax.impl list)

let doc_search ~diagnostics (docstrings : Vec_comment.t) (last_pos : Loc.t ref)
    (ast : Syntax.impl) =
  match ast with
  | Ptop_typedef td ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:td.loc_ docstrings
      in
      td.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := td.loc_
  | Ptop_funcdef fd ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:fd.loc_ docstrings
      in
      fd.fun_decl.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := fd.loc_
  | Ptop_letdef ld ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:ld.loc_ docstrings
      in
      ld.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := ld.loc_
  | Ptop_expr { loc_; _ } | Ptop_impl_relation { loc_; _ } -> last_pos := loc_
  | Ptop_test test ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:test.loc_ docstrings
      in
      test.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := test.loc_
  | Ptop_trait trait ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:trait.trait_loc_ docstrings
      in
      trait.trait_doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := trait.trait_loc_
  | Ptop_trait_alias trait_alias ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:trait_alias.loc_ docstrings
      in
      trait_alias.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := trait_alias.loc_
  | Ptop_impl impl ->
      let comments =
        Vec_comment.search ~last:!last_pos ~loc_:impl.loc_ docstrings
      in
      impl.doc_ <- Docstring.of_comments ~diagnostics comments;
      last_pos := impl.loc_

let parse_by_menhir ~diagnostics segments =
  (Basic_lst.concat_map segments (fun x -> menhir_parse_toplevel ~diagnostics x)
    : Syntax.impls)

let parse_by_handrolled ~diagnostics segments =
  (Basic_lst.concat_map segments (fun segment ->
       let state = Parser_core.init_state ~diagnostics segment in
       Parser_main.parse_toplevel state)
    : Syntax.impls)

let parse_segment ~diagnostics segment =
  (let menhir_diag = Diagnostics.make () in
   let menhir_output = menhir_parse_toplevel ~diagnostics:menhir_diag segment in
   if not (Diagnostics.has_fatal_errors menhir_diag) then (
     Diagnostics.merge_into diagnostics menhir_diag;
     menhir_output)
   else (
     Segment.reset segment;
     let hand_diag = Diagnostics.make () in
     let state = Parser_core.init_state ~diagnostics:hand_diag segment in
     let hand_output = Parser_main.parse_toplevel state in
     if Diagnostics.has_fatal_errors hand_diag then
       Diagnostics.merge_into diagnostics hand_diag
     else Diagnostics.merge_into diagnostics menhir_diag;
     hand_output)
    : Syntax.impls)

let parse_by_mix ~diagnostics segments =
  (Basic_lst.concat_map segments (parse_segment ~diagnostics) : Syntax.impls)

let check_ast ~diagnostics ?name segment menhir_ast handrolled_ast =
  Basic_ref.protect Basic_config.show_loc false (fun _ ->
      let s1 = Syntax.sexp_of_impls menhir_ast in
      let s2 = Syntax.sexp_of_impls handrolled_ast in
      if not (S.equal s1 s2) then
        Diagnostics.add_warning diagnostics
          {
            kind =
              Warnings.Parser_inconsistency
                {
                  file_name = Option.value ~default:"unknown file" name;
                  segment = S.to_string (Parsing_segment.sexp_of_t segment);
                  is_menhir_succeed = true;
                  is_handrolled_succeed = true;
                };
            loc = Loc.no_location;
          })

let impl_of_segments ~diagnostics ?name ~docstrings ~transform
    (segments : Segment.t list) =
  (let ast =
     match Basic_config.current_parser () with
     | `Menhir -> parse_by_menhir ~diagnostics segments
     | `Handrolled -> parse_by_handrolled ~diagnostics segments
     | `Both -> (
         match Basic_config.env with
         | Release -> parse_by_mix ~diagnostics segments
         | Debug ->
             Basic_lst.concat_map segments (fun segment ->
                 let menhir_diagnostics = Diagnostics.make () in
                 let handrolled_diagnostics = Diagnostics.make () in
                 let menhir_ast =
                   menhir_parse_toplevel ~diagnostics:menhir_diagnostics segment
                 in
                 Segment.reset segment;
                 let handrolled_ast =
                   let state =
                     Parser_core.init_state ~diagnostics:handrolled_diagnostics
                       segment
                   in
                   Parser_main.parse_toplevel state
                 in
                 let report_inconsistency is_menhir_succeed
                     is_handrolled_succeed =
                   Diagnostics.add_warning diagnostics
                     {
                       kind =
                         Warnings.Parser_inconsistency
                           {
                             file_name =
                               Option.value ~default:"unknown file" name;
                             segment =
                               S.to_string
                                 ((Segment.sexp_of_t [@merlin.hide]) segment);
                             is_menhir_succeed;
                             is_handrolled_succeed;
                           };
                       loc = Loc.no_location;
                     }
                 in
                 match
                   ( not (Diagnostics.has_fatal_errors menhir_diagnostics),
                     not (Diagnostics.has_fatal_errors handrolled_diagnostics)
                   )
                 with
                 | true, true ->
                     check_ast ~diagnostics:handrolled_diagnostics ?name segment
                       menhir_ast handrolled_ast;
                     Diagnostics.merge_into diagnostics handrolled_diagnostics;
                     menhir_ast
                 | false, false ->
                     Diagnostics.merge_into diagnostics handrolled_diagnostics;
                     handrolled_ast
                 | false, true ->
                     report_inconsistency false true;
                     Diagnostics.merge_into diagnostics menhir_diagnostics;
                     handrolled_ast
                 | true, false ->
                     report_inconsistency true false;
                     Diagnostics.merge_into diagnostics menhir_diagnostics;
                     menhir_ast))
   in
   let last_pos = ref Loc.no_location in
   Lst.iter ast ~f:(doc_search ~diagnostics docstrings last_pos);
   if transform then Ast_lint.post_process ~diagnostics ast else ast
    : Syntax.impls)

let debug_tokens_info name (tokens_list : Vec_token.t list) =
  let str =
    String.concat "\n" (Lst.map tokens_list Vec_token.string_of_tokens)
  in
  match name with
  | Some name -> Basic_io.write (name ^ ".tokens") str
  | None -> print_endline str

let debug_segment_info name source =
  let diagnostics = Diagnostics.make () in
  let text_segs = Text_segment.of_string ~name:"" source in
  let slice_points =
    Lst.concat_map text_segs (fun text_segment ->
        let docstrings = Basic_vec.empty () in
        let tokens =
          Unicode_lex.tokens_of_segment ~diagnostics ~docstrings ~comment:true
            text_segment
        in
        let segments = Toplevel_segments.toplevel_segments tokens in
        Lst.map segments (fun seg -> (Segment.get_start_pos seg).pos_lnum))
  in
  let codes = String.split_on_char '\n' source in
  let divider = "       +------------------------\n" in
  let buf = Buffer.create 1024 in
  let add_line line code =
    Buffer.add_string buf (Printf.sprintf "%6d | %s\n" line code)
  in
  let rec loop slice_points current_line = function
    | [] -> Buffer.contents buf
    | code :: codes ->
        let slice_points =
          match slice_points with
          | point :: points when point = current_line ->
              Buffer.add_string buf divider;
              points
          | points -> points
        in
        add_line current_line code;
        loop slice_points (current_line + 1) codes
  in
  let str = loop slice_points 1 codes in
  match name with
  | Some name -> Basic_io.write (name ^ ".segments") str
  | None -> print_endline str

let impl_of_string ~diagnostics ?name ?(debug_tokens = false)
    ?(debug_segments = false) ?directive_handler ~transform source =
  let text_segs = Text_segment.of_string ?name source in
  let directive = ref [] in
  let pairs =
    let first = ref true in
    Basic_lst.map text_segs (fun text_segment ->
        let docstrings = Basic_vec.empty () in
        let tokens =
          Unicode_lex.tokens_of_segment ~diagnostics ~docstrings ~comment:true
            text_segment
        in
        let segments = Toplevel_segments.toplevel_segments tokens in
        if !first then (
          first := false;
          match (segments, directive_handler) with
          | seg :: _, Some handler ->
              let ds = Header_parser.parse seg in
              directive := ds;
              handler ds
          | _ -> ());
        let ast =
          impl_of_segments ~diagnostics ?name ~transform ~docstrings segments
        in
        (ast, tokens))
  in
  let tokens_list = Lst.map pairs snd in
  if debug_segments then debug_segment_info name source;
  if debug_tokens then debug_tokens_info name tokens_list;
  let ast = List.concat_map fst pairs in
  let pkg = !Basic_config.current_package in
  {
    ast;
    directive = !directive;
    tokens_list;
    name = Option.value ~default:"" name;
    pkg;
  }

let parse ~diagnostics ?(debug_tokens = false) ?(debug_segments = false)
    ?directive_handler ~transform path =
  impl_of_string ~diagnostics ~debug_tokens ~debug_segments ~transform
    ?directive_handler ~name:(Filename.basename path)
    (In_channel.with_open_bin path In_channel.input_all)
