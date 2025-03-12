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


module Parser_core = Parsing_core
module Parser_util = Parsing_util
module Compact = Parsing_compact
module Syntax = Parsing_syntax
module Menhir_token = Lex_menhir_token
module Longident = Basic_longident
module Vec = Basic_vec
module Lst = Basic_lst

type parse_state = Parser_core.parse_state
type position = Parser_core.position
type token = Menhir_token.token

type token_kind = Menhir_token.token_kind =
  | Token_kind : _ Menhir_token.terminal -> token_kind
[@@unboxed]

let loc_of_expression = Syntax.loc_of_expression
and loc_of_pattern = Syntax.loc_of_pattern
and loc_of_type_expression = Syntax.loc_of_type_expression

let add_error = Parser_core.add_error
and add_error_failed_to_parse = Parser_core.add_error_failed_to_parse
and add_error_skipped = Parser_core.add_error_skipped
and add_error_unexpected = Parser_core.add_error_unexpected
and debugged = Parser_core.debugged
and expected_dot_int = Parser_core.expected_dot_int
and expected_dot_lident = Parser_core.expected_dot_lident
and expected_dot_uident = Parser_core.expected_dot_uident
and expected_lident = Parser_core.expected_lident
and expected_uident = Parser_core.expected_uident
and expected_pkg_name = Parser_core.expected_pkg_name
and expected_int = Parser_core.expected_int
and expected_string = Parser_core.expected_string
and expected_token = Parser_core.expected_token
and get_mode = Parser_core.get_mode
and loc_start_with = Parser_core.loc_start_with
and located = Parser_core.located
and option = Parser_core.option
and parser_panic = Parser_core.parser_panic
and peek_location = Parser_core.peek_location
and peek_loc_start = Parser_core.peek_loc_start
and peek_loc_end = Parser_core.peek_loc_end
and peek_nth_token = Parser_core.peek_nth_token
and peek_nth_token_kind = Parser_core.peek_nth_token_kind
and peek_token = Parser_core.peek_token
and get_absolute_loc = Parser_core.get_absolute_loc
and pop_sync = Parser_core.pop_sync
and push_sync = Parser_core.push_sync
and sepby = Parser_core.sepby
and sepby1 = Parser_core.sepby1
and sepby_with_follow = Parser_core.sepby_with_follow
and skip_this = Parser_core.skip_this
and surround = Parser_core.surround
and surround_sepby = Parser_core.surround_sepby
and with_follow = Parser_core.with_follow
and with_sync = Parser_core.with_sync
and with_syncs = Parser_core.with_syncs

let first_qual_lident : token_kind list =
  [ Token_kind T_LIDENT; Token_kind T_PACKAGE_NAME ]

let first_qual_luident : token_kind list =
  [ Token_kind T_LIDENT; Token_kind T_UIDENT; Token_kind T_PACKAGE_NAME ]

let first_luident : token_kind list =
  [ Token_kind T_LIDENT; Token_kind T_UIDENT ]

let first_constr : token_kind list =
  [ Token_kind T_LIDENT; Token_kind T_UIDENT; Token_kind T_PACKAGE_NAME ]

let first_impl : token_kind list =
  [
    Token_kind T_PUB;
    Token_kind T_PRIV;
    Token_kind T_TYPE;
    Token_kind T_TYPEALIAS;
    Token_kind T_ASYNC;
    Token_kind T_FN;
    Token_kind T_STRUCT;
    Token_kind T_ENUM;
    Token_kind T_LET;
    Token_kind T_CONST;
    Token_kind T_EXTERN;
    Token_kind T_TEST;
    Token_kind T_IMPL;
    Token_kind T_TRAIT;
    Token_kind T_TRAITALIAS;
  ]

let first_simple_expr : token_kind list =
  [
    Token_kind T_TRUE;
    Token_kind T_FALSE;
    Token_kind T_INT;
    Token_kind T_FLOAT;
    Token_kind T_CHAR;
    Token_kind T_STRING;
    Token_kind T_MULTILINE_STRING;
    Token_kind T_BYTE;
    Token_kind T_INTERP;
    Token_kind T_PACKAGE_NAME;
    Token_kind T_ASYNC;
    Token_kind T_FN;
    Token_kind T_LIDENT;
    Token_kind T_UIDENT;
    Token_kind T_LPAREN;
    Token_kind T_LBRACKET;
    Token_kind T_LBRACE;
    Token_kind T_UNDERSCORE;
  ]

let first_expr : token_kind list =
  [
    Token_kind T_IF;
    Token_kind T_MATCH;
    Token_kind T_PLUS;
    Token_kind T_MINUS;
    Token_kind T_LOOP;
    Token_kind T_WHILE;
    Token_kind T_FOR;
    Token_kind T_POST_LABEL;
    Token_kind T_GUARD;
    Token_kind T_TRY;
  ]
  @ first_simple_expr

let first_map_pattern_key : token_kind list =
  [
    Token_kind T_TRUE;
    Token_kind T_FALSE;
    Token_kind T_CHAR;
    Token_kind T_INT;
    Token_kind T_FLOAT;
    Token_kind T_MINUS;
    Token_kind T_STRING;
  ]

let first_type : token_kind list =
  [
    Token_kind T_PACKAGE_NAME;
    Token_kind T_LIDENT;
    Token_kind T_UIDENT;
    Token_kind T_LPAREN;
    Token_kind T_UNDERSCORE;
  ]

let parse_optional_bang (state : parse_state) =
  (match peek_token state with
   | EXCLAMATION ->
       let loc = peek_location state in
       skip_this state;
       Some loc
   | _ -> None
    : Rloc.t option)

let rec parse_attributes (state : parse_state) =
  (match peek_token state with
   | ATTRIBUTE content ->
       let loc_ = peek_location state in
       skip_this state;
       Parsing_util.make_attribute ~loc_ content :: parse_attributes state
   | _ -> []
    : Attribute.t list)

let rec parse_toplevel (state : parse_state) =
  (debugged "toplevel" state (fun state ->
       match get_mode state with
       | Normal ->
           let res = ref [] in
           push_sync state (Token_kind T_EOF);
           let rec loop () =
             if get_mode state = Normal && peek_token state <> EOF then (
               let top =
                 with_syncs state first_impl (fun s ->
                     Parsing_core.update_base state;
                     parse_top s)
               in
               res := top :: !res;
               match peek_token state with
               | SEMI _ ->
                   expected_token (Token_kind T_SEMI) state;
                   loop ()
               | PUB | PRIV | TYPE | TYPEALIAS | TRAIT | TRAITALIAS | ASYNC | FN
               | STRUCT | ENUM | LET | CONST | EXTERN | TEST | IMPL ->
                   loop ()
               | EOF ->
                   pop_sync state (Token_kind T_EOF);
                   expected_token (Token_kind T_EOF) state
               | other ->
                   let loc_ = with_syncs state first_impl parser_panic in
                   add_error_unexpected state other first_impl loc_;
                   loop ())
           in
           loop ();
           List.rev !res
       | Panic { loc; _ } ->
           add_error_skipped state "toplevel" loc;
           [])
    : Syntax.impl list)

and parse_expression ~diagnostics segment =
  (let state = Parser_core.init_state ~diagnostics segment in
   with_sync state (Token_kind T_EOF) parse_expr
    : Syntax.expr)

and parse_qual_lident (state : parse_state) =
  (debugged "qual_lident" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | LIDENT _ ->
                let id = expected_lident state in
                Lident id
            | PACKAGE_NAME _ ->
                let pkg = expected_pkg_name state in
                let id = expected_dot_lident state in
                Ldot { pkg; id }
            | other ->
                let other_loc = peek_location state in
                add_error_unexpected state other first_qual_lident other_loc;
                let _ = parser_panic state in
                Lident "?qual_lident")
        | Panic { loc; _ } ->
            add_error_skipped state "qual_lident" loc;
            Lident "?qual_lident"
         : Longident.t))
    : Longident.t)

and parse_qual_luident (state : parse_state) =
  (debugged "qual_luident" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | LIDENT _ ->
                let id = expected_lident state in
                Lident id
            | UIDENT _ ->
                let id = expected_uident state in
                Lident id
            | PACKAGE_NAME _ -> (
                let pkg = expected_pkg_name state in
                match peek_token state with
                | DOT_LIDENT id | DOT_UIDENT id ->
                    skip_this state;
                    Ldot { pkg; id }
                | other ->
                    let other_loc = peek_location state in
                    add_error_unexpected state other
                      [ Token_kind T_DOT_LIDENT; Token_kind T_DOT_UIDENT ]
                      other_loc;
                    let _ = parser_panic state in
                    Lident "?qual_luident")
            | other ->
                let other_loc = peek_location state in
                add_error_unexpected state other first_qual_luident other_loc;
                let _ = parser_panic state in
                Lident "?qual_luident")
        | Panic { loc; _ } ->
            add_error_skipped state "qual_luident" loc;
            Lident "?qual_luident"
         : Longident.t))
    : Longident.t)

and parse_luident (state : parse_state) =
  (debugged "luident" state (fun state ->
       match get_mode state with
       | Normal -> (
           match peek_token state with
           | LIDENT _ -> expected_lident state
           | UIDENT _ -> expected_uident state
           | other ->
               let other_loc = peek_location state in
               add_error_unexpected state other first_luident other_loc;
               let _ = parser_panic state in
               "?luident")
       | Panic { loc; _ } ->
           add_error_skipped state "luident" loc;
           "?luident")
    : string)

and parse_binder (state : parse_state) =
  (debugged "binder" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_ = peek_location state in
            let binder_name = expected_lident state in
            { binder_name; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "binder" loc;
            { binder_name = "?binder"; loc_ = loc }
         : Syntax.binder))
    : Syntax.binder)

and parse_type_name (state : parse_state) =
  (debugged "type_name" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            let is_object =
              match peek_token state with
              | AMPER ->
                  skip_this state;
                  true
              | _ -> false
            in
            let name = parse_qual_luident state in
            let loc_ = loc_start_with state loc_start in
            { name; is_object; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "type_name" loc;
            { name = Lident "?type_name"; is_object = false; loc_ = loc }
         : Syntax.type_name))
    : Syntax.type_name)

and parse_fun_binder (state : parse_state) =
  (debugged "fun_binder" state (fun state ->
       match get_mode state with
       | Normal -> (
           let parse_qual_with_coloncolon state =
             let type_name = parse_type_name state in
             expected_token (Token_kind T_COLONCOLON) state;
             let loc_func_name = peek_location state in
             let func_name = expected_lident state in
             ( Some type_name,
               ({ binder_name = func_name; loc_ = loc_func_name }
                 : Syntax.binder) )
           in
           match peek_token state with
           | PACKAGE_NAME _ | UIDENT _ | AMPER ->
               parse_qual_with_coloncolon state
           | LIDENT _ when peek_nth_token_kind state 1 = Token_kind T_COLONCOLON
             ->
               parse_qual_with_coloncolon state
           | LIDENT _ ->
               let id = parse_binder state in
               (None, id)
           | t ->
               let loc_ = parser_panic state in
               add_error_unexpected state t first_qual_luident loc_;
               (None, { binder_name = "?fun_binder"; loc_ }))
       | Panic { loc; _ } ->
           add_error_skipped state "fun_binder" loc;
           (None, { binder_name = "?fun_binder"; loc_ = loc }))
    : Syntax.type_name option * Syntax.binder)

and parse_var (state : parse_state) =
  (debugged "var" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            let var_name = parse_qual_lident state in
            let loc_ = loc_start_with state loc_start in
            { var_name; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "var" loc;
            { var_name = Lident "?qual_lident"; loc_ = loc }
         : Syntax.var))
    : Syntax.var)

and parse_tvar_binder (state : parse_state) =
  (debugged "tvar_binder" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            let tvar_name = parse_luident state in
            let loc_ = loc_start_with state loc_start in
            let tvar_constraints =
              option [ Token_kind T_COLON ]
                (fun state ->
                  expected_token (Token_kind T_COLON) state;
                  (sepby1 (Token_kind T_PLUS) (fun state ->
                       (let loc_ = peek_location state in
                        let tvc_trait : Longident.t =
                          match peek_token state with
                          | UIDENT _ -> (
                              let id = expected_uident state in
                              match peek_token state with
                              | QUESTION ->
                                  skip_this state;
                                  Lident (id ^ "?")
                              | _ -> Lident id)
                          | _ -> parse_qual_luident state
                        in
                        { tvc_trait; loc_ }
                         : Syntax.tvar_constraint)))
                    state)
                state
            in
            let tvar_constraints = Option.value ~default:[] tvar_constraints in
            { tvar_name; tvar_constraints; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "tvar binder" loc;
            { tvar_name = "?tvar_name"; tvar_constraints = []; loc_ = loc }
         : Syntax.tvar_binder))
    : Syntax.tvar_binder)

and parse_type_decl_binder (state : parse_state) =
  (debugged "type_decl_binder" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            let tvar_name =
              match peek_token state with
              | UNDERSCORE ->
                  skip_this state;
                  None
              | _ -> Some (parse_luident state)
            in
            let loc_ = loc_start_with state loc_start in
            { tvar_name; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "tvar binder" loc;
            { tvar_name = None; loc_ = loc }
         : Syntax.type_decl_binder))
    : Syntax.type_decl_binder)

and parse_constr (state : parse_state) =
  (debugged "constr" state (fun state ->
       (match get_mode state with
        | Normal -> (
            let loc_start = peek_loc_start state in
            match peek_token state with
            | UIDENT _
              when peek_nth_token_kind state 1 <> Token_kind T_COLONCOLON ->
                let loc_ = peek_location state in
                let str = parse_luident state in
                let loc_constr = loc_start_with state loc_start in
                {
                  constr_name = { name = str; loc_ };
                  extra_info = No_extra_info;
                  loc_ = loc_constr;
                }
            | PACKAGE_NAME pkg
              when peek_nth_token_kind state 1 = Token_kind T_DOT_UIDENT
                   && peek_nth_token_kind state 2 <> Token_kind T_COLONCOLON ->
                skip_this state;
                let constr_name : Syntax.constr_name =
                  let loc_start = peek_loc_start state in
                  let name = Parser_core.expected_dot_uident state in
                  { name; loc_ = loc_start_with state loc_start }
                in
                let loc_ = loc_start_with state loc_start in
                { constr_name; extra_info = Package pkg; loc_ }
            | LIDENT _ | UIDENT _ | PACKAGE_NAME _ | AMPER ->
                let type_name = parse_type_name state in
                expected_token (Token_kind T_COLONCOLON) state;
                let right_loc = peek_location state in
                let right = Parser_core.expected_uident ~default:"" state in
                let loc_ = loc_start_with state loc_start in
                {
                  constr_name = { name = right; loc_ = right_loc };
                  extra_info = Type_name type_name;
                  loc_;
                }
            | other ->
                let other_loc = peek_location state in
                add_error_unexpected state other first_constr other_loc;
                let loc_ = parser_panic state in
                {
                  constr_name = { name = "?constr"; loc_ };
                  extra_info = No_extra_info;
                  loc_;
                })
        | Panic { loc; _ } ->
            add_error_skipped state "constr" loc;
            {
              constr_name = { name = "?constr"; loc_ = loc };
              extra_info = No_extra_info;
              loc_ = loc;
            }
         : Syntax.constructor))
    : Syntax.constructor)

and parse_opt_typ_params (state : parse_state) =
  (debugged "typ_params" state (fun state ->
       match get_mode state with
       | Normal -> (
           let res =
             option [ Token_kind T_LBRACKET ]
               (Parsing_core.surround_sepby (Token_kind T_LBRACKET)
                  (Token_kind T_RBRACKET) (Token_kind T_COMMA) parse_tvar_binder)
               state
           in
           match res with Some xs -> xs | None -> [])
       | Panic { loc; _ } ->
           add_error_skipped state "typ_params" loc;
           [])
    : Syntax.tvar_binder list)

and parse_opt_typ_params_no_constraints (state : parse_state) =
  (debugged "typ_params" state (fun state ->
       match get_mode state with
       | Normal -> (
           let res =
             option [ Token_kind T_LBRACKET ]
               (surround_sepby (Token_kind T_LBRACKET) (Token_kind T_RBRACKET)
                  (Token_kind T_COMMA) parse_type_decl_binder)
               state
           in
           match res with Some xs -> xs | None -> [])
       | Panic { loc; _ } ->
           add_error_skipped state "typ_params" loc;
           [])
    : Syntax.type_decl_binder list)

and parse_local_type_decl (state : parse_state) =
  (debugged "local_type_decl" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | STRUCT ->
                expected_token (Token_kind T_STRUCT) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let fs =
                  Parsing_core.surround_sepby
                    ~invalid_delims:[ Token_kind T_COMMA ] (Token_kind T_LBRACE)
                    (Token_kind T_RBRACE) (Token_kind T_SEMI) parse_field_decl
                    state
                in
                let deriving_ = parse_deriving_directive_list state in
                {
                  local_tycon = tycon;
                  local_tycon_loc_ = tycon_loc_;
                  local_components = Ptd_record fs;
                  deriving_;
                }
            | ENUM ->
                expected_token (Token_kind T_ENUM) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let cs =
                  surround_sepby ~invalid_delims:[ Token_kind T_COMMA ]
                    (Token_kind T_LBRACE) (Token_kind T_RBRACE)
                    (Token_kind T_SEMI) parse_constr_decl state
                in
                let deriving_ = parse_deriving_directive_list state in
                {
                  local_tycon = tycon;
                  local_tycon_loc_ = tycon_loc_;
                  local_components = Ptd_variant cs;
                  deriving_;
                }
            | TYPE ->
                expected_token (Token_kind T_TYPE) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let typ = parse_type state in
                let deriving_ = parse_deriving_directive_list state in
                {
                  local_tycon = tycon;
                  local_tycon_loc_ = tycon_loc_;
                  local_components = Ptd_newtype typ;
                  deriving_;
                }
            | other ->
                let other_loc = peek_location state in
                add_error_unexpected state other
                  [ Token_kind T_STRUCT; Token_kind T_ENUM ]
                  other_loc;
                let loc_ = parser_panic state in
                {
                  local_tycon = "?local_tycon";
                  local_tycon_loc_ = loc_;
                  local_components = Ptd_variant [];
                  deriving_ = [];
                })
        | Panic { loc; _ } ->
            add_error_skipped state "local_type_decl" loc;
            {
              local_tycon = "?local_tycon";
              local_tycon_loc_ = loc;
              local_components = Ptd_variant [];
              deriving_ = [];
            }
         : Syntax.local_type_decl))
    : Syntax.local_type_decl)

and parse_fun_decl ~attrs (state : parse_state) =
  (debugged "fun_decl" state (fun state ->
       (match get_mode state with
        | Normal ->
            let is_async =
              match peek_token state with
              | ASYNC ->
                  skip_this state;
                  true
              | _ -> false
            in
            expected_token (Menhir_token.kind_of_token FN) state;
            let type_name, name = parse_fun_binder state in
            let has_error = parse_optional_bang state in
            let quantifiers = parse_opt_typ_params state in
            let param_loc_start = peek_loc_start state in
            let decl_params =
              option [ Token_kind T_LPAREN ]
                (surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                   (Token_kind T_COMMA) parse_parameter)
                state
            in
            let params_loc_ = loc_start_with state param_loc_start in
            let return_type =
              option
                [ Token_kind T_THIN_ARROW ]
                (fun state ->
                  expected_token (Token_kind T_THIN_ARROW) state;
                  let res = parse_return_type state in
                  res)
                state
            in
            {
              name;
              has_error;
              is_async;
              type_name;
              quantifiers;
              params_loc_;
              decl_params;
              return_type;
              vis = Vis_default;
              doc_ = Docstring.empty;
              attrs;
            }
        | Panic { loc; _ } ->
            add_error_skipped state "fun_decl" loc;
            {
              name = { binder_name = "???"; loc_ = loc };
              has_error = None;
              is_async = false;
              type_name = None;
              decl_params = Some [];
              params_loc_ = loc;
              quantifiers = [];
              return_type = None;
              vis = Vis_default;
              doc_ = Docstring.empty;
              attrs;
            }
         : Syntax.fun_decl))
    : Syntax.fun_decl)

and parse_trait_method_decl (state : parse_state) =
  (debugged "trait_method_decl" state (fun state ->
       match get_mode state with
       | Normal ->
           let loc_start = Parser_core.peek_loc_start state in
           push_sync state (Token_kind T_LPAREN);
           let func_name = parse_binder state in
           let has_error = parse_optional_bang state in
           let quantifiers = parse_opt_typ_params state in
           pop_sync state (Token_kind T_LPAREN);
           expected_token (Token_kind T_LPAREN) state;
           let params, _ =
             sepby_with_follow (Token_kind T_COMMA) [ Token_kind T_RPAREN ]
               (fun state ->
                 (match peek_token state with
                  | POST_LABEL label_name as tok ->
                      let label : Syntax.label =
                        let label_loc =
                          match tok with
                          | POST_LABEL _ ->
                              Rloc.trim_last_char (peek_location state)
                          | _ -> assert false
                        in
                        skip_this state;
                        { label_name; loc_ = label_loc }
                      in
                      expected_token (Token_kind T_COLON) state;
                      let typ = parse_type state in
                      { tmparam_label = Some label; tmparam_typ = typ }
                  | _ ->
                      let typ = parse_type state in
                      { tmparam_label = None; tmparam_typ = typ }
                   : Syntax.trait_method_param))
               state
           in
           expected_token (Token_kind T_RPAREN) state;
           let return_type =
             match peek_token state with
             | THIN_ARROW ->
                 expected_token (Token_kind T_THIN_ARROW) state;
                 let t = parse_return_type state in
                 Some t
             | _ -> None
           in
           let loc_ = loc_start_with state loc_start in
           Syntax.Trait_method
             {
               name = func_name;
               has_error = (match has_error with Some _ -> true | _ -> false);
               quantifiers;
               params;
               return_type;
               loc_;
             }
       | Panic { loc; _ } ->
           add_error_skipped state "trait_method_decl" loc;
           Trait_method
             {
               name = { binder_name = "?"; loc_ = loc };
               has_error = false;
               quantifiers = [];
               params = [];
               return_type = None;
               loc_ = loc;
             })
    : Syntax.trait_method_decl)

and parse_trait_decl ~attrs ~(vis : Syntax.visibility) (state : parse_state) =
  (debugged "trait_decl" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            expected_token (Token_kind T_TRAIT) state;
            let name = parse_luident state in
            let loc_ = peek_location state in
            let supers =
              match peek_token state with
              | COLON ->
                  skip_this state;
                  with_follow [ Token_kind T_LBRACE ]
                    (sepby1 (Token_kind T_PLUS) (fun state ->
                         (let loc_ = peek_location state in
                          let tvc_trait = parse_qual_luident state in
                          { tvc_trait; loc_ }
                           : Syntax.tvar_constraint)))
                    state
              | _ -> []
            in
            expected_token (Token_kind T_LBRACE) state;
            let methods, _ =
              sepby_with_follow (Token_kind T_SEMI) [ Token_kind T_RBRACE ]
                parse_trait_method_decl state
            in
            expected_token (Token_kind T_RBRACE) state;
            let trait_loc_ = loc_start_with state loc_start in
            let trait_name : Syntax.binder = { binder_name = name; loc_ } in
            {
              trait_name;
              trait_supers = supers;
              trait_methods = methods;
              trait_vis = vis;
              trait_loc_ = get_absolute_loc state trait_loc_;
              trait_doc_ = Docstring.empty;
              trait_attrs = attrs;
            }
        | Panic { loc; _ } ->
            add_error_skipped state "trait_decl" loc;
            {
              trait_name = { binder_name = "?"; loc_ = loc };
              trait_supers = [];
              trait_methods = [];
              trait_vis = Vis_default;
              trait_loc_ = get_absolute_loc state loc;
              trait_doc_ = Docstring.empty;
              trait_attrs = attrs;
            }
         : Syntax.trait_decl))
    : Syntax.trait_decl)

and parse_visibility (state : parse_state) =
  (debugged "visibility" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | PRIV ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_PRIV) state;
                Vis_priv { loc_ }
            | PUB ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_PUB) state;
                let pub_attr =
                  option [ Token_kind T_LPAREN ]
                    (surround (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                       (fun state ->
                         match peek_token state with
                         | READONLY ->
                             skip_this state;
                             "readonly"
                         | LIDENT attr ->
                             skip_this state;
                             attr
                         | tok ->
                             let loc = peek_location state in
                             let _ = parser_panic state in
                             add_error_unexpected state tok
                               [ Token_kind T_READONLY; Token_kind T_LIDENT ]
                               loc;
                             ""))
                    state
                in
                let loc_ = loc_start_with state loc_start in
                Vis_pub { attr = pub_attr; loc_ }
            | _ -> Vis_default)
        | Panic { loc; _ } ->
            add_error_skipped state "visibility" loc;
            Vis_default
         : Syntax.visibility))
    : Syntax.visibility)

and parse_top (state : parse_state) =
  (debugged "top" state (fun state ->
       (match get_mode state with
        | Normal -> (
            let loc_start = peek_loc_start state in
            let attrs = parse_attributes state in
            let vis = parse_visibility state in
            match peek_token state with
            | TYPE when peek_nth_token_kind state 1 = Token_kind T_EXCLAMATION
              ->
                expected_token (Token_kind T_TYPE) state;
                expected_token (Token_kind T_EXCLAMATION) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let components : Syntax.type_desc =
                  match peek_token state with
                  | LBRACE ->
                      let cs =
                        surround_sepby ~invalid_delims:[ Token_kind T_COMMA ]
                          (Token_kind T_LBRACE) (Token_kind T_RBRACE)
                          (Token_kind T_SEMI) parse_constr_decl state
                      in
                      Ptd_error (Enum_payload cs)
                  | _ ->
                      let typ = option first_type parse_type state in
                      let exception_decl : Syntax.exception_decl =
                        match typ with
                        | Some ty -> Single_payload ty
                        | None -> No_payload
                      in
                      Ptd_error exception_decl
                in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params = [];
                    components;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | TYPE ->
                expected_token (Token_kind T_TYPE) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let params = parse_opt_typ_params_no_constraints state in
                let components : Syntax.type_desc =
                  match peek_token state with
                  | SEMI _ | EOF | DERIVE -> Ptd_abstract
                  | _ ->
                      let typ = parse_type state in
                      Ptd_newtype typ
                in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params;
                    components;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | TYPEALIAS ->
                skip_this state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let params = parse_opt_typ_params_no_constraints state in
                expected_token (Token_kind T_EQUAL) state;
                let typ = parse_type state in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params;
                    components = Ptd_alias typ;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | STRUCT ->
                expected_token (Token_kind T_STRUCT) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let params = parse_opt_typ_params_no_constraints state in
                let fs =
                  surround_sepby ~invalid_delims:[ Token_kind T_COMMA ]
                    (Token_kind T_LBRACE) (Token_kind T_RBRACE)
                    (Token_kind T_SEMI) parse_field_decl state
                in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params;
                    components = Ptd_record fs;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | ENUM ->
                expected_token (Token_kind T_ENUM) state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let params = parse_opt_typ_params_no_constraints state in
                let cs =
                  surround_sepby ~invalid_delims:[ Token_kind T_COMMA ]
                    (Token_kind T_LBRACE) (Token_kind T_RBRACE)
                    (Token_kind T_SEMI) parse_constr_decl state
                in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params;
                    components = Ptd_variant cs;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | LET | CONST ->
                let is_constant, binder =
                  match peek_token state with
                  | LET ->
                      skip_this state;
                      (false, parse_binder state)
                  | CONST ->
                      skip_this state;
                      let loc_ = peek_location state in
                      let binder_name = expected_uident state in
                      (true, { binder_name; loc_ })
                  | _ -> assert false
                in
                let ty = parse_opt_annot state in
                expected_token (Token_kind T_EQUAL) state;
                let expr = parse_expr state in
                let loc_ = loc_start_with state loc_start in
                Ptop_letdef
                  {
                    binder;
                    ty;
                    expr;
                    is_constant;
                    vis;
                    loc_ = get_absolute_loc state loc_;
                    doc_ = Docstring.empty;
                    attrs;
                  }
            | EXTERN when peek_nth_token_kind state 1 = Token_kind T_TYPE ->
                skip_this state;
                skip_this state;
                let tycon, tycon_loc_ =
                  let loc_ = peek_location state in
                  let res = parse_luident state in
                  (res, loc_)
                in
                let params = parse_opt_typ_params_no_constraints state in
                let deriving_ = parse_deriving_directive_list state in
                let loc_ = loc_start_with state loc_start in
                Ptop_typedef
                  {
                    tycon;
                    tycon_loc_;
                    params;
                    components = Ptd_extern;
                    type_vis = vis;
                    doc_ = Docstring.empty;
                    deriving_;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                  }
            | EXTERN -> (
                expected_token (Token_kind T_EXTERN) state;
                let language = expected_string state in
                let fun_decl = parse_fun_decl ~attrs state in
                let fun_decl = { fun_decl with vis } in
                expected_token (Token_kind T_EQUAL) state;
                match peek_token state with
                | MULTILINE_STRING _ ->
                    let rec loop xs =
                      match peek_token state with
                      | MULTILINE_STRING x ->
                          expected_token (Token_kind T_MULTILINE_STRING) state;
                          loop (x :: xs)
                      | _ -> xs
                    in
                    let xs = loop [] in
                    let loc_ = loc_start_with state loc_start in
                    Ptop_funcdef
                      {
                        fun_decl;
                        decl_body =
                          Decl_stubs
                            (Embedded
                               {
                                 language = Some language;
                                 code = Code_multiline_string (List.rev xs);
                               });
                        loc_ = get_absolute_loc state loc_;
                      }
                | _ ->
                    let body = expected_string state in
                    let loc_ = loc_start_with state loc_start in
                    Ptop_funcdef
                      {
                        fun_decl;
                        decl_body =
                          Decl_stubs
                            (Embedded
                               {
                                 language = Some language;
                                 code = Code_string body;
                               });
                        loc_ = get_absolute_loc state loc_;
                      })
            | ASYNC | FN -> (
                let fun_decl = parse_fun_decl ~attrs state in
                let fun_decl = { fun_decl with vis } in
                match peek_token state with
                | EQUAL -> (
                    expected_token (Token_kind T_EQUAL) state;
                    match peek_token state with
                    | MULTILINE_STRING _ ->
                        let rec loop xs =
                          match peek_token state with
                          | MULTILINE_STRING x ->
                              expected_token (Token_kind T_MULTILINE_STRING)
                                state;
                              loop (x :: xs)
                          | _ -> xs
                        in
                        let xs = loop [] in
                        let loc_ = loc_start_with state loc_start in
                        Ptop_funcdef
                          {
                            fun_decl;
                            decl_body =
                              Decl_stubs
                                (Embedded
                                   {
                                     language = None;
                                     code = Code_multiline_string (List.rev xs);
                                   });
                            loc_ = get_absolute_loc state loc_;
                          }
                    | _ -> (
                        let name1 = expected_string state in
                        match peek_token state with
                        | STRING name2 ->
                            let _ = expected_string state in
                            let loc_ = loc_start_with state loc_start in
                            Ptop_funcdef
                              {
                                fun_decl;
                                decl_body =
                                  Decl_stubs
                                    (Import
                                       {
                                         module_name = name1;
                                         func_name = name2;
                                       });
                                loc_ = get_absolute_loc state loc_;
                              }
                        | _ ->
                            let loc_ = loc_start_with state loc_start in
                            Ptop_funcdef
                              {
                                fun_decl;
                                decl_body =
                                  Decl_stubs
                                    (Embedded
                                       {
                                         language = None;
                                         code = Code_string name1;
                                       });
                                loc_ = get_absolute_loc state loc_;
                              }))
                | _ ->
                    let local_types, body =
                      parse_block_expr_with_local_types state
                    in
                    let loc_ = loc_start_with state loc_start in
                    Ptop_funcdef
                      {
                        fun_decl;
                        decl_body = Decl_body { expr = body; local_types };
                        loc_ = get_absolute_loc state loc_;
                      })
            | TRAIT -> Ptop_trait (parse_trait_decl ~attrs ~vis state)
            | TRAITALIAS ->
                skip_this state;
                let binder : Syntax.binder =
                  let loc_ = peek_location state in
                  let binder_name = parse_luident state in
                  { binder_name; loc_ }
                in
                expected_token (Token_kind T_EQUAL) state;
                let target = parse_type_name state in
                let loc_ = loc_start_with state loc_start in
                Ptop_trait_alias
                  {
                    binder;
                    target;
                    vis;
                    loc_ = get_absolute_loc state loc_;
                    attrs;
                    doc_ = Docstring.empty;
                  }
            | TEST ->
                skip_this state;
                let name =
                  match peek_token state with
                  | STRING _ ->
                      let name_loc = peek_location state in
                      let v = expected_string state in
                      Some ({ v; loc_ = name_loc } : _ Rloc.loced)
                  | _ -> None
                in
                let params =
                  match peek_token state with
                  | LPAREN ->
                      let params =
                        surround_sepby (Token_kind T_LPAREN)
                          (Token_kind T_RPAREN) (Token_kind T_COMMA)
                          parse_parameter state
                      in
                      Some params
                  | _ -> None
                in
                let local_types, expr =
                  parse_block_expr_with_local_types state
                in
                let loc_ = loc_start_with state loc_start in
                Ptop_test
                  {
                    name;
                    params;
                    expr;
                    local_types;
                    loc_ = get_absolute_loc state loc_;
                    doc_ = Docstring.empty;
                    attrs;
                  }
            | IMPL -> (
                skip_this state;
                push_sync state (Token_kind T_WITH);
                push_sync state (Token_kind T_FOR);
                let quantifiers = parse_opt_typ_params state in
                let trait = parse_type_name state in
                pop_sync state (Token_kind T_FOR);
                match (get_mode state, peek_token state) with
                | Normal, FOR ->
                    skip_this state;
                    let self_ty = parse_type state in
                    pop_sync state (Token_kind T_WITH);
                    expected_token (Token_kind T_WITH) state;
                    push_sync state (Token_kind T_LBRACE);
                    let method_name = parse_binder state in
                    let has_error = parse_optional_bang state in
                    let params =
                      surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                        (Token_kind T_COMMA) parse_parameter state
                    in
                    let ret_ty =
                      option
                        [ Token_kind T_THIN_ARROW ]
                        (fun state ->
                          skip_this state;
                          parse_return_type state)
                        state
                    in
                    let header_loc_ = loc_start_with state loc_start in
                    pop_sync state (Token_kind T_LBRACE);
                    let body : Syntax.decl_body =
                      match peek_token state with
                      | EQUAL ->
                          skip_this state;
                          let code = expected_string state in
                          Decl_stubs
                            (Embedded
                               { language = None; code = Code_string code })
                      | _ ->
                          let local_types, body =
                            parse_block_expr_with_local_types state
                          in
                          Decl_body { expr = body; local_types }
                    in
                    Ptop_impl
                      {
                        self_ty = Some self_ty;
                        trait;
                        method_name;
                        has_error =
                          (match has_error with Some _ -> true | _ -> false);
                        quantifiers;
                        params;
                        ret_ty;
                        vis;
                        body;
                        loc_ =
                          get_absolute_loc state
                            (loc_start_with state loc_start);
                        header_loc_;
                        doc_ = Docstring.empty;
                        attrs;
                      }
                | Normal, WITH ->
                    pop_sync state (Token_kind T_WITH);
                    skip_this state;
                    push_sync state (Token_kind T_LBRACE);
                    let method_name = parse_binder state in
                    let has_error = parse_optional_bang state in
                    let params =
                      surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                        (Token_kind T_COMMA) parse_parameter state
                    in
                    let ret_ty =
                      option
                        [ Token_kind T_THIN_ARROW ]
                        (fun state ->
                          skip_this state;
                          parse_return_type state)
                        state
                    in
                    let header_loc_ = loc_start_with state loc_start in
                    pop_sync state (Token_kind T_LBRACE);
                    let body : Syntax.decl_body =
                      match peek_token state with
                      | EQUAL ->
                          skip_this state;
                          let code = expected_string state in
                          Decl_stubs
                            (Embedded
                               { language = None; code = Code_string code })
                      | _ ->
                          let local_types, body =
                            parse_block_expr_with_local_types state
                          in
                          Decl_body { expr = body; local_types }
                    in
                    Ptop_impl
                      {
                        self_ty = None;
                        trait;
                        method_name;
                        has_error =
                          (match has_error with Some _ -> true | _ -> false);
                        quantifiers;
                        params;
                        ret_ty;
                        vis;
                        body;
                        loc_ =
                          get_absolute_loc state
                            (loc_start_with state loc_start);
                        header_loc_;
                        doc_ = Docstring.empty;
                        attrs;
                      }
                | Normal, other ->
                    pop_sync state (Token_kind T_WITH);
                    let other_loc = peek_location state in
                    add_error_unexpected state other
                      [ Token_kind T_FOR; Token_kind T_WITH ]
                      other_loc;
                    let header_loc_ = loc_start_with state loc_start in
                    let _ = parser_panic state in
                    let loc_ = loc_start_with state loc_start in
                    Ptop_impl
                      {
                        self_ty = None;
                        trait;
                        method_name =
                          { binder_name = "*"; loc_ = Rloc.no_location };
                        has_error = false;
                        quantifiers = [];
                        params = [];
                        ret_ty = None;
                        vis;
                        body =
                          Decl_body
                            {
                              expr =
                                Pexpr_hole
                                  {
                                    loc_ = Rloc.no_location;
                                    kind = Synthesized;
                                  };
                              local_types = [];
                            };
                        loc_ = get_absolute_loc state loc_;
                        header_loc_;
                        doc_ = Docstring.empty;
                        attrs;
                      }
                | Panic { loc = loc_; _ }, _ ->
                    pop_sync state (Token_kind T_WITH);
                    Ptop_expr
                      {
                        expr = Pexpr_hole { loc_; kind = Synthesized };
                        is_main = false;
                        local_types = [];
                        loc_ = get_absolute_loc state loc_;
                      })
            | other ->
                let other_loc = peek_location state in
                add_error_unexpected state other first_impl other_loc;
                let loc_ = parser_panic state in
                Ptop_expr
                  {
                    expr = Pexpr_hole { loc_; kind = Synthesized };
                    is_main = false;
                    local_types = [];
                    loc_ = get_absolute_loc state loc_;
                  })
        | Panic { loc; _ } ->
            add_error_skipped state "impl" loc;
            Ptop_expr
              {
                expr = Pexpr_hole { loc_ = loc; kind = Synthesized };
                is_main = false;
                local_types = [];
                loc_ = get_absolute_loc state loc;
              }
         : Syntax.impl))
    : Syntax.impl)

and parse_deriving_directive_list (state : parse_state) =
  (debugged "deriving_directive_list" state (fun state ->
       match get_mode state with
       | Normal -> (
           match peek_token state with
           | DERIVE ->
               skip_this state;
               surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                 (Token_kind T_COMMA) parse_deriving_directive state
           | _ -> [])
       | Panic { loc; _ } ->
           add_error_skipped state "deriving_directive" loc;
           [])
    : Syntax.deriving_directive list)

and parse_deriving_directive (state : parse_state) =
  (let start_loc = peek_loc_start state in
   let type_name_ = parse_type_name state in
   let args =
     match peek_token state with
     | LPAREN ->
         surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
           (Token_kind T_COMMA) parse_argument state
     | _ -> []
   in
   { type_name_; args; loc_ = loc_start_with state start_loc }
    : Syntax.deriving_directive)

and parse_field_decl (state : parse_state) =
  (debugged "field_decl" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            let vis = parse_visibility state in
            let mutflag =
              option [ Token_kind T_MUTABLE ]
                (expected_token (Token_kind T_MUTABLE))
                state
            in
            let name, name_loc =
              let loc_ = peek_location state in
              let res = expected_lident state in
              (res, loc_)
            in
            expected_token (Token_kind T_COLON) state;
            let typ = parse_type state in
            let field_loc_ = loc_start_with state loc_start in
            {
              field_name = { label = name; loc_ = name_loc };
              field_ty = typ;
              field_mut = mutflag <> None;
              field_vis = vis;
              field_loc_;
            }
        | Panic { loc; _ } ->
            add_error_skipped state "field_decl" loc;
            {
              field_name = { label = "?field-name"; loc_ = loc };
              field_ty = Ptype_any { loc_ = loc };
              field_mut = false;
              field_vis = Vis_default;
              field_loc_ = loc;
            }
         : Syntax.field_decl))
    : Syntax.field_decl)

and parse_constr_decl (state : parse_state) =
  (debugged "constr_decl" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            (match peek_token state with
            | PRIV | PUB | READONLY ->
                let loc = peek_location state in
                skip_this state;
                add_error state (Errors.enum_no_individual_visibility loc)
            | _ -> ());
            let constr_name_loc = peek_location state in
            let name = expected_uident state in
            let constr_name : Syntax.constr_name =
              { name; loc_ = constr_name_loc }
            in
            let params =
              option [ Token_kind T_LPAREN ]
                (surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                   (Token_kind T_COMMA) (fun state ->
                     (let cparam_mut =
                        match peek_token state with
                        | MUTABLE ->
                            skip_this state;
                            true
                        | _ -> false
                      in
                      match peek_token state with
                      | POST_LABEL label_name as tok ->
                          let label_loc =
                            match tok with
                            | POST_LABEL _ ->
                                Rloc.trim_last_char (peek_location state)
                            | _ -> assert false
                          in
                          let label : Syntax.label =
                            { label_name; loc_ = label_loc }
                          in
                          skip_this state;
                          expected_token (Token_kind T_COLON) state;
                          let typ = parse_type state in
                          {
                            cparam_label = Some label;
                            cparam_mut;
                            cparam_typ = typ;
                          }
                      | _ ->
                          let typ = parse_type state in
                          { cparam_label = None; cparam_mut; cparam_typ = typ }
                       : Syntax.constr_param)))
                state
            in
            let constr_tag =
              match peek_token state with
              | EQUAL ->
                  skip_this state;
                  let tag_loc = peek_location state in
                  let tag = expected_int state in
                  Some (tag, tag_loc)
              | _ -> None
            in
            let constr_loc_ = loc_start_with state loc_start in
            { constr_name; constr_args = params; constr_tag; constr_loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "constr_decl" loc;
            {
              constr_name = { name = "?constr-name"; loc_ = loc };
              constr_args = None;
              constr_tag = None;
              constr_loc_ = loc;
            }
         : Syntax.constr_decl))
    : Syntax.constr_decl)

and parse_pattern (state : parse_state) =
  (debugged "pattern" state (fun state ->
       match get_mode state with
       | Normal ->
           let loc_start = peek_loc_start state in
           let pat = parse_or_pattern state in
           let rec loop pat =
             match peek_token state with
             | AS ->
                 expected_token (Token_kind T_AS) state;
                 let binder = parse_binder state in
                 let loc_ = loc_start_with state loc_start in
                 loop (Syntax.Ppat_alias { pat; alias = binder; loc_ })
             | _ -> pat
           in
           loop pat
       | Panic { loc; _ } ->
           add_error_skipped state "pattern" loc;
           Ppat_any { loc_ = loc })
    : Syntax.pattern)

and parse_or_pattern (state : parse_state) =
  (debugged "or pattern" state (fun state ->
       match get_mode state with
       | Normal -> (
           let loc_start = peek_loc_start state in
           let pat = parse_range_pattern state in
           match peek_token state with
           | BAR ->
               expected_token (Token_kind T_BAR) state;
               let pat2 = parse_or_pattern state in
               let loc_ = loc_start_with state loc_start in
               Syntax.Ppat_or { pat1 = pat; pat2; loc_ }
           | _ -> pat)
       | Panic { loc; _ } ->
           add_error_skipped state "pattern" loc;
           Ppat_any { loc_ = loc })
    : Syntax.pattern)

and parse_range_pattern (state : parse_state) =
  (debugged "range pattern" state (fun state ->
       match get_mode state with
       | Normal -> (
           let loc_start = peek_loc_start state in
           let lhs = parse_simple_pattern state in
           match peek_token state with
           | RANGE_INCLUSIVE ->
               skip_this state;
               let rhs = parse_simple_pattern state in
               let loc_ = loc_start_with state loc_start in
               Syntax.Ppat_range { lhs; rhs; inclusive = true; loc_ }
           | RANGE_EXCLUSIVE ->
               skip_this state;
               let rhs = parse_simple_pattern state in
               let loc_ = loc_start_with state loc_start in
               Syntax.Ppat_range { lhs; rhs; inclusive = false; loc_ }
           | _ -> lhs)
       | Panic { loc; _ } ->
           add_error_skipped state "pattern" loc;
           Ppat_any { loc_ = loc })
    : Syntax.pattern)

and parse_simple_pattern (state : parse_state) =
  (debugged "simple_pattern" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | TRUE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_TRUE) state;
                Ppat_constant { c = Const_bool true; loc_ }
            | FALSE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_FALSE) state;
                Ppat_constant { c = Const_bool false; loc_ }
            | CHAR x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_CHAR) state;
                Ppat_constant { c = Const_char x; loc_ }
            | INT x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INT) state;
                Ppat_constant { c = Parser_util.make_int x; loc_ }
            | BYTE x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BYTE) state;
                Ppat_constant { c = Const_byte x; loc_ }
            | FLOAT x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_FLOAT) state;
                Ppat_constant { c = Const_double x; loc_ }
            | MINUS -> (
                expected_token (Token_kind T_MINUS) state;
                match peek_token state with
                | INT x ->
                    let loc_ = peek_location state in
                    expected_token (Token_kind T_INT) state;
                    Ppat_constant { c = Parser_util.make_int ("-" ^ x); loc_ }
                | FLOAT x ->
                    let loc_ = peek_location state in
                    expected_token (Token_kind T_FLOAT) state;
                    Ppat_constant { c = Const_double ("-" ^ x); loc_ }
                | other ->
                    let other_loc = peek_location state in
                    add_error_failed_to_parse state other
                      "`-Int` or `-Float` in simple pattern" other_loc;
                    let loc_ = parser_panic state in
                    Ppat_any { loc_ })
            | BYTES x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BYTES) state;
                Ppat_constant { c = Const_bytes x; loc_ }
            | STRING x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_STRING) state;
                Ppat_constant { c = Const_string x; loc_ }
            | UNDERSCORE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_UNDERSCORE) state;
                Ppat_any { loc_ }
            | LIDENT _
              when peek_nth_token_kind state 1 <> Token_kind T_COLONCOLON ->
                let binder = parse_binder state in
                Ppat_var binder
            | PACKAGE_NAME _ | UIDENT _ | LIDENT _ ->
                let loc_start = peek_loc_start state in
                let constr = parse_constr state in
                let maybe_args =
                  option [ Token_kind T_LPAREN ]
                    (surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                       (Token_kind T_COMMA) (fun state ->
                         match peek_token state with
                         | DOTDOT ->
                             let loc = peek_location state in
                             skip_this state;
                             `Dotdot loc
                         | _ -> `Arg (parse_constr_pat_argument state)))
                    state
                in
                let is_open = ref false in
                let args =
                  match maybe_args with
                  | None -> None
                  | Some args ->
                      let rec check_args args =
                        match args with
                        | [] -> []
                        | `Dotdot _ :: [] ->
                            is_open := true;
                            []
                        | `Dotdot loc :: args ->
                            is_open := true;
                            add_error state
                              (Errors.dotdot_in_middle_of_pattern ~kind:`Constr
                                 ~loc);
                            check_args args
                        | `Arg arg :: args -> arg :: check_args args
                      in
                      Some (check_args args)
                in
                let loc_ = loc_start_with state loc_start in
                Ppat_constr { constr; args; is_open = !is_open; loc_ }
            | LPAREN ->
                located state (fun state ->
                    surround (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                      (fun state ->
                        let first = parse_pattern state in
                        match peek_token state with
                        | COMMA -> (
                            expected_token (Token_kind T_COMMA) state;
                            let pats, _ =
                              sepby_with_follow (Token_kind T_COMMA)
                                [ Token_kind T_RPAREN ] parse_pattern state
                            in
                            match pats with
                            | [] -> fun _ -> first
                            | xs ->
                                fun loc_ ->
                                  Ppat_tuple { pats = first :: xs; loc_ })
                        | COLON ->
                            expected_token (Token_kind T_COLON) state;
                            let ty = parse_type state in
                            fun loc_ ->
                              Ppat_constraint { pat = first; ty; loc_ }
                        | RPAREN -> fun _ -> first
                        | other ->
                            let other_loc = peek_location state in
                            add_error_unexpected state other
                              [
                                Token_kind T_COMMA;
                                Token_kind T_COLON;
                                Token_kind T_RPAREN;
                              ]
                              other_loc;
                            let _ = parser_panic state in
                            fun _ -> first)
                      state)
            | LBRACKET ->
                let loc_start = peek_loc_start state in
                let pats1 = Vec.empty () in
                let pats2 = Vec.empty () in
                let dotdot = ref `No_dotdot in
                let _ =
                  surround_sepby (Token_kind T_LBRACKET) (Token_kind T_RBRACKET)
                    (Token_kind T_COMMA)
                    (fun state ->
                      match peek_token state with
                      | DOTDOT -> (
                          let pat_loc_start = peek_loc_start state in
                          skip_this state;
                          let push_pat pat =
                            if !dotdot = `No_dotdot then Vec.push pats1 pat
                            else Vec.push pats2 pat
                          in
                          match peek_token state with
                          | STRING s ->
                              skip_this state;
                              let pat : Syntax.array_pattern =
                                String_spread s
                              in
                              push_pat pat
                          | UIDENT _ ->
                              let id_loc_ = peek_location state in
                              let binder_name = expected_uident state in
                              let binder : Syntax.binder =
                                { binder_name; loc_ = id_loc_ }
                              in
                              let loc_ = loc_start_with state pat_loc_start in
                              let pat : Syntax.array_pattern =
                                String_spread_const { binder; pkg = None; loc_ }
                              in
                              push_pat pat
                          | PACKAGE_NAME pkg ->
                              skip_this state;
                              let id_loc_ = peek_location state in
                              let binder_name = expected_dot_uident state in
                              let binder : Syntax.binder =
                                { binder_name; loc_ = id_loc_ }
                              in
                              let loc_ = loc_start_with state pat_loc_start in
                              let pat : Syntax.array_pattern =
                                String_spread_const
                                  { binder; pkg = Some pkg; loc_ }
                              in
                              push_pat pat
                          | _ -> (
                              match !dotdot with
                              | `Dotdot_no_binder | `Dotdot_with_binder _ ->
                                  add_error state
                                    (Errors.array_pat_multiple_dotdot
                                       (peek_location state))
                              | `No_dotdot -> (
                                  match peek_token state with
                                  | AS ->
                                      expected_token (Token_kind T_AS) state;
                                      let binder = parse_binder state in
                                      dotdot := `Dotdot_with_binder binder
                                  | LIDENT _ ->
                                      let binder = parse_binder state in
                                      dotdot := `Dotdot_with_binder binder
                                  | _ -> dotdot := `Dotdot_no_binder)))
                      | _ ->
                          let pat : Syntax.array_pattern =
                            Pattern (parse_pattern state)
                          in
                          if !dotdot = `No_dotdot then Vec.push pats1 pat
                          else Vec.push pats2 pat)
                    state
                in
                let pats1_list = Vec.to_list pats1 in
                let pats2_list = Vec.to_list pats2 in
                let pats : Syntax.array_patterns =
                  match !dotdot with
                  | `No_dotdot -> Closed pats1_list
                  | `Dotdot_no_binder -> Open (pats1_list, pats2_list, None)
                  | `Dotdot_with_binder b ->
                      Open (pats1_list, pats2_list, Some b)
                in
                let loc_ = loc_start_with state loc_start in
                Ppat_array { pats; loc_ }
            | LBRACE -> (
                let loc_start = peek_loc_start state in
                let kind : [ `Map | `Record | `Unknown ] ref = ref `Unknown in
                expected_token (Token_kind T_LBRACE) state;
                push_sync state (Token_kind T_RBRACE);
                let field_pat_or_dotdots, _ =
                  sepby_with_follow (Token_kind T_COMMA) [ Token_kind T_RBRACE ]
                    (fun state ->
                      let loc_start = peek_loc_start state in
                      let parse_map_elem_pat key =
                        (match !kind with `Unknown -> kind := `Map | _ -> ());
                        skip_this state;
                        let key_loc_ = loc_start_with state loc_start in
                        let match_absent =
                          match peek_token state with
                          | QUESTION ->
                              skip_this state;
                              true
                          | _ -> false
                        in
                        expected_token (Token_kind T_COLON) state;
                        let pat = parse_pattern state in
                        let loc_ = loc_start_with state loc_start in
                        `Map_elem
                          (Syntax.Map_pat_elem
                             { key; pat; match_absent; key_loc_; loc_ })
                          [@@local]
                      in
                      match peek_token state with
                      | DOTDOT ->
                          let loc = peek_location state in
                          skip_this state;
                          `Dotdot loc
                      | TRUE -> parse_map_elem_pat (Const_bool true)
                      | FALSE -> parse_map_elem_pat (Const_bool false)
                      | CHAR x -> parse_map_elem_pat (Const_char x)
                      | INT x -> parse_map_elem_pat (Parser_util.make_int x)
                      | FLOAT x -> parse_map_elem_pat (Const_double x)
                      | MINUS -> (
                          expected_token (Token_kind T_MINUS) state;
                          match peek_token state with
                          | INT x ->
                              parse_map_elem_pat
                                (Parser_util.make_int ("-" ^ x))
                          | FLOAT x ->
                              parse_map_elem_pat (Const_double ("-" ^ x))
                          | other ->
                              let other_loc = peek_location state in
                              add_error_failed_to_parse state other
                                "`-Int` or `-Float` in key of map pattern"
                                other_loc;
                              let _ = parser_panic state in
                              `Error)
                      | STRING x -> parse_map_elem_pat (Const_string x)
                      | LIDENT _ ->
                          (match !kind with
                          | `Unknown -> kind := `Record
                          | _ -> ());
                          `Field (parse_field_pat state)
                      | other ->
                          let expected =
                            match !kind with
                            | `Unknown ->
                                Token_kind T_LIDENT :: first_map_pattern_key
                            | `Map -> first_map_pattern_key
                            | `Record -> [ Token_kind T_LIDENT ]
                          in
                          add_error_unexpected state other expected
                            (peek_location state);
                          let _ = parser_panic state in
                          `Error)
                    state
                in
                pop_sync state (Token_kind T_RBRACE);
                expected_token (Token_kind T_RBRACE) state;
                let loc_ = loc_start_with state loc_start in
                match !kind with
                | `Unknown | `Record ->
                    let rec collect_fields ~has_dotdot acc field_or_dotdots =
                      (match field_or_dotdots with
                       | [] ->
                           Ppat_record
                             {
                               fields = List.rev acc;
                               is_closed = not has_dotdot;
                               loc_;
                             }
                       | `Dotdot loc :: rest ->
                           if rest <> [] then
                             add_error state
                               (Errors.dotdot_in_middle_of_pattern ~kind:`Record
                                  ~loc);
                           collect_fields ~has_dotdot:true acc rest
                       | `Field field_pat :: rest ->
                           collect_fields ~has_dotdot (field_pat :: acc) rest
                       | `Map_elem (Syntax.Map_pat_elem { loc_; _ }) :: rest ->
                           add_error state
                             (Errors.cannot_mix_record_and_map_pat loc_);
                           collect_fields ~has_dotdot acc rest
                       | `Error :: rest -> collect_fields ~has_dotdot acc rest
                        : Syntax.pattern)
                    in
                    collect_fields ~has_dotdot:false [] field_pat_or_dotdots
                | `Map ->
                    let rec collect_elems ~has_dotdot acc field_or_dotdots =
                      (match field_or_dotdots with
                       | [] ->
                           let elems = List.rev acc in
                           Ppat_map { elems; is_closed = not has_dotdot; loc_ }
                       | `Dotdot loc :: rest ->
                           if rest <> [] then
                             add_error state
                               (Errors.dotdot_in_middle_of_pattern ~kind:`Map
                                  ~loc);
                           collect_elems ~has_dotdot:true acc rest
                       | `Field (Syntax.Field_pat { loc_; _ }) :: rest ->
                           add_error state
                             (Errors.cannot_mix_record_and_map_pat loc_);
                           collect_elems ~has_dotdot acc rest
                       | `Error :: rest -> collect_elems ~has_dotdot acc rest
                       | `Map_elem elem :: rest ->
                           collect_elems ~has_dotdot (elem :: acc) rest
                        : Syntax.pattern)
                    in
                    collect_elems ~has_dotdot:false [] field_pat_or_dotdots)
            | other ->
                let other_loc = peek_location state in
                add_error_failed_to_parse state other "simple pattern" other_loc;
                let loc_ = parser_panic state in
                Ppat_any { loc_ })
        | Panic { loc; _ } ->
            add_error_skipped state "simple_pattern" loc;
            Ppat_any { loc_ = loc }
         : Syntax.pattern))
    : Syntax.pattern)

and parse_label (state : parse_state) =
  (debugged "label" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_ = peek_location state in
            let label_name = expected_lident state in
            { label_name; loc_ }
        | Panic { loc; _ } -> { label_name = "?lident"; loc_ = loc }
         : Syntax.label))
    : Syntax.label)

and parse_field_pat (state : parse_state) =
  (debugged "filed_pat" state (fun state ->
       (match get_mode state with
        | Normal -> (
            let loc_start = peek_loc_start state in
            let label = parse_label state in
            match peek_token state with
            | COLON ->
                expected_token (Token_kind T_COLON) state;
                let pattern = parse_pattern state in
                let loc_ = loc_start_with state loc_start in
                Field_pat { label; pattern; is_pun = false; loc_ }
            | _ ->
                let loc_ = loc_start_with state loc_start in
                Field_pat
                  {
                    label;
                    pattern =
                      Ppat_var
                        {
                          Syntax.binder_name = label.label_name;
                          loc_ = label.loc_;
                        };
                    is_pun = true;
                    loc_;
                  })
        | Panic { loc; _ } ->
            Field_pat
              {
                label = { label_name = "?ident"; loc_ = loc };
                pattern = Ppat_any { loc_ = loc };
                is_pun = false;
                loc_ = loc;
              }
         : Syntax.field_pat))
    : Syntax.field_pat)

and parse_constr_pat_argument (state : parse_state) =
  (debugged "argument" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | POST_LABEL label_name ->
                let label : Syntax.label =
                  { label_name; loc_ = peek_location state }
                in
                skip_this state;
                let pat, (kind : Syntax.argument_kind) =
                  match peek_token state with
                  | EQUAL ->
                      skip_this state;
                      let pat = parse_pattern state in
                      add_error state
                        {
                          loc = label.loc_;
                          message =
                            "unexpected `~` in argument. Did you mean \
                             `label=pattern` or `label~`?";
                          error_code = Error_code.invalid_tilde_argument;
                        };
                      (pat, Labelled label)
                  | COMMA | RPAREN ->
                      ( Parser_util.label_to_pat
                          ~loc_:(Rloc.trim_last_char label.loc_)
                          label,
                        Labelled_pun label )
                  | _ ->
                      (Ppat_any { loc_ = peek_location state }, Labelled label)
                in
                Constr_pat_arg { pat; kind }
            | (LIDENT label_name | UIDENT label_name)
              when peek_nth_token_kind state 1 = Token_kind T_EQUAL ->
                let label : Syntax.label =
                  { label_name; loc_ = peek_location state }
                in
                skip_this state;
                skip_this state;
                let pat = parse_pattern state in
                Constr_pat_arg { pat; kind = Labelled label }
            | _ ->
                let pat = parse_pattern state in
                Constr_pat_arg { pat; kind = Positional })
        | Panic { loc; _ } ->
            add_error_skipped state "argument" loc;
            Constr_pat_arg { pat = Ppat_any { loc_ = loc }; kind = Positional }
         : Syntax.constr_pat_arg))
    : Syntax.constr_pat_arg)

and is_binop (tok : token) =
  (match tok with
   | BARBAR | AMPERAMPER | BAR | CARET | AMPER | INFIX1 _ | INFIX2 _ | PLUS
   | MINUS | INFIX3 _ | INFIX4 _ ->
       true
   | _ -> false
    : bool)

and get_binop_prior_assoc (tok : token) =
  (match tok with
   | BARBAR -> (1, `Right)
   | AMPERAMPER -> (2, `Right)
   | BAR -> (4, `Left)
   | CARET -> (5, `Left)
   | AMPER -> (6, `Left)
   | INFIX1 _ -> (7, `Left)
   | INFIX2 _ -> (8, `Left)
   | PLUS | MINUS -> (9, `Left)
   | INFIX3 _ -> (10, `Left)
   | INFIX4 _ -> (11, `Left)
   | _ -> failwith "not a binop!"
    : int * [ `Left | `Right | `Nonassoc ])

and parse_binop (state : parse_state) =
  (debugged "binop" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | BARBAR ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BARBAR) state;
                { var_name = Lident "||"; loc_ }
            | AMPERAMPER ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_AMPERAMPER) state;
                { var_name = Lident "&&"; loc_ }
            | BAR ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BAR) state;
                { var_name = Lident "|"; loc_ }
            | CARET ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_CARET) state;
                { var_name = Lident "^"; loc_ }
            | AMPER ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_AMPER) state;
                { var_name = Lident "&"; loc_ }
            | INFIX1 x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INFIX1) state;
                { var_name = Lident x; loc_ }
            | INFIX2 x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INFIX2) state;
                { var_name = Lident x; loc_ }
            | PLUS ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_PLUS) state;
                { var_name = Lident "+"; loc_ }
            | MINUS ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_MINUS) state;
                { var_name = Lident "-"; loc_ }
            | INFIX3 x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INFIX3) state;
                { var_name = Lident x; loc_ }
            | INFIX4 x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INFIX4) state;
                { var_name = Lident x; loc_ }
            | other ->
                let other_loc = peek_location state in
                add_error_failed_to_parse state other "binary operator"
                  other_loc;
                let loc_ = parser_panic state in
                { var_name = Lident "?binop"; loc_ })
        | Panic { loc; _ } ->
            add_error_skipped state "binop" loc;
            { var_name = Lident "?binop"; loc_ = loc }
         : Syntax.var))
    : Syntax.var)

and parse_loop_expr (state : parse_state) label =
  (let loc_start = peek_loc_start state in
   skip_this state;
   let args =
     with_follow [ Token_kind T_LBRACE ]
       (fun state -> sepby1 (Token_kind T_COMMA) parse_expr state)
       state
   in
   let loop_loc_ = Rloc.of_pos (loc_start, peek_loc_end state) in
   let body =
     match get_mode state with
     | Normal -> parse_match_fn_body state
     | Panic _ -> []
   in
   let loc_ = loc_start_with state loc_start in
   Pexpr_loop { args; body; loop_loc_; loc_; label }
    : Syntax.expr)

and parse_for_expr (state : parse_state) label =
  (let loc_start = peek_loc_start state in
   skip_this state;
   let handle_error expected =
     (let loc = peek_location state in
      let token = peek_token state in
      add_error_unexpected state token expected loc;
      push_sync state (Token_kind T_LBRACE);
      let _ = parser_panic state in
      pop_sync state (Token_kind T_LBRACE);
      let body = parse_block_expr state in
      let loc_ = loc_start_with state loc_start in
      Pexpr_for
        {
          binders = [];
          condition = None;
          continue_block = [];
          body;
          for_else = None;
          loc_;
          label;
        }
       : Syntax.expr)
       [@@inline]
   in
   match peek_token state with
   | SEMI _ | LBRACE -> parse_for_loop state ~loc_start label
   | UNDERSCORE | IN -> parse_foreach_loop state ~loc_start label
   | LIDENT _ -> (
       match peek_nth_token state 1 with
       | EQUAL -> parse_for_loop state ~loc_start label
       | COMMA | IN -> parse_foreach_loop state ~loc_start label
       | _ ->
           skip_this state;
           handle_error
             [ Token_kind T_EQUAL; Token_kind T_COMMA; Token_kind T_IN ])
   | _ ->
       handle_error
         [ Token_kind T_LIDENT; Token_kind T_SEMI; Token_kind T_LBRACE ]
    : Syntax.expr)

and parse_foreach_loop state ~loc_start label =
  (debugged "for .. in" state (fun state ->
       (let binders =
          with_follow [ Token_kind T_IN ]
            (sepby1 (Token_kind T_COMMA) (fun state ->
                 match peek_token state with
                 | UNDERSCORE ->
                     skip_this state;
                     None
                 | LIDENT name ->
                     let loc_ = peek_location state in
                     skip_this state;
                     Some ({ binder_name = name; loc_ } : Syntax.binder)
                 | token ->
                     add_error_unexpected state token
                       [ Token_kind T_LIDENT; Token_kind T_UNDERSCORE ]
                       (peek_location state);
                     let _ = parser_panic state in
                     None))
            state
        in
        expected_token (Token_kind T_IN) state;
        let expr = parse_expr state in
        let body = parse_block_expr state in
        let else_block =
          match peek_token state with
          | ELSE ->
              skip_this state;
              Some (parse_block_expr state)
          | _ -> None
        in
        let loc_ = loc_start_with state loc_start in
        Pexpr_foreach { binders; expr; body; else_block; loc_; label }
         : Syntax.expr))
    : Syntax.expr)

and parse_for_loop state ~loc_start label =
  debugged "for" state (fun state ->
      (let parse_binder_with_value state =
         let binder = parse_binder state in
         expected_token (Token_kind T_EQUAL) state;
         let value = parse_expr state in
         (binder, value)
       in
       let binders, _ =
         sepby_with_follow (Token_kind T_COMMA)
           [ Token_kind T_SEMI; Token_kind T_LBRACE ]
           parse_binder_with_value state
       in
       let condition, continue_block =
         match peek_token state with
         | LBRACE -> (None, [])
         | SEMI _ ->
             with_sync state (Token_kind T_LBRACE) (fun state ->
                 expected_token (Token_kind T_SEMI) state;
                 let condition = option first_expr parse_infix_expr state in
                 expected_token (Token_kind T_SEMI) state;
                 let continue_block, _ =
                   sepby_with_follow (Token_kind T_COMMA)
                     [ Token_kind T_LBRACE ] parse_binder_with_value state
                 in
                 (condition, continue_block))
         | other ->
             let other_loc = peek_location state in
             add_error_unexpected state other
               [ Token_kind T_SEMI; Token_kind T_LBRACE ]
               other_loc;
             (None, [])
       in
       let body = parse_block_expr state in
       match peek_token state with
       | ELSE ->
           with_follow [ Token_kind T_LBRACE ]
             (expected_token (Token_kind T_ELSE))
             state;
           let else_block = parse_block_expr state in
           let loc_ = loc_start_with state loc_start in
           Pexpr_for
             {
               binders;
               condition;
               continue_block;
               body;
               for_else = Some else_block;
               label;
               loc_;
             }
       | _ ->
           let loc_ = loc_start_with state loc_start in
           Pexpr_for
             {
               binders;
               condition;
               continue_block;
               body;
               for_else = None;
               label;
               loc_;
             }
        : Syntax.expr))

and parse_while_expr (state : parse_state) label =
  (let loc_start = peek_loc_start state in
   expected_token (Token_kind T_WHILE) state;
   let loop_cond = with_follow [ Token_kind T_LBRACE ] parse_expr state in
   let loop_body = parse_block_expr state in
   match peek_token state with
   | ELSE ->
       with_follow [ Token_kind T_LBRACE ]
         (expected_token (Token_kind T_ELSE))
         state;
       let while_else = parse_block_expr state in
       let loc_ = loc_start_with state loc_start in
       Pexpr_while
         { loop_cond; loop_body; while_else = Some while_else; loc_; label }
   | _ ->
       let loc_ = loc_start_with state loc_start in
       Pexpr_while { loop_cond; loop_body; loc_; while_else = None; label }
    : Syntax.expr)

and parse_try_expr (state : parse_state) =
  (let loc_start = peek_loc_start state in
   let try_loc_ = peek_location state in
   expected_token (Token_kind T_TRY) state;
   let body = parse_expr state in
   let catch_start = peek_loc_start state in
   let catch_all =
     match peek_token state with
     | CATCH -> (
         skip_this state;
         match peek_token state with
         | EXCLAMATION ->
             skip_this state;
             true
         | _ -> false)
     | _ -> false
   in
   let catch_loc_ = Rloc.of_pos (catch_start, peek_loc_end state) in
   let catch =
     surround_sepby (Token_kind T_LBRACE) (Token_kind T_RBRACE)
       (Token_kind T_SEMI) parse_case state
   in
   let try_else, else_loc_ =
     match peek_token state with
     | ELSE ->
         let else_start = peek_loc_start state in
         skip_this state;
         let else_loc_ = Rloc.of_pos (else_start, peek_loc_end state) in
         let try_else =
           surround_sepby (Token_kind T_LBRACE) (Token_kind T_RBRACE)
             (Token_kind T_SEMI) parse_case state
         in
         (Some try_else, else_loc_)
     | _ -> (None, Rloc.no_location)
   in
   let loc_ = loc_start_with state loc_start in
   Pexpr_try
     { body; catch; catch_all; try_else; else_loc_; loc_; catch_loc_; try_loc_ }
    : Syntax.expr)

and parse_if_expr (state : parse_state) =
  (let loc_start = peek_loc_start state in
   expected_token (Token_kind T_IF) state;
   let cond = with_follow [ Token_kind T_LBRACE ] parse_expr state in
   let ifso = parse_block_expr state in
   match peek_token state with
   | ELSE -> (
       expected_token (Token_kind T_ELSE) state;
       match peek_token state with
       | IF ->
           let ifnot = parse_expr state in
           let loc_ = loc_start_with state loc_start in
           Pexpr_if { cond; ifso; ifnot = Some ifnot; loc_ }
       | _ ->
           let ifnot = parse_block_expr state in
           let loc_ = loc_start_with state loc_start in
           Pexpr_if { cond; ifso; ifnot = Some ifnot; loc_ })
   | _ ->
       let loc_ = loc_start_with state loc_start in
       Pexpr_if { cond; ifso; ifnot = None; loc_ }
    : Syntax.expr)

and parse_match_expr (state : parse_state) =
  (let loc_start = peek_loc_start state in
   expected_token (Token_kind T_MATCH) state;
   let expr = with_follow [ Token_kind T_LBRACE ] parse_expr state in
   let match_loc_ = Rloc.of_pos (loc_start, peek_loc_end state) in
   let cases =
     surround_sepby ~invalid_delims:[ Token_kind T_COMMA ] (Token_kind T_LBRACE)
       (Token_kind T_RBRACE) (Token_kind T_SEMI) parse_case state
   in
   let loc_ = loc_start_with state loc_start in
   Pexpr_match { expr; cases; match_loc_; loc_ }
    : Syntax.expr)

and parse_case (state : parse_state) =
  (let pat =
     with_syncs state [ Token_kind T_FAT_ARROW; Token_kind T_IF ] parse_pattern
   in
   let guard =
     match peek_token state with
     | IF ->
         skip_this state;
         Some (parse_expr state)
     | _ -> None
   in
   expected_token (Token_kind T_FAT_ARROW) state;
   let act = parse_expr_stmt state in
   { pattern = pat; guard; body = act }
    : Syntax.case)

and parse_multi_pattern_case (state : parse_state) =
  (let parse_patterns state =
     let patterns, _ =
       sepby_with_follow (Token_kind T_COMMA)
         [ Token_kind T_FAT_ARROW; Token_kind T_IF ]
         parse_pattern state
     in
     patterns
   in
   let pats =
     with_syncs state [ Token_kind T_FAT_ARROW; Token_kind T_IF ] parse_patterns
   in
   let guard =
     match peek_token state with
     | IF ->
         skip_this state;
         Some (parse_expr state)
     | _ -> None
   in
   expected_token (Token_kind T_FAT_ARROW) state;
   let act = parse_expr_stmt state in
   { patterns = pats; guard; body = act }
    : Syntax.multi_arg_case)

and parse_expr (state : parse_state) =
  (debugged "expr" state (fun state ->
       match get_mode state with
       | Normal -> (
           match peek_token state with
           | POST_LABEL label_name -> (
               let label : Syntax.label =
                 { label_name; loc_ = peek_location state }
               in
               skip_this state;
               expected_token (Token_kind T_COLON) state;
               match peek_token state with
               | LOOP -> parse_loop_expr state (Some label)
               | FOR -> parse_for_expr state (Some label)
               | WHILE -> parse_while_expr state (Some label)
               | other ->
                   let loc = peek_location state in
                   add_error_unexpected state other
                     [ Token_kind T_LOOP; Token_kind T_FOR; Token_kind T_WHILE ]
                     loc;
                   let _ = parser_panic state in
                   Pexpr_hole { loc_ = loc; kind = Synthesized })
           | LOOP -> parse_loop_expr state None
           | FOR -> parse_for_expr state None
           | WHILE -> parse_while_expr state None
           | TRY -> parse_try_expr state
           | IF -> parse_if_expr state
           | MATCH -> parse_match_expr state
           | _ -> parse_pipe_expr state)
       | Panic { loc; _ } ->
           add_error_skipped state "expr" loc;
           Pexpr_hole { loc_ = loc; kind = Synthesized })
    : Syntax.expr)

and parse_pipe_expr (state : parse_state) =
  (debugged "pipe_expr" state (fun state ->
       match get_mode state with
       | Normal ->
           let start_loc = peek_loc_start state in
           let rec loop lhs =
             match peek_token state with
             | PIPE ->
                 skip_this state;
                 let rhs = parse_infix_expr state in
                 let loc_ = loc_start_with state start_loc in
                 let lhs : Syntax.expr = Pexpr_pipe { lhs; rhs; loc_ } in
                 loop lhs
             | _ -> lhs
           in
           loop (parse_infix_expr state)
       | Panic { loc; _ } ->
           add_error_skipped state "pipe_expr" loc;
           Pexpr_hole { loc_ = loc; kind = Synthesized })
    : Syntax.expr)

and parse_infix_expr (state : parse_state) =
  (debugged "infix_expr" state (fun state ->
       let make_infix ((lpos, lhs) : position * Syntax.expr)
           ((_, op) : token * Syntax.var) ((_, rhs) : position * Syntax.expr) =
         (let loc_ = loc_start_with state lpos in
          (lpos, Syntax.Pexpr_infix { lhs; op; rhs; loc_ })
           : position * Syntax.expr)
       in
       let rec push_infix ~(expr_stack : (position * Syntax.expr) list)
           ~(op_stack : (token * Syntax.var) list) ~(next_op : token) =
         (match (expr_stack, op_stack) with
          | _, [] -> (expr_stack, op_stack)
          | rhs :: lhs :: expr_stack', op :: op_stack' ->
              let priority1, assoc1 = get_binop_prior_assoc (fst op) in
              let priority2, _ = get_binop_prior_assoc next_op in
              if
                priority1 > priority2
                || (priority1 = priority2 && assoc1 = `Left)
              then
                let new_expr = make_infix lhs op rhs in
                push_infix ~expr_stack:(new_expr :: expr_stack')
                  ~op_stack:op_stack' ~next_op
              else (expr_stack, op_stack)
          | _ -> assert false
           : (position * Syntax.expr) list * (token * Syntax.var) list)
       in
       let rec reduce_current_stack ~expr_stack ~op_stack =
         match (expr_stack, op_stack) with
         | expr :: [], [] -> expr
         | rhs :: lhs :: expr_stack', op :: op_stack' ->
             let new_expr = make_infix lhs op rhs in
             reduce_current_stack ~expr_stack:(new_expr :: expr_stack')
               ~op_stack:op_stack'
         | _ -> assert false
       in
       let rec loop ~expr_stack ~op_stack state =
         if get_mode state <> Normal then
           reduce_current_stack ~expr_stack ~op_stack
         else
           let tok = peek_token state in
           if not (is_binop tok) then reduce_current_stack ~expr_stack ~op_stack
           else
             let expr_stack, op_stack =
               push_infix ~expr_stack ~op_stack ~next_op:tok
             in
             let var = parse_binop state in
             let rhs_start = peek_loc_start state in
             let new_rhs = parse_postfix_expr state in
             loop
               ~expr_stack:((rhs_start, new_rhs) :: expr_stack)
               ~op_stack:((tok, var) :: op_stack) state
       in
       let first_start = peek_loc_start state in
       let first_expr = parse_postfix_expr state in
       snd (loop ~expr_stack:[ (first_start, first_expr) ] ~op_stack:[] state))
    : Syntax.expr)

and parse_postfix_expr (state : parse_state) =
  (debugged "postfix expr" state (fun state ->
       let start_loc = peek_loc_start state in
       let expr = parse_range_expr state in
       match peek_token state with
       | AS ->
           skip_this state;
           let trait = parse_type_name state in
           Syntax.Pexpr_as
             { expr; trait; loc_ = loc_start_with state start_loc }
       | IS ->
           skip_this state;
           let pat = parse_range_pattern state in
           Pexpr_is { expr; pat; loc_ = loc_start_with state start_loc }
       | _ -> expr)
    : Syntax.expr)

and parse_range_expr (state : parse_state) =
  (debugged "range_expr" state (fun state ->
       let start_loc = peek_loc_start state in
       let lhs = parse_prefix_expr state in
       let op_start_loc = peek_loc_start state in
       match peek_token state with
       | RANGE_INCLUSIVE ->
           skip_this state;
           let op_loc = loc_start_with state op_start_loc in
           let rhs = parse_prefix_expr state in
           let loc_ = loc_start_with state start_loc in
           Syntax.Pexpr_infix
             { op = { var_name = Lident "..="; loc_ = op_loc }; lhs; rhs; loc_ }
       | RANGE_EXCLUSIVE ->
           skip_this state;
           let op_loc = loc_start_with state op_start_loc in
           let rhs = parse_prefix_expr state in
           let loc_ = loc_start_with state start_loc in
           Syntax.Pexpr_infix
             { op = { var_name = Lident "..<"; loc_ = op_loc }; lhs; rhs; loc_ }
       | _ -> lhs)
    : Syntax.expr)

and parse_prefix_expr (state : parse_state) =
  (debugged "prefix expr" state (fun state ->
       match get_mode state with
       | Normal -> (
           let loc_start = peek_loc_start state in
           match peek_token state with
           | PLUS ->
               expected_token (Token_kind T_PLUS) state;
               let expr = parse_prefix_expr state in
               let loc_ = loc_start_with state loc_start in
               Parser_util.make_uplus ~loc_ "+" expr
           | MINUS ->
               expected_token (Token_kind T_MINUS) state;
               let expr = parse_prefix_expr state in
               let loc_ = loc_start_with state loc_start in
               Parser_util.make_uminus ~loc_ "-" expr
           | _ ->
               let rec loop (left : Syntax.expr) =
                 match peek_token state with
                 | EXCLAMATION | QUESTION | LPAREN ->
                     let attr = parse_apply_attr state in
                     let args =
                       surround_sepby (Token_kind T_LPAREN)
                         (Token_kind T_RPAREN) (Token_kind T_COMMA)
                         parse_argument state
                     in
                     let loc_ = loc_start_with state loc_start in
                     loop (Pexpr_apply { func = left; args; attr; loc_ })
                 | LBRACKET ->
                     let parse_index state =
                       match peek_token state with
                       | COLON ->
                           skip_this state;
                           let end_index = option first_expr parse_expr state in
                           `SliceIndex (None, end_index)
                       | _ -> (
                           let start_index = parse_expr state in
                           match peek_token state with
                           | COLON ->
                               skip_this state;
                               let end_index =
                                 option first_expr parse_expr state
                               in
                               `SliceIndex (Some start_index, end_index)
                           | _ -> `ArrayIndex start_index)
                     in
                     let index_loc_start = peek_loc_start state in
                     let index =
                       surround (Token_kind T_LBRACKET) (Token_kind T_RBRACKET)
                         parse_index state
                     in
                     let loc_ = loc_start_with state loc_start in
                     let index_loc_ = loc_start_with state index_loc_start in
                     let array_get_expr : Syntax.expr =
                       match index with
                       | `ArrayIndex index ->
                           Pexpr_array_get { loc_; array = left; index }
                       | `SliceIndex (start_index, end_index) ->
                           Pexpr_array_get_slice
                             {
                               loc_;
                               index_loc_;
                               array = left;
                               start_index;
                               end_index;
                             }
                     in
                     loop array_get_expr
                 | DOT_LIDENT _ -> (
                     let label : Syntax.label =
                       let loc_ = peek_location state in
                       let label_name = expected_dot_lident state in
                       { label_name; loc_ }
                     in
                     let attr = parse_apply_attr state in
                     match peek_token state with
                     | LPAREN ->
                         let args =
                           surround_sepby (Token_kind T_LPAREN)
                             (Token_kind T_RPAREN) (Token_kind T_COMMA)
                             parse_argument state
                         in
                         let loc_ = loc_start_with state loc_start in
                         let expr : Syntax.expr =
                           Pexpr_dot_apply
                             {
                               self = left;
                               method_name = label;
                               args;
                               return_self = false;
                               attr;
                               loc_;
                             }
                         in
                         loop expr
                     | _ ->
                         let loc_ = loc_start_with state loc_start in
                         let accessor : Syntax.accessor =
                           if label.label_name = "_" then Newtype
                           else Label label
                         in
                         loop (Pexpr_field { record = left; accessor; loc_ }))
                 | DOTDOT ->
                     skip_this state;
                     let label = parse_label state in
                     let attr = parse_apply_attr state in
                     let args =
                       surround_sepby (Token_kind T_LPAREN)
                         (Token_kind T_RPAREN) (Token_kind T_COMMA)
                         parse_argument state
                     in
                     let loc_ = loc_start_with state loc_start in
                     let expr : Syntax.expr =
                       Pexpr_dot_apply
                         {
                           self = left;
                           method_name = label;
                           args;
                           return_self = true;
                           attr;
                           loc_;
                         }
                     in
                     loop expr
                 | DOT_INT _ ->
                     let accessor : Syntax.accessor =
                       let loc_ = peek_location state in
                       let tuple_index = expected_dot_int state in
                       Index { tuple_index; loc_ }
                     in
                     let loc_ = loc_start_with state loc_start in
                     loop (Pexpr_field { record = left; accessor; loc_ })
                 | _ -> left
               in
               let left = parse_simple_expr state in
               loop left)
       | Panic { loc; _ } ->
           add_error_skipped state "chain_expr" loc;
           Pexpr_hole { loc_ = loc; kind = Synthesized })
    : Syntax.expr)

and parse_constr_expr ~loc_start ~(extra_info : Syntax.constructor_extra_info)
    state =
  (debugged "constr_expr" state (fun state ->
       (match get_mode state with
        | Normal ->
            let name_loc_start = peek_loc_start state in
            let constr_name =
              match extra_info with
              | Package _ -> expected_dot_uident state
              | No_extra_info | Type_name _ -> expected_uident state
            in
            let name_loc = loc_start_with state name_loc_start in
            let loc_ = loc_start_with state loc_start in
            let constr : Syntax.constructor =
              {
                extra_info;
                constr_name = { name = constr_name; loc_ = name_loc };
                loc_;
              }
            in
            Pexpr_constr { constr; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "constr_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_argument (state : parse_state) =
  (debugged "argument" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | (LIDENT label_name | UIDENT label_name)
              when peek_nth_token_kind state 1 = Token_kind T_EQUAL
                   || peek_nth_token_kind state 1 = Token_kind T_QUESTION
                      && peek_nth_token_kind state 2 = Token_kind T_EQUAL ->
                let label : Syntax.label =
                  { label_name; loc_ = peek_location state }
                in
                skip_this state;
                let arg_kind =
                  match peek_token state with
                  | QUESTION ->
                      let question_loc = peek_location state in
                      skip_this state;
                      Syntax.Labelled_option { label; question_loc }
                  | _ -> Labelled label
                in
                skip_this state;
                let arg_value = parse_expr state in
                { arg_value; arg_kind }
            | POST_LABEL label_name ->
                let label : Syntax.label =
                  { label_name; loc_ = peek_location state }
                in
                skip_this state;
                let arg_kind = Syntax.Labelled_pun label in
                let arg_value =
                  Parser_util.label_to_expr
                    ~loc_:(Rloc.trim_last_char label.loc_)
                    label
                in
                { arg_value; arg_kind }
            | LIDENT label_name
              when peek_nth_token_kind state 1 = Token_kind T_QUESTION
                   && (peek_nth_token_kind state 2 = Token_kind T_COMMA
                      || peek_nth_token_kind state 2 = Token_kind T_RPAREN) ->
                let label : Syntax.label =
                  { label_name; loc_ = peek_location state }
                in
                skip_this state;
                let question_loc = peek_location state in
                skip_this state;
                let arg_kind =
                  Syntax.Labelled_option_pun { label; question_loc }
                in
                let arg_value =
                  Parser_util.label_to_expr ~loc_:label.loc_ label
                in
                { arg_value; arg_kind }
            | _ ->
                let arg_value = parse_expr state in
                { arg_value; arg_kind = Positional })
        | Panic { loc; _ } ->
            add_error_skipped state "argument" loc;
            {
              arg_value = Pexpr_hole { loc_ = loc; kind = Synthesized };
              arg_kind = Positional;
            }
         : Syntax.argument))
    : Syntax.argument)

and parse_func fn_loc_start ~has_error ~is_async (state : parse_state) =
  (debugged "func" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | LPAREN ->
                let param_loc_start = peek_loc_start state in
                let params =
                  surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                    (Token_kind T_COMMA) parse_parameter state
                in
                let params_loc_ = loc_start_with state param_loc_start in
                let res_ty =
                  match peek_token state with
                  | THIN_ARROW ->
                      expected_token (Token_kind T_THIN_ARROW) state;
                      let ty = parse_return_type state in
                      Some ty
                  | _ -> None
                in
                let block = parse_block_expr state in
                Lambda
                  {
                    parameters = params;
                    params_loc_;
                    body = block;
                    return_type = res_ty;
                    kind_ = Lambda;
                    has_error;
                    is_async;
                  }
            | LBRACE ->
                let loc_start = peek_loc_start state in
                let fn_loc_ = Rloc.of_pos (fn_loc_start, peek_loc_end state) in
                let cases = parse_match_fn_body state in
                let loc_ = loc_start_with state loc_start in
                Match { cases; has_error; is_async; fn_loc_; loc_ }
            | other ->
                let other_loc = peek_location state in
                add_error_failed_to_parse state other "func" other_loc;
                let loc_ = parser_panic state in
                Lambda
                  {
                    parameters = [];
                    params_loc_ = loc_;
                    body = Pexpr_hole { loc_; kind = Synthesized };
                    return_type = None;
                    kind_ = Lambda;
                    has_error;
                    is_async;
                  })
        | Panic { loc; _ } ->
            add_error_skipped state "func" loc;
            Lambda
              {
                parameters = [];
                params_loc_ = loc;
                body = Pexpr_hole { loc_ = loc; kind = Synthesized };
                return_type = None;
                kind_ = Lambda;
                has_error;
                is_async;
              }
         : Syntax.func))
    : Syntax.func)

and parse_parameter (state : parse_state) =
  (debugged "parameter" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | UNDERSCORE ->
                let loc_ = peek_location state in
                skip_this state;
                let param_annot = parse_opt_annot state in
                Discard_positional { loc_; ty = param_annot }
            | POST_LABEL label_name -> (
                let param_binder : Syntax.binder =
                  {
                    binder_name = label_name;
                    loc_ = Rloc.trim_last_char (peek_location state);
                  }
                in
                skip_this state;
                let param_annot = parse_opt_annot state in
                match (get_mode state, peek_token state) with
                | Normal, EQUAL ->
                    skip_this state;
                    let default = parse_expr state in
                    Optional
                      { default; binder = param_binder; ty = param_annot }
                | _ -> Labelled { binder = param_binder; ty = param_annot })
            | LIDENT label_name
              when peek_nth_token_kind state 1 = Token_kind T_QUESTION -> (
                let param_binder : Syntax.binder =
                  { binder_name = label_name; loc_ = peek_location state }
                in
                skip_this state;
                skip_this state;
                let param_annot = parse_opt_annot state in
                match (get_mode state, peek_token state) with
                | Normal, EQUAL ->
                    skip_this state;
                    let default = parse_expr state in
                    add_error state
                      (Errors.no_default_for_question_optional ~label:label_name
                         ~loc:(Syntax.loc_of_expression default));
                    Optional
                      { default; binder = param_binder; ty = param_annot }
                | _ ->
                    Question_optional
                      { binder = param_binder; ty = param_annot })
            | LIDENT param_name -> (
                let param_binder : Syntax.binder =
                  { binder_name = param_name; loc_ = peek_location state }
                in
                skip_this state;
                let param_annot = parse_opt_annot state in
                match (get_mode state, peek_token state) with
                | Normal, EQUAL ->
                    let eq_loc = peek_location state in
                    skip_this state;
                    let default = parse_expr state in
                    add_error state
                      (Errors.positional_argument_no_default eq_loc);
                    Optional
                      { default; binder = param_binder; ty = param_annot }
                | _ -> Positional { binder = param_binder; ty = param_annot })
            | other ->
                let other_loc = peek_location state in
                add_error_failed_to_parse state other "parameter name or label"
                  other_loc;
                let _ = parser_panic state in
                Positional
                  {
                    binder =
                      { binder_name = "?binder"; loc_ = Rloc.no_location };
                    ty = None;
                  })
        | Panic { loc; _ } ->
            add_error_skipped state "parameter" loc;
            Positional
              {
                binder = { binder_name = "?binder"; loc_ = Rloc.no_location };
                ty = None;
              }
         : Syntax.parameter))
    : Syntax.parameter)

and parse_apply_attr (state : parse_state) =
  (match get_mode state with
   | Normal -> (
       match peek_token state with
       | EXCLAMATION -> (
           skip_this state;
           match peek_token state with
           | EXCLAMATION ->
               skip_this state;
               Double_exclamation
           | _ -> Exclamation)
       | QUESTION ->
           skip_this state;
           Question
       | _ -> No_attr)
   | Panic _ -> No_attr
    : Syntax.apply_attr)

and parse_simple_expr (state : parse_state) =
  (debugged "simple_expr" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | TRUE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_TRUE) state;
                Pexpr_constant { c = Const_bool true; loc_ }
            | FALSE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_FALSE) state;
                Pexpr_constant { c = Const_bool false; loc_ }
            | BYTE x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BYTE) state;
                Pexpr_constant { c = Const_byte x; loc_ }
            | CHAR x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_CHAR) state;
                Pexpr_constant { c = Const_char x; loc_ }
            | INT x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INT) state;
                Pexpr_constant { c = Parser_util.make_int x; loc_ }
            | FLOAT x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_FLOAT) state;
                Pexpr_constant { c = Const_double x; loc_ }
            | STRING x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_STRING) state;
                Pexpr_constant { c = Const_string x; loc_ }
            | BYTES x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_BYTES) state;
                Pexpr_constant { c = Const_bytes x; loc_ }
            | MULTILINE_STRING _ | MULTILINE_INTERP _ ->
                let loc_start = peek_loc_start state in
                let rec loop xs =
                  match peek_token state with
                  | MULTILINE_STRING x ->
                      expected_token (Token_kind T_MULTILINE_STRING) state;
                      loop (Syntax.Multiline_string x :: xs)
                  | MULTILINE_INTERP x ->
                      expected_token (Token_kind T_MULTILINE_INTERP) state;
                      loop
                        (Syntax.Multiline_interp (Parsing_util.make_interps x)
                        :: xs)
                  | _ -> List.rev xs
                in
                let elems = loop [] in
                let loc_ = loc_start_with state loc_start in
                Syntax.Pexpr_multiline_string { loc_; elems }
            | INTERP x ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_INTERP) state;
                Parser_util.make_Pexpr_interp ~loc_ x
            | PACKAGE_NAME pkg -> (
                match peek_nth_token_kind state 1 with
                | Token_kind (T_DOT_LIDENT | T_DOT_UIDENT)
                  when peek_nth_token_kind state 2 = Token_kind T_COLONCOLON ->
                    parse_coloncolon_expr state
                | Token_kind T_DOT_UIDENT ->
                    let loc_start = peek_loc_start state in
                    skip_this state;
                    parse_constr_expr ~loc_start ~extra_info:(Package pkg) state
                | _ ->
                    let loc_start = peek_loc_start state in
                    let var = parse_qual_lident state in
                    let loc_ = loc_start_with state loc_start in
                    Pexpr_ident { id = { var_name = var; loc_ }; loc_ })
            | (LIDENT _ | UIDENT _)
              when peek_nth_token_kind state 1 = Token_kind T_COLONCOLON ->
                parse_coloncolon_expr state
            | AMPER -> parse_coloncolon_expr state
            | LIDENT _ ->
                let loc_start = peek_loc_start state in
                let var = parse_var state in
                let loc_ = loc_start_with state loc_start in
                Pexpr_ident { id = var; loc_ }
            | UIDENT _ ->
                let loc_start = peek_loc_start state in
                parse_constr_expr ~loc_start ~extra_info:No_extra_info state
            | ASYNC ->
                let loc_start = peek_loc_start state in
                skip_this state;
                skip_this state;
                let has_error = parse_optional_bang state in
                let func =
                  parse_func loc_start ~has_error ~is_async:true state
                in
                let loc_ = loc_start_with state loc_start in
                Pexpr_function { func; loc_ }
            | FN ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_FN) state;
                let has_error = parse_optional_bang state in
                let func =
                  parse_func loc_start ~has_error ~is_async:false state
                in
                let loc_ = loc_start_with state loc_start in
                Pexpr_function { func; loc_ }
            | LPAREN ->
                located state (fun state ->
                    surround (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                      (fun state ->
                        (let first = option first_expr parse_expr state in
                         match first with
                         | None ->
                             fun loc_ -> Pexpr_unit { loc_; faked = false }
                         | Some first -> (
                             match peek_token state with
                             | COLON ->
                                 expected_token (Token_kind T_COLON) state;
                                 let ty = parse_type state in
                                 fun loc_ ->
                                   Pexpr_constraint { expr = first; ty; loc_ }
                             | COMMA ->
                                 expected_token (Token_kind T_COMMA) state;
                                 let rest, _ =
                                   sepby_with_follow (Token_kind T_COMMA)
                                     [ Token_kind T_RPAREN ] parse_expr state
                                 in
                                 fun loc_ ->
                                   Pexpr_tuple { exprs = first :: rest; loc_ }
                             | RPAREN ->
                                 fun loc_ ->
                                   Pexpr_group
                                     { expr = first; group = Group_paren; loc_ }
                             | _ -> fun _ -> first)
                          : Rloc.t -> Syntax.expr))
                      state)
            | LBRACKET ->
                let loc_start = peek_loc_start state in
                let to_spread = ref false in
                let elems =
                  surround_sepby (Token_kind T_LBRACKET) (Token_kind T_RBRACKET)
                    (Token_kind T_COMMA)
                    (fun state ->
                      (match peek_token state with
                       | DOTDOT ->
                           to_spread := true;
                           let spread_loc_start = peek_loc_start state in
                           expected_token (Token_kind T_DOTDOT) state;
                           Elem_spread
                             {
                               expr = parse_expr state;
                               loc_ = loc_start_with state spread_loc_start;
                             }
                       | _ -> Elem_regular (parse_expr state)
                        : Syntax.spreadable_elem))
                    state
                in
                let loc_ = loc_start_with state loc_start in
                if !to_spread then Pexpr_array_spread { elems; loc_ }
                else
                  Pexpr_array
                    {
                      exprs =
                        Lst.map elems (function
                          | Syntax.Elem_regular e -> e
                          | Syntax.Elem_spread _ -> assert false);
                      loc_;
                    }
            | LBRACE -> (
                let loc_start = peek_loc_start state in
                match peek_nth_token state 1 with
                | DOTDOT -> parse_record_expr ~type_name:None state
                | LIDENT _ -> (
                    match peek_nth_token state 2 with
                    | SEMI _ when peek_nth_token state 3 = RBRACE ->
                        parse_semi_trailing_record state
                    | COLON | COMMA | RBRACE ->
                        parse_record_expr ~type_name:None state
                    | _ ->
                        let loc_ = loc_start_with state loc_start in
                        let expr = parse_block_expr state in
                        Pexpr_group { expr; group = Group_brace; loc_ })
                | RBRACE -> parse_map_expr state
                | (TRUE | FALSE | CHAR _ | INT _ | FLOAT _ | MINUS | STRING _)
                  when peek_nth_token state 2 = COLON ->
                    parse_map_expr state
                | _ ->
                    let loc_ = loc_start_with state loc_start in
                    let expr = parse_block_expr state in
                    Pexpr_group { expr; group = Group_brace; loc_ })
            | UNDERSCORE ->
                let loc_ = peek_location state in
                expected_token (Token_kind T_UNDERSCORE) state;
                Pexpr_hole { loc_; kind = Incomplete }
            | other ->
                let other_loc = peek_location state in
                add_error_failed_to_parse state other "simple expression"
                  other_loc;
                let loc_ = parser_panic state in
                Pexpr_hole { loc_; kind = Synthesized })
        | Panic { loc; _ } ->
            add_error_skipped state "simple_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_coloncolon_expr (state : parse_state) =
  (debugged "coloncolon_expr" state (fun state ->
       (let loc_start = peek_loc_start state in
        let type_name = parse_type_name state in
        expected_token (Token_kind T_COLONCOLON) state;
        match (get_mode state, peek_token state) with
        | Normal, LIDENT _ ->
            let method_name = parse_label state in
            let loc_ = loc_start_with state loc_start in
            Pexpr_method { type_name; method_name; loc_ }
        | Normal, UIDENT _ ->
            parse_constr_expr ~loc_start ~extra_info:(Type_name type_name) state
        | Normal, LBRACE -> parse_record_expr ~type_name:(Some type_name) state
        | Normal, token ->
            add_error_failed_to_parse state token
              "method name, constructor name, \"[\" or \"{\""
              (peek_location state);
            let skipped_loc = parser_panic state in
            let loc_ = Rloc.of_pos (loc_start, Rloc.get_end skipped_loc) in
            Pexpr_constr
              {
                constr =
                  {
                    extra_info = Type_name type_name;
                    constr_name = { name = "?constr"; loc_ = Rloc.no_location };
                    loc_;
                  };
                loc_;
              }
        | Panic { loc; _ }, _ ->
            add_error_skipped state "coloncolon_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_field_def (state : parse_state) =
  (debugged "field_def" state (fun state ->
       (match get_mode state with
        | Normal -> (
            let loc_start = peek_loc_start state in
            let label = parse_label state in
            let expr_opt =
              match peek_token state with
              | EQUAL ->
                  let loc = peek_location state in
                  add_error state (Errors.invalid_equal_in_struct_expr ~loc);
                  skip_this state;
                  Some (parse_expr state)
              | COLON ->
                  skip_this state;
                  Some (parse_expr state)
              | _ -> None
            in
            match expr_opt with
            | Some expr ->
                let loc_ = loc_start_with state loc_start in
                Field_def { label; expr; is_pun = false; loc_ }
            | None ->
                let loc_ = loc_start_with state loc_start in
                Field_def
                  {
                    label;
                    expr = Parser_util.label_to_expr ~loc_:label.loc_ label;
                    is_pun = true;
                    loc_;
                  })
        | Panic { loc; _ } ->
            add_error_skipped state "filed_def" loc;
            Field_def
              {
                label = { label_name = "?"; loc_ = loc };
                expr = Pexpr_hole { loc_ = loc; kind = Synthesized };
                is_pun = false;
                loc_ = loc;
              }
         : Syntax.field_def))
    : Syntax.field_def)

and parse_semi_trailing_record (state : parse_state) =
  (debugged "semi trailing record_expr" state (fun state ->
       (match get_mode state with
        | Normal ->
            let loc_start = peek_loc_start state in
            expected_token (Token_kind T_LBRACE) state;
            let field = parse_field_def state in
            expected_token (Token_kind T_SEMI) state;
            expected_token (Token_kind T_RBRACE) state;
            Pexpr_record
              {
                type_name = None;
                fields = [ field ];
                trailing = Trailing_semi;
                loc_ = loc_start_with state loc_start;
              }
        | Panic { loc; _ } ->
            add_error_skipped state "semi trailing record_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_record_expr ~type_name state =
  (debugged "record_expr" state (fun state ->
       (match get_mode state with
        | Normal -> (
            let parse_fields state =
              sepby_with_follow (Token_kind T_COMMA) [ Token_kind T_RBRACE ]
                parse_field_def state
            in
            let loc_start = peek_loc_start state in
            expected_token (Token_kind T_LBRACE) state;
            let spread_record, fields, trailing_comma =
              match peek_token state with
              | DOTDOT ->
                  expected_token (Token_kind T_DOTDOT) state;
                  let spread_record = parse_expr state in
                  let fields, trailing =
                    match peek_token state with
                    | COMMA ->
                        expected_token (Token_kind T_COMMA) state;
                        parse_fields state
                    | _ -> ([], false)
                  in
                  (Some spread_record, fields, trailing)
              | _ ->
                  let fields, trailing_semi = parse_fields state in
                  (None, fields, trailing_semi)
            in
            expected_token (Token_kind T_RBRACE) state;
            let loc_ = loc_start_with state loc_start in
            match spread_record with
            | Some record ->
                Pexpr_record_update { type_name; record; fields; loc_ }
            | None ->
                Pexpr_record
                  {
                    type_name;
                    fields;
                    loc_;
                    trailing =
                      (if trailing_comma then Trailing_comma else Trailing_none);
                  })
        | Panic { loc; _ } ->
            add_error_skipped state "record_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_map_expr (state : parse_state) =
  (debugged "map_expr" state (fun state ->
       (match get_mode state with
        | Normal ->
            let parse_elem state =
              let loc_start = peek_loc_start state in
              let parse_map_expr_elem key =
                skip_this state;
                let key_loc_ = loc_start_with state loc_start in
                expected_token (Token_kind T_COLON) state;
                let expr = parse_expr state in
                let loc_ = loc_start_with state loc_start in
                Some (Syntax.Map_expr_elem { key; expr; key_loc_; loc_ })
                  [@@local]
              in
              match peek_token state with
              | TRUE -> parse_map_expr_elem (Const_bool true)
              | FALSE -> parse_map_expr_elem (Const_bool false)
              | CHAR x -> parse_map_expr_elem (Const_char x)
              | INT x -> parse_map_expr_elem (Parser_util.make_int x)
              | FLOAT x -> parse_map_expr_elem (Const_double x)
              | MINUS -> (
                  expected_token (Token_kind T_MINUS) state;
                  match peek_token state with
                  | INT x ->
                      parse_map_expr_elem (Parser_util.make_int ("-" ^ x))
                  | FLOAT x -> parse_map_expr_elem (Const_double ("-" ^ x))
                  | other ->
                      let other_loc = peek_location state in
                      add_error_failed_to_parse state other
                        "`-Int` or `-Float` in key of map pattern" other_loc;
                      let _ = parser_panic state in
                      None)
              | STRING x -> parse_map_expr_elem (Const_string x)
              | other ->
                  add_error_unexpected state other first_map_pattern_key
                    (peek_location state);
                  let _ = parser_panic state in
                  None
            in
            let loc_start = peek_loc_start state in
            let elems =
              surround_sepby (Token_kind T_LBRACE) (Token_kind T_RBRACE)
                (Token_kind T_COMMA) parse_elem state
            in
            let elems =
              Lst.fold_right elems [] (fun elem_opt ->
                  fun acc ->
                   match elem_opt with None -> acc | Some elem -> elem :: acc)
            in
            let loc_ = loc_start_with state loc_start in
            Pexpr_map { elems; loc_ }
        | Panic { loc; _ } ->
            add_error_skipped state "record_expr" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_opt_annot (state : parse_state) =
  (debugged "opt_annot" state (fun state ->
       match get_mode state with
       | Normal -> (
           match peek_token state with
           | COLON ->
               expected_token (Token_kind T_COLON) state;
               let res = parse_type state in
               Some res
           | _ -> None)
       | Panic { loc; _ } ->
           add_error_skipped state "opt_annot" loc;
           None)
    : Syntax.typ option)

and parse_block_expr_with_local_types (state : parse_state) =
  (debugged "block_expr_with_local_types" state (fun state ->
       match get_mode state with
       | Normal ->
           let loc_start = peek_loc_start state in
           let local_types, stmts =
             surround (Token_kind T_LBRACE) (Token_kind T_RBRACE)
               (fun s ->
                 let semi = Token_kind T_SEMI in
                 let local_types, stmts =
                   match peek_token state with
                   | STRUCT | ENUM | TYPE ->
                       let local_types = Vec.empty () in
                       let rec loop () =
                         match peek_token state with
                         | STRUCT | ENUM | TYPE -> (
                             push_sync state semi;
                             let local_type = parse_local_type_decl state in
                             Vec.push local_types local_type;
                             pop_sync state semi;
                             match peek_token state with
                             | SEMI _ ->
                                 skip_this state;
                                 loop ()
                             | _ -> [])
                         | _ ->
                             let stmts, _ =
                               sepby_with_follow semi [ Token_kind T_RBRACE ]
                                 parse_stmt s
                             in
                             stmts
                       in
                       let stmts = loop () in
                       (Vec.to_list local_types, stmts)
                   | _ ->
                       let stmts, _ =
                         sepby_with_follow semi [ Token_kind T_RBRACE ]
                           parse_stmt s
                       in
                       ([], stmts)
                 in
                 (local_types, stmts))
               state
           in
           let loc_ = loc_start_with state loc_start in
           (local_types, Compact.compact_rev (List.rev stmts) loc_)
       | Panic { loc; _ } ->
           add_error_skipped state "block_expr_with_local_types" loc;
           ([], Pexpr_hole { loc_ = loc; kind = Synthesized }))
    : Syntax.local_type_decl list * Syntax.expr)

and parse_block_expr (state : parse_state) =
  (debugged "block_expr" state (fun state ->
       match get_mode state with
       | Normal ->
           let loc_start = peek_loc_start state in
           let stmts =
             surround_sepby (Token_kind T_LBRACE) (Token_kind T_RBRACE)
               (Token_kind T_SEMI) parse_stmt state
           in
           let loc_ = loc_start_with state loc_start in
           Compact.compact_rev (List.rev stmts) loc_
       | Panic { loc; _ } ->
           add_error_skipped state "block_expr" loc;
           Pexpr_hole { loc_ = loc; kind = Synthesized })
    : Syntax.expr)

and parse_stmt (state : parse_state) =
  (debugged "stmt" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | LET -> (
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_LET) state;
                match peek_token state with
                | MUTABLE ->
                    expected_token (Token_kind T_MUTABLE) state;
                    let binder = parse_binder state in
                    let ty_opt = parse_opt_annot state in
                    expected_token (Token_kind T_EQUAL) state;
                    let expr = parse_expr state in
                    let loc = loc_start_with state loc_start in
                    Stmt_letmut { binder; ty_opt; expr; loc }
                | _ ->
                    let pat = parse_pattern state in
                    let ty_opt = parse_opt_annot state in
                    let pat : Syntax.pattern =
                      match ty_opt with
                      | Some ty ->
                          Ppat_constraint
                            {
                              pat;
                              ty;
                              loc_ =
                                Rloc.merge (loc_of_pattern pat)
                                  (loc_of_type_expression ty);
                            }
                      | None -> pat
                    in
                    expected_token (Token_kind T_EQUAL) state;
                    let expr = parse_expr state in
                    let loc = loc_start_with state loc_start in
                    Stmt_let { pat; expr; loc })
            | ASYNC
              when peek_nth_token_kind state 1 = Token_kind T_FN
                   && peek_nth_token_kind state 2 = Token_kind T_LIDENT ->
                let loc_start = peek_loc_start state in
                skip_this state;
                skip_this state;
                let binder = parse_binder state in
                let has_error = parse_optional_bang state in
                let func =
                  parse_func loc_start ~has_error ~is_async:true state
                in
                let loc = loc_start_with state loc_start in
                Stmt_func { binder; func; loc }
            | FN when peek_nth_token_kind state 1 = Token_kind T_LIDENT ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_FN) state;
                let binder = parse_binder state in
                let has_error = parse_optional_bang state in
                let func =
                  parse_func loc_start ~has_error ~is_async:false state
                in
                let loc = loc_start_with state loc_start in
                Stmt_func { binder; func; loc }
            | GUARD -> (
                let loc_start = peek_loc_start state in
                skip_this state;
                match peek_token state with
                | LET ->
                    skip_this state;
                    push_sync state (Token_kind T_ELSE);
                    let pat = parse_pattern state in
                    expected_token (Token_kind T_EQUAL) state;
                    let expr = parse_infix_expr state in
                    pop_sync state (Token_kind T_ELSE);
                    let otherwise =
                      match peek_token state with
                      | ELSE ->
                          skip_this state;
                          Some
                            (surround_sepby (Token_kind T_LBRACE)
                               (Token_kind T_RBRACE) (Token_kind T_SEMI)
                               parse_case state)
                      | _ -> None
                    in
                    let loc = loc_start_with state loc_start in
                    Parsing_compact.Stmt_guard_let { pat; expr; otherwise; loc }
                | _ ->
                    let cond =
                      with_sync state (Token_kind T_ELSE) parse_infix_expr
                    in
                    let otherwise =
                      match peek_token state with
                      | ELSE ->
                          skip_this state;
                          Some (parse_block_expr state)
                      | _ -> None
                    in
                    let loc = loc_start_with state loc_start in
                    Parsing_compact.Stmt_guard { cond; otherwise; loc })
            | _ -> Stmt_expr { expr = parse_expr_stmt state })
        | Panic { loc; _ } ->
            add_error_skipped state "stmt" loc;
            Stmt_expr { expr = Pexpr_hole { loc_ = loc; kind = Synthesized } }
         : Compact.semi_expr_prop))
    : Compact.semi_expr_prop)

and parse_assign_or_simple_expr state =
  (let loc_start = peek_loc_start state in
   let lhs = parse_expr state in
   match peek_token state with
   | EQUAL -> (
       expected_token (Token_kind T_EQUAL) state;
       let rhs = parse_expr state in
       let loc_ = loc_start_with state loc_start in
       match Parser_util.make_assign_opt ~loc_ lhs rhs with
       | Some res -> res
       | None ->
           add_error state (Errors.invalid_left_value (loc_of_expression lhs));
           lhs)
   | AUGMENTED_ASSIGNMENT op -> (
       let loc_ = peek_location state in
       let op : Syntax.var = { var_name = Lident op; loc_ } in
       expected_token (Token_kind T_AUGMENTED_ASSIGNMENT) state;
       let rhs = parse_expr state in
       let loc_ = loc_start_with state loc_start in
       match Parser_util.make_augmented_assign_opt ~loc_ op lhs rhs with
       | Some res -> res
       | None ->
           add_error state (Errors.invalid_left_value (loc_of_expression lhs));
           lhs)
   | _ -> lhs
    : Syntax.expr)

and parse_expr_stmt (state : parse_state) =
  (debugged "expr_stmt" state (fun state ->
       (match get_mode state with
        | Normal -> (
            match peek_token state with
            | BREAK ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_BREAK) state;
                let label : Syntax.label option =
                  match peek_token state with
                  | POST_LABEL label_name ->
                      skip_this state;
                      Some { label_name; loc_ = peek_location state }
                  | _ -> None
                in
                let arg = option first_expr parse_expr state in
                Pexpr_break
                  { arg; loc_ = loc_start_with state loc_start; label }
            | CONTINUE ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_CONTINUE) state;
                let label : Syntax.label option =
                  match peek_token state with
                  | POST_LABEL label_name ->
                      skip_this state;
                      Some { label_name; loc_ = peek_location state }
                  | _ -> None
                in
                let args =
                  sepby (Token_kind T_COMMA) first_expr parse_expr state
                in
                Pexpr_continue
                  { args; loc_ = loc_start_with state loc_start; label }
            | RETURN ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_RETURN) state;
                let expr = option first_expr parse_expr state in
                let loc_ = loc_start_with state loc_start in
                Pexpr_return { return_value = expr; loc_ }
            | RAISE ->
                let loc_start = peek_loc_start state in
                expected_token (Token_kind T_RAISE) state;
                let expr = parse_expr state in
                let loc_ = loc_start_with state loc_start in
                Pexpr_raise { err_value = expr; loc_ }
            | ELLIPSIS ->
                let loc_ = peek_location state in
                skip_this state;
                Pexpr_hole { loc_; kind = Todo }
            | _ -> parse_assign_or_simple_expr state)
        | Panic { loc; _ } ->
            add_error_skipped state "stmt" loc;
            Pexpr_hole { loc_ = loc; kind = Synthesized }
         : Syntax.expr))
    : Syntax.expr)

and parse_match_fn_body state =
  (debugged "match_fn_body" state (fun state ->
       match get_mode state with
       | Normal ->
           surround_sepby (Token_kind T_LBRACE) (Token_kind T_RBRACE)
             (Token_kind T_SEMI) parse_multi_pattern_case state
       | Panic { loc; _ } ->
           add_error_skipped state "match_fn_body" loc;
           [])
    : Syntax.multi_arg_case list)

and parse_return_type (state : parse_state) =
  (debugged "return_type" state (fun state ->
       match get_mode state with
       | Normal -> (
           let ty_res = parse_type state in
           match peek_token state with
           | EXCLAMATION ->
               let loc_ = peek_location state in
               expected_token (Token_kind T_EXCLAMATION) state;
               let ty_err : Syntax.error_typ =
                 match option first_type parse_type state with
                 | Some ty -> Error_typ { ty }
                 | None -> Default_error_typ { loc_ }
               in
               (ty_res, ty_err)
           | _ -> (ty_res, No_error_typ))
       | Panic { loc; _ } ->
           add_error_skipped state "return_type" loc;
           (Ptype_any { loc_ = loc }, No_error_typ))
    : Syntax.typ * Syntax.error_typ)

and parse_type (state : parse_state) =
  (let rec parse_question_mark state loc_start ty =
     match peek_token state with
     | QUESTION ->
         let constr_loc = peek_location state in
         expected_token (Token_kind T_QUESTION) state;
         let loc_ = loc_start_with state loc_start in
         let ty = Parsing_util.make_Ptype_option ~loc_ ~constr_loc ty in
         parse_question_mark state loc_start ty
     | _ -> ty
   in
   debugged "type" state (fun state ->
       match get_mode state with
       | Normal ->
           let loc_start = peek_loc_start state in
           let ty : Syntax.typ =
             match peek_token state with
             | PACKAGE_NAME _ | LIDENT _ | UIDENT _ ->
                 let name_loc = peek_location state in
                 let name = parse_qual_luident state in
                 let args =
                   option [ Token_kind T_LBRACKET ]
                     (surround_sepby (Token_kind T_LBRACKET)
                        (Token_kind T_RBRACKET) (Token_kind T_COMMA) parse_type)
                     state
                 in
                 let loc_ = loc_start_with state loc_start in
                 Ptype_name
                   {
                     constr_id = { lid = name; loc_ = name_loc };
                     tys = Option.value ~default:[] args;
                     loc_;
                   }
             | ASYNC ->
                 skip_this state;
                 let tys =
                   surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                     (Token_kind T_COMMA) parse_type state
                 in
                 expected_token (Token_kind T_THIN_ARROW) state;
                 let ty_res, ty_err = parse_return_type state in
                 let loc_ = loc_start_with state loc_start in
                 Ptype_arrow
                   { ty_arg = tys; ty_res; ty_err; is_async = true; loc_ }
             | LPAREN -> (
                 let tys =
                   surround_sepby (Token_kind T_LPAREN) (Token_kind T_RPAREN)
                     (Token_kind T_COMMA) parse_type state
                 in
                 match tys with
                 | [] ->
                     expected_token (Token_kind T_THIN_ARROW) state;
                     let ty_res, ty_err = parse_return_type state in
                     let loc_ = loc_start_with state loc_start in
                     Ptype_arrow
                       { ty_arg = []; ty_res; ty_err; is_async = false; loc_ }
                 | tys -> (
                     match peek_token state with
                     | THIN_ARROW ->
                         expected_token (Token_kind T_THIN_ARROW) state;
                         let ty_res, ty_err = parse_return_type state in
                         let loc_ = loc_start_with state loc_start in
                         Ptype_arrow
                           {
                             ty_arg = tys;
                             ty_res;
                             ty_err;
                             is_async = false;
                             loc_;
                           }
                     | _ -> (
                         match tys with
                         | [] -> failwith "unreachable!"
                         | x :: [] -> x
                         | _ :: _ ->
                             let loc_ = loc_start_with state loc_start in
                             Ptype_tuple { tys; loc_ })))
             | UNDERSCORE ->
                 let loc_ = peek_location state in
                 expected_token (Token_kind T_UNDERSCORE) state;
                 Ptype_any { loc_ }
             | AMPER ->
                 skip_this state;
                 let loc_start = peek_loc_start state in
                 let lid = parse_qual_luident state in
                 let loc_ = loc_start_with state loc_start in
                 Ptype_object { lid; loc_ }
             | other ->
                 let other_loc = peek_location state in
                 add_error_failed_to_parse state other "type" other_loc;
                 let loc_ = parser_panic state in
                 Ptype_any { loc_ }
           in
           parse_question_mark state loc_start ty
       | Panic { loc; _ } ->
           add_error_skipped state "type" loc;
           Ptype_any { loc_ = loc })
    : Syntax.typ)
