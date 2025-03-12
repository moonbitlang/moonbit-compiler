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


module Map_string = Basic_map_string

let error loc e =
  raise (Json_lexer.Error (Loc.get_start loc, Loc.get_end loc, e))

let parse_json (tokens : (Json_lexer.token * Loc.t) Basic_vec.t) =
  let curr = ref 0 in
  let next_token () =
    (let t = Basic_vec.get tokens !curr in
     incr curr;
     t
      : Json_lexer.token * Loc.t)
  in
  let peek_token () = (Basic_vec.get tokens !curr : Json_lexer.token * Loc.t) in
  let skip () = incr curr in
  let rec json () =
    (match next_token () with
     | True, loc -> True loc
     | False, loc -> False loc
     | Null, loc -> Null loc
     | Number s, loc -> Float { float = s; loc }
     | String s, loc -> Str { str = s; loc }
     | Lbracket, loc -> parse_array loc []
     | Lbrace, loc -> parse_map loc Map_string.empty
     | _, loc -> error loc Unexpected_token
      : Json_types.t)
  and parse_array loc_start acc =
    (match peek_token () with
     | Rbracket, loc_end ->
         skip ();
         Arr
           {
             loc = Loc.merge loc_start loc_end;
             content = Basic_arr.reverse_of_list acc;
           }
     | _ -> (
         let item = json () in
         match next_token () with
         | Comma, _ -> parse_array loc_start (item :: acc)
         | Rbracket, loc_end ->
             Arr
               {
                 content = Basic_arr.reverse_of_list (item :: acc);
                 loc = Loc.merge loc_start loc_end;
               }
         | _, loc -> error loc Expect_comma_or_rbracket)
      : Json_types.t)
  and parse_map loc_start acc =
    (match next_token () with
     | Rbrace, loc_end -> Obj { map = acc; loc = Loc.merge loc_start loc_end }
     | String key, _ -> (
         match next_token () with
         | Colon, _ -> (
             let value = json () in
             match next_token () with
             | Rbrace, loc_end ->
                 Obj
                   {
                     map = Map_string.add acc key value;
                     loc = Loc.merge loc_start loc_end;
                   }
             | Comma, _ -> parse_map loc_start (Map_string.add acc key value)
             | _, loc -> error loc Expect_comma_or_rbrace)
         | _, loc -> error loc Expect_colon)
     | _, loc -> error loc Expect_string_or_rbrace
      : Json_types.t)
  in
  let v = json () in
  match peek_token () with Eof, _ -> v | _, loc -> error loc Expect_eof

let parse_string ~diagnostics ?fname s =
  try
    let tokens = Json_lexer.lex_json ?fname s in
    parse_json tokens
  with Json_lexer.Error (loc_start, loc_end, err) ->
    let err_msg = Json_lexer.report_error err in
    Diagnostics.add_error diagnostics
      (Errors.json_parse_error ~loc_start ~loc_end err_msg);
    Null Loc.no_location

let parse_json_from_string ~diagnostics ?fname s =
  parse_string ~diagnostics ?fname s

let parse_json_from_file ~diagnostics ?fname path =
  let str = In_channel.with_open_bin path In_channel.input_all in
  parse_string ~diagnostics ?fname str
