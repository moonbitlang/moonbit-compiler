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
module Lst = Basic_lst

type test_info = {
  index : int;
  func : string;
  has_args : bool;
  name : string option;
}

let test_info_to_mbt_tuple = function
  | { func; name = Some name; _ } ->
      Stdlib.String.concat ""
        [ "("; func; ", [\""; name; "\"])" ] [@merlin.hide]
  | { func; name = None; _ } ->
      ("(" ^ func ^ ", [])" : Stdlib.String.t) [@merlin.hide]

let test_info_to_mbt_index_pair = function
  | { index; func; name = Some name; _ } ->
      let index = Int.to_string index in
      (Stdlib.String.concat ""
         [ index; ": ("; func; ", [\""; name; "\"])" ] [@merlin.hide])
  | { index; func; name = None; _ } ->
      let index = Int.to_string index in
      (Stdlib.String.concat "" [ index; ": ("; func; ", [])" ] [@merlin.hide])

let tests_of_file filename impls =
  (let rec aux i acc = function
     | [] -> List.rev acc
     | impl :: impls -> (
         match impl with
         | Syntax.Ptop_test { name; params; _ } ->
             let name =
               match name with
               | Some name -> Some name.v.string_repr
               | None -> None
             in
             let func = Test_util.gen_test_name filename i in
             let has_args =
               match params with None -> false | Some _ -> true
             in
             let info = { index = i; func; has_args; name } in
             aux (i + 1) (info :: acc) impls
         | _ -> aux i acc impls)
   in
   aux 0 [] impls
    : test_info list)

let extract_tests (input : (string * Syntax.impls) list) =
  let tests =
    Lst.map input (fun (path, impl) ->
        let filename = Filename.basename path in
        let tests = tests_of_file filename impl in
        (filename, tests))
  in
  let no_args_tests =
    Lst.map tests (fun (filename, tests) ->
        (filename, Lst.filter tests (fun test -> not test.has_args)))
  in
  let with_args_tests =
    Lst.map tests (fun (filename, tests) ->
        (filename, Lst.filter tests (fun test -> test.has_args)))
  in
  (tests, no_args_tests, with_args_tests)

let gen_test_info (input : (string * Syntax.impls) list) =
  let _, no_args_map, with_args_map = extract_tests input in
  let file_tests_to_string (filename, tests) =
    let filename = Basic_strutil.escaped_string filename in
    let tests = String.concat "," (Lst.map tests test_info_to_mbt_index_pair) in
    Stdlib.String.concat "" [ " \""; filename; "\": {"; tests; "} " ]
  in
  let no_args_map_str =
    let str = String.concat "," (Lst.map no_args_map file_tests_to_string) in
    ("let no_args_tests = {" ^ str ^ "}" : Stdlib.String.t)
  in
  let with_args_map_str =
    let str = String.concat "," (Lst.map with_args_map file_tests_to_string) in
    ("let with_args_tests = {" ^ str ^ "}" : Stdlib.String.t)
  in
  let old_tests_str =
    let tests =
      String.concat ","
        (Lst.map no_args_map (fun (filename, tests) ->
             let filename = Basic_strutil.escaped_string filename in
             let tests =
               String.concat "," (Lst.map tests test_info_to_mbt_tuple)
             in
             Stdlib.String.concat "" [ " \""; filename; "\": ["; tests; "] " ]))
    in
    (" let tests = {" ^ tests ^ "} " : Stdlib.String.t)
  in
  String.concat "\n  " [ old_tests_str; no_args_map_str; with_args_map_str ]

let gen_test_info_json (input : (string * Syntax.impls) list) =
  let tests, no_args_tests, with_args_tests = extract_tests input in
  let json_of_test_info (test_info : test_info) =
    `Assoc
      [
        ("index", `Int test_info.index);
        ("func", `String test_info.func);
        ("name", match test_info.name with Some n -> `String n | None -> `Null);
      ]
  in
  let json_of_test_map (test_map : (string * test_info list) list) =
    `Assoc
      (Lst.map test_map (fun (filename, tests) ->
           let tests_json =
             (fun tl -> `List tl) (Lst.map tests json_of_test_info)
           in
           (filename, tests_json)))
  in
  let j =
    `Assoc
      [
        ("tests", json_of_test_map tests);
        ("no_args_tests", json_of_test_map no_args_tests);
        ("with_args_tests", json_of_test_map with_args_tests);
      ]
  in
  Json.to_string j
