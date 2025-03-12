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

type pkg_info = {
  pkg_loc : Loc.t;
  alias_info : (string * Loc.t) option;
  need_analyze_usage : bool;
  direct_uses : (string * Loc.t) list;
}

type import_kind = Wbtest | Bbtest | Normal
type import_items = pkg_info Map_string.t

let parse_import_item ~import_kind (pkg_config : Json_types.t option) =
  (let map = ref Map_string.empty in
   let add_item name info = map := Map_string.add !map name info in
   let parse_json_array arr ~need_analyze_usage =
     Array.iter
       (fun (item : Json_types.t) ->
         match item with
         | Str { str; loc = pkg_loc } ->
             add_item str
               {
                 pkg_loc;
                 alias_info = None;
                 need_analyze_usage;
                 direct_uses = [];
               }
         | Obj { map; loc = _ } -> (
             match Map_string.find_opt map "path" with
             | Some (Str { str; loc = pkg_loc }) ->
                 let alias_info =
                   match Map_string.find_opt map "alias" with
                   | Some (Str { str = alias_str; loc = alias_loc }) ->
                       Some (alias_str, alias_loc)
                   | _ -> None
                 in
                 let direct_uses =
                   match Map_string.find_opt map "value" with
                   | Some (Arr { content; _ }) ->
                       Array.fold_left
                         (fun acc ->
                           fun (use : Json_types.t) ->
                            match use with
                            | Str { str; loc } -> (str, loc) :: acc
                            | _ -> acc)
                         [] content
                   | _ -> []
                 in
                 add_item str
                   { pkg_loc; alias_info; need_analyze_usage; direct_uses }
             | _ -> ())
         | _ -> ())
       arr
   in
   let parse_import (obj : Json_types.t Basic_map_string.t) field_name
       ~need_analyze_usage =
     match Basic_map_string.find_opt obj field_name with
     | Some (Arr { content = arr; _ }) ->
         parse_json_array arr ~need_analyze_usage
     | _ -> ()
   in
   (match pkg_config with
   | Some (Obj { map; loc = _ }) -> (
       parse_import map "import" ~need_analyze_usage:(import_kind = Normal);
       match import_kind with
       | Normal -> ()
       | Wbtest -> parse_import map "wbtest-import" ~need_analyze_usage:true
       | Bbtest ->
           (match Basic_map_string.find_opt map "test-import-all" with
           | Some (True _) -> Basic_config.blackbox_test_import_all := true
           | Some (False _) -> Basic_config.blackbox_test_import_all := false
           | _ -> ());
           parse_import map "test-import" ~need_analyze_usage:true)
   | _ -> ());
   !map
    : import_items)

type is_main_info = Is_main of Loc.t | Not_main

let parse_is_main (pkg_config : Json_types.t option) =
  (match pkg_config with
   | Some (Obj { map; loc = _ }) -> (
       match Map_string.find_opt map "is-main" with
       | Some (True loc) -> Is_main loc
       | _ -> (
           match Map_string.find_opt map "is_main" with
           | Some (True loc) -> Is_main loc
           | _ -> Not_main))
   | _ -> Not_main
    : is_main_info)

let parse_memory_limits (memory_limits_config : Json_types.t option) =
  (match memory_limits_config with
   | Some (Obj { map; loc = _ }) -> (
       (match Map_string.find_opt map "min" with
       | Some (Float { float = min; _ }) ->
           Basic_config.memory_limits_min := int_of_string_opt min
       | _ -> ());
       match Map_string.find_opt map "max" with
       | Some (Float { float = max; _ }) ->
           Basic_config.memory_limits_max := int_of_string_opt max
       | _ -> ())
   | _ -> ()
    : unit)

let parse_import_memory (import_memory_config : Json_types.t option) =
  (match import_memory_config with
   | Some (Obj { map; loc = _ }) -> (
       (match Map_string.find_opt map "name" with
       | Some (Str { str; _ }) -> Basic_config.import_memory_name := Some str
       | _ -> ());
       match Map_string.find_opt map "module" with
       | Some (Str { str; _ }) -> Basic_config.import_memory_module := Some str
       | _ -> ())
   | _ -> ()
    : unit)

let parse_shared_memory (shared_memory : Json_types.t option) =
  (match shared_memory with
   | Some (True _) -> Basic_config.shared_memory := true
   | _ -> ()
    : unit)

let link_core_load_pkg_config (pkg_config : Json_types.t) =
  (match pkg_config with
   | Obj { map; loc = _ } -> (
       match Map_string.find_opt map "link" with
       | Some (Obj { map; loc = _ }) -> (
           (match Map_string.find_opt map "wasm-gc" with
           | Some (Obj { map; loc = _ }) ->
               (match Map_string.find_opt map "use-js-builtin-string" with
               | Some (True _) -> Basic_config.use_js_builtin_string := true
               | _ -> ());
               (match Map_string.find_opt map "imported-string-constants" with
               | Some (Str { str; _ }) ->
                   Basic_config.const_string_module_name := str
               | _ -> ());
               parse_import_memory (Map_string.find_opt map "import-memory");
               parse_memory_limits (Map_string.find_opt map "memory-limits");
               parse_shared_memory (Map_string.find_opt map "shared-memory")
           | _ -> ());
           match Map_string.find_opt map "wasm" with
           | Some (Obj { map; loc = _ }) ->
               parse_import_memory (Map_string.find_opt map "import-memory");
               parse_memory_limits (Map_string.find_opt map "memory-limits");
               parse_shared_memory (Map_string.find_opt map "shared-memory")
           | _ -> ())
       | _ -> ())
   | _ -> ()
    : unit)

let parse_warn_alert (pkg_config : Json_types.t option) =
  (match pkg_config with
   | Some (Obj { map; loc = _ }) -> (
       (match Map_string.find_opt map "warn-list" with
       | Some (Str { str; _ }) -> Warnings.parse_options false str
       | _ -> ());
       match Map_string.find_opt map "alert-list" with
       | Some (Str { str; _ }) -> Alerts.parse_options str
       | _ -> ())
   | _ -> ()
    : unit)
