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

type target = Wasm_gc

include struct
  let _ = fun (_ : target) -> ()
  let sexp_of_target = (function Wasm_gc -> S.Atom "Wasm_gc" : target -> S.t)
  let _ = sexp_of_target

  let (hash_fold_target : Ppx_base.state -> target -> Ppx_base.state) =
    (fun hsv ->
       fun arg -> Ppx_base.hash_fold_int hsv (match arg with Wasm_gc -> 1)
      : Ppx_base.state -> target -> Ppx_base.state)

  let _ = hash_fold_target

  let (hash_target : target -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_target hsv arg)
    in
    fun x -> func x

  let _ = hash_target
  let equal_target = (Stdlib.( = ) : target -> target -> bool)
  let _ = equal_target
end

type js_format = Esm | Cjs | Iife
type error_format = Human | Json

let parse_target_exn = function "wasm-gc" -> Wasm_gc | _ -> assert false

let parse_error_format_exn = function
  | "human" -> Human
  | "json" -> Json
  | _ -> assert false

let mi_magic_str = "MINTF230520"
let core_magic_str = "MCORE240123"
let magic_str_len = String.length mi_magic_str

let input_magic_str ic =
  try Some (Stdlib.really_input_string ic magic_str_len)
  with End_of_file -> None

let default_package_name = ""
let show_loc = ref false
let show_doc = ref true
let current_package = ref default_package_name
let current_import_map : string Basic_map_string.t ref = ref Map_string.empty
let builtin_package = "moonbitlang/core/builtin"
let debug = ref false
let no_opt = ref false
let use_block_params = ref true
let test_mode = ref false
let std_path = ref ""
let export_memory_name : string option ref = ref None
let import_memory_module : string option ref = ref None
let import_memory_name : string option ref = ref None
let memory_limits_min : int option ref = ref None
let memory_limits_max : int option ref = ref None
let shared_memory : bool ref = ref false
let heap_memory_start = ref 10_000
let target = ref Wasm_gc
let js_format = ref Esm
let error_format = ref Human
let leak_check = ref false
let memory_safety_check = ref false
let use_js_builtin_string = ref false
let blackbox_test_import_all_default = true
let blackbox_test_import_all = ref blackbox_test_import_all_default
let const_string_module_name = ref "_"

type parser_select = [ `Menhir | `Handrolled | `Both ]

let parser : parser_select ref = ref `Both
let verbose = ref false

let parse_symbol_exn s =
  parser :=
    match s with
    | "yacc" -> `Menhir
    | "hand" -> `Handrolled
    | "both" -> `Both
    | _ -> assert false

let current_parser () = !parser
let symbols = [ "yacc"; "hand"; "both" ]

let current_parser_display () =
  match !parser with
  | `Menhir -> "yacc"
  | `Handrolled -> "hand"
  | `Both -> "both"

type env = Release | Debug

let env = Release
let block_line = "///|"
