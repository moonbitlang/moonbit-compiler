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


module Config = Basic_config

module Common_Opt = struct
  let elim_unused_let = ref true
  let shrink_wasm = ref true
  let rc = ref true
  let source_map = ref false
  let source_map_url = ref ""
  let debug_tokens = ref false
  let debug_segments = ref false
  let debug_source = ref false
  let contification = ref true
  let wasi = ref false
  let wat_plain_mode = ref false
  let dedup_wasm = ref false
  let quiet = ref false
  let no_intermediate_file = ref false
  let enable_coverage = ref false
  let enable_value_tracing = ref false
  let coverage_package_override = ref None
  let is_main = ref false

  type llvm_opt = O0 | O1 | O2 | O3 | Os | Oz

  let llvm_opt = ref O2
  let llvm_target : string option ref = ref None
  let emit_text_format = ref false
  let emit_llvm_ir = ref false

  let set_coverage_package_override pkg =
    if pkg = "@self" then coverage_package_override := Some None
    else coverage_package_override := Some (Some pkg)

  let warn_help =
    ( "-warn-help",
      Arg.Unit Warnings.help_warnings,
      " show description of warning numbers" )

  let set_llvm_opt s =
    llvm_opt :=
      match s with
      | "0" -> O0
      | "1" -> O1
      | "2" -> O2
      | "3" -> O3
      | "s" -> Os
      | "z" -> Oz
      | _ ->
          failwith
            "unknown llvm optimization level, should be 0, 1, 2, 3, s or z"

  let set_o0 () =
    Config.no_opt := true;
    llvm_opt := O0

  let warn_list =
    ( "-w",
      Arg.String (fun s -> Warnings.parse_options false s),
      " <list>  Enable or disable warnings according to <list>:\n\
       \t+<spec>   enable warnings in <spec>\n\
       \t-<spec>   disable warnings in <spec>\n\
       \t@<spec>   enable warnings in <spec> and treat them as errors\n\
       \t<spec> can be:\n\
       \t<num>             a single warning number\n\
       \t<num1>..<num2>    a range of consecutive warning numbers\n\
       \tdefault setting is " ^ Warnings.default_warnings )

  let alert_list =
    ( "-alert",
      Arg.String Alerts.parse_options,
      " <list>  Enable or disable warnings according to <list>:\n\
       \t+<id>   enable alert\n\
       \t-<id>   disable alert\n\
       \t@<id>   enable alert and treat it as error\n\
       \tIf <id> is 'all', is stands for all alerts\n\
       \tdefault setting is " ^ Alerts.default_alerts )

  let llvm_list =
    [
      ( "-llvm-opt",
        Arg.String set_llvm_opt,
        "set llvm optimization level. available levels: 0, 1, 2, 3, s, z. \
         (LLVM backend only)" );
      ( "-llvm-target",
        Arg.String (fun target -> llvm_target := Some target),
        "set llvm target triple. unset means current target. (LLVM backend \
         only)" );
      ( "-S",
        Arg.Set emit_text_format,
        "emit text format of the result instead of binary (LLVM backend only)"
      );
      ( "-emit-llvm-ir",
        Arg.Set emit_llvm_ir,
        "emit LLVM IR or bitcode instead of machine code (LLVM backend only)" );
    ]
end

module Linkcore_Opt = struct
  let output_file = ref ""
  let link_main = ref "main"
  let pkg_config_path = ref ""
  let exported_functions = Basic_hash_string.create 17
  let emit_dts = ref true
  let stop_on_main = ref false

  let spec =
    [
      ("-g", Arg.Set Config.debug, "save debugging information");
      ("-O0", Arg.Unit Common_Opt.set_o0, " turn off optimization");
      ("-source-map", Arg.Set Common_Opt.source_map, "emit sourcemap");
      ( "-source-map-url",
        Arg.Set_string Common_Opt.source_map_url,
        "set url of source map" );
      ( "-pkg-sources",
        Arg.String
          (fun pair ->
            let name, src_dir = Basic_strutil.split_on_first ':' pair in
            Pkg_path_tbl.add_pkg Loc.pkg_path_tbl name src_dir),
        "Tell compiler where the package sources are. format: <package \
         name>:<package source directory>" );
      ( "-o",
        Arg.Set_string output_file,
        "output file, suffix with .wat or .wasm" );
      ( "-wat-plain-mode",
        Arg.Set Common_Opt.wat_plain_mode,
        " output plain style wat" );
      ( "-target",
        Arg.Symbol
          ( [ "wasm-gc"; "wasm"; "js"; "native"; "llvm" ],
            fun target -> Config.target := Basic_config.parse_target_exn target
          ),
        "set compilation target. available targets: wasm, wasm-gc, js, native"
      );
      ("-no-dts", Arg.Clear emit_dts, "Do not emit typescript declaration file");
      ("-stop-on-main", Arg.Set stop_on_main, "stop on main function");
      ( "-js-format",
        Arg.Symbol
          ( [ "esm"; "cjs"; "iife" ],
            fun fmt ->
              Config.js_format :=
                match fmt with
                | "esm" -> Esm
                | "cjs" -> Cjs
                | "iife" -> Iife
                | _ -> assert false ),
        "set js format" );
      ( "-main",
        Arg.Set_string link_main,
        "the \"main\" package in the process of linking, whose public \
         definitions will be exported" );
      ("-dsource", Arg.Set Common_Opt.debug_source, "debug source");
      ( "-exported_functions",
        Arg.String
          (fun target ->
            Exported_functions.parse_exported_functions target
              exported_functions),
        "exported functions in the package, format: \
         <function1>:<export_name1>,<function2>:<export_name2>,... where the \
         export name can be omitted and the original name will be used." );
      ( "-export-memory-name",
        Arg.String (fun name -> Config.export_memory_name := Some name),
        "customize exported memory name in wasm backend" );
      ( "-import-memory-module",
        Arg.String (fun name -> Config.import_memory_module := Some name),
        "specify the module of the memory to import" );
      ( "-import-memory-name",
        Arg.String (fun name -> Config.import_memory_name := Some name),
        "specify the name of the memory to import" );
      ( "-memory-limits-min",
        Arg.Int (fun min -> Config.memory_limits_min := Some min),
        "specify the minimum size of the memory" );
      ( "-memory-limits-max",
        Arg.Int (fun max -> Config.memory_limits_max := Some max),
        "specify the maximum size of the memory" );
      ( "-shared-memory",
        Arg.Set Config.shared_memory,
        "specify the memory to be shared" );
      ( "-heap-start-address",
        Arg.Set_int Config.heap_memory_start,
        "reserve the memory below the start address" );
      ( "-no-block-params",
        Arg.Clear Basic_config.use_block_params,
        "not using block params for compatibility with binaryen" );
      ("-test-mode", Arg.Set Config.test_mode, "enable test mode");
      ("-no-rc", Arg.Clear Common_Opt.rc, "disable reference counting");
      ( "-memory-safety-check",
        Arg.Set Config.memory_safety_check,
        "enable memory safety check" );
      ( "-leak-check",
        Arg.Set Config.leak_check,
        "enable memory leak check for wasm_linear backend" );
      ( "-use-js-builtin-string",
        Arg.Set Config.use_js_builtin_string,
        "use js builtin string" );
      ( "-pkg-config-path",
        Arg.String (fun path -> pkg_config_path := path),
        "path to moon.pkg.json" );
    ]
    @ Common_Opt.llvm_list
end

module Buildpkg_Opt = struct
  let mi_files = ref []
  let output_file = ref ""
  let no_mi = ref false
  let patch_file = ref ""
  let blackbox_test = ref false
  let whitebox_test = ref false

  let spec =
    [
      ( "-i",
        Arg.String (fun mi -> mi_files := mi :: !mi_files),
        "dependent .mi files" );
      ("-g", Arg.Set Config.debug, "save debugging information");
      ("-O0", Arg.Set Config.no_opt, " turn off optimization");
      ("-source-map", Arg.Set Common_Opt.source_map, "emit sourcemap");
      ( "-source-map-url",
        Arg.Set_string Common_Opt.source_map_url,
        "set url of source map" );
      ("-dsource", Arg.Set Common_Opt.debug_source, "debug source");
      ( "-o",
        Arg.Set_string output_file,
        "output file (suffix with .core by convention)" );
      ( "-target",
        Arg.Symbol
          ( [ "wasm-gc"; "wasm"; "js"; "native"; "llvm" ],
            fun target -> Config.target := Basic_config.parse_target_exn target
          ),
        "set compilation target. available targets: wasm, wasm-gc, js, native"
      );
      ("-pkg", Arg.Set_string Basic_config.current_package, "package name");
      ( "-pkg-sources",
        Arg.String
          (fun pair ->
            let name, src_dir = Basic_strutil.split_on_first ':' pair in
            Pkg_path_tbl.add_pkg Loc.pkg_path_tbl name src_dir),
        "Tell compiler where the package sources are. format: <package \
         name>:<package source directory>" );
      ( "-std-path",
        Arg.Set_string Basic_config.std_path,
        "path containing standard library .mi files" );
      ( "-enable-coverage",
        Arg.Set Common_Opt.enable_coverage,
        " Enable code coverage tracking" );
      ("-enable-value-tracing", Arg.Set Common_Opt.enable_value_tracing, "");
      ( "-coverage-package-override",
        Arg.String Common_Opt.set_coverage_package_override,
        "Override package name for coverage tracking. use \"@self\" for \
         current package." );
      ("-is-main", Arg.Set Common_Opt.is_main, "main package");
      ("-blackbox-test", Arg.Set blackbox_test, "check blackbox tests");
      ("-whitebox-test", Arg.Set whitebox_test, "check whitebox tests");
      ( "-error-format",
        Arg.Symbol
          ( [ "human"; "json" ],
            fun error_format ->
              Config.error_format :=
                Basic_config.parse_error_format_exn error_format ),
        " set format of diagnostics. available formats: human, json" );
      ("-patch-file", Arg.Set_string patch_file, "patch file");
      ("-no-mi", Arg.Set no_mi, "don't generate .mi file");
    ]
    @ Common_Opt.llvm_list
    @ [ Common_Opt.warn_help; Common_Opt.warn_list; Common_Opt.alert_list ]
end

module Check_Opt = struct
  let mi_files = ref []
  let output_file = ref ""
  let no_mi = ref false
  let patch_file = ref ""
  let blackbox_test = ref false
  let whitebox_test = ref false

  let spec =
    [
      ( "-i",
        Arg.String (fun mi -> mi_files := mi :: !mi_files),
        "dependent .mi files" );
      ( "-o",
        Arg.Set_string output_file,
        "output file (suffix with .mi by convention)" );
      ( "-target",
        Arg.Symbol
          ( [ "wasm-gc"; "wasm"; "js"; "native"; "llvm" ],
            fun target -> Config.target := Basic_config.parse_target_exn target
          ),
        "set compilation target. available targets: wasm, wasm-gc, js, native"
      );
      ("-pkg", Arg.Set_string Basic_config.current_package, "package name");
      ( "-pkg-sources",
        Arg.String
          (fun pair ->
            let name, src_dir = Basic_strutil.split_on_first ':' pair in
            Pkg_path_tbl.add_pkg Loc.pkg_path_tbl name src_dir),
        "Tell compiler where the package sources are. format: <package \
         name>:<package source directory>" );
      ( "-std-path",
        Arg.Set_string Basic_config.std_path,
        "path containing standard library .mi files" );
      ("-is-main", Arg.Set Common_Opt.is_main, "main package");
      ("-blackbox-test", Arg.Set blackbox_test, "check blackbox tests");
      ("-whitebox-test", Arg.Set whitebox_test, "check whitebox tests");
      ( "-error-format",
        Arg.Symbol
          ( [ "human"; "json" ],
            fun error_format ->
              Config.error_format :=
                Basic_config.parse_error_format_exn error_format ),
        " set format of diagnostics. available formats: human, json" );
      ("-patch-file", Arg.Set_string patch_file, "patch file");
      ("-no-mi", Arg.Set no_mi, "don't generate .mi file");
      Common_Opt.warn_help;
      Common_Opt.warn_list;
      Common_Opt.alert_list;
    ]
end

module Compile_Opt = struct
  let output_file = ref ""
  let extra_deps = ref []
  let export_functions_info = ref None
  let wat_stats = ref false
  let emit_dts = ref true
  let stop_after_parsing = ref false
  let stop_after_typing = ref false
  let stop_on_main = ref false

  let spec =
    [
      ("-q", Arg.Set Common_Opt.quiet, "quite mode");
      ("-verbose", Arg.Set Config.verbose, " verbose mode for debugging");
      ("-g", Arg.Set Config.debug, "save debugging information");
      ("-O0", Arg.Set Config.no_opt, " turn off optimization");
      ("-dsource", Arg.Set Common_Opt.debug_source, "debug source");
      ("-source-map", Arg.Set Common_Opt.source_map, "emit sourcemap");
      ( "-source-map-url",
        Arg.Set_string Common_Opt.source_map_url,
        "set url of source map" );
      ("-no-dts", Arg.Clear emit_dts, "Do not emit typescript declaration file");
      ("-stop-on-main", Arg.Set stop_on_main, "stop on main function");
      ( "-o",
        Arg.Set_string output_file,
        "output file, suffix with .wat or .wasm" );
      ( "-pass-no-unused-let",
        Arg.Clear Common_Opt.elim_unused_let,
        " remove some unused binding created by let or letrec" );
      ( "-wat-stats",
        Arg.Set wat_stats,
        " emit binary size information in .wat.stats" );
      ( "-wat-plain-mode",
        Arg.Set Common_Opt.wat_plain_mode,
        " output plain style wat" );
      ( "-pass-no-shrink-wasm",
        Arg.Clear Common_Opt.shrink_wasm,
        " remove unused runtime from wasm" );
      ( "-pass-no-contification",
        Arg.Clear Common_Opt.contification,
        " remove contification pass" );
      ("-no-rc", Arg.Clear Common_Opt.rc, " disable reference counting GC");
      ( "-target",
        Arg.Symbol
          ( [ "wasm-gc"; "wasm"; "js"; "native"; "llvm" ],
            fun target -> Config.target := Basic_config.parse_target_exn target
          ),
        " set compilation target. available targets: wasm, wasm-gc, js, native"
      );
      ( "-js-format",
        Arg.Symbol
          ( [ "esm"; "cjs"; "iife" ],
            fun fmt ->
              Config.js_format :=
                match fmt with
                | "esm" -> Esm
                | "cjs" -> Cjs
                | "iife" -> Iife
                | _ -> assert false ),
        "set js format" );
      ("-wasi", Arg.Set Common_Opt.wasi, " compile to WASI");
      ("-debug-tokens", Arg.Set Common_Opt.debug_tokens, " debug token stream");
      ("-debug-segments", Arg.Set Common_Opt.debug_segments, " debug segments");
      ( "-parser",
        Arg.Symbol (Basic_config.symbols, Basic_config.parse_symbol_exn),
        " option to set the parser, default: "
        ^ Basic_config.current_parser_display () );
      ("-stop-after-parsing", Arg.Set stop_after_parsing, " stop after parsing");
      ("-stop-after-typing", Arg.Set stop_after_typing, " stop after typing");
      ("-show-loc", Arg.Set Basic_config.show_loc, " show loc in ir");
      ( "-no-intermediate-file",
        Arg.Set Common_Opt.no_intermediate_file,
        " don't generate intermediate IR files" );
      ( "-enable-coverage",
        Arg.Set Common_Opt.enable_coverage,
        " Enable code coverage tracking" );
      ("-enable-value-tracing", Arg.Set Common_Opt.enable_value_tracing, "");
      ( "-coverage-package-override",
        Arg.String Common_Opt.set_coverage_package_override,
        "Override package name for coverage tracking. use \"@self\" for \
         current package." );
      ( "-std-path",
        Arg.Set_string Basic_config.std_path,
        "path containing standard library .mi files" );
      ( "-l",
        Arg.String (fun dep -> extra_deps := dep :: !extra_deps),
        "[.core] binary of extra dependencies" );
      ( "-exported_functions",
        Arg.String
          (fun target ->
            let exported_functions = Basic_hash_string.create 17 in
            Exported_functions.parse_exported_functions target
              exported_functions;
            export_functions_info :=
              Some (Exported_functions.Export_selected exported_functions)),
        "exported functions in the package, format: \
         <function1>:<export_name1>,<function2>:<export_name2>,... where the \
         export name can be omitted and the original name will be used." );
      ( "-export-memory-name",
        Arg.String (fun name -> Config.export_memory_name := Some name),
        "customize exported memory name in wasm backend" );
      ( "-no-block-params",
        Arg.Clear Basic_config.use_block_params,
        "not using block params for compatibility with binaryen" );
      ("-test-mode", Arg.Set Config.test_mode, "enable test mode");
      ( "-error-format",
        Arg.Symbol
          ( [ "human"; "json" ],
            fun error_format ->
              Config.error_format :=
                Basic_config.parse_error_format_exn error_format ),
        " set format of diagnostics. available formats: human, json" );
      ( "-leak-check",
        Arg.Set Config.leak_check,
        "enable memory leak check for wasm_linear backend" );
      ( "-memory-safety-check",
        Arg.Set Config.memory_safety_check,
        "enable memory safety check" );
      ( "-use-js-builtin-string",
        Arg.Set Config.use_js_builtin_string,
        "use js builtin string" );
    ]
    @ Common_Opt.llvm_list
    @ [ Common_Opt.warn_help; Common_Opt.warn_list; Common_Opt.alert_list ]
end

module Gentestinfo_Opt = struct
  let patch_file = ref ""
  let json = ref false

  let spec =
    [
      ("-json", Arg.Set json, "output in json format");
      ("-patch-file", Arg.Set_string patch_file, "patch file");
    ]
end
