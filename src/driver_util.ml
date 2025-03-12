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


module Lst = Basic_lst

let ( |-> ) obj callback =
  callback obj;
  obj

type core_passes =
  [ `Core_Contify
  | `Core_DCE
  | `Core_End
  | `Core_Inline_Single_Use_Join
  | `Core_Lambda_Lift
  | `Core_Remove_Let_Alias
  | `Core_Stackalloc
  | `Core_Unbox_Loop_Params
  | `Core_Propagate_Constr
  | `Core_Propagate_Constr
  | `Core_Start
  | `Core_no_async ]

type clam_passes = [ `Clam_End | `Clam_Start | `Clam_Unused_Let ]
type mbt_input = File_Path of string | Name_Content of string * string

type mi_input = Typecheck_driver_util.mi_input =
  | Import_Path of string
  | Import_Path_Content of string * string

type std_input = Typecheck_driver_util.std_input =
  | Std_Path of string
  | Std_Content of (string * string) list

type target =
  | Wasm_gc of {
      clam_callback : clam_passes -> Clam.prog -> unit;
      dwarfsm_callback : Dwarfsm_ast.module_ -> unit;
    }

let parse ~diagnostics ~(debug_tokens : bool) (input : mbt_input) =
  (match input with
   | File_Path path ->
       Parsing_parse.parse ~diagnostics ~debug_tokens ~transform:false path
   | Name_Content (name, content) ->
       Parsing_parse.impl_of_string ~name ~debug_tokens ~diagnostics
         ~transform:false content
    : Parsing_parse.output)

let postprocess_ast ~diagnostics (output : Parsing_parse.output) =
  ({ output with ast = Parsing_ast_lint.post_process ~diagnostics output.ast }
    : Parsing_parse.output)

let tast_of_ast = Typecheck_driver_util.tast_of_ast

let core_of_tast ~(no_opt : bool) ~(contification : bool) ~(genv : Global_env.t)
    ~(tast : Typedtree.output) ~core_callback =
  (let pass ~cond ~stage f prog =
     if cond then f prog |-> core_callback ~global_env:genv stage else prog
   in
   let prog = Core_of_tast.transl ~global_env:genv tast in
   pass ~cond:(not no_opt) ~stage:`Core_DCE Core_dce.eliminate_dead_code
     (pass ~cond:(not no_opt) ~stage:`Core_Lambda_Lift Lambda_lift.lift_program
        (pass ~cond:(not no_opt) ~stage:`Core_Propagate_Constr
           (Pass_propagate_constr.propagate_constr genv)
           (pass ~cond:(not no_opt) ~stage:`Core_Unbox_Loop_Params
              (Pass_unbox_loop_params.unbox_loop_params genv)
              (pass ~cond:(not no_opt) ~stage:`Core_Stackalloc
                 Pass_stackalloc.unbox_mut_records
                 (pass ~cond:(not no_opt) ~stage:`Core_Remove_Let_Alias
                    Pass_let_alias.remove_let_alias
                    (pass ~cond:contification ~stage:`Core_Contify
                       Pass_contification.contify
                       (Eliminate_async.eliminate_async ~global_env:genv
                          (pass ~cond:(not no_opt)
                             ~stage:`Core_Inline_Single_Use_Join
                             Pass_inline_single_use_join.inline_single_use_join
                             prog
                          |-> core_callback ~global_env:genv `Core_Start)
                       |-> core_callback ~global_env:genv `Core_no_async)))))))
   |-> core_callback ~global_env:genv `Core_End
    : Core.program)

let apply_patch_add (patches : Patch.t list) (mbt_files : mbt_input list) =
  (mbt_files
   @ Basic_lst.fold_left patches [] (fun acc ->
         fun patch ->
          match patch with
          | PAdd { name; content } -> Name_Content (name, content) :: acc
          | PDrop _ -> acc)
    : mbt_input list)

let apply_patch_drop ~(diagnostics : Diagnostics.t) (patches : Patch.t list)
    (ast : Parsing_parse.output) =
  (match ast.name with
   | "" -> ast
   | name -> (
       let patch =
         Basic_lst.find_opt patches (fun patch ->
             match patch with
             | PDrop { file; index = _ } when Filename.basename file = name ->
                 Some patch
             | _ -> None)
       in
       match patch with
       | Some (PDrop { file = _; index }) ->
           let loc = Parsing_syntax.loc_of_impl (List.nth ast.ast index) in
           Diagnostics.remove_diagnostics_inside_loc diagnostics loc;
           {
             ast with
             ast = List.filteri (fun i -> fun _ -> i <> index) ast.ast;
           }
       | _ -> ast)
    : Parsing_parse.output)

type process_result = {
  mbt_files : mbt_input list;
  build_context : Typeutil.build_context;
  pkgs : Pkg.pkg_tbl;
  import_items : Pkg_config_util.import_items;
}

let process_config_and_input ~(pkg_config_file : string option)
    ~(mbt_files : mbt_input list) ~(is_main : bool)
    ~(import_kind : Pkg_config_util.import_kind) ~(imports : mi_input list)
    ~(std_import : std_input option) ~(diagnostics : Diagnostics.t) =
  (let pkg_config =
     match pkg_config_file with
     | Some path ->
         Some
           (Json_parse.parse_json_from_file ~diagnostics
              ~fname:(Filename.basename path) path)
     | None -> None
   in
   Pkg_config_util.parse_warn_alert pkg_config;
   let build_context : Typeutil.build_context =
     match Pkg_config_util.parse_is_main pkg_config with
     | Is_main loc -> Exec { is_main_loc = loc }
     | Not_main ->
         if is_main then Exec { is_main_loc = Loc.no_location } else Lib
   in
   let pkgs = Pkg.create_tbl () in
   let import_items =
     Pkg_config_util.parse_import_item ~import_kind pkg_config
   in
   Basic_lst.iter imports ~f:(function
     | Import_Path_Content (imp_str, mi_content) ->
         let imp = Parsing_import_path.parse imp_str in
         Pkg.load_mi ~import_items pkgs imp mi_content ~diagnostics
     | Import_Path imp_str ->
         let imp = Parsing_import_path.parse imp_str in
         let mi_content =
           Stdlib.In_channel.with_open_bin imp.path Stdlib.In_channel.input_all
         in
         Pkg.load_mi ~import_items pkgs imp mi_content ~diagnostics);
   (match std_import with
   | Some (Std_Content imports) ->
       Basic_lst.iter imports ~f:(fun (imp_str, mi_content) ->
           let imp = Parsing_import_path.parse imp_str in
           Pkg.load_mi ~import_items pkgs imp mi_content ~diagnostics)
   | Some (Std_Path std_path) ->
       if std_path <> "" then Pkg.load_std pkgs ~std_path ~diagnostics
   | None -> ());
   { mbt_files; build_context; pkgs; import_items }
    : process_result)

let check ~diagnostics ?(std_import : std_input option)
    ~(imports : mi_input list) ~(debug_tokens : bool) ~(is_main : bool)
    ~(quiet : bool) ~(genv_callback : Global_env.t -> unit)
    ~(tast_callback : Typedtree.output -> unit) ?(patches : Patch.t list = [])
    ~(pkg_config_file : string option)
    ~(import_kind : Pkg_config_util.import_kind) (mbt_files : mbt_input list) =
  (let { mbt_files; build_context; pkgs; import_items } =
     process_config_and_input ~pkg_config_file ~mbt_files ~is_main ~diagnostics
       ~import_kind ~imports ~std_import
   in
   let patched_mbt_files = apply_patch_add patches mbt_files in
   let outputs =
     Lst.map patched_mbt_files (fun input ->
         let ast = parse ~diagnostics ~debug_tokens input in
         let ast = apply_patch_drop ~diagnostics patches ast in
         postprocess_ast ~diagnostics ast)
   in
   tast_of_ast ~diagnostics ~build_context ~import_kind ~quiet ~genv_callback
     ~tast_callback ~import_items ~pkgs outputs
    : Typedtree.output * Global_env.t)

let build_package ~diagnostics ?(std_import : std_input option)
    ~(imports : mi_input list) ~(debug_tokens : bool) ~(no_opt : bool)
    ~(contification : bool) ~(is_main : bool) ~(quiet : bool)
    ~(tracing_callback : Parsing_parse.output list -> Parsing_parse.output list)
    ~(profile_callback : Parsing_parse.output list -> Parsing_parse.output list)
    ~(debug_source_callback : string -> Parsing_parse.output -> unit)
    ~(genv_callback : Global_env.t -> unit)
    ~(tast_callback : Typedtree.output -> unit) ~core_callback
    ~(pkg_config_file : string option)
    ~(import_kind : Pkg_config_util.import_kind) ?(patches : Patch.t list = [])
    (mbt_files : mbt_input list) =
  (let { mbt_files; build_context; pkgs; import_items } =
     process_config_and_input ~pkg_config_file ~mbt_files ~is_main ~diagnostics
       ~import_kind ~imports ~std_import
   in
   let mbt_files = apply_patch_add patches mbt_files in
   let asts =
     (fun xs ->
       (fun xs ->
         (fun xs -> Lst.map xs (postprocess_ast ~diagnostics))
           (profile_callback
              (tracing_callback
                 (Lst.map xs (apply_patch_drop ~diagnostics patches)))))
         (Lst.map xs (parse ~diagnostics ~debug_tokens)))
       mbt_files
   in
   Basic_lst.iter2 mbt_files asts (fun mbt_file ->
       fun ast ->
        match mbt_file with
        | Name_Content _ -> ()
        | File_Path path -> debug_source_callback path ast);
   (fun (tast, genv) ->
     core_of_tast ~no_opt ~contification ~genv ~tast ~core_callback)
     ( tast_of_ast ~diagnostics ~build_context ~import_kind ~import_items ~pkgs
         ~quiet ~genv_callback ~tast_callback asts
     |-> fun _ -> Diagnostics.check_diagnostics diagnostics )
    : Core.program)

type core_input = Core_Path of string | Core_Content of string

let monofy_core_link ~(link_output : Core_link.output) ~exported_functions =
  (let monofy_env =
     Monofy_env.make ~regular_methods:link_output.methods
       ~extension_methods:link_output.ext_methods
   in
   let mono_core =
     Monofy.monofy ~monofy_env ~stype_defs:link_output.types ~exported_functions
       link_output.linked_program
   in
   let mono_core = Pass_layout.optimize_layout mono_core in
   mono_core
    : Mcore.t)

let clam_of_mcore ~(elim_unused_let : bool) (core : Mcore.t) ~clam_callback =
  (let pass ~cond ~stage f prog =
     if cond then f prog |-> clam_callback stage else prog
   in
   pass ~cond:elim_unused_let ~stage:`Clam_Unused_Let
     Pass_unused_let.unused_let_opt
     (Clam_of_core.transl_prog core |-> clam_callback `Clam_Start)
   |-> clam_callback `Clam_End
    : Clam.prog)

let wasm_gen ~(elim_unused_let : bool) (core : Mcore.t) ~(target : target) =
  match target with
  | Wasm_gc { clam_callback; _ } ->
      Wasm_of_clam_gc.compile
        (clam_of_mcore ~elim_unused_let ~clam_callback core)

let link_core ~(shrink_wasm : bool) ~(elim_unused_let : bool)
    ~(core_inputs : core_input Basic_vec.t)
    ~(exported_functions : string Basic_hash_string.t) ~(target : target) =
  let targets : Core_link.linking_target Basic_vec.t = Basic_vec.empty () in
  Basic_vec.iter core_inputs (function
    | Core_Path path -> Basic_vec.push targets (Core_link.File_path path)
    | Core_Content content ->
        Basic_vec.push targets
          (Core_link.Core_format (Core_format.of_string content)));
  let link_output = Core_link.link ~targets in
  let mono_core =
    monofy_core_link ~link_output
      ~exported_functions:
        (Exported_functions.Export_selected exported_functions)
  in
  match target with
  | Wasm_gc { dwarfsm_callback; _ } ->
      let mod_ = wasm_gen ~elim_unused_let ~target mono_core in
      dwarfsm_callback (if shrink_wasm then Shrink_wasmir.shrink mod_ else mod_)

let gen_test_info ~(diagnostics : Diagnostics.t) ~(json : bool)
    ?(patches : Patch.t list = []) (mbt_files : mbt_input list) =
  (let parse_and_patch mbt_file =
     let ast = parse ~diagnostics ~debug_tokens:false mbt_file in
     let ast = apply_patch_drop ~diagnostics patches ast in
     ast
   in
   let mbt_files = apply_patch_add patches mbt_files in
   let inputs =
     Basic_lst.map mbt_files (fun mbt_file ->
         match mbt_file with
         | File_Path path -> (path, (parse_and_patch mbt_file).ast)
         | Name_Content (name, _) -> (name, (parse_and_patch mbt_file).ast))
   in
   if json then Gen_test_info.gen_test_info_json inputs
   else Gen_test_info.gen_test_info inputs
    : string)
