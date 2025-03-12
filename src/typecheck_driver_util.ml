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


let ( |-> ) obj callback =
  callback obj;
  obj

type mi_input = Import_Path of string | Import_Path_Content of string * string
type std_input = Std_Path of string | Std_Content of (string * string) list

let tast_of_ast ~diagnostics ~(build_context : Typeutil.build_context)
    ~(quiet : bool) ~(genv_callback : Global_env.t -> unit)
    ~(tast_callback : Typedtree.output -> unit)
    ~(import_items : Pkg_config_util.import_items) ~(pkgs : Pkg.pkg_tbl)
    ~(import_kind : Pkg_config_util.import_kind)
    (asts : Parsing_parse.output list) =
  (let asts = Basic_lst.map asts (fun a -> a.ast) in
   let top_output =
     Toplevel_typer.check_toplevel ~pkgs ~build_context asts ~diagnostics
   in
   let genv = top_output.global_env |-> genv_callback in
   let tast = Typer.type_check top_output ~diagnostics in
   if (not quiet) && not (Diagnostics.has_fatal_errors diagnostics) then
     Check_match.analyze ~diagnostics (genv, tast);
   let tast = Topo_sort.topo_sort tast ~diagnostics |-> tast_callback in
   Global_env.report_unused_pkg ~diagnostics genv;
   Dead_code.analyze_unused ~diagnostics ~import_items ~build_context
     ~import_kind tast;
   (tast, genv)
    : Typedtree.output * Global_env.t)
