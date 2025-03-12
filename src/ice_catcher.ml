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


let ice_message =
  {|
         --  --
       /  //  / __--------_
      /  //  /_/            \
   ---      -                \ __
  / X        /        ____   /   )
  *_________/__/_____/______/ `--

Oops, the compiler has encountered an unexpected situation.
This is a bug in the compiler.

A bug report containing the error description and relevant code would be
greatly appreciated. You can submit the bug report here:
|}

let escape_argv argv =
  let escape_arg arg =
    let escaped = Basic_strutil.escaped_string arg in
    if escaped <> arg then "\"" ^ escaped ^ "\"" else arg
  in
  Array.map escape_arg argv

let default_bug_report_url =
  "https://github.com/moonbitlang/moonbit-docs/issues/new?labels=bug,ICE"

let default_environment_printer () =
  let version = Version.version in
  Printf.eprintf "moonc version: %s\n" version

let current_bug_report_url = ref default_bug_report_url
let current_environment_printer = ref default_environment_printer

let run_with_protection f =
  try f ()
  with exn ->
    Printf.eprintf "%s\n" ice_message;
    Printf.eprintf "  %s\n\n" !current_bug_report_url;
    let error_message =
      match exn with Failure msg -> msg | _ -> Printexc.to_string exn
    in
    Printf.eprintf "Error: %s\n\n" error_message;
    (if Printexc.backtrace_status () then
       let backtrace = Printexc.get_backtrace () in
       Printf.eprintf "Backtrace:\n%s\n" backtrace);
    let argv = Sys.argv in
    let argv = escape_argv argv in
    Printf.eprintf "Compiler args: %s\n\n"
      (String.concat " " (Array.to_list argv));
    !current_environment_printer ();
    exit 199

let install_ice_hook ?bug_report_url ?environment_printer () =
  if bug_report_url <> None then
    current_bug_report_url := Option.get bug_report_url;
  if environment_printer <> None then
    current_environment_printer := Option.get environment_printer
[@@live "We will need this some day"]
