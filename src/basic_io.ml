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

let write file content =
  let out = open_out_bin file in
  output_string out content;
  close_out_noerr out

let write_s file s = write file (S.to_string s)

let write_ss file (ss : S.t list) =
  match ss with
  | [] -> write file ""
  | s :: ss ->
      let out = open_out_bin file in
      output_string out (S.to_string s);
      Lst.iter ss ~f:(fun s ->
          output_string out "\n";
          output_string out (S.to_string s));
      close_out_noerr out

let write_wexps file (wexps : W.t list) =
  match wexps with
  | [] -> write file ""
  | s :: ss ->
      let out = open_out_bin file in
      output_string out (W.to_string s);
      Lst.iter ss ~f:(fun s ->
          output_string out "\n";
          output_string out (W.to_string s));
      close_out_noerr out

let load_file file = In_channel.with_open_bin file In_channel.input_all
