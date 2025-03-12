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


module Byteseq = Basic_byteseq
module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Ast = Dwarfsm_ast
module Encode_context = Dwarfsm_encode_context
module Encode_resolve = Dwarfsm_encode_resolve
module Encode_wasm = Dwarfsm_encode_wasm

let ( ^^ ) = Byteseq.O.( ^^ )
and ( ^^= ) = Byteseq.O.( ^^= )

let with_length_preceded = Basic_encoders.with_length_preceded
and int_uleb128 = Basic_encoders.int_uleb128
and string = Basic_encoders.string
and int_vlq64 = Basic_encoders.int_vlq64
and json_string = Basic_encoders.json_string

let equal_code_pos (pc1, (pos1 : Ast.source_pos)) (pc2, (pos2 : Ast.source_pos))
    =
  Dwarfsm_basic.absolute_pc_of pc1 = Dwarfsm_basic.absolute_pc_of pc2
  && pos1.pkg = pos2.pkg && pos1.file = pos2.file && pos1.line = pos2.line
  && pos1.col = pos2.col

let module_ ~emit_names m =
  let ctx = Encode_context.make_context () in
  Encode_resolve.resolve ctx m;
  let wasm = Encode_wasm.encode ~emit_names ctx in
  Byteseq.to_string wasm

let module_with_source_map ~file ?source_map_url ?source_loader m =
  let ctx = Encode_context.make_context () in
  Encode_resolve.resolve ctx m;
  let code_pos = Vec.empty () in
  let add_code_pos rel_pc pos = Vec.push code_pos (rel_pc, pos) in
  let wasm =
    Encode_wasm.encode ~add_code_pos
      ~custom_sections:(fun () ->
        [
          ( "sourceMappingURL",
            with_length_preceded ~f:int_uleb128
              (match source_map_url with
              | Some url -> string url
              | None -> string "./" ^^ string file ^^ string ".map") );
        ])
      ~emit_names:true ctx
  in
  let wasm = Byteseq.to_string wasm in
  let codesec_offset =
    let read_int_leb128 data ofs =
      let n = ref 0 in
      let shift = ref 0 in
      let ofs = ref ofs in
      let b = ref (Char.code data.[!ofs]) in
      incr ofs;
      while !b >= 128 do
        n := !n lor ((!b - 128) lsl !shift);
        b := Char.code data.[!ofs];
        incr ofs;
        shift := !shift + 7
      done;
      (!n + (!b lsl !shift), !ofs)
    in
    let rec find_codesec_offset ofs =
      let id, ofs = read_int_leb128 wasm ofs in
      let size, ofs = read_int_leb128 wasm ofs in
      if id = 10 then ofs else find_codesec_offset (ofs + size)
    in
    find_codesec_offset 8
  in
  let source_map_buf = ref Byteseq.empty in
  source_map_buf ^^= string "{\"version\":3";
  let source_index_tbl = Hash_string.create 0 in
  let sources_buf = ref Byteseq.empty in
  let sources_content_buf = ref Byteseq.empty in
  let no_comma_sources = ref true in
  let no_comma_mappings = ref true in
  let mappings_buf = ref Byteseq.empty in
  let last_addr = ref ~-1 in
  let last_src_file = ref ~-1 in
  let last_src_line = ref ~-1 in
  let last_src_column = ref ~-1 in
  let field x last_ref =
    let res = int_vlq64 (if !last_ref = -1 then x else x - !last_ref) in
    last_ref := x;
    res
  in
  let get_source_content pkg file =
    match source_loader with
    | Some f -> f ~pkg file
    | None -> ("moonbit:///@" ^ pkg ^ "/" ^ file, None)
  in
  let last_code_pos = ref None in
  Vec.iter code_pos
    (fun ((rel_pc, { pkg; file; line = line1; col = column }) as code_pos) ->
      match !last_code_pos with
      | Some last_code_pos when equal_code_pos code_pos last_code_pos -> ()
      | _ ->
          last_code_pos := Some code_pos;
          let line = line1 - 1 in
          let file_index =
            let source, content = get_source_content pkg file in
            Hash_string.find_or_update source_index_tbl source ~update:(fun _ ->
                let index = Hash_string.length source_index_tbl in
                if not !no_comma_sources then (
                  sources_buf ^^= string ",";
                  sources_content_buf ^^= string ",");
                no_comma_sources := false;
                sources_buf ^^= json_string source;
                (sources_content_buf
                ^^=
                match content with
                | None -> string "null"
                | Some content -> json_string content);
                index)
          in
          if not !no_comma_mappings then mappings_buf ^^= string ",";
          no_comma_mappings := false;
          let addr = codesec_offset + Dwarfsm_basic.absolute_pc_of rel_pc in
          mappings_buf ^^= field addr last_addr
          ^^ field file_index last_src_file
          ^^ field line last_src_line
          ^^ field column last_src_column);
  source_map_buf ^^= string ",\"sources\":[" ^^ !sources_buf ^^ string "]";
  source_map_buf
  ^^= string ",\"sourcesContent\":["
  ^^ !sources_content_buf ^^ string "]";
  source_map_buf ^^= string ",\"mappings\":\"" ^^ !mappings_buf ^^ string "\"";
  source_map_buf ^^= string "}";
  (wasm, Byteseq.to_string !source_map_buf)
