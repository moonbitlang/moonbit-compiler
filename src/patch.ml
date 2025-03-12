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


type t =
  | PDrop of { file : string; index : int }
  | PAdd of { name : string; content : string }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (function
     | PDrop { file = file__002_; index = index__004_ } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_int index__004_ in
           (S.List [ S.Atom "index"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_string file__002_ in
           (S.List [ S.Atom "file"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "PDrop" :: bnds__001_)
     | PAdd { name = name__007_; content = content__009_ } ->
         let bnds__006_ = ([] : _ Stdlib.List.t) in
         let bnds__006_ =
           let arg__010_ = Moon_sexp_conv.sexp_of_string content__009_ in
           (S.List [ S.Atom "content"; arg__010_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         let bnds__006_ =
           let arg__008_ = Moon_sexp_conv.sexp_of_string name__007_ in
           (S.List [ S.Atom "name"; arg__008_ ] :: bnds__006_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "PAdd" :: bnds__006_)
      : t -> S.t)

  let _ = sexp_of_t
end

let push_pdrop ~(patches : t Basic_vec.t) (json : Json_types.t) =
  (let exception Ret in
   try
     match json with
     | Json_types.Obj { map; loc = _ } ->
         let file =
           match Basic_map_string.find_exn map "file" with
           | Json_types.Str { str; loc = _ } -> str
           | _ -> raise Ret
         in
         let index =
           match Basic_map_string.find_exn map "index" with
           | Json_types.Float { float; loc = _ } -> int_of_string float
           | _ -> raise Ret
         in
         Basic_vec.push patches (PDrop { file; index })
     | _ -> raise Ret
   with Ret -> ()
    : unit)

let push_padd ~(patches : t Basic_vec.t) (json : Json_types.t) =
  (let exception Ret in
   try
     match json with
     | Json_types.Obj { map; loc = _ } ->
         let name =
           match Basic_map_string.find_exn map "name" with
           | Json_types.Str { str; loc = _ } -> str
           | _ -> raise Ret
         in
         let content =
           match Basic_map_string.find_exn map "content" with
           | Json_types.Str { str; loc = _ } -> str
           | _ -> raise Ret
         in
         Basic_vec.push patches (PAdd { name; content })
     | _ -> raise Ret
   with Ret -> ()
    : unit)

let parse_patch_from_json (json : Json_types.t) =
  (let patches = Basic_vec.empty () in
   (match json with
   | Json_types.Obj { map; loc = _ } ->
       let drops =
         match Basic_map_string.find_exn map "drops" with
         | Json_types.Arr { content; loc = _ } -> content
         | _ -> failwith "parse_patch: drops is not an array"
       in
       Array.iter (push_pdrop ~patches) drops;
       let adds =
         match Basic_map_string.find_exn map "patches" with
         | Json_types.Arr { content; loc = _ } -> content
         | _ -> failwith "parse_patch: patches is not an array"
       in
       Array.iter (push_padd ~patches) adds
   | _ -> failwith "parse_patch: json is not a map");
   Basic_vec.to_list patches
    : t list)

let parse_patch_from_string ~(content : string) =
  (let d = Diagnostics.make () in
   let json = Json_parse.parse_json_from_string ~diagnostics:d content in
   parse_patch_from_json json
    : t list)

let parse_patch_from_file ~(file_path : string) =
  (let d = Diagnostics.make () in
   let json = Json_parse.parse_json_from_file ~diagnostics:d file_path in
   parse_patch_from_json json
    : t list)
