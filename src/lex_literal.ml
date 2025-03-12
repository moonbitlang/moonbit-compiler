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


module Uchar_utils = Basic_uchar_utils

type char_literal = { char_val : Uchar_utils.uchar; char_repr : string }

include struct
  let _ = fun (_ : char_literal) -> ()

  let sexp_of_char_literal =
    (fun { char_val = char_val__002_; char_repr = char_repr__004_ } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_string char_repr__004_ in
         (S.List [ S.Atom "char_repr"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Uchar_utils.sexp_of_uchar char_val__002_ in
         (S.List [ S.Atom "char_val"; arg__003_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : char_literal -> S.t)

  let _ = sexp_of_char_literal
end

type string_literal = { string_val : string; string_repr : string }

include struct
  let _ = fun (_ : string_literal) -> ()

  let sexp_of_string_literal =
    (fun { string_val = string_val__007_; string_repr = string_repr__009_ } ->
       let bnds__006_ = ([] : _ Stdlib.List.t) in
       let bnds__006_ =
         let arg__010_ = Moon_sexp_conv.sexp_of_string string_repr__009_ in
         (S.List [ S.Atom "string_repr"; arg__010_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__008_ = Moon_sexp_conv.sexp_of_string string_val__007_ in
         (S.List [ S.Atom "string_val"; arg__008_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       S.List bnds__006_
      : string_literal -> S.t)

  let _ = sexp_of_string_literal
end

type byte_literal = { byte_val : int; byte_repr : string }

include struct
  let _ = fun (_ : byte_literal) -> ()

  let sexp_of_byte_literal =
    (fun { byte_val = byte_val__012_; byte_repr = byte_repr__014_ } ->
       let bnds__011_ = ([] : _ Stdlib.List.t) in
       let bnds__011_ =
         let arg__015_ = Moon_sexp_conv.sexp_of_string byte_repr__014_ in
         (S.List [ S.Atom "byte_repr"; arg__015_ ] :: bnds__011_
           : _ Stdlib.List.t)
       in
       let bnds__011_ =
         let arg__013_ = Moon_sexp_conv.sexp_of_int byte_val__012_ in
         (S.List [ S.Atom "byte_val"; arg__013_ ] :: bnds__011_
           : _ Stdlib.List.t)
       in
       S.List bnds__011_
      : byte_literal -> S.t)

  let _ = sexp_of_byte_literal
end

type bytes_literal = { bytes_val : string; bytes_repr : string }

include struct
  let _ = fun (_ : bytes_literal) -> ()

  let sexp_of_bytes_literal =
    (fun { bytes_val = bytes_val__017_; bytes_repr = bytes_repr__019_ } ->
       let bnds__016_ = ([] : _ Stdlib.List.t) in
       let bnds__016_ =
         let arg__020_ = Moon_sexp_conv.sexp_of_string bytes_repr__019_ in
         (S.List [ S.Atom "bytes_repr"; arg__020_ ] :: bnds__016_
           : _ Stdlib.List.t)
       in
       let bnds__016_ =
         let arg__018_ = Moon_sexp_conv.sexp_of_string bytes_val__017_ in
         (S.List [ S.Atom "bytes_val"; arg__018_ ] :: bnds__016_
           : _ Stdlib.List.t)
       in
       S.List bnds__016_
      : bytes_literal -> S.t)

  let _ = sexp_of_bytes_literal
end

let hide_loc _ = not !Basic_config.show_loc

type interp_source = { source : string; loc_ : Loc.t [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : interp_source) -> ()

  let sexp_of_interp_source =
    (let (drop_if__025_ : Loc.t -> Stdlib.Bool.t) = hide_loc in
     fun { source = source__022_; loc_ = loc___026_ } ->
       let bnds__021_ = ([] : _ Stdlib.List.t) in
       let bnds__021_ =
         if drop_if__025_ loc___026_ then bnds__021_
         else
           let arg__028_ = Loc.sexp_of_t loc___026_ in
           let bnd__027_ = S.List [ S.Atom "loc_"; arg__028_ ] in
           (bnd__027_ :: bnds__021_ : _ Stdlib.List.t)
       in
       let bnds__021_ =
         let arg__023_ = Moon_sexp_conv.sexp_of_string source__022_ in
         (S.List [ S.Atom "source"; arg__023_ ] :: bnds__021_ : _ Stdlib.List.t)
       in
       S.List bnds__021_
      : interp_source -> S.t)

  let _ = sexp_of_interp_source
end

type interp_elem =
  | Interp_lit of {
      c : string;
      repr : string; [@sexp_drop_if fun _ -> true]
      loc_ : Loc.t; [@sexp_drop_if hide_loc]
    }
  | Interp_source of interp_source

include struct
  let _ = fun (_ : interp_elem) -> ()

  let sexp_of_interp_elem =
    (let (drop_if__033_ : string -> Stdlib.Bool.t) = fun _ -> true
     and (drop_if__038_ : Loc.t -> Stdlib.Bool.t) = hide_loc in
     function
     | Interp_lit { c = c__030_; repr = repr__034_; loc_ = loc___039_ } ->
         let bnds__029_ = ([] : _ Stdlib.List.t) in
         let bnds__029_ =
           if drop_if__038_ loc___039_ then bnds__029_
           else
             let arg__041_ = Loc.sexp_of_t loc___039_ in
             let bnd__040_ = S.List [ S.Atom "loc_"; arg__041_ ] in
             (bnd__040_ :: bnds__029_ : _ Stdlib.List.t)
         in
         let bnds__029_ =
           if drop_if__033_ repr__034_ then bnds__029_
           else
             let arg__036_ = Moon_sexp_conv.sexp_of_string repr__034_ in
             let bnd__035_ = S.List [ S.Atom "repr"; arg__036_ ] in
             (bnd__035_ :: bnds__029_ : _ Stdlib.List.t)
         in
         let bnds__029_ =
           let arg__031_ = Moon_sexp_conv.sexp_of_string c__030_ in
           (S.List [ S.Atom "c"; arg__031_ ] :: bnds__029_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Interp_lit" :: bnds__029_)
     | Interp_source arg0__042_ ->
         let res0__043_ = sexp_of_interp_source arg0__042_ in
         S.List [ S.Atom "Interp_source"; res0__043_ ]
      : interp_elem -> S.t)

  let _ = sexp_of_interp_elem
end

type interp_literal = interp_elem list

include struct
  let _ = fun (_ : interp_literal) -> ()

  let sexp_of_interp_literal =
    (fun x__044_ -> Moon_sexp_conv.sexp_of_list sexp_of_interp_elem x__044_
      : interp_literal -> S.t)

  let _ = sexp_of_interp_literal
end

let interp_content_to_string elems =
  List.fold_left
    (fun acc ->
      fun elem ->
       match elem with
       | Interp_lit { c; _ } -> acc ^ c
       | Interp_source { source; _ } -> acc ^ "\\{" ^ source ^ "}")
    "" elems

let interp_literal_to_string elems =
  "\"" ^ interp_content_to_string elems ^ "\""
