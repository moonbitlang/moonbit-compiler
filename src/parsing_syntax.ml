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


module Literal = Lex_literal

type absolute_loc = Loc.t

include struct
  let _ = fun (_ : absolute_loc) -> ()
  let sexp_of_absolute_loc = (Loc.sexp_of_t : absolute_loc -> S.t)
  let _ = sexp_of_absolute_loc
end

type location = Rloc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Rloc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

let hide_loc _ = not !Basic_config.show_loc

type string_literal = Literal.string_literal

include struct
  let _ = fun (_ : string_literal) -> ()

  let sexp_of_string_literal =
    (Literal.sexp_of_string_literal : string_literal -> S.t)

  let _ = sexp_of_string_literal
end

type char_literal = Literal.char_literal

include struct
  let _ = fun (_ : char_literal) -> ()

  let sexp_of_char_literal =
    (Literal.sexp_of_char_literal : char_literal -> S.t)

  let _ = sexp_of_char_literal
end

type byte_literal = Literal.byte_literal

include struct
  let _ = fun (_ : byte_literal) -> ()

  let sexp_of_byte_literal =
    (Literal.sexp_of_byte_literal : byte_literal -> S.t)

  let _ = sexp_of_byte_literal
end

type bytes_literal = Literal.bytes_literal

include struct
  let _ = fun (_ : bytes_literal) -> ()

  let sexp_of_bytes_literal =
    (Literal.sexp_of_bytes_literal : bytes_literal -> S.t)

  let _ = sexp_of_bytes_literal
end

type constant =
  | Const_bool of bool
  | Const_byte of byte_literal
  | Const_bytes of bytes_literal
  | Const_char of char_literal
  | Const_int of string
  | Const_int64 of string
  | Const_uint of string
  | Const_uint64 of string
  | Const_double of string
  | Const_string of string_literal
  | Const_bigint of string

include struct
  let _ = fun (_ : constant) -> ()

  let sexp_of_constant =
    (function
     | Const_bool arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_bool arg0__001_ in
         S.List [ S.Atom "Const_bool"; res0__002_ ]
     | Const_byte arg0__003_ ->
         let res0__004_ = sexp_of_byte_literal arg0__003_ in
         S.List [ S.Atom "Const_byte"; res0__004_ ]
     | Const_bytes arg0__005_ ->
         let res0__006_ = sexp_of_bytes_literal arg0__005_ in
         S.List [ S.Atom "Const_bytes"; res0__006_ ]
     | Const_char arg0__007_ ->
         let res0__008_ = sexp_of_char_literal arg0__007_ in
         S.List [ S.Atom "Const_char"; res0__008_ ]
     | Const_int arg0__009_ ->
         let res0__010_ = Moon_sexp_conv.sexp_of_string arg0__009_ in
         S.List [ S.Atom "Const_int"; res0__010_ ]
     | Const_int64 arg0__011_ ->
         let res0__012_ = Moon_sexp_conv.sexp_of_string arg0__011_ in
         S.List [ S.Atom "Const_int64"; res0__012_ ]
     | Const_uint arg0__013_ ->
         let res0__014_ = Moon_sexp_conv.sexp_of_string arg0__013_ in
         S.List [ S.Atom "Const_uint"; res0__014_ ]
     | Const_uint64 arg0__015_ ->
         let res0__016_ = Moon_sexp_conv.sexp_of_string arg0__015_ in
         S.List [ S.Atom "Const_uint64"; res0__016_ ]
     | Const_double arg0__017_ ->
         let res0__018_ = Moon_sexp_conv.sexp_of_string arg0__017_ in
         S.List [ S.Atom "Const_double"; res0__018_ ]
     | Const_string arg0__019_ ->
         let res0__020_ = sexp_of_string_literal arg0__019_ in
         S.List [ S.Atom "Const_string"; res0__020_ ]
     | Const_bigint arg0__021_ ->
         let res0__022_ = Moon_sexp_conv.sexp_of_string arg0__021_ in
         S.List [ S.Atom "Const_bigint"; res0__022_ ]
      : constant -> S.t)

  let _ = sexp_of_constant
end

type longident = Basic_longident.t

include struct
  let _ = fun (_ : longident) -> ()
  let sexp_of_longident = (Basic_longident.sexp_of_t : longident -> S.t)
  let _ = sexp_of_longident
end

type docstring = Docstring.t

include struct
  let _ = fun (_ : docstring) -> ()
  let sexp_of_docstring = (Docstring.sexp_of_t : docstring -> S.t)
  let _ = sexp_of_docstring
end

type attribute = Attribute.t

include struct
  let _ = fun (_ : attribute) -> ()
  let sexp_of_attribute = (Attribute.sexp_of_t : attribute -> S.t)
  let _ = sexp_of_attribute
end

type constrid_loc = {
  lid : Basic_longident.t;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : constrid_loc) -> ()

  let sexp_of_constrid_loc =
    (let (drop_if__027_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { lid = lid__024_; loc_ = loc___028_ } ->
       let bnds__023_ = ([] : _ Stdlib.List.t) in
       let bnds__023_ =
         if drop_if__027_ loc___028_ then bnds__023_
         else
           let arg__030_ = sexp_of_location loc___028_ in
           let bnd__029_ = S.List [ S.Atom "loc_"; arg__030_ ] in
           (bnd__029_ :: bnds__023_ : _ Stdlib.List.t)
       in
       let bnds__023_ =
         let arg__025_ = Basic_longident.sexp_of_t lid__024_ in
         (S.List [ S.Atom "lid"; arg__025_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       S.List bnds__023_
      : constrid_loc -> S.t)

  let _ = sexp_of_constrid_loc
end

type label = { label_name : string; loc_ : location [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : label) -> ()

  let sexp_of_label =
    (let (drop_if__035_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { label_name = label_name__032_; loc_ = loc___036_ } ->
       let bnds__031_ = ([] : _ Stdlib.List.t) in
       let bnds__031_ =
         if drop_if__035_ loc___036_ then bnds__031_
         else
           let arg__038_ = sexp_of_location loc___036_ in
           let bnd__037_ = S.List [ S.Atom "loc_"; arg__038_ ] in
           (bnd__037_ :: bnds__031_ : _ Stdlib.List.t)
       in
       let bnds__031_ =
         let arg__033_ = Moon_sexp_conv.sexp_of_string label_name__032_ in
         (S.List [ S.Atom "label_name"; arg__033_ ] :: bnds__031_
           : _ Stdlib.List.t)
       in
       S.List bnds__031_
      : label -> S.t)

  let _ = sexp_of_label
end

type accessor =
  | Label of label
  | Index of { tuple_index : int; loc_ : location [@sexp_drop_if hide_loc] }
  | Newtype

include struct
  let _ = fun (_ : accessor) -> ()

  let sexp_of_accessor =
    (let (drop_if__045_ : location -> Stdlib.Bool.t) = hide_loc in
     function
     | Label arg0__039_ ->
         let res0__040_ = sexp_of_label arg0__039_ in
         S.List [ S.Atom "Label"; res0__040_ ]
     | Index { tuple_index = tuple_index__042_; loc_ = loc___046_ } ->
         let bnds__041_ = ([] : _ Stdlib.List.t) in
         let bnds__041_ =
           if drop_if__045_ loc___046_ then bnds__041_
           else
             let arg__048_ = sexp_of_location loc___046_ in
             let bnd__047_ = S.List [ S.Atom "loc_"; arg__048_ ] in
             (bnd__047_ :: bnds__041_ : _ Stdlib.List.t)
         in
         let bnds__041_ =
           let arg__043_ = Moon_sexp_conv.sexp_of_int tuple_index__042_ in
           (S.List [ S.Atom "tuple_index"; arg__043_ ] :: bnds__041_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Index" :: bnds__041_)
     | Newtype -> S.Atom "Newtype"
      : accessor -> S.t)

  let _ = sexp_of_accessor
end

type constr_name = { name : string; loc_ : location [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : constr_name) -> ()

  let sexp_of_constr_name =
    (let (drop_if__053_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { name = name__050_; loc_ = loc___054_ } ->
       let bnds__049_ = ([] : _ Stdlib.List.t) in
       let bnds__049_ =
         if drop_if__053_ loc___054_ then bnds__049_
         else
           let arg__056_ = sexp_of_location loc___054_ in
           let bnd__055_ = S.List [ S.Atom "loc_"; arg__056_ ] in
           (bnd__055_ :: bnds__049_ : _ Stdlib.List.t)
       in
       let bnds__049_ =
         let arg__051_ = Moon_sexp_conv.sexp_of_string name__050_ in
         (S.List [ S.Atom "name"; arg__051_ ] :: bnds__049_ : _ Stdlib.List.t)
       in
       S.List bnds__049_
      : constr_name -> S.t)

  let _ = sexp_of_constr_name
end

type type_name = {
  name : longident;
  is_object : bool; [@sexp_drop_if fun x -> not x]
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : type_name) -> ()

  let sexp_of_type_name =
    (let (drop_if__061_ : bool -> Stdlib.Bool.t) = fun x -> not x
     and (drop_if__066_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { name = name__058_; is_object = is_object__062_; loc_ = loc___067_ }
       ->
       let bnds__057_ = ([] : _ Stdlib.List.t) in
       let bnds__057_ =
         if drop_if__066_ loc___067_ then bnds__057_
         else
           let arg__069_ = sexp_of_location loc___067_ in
           let bnd__068_ = S.List [ S.Atom "loc_"; arg__069_ ] in
           (bnd__068_ :: bnds__057_ : _ Stdlib.List.t)
       in
       let bnds__057_ =
         if drop_if__061_ is_object__062_ then bnds__057_
         else
           let arg__064_ = Moon_sexp_conv.sexp_of_bool is_object__062_ in
           let bnd__063_ = S.List [ S.Atom "is_object"; arg__064_ ] in
           (bnd__063_ :: bnds__057_ : _ Stdlib.List.t)
       in
       let bnds__057_ =
         let arg__059_ = sexp_of_longident name__058_ in
         (S.List [ S.Atom "name"; arg__059_ ] :: bnds__057_ : _ Stdlib.List.t)
       in
       S.List bnds__057_
      : type_name -> S.t)

  let _ = sexp_of_type_name
end

type constructor_extra_info =
  | Type_name of type_name
  | Package of string
  | No_extra_info

include struct
  let _ = fun (_ : constructor_extra_info) -> ()

  let sexp_of_constructor_extra_info =
    (function
     | Type_name arg0__070_ ->
         let res0__071_ = sexp_of_type_name arg0__070_ in
         S.List [ S.Atom "Type_name"; res0__071_ ]
     | Package arg0__072_ ->
         let res0__073_ = Moon_sexp_conv.sexp_of_string arg0__072_ in
         S.List [ S.Atom "Package"; res0__073_ ]
     | No_extra_info -> S.Atom "No_extra_info"
      : constructor_extra_info -> S.t)

  let _ = sexp_of_constructor_extra_info
end

type constructor = {
  constr_name : constr_name;
  extra_info : constructor_extra_info;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : constructor) -> ()

  let sexp_of_constructor =
    (let (drop_if__080_ : location -> Stdlib.Bool.t) = hide_loc in
     fun {
           constr_name = constr_name__075_;
           extra_info = extra_info__077_;
           loc_ = loc___081_;
         } ->
       let bnds__074_ = ([] : _ Stdlib.List.t) in
       let bnds__074_ =
         if drop_if__080_ loc___081_ then bnds__074_
         else
           let arg__083_ = sexp_of_location loc___081_ in
           let bnd__082_ = S.List [ S.Atom "loc_"; arg__083_ ] in
           (bnd__082_ :: bnds__074_ : _ Stdlib.List.t)
       in
       let bnds__074_ =
         let arg__078_ = sexp_of_constructor_extra_info extra_info__077_ in
         (S.List [ S.Atom "extra_info"; arg__078_ ] :: bnds__074_
           : _ Stdlib.List.t)
       in
       let bnds__074_ =
         let arg__076_ = sexp_of_constr_name constr_name__075_ in
         (S.List [ S.Atom "constr_name"; arg__076_ ] :: bnds__074_
           : _ Stdlib.List.t)
       in
       S.List bnds__074_
      : constructor -> S.t)

  let _ = sexp_of_constructor
end

type binder = { binder_name : string; loc_ : location [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : binder) -> ()

  let sexp_of_binder =
    (let (drop_if__088_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { binder_name = binder_name__085_; loc_ = loc___089_ } ->
       let bnds__084_ = ([] : _ Stdlib.List.t) in
       let bnds__084_ =
         if drop_if__088_ loc___089_ then bnds__084_
         else
           let arg__091_ = sexp_of_location loc___089_ in
           let bnd__090_ = S.List [ S.Atom "loc_"; arg__091_ ] in
           (bnd__090_ :: bnds__084_ : _ Stdlib.List.t)
       in
       let bnds__084_ =
         let arg__086_ = Moon_sexp_conv.sexp_of_string binder_name__085_ in
         (S.List [ S.Atom "binder_name"; arg__086_ ] :: bnds__084_
           : _ Stdlib.List.t)
       in
       S.List bnds__084_
      : binder -> S.t)

  let _ = sexp_of_binder
end

type tvar_constraint = {
  tvc_trait : longident;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : tvar_constraint) -> ()

  let sexp_of_tvar_constraint =
    (let (drop_if__096_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { tvc_trait = tvc_trait__093_; loc_ = loc___097_ } ->
       let bnds__092_ = ([] : _ Stdlib.List.t) in
       let bnds__092_ =
         if drop_if__096_ loc___097_ then bnds__092_
         else
           let arg__099_ = sexp_of_location loc___097_ in
           let bnd__098_ = S.List [ S.Atom "loc_"; arg__099_ ] in
           (bnd__098_ :: bnds__092_ : _ Stdlib.List.t)
       in
       let bnds__092_ =
         let arg__094_ = sexp_of_longident tvc_trait__093_ in
         (S.List [ S.Atom "tvc_trait"; arg__094_ ] :: bnds__092_
           : _ Stdlib.List.t)
       in
       S.List bnds__092_
      : tvar_constraint -> S.t)

  let _ = sexp_of_tvar_constraint
end

type type_decl_binder = {
  tvar_name : string option;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : type_decl_binder) -> ()

  let sexp_of_type_decl_binder =
    (let (drop_if__104_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { tvar_name = tvar_name__101_; loc_ = loc___105_ } ->
       let bnds__100_ = ([] : _ Stdlib.List.t) in
       let bnds__100_ =
         if drop_if__104_ loc___105_ then bnds__100_
         else
           let arg__107_ = sexp_of_location loc___105_ in
           let bnd__106_ = S.List [ S.Atom "loc_"; arg__107_ ] in
           (bnd__106_ :: bnds__100_ : _ Stdlib.List.t)
       in
       let bnds__100_ =
         let arg__102_ =
           Moon_sexp_conv.sexp_of_option Moon_sexp_conv.sexp_of_string
             tvar_name__101_
         in
         (S.List [ S.Atom "tvar_name"; arg__102_ ] :: bnds__100_
           : _ Stdlib.List.t)
       in
       S.List bnds__100_
      : type_decl_binder -> S.t)

  let _ = sexp_of_type_decl_binder
end

type tvar_binder = {
  tvar_name : string;
  tvar_constraints : tvar_constraint list; [@list]
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : tvar_binder) -> ()

  let sexp_of_tvar_binder =
    (let (drop_if__116_ : location -> Stdlib.Bool.t) = hide_loc in
     fun {
           tvar_name = tvar_name__109_;
           tvar_constraints = tvar_constraints__112_;
           loc_ = loc___117_;
         } ->
       let bnds__108_ = ([] : _ Stdlib.List.t) in
       let bnds__108_ =
         if drop_if__116_ loc___117_ then bnds__108_
         else
           let arg__119_ = sexp_of_location loc___117_ in
           let bnd__118_ = S.List [ S.Atom "loc_"; arg__119_ ] in
           (bnd__118_ :: bnds__108_ : _ Stdlib.List.t)
       in
       let bnds__108_ =
         if match tvar_constraints__112_ with [] -> true | _ -> false then
           bnds__108_
         else
           let arg__114_ =
             (Moon_sexp_conv.sexp_of_list sexp_of_tvar_constraint)
               tvar_constraints__112_
           in
           let bnd__113_ = S.List [ S.Atom "tvar_constraints"; arg__114_ ] in
           (bnd__113_ :: bnds__108_ : _ Stdlib.List.t)
       in
       let bnds__108_ =
         let arg__110_ = Moon_sexp_conv.sexp_of_string tvar_name__109_ in
         (S.List [ S.Atom "tvar_name"; arg__110_ ] :: bnds__108_
           : _ Stdlib.List.t)
       in
       S.List bnds__108_
      : tvar_binder -> S.t)

  let _ = sexp_of_tvar_binder
end

type test_name = string_literal Rloc.loced option

include struct
  let _ = fun (_ : test_name) -> ()

  let sexp_of_test_name =
    (fun x__120_ ->
       Moon_sexp_conv.sexp_of_option
         (Rloc.sexp_of_loced sexp_of_string_literal)
         x__120_
      : test_name -> S.t)

  let _ = sexp_of_test_name
end

type hole = Synthesized | Incomplete | Todo

include struct
  let _ = fun (_ : hole) -> ()

  let sexp_of_hole =
    (function
     | Synthesized -> S.Atom "Synthesized"
     | Incomplete -> S.Atom "Incomplete"
     | Todo -> S.Atom "Todo"
      : hole -> S.t)

  let _ = sexp_of_hole
end

let sexp_of_tvar_binder tvb =
  if tvb.tvar_constraints = [] then Moon_sexp_conv.sexp_of_string tvb.tvar_name
  else sexp_of_tvar_binder tvb

let sexp_of_type_decl_binder (tvb : type_decl_binder) =
  match tvb.tvar_name with
  | Some name -> Moon_sexp_conv.sexp_of_string name
  | None -> S.Atom "_"

type var = { var_name : longident; loc_ : location [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : var) -> ()

  let sexp_of_var =
    (let (drop_if__125_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { var_name = var_name__122_; loc_ = loc___126_ } ->
       let bnds__121_ = ([] : _ Stdlib.List.t) in
       let bnds__121_ =
         if drop_if__125_ loc___126_ then bnds__121_
         else
           let arg__128_ = sexp_of_location loc___126_ in
           let bnd__127_ = S.List [ S.Atom "loc_"; arg__128_ ] in
           (bnd__127_ :: bnds__121_ : _ Stdlib.List.t)
       in
       let bnds__121_ =
         let arg__123_ = sexp_of_longident var_name__122_ in
         (S.List [ S.Atom "var_name"; arg__123_ ] :: bnds__121_
           : _ Stdlib.List.t)
       in
       S.List bnds__121_
      : var -> S.t)

  let _ = sexp_of_var
end

let sexp_of_constant (c : constant) =
  match c with
  | Const_bool b -> Moon_sexp_conv.sexp_of_bool b
  | Const_byte b -> S.Atom b.byte_repr
  | Const_bytes lit -> S.Atom lit.bytes_repr
  | Const_char c -> Basic_uchar_utils.sexp_of_uchar c.char_val
  | Const_double f -> S.Atom f
  | Const_int s | Const_int64 s | Const_uint s | Const_uint64 s -> S.Atom s
  | Const_string s -> Moon_sexp_conv.sexp_of_string s.string_val
  | Const_bigint s -> Moon_sexp_conv.sexp_of_string s

type argument_kind =
  | Positional
  | Labelled of label
  | Labelled_pun of label
  | Labelled_option of { label : label; question_loc : location }
  | Labelled_option_pun of { label : label; question_loc : location }

include struct
  let _ = fun (_ : argument_kind) -> ()

  let sexp_of_argument_kind =
    (function
     | Positional -> S.Atom "Positional"
     | Labelled arg0__129_ ->
         let res0__130_ = sexp_of_label arg0__129_ in
         S.List [ S.Atom "Labelled"; res0__130_ ]
     | Labelled_pun arg0__131_ ->
         let res0__132_ = sexp_of_label arg0__131_ in
         S.List [ S.Atom "Labelled_pun"; res0__132_ ]
     | Labelled_option
         { label = label__134_; question_loc = question_loc__136_ } ->
         let bnds__133_ = ([] : _ Stdlib.List.t) in
         let bnds__133_ =
           let arg__137_ = sexp_of_location question_loc__136_ in
           (S.List [ S.Atom "question_loc"; arg__137_ ] :: bnds__133_
             : _ Stdlib.List.t)
         in
         let bnds__133_ =
           let arg__135_ = sexp_of_label label__134_ in
           (S.List [ S.Atom "label"; arg__135_ ] :: bnds__133_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Labelled_option" :: bnds__133_)
     | Labelled_option_pun
         { label = label__139_; question_loc = question_loc__141_ } ->
         let bnds__138_ = ([] : _ Stdlib.List.t) in
         let bnds__138_ =
           let arg__142_ = sexp_of_location question_loc__141_ in
           (S.List [ S.Atom "question_loc"; arg__142_ ] :: bnds__138_
             : _ Stdlib.List.t)
         in
         let bnds__138_ =
           let arg__140_ = sexp_of_label label__139_ in
           (S.List [ S.Atom "label"; arg__140_ ] :: bnds__138_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Labelled_option_pun" :: bnds__138_)
      : argument_kind -> S.t)

  let _ = sexp_of_argument_kind
end

type fn_kind = Lambda | Matrix

include struct
  let _ = fun (_ : fn_kind) -> ()

  let sexp_of_fn_kind =
    (function Lambda -> S.Atom "Lambda" | Matrix -> S.Atom "Matrix"
      : fn_kind -> S.t)

  let _ = sexp_of_fn_kind
end

type group = Group_brace | Group_paren

include struct
  let _ = fun (_ : group) -> ()

  let sexp_of_group =
    (function
     | Group_brace -> S.Atom "Group_brace"
     | Group_paren -> S.Atom "Group_paren"
      : group -> S.t)

  let _ = sexp_of_group
end

type trailing_mark = Trailing_comma | Trailing_semi | Trailing_none

include struct
  let _ = fun (_ : trailing_mark) -> ()

  let sexp_of_trailing_mark =
    (function
     | Trailing_comma -> S.Atom "Trailing_comma"
     | Trailing_semi -> S.Atom "Trailing_semi"
     | Trailing_none -> S.Atom "Trailing_none"
      : trailing_mark -> S.t)

  let _ = sexp_of_trailing_mark
end

type apply_attr = No_attr | Exclamation | Question | Double_exclamation

include struct
  let _ = fun (_ : apply_attr) -> ()

  let sexp_of_apply_attr =
    (function
     | No_attr -> S.Atom "No_attr"
     | Exclamation -> S.Atom "Exclamation"
     | Question -> S.Atom "Question"
     | Double_exclamation -> S.Atom "Double_exclamation"
      : apply_attr -> S.t)

  let _ = sexp_of_apply_attr
end

include struct
  class ['a] iterbase =
    object
      method visit_longident : 'a -> longident -> unit = fun _ -> fun _ -> ()
      method visit_label : 'a -> label -> unit = fun _ -> fun _ -> ()
      method visit_accessor : 'a -> accessor -> unit = fun _ -> fun _ -> ()

      method visit_constructor : 'a -> constructor -> unit =
        fun _ -> fun _ -> ()

      method visit_constr_name : 'a -> constr_name -> unit =
        fun _ -> fun _ -> ()

      method visit_var : 'a -> var -> unit = fun _ -> fun _ -> ()
      method visit_binder : 'a -> binder -> unit = fun _ -> fun _ -> ()

      method visit_tvar_constraint : 'a -> tvar_constraint -> unit =
        fun _ -> fun _ -> ()

      method visit_tvar_binder : 'a -> tvar_binder -> unit =
        fun _ -> fun _ -> ()

      method visit_type_decl_binder : 'a -> type_decl_binder -> unit =
        fun _ -> fun _ -> ()

      method visit_constrid_loc : 'a -> constrid_loc -> unit =
        fun _ -> fun _ -> ()

      method visit_argument_kind : 'a -> argument_kind -> unit =
        fun _ -> fun _ -> ()

      method visit_fn_kind : 'a -> fn_kind -> unit = fun _ -> fun _ -> ()
      method visit_type_name : 'a -> type_name -> unit = fun _ -> fun _ -> ()

      method private visit_string_literal : 'a -> string_literal -> unit =
        fun _ -> fun _ -> ()

      method private visit_docstring : 'a -> docstring -> unit =
        fun _ -> fun _ -> ()

      method visit_attribute : 'a -> attribute -> unit = fun _ -> fun _ -> ()

      method private visit_interp_source : 'a -> Literal.interp_source -> unit =
        fun _ -> fun _ -> ()

      method private visit_hole : 'a -> hole -> unit = fun _ -> fun _ -> ()
    end

  class ['a] mapbase =
    object
      method visit_longident : 'a -> longident -> longident =
        fun _ -> fun e -> e

      method visit_label : 'a -> label -> label = fun _ -> fun e -> e
      method visit_accessor : 'a -> accessor -> accessor = fun _ -> fun e -> e

      method visit_constructor : 'a -> constructor -> constructor =
        fun _ -> fun e -> e

      method visit_constr_name : 'a -> constr_name -> constr_name =
        fun _ -> fun e -> e

      method visit_var : 'a -> var -> var = fun _ -> fun e -> e
      method visit_binder : 'a -> binder -> binder = fun _ -> fun e -> e

      method visit_tvar_constraint : 'a -> tvar_constraint -> tvar_constraint =
        fun _ -> fun e -> e

      method visit_type_decl_binder : 'a -> type_decl_binder -> type_decl_binder
          =
        fun _ -> fun e -> e

      method visit_tvar_binder : 'a -> tvar_binder -> tvar_binder =
        fun _ -> fun e -> e

      method visit_constrid_loc : 'a -> constrid_loc -> constrid_loc =
        fun _ -> fun e -> e

      method visit_argument_kind : 'a -> argument_kind -> argument_kind =
        fun _ -> fun e -> e

      method visit_fn_kind : 'a -> fn_kind -> fn_kind = fun _ -> fun e -> e

      method visit_type_name : 'a -> type_name -> type_name =
        fun _ -> fun e -> e

      method private visit_string_literal :
          'a -> string_literal -> string_literal =
        fun _ -> fun e -> e

      method private visit_docstring : 'a -> docstring -> docstring =
        fun _ -> fun e -> e

      method visit_attribute : 'a -> attribute -> attribute =
        fun _ -> fun e -> e

      method private visit_interp_source :
          'a -> Literal.interp_source -> Literal.interp_source =
        fun _ -> fun e -> e

      method private visit_hole : 'a -> hole -> hole = fun _ -> fun e -> e
    end

  class ['a] sexpbase =
    object
      inherit [_] Sexp_visitors.sexp

      method visit_location : 'a -> location -> S.t =
        fun _ -> fun x -> sexp_of_location x

      method visit_absolute_loc : 'a -> absolute_loc -> S.t =
        fun _ -> fun x -> sexp_of_absolute_loc x

      method visit_constant : 'a -> constant -> S.t =
        fun _ -> fun x -> sexp_of_constant x

      method visit_longident : 'a -> longident -> S.t =
        fun _ -> fun x -> sexp_of_longident x

      method visit_label : 'a -> label -> S.t =
        fun _ -> fun x -> sexp_of_label x

      method visit_accessor : 'a -> accessor -> S.t =
        fun _ -> fun x -> sexp_of_accessor x

      method visit_constructor : 'a -> constructor -> S.t =
        fun _ -> fun x -> sexp_of_constructor x

      method visit_constr_name : 'a -> constr_name -> S.t =
        fun _ -> fun x -> sexp_of_constr_name x

      method visit_var : 'a -> var -> S.t = fun _ -> fun x -> sexp_of_var x

      method visit_binder : 'a -> binder -> S.t =
        fun _ -> fun x -> sexp_of_binder x

      method visit_tvar_constraint : 'a -> tvar_constraint -> S.t =
        fun _ -> fun x -> sexp_of_tvar_constraint x

      method visit_tvar_binder : 'a -> tvar_binder -> S.t =
        fun _ -> fun x -> sexp_of_tvar_binder x

      method visit_type_decl_binder : 'a -> type_decl_binder -> S.t =
        fun _ -> fun x -> sexp_of_type_decl_binder x

      method visit_constrid_loc : 'a -> constrid_loc -> S.t =
        fun _ -> fun x -> sexp_of_constrid_loc x

      method visit_argument_kind : 'a -> argument_kind -> S.t =
        fun _ -> fun x -> sexp_of_argument_kind x

      method visit_fn_kind : 'a -> fn_kind -> S.t =
        fun _ -> fun x -> sexp_of_fn_kind x

      method visit_type_name : 'a -> type_name -> S.t =
        fun _ -> fun x -> sexp_of_type_name x

      method visit_docstring : 'a -> docstring -> S.t =
        fun _ -> fun x -> sexp_of_docstring x

      method visit_attribute : 'a -> attribute -> S.t =
        fun _ -> fun x -> sexp_of_attribute x

      method visit_string_literal : 'a -> string_literal -> S.t =
        fun _ -> fun x -> sexp_of_string_literal x

      method visit_apply_attr : 'a -> apply_attr -> S.t =
        fun _ -> fun x -> sexp_of_apply_attr x

      method private visit_test_name : 'a -> test_name -> S.t =
        fun _ -> fun x -> sexp_of_test_name x

      method private visit_group : 'a -> group -> S.t =
        fun _ -> fun x -> sexp_of_group x

      method private visit_trailing_mark : 'a -> trailing_mark -> S.t =
        fun _ -> fun x -> sexp_of_trailing_mark x

      method private visit_interp_source : 'a -> Literal.interp_source -> S.t =
        fun _ -> fun x -> Literal.sexp_of_interp_source x

      method private visit_hole : 'a -> hole -> S.t =
        fun _ -> fun x -> sexp_of_hole x
    end

  type expr =
    | Pexpr_apply of {
        func : expr;
        args : argument list;
        attr : apply_attr;
        loc_ : location;
      }
    | Pexpr_infix of { op : var; lhs : expr; rhs : expr; loc_ : location }
    | Pexpr_unary of { op : var; expr : expr; loc_ : location }
    | Pexpr_array of { exprs : expr list; loc_ : location }
    | Pexpr_array_spread of { elems : spreadable_elem list; loc_ : location }
    | Pexpr_array_get of { array : expr; index : expr; loc_ : location }
    | Pexpr_array_get_slice of {
        array : expr;
        start_index : expr option;
        end_index : expr option;
        index_loc_ : location;
        loc_ : location;
      }
    | Pexpr_array_set of {
        array : expr;
        index : expr;
        value : expr;
        loc_ : location;
      }
    | Pexpr_array_augmented_set of {
        op : var;
        array : expr;
        index : expr;
        value : expr;
        loc_ : location;
      }
    | Pexpr_constant of { c : constant; loc_ : location }
    | Pexpr_multiline_string of {
        elems : multiline_string_elem list;
        loc_ : location;
      }
    | Pexpr_interp of { elems : interp_elem list; loc_ : location }
    | Pexpr_constraint of { expr : expr; ty : typ; loc_ : location }
    | Pexpr_constr of { constr : constructor; loc_ : location }
    | Pexpr_while of {
        loop_cond : expr;
        loop_body : expr;
        while_else : expr option;
        label : label option;
        loc_ : location;
      }
    | Pexpr_function of { func : func; loc_ : location }
    | Pexpr_ident of { id : var; loc_ : location }
    | Pexpr_if of {
        cond : expr;
        ifso : expr;
        ifnot : expr option;
        loc_ : location;
      }
    | Pexpr_guard of {
        cond : expr;
        otherwise : expr option;
        body : expr;
        loc_ : location;
      }
    | Pexpr_guard_let of {
        pat : pattern;
        expr : expr;
        otherwise : case list option;
        body : expr;
        loc_ : location;
      }
    | Pexpr_is of { expr : expr; pat : pattern; loc_ : location }
    | Pexpr_letfn of {
        name : binder;
        func : func;
        body : expr;
        loc_ : location;
      }
    | Pexpr_letrec of {
        bindings : (binder * func) list;
        body : expr;
        loc_ : location;
      }
    | Pexpr_let of {
        pattern : pattern;
        expr : expr;
        body : expr;
        loc_ : location;
      }
    | Pexpr_sequence of { exprs : expr list; last_expr : expr; loc_ : location }
    | Pexpr_tuple of { exprs : expr list; loc_ : location }
    | Pexpr_record of {
        type_name : type_name option;
        fields : field_def list;
        trailing : trailing_mark;
        loc_ : location;
      }
    | Pexpr_record_update of {
        type_name : type_name option;
        record : expr;
        fields : field_def list;
        loc_ : location;
      }
    | Pexpr_field of { record : expr; accessor : accessor; loc_ : location }
    | Pexpr_method of {
        type_name : type_name;
        method_name : label;
        loc_ : location;
      }
    | Pexpr_dot_apply of {
        self : expr;
        method_name : label;
        args : argument list;
        return_self : bool;
        attr : apply_attr;
        loc_ : location;
      }
    | Pexpr_as of { expr : expr; trait : type_name; loc_ : location }
    | Pexpr_mutate of {
        record : expr;
        accessor : accessor;
        field : expr;
        augmented_by : var option;
        loc_ : location;
      }
    | Pexpr_match of {
        expr : expr;
        cases : case list;
        match_loc_ : location;
        loc_ : location;
      }
    | Pexpr_letmut of {
        binder : binder;
        ty : typ option;
        expr : expr;
        body : expr;
        loc_ : location;
      }
    | Pexpr_pipe of { lhs : expr; rhs : expr; loc_ : location }
    | Pexpr_assign of {
        var : var;
        expr : expr;
        augmented_by : var option;
        loc_ : location;
      }
    | Pexpr_hole of { loc_ : location; kind : hole }
    | Pexpr_return of { return_value : expr option; loc_ : location }
    | Pexpr_raise of { err_value : expr; loc_ : location }
    | Pexpr_unit of { loc_ : location; faked : bool }
    | Pexpr_break of {
        arg : expr option;
        label : label option;
        loc_ : location;
      }
    | Pexpr_continue of {
        args : expr list;
        label : label option;
        loc_ : location;
      }
    | Pexpr_loop of {
        args : expr list;
        body : multi_arg_case list;
        label : label option;
        loop_loc_ : location;
        loc_ : location;
      }
    | Pexpr_for of {
        binders : (binder * expr) list;
        condition : expr option;
        continue_block : (binder * expr) list;
        body : expr;
        for_else : expr option;
        label : label option;
        loc_ : location;
      }
    | Pexpr_foreach of {
        binders : binder option list;
        expr : expr;
        body : expr;
        else_block : expr option;
        label : label option;
        loc_ : location;
      }
    | Pexpr_try of {
        body : expr;
        catch : case list;
        catch_all : bool;
        try_else : case list option;
        try_loc_ : location;
        catch_loc_ : location;
        else_loc_ : location;
        loc_ : location;
      }
    | Pexpr_map of { elems : map_expr_elem list; loc_ : location }
    | Pexpr_group of { expr : expr; group : group; loc_ : location }
    | Pexpr_static_assert of { asserts : static_assertion list; body : expr }

  and static_assertion = {
    assert_type : typ;
    assert_trait : longident;
    assert_loc : location;
    assert_msg : string;
  }

  and argument = { arg_value : expr; arg_kind : argument_kind }
  and parameters = parameter list

  and parameter =
    | Discard_positional of { ty : typ option; loc_ : location }
    | Positional of { binder : binder; ty : typ option }
    | Labelled of { binder : binder; ty : typ option }
    | Optional of { binder : binder; default : expr; ty : typ option }
    | Question_optional of { binder : binder; ty : typ option }

  and func =
    | Lambda of {
        parameters : parameters;
        params_loc_ : location;
        body : expr;
        return_type : (typ * error_typ) option;
        kind_ : fn_kind;
        has_error : location option;
        is_async : bool;
      }
    | Match of {
        cases : multi_arg_case list;
        has_error : location option;
        is_async : bool;
        fn_loc_ : location;
        loc_ : location;
      }

  and case = { pattern : pattern; guard : expr option; body : expr }

  and multi_arg_case = {
    patterns : pattern list;
    guard : expr option;
    body : expr;
  }

  and spreadable_elem =
    | Elem_regular of expr
    | Elem_spread of { expr : expr; loc_ : location }

  and map_expr_elem =
    | Map_expr_elem of {
        key : constant;
        expr : expr;
        key_loc_ : location;
        loc_ : location;
      }

  and error_typ =
    | Error_typ of { ty : typ }
    | Default_error_typ of { loc_ : location }
    | No_error_typ

  and typ =
    | Ptype_any of { loc_ : location }
    | Ptype_arrow of {
        ty_arg : typ list;
        ty_res : typ;
        ty_err : error_typ;
        is_async : bool;
        loc_ : location;
      }
    | Ptype_tuple of { tys : typ list; loc_ : location }
    | Ptype_name of {
        constr_id : constrid_loc;
        tys : typ list;
        loc_ : location;
      }
    | Ptype_option of { ty : typ; loc_ : location; question_loc : location }
    | Ptype_object of constrid_loc

  and pattern =
    | Ppat_alias of { pat : pattern; alias : binder; loc_ : location }
    | Ppat_any of { loc_ : location }
    | Ppat_array of { pats : array_patterns; loc_ : location }
    | Ppat_constant of { c : constant; loc_ : location }
    | Ppat_constraint of { pat : pattern; ty : typ; loc_ : location }
    | Ppat_constr of {
        constr : constructor;
        args : constr_pat_arg list option;
        is_open : bool;
        loc_ : location;
      }
    | Ppat_or of { pat1 : pattern; pat2 : pattern; loc_ : location }
    | Ppat_tuple of { pats : pattern list; loc_ : location }
    | Ppat_var of binder
    | Ppat_record of {
        fields : field_pat list;
        is_closed : bool;
        loc_ : location;
      }
    | Ppat_map of {
        elems : map_pat_elem list;
        is_closed : bool;
        loc_ : location;
      }
    | Ppat_range of {
        lhs : pattern;
        rhs : pattern;
        inclusive : bool;
        loc_ : location;
      }

  and array_patterns =
    | Closed of array_pattern list
    | Open of array_pattern list * array_pattern list * binder option

  and array_pattern =
    | Pattern of pattern
    | String_spread of string_literal
    | String_spread_const of {
        binder : binder;
        pkg : string option;
        loc_ : location;
      }

  and field_def =
    | Field_def of {
        label : label;
        expr : expr;
        is_pun : bool;
        loc_ : location;
      }

  and field_pat =
    | Field_pat of {
        label : label;
        pattern : pattern;
        is_pun : bool;
        loc_ : location;
      }

  and constr_pat_arg =
    | Constr_pat_arg of { pat : pattern; kind : argument_kind }

  and map_pat_elem =
    | Map_pat_elem of {
        key : constant;
        pat : pattern;
        match_absent : bool;
        key_loc_ : location;
        loc_ : location;
      }

  and constr_param = {
    cparam_typ : typ;
    cparam_mut : bool;
    cparam_label : label option;
  }

  and constr_decl = {
    constr_name : constr_name;
    constr_args : constr_param list option;
    constr_tag : (string * location) option;
    constr_loc_ : location;
  }

  and field_name = { label : string; loc_ : location }

  and field_decl = {
    field_name : field_name;
    field_ty : typ;
    field_mut : bool;
    field_vis : visibility;
    field_loc_ : location;
  }

  and exception_decl =
    | No_payload
    | Single_payload of typ
    | Enum_payload of constr_decl list

  and type_desc =
    | Ptd_abstract
    | Ptd_extern
    | Ptd_newtype of typ
    | Ptd_error of exception_decl
    | Ptd_variant of constr_decl list
    | Ptd_record of field_decl list
    | Ptd_alias of typ

  and type_decl = {
    tycon : string;
    tycon_loc_ : location;
    params : type_decl_binder list;
    components : type_desc;
    attrs : attribute list;
    mutable doc_ : docstring;
    type_vis : visibility;
    deriving_ : deriving_directive list;
    loc_ : absolute_loc;
  }

  and local_type_decl = {
    local_tycon : string;
    local_tycon_loc_ : location;
    local_components : type_desc;
    deriving_ : deriving_directive list;
  }

  and deriving_directive = {
    type_name_ : type_name;
    args : argument list;
    loc_ : location;
  }

  and visibility =
    | Vis_default
    | Vis_pub of { attr : string option; loc_ : location }
    | Vis_priv of { loc_ : location }

  and func_stubs =
    | Import of { module_name : string_literal; func_name : string_literal }
    | Embedded of { language : string_literal option; code : embedded_code }

  and embedded_code =
    | Code_string of string_literal
    | Code_multiline_string of string list

  and decl_body =
    | Decl_body of { local_types : local_type_decl list; expr : expr }
    | Decl_stubs of func_stubs

  and fun_decl = {
    type_name : type_name option;
    name : binder;
    has_error : location option;
    is_async : bool;
    decl_params : parameters option;
    params_loc_ : location;
    quantifiers : tvar_binder list;
    return_type : (typ * error_typ) option;
    vis : visibility;
    attrs : attribute list;
    mutable doc_ : docstring;
  }

  and trait_method_param = { tmparam_typ : typ; tmparam_label : label option }

  and trait_method_decl =
    | Trait_method of {
        name : binder;
        has_error : bool;
        quantifiers : tvar_binder list;
        params : trait_method_param list;
        return_type : (typ * error_typ) option;
        loc_ : location;
      }

  and trait_decl = {
    trait_name : binder;
    trait_supers : tvar_constraint list;
    trait_methods : trait_method_decl list;
    trait_vis : visibility;
    trait_loc_ : absolute_loc;
    trait_attrs : attribute list;
    mutable trait_doc_ : docstring;
  }

  and impl =
    | Ptop_expr of {
        expr : expr;
        is_main : bool;
        local_types : local_type_decl list;
        loc_ : absolute_loc;
      }
    | Ptop_test of {
        expr : expr;
        name : test_name;
        params : parameters option;
        local_types : local_type_decl list;
        loc_ : absolute_loc;
        attrs : attribute list;
        mutable doc_ : docstring;
      }
    | Ptop_typedef of type_decl
    | Ptop_funcdef of {
        fun_decl : fun_decl;
        decl_body : decl_body;
        loc_ : absolute_loc;
      }
    | Ptop_letdef of {
        binder : binder;
        ty : typ option;
        expr : expr;
        vis : visibility;
        is_constant : bool;
        loc_ : absolute_loc;
        attrs : attribute list;
        mutable doc_ : docstring;
      }
    | Ptop_trait of trait_decl
    | Ptop_trait_alias of {
        binder : binder;
        target : type_name;
        vis : visibility;
        loc_ : absolute_loc;
        attrs : attribute list;
        mutable doc_ : docstring;
      }
    | Ptop_impl of {
        self_ty : typ option;
        trait : type_name;
        method_name : binder;
        has_error : bool;
        quantifiers : tvar_binder list;
        params : parameters;
        ret_ty : (typ * error_typ) option;
        body : decl_body;
        vis : visibility;
        loc_ : absolute_loc;
        header_loc_ : location;
        attrs : attribute list;
        mutable doc_ : docstring;
      }
    | Ptop_impl_relation of {
        self_ty : typ;
        trait : type_name;
        quantifiers : tvar_binder list;
        is_pub : bool;
        loc_ : absolute_loc;
      } [@dead "impl.Ptop_impl_relation"]

  and interp_elem =
    | Interp_lit of { str : string; repr : string; loc_ : location }
    | Interp_expr of { expr : expr; loc_ : location }
    | Interp_source of Literal.interp_source

  and multiline_string_elem =
    | Multiline_string of string
    | Multiline_interp of interp_elem list

  and impls = impl list

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] sexp =
      object (self : 'self)
        inherit [_] sexpbase

        method visit_Pexpr_apply :
            _ -> expr -> argument list -> apply_attr -> location -> S.t =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fattr ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_ffunc in
                let _visitors_r1 =
                  self#visit_list self#visit_argument env _visitors_fargs
                in
                let _visitors_r2 = self#visit_apply_attr env _visitors_fattr in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_apply"
                  [
                    ("func", _visitors_r0);
                    ("args", _visitors_r1);
                    ("attr", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_infix : _ -> var -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_flhs ->
              fun _visitors_frhs ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fop in
                let _visitors_r1 = self#visit_expr env _visitors_flhs in
                let _visitors_r2 = self#visit_expr env _visitors_frhs in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_infix"
                  [
                    ("op", _visitors_r0);
                    ("lhs", _visitors_r1);
                    ("rhs", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_unary : _ -> var -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_fexpr ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fop in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_unary"
                 [
                   ("op", _visitors_r0);
                   ("expr", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_array : _ -> expr list -> location -> S.t =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_expr env _visitors_fexprs
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_array"
                [ ("exprs", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_array_spread :
            _ -> spreadable_elem list -> location -> S.t =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_spreadable_elem env _visitors_felems
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_array_spread"
                [ ("elems", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_array_get : _ -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_farray in
               let _visitors_r1 = self#visit_expr env _visitors_findex in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_array_get"
                 [
                   ("array", _visitors_r0);
                   ("index", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_array_get_slice :
            _ ->
            expr ->
            expr option ->
            expr option ->
            location ->
            location ->
            S.t =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_fstart_index ->
              fun _visitors_fend_index ->
               fun _visitors_findex_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_farray in
                 let _visitors_r1 =
                   self#visit_option self#visit_expr env _visitors_fstart_index
                 in
                 let _visitors_r2 =
                   self#visit_option self#visit_expr env _visitors_fend_index
                 in
                 let _visitors_r3 =
                   self#visit_location env _visitors_findex_loc_
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_array_get_slice"
                   [
                     ("array", _visitors_r0);
                     ("start_index", _visitors_r1);
                     ("end_index", _visitors_r2);
                     ("index_loc_", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_array_set :
            _ -> expr -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_fvalue ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_farray in
                let _visitors_r1 = self#visit_expr env _visitors_findex in
                let _visitors_r2 = self#visit_expr env _visitors_fvalue in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_array_set"
                  [
                    ("array", _visitors_r0);
                    ("index", _visitors_r1);
                    ("value", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_array_augmented_set :
            _ -> var -> expr -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_farray ->
              fun _visitors_findex ->
               fun _visitors_fvalue ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fop in
                 let _visitors_r1 = self#visit_expr env _visitors_farray in
                 let _visitors_r2 = self#visit_expr env _visitors_findex in
                 let _visitors_r3 = self#visit_expr env _visitors_fvalue in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_array_augmented_set"
                   [
                     ("op", _visitors_r0);
                     ("array", _visitors_r1);
                     ("index", _visitors_r2);
                     ("value", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_constant : _ -> constant -> location -> S.t =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constant env _visitors_fc in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_constant"
                [ ("c", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_multiline_string :
            _ -> multiline_string_elem list -> location -> S.t =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_multiline_string_elem env
                  _visitors_felems
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_multiline_string"
                [ ("elems", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_interp : _ -> interp_elem list -> location -> S.t =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_interp_elem env _visitors_felems
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_interp"
                [ ("elems", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_constraint : _ -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_constraint"
                 [
                   ("expr", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_constr : _ -> constructor -> location -> S.t =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constructor env _visitors_fconstr in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_constr"
                [ ("constr", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_while :
            _ -> expr -> expr -> expr option -> label option -> location -> S.t
            =
          fun env ->
            fun _visitors_floop_cond ->
             fun _visitors_floop_body ->
              fun _visitors_fwhile_else ->
               fun _visitors_flabel ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                 let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                 let _visitors_r2 =
                   self#visit_option self#visit_expr env _visitors_fwhile_else
                 in
                 let _visitors_r3 =
                   self#visit_option self#visit_label env _visitors_flabel
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_while"
                   [
                     ("loop_cond", _visitors_r0);
                     ("loop_body", _visitors_r1);
                     ("while_else", _visitors_r2);
                     ("label", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_function : _ -> func -> location -> S.t =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_func env _visitors_ffunc in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_function"
                [ ("func", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_ident : _ -> var -> location -> S.t =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_var env _visitors_fid in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_ident"
                [ ("id", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_if :
            _ -> expr -> expr -> expr option -> location -> S.t =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 = self#visit_expr env _visitors_fifso in
                let _visitors_r2 =
                  self#visit_option self#visit_expr env _visitors_fifnot
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_if"
                  [
                    ("cond", _visitors_r0);
                    ("ifso", _visitors_r1);
                    ("ifnot", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_guard :
            _ -> expr -> expr option -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 =
                  self#visit_option self#visit_expr env _visitors_fotherwise
                in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_guard"
                  [
                    ("cond", _visitors_r0);
                    ("otherwise", _visitors_r1);
                    ("body", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_guard_let :
            _ -> pattern -> expr -> case list option -> expr -> location -> S.t
            =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fexpr ->
              fun _visitors_fotherwise ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r2 =
                   self#visit_option
                     (self#visit_list self#visit_case)
                     env _visitors_fotherwise
                 in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_guard_let"
                   [
                     ("pat", _visitors_r0);
                     ("expr", _visitors_r1);
                     ("otherwise", _visitors_r2);
                     ("body", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_is : _ -> expr -> pattern -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fpat ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_is"
                 [
                   ("expr", _visitors_r0);
                   ("pat", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_letfn :
            _ -> binder -> func -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffunc ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fname in
                let _visitors_r1 = self#visit_func env _visitors_ffunc in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_letfn"
                  [
                    ("name", _visitors_r0);
                    ("func", _visitors_r1);
                    ("body", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_letrec :
            _ -> (binder * func) list -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list
                   (fun env ->
                     fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 = self#visit_binder env _visitors_c0 in
                      let _visitors_r1 = self#visit_func env _visitors_c1 in
                      self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                   env _visitors_fbindings
               in
               let _visitors_r1 = self#visit_expr env _visitors_fbody in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_letrec"
                 [
                   ("bindings", _visitors_r0);
                   ("body", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_let : _ -> pattern -> expr -> expr -> location -> S.t
            =
          fun env ->
            fun _visitors_fpattern ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_let"
                  [
                    ("pattern", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("body", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_sequence : _ -> expr list -> expr -> location -> S.t
            =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_expr env _visitors_fexprs
               in
               let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_sequence"
                 [
                   ("exprs", _visitors_r0);
                   ("last_expr", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_tuple : _ -> expr list -> location -> S.t =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_expr env _visitors_fexprs
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_tuple"
                [ ("exprs", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_record :
            _ ->
            type_name option ->
            field_def list ->
            trailing_mark ->
            location ->
            S.t =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_ffields ->
              fun _visitors_ftrailing ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_option self#visit_type_name env
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  self#visit_list self#visit_field_def env _visitors_ffields
                in
                let _visitors_r2 =
                  self#visit_trailing_mark env _visitors_ftrailing
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_record"
                  [
                    ("type_name", _visitors_r0);
                    ("fields", _visitors_r1);
                    ("trailing", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_record_update :
            _ -> type_name option -> expr -> field_def list -> location -> S.t =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_frecord ->
              fun _visitors_ffields ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_option self#visit_type_name env
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  self#visit_list self#visit_field_def env _visitors_ffields
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_record_update"
                  [
                    ("type_name", _visitors_r0);
                    ("record", _visitors_r1);
                    ("fields", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_field : _ -> expr -> accessor -> location -> S.t =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_field"
                 [
                   ("record", _visitors_r0);
                   ("accessor", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_method : _ -> type_name -> label -> location -> S.t =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_fmethod_name ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_type_name env _visitors_ftype_name
               in
               let _visitors_r1 = self#visit_label env _visitors_fmethod_name in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_method"
                 [
                   ("type_name", _visitors_r0);
                   ("method_name", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_dot_apply :
            _ ->
            expr ->
            label ->
            argument list ->
            bool ->
            apply_attr ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fself ->
             fun _visitors_fmethod_name ->
              fun _visitors_fargs ->
               fun _visitors_freturn_self ->
                fun _visitors_fattr ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_fself in
                  let _visitors_r1 =
                    self#visit_label env _visitors_fmethod_name
                  in
                  let _visitors_r2 =
                    self#visit_list self#visit_argument env _visitors_fargs
                  in
                  let _visitors_r3 =
                    self#visit_bool env _visitors_freturn_self
                  in
                  let _visitors_r4 =
                    self#visit_apply_attr env _visitors_fattr
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Pexpr_dot_apply"
                    [
                      ("self", _visitors_r0);
                      ("method_name", _visitors_r1);
                      ("args", _visitors_r2);
                      ("return_self", _visitors_r3);
                      ("attr", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Pexpr_as : _ -> expr -> type_name -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_as"
                 [
                   ("expr", _visitors_r0);
                   ("trait", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_mutate :
            _ -> expr -> accessor -> expr -> var option -> location -> S.t =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_ffield ->
               fun _visitors_faugmented_by ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 =
                   self#visit_option self#visit_var env _visitors_faugmented_by
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_mutate"
                   [
                     ("record", _visitors_r0);
                     ("accessor", _visitors_r1);
                     ("field", _visitors_r2);
                     ("augmented_by", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_match :
            _ -> expr -> case list -> location -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fcases ->
              fun _visitors_fmatch_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  self#visit_list self#visit_case env _visitors_fcases
                in
                let _visitors_r2 =
                  self#visit_location env _visitors_fmatch_loc_
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_match"
                  [
                    ("expr", _visitors_r0);
                    ("cases", _visitors_r1);
                    ("match_loc_", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_letmut :
            _ -> binder -> typ option -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                 let _visitors_r1 =
                   self#visit_option self#visit_typ env _visitors_fty
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_letmut"
                   [
                     ("binder", _visitors_r0);
                     ("ty", _visitors_r1);
                     ("expr", _visitors_r2);
                     ("body", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_pipe : _ -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_pipe"
                 [
                   ("lhs", _visitors_r0);
                   ("rhs", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_assign :
            _ -> var -> expr -> var option -> location -> S.t =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_faugmented_by ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  self#visit_option self#visit_var env _visitors_faugmented_by
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Pexpr_assign"
                  [
                    ("var", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("augmented_by", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Pexpr_hole : _ -> location -> hole -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_location env _visitors_floc_ in
              let _visitors_r1 = self#visit_hole env _visitors_fkind in
              self#visit_inline_record env "Pexpr_hole"
                [ ("loc_", _visitors_r0); ("kind", _visitors_r1) ]

        method visit_Pexpr_return : _ -> expr option -> location -> S.t =
          fun env ->
            fun _visitors_freturn_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_option self#visit_expr env _visitors_freturn_value
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_return"
                [ ("return_value", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_raise : _ -> expr -> location -> S.t =
          fun env ->
            fun _visitors_ferr_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_ferr_value in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_raise"
                [ ("err_value", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_unit : _ -> location -> bool -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_ffaked ->
              let _visitors_r0 = self#visit_location env _visitors_floc_ in
              let _visitors_r1 = self#visit_bool env _visitors_ffaked in
              self#visit_inline_record env "Pexpr_unit"
                [ ("loc_", _visitors_r0); ("faked", _visitors_r1) ]

        method visit_Pexpr_break :
            _ -> expr option -> label option -> location -> S.t =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_option self#visit_expr env _visitors_farg
               in
               let _visitors_r1 =
                 self#visit_option self#visit_label env _visitors_flabel
               in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_break"
                 [
                   ("arg", _visitors_r0);
                   ("label", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_continue :
            _ -> expr list -> label option -> location -> S.t =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_expr env _visitors_fargs
               in
               let _visitors_r1 =
                 self#visit_option self#visit_label env _visitors_flabel
               in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_continue"
                 [
                   ("args", _visitors_r0);
                   ("label", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_loop :
            _ ->
            expr list ->
            multi_arg_case list ->
            label option ->
            location ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_fbody ->
              fun _visitors_flabel ->
               fun _visitors_floop_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   self#visit_list self#visit_expr env _visitors_fargs
                 in
                 let _visitors_r1 =
                   self#visit_list self#visit_multi_arg_case env _visitors_fbody
                 in
                 let _visitors_r2 =
                   self#visit_option self#visit_label env _visitors_flabel
                 in
                 let _visitors_r3 =
                   self#visit_location env _visitors_floop_loc_
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Pexpr_loop"
                   [
                     ("args", _visitors_r0);
                     ("body", _visitors_r1);
                     ("label", _visitors_r2);
                     ("loop_loc_", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Pexpr_for :
            _ ->
            (binder * expr) list ->
            expr option ->
            (binder * expr) list ->
            expr ->
            expr option ->
            label option ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fcondition ->
              fun _visitors_fcontinue_block ->
               fun _visitors_fbody ->
                fun _visitors_ffor_else ->
                 fun _visitors_flabel ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     self#visit_list
                       (fun env ->
                         fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            self#visit_binder env _visitors_c0
                          in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                       env _visitors_fbinders
                   in
                   let _visitors_r1 =
                     self#visit_option self#visit_expr env _visitors_fcondition
                   in
                   let _visitors_r2 =
                     self#visit_list
                       (fun env ->
                         fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            self#visit_binder env _visitors_c0
                          in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                       env _visitors_fcontinue_block
                   in
                   let _visitors_r3 = self#visit_expr env _visitors_fbody in
                   let _visitors_r4 =
                     self#visit_option self#visit_expr env _visitors_ffor_else
                   in
                   let _visitors_r5 =
                     self#visit_option self#visit_label env _visitors_flabel
                   in
                   let _visitors_r6 = self#visit_location env _visitors_floc_ in
                   self#visit_inline_record env "Pexpr_for"
                     [
                       ("binders", _visitors_r0);
                       ("condition", _visitors_r1);
                       ("continue_block", _visitors_r2);
                       ("body", _visitors_r3);
                       ("for_else", _visitors_r4);
                       ("label", _visitors_r5);
                       ("loc_", _visitors_r6);
                     ]

        method visit_Pexpr_foreach :
            _ ->
            binder option list ->
            expr ->
            expr ->
            expr option ->
            label option ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_felse_block ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    self#visit_list
                      (self#visit_option self#visit_binder)
                      env _visitors_fbinders
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 =
                    self#visit_option self#visit_expr env _visitors_felse_block
                  in
                  let _visitors_r4 =
                    self#visit_option self#visit_label env _visitors_flabel
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Pexpr_foreach"
                    [
                      ("binders", _visitors_r0);
                      ("expr", _visitors_r1);
                      ("body", _visitors_r2);
                      ("else_block", _visitors_r3);
                      ("label", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Pexpr_try :
            _ ->
            expr ->
            case list ->
            bool ->
            case list option ->
            location ->
            location ->
            location ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fbody ->
             fun _visitors_fcatch ->
              fun _visitors_fcatch_all ->
               fun _visitors_ftry_else ->
                fun _visitors_ftry_loc_ ->
                 fun _visitors_fcatch_loc_ ->
                  fun _visitors_felse_loc_ ->
                   fun _visitors_floc_ ->
                    let _visitors_r0 = self#visit_expr env _visitors_fbody in
                    let _visitors_r1 =
                      self#visit_list self#visit_case env _visitors_fcatch
                    in
                    let _visitors_r2 =
                      self#visit_bool env _visitors_fcatch_all
                    in
                    let _visitors_r3 =
                      self#visit_option
                        (self#visit_list self#visit_case)
                        env _visitors_ftry_else
                    in
                    let _visitors_r4 =
                      self#visit_location env _visitors_ftry_loc_
                    in
                    let _visitors_r5 =
                      self#visit_location env _visitors_fcatch_loc_
                    in
                    let _visitors_r6 =
                      self#visit_location env _visitors_felse_loc_
                    in
                    let _visitors_r7 =
                      self#visit_location env _visitors_floc_
                    in
                    self#visit_inline_record env "Pexpr_try"
                      [
                        ("body", _visitors_r0);
                        ("catch", _visitors_r1);
                        ("catch_all", _visitors_r2);
                        ("try_else", _visitors_r3);
                        ("try_loc_", _visitors_r4);
                        ("catch_loc_", _visitors_r5);
                        ("else_loc_", _visitors_r6);
                        ("loc_", _visitors_r7);
                      ]

        method visit_Pexpr_map : _ -> map_expr_elem list -> location -> S.t =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_map_expr_elem env _visitors_felems
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Pexpr_map"
                [ ("elems", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Pexpr_group : _ -> expr -> group -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fgroup ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_group env _visitors_fgroup in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Pexpr_group"
                 [
                   ("expr", _visitors_r0);
                   ("group", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Pexpr_static_assert :
            _ -> static_assertion list -> expr -> S.t =
          fun env ->
            fun _visitors_fasserts ->
             fun _visitors_fbody ->
              let _visitors_r0 =
                self#visit_list self#visit_static_assertion env
                  _visitors_fasserts
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              self#visit_inline_record env "Pexpr_static_assert"
                [ ("asserts", _visitors_r0); ("body", _visitors_r1) ]

        method visit_expr : _ -> expr -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fattr _visitors_floc_
             | Pexpr_infix
                 {
                   op = _visitors_fop;
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_infix env _visitors_fop _visitors_flhs
                   _visitors_frhs _visitors_floc_
             | Pexpr_unary
                 {
                   op = _visitors_fop;
                   expr = _visitors_fexpr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_unary env _visitors_fop _visitors_fexpr
                   _visitors_floc_
             | Pexpr_array { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_array env _visitors_fexprs _visitors_floc_
             | Pexpr_array_spread
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_array_spread env _visitors_felems
                   _visitors_floc_
             | Pexpr_array_get
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get env _visitors_farray
                   _visitors_findex _visitors_floc_
             | Pexpr_array_get_slice
                 {
                   array = _visitors_farray;
                   start_index = _visitors_fstart_index;
                   end_index = _visitors_fend_index;
                   index_loc_ = _visitors_findex_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get_slice env _visitors_farray
                   _visitors_fstart_index _visitors_fend_index
                   _visitors_findex_loc_ _visitors_floc_
             | Pexpr_array_set
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_set env _visitors_farray
                   _visitors_findex _visitors_fvalue _visitors_floc_
             | Pexpr_array_augmented_set
                 {
                   op = _visitors_fop;
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_augmented_set env _visitors_fop
                   _visitors_farray _visitors_findex _visitors_fvalue
                   _visitors_floc_
             | Pexpr_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constant env _visitors_fc _visitors_floc_
             | Pexpr_multiline_string
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_multiline_string env _visitors_felems
                   _visitors_floc_
             | Pexpr_interp { elems = _visitors_felems; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_interp env _visitors_felems _visitors_floc_
             | Pexpr_constraint
                 {
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_constraint env _visitors_fexpr _visitors_fty
                   _visitors_floc_
             | Pexpr_constr
                 { constr = _visitors_fconstr; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constr env _visitors_fconstr _visitors_floc_
             | Pexpr_while
                 {
                   loop_cond = _visitors_floop_cond;
                   loop_body = _visitors_floop_body;
                   while_else = _visitors_fwhile_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_while env _visitors_floop_cond
                   _visitors_floop_body _visitors_fwhile_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_function { func = _visitors_ffunc; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_function env _visitors_ffunc _visitors_floc_
             | Pexpr_ident { id = _visitors_fid; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_ident env _visitors_fid _visitors_floc_
             | Pexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_floc_
             | Pexpr_guard
                 {
                   cond = _visitors_fcond;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard env _visitors_fcond _visitors_fotherwise
                   _visitors_fbody _visitors_floc_
             | Pexpr_guard_let
                 {
                   pat = _visitors_fpat;
                   expr = _visitors_fexpr;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard_let env _visitors_fpat _visitors_fexpr
                   _visitors_fotherwise _visitors_fbody _visitors_floc_
             | Pexpr_is
                 {
                   expr = _visitors_fexpr;
                   pat = _visitors_fpat;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_is env _visitors_fexpr _visitors_fpat
                   _visitors_floc_
             | Pexpr_letfn
                 {
                   name = _visitors_fname;
                   func = _visitors_ffunc;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letfn env _visitors_fname _visitors_ffunc
                   _visitors_fbody _visitors_floc_
             | Pexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_floc_
             | Pexpr_let
                 {
                   pattern = _visitors_fpattern;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_let env _visitors_fpattern _visitors_fexpr
                   _visitors_fbody _visitors_floc_
             | Pexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_floc_
             | Pexpr_tuple { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_tuple env _visitors_fexprs _visitors_floc_
             | Pexpr_record
                 {
                   type_name = _visitors_ftype_name;
                   fields = _visitors_ffields;
                   trailing = _visitors_ftrailing;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record env _visitors_ftype_name
                   _visitors_ffields _visitors_ftrailing _visitors_floc_
             | Pexpr_record_update
                 {
                   type_name = _visitors_ftype_name;
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record_update env _visitors_ftype_name
                   _visitors_frecord _visitors_ffields _visitors_floc_
             | Pexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_floc_
             | Pexpr_method
                 {
                   type_name = _visitors_ftype_name;
                   method_name = _visitors_fmethod_name;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_method env _visitors_ftype_name
                   _visitors_fmethod_name _visitors_floc_
             | Pexpr_dot_apply
                 {
                   self = _visitors_fself;
                   method_name = _visitors_fmethod_name;
                   args = _visitors_fargs;
                   return_self = _visitors_freturn_self;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_dot_apply env _visitors_fself
                   _visitors_fmethod_name _visitors_fargs _visitors_freturn_self
                   _visitors_fattr _visitors_floc_
             | Pexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_floc_
             | Pexpr_mutate
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   field = _visitors_ffield;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_mutate env _visitors_frecord
                   _visitors_faccessor _visitors_ffield _visitors_faugmented_by
                   _visitors_floc_
             | Pexpr_match
                 {
                   expr = _visitors_fexpr;
                   cases = _visitors_fcases;
                   match_loc_ = _visitors_fmatch_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_match env _visitors_fexpr _visitors_fcases
                   _visitors_fmatch_loc_ _visitors_floc_
             | Pexpr_letmut
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letmut env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fbody _visitors_floc_
             | Pexpr_pipe
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_pipe env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Pexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_faugmented_by _visitors_floc_
             | Pexpr_hole { loc_ = _visitors_floc_; kind = _visitors_fkind } ->
                 self#visit_Pexpr_hole env _visitors_floc_ _visitors_fkind
             | Pexpr_return
                 {
                   return_value = _visitors_freturn_value;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_return env _visitors_freturn_value
                   _visitors_floc_
             | Pexpr_raise
                 { err_value = _visitors_ferr_value; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_raise env _visitors_ferr_value _visitors_floc_
             | Pexpr_unit { loc_ = _visitors_floc_; faked = _visitors_ffaked }
               ->
                 self#visit_Pexpr_unit env _visitors_floc_ _visitors_ffaked
             | Pexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_break env _visitors_farg _visitors_flabel
                   _visitors_floc_
             | Pexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_floc_
             | Pexpr_loop
                 {
                   args = _visitors_fargs;
                   body = _visitors_fbody;
                   label = _visitors_flabel;
                   loop_loc_ = _visitors_floop_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_loop env _visitors_fargs _visitors_fbody
                   _visitors_flabel _visitors_floop_loc_ _visitors_floc_
             | Pexpr_for
                 {
                   binders = _visitors_fbinders;
                   condition = _visitors_fcondition;
                   continue_block = _visitors_fcontinue_block;
                   body = _visitors_fbody;
                   for_else = _visitors_ffor_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_for env _visitors_fbinders
                   _visitors_fcondition _visitors_fcontinue_block
                   _visitors_fbody _visitors_ffor_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_foreach
                 {
                   binders = _visitors_fbinders;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   else_block = _visitors_felse_block;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_foreach env _visitors_fbinders _visitors_fexpr
                   _visitors_fbody _visitors_felse_block _visitors_flabel
                   _visitors_floc_
             | Pexpr_try
                 {
                   body = _visitors_fbody;
                   catch = _visitors_fcatch;
                   catch_all = _visitors_fcatch_all;
                   try_else = _visitors_ftry_else;
                   try_loc_ = _visitors_ftry_loc_;
                   catch_loc_ = _visitors_fcatch_loc_;
                   else_loc_ = _visitors_felse_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_try env _visitors_fbody _visitors_fcatch
                   _visitors_fcatch_all _visitors_ftry_else _visitors_ftry_loc_
                   _visitors_fcatch_loc_ _visitors_felse_loc_ _visitors_floc_
             | Pexpr_map { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_map env _visitors_felems _visitors_floc_
             | Pexpr_group
                 {
                   expr = _visitors_fexpr;
                   group = _visitors_fgroup;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_group env _visitors_fexpr _visitors_fgroup
                   _visitors_floc_
             | Pexpr_static_assert
                 { asserts = _visitors_fasserts; body = _visitors_fbody } ->
                 self#visit_Pexpr_static_assert env _visitors_fasserts
                   _visitors_fbody

        method visit_static_assertion : _ -> static_assertion -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.assert_type in
             let _visitors_r1 =
               self#visit_longident env _visitors_this.assert_trait
             in
             let _visitors_r2 =
               self#visit_location env _visitors_this.assert_loc
             in
             let _visitors_r3 =
               self#visit_string env _visitors_this.assert_msg
             in
             self#visit_record env
               [
                 ("assert_type", _visitors_r0);
                 ("assert_trait", _visitors_r1);
                 ("assert_loc", _visitors_r2);
                 ("assert_msg", _visitors_r3);
               ]

        method visit_argument : _ -> argument -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
             let _visitors_r1 =
               self#visit_argument_kind env _visitors_this.arg_kind
             in
             self#visit_record env
               [ ("arg_value", _visitors_r0); ("arg_kind", _visitors_r1) ]

        method visit_parameters : _ -> parameters -> S.t =
          fun env -> self#visit_list self#visit_parameter env

        method visit_Discard_positional : _ -> typ option -> location -> S.t =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_option self#visit_typ env _visitors_fty
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Discard_positional"
                [ ("ty", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Positional : _ -> binder -> typ option -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                self#visit_option self#visit_typ env _visitors_fty
              in
              self#visit_inline_record env "Positional"
                [ ("binder", _visitors_r0); ("ty", _visitors_r1) ]

        method visit_Labelled : _ -> binder -> typ option -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                self#visit_option self#visit_typ env _visitors_fty
              in
              self#visit_inline_record env "Labelled"
                [ ("binder", _visitors_r0); ("ty", _visitors_r1) ]

        method visit_Optional : _ -> binder -> expr -> typ option -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fdefault ->
              fun _visitors_fty ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 = self#visit_expr env _visitors_fdefault in
               let _visitors_r2 =
                 self#visit_option self#visit_typ env _visitors_fty
               in
               self#visit_inline_record env "Optional"
                 [
                   ("binder", _visitors_r0);
                   ("default", _visitors_r1);
                   ("ty", _visitors_r2);
                 ]

        method visit_Question_optional : _ -> binder -> typ option -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                self#visit_option self#visit_typ env _visitors_fty
              in
              self#visit_inline_record env "Question_optional"
                [ ("binder", _visitors_r0); ("ty", _visitors_r1) ]

        method visit_parameter : _ -> parameter -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Discard_positional { ty = _visitors_fty; loc_ = _visitors_floc_ }
               ->
                 self#visit_Discard_positional env _visitors_fty _visitors_floc_
             | Positional { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Positional env _visitors_fbinder _visitors_fty
             | Labelled { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Labelled env _visitors_fbinder _visitors_fty
             | Optional
                 {
                   binder = _visitors_fbinder;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                 } ->
                 self#visit_Optional env _visitors_fbinder _visitors_fdefault
                   _visitors_fty
             | Question_optional
                 { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Question_optional env _visitors_fbinder
                   _visitors_fty

        method visit_Lambda :
            _ ->
            parameters ->
            location ->
            expr ->
            (typ * error_typ) option ->
            fn_kind ->
            location option ->
            bool ->
            S.t =
          fun env ->
            fun _visitors_fparameters ->
             fun _visitors_fparams_loc_ ->
              fun _visitors_fbody ->
               fun _visitors_freturn_type ->
                fun _visitors_fkind_ ->
                 fun _visitors_fhas_error ->
                  fun _visitors_fis_async ->
                   let _visitors_r0 =
                     self#visit_parameters env _visitors_fparameters
                   in
                   let _visitors_r1 =
                     self#visit_location env _visitors_fparams_loc_
                   in
                   let _visitors_r2 = self#visit_expr env _visitors_fbody in
                   let _visitors_r3 =
                     self#visit_option
                       (fun env ->
                         fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 = self#visit_typ env _visitors_c0 in
                          let _visitors_r1 =
                            self#visit_error_typ env _visitors_c1
                          in
                          self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                       env _visitors_freturn_type
                   in
                   let _visitors_r4 = self#visit_fn_kind env _visitors_fkind_ in
                   let _visitors_r5 =
                     self#visit_option self#visit_location env
                       _visitors_fhas_error
                   in
                   let _visitors_r6 = self#visit_bool env _visitors_fis_async in
                   self#visit_inline_record env "Lambda"
                     [
                       ("parameters", _visitors_r0);
                       ("params_loc_", _visitors_r1);
                       ("body", _visitors_r2);
                       ("return_type", _visitors_r3);
                       ("kind_", _visitors_r4);
                       ("has_error", _visitors_r5);
                       ("is_async", _visitors_r6);
                     ]

        method visit_Match :
            _ ->
            multi_arg_case list ->
            location option ->
            bool ->
            location ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fcases ->
             fun _visitors_fhas_error ->
              fun _visitors_fis_async ->
               fun _visitors_ffn_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   self#visit_list self#visit_multi_arg_case env
                     _visitors_fcases
                 in
                 let _visitors_r1 =
                   self#visit_option self#visit_location env
                     _visitors_fhas_error
                 in
                 let _visitors_r2 = self#visit_bool env _visitors_fis_async in
                 let _visitors_r3 =
                   self#visit_location env _visitors_ffn_loc_
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Match"
                   [
                     ("cases", _visitors_r0);
                     ("has_error", _visitors_r1);
                     ("is_async", _visitors_r2);
                     ("fn_loc_", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_func : _ -> func -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Lambda
                 {
                   parameters = _visitors_fparameters;
                   params_loc_ = _visitors_fparams_loc_;
                   body = _visitors_fbody;
                   return_type = _visitors_freturn_type;
                   kind_ = _visitors_fkind_;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                 } ->
                 self#visit_Lambda env _visitors_fparameters
                   _visitors_fparams_loc_ _visitors_fbody _visitors_freturn_type
                   _visitors_fkind_ _visitors_fhas_error _visitors_fis_async
             | Match
                 {
                   cases = _visitors_fcases;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                   fn_loc_ = _visitors_ffn_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Match env _visitors_fcases _visitors_fhas_error
                   _visitors_fis_async _visitors_ffn_loc_ _visitors_floc_

        method visit_case : _ -> case -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_pattern env _visitors_this.pattern in
             let _visitors_r1 =
               self#visit_option self#visit_expr env _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             self#visit_record env
               [
                 ("pattern", _visitors_r0);
                 ("guard", _visitors_r1);
                 ("body", _visitors_r2);
               ]

        method visit_multi_arg_case : _ -> multi_arg_case -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_list self#visit_pattern env _visitors_this.patterns
             in
             let _visitors_r1 =
               self#visit_option self#visit_expr env _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             self#visit_record env
               [
                 ("patterns", _visitors_r0);
                 ("guard", _visitors_r1);
                 ("body", _visitors_r2);
               ]

        method visit_Elem_regular : _ -> expr -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_expr env _visitors_c0 in
             self#visit_inline_tuple env "Elem_regular" [ _visitors_r0 ]

        method visit_Elem_spread : _ -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Elem_spread"
                [ ("expr", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_spreadable_elem : _ -> spreadable_elem -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Elem_regular _visitors_c0 ->
                 self#visit_Elem_regular env _visitors_c0
             | Elem_spread { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Elem_spread env _visitors_fexpr _visitors_floc_

        method visit_Map_expr_elem :
            _ -> constant -> expr -> location -> location -> S.t =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fexpr ->
              fun _visitors_fkey_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_constant env _visitors_fkey in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  self#visit_location env _visitors_fkey_loc_
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Map_expr_elem"
                  [
                    ("key", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("key_loc_", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_map_expr_elem : _ -> map_expr_elem -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_expr_elem
                 {
                   key = _visitors_fkey;
                   expr = _visitors_fexpr;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_expr_elem env _visitors_fkey _visitors_fexpr
                   _visitors_fkey_loc_ _visitors_floc_

        method visit_Error_typ : _ -> typ -> S.t =
          fun env ->
            fun _visitors_fty ->
             let _visitors_r0 = self#visit_typ env _visitors_fty in
             self#visit_inline_record env "Error_typ" [ ("ty", _visitors_r0) ]

        method visit_Default_error_typ : _ -> location -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Default_error_typ"
               [ ("loc_", _visitors_r0) ]

        method visit_No_error_typ : _ -> S.t =
          fun env -> self#visit_inline_tuple env "No_error_typ" []

        method visit_error_typ : _ -> error_typ -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Error_typ { ty = _visitors_fty } ->
                 self#visit_Error_typ env _visitors_fty
             | Default_error_typ { loc_ = _visitors_floc_ } ->
                 self#visit_Default_error_typ env _visitors_floc_
             | No_error_typ -> self#visit_No_error_typ env

        method visit_Ptype_any : _ -> location -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Ptype_any" [ ("loc_", _visitors_r0) ]

        method visit_Ptype_arrow :
            _ -> typ list -> typ -> error_typ -> bool -> location -> S.t =
          fun env ->
            fun _visitors_fty_arg ->
             fun _visitors_fty_res ->
              fun _visitors_fty_err ->
               fun _visitors_fis_async ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   self#visit_list self#visit_typ env _visitors_fty_arg
                 in
                 let _visitors_r1 = self#visit_typ env _visitors_fty_res in
                 let _visitors_r2 =
                   self#visit_error_typ env _visitors_fty_err
                 in
                 let _visitors_r3 = self#visit_bool env _visitors_fis_async in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Ptype_arrow"
                   [
                     ("ty_arg", _visitors_r0);
                     ("ty_res", _visitors_r1);
                     ("ty_err", _visitors_r2);
                     ("is_async", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Ptype_tuple : _ -> typ list -> location -> S.t =
          fun env ->
            fun _visitors_ftys ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_typ env _visitors_ftys
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Ptype_tuple"
                [ ("tys", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Ptype_name :
            _ -> constrid_loc -> typ list -> location -> S.t =
          fun env ->
            fun _visitors_fconstr_id ->
             fun _visitors_ftys ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr_id
               in
               let _visitors_r1 =
                 self#visit_list self#visit_typ env _visitors_ftys
               in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ptype_name"
                 [
                   ("constr_id", _visitors_r0);
                   ("tys", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ptype_option : _ -> typ -> location -> location -> S.t =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              fun _visitors_fquestion_loc ->
               let _visitors_r0 = self#visit_typ env _visitors_fty in
               let _visitors_r1 = self#visit_location env _visitors_floc_ in
               let _visitors_r2 =
                 self#visit_location env _visitors_fquestion_loc
               in
               self#visit_inline_record env "Ptype_option"
                 [
                   ("ty", _visitors_r0);
                   ("loc_", _visitors_r1);
                   ("question_loc", _visitors_r2);
                 ]

        method visit_Ptype_object : _ -> constrid_loc -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_constrid_loc env _visitors_c0 in
             self#visit_inline_tuple env "Ptype_object" [ _visitors_r0 ]

        method visit_typ : _ -> typ -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptype_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_any env _visitors_floc_
             | Ptype_arrow
                 {
                   ty_arg = _visitors_fty_arg;
                   ty_res = _visitors_fty_res;
                   ty_err = _visitors_fty_err;
                   is_async = _visitors_fis_async;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_arrow env _visitors_fty_arg _visitors_fty_res
                   _visitors_fty_err _visitors_fis_async _visitors_floc_
             | Ptype_tuple { tys = _visitors_ftys; loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_tuple env _visitors_ftys _visitors_floc_
             | Ptype_name
                 {
                   constr_id = _visitors_fconstr_id;
                   tys = _visitors_ftys;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_name env _visitors_fconstr_id _visitors_ftys
                   _visitors_floc_
             | Ptype_option
                 {
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                   question_loc = _visitors_fquestion_loc;
                 } ->
                 self#visit_Ptype_option env _visitors_fty _visitors_floc_
                   _visitors_fquestion_loc
             | Ptype_object _visitors_c0 ->
                 self#visit_Ptype_object env _visitors_c0

        method visit_Ppat_alias : _ -> pattern -> binder -> location -> S.t =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_falias ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_binder env _visitors_falias in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ppat_alias"
                 [
                   ("pat", _visitors_r0);
                   ("alias", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ppat_any : _ -> location -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Ppat_any" [ ("loc_", _visitors_r0) ]

        method visit_Ppat_array : _ -> array_patterns -> location -> S.t =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_array_patterns env _visitors_fpats
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Ppat_array"
                [ ("pats", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Ppat_constant : _ -> constant -> location -> S.t =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constant env _visitors_fc in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Ppat_constant"
                [ ("c", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Ppat_constraint : _ -> pattern -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ppat_constraint"
                 [
                   ("pat", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ppat_constr :
            _ ->
            constructor ->
            constr_pat_arg list option ->
            bool ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_fargs ->
              fun _visitors_fis_open ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_constructor env _visitors_fconstr
                in
                let _visitors_r1 =
                  self#visit_option
                    (self#visit_list self#visit_constr_pat_arg)
                    env _visitors_fargs
                in
                let _visitors_r2 = self#visit_bool env _visitors_fis_open in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Ppat_constr"
                  [
                    ("constr", _visitors_r0);
                    ("args", _visitors_r1);
                    ("is_open", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Ppat_or : _ -> pattern -> pattern -> location -> S.t =
          fun env ->
            fun _visitors_fpat1 ->
             fun _visitors_fpat2 ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat1 in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat2 in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ppat_or"
                 [
                   ("pat1", _visitors_r0);
                   ("pat2", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ppat_tuple : _ -> pattern list -> location -> S.t =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_pattern env _visitors_fpats
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Ppat_tuple"
                [ ("pats", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Ppat_var : _ -> binder -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_binder env _visitors_c0 in
             self#visit_inline_tuple env "Ppat_var" [ _visitors_r0 ]

        method visit_Ppat_record :
            _ -> field_pat list -> bool -> location -> S.t =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_field_pat env _visitors_ffields
               in
               let _visitors_r1 = self#visit_bool env _visitors_fis_closed in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ppat_record"
                 [
                   ("fields", _visitors_r0);
                   ("is_closed", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ppat_map :
            _ -> map_pat_elem list -> bool -> location -> S.t =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_map_pat_elem env _visitors_felems
               in
               let _visitors_r1 = self#visit_bool env _visitors_fis_closed in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Ppat_map"
                 [
                   ("elems", _visitors_r0);
                   ("is_closed", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ppat_range :
            _ -> pattern -> pattern -> bool -> location -> S.t =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_finclusive ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_flhs in
                let _visitors_r1 = self#visit_pattern env _visitors_frhs in
                let _visitors_r2 = self#visit_bool env _visitors_finclusive in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Ppat_range"
                  [
                    ("lhs", _visitors_r0);
                    ("rhs", _visitors_r1);
                    ("inclusive", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_pattern : _ -> pattern -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ppat_alias
                 {
                   pat = _visitors_fpat;
                   alias = _visitors_falias;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_alias env _visitors_fpat _visitors_falias
                   _visitors_floc_
             | Ppat_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_any env _visitors_floc_
             | Ppat_array { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_array env _visitors_fpats _visitors_floc_
             | Ppat_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_constant env _visitors_fc _visitors_floc_
             | Ppat_constraint
                 {
                   pat = _visitors_fpat;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constraint env _visitors_fpat _visitors_fty
                   _visitors_floc_
             | Ppat_constr
                 {
                   constr = _visitors_fconstr;
                   args = _visitors_fargs;
                   is_open = _visitors_fis_open;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constr env _visitors_fconstr _visitors_fargs
                   _visitors_fis_open _visitors_floc_
             | Ppat_or
                 {
                   pat1 = _visitors_fpat1;
                   pat2 = _visitors_fpat2;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_or env _visitors_fpat1 _visitors_fpat2
                   _visitors_floc_
             | Ppat_tuple { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_tuple env _visitors_fpats _visitors_floc_
             | Ppat_var _visitors_c0 -> self#visit_Ppat_var env _visitors_c0
             | Ppat_record
                 {
                   fields = _visitors_ffields;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_record env _visitors_ffields
                   _visitors_fis_closed _visitors_floc_
             | Ppat_map
                 {
                   elems = _visitors_felems;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_map env _visitors_felems _visitors_fis_closed
                   _visitors_floc_
             | Ppat_range
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   inclusive = _visitors_finclusive;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_range env _visitors_flhs _visitors_frhs
                   _visitors_finclusive _visitors_floc_

        method visit_Closed : _ -> array_pattern list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_array_pattern env _visitors_c0
             in
             self#visit_inline_tuple env "Closed" [ _visitors_r0 ]

        method visit_Open :
            _ ->
            array_pattern list ->
            array_pattern list ->
            binder option ->
            S.t =
          fun env ->
            fun _visitors_c0 ->
             fun _visitors_c1 ->
              fun _visitors_c2 ->
               let _visitors_r0 =
                 self#visit_list self#visit_array_pattern env _visitors_c0
               in
               let _visitors_r1 =
                 self#visit_list self#visit_array_pattern env _visitors_c1
               in
               let _visitors_r2 =
                 self#visit_option self#visit_binder env _visitors_c2
               in
               self#visit_inline_tuple env "Open"
                 [ _visitors_r0; _visitors_r1; _visitors_r2 ]

        method visit_array_patterns : _ -> array_patterns -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
             | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
                 self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

        method visit_Pattern : _ -> pattern -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_pattern env _visitors_c0 in
             self#visit_inline_tuple env "Pattern" [ _visitors_r0 ]

        method visit_String_spread : _ -> string_literal -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             self#visit_inline_tuple env "String_spread" [ _visitors_r0 ]

        method visit_String_spread_const :
            _ -> binder -> string option -> location -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fpkg ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 =
                 self#visit_option self#visit_string env _visitors_fpkg
               in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "String_spread_const"
                 [
                   ("binder", _visitors_r0);
                   ("pkg", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_array_pattern : _ -> array_pattern -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pattern _visitors_c0 -> self#visit_Pattern env _visitors_c0
             | String_spread _visitors_c0 ->
                 self#visit_String_spread env _visitors_c0
             | String_spread_const
                 {
                   binder = _visitors_fbinder;
                   pkg = _visitors_fpkg;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_String_spread_const env _visitors_fbinder
                   _visitors_fpkg _visitors_floc_

        method visit_Field_def : _ -> label -> expr -> bool -> location -> S.t =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_bool env _visitors_fis_pun in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Field_def"
                  [
                    ("label", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("is_pun", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_field_def : _ -> field_def -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_def
                 {
                   label = _visitors_flabel;
                   expr = _visitors_fexpr;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_def env _visitors_flabel _visitors_fexpr
                   _visitors_fis_pun _visitors_floc_

        method visit_Field_pat :
            _ -> label -> pattern -> bool -> location -> S.t =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fpattern ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r2 = self#visit_bool env _visitors_fis_pun in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Field_pat"
                  [
                    ("label", _visitors_r0);
                    ("pattern", _visitors_r1);
                    ("is_pun", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_field_pat : _ -> field_pat -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_pat
                 {
                   label = _visitors_flabel;
                   pattern = _visitors_fpattern;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_pat env _visitors_flabel _visitors_fpattern
                   _visitors_fis_pun _visitors_floc_

        method visit_Constr_pat_arg : _ -> pattern -> argument_kind -> S.t =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_pattern env _visitors_fpat in
              let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
              self#visit_inline_record env "Constr_pat_arg"
                [ ("pat", _visitors_r0); ("kind", _visitors_r1) ]

        method visit_constr_pat_arg : _ -> constr_pat_arg -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Constr_pat_arg { pat = _visitors_fpat; kind = _visitors_fkind }
               ->
                 self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind

        method visit_Map_pat_elem :
            _ -> constant -> pattern -> bool -> location -> location -> S.t =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fpat ->
              fun _visitors_fmatch_absent ->
               fun _visitors_fkey_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_constant env _visitors_fkey in
                 let _visitors_r1 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r2 =
                   self#visit_bool env _visitors_fmatch_absent
                 in
                 let _visitors_r3 =
                   self#visit_location env _visitors_fkey_loc_
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Map_pat_elem"
                   [
                     ("key", _visitors_r0);
                     ("pat", _visitors_r1);
                     ("match_absent", _visitors_r2);
                     ("key_loc_", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_map_pat_elem : _ -> map_pat_elem -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_pat_elem
                 {
                   key = _visitors_fkey;
                   pat = _visitors_fpat;
                   match_absent = _visitors_fmatch_absent;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_pat_elem env _visitors_fkey _visitors_fpat
                   _visitors_fmatch_absent _visitors_fkey_loc_ _visitors_floc_

        method visit_constr_param : _ -> constr_param -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.cparam_typ in
             let _visitors_r1 = self#visit_bool env _visitors_this.cparam_mut in
             let _visitors_r2 =
               self#visit_option self#visit_label env
                 _visitors_this.cparam_label
             in
             self#visit_record env
               [
                 ("cparam_typ", _visitors_r0);
                 ("cparam_mut", _visitors_r1);
                 ("cparam_label", _visitors_r2);
               ]

        method visit_constr_decl : _ -> constr_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_constr_name env _visitors_this.constr_name
             in
             let _visitors_r1 =
               self#visit_option
                 (self#visit_list self#visit_constr_param)
                 env _visitors_this.constr_args
             in
             let _visitors_r2 =
               self#visit_option
                 (fun env ->
                   fun (_visitors_c0, _visitors_c1) ->
                    let _visitors_r0 = self#visit_string env _visitors_c0 in
                    let _visitors_r1 = self#visit_location env _visitors_c1 in
                    self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                 env _visitors_this.constr_tag
             in
             let _visitors_r3 =
               self#visit_location env _visitors_this.constr_loc_
             in
             self#visit_record env
               [
                 ("constr_name", _visitors_r0);
                 ("constr_args", _visitors_r1);
                 ("constr_tag", _visitors_r2);
                 ("constr_loc_", _visitors_r3);
               ]

        method visit_field_name : _ -> field_name -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_string env _visitors_this.label in
             let _visitors_r1 = self#visit_location env _visitors_this.loc_ in
             self#visit_record env
               [ ("label", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_field_decl : _ -> field_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_field_name env _visitors_this.field_name
             in
             let _visitors_r1 = self#visit_typ env _visitors_this.field_ty in
             let _visitors_r2 = self#visit_bool env _visitors_this.field_mut in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.field_vis
             in
             let _visitors_r4 =
               self#visit_location env _visitors_this.field_loc_
             in
             self#visit_record env
               [
                 ("field_name", _visitors_r0);
                 ("field_ty", _visitors_r1);
                 ("field_mut", _visitors_r2);
                 ("field_vis", _visitors_r3);
                 ("field_loc_", _visitors_r4);
               ]

        method visit_No_payload : _ -> S.t =
          fun env -> self#visit_inline_tuple env "No_payload" []

        method visit_Single_payload : _ -> typ -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             self#visit_inline_tuple env "Single_payload" [ _visitors_r0 ]

        method visit_Enum_payload : _ -> constr_decl list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_constr_decl env _visitors_c0
             in
             self#visit_inline_tuple env "Enum_payload" [ _visitors_r0 ]

        method visit_exception_decl : _ -> exception_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | No_payload -> self#visit_No_payload env
             | Single_payload _visitors_c0 ->
                 self#visit_Single_payload env _visitors_c0
             | Enum_payload _visitors_c0 ->
                 self#visit_Enum_payload env _visitors_c0

        method visit_Ptd_abstract : _ -> S.t =
          fun env -> self#visit_inline_tuple env "Ptd_abstract" []

        method visit_Ptd_extern : _ -> S.t =
          fun env -> self#visit_inline_tuple env "Ptd_extern" []

        method visit_Ptd_newtype : _ -> typ -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             self#visit_inline_tuple env "Ptd_newtype" [ _visitors_r0 ]

        method visit_Ptd_error : _ -> exception_decl -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
             self#visit_inline_tuple env "Ptd_error" [ _visitors_r0 ]

        method visit_Ptd_variant : _ -> constr_decl list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_constr_decl env _visitors_c0
             in
             self#visit_inline_tuple env "Ptd_variant" [ _visitors_r0 ]

        method visit_Ptd_record : _ -> field_decl list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_field_decl env _visitors_c0
             in
             self#visit_inline_tuple env "Ptd_record" [ _visitors_r0 ]

        method visit_Ptd_alias : _ -> typ -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             self#visit_inline_tuple env "Ptd_alias" [ _visitors_r0 ]

        method visit_type_desc : _ -> type_desc -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptd_abstract -> self#visit_Ptd_abstract env
             | Ptd_extern -> self#visit_Ptd_extern env
             | Ptd_newtype _visitors_c0 ->
                 self#visit_Ptd_newtype env _visitors_c0
             | Ptd_error _visitors_c0 -> self#visit_Ptd_error env _visitors_c0
             | Ptd_variant _visitors_c0 ->
                 self#visit_Ptd_variant env _visitors_c0
             | Ptd_record _visitors_c0 -> self#visit_Ptd_record env _visitors_c0
             | Ptd_alias _visitors_c0 -> self#visit_Ptd_alias env _visitors_c0

        method visit_type_decl : _ -> type_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_string env _visitors_this.tycon in
             let _visitors_r1 =
               self#visit_location env _visitors_this.tycon_loc_
             in
             let _visitors_r2 =
               self#visit_list self#visit_type_decl_binder env
                 _visitors_this.params
             in
             let _visitors_r3 =
               self#visit_type_desc env _visitors_this.components
             in
             let _visitors_r4 =
               self#visit_list self#visit_attribute env _visitors_this.attrs
             in
             let _visitors_r5 = self#visit_docstring env _visitors_this.doc_ in
             let _visitors_r6 =
               self#visit_visibility env _visitors_this.type_vis
             in
             let _visitors_r7 =
               self#visit_list self#visit_deriving_directive env
                 _visitors_this.deriving_
             in
             let _visitors_r8 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             self#visit_record env
               [
                 ("tycon", _visitors_r0);
                 ("tycon_loc_", _visitors_r1);
                 ("params", _visitors_r2);
                 ("components", _visitors_r3);
                 ("attrs", _visitors_r4);
                 ("doc_", _visitors_r5);
                 ("type_vis", _visitors_r6);
                 ("deriving_", _visitors_r7);
                 ("loc_", _visitors_r8);
               ]

        method visit_local_type_decl : _ -> local_type_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_string env _visitors_this.local_tycon
             in
             let _visitors_r1 =
               self#visit_location env _visitors_this.local_tycon_loc_
             in
             let _visitors_r2 =
               self#visit_type_desc env _visitors_this.local_components
             in
             let _visitors_r3 =
               self#visit_list self#visit_deriving_directive env
                 _visitors_this.deriving_
             in
             self#visit_record env
               [
                 ("local_tycon", _visitors_r0);
                 ("local_tycon_loc_", _visitors_r1);
                 ("local_components", _visitors_r2);
                 ("deriving_", _visitors_r3);
               ]

        method visit_deriving_directive : _ -> deriving_directive -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_type_name env _visitors_this.type_name_
             in
             let _visitors_r1 =
               self#visit_list self#visit_argument env _visitors_this.args
             in
             let _visitors_r2 = self#visit_location env _visitors_this.loc_ in
             self#visit_record env
               [
                 ("type_name_", _visitors_r0);
                 ("args", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

        method visit_Vis_default : _ -> S.t =
          fun env -> self#visit_inline_tuple env "Vis_default" []

        method visit_Vis_pub : _ -> string option -> location -> S.t =
          fun env ->
            fun _visitors_fattr ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_option self#visit_string env _visitors_fattr
              in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Vis_pub"
                [ ("attr", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Vis_priv : _ -> location -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Vis_priv" [ ("loc_", _visitors_r0) ]

        method visit_visibility : _ -> visibility -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Vis_default -> self#visit_Vis_default env
             | Vis_pub { attr = _visitors_fattr; loc_ = _visitors_floc_ } ->
                 self#visit_Vis_pub env _visitors_fattr _visitors_floc_
             | Vis_priv { loc_ = _visitors_floc_ } ->
                 self#visit_Vis_priv env _visitors_floc_

        method visit_Import : _ -> string_literal -> string_literal -> S.t =
          fun env ->
            fun _visitors_fmodule_name ->
             fun _visitors_ffunc_name ->
              let _visitors_r0 =
                self#visit_string_literal env _visitors_fmodule_name
              in
              let _visitors_r1 =
                self#visit_string_literal env _visitors_ffunc_name
              in
              self#visit_inline_record env "Import"
                [ ("module_name", _visitors_r0); ("func_name", _visitors_r1) ]

        method visit_Embedded :
            _ -> string_literal option -> embedded_code -> S.t =
          fun env ->
            fun _visitors_flanguage ->
             fun _visitors_fcode ->
              let _visitors_r0 =
                self#visit_option self#visit_string_literal env
                  _visitors_flanguage
              in
              let _visitors_r1 = self#visit_embedded_code env _visitors_fcode in
              self#visit_inline_record env "Embedded"
                [ ("language", _visitors_r0); ("code", _visitors_r1) ]

        method visit_func_stubs : _ -> func_stubs -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Import
                 {
                   module_name = _visitors_fmodule_name;
                   func_name = _visitors_ffunc_name;
                 } ->
                 self#visit_Import env _visitors_fmodule_name
                   _visitors_ffunc_name
             | Embedded
                 { language = _visitors_flanguage; code = _visitors_fcode } ->
                 self#visit_Embedded env _visitors_flanguage _visitors_fcode

        method visit_Code_string : _ -> string_literal -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             self#visit_inline_tuple env "Code_string" [ _visitors_r0 ]

        method visit_Code_multiline_string : _ -> string list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_string env _visitors_c0
             in
             self#visit_inline_tuple env "Code_multiline_string"
               [ _visitors_r0 ]

        method visit_embedded_code : _ -> embedded_code -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Code_string _visitors_c0 ->
                 self#visit_Code_string env _visitors_c0
             | Code_multiline_string _visitors_c0 ->
                 self#visit_Code_multiline_string env _visitors_c0

        method visit_Decl_body : _ -> local_type_decl list -> expr -> S.t =
          fun env ->
            fun _visitors_flocal_types ->
             fun _visitors_fexpr ->
              let _visitors_r0 =
                self#visit_list self#visit_local_type_decl env
                  _visitors_flocal_types
              in
              let _visitors_r1 = self#visit_expr env _visitors_fexpr in
              self#visit_inline_record env "Decl_body"
                [ ("local_types", _visitors_r0); ("expr", _visitors_r1) ]

        method visit_Decl_stubs : _ -> func_stubs -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
             self#visit_inline_tuple env "Decl_stubs" [ _visitors_r0 ]

        method visit_decl_body : _ -> decl_body -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Decl_body
                 {
                   local_types = _visitors_flocal_types;
                   expr = _visitors_fexpr;
                 } ->
                 self#visit_Decl_body env _visitors_flocal_types _visitors_fexpr
             | Decl_stubs _visitors_c0 -> self#visit_Decl_stubs env _visitors_c0

        method visit_fun_decl : _ -> fun_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_option self#visit_type_name env
                 _visitors_this.type_name
             in
             let _visitors_r1 = self#visit_binder env _visitors_this.name in
             let _visitors_r2 =
               self#visit_option self#visit_location env
                 _visitors_this.has_error
             in
             let _visitors_r3 = self#visit_bool env _visitors_this.is_async in
             let _visitors_r4 =
               self#visit_option self#visit_parameters env
                 _visitors_this.decl_params
             in
             let _visitors_r5 =
               self#visit_location env _visitors_this.params_loc_
             in
             let _visitors_r6 =
               self#visit_list self#visit_tvar_binder env
                 _visitors_this.quantifiers
             in
             let _visitors_r7 =
               self#visit_option
                 (fun env ->
                   fun (_visitors_c0, _visitors_c1) ->
                    let _visitors_r0 = self#visit_typ env _visitors_c0 in
                    let _visitors_r1 = self#visit_error_typ env _visitors_c1 in
                    self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                 env _visitors_this.return_type
             in
             let _visitors_r8 = self#visit_visibility env _visitors_this.vis in
             let _visitors_r9 =
               self#visit_list self#visit_attribute env _visitors_this.attrs
             in
             let _visitors_r10 = self#visit_docstring env _visitors_this.doc_ in
             self#visit_record env
               [
                 ("type_name", _visitors_r0);
                 ("name", _visitors_r1);
                 ("has_error", _visitors_r2);
                 ("is_async", _visitors_r3);
                 ("decl_params", _visitors_r4);
                 ("params_loc_", _visitors_r5);
                 ("quantifiers", _visitors_r6);
                 ("return_type", _visitors_r7);
                 ("vis", _visitors_r8);
                 ("attrs", _visitors_r9);
                 ("doc_", _visitors_r10);
               ]

        method visit_trait_method_param : _ -> trait_method_param -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.tmparam_typ in
             let _visitors_r1 =
               self#visit_option self#visit_label env
                 _visitors_this.tmparam_label
             in
             self#visit_record env
               [
                 ("tmparam_typ", _visitors_r0); ("tmparam_label", _visitors_r1);
               ]

        method visit_Trait_method :
            _ ->
            binder ->
            bool ->
            tvar_binder list ->
            trait_method_param list ->
            (typ * error_typ) option ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_fhas_error ->
              fun _visitors_fquantifiers ->
               fun _visitors_fparams ->
                fun _visitors_freturn_type ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 = self#visit_bool env _visitors_fhas_error in
                  let _visitors_r2 =
                    self#visit_list self#visit_tvar_binder env
                      _visitors_fquantifiers
                  in
                  let _visitors_r3 =
                    self#visit_list self#visit_trait_method_param env
                      _visitors_fparams
                  in
                  let _visitors_r4 =
                    self#visit_option
                      (fun env ->
                        fun (_visitors_c0, _visitors_c1) ->
                         let _visitors_r0 = self#visit_typ env _visitors_c0 in
                         let _visitors_r1 =
                           self#visit_error_typ env _visitors_c1
                         in
                         self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                      env _visitors_freturn_type
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Trait_method"
                    [
                      ("name", _visitors_r0);
                      ("has_error", _visitors_r1);
                      ("quantifiers", _visitors_r2);
                      ("params", _visitors_r3);
                      ("return_type", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_trait_method_decl : _ -> trait_method_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Trait_method
                 {
                   name = _visitors_fname;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   return_type = _visitors_freturn_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Trait_method env _visitors_fname
                   _visitors_fhas_error _visitors_fquantifiers _visitors_fparams
                   _visitors_freturn_type _visitors_floc_

        method visit_trait_decl : _ -> trait_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_binder env _visitors_this.trait_name
             in
             let _visitors_r1 =
               self#visit_list self#visit_tvar_constraint env
                 _visitors_this.trait_supers
             in
             let _visitors_r2 =
               self#visit_list self#visit_trait_method_decl env
                 _visitors_this.trait_methods
             in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.trait_vis
             in
             let _visitors_r4 =
               self#visit_absolute_loc env _visitors_this.trait_loc_
             in
             let _visitors_r5 =
               self#visit_list self#visit_attribute env
                 _visitors_this.trait_attrs
             in
             let _visitors_r6 =
               self#visit_docstring env _visitors_this.trait_doc_
             in
             self#visit_record env
               [
                 ("trait_name", _visitors_r0);
                 ("trait_supers", _visitors_r1);
                 ("trait_methods", _visitors_r2);
                 ("trait_vis", _visitors_r3);
                 ("trait_loc_", _visitors_r4);
                 ("trait_attrs", _visitors_r5);
                 ("trait_doc_", _visitors_r6);
               ]

        method visit_Ptop_expr :
            _ -> expr -> bool -> local_type_decl list -> absolute_loc -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_flocal_types ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 = self#visit_bool env _visitors_fis_main in
                let _visitors_r2 =
                  self#visit_list self#visit_local_type_decl env
                    _visitors_flocal_types
                in
                let _visitors_r3 =
                  self#visit_absolute_loc env _visitors_floc_
                in
                self#visit_inline_record env "Ptop_expr"
                  [
                    ("expr", _visitors_r0);
                    ("is_main", _visitors_r1);
                    ("local_types", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Ptop_test :
            _ ->
            expr ->
            test_name ->
            parameters option ->
            local_type_decl list ->
            absolute_loc ->
            attribute list ->
            docstring ->
            S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fname ->
              fun _visitors_fparams ->
               fun _visitors_flocal_types ->
                fun _visitors_floc_ ->
                 fun _visitors_fattrs ->
                  fun _visitors_fdoc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                   let _visitors_r1 =
                     self#visit_test_name env _visitors_fname
                   in
                   let _visitors_r2 =
                     self#visit_option self#visit_parameters env
                       _visitors_fparams
                   in
                   let _visitors_r3 =
                     self#visit_list self#visit_local_type_decl env
                       _visitors_flocal_types
                   in
                   let _visitors_r4 =
                     self#visit_absolute_loc env _visitors_floc_
                   in
                   let _visitors_r5 =
                     self#visit_list self#visit_attribute env _visitors_fattrs
                   in
                   let _visitors_r6 =
                     self#visit_docstring env _visitors_fdoc_
                   in
                   self#visit_inline_record env "Ptop_test"
                     [
                       ("expr", _visitors_r0);
                       ("name", _visitors_r1);
                       ("params", _visitors_r2);
                       ("local_types", _visitors_r3);
                       ("loc_", _visitors_r4);
                       ("attrs", _visitors_r5);
                       ("doc_", _visitors_r6);
                     ]

        method visit_Ptop_typedef : _ -> type_decl -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_type_decl env _visitors_c0 in
             self#visit_inline_tuple env "Ptop_typedef" [ _visitors_r0 ]

        method visit_Ptop_funcdef :
            _ -> fun_decl -> decl_body -> absolute_loc -> S.t =
          fun env ->
            fun _visitors_ffun_decl ->
             fun _visitors_fdecl_body ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
               let _visitors_r1 =
                 self#visit_decl_body env _visitors_fdecl_body
               in
               let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
               self#visit_inline_record env "Ptop_funcdef"
                 [
                   ("fun_decl", _visitors_r0);
                   ("decl_body", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Ptop_letdef :
            _ ->
            binder ->
            typ option ->
            expr ->
            visibility ->
            bool ->
            absolute_loc ->
            attribute list ->
            docstring ->
            S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fvis ->
                fun _visitors_fis_constant ->
                 fun _visitors_floc_ ->
                  fun _visitors_fattrs ->
                   fun _visitors_fdoc_ ->
                    let _visitors_r0 =
                      self#visit_binder env _visitors_fbinder
                    in
                    let _visitors_r1 =
                      self#visit_option self#visit_typ env _visitors_fty
                    in
                    let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                    let _visitors_r3 =
                      self#visit_visibility env _visitors_fvis
                    in
                    let _visitors_r4 =
                      self#visit_bool env _visitors_fis_constant
                    in
                    let _visitors_r5 =
                      self#visit_absolute_loc env _visitors_floc_
                    in
                    let _visitors_r6 =
                      self#visit_list self#visit_attribute env _visitors_fattrs
                    in
                    let _visitors_r7 =
                      self#visit_docstring env _visitors_fdoc_
                    in
                    self#visit_inline_record env "Ptop_letdef"
                      [
                        ("binder", _visitors_r0);
                        ("ty", _visitors_r1);
                        ("expr", _visitors_r2);
                        ("vis", _visitors_r3);
                        ("is_constant", _visitors_r4);
                        ("loc_", _visitors_r5);
                        ("attrs", _visitors_r6);
                        ("doc_", _visitors_r7);
                      ]

        method visit_Ptop_trait : _ -> trait_decl -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_trait_decl env _visitors_c0 in
             self#visit_inline_tuple env "Ptop_trait" [ _visitors_r0 ]

        method visit_Ptop_trait_alias :
            _ ->
            binder ->
            type_name ->
            visibility ->
            absolute_loc ->
            attribute list ->
            docstring ->
            S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ftarget ->
              fun _visitors_fvis ->
               fun _visitors_floc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fdoc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_type_name env _visitors_ftarget
                  in
                  let _visitors_r2 = self#visit_visibility env _visitors_fvis in
                  let _visitors_r3 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  let _visitors_r4 =
                    self#visit_list self#visit_attribute env _visitors_fattrs
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  self#visit_inline_record env "Ptop_trait_alias"
                    [
                      ("binder", _visitors_r0);
                      ("target", _visitors_r1);
                      ("vis", _visitors_r2);
                      ("loc_", _visitors_r3);
                      ("attrs", _visitors_r4);
                      ("doc_", _visitors_r5);
                    ]

        method visit_Ptop_impl :
            _ ->
            typ option ->
            type_name ->
            binder ->
            bool ->
            tvar_binder list ->
            parameters ->
            (typ * error_typ) option ->
            decl_body ->
            visibility ->
            absolute_loc ->
            location ->
            attribute list ->
            docstring ->
            S.t =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fmethod_name ->
               fun _visitors_fhas_error ->
                fun _visitors_fquantifiers ->
                 fun _visitors_fparams ->
                  fun _visitors_fret_ty ->
                   fun _visitors_fbody ->
                    fun _visitors_fvis ->
                     fun _visitors_floc_ ->
                      fun _visitors_fheader_loc_ ->
                       fun _visitors_fattrs ->
                        fun _visitors_fdoc_ ->
                         let _visitors_r0 =
                           self#visit_option self#visit_typ env
                             _visitors_fself_ty
                         in
                         let _visitors_r1 =
                           self#visit_type_name env _visitors_ftrait
                         in
                         let _visitors_r2 =
                           self#visit_binder env _visitors_fmethod_name
                         in
                         let _visitors_r3 =
                           self#visit_bool env _visitors_fhas_error
                         in
                         let _visitors_r4 =
                           self#visit_list self#visit_tvar_binder env
                             _visitors_fquantifiers
                         in
                         let _visitors_r5 =
                           self#visit_parameters env _visitors_fparams
                         in
                         let _visitors_r6 =
                           self#visit_option
                             (fun env ->
                               fun (_visitors_c0, _visitors_c1) ->
                                let _visitors_r0 =
                                  self#visit_typ env _visitors_c0
                                in
                                let _visitors_r1 =
                                  self#visit_error_typ env _visitors_c1
                                in
                                self#visit_tuple env
                                  [ _visitors_r0; _visitors_r1 ])
                             env _visitors_fret_ty
                         in
                         let _visitors_r7 =
                           self#visit_decl_body env _visitors_fbody
                         in
                         let _visitors_r8 =
                           self#visit_visibility env _visitors_fvis
                         in
                         let _visitors_r9 =
                           self#visit_absolute_loc env _visitors_floc_
                         in
                         let _visitors_r10 =
                           self#visit_location env _visitors_fheader_loc_
                         in
                         let _visitors_r11 =
                           self#visit_list self#visit_attribute env
                             _visitors_fattrs
                         in
                         let _visitors_r12 =
                           self#visit_docstring env _visitors_fdoc_
                         in
                         self#visit_inline_record env "Ptop_impl"
                           [
                             ("self_ty", _visitors_r0);
                             ("trait", _visitors_r1);
                             ("method_name", _visitors_r2);
                             ("has_error", _visitors_r3);
                             ("quantifiers", _visitors_r4);
                             ("params", _visitors_r5);
                             ("ret_ty", _visitors_r6);
                             ("body", _visitors_r7);
                             ("vis", _visitors_r8);
                             ("loc_", _visitors_r9);
                             ("header_loc_", _visitors_r10);
                             ("attrs", _visitors_r11);
                             ("doc_", _visitors_r12);
                           ]

        method visit_Ptop_impl_relation :
            _ ->
            typ ->
            type_name ->
            tvar_binder list ->
            bool ->
            absolute_loc ->
            S.t =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fquantifiers ->
               fun _visitors_fis_pub ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
                 let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
                 let _visitors_r2 =
                   self#visit_list self#visit_tvar_binder env
                     _visitors_fquantifiers
                 in
                 let _visitors_r3 = self#visit_bool env _visitors_fis_pub in
                 let _visitors_r4 =
                   self#visit_absolute_loc env _visitors_floc_
                 in
                 self#visit_inline_record env "Ptop_impl_relation"
                   [
                     ("self_ty", _visitors_r0);
                     ("trait", _visitors_r1);
                     ("quantifiers", _visitors_r2);
                     ("is_pub", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_impl : _ -> impl -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_flocal_types _visitors_floc_
             | Ptop_test
                 {
                   expr = _visitors_fexpr;
                   name = _visitors_fname;
                   params = _visitors_fparams;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_test env _visitors_fexpr _visitors_fname
                   _visitors_fparams _visitors_flocal_types _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_typedef _visitors_c0 ->
                 self#visit_Ptop_typedef env _visitors_c0
             | Ptop_funcdef
                 {
                   fun_decl = _visitors_ffun_decl;
                   decl_body = _visitors_fdecl_body;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_funcdef env _visitors_ffun_decl
                   _visitors_fdecl_body _visitors_floc_
             | Ptop_letdef
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   vis = _visitors_fvis;
                   is_constant = _visitors_fis_constant;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_letdef env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fvis _visitors_fis_constant
                   _visitors_floc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_trait _visitors_c0 -> self#visit_Ptop_trait env _visitors_c0
             | Ptop_trait_alias
                 {
                   binder = _visitors_fbinder;
                   target = _visitors_ftarget;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_trait_alias env _visitors_fbinder
                   _visitors_ftarget _visitors_fvis _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_impl
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   method_name = _visitors_fmethod_name;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   ret_ty = _visitors_fret_ty;
                   body = _visitors_fbody;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   header_loc_ = _visitors_fheader_loc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_impl env _visitors_fself_ty _visitors_ftrait
                   _visitors_fmethod_name _visitors_fhas_error
                   _visitors_fquantifiers _visitors_fparams _visitors_fret_ty
                   _visitors_fbody _visitors_fvis _visitors_floc_
                   _visitors_fheader_loc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_impl_relation
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   quantifiers = _visitors_fquantifiers;
                   is_pub = _visitors_fis_pub;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_impl_relation env _visitors_fself_ty
                   _visitors_ftrait _visitors_fquantifiers _visitors_fis_pub
                   _visitors_floc_

        method visit_Interp_lit : _ -> string -> string -> location -> S.t =
          fun env ->
            fun _visitors_fstr ->
             fun _visitors_frepr ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_string env _visitors_fstr in
               let _visitors_r1 = self#visit_string env _visitors_frepr in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Interp_lit"
                 [
                   ("str", _visitors_r0);
                   ("repr", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Interp_expr : _ -> expr -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Interp_expr"
                [ ("expr", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Interp_source : _ -> Literal.interp_source -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_interp_source env _visitors_c0 in
             self#visit_inline_tuple env "Interp_source" [ _visitors_r0 ]

        method visit_interp_elem : _ -> interp_elem -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Interp_lit
                 {
                   str = _visitors_fstr;
                   repr = _visitors_frepr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Interp_lit env _visitors_fstr _visitors_frepr
                   _visitors_floc_
             | Interp_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Interp_expr env _visitors_fexpr _visitors_floc_
             | Interp_source _visitors_c0 ->
                 self#visit_Interp_source env _visitors_c0

        method visit_Multiline_string : _ -> string -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string env _visitors_c0 in
             self#visit_inline_tuple env "Multiline_string" [ _visitors_r0 ]

        method visit_Multiline_interp : _ -> interp_elem list -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               self#visit_list self#visit_interp_elem env _visitors_c0
             in
             self#visit_inline_tuple env "Multiline_interp" [ _visitors_r0 ]

        method visit_multiline_string_elem : _ -> multiline_string_elem -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Multiline_string _visitors_c0 ->
                 self#visit_Multiline_string env _visitors_c0
             | Multiline_interp _visitors_c0 ->
                 self#visit_Multiline_interp env _visitors_c0

        method visit_impls : _ -> impls -> S.t =
          fun env -> self#visit_list self#visit_impl env
      end

    [@@@VISITORS.END]
  end

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] iter =
      object (self : 'self)
        inherit [_] iterbase

        method visit_Pexpr_apply :
            _ -> expr -> argument list -> apply_attr -> location -> unit =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fattr ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_ffunc in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_argument env))
                    _visitors_fargs
                in
                let _visitors_r2 = (fun _visitors_this -> ()) _visitors_fattr in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_infix : _ -> var -> expr -> expr -> location -> unit
            =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_flhs ->
              fun _visitors_frhs ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fop in
                let _visitors_r1 = self#visit_expr env _visitors_flhs in
                let _visitors_r2 = self#visit_expr env _visitors_frhs in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_unary : _ -> var -> expr -> location -> unit =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_fexpr ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fop in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_array : _ -> expr list -> location -> unit =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_array_spread :
            _ -> spreadable_elem list -> location -> unit =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_spreadable_elem env))
                  _visitors_felems
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_array_get : _ -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_farray in
               let _visitors_r1 = self#visit_expr env _visitors_findex in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_array_get_slice :
            _ ->
            expr ->
            expr option ->
            expr option ->
            location ->
            location ->
            unit =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_fstart_index ->
              fun _visitors_fend_index ->
               fun _visitors_findex_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_farray in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_fstart_index
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_fend_index
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_findex_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_array_set :
            _ -> expr -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_fvalue ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_farray in
                let _visitors_r1 = self#visit_expr env _visitors_findex in
                let _visitors_r2 = self#visit_expr env _visitors_fvalue in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_array_augmented_set :
            _ -> var -> expr -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_farray ->
              fun _visitors_findex ->
               fun _visitors_fvalue ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fop in
                 let _visitors_r1 = self#visit_expr env _visitors_farray in
                 let _visitors_r2 = self#visit_expr env _visitors_findex in
                 let _visitors_r3 = self#visit_expr env _visitors_fvalue in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_constant : _ -> constant -> location -> unit =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fc in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_multiline_string :
            _ -> multiline_string_elem list -> location -> unit =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_multiline_string_elem env))
                  _visitors_felems
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_interp : _ -> interp_elem list -> location -> unit =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_interp_elem env))
                  _visitors_felems
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_constraint : _ -> expr -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_constr : _ -> constructor -> location -> unit =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constructor env _visitors_fconstr in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_while :
            _ -> expr -> expr -> expr option -> label option -> location -> unit
            =
          fun env ->
            fun _visitors_floop_cond ->
             fun _visitors_floop_body ->
              fun _visitors_fwhile_else ->
               fun _visitors_flabel ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                 let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_fwhile_else
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_label env) t
                     | None -> ())
                     _visitors_flabel
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_function : _ -> func -> location -> unit =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_func env _visitors_ffunc in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_ident : _ -> var -> location -> unit =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_var env _visitors_fid in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_if :
            _ -> expr -> expr -> expr option -> location -> unit =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 = self#visit_expr env _visitors_fifso in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_expr env) t
                    | None -> ())
                    _visitors_fifnot
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_guard :
            _ -> expr -> expr option -> expr -> location -> unit =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_expr env) t
                    | None -> ())
                    _visitors_fotherwise
                in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_guard_let :
            _ -> pattern -> expr -> case list option -> expr -> location -> unit
            =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fexpr ->
              fun _visitors_fotherwise ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t ->
                         (fun _visitors_this ->
                           Basic_lst.iter _visitors_this
                             ~f:(self#visit_case env))
                           t
                     | None -> ())
                     _visitors_fotherwise
                 in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_is : _ -> expr -> pattern -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fpat ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_letfn :
            _ -> binder -> func -> expr -> location -> unit =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffunc ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fname in
                let _visitors_r1 = self#visit_func env _visitors_ffunc in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_letrec :
            _ -> (binder * func) list -> expr -> location -> unit =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this
                     ~f:(fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_binder env _visitors_c0 in
                       let _visitors_r1 = self#visit_func env _visitors_c1 in
                       ()))
                   _visitors_fbindings
               in
               let _visitors_r1 = self#visit_expr env _visitors_fbody in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_let :
            _ -> pattern -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_fpattern ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_sequence : _ -> expr list -> expr -> location -> unit
            =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_tuple : _ -> expr list -> location -> unit =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_record :
            _ ->
            type_name option ->
            field_def list ->
            trailing_mark ->
            location ->
            unit =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_ffields ->
              fun _visitors_ftrailing ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_type_name env) t
                    | None -> ())
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_ftrailing
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_record_update :
            _ -> type_name option -> expr -> field_def list -> location -> unit
            =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_frecord ->
              fun _visitors_ffields ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_type_name env) t
                    | None -> ())
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_field : _ -> expr -> accessor -> location -> unit =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_method : _ -> type_name -> label -> location -> unit
            =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_fmethod_name ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_type_name env _visitors_ftype_name
               in
               let _visitors_r1 = self#visit_label env _visitors_fmethod_name in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_dot_apply :
            _ ->
            expr ->
            label ->
            argument list ->
            bool ->
            apply_attr ->
            location ->
            unit =
          fun env ->
            fun _visitors_fself ->
             fun _visitors_fmethod_name ->
              fun _visitors_fargs ->
               fun _visitors_freturn_self ->
                fun _visitors_fattr ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_fself in
                  let _visitors_r1 =
                    self#visit_label env _visitors_fmethod_name
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_argument env))
                      _visitors_fargs
                  in
                  let _visitors_r3 =
                    (fun _visitors_this -> ()) _visitors_freturn_self
                  in
                  let _visitors_r4 =
                    (fun _visitors_this -> ()) _visitors_fattr
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> ()) _visitors_floc_
                  in
                  ()

        method visit_Pexpr_as : _ -> expr -> type_name -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_mutate :
            _ -> expr -> accessor -> expr -> var option -> location -> unit =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_ffield ->
               fun _visitors_faugmented_by ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_var env) t
                     | None -> ())
                     _visitors_faugmented_by
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_match :
            _ -> expr -> case list -> location -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fcases ->
              fun _visitors_fmatch_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_case env))
                    _visitors_fcases
                in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fmatch_loc_
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_letmut :
            _ -> binder -> typ option -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_typ env) t
                     | None -> ())
                     _visitors_fty
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_pipe : _ -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_assign :
            _ -> var -> expr -> var option -> location -> unit =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_faugmented_by ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_var env) t
                    | None -> ())
                    _visitors_faugmented_by
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Pexpr_hole : _ -> location -> hole -> unit =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_fkind ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
              let _visitors_r1 = self#visit_hole env _visitors_fkind in
              ()

        method visit_Pexpr_return : _ -> expr option -> location -> unit =
          fun env ->
            fun _visitors_freturn_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_expr env) t
                  | None -> ())
                  _visitors_freturn_value
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_raise : _ -> expr -> location -> unit =
          fun env ->
            fun _visitors_ferr_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_ferr_value in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_unit : _ -> location -> bool -> unit =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_ffaked ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_ffaked in
              ()

        method visit_Pexpr_break :
            _ -> expr option -> label option -> location -> unit =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_expr env) t
                   | None -> ())
                   _visitors_farg
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_label env) t
                   | None -> ())
                   _visitors_flabel
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_continue :
            _ -> expr list -> label option -> location -> unit =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                   _visitors_fargs
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_label env) t
                   | None -> ())
                   _visitors_flabel
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_loop :
            _ ->
            expr list ->
            multi_arg_case list ->
            label option ->
            location ->
            location ->
            unit =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_fbody ->
              fun _visitors_flabel ->
               fun _visitors_floop_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                     _visitors_fargs
                 in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this
                       ~f:(self#visit_multi_arg_case env))
                     _visitors_fbody
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_label env) t
                     | None -> ())
                     _visitors_flabel
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_floop_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Pexpr_for :
            _ ->
            (binder * expr) list ->
            expr option ->
            (binder * expr) list ->
            expr ->
            expr option ->
            label option ->
            location ->
            unit =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fcondition ->
              fun _visitors_fcontinue_block ->
               fun _visitors_fbody ->
                fun _visitors_ffor_else ->
                 fun _visitors_flabel ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(fun (_visitors_c0, _visitors_c1) ->
                           let _visitors_r0 =
                             self#visit_binder env _visitors_c0
                           in
                           let _visitors_r1 =
                             self#visit_expr env _visitors_c1
                           in
                           ()))
                       _visitors_fbinders
                   in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_expr env) t
                       | None -> ())
                       _visitors_fcondition
                   in
                   let _visitors_r2 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(fun (_visitors_c0, _visitors_c1) ->
                           let _visitors_r0 =
                             self#visit_binder env _visitors_c0
                           in
                           let _visitors_r1 =
                             self#visit_expr env _visitors_c1
                           in
                           ()))
                       _visitors_fcontinue_block
                   in
                   let _visitors_r3 = self#visit_expr env _visitors_fbody in
                   let _visitors_r4 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_expr env) t
                       | None -> ())
                       _visitors_ffor_else
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_label env) t
                       | None -> ())
                       _visitors_flabel
                   in
                   let _visitors_r6 =
                     (fun _visitors_this -> ()) _visitors_floc_
                   in
                   ()

        method visit_Pexpr_foreach :
            _ ->
            binder option list ->
            expr ->
            expr ->
            expr option ->
            label option ->
            location ->
            unit =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_felse_block ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(fun _visitors_this ->
                          match _visitors_this with
                          | Some t -> (self#visit_binder env) t
                          | None -> ()))
                      _visitors_fbinders
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_expr env) t
                      | None -> ())
                      _visitors_felse_block
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_label env) t
                      | None -> ())
                      _visitors_flabel
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> ()) _visitors_floc_
                  in
                  ()

        method visit_Pexpr_try :
            _ ->
            expr ->
            case list ->
            bool ->
            case list option ->
            location ->
            location ->
            location ->
            location ->
            unit =
          fun env ->
            fun _visitors_fbody ->
             fun _visitors_fcatch ->
              fun _visitors_fcatch_all ->
               fun _visitors_ftry_else ->
                fun _visitors_ftry_loc_ ->
                 fun _visitors_fcatch_loc_ ->
                  fun _visitors_felse_loc_ ->
                   fun _visitors_floc_ ->
                    let _visitors_r0 = self#visit_expr env _visitors_fbody in
                    let _visitors_r1 =
                      (fun _visitors_this ->
                        Basic_lst.iter _visitors_this ~f:(self#visit_case env))
                        _visitors_fcatch
                    in
                    let _visitors_r2 =
                      (fun _visitors_this -> ()) _visitors_fcatch_all
                    in
                    let _visitors_r3 =
                      (fun _visitors_this ->
                        match _visitors_this with
                        | Some t ->
                            (fun _visitors_this ->
                              Basic_lst.iter _visitors_this
                                ~f:(self#visit_case env))
                              t
                        | None -> ())
                        _visitors_ftry_else
                    in
                    let _visitors_r4 =
                      (fun _visitors_this -> ()) _visitors_ftry_loc_
                    in
                    let _visitors_r5 =
                      (fun _visitors_this -> ()) _visitors_fcatch_loc_
                    in
                    let _visitors_r6 =
                      (fun _visitors_this -> ()) _visitors_felse_loc_
                    in
                    let _visitors_r7 =
                      (fun _visitors_this -> ()) _visitors_floc_
                    in
                    ()

        method visit_Pexpr_map : _ -> map_expr_elem list -> location -> unit =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_map_expr_elem env))
                  _visitors_felems
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Pexpr_group : _ -> expr -> group -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fgroup ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = (fun _visitors_this -> ()) _visitors_fgroup in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Pexpr_static_assert :
            _ -> static_assertion list -> expr -> unit =
          fun env ->
            fun _visitors_fasserts ->
             fun _visitors_fbody ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_static_assertion env))
                  _visitors_fasserts
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              ()

        method visit_expr : _ -> expr -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fattr _visitors_floc_
             | Pexpr_infix
                 {
                   op = _visitors_fop;
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_infix env _visitors_fop _visitors_flhs
                   _visitors_frhs _visitors_floc_
             | Pexpr_unary
                 {
                   op = _visitors_fop;
                   expr = _visitors_fexpr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_unary env _visitors_fop _visitors_fexpr
                   _visitors_floc_
             | Pexpr_array { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_array env _visitors_fexprs _visitors_floc_
             | Pexpr_array_spread
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_array_spread env _visitors_felems
                   _visitors_floc_
             | Pexpr_array_get
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get env _visitors_farray
                   _visitors_findex _visitors_floc_
             | Pexpr_array_get_slice
                 {
                   array = _visitors_farray;
                   start_index = _visitors_fstart_index;
                   end_index = _visitors_fend_index;
                   index_loc_ = _visitors_findex_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get_slice env _visitors_farray
                   _visitors_fstart_index _visitors_fend_index
                   _visitors_findex_loc_ _visitors_floc_
             | Pexpr_array_set
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_set env _visitors_farray
                   _visitors_findex _visitors_fvalue _visitors_floc_
             | Pexpr_array_augmented_set
                 {
                   op = _visitors_fop;
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_augmented_set env _visitors_fop
                   _visitors_farray _visitors_findex _visitors_fvalue
                   _visitors_floc_
             | Pexpr_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constant env _visitors_fc _visitors_floc_
             | Pexpr_multiline_string
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_multiline_string env _visitors_felems
                   _visitors_floc_
             | Pexpr_interp { elems = _visitors_felems; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_interp env _visitors_felems _visitors_floc_
             | Pexpr_constraint
                 {
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_constraint env _visitors_fexpr _visitors_fty
                   _visitors_floc_
             | Pexpr_constr
                 { constr = _visitors_fconstr; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constr env _visitors_fconstr _visitors_floc_
             | Pexpr_while
                 {
                   loop_cond = _visitors_floop_cond;
                   loop_body = _visitors_floop_body;
                   while_else = _visitors_fwhile_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_while env _visitors_floop_cond
                   _visitors_floop_body _visitors_fwhile_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_function { func = _visitors_ffunc; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_function env _visitors_ffunc _visitors_floc_
             | Pexpr_ident { id = _visitors_fid; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_ident env _visitors_fid _visitors_floc_
             | Pexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_floc_
             | Pexpr_guard
                 {
                   cond = _visitors_fcond;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard env _visitors_fcond _visitors_fotherwise
                   _visitors_fbody _visitors_floc_
             | Pexpr_guard_let
                 {
                   pat = _visitors_fpat;
                   expr = _visitors_fexpr;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard_let env _visitors_fpat _visitors_fexpr
                   _visitors_fotherwise _visitors_fbody _visitors_floc_
             | Pexpr_is
                 {
                   expr = _visitors_fexpr;
                   pat = _visitors_fpat;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_is env _visitors_fexpr _visitors_fpat
                   _visitors_floc_
             | Pexpr_letfn
                 {
                   name = _visitors_fname;
                   func = _visitors_ffunc;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letfn env _visitors_fname _visitors_ffunc
                   _visitors_fbody _visitors_floc_
             | Pexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_floc_
             | Pexpr_let
                 {
                   pattern = _visitors_fpattern;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_let env _visitors_fpattern _visitors_fexpr
                   _visitors_fbody _visitors_floc_
             | Pexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_floc_
             | Pexpr_tuple { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_tuple env _visitors_fexprs _visitors_floc_
             | Pexpr_record
                 {
                   type_name = _visitors_ftype_name;
                   fields = _visitors_ffields;
                   trailing = _visitors_ftrailing;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record env _visitors_ftype_name
                   _visitors_ffields _visitors_ftrailing _visitors_floc_
             | Pexpr_record_update
                 {
                   type_name = _visitors_ftype_name;
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record_update env _visitors_ftype_name
                   _visitors_frecord _visitors_ffields _visitors_floc_
             | Pexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_floc_
             | Pexpr_method
                 {
                   type_name = _visitors_ftype_name;
                   method_name = _visitors_fmethod_name;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_method env _visitors_ftype_name
                   _visitors_fmethod_name _visitors_floc_
             | Pexpr_dot_apply
                 {
                   self = _visitors_fself;
                   method_name = _visitors_fmethod_name;
                   args = _visitors_fargs;
                   return_self = _visitors_freturn_self;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_dot_apply env _visitors_fself
                   _visitors_fmethod_name _visitors_fargs _visitors_freturn_self
                   _visitors_fattr _visitors_floc_
             | Pexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_floc_
             | Pexpr_mutate
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   field = _visitors_ffield;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_mutate env _visitors_frecord
                   _visitors_faccessor _visitors_ffield _visitors_faugmented_by
                   _visitors_floc_
             | Pexpr_match
                 {
                   expr = _visitors_fexpr;
                   cases = _visitors_fcases;
                   match_loc_ = _visitors_fmatch_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_match env _visitors_fexpr _visitors_fcases
                   _visitors_fmatch_loc_ _visitors_floc_
             | Pexpr_letmut
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letmut env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fbody _visitors_floc_
             | Pexpr_pipe
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_pipe env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Pexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_faugmented_by _visitors_floc_
             | Pexpr_hole { loc_ = _visitors_floc_; kind = _visitors_fkind } ->
                 self#visit_Pexpr_hole env _visitors_floc_ _visitors_fkind
             | Pexpr_return
                 {
                   return_value = _visitors_freturn_value;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_return env _visitors_freturn_value
                   _visitors_floc_
             | Pexpr_raise
                 { err_value = _visitors_ferr_value; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_raise env _visitors_ferr_value _visitors_floc_
             | Pexpr_unit { loc_ = _visitors_floc_; faked = _visitors_ffaked }
               ->
                 self#visit_Pexpr_unit env _visitors_floc_ _visitors_ffaked
             | Pexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_break env _visitors_farg _visitors_flabel
                   _visitors_floc_
             | Pexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_floc_
             | Pexpr_loop
                 {
                   args = _visitors_fargs;
                   body = _visitors_fbody;
                   label = _visitors_flabel;
                   loop_loc_ = _visitors_floop_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_loop env _visitors_fargs _visitors_fbody
                   _visitors_flabel _visitors_floop_loc_ _visitors_floc_
             | Pexpr_for
                 {
                   binders = _visitors_fbinders;
                   condition = _visitors_fcondition;
                   continue_block = _visitors_fcontinue_block;
                   body = _visitors_fbody;
                   for_else = _visitors_ffor_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_for env _visitors_fbinders
                   _visitors_fcondition _visitors_fcontinue_block
                   _visitors_fbody _visitors_ffor_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_foreach
                 {
                   binders = _visitors_fbinders;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   else_block = _visitors_felse_block;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_foreach env _visitors_fbinders _visitors_fexpr
                   _visitors_fbody _visitors_felse_block _visitors_flabel
                   _visitors_floc_
             | Pexpr_try
                 {
                   body = _visitors_fbody;
                   catch = _visitors_fcatch;
                   catch_all = _visitors_fcatch_all;
                   try_else = _visitors_ftry_else;
                   try_loc_ = _visitors_ftry_loc_;
                   catch_loc_ = _visitors_fcatch_loc_;
                   else_loc_ = _visitors_felse_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_try env _visitors_fbody _visitors_fcatch
                   _visitors_fcatch_all _visitors_ftry_else _visitors_ftry_loc_
                   _visitors_fcatch_loc_ _visitors_felse_loc_ _visitors_floc_
             | Pexpr_map { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_map env _visitors_felems _visitors_floc_
             | Pexpr_group
                 {
                   expr = _visitors_fexpr;
                   group = _visitors_fgroup;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_group env _visitors_fexpr _visitors_fgroup
                   _visitors_floc_
             | Pexpr_static_assert
                 { asserts = _visitors_fasserts; body = _visitors_fbody } ->
                 self#visit_Pexpr_static_assert env _visitors_fasserts
                   _visitors_fbody

        method visit_static_assertion : _ -> static_assertion -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.assert_type in
             let _visitors_r1 =
               self#visit_longident env _visitors_this.assert_trait
             in
             let _visitors_r2 =
               (fun _visitors_this -> ()) _visitors_this.assert_loc
             in
             let _visitors_r3 =
               (fun _visitors_this -> ()) _visitors_this.assert_msg
             in
             ()

        method visit_argument : _ -> argument -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
             let _visitors_r1 =
               self#visit_argument_kind env _visitors_this.arg_kind
             in
             ()

        method visit_parameters : _ -> parameters -> unit =
          fun env ->
            fun _visitors_this ->
             Basic_lst.iter _visitors_this ~f:(self#visit_parameter env)

        method visit_Discard_positional : _ -> typ option -> location -> unit =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_typ env) t
                  | None -> ())
                  _visitors_fty
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Positional : _ -> binder -> typ option -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_typ env) t
                  | None -> ())
                  _visitors_fty
              in
              ()

        method visit_Labelled : _ -> binder -> typ option -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_typ env) t
                  | None -> ())
                  _visitors_fty
              in
              ()

        method visit_Optional : _ -> binder -> expr -> typ option -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fdefault ->
              fun _visitors_fty ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 = self#visit_expr env _visitors_fdefault in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_typ env) t
                   | None -> ())
                   _visitors_fty
               in
               ()

        method visit_Question_optional : _ -> binder -> typ option -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_typ env) t
                  | None -> ())
                  _visitors_fty
              in
              ()

        method visit_parameter : _ -> parameter -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Discard_positional { ty = _visitors_fty; loc_ = _visitors_floc_ }
               ->
                 self#visit_Discard_positional env _visitors_fty _visitors_floc_
             | Positional { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Positional env _visitors_fbinder _visitors_fty
             | Labelled { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Labelled env _visitors_fbinder _visitors_fty
             | Optional
                 {
                   binder = _visitors_fbinder;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                 } ->
                 self#visit_Optional env _visitors_fbinder _visitors_fdefault
                   _visitors_fty
             | Question_optional
                 { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Question_optional env _visitors_fbinder
                   _visitors_fty

        method visit_Lambda :
            _ ->
            parameters ->
            location ->
            expr ->
            (typ * error_typ) option ->
            fn_kind ->
            location option ->
            bool ->
            unit =
          fun env ->
            fun _visitors_fparameters ->
             fun _visitors_fparams_loc_ ->
              fun _visitors_fbody ->
               fun _visitors_freturn_type ->
                fun _visitors_fkind_ ->
                 fun _visitors_fhas_error ->
                  fun _visitors_fis_async ->
                   let _visitors_r0 =
                     self#visit_parameters env _visitors_fparameters
                   in
                   let _visitors_r1 =
                     (fun _visitors_this -> ()) _visitors_fparams_loc_
                   in
                   let _visitors_r2 = self#visit_expr env _visitors_fbody in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           (fun (_visitors_c0, _visitors_c1) ->
                             let _visitors_r0 =
                               self#visit_typ env _visitors_c0
                             in
                             let _visitors_r1 =
                               self#visit_error_typ env _visitors_c1
                             in
                             ())
                             t
                       | None -> ())
                       _visitors_freturn_type
                   in
                   let _visitors_r4 = self#visit_fn_kind env _visitors_fkind_ in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (fun _visitors_this -> ()) t
                       | None -> ())
                       _visitors_fhas_error
                   in
                   let _visitors_r6 =
                     (fun _visitors_this -> ()) _visitors_fis_async
                   in
                   ()

        method visit_Match :
            _ ->
            multi_arg_case list ->
            location option ->
            bool ->
            location ->
            location ->
            unit =
          fun env ->
            fun _visitors_fcases ->
             fun _visitors_fhas_error ->
              fun _visitors_fis_async ->
               fun _visitors_ffn_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this
                       ~f:(self#visit_multi_arg_case env))
                     _visitors_fcases
                 in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (fun _visitors_this -> ()) t
                     | None -> ())
                     _visitors_fhas_error
                 in
                 let _visitors_r2 =
                   (fun _visitors_this -> ()) _visitors_fis_async
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_ffn_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_func : _ -> func -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Lambda
                 {
                   parameters = _visitors_fparameters;
                   params_loc_ = _visitors_fparams_loc_;
                   body = _visitors_fbody;
                   return_type = _visitors_freturn_type;
                   kind_ = _visitors_fkind_;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                 } ->
                 self#visit_Lambda env _visitors_fparameters
                   _visitors_fparams_loc_ _visitors_fbody _visitors_freturn_type
                   _visitors_fkind_ _visitors_fhas_error _visitors_fis_async
             | Match
                 {
                   cases = _visitors_fcases;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                   fn_loc_ = _visitors_ffn_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Match env _visitors_fcases _visitors_fhas_error
                   _visitors_fis_async _visitors_ffn_loc_ _visitors_floc_

        method visit_case : _ -> case -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_pattern env _visitors_this.pattern in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_expr env) t
                 | None -> ())
                 _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             ()

        method visit_multi_arg_case : _ -> multi_arg_case -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_pattern env))
                 _visitors_this.patterns
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_expr env) t
                 | None -> ())
                 _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             ()

        method visit_Elem_regular : _ -> expr -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_expr env _visitors_c0 in
             ()

        method visit_Elem_spread : _ -> expr -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_spreadable_elem : _ -> spreadable_elem -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Elem_regular _visitors_c0 ->
                 self#visit_Elem_regular env _visitors_c0
             | Elem_spread { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Elem_spread env _visitors_fexpr _visitors_floc_

        method visit_Map_expr_elem :
            _ -> constant -> expr -> location -> location -> unit =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fexpr ->
              fun _visitors_fkey_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fkey in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fkey_loc_
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_map_expr_elem : _ -> map_expr_elem -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_expr_elem
                 {
                   key = _visitors_fkey;
                   expr = _visitors_fexpr;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_expr_elem env _visitors_fkey _visitors_fexpr
                   _visitors_fkey_loc_ _visitors_floc_

        method visit_Error_typ : _ -> typ -> unit =
          fun env ->
            fun _visitors_fty ->
             let _visitors_r0 = self#visit_typ env _visitors_fty in
             ()

        method visit_Default_error_typ : _ -> location -> unit =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
             ()

        method visit_No_error_typ : _ -> unit = fun env -> ()

        method visit_error_typ : _ -> error_typ -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Error_typ { ty = _visitors_fty } ->
                 self#visit_Error_typ env _visitors_fty
             | Default_error_typ { loc_ = _visitors_floc_ } ->
                 self#visit_Default_error_typ env _visitors_floc_
             | No_error_typ -> self#visit_No_error_typ env

        method visit_Ptype_any : _ -> location -> unit =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
             ()

        method visit_Ptype_arrow :
            _ -> typ list -> typ -> error_typ -> bool -> location -> unit =
          fun env ->
            fun _visitors_fty_arg ->
             fun _visitors_fty_res ->
              fun _visitors_fty_err ->
               fun _visitors_fis_async ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                     _visitors_fty_arg
                 in
                 let _visitors_r1 = self#visit_typ env _visitors_fty_res in
                 let _visitors_r2 =
                   self#visit_error_typ env _visitors_fty_err
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_fis_async
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_Ptype_tuple : _ -> typ list -> location -> unit =
          fun env ->
            fun _visitors_ftys ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                  _visitors_ftys
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Ptype_name :
            _ -> constrid_loc -> typ list -> location -> unit =
          fun env ->
            fun _visitors_fconstr_id ->
             fun _visitors_ftys ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr_id
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                   _visitors_ftys
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ptype_option : _ -> typ -> location -> location -> unit =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              fun _visitors_fquestion_loc ->
               let _visitors_r0 = self#visit_typ env _visitors_fty in
               let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
               let _visitors_r2 =
                 (fun _visitors_this -> ()) _visitors_fquestion_loc
               in
               ()

        method visit_Ptype_object : _ -> constrid_loc -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_constrid_loc env _visitors_c0 in
             ()

        method visit_typ : _ -> typ -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptype_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_any env _visitors_floc_
             | Ptype_arrow
                 {
                   ty_arg = _visitors_fty_arg;
                   ty_res = _visitors_fty_res;
                   ty_err = _visitors_fty_err;
                   is_async = _visitors_fis_async;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_arrow env _visitors_fty_arg _visitors_fty_res
                   _visitors_fty_err _visitors_fis_async _visitors_floc_
             | Ptype_tuple { tys = _visitors_ftys; loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_tuple env _visitors_ftys _visitors_floc_
             | Ptype_name
                 {
                   constr_id = _visitors_fconstr_id;
                   tys = _visitors_ftys;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_name env _visitors_fconstr_id _visitors_ftys
                   _visitors_floc_
             | Ptype_option
                 {
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                   question_loc = _visitors_fquestion_loc;
                 } ->
                 self#visit_Ptype_option env _visitors_fty _visitors_floc_
                   _visitors_fquestion_loc
             | Ptype_object _visitors_c0 ->
                 self#visit_Ptype_object env _visitors_c0

        method visit_Ppat_alias : _ -> pattern -> binder -> location -> unit =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_falias ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_binder env _visitors_falias in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ppat_any : _ -> location -> unit =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
             ()

        method visit_Ppat_array : _ -> array_patterns -> location -> unit =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_array_patterns env _visitors_fpats
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Ppat_constant : _ -> constant -> location -> unit =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fc in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Ppat_constraint : _ -> pattern -> typ -> location -> unit =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ppat_constr :
            _ ->
            constructor ->
            constr_pat_arg list option ->
            bool ->
            location ->
            unit =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_fargs ->
              fun _visitors_fis_open ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_constructor env _visitors_fconstr
                in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t ->
                        (fun _visitors_this ->
                          Basic_lst.iter _visitors_this
                            ~f:(self#visit_constr_pat_arg env))
                          t
                    | None -> ())
                    _visitors_fargs
                in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fis_open
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Ppat_or : _ -> pattern -> pattern -> location -> unit =
          fun env ->
            fun _visitors_fpat1 ->
             fun _visitors_fpat2 ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat1 in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat2 in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ppat_tuple : _ -> pattern list -> location -> unit =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_pattern env))
                  _visitors_fpats
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Ppat_var : _ -> binder -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_binder env _visitors_c0 in
             ()

        method visit_Ppat_record :
            _ -> field_pat list -> bool -> location -> unit =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_field_pat env))
                   _visitors_ffields
               in
               let _visitors_r1 =
                 (fun _visitors_this -> ()) _visitors_fis_closed
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ppat_map :
            _ -> map_pat_elem list -> bool -> location -> unit =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this
                     ~f:(self#visit_map_pat_elem env))
                   _visitors_felems
               in
               let _visitors_r1 =
                 (fun _visitors_this -> ()) _visitors_fis_closed
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ppat_range :
            _ -> pattern -> pattern -> bool -> location -> unit =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_finclusive ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_flhs in
                let _visitors_r1 = self#visit_pattern env _visitors_frhs in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_finclusive
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_pattern : _ -> pattern -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ppat_alias
                 {
                   pat = _visitors_fpat;
                   alias = _visitors_falias;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_alias env _visitors_fpat _visitors_falias
                   _visitors_floc_
             | Ppat_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_any env _visitors_floc_
             | Ppat_array { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_array env _visitors_fpats _visitors_floc_
             | Ppat_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_constant env _visitors_fc _visitors_floc_
             | Ppat_constraint
                 {
                   pat = _visitors_fpat;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constraint env _visitors_fpat _visitors_fty
                   _visitors_floc_
             | Ppat_constr
                 {
                   constr = _visitors_fconstr;
                   args = _visitors_fargs;
                   is_open = _visitors_fis_open;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constr env _visitors_fconstr _visitors_fargs
                   _visitors_fis_open _visitors_floc_
             | Ppat_or
                 {
                   pat1 = _visitors_fpat1;
                   pat2 = _visitors_fpat2;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_or env _visitors_fpat1 _visitors_fpat2
                   _visitors_floc_
             | Ppat_tuple { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_tuple env _visitors_fpats _visitors_floc_
             | Ppat_var _visitors_c0 -> self#visit_Ppat_var env _visitors_c0
             | Ppat_record
                 {
                   fields = _visitors_ffields;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_record env _visitors_ffields
                   _visitors_fis_closed _visitors_floc_
             | Ppat_map
                 {
                   elems = _visitors_felems;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_map env _visitors_felems _visitors_fis_closed
                   _visitors_floc_
             | Ppat_range
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   inclusive = _visitors_finclusive;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_range env _visitors_flhs _visitors_frhs
                   _visitors_finclusive _visitors_floc_

        method visit_Closed : _ -> array_pattern list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_array_pattern env))
                 _visitors_c0
             in
             ()

        method visit_Open :
            _ ->
            array_pattern list ->
            array_pattern list ->
            binder option ->
            unit =
          fun env ->
            fun _visitors_c0 ->
             fun _visitors_c1 ->
              fun _visitors_c2 ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this
                     ~f:(self#visit_array_pattern env))
                   _visitors_c0
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this
                     ~f:(self#visit_array_pattern env))
                   _visitors_c1
               in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_binder env) t
                   | None -> ())
                   _visitors_c2
               in
               ()

        method visit_array_patterns : _ -> array_patterns -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
             | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
                 self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

        method visit_Pattern : _ -> pattern -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_pattern env _visitors_c0 in
             ()

        method visit_String_spread : _ -> string_literal -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             ()

        method visit_String_spread_const :
            _ -> binder -> string option -> location -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fpkg ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (fun _visitors_this -> ()) t
                   | None -> ())
                   _visitors_fpkg
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_array_pattern : _ -> array_pattern -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pattern _visitors_c0 -> self#visit_Pattern env _visitors_c0
             | String_spread _visitors_c0 ->
                 self#visit_String_spread env _visitors_c0
             | String_spread_const
                 {
                   binder = _visitors_fbinder;
                   pkg = _visitors_fpkg;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_String_spread_const env _visitors_fbinder
                   _visitors_fpkg _visitors_floc_

        method visit_Field_def : _ -> label -> expr -> bool -> location -> unit
            =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fis_pun
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_field_def : _ -> field_def -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_def
                 {
                   label = _visitors_flabel;
                   expr = _visitors_fexpr;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_def env _visitors_flabel _visitors_fexpr
                   _visitors_fis_pun _visitors_floc_

        method visit_Field_pat :
            _ -> label -> pattern -> bool -> location -> unit =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fpattern ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fis_pun
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_field_pat : _ -> field_pat -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_pat
                 {
                   label = _visitors_flabel;
                   pattern = _visitors_fpattern;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_pat env _visitors_flabel _visitors_fpattern
                   _visitors_fis_pun _visitors_floc_

        method visit_Constr_pat_arg : _ -> pattern -> argument_kind -> unit =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_pattern env _visitors_fpat in
              let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
              ()

        method visit_constr_pat_arg : _ -> constr_pat_arg -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Constr_pat_arg { pat = _visitors_fpat; kind = _visitors_fkind }
               ->
                 self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind

        method visit_Map_pat_elem :
            _ -> constant -> pattern -> bool -> location -> location -> unit =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fpat ->
              fun _visitors_fmatch_absent ->
               fun _visitors_fkey_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fkey in
                 let _visitors_r1 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r2 =
                   (fun _visitors_this -> ()) _visitors_fmatch_absent
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_fkey_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_map_pat_elem : _ -> map_pat_elem -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_pat_elem
                 {
                   key = _visitors_fkey;
                   pat = _visitors_fpat;
                   match_absent = _visitors_fmatch_absent;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_pat_elem env _visitors_fkey _visitors_fpat
                   _visitors_fmatch_absent _visitors_fkey_loc_ _visitors_floc_

        method visit_constr_param : _ -> constr_param -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.cparam_typ in
             let _visitors_r1 =
               (fun _visitors_this -> ()) _visitors_this.cparam_mut
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_label env) t
                 | None -> ())
                 _visitors_this.cparam_label
             in
             ()

        method visit_constr_decl : _ -> constr_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_constr_name env _visitors_this.constr_name
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(self#visit_constr_param env))
                       t
                 | None -> ())
                 _visitors_this.constr_args
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     (fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 =
                         (fun _visitors_this -> ()) _visitors_c0
                       in
                       let _visitors_r1 =
                         (fun _visitors_this -> ()) _visitors_c1
                       in
                       ())
                       t
                 | None -> ())
                 _visitors_this.constr_tag
             in
             let _visitors_r3 =
               (fun _visitors_this -> ()) _visitors_this.constr_loc_
             in
             ()

        method visit_field_name : _ -> field_name -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> ()) _visitors_this.label
             in
             let _visitors_r1 =
               (fun _visitors_this -> ()) _visitors_this.loc_
             in
             ()

        method visit_field_decl : _ -> field_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_field_name env _visitors_this.field_name
             in
             let _visitors_r1 = self#visit_typ env _visitors_this.field_ty in
             let _visitors_r2 =
               (fun _visitors_this -> ()) _visitors_this.field_mut
             in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.field_vis
             in
             let _visitors_r4 =
               (fun _visitors_this -> ()) _visitors_this.field_loc_
             in
             ()

        method visit_No_payload : _ -> unit = fun env -> ()

        method visit_Single_payload : _ -> typ -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             ()

        method visit_Enum_payload : _ -> constr_decl list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_constr_decl env))
                 _visitors_c0
             in
             ()

        method visit_exception_decl : _ -> exception_decl -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | No_payload -> self#visit_No_payload env
             | Single_payload _visitors_c0 ->
                 self#visit_Single_payload env _visitors_c0
             | Enum_payload _visitors_c0 ->
                 self#visit_Enum_payload env _visitors_c0

        method visit_Ptd_abstract : _ -> unit = fun env -> ()
        method visit_Ptd_extern : _ -> unit = fun env -> ()

        method visit_Ptd_newtype : _ -> typ -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             ()

        method visit_Ptd_error : _ -> exception_decl -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
             ()

        method visit_Ptd_variant : _ -> constr_decl list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_constr_decl env))
                 _visitors_c0
             in
             ()

        method visit_Ptd_record : _ -> field_decl list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_field_decl env))
                 _visitors_c0
             in
             ()

        method visit_Ptd_alias : _ -> typ -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             ()

        method visit_type_desc : _ -> type_desc -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptd_abstract -> self#visit_Ptd_abstract env
             | Ptd_extern -> self#visit_Ptd_extern env
             | Ptd_newtype _visitors_c0 ->
                 self#visit_Ptd_newtype env _visitors_c0
             | Ptd_error _visitors_c0 -> self#visit_Ptd_error env _visitors_c0
             | Ptd_variant _visitors_c0 ->
                 self#visit_Ptd_variant env _visitors_c0
             | Ptd_record _visitors_c0 -> self#visit_Ptd_record env _visitors_c0
             | Ptd_alias _visitors_c0 -> self#visit_Ptd_alias env _visitors_c0

        method visit_type_decl : _ -> type_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> ()) _visitors_this.tycon
             in
             let _visitors_r1 =
               (fun _visitors_this -> ()) _visitors_this.tycon_loc_
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_type_decl_binder env))
                 _visitors_this.params
             in
             let _visitors_r3 =
               self#visit_type_desc env _visitors_this.components
             in
             let _visitors_r4 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_attribute env))
                 _visitors_this.attrs
             in
             let _visitors_r5 = self#visit_docstring env _visitors_this.doc_ in
             let _visitors_r6 =
               self#visit_visibility env _visitors_this.type_vis
             in
             let _visitors_r7 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_deriving_directive env))
                 _visitors_this.deriving_
             in
             let _visitors_r8 =
               (fun _visitors_this -> ()) _visitors_this.loc_
             in
             ()

        method visit_local_type_decl : _ -> local_type_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> ()) _visitors_this.local_tycon
             in
             let _visitors_r1 =
               (fun _visitors_this -> ()) _visitors_this.local_tycon_loc_
             in
             let _visitors_r2 =
               self#visit_type_desc env _visitors_this.local_components
             in
             let _visitors_r3 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_deriving_directive env))
                 _visitors_this.deriving_
             in
             ()

        method visit_deriving_directive : _ -> deriving_directive -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_type_name env _visitors_this.type_name_
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_argument env))
                 _visitors_this.args
             in
             let _visitors_r2 =
               (fun _visitors_this -> ()) _visitors_this.loc_
             in
             ()

        method visit_Vis_default : _ -> unit = fun env -> ()

        method visit_Vis_pub : _ -> string option -> location -> unit =
          fun env ->
            fun _visitors_fattr ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (fun _visitors_this -> ()) t
                  | None -> ())
                  _visitors_fattr
              in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Vis_priv : _ -> location -> unit =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_floc_ in
             ()

        method visit_visibility : _ -> visibility -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Vis_default -> self#visit_Vis_default env
             | Vis_pub { attr = _visitors_fattr; loc_ = _visitors_floc_ } ->
                 self#visit_Vis_pub env _visitors_fattr _visitors_floc_
             | Vis_priv { loc_ = _visitors_floc_ } ->
                 self#visit_Vis_priv env _visitors_floc_

        method visit_Import : _ -> string_literal -> string_literal -> unit =
          fun env ->
            fun _visitors_fmodule_name ->
             fun _visitors_ffunc_name ->
              let _visitors_r0 =
                self#visit_string_literal env _visitors_fmodule_name
              in
              let _visitors_r1 =
                self#visit_string_literal env _visitors_ffunc_name
              in
              ()

        method visit_Embedded :
            _ -> string_literal option -> embedded_code -> unit =
          fun env ->
            fun _visitors_flanguage ->
             fun _visitors_fcode ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_string_literal env) t
                  | None -> ())
                  _visitors_flanguage
              in
              let _visitors_r1 = self#visit_embedded_code env _visitors_fcode in
              ()

        method visit_func_stubs : _ -> func_stubs -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Import
                 {
                   module_name = _visitors_fmodule_name;
                   func_name = _visitors_ffunc_name;
                 } ->
                 self#visit_Import env _visitors_fmodule_name
                   _visitors_ffunc_name
             | Embedded
                 { language = _visitors_flanguage; code = _visitors_fcode } ->
                 self#visit_Embedded env _visitors_flanguage _visitors_fcode

        method visit_Code_string : _ -> string_literal -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             ()

        method visit_Code_multiline_string : _ -> string list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(fun _visitors_this -> ()))
                 _visitors_c0
             in
             ()

        method visit_embedded_code : _ -> embedded_code -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Code_string _visitors_c0 ->
                 self#visit_Code_string env _visitors_c0
             | Code_multiline_string _visitors_c0 ->
                 self#visit_Code_multiline_string env _visitors_c0

        method visit_Decl_body : _ -> local_type_decl list -> expr -> unit =
          fun env ->
            fun _visitors_flocal_types ->
             fun _visitors_fexpr ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_local_type_decl env))
                  _visitors_flocal_types
              in
              let _visitors_r1 = self#visit_expr env _visitors_fexpr in
              ()

        method visit_Decl_stubs : _ -> func_stubs -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
             ()

        method visit_decl_body : _ -> decl_body -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Decl_body
                 {
                   local_types = _visitors_flocal_types;
                   expr = _visitors_fexpr;
                 } ->
                 self#visit_Decl_body env _visitors_flocal_types _visitors_fexpr
             | Decl_stubs _visitors_c0 -> self#visit_Decl_stubs env _visitors_c0

        method visit_fun_decl : _ -> fun_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_type_name env) t
                 | None -> ())
                 _visitors_this.type_name
             in
             let _visitors_r1 = self#visit_binder env _visitors_this.name in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (fun _visitors_this -> ()) t
                 | None -> ())
                 _visitors_this.has_error
             in
             let _visitors_r3 =
               (fun _visitors_this -> ()) _visitors_this.is_async
             in
             let _visitors_r4 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_parameters env) t
                 | None -> ())
                 _visitors_this.decl_params
             in
             let _visitors_r5 =
               (fun _visitors_this -> ()) _visitors_this.params_loc_
             in
             let _visitors_r6 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_tvar_binder env))
                 _visitors_this.quantifiers
             in
             let _visitors_r7 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     (fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_typ env _visitors_c0 in
                       let _visitors_r1 =
                         self#visit_error_typ env _visitors_c1
                       in
                       ())
                       t
                 | None -> ())
                 _visitors_this.return_type
             in
             let _visitors_r8 = self#visit_visibility env _visitors_this.vis in
             let _visitors_r9 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_attribute env))
                 _visitors_this.attrs
             in
             let _visitors_r10 = self#visit_docstring env _visitors_this.doc_ in
             ()

        method visit_trait_method_param : _ -> trait_method_param -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.tmparam_typ in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_label env) t
                 | None -> ())
                 _visitors_this.tmparam_label
             in
             ()

        method visit_Trait_method :
            _ ->
            binder ->
            bool ->
            tvar_binder list ->
            trait_method_param list ->
            (typ * error_typ) option ->
            location ->
            unit =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_fhas_error ->
              fun _visitors_fquantifiers ->
               fun _visitors_fparams ->
                fun _visitors_freturn_type ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 =
                    (fun _visitors_this -> ()) _visitors_fhas_error
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this
                        ~f:(self#visit_tvar_binder env))
                      _visitors_fquantifiers
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this
                        ~f:(self#visit_trait_method_param env))
                      _visitors_fparams
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t ->
                          (fun (_visitors_c0, _visitors_c1) ->
                            let _visitors_r0 =
                              self#visit_typ env _visitors_c0
                            in
                            let _visitors_r1 =
                              self#visit_error_typ env _visitors_c1
                            in
                            ())
                            t
                      | None -> ())
                      _visitors_freturn_type
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> ()) _visitors_floc_
                  in
                  ()

        method visit_trait_method_decl : _ -> trait_method_decl -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Trait_method
                 {
                   name = _visitors_fname;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   return_type = _visitors_freturn_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Trait_method env _visitors_fname
                   _visitors_fhas_error _visitors_fquantifiers _visitors_fparams
                   _visitors_freturn_type _visitors_floc_

        method visit_trait_decl : _ -> trait_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_binder env _visitors_this.trait_name
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_tvar_constraint env))
                 _visitors_this.trait_supers
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_trait_method_decl env))
                 _visitors_this.trait_methods
             in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.trait_vis
             in
             let _visitors_r4 =
               (fun _visitors_this -> ()) _visitors_this.trait_loc_
             in
             let _visitors_r5 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_attribute env))
                 _visitors_this.trait_attrs
             in
             let _visitors_r6 =
               self#visit_docstring env _visitors_this.trait_doc_
             in
             ()

        method visit_Ptop_expr :
            _ -> expr -> bool -> local_type_decl list -> absolute_loc -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_flocal_types ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  (fun _visitors_this -> ()) _visitors_fis_main
                in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this
                      ~f:(self#visit_local_type_decl env))
                    _visitors_flocal_types
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_floc_ in
                ()

        method visit_Ptop_test :
            _ ->
            expr ->
            test_name ->
            parameters option ->
            local_type_decl list ->
            absolute_loc ->
            attribute list ->
            docstring ->
            unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fname ->
              fun _visitors_fparams ->
               fun _visitors_flocal_types ->
                fun _visitors_floc_ ->
                 fun _visitors_fattrs ->
                  fun _visitors_fdoc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                   let _visitors_r1 =
                     (fun _visitors_this -> ()) _visitors_fname
                   in
                   let _visitors_r2 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_parameters env) t
                       | None -> ())
                       _visitors_fparams
                   in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(self#visit_local_type_decl env))
                       _visitors_flocal_types
                   in
                   let _visitors_r4 =
                     (fun _visitors_this -> ()) _visitors_floc_
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(self#visit_attribute env))
                       _visitors_fattrs
                   in
                   let _visitors_r6 =
                     self#visit_docstring env _visitors_fdoc_
                   in
                   ()

        method visit_Ptop_typedef : _ -> type_decl -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_type_decl env _visitors_c0 in
             ()

        method visit_Ptop_funcdef :
            _ -> fun_decl -> decl_body -> absolute_loc -> unit =
          fun env ->
            fun _visitors_ffun_decl ->
             fun _visitors_fdecl_body ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
               let _visitors_r1 =
                 self#visit_decl_body env _visitors_fdecl_body
               in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Ptop_letdef :
            _ ->
            binder ->
            typ option ->
            expr ->
            visibility ->
            bool ->
            absolute_loc ->
            attribute list ->
            docstring ->
            unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fvis ->
                fun _visitors_fis_constant ->
                 fun _visitors_floc_ ->
                  fun _visitors_fattrs ->
                   fun _visitors_fdoc_ ->
                    let _visitors_r0 =
                      self#visit_binder env _visitors_fbinder
                    in
                    let _visitors_r1 =
                      (fun _visitors_this ->
                        match _visitors_this with
                        | Some t -> (self#visit_typ env) t
                        | None -> ())
                        _visitors_fty
                    in
                    let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                    let _visitors_r3 =
                      self#visit_visibility env _visitors_fvis
                    in
                    let _visitors_r4 =
                      (fun _visitors_this -> ()) _visitors_fis_constant
                    in
                    let _visitors_r5 =
                      (fun _visitors_this -> ()) _visitors_floc_
                    in
                    let _visitors_r6 =
                      (fun _visitors_this ->
                        Basic_lst.iter _visitors_this
                          ~f:(self#visit_attribute env))
                        _visitors_fattrs
                    in
                    let _visitors_r7 =
                      self#visit_docstring env _visitors_fdoc_
                    in
                    ()

        method visit_Ptop_trait : _ -> trait_decl -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_trait_decl env _visitors_c0 in
             ()

        method visit_Ptop_trait_alias :
            _ ->
            binder ->
            type_name ->
            visibility ->
            absolute_loc ->
            attribute list ->
            docstring ->
            unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ftarget ->
              fun _visitors_fvis ->
               fun _visitors_floc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fdoc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_type_name env _visitors_ftarget
                  in
                  let _visitors_r2 = self#visit_visibility env _visitors_fvis in
                  let _visitors_r3 =
                    (fun _visitors_this -> ()) _visitors_floc_
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this
                        ~f:(self#visit_attribute env))
                      _visitors_fattrs
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  ()

        method visit_Ptop_impl :
            _ ->
            typ option ->
            type_name ->
            binder ->
            bool ->
            tvar_binder list ->
            parameters ->
            (typ * error_typ) option ->
            decl_body ->
            visibility ->
            absolute_loc ->
            location ->
            attribute list ->
            docstring ->
            unit =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fmethod_name ->
               fun _visitors_fhas_error ->
                fun _visitors_fquantifiers ->
                 fun _visitors_fparams ->
                  fun _visitors_fret_ty ->
                   fun _visitors_fbody ->
                    fun _visitors_fvis ->
                     fun _visitors_floc_ ->
                      fun _visitors_fheader_loc_ ->
                       fun _visitors_fattrs ->
                        fun _visitors_fdoc_ ->
                         let _visitors_r0 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t -> (self#visit_typ env) t
                             | None -> ())
                             _visitors_fself_ty
                         in
                         let _visitors_r1 =
                           self#visit_type_name env _visitors_ftrait
                         in
                         let _visitors_r2 =
                           self#visit_binder env _visitors_fmethod_name
                         in
                         let _visitors_r3 =
                           (fun _visitors_this -> ()) _visitors_fhas_error
                         in
                         let _visitors_r4 =
                           (fun _visitors_this ->
                             Basic_lst.iter _visitors_this
                               ~f:(self#visit_tvar_binder env))
                             _visitors_fquantifiers
                         in
                         let _visitors_r5 =
                           self#visit_parameters env _visitors_fparams
                         in
                         let _visitors_r6 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t ->
                                 (fun (_visitors_c0, _visitors_c1) ->
                                   let _visitors_r0 =
                                     self#visit_typ env _visitors_c0
                                   in
                                   let _visitors_r1 =
                                     self#visit_error_typ env _visitors_c1
                                   in
                                   ())
                                   t
                             | None -> ())
                             _visitors_fret_ty
                         in
                         let _visitors_r7 =
                           self#visit_decl_body env _visitors_fbody
                         in
                         let _visitors_r8 =
                           self#visit_visibility env _visitors_fvis
                         in
                         let _visitors_r9 =
                           (fun _visitors_this -> ()) _visitors_floc_
                         in
                         let _visitors_r10 =
                           (fun _visitors_this -> ()) _visitors_fheader_loc_
                         in
                         let _visitors_r11 =
                           (fun _visitors_this ->
                             Basic_lst.iter _visitors_this
                               ~f:(self#visit_attribute env))
                             _visitors_fattrs
                         in
                         let _visitors_r12 =
                           self#visit_docstring env _visitors_fdoc_
                         in
                         ()

        method visit_Ptop_impl_relation :
            _ ->
            typ ->
            type_name ->
            tvar_binder list ->
            bool ->
            absolute_loc ->
            unit =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fquantifiers ->
               fun _visitors_fis_pub ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
                 let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this
                       ~f:(self#visit_tvar_binder env))
                     _visitors_fquantifiers
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> ()) _visitors_fis_pub
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> ()) _visitors_floc_
                 in
                 ()

        method visit_impl : _ -> impl -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_flocal_types _visitors_floc_
             | Ptop_test
                 {
                   expr = _visitors_fexpr;
                   name = _visitors_fname;
                   params = _visitors_fparams;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_test env _visitors_fexpr _visitors_fname
                   _visitors_fparams _visitors_flocal_types _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_typedef _visitors_c0 ->
                 self#visit_Ptop_typedef env _visitors_c0
             | Ptop_funcdef
                 {
                   fun_decl = _visitors_ffun_decl;
                   decl_body = _visitors_fdecl_body;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_funcdef env _visitors_ffun_decl
                   _visitors_fdecl_body _visitors_floc_
             | Ptop_letdef
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   vis = _visitors_fvis;
                   is_constant = _visitors_fis_constant;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_letdef env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fvis _visitors_fis_constant
                   _visitors_floc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_trait _visitors_c0 -> self#visit_Ptop_trait env _visitors_c0
             | Ptop_trait_alias
                 {
                   binder = _visitors_fbinder;
                   target = _visitors_ftarget;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_trait_alias env _visitors_fbinder
                   _visitors_ftarget _visitors_fvis _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_impl
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   method_name = _visitors_fmethod_name;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   ret_ty = _visitors_fret_ty;
                   body = _visitors_fbody;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   header_loc_ = _visitors_fheader_loc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_impl env _visitors_fself_ty _visitors_ftrait
                   _visitors_fmethod_name _visitors_fhas_error
                   _visitors_fquantifiers _visitors_fparams _visitors_fret_ty
                   _visitors_fbody _visitors_fvis _visitors_floc_
                   _visitors_fheader_loc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_impl_relation
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   quantifiers = _visitors_fquantifiers;
                   is_pub = _visitors_fis_pub;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_impl_relation env _visitors_fself_ty
                   _visitors_ftrait _visitors_fquantifiers _visitors_fis_pub
                   _visitors_floc_

        method visit_Interp_lit : _ -> string -> string -> location -> unit =
          fun env ->
            fun _visitors_fstr ->
             fun _visitors_frepr ->
              fun _visitors_floc_ ->
               let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fstr in
               let _visitors_r1 = (fun _visitors_this -> ()) _visitors_frepr in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_floc_ in
               ()

        method visit_Interp_expr : _ -> expr -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
              ()

        method visit_Interp_source : _ -> Literal.interp_source -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_interp_source env _visitors_c0 in
             ()

        method visit_interp_elem : _ -> interp_elem -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Interp_lit
                 {
                   str = _visitors_fstr;
                   repr = _visitors_frepr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Interp_lit env _visitors_fstr _visitors_frepr
                   _visitors_floc_
             | Interp_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Interp_expr env _visitors_fexpr _visitors_floc_
             | Interp_source _visitors_c0 ->
                 self#visit_Interp_source env _visitors_c0

        method visit_Multiline_string : _ -> string -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_c0 in
             ()

        method visit_Multiline_interp : _ -> interp_elem list -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_interp_elem env))
                 _visitors_c0
             in
             ()

        method visit_multiline_string_elem : _ -> multiline_string_elem -> unit
            =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Multiline_string _visitors_c0 ->
                 self#visit_Multiline_string env _visitors_c0
             | Multiline_interp _visitors_c0 ->
                 self#visit_Multiline_interp env _visitors_c0

        method visit_impls : _ -> impls -> unit =
          fun env ->
            fun _visitors_this ->
             Basic_lst.iter _visitors_this ~f:(self#visit_impl env)
      end

    [@@@VISITORS.END]
  end

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] map =
      object (self : 'self)
        inherit [_] mapbase

        method visit_Pexpr_apply :
            _ -> expr -> argument list -> apply_attr -> location -> expr =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fattr ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_ffunc in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_argument env))
                    _visitors_fargs
                in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fattr
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_apply
                  {
                    func = _visitors_r0;
                    args = _visitors_r1;
                    attr = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_infix : _ -> var -> expr -> expr -> location -> expr
            =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_flhs ->
              fun _visitors_frhs ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fop in
                let _visitors_r1 = self#visit_expr env _visitors_flhs in
                let _visitors_r2 = self#visit_expr env _visitors_frhs in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_infix
                  {
                    op = _visitors_r0;
                    lhs = _visitors_r1;
                    rhs = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_unary : _ -> var -> expr -> location -> expr =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_fexpr ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fop in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_unary
                 { op = _visitors_r0; expr = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Pexpr_array : _ -> expr list -> location -> expr =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_array { exprs = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_array_spread :
            _ -> spreadable_elem list -> location -> expr =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_spreadable_elem env))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_array_spread { elems = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_array_get : _ -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_farray in
               let _visitors_r1 = self#visit_expr env _visitors_findex in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_array_get
                 {
                   array = _visitors_r0;
                   index = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_array_get_slice :
            _ ->
            expr ->
            expr option ->
            expr option ->
            location ->
            location ->
            expr =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_fstart_index ->
              fun _visitors_fend_index ->
               fun _visitors_findex_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_farray in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_fstart_index
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_fend_index
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_findex_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_array_get_slice
                   {
                     array = _visitors_r0;
                     start_index = _visitors_r1;
                     end_index = _visitors_r2;
                     index_loc_ = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_array_set :
            _ -> expr -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_farray ->
             fun _visitors_findex ->
              fun _visitors_fvalue ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_farray in
                let _visitors_r1 = self#visit_expr env _visitors_findex in
                let _visitors_r2 = self#visit_expr env _visitors_fvalue in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_array_set
                  {
                    array = _visitors_r0;
                    index = _visitors_r1;
                    value = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_array_augmented_set :
            _ -> var -> expr -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_fop ->
             fun _visitors_farray ->
              fun _visitors_findex ->
               fun _visitors_fvalue ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fop in
                 let _visitors_r1 = self#visit_expr env _visitors_farray in
                 let _visitors_r2 = self#visit_expr env _visitors_findex in
                 let _visitors_r3 = self#visit_expr env _visitors_fvalue in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_array_augmented_set
                   {
                     op = _visitors_r0;
                     array = _visitors_r1;
                     index = _visitors_r2;
                     value = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_constant : _ -> constant -> location -> expr =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_fc
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_constant { c = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_multiline_string :
            _ -> multiline_string_elem list -> location -> expr =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (self#visit_multiline_string_elem env))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_multiline_string
                { elems = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_interp : _ -> interp_elem list -> location -> expr =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_interp_elem env))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_interp { elems = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_constraint : _ -> expr -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_constraint
                 { expr = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Pexpr_constr : _ -> constructor -> location -> expr =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constructor env _visitors_fconstr in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_constr { constr = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_while :
            _ -> expr -> expr -> expr option -> label option -> location -> expr
            =
          fun env ->
            fun _visitors_floop_cond ->
             fun _visitors_floop_body ->
              fun _visitors_fwhile_else ->
               fun _visitors_flabel ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                 let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_fwhile_else
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_label env) t)
                     | None -> None)
                     _visitors_flabel
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_while
                   {
                     loop_cond = _visitors_r0;
                     loop_body = _visitors_r1;
                     while_else = _visitors_r2;
                     label = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_function : _ -> func -> location -> expr =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_func env _visitors_ffunc in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_function { func = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_ident : _ -> var -> location -> expr =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_var env _visitors_fid in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_ident { id = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_if :
            _ -> expr -> expr -> expr option -> location -> expr =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 = self#visit_expr env _visitors_fifso in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_expr env) t)
                    | None -> None)
                    _visitors_fifnot
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_if
                  {
                    cond = _visitors_r0;
                    ifso = _visitors_r1;
                    ifnot = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_guard :
            _ -> expr -> expr option -> expr -> location -> expr =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fcond in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_expr env) t)
                    | None -> None)
                    _visitors_fotherwise
                in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_guard
                  {
                    cond = _visitors_r0;
                    otherwise = _visitors_r1;
                    body = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_guard_let :
            _ -> pattern -> expr -> case list option -> expr -> location -> expr
            =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fexpr ->
              fun _visitors_fotherwise ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t ->
                         Some
                           ((fun _visitors_this ->
                              Basic_lst.map _visitors_this (self#visit_case env))
                              t)
                     | None -> None)
                     _visitors_fotherwise
                 in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_guard_let
                   {
                     pat = _visitors_r0;
                     expr = _visitors_r1;
                     otherwise = _visitors_r2;
                     body = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_is : _ -> expr -> pattern -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fpat ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_is
                 {
                   expr = _visitors_r0;
                   pat = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_letfn :
            _ -> binder -> func -> expr -> location -> expr =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffunc ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fname in
                let _visitors_r1 = self#visit_func env _visitors_ffunc in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_letfn
                  {
                    name = _visitors_r0;
                    func = _visitors_r1;
                    body = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_letrec :
            _ -> (binder * func) list -> expr -> location -> expr =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this
                     (fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_binder env _visitors_c0 in
                       let _visitors_r1 = self#visit_func env _visitors_c1 in
                       (_visitors_r0, _visitors_r1)))
                   _visitors_fbindings
               in
               let _visitors_r1 = self#visit_expr env _visitors_fbody in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_letrec
                 {
                   bindings = _visitors_r0;
                   body = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_let :
            _ -> pattern -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_fpattern ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_let
                  {
                    pattern = _visitors_r0;
                    expr = _visitors_r1;
                    body = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_sequence : _ -> expr list -> expr -> location -> expr
            =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_sequence
                 {
                   exprs = _visitors_r0;
                   last_expr = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_tuple : _ -> expr list -> location -> expr =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_tuple { exprs = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_record :
            _ ->
            type_name option ->
            field_def list ->
            trailing_mark ->
            location ->
            expr =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_ffields ->
              fun _visitors_ftrailing ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_type_name env) t)
                    | None -> None)
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_ftrailing
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_record
                  {
                    type_name = _visitors_r0;
                    fields = _visitors_r1;
                    trailing = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_record_update :
            _ -> type_name option -> expr -> field_def list -> location -> expr
            =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_frecord ->
              fun _visitors_ffields ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_type_name env) t)
                    | None -> None)
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_record_update
                  {
                    type_name = _visitors_r0;
                    record = _visitors_r1;
                    fields = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_field : _ -> expr -> accessor -> location -> expr =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_field
                 {
                   record = _visitors_r0;
                   accessor = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_method : _ -> type_name -> label -> location -> expr
            =
          fun env ->
            fun _visitors_ftype_name ->
             fun _visitors_fmethod_name ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_type_name env _visitors_ftype_name
               in
               let _visitors_r1 = self#visit_label env _visitors_fmethod_name in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_method
                 {
                   type_name = _visitors_r0;
                   method_name = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_dot_apply :
            _ ->
            expr ->
            label ->
            argument list ->
            bool ->
            apply_attr ->
            location ->
            expr =
          fun env ->
            fun _visitors_fself ->
             fun _visitors_fmethod_name ->
              fun _visitors_fargs ->
               fun _visitors_freturn_self ->
                fun _visitors_fattr ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_fself in
                  let _visitors_r1 =
                    self#visit_label env _visitors_fmethod_name
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_argument env))
                      _visitors_fargs
                  in
                  let _visitors_r3 =
                    (fun _visitors_this -> _visitors_this)
                      _visitors_freturn_self
                  in
                  let _visitors_r4 =
                    (fun _visitors_this -> _visitors_this) _visitors_fattr
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> _visitors_this) _visitors_floc_
                  in
                  Pexpr_dot_apply
                    {
                      self = _visitors_r0;
                      method_name = _visitors_r1;
                      args = _visitors_r2;
                      return_self = _visitors_r3;
                      attr = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Pexpr_as : _ -> expr -> type_name -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_as
                 {
                   expr = _visitors_r0;
                   trait = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_mutate :
            _ -> expr -> accessor -> expr -> var option -> location -> expr =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_ffield ->
               fun _visitors_faugmented_by ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_var env) t)
                     | None -> None)
                     _visitors_faugmented_by
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_mutate
                   {
                     record = _visitors_r0;
                     accessor = _visitors_r1;
                     field = _visitors_r2;
                     augmented_by = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_match :
            _ -> expr -> case list -> location -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fcases ->
              fun _visitors_fmatch_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_case env))
                    _visitors_fcases
                in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fmatch_loc_
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_match
                  {
                    expr = _visitors_r0;
                    cases = _visitors_r1;
                    match_loc_ = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_letmut :
            _ -> binder -> typ option -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fbody ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_typ env) t)
                     | None -> None)
                     _visitors_fty
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                 let _visitors_r3 = self#visit_expr env _visitors_fbody in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_letmut
                   {
                     binder = _visitors_r0;
                     ty = _visitors_r1;
                     expr = _visitors_r2;
                     body = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_pipe : _ -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_pipe
                 { lhs = _visitors_r0; rhs = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Pexpr_assign :
            _ -> var -> expr -> var option -> location -> expr =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_faugmented_by ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_var env) t)
                    | None -> None)
                    _visitors_faugmented_by
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Pexpr_assign
                  {
                    var = _visitors_r0;
                    expr = _visitors_r1;
                    augmented_by = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Pexpr_hole : _ -> location -> hole -> expr =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_fkind ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              let _visitors_r1 = self#visit_hole env _visitors_fkind in
              Pexpr_hole { loc_ = _visitors_r0; kind = _visitors_r1 }

        method visit_Pexpr_return : _ -> expr option -> location -> expr =
          fun env ->
            fun _visitors_freturn_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_expr env) t)
                  | None -> None)
                  _visitors_freturn_value
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_return { return_value = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_raise : _ -> expr -> location -> expr =
          fun env ->
            fun _visitors_ferr_value ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_ferr_value in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_raise { err_value = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_unit : _ -> location -> bool -> expr =
          fun env ->
            fun _visitors_floc_ ->
             fun _visitors_ffaked ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_ffaked
              in
              Pexpr_unit { loc_ = _visitors_r0; faked = _visitors_r1 }

        method visit_Pexpr_break :
            _ -> expr option -> label option -> location -> expr =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_expr env) t)
                   | None -> None)
                   _visitors_farg
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_label env) t)
                   | None -> None)
                   _visitors_flabel
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_break
                 {
                   arg = _visitors_r0;
                   label = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_continue :
            _ -> expr list -> label option -> location -> expr =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_expr env))
                   _visitors_fargs
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_label env) t)
                   | None -> None)
                   _visitors_flabel
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_continue
                 {
                   args = _visitors_r0;
                   label = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_loop :
            _ ->
            expr list ->
            multi_arg_case list ->
            label option ->
            location ->
            location ->
            expr =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_fbody ->
              fun _visitors_flabel ->
               fun _visitors_floop_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this (self#visit_expr env))
                     _visitors_fargs
                 in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this
                       (self#visit_multi_arg_case env))
                     _visitors_fbody
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_label env) t)
                     | None -> None)
                     _visitors_flabel
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_floop_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Pexpr_loop
                   {
                     args = _visitors_r0;
                     body = _visitors_r1;
                     label = _visitors_r2;
                     loop_loc_ = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Pexpr_for :
            _ ->
            (binder * expr) list ->
            expr option ->
            (binder * expr) list ->
            expr ->
            expr option ->
            label option ->
            location ->
            expr =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fcondition ->
              fun _visitors_fcontinue_block ->
               fun _visitors_fbody ->
                fun _visitors_ffor_else ->
                 fun _visitors_flabel ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this
                         (fun (_visitors_c0, _visitors_c1) ->
                           let _visitors_r0 =
                             self#visit_binder env _visitors_c0
                           in
                           let _visitors_r1 =
                             self#visit_expr env _visitors_c1
                           in
                           (_visitors_r0, _visitors_r1)))
                       _visitors_fbinders
                   in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_expr env) t)
                       | None -> None)
                       _visitors_fcondition
                   in
                   let _visitors_r2 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this
                         (fun (_visitors_c0, _visitors_c1) ->
                           let _visitors_r0 =
                             self#visit_binder env _visitors_c0
                           in
                           let _visitors_r1 =
                             self#visit_expr env _visitors_c1
                           in
                           (_visitors_r0, _visitors_r1)))
                       _visitors_fcontinue_block
                   in
                   let _visitors_r3 = self#visit_expr env _visitors_fbody in
                   let _visitors_r4 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_expr env) t)
                       | None -> None)
                       _visitors_ffor_else
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_label env) t)
                       | None -> None)
                       _visitors_flabel
                   in
                   let _visitors_r6 =
                     (fun _visitors_this -> _visitors_this) _visitors_floc_
                   in
                   Pexpr_for
                     {
                       binders = _visitors_r0;
                       condition = _visitors_r1;
                       continue_block = _visitors_r2;
                       body = _visitors_r3;
                       for_else = _visitors_r4;
                       label = _visitors_r5;
                       loc_ = _visitors_r6;
                     }

        method visit_Pexpr_foreach :
            _ ->
            binder option list ->
            expr ->
            expr ->
            expr option ->
            label option ->
            location ->
            expr =
          fun env ->
            fun _visitors_fbinders ->
             fun _visitors_fexpr ->
              fun _visitors_fbody ->
               fun _visitors_felse_block ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (fun _visitors_this ->
                          match _visitors_this with
                          | Some t -> Some ((self#visit_binder env) t)
                          | None -> None))
                      _visitors_fbinders
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_expr env) t)
                      | None -> None)
                      _visitors_felse_block
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_label env) t)
                      | None -> None)
                      _visitors_flabel
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> _visitors_this) _visitors_floc_
                  in
                  Pexpr_foreach
                    {
                      binders = _visitors_r0;
                      expr = _visitors_r1;
                      body = _visitors_r2;
                      else_block = _visitors_r3;
                      label = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Pexpr_try :
            _ ->
            expr ->
            case list ->
            bool ->
            case list option ->
            location ->
            location ->
            location ->
            location ->
            expr =
          fun env ->
            fun _visitors_fbody ->
             fun _visitors_fcatch ->
              fun _visitors_fcatch_all ->
               fun _visitors_ftry_else ->
                fun _visitors_ftry_loc_ ->
                 fun _visitors_fcatch_loc_ ->
                  fun _visitors_felse_loc_ ->
                   fun _visitors_floc_ ->
                    let _visitors_r0 = self#visit_expr env _visitors_fbody in
                    let _visitors_r1 =
                      (fun _visitors_this ->
                        Basic_lst.map _visitors_this (self#visit_case env))
                        _visitors_fcatch
                    in
                    let _visitors_r2 =
                      (fun _visitors_this -> _visitors_this)
                        _visitors_fcatch_all
                    in
                    let _visitors_r3 =
                      (fun _visitors_this ->
                        match _visitors_this with
                        | Some t ->
                            Some
                              ((fun _visitors_this ->
                                 Basic_lst.map _visitors_this
                                   (self#visit_case env))
                                 t)
                        | None -> None)
                        _visitors_ftry_else
                    in
                    let _visitors_r4 =
                      (fun _visitors_this -> _visitors_this) _visitors_ftry_loc_
                    in
                    let _visitors_r5 =
                      (fun _visitors_this -> _visitors_this)
                        _visitors_fcatch_loc_
                    in
                    let _visitors_r6 =
                      (fun _visitors_this -> _visitors_this)
                        _visitors_felse_loc_
                    in
                    let _visitors_r7 =
                      (fun _visitors_this -> _visitors_this) _visitors_floc_
                    in
                    Pexpr_try
                      {
                        body = _visitors_r0;
                        catch = _visitors_r1;
                        catch_all = _visitors_r2;
                        try_else = _visitors_r3;
                        try_loc_ = _visitors_r4;
                        catch_loc_ = _visitors_r5;
                        else_loc_ = _visitors_r6;
                        loc_ = _visitors_r7;
                      }

        method visit_Pexpr_map : _ -> map_expr_elem list -> location -> expr =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_map_expr_elem env))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Pexpr_map { elems = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Pexpr_group : _ -> expr -> group -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fgroup ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fgroup
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Pexpr_group
                 {
                   expr = _visitors_r0;
                   group = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Pexpr_static_assert :
            _ -> static_assertion list -> expr -> expr =
          fun env ->
            fun _visitors_fasserts ->
             fun _visitors_fbody ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_static_assertion env))
                  _visitors_fasserts
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              Pexpr_static_assert
                { asserts = _visitors_r0; body = _visitors_r1 }

        method visit_expr : _ -> expr -> expr =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fattr _visitors_floc_
             | Pexpr_infix
                 {
                   op = _visitors_fop;
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_infix env _visitors_fop _visitors_flhs
                   _visitors_frhs _visitors_floc_
             | Pexpr_unary
                 {
                   op = _visitors_fop;
                   expr = _visitors_fexpr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_unary env _visitors_fop _visitors_fexpr
                   _visitors_floc_
             | Pexpr_array { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_array env _visitors_fexprs _visitors_floc_
             | Pexpr_array_spread
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_array_spread env _visitors_felems
                   _visitors_floc_
             | Pexpr_array_get
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get env _visitors_farray
                   _visitors_findex _visitors_floc_
             | Pexpr_array_get_slice
                 {
                   array = _visitors_farray;
                   start_index = _visitors_fstart_index;
                   end_index = _visitors_fend_index;
                   index_loc_ = _visitors_findex_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_get_slice env _visitors_farray
                   _visitors_fstart_index _visitors_fend_index
                   _visitors_findex_loc_ _visitors_floc_
             | Pexpr_array_set
                 {
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_set env _visitors_farray
                   _visitors_findex _visitors_fvalue _visitors_floc_
             | Pexpr_array_augmented_set
                 {
                   op = _visitors_fop;
                   array = _visitors_farray;
                   index = _visitors_findex;
                   value = _visitors_fvalue;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_array_augmented_set env _visitors_fop
                   _visitors_farray _visitors_findex _visitors_fvalue
                   _visitors_floc_
             | Pexpr_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constant env _visitors_fc _visitors_floc_
             | Pexpr_multiline_string
                 { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_multiline_string env _visitors_felems
                   _visitors_floc_
             | Pexpr_interp { elems = _visitors_felems; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_interp env _visitors_felems _visitors_floc_
             | Pexpr_constraint
                 {
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_constraint env _visitors_fexpr _visitors_fty
                   _visitors_floc_
             | Pexpr_constr
                 { constr = _visitors_fconstr; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_constr env _visitors_fconstr _visitors_floc_
             | Pexpr_while
                 {
                   loop_cond = _visitors_floop_cond;
                   loop_body = _visitors_floop_body;
                   while_else = _visitors_fwhile_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_while env _visitors_floop_cond
                   _visitors_floop_body _visitors_fwhile_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_function { func = _visitors_ffunc; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_function env _visitors_ffunc _visitors_floc_
             | Pexpr_ident { id = _visitors_fid; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_ident env _visitors_fid _visitors_floc_
             | Pexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_floc_
             | Pexpr_guard
                 {
                   cond = _visitors_fcond;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard env _visitors_fcond _visitors_fotherwise
                   _visitors_fbody _visitors_floc_
             | Pexpr_guard_let
                 {
                   pat = _visitors_fpat;
                   expr = _visitors_fexpr;
                   otherwise = _visitors_fotherwise;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_guard_let env _visitors_fpat _visitors_fexpr
                   _visitors_fotherwise _visitors_fbody _visitors_floc_
             | Pexpr_is
                 {
                   expr = _visitors_fexpr;
                   pat = _visitors_fpat;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_is env _visitors_fexpr _visitors_fpat
                   _visitors_floc_
             | Pexpr_letfn
                 {
                   name = _visitors_fname;
                   func = _visitors_ffunc;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letfn env _visitors_fname _visitors_ffunc
                   _visitors_fbody _visitors_floc_
             | Pexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_floc_
             | Pexpr_let
                 {
                   pattern = _visitors_fpattern;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_let env _visitors_fpattern _visitors_fexpr
                   _visitors_fbody _visitors_floc_
             | Pexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_floc_
             | Pexpr_tuple { exprs = _visitors_fexprs; loc_ = _visitors_floc_ }
               ->
                 self#visit_Pexpr_tuple env _visitors_fexprs _visitors_floc_
             | Pexpr_record
                 {
                   type_name = _visitors_ftype_name;
                   fields = _visitors_ffields;
                   trailing = _visitors_ftrailing;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record env _visitors_ftype_name
                   _visitors_ffields _visitors_ftrailing _visitors_floc_
             | Pexpr_record_update
                 {
                   type_name = _visitors_ftype_name;
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_record_update env _visitors_ftype_name
                   _visitors_frecord _visitors_ffields _visitors_floc_
             | Pexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_floc_
             | Pexpr_method
                 {
                   type_name = _visitors_ftype_name;
                   method_name = _visitors_fmethod_name;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_method env _visitors_ftype_name
                   _visitors_fmethod_name _visitors_floc_
             | Pexpr_dot_apply
                 {
                   self = _visitors_fself;
                   method_name = _visitors_fmethod_name;
                   args = _visitors_fargs;
                   return_self = _visitors_freturn_self;
                   attr = _visitors_fattr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_dot_apply env _visitors_fself
                   _visitors_fmethod_name _visitors_fargs _visitors_freturn_self
                   _visitors_fattr _visitors_floc_
             | Pexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_floc_
             | Pexpr_mutate
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   field = _visitors_ffield;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_mutate env _visitors_frecord
                   _visitors_faccessor _visitors_ffield _visitors_faugmented_by
                   _visitors_floc_
             | Pexpr_match
                 {
                   expr = _visitors_fexpr;
                   cases = _visitors_fcases;
                   match_loc_ = _visitors_fmatch_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_match env _visitors_fexpr _visitors_fcases
                   _visitors_fmatch_loc_ _visitors_floc_
             | Pexpr_letmut
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_letmut env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fbody _visitors_floc_
             | Pexpr_pipe
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_pipe env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Pexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   augmented_by = _visitors_faugmented_by;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_faugmented_by _visitors_floc_
             | Pexpr_hole { loc_ = _visitors_floc_; kind = _visitors_fkind } ->
                 self#visit_Pexpr_hole env _visitors_floc_ _visitors_fkind
             | Pexpr_return
                 {
                   return_value = _visitors_freturn_value;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_return env _visitors_freturn_value
                   _visitors_floc_
             | Pexpr_raise
                 { err_value = _visitors_ferr_value; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_raise env _visitors_ferr_value _visitors_floc_
             | Pexpr_unit { loc_ = _visitors_floc_; faked = _visitors_ffaked }
               ->
                 self#visit_Pexpr_unit env _visitors_floc_ _visitors_ffaked
             | Pexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_break env _visitors_farg _visitors_flabel
                   _visitors_floc_
             | Pexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_floc_
             | Pexpr_loop
                 {
                   args = _visitors_fargs;
                   body = _visitors_fbody;
                   label = _visitors_flabel;
                   loop_loc_ = _visitors_floop_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_loop env _visitors_fargs _visitors_fbody
                   _visitors_flabel _visitors_floop_loc_ _visitors_floc_
             | Pexpr_for
                 {
                   binders = _visitors_fbinders;
                   condition = _visitors_fcondition;
                   continue_block = _visitors_fcontinue_block;
                   body = _visitors_fbody;
                   for_else = _visitors_ffor_else;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_for env _visitors_fbinders
                   _visitors_fcondition _visitors_fcontinue_block
                   _visitors_fbody _visitors_ffor_else _visitors_flabel
                   _visitors_floc_
             | Pexpr_foreach
                 {
                   binders = _visitors_fbinders;
                   expr = _visitors_fexpr;
                   body = _visitors_fbody;
                   else_block = _visitors_felse_block;
                   label = _visitors_flabel;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_foreach env _visitors_fbinders _visitors_fexpr
                   _visitors_fbody _visitors_felse_block _visitors_flabel
                   _visitors_floc_
             | Pexpr_try
                 {
                   body = _visitors_fbody;
                   catch = _visitors_fcatch;
                   catch_all = _visitors_fcatch_all;
                   try_else = _visitors_ftry_else;
                   try_loc_ = _visitors_ftry_loc_;
                   catch_loc_ = _visitors_fcatch_loc_;
                   else_loc_ = _visitors_felse_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_try env _visitors_fbody _visitors_fcatch
                   _visitors_fcatch_all _visitors_ftry_else _visitors_ftry_loc_
                   _visitors_fcatch_loc_ _visitors_felse_loc_ _visitors_floc_
             | Pexpr_map { elems = _visitors_felems; loc_ = _visitors_floc_ } ->
                 self#visit_Pexpr_map env _visitors_felems _visitors_floc_
             | Pexpr_group
                 {
                   expr = _visitors_fexpr;
                   group = _visitors_fgroup;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Pexpr_group env _visitors_fexpr _visitors_fgroup
                   _visitors_floc_
             | Pexpr_static_assert
                 { asserts = _visitors_fasserts; body = _visitors_fbody } ->
                 self#visit_Pexpr_static_assert env _visitors_fasserts
                   _visitors_fbody

        method visit_static_assertion :
            _ -> static_assertion -> static_assertion =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.assert_type in
             let _visitors_r1 =
               self#visit_longident env _visitors_this.assert_trait
             in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_this.assert_loc
             in
             let _visitors_r3 =
               (fun _visitors_this -> _visitors_this) _visitors_this.assert_msg
             in
             {
               assert_type = _visitors_r0;
               assert_trait = _visitors_r1;
               assert_loc = _visitors_r2;
               assert_msg = _visitors_r3;
             }

        method visit_argument : _ -> argument -> argument =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
             let _visitors_r1 =
               self#visit_argument_kind env _visitors_this.arg_kind
             in
             { arg_value = _visitors_r0; arg_kind = _visitors_r1 }

        method visit_parameters : _ -> parameters -> parameters =
          fun env ->
            fun _visitors_this ->
             Basic_lst.map _visitors_this (self#visit_parameter env)

        method visit_Discard_positional :
            _ -> typ option -> location -> parameter =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_typ env) t)
                  | None -> None)
                  _visitors_fty
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Discard_positional { ty = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Positional : _ -> binder -> typ option -> parameter =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_typ env) t)
                  | None -> None)
                  _visitors_fty
              in
              Positional { binder = _visitors_r0; ty = _visitors_r1 }

        method visit_Labelled : _ -> binder -> typ option -> parameter =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_typ env) t)
                  | None -> None)
                  _visitors_fty
              in
              Labelled { binder = _visitors_r0; ty = _visitors_r1 }

        method visit_Optional : _ -> binder -> expr -> typ option -> parameter =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fdefault ->
              fun _visitors_fty ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 = self#visit_expr env _visitors_fdefault in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_typ env) t)
                   | None -> None)
                   _visitors_fty
               in
               Optional
                 {
                   binder = _visitors_r0;
                   default = _visitors_r1;
                   ty = _visitors_r2;
                 }

        method visit_Question_optional : _ -> binder -> typ option -> parameter
            =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_typ env) t)
                  | None -> None)
                  _visitors_fty
              in
              Question_optional { binder = _visitors_r0; ty = _visitors_r1 }

        method visit_parameter : _ -> parameter -> parameter =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Discard_positional { ty = _visitors_fty; loc_ = _visitors_floc_ }
               ->
                 self#visit_Discard_positional env _visitors_fty _visitors_floc_
             | Positional { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Positional env _visitors_fbinder _visitors_fty
             | Labelled { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Labelled env _visitors_fbinder _visitors_fty
             | Optional
                 {
                   binder = _visitors_fbinder;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                 } ->
                 self#visit_Optional env _visitors_fbinder _visitors_fdefault
                   _visitors_fty
             | Question_optional
                 { binder = _visitors_fbinder; ty = _visitors_fty } ->
                 self#visit_Question_optional env _visitors_fbinder
                   _visitors_fty

        method visit_Lambda :
            _ ->
            parameters ->
            location ->
            expr ->
            (typ * error_typ) option ->
            fn_kind ->
            location option ->
            bool ->
            func =
          fun env ->
            fun _visitors_fparameters ->
             fun _visitors_fparams_loc_ ->
              fun _visitors_fbody ->
               fun _visitors_freturn_type ->
                fun _visitors_fkind_ ->
                 fun _visitors_fhas_error ->
                  fun _visitors_fis_async ->
                   let _visitors_r0 =
                     self#visit_parameters env _visitors_fparameters
                   in
                   let _visitors_r1 =
                     (fun _visitors_this -> _visitors_this)
                       _visitors_fparams_loc_
                   in
                   let _visitors_r2 = self#visit_expr env _visitors_fbody in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           Some
                             ((fun (_visitors_c0, _visitors_c1) ->
                                let _visitors_r0 =
                                  self#visit_typ env _visitors_c0
                                in
                                let _visitors_r1 =
                                  self#visit_error_typ env _visitors_c1
                                in
                                (_visitors_r0, _visitors_r1))
                                t)
                       | None -> None)
                       _visitors_freturn_type
                   in
                   let _visitors_r4 = self#visit_fn_kind env _visitors_fkind_ in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           Some ((fun _visitors_this -> _visitors_this) t)
                       | None -> None)
                       _visitors_fhas_error
                   in
                   let _visitors_r6 =
                     (fun _visitors_this -> _visitors_this) _visitors_fis_async
                   in
                   Lambda
                     {
                       parameters = _visitors_r0;
                       params_loc_ = _visitors_r1;
                       body = _visitors_r2;
                       return_type = _visitors_r3;
                       kind_ = _visitors_r4;
                       has_error = _visitors_r5;
                       is_async = _visitors_r6;
                     }

        method visit_Match :
            _ ->
            multi_arg_case list ->
            location option ->
            bool ->
            location ->
            location ->
            func =
          fun env ->
            fun _visitors_fcases ->
             fun _visitors_fhas_error ->
              fun _visitors_fis_async ->
               fun _visitors_ffn_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this
                       (self#visit_multi_arg_case env))
                     _visitors_fcases
                 in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((fun _visitors_this -> _visitors_this) t)
                     | None -> None)
                     _visitors_fhas_error
                 in
                 let _visitors_r2 =
                   (fun _visitors_this -> _visitors_this) _visitors_fis_async
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_ffn_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Match
                   {
                     cases = _visitors_r0;
                     has_error = _visitors_r1;
                     is_async = _visitors_r2;
                     fn_loc_ = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_func : _ -> func -> func =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Lambda
                 {
                   parameters = _visitors_fparameters;
                   params_loc_ = _visitors_fparams_loc_;
                   body = _visitors_fbody;
                   return_type = _visitors_freturn_type;
                   kind_ = _visitors_fkind_;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                 } ->
                 self#visit_Lambda env _visitors_fparameters
                   _visitors_fparams_loc_ _visitors_fbody _visitors_freturn_type
                   _visitors_fkind_ _visitors_fhas_error _visitors_fis_async
             | Match
                 {
                   cases = _visitors_fcases;
                   has_error = _visitors_fhas_error;
                   is_async = _visitors_fis_async;
                   fn_loc_ = _visitors_ffn_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Match env _visitors_fcases _visitors_fhas_error
                   _visitors_fis_async _visitors_ffn_loc_ _visitors_floc_

        method visit_case : _ -> case -> case =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_pattern env _visitors_this.pattern in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_expr env) t)
                 | None -> None)
                 _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             {
               pattern = _visitors_r0;
               guard = _visitors_r1;
               body = _visitors_r2;
             }

        method visit_multi_arg_case : _ -> multi_arg_case -> multi_arg_case =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_pattern env))
                 _visitors_this.patterns
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_expr env) t)
                 | None -> None)
                 _visitors_this.guard
             in
             let _visitors_r2 = self#visit_expr env _visitors_this.body in
             {
               patterns = _visitors_r0;
               guard = _visitors_r1;
               body = _visitors_r2;
             }

        method visit_Elem_regular : _ -> expr -> spreadable_elem =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_expr env _visitors_c0 in
             Elem_regular _visitors_r0

        method visit_Elem_spread : _ -> expr -> location -> spreadable_elem =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Elem_spread { expr = _visitors_r0; loc_ = _visitors_r1 }

        method visit_spreadable_elem : _ -> spreadable_elem -> spreadable_elem =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Elem_regular _visitors_c0 ->
                 self#visit_Elem_regular env _visitors_c0
             | Elem_spread { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Elem_spread env _visitors_fexpr _visitors_floc_

        method visit_Map_expr_elem :
            _ -> constant -> expr -> location -> location -> map_expr_elem =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fexpr ->
              fun _visitors_fkey_loc_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this -> _visitors_this) _visitors_fkey
                in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fkey_loc_
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Map_expr_elem
                  {
                    key = _visitors_r0;
                    expr = _visitors_r1;
                    key_loc_ = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_map_expr_elem : _ -> map_expr_elem -> map_expr_elem =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_expr_elem
                 {
                   key = _visitors_fkey;
                   expr = _visitors_fexpr;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_expr_elem env _visitors_fkey _visitors_fexpr
                   _visitors_fkey_loc_ _visitors_floc_

        method visit_Error_typ : _ -> typ -> error_typ =
          fun env ->
            fun _visitors_fty ->
             let _visitors_r0 = self#visit_typ env _visitors_fty in
             Error_typ { ty = _visitors_r0 }

        method visit_Default_error_typ : _ -> location -> error_typ =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_floc_
             in
             Default_error_typ { loc_ = _visitors_r0 }

        method visit_No_error_typ : _ -> error_typ = fun env -> No_error_typ

        method visit_error_typ : _ -> error_typ -> error_typ =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Error_typ { ty = _visitors_fty } ->
                 self#visit_Error_typ env _visitors_fty
             | Default_error_typ { loc_ = _visitors_floc_ } ->
                 self#visit_Default_error_typ env _visitors_floc_
             | No_error_typ -> self#visit_No_error_typ env

        method visit_Ptype_any : _ -> location -> typ =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_floc_
             in
             Ptype_any { loc_ = _visitors_r0 }

        method visit_Ptype_arrow :
            _ -> typ list -> typ -> error_typ -> bool -> location -> typ =
          fun env ->
            fun _visitors_fty_arg ->
             fun _visitors_fty_res ->
              fun _visitors_fty_err ->
               fun _visitors_fis_async ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this (self#visit_typ env))
                     _visitors_fty_arg
                 in
                 let _visitors_r1 = self#visit_typ env _visitors_fty_res in
                 let _visitors_r2 =
                   self#visit_error_typ env _visitors_fty_err
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_fis_async
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Ptype_arrow
                   {
                     ty_arg = _visitors_r0;
                     ty_res = _visitors_r1;
                     ty_err = _visitors_r2;
                     is_async = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Ptype_tuple : _ -> typ list -> location -> typ =
          fun env ->
            fun _visitors_ftys ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_typ env))
                  _visitors_ftys
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Ptype_tuple { tys = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Ptype_name :
            _ -> constrid_loc -> typ list -> location -> typ =
          fun env ->
            fun _visitors_fconstr_id ->
             fun _visitors_ftys ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr_id
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_typ env))
                   _visitors_ftys
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ptype_name
                 {
                   constr_id = _visitors_r0;
                   tys = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ptype_option : _ -> typ -> location -> location -> typ =
          fun env ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              fun _visitors_fquestion_loc ->
               let _visitors_r0 = self#visit_typ env _visitors_fty in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_fquestion_loc
               in
               Ptype_option
                 {
                   ty = _visitors_r0;
                   loc_ = _visitors_r1;
                   question_loc = _visitors_r2;
                 }

        method visit_Ptype_object : _ -> constrid_loc -> typ =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_constrid_loc env _visitors_c0 in
             Ptype_object _visitors_r0

        method visit_typ : _ -> typ -> typ =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptype_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_any env _visitors_floc_
             | Ptype_arrow
                 {
                   ty_arg = _visitors_fty_arg;
                   ty_res = _visitors_fty_res;
                   ty_err = _visitors_fty_err;
                   is_async = _visitors_fis_async;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_arrow env _visitors_fty_arg _visitors_fty_res
                   _visitors_fty_err _visitors_fis_async _visitors_floc_
             | Ptype_tuple { tys = _visitors_ftys; loc_ = _visitors_floc_ } ->
                 self#visit_Ptype_tuple env _visitors_ftys _visitors_floc_
             | Ptype_name
                 {
                   constr_id = _visitors_fconstr_id;
                   tys = _visitors_ftys;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptype_name env _visitors_fconstr_id _visitors_ftys
                   _visitors_floc_
             | Ptype_option
                 {
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                   question_loc = _visitors_fquestion_loc;
                 } ->
                 self#visit_Ptype_option env _visitors_fty _visitors_floc_
                   _visitors_fquestion_loc
             | Ptype_object _visitors_c0 ->
                 self#visit_Ptype_object env _visitors_c0

        method visit_Ppat_alias : _ -> pattern -> binder -> location -> pattern
            =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_falias ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_binder env _visitors_falias in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ppat_alias
                 {
                   pat = _visitors_r0;
                   alias = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ppat_any : _ -> location -> pattern =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_floc_
             in
             Ppat_any { loc_ = _visitors_r0 }

        method visit_Ppat_array : _ -> array_patterns -> location -> pattern =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_array_patterns env _visitors_fpats
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Ppat_array { pats = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Ppat_constant : _ -> constant -> location -> pattern =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_fc
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Ppat_constant { c = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Ppat_constraint :
            _ -> pattern -> typ -> location -> pattern =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ppat_constraint
                 { pat = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Ppat_constr :
            _ ->
            constructor ->
            constr_pat_arg list option ->
            bool ->
            location ->
            pattern =
          fun env ->
            fun _visitors_fconstr ->
             fun _visitors_fargs ->
              fun _visitors_fis_open ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_constructor env _visitors_fconstr
                in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t ->
                        Some
                          ((fun _visitors_this ->
                             Basic_lst.map _visitors_this
                               (self#visit_constr_pat_arg env))
                             t)
                    | None -> None)
                    _visitors_fargs
                in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_open
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Ppat_constr
                  {
                    constr = _visitors_r0;
                    args = _visitors_r1;
                    is_open = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Ppat_or : _ -> pattern -> pattern -> location -> pattern =
          fun env ->
            fun _visitors_fpat1 ->
             fun _visitors_fpat2 ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pattern env _visitors_fpat1 in
               let _visitors_r1 = self#visit_pattern env _visitors_fpat2 in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ppat_or
                 {
                   pat1 = _visitors_r0;
                   pat2 = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ppat_tuple : _ -> pattern list -> location -> pattern =
          fun env ->
            fun _visitors_fpats ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_pattern env))
                  _visitors_fpats
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Ppat_tuple { pats = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Ppat_var : _ -> binder -> pattern =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_binder env _visitors_c0 in
             Ppat_var _visitors_r0

        method visit_Ppat_record :
            _ -> field_pat list -> bool -> location -> pattern =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_field_pat env))
                   _visitors_ffields
               in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_closed
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ppat_record
                 {
                   fields = _visitors_r0;
                   is_closed = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ppat_map :
            _ -> map_pat_elem list -> bool -> location -> pattern =
          fun env ->
            fun _visitors_felems ->
             fun _visitors_fis_closed ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_map_pat_elem env))
                   _visitors_felems
               in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_closed
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ppat_map
                 {
                   elems = _visitors_r0;
                   is_closed = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ppat_range :
            _ -> pattern -> pattern -> bool -> location -> pattern =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_finclusive ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pattern env _visitors_flhs in
                let _visitors_r1 = self#visit_pattern env _visitors_frhs in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_finclusive
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Ppat_range
                  {
                    lhs = _visitors_r0;
                    rhs = _visitors_r1;
                    inclusive = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_pattern : _ -> pattern -> pattern =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ppat_alias
                 {
                   pat = _visitors_fpat;
                   alias = _visitors_falias;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_alias env _visitors_fpat _visitors_falias
                   _visitors_floc_
             | Ppat_any { loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_any env _visitors_floc_
             | Ppat_array { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_array env _visitors_fpats _visitors_floc_
             | Ppat_constant { c = _visitors_fc; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_constant env _visitors_fc _visitors_floc_
             | Ppat_constraint
                 {
                   pat = _visitors_fpat;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constraint env _visitors_fpat _visitors_fty
                   _visitors_floc_
             | Ppat_constr
                 {
                   constr = _visitors_fconstr;
                   args = _visitors_fargs;
                   is_open = _visitors_fis_open;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_constr env _visitors_fconstr _visitors_fargs
                   _visitors_fis_open _visitors_floc_
             | Ppat_or
                 {
                   pat1 = _visitors_fpat1;
                   pat2 = _visitors_fpat2;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_or env _visitors_fpat1 _visitors_fpat2
                   _visitors_floc_
             | Ppat_tuple { pats = _visitors_fpats; loc_ = _visitors_floc_ } ->
                 self#visit_Ppat_tuple env _visitors_fpats _visitors_floc_
             | Ppat_var _visitors_c0 -> self#visit_Ppat_var env _visitors_c0
             | Ppat_record
                 {
                   fields = _visitors_ffields;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_record env _visitors_ffields
                   _visitors_fis_closed _visitors_floc_
             | Ppat_map
                 {
                   elems = _visitors_felems;
                   is_closed = _visitors_fis_closed;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_map env _visitors_felems _visitors_fis_closed
                   _visitors_floc_
             | Ppat_range
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   inclusive = _visitors_finclusive;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ppat_range env _visitors_flhs _visitors_frhs
                   _visitors_finclusive _visitors_floc_

        method visit_Closed : _ -> array_pattern list -> array_patterns =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_array_pattern env))
                 _visitors_c0
             in
             Closed _visitors_r0

        method visit_Open :
            _ ->
            array_pattern list ->
            array_pattern list ->
            binder option ->
            array_patterns =
          fun env ->
            fun _visitors_c0 ->
             fun _visitors_c1 ->
              fun _visitors_c2 ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_array_pattern env))
                   _visitors_c0
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_array_pattern env))
                   _visitors_c1
               in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_binder env) t)
                   | None -> None)
                   _visitors_c2
               in
               Open (_visitors_r0, _visitors_r1, _visitors_r2)

        method visit_array_patterns : _ -> array_patterns -> array_patterns =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
             | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
                 self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

        method visit_Pattern : _ -> pattern -> array_pattern =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_pattern env _visitors_c0 in
             Pattern _visitors_r0

        method visit_String_spread : _ -> string_literal -> array_pattern =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             String_spread _visitors_r0

        method visit_String_spread_const :
            _ -> binder -> string option -> location -> array_pattern =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fpkg ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_binder env _visitors_fbinder in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((fun _visitors_this -> _visitors_this) t)
                   | None -> None)
                   _visitors_fpkg
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               String_spread_const
                 {
                   binder = _visitors_r0;
                   pkg = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_array_pattern : _ -> array_pattern -> array_pattern =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Pattern _visitors_c0 -> self#visit_Pattern env _visitors_c0
             | String_spread _visitors_c0 ->
                 self#visit_String_spread env _visitors_c0
             | String_spread_const
                 {
                   binder = _visitors_fbinder;
                   pkg = _visitors_fpkg;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_String_spread_const env _visitors_fbinder
                   _visitors_fpkg _visitors_floc_

        method visit_Field_def :
            _ -> label -> expr -> bool -> location -> field_def =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_pun
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Field_def
                  {
                    label = _visitors_r0;
                    expr = _visitors_r1;
                    is_pun = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_field_def : _ -> field_def -> field_def =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_def
                 {
                   label = _visitors_flabel;
                   expr = _visitors_fexpr;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_def env _visitors_flabel _visitors_fexpr
                   _visitors_fis_pun _visitors_floc_

        method visit_Field_pat :
            _ -> label -> pattern -> bool -> location -> field_pat =
          fun env ->
            fun _visitors_flabel ->
             fun _visitors_fpattern ->
              fun _visitors_fis_pun ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_label env _visitors_flabel in
                let _visitors_r1 = self#visit_pattern env _visitors_fpattern in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_pun
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Field_pat
                  {
                    label = _visitors_r0;
                    pattern = _visitors_r1;
                    is_pun = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_field_pat : _ -> field_pat -> field_pat =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Field_pat
                 {
                   label = _visitors_flabel;
                   pattern = _visitors_fpattern;
                   is_pun = _visitors_fis_pun;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Field_pat env _visitors_flabel _visitors_fpattern
                   _visitors_fis_pun _visitors_floc_

        method visit_Constr_pat_arg :
            _ -> pattern -> argument_kind -> constr_pat_arg =
          fun env ->
            fun _visitors_fpat ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_pattern env _visitors_fpat in
              let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
              Constr_pat_arg { pat = _visitors_r0; kind = _visitors_r1 }

        method visit_constr_pat_arg : _ -> constr_pat_arg -> constr_pat_arg =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Constr_pat_arg { pat = _visitors_fpat; kind = _visitors_fkind }
               ->
                 self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind

        method visit_Map_pat_elem :
            _ ->
            constant ->
            pattern ->
            bool ->
            location ->
            location ->
            map_pat_elem =
          fun env ->
            fun _visitors_fkey ->
             fun _visitors_fpat ->
              fun _visitors_fmatch_absent ->
               fun _visitors_fkey_loc_ ->
                fun _visitors_floc_ ->
                 let _visitors_r0 =
                   (fun _visitors_this -> _visitors_this) _visitors_fkey
                 in
                 let _visitors_r1 = self#visit_pattern env _visitors_fpat in
                 let _visitors_r2 =
                   (fun _visitors_this -> _visitors_this)
                     _visitors_fmatch_absent
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_fkey_loc_
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Map_pat_elem
                   {
                     key = _visitors_r0;
                     pat = _visitors_r1;
                     match_absent = _visitors_r2;
                     key_loc_ = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_map_pat_elem : _ -> map_pat_elem -> map_pat_elem =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Map_pat_elem
                 {
                   key = _visitors_fkey;
                   pat = _visitors_fpat;
                   match_absent = _visitors_fmatch_absent;
                   key_loc_ = _visitors_fkey_loc_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Map_pat_elem env _visitors_fkey _visitors_fpat
                   _visitors_fmatch_absent _visitors_fkey_loc_ _visitors_floc_

        method visit_constr_param : _ -> constr_param -> constr_param =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.cparam_typ in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_this.cparam_mut
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_label env) t)
                 | None -> None)
                 _visitors_this.cparam_label
             in
             {
               cparam_typ = _visitors_r0;
               cparam_mut = _visitors_r1;
               cparam_label = _visitors_r2;
             }

        method visit_constr_decl : _ -> constr_decl -> constr_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_constr_name env _visitors_this.constr_name
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     Some
                       ((fun _visitors_this ->
                          Basic_lst.map _visitors_this
                            (self#visit_constr_param env))
                          t)
                 | None -> None)
                 _visitors_this.constr_args
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     Some
                       ((fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            (fun _visitors_this -> _visitors_this) _visitors_c0
                          in
                          let _visitors_r1 =
                            (fun _visitors_this -> _visitors_this) _visitors_c1
                          in
                          (_visitors_r0, _visitors_r1))
                          t)
                 | None -> None)
                 _visitors_this.constr_tag
             in
             let _visitors_r3 =
               (fun _visitors_this -> _visitors_this) _visitors_this.constr_loc_
             in
             {
               constr_name = _visitors_r0;
               constr_args = _visitors_r1;
               constr_tag = _visitors_r2;
               constr_loc_ = _visitors_r3;
             }

        method visit_field_name : _ -> field_name -> field_name =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_this.label
             in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_this.loc_
             in
             { label = _visitors_r0; loc_ = _visitors_r1 }

        method visit_field_decl : _ -> field_decl -> field_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_field_name env _visitors_this.field_name
             in
             let _visitors_r1 = self#visit_typ env _visitors_this.field_ty in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_this.field_mut
             in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.field_vis
             in
             let _visitors_r4 =
               (fun _visitors_this -> _visitors_this) _visitors_this.field_loc_
             in
             {
               field_name = _visitors_r0;
               field_ty = _visitors_r1;
               field_mut = _visitors_r2;
               field_vis = _visitors_r3;
               field_loc_ = _visitors_r4;
             }

        method visit_No_payload : _ -> exception_decl = fun env -> No_payload

        method visit_Single_payload : _ -> typ -> exception_decl =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             Single_payload _visitors_r0

        method visit_Enum_payload : _ -> constr_decl list -> exception_decl =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_constr_decl env))
                 _visitors_c0
             in
             Enum_payload _visitors_r0

        method visit_exception_decl : _ -> exception_decl -> exception_decl =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | No_payload -> self#visit_No_payload env
             | Single_payload _visitors_c0 ->
                 self#visit_Single_payload env _visitors_c0
             | Enum_payload _visitors_c0 ->
                 self#visit_Enum_payload env _visitors_c0

        method visit_Ptd_abstract : _ -> type_desc = fun env -> Ptd_abstract
        method visit_Ptd_extern : _ -> type_desc = fun env -> Ptd_extern

        method visit_Ptd_newtype : _ -> typ -> type_desc =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             Ptd_newtype _visitors_r0

        method visit_Ptd_error : _ -> exception_decl -> type_desc =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
             Ptd_error _visitors_r0

        method visit_Ptd_variant : _ -> constr_decl list -> type_desc =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_constr_decl env))
                 _visitors_c0
             in
             Ptd_variant _visitors_r0

        method visit_Ptd_record : _ -> field_decl list -> type_desc =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_field_decl env))
                 _visitors_c0
             in
             Ptd_record _visitors_r0

        method visit_Ptd_alias : _ -> typ -> type_desc =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_typ env _visitors_c0 in
             Ptd_alias _visitors_r0

        method visit_type_desc : _ -> type_desc -> type_desc =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptd_abstract -> self#visit_Ptd_abstract env
             | Ptd_extern -> self#visit_Ptd_extern env
             | Ptd_newtype _visitors_c0 ->
                 self#visit_Ptd_newtype env _visitors_c0
             | Ptd_error _visitors_c0 -> self#visit_Ptd_error env _visitors_c0
             | Ptd_variant _visitors_c0 ->
                 self#visit_Ptd_variant env _visitors_c0
             | Ptd_record _visitors_c0 -> self#visit_Ptd_record env _visitors_c0
             | Ptd_alias _visitors_c0 -> self#visit_Ptd_alias env _visitors_c0

        method visit_type_decl : _ -> type_decl -> type_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_this.tycon
             in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_this.tycon_loc_
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_type_decl_binder env))
                 _visitors_this.params
             in
             let _visitors_r3 =
               self#visit_type_desc env _visitors_this.components
             in
             let _visitors_r4 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_attribute env))
                 _visitors_this.attrs
             in
             let _visitors_r5 = self#visit_docstring env _visitors_this.doc_ in
             let _visitors_r6 =
               self#visit_visibility env _visitors_this.type_vis
             in
             let _visitors_r7 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this
                   (self#visit_deriving_directive env))
                 _visitors_this.deriving_
             in
             let _visitors_r8 =
               (fun _visitors_this -> _visitors_this) _visitors_this.loc_
             in
             {
               tycon = _visitors_r0;
               tycon_loc_ = _visitors_r1;
               params = _visitors_r2;
               components = _visitors_r3;
               attrs = _visitors_r4;
               doc_ = _visitors_r5;
               type_vis = _visitors_r6;
               deriving_ = _visitors_r7;
               loc_ = _visitors_r8;
             }

        method visit_local_type_decl : _ -> local_type_decl -> local_type_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_this.local_tycon
             in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this)
                 _visitors_this.local_tycon_loc_
             in
             let _visitors_r2 =
               self#visit_type_desc env _visitors_this.local_components
             in
             let _visitors_r3 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this
                   (self#visit_deriving_directive env))
                 _visitors_this.deriving_
             in
             {
               local_tycon = _visitors_r0;
               local_tycon_loc_ = _visitors_r1;
               local_components = _visitors_r2;
               deriving_ = _visitors_r3;
             }

        method visit_deriving_directive :
            _ -> deriving_directive -> deriving_directive =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_type_name env _visitors_this.type_name_
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_argument env))
                 _visitors_this.args
             in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_this.loc_
             in
             {
               type_name_ = _visitors_r0;
               args = _visitors_r1;
               loc_ = _visitors_r2;
             }

        method visit_Vis_default : _ -> visibility = fun env -> Vis_default

        method visit_Vis_pub : _ -> string option -> location -> visibility =
          fun env ->
            fun _visitors_fattr ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((fun _visitors_this -> _visitors_this) t)
                  | None -> None)
                  _visitors_fattr
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Vis_pub { attr = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Vis_priv : _ -> location -> visibility =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_floc_
             in
             Vis_priv { loc_ = _visitors_r0 }

        method visit_visibility : _ -> visibility -> visibility =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Vis_default -> self#visit_Vis_default env
             | Vis_pub { attr = _visitors_fattr; loc_ = _visitors_floc_ } ->
                 self#visit_Vis_pub env _visitors_fattr _visitors_floc_
             | Vis_priv { loc_ = _visitors_floc_ } ->
                 self#visit_Vis_priv env _visitors_floc_

        method visit_Import :
            _ -> string_literal -> string_literal -> func_stubs =
          fun env ->
            fun _visitors_fmodule_name ->
             fun _visitors_ffunc_name ->
              let _visitors_r0 =
                self#visit_string_literal env _visitors_fmodule_name
              in
              let _visitors_r1 =
                self#visit_string_literal env _visitors_ffunc_name
              in
              Import { module_name = _visitors_r0; func_name = _visitors_r1 }

        method visit_Embedded :
            _ -> string_literal option -> embedded_code -> func_stubs =
          fun env ->
            fun _visitors_flanguage ->
             fun _visitors_fcode ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_string_literal env) t)
                  | None -> None)
                  _visitors_flanguage
              in
              let _visitors_r1 = self#visit_embedded_code env _visitors_fcode in
              Embedded { language = _visitors_r0; code = _visitors_r1 }

        method visit_func_stubs : _ -> func_stubs -> func_stubs =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Import
                 {
                   module_name = _visitors_fmodule_name;
                   func_name = _visitors_ffunc_name;
                 } ->
                 self#visit_Import env _visitors_fmodule_name
                   _visitors_ffunc_name
             | Embedded
                 { language = _visitors_flanguage; code = _visitors_fcode } ->
                 self#visit_Embedded env _visitors_flanguage _visitors_fcode

        method visit_Code_string : _ -> string_literal -> embedded_code =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_string_literal env _visitors_c0 in
             Code_string _visitors_r0

        method visit_Code_multiline_string : _ -> string list -> embedded_code =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (fun _visitors_this ->
                     _visitors_this))
                 _visitors_c0
             in
             Code_multiline_string _visitors_r0

        method visit_embedded_code : _ -> embedded_code -> embedded_code =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Code_string _visitors_c0 ->
                 self#visit_Code_string env _visitors_c0
             | Code_multiline_string _visitors_c0 ->
                 self#visit_Code_multiline_string env _visitors_c0

        method visit_Decl_body : _ -> local_type_decl list -> expr -> decl_body
            =
          fun env ->
            fun _visitors_flocal_types ->
             fun _visitors_fexpr ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_local_type_decl env))
                  _visitors_flocal_types
              in
              let _visitors_r1 = self#visit_expr env _visitors_fexpr in
              Decl_body { local_types = _visitors_r0; expr = _visitors_r1 }

        method visit_Decl_stubs : _ -> func_stubs -> decl_body =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
             Decl_stubs _visitors_r0

        method visit_decl_body : _ -> decl_body -> decl_body =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Decl_body
                 {
                   local_types = _visitors_flocal_types;
                   expr = _visitors_fexpr;
                 } ->
                 self#visit_Decl_body env _visitors_flocal_types _visitors_fexpr
             | Decl_stubs _visitors_c0 -> self#visit_Decl_stubs env _visitors_c0

        method visit_fun_decl : _ -> fun_decl -> fun_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_type_name env) t)
                 | None -> None)
                 _visitors_this.type_name
             in
             let _visitors_r1 = self#visit_binder env _visitors_this.name in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((fun _visitors_this -> _visitors_this) t)
                 | None -> None)
                 _visitors_this.has_error
             in
             let _visitors_r3 =
               (fun _visitors_this -> _visitors_this) _visitors_this.is_async
             in
             let _visitors_r4 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_parameters env) t)
                 | None -> None)
                 _visitors_this.decl_params
             in
             let _visitors_r5 =
               (fun _visitors_this -> _visitors_this) _visitors_this.params_loc_
             in
             let _visitors_r6 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_tvar_binder env))
                 _visitors_this.quantifiers
             in
             let _visitors_r7 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     Some
                       ((fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 = self#visit_typ env _visitors_c0 in
                          let _visitors_r1 =
                            self#visit_error_typ env _visitors_c1
                          in
                          (_visitors_r0, _visitors_r1))
                          t)
                 | None -> None)
                 _visitors_this.return_type
             in
             let _visitors_r8 = self#visit_visibility env _visitors_this.vis in
             let _visitors_r9 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_attribute env))
                 _visitors_this.attrs
             in
             let _visitors_r10 = self#visit_docstring env _visitors_this.doc_ in
             {
               type_name = _visitors_r0;
               name = _visitors_r1;
               has_error = _visitors_r2;
               is_async = _visitors_r3;
               decl_params = _visitors_r4;
               params_loc_ = _visitors_r5;
               quantifiers = _visitors_r6;
               return_type = _visitors_r7;
               vis = _visitors_r8;
               attrs = _visitors_r9;
               doc_ = _visitors_r10;
             }

        method visit_trait_method_param :
            _ -> trait_method_param -> trait_method_param =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_typ env _visitors_this.tmparam_typ in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_label env) t)
                 | None -> None)
                 _visitors_this.tmparam_label
             in
             { tmparam_typ = _visitors_r0; tmparam_label = _visitors_r1 }

        method visit_Trait_method :
            _ ->
            binder ->
            bool ->
            tvar_binder list ->
            trait_method_param list ->
            (typ * error_typ) option ->
            location ->
            trait_method_decl =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_fhas_error ->
              fun _visitors_fquantifiers ->
               fun _visitors_fparams ->
                fun _visitors_freturn_type ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 =
                    (fun _visitors_this -> _visitors_this) _visitors_fhas_error
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_tvar_binder env))
                      _visitors_fquantifiers
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this
                        (self#visit_trait_method_param env))
                      _visitors_fparams
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t ->
                          Some
                            ((fun (_visitors_c0, _visitors_c1) ->
                               let _visitors_r0 =
                                 self#visit_typ env _visitors_c0
                               in
                               let _visitors_r1 =
                                 self#visit_error_typ env _visitors_c1
                               in
                               (_visitors_r0, _visitors_r1))
                               t)
                      | None -> None)
                      _visitors_freturn_type
                  in
                  let _visitors_r5 =
                    (fun _visitors_this -> _visitors_this) _visitors_floc_
                  in
                  Trait_method
                    {
                      name = _visitors_r0;
                      has_error = _visitors_r1;
                      quantifiers = _visitors_r2;
                      params = _visitors_r3;
                      return_type = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_trait_method_decl :
            _ -> trait_method_decl -> trait_method_decl =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Trait_method
                 {
                   name = _visitors_fname;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   return_type = _visitors_freturn_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Trait_method env _visitors_fname
                   _visitors_fhas_error _visitors_fquantifiers _visitors_fparams
                   _visitors_freturn_type _visitors_floc_

        method visit_trait_decl : _ -> trait_decl -> trait_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_binder env _visitors_this.trait_name
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_tvar_constraint env))
                 _visitors_this.trait_supers
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_trait_method_decl env))
                 _visitors_this.trait_methods
             in
             let _visitors_r3 =
               self#visit_visibility env _visitors_this.trait_vis
             in
             let _visitors_r4 =
               (fun _visitors_this -> _visitors_this) _visitors_this.trait_loc_
             in
             let _visitors_r5 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_attribute env))
                 _visitors_this.trait_attrs
             in
             let _visitors_r6 =
               self#visit_docstring env _visitors_this.trait_doc_
             in
             {
               trait_name = _visitors_r0;
               trait_supers = _visitors_r1;
               trait_methods = _visitors_r2;
               trait_vis = _visitors_r3;
               trait_loc_ = _visitors_r4;
               trait_attrs = _visitors_r5;
               trait_doc_ = _visitors_r6;
             }

        method visit_Ptop_expr :
            _ -> expr -> bool -> local_type_decl list -> absolute_loc -> impl =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_flocal_types ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_main
                in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this
                      (self#visit_local_type_decl env))
                    _visitors_flocal_types
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_floc_
                in
                Ptop_expr
                  {
                    expr = _visitors_r0;
                    is_main = _visitors_r1;
                    local_types = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Ptop_test :
            _ ->
            expr ->
            test_name ->
            parameters option ->
            local_type_decl list ->
            absolute_loc ->
            attribute list ->
            docstring ->
            impl =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fname ->
              fun _visitors_fparams ->
               fun _visitors_flocal_types ->
                fun _visitors_floc_ ->
                 fun _visitors_fattrs ->
                  fun _visitors_fdoc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                   let _visitors_r1 =
                     (fun _visitors_this -> _visitors_this) _visitors_fname
                   in
                   let _visitors_r2 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_parameters env) t)
                       | None -> None)
                       _visitors_fparams
                   in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this
                         (self#visit_local_type_decl env))
                       _visitors_flocal_types
                   in
                   let _visitors_r4 =
                     (fun _visitors_this -> _visitors_this) _visitors_floc_
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this (self#visit_attribute env))
                       _visitors_fattrs
                   in
                   let _visitors_r6 =
                     self#visit_docstring env _visitors_fdoc_
                   in
                   Ptop_test
                     {
                       expr = _visitors_r0;
                       name = _visitors_r1;
                       params = _visitors_r2;
                       local_types = _visitors_r3;
                       loc_ = _visitors_r4;
                       attrs = _visitors_r5;
                       doc_ = _visitors_r6;
                     }

        method visit_Ptop_typedef : _ -> type_decl -> impl =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_type_decl env _visitors_c0 in
             Ptop_typedef _visitors_r0

        method visit_Ptop_funcdef :
            _ -> fun_decl -> decl_body -> absolute_loc -> impl =
          fun env ->
            fun _visitors_ffun_decl ->
             fun _visitors_fdecl_body ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
               let _visitors_r1 =
                 self#visit_decl_body env _visitors_fdecl_body
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Ptop_funcdef
                 {
                   fun_decl = _visitors_r0;
                   decl_body = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Ptop_letdef :
            _ ->
            binder ->
            typ option ->
            expr ->
            visibility ->
            bool ->
            absolute_loc ->
            attribute list ->
            docstring ->
            impl =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fty ->
              fun _visitors_fexpr ->
               fun _visitors_fvis ->
                fun _visitors_fis_constant ->
                 fun _visitors_floc_ ->
                  fun _visitors_fattrs ->
                   fun _visitors_fdoc_ ->
                    let _visitors_r0 =
                      self#visit_binder env _visitors_fbinder
                    in
                    let _visitors_r1 =
                      (fun _visitors_this ->
                        match _visitors_this with
                        | Some t -> Some ((self#visit_typ env) t)
                        | None -> None)
                        _visitors_fty
                    in
                    let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                    let _visitors_r3 =
                      self#visit_visibility env _visitors_fvis
                    in
                    let _visitors_r4 =
                      (fun _visitors_this -> _visitors_this)
                        _visitors_fis_constant
                    in
                    let _visitors_r5 =
                      (fun _visitors_this -> _visitors_this) _visitors_floc_
                    in
                    let _visitors_r6 =
                      (fun _visitors_this ->
                        Basic_lst.map _visitors_this (self#visit_attribute env))
                        _visitors_fattrs
                    in
                    let _visitors_r7 =
                      self#visit_docstring env _visitors_fdoc_
                    in
                    Ptop_letdef
                      {
                        binder = _visitors_r0;
                        ty = _visitors_r1;
                        expr = _visitors_r2;
                        vis = _visitors_r3;
                        is_constant = _visitors_r4;
                        loc_ = _visitors_r5;
                        attrs = _visitors_r6;
                        doc_ = _visitors_r7;
                      }

        method visit_Ptop_trait : _ -> trait_decl -> impl =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_trait_decl env _visitors_c0 in
             Ptop_trait _visitors_r0

        method visit_Ptop_trait_alias :
            _ ->
            binder ->
            type_name ->
            visibility ->
            absolute_loc ->
            attribute list ->
            docstring ->
            impl =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ftarget ->
              fun _visitors_fvis ->
               fun _visitors_floc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fdoc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_type_name env _visitors_ftarget
                  in
                  let _visitors_r2 = self#visit_visibility env _visitors_fvis in
                  let _visitors_r3 =
                    (fun _visitors_this -> _visitors_this) _visitors_floc_
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_attribute env))
                      _visitors_fattrs
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  Ptop_trait_alias
                    {
                      binder = _visitors_r0;
                      target = _visitors_r1;
                      vis = _visitors_r2;
                      loc_ = _visitors_r3;
                      attrs = _visitors_r4;
                      doc_ = _visitors_r5;
                    }

        method visit_Ptop_impl :
            _ ->
            typ option ->
            type_name ->
            binder ->
            bool ->
            tvar_binder list ->
            parameters ->
            (typ * error_typ) option ->
            decl_body ->
            visibility ->
            absolute_loc ->
            location ->
            attribute list ->
            docstring ->
            impl =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fmethod_name ->
               fun _visitors_fhas_error ->
                fun _visitors_fquantifiers ->
                 fun _visitors_fparams ->
                  fun _visitors_fret_ty ->
                   fun _visitors_fbody ->
                    fun _visitors_fvis ->
                     fun _visitors_floc_ ->
                      fun _visitors_fheader_loc_ ->
                       fun _visitors_fattrs ->
                        fun _visitors_fdoc_ ->
                         let _visitors_r0 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t -> Some ((self#visit_typ env) t)
                             | None -> None)
                             _visitors_fself_ty
                         in
                         let _visitors_r1 =
                           self#visit_type_name env _visitors_ftrait
                         in
                         let _visitors_r2 =
                           self#visit_binder env _visitors_fmethod_name
                         in
                         let _visitors_r3 =
                           (fun _visitors_this -> _visitors_this)
                             _visitors_fhas_error
                         in
                         let _visitors_r4 =
                           (fun _visitors_this ->
                             Basic_lst.map _visitors_this
                               (self#visit_tvar_binder env))
                             _visitors_fquantifiers
                         in
                         let _visitors_r5 =
                           self#visit_parameters env _visitors_fparams
                         in
                         let _visitors_r6 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t ->
                                 Some
                                   ((fun (_visitors_c0, _visitors_c1) ->
                                      let _visitors_r0 =
                                        self#visit_typ env _visitors_c0
                                      in
                                      let _visitors_r1 =
                                        self#visit_error_typ env _visitors_c1
                                      in
                                      (_visitors_r0, _visitors_r1))
                                      t)
                             | None -> None)
                             _visitors_fret_ty
                         in
                         let _visitors_r7 =
                           self#visit_decl_body env _visitors_fbody
                         in
                         let _visitors_r8 =
                           self#visit_visibility env _visitors_fvis
                         in
                         let _visitors_r9 =
                           (fun _visitors_this -> _visitors_this)
                             _visitors_floc_
                         in
                         let _visitors_r10 =
                           (fun _visitors_this -> _visitors_this)
                             _visitors_fheader_loc_
                         in
                         let _visitors_r11 =
                           (fun _visitors_this ->
                             Basic_lst.map _visitors_this
                               (self#visit_attribute env))
                             _visitors_fattrs
                         in
                         let _visitors_r12 =
                           self#visit_docstring env _visitors_fdoc_
                         in
                         Ptop_impl
                           {
                             self_ty = _visitors_r0;
                             trait = _visitors_r1;
                             method_name = _visitors_r2;
                             has_error = _visitors_r3;
                             quantifiers = _visitors_r4;
                             params = _visitors_r5;
                             ret_ty = _visitors_r6;
                             body = _visitors_r7;
                             vis = _visitors_r8;
                             loc_ = _visitors_r9;
                             header_loc_ = _visitors_r10;
                             attrs = _visitors_r11;
                             doc_ = _visitors_r12;
                           }

        method visit_Ptop_impl_relation :
            _ ->
            typ ->
            type_name ->
            tvar_binder list ->
            bool ->
            absolute_loc ->
            impl =
          fun env ->
            fun _visitors_fself_ty ->
             fun _visitors_ftrait ->
              fun _visitors_fquantifiers ->
               fun _visitors_fis_pub ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
                 let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this (self#visit_tvar_binder env))
                     _visitors_fquantifiers
                 in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_fis_pub
                 in
                 let _visitors_r4 =
                   (fun _visitors_this -> _visitors_this) _visitors_floc_
                 in
                 Ptop_impl_relation
                   {
                     self_ty = _visitors_r0;
                     trait = _visitors_r1;
                     quantifiers = _visitors_r2;
                     is_pub = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_impl : _ -> impl -> impl =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ptop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_flocal_types _visitors_floc_
             | Ptop_test
                 {
                   expr = _visitors_fexpr;
                   name = _visitors_fname;
                   params = _visitors_fparams;
                   local_types = _visitors_flocal_types;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_test env _visitors_fexpr _visitors_fname
                   _visitors_fparams _visitors_flocal_types _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_typedef _visitors_c0 ->
                 self#visit_Ptop_typedef env _visitors_c0
             | Ptop_funcdef
                 {
                   fun_decl = _visitors_ffun_decl;
                   decl_body = _visitors_fdecl_body;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_funcdef env _visitors_ffun_decl
                   _visitors_fdecl_body _visitors_floc_
             | Ptop_letdef
                 {
                   binder = _visitors_fbinder;
                   ty = _visitors_fty;
                   expr = _visitors_fexpr;
                   vis = _visitors_fvis;
                   is_constant = _visitors_fis_constant;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_letdef env _visitors_fbinder _visitors_fty
                   _visitors_fexpr _visitors_fvis _visitors_fis_constant
                   _visitors_floc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_trait _visitors_c0 -> self#visit_Ptop_trait env _visitors_c0
             | Ptop_trait_alias
                 {
                   binder = _visitors_fbinder;
                   target = _visitors_ftarget;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_trait_alias env _visitors_fbinder
                   _visitors_ftarget _visitors_fvis _visitors_floc_
                   _visitors_fattrs _visitors_fdoc_
             | Ptop_impl
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   method_name = _visitors_fmethod_name;
                   has_error = _visitors_fhas_error;
                   quantifiers = _visitors_fquantifiers;
                   params = _visitors_fparams;
                   ret_ty = _visitors_fret_ty;
                   body = _visitors_fbody;
                   vis = _visitors_fvis;
                   loc_ = _visitors_floc_;
                   header_loc_ = _visitors_fheader_loc_;
                   attrs = _visitors_fattrs;
                   doc_ = _visitors_fdoc_;
                 } ->
                 self#visit_Ptop_impl env _visitors_fself_ty _visitors_ftrait
                   _visitors_fmethod_name _visitors_fhas_error
                   _visitors_fquantifiers _visitors_fparams _visitors_fret_ty
                   _visitors_fbody _visitors_fvis _visitors_floc_
                   _visitors_fheader_loc_ _visitors_fattrs _visitors_fdoc_
             | Ptop_impl_relation
                 {
                   self_ty = _visitors_fself_ty;
                   trait = _visitors_ftrait;
                   quantifiers = _visitors_fquantifiers;
                   is_pub = _visitors_fis_pub;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ptop_impl_relation env _visitors_fself_ty
                   _visitors_ftrait _visitors_fquantifiers _visitors_fis_pub
                   _visitors_floc_

        method visit_Interp_lit :
            _ -> string -> string -> location -> interp_elem =
          fun env ->
            fun _visitors_fstr ->
             fun _visitors_frepr ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this -> _visitors_this) _visitors_fstr
               in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_frepr
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_floc_
               in
               Interp_lit
                 {
                   str = _visitors_r0;
                   repr = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Interp_expr : _ -> expr -> location -> interp_elem =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_floc_
              in
              Interp_expr { expr = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Interp_source : _ -> Literal.interp_source -> interp_elem =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_interp_source env _visitors_c0 in
             Interp_source _visitors_r0

        method visit_interp_elem : _ -> interp_elem -> interp_elem =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Interp_lit
                 {
                   str = _visitors_fstr;
                   repr = _visitors_frepr;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Interp_lit env _visitors_fstr _visitors_frepr
                   _visitors_floc_
             | Interp_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Interp_expr env _visitors_fexpr _visitors_floc_
             | Interp_source _visitors_c0 ->
                 self#visit_Interp_source env _visitors_c0

        method visit_Multiline_string : _ -> string -> multiline_string_elem =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_c0
             in
             Multiline_string _visitors_r0

        method visit_Multiline_interp :
            _ -> interp_elem list -> multiline_string_elem =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_interp_elem env))
                 _visitors_c0
             in
             Multiline_interp _visitors_r0

        method visit_multiline_string_elem :
            _ -> multiline_string_elem -> multiline_string_elem =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Multiline_string _visitors_c0 ->
                 self#visit_Multiline_string env _visitors_c0
             | Multiline_interp _visitors_c0 ->
                 self#visit_Multiline_interp env _visitors_c0

        method visit_impls : _ -> impls -> impls =
          fun env ->
            fun _visitors_this ->
             Basic_lst.map _visitors_this (self#visit_impl env)
      end

    [@@@VISITORS.END]
  end

  include struct
    let _ = fun (_ : expr) -> ()
    let _ = fun (_ : static_assertion) -> ()
    let _ = fun (_ : argument) -> ()
    let _ = fun (_ : parameters) -> ()
    let _ = fun (_ : parameter) -> ()
    let _ = fun (_ : func) -> ()
    let _ = fun (_ : case) -> ()
    let _ = fun (_ : multi_arg_case) -> ()
    let _ = fun (_ : spreadable_elem) -> ()
    let _ = fun (_ : map_expr_elem) -> ()
    let _ = fun (_ : error_typ) -> ()
    let _ = fun (_ : typ) -> ()
    let _ = fun (_ : pattern) -> ()
    let _ = fun (_ : array_patterns) -> ()
    let _ = fun (_ : array_pattern) -> ()
    let _ = fun (_ : field_def) -> ()
    let _ = fun (_ : field_pat) -> ()
    let _ = fun (_ : constr_pat_arg) -> ()
    let _ = fun (_ : map_pat_elem) -> ()
    let _ = fun (_ : constr_param) -> ()
    let _ = fun (_ : constr_decl) -> ()
    let _ = fun (_ : field_name) -> ()
    let _ = fun (_ : field_decl) -> ()
    let _ = fun (_ : exception_decl) -> ()
    let _ = fun (_ : type_desc) -> ()
    let _ = fun (_ : type_decl) -> ()
    let _ = fun (_ : local_type_decl) -> ()
    let _ = fun (_ : deriving_directive) -> ()
    let _ = fun (_ : visibility) -> ()
    let _ = fun (_ : func_stubs) -> ()
    let _ = fun (_ : embedded_code) -> ()
    let _ = fun (_ : decl_body) -> ()
    let _ = fun (_ : fun_decl) -> ()
    let _ = fun (_ : trait_method_param) -> ()
    let _ = fun (_ : trait_method_decl) -> ()
    let _ = fun (_ : trait_decl) -> ()
    let _ = fun (_ : impl) -> ()
    let _ = fun (_ : interp_elem) -> ()
    let _ = fun (_ : multiline_string_elem) -> ()
    let _ = fun (_ : impls) -> ()
  end
end

let filter_fields ctor fields =
  Basic_lst.fold_right fields [] (fun field ->
      fun acc ->
       match field with
       | ( ( "params_loc_" | "loc_" | "index_loc_" | "try_loc_" | "catch_loc_"
           | "constr_loc" | "tycon_loc_" | "local_tycon_loc_" | "constr_loc_"
           | "field_loc_" | "header_loc_" | "trait_loc_" | "question_loc"
           | "match_loc_" | "loop_loc_" | "fn_loc_" | "key_loc_" | "else_loc_"
             ),
           _ )
         when not !Basic_config.show_loc ->
           acc
       | "args_loc_", _ -> acc
       | "repr", _ -> acc
       | ("doc_" | "trait_doc_"), _ when not !Basic_config.show_doc -> acc
       | ("is_pub" | "abbreviation" | "faked"), S.Atom "false" -> acc
       | "faked", S.Atom "true" -> ("faked", S.Atom "faked") :: acc
       | ("field_vis" | "type_vis"), S.Atom "Vis_default" -> acc
       | "deriving_", List [] -> acc
       | ("ty_params_" | "type_name"), List [] -> acc
       | "is_closed", S.Atom "true" -> acc
       | "continue_block", List [] -> acc
       | "args", List [] when ctor = "Pexpr_continue" -> acc
       | "arg", List [] when ctor = "Pexpr_break" -> acc
       | "trait_supers", List [] -> acc
       | "param_kind", Atom "Positional" -> acc
       | "is_main", S.Atom "false" -> acc
       | "arg_is_pun_", Atom "false" -> acc
       | ("doc_" | "intf_doc_"), List [] -> acc
       | "pragmas", List [] -> acc
       | "kind_", Atom "Lambda" -> acc
       | "is_open", Atom "false" when ctor = "Ppat_constr" -> acc
       | "augmented_by", List [] -> acc
       | "return_self", Atom "false" -> acc
       | "return_self", Atom "true" ->
           ("return_self", List [ Atom "return_self" ]) :: acc
       | "has_error", Atom "false" -> acc
       | "has_error", List [] -> acc
       | "has_error", List (_ :: []) when not !Basic_config.show_loc ->
           ("has_error", Atom "true") :: acc
       | "extend_error", Atom "false" -> acc
       | "attr", Atom "No_attr" -> acc
       | "local_types", List [] -> acc
       | "is_constant", Atom "false" when ctor = "Ptop_letdef" -> acc
       | "is_constant", Atom "true" when ctor = "Ptop_letdef" ->
           ("is_constant", List [ Atom "is_constant" ]) :: acc
       | "label", List [] -> acc
       | "is_async", Atom "false" -> acc
       | "is_async", Atom "true" -> ("is_async", List [ Atom "async" ]) :: acc
       | ("attrs" | "trait_attrs"), List [] -> acc
       | "constr_tag", List [] -> acc
       | _ -> field :: acc)

let loc_of_impl i =
  match i with
  | Ptop_expr { loc_; _ }
  | Ptop_test { loc_; _ }
  | Ptop_typedef { loc_; _ }
  | Ptop_funcdef { loc_; _ }
  | Ptop_letdef { loc_; _ }
  | Ptop_trait { trait_loc_ = loc_; _ }
  | Ptop_trait_alias { loc_; _ }
  | Ptop_impl { loc_; _ }
  | Ptop_impl_relation { loc_; _ } ->
      loc_

let binder_name_of_param (p : parameter) =
  (match p with
   | Discard_positional _ -> "_"
   | Positional { binder; _ }
   | Labelled { binder; _ }
   | Optional { binder; _ }
   | Question_optional { binder; _ } ->
       binder.binder_name
    : string)

let ty_of_param (p : parameter) =
  (match p with
   | Discard_positional { ty; _ } -> ty
   | Positional { ty; _ } -> ty
   | Labelled { ty; _ } -> ty
   | Optional { ty; _ } -> ty
   | Question_optional { ty; _ } -> ty
    : typ option)

let loc_of_param_binder (p : parameter) =
  (match p with
   | Discard_positional { loc_; _ } -> loc_
   | Positional { binder; _ } -> binder.loc_
   | Labelled { binder; _ } -> binder.loc_
   | Optional { binder; _ } -> binder.loc_
   | Question_optional { binder; _ } -> binder.loc_
    : location)

type loc_ctx = Use_absolute_loc of absolute_loc | Use_relative_loc

let sexp =
  object (self)
    inherit [_] sexp as super

    method! visit_inline_record env ctor fields =
      super#visit_inline_record env ctor (filter_fields ctor fields)

    method! visit_record env fields =
      super#visit_record env (filter_fields "" fields)

    method! visit_docstring env docstring =
      let comment = Docstring.comment_string docstring in
      if comment = "" && Docstring.pragmas docstring = [] then S.List []
      else if Docstring.pragmas docstring = [] then S.Atom comment
      else super#visit_docstring env docstring

    method! visit_parameter env p =
      match p with
      | Positional { binder; ty } ->
          List
            [
              self#visit_binder env binder;
              Moon_sexp_conv.sexp_of_option (self#visit_typ env) ty;
            ]
      | Discard_positional _ | Labelled _ | Optional _ | Question_optional _ ->
          super#visit_parameter env p

    method! visit_argument env arg =
      match arg.arg_kind with
      | Positional -> self#visit_expr env arg.arg_value
      | kind ->
          S.List
            [ self#visit_expr env arg.arg_value; sexp_of_argument_kind kind ]

    method! visit_constr_param env cparam =
      let typ = self#visit_typ env cparam.cparam_typ in
      match cparam.cparam_label with
      | None ->
          if cparam.cparam_mut then S.List [ typ; List [ Atom "mut" ] ] else typ
      | Some label ->
          if cparam.cparam_mut then
            List
              [
                Atom "Labelled";
                self#visit_label env label;
                typ;
                List [ Atom "mut" ];
              ]
          else List [ Atom "Labelled"; self#visit_label env label; typ ]

    method! visit_Constr_pat_arg env pat kind =
      match kind with
      | Positional -> self#visit_pattern env pat
      | _ -> S.List [ self#visit_pattern env pat; sexp_of_argument_kind kind ]

    method! visit_location env loc =
      match env with
      | Use_absolute_loc base -> sexp_of_absolute_loc (Rloc.to_loc ~base loc)
      | Use_relative_loc -> super#visit_location env loc

    method! visit_impl env impl =
      match env with
      | Use_absolute_loc _ ->
          let base = loc_of_impl impl in
          super#visit_impl (Use_absolute_loc base) impl
      | Use_relative_loc -> super#visit_impl Use_relative_loc impl

    method! visit_array_pattern env pat =
      match pat with
      | Pattern p -> self#visit_pattern env p
      | String_spread s -> S.List [ Atom "String_spread"; Atom s.string_val ]
      | String_spread_const { binder; pkg = None } ->
          S.List [ Atom "String_spread_const"; Atom binder.binder_name ]
      | String_spread_const { binder; pkg = Some pkg } ->
          S.List
            [
              Atom "String_spread_const";
              Atom
                (Stdlib.String.concat ""
                   [ "@"; pkg; "."; binder.binder_name ] [@merlin.hide]);
            ]

    method! visit_case env case =
      match case with
      | { pattern = p; guard = None; body = action } ->
          S.List [ self#visit_pattern env p; self#visit_expr env action ]
      | _ -> super#visit_case env case

    method! visit_multi_arg_case env case =
      match case with
      | { patterns = p; guard = None; body = action } ->
          S.List
            [
              S.List (Basic_lst.map p (self#visit_pattern env));
              self#visit_expr env action;
            ]
      | _ -> super#visit_multi_arg_case env case
  end

let sexp_of_impl impl = sexp#visit_impl Use_relative_loc impl

let sexp_of_impls ?(use_absolute_loc = false) impls =
  let ctx =
    if use_absolute_loc then Use_absolute_loc Loc.no_location
    else Use_relative_loc
  in
  sexp#visit_impls ctx impls

let sexp_of_visibility vis = sexp#visit_visibility Use_relative_loc vis

let sexp_of_type_decl type_decl =
  sexp#visit_type_decl Use_relative_loc type_decl

let sexp_of_trait_decl trait_decl =
  sexp#visit_trait_decl Use_relative_loc trait_decl

let string_of_vis = function
  | Vis_default -> "abstract"
  | Vis_priv _ -> "private"
  | Vis_pub { attr = None } -> "public"
  | Vis_pub { attr = Some attr } -> "public " ^ attr

let string_of_apply_attr = function
  | Exclamation -> "!"
  | Question -> "?"
  | Double_exclamation -> "!!"
  | No_attr -> ""

let loc_of_expression e =
  match e with
  | Pexpr_apply { loc_; _ }
  | Pexpr_array { loc_; _ }
  | Pexpr_array_spread { loc_; _ }
  | Pexpr_array_get_slice { loc_; _ }
  | Pexpr_array_get { loc_; _ }
  | Pexpr_array_set { loc_; _ }
  | Pexpr_array_augmented_set { loc_; _ }
  | Pexpr_constant { loc_; _ }
  | Pexpr_interp { loc_; _ }
  | Pexpr_constraint { loc_; _ }
  | Pexpr_constr { loc_; _ }
  | Pexpr_while { loc_; _ }
  | Pexpr_function { loc_; _ }
  | Pexpr_ident { loc_; _ }
  | Pexpr_if { loc_; _ }
  | Pexpr_letrec { loc_; _ }
  | Pexpr_let { loc_; _ }
  | Pexpr_letfn { loc_; _ }
  | Pexpr_sequence { loc_; _ }
  | Pexpr_tuple { loc_; _ }
  | Pexpr_match { loc_; _ }
  | Pexpr_record { loc_; _ }
  | Pexpr_record_update { loc_; _ }
  | Pexpr_mutate { loc_; _ }
  | Pexpr_field { loc_; _ }
  | Pexpr_method { loc_; _ }
  | Pexpr_dot_apply { loc_; _ }
  | Pexpr_as { loc_; _ }
  | Pexpr_letmut { loc_; _ }
  | Pexpr_assign { loc_; _ }
  | Pexpr_unit { loc_; _ }
  | Pexpr_break { loc_; _ }
  | Pexpr_continue { loc_; _ }
  | Pexpr_loop { loc_; _ }
  | Pexpr_for { loc_; _ }
  | Pexpr_foreach { loc_; _ }
  | Pexpr_try { loc_; _ }
  | Pexpr_return { loc_; _ }
  | Pexpr_raise { loc_; _ }
  | Pexpr_hole { loc_; _ }
  | Pexpr_infix { loc_; _ }
  | Pexpr_unary { loc_; _ }
  | Pexpr_guard { loc_; _ }
  | Pexpr_guard_let { loc_; _ }
  | Pexpr_pipe { loc_; _ }
  | Pexpr_map { loc_; _ }
  | Pexpr_group { loc_; _ }
  | Pexpr_multiline_string { loc_; _ }
  | Pexpr_is { loc_; _ } ->
      loc_
  | Pexpr_static_assert _ -> Rloc.no_location

let loc_of_pattern p =
  match p with
  | Ppat_alias { loc_; _ }
  | Ppat_any { loc_; _ }
  | Ppat_array { loc_; _ }
  | Ppat_constant { loc_; _ }
  | Ppat_constraint { loc_; _ }
  | Ppat_constr { loc_; _ }
  | Ppat_or { loc_; _ }
  | Ppat_tuple { loc_; _ }
  | Ppat_var { loc_; _ }
  | Ppat_range { loc_; _ }
  | Ppat_record { loc_; _ }
  | Ppat_map { loc_; _ } ->
      loc_

let loc_of_type_expression te =
  match te with
  | Ptype_any { loc_ }
  | Ptype_arrow { loc_; _ }
  | Ptype_tuple { loc_; _ }
  | Ptype_name { loc_; _ }
  | Ptype_option { loc_; _ }
  | Ptype_object { loc_; _ } ->
      loc_
