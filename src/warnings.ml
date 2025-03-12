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


type loc = Loc.t
type unused_kind = Unused | No_construct | No_read

include struct
  let _ = fun (_ : unused_kind) -> ()

  let sexp_of_unused_kind =
    (function
     | Unused -> S.Atom "Unused"
     | No_construct -> S.Atom "No_construct"
     | No_read -> S.Atom "No_read"
      : unused_kind -> S.t)

  let _ = sexp_of_unused_kind
  let equal_unused_kind = (Stdlib.( = ) : unused_kind -> unused_kind -> bool)
  let _ = equal_unused_kind
  let compare_unused_kind = (Stdlib.compare : unused_kind -> unused_kind -> int)
  let _ = compare_unused_kind
end

type kind =
  | Unused_func of string
  | Unused_var of { var_name : string; is_toplevel : bool }
  | Unused_type_declaration of string
  | Unused_abstract_type of { name : string; kind : string }
  | Unused_tvar of string
  | Unused_constructor of { constr : string; kind : unused_kind }
  | Unused_field of string
  | Unused_constr_arg of { constr : string; index : int }
  | Unused_constr_field of {
      constr : string;
      label : string;
      is_mutated : bool;
    }
  | Redundant_modifier of { modifier : string; field : string }
  | Struct_never_constructed of string
  | Unused_pat
  | Partial_match of {
      hint_cases : string list;
      is_let_match : bool;
      has_guard : bool;
    }
  | Unreachable
  | Unresolved_tvar of string
  | Lowercase_type_name of string
  | Unused_mutability of string
  | Parser_inconsistency of {
      file_name : string;
      segment : string;
      is_menhir_succeed : bool;
      is_handrolled_succeed : bool;
    }
  | Useless_loop
  | Toplevel_not_left_aligned
  | Unexpected_pragmas of string
  | Omitted_constr_argument of { constr : string; labels : string list }
  | Ambiguous_block
  | Useless_try
  | Useless_error_type
  | Useless_catch_all
  | Deprecated_syntax of {
      old_usage : string;
      purpose : string;
      new_usage : string option;
    }
  | Todo
  | Unused_package of { name : string; is_alias : bool }
  | Empty_package_alias
  | Optional_arg_never_supplied of string
  | Optional_arg_always_supplied of string
  | Unused_import_value of string
  | Reserved_keyword of string
  | Loop_label_shadow of string
  | Loop_label_unused of string
  | Unused_guard
  | Closed_map_pattern
  | Invalid_attribute of string
  | Unused_attribute of string
  | Invalid_inline_wasm of string
  | Implement_trait_with_method of {
      trait : string;
      typ : string;
      methods : string list;
    }

include struct
  let _ = fun (_ : kind) -> ()

  let sexp_of_kind =
    (function
     | Unused_func arg0__005_ ->
         let res0__006_ = Moon_sexp_conv.sexp_of_string arg0__005_ in
         S.List [ S.Atom "Unused_func"; res0__006_ ]
     | Unused_var { var_name = var_name__008_; is_toplevel = is_toplevel__010_ }
       ->
         let bnds__007_ = ([] : _ Stdlib.List.t) in
         let bnds__007_ =
           let arg__011_ = Moon_sexp_conv.sexp_of_bool is_toplevel__010_ in
           (S.List [ S.Atom "is_toplevel"; arg__011_ ] :: bnds__007_
             : _ Stdlib.List.t)
         in
         let bnds__007_ =
           let arg__009_ = Moon_sexp_conv.sexp_of_string var_name__008_ in
           (S.List [ S.Atom "var_name"; arg__009_ ] :: bnds__007_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_var" :: bnds__007_)
     | Unused_type_declaration arg0__012_ ->
         let res0__013_ = Moon_sexp_conv.sexp_of_string arg0__012_ in
         S.List [ S.Atom "Unused_type_declaration"; res0__013_ ]
     | Unused_abstract_type { name = name__015_; kind = kind__017_ } ->
         let bnds__014_ = ([] : _ Stdlib.List.t) in
         let bnds__014_ =
           let arg__018_ = Moon_sexp_conv.sexp_of_string kind__017_ in
           (S.List [ S.Atom "kind"; arg__018_ ] :: bnds__014_ : _ Stdlib.List.t)
         in
         let bnds__014_ =
           let arg__016_ = Moon_sexp_conv.sexp_of_string name__015_ in
           (S.List [ S.Atom "name"; arg__016_ ] :: bnds__014_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_abstract_type" :: bnds__014_)
     | Unused_tvar arg0__019_ ->
         let res0__020_ = Moon_sexp_conv.sexp_of_string arg0__019_ in
         S.List [ S.Atom "Unused_tvar"; res0__020_ ]
     | Unused_constructor { constr = constr__022_; kind = kind__024_ } ->
         let bnds__021_ = ([] : _ Stdlib.List.t) in
         let bnds__021_ =
           let arg__025_ = sexp_of_unused_kind kind__024_ in
           (S.List [ S.Atom "kind"; arg__025_ ] :: bnds__021_ : _ Stdlib.List.t)
         in
         let bnds__021_ =
           let arg__023_ = Moon_sexp_conv.sexp_of_string constr__022_ in
           (S.List [ S.Atom "constr"; arg__023_ ] :: bnds__021_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_constructor" :: bnds__021_)
     | Unused_field arg0__026_ ->
         let res0__027_ = Moon_sexp_conv.sexp_of_string arg0__026_ in
         S.List [ S.Atom "Unused_field"; res0__027_ ]
     | Unused_constr_arg { constr = constr__029_; index = index__031_ } ->
         let bnds__028_ = ([] : _ Stdlib.List.t) in
         let bnds__028_ =
           let arg__032_ = Moon_sexp_conv.sexp_of_int index__031_ in
           (S.List [ S.Atom "index"; arg__032_ ] :: bnds__028_
             : _ Stdlib.List.t)
         in
         let bnds__028_ =
           let arg__030_ = Moon_sexp_conv.sexp_of_string constr__029_ in
           (S.List [ S.Atom "constr"; arg__030_ ] :: bnds__028_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_constr_arg" :: bnds__028_)
     | Unused_constr_field
         {
           constr = constr__034_;
           label = label__036_;
           is_mutated = is_mutated__038_;
         } ->
         let bnds__033_ = ([] : _ Stdlib.List.t) in
         let bnds__033_ =
           let arg__039_ = Moon_sexp_conv.sexp_of_bool is_mutated__038_ in
           (S.List [ S.Atom "is_mutated"; arg__039_ ] :: bnds__033_
             : _ Stdlib.List.t)
         in
         let bnds__033_ =
           let arg__037_ = Moon_sexp_conv.sexp_of_string label__036_ in
           (S.List [ S.Atom "label"; arg__037_ ] :: bnds__033_
             : _ Stdlib.List.t)
         in
         let bnds__033_ =
           let arg__035_ = Moon_sexp_conv.sexp_of_string constr__034_ in
           (S.List [ S.Atom "constr"; arg__035_ ] :: bnds__033_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_constr_field" :: bnds__033_)
     | Redundant_modifier { modifier = modifier__041_; field = field__043_ } ->
         let bnds__040_ = ([] : _ Stdlib.List.t) in
         let bnds__040_ =
           let arg__044_ = Moon_sexp_conv.sexp_of_string field__043_ in
           (S.List [ S.Atom "field"; arg__044_ ] :: bnds__040_
             : _ Stdlib.List.t)
         in
         let bnds__040_ =
           let arg__042_ = Moon_sexp_conv.sexp_of_string modifier__041_ in
           (S.List [ S.Atom "modifier"; arg__042_ ] :: bnds__040_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Redundant_modifier" :: bnds__040_)
     | Struct_never_constructed arg0__045_ ->
         let res0__046_ = Moon_sexp_conv.sexp_of_string arg0__045_ in
         S.List [ S.Atom "Struct_never_constructed"; res0__046_ ]
     | Unused_pat -> S.Atom "Unused_pat"
     | Partial_match
         {
           hint_cases = hint_cases__048_;
           is_let_match = is_let_match__050_;
           has_guard = has_guard__052_;
         } ->
         let bnds__047_ = ([] : _ Stdlib.List.t) in
         let bnds__047_ =
           let arg__053_ = Moon_sexp_conv.sexp_of_bool has_guard__052_ in
           (S.List [ S.Atom "has_guard"; arg__053_ ] :: bnds__047_
             : _ Stdlib.List.t)
         in
         let bnds__047_ =
           let arg__051_ = Moon_sexp_conv.sexp_of_bool is_let_match__050_ in
           (S.List [ S.Atom "is_let_match"; arg__051_ ] :: bnds__047_
             : _ Stdlib.List.t)
         in
         let bnds__047_ =
           let arg__049_ =
             Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
               hint_cases__048_
           in
           (S.List [ S.Atom "hint_cases"; arg__049_ ] :: bnds__047_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Partial_match" :: bnds__047_)
     | Unreachable -> S.Atom "Unreachable"
     | Unresolved_tvar arg0__054_ ->
         let res0__055_ = Moon_sexp_conv.sexp_of_string arg0__054_ in
         S.List [ S.Atom "Unresolved_tvar"; res0__055_ ]
     | Lowercase_type_name arg0__056_ ->
         let res0__057_ = Moon_sexp_conv.sexp_of_string arg0__056_ in
         S.List [ S.Atom "Lowercase_type_name"; res0__057_ ]
     | Unused_mutability arg0__058_ ->
         let res0__059_ = Moon_sexp_conv.sexp_of_string arg0__058_ in
         S.List [ S.Atom "Unused_mutability"; res0__059_ ]
     | Parser_inconsistency
         {
           file_name = file_name__061_;
           segment = segment__063_;
           is_menhir_succeed = is_menhir_succeed__065_;
           is_handrolled_succeed = is_handrolled_succeed__067_;
         } ->
         let bnds__060_ = ([] : _ Stdlib.List.t) in
         let bnds__060_ =
           let arg__068_ =
             Moon_sexp_conv.sexp_of_bool is_handrolled_succeed__067_
           in
           (S.List [ S.Atom "is_handrolled_succeed"; arg__068_ ] :: bnds__060_
             : _ Stdlib.List.t)
         in
         let bnds__060_ =
           let arg__066_ =
             Moon_sexp_conv.sexp_of_bool is_menhir_succeed__065_
           in
           (S.List [ S.Atom "is_menhir_succeed"; arg__066_ ] :: bnds__060_
             : _ Stdlib.List.t)
         in
         let bnds__060_ =
           let arg__064_ = Moon_sexp_conv.sexp_of_string segment__063_ in
           (S.List [ S.Atom "segment"; arg__064_ ] :: bnds__060_
             : _ Stdlib.List.t)
         in
         let bnds__060_ =
           let arg__062_ = Moon_sexp_conv.sexp_of_string file_name__061_ in
           (S.List [ S.Atom "file_name"; arg__062_ ] :: bnds__060_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Parser_inconsistency" :: bnds__060_)
     | Useless_loop -> S.Atom "Useless_loop"
     | Toplevel_not_left_aligned -> S.Atom "Toplevel_not_left_aligned"
     | Unexpected_pragmas arg0__069_ ->
         let res0__070_ = Moon_sexp_conv.sexp_of_string arg0__069_ in
         S.List [ S.Atom "Unexpected_pragmas"; res0__070_ ]
     | Omitted_constr_argument { constr = constr__072_; labels = labels__074_ }
       ->
         let bnds__071_ = ([] : _ Stdlib.List.t) in
         let bnds__071_ =
           let arg__075_ =
             Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
               labels__074_
           in
           (S.List [ S.Atom "labels"; arg__075_ ] :: bnds__071_
             : _ Stdlib.List.t)
         in
         let bnds__071_ =
           let arg__073_ = Moon_sexp_conv.sexp_of_string constr__072_ in
           (S.List [ S.Atom "constr"; arg__073_ ] :: bnds__071_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Omitted_constr_argument" :: bnds__071_)
     | Ambiguous_block -> S.Atom "Ambiguous_block"
     | Useless_try -> S.Atom "Useless_try"
     | Useless_error_type -> S.Atom "Useless_error_type"
     | Useless_catch_all -> S.Atom "Useless_catch_all"
     | Deprecated_syntax
         {
           old_usage = old_usage__077_;
           purpose = purpose__079_;
           new_usage = new_usage__081_;
         } ->
         let bnds__076_ = ([] : _ Stdlib.List.t) in
         let bnds__076_ =
           let arg__082_ =
             Moon_sexp_conv.sexp_of_option Moon_sexp_conv.sexp_of_string
               new_usage__081_
           in
           (S.List [ S.Atom "new_usage"; arg__082_ ] :: bnds__076_
             : _ Stdlib.List.t)
         in
         let bnds__076_ =
           let arg__080_ = Moon_sexp_conv.sexp_of_string purpose__079_ in
           (S.List [ S.Atom "purpose"; arg__080_ ] :: bnds__076_
             : _ Stdlib.List.t)
         in
         let bnds__076_ =
           let arg__078_ = Moon_sexp_conv.sexp_of_string old_usage__077_ in
           (S.List [ S.Atom "old_usage"; arg__078_ ] :: bnds__076_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Deprecated_syntax" :: bnds__076_)
     | Todo -> S.Atom "Todo"
     | Unused_package { name = name__084_; is_alias = is_alias__086_ } ->
         let bnds__083_ = ([] : _ Stdlib.List.t) in
         let bnds__083_ =
           let arg__087_ = Moon_sexp_conv.sexp_of_bool is_alias__086_ in
           (S.List [ S.Atom "is_alias"; arg__087_ ] :: bnds__083_
             : _ Stdlib.List.t)
         in
         let bnds__083_ =
           let arg__085_ = Moon_sexp_conv.sexp_of_string name__084_ in
           (S.List [ S.Atom "name"; arg__085_ ] :: bnds__083_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Unused_package" :: bnds__083_)
     | Empty_package_alias -> S.Atom "Empty_package_alias"
     | Optional_arg_never_supplied arg0__088_ ->
         let res0__089_ = Moon_sexp_conv.sexp_of_string arg0__088_ in
         S.List [ S.Atom "Optional_arg_never_supplied"; res0__089_ ]
     | Optional_arg_always_supplied arg0__090_ ->
         let res0__091_ = Moon_sexp_conv.sexp_of_string arg0__090_ in
         S.List [ S.Atom "Optional_arg_always_supplied"; res0__091_ ]
     | Unused_import_value arg0__092_ ->
         let res0__093_ = Moon_sexp_conv.sexp_of_string arg0__092_ in
         S.List [ S.Atom "Unused_import_value"; res0__093_ ]
     | Reserved_keyword arg0__094_ ->
         let res0__095_ = Moon_sexp_conv.sexp_of_string arg0__094_ in
         S.List [ S.Atom "Reserved_keyword"; res0__095_ ]
     | Loop_label_shadow arg0__096_ ->
         let res0__097_ = Moon_sexp_conv.sexp_of_string arg0__096_ in
         S.List [ S.Atom "Loop_label_shadow"; res0__097_ ]
     | Loop_label_unused arg0__098_ ->
         let res0__099_ = Moon_sexp_conv.sexp_of_string arg0__098_ in
         S.List [ S.Atom "Loop_label_unused"; res0__099_ ]
     | Unused_guard -> S.Atom "Unused_guard"
     | Closed_map_pattern -> S.Atom "Closed_map_pattern"
     | Invalid_attribute arg0__100_ ->
         let res0__101_ = Moon_sexp_conv.sexp_of_string arg0__100_ in
         S.List [ S.Atom "Invalid_attribute"; res0__101_ ]
     | Unused_attribute arg0__102_ ->
         let res0__103_ = Moon_sexp_conv.sexp_of_string arg0__102_ in
         S.List [ S.Atom "Unused_attribute"; res0__103_ ]
     | Invalid_inline_wasm arg0__104_ ->
         let res0__105_ = Moon_sexp_conv.sexp_of_string arg0__104_ in
         S.List [ S.Atom "Invalid_inline_wasm"; res0__105_ ]
     | Implement_trait_with_method
         { trait = trait__107_; typ = typ__109_; methods = methods__111_ } ->
         let bnds__106_ = ([] : _ Stdlib.List.t) in
         let bnds__106_ =
           let arg__112_ =
             Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
               methods__111_
           in
           (S.List [ S.Atom "methods"; arg__112_ ] :: bnds__106_
             : _ Stdlib.List.t)
         in
         let bnds__106_ =
           let arg__110_ = Moon_sexp_conv.sexp_of_string typ__109_ in
           (S.List [ S.Atom "typ"; arg__110_ ] :: bnds__106_ : _ Stdlib.List.t)
         in
         let bnds__106_ =
           let arg__108_ = Moon_sexp_conv.sexp_of_string trait__107_ in
           (S.List [ S.Atom "trait"; arg__108_ ] :: bnds__106_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Implement_trait_with_method" :: bnds__106_)
      : kind -> S.t)

  let _ = sexp_of_kind

  let compare_kind =
    (fun a__113_ ->
       fun b__114_ ->
        if Stdlib.( == ) a__113_ b__114_ then 0
        else
          match (a__113_, b__114_) with
          | Unused_func _a__115_, Unused_func _b__116_ ->
              Stdlib.compare (_a__115_ : string) _b__116_
          | Unused_func _, _ -> -1
          | _, Unused_func _ -> 1
          | Unused_var _a__117_, Unused_var _b__118_ -> (
              match
                Stdlib.compare (_a__117_.var_name : string) _b__118_.var_name
              with
              | 0 ->
                  Stdlib.compare
                    (_a__117_.is_toplevel : bool)
                    _b__118_.is_toplevel
              | n -> n)
          | Unused_var _, _ -> -1
          | _, Unused_var _ -> 1
          | Unused_type_declaration _a__119_, Unused_type_declaration _b__120_
            ->
              Stdlib.compare (_a__119_ : string) _b__120_
          | Unused_type_declaration _, _ -> -1
          | _, Unused_type_declaration _ -> 1
          | Unused_abstract_type _a__121_, Unused_abstract_type _b__122_ -> (
              match Stdlib.compare (_a__121_.name : string) _b__122_.name with
              | 0 -> Stdlib.compare (_a__121_.kind : string) _b__122_.kind
              | n -> n)
          | Unused_abstract_type _, _ -> -1
          | _, Unused_abstract_type _ -> 1
          | Unused_tvar _a__123_, Unused_tvar _b__124_ ->
              Stdlib.compare (_a__123_ : string) _b__124_
          | Unused_tvar _, _ -> -1
          | _, Unused_tvar _ -> 1
          | Unused_constructor _a__125_, Unused_constructor _b__126_ -> (
              match
                Stdlib.compare (_a__125_.constr : string) _b__126_.constr
              with
              | 0 -> compare_unused_kind _a__125_.kind _b__126_.kind
              | n -> n)
          | Unused_constructor _, _ -> -1
          | _, Unused_constructor _ -> 1
          | Unused_field _a__127_, Unused_field _b__128_ ->
              Stdlib.compare (_a__127_ : string) _b__128_
          | Unused_field _, _ -> -1
          | _, Unused_field _ -> 1
          | Unused_constr_arg _a__129_, Unused_constr_arg _b__130_ -> (
              match
                Stdlib.compare (_a__129_.constr : string) _b__130_.constr
              with
              | 0 -> Stdlib.compare (_a__129_.index : int) _b__130_.index
              | n -> n)
          | Unused_constr_arg _, _ -> -1
          | _, Unused_constr_arg _ -> 1
          | Unused_constr_field _a__131_, Unused_constr_field _b__132_ -> (
              match
                Stdlib.compare (_a__131_.constr : string) _b__132_.constr
              with
              | 0 -> (
                  match
                    Stdlib.compare (_a__131_.label : string) _b__132_.label
                  with
                  | 0 ->
                      Stdlib.compare
                        (_a__131_.is_mutated : bool)
                        _b__132_.is_mutated
                  | n -> n)
              | n -> n)
          | Unused_constr_field _, _ -> -1
          | _, Unused_constr_field _ -> 1
          | Redundant_modifier _a__133_, Redundant_modifier _b__134_ -> (
              match
                Stdlib.compare (_a__133_.modifier : string) _b__134_.modifier
              with
              | 0 -> Stdlib.compare (_a__133_.field : string) _b__134_.field
              | n -> n)
          | Redundant_modifier _, _ -> -1
          | _, Redundant_modifier _ -> 1
          | Struct_never_constructed _a__135_, Struct_never_constructed _b__136_
            ->
              Stdlib.compare (_a__135_ : string) _b__136_
          | Struct_never_constructed _, _ -> -1
          | _, Struct_never_constructed _ -> 1
          | Unused_pat, Unused_pat -> 0
          | Unused_pat, _ -> -1
          | _, Unused_pat -> 1
          | Partial_match _a__137_, Partial_match _b__138_ -> (
              match
                Ppx_base.compare_list
                  (fun a__139_ ->
                    fun (b__140_ [@merlin.hide]) ->
                     (Stdlib.compare (a__139_ : string) b__140_ [@merlin.hide]))
                  _a__137_.hint_cases _b__138_.hint_cases
              with
              | 0 -> (
                  match
                    Stdlib.compare
                      (_a__137_.is_let_match : bool)
                      _b__138_.is_let_match
                  with
                  | 0 ->
                      Stdlib.compare
                        (_a__137_.has_guard : bool)
                        _b__138_.has_guard
                  | n -> n)
              | n -> n)
          | Partial_match _, _ -> -1
          | _, Partial_match _ -> 1
          | Unreachable, Unreachable -> 0
          | Unreachable, _ -> -1
          | _, Unreachable -> 1
          | Unresolved_tvar _a__141_, Unresolved_tvar _b__142_ ->
              Stdlib.compare (_a__141_ : string) _b__142_
          | Unresolved_tvar _, _ -> -1
          | _, Unresolved_tvar _ -> 1
          | Lowercase_type_name _a__143_, Lowercase_type_name _b__144_ ->
              Stdlib.compare (_a__143_ : string) _b__144_
          | Lowercase_type_name _, _ -> -1
          | _, Lowercase_type_name _ -> 1
          | Unused_mutability _a__145_, Unused_mutability _b__146_ ->
              Stdlib.compare (_a__145_ : string) _b__146_
          | Unused_mutability _, _ -> -1
          | _, Unused_mutability _ -> 1
          | Parser_inconsistency _a__147_, Parser_inconsistency _b__148_ -> (
              match
                Stdlib.compare (_a__147_.file_name : string) _b__148_.file_name
              with
              | 0 -> (
                  match
                    Stdlib.compare (_a__147_.segment : string) _b__148_.segment
                  with
                  | 0 -> (
                      match
                        Stdlib.compare
                          (_a__147_.is_menhir_succeed : bool)
                          _b__148_.is_menhir_succeed
                      with
                      | 0 ->
                          Stdlib.compare
                            (_a__147_.is_handrolled_succeed : bool)
                            _b__148_.is_handrolled_succeed
                      | n -> n)
                  | n -> n)
              | n -> n)
          | Parser_inconsistency _, _ -> -1
          | _, Parser_inconsistency _ -> 1
          | Useless_loop, Useless_loop -> 0
          | Useless_loop, _ -> -1
          | _, Useless_loop -> 1
          | Toplevel_not_left_aligned, Toplevel_not_left_aligned -> 0
          | Toplevel_not_left_aligned, _ -> -1
          | _, Toplevel_not_left_aligned -> 1
          | Unexpected_pragmas _a__149_, Unexpected_pragmas _b__150_ ->
              Stdlib.compare (_a__149_ : string) _b__150_
          | Unexpected_pragmas _, _ -> -1
          | _, Unexpected_pragmas _ -> 1
          | Omitted_constr_argument _a__151_, Omitted_constr_argument _b__152_
            -> (
              match
                Stdlib.compare (_a__151_.constr : string) _b__152_.constr
              with
              | 0 ->
                  Ppx_base.compare_list
                    (fun a__153_ ->
                      fun (b__154_ [@merlin.hide]) ->
                       (Stdlib.compare
                          (a__153_ : string)
                          b__154_ [@merlin.hide]))
                    _a__151_.labels _b__152_.labels
              | n -> n)
          | Omitted_constr_argument _, _ -> -1
          | _, Omitted_constr_argument _ -> 1
          | Ambiguous_block, Ambiguous_block -> 0
          | Ambiguous_block, _ -> -1
          | _, Ambiguous_block -> 1
          | Useless_try, Useless_try -> 0
          | Useless_try, _ -> -1
          | _, Useless_try -> 1
          | Useless_error_type, Useless_error_type -> 0
          | Useless_error_type, _ -> -1
          | _, Useless_error_type -> 1
          | Useless_catch_all, Useless_catch_all -> 0
          | Useless_catch_all, _ -> -1
          | _, Useless_catch_all -> 1
          | Deprecated_syntax _a__155_, Deprecated_syntax _b__156_ -> (
              match
                Stdlib.compare (_a__155_.old_usage : string) _b__156_.old_usage
              with
              | 0 -> (
                  match
                    Stdlib.compare (_a__155_.purpose : string) _b__156_.purpose
                  with
                  | 0 -> (
                      match (_a__155_.new_usage, _b__156_.new_usage) with
                      | None, None -> 0
                      | None, Some _ -> -1
                      | Some _, None -> 1
                      | Some __option_x, Some __option_y ->
                          (fun a__157_ ->
                            fun (b__158_ [@merlin.hide]) ->
                             (Stdlib.compare
                                (a__157_ : string)
                                b__158_ [@merlin.hide]))
                            __option_x __option_y)
                  | n -> n)
              | n -> n)
          | Deprecated_syntax _, _ -> -1
          | _, Deprecated_syntax _ -> 1
          | Todo, Todo -> 0
          | Todo, _ -> -1
          | _, Todo -> 1
          | Unused_package _a__159_, Unused_package _b__160_ -> (
              match Stdlib.compare (_a__159_.name : string) _b__160_.name with
              | 0 -> Stdlib.compare (_a__159_.is_alias : bool) _b__160_.is_alias
              | n -> n)
          | Unused_package _, _ -> -1
          | _, Unused_package _ -> 1
          | Empty_package_alias, Empty_package_alias -> 0
          | Empty_package_alias, _ -> -1
          | _, Empty_package_alias -> 1
          | ( Optional_arg_never_supplied _a__161_,
              Optional_arg_never_supplied _b__162_ ) ->
              Stdlib.compare (_a__161_ : string) _b__162_
          | Optional_arg_never_supplied _, _ -> -1
          | _, Optional_arg_never_supplied _ -> 1
          | ( Optional_arg_always_supplied _a__163_,
              Optional_arg_always_supplied _b__164_ ) ->
              Stdlib.compare (_a__163_ : string) _b__164_
          | Optional_arg_always_supplied _, _ -> -1
          | _, Optional_arg_always_supplied _ -> 1
          | Unused_import_value _a__165_, Unused_import_value _b__166_ ->
              Stdlib.compare (_a__165_ : string) _b__166_
          | Unused_import_value _, _ -> -1
          | _, Unused_import_value _ -> 1
          | Reserved_keyword _a__167_, Reserved_keyword _b__168_ ->
              Stdlib.compare (_a__167_ : string) _b__168_
          | Reserved_keyword _, _ -> -1
          | _, Reserved_keyword _ -> 1
          | Loop_label_shadow _a__169_, Loop_label_shadow _b__170_ ->
              Stdlib.compare (_a__169_ : string) _b__170_
          | Loop_label_shadow _, _ -> -1
          | _, Loop_label_shadow _ -> 1
          | Loop_label_unused _a__171_, Loop_label_unused _b__172_ ->
              Stdlib.compare (_a__171_ : string) _b__172_
          | Loop_label_unused _, _ -> -1
          | _, Loop_label_unused _ -> 1
          | Unused_guard, Unused_guard -> 0
          | Unused_guard, _ -> -1
          | _, Unused_guard -> 1
          | Closed_map_pattern, Closed_map_pattern -> 0
          | Closed_map_pattern, _ -> -1
          | _, Closed_map_pattern -> 1
          | Invalid_attribute _a__173_, Invalid_attribute _b__174_ ->
              Stdlib.compare (_a__173_ : string) _b__174_
          | Invalid_attribute _, _ -> -1
          | _, Invalid_attribute _ -> 1
          | Unused_attribute _a__175_, Unused_attribute _b__176_ ->
              Stdlib.compare (_a__175_ : string) _b__176_
          | Unused_attribute _, _ -> -1
          | _, Unused_attribute _ -> 1
          | Invalid_inline_wasm _a__177_, Invalid_inline_wasm _b__178_ ->
              Stdlib.compare (_a__177_ : string) _b__178_
          | Invalid_inline_wasm _, _ -> -1
          | _, Invalid_inline_wasm _ -> 1
          | ( Implement_trait_with_method _a__179_,
              Implement_trait_with_method _b__180_ ) -> (
              match Stdlib.compare (_a__179_.trait : string) _b__180_.trait with
              | 0 -> (
                  match Stdlib.compare (_a__179_.typ : string) _b__180_.typ with
                  | 0 ->
                      Ppx_base.compare_list
                        (fun a__181_ ->
                          fun (b__182_ [@merlin.hide]) ->
                           (Stdlib.compare
                              (a__181_ : string)
                              b__182_ [@merlin.hide]))
                        _a__179_.methods _b__180_.methods
                  | n -> n)
              | n -> n)
      : kind -> kind -> int)

  let _ = compare_kind

  let equal_kind =
    (fun a__183_ ->
       fun b__184_ ->
        if Stdlib.( == ) a__183_ b__184_ then true
        else
          match (a__183_, b__184_) with
          | Unused_func _a__185_, Unused_func _b__186_ ->
              Stdlib.( = ) (_a__185_ : string) _b__186_
          | Unused_func _, _ -> false
          | _, Unused_func _ -> false
          | Unused_var _a__187_, Unused_var _b__188_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__187_.var_name : string) _b__188_.var_name)
                (Stdlib.( = )
                   (_a__187_.is_toplevel : bool)
                   _b__188_.is_toplevel)
          | Unused_var _, _ -> false
          | _, Unused_var _ -> false
          | Unused_type_declaration _a__189_, Unused_type_declaration _b__190_
            ->
              Stdlib.( = ) (_a__189_ : string) _b__190_
          | Unused_type_declaration _, _ -> false
          | _, Unused_type_declaration _ -> false
          | Unused_abstract_type _a__191_, Unused_abstract_type _b__192_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__191_.name : string) _b__192_.name)
                (Stdlib.( = ) (_a__191_.kind : string) _b__192_.kind)
          | Unused_abstract_type _, _ -> false
          | _, Unused_abstract_type _ -> false
          | Unused_tvar _a__193_, Unused_tvar _b__194_ ->
              Stdlib.( = ) (_a__193_ : string) _b__194_
          | Unused_tvar _, _ -> false
          | _, Unused_tvar _ -> false
          | Unused_constructor _a__195_, Unused_constructor _b__196_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__195_.constr : string) _b__196_.constr)
                (equal_unused_kind _a__195_.kind _b__196_.kind)
          | Unused_constructor _, _ -> false
          | _, Unused_constructor _ -> false
          | Unused_field _a__197_, Unused_field _b__198_ ->
              Stdlib.( = ) (_a__197_ : string) _b__198_
          | Unused_field _, _ -> false
          | _, Unused_field _ -> false
          | Unused_constr_arg _a__199_, Unused_constr_arg _b__200_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__199_.constr : string) _b__200_.constr)
                (Stdlib.( = ) (_a__199_.index : int) _b__200_.index)
          | Unused_constr_arg _, _ -> false
          | _, Unused_constr_arg _ -> false
          | Unused_constr_field _a__201_, Unused_constr_field _b__202_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__201_.constr : string) _b__202_.constr)
                (Stdlib.( && )
                   (Stdlib.( = ) (_a__201_.label : string) _b__202_.label)
                   (Stdlib.( = )
                      (_a__201_.is_mutated : bool)
                      _b__202_.is_mutated))
          | Unused_constr_field _, _ -> false
          | _, Unused_constr_field _ -> false
          | Redundant_modifier _a__203_, Redundant_modifier _b__204_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__203_.modifier : string) _b__204_.modifier)
                (Stdlib.( = ) (_a__203_.field : string) _b__204_.field)
          | Redundant_modifier _, _ -> false
          | _, Redundant_modifier _ -> false
          | Struct_never_constructed _a__205_, Struct_never_constructed _b__206_
            ->
              Stdlib.( = ) (_a__205_ : string) _b__206_
          | Struct_never_constructed _, _ -> false
          | _, Struct_never_constructed _ -> false
          | Unused_pat, Unused_pat -> true
          | Unused_pat, _ -> false
          | _, Unused_pat -> false
          | Partial_match _a__207_, Partial_match _b__208_ ->
              Stdlib.( && )
                (Ppx_base.equal_list
                   (fun a__209_ ->
                     fun (b__210_ [@merlin.hide]) ->
                      (Stdlib.( = ) (a__209_ : string) b__210_ [@merlin.hide]))
                   _a__207_.hint_cases _b__208_.hint_cases)
                (Stdlib.( && )
                   (Stdlib.( = )
                      (_a__207_.is_let_match : bool)
                      _b__208_.is_let_match)
                   (Stdlib.( = ) (_a__207_.has_guard : bool) _b__208_.has_guard))
          | Partial_match _, _ -> false
          | _, Partial_match _ -> false
          | Unreachable, Unreachable -> true
          | Unreachable, _ -> false
          | _, Unreachable -> false
          | Unresolved_tvar _a__211_, Unresolved_tvar _b__212_ ->
              Stdlib.( = ) (_a__211_ : string) _b__212_
          | Unresolved_tvar _, _ -> false
          | _, Unresolved_tvar _ -> false
          | Lowercase_type_name _a__213_, Lowercase_type_name _b__214_ ->
              Stdlib.( = ) (_a__213_ : string) _b__214_
          | Lowercase_type_name _, _ -> false
          | _, Lowercase_type_name _ -> false
          | Unused_mutability _a__215_, Unused_mutability _b__216_ ->
              Stdlib.( = ) (_a__215_ : string) _b__216_
          | Unused_mutability _, _ -> false
          | _, Unused_mutability _ -> false
          | Parser_inconsistency _a__217_, Parser_inconsistency _b__218_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__217_.file_name : string) _b__218_.file_name)
                (Stdlib.( && )
                   (Stdlib.( = ) (_a__217_.segment : string) _b__218_.segment)
                   (Stdlib.( && )
                      (Stdlib.( = )
                         (_a__217_.is_menhir_succeed : bool)
                         _b__218_.is_menhir_succeed)
                      (Stdlib.( = )
                         (_a__217_.is_handrolled_succeed : bool)
                         _b__218_.is_handrolled_succeed)))
          | Parser_inconsistency _, _ -> false
          | _, Parser_inconsistency _ -> false
          | Useless_loop, Useless_loop -> true
          | Useless_loop, _ -> false
          | _, Useless_loop -> false
          | Toplevel_not_left_aligned, Toplevel_not_left_aligned -> true
          | Toplevel_not_left_aligned, _ -> false
          | _, Toplevel_not_left_aligned -> false
          | Unexpected_pragmas _a__219_, Unexpected_pragmas _b__220_ ->
              Stdlib.( = ) (_a__219_ : string) _b__220_
          | Unexpected_pragmas _, _ -> false
          | _, Unexpected_pragmas _ -> false
          | Omitted_constr_argument _a__221_, Omitted_constr_argument _b__222_
            ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__221_.constr : string) _b__222_.constr)
                (Ppx_base.equal_list
                   (fun a__223_ ->
                     fun (b__224_ [@merlin.hide]) ->
                      (Stdlib.( = ) (a__223_ : string) b__224_ [@merlin.hide]))
                   _a__221_.labels _b__222_.labels)
          | Omitted_constr_argument _, _ -> false
          | _, Omitted_constr_argument _ -> false
          | Ambiguous_block, Ambiguous_block -> true
          | Ambiguous_block, _ -> false
          | _, Ambiguous_block -> false
          | Useless_try, Useless_try -> true
          | Useless_try, _ -> false
          | _, Useless_try -> false
          | Useless_error_type, Useless_error_type -> true
          | Useless_error_type, _ -> false
          | _, Useless_error_type -> false
          | Useless_catch_all, Useless_catch_all -> true
          | Useless_catch_all, _ -> false
          | _, Useless_catch_all -> false
          | Deprecated_syntax _a__225_, Deprecated_syntax _b__226_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__225_.old_usage : string) _b__226_.old_usage)
                (Stdlib.( && )
                   (Stdlib.( = ) (_a__225_.purpose : string) _b__226_.purpose)
                   (match (_a__225_.new_usage, _b__226_.new_usage) with
                   | None, None -> true
                   | None, Some _ -> false
                   | Some _, None -> false
                   | Some __option_x, Some __option_y ->
                       (fun a__227_ ->
                         fun (b__228_ [@merlin.hide]) ->
                          (Stdlib.( = )
                             (a__227_ : string)
                             b__228_ [@merlin.hide]))
                         __option_x __option_y))
          | Deprecated_syntax _, _ -> false
          | _, Deprecated_syntax _ -> false
          | Todo, Todo -> true
          | Todo, _ -> false
          | _, Todo -> false
          | Unused_package _a__229_, Unused_package _b__230_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__229_.name : string) _b__230_.name)
                (Stdlib.( = ) (_a__229_.is_alias : bool) _b__230_.is_alias)
          | Unused_package _, _ -> false
          | _, Unused_package _ -> false
          | Empty_package_alias, Empty_package_alias -> true
          | Empty_package_alias, _ -> false
          | _, Empty_package_alias -> false
          | ( Optional_arg_never_supplied _a__231_,
              Optional_arg_never_supplied _b__232_ ) ->
              Stdlib.( = ) (_a__231_ : string) _b__232_
          | Optional_arg_never_supplied _, _ -> false
          | _, Optional_arg_never_supplied _ -> false
          | ( Optional_arg_always_supplied _a__233_,
              Optional_arg_always_supplied _b__234_ ) ->
              Stdlib.( = ) (_a__233_ : string) _b__234_
          | Optional_arg_always_supplied _, _ -> false
          | _, Optional_arg_always_supplied _ -> false
          | Unused_import_value _a__235_, Unused_import_value _b__236_ ->
              Stdlib.( = ) (_a__235_ : string) _b__236_
          | Unused_import_value _, _ -> false
          | _, Unused_import_value _ -> false
          | Reserved_keyword _a__237_, Reserved_keyword _b__238_ ->
              Stdlib.( = ) (_a__237_ : string) _b__238_
          | Reserved_keyword _, _ -> false
          | _, Reserved_keyword _ -> false
          | Loop_label_shadow _a__239_, Loop_label_shadow _b__240_ ->
              Stdlib.( = ) (_a__239_ : string) _b__240_
          | Loop_label_shadow _, _ -> false
          | _, Loop_label_shadow _ -> false
          | Loop_label_unused _a__241_, Loop_label_unused _b__242_ ->
              Stdlib.( = ) (_a__241_ : string) _b__242_
          | Loop_label_unused _, _ -> false
          | _, Loop_label_unused _ -> false
          | Unused_guard, Unused_guard -> true
          | Unused_guard, _ -> false
          | _, Unused_guard -> false
          | Closed_map_pattern, Closed_map_pattern -> true
          | Closed_map_pattern, _ -> false
          | _, Closed_map_pattern -> false
          | Invalid_attribute _a__243_, Invalid_attribute _b__244_ ->
              Stdlib.( = ) (_a__243_ : string) _b__244_
          | Invalid_attribute _, _ -> false
          | _, Invalid_attribute _ -> false
          | Unused_attribute _a__245_, Unused_attribute _b__246_ ->
              Stdlib.( = ) (_a__245_ : string) _b__246_
          | Unused_attribute _, _ -> false
          | _, Unused_attribute _ -> false
          | Invalid_inline_wasm _a__247_, Invalid_inline_wasm _b__248_ ->
              Stdlib.( = ) (_a__247_ : string) _b__248_
          | Invalid_inline_wasm _, _ -> false
          | _, Invalid_inline_wasm _ -> false
          | ( Implement_trait_with_method _a__249_,
              Implement_trait_with_method _b__250_ ) ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__249_.trait : string) _b__250_.trait)
                (Stdlib.( && )
                   (Stdlib.( = ) (_a__249_.typ : string) _b__250_.typ)
                   (Ppx_base.equal_list
                      (fun a__251_ ->
                        fun (b__252_ [@merlin.hide]) ->
                         (Stdlib.( = )
                            (a__251_ : string)
                            b__252_ [@merlin.hide]))
                      _a__249_.methods _b__250_.methods))
      : kind -> kind -> bool)

  let _ = equal_kind
end

type t = { loc : Loc.t; kind : kind }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { loc = loc__254_; kind = kind__256_ } ->
       let bnds__253_ = ([] : _ Stdlib.List.t) in
       let bnds__253_ =
         let arg__257_ = sexp_of_kind kind__256_ in
         (S.List [ S.Atom "kind"; arg__257_ ] :: bnds__253_ : _ Stdlib.List.t)
       in
       let bnds__253_ =
         let arg__255_ = Loc.sexp_of_t loc__254_ in
         (S.List [ S.Atom "loc"; arg__255_ ] :: bnds__253_ : _ Stdlib.List.t)
       in
       S.List bnds__253_
      : t -> S.t)

  let _ = sexp_of_t

  let compare =
    (fun a__258_ ->
       fun b__259_ ->
        if Stdlib.( == ) a__258_ b__259_ then 0
        else
          match Loc.compare a__258_.loc b__259_.loc with
          | 0 -> compare_kind a__258_.kind b__259_.kind
          | n -> n
      : t -> t -> int)

  let _ = compare

  let equal =
    (fun a__260_ ->
       fun b__261_ ->
        if Stdlib.( == ) a__260_ b__261_ then true
        else
          Stdlib.( && )
            (Loc.equal a__260_.loc b__261_.loc)
            (equal_kind a__260_.kind b__261_.kind)
      : t -> t -> bool)

  let _ = equal
end

let number = function
  | Unused_func _ -> 1
  | Unused_var _ -> 2
  | Unused_type_declaration _ -> 3
  | Unused_abstract_type _ -> 4
  | Unused_tvar _ -> 5
  | Unused_constructor _ -> 6
  | Unused_field _ | Unused_constr_arg _ | Unused_constr_field _ -> 7
  | Redundant_modifier _ -> 8
  | Struct_never_constructed _ -> 9
  | Unused_pat -> 10
  | Partial_match _ -> 11
  | Unreachable -> 12
  | Unresolved_tvar _ -> 13
  | Lowercase_type_name _ -> 14
  | Unused_mutability _ -> 15
  | Parser_inconsistency _ -> 16
  | Useless_loop -> 18
  | Toplevel_not_left_aligned -> 19
  | Unexpected_pragmas _ -> 20
  | Omitted_constr_argument _ -> 21
  | Ambiguous_block -> 22
  | Useless_try -> 23
  | Useless_error_type -> 24
  | Useless_catch_all -> 26
  | Deprecated_syntax _ -> 27
  | Todo -> 28
  | Unused_package _ -> 29
  | Empty_package_alias -> 30
  | Optional_arg_never_supplied _ -> 31
  | Optional_arg_always_supplied _ -> 32
  | Unused_import_value _ -> 33
  | Reserved_keyword _ -> 35
  | Loop_label_shadow _ -> 36
  | Loop_label_unused _ -> 37
  | Unused_guard -> 38
  | Closed_map_pattern -> 41
  | Invalid_attribute _ -> 42
  | Unused_attribute _ -> 43
  | Invalid_inline_wasm _ -> 44
  | Implement_trait_with_method _ -> 45

let last_warning_id = 45

let message ?(as_error = false) (x : kind) =
  (let leading = if as_error then "Error (warning): " else "Warning: " in
   let msg =
     match x with
     | Unused_func s ->
         ("Unused function '" ^ s ^ "'" : Stdlib.String.t) [@merlin.hide]
     | Unused_var { var_name; is_toplevel } ->
         if is_toplevel then
           ("Unused toplevel variable '" ^ var_name
            ^ "'. Note if the body contains side effect, it will not happen. \
               Use `fn init { .. }` to wrap the effect."
             : Stdlib.String.t)
             [@merlin.hide]
         else
           ("Unused variable '" ^ var_name ^ "'"
             : Stdlib.String.t)
             [@merlin.hide]
     | Unused_type_declaration s ->
         ("Unused type '" ^ s ^ "'" : Stdlib.String.t) [@merlin.hide]
     | Unused_abstract_type { name; kind } ->
         Stdlib.String.concat ""
           [
             "The ";
             kind;
             " '";
             name;
             "' does not occur in public signature of current package, \
              consider marking it as `priv`.";
           ] [@merlin.hide]
     | Unused_tvar s ->
         ("Unused generic type variable '" ^ s ^ "'"
           : Stdlib.String.t)
           [@merlin.hide]
     | Unused_constructor { constr; kind } -> (
         match kind with
         | Unused ->
             ("Variant '" ^ constr ^ "' is unused"
               : Stdlib.String.t)
               [@merlin.hide]
         | No_construct ->
             ("Variant '" ^ constr ^ "' is never constructed"
               : Stdlib.String.t)
               [@merlin.hide]
         | No_read ->
             ("Variant '" ^ constr ^ "' is never read"
               : Stdlib.String.t)
               [@merlin.hide])
     | Unused_field s ->
         ("Field '" ^ s ^ "' is never read" : Stdlib.String.t) [@merlin.hide]
     | Unused_constr_arg { constr; index } ->
         let index =
           match index with
           | 0 -> "1st"
           | 1 -> "2nd"
           | 2 -> "3rd"
           | n ->
               (Int.to_string (n + 1) ^ "th" : Stdlib.String.t) [@merlin.hide]
         in
         (Stdlib.String.concat ""
            [
              "The ";
              index;
              " positional argument of constructor '";
              constr;
              "' is unused.";
            ] [@merlin.hide])
     | Unused_constr_field { constr; label; is_mutated = false } ->
         Stdlib.String.concat ""
           [ "Field '"; label; "' of constructor '"; constr; "' is unused." ]
         [@merlin.hide]
     | Unused_constr_field { constr; label; is_mutated = true } ->
         Stdlib.String.concat ""
           [
             "Field '"; label; "' of constructor '"; constr; "' is never read.";
           ] [@merlin.hide]
     | Redundant_modifier { modifier; field } ->
         Stdlib.String.concat ""
           [
             "The ";
             modifier;
             " modifier is redundant here since field ";
             field;
             " is ";
             modifier;
             " by default";
           ] [@merlin.hide]
     | Struct_never_constructed name ->
         ("The struct " ^ name ^ " is never constructed"
           : Stdlib.String.t)
           [@merlin.hide]
     | Unused_pat -> "Unused pattern"
     | Partial_match { hint_cases = lst; is_let_match; has_guard } ->
         let hint_cases = String.concat "\n" lst in
         if is_let_match then
           ("Partial match, some hints:\n" ^ hint_cases
            ^ " \n\
              \  note: You can use guard-let if the missing cases are not \
               expected to happen."
             : Stdlib.String.t)
             [@merlin.hide]
         else if has_guard then
           ("Partial match, some hints:\n" ^ hint_cases
            ^ "\n  note: the compiler won't take guard conditions into account."
             : Stdlib.String.t)
             [@merlin.hide]
         else
           ("Partial match, some hints:\n" ^ hint_cases
             : Stdlib.String.t)
             [@merlin.hide]
     | Unreachable -> "Unreachable code"
     | Unresolved_tvar s ->
         ("The type of this expression is " ^ s
          ^ ", which contains unresolved type variables. The type variable is \
             default to Unit."
           : Stdlib.String.t)
           [@merlin.hide]
     | Lowercase_type_name s ->
         ("Type name '" ^ s ^ "' should be capitalized."
           : Stdlib.String.t)
           [@merlin.hide]
     | Unused_mutability s ->
         ("The mutability of " ^ s ^ " is never used."
           : Stdlib.String.t)
           [@merlin.hide]
     | Parser_inconsistency
         { file_name; segment; is_menhir_succeed; is_handrolled_succeed } ->
         let p x = if x then "succeed" else "failed" in
         (Stdlib.String.concat ""
            [
              "parser consistency check failed at '";
              file_name;
              "' ";
              segment;
              " (menhir parser ";
              p is_menhir_succeed;
              ", handrolled parser ";
              p is_handrolled_succeed;
              ")";
            ] [@merlin.hide])
     | Useless_loop ->
         ("There is no [continue] in this loop expression, so [loop] is \
           useless here."
           : Stdlib.String.t)
           [@merlin.hide]
     | Toplevel_not_left_aligned ->
         ("Toplevel declaration is not left aligned."
           : Stdlib.String.t)
           [@merlin.hide]
     | Unexpected_pragmas s ->
         ("Invalid pragma, " ^ s : Stdlib.String.t) [@merlin.hide]
     | Omitted_constr_argument { constr; labels } ->
         let labels_str = String.concat "," labels in
         (Stdlib.String.concat ""
            [
              "The argument(s) ";
              labels_str;
              " of constructor ";
              constr;
              " are omitted (To ignore them, add \"..\" to the end of argument \
               list).";
            ] [@merlin.hide])
     | Ambiguous_block ->
         ("Ambiguous block expression. Use `id` directly, or use `{ id, }` to \
           clarify a struct literal."
           : Stdlib.String.t)
           [@merlin.hide]
     | Useless_try -> "The body of this try expression never raises any error."
     | Useless_error_type -> "The error type of this function is never used."
     | Useless_catch_all ->
         "The patterns are complete so the usage of `catch!` is useless. Use \
          `catch` instead."
     | Deprecated_syntax { old_usage; purpose; new_usage } ->
         let new_usage =
           match new_usage with
           | None -> ""
           | Some new_usage ->
               (" Use " ^ new_usage ^ " instead."
                 : Stdlib.String.t)
                 [@merlin.hide]
         in
         (Stdlib.String.concat ""
            [
              "The syntax ";
              old_usage;
              " for ";
              purpose;
              " is deprecated.";
              new_usage;
            ] [@merlin.hide])
     | Todo -> "unfinished code"
     | Unused_package { name; is_alias } ->
         if is_alias then
           ("Unused package alias '" ^ name ^ "'"
             : Stdlib.String.t)
             [@merlin.hide]
         else ("Unused package '" ^ name ^ "'" : Stdlib.String.t) [@merlin.hide]
     | Empty_package_alias ->
         "The package alias is empty. The default package alias will be used \
          instead."
     | Optional_arg_never_supplied label ->
         ("The optional argument '" ^ label ^ "' is never supplied."
           : Stdlib.String.t)
           [@merlin.hide]
     | Optional_arg_always_supplied label ->
         ("Default value of optional argument '" ^ label ^ "' is unused."
           : Stdlib.String.t)
           [@merlin.hide]
     | Unused_import_value name ->
         ("The import value " ^ name ^ " is never used directly. "
           : Stdlib.String.t)
           [@merlin.hide]
     | Reserved_keyword s ->
         ("The word `" ^ s
          ^ "` is reserved for possible future use. Please consider using \
             another name."
           : Stdlib.String.t)
           [@merlin.hide]
     | Loop_label_shadow label ->
         ("The label name `" ^ label
          ^ "` shadows a label name that is already in scope."
           : Stdlib.String.t)
           [@merlin.hide]
     | Loop_label_unused label ->
         ("The label name `" ^ label ^ "` is never used."
           : Stdlib.String.t)
           [@merlin.hide]
     | Unused_guard -> "Useless guard because the pattern is irrefutable."
     | Closed_map_pattern ->
         "Map patterns are always open, so `..` should be added to this \
          pattern."
     | Invalid_attribute s -> s
     | Unused_attribute s ->
         ("Unused attribute '" ^ s ^ "'." : Stdlib.String.t) [@merlin.hide]
     | Invalid_inline_wasm s -> s
     | Implement_trait_with_method { trait; typ; methods } ->
         let methods = String.concat ", " methods in
         (Stdlib.String.concat ""
            [
              "Type ";
              typ;
              " implements trait ";
              trait;
              " with regular methods ";
              methods;
              ", this behavior is deprecated and will be removed in the \
               future, write explicit `impl` instead.";
            ] [@merlin.hide])
   in
   leading ^ msg
    : string)

type state = { active : bool array; error : bool array }

let current =
  ref
    {
      active = Array.make (last_warning_id + 1) true;
      error = Array.make (last_warning_id + 1) false;
    }

let disabled = ref false
let disable_warn_as_error = ref false
let is_active x = (not !disabled) && !current.active.(number x)
let is_error x = (not !disable_warn_as_error) && !current.error.(number x)
let without_warn_as_error f = Basic_ref.protect disable_warn_as_error true f

let parse_opt active error flags s =
  let set i = flags.(i) <- true in
  let reset i =
    flags.(i) <- false;
    error.(i) <- false
  in
  let both i =
    active.(i) <- true;
    error.(i) <- true
  in
  let error msg = raise (Arg.Bad ("Ill-formed list of warnings: " ^ msg)) in
  let unknown_token c = error ("unexpected token '" ^ String.make 1 c ^ "'") in
  let readint i =
    let rec go acc i =
      if i >= String.length s then (i, acc)
      else
        match s.[i] with
        | '0' .. '9' -> go ((10 * acc) + Char.code s.[i] - Char.code '0') (i + 1)
        | _ -> (i, acc)
    in
    go 0 i
  in
  let readrange i =
    let i, n1 = readint i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = readint (i + 2) in
      if n2 < n1 then
        error (string_of_int n2 ^ " is smaller than " ^ string_of_int n1)
      else (i, n1, n2)
    else (i, n1, n1)
  in
  let alpha f i =
    if i >= String.length s then error "unexpected end"
    else
      match s.[i] with
      | 'A' | 'a' ->
          for j = 1 to last_warning_id do
            f j
          done;
          i + 1
      | '0' .. '9' ->
          let i, n1, n2 = readrange i in
          for j = n1 to Int.min n2 last_warning_id do
            f j
          done;
          i
      | _ -> unknown_token s.[i]
  in
  let rec loop i =
    if i < String.length s then
      match s.[i] with
      | 'A' | 'a' -> loop (alpha set i)
      | '+' -> loop (alpha set (i + 1))
      | '-' -> loop (alpha reset (i + 1))
      | '@' -> loop (alpha both (i + 1))
      | _ -> unknown_token s.[i]
  in
  loop 0

let parse_options errflag s =
  let error = Array.copy !current.error in
  let active = Array.copy !current.active in
  parse_opt active error (if errflag then error else active) s;
  current := { active; error }

let default_warnings = "+a-31-32"
let default_warnings_as_errors = "-a+11+15+23+24+44"

let reset () =
  parse_options false default_warnings;
  parse_options true default_warnings_as_errors

let () = reset ()

let descriptions =
  [
    (1, "Unused function.");
    (2, "Unused variable.");
    (3, "Unused type declaration.");
    (4, "Unused abstract type.");
    (5, "Unused type variable.");
    (6, "Unused constructor.");
    (7, "Unused field or constructor argument.");
    (8, "Redunant modifier.");
    (9, "Unused function declaration.");
    (10, "Struct never constructed.");
    (11, "Partial pattern matching.");
    (12, "Unreachable code.");
    (13, "Unresolved type variable.");
    (14, "Lowercase type name.");
    (15, "Unused mutability.");
    (16, "Parser inconsistency.");
    (18, "Useless loop expression.");
    (19, "Top-level declaration is not left aligned.");
    (20, "Invalid pragma");
    (21, "Some arguments of constructor are omitted in pattern.");
    (22, "Ambiguous block.");
    (23, "Useless try expression.");
    (24, "Useless error type.");
    (26, "Useless catch all.");
    (27, "Deprecated syntax.");
    (28, "Todo");
    (29, "Unused package.");
    (30, "Empty package alias.");
    (31, "Optional argument never supplied.");
    (32, "Default value of optional argument never used.");
    (33, "Unused import value");
    (35, "Reserved keyword.");
    (36, "Loop label shadows another label.");
    (37, "Unused loop label.");
    (38, "Useless guard.");
    (39, "Duplicated method.");
    (40, "Call a qualified method using regular call syntax.");
    (41, "Closed map pattern.");
    (42, "Invalid attribute.");
    (43, "Unused attribute.");
    (44, "Invalid inline-wasm.");
    (45, "Type implments trait with regular methods.");
  ]

let help_warnings () =
  print_endline "Available warnings: ";
  Basic_lst.iter descriptions ~f:(fun (i, s) -> Printf.printf "%3i %s\n" i s);
  print_endline "  A all warnings";
  exit 0
