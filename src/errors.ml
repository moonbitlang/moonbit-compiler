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


module Longident = Basic_longident
module Type_path = Basic_type_path
module Ident = Basic_ident
module Lst = Basic_lst

type local_error = Local_diagnostics.error
type loc = Rloc.t

let type_path_name p =
  Type_path.short_name ~cur_pkg_name:(Some !Basic_config.current_package) p

let swallow_error : local_error = Local_diagnostics.swallow_error

let internal message =
  ({
     message = ("Compiler Internal Error: " ^ message : Stdlib.String.t);
     loc = Loc.no_location;
     error_code = Error_code.internal;
   }
    : Diagnostics.report)

let lexing_error ~loc_start ~loc_end message =
  ({
     loc = Loc.of_menhir (loc_start, loc_end);
     message = "Lexing error: " ^ message;
     error_code = Error_code.lexing_error;
   }
    : Diagnostics.report)

let parse_error ~loc_start ~loc_end message =
  ({
     loc = Loc.of_menhir (loc_start, loc_end);
     message;
     error_code = Error_code.parse_error;
   }
    : Diagnostics.report)

let attribute_parse_error ~loc message =
  ({ loc; message; error_code = Error_code.parse_error } : local_error)

let json_parse_error ~loc_start ~loc_end message =
  ({
     loc = Loc.of_menhir (loc_start, loc_end);
     message;
     error_code = Error_code.json_parse_error;
   }
    : Diagnostics.report)

let invalid_init_or_main ~(kind : [ `Init | `Main ]) ~loc =
  (let message = match kind with `Init -> "Init" | `Main -> "Main" in
   {
     loc;
     message =
       (message ^ " function must have no arguments and no return value."
         : Stdlib.String.t);
     error_code = Error_code.invalid_init_or_main;
   }
    : Diagnostics.report)

let missing_parameter_list ~name ~loc =
  (let message =
     ("Missing parameters list. Add `()` if function `" ^ name
      ^ "` has 0 parameter."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.missing_parameter_list }
    : local_error)

let unexpected_token ~found ~expected ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Parse error, unexpected token ";
         found;
         ", you may expect ";
         expected;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.parse_error }
    : Diagnostics.report)

let unexpected_line_break ~expected ~loc =
  (let message =
     ("Unexpected line break here, missing " ^ expected
      ^ " at the end of this line."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.parse_error }
    : Diagnostics.report)

let unexpected_token_maybe_forget_indent ~expected ~next ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Parse error, expect ";
         expected;
         ". Did you forget to indent the local ";
         next;
         "?";
       ]
   in
   { message; loc; error_code = Error_code.parse_error }
    : Diagnostics.report)

let invalid_visibility ~entity ~vis ~loc =
  (let message =
     Stdlib.String.concat "" [ "No '"; vis; "' visibility for "; entity; "." ]
   in
   { message; loc; error_code = Error_code.invalid_visibility }
    : local_error)

let enum_no_individual_visibility loc =
  ({
     message = "No individual visibility for enum constructor.";
     loc;
     error_code = Error_code.enum_no_individual_visibility;
   }
    : local_error)

let dotdot_in_middle_of_pattern ~(kind : [ `Record | `Constr | `Map ]) ~loc =
  (let component, kind =
     match kind with
     | `Record -> ("field", "record")
     | `Constr -> ("argument", "constructor arguments")
     | `Map -> ("element", "map")
   in
   let message =
     Stdlib.String.concat ""
       [
         "Unexpected `..` here, add `, ..` behind the last ";
         component;
         " to ignore the rest of ";
         kind;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.dotdot_in_middle_of_pattern }
    : local_error)

let array_pat_multiple_dotdot loc =
  ({
     message = "At most one `..` is allowed in array pattern.";
     loc;
     error_code = Error_code.array_pat_multiple_dotdot;
   }
    : local_error)

let record_pattern_only_dotdot loc =
  ({
     message =
       "Record pattern cannot contain only `..`, use wildcard pattern `_` \
        instead.";
     loc;
     error_code = Error_code.record_pattern_only_dotdot;
   }
    : local_error)

let positional_argument_no_default loc =
  ({
     message = "Only labelled arguments can have default value.";
     loc;
     error_code = Error_code.positional_argument_no_default;
   }
    : local_error)

let invalid_left_value loc =
  ({
     message = "Invalid left value for assignment.";
     loc;
     error_code = Error_code.invalid_left_value;
   }
    : local_error)

let bad_range_pattern_operand loc =
  ({
     message =
       "Bounds of range pattern must be constant, named constant or wildcard.";
     loc;
     error_code = Error_code.bad_range_pattern_operand;
   }
    : local_error)

let inclusive_range_pattern_no_upper_bound loc =
  ({
     message = "Inclusive range pattern `a..=b` cannot have `_` as upper bound";
     loc;
     error_code = Error_code.inclusive_range_pattern_no_upper_bound;
   }
    : local_error)

let invalid_equal_in_struct_expr ~loc =
  (let message =
     "Unexpected `=` in struct expression. The correct syntax for struct \
      expression is `{ field: expression }`."
   in
   { message; loc; error_code = Error_code.invalid_equal_in_struct_expr }
    : local_error)

let cannot_mix_record_and_map_pat loc =
  ({
     message = "Record pattern and map pattern cannot be mixed.";
     loc;
     error_code = Error_code.cannot_mix_record_and_map_pat;
   }
    : local_error)

let inline_wasm_syntax_error ~message ~loc_inside_wasm ~loc =
  ({
     message =
       Stdlib.String.concat ""
         [
           "Inline wasm syntax error: ";
           message;
           " at ";
           Loc.loc_range_string_no_filename loc_inside_wasm;
         ];
     loc;
     error_code = Error_code.inline_wasm_syntax_error;
   }
    : Diagnostics.report)

let duplicate_tvar ~name ~loc =
  ({
     loc;
     message =
       ("Generic type variable name '" ^ name ^ "' is already used."
         : Stdlib.String.t);
     error_code = Error_code.duplicate_tvar;
   }
    : local_error)

let field_visibility ~field_vis ~type_vis ~loc =
  ({
     loc;
     message =
       Stdlib.String.concat ""
         [
           "A ";
           field_vis;
           " field cannot be declared within a ";
           type_vis;
           " struct.";
         ];
     error_code = Error_code.field_visibility;
   }
    : local_error)

let unsupported_modifier ~modifier ~loc =
  (let message =
     ("The " ^ modifier ^ " modifier is not supported here" : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unsupported_modifier }
    : local_error)

let reserved_type_name ~(decl_kind : [ `Tvar | `Type | `Trait ]) ~name ~loc =
  (let name_decl_kind =
     match decl_kind with
     | `Tvar -> "type variable"
     | `Type -> "type"
     | `Trait -> "trait"
   in
   let message =
     Stdlib.String.concat ""
       [
         "\"";
         name;
         "\" is a reserved type name. Cannot declare it as ";
         name_decl_kind;
       ]
   in
   { message; loc; error_code = Error_code.reserved_type_name }
    : local_error)

let trait_method_cannot_poly loc =
  (let message = "polymorphic trait method is not supported" in
   { message; loc; error_code = Error_code.trait_method_cannot_poly }
    : local_error)

let trait_duplicate_method ~trait ~name ~first ~second =
  (let message =
     Stdlib.String.concat ""
       [
         "method ";
         name;
         " of trait ";
         trait;
         " is declared multiple times (first at ";
         Loc.to_string first;
         ")";
       ]
   in
   { message; loc = second; error_code = Error_code.trait_duplicate_method }
    : local_error)

let duplicate_local_fns ~name ~loc ~prev_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "local function '";
         name;
         "' is already defined at ";
         Int.to_string (Loc.line_number prev_loc);
         ":";
         Int.to_string (Loc.column_number prev_loc);
       ]
   in
   { message; loc; error_code = Error_code.duplicate_local_fns }
    : local_error)

let illform_constr_arg loc =
  (let message =
     ("constructor without payload cannot be called with ()" : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.illform_constr_arg }
    : local_error)

let ffi_cannot_poly loc =
  (let message = "FFI function cannot have type parameters." in
   { message; loc; error_code = Error_code.ffi_cannot_poly }
    : Diagnostics.report)

let matchfn_arity_mismatch ~loc ~expected ~actual =
  (let message =
     Stdlib.String.concat ""
       [
         "Match function expects ";
         Int.to_string expected;
         " arguments, but ";
         Int.to_string actual;
         " arguments are provided.";
       ]
   in
   { message; loc; error_code = Error_code.matchfn_arity_mismatch }
    : local_error)

let no_vis_on_default_impl loc =
  (let message =
     ("`pub` is not allowed on default implementation for traits."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.no_vis_on_default_impl }
    : local_error)

let no_quantifiers_on_default_impl loc =
  (let message =
     ("Type parameters are not allowed on default implementation for traits."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.no_quantifiers_on_default_impl }
    : local_error)

let constr_no_mut_positional_field loc =
  (let message =
     ("Mutable constructor fields are only allowed on labelled arguments."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.constr_no_mut_positional_field }
    : local_error)

let func_param_num_mismatch ~expected ~actual ~ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "This function has type ";
         ty;
         ", which expects ";
         Int.to_string expected;
         " argument(s), but is given ";
         Int.to_string actual;
         " argument(s).";
       ]
   in
   { message; loc; error_code = Error_code.func_param_num_mismatch }
    : local_error)

let generic_type_mismatch ~header ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         header;
         "\n        has type : ";
         actual;
         "\n        wanted   : ";
         expected;
       ]
   in
   { message; loc; error_code = Error_code.type_mismatch }
    : local_error)

let type_mismatch ~expected ~actual ~loc =
  generic_type_mismatch ~header:"Type Mismatch" ~expected ~actual ~loc

let expr_unify ~expected ~actual ~loc =
  generic_type_mismatch ~header:"Expr Type Mismatch" ~expected ~actual ~loc

let pat_unify ~expected ~actual ~loc =
  generic_type_mismatch ~header:"Pattern Type Mismatch" ~expected ~actual ~loc

let param_unify ~name ~expected ~actual ~loc =
  generic_type_mismatch
    ~header:
      ("Parameter Type Mismatch(parameter " ^ name ^ ")" : Stdlib.String.t)
    ~expected ~actual ~loc

let constr_unify ~name ~expected ~actual ~loc =
  generic_type_mismatch
    ~header:("Constr Type Mismatch(constructor " ^ name ^ ")" : Stdlib.String.t)
    ~expected ~actual ~loc

let cascade_type_mismatch ~actual ~loc =
  (let message =
     ("This method returns " ^ actual
      ^ ", but only methods that return Unit can be used with `..`."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.type_mismatch }
    : local_error)

let cannot_resolve_method ~ty ~name ~loc ~hint =
  (let message =
     match hint with
     | `Record ->
         Stdlib.String.concat ""
           [
             "Type ";
             ty;
             " has no method ";
             name;
             ".\n  (hint: to apply record field as function, write `(x.";
             name;
             ")(...)` instead)";
           ] [@merlin.hide]
     | `Trait ->
         Stdlib.String.concat ""
           [ "Trait "; ty; " has no method "; name; "." ] [@merlin.hide]
     | `No_hint ->
         Stdlib.String.concat ""
           [ "Type "; ty; " has no method "; name; "." ] [@merlin.hide]
   in
   { message; loc; error_code = Error_code.cannot_resolve_method }
    : local_error)

let cannot_resolve_infix_op ~method_name ~op_name ~ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Please implement the method ";
         method_name;
         " for the type ";
         ty;
         " to use the infix operator \"";
         op_name;
         "\".";
       ]
   in
   { message; loc; error_code = Error_code.cannot_resolve_infix_op }
    : local_error)

let ambiguous_trait_method ~label ~ty ~first ~second ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         label;
         " of type ";
         ty;
         " is ambiguious, it may come from trait ";
         first;
         " or ";
         second;
       ]
   in
   { message; loc; error_code = Error_code.ambiguous_trait_method }
    : local_error)

let cannot_resolve_trait ~loc ~message =
  ({ message; loc; error_code = Error_code.cannot_resolve_trait } : local_error)

let duplicated_label_in_decl ~label ~first_loc ~second_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The label ";
         label;
         "~ is declared twice in this function, first in ";
         Loc.to_string first_loc;
       ]
   in
   {
     message;
     loc = second_loc;
     error_code = Error_code.duplicated_label_in_decl;
   }
    : local_error)

let pkg_not_loaded ~pkg ~loc =
  (let message =
     ("Package \"" ^ pkg ^ "\" not found in the loaded packages."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.pkg_not_loaded }
    : local_error)

let unbound_value ~name ~loc =
  (let message =
     match (name : Longident.t) with
     | Lident id ->
         ("The value identifier " ^ id ^ " is unbound."
           : Stdlib.String.t)
           [@merlin.hide]
     | Ldot { pkg; id } ->
         Stdlib.String.concat ""
           [ "Value "; id; " not found in package `"; pkg; "`." ] [@merlin.hide]
   in
   { message; loc; error_code = Error_code.unbound_value }
    : local_error)

let unbound_trait ~name ~loc =
  (let message =
     match (name : Longident.t) with
     | Lident id ->
         ("The trait " ^ id ^ " is not found." : Stdlib.String.t) [@merlin.hide]
     | Ldot { pkg; id } ->
         Stdlib.String.concat ""
           [ "Trait "; id; " not found in package `"; pkg; "`." ] [@merlin.hide]
   in
   { message; loc; error_code = Error_code.unbound_trait }
    : local_error)

let unbound_type_or_trait ~name ~loc =
  (let message =
     ("The type/trait " ^ Longident.to_string name ^ " is not found."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unbound_type_or_trait }
    : local_error)

let unbound_field ~name ~loc =
  (let message = ("The field " ^ name ^ " is not found." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.unbound_field }
    : local_error)

let not_a_record ~may_be_method ~ty ~kind ~loc =
  (let base_message =
     Stdlib.String.concat ""
       [
         "This expression has type ";
         ty;
         ", which is a ";
         kind;
         " type and not a record.";
       ]
   in
   let message =
     match may_be_method with
     | Some field ->
         Stdlib.String.concat ""
           [
             base_message;
             "\n  (hint: to pass method as function, write `fn (...) { x.";
             field;
             "(...) }` instead)";
           ] [@merlin.hide]
     | None -> base_message
   in
   { message; loc; error_code = Error_code.not_a_record }
    : local_error)

let not_a_variant ~ty ~kind ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The type ";
         ty;
         " is a ";
         kind;
         " type and not a variant, so it does not have any constructor.";
       ]
   in
   { message; loc; error_code = Error_code.not_a_variant }
    : local_error)

let field_not_found ~ty ~label ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "The record type "; ty; " does not have the field "; label; "." ]
   in
   { message; loc; error_code = Error_code.field_not_found }
    : local_error)

let constr_not_found ~ty ~constr ~loc =
  (let message =
     match ty with
     | None ->
         ("The value " ^ constr ^ " is undefined."
           : Stdlib.String.t)
           [@merlin.hide]
     | Some ty ->
         Stdlib.String.concat ""
           [
             "The variant type ";
             ty;
             " does not have the constructor ";
             constr;
             ".";
           ] [@merlin.hide]
   in
   { message; loc; error_code = Error_code.constr_not_found }
    : local_error)

let type_not_found ~tycon ~loc =
  (let message = ("The type " ^ tycon ^ " is undefined." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.type_not_found }
    : local_error)

let cannot_resolve_record ~labels ~loc =
  (let labels = String.concat ", " labels in
   let message =
     ("There is no record definition with the fields: " ^ labels ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.cannot_resolve_record }
    : local_error)

let ambiguous_record ~names ~loc =
  (let names = String.concat ", " names in
   let message =
     ("Mutiple possible record types detected: " ^ names
      ^ ", please add more annotation."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.ambiguous_record }
    : local_error)

let readonly_type ~name ~loc =
  (let message =
     ("Cannot create values of the read-only type: " ^ name ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.readonly_type }
    : local_error)

let cannot_create_struct_with_priv_field ~name ~loc =
  (let message =
     ("Cannot create values of struct type " ^ name
      ^ " because it contains private field(s)."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.readonly_type }
    : local_error)

let pkg_not_imported ~name ~action ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "Cannot "; action; ": package "; name; " is not imported." ]
   in
   { message; loc; error_code = Error_code.pkg_not_imported }
    : local_error)

let type_not_object_safe ~name ~reasons ~loc =
  (let header =
     ("Trait object for " ^ name ^ " is not allowed:" : Stdlib.String.t)
   in
   let message =
     match reasons with
     | [] -> assert false
     | reason :: [] -> header ^ " " ^ reason
     | reasons -> header ^ "\n    " ^ String.concat "\n    " reasons
   in
   { message; loc; error_code = Error_code.type_not_object_safe }
    : local_error)

let method_not_found_in_trait ~trait ~method_name ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "There is no method "; method_name; " in trait "; type_path_name trait;
       ]
   in
   { message; loc; error_code = Error_code.method_not_found_in_trait }
    : local_error)

let cannot_use_method_of_abstract_trait ~trait ~method_name ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Cannot use method ";
         method_name;
         " of abstract trait ";
         type_path_name trait;
       ]
   in
   { message; loc; error_code = Error_code.method_not_found_in_trait }
    : local_error)

let type_constr_arity_mismatch ~kind ~id ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The ";
         kind;
         " ";
         Longident.to_string id;
         " expects ";
         Int.to_string expected;
         " argument(s), but is here given ";
         Int.to_string actual;
         " argument(s).";
       ]
   in
   { message; loc; error_code = Error_code.type_constr_arity_mismatch }
    : local_error)

let unexpected_partial_type loc =
  (let message =
     ("Partial type is not allowed in toplevel declarations." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unexpected_partial_type }
    : local_error)

let invalid_stub_type loc =
  (let message = ("Invalid stub type." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.invalid_stub_type }
    : local_error)

let duplicate_record_field ~label ~context ~loc =
  (let message =
     match context with
     | `Pattern ->
         ("The record field " ^ label
          ^ " is matched several times in this pattern."
           : Stdlib.String.t)
           [@merlin.hide]
     | `Creation ->
         ("The record field " ^ label ^ " is defined several times."
           : Stdlib.String.t)
           [@merlin.hide]
   in
   { message; loc; error_code = Error_code.duplicate_record_field }
    : local_error)

let missing_fields_in_record ~labels ~ty ~context ~loc =
  (let message =
     let labels_str = String.concat ", " labels in
     match context with
     | `Pattern ->
         ("Record fields " ^ labels_str
          ^ " are unmatched, use `..` to ignore them."
           : Stdlib.String.t)
           [@merlin.hide]
     | `Creation ->
         Stdlib.String.concat ""
           [ "Record fields "; labels_str; " are undefined for type "; ty ]
         [@merlin.hide]
   in
   { message; loc; error_code = Error_code.missing_fields_in_record }
    : local_error)

let superfluous_field ~label ~ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "The fields "; label; " is not defined in the record type "; ty; "." ]
   in
   { message; loc; error_code = Error_code.superfluous_field }
    : local_error)

let cannot_depend_private ~entity ~loc =
  (let message =
     ("A public definition cannot depend on private " ^ entity
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.cannot_depend_private }
    : local_error)

let alias_with_priv_target_in_pub_sig ~alias ~priv_type ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The alias ";
         alias;
         " cannot be used in public signature, becasu it mentions private type ";
         priv_type;
       ]
   in
   { message; loc; error_code = Error_code.cannot_depend_private }
    : local_error)

let pkg_not_found ~pkg =
  (let message =
     ("Package " ^ pkg ^ " not found when loading packages." : Stdlib.String.t)
   in
   { message; loc = Loc.no_location; error_code = Error_code.pkg_not_found }
    : Diagnostics.report)

let pkg_wrong_format ~pkg =
  (let message =
     ("The package file of " ^ pkg ^ " is in wrong format" : Stdlib.String.t)
   in
   { message; loc = Loc.no_location; error_code = Error_code.pkg_wrong_format }
    : Diagnostics.report)

let pkg_magic_mismatch ~pkg =
  (let message =
     ("Magic number mismatch for the package file of " ^ pkg : Stdlib.String.t)
   in
   {
     message;
     loc = Loc.no_location;
     error_code = Error_code.pkg_magic_mismatch;
   }
    : Diagnostics.report)

let cycle_definitions ~cycle ~locs =
  (let rec make_string (idents : Ident.t list) head =
     match idents with
     | [] -> ""
     | i :: [] -> Ident.base_name i ^ " -> " ^ Ident.base_name head
     | i :: rest -> Ident.base_name i ^ " -> " ^ make_string rest head
   in
   let make_report loc =
     let message =
       ("Definition cycle detected : " ^ make_string cycle (List.hd cycle)
         : Stdlib.String.t)
     in
     ({ message; loc; error_code = Error_code.cycle_definitions }
       : Diagnostics.report)
   in
   Lst.map locs make_report
    : Diagnostics.report list)

let redeclare ~kind ~name ~first_loc ~second_loc
    ~(extra_message : string option) =
  (let message =
     Stdlib.String.concat ""
       [
         "The ";
         kind;
         " ";
         name;
         " is declared twice: it was previously defined at ";
         Loc.to_string first_loc;
         ".";
       ]
   in
   let message =
     match extra_message with
     | None -> message
     | Some extra_message -> message ^ "\n  " ^ extra_message
   in
   { message; loc = second_loc; error_code = Error_code.redeclare }
    : Diagnostics.report)

let value_redeclare ~name ~first_loc ~second_loc ~extra_message =
  redeclare ~kind:"toplevel identifier" ~name ~first_loc ~second_loc
    ~extra_message

let type_redeclare ~name ~first_loc ~second_loc ~extra_message =
  redeclare ~kind:"type" ~name ~first_loc ~second_loc ~extra_message

let local_type_redeclare ~name ~first_loc ~second_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The local type ";
         name;
         " is declared twice: it was previously defined at ";
         Loc.to_string first_loc;
         ".";
       ]
   in
   { message; loc = second_loc; error_code = Error_code.redeclare }
    : local_error)

let trait_redeclare ~name ~first_loc ~second_loc =
  redeclare ~kind:"trait" ~name ~first_loc ~second_loc ~extra_message:None

let direct_use_redeclare ~name ~first_loc ~second_loc =
  redeclare ~kind:"import item" ~name ~first_loc ~second_loc ~extra_message:None

let type_trait_duplicate ~name ~first_kind ~first_loc ~second_kind ~second_loc
    ~extra_message =
  (let message =
     Stdlib.String.concat ""
       [
         "The ";
         second_kind;
         " ";
         name;
         " duplicates with ";
         first_kind;
         " ";
         name;
         " previously defined at ";
         Loc.to_string first_loc;
         ".";
       ]
   in
   let message =
     match extra_message with
     | None -> message
     | Some extra_message -> message ^ "\n  " ^ extra_message
   in
   { message; loc = second_loc; error_code = Error_code.type_trait_duplicate }
    : Diagnostics.report)

let invalid_self_type loc =
  (let message = "Invalid type for \"self\": must be a type constructor" in
   { message; loc; error_code = Error_code.invalid_self_type }
    : local_error)

let cannot_determine_self_type loc =
  (let message =
     ("Cannot determine self type of extension method. [Self] does not occur \
       in the signature of the method"
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.cannot_determine_self_type }
    : local_error)

let field_duplicate ~name ~loc =
  (let message =
     ("field " ^ name ^ " is already declared." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.field_duplicate }
    : local_error)

let method_duplicate ~method_name ~type_name ~first_loc ~second_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The method ";
         method_name;
         " for type ";
         type_path_name type_name;
         " has been defined at ";
         Loc.to_string first_loc;
         ".";
       ]
   in
   { message; loc = second_loc; error_code = Error_code.method_duplicate }
    : local_error)

let constructor_duplicate ~name ~loc =
  (let message =
     ("The constructor " ^ name ^ " is duplicate." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.constructor_duplicate }
    : local_error)

let method_on_foreign_type ~method_name ~type_name ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Cannot define method ";
         method_name;
         " for foreign type ";
         type_path_name type_name;
       ]
   in
   { message; loc; error_code = Error_code.method_on_foreign_type }
    : local_error)

let ext_method_type_mismatch ~trait ~method_name ~expected ~actual ~loc =
  (let trait =
     Type_path.short_name ~cur_pkg_name:(Some !Basic_config.current_package)
       trait
   in
   let message =
     Stdlib.String.concat ""
       [
         "Method ";
         method_name;
         " of trait ";
         trait;
         " is expected to have type ";
         expected;
         ", it cannot be implemened with type ";
         actual;
       ]
   in
   { message; loc; error_code = Error_code.ext_method_type_mismatch }
    : local_error)

let ext_method_foreign_trait_foreign_type ~trait ~type_name ~method_name ~loc =
  (let trait = type_path_name trait in
   let type_name = type_path_name type_name in
   let message =
     Stdlib.String.concat ""
       [
         "Cannot define method ";
         method_name;
         " of foreign trait ";
         trait;
         " for foreign type ";
         type_name;
       ]
   in
   {
     message;
     loc;
     error_code = Error_code.ext_method_foreign_trait_foreign_type;
   }
    : local_error)

let priv_ext_shadows_pub_method ~method_name ~trait ~type_name ~prev_loc ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "This `impl` shadows method ";
         method_name;
         " of ";
         type_path_name type_name;
         " previously defined at ";
         Loc.to_string prev_loc;
         ". This will result in different implementations for ";
         type_path_name trait;
         " inside and outside current package.";
       ]
   in
   { message; loc; error_code = Error_code.priv_ext_shadows_pub_method }
    : Diagnostics.report)

let trait_not_implemented ~trait ~type_name ~failure_reasons ~loc =
  (let reasons = String.concat "\n  " failure_reasons in
   let message =
     Stdlib.String.concat ""
       [
         "Type ";
         type_path_name type_name;
         " does not implement trait ";
         type_path_name trait;
         ", although an `impl` is defined. hint:\n  ";
         reasons;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.trait_not_implemented }
    : Diagnostics.report)

let bad_operator_arity ~method_name ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "overloaded operator \"";
         method_name;
         "\" should accept ";
         Int.to_string expected;
         " arguments, but it accepts ";
         Int.to_string actual;
         " arguments";
       ]
   in
   { message; loc; error_code = Error_code.bad_operator_arity }
    : local_error)

let bad_operator_type ~method_name ~first ~second ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "overloaded operator \"";
         method_name;
         "\" has inconsistent parameter type: first parameter has type ";
         first;
         " while second parameter has type ";
         second;
       ]
   in
   { message; loc; error_code = Error_code.bad_operator_type }
    : local_error)

let missing_main ~loc =
  ({
     message = "Missing main function in the main package.";
     loc;
     error_code = Error_code.missing_main;
   }
    : Diagnostics.report)

let multiple_main ~first_loc ~second_loc =
  (let message =
     ("Main function is already defined at " ^ Loc.to_string first_loc ^ "."
       : Stdlib.String.t)
   in
   { message; loc = second_loc; error_code = Error_code.multiple_main }
    : Diagnostics.report)

let unexpected_main loc =
  ({
     loc;
     message = "Unexpected main function in the non-main package.";
     error_code = Error_code.unexpected_main;
   }
    : Diagnostics.report)

let unknown_intrinsic ~name ~loc =
  (let message = ("Unknown intrinsic " ^ name ^ "." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.unknown_intrinsic }
    : Diagnostics.report)

let multiple_intrinsic loc =
  (let message = "Multiple intrinsic is not unsupported." in
   { message; loc; error_code = Error_code.multiple_intrinsic }
    : Diagnostics.report)

let default_method_duplicate ~trait ~method_name ~first_loc ~second_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Method ";
         method_name;
         " of trait ";
         type_path_name trait;
         " already has a default implementation at ";
         Loc.to_string first_loc;
       ]
   in
   {
     message;
     loc = second_loc;
     error_code = Error_code.default_method_duplicate;
   }
    : local_error)

let default_method_on_foreign ~trait ~loc =
  (let message =
     ("Cannot provide default implementation for foreign trait "
      ^ type_path_name trait
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.default_method_on_foreign }
    : local_error)

let let_missing_annot ~name ~loc ~reason =
  (let message =
     match reason with
     | `Cannot_infer ->
         ("Cannot infer the type of variable " ^ name
          ^ ", please add more type annotation."
           : Stdlib.String.t)
           [@merlin.hide]
     | `Pub_not_literal ->
         ("Public definition " ^ name ^ " must be annotated with its type."
           : Stdlib.String.t)
           [@merlin.hide]
   in
   { message; loc; error_code = Error_code.let_missing_annot }
    : local_error)

let missing_param_annot ~name ~loc =
  (let message =
     ("Missing type annotation for the parameter " ^ name ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.missing_param_annot }
    : local_error)

let missing_return_annot loc =
  (let message =
     ("Missing type annotation for the return value." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.missing_return_annot }
    : local_error)

let derive_unsupported_trait ~tycon ~trait ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Don't know how to derive trait ";
         Longident.to_string trait;
         " for type ";
         type_path_name tycon;
       ]
   in
   { message; loc; error_code = Error_code.derive_unsupported_trait }
    : local_error)

let cannot_derive ~tycon ~trait ~reason ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Cannot derive trait ";
         Longident.to_string trait;
         " for type ";
         tycon;
         ": ";
         reason;
       ]
   in
   { message; loc; error_code = Error_code.cannot_derive }
    : local_error)

let derive_method_exists ~trait ~type_name ~method_name ~prev_loc ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Cannot derive trait ";
         type_path_name trait;
         " for ";
         type_path_name type_name;
         ": method ";
         method_name;
         " is already defined at ";
         Loc.to_string prev_loc;
       ]
   in
   { message; loc; error_code = Error_code.derive_method_exists }
    : local_error)

let arity_mismatch ~label ~expected ~actual ~has_label ~loc =
  (let arg_desc = if has_label then "positional arguments" else "arguments" in
   let message =
     Stdlib.String.concat ""
       [
         label;
         " requires ";
         Int.to_string expected;
         " ";
         arg_desc;
         ", but is given ";
         Int.to_string actual;
         " ";
         arg_desc;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.arity_mismatch }
    : local_error)

let constr_arity_mismatch ~name ~expected ~actual ~has_label ~loc =
  arity_mismatch
    ~label:("The constructor " ^ name : Stdlib.String.t)
    ~expected ~actual ~has_label ~loc

let constant_constr_cannot_have_args ~name ~loc =
  (let message =
     (name ^ " is a constant constructor, it cannot be applied to arguments."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.arity_mismatch }
    : local_error)

let fn_arity_mismatch ~func_ty ~expected ~actual ~has_label ~loc =
  arity_mismatch
    ~label:("This function has type " ^ func_ty ^ ", which" : Stdlib.String.t)
    ~expected ~actual ~has_label ~loc

let non_linear_pattern ~name ~loc =
  (let message =
     ("The identifier " ^ name ^ " is bound more than once in the same pattern."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.non_linear_pattern }
    : local_error)

let inconsistent_or_pattern ~name ~loc =
  (let message =
     ("Variable " ^ name ^ " is not bound in all patterns." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.inconsistent_or_pattern }
    : local_error)

let no_op_as_view ~ty ~loc =
  (let message =
     ("The type " ^ ty ^ " does not implement `op_as_view` method."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.no_op_as_view }
    : local_error)

let duplicated_fn_label ~label ~second_loc =
  (let message =
     ("The label " ^ label ^ "~ is supplied twice." : Stdlib.String.t)
   in
   { message; loc = second_loc; error_code = Error_code.duplicated_fn_label }
    : local_error)

let superfluous_arg_label ~label ~(kind : string) ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "This "; kind; " has no parameter with label "; label; "~." ]
   in
   { message; loc; error_code = Error_code.superfluous_fn_label }
    : local_error)

let missing_fn_label ~labels ~loc =
  (let labels_str = String.concat ", " (Lst.map labels (fun l -> l ^ "~")) in
   let message =
     ("The labels " ^ labels_str
      ^ " are required by this function, but not supplied."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.missing_fn_label }
    : local_error)

let not_mutable ~id ~loc =
  (let message =
     ("The variable " ^ Longident.to_string id ^ " is not mutable."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.not_mutable }
    : local_error)

let immutable_field ~label ~loc =
  (let message =
     ("The record field " ^ label ^ " is immutable." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.immutable_field }
    : local_error)

let no_tuple_index ~required ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         Int.to_string actual;
         "-tuple has no field `";
         Int.to_string required;
         "`";
       ]
   in
   { message; loc; error_code = Error_code.no_tuple_index }
    : local_error)

let tuple_not_mutable loc =
  (let message = "tuples are not mutable" in
   { message; loc; error_code = Error_code.tuple_not_mutable }
    : local_error)

let no_such_field ~ty ~field ~may_be_method ~loc =
  (let message =
     if may_be_method then
       Stdlib.String.concat ""
         [
           "The type ";
           ty;
           " has no field ";
           field;
           ".\n  (hint: to pass method as function, write `fn (...) { x.";
           field;
           "(...) }` instead)";
         ]
     else
       Stdlib.String.concat "" [ "The type "; ty; " has no field "; field; "." ]
   in
   { message; loc; error_code = Error_code.no_such_field }
    : local_error)

let record_type_missing loc =
  (let message = "Missing annotation for this empty record." in
   { message; loc; error_code = Error_code.record_type_missing }
    : local_error)

let not_a_record_type ~name ~loc =
  (let message =
     ("The type " ^ name ^ " is not a record type" : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.not_a_record_type }
    : local_error)

let mutate_readonly_field ~label ~loc =
  (let message =
     ("Cannot modify a read-only field: " ^ label : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.mutate_readonly_field }
    : local_error)

let overflow ~value ~loc =
  (let message =
     ("Integer literal " ^ value ^ " is out of range." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.overflow }
    : local_error)

let overloaded_string_interpolation_for_bytes ~loc =
  (let message =
     ("String literals with interpolation cannot be overloaded for `Bytes` \
       type."
       : Stdlib.String.t)
   in
   {
     message;
     loc;
     error_code = Error_code.overloaded_string_interpolation_for_bytes;
   }
    : local_error)

let not_a_type ~name ~loc =
  (let message = ("`" ^ name ^ "` is a trait, not a type" : Stdlib.String.t) in
   { message; loc; error_code = Error_code.not_a_type }
    : local_error)

let not_a_trait ~name ~loc =
  (let message = ("The type " ^ name ^ " is not a trait" : Stdlib.String.t) in
   { message; loc; error_code = Error_code.not_a_trait }
    : local_error)

let unsupported_pipe_expr loc =
  (let message = "Unsupported expression after the pipe operator." in
   { message; loc; error_code = Error_code.unsupported_pipe_expr }
    : local_error)

let outside_loop ~msg ~loc =
  ({
     loc;
     message = ("'" ^ msg ^ "' outside of a loop" : Stdlib.String.t);
     error_code = Error_code.outside_loop;
   }
    : local_error)

let loop_pat_arity_mismatch ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "This loop has ";
         Int.to_string expected;
         " arguments, but ";
         Int.to_string actual;
         " patterns are supplied";
       ]
   in
   { message; loc; error_code = Error_code.loop_pat_arity_mismatch }
    : local_error)

let continue_arity_mismatch ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Current loop expects ";
         Int.to_string expected;
         " arguments, but `continue` is supplied with ";
         Int.to_string actual;
         " arguments";
       ]
   in
   { message; loc; error_code = Error_code.continue_arity_mismatch }
    : local_error)

let break_type_mismatch ~expected ~actual ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Current loop has result type ";
         expected;
         ", but `break` is supplied with ";
         actual;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.break_type_mismatch }
    : local_error)

let unknown_binder_in_for_steps ~name ~loc =
  (let message =
     ("Unknown binder " ^ name
      ^ " in the for-loop steps. Binders in the steps must be declared in the \
         initialization block of the for-loop."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unknown_binder_in_for_steps }
    : local_error)

let duplicate_for_binder ~name ~loc =
  (let message =
     (name ^ " is declared multiple times in this for-loop" : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.duplicate_for_binder }
    : local_error)

let need_else_branch ~loop_kind ~ty ~loc =
  (let loop_kind_string =
     match loop_kind with `For -> "for" | `While -> "while"
   in
   let message =
     Stdlib.String.concat ""
       [
         "The ";
         loop_kind_string;
         " loop is expected to yield a value of type ";
         ty;
         ", please add an `else` branch.";
       ]
   in
   { message; loc; error_code = Error_code.need_else_branch }
    : local_error)

let invalid_return loc =
  ({
     loc;
     message = ("Return must be inside a function." : Stdlib.String.t);
     error_code = Error_code.invalid_return;
   }
    : local_error)

let invalid_break ~loop_kind ~loc =
  (let loop_kind_string =
     match loop_kind with `For -> "for" | `While -> "while"
   in
   let message =
     ("The " ^ loop_kind_string
      ^ " loop is not expected to yield a value, please remove the argument of \
         the `break` or add an `else` branch."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.invalid_break }
    : local_error)

let ambiguous_break loc =
  (let message = ("The usage break statement is invalid." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.ambiguous_break }
    : local_error)

let ambiguous_continue loc =
  (let message =
     ("The usage continue statement is invalid." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.ambiguous_continue }
    : local_error)

let constr_no_such_field ~ty ~constr ~field ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Constructor ";
         constr;
         " of type ";
         type_path_name ty;
         " has no field ";
         field;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.constr_no_such_field }
    : local_error)

let no_local_labelled_function loc =
  (let message = "Only toplevel functions can have labelled arguments." in
   { message; loc; error_code = Error_code.no_local_labelled_function }
    : local_error)

let unsupported_autofill ~ty ~loc =
  (let message =
     ("Cannot auto-fill parameter of type " ^ ty : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unsupported_autofill }
    : local_error)

let found_hole loc =
  (let message = "Found hole _ " in
   { message; loc; error_code = Error_code.found_hole }
    : local_error)

let no_first_class_labelled_function loc =
  (let message =
     "Function with labelled arguments can only be applied directly."
   in
   { message; loc; error_code = Error_code.no_first_class_labelled_function }
    : local_error)

let cannot_use_map_pattern_no_method ~ty ~loc =
  (let message =
     ("Please implement method `op_get` for type " ^ ty
      ^ " to match it with map pattern."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.cannot_use_map_pattern }
    : local_error)

let cannot_use_map_pattern_method_type_mismatch ~ty ~actual_ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "Cannot match type ";
         ty;
         " with map pattern: its method `op_get` has incorrect type:\n\
         \    wanted: (Self, K) -> Option[V]\n\
         \    has   : ";
         actual_ty;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.cannot_use_map_pattern }
    : local_error)

let nontoplevel_func_cannot_have_labelled_arg ~loc =
  (let message =
     ("This function is not a toplevel function, so it cannot have labelled \
       arguments"
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unknown_func_labelled_arg }
    : local_error)

let error_type_mismatch ~expected_ty ~actual_ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The error type is mismatched:\n    wanted: ";
         expected_ty;
         "\n    has   : ";
         actual_ty;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.cannot_use_map_pattern }
    : local_error)

let unhandled_error ~err_ty ~loc =
  (let message =
     ("The application might raise errors of type " ^ err_ty
      ^ ", but it's not handled. Try adding a infix operator `!` or `?` to the \
         application, so that it looks like `...!(...)` or `...?(...)`."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.unhandled_error }
    : local_error)

let invalid_apply_attr ~(kind : [ `UnknownType | `NoErrorType | `Constructor ])
    ~attr ~loc =
  (let message =
     match kind with
     | `Constructor ->
         ("The attribute `" ^ attr ^ "` cannot be used on constructors."
           : Stdlib.String.t)
           [@merlin.hide]
     | `UnknownType ->
         ("The type of function is unknown so the attribute `" ^ attr
          ^ "` cannot be used."
           : Stdlib.String.t)
           [@merlin.hide]
     | `NoErrorType ->
         ("The attribute `" ^ attr
          ^ "` cannot be used on application that does not raise errors"
           : Stdlib.String.t)
           [@merlin.hide]
   in
   { message; loc; error_code = Error_code.invalid_apply_attr }
    : local_error)

let invalid_raise ~(kind : [ `Catchall | `Rethrow of string | `Raise ]) ~loc =
  (let reason =
     match kind with
     | `Catchall -> "catch! will rethrow unhandled error, and"
     | `Rethrow attr ->
         ("`" ^ attr
          ^ "` operator will rethrow the error raised in the function \
             application, and"
           : Stdlib.String.t)
           [@merlin.hide]
     | `Raise -> "raise"
   in
   {
     loc;
     message =
       (reason
        ^ " can only be used inside a function with error types in its \
           signature. Please fix the return type of this function. \n\
           For local functions, you could use `fn func_name!()` to declare the \
           function with an error type, and let the compiler infer the error \
           type.\n\
           For anonymous functions, use `fn!` instead of `fn` to declare the \
           function with an error type."
         : Stdlib.String.t);
     error_code = Error_code.invalid_raise;
   }
    : local_error)

let ambiguous_constructor ~name ~first_ty ~second_ty ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The constructor ";
         name;
         " is ambiguous: it may come from type ";
         first_ty;
         " or ";
         second_ty;
         ".";
       ]
   in
   { message; loc; error_code = Error_code.ambiguous_constructor }
    : local_error)

let double_exclamation_with_cascade loc =
  (let message = ("`?` operator cannot be used with `..`." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.double_exclamation_with_cascade }
    : local_error)

let not_error_subtype ty loc =
  (let message = ("Type " ^ ty ^ " is not an error type." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.not_error_subtype }
    : local_error)

let invalid_type_alias_target loc =
  (let message =
     ("Target of type alias must not be a type parameter." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.invalid_type_alias_target }
    : local_error)

let cycle_in_type_alias ~cycle ~kind ~loc =
  (let cycle = String.concat " -> " cycle in
   let type_or_trait = match kind with `Type -> "type" | `Trait -> "trait" in
   let message =
     Stdlib.String.concat ""
       [ "Found cycle "; cycle; " in "; type_or_trait; " alias." ]
   in
   { message; loc; error_code = Error_code.cycle_in_type_alias }
    : local_error)

let type_alias_cannot_derive loc =
  (let message = "`derive` is not allowed for type alias" in
   { message; loc; error_code = Error_code.type_alias_cannot_derive }
    : local_error)

let type_alias_not_a_constructor ~alias_name ~loc =
  (let message =
     ("The type alias " ^ alias_name
      ^ " is a function type, not a type constructor."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.type_alias_not_a_constructor }
    : local_error)

let invalid_test_parameter loc =
  (let message =
     "Invalid test parameter. Only one parameter with type `@test.T` is \
      allowed."
   in
   { message; loc; error_code = Error_code.invalid_test_parameter }
    : Diagnostics.report)

let foreach_loop_variable_count_mismatch ~actual ~expected ~loc =
  (let expected =
     match expected with
     | None -> "at most 2"
     | Some expected -> Int.to_string expected
   in
   let message =
     Stdlib.String.concat ""
       [
         "This `for .. in` loop has ";
         Int.to_string actual;
         " loop variables, but ";
         expected;
         " is expected.";
       ]
   in
   {
     message;
     loc;
     error_code = Error_code.foreach_loop_variable_count_mismatch;
   }
    : local_error)

let anonymous_missing_error_annotation loc =
  (let message =
     ("The return type of this anonymous function is expected include an error \
       type. Please add the error type to the return type annotation or use \
       `fn!` instead."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.anonymous_missing_error_annotation }
    : local_error)

let inconsistent_impl ~trait ~type_name ~reason ~loc1 ~loc2 =
  (let reason =
     match reason with
     | `Self_type_mismatch (ty1, ty2) ->
         Stdlib.String.concat ""
           [
             "implementations have different self type:\n\
             \  self type of first impl : ";
             ty1;
             "\n  self type of second impl: ";
             ty2;
           ] [@merlin.hide]
     | `Type_parameter_bound ->
         ("type parameters of implementations have different constraints"
           : Stdlib.String.t)
           [@merlin.hide]
   in
   let message =
     Stdlib.String.concat ""
       [
         "Inconsistent `impl` of trait ";
         trait;
         " for ";
         type_name;
         " at ";
         Loc.to_string loc1;
         " and ";
         Loc.to_string loc2;
         ": ";
         reason;
       ]
   in
   { message; loc = loc2; error_code = Error_code.inconsistent_impl }
    : Diagnostics.report)

let no_default_for_question_optional ~label ~loc =
  (let message =
     ("The parameter " ^ label ^ "? already has default value `None`."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.no_default_for_question_optional }
    : local_error)

let invalid_extra_delimiter ~delimiter ~loc =
  (let message =
     ("Expecting a newline or `;` here, but encountered " ^ delimiter ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.invalid_extra_delimiter }
    : Diagnostics.report)

let not_a_newtype ~actual_ty ~loc =
  (let message =
     ("This expression has type " ^ actual_ty ^ ", which is not a newtype."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.not_a_newtype }
    : local_error)

let range_operator_only_in_for loc =
  (let message =
     ("Range operators are currently only supported in `for .. in` loops."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.range_operator_only_in_for }
    : local_error)

let range_operator_unsupported_type ~actual_ty ~loc =
  (let message =
     ("Range operators only support builtin integer types, they cannot be used \
       on type " ^ actual_ty ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.range_operator_unsupported_type }
    : local_error)

let non_unit_cannot_be_ignored ~ty ~loc =
  (let message =
     ("This expression has type " ^ ty
      ^ ", its value cannot be implicitly ignored (hint: use `ignore(...)` or \
         `let _ = ...` to explicitly ignore it)."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.non_unit_cannot_be_ignored }
    : local_error)

let c_stub_invalid_function_name loc =
  (let message = "Invalid C function name in extern \"C\" declaration" in
   { message; loc; error_code = Error_code.c_stub_invalid_function_name }
    : Diagnostics.report)

let invalid_question_arg_application ~label ~loc =
  (let message =
     Stdlib.String.concat ""
       [
         "This form of application is invalid for argument ";
         label;
         "~, because it is not declared with ";
         label;
         "? : _.";
       ]
   in
   { message; loc; error_code = Error_code.invalid_question_arg_application }
    : local_error)

let direct_use_not_found ~pkg ~id ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "Value "; id; " not found in package `"; pkg; "`." ]
   in
   { message; loc; error_code = Error_code.unbound_value }
    : Diagnostics.report)

let constant_not_constant ~reason ~loc =
  (let message =
     match reason with
     | `Not_const -> "This 'const' declaration is not constant."
     | `Unsupported ->
         "This kind of computation is currently not supported in `const` \
          declaration."
     | `Arith_error ->
         "Arithmetic error detected when evaluating `const` declaration."
   in
   { message; loc; error_code = Error_code.constant_not_constant }
    : local_error)

let invalid_constant_type ~ty ~loc =
  (let message =
     (ty
      ^ " is not a valid constant type, only immutable primitive types are \
         allowed."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.invalid_constant_type }
    : local_error)

let constant_constr_duplicate ~name ~const_loc ~constr_loc =
  (let message =
     Stdlib.String.concat ""
       [
         "The constant ";
         name;
         " duplicates with constructor declared at ";
         Loc.to_string constr_loc;
         ".";
       ]
   in
   { message; loc = const_loc; error_code = Error_code.redeclare }
    : Diagnostics.report)

let constant_pat_with_args ~name ~loc =
  (let message =
     ("'" ^ name
      ^ "' is a constant, not a constructor, it cannot be applied to arguments."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.constant_pat_with_args }
    : local_error)

let cannot_implement_sealed_trait ~trait ~trait_vis ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "Cannot implement trait '"; trait; "' because it is "; trait_vis; "." ]
   in
   { message; loc; error_code = Error_code.cannot_implement_abstract_trait }
    : local_error)

let range_pattern_unsupported_type ~ty ~loc =
  (let message =
     ("Type " ^ ty ^ " is not supported by range pattern." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.range_pattern_unsupported_type }
    : local_error)

let range_pattern_invalid_range ~inclusive ~loc =
  (let message =
     if inclusive then "Range pattern `a..=b` must satisfy `a <= b`."
     else "Range pattern `a..<b` must satisfy `a < b`."
   in
   { message; loc; error_code = Error_code.range_pattern_invalid_range }
    : local_error)

let use_undeclared_loop_label ~loc label =
  (let message = ("The label " ^ label ^ " is undeclared." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.use_undeclared_loop_label }
    : local_error)

let unused_tvar ~name ~loc =
  (let message = ("Unused type parameter '" ^ name ^ "'" : Stdlib.String.t) in
   { message; loc; error_code = Error_code.unused_tvar }
    : local_error)

let async_not_allowed_in_context ~context ~loc =
  (let message =
     ("cannot call async function in " ^ context : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.async_not_allowed_in_context }
    : local_error)

let async_call_not_marked loc =
  (let message =
     ("async function call must be marked with `!!`" : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.async_call_attr_mismatch }
    : local_error)

let double_excl_not_async_call loc =
  (let message =
     ("this function call is marked with `!!`, but it is not async"
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.async_call_attr_mismatch }
    : local_error)

let func_ref_no_capture ~captures ~loc =
  (let captures = String.concat ", " captures in
   let message =
     ("This function is expected to be a capture-free because its expected \
       type is `FuncRef`, but it captures " ^ captures ^ "."
       : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.func_ref_no_capture }
    : local_error)

let constant_not_found ~name ~loc =
  (let message = ("Constant " ^ name ^ " not found." : Stdlib.String.t) in
   { message; loc; error_code = Error_code.constant_not_found }
    : local_error)

let enum_tag_duplicate ~first ~tag ~loc =
  (let message =
     Stdlib.String.concat ""
       [ "The tag "; tag; " is already used by constructor "; first; "." ]
   in
   { message; loc; error_code = Error_code.enum_tag_duplicate }
    : local_error)

let non_constant_enum_no_custom_tag loc =
  (let message =
     ("This enum is not constant, it cannot have custom tag." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.non_constant_enum_no_custom_tag }
    : local_error)

let cycle_in_const_decl ~cycle ~loc =
  (let cycle = String.concat " -> " cycle in
   let message =
     ("Found cycle " ^ cycle ^ " in `const` declaration." : Stdlib.String.t)
   in
   { message; loc; error_code = Error_code.cycle_in_const_decl }
    : Diagnostics.report)
