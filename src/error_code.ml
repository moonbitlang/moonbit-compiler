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


type t = int

include struct
  let _ = fun (_ : t) -> ()
  let sexp_of_t = (Moon_sexp_conv.sexp_of_int : t -> S.t)
  let _ = sexp_of_t

  let compare =
    (fun a__001_ -> fun b__002_ -> Stdlib.compare (a__001_ : int) b__002_
      : t -> t -> int)

  let _ = compare

  let equal =
    (fun a__003_ -> fun b__004_ -> Stdlib.( = ) (a__003_ : int) b__004_
      : t -> t -> bool)

  let _ = equal
end

let swallow_error = 0
let internal = 1
let alert = 2000
let lexing_error = 3001
let parse_error = 3002
let invalid_init_or_main = 3003
let missing_parameter_list = 3004
let invalid_visibility = 3005
let enum_no_individual_visibility = 3006
let dotdot_in_middle_of_pattern = 3007
let array_pat_multiple_dotdot = 3008
let record_pattern_only_dotdot = 3009
let positional_argument_no_default = 3010
let invalid_left_value = 3011
let cannot_mix_record_and_map_pat = 3012
let inline_wasm_syntax_error = 3014
let no_default_for_question_optional = 3015
let invalid_tilde_argument = 3016
let json_parse_error = 3017
let bad_range_pattern_operand = 3018
let inclusive_range_pattern_no_upper_bound = 3019
let invalid_equal_in_struct_expr = 3020
let invalid_extra_delimiter = 3800
let duplicate_tvar = 4000
let field_visibility = 4001
let unsupported_modifier = 4002
let reserved_type_name = 4003
let trait_method_cannot_poly = 4004
let trait_duplicate_method = 4005
let duplicate_local_fns = 4006
let illform_constr_arg = 4007
let ffi_cannot_poly = 4008
let matchfn_arity_mismatch = 4009
let no_vis_on_default_impl = 4010
let no_quantifiers_on_default_impl = 4011
let constr_no_mut_positional_field = 4012
let func_param_num_mismatch = 4013
let type_mismatch = 4014
let cannot_resolve_method = 4015
let cannot_resolve_infix_op = 4016
let ambiguous_trait_method = 4017
let cannot_resolve_trait = 4018
let duplicated_label_in_decl = 4019
let pkg_not_loaded = 4020
let unbound_value = 4021
let unbound_trait = 4023
let unbound_type_or_trait = 4024
let unbound_field = 4026
let unused_tvar = 4027
let not_a_record = 4028
let not_a_variant = 4029
let field_not_found = 4030
let constr_not_found = 4031
let type_not_found = 4032
let cannot_resolve_record = 4033
let ambiguous_record = 4034
let readonly_type = 4036
let pkg_not_imported = 4037
let type_not_object_safe = 4038
let method_not_found_in_trait = 4039
let type_constr_arity_mismatch = 4040
let unexpected_partial_type = 4041
let invalid_stub_type = 4042
let duplicate_record_field = 4043
let missing_fields_in_record = 4044
let superfluous_field = 4045
let cannot_depend_private = 4046
let pkg_not_found = 4047
let pkg_wrong_format = 4048
let pkg_magic_mismatch = 4049
let cycle_definitions = 4050
let redeclare = 4051
let type_trait_duplicate = 4052
let invalid_self_type = 4053
let cannot_determine_self_type = 4054
let field_duplicate = 4055
let method_duplicate = 4056
let constructor_duplicate = 4057
let method_on_foreign_type = 4059
let ext_method_type_mismatch = 4060
let ext_method_foreign_trait_foreign_type = 4061
let priv_ext_shadows_pub_method = 4062
let trait_not_implemented = 4063
let bad_operator_arity = 4065
let bad_operator_type = 4066
let missing_main = 4067
let multiple_main = 4068
let unexpected_main = 4069
let unknown_intrinsic = 4070
let multiple_intrinsic = 4071
let default_method_duplicate = 4072
let default_method_on_foreign = 4073
let let_missing_annot = 4074
let missing_param_annot = 4075
let missing_return_annot = 4076
let derive_unsupported_trait = 4077
let cannot_derive = 4078
let derive_method_exists = 4079
let arity_mismatch = 4080
let non_linear_pattern = 4081
let inconsistent_or_pattern = 4082
let no_op_as_view = 4083
let duplicated_fn_label = 4084
let superfluous_fn_label = 4085
let missing_fn_label = 4086
let not_mutable = 4087
let immutable_field = 4088
let no_tuple_index = 4089
let tuple_not_mutable = 4090
let no_such_field = 4091
let record_type_missing = 4092
let not_a_record_type = 4093
let mutate_readonly_field = 4094
let overflow = 4095
let overloaded_string_interpolation_for_bytes = 4096
let not_a_type = 4099
let not_a_trait = 4100
let unsupported_pipe_expr = 4101
let outside_loop = 4102
let loop_pat_arity_mismatch = 4103
let continue_arity_mismatch = 4104
let break_type_mismatch = 4105
let unknown_binder_in_for_steps = 4106
let duplicate_for_binder = 4107
let need_else_branch = 4108
let invalid_return = 4109
let invalid_break = 4110
let ambiguous_break = 4111
let ambiguous_continue = 4112
let constr_no_such_field = 4113
let no_local_labelled_function = 4114
let unsupported_autofill = 4115
let found_hole = 4116
let no_first_class_labelled_function = 4117
let cannot_use_map_pattern = 4118
let unknown_func_labelled_arg = 4119
let unhandled_error = 4120
let invalid_apply_attr = 4121
let invalid_raise = 4122
let ambiguous_constructor = 4124
let double_exclamation_with_cascade = 4125
let not_error_subtype = 4127
let invalid_type_alias_target = 4128
let cycle_in_type_alias = 4129
let type_alias_cannot_derive = 4130
let type_alias_not_a_constructor = 4131
let invalid_test_parameter = 4132
let foreach_loop_variable_count_mismatch = 4133
let anonymous_missing_error_annotation = 4134
let inconsistent_impl = 4135
let not_a_newtype = 4136
let range_operator_only_in_for = 4137
let range_operator_unsupported_type = 4138
let non_unit_cannot_be_ignored = 4139
let c_stub_invalid_function_name = 4140
let invalid_question_arg_application = 4141
let constant_not_constant = 4142
let invalid_constant_type = 4143
let constant_pat_with_args = 4144
let cannot_implement_abstract_trait = 4145
let range_pattern_unsupported_type = 4146
let range_pattern_invalid_range = 4147
let use_undeclared_loop_label = 4148
let async_not_allowed_in_context = 4149
let async_call_attr_mismatch = 4150
let func_ref_no_capture = 4151
let constant_not_found = 4152
let enum_tag_duplicate = 4153
let non_constant_enum_no_custom_tag = 4154
let cycle_in_const_decl = 4155
