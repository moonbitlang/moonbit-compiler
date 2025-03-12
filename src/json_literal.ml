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
module Loc = Rloc

let type_path_json = Basic_type_path.Builtin.type_path_json

type json_constructor = string

let true_ : json_constructor = "True"
let false_ : json_constructor = "False"
let number : json_constructor = "Number"
let string : json_constructor = "String"
let array : json_constructor = "Array"
let object_ : json_constructor = "Object"

let get_json_constr_info ~global_env ~diagnostics constr_name ~action ~loc =
  match Global_env.find_type_by_path global_env type_path_json with
  | Some { ty_desc = Variant_type constrs; _ } ->
      Lst.find_first constrs (fun constr -> constr.constr_name = constr_name)
  | Some _ -> None
  | None ->
      Typeutil.add_local_typing_error diagnostics
        (Errors.pkg_not_imported ~name:"moonbitlang/core/json"
           ~action:
             ((action ^ " of type " ^ Type_path_util.name type_path_json
               : Stdlib.String.t)
               [@merlin.hide])
           ~loc);
      None

let make_json_pat ~global_env ~diagnostics constr_name args ~loc =
  (let args =
     Lst.mapi args (fun pos ->
         fun pat ->
          (Constr_pat_arg { pat; kind = Positional; pos }
            : Typedtree.constr_pat_arg))
   in
   match
     get_json_constr_info ~global_env ~diagnostics constr_name ~loc
       ~action:"destruct value"
   with
   | Some { cs_tag; cs_args; cs_arity_; _ } ->
       Tpat_constr
         {
           type_name = None;
           constr = { name = constr_name; loc_ = Loc.no_location };
           args;
           tag = cs_tag;
           ty = Stype.json;
           used_error_subtyping = false;
           arity_ = cs_arity_;
           all_args_ = cs_args;
           loc_ = loc;
         }
   | None -> Tpat_any { ty = Stype.json; loc_ = loc }
    : Typedtree.pat)

let make_json_const_expr ~global_env ~diagnostics constr_name ~loc =
  (let tag =
     match
       get_json_constr_info ~global_env ~diagnostics constr_name ~loc
         ~action:"create value"
     with
     | Some { cs_tag; _ } -> cs_tag
     | None -> Typeutil.unknown_tag
   in
   Texpr_constr
     {
       type_name = None;
       constr = { name = constr_name; loc_ = Loc.no_location };
       tag;
       ty = Stype.json;
       arity_ = Fn_arity.simple 0;
       loc_ = loc;
     }
    : Typedtree.expr)

let make_json_expr ~global_env ~diagnostics constr_name arg ~loc =
  (let tag =
     match
       get_json_constr_info ~global_env ~diagnostics constr_name ~loc
         ~action:"create value"
     with
     | Some { cs_tag; _ } -> cs_tag
     | None -> Typeutil.unknown_tag
   in
   Texpr_apply
     {
       func =
         Texpr_constr
           {
             type_name = None;
             constr = { name = constr_name; loc_ = Loc.no_location };
             tag;
             ty =
               Builtin.type_arrow
                 [ Typedtree_util.type_of_typed_expr arg ]
                 Stype.json ~err_ty:None ~is_async:false;
             arity_ = Fn_arity.simple 1;
             loc_ = Loc.no_location;
           };
       args = [ { arg_value = arg; arg_kind = Positional } ];
       ty = Stype.json;
       kind_ = Normal;
       loc_ = loc;
     }
    : Typedtree.expr)
