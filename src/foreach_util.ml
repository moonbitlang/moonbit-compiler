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


module Ident = Basic_core_ident
module Vec = Basic_vec
module Lst = Basic_lst

let iter_result_end : Core.expr = Core.const (C_int { v = 0l; repr = None })

let iter_result_continue : Core.expr =
  Core.const (C_int { v = 1l; repr = None })

let foreach_result_params : Tvar_env.t =
  Tvar_env.of_list_mapi [ "B"; "R"; "E"; "P" ] (fun index ->
      fun name ->
       Tvar_env.tparam_info ~name
         ~typ:(Tparam { index; name_ = name })
         ~constraints:[] ~loc:Rloc.no_location)

let [
      foreach_param_break;
      foreach_param_ret;
      foreach_param_err;
      foreach_param_payload;
    ] =
  Tvar_env.get_types foreach_result_params
[@@warning "-partial-match"]

let foreach_result_tpath = Basic_type_path.Builtin.type_path_foreach_result

let type_foreach_result break return error payload =
  (T_constr
     {
       type_constructor = foreach_result_tpath;
       tys = [ break; return; error; payload ];
       generic_ = false;
       is_suberror_ = false;
     }
    : Stype.t)

let foreach_result_type_poly : Stype.t =
  T_constr
    {
      type_constructor = foreach_result_tpath;
      tys = Tvar_env.get_types foreach_result_params;
      generic_ = true;
      is_suberror_ = false;
    }

let total = Basic_constr_info.Index_set.singleton 0 4

let continue : Typedecl_info.constructor =
  {
    constr_name = "Continue";
    cs_res = foreach_result_type_poly;
    cs_args = [];
    cs_tag =
      Constr_tag_regular
        { total; index = 0; name_ = "Continue"; repr_ = Constant };
    cs_vis = Read_write;
    cs_ty_params_ = foreach_result_params;
    cs_arity_ = Fn_arity.simple 0;
    cs_constr_loc_ = Loc.no_location;
    cs_loc_ = Loc.no_location;
  }

let break : Typedecl_info.constructor =
  {
    constr_name = "Break";
    cs_res = foreach_result_type_poly;
    cs_args = [ foreach_param_break ];
    cs_tag =
      Constr_tag_regular
        { total; index = 1; name_ = "Break"; repr_ = Non_constant };
    cs_vis = Read_write;
    cs_ty_params_ = foreach_result_params;
    cs_arity_ = Fn_arity.simple 1;
    cs_constr_loc_ = Loc.no_location;
    cs_loc_ = Loc.no_location;
  }

let return : Typedecl_info.constructor =
  {
    constr_name = "Return";
    cs_res = foreach_result_type_poly;
    cs_args = [ foreach_param_ret ];
    cs_tag =
      Constr_tag_regular
        { total; index = 2; name_ = "Return"; repr_ = Non_constant };
    cs_vis = Read_write;
    cs_ty_params_ = foreach_result_params;
    cs_arity_ = Fn_arity.simple 1;
    cs_constr_loc_ = Loc.no_location;
    cs_loc_ = Loc.no_location;
  }

let error : Typedecl_info.constructor =
  {
    constr_name = "Error";
    cs_res = foreach_result_type_poly;
    cs_args = [ foreach_param_err ];
    cs_tag =
      Constr_tag_regular
        { total; index = 3; name_ = "Error"; repr_ = Non_constant };
    cs_vis = Read_write;
    cs_ty_params_ = foreach_result_params;
    cs_arity_ = Fn_arity.simple 1;
    cs_constr_loc_ = Loc.no_location;
    cs_loc_ = Loc.no_location;
  }

let jump_outer : Typedecl_info.constructor =
  {
    constr_name = "JumpOuter";
    cs_res = foreach_result_type_poly;
    cs_args = [ foreach_param_payload ];
    cs_tag =
      Constr_tag_regular
        { total; index = 4; name_ = "JumpOuter"; repr_ = Non_constant };
    cs_vis = Read_write;
    cs_ty_params_ = foreach_result_params;
    cs_arity_ = Fn_arity.simple 1;
    cs_constr_loc_ = Loc.no_location;
    cs_loc_ = Loc.no_location;
  }

let foreach_result : Typedecl_info.t =
  {
    ty_constr = foreach_result_tpath;
    ty_arity = 3;
    ty_desc = Variant_type [ continue; break; return; error; jump_outer ];
    ty_vis = Vis_fully_pub;
    ty_params_ = foreach_result_params;
    ty_loc_ = Loc.no_location;
    ty_doc_ = Docstring.empty;
    ty_attrs = [];
    ty_is_only_tag_enum_ = false;
    ty_is_suberror_ = false;
  }

let get_first_enum_field (obj : Ident.t) tag ~ty ~constr_ty =
  Core.prim ~ty
    (Penum_field { index = 0; tag })
    [ Core.var obj ~ty:(Type.make_constr_type constr_ty ~tag) ]

type labelled_jump_info = {
  label : Label.t;
  break_type : Stype.t option;
  continue_types : Stype.t list;
  need_extra_no_payload_continue : bool;
}

type jump_outer_constr_info = {
  break_tag : Basic_constr_info.constr_tag;
  break_type : Stype.t option;
  continue_tag : Basic_constr_info.constr_tag;
  continue_types : Stype.t list;
  extra_continue_tag : Basic_constr_info.constr_tag option;
}

type jump_outer_ctx = {
  payload_type : Stype.t;
  constrs : jump_outer_constr_info Label.Map.t;
}

let make_jump_outer_ctx ~(global_env : Global_env.t) ~tvar_env
    (info : labelled_jump_info list) =
  (if info = [] then { payload_type = Stype.unit; constrs = Label.Map.empty }
   else
     let constrs = Vec.empty () in
     let total =
       Lst.fold_right info 0 (fun i ->
           fun acc ->
            if i.need_extra_no_payload_continue then acc + 3 else acc + 2)
     in
     let total = Basic_constr_info.Index_set.singleton 0 (total - 1) in
     let toplevel_id : Basic_type_path.toplevel_id =
       T_regular
         { pkg = !Basic_config.current_package; name = "jump_outer_helper" }
     in
     let type_name = "JumpOuterPayload" ^ Int.to_string (Basic_uuid.next ()) in
     let type_constructor = Basic_type_path.local_type toplevel_id type_name in
     let result_ty : Stype.t =
       T_constr
         {
           type_constructor;
           tys = Tvar_env.get_types tvar_env;
           generic_ = false;
           is_suberror_ = false;
         }
     in
     let decl_constrs = Vec.empty () in
     let tag_index = ref 0 in
     Lst.iter info
       ~f:(fun
           { label; break_type; continue_types; need_extra_no_payload_continue }
         ->
         let label_name = Label.basename label in
         let break_tag : Basic_constr_info.constr_tag =
           Constr_tag_regular
             {
               total;
               index = !tag_index;
               repr_ =
                 (match break_type with None -> Constant | _ -> Non_constant);
               name_ = label_name ^ "break";
             }
         in
         incr tag_index;
         let continue_tag : Basic_constr_info.constr_tag =
           Constr_tag_regular
             {
               total;
               index = !tag_index;
               repr_ =
                 (match continue_types with
                 | [] -> Constant
                 | _ -> Non_constant);
               name_ = label_name ^ "continue";
             }
         in
         incr tag_index;
         let break_constr_decl : Typedecl_info.constructor =
           {
             constr_name = label_name ^ "break";
             cs_res = result_ty;
             cs_args = (match break_type with None -> [] | Some t -> [ t ]);
             cs_tag = break_tag;
             cs_vis = Read_write;
             cs_ty_params_ = tvar_env;
             cs_arity_ = Fn_arity.simple (if break_type = None then 0 else 1);
             cs_constr_loc_ = Loc.no_location;
             cs_loc_ = Loc.no_location;
           }
         in
         let continue_constr_decl : Typedecl_info.constructor =
           {
             constr_name = label_name ^ "continue";
             cs_res = result_ty;
             cs_args = continue_types;
             cs_tag = continue_tag;
             cs_vis = Read_write;
             cs_ty_params_ = tvar_env;
             cs_arity_ = Fn_arity.simple (List.length continue_types);
             cs_constr_loc_ = Loc.no_location;
             cs_loc_ = Loc.no_location;
           }
         in
         Vec.push decl_constrs break_constr_decl;
         Vec.push decl_constrs continue_constr_decl;
         let extra_continue_tag =
           if need_extra_no_payload_continue then (
             let extra_continue_tag : Basic_constr_info.constr_tag =
               Constr_tag_regular
                 {
                   total;
                   index = !tag_index;
                   repr_ = Constant;
                   name_ = label_name ^ "extra_continue";
                 }
             in
             incr tag_index;
             let extra_continue_constr_decl : Typedecl_info.constructor =
               {
                 constr_name = label_name ^ "extra_continue";
                 cs_res = result_ty;
                 cs_args = [];
                 cs_tag = extra_continue_tag;
                 cs_vis = Read_write;
                 cs_ty_params_ = tvar_env;
                 cs_arity_ = Fn_arity.simple 0;
                 cs_constr_loc_ = Loc.no_location;
                 cs_loc_ = Loc.no_location;
               }
             in
             Vec.push decl_constrs extra_continue_constr_decl;
             Some extra_continue_tag)
           else None
         in
         Vec.push constrs
           ( label,
             {
               break_tag;
               break_type;
               continue_tag;
               continue_types;
               extra_continue_tag;
             } ));
     let decl : Local_type.t =
       {
         name = type_name;
         toplevel_id;
         kind = Enum (Vec.to_list decl_constrs);
         loc_ = Rloc.no_location;
         is_only_tag_enum = false;
         ty_params_ = tvar_env;
       }
     in
     Global_env.add_local_type_after_typing global_env decl;
     {
       payload_type = result_ty;
       constrs = Label.Map.of_array (Vec.to_array constrs);
     }
    : jump_outer_ctx)
