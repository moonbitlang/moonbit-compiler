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
module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Syntax = Parsing_syntax
module Ident = Basic_ident

let rec eval_const_expr (expr : Typedtree.expr) ~diagnostics =
  (let error reason loc =
     Local_diagnostics.add_error diagnostics
       (Errors.constant_not_constant ~reason ~loc);
     None
       [@@local]
   in
   match expr with
   | Texpr_constant { c; ty = _; name_ = _ } -> Some c
   | Texpr_apply
       {
         func =
           Texpr_ident
             { kind = Prim (Parith { operand_type; operator = Neg }); _ };
         args = { arg_value = target; arg_kind = Positional } :: [];
         ty = _;
         loc_;
       } -> (
       match eval_const_expr target ~diagnostics with
       | Some c -> (
           match operand_type with
           | I32 | I64 | F64 -> Constant.eval_negation c
           | I16 | F32 -> error `Unsupported loc_
           | U8 | U16 | U32 | U64 -> None)
       | _ -> None)
   | Texpr_apply
       {
         func =
           Texpr_ident { kind = Prim (Parith { operand_type; operator }); _ };
         kind_ = Infix;
         args =
           [
             { arg_value = lhs; arg_kind = Positional };
             { arg_value = rhs; arg_kind = Positional };
           ];
         ty = _;
         loc_;
       } -> (
       match
         (eval_const_expr lhs ~diagnostics, eval_const_expr rhs ~diagnostics)
       with
       | Some c1, Some c2 -> (
           match operator with
           | Add | Sub | Mul | Div | Mod -> (
               match operand_type with
               | I32 | U32 | I64 | U64 | F64 -> (
                   match Constant.eval_arith operand_type operator c1 c2 with
                   | Some _ as result -> result
                   | None -> error `Arith_error loc_)
               | F32 | U8 | I16 | U16 -> error `Unsupported loc_)
           | Sqrt | Neg | Abs -> None)
       | _ -> None)
   | Texpr_apply
       {
         func =
           Texpr_ident { kind = Prim (Pbitwise { operand_type; operator }); _ };
         kind_ = Infix;
         args =
           [
             { arg_value = lhs; arg_kind = Positional };
             { arg_value = rhs; arg_kind = Positional };
           ];
         ty = _;
         loc_;
       } -> (
       match
         (eval_const_expr lhs ~diagnostics, eval_const_expr rhs ~diagnostics)
       with
       | Some c1, Some c2 -> (
           match operand_type with
           | I32 | U32 | I64 | U64 ->
               Constant.eval_bitwise operand_type operator c1 c2
           | I16 | U16 | U8 -> error `Unsupported loc_
           | F32 | F64 -> None)
       | _ -> None)
   | Texpr_apply
       {
         func =
           Texpr_ident
             { kind = Prim (Pcomparison { operand_type; operator }); _ };
         kind_ = Infix;
         args =
           [
             { arg_value = lhs; arg_kind = Positional };
             { arg_value = rhs; arg_kind = Positional };
           ];
         ty = _;
         loc_;
       } -> (
       match
         (eval_const_expr lhs ~diagnostics, eval_const_expr rhs ~diagnostics)
       with
       | Some c1, Some c2 -> (
           match operand_type with
           | I32 | U32 | I64 | U64 | F64 ->
               Constant.eval_comparison operand_type operator c1 c2
           | I16 | U16 | U8 | F32 -> error `Unsupported loc_)
       | _ -> None)
   | Texpr_constraint { expr; _ } -> eval_const_expr expr ~diagnostics
   | Texpr_hole { kind = Synthesized; _ } -> None
   | _ -> error `Not_const (Typedtree.loc_of_typed_expr expr)
    : Constant.t option)

let typing_const_decl
    ({ binder; expr; is_pub; loc_; doc_; attrs; konstraint; id; typ } :
      Local_typing_worklist.const_decl) ~global_env ~diagnostics =
  (let binder_loc = Rloc.to_loc ~base:loc_ binder.loc_ in
   let local_diagnostics = Local_diagnostics.make ~base:loc_ in
   let binder, expr =
     Typer.typing_impl_let binder expr ~id:(Ident.of_qual_ident id) ~typ
       ~global_env ~diagnostics:local_diagnostics
   in
   let kind : Value_info.value_kind =
     match Stype.type_repr typ with
     | _ when Local_diagnostics.has_fatal_errors local_diagnostics -> Normal
     | T_builtin _ -> (
         match eval_const_expr expr ~diagnostics:local_diagnostics with
         | Some value -> Const value
         | None -> Normal)
     | T_blackhole | Tvar { contents = Tnolink Tvar_error } -> Normal
     | typ ->
         let loc =
           match konstraint with
           | Some konstraint -> Typedtree.loc_of_typ konstraint
           | None -> Typedtree.loc_of_typed_expr expr
         in
         Local_diagnostics.add_error local_diagnostics
           (Errors.invalid_constant_type ~ty:(Printer.type_to_string typ) ~loc);
         Normal
   in
   let toplevel_env = Global_env.get_toplevel_values global_env in
   let value_info : Value_info.toplevel =
     {
       id;
       typ;
       pub = is_pub;
       kind;
       loc_ = binder_loc;
       doc_;
       attrs;
       ty_params_ = Tvar_env.empty;
       arity_ = None;
       param_names_ = [];
       direct_use_loc_ = Not_direct_use;
     }
   in
   Typing_info.add_value toplevel_env value_info;
   Local_diagnostics.add_to_global local_diagnostics diagnostics;
   Timpl_letdef
     {
       binder;
       expr;
       is_pub;
       loc_;
       konstraint;
       doc_;
       attrs;
       is_generated_ = false;
     }
    : Typedtree.impl)

let typing_const_decls (const_decls : Local_typing_worklist.const_decl Vec.t)
    ~global_env ~diagnostics =
  let decls = Hash_string.create (Vec.length const_decls) in
  let toplevel_env = Global_env.get_toplevel_values global_env in
  Vec.iter const_decls (fun decl ->
      let name = decl.binder.binder_name in
      match Hash_string.find_opt decls name with
      | Some (`Unresolved (prev_def : Local_typing_worklist.const_decl)) ->
          Diagnostics.add_error diagnostics
            (Errors.value_redeclare ~name
               ~first_loc:(Rloc.to_loc ~base:prev_def.loc_ prev_def.binder.loc_)
               ~second_loc:(Rloc.to_loc ~base:decl.loc_ decl.binder.loc_)
               ~extra_message:None)
      | Some (`Resolved _) -> assert false
      | None ->
          (match Typing_info.find_constructor toplevel_env name with
          | constr :: _ ->
              Diagnostics.add_error diagnostics
                (Errors.constant_constr_duplicate ~name
                   ~constr_loc:constr.cs_loc_
                   ~const_loc:(Rloc.to_loc ~base:decl.loc_ decl.binder.loc_))
          | [] -> ());
          Hash_string.add decls decl.binder.binder_name (`Unresolved decl));
  let visitor =
    object (self)
      inherit [_] Syntax.iter

      method! visit_constructor visiting constr =
        match constr.extra_info with
        | No_extra_info -> (
            let name = constr.constr_name.name in
            if Lst.mem_string visiting name then
              match[@warning "-fragile-match"]
                Hash_string.find_exn decls name
              with
              | `Unresolved decl ->
                  Diagnostics.add_error diagnostics
                    (Errors.cycle_in_const_decl
                       ~cycle:(List.rev (name :: visiting))
                       ~loc:(Rloc.to_loc ~base:decl.loc_ decl.binder.loc_));
                  let dummy_value_info : Value_info.toplevel =
                    {
                      id = decl.id;
                      typ = decl.typ;
                      pub = decl.is_pub;
                      kind = Normal;
                      loc_ = Loc.no_location;
                      doc_ = decl.doc_;
                      attrs = decl.attrs;
                      ty_params_ = Tvar_env.empty;
                      arity_ = None;
                      param_names_ = [];
                      direct_use_loc_ = Not_direct_use;
                    }
                  in
                  Typing_info.add_value toplevel_env dummy_value_info
              | _ -> assert false
            else
              match Hash_string.find_opt decls name with
              | Some (`Unresolved decl) ->
                  ignore
                    (self#visit_const_decl visiting decl ~is_redeclare:false)
              | Some (`Resolved _) | None -> ())
        | Package _ | Type_name _ -> ()

      method visit_const_decl visiting decl ~is_redeclare =
        self#visit_expr (decl.binder.binder_name :: visiting) decl.expr;
        let result = typing_const_decl decl ~global_env ~diagnostics in
        if not is_redeclare then
          Hash_string.replace decls decl.binder.binder_name (`Resolved result);
        result
    end
  in
  Vec.map_into_list const_decls ~unorder:(fun decl ->
      match Hash_string.find_exn decls decl.binder.binder_name with
      | `Unresolved decl' ->
          let is_redeclare = Basic_prelude.phys_not_equal decl' decl in
          visitor#visit_const_decl [] decl ~is_redeclare
      | `Resolved result -> result)
