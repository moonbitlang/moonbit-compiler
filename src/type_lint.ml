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

let lint_typ loc outmost_typ ~diagnostics =
  let type_unit = Stype.unit in
  let rec go (ty : Stype.t) =
    match Stype.type_repr ty with
    | Tvar link ->
        (match !link with
        | Tnolink Tvar_error -> ()
        | _ ->
            let warn =
              Warnings.Unresolved_tvar (Printer.type_to_string outmost_typ)
            in
            Local_diagnostics.add_warning diagnostics { kind = warn; loc });
        link := Tlink type_unit
    | Tarrow { params_ty; ret_ty; err_ty; is_async = _ } -> (
        Lst.iter params_ty ~f:go;
        go ret_ty;
        match err_ty with None -> () | Some ty -> go ty)
    | T_constr { tys; type_constructor = _ } -> Lst.iter tys ~f:go
    | Tparam _ | T_trait _ | T_builtin _ | T_blackhole -> ()
  in
  go outmost_typ

let lint_obj =
  object (self)
    inherit [_] Typedtree.iter as super

    method! visit_Param diagnostics binder _ ty _ =
      lint_typ binder.loc_ ty ~diagnostics

    method! visit_Texpr_ident diagnostics _ ty_args_ arity _ ty loc_ =
      lint_typ ~diagnostics loc_ ty;
      Array.iter (lint_typ loc_ ~diagnostics) ty_args_;
      match arity with
      | Some arity when not (Fn_arity.is_simple arity) ->
          Typeutil.add_local_typing_error diagnostics
            (Errors.no_first_class_labelled_function loc_)
      | _ -> ()

    method! visit_Texpr_method diagnostics _ ty_args_ arity _ _ ty loc_ =
      lint_typ ~diagnostics loc_ ty;
      Array.iter (lint_typ ~diagnostics loc_) ty_args_;
      match arity with
      | Some arity when not (Fn_arity.is_simple arity) ->
          Typeutil.add_local_typing_error diagnostics
            (Errors.no_first_class_labelled_function loc_)
      | _ -> ()

    method! visit_Texpr_unresolved_method diagnostics _trait_name _method_name
        self_type arity _ty loc_ =
      lint_typ ~diagnostics loc_ self_type;
      match arity with
      | Some arity when not (Fn_arity.is_simple arity) ->
          Typeutil.add_local_typing_error diagnostics
            (Errors.no_first_class_labelled_function loc_)
      | _ -> ()

    method! visit_Texpr_apply diagnostics func args _ty _kind _loc =
      (match func with
      | Texpr_ident { ty_args_; ty; loc_; _ }
      | Texpr_method { ty_args_; ty; loc_; _ } ->
          lint_typ ~diagnostics loc_ ty;
          Array.iter (lint_typ ~diagnostics loc_) ty_args_
      | Texpr_unresolved_method { self_type; loc_; _ } ->
          lint_typ ~diagnostics loc_ self_type
      | _ -> self#visit_expr diagnostics func);
      Lst.iter args ~f:(self#visit_argument diagnostics)

    method! visit_Pipe_partial_apply diagnostics func args _loc =
      (match func with
      | Texpr_ident { ty_args_; ty; loc_; _ }
      | Texpr_method { ty_args_; ty; loc_; _ } ->
          lint_typ ~diagnostics loc_ ty;
          Array.iter (lint_typ ~diagnostics loc_) ty_args_
      | Texpr_unresolved_method { self_type; loc_; _ } ->
          lint_typ ~diagnostics loc_ self_type
      | _ -> self#visit_expr diagnostics func);
      Lst.iter args ~f:(self#visit_argument diagnostics)

    method! visit_Texpr_try diagnostics body catch _catch_all try_else ty err_ty
        _catch_loc _else_loc loc =
      self#visit_expr diagnostics body;
      Lst.iter catch ~f:(fun case -> self#visit_match_case diagnostics case);
      (match try_else with
      | None -> ()
      | Some try_else ->
          Lst.iter try_else ~f:(fun case ->
              self#visit_match_case diagnostics case));
      lint_typ ~diagnostics loc ty;
      lint_typ ~diagnostics loc err_ty

    method! visit_expr diagnostics expr =
      super#visit_expr diagnostics expr;
      lint_typ ~diagnostics
        (Typedtree.loc_of_typed_expr expr)
        (Typedtree_util.type_of_typed_expr expr)
  end

let type_lint (expr : Typedtree.expr) ~diagnostics =
  lint_obj#visit_expr diagnostics expr

let type_lint_fn (fn : Typedtree.fn) ~diagnostics =
  type_lint fn.body ~diagnostics;
  Lst.iter fn.params ~f:(fun p ->
      match p with
      | Param { kind = Optional default; _ } -> type_lint default ~diagnostics
      | _ -> ())
