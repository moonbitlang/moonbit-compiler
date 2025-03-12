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


module Type_path = Basic_type_path
module Lst = Basic_lst

type control_info =
  | Not_in_loop
  | Ambiguous_position
  | In_while
  | In_while_with_else of { break : Stype.t }
  | In_loop of {
      break : Stype.t;
      continue : Stype.t list;
      has_continue : bool ref;
    }
  | In_for of { break : Stype.t option; continue : Stype.t list }
  | In_foreach of { break : Stype.t option }

type open_error_ctx =
  | Empty_ctx
  | Tparam of { index : int; name_ : string }
  | Suberrors of Basic_type_path.t list

type fixed_error_ctx =
  | Supererror
  | Tparam of { index : int; name_ : string }
  | Suberror of Basic_type_path.t

type error_ctx = Fixed_ctx of fixed_error_ctx | Open_ctx of open_error_ctx

let error_ctx_to_stype ctx =
  (match ctx with
   | Open_ctx Empty_ctx -> Stype.new_type_var Stype.Tvar_error
   | Open_ctx (Suberrors es) -> (
       match es with
       | [] -> assert false
       | p :: [] ->
           T_constr
             {
               type_constructor = p;
               tys = [];
               generic_ = false;
               is_suberror_ = true;
             }
       | _ -> Stype.error)
   | Open_ctx (Tparam { index; name_ }) | Fixed_ctx (Tparam { index; name_ }) ->
       Stype.Tparam { index; name_ }
   | Fixed_ctx Supererror -> Stype.error
   | Fixed_ctx (Suberror p) ->
       T_constr
         {
           type_constructor = p;
           tys = [];
           generic_ = false;
           is_suberror_ = false;
         }
    : Stype.t)

type labeled_loop_info = {
  control_info : control_info;
  label : Label.t;
  used : bool ref;
}

type async_ctx = In_sync_fn | In_async_fn | In_foreach

type t = {
  return : Stype.t option;
  error_ctx : error_ctx ref option;
  async_ctx : async_ctx;
  control_info : control_info;
  may_has_error : bool ref;
  labeled_controls : (string * labeled_loop_info) list;
}

let empty =
  {
    return = None;
    error_ctx = None;
    async_ctx = In_sync_fn;
    control_info = Not_in_loop;
    may_has_error = ref false;
    labeled_controls = [];
  }

let make_fn ~return ~error_ctx ~is_async =
  {
    return = Some return;
    error_ctx;
    async_ctx = (if is_async then In_async_fn else In_sync_fn);
    control_info = Not_in_loop;
    may_has_error = ref false;
    labeled_controls = [];
  }

let warn_if_label_shadowed (label : Typedtree.loop_label_binder)
    (parent : (string * labeled_loop_info) list) diagnostics =
  if Lst.exists parent (fun (name, _) -> name = Label.basename label.label) then
    Local_diagnostics.add_warning diagnostics
      {
        kind = Warnings.Loop_label_shadow (Label.basename label.label);
        loc = label.loc_;
      }

let warn_if_label_unused (label : Typedtree.loop_label_binder option) (ctx : t)
    ~diagnostics =
  match label with
  | None -> ()
  | Some label ->
      let _, { used; _ } = List.hd ctx.labeled_controls in
      if not !used then
        Local_diagnostics.add_warning diagnostics
          {
            kind = Warnings.Loop_label_unused (Label.basename label.label);
            loc = label.loc_;
          }

let with_while ~break_typ ~(label : Typedtree.loop_label_binder option) parent
    ~diagnostics =
  let control_info =
    match break_typ with
    | None -> In_while
    | Some break_typ -> In_while_with_else { break = break_typ }
  in
  let labeled_controls =
    match label with
    | None -> parent.labeled_controls
    | Some label ->
        warn_if_label_shadowed label parent.labeled_controls diagnostics;
        ( Label.basename label.label,
          { label = label.label; control_info; used = ref false } )
        :: parent.labeled_controls
  in
  {
    return = parent.return;
    error_ctx = parent.error_ctx;
    async_ctx = parent.async_ctx;
    control_info;
    may_has_error = parent.may_has_error;
    labeled_controls;
  }

let with_loop ~arg_typs ~result_typ
    ~(label : Typedtree.loop_label_binder option) parent ~diagnostics =
  let control_info =
    In_loop
      { break = result_typ; continue = arg_typs; has_continue = ref false }
  in
  let labeled_controls =
    match label with
    | None -> parent.labeled_controls
    | Some label ->
        warn_if_label_shadowed label parent.labeled_controls diagnostics;
        ( Label.basename label.label,
          { label = label.label; control_info; used = ref false } )
        :: parent.labeled_controls
  in
  {
    return = parent.return;
    error_ctx = parent.error_ctx;
    async_ctx = parent.async_ctx;
    control_info;
    may_has_error = parent.may_has_error;
    labeled_controls;
  }

let loop_has_continue (ctx : t) =
  (match ctx.control_info with
   | In_loop { has_continue; _ } -> !has_continue
   | _ -> assert false
    : bool)

let with_for ~break_typ ~arg_typs ~(label : Typedtree.loop_label_binder option)
    parent ~diagnostics =
  let control_info = In_for { continue = arg_typs; break = break_typ } in
  let labeled_controls =
    match label with
    | None -> parent.labeled_controls
    | Some label ->
        warn_if_label_shadowed label parent.labeled_controls diagnostics;
        ( Label.basename label.label,
          { label = label.label; control_info; used = ref false } )
        :: parent.labeled_controls
  in
  {
    return = parent.return;
    error_ctx = parent.error_ctx;
    async_ctx = parent.async_ctx;
    control_info;
    may_has_error = parent.may_has_error;
    labeled_controls;
  }

let with_foreach ~break_typ ~(label : Typedtree.loop_label_binder option) parent
    ~diagnostics =
  let control_info : control_info = In_foreach { break = break_typ } in
  let labeled_controls =
    match label with
    | None -> parent.labeled_controls
    | Some label ->
        warn_if_label_shadowed label parent.labeled_controls diagnostics;
        ( Label.basename label.label,
          { label = label.label; control_info; used = ref false } )
        :: parent.labeled_controls
  in
  {
    return = parent.return;
    error_ctx = parent.error_ctx;
    async_ctx = In_foreach;
    may_has_error = parent.may_has_error;
    control_info = In_foreach { break = break_typ };
    labeled_controls;
  }

let with_ambiguous_position parent =
  {
    return = parent.return;
    error_ctx = parent.error_ctx;
    async_ctx = parent.async_ctx;
    control_info = Ambiguous_position;
    may_has_error = parent.may_has_error;
    labeled_controls = parent.labeled_controls;
  }

let with_error_ctx ~error_ctx parent =
  {
    parent with
    error_ctx = Some error_ctx;
    may_has_error = ref false;
    labeled_controls = parent.labeled_controls;
  }

let check_error_in_ctx ~error_ty ~(ctx : error_ctx ref) loc =
  (let is_tvar (typ : Stype.t) =
     (let typ = Stype.type_repr typ in
      match typ with Tvar _ -> true | _ -> false
       : bool)
       [@@local]
   in
   let error_ty = Stype.type_repr error_ty in
   let make_error expect_ty =
     let expected_ty, error_ty =
       Printer.type_pair_to_string expect_ty error_ty
     in
     Some (Errors.error_type_mismatch ~expected_ty ~actual_ty:error_ty ~loc)
       [@@local]
   in
   if is_tvar error_ty then Ctype.unify_exn error_ty Stype.error;
   match !ctx with
   | Fixed_ctx expect_ty -> (
       match expect_ty with
       | Supererror -> None
       | Tparam { index; name_ } -> (
           match error_ty with
           | Tparam { index = index' } when index = index' -> None
           | _ ->
               let expect_ty : Stype.t = Tparam { index; name_ } in
               make_error expect_ty)
       | Suberror p -> (
           match error_ty with
           | T_constr { type_constructor = p'; _ } when Type_path.equal p p' ->
               None
           | _ ->
               let expect_ty : Stype.t =
                 T_constr
                   {
                     type_constructor = p;
                     tys = [];
                     generic_ = false;
                     is_suberror_ = true;
                   }
               in
               make_error expect_ty))
   | Open_ctx open_ctx -> (
       match open_ctx with
       | Empty_ctx -> (
           match error_ty with
           | T_constr { type_constructor = p; is_suberror_ = true; _ } ->
               ctx := Open_ctx (Suberrors [ p ]);
               None
           | Tparam { index; name_ } ->
               ctx := Open_ctx (Tparam { index; name_ });
               None
           | T_constr { type_constructor = Type_path.T_error; _ } ->
               ctx := Fixed_ctx Supererror;
               None
           | _ -> None)
       | Tparam { index } -> (
           match error_ty with
           | Tparam { index = index' } ->
               if index <> index' then ctx := Fixed_ctx Supererror;
               None
           | T_constr { type_constructor = _; is_suberror_ = true; _ }
           | T_constr { type_constructor = Type_path.T_error; _ } ->
               ctx := Fixed_ctx Supererror;
               None
           | _ -> None)
       | Suberrors ps -> (
           match error_ty with
           | T_constr { type_constructor = p; is_suberror_ = true; _ } ->
               if not (Basic_lst.exists ps (Type_path.equal p)) then
                 ctx := Open_ctx (Suberrors (p :: ps));
               None
           | Tparam _ | T_constr { type_constructor = Type_path.T_error; _ } ->
               ctx := Fixed_ctx Supererror;
               None
           | _ -> None))
    : Local_diagnostics.error option)

let async_is_allowed (ctx : t) ~loc =
  (match ctx.async_ctx with
   | In_async_fn -> None
   | In_sync_fn ->
       Some
         (Errors.async_not_allowed_in_context ~context:"non-async function" ~loc)
   | In_foreach ->
       Some
         (Errors.async_not_allowed_in_context ~context:"`for .. in` loop" ~loc)
    : Local_diagnostics.error option)
