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
module Ident_set = Ident.Set
module Ident_hashmap = Ident.Hash
module VI = Basic_vec_int
module Vec = Basic_vec
module Lst = Basic_lst
module Type_path = Basic_type_path

let specialize (qual_name : Basic_qual_ident.t) (ty_args : Core.typ array) =
  (match qual_name with
   | Qregular { pkg; name } when pkg = Basic_config.builtin_package -> (
       match name with
       | "op_lt" -> (
           match Type.classify_as_builtin ty_args.(0) with
           | `Int -> Some Primitive.lt_int
           | `Int64 -> Some Primitive.lt_int64
           | `UInt -> Some Primitive.lt_uint
           | `UInt64 -> Some Primitive.lt_uint64
           | `Double -> Some Primitive.lt_double
           | `Float -> Some Primitive.lt_float
           | `Char -> Some Primitive.lt_int
           | `Byte | `Int16 | `UInt16 -> Some Primitive.lt_int
           | `Other -> None)
       | "op_le" -> (
           match Type.classify_as_builtin ty_args.(0) with
           | `Int -> Some Primitive.le_int
           | `Int64 -> Some Primitive.le_int64
           | `UInt -> Some Primitive.le_uint
           | `UInt64 -> Some Primitive.le_uint64
           | `Float -> Some Primitive.le_float
           | `Double -> Some Primitive.le_double
           | `Char -> Some Primitive.le_int
           | `Byte | `Int16 | `UInt16 -> Some Primitive.le_int
           | `Other -> None)
       | "op_ge" -> (
           match Type.classify_as_builtin ty_args.(0) with
           | `Int -> Some Primitive.ge_int
           | `Int64 -> Some Primitive.ge_int64
           | `UInt -> Some Primitive.ge_uint
           | `UInt64 -> Some Primitive.ge_uint64
           | `Float -> Some Primitive.ge_float
           | `Double -> Some Primitive.ge_double
           | `Char -> Some Primitive.ge_int
           | `Byte | `Int16 | `UInt16 -> Some Primitive.ge_int
           | `Other -> None)
       | "op_gt" -> (
           match Type.classify_as_builtin ty_args.(0) with
           | `Int -> Some Primitive.gt_int
           | `Int64 -> Some Primitive.gt_int64
           | `UInt -> Some Primitive.gt_uint
           | `UInt64 -> Some Primitive.gt_uint64
           | `Float -> Some Primitive.gt_float
           | `Double -> Some Primitive.gt_double
           | `Char -> Some Primitive.gt_int
           | `Byte | `Int16 | `UInt16 -> Some Primitive.gt_int
           | `Other -> None)
       | "op_notequal" -> (
           match Type.classify_as_builtin ty_args.(0) with
           | `Int -> Some Primitive.ne_int
           | `Int64 -> Some Primitive.ne_int64
           | `UInt -> Some Primitive.ne_uint
           | `UInt64 -> Some Primitive.ne_uint64
           | `Float -> Some Primitive.ne_float
           | `Double -> Some Primitive.ne_double
           | `Char -> Some Primitive.ne_int
           | `Byte | `Int16 | `UInt16 -> Some Primitive.ne_int
           | `Other -> None)
       | _ -> None)
   | _ -> None
    : Primitive.prim option)

let specializable (qual_name : Basic_qual_ident.t) (ty_args : Core.typ array) =
  (match specialize qual_name ty_args with Some _ -> true | _ -> false : bool)

type fn_group =
  | Non_rec of (Ident.t * Core.fn)
  | Rec of (Ident.t * Core.fn) list

let free_vars ~(exclude : Ident.Set.t) (fn : Core.fn) =
  (let go_ident ~env ~acc (id : Ident.t) ty =
     match id with
     | Pident _ | Pmutable_ident _ ->
         if Ident_set.mem env id then acc else Ident.Map.add acc id ty
     | Pdot _ | Plocal_method _ -> acc
   in
   let rec go_func ~env ~acc (func : Core.fn) =
     let new_env =
       Lst.fold_left func.params env (fun env ->
           fun p -> Ident_set.add env p.binder)
     in
     go ~env:new_env ~acc func.body
   and go ~env ~acc (expr : Core.expr) =
     match expr with
     | Cexpr_const _ -> acc
     | Cexpr_unit _ -> acc
     | Cexpr_var { id; ty; prim = _ } -> go_ident ~env ~acc id ty
     | Cexpr_as { expr; trait = _; obj_type = _ } -> go ~env ~acc expr
     | Cexpr_prim { prim = _; args; ty = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_and { lhs; rhs; _ } | Cexpr_or { lhs; rhs; _ } ->
         let acc1 = go ~env ~acc lhs in
         go ~env ~acc:acc1 rhs
     | Cexpr_let { name; rhs; body; ty = _ } ->
         let new_env = Ident_set.add env name in
         let new_acc = go ~env ~acc rhs in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_letfn { name; fn; body; ty = _; kind = _ } ->
         let new_env = Ident_set.add env name in
         let new_acc = go_func ~env:new_env ~acc fn in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_function { func; ty = _ } -> go_func ~env ~acc func
     | Cexpr_apply { func; args; kind; ty = _; prim = _ } ->
         let new_acc =
           match kind with
           | Normal { func_ty } | Async { func_ty } ->
               go_ident ~env ~acc func func_ty
           | Join -> acc
         in
         Lst.fold_left args new_acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_letrec { bindings; body; ty = _ } ->
         let new_env =
           Lst.fold_left bindings env (fun env ->
               fun (binder, _) -> Ident_set.add env binder)
         in
         let new_acc =
           Lst.fold_left bindings acc (fun acc ->
               fun (_, fn) -> go_func ~env:new_env ~acc fn)
         in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_constr { tag = _; args; ty = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_tuple { exprs; ty = _ } ->
         Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
     | Cexpr_record { fields; ty = _ } ->
         Lst.fold_left fields acc (fun acc ->
             fun field -> go ~env ~acc field.expr)
     | Cexpr_record_update { record; fields; fields_num = _; ty = _ } ->
         let new_acc = go ~env ~acc record in
         Lst.fold_left fields new_acc (fun acc ->
             fun field -> go ~env ~acc field.expr)
     | Cexpr_field { record; accessor = _; pos = _; ty = _ } ->
         go ~env ~acc record
     | Cexpr_mutate { record; label = _; field; pos = _; ty = _ } ->
         let new_acc = go ~env ~acc record in
         go ~env ~acc:new_acc field
     | Cexpr_array { exprs; ty = _ } ->
         Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
     | Cexpr_assign { var; expr; ty = _ } ->
         let new_acc = go_ident ~env ~acc var (Core.type_of_expr expr) in
         go ~env ~acc:new_acc expr
     | Cexpr_sequence { exprs; last_expr; ty = _ } ->
         let new_acc =
           Lst.fold_left exprs acc (fun acc -> fun expr -> go ~env ~acc expr)
         in
         go ~env ~acc:new_acc last_expr
     | Cexpr_if { cond; ifso; ifnot; ty = _ } -> (
         let acc1 = go ~env ~acc cond in
         let acc2 = go ~env ~acc:acc1 ifso in
         match ifnot with Some ifnot -> go ~env ~acc:acc2 ifnot | None -> acc2)
     | Cexpr_switch_constr { obj; cases; default; ty = _ } -> (
         let acc1 = go ~env ~acc obj in
         let acc2 =
           Lst.fold_left cases acc1 (fun acc ->
               fun (_tag, binder, case) ->
                let new_env =
                  match binder with
                  | None -> env
                  | Some binder -> Ident_set.add env binder
                in
                go ~env:new_env ~acc case)
         in
         match default with
         | Some default -> go ~env ~acc:acc2 default
         | None -> acc2)
     | Cexpr_switch_constant { obj; cases; default; ty = _ } ->
         let acc1 = go ~env ~acc obj in
         let acc2 =
           Lst.fold_left cases acc1 (fun acc ->
               fun (_const, case) -> go ~env ~acc case)
         in
         go ~env ~acc:acc2 default
     | Cexpr_loop { params; body; args; ty = _; label = _ } ->
         let new_env =
           Lst.fold_left params env (fun env ->
               fun p -> Ident_set.add env p.binder)
         in
         let new_acc =
           Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
         in
         go ~env:new_env ~acc:new_acc body
     | Cexpr_break { arg; ty = _; label = _ } -> (
         match arg with Some arg -> go ~env ~acc arg | None -> acc)
     | Cexpr_continue { args; ty = _; label = _ } ->
         Lst.fold_left args acc (fun acc -> fun arg -> go ~env ~acc arg)
     | Cexpr_handle_error { obj; ty = _; handle_kind = _ } -> go ~env ~acc obj
     | Cexpr_return { expr; _ } -> go ~env ~acc expr
   in
   let init_env =
     Lst.fold_left fn.params exclude (fun env ->
         fun p -> Ident_set.add env p.binder)
   in
   go ~env:init_env ~acc:Ident.Map.empty fn.body
    : Stype.t Ident.Map.t)

let group_local_fn_bindings (bindings : (Ident.t * Core.fn) list) =
  (let n = List.length bindings in
   let binding_array = Array.of_list bindings in
   let binder_index_map =
     Ident_hashmap.of_list
       (List.mapi (fun i -> fun (binder, _) -> (binder, i)) bindings)
   in
   let adjacency_array =
     Array.init n (fun src ->
         let _, src_fn = binding_array.(src) in
         let dst_binders = free_vars ~exclude:Ident_set.empty src_fn in
         let dst_vec = VI.empty () in
         Ident.Map.iter dst_binders (fun dst_binder ->
             fun _ ->
              let dst = Ident_hashmap.find_opt binder_index_map dst_binder in
              match dst with Some dst -> VI.push dst_vec dst | None -> ());
         dst_vec)
   in
   let scc = Basic_scc.graph adjacency_array in
   Vec.map_into_list scc ~unorder:(fun group ->
       if VI.length group = 1 then
         let i = VI.get group 0 in
         let binder, fn = binding_array.(i) in
         if Ident.Map.mem (free_vars ~exclude:Ident_set.empty fn) binder then
           Rec [ (binder, fn) ]
         else Non_rec (binder, fn)
       else Rec (VI.map_into_list group ~unorder:(fun i -> binding_array.(i))))
    : fn_group list)

module Transform_return : sig
  val transform_return_in_fn_body : Core.expr -> Core.expr
end = struct
  type return_ctx = { return_join : Ident.t; mutable need_return_join : bool }
  type error_ctx = { raise_join : Ident.t; mutable need_raise_join : bool }

  type transform_return_ctx = {
    return_ctx : return_ctx;
    error_ctx : error_ctx option;
  }

  let wrap_err expr ~ok_ty ~err_ty ~loc =
    let result_ty = Stype.make_multi_value_result_ty ~ok_ty ~err_ty in
    let err_tag = Builtin.constr_err.cs_tag in
    Core.prim ~loc ~ty:result_ty
      (Primitive.Pmake_value_or_error { tag = err_tag })
      [ expr ]

  let wrap_ok expr ~ok_ty ~err_ty ~loc =
    let result_ty = Stype.make_multi_value_result_ty ~ok_ty ~err_ty in
    let ok_tag = Builtin.constr_ok.cs_tag in
    Core.prim ~loc ~ty:result_ty
      (Primitive.Pmake_value_or_error { tag = ok_tag })
      [ expr ]

  let transform_return =
    object (self)
      inherit [_] Core.Map.map as super

      method! visit_Cexpr_handle_error ctx obj handle_kind ty loc_ =
        let handle_kind : Core.handle_kind =
          match handle_kind with
          | Return_err _ -> (
              match[@warning "-fragile-match"] ctx.error_ctx with
              | Some error_ctx ->
                  error_ctx.need_raise_join <- true;
                  Joinapply error_ctx.raise_join
              | _ -> assert false)
          | _ -> handle_kind
        in
        super#visit_Cexpr_handle_error ctx obj handle_kind ty loc_

      method! visit_Cexpr_return ctx expr kind ty loc_ =
        let expr = self#visit_expr ctx expr in
        match kind with
        | Error_result { is_error = false; _ } | Single_value ->
            let return_ctx = ctx.return_ctx in
            return_ctx.need_return_join <- true;
            Core.join_apply ~loc:loc_ ~ty return_ctx.return_join [ expr ]
        | Error_result { is_error = true; _ } -> (
            match[@warning "-fragile-match"] ctx.error_ctx with
            | Some error_ctx ->
                error_ctx.need_raise_join <- true;
                Core.join_apply ~loc:loc_ ~ty error_ctx.raise_join [ expr ]
            | _ -> assert false)

      method! visit_Cexpr_letfn ctx name fn body ty kind loc_ =
        match kind with
        | Tail_join | Nontail_join ->
            super#visit_Cexpr_letfn ctx name fn body ty kind loc_
        | Rec | Nonrec ->
            Core.letfn ~kind ~loc:loc_ name fn (self#visit_expr ctx body)

      method! visit_Cexpr_letrec ctx bindings body _ty loc_ =
        Core.letrec ~loc:loc_ bindings (self#visit_expr ctx body)

      method! visit_Cexpr_function _ctx fn ty is_raw loc_ =
        if is_raw then
          Core.raw_function ~loc:loc_ ~ty fn.params fn.body
            ~is_async:fn.is_async
        else
          Core.function_ ~loc:loc_ ~ty fn.params fn.body ~is_async:fn.is_async
    end

  let ghost_loc_ = Rloc.no_location

  let transform_return_in_fn_body body =
    let return_join = Ident.fresh "return" in
    let return_value_id = Ident.fresh "*return_value" in
    let return_ctx = { return_join; need_return_join = false } in
    let error_ctx, return_value_ty, error_ty =
      match Stype.type_repr (Core.type_of_expr body) with
      | T_constr
          {
            type_constructor = T_error_value_result;
            tys = [ ok_ty; err_ty ];
            _;
          } ->
          let raise_join = Ident.fresh "*return_err" in
          (Some { raise_join; need_raise_join = false }, ok_ty, Some err_ty)
      | ty -> (None, ty, None)
    in
    let body = transform_return#visit_expr { return_ctx; error_ctx } body in
    match error_ty with
    | None ->
        if return_ctx.need_return_join then
          Core.letfn ~kind:Nontail_join return_join
            {
              params =
                [
                  {
                    binder = return_value_id;
                    ty = return_value_ty;
                    loc_ = ghost_loc_;
                  };
                ];
              body = Core.var ~ty_args_:[||] ~ty:return_value_ty return_value_id;
              is_async = false;
            }
            body
        else body
    | Some err_ty -> (
        match[@warning "-fragile-match"] error_ctx with
        | Some error_ctx -> (
            let add_return body =
              let var =
                Core.var ~ty_args_:[||] ~ty:return_value_ty return_value_id
              in
              let wrapped_var =
                wrap_ok var ~ok_ty:return_value_ty ~err_ty ~loc:ghost_loc_
              in
              let fn : Core.fn =
                {
                  params =
                    [
                      {
                        binder = return_value_id;
                        ty = return_value_ty;
                        loc_ = ghost_loc_;
                      };
                    ];
                  body = wrapped_var;
                  is_async = false;
                }
              in
              Core.letfn ~kind:Nontail_join return_join fn body
            in
            let add_err_return body =
              let err_var_id = Ident.fresh "*return_err_value" in
              let var = Core.var ~ty_args_:[||] ~ty:err_ty err_var_id in
              let wrapped_var =
                wrap_err var ~ok_ty:return_value_ty ~err_ty ~loc:ghost_loc_
              in
              let fn : Core.fn =
                {
                  params =
                    [ { binder = err_var_id; ty = err_ty; loc_ = ghost_loc_ } ];
                  body = wrapped_var;
                  is_async = false;
                }
              in
              Core.letfn ~kind:Nontail_join error_ctx.raise_join fn body
            in
            match (return_ctx.need_return_join, error_ctx.need_raise_join) with
            | false, false -> body
            | true, false -> add_return body
            | false, true -> add_err_return body
            | true, true -> add_return (add_err_return body))
        | _ -> assert false)
end

include Transform_return

type subst_ctx = { old_id : Ident.t; new_id : Ident.t }

let subst_obj =
  object
    inherit [_] Core.Map.map

    method! visit_var (ctx : subst_ctx) var =
      if Ident.equal var ctx.old_id then ctx.new_id else var
  end

let inline_array_iter ~(arr_id : Ident.t) ~(arr_ty : Stype.t)
    ~(elem_ty : Stype.t) ~(anon_func_param : Ident.t)
    ~(anon_func_body : Core.expr) =
  let len_id = Ident.fresh "*len" in
  let arr_expr = Core.var ~ty:arr_ty arr_id in
  let len_rhs =
    Core.prim ~ty:Stype.int Primitive.Pfixedarray_length [ arr_expr ]
  in
  let len_expr = Core.var ~ty:Stype.int len_id in
  let i_id = Ident.fresh "*i" in
  let i_expr = Core.var ~ty:Stype.int i_id in
  let i_param : Core.param =
    { binder = i_id; ty = Stype.int; loc_ = Rloc.no_location }
  in
  let zero = Core.const (Constant.C_int { v = 0l; repr = None }) in
  let one = Core.const (Constant.C_int { v = 1l; repr = None }) in
  let elem_id = Ident.fresh "*elem" in
  let loop_label = Label.fresh "*loop" in
  let elem_rhs =
    Core.prim ~ty:elem_ty
      (Pfixedarray_get_item { kind = Unsafe })
      [ arr_expr; i_expr ]
  in
  let if_body =
    subst_obj#visit_expr
      { old_id = anon_func_param; new_id = elem_id }
      anon_func_body
  in
  let cont =
    Core.continue
      [
        Core.prim
          (Parith { operand_type = I32; operator = Add })
          [ i_expr; one ] ~ty:Stype.int;
      ]
      loop_label Stype.unit
  in
  let ifso = Core.let_ elem_id elem_rhs (Core.sequence2 if_body cont) in
  let if_expr =
    Core.if_ ~ifso
      (Core.prim ~ty:Stype.bool Primitive.lt_int [ i_expr; len_expr ])
  in
  let loop =
    Core.loop [ i_param ] if_expr [ zero ] loop_label ~loc:Rloc.no_location
  in
  Core.let_ len_id len_rhs loop

let visitor =
  object
    inherit [_] Core.Map.map
    method! visit_binder subst id = Ident.Hash.find_default subst id id
    method! visit_var subst id = Ident.Hash.find_default subst id id
  end

let substitute ~subst (expr : Core.expr) =
  (visitor#visit_expr subst expr : Core.expr)

let single_visitor =
  object
    inherit [_] Core.Map.map
    method! visit_binder (k, v) id = if Ident.equal id k then v else id
    method! visit_var (k, v) id = if Ident.equal id k then v else id
  end

let rec flat_let ?(loc = Rloc.no_location) (id : Ident.t) (rhs : Core.expr)
    (body : Core.expr) =
  (match rhs with
   | Cexpr_let { name = id2; rhs = rhs2; body = body2; ty = _; loc_ } ->
       let body = flat_let id body2 body in
       Core.let_ id2 rhs2 body ~loc:loc_
   | Cexpr_var { id = id2; _ } -> single_visitor#visit_expr (id, id2) body
   | _ -> Core.let_ id rhs body ~loc
    : Core.expr)

let rec apply_with_beta ?(loc = Rloc.no_location) (func : Core.expr)
    (args : Core.expr list) ty =
  match func with
  | Cexpr_var { id = func; ty = func_ty; ty_args_; prim } ->
      Core.apply ~loc ~ty_args_ ~kind:(Normal { func_ty }) ~ty ~prim func args
  | Cexpr_function { func = { params; body; is_async = _ }; _ } ->
      Lst.fold_left2 params args body (fun p ->
          fun a ->
           fun body -> flat_let p.binder a (transform_return_in_fn_body body))
  | Cexpr_let { name; rhs; body; ty = _ } ->
      flat_let name rhs (apply_with_beta body args ty) ~loc
  | func ->
      let id = Ident.fresh "*func" in
      let func_ty = Core.type_of_expr func in
      let app : Core.expr =
        Core.apply ~loc ~kind:(Normal { func_ty }) ~ty id args
      in
      Core.let_ id func app

let zero_expr = Core.const (Constant.C_int { v = 0l; repr = None })
let one_expr = Core.const (Constant.C_int { v = 1l; repr = None })
let iter_go_expr = one_expr
let iter_end_expr = zero_expr
let lt_expr e1 e2 = Core.prim ~ty:Stype.bool Primitive.lt_int [ e1; e2 ]
let eq_expr e1 e2 = Core.prim ~ty:Stype.bool Primitive.eq_int [ e1; e2 ]
let and_expr e1 e2 = Core.and_ ~loc:Rloc.no_location e1 e2
let or_expr e1 e2 = Core.or_ ~loc:Rloc.no_location e1 e2

let mutable_var_label : Parsing_syntax.label =
  { label_name = "val"; loc_ = Rloc.no_location }

let mutable_var_type (ty : Core.typ) = (Builtin.type_ref ty : Core.typ)

let add_one_expr e =
  Core.prim
    (Parith { operand_type = I32; operator = Add })
    [ e; one_expr ] ~ty:Stype.int

let anon_func ?(loc = Rloc.no_location) (param_ty : Stype.t)
    (body : Core.expr -> Core.expr) =
  (let p = Ident.fresh "*p" in
   let p_var = Core.var ~ty:param_ty p in
   let p_param : Core.param =
     { binder = p; ty = param_ty; loc_ = Rloc.no_location }
   in
   let body = body p_var in
   let ty =
     Stype.Tarrow
       {
         params_ty = [ param_ty ];
         ret_ty = Core.type_of_expr body;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   Core.function_ ~loc [ p_param ] body ~ty ~is_async:false
    : Core.expr)

let ref_init expr =
  let ty = Core.type_of_expr expr in
  Core.record ~ty:(mutable_var_type ty)
    [ { label = mutable_var_label; pos = 0; expr; is_mut = true } ]

let ignore_ ?loc e = Core.prim ?loc ~ty:Stype.unit Pignore [ e ]

let make_iter_iter ~(it : Core.expr) ~(f : Core.expr) ~loc =
  (let a_ty =
     match Stype.type_repr (Core.type_of_expr f) with
     | Tarrow { params_ty = a_ty :: []; _ } -> a_ty
     | _ -> assert false
   in
   let arg =
     anon_func a_ty (fun a_var ->
         Core.sequence2 (apply_with_beta f [ a_var ] Stype.unit) iter_go_expr)
   in
   ignore_ ~loc (apply_with_beta it [ arg ] Stype.int ~loc)
    : Core.expr)

let make_iter_map ~(it : Core.expr) ~(f : Core.expr) ~loc =
  (let a_ty, b_ty =
     match Stype.type_repr (Core.type_of_expr f) with
     | Tarrow
         {
           ret_ty = b_ty;
           params_ty = a_ty :: [];
           err_ty = None;
           is_async = false;
         } ->
         (a_ty, b_ty)
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ b_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   anon_func k_ty (fun k_var ->
       let arg =
         anon_func a_ty (fun a_var ->
             apply_with_beta k_var
               [ apply_with_beta f [ a_var ] b_ty ]
               Stype.int)
       in
       apply_with_beta it [ arg ] Stype.int ~loc)
    : Core.expr)

let make_iter_filter ~(it : Core.expr) ~(f : Core.expr) ~loc =
  (let a_ty =
     match Stype.type_repr (Core.type_of_expr f) with
     | Tarrow { params_ty = a_ty :: []; _ } -> a_ty
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ a_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   anon_func k_ty (fun k_var ->
       let arg =
         anon_func a_ty (fun a_var ->
             let ifso = apply_with_beta k_var [ a_var ] Stype.int in
             let ifnot = iter_go_expr in
             Core.if_ (apply_with_beta f [ a_var ] Stype.bool) ~ifso ~ifnot)
       in
       apply_with_beta it [ arg ] Stype.int ~loc)
    : Core.expr)

let make_iter_take ~(it : Core.expr) ~(n : Core.expr) ~loc =
  (let a_ty =
     match Stype.type_repr (Core.type_of_expr it) with
     | T_constr { type_constructor = _; tys = a_ty :: [] } -> a_ty
     | Tarrow { params_ty = Tarrow { params_ty = a_ty :: []; _ } :: []; _ } ->
         a_ty
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ a_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   let i_id = Ident.fresh "*i" in
   let i_expr = Core.var ~ty:(mutable_var_type Stype.int) i_id in
   let i_init = ref_init zero_expr in
   let i_field =
     Core.field ~pos:0 ~ty:Stype.int i_expr (Label mutable_var_label)
   in
   let bind_i e = Core.let_ i_id i_init e in
   anon_func k_ty ~loc (fun k_var ->
       let mutate_i =
         Core.mutate ~pos:0 i_expr mutable_var_label (add_one_expr i_field)
       in
       let arg =
         anon_func a_ty (fun a_var ->
             let ifso = Core.sequence2 mutate_i iter_go_expr in
             let k_a = apply_with_beta k_var [ a_var ] Stype.int in
             let cond =
               and_expr (lt_expr i_field n) (eq_expr k_a iter_go_expr)
             in
             Core.if_ cond ~ifso ~ifnot:iter_end_expr)
       in
       let cond =
         or_expr
           (eq_expr (apply_with_beta it [ arg ] Stype.int) iter_go_expr)
           (eq_expr i_field n)
       in
       bind_i (Core.if_ cond ~ifso:iter_go_expr ~ifnot:iter_end_expr))
    : Core.expr)

let make_iter_reduce ~(it : Core.expr) ~(init : Core.expr) ~(f : Core.expr) ~loc
    =
  (let a_ty, b_ty =
     match Stype.type_repr (Core.type_of_expr f) with
     | Tarrow { params_ty = [ b_ty; a_ty ]; _ } -> (a_ty, b_ty)
     | _ -> assert false
   in
   let acc_id = Ident.fresh "*acc" in
   let acc_expr = Core.var ~ty:(mutable_var_type b_ty) acc_id in
   let acc_init = ref_init init in
   let bind_acc e = Core.let_ acc_id acc_init e in
   let acc_field =
     Core.field ~pos:0 ~ty:b_ty acc_expr (Label mutable_var_label)
   in
   let arg =
     anon_func a_ty (fun a_var ->
         let mutate =
           Core.mutate ~pos:0 acc_expr mutable_var_label
             (apply_with_beta f [ acc_field; a_var ] b_ty)
         in
         Core.sequence2 mutate iter_go_expr)
   in
   bind_acc
     (Core.sequence2
        (ignore_ (apply_with_beta it [ arg ] Stype.int))
        acc_field ~loc)
    : Core.expr)

let make_iter_flat_map ~(it : Core.expr) ~(f : Core.expr) ~loc =
  (let a_ty, b_ty =
     match Stype.type_repr (Core.type_of_expr f) with
     | Tarrow
         {
           params_ty = a_ty :: [];
           ret_ty =
             ( Tarrow
                 { params_ty = Tarrow { params_ty = b_ty :: []; _ } :: []; _ }
             | T_constr { type_constructor = _; tys = b_ty :: [] } );
           _;
         } ->
         (a_ty, b_ty)
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ b_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   let iter_b_ty =
     Stype.Tarrow
       {
         params_ty = [ k_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   anon_func k_ty (fun k_var ->
       let arg =
         anon_func a_ty (fun a_var ->
             let f_a = apply_with_beta f [ a_var ] iter_b_ty in
             apply_with_beta f_a [ k_var ] Stype.int)
       in
       apply_with_beta it [ arg ] Stype.int ~loc)
    : Core.expr)

let make_iter_repeat (a : Core.expr) ~loc =
  (let a_ty = Core.type_of_expr a in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ a_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   let loop_label = Label.fresh "*loop" in
   anon_func k_ty (fun k_var ->
       let ifso = Core.continue [] loop_label Stype.int in
       let ifnot =
         Core.break (Some iter_end_expr) loop_label Stype.int
           ~loc_:Rloc.no_location
       in
       let cond =
         eq_expr (apply_with_beta k_var [ a ] Stype.int) iter_go_expr
       in
       let if_expr = Core.if_ cond ~ifso ~ifnot in
       Core.loop [] if_expr [] loop_label ~loc)
    : Core.expr)

let make_iter_concat (it : Core.expr) (other_it : Core.expr) ~loc =
  (let a_ty =
     match Stype.type_repr (Core.type_of_expr it) with
     | T_constr { type_constructor = _; tys = a_ty :: [] } -> a_ty
     | Tarrow { params_ty = Tarrow { params_ty = a_ty :: []; _ } :: []; _ } ->
         a_ty
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ a_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   anon_func ~loc k_ty (fun k_var ->
       let it_k = apply_with_beta it [ k_var ] Stype.int in
       let other_it_k = apply_with_beta other_it [ k_var ] Stype.int in
       let cond =
         and_expr (eq_expr it_k iter_go_expr) (eq_expr other_it_k iter_go_expr)
       in
       Core.if_ cond ~ifso:iter_go_expr ~ifnot:iter_end_expr)
    : Core.expr)

let make_iter_from_array ~(arr : Core.expr) ~loc =
  (let arr_ty = Core.type_of_expr arr in
   let elem_ty =
     match Stype.type_repr arr_ty with
     | T_constr { type_constructor = T_fixedarray; tys = elem_ty :: [] } ->
         elem_ty
     | _ -> assert false
   in
   let k_ty =
     Stype.Tarrow
       {
         params_ty = [ elem_ty ];
         ret_ty = Stype.int;
         err_ty = None;
         is_async = false;
         generic_ = false;
       }
   in
   anon_func ~loc k_ty (fun k_var ->
       let arr_id = Ident.fresh "*arr" in
       let arr_expr = Core.var ~ty:arr_ty arr_id in
       let i_id = Ident.fresh "*i" in
       let i_expr = Core.var ~ty:Stype.int i_id in
       let i_param : Core.param =
         { binder = i_id; ty = Stype.int; loc_ = Rloc.no_location }
       in
       let k_arr_i =
         apply_with_beta k_var
           [
             Core.prim ~ty:elem_ty
               (Pfixedarray_get_item { kind = Unsafe })
               [ arr_expr; i_expr ];
           ]
           Stype.int
       in
       let loop_label = Label.fresh "*loop" in
       let if_cond = eq_expr k_arr_i iter_go_expr in
       let cont = Core.continue [ add_one_expr i_expr ] loop_label Stype.int in
       let brk_false =
         Core.break (Some iter_end_expr) loop_label Stype.int
           ~loc_:Rloc.no_location
       in
       let brk_true =
         Core.break (Some iter_go_expr) loop_label Stype.int
           ~loc_:Rloc.no_location
       in
       let ifso = Core.if_ if_cond ~ifso:cont ~ifnot:brk_false in
       let len_id = Ident.fresh "*len" in
       let len_rhs =
         Core.prim ~ty:Stype.int Primitive.Pfixedarray_length [ arr_expr ]
       in
       let len_expr = Core.var ~ty:Stype.int len_id in
       let if_expr = Core.if_ ~ifso (lt_expr i_expr len_expr) ~ifnot:brk_true in
       let loop =
         Core.loop [ i_param ] if_expr [ zero_expr ] loop_label
           ~loc:Rloc.no_location
       in
       Core.let_ arr_id arr (Core.let_ len_id len_rhs loop))
    : Core.expr)

let make_array_length ?loc arr =
  Core.field ?loc ~ty:Stype.int ~pos:1 arr
    (Label { label_name = "len"; loc_ = Rloc.no_location })

let make_array_buffer ~elem_ty ~buf_label_name arr =
  let ty = Builtin.type_fixedarray (Builtin.type_maybe_uninit elem_ty) in
  Core.field ~ty ~pos:0 arr
    (Label { label_name = buf_label_name; loc_ = Rloc.no_location })

let make_array_unsafe_get ~elem_ty ~buf_label_name arr index =
  Core.prim ~ty:elem_ty
    (Pfixedarray_get_item { kind = Unsafe })
    [ make_array_buffer ~elem_ty ~buf_label_name arr; index ]

let make_array_unsafe_set arr index value =
  let elem_ty = Core.type_of_expr value in
  Core.prim ~ty:Stype.unit
    (Pfixedarray_set_item { set_kind = Unsafe })
    [ make_array_buffer ~elem_ty ~buf_label_name:"buf" arr; index; value ]

let make_arrayview_start arr =
  Core.field ~ty:Stype.int ~pos:1 arr
    (Label { label_name = "start"; loc_ = Rloc.no_location })

let make_arrayview_length ~loc arr =
  Core.field ~loc ~ty:Stype.int ~pos:2 arr
    (Label { label_name = "len"; loc_ = Rloc.no_location })

let make_int_add e1 e2 =
  (Core.prim ~ty:Stype.int
     (Parith { operand_type = I32; operator = Add })
     [ e1; e2 ]
    : Core.expr)

let make_int_sub e1 e2 =
  (Core.prim ~ty:Stype.int
     (Parith { operand_type = I32; operator = Sub })
     [ e1; e2 ]
    : Core.expr)

let try_apply_intrinsic (intrinsic : Moon_intrinsic.t) (args : Core.expr list)
    ~loc ~ty =
  let rec bind_impure (rhs : Core.expr) (cont : Core.expr -> Core.expr) =
    match rhs with
    | Cexpr_var { id = Pident _ | Pdot _ | Plocal_method _; _ }
    | Cexpr_function _ | Cexpr_const _ ->
        cont rhs
    | Cexpr_let { name; rhs; body; ty = _ } ->
        Core.let_ name rhs (bind_impure body cont)
    | _ ->
        let id = Ident.fresh "*bind" in
        let var = Core.var ~ty:(Core.type_of_expr rhs) id in
        Core.let_ id rhs (cont var)
  in
  match (intrinsic, args) with
  | FixedArray_iter, [ arr; Cexpr_function { func; ty = _ } ] ->
      let arr_ty = Core.type_of_expr arr in
      let elem_ty =
        match Stype.type_repr arr_ty with
        | T_constr { type_constructor = T_fixedarray; tys = elem_ty :: [] } ->
            elem_ty
        | _ -> assert false
      in
      let anon_func_param =
        match func.params with p :: [] -> p.binder | _ -> assert false
      in
      Some
        (Core.bind ~loc arr (fun arr_id ->
             inline_array_iter ~arr_id ~arr_ty ~elem_ty ~anon_func_param
               ~anon_func_body:func.body))
  | Iter_iter, [ it; f ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure f (fun f -> make_iter_iter ~it ~f ~loc)))
  | Iter_map, [ it; f ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure f (fun f -> make_iter_map ~it ~f ~loc)))
  | Iter_filter, [ it; f ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure f (fun f -> make_iter_filter ~it ~f ~loc)))
  | Iter_from_array, arr :: [] ->
      Some (bind_impure arr (fun arr -> make_iter_from_array ~arr ~loc))
  | Iter_take, [ it; n ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure n (fun n -> make_iter_take ~it ~n ~loc)))
  | Iter_reduce, [ it; init; f ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure init (fun init ->
                 bind_impure f (fun f -> make_iter_reduce ~it ~f ~init ~loc))))
  | Iter_flat_map, [ it; f ] ->
      Some
        (bind_impure it (fun it ->
             bind_impure f (fun f -> make_iter_flat_map ~it ~f ~loc)))
  | Iter_repeat, a :: [] ->
      Some (bind_impure a (fun a -> make_iter_repeat a ~loc))
  | Iter_concat, [ a; b ] ->
      Some
        (bind_impure a (fun a ->
             bind_impure b (fun b -> make_iter_concat a b ~loc)))
  | Array_length, arr :: [] -> Some (make_array_length ~loc arr)
  | Array_unsafe_get, [ arr; index ] ->
      Some (make_array_unsafe_get ~elem_ty:ty ~buf_label_name:"buf" arr index)
  | Array_get, [ arr; index ] ->
      Some
        (Core.bind arr (fun arr_id ->
             let arr = Core.var ~ty:(Core.type_of_expr arr) arr_id in
             Core.bind index (fun index_id ->
                 let index = Core.var ~ty:(Core.type_of_expr index) index_id in
                 let len = make_array_length arr in
                 let range_check =
                   or_expr
                     (Core.prim ~ty:Stype.bool
                        (Pcomparison { operand_type = I32; operator = Lt })
                        [ index; Core.const (C_int { v = 0l; repr = None }) ])
                     (Core.prim ~ty:Stype.bool
                        (Pcomparison { operand_type = I32; operator = Ge })
                        [ index; len ])
                 in
                 Core.sequence2
                   (Core.if_ ~loc range_check
                      ~ifso:(Core.prim ~ty:Stype.unit Ppanic []))
                   (make_array_unsafe_get ~elem_ty:ty ~buf_label_name:"buf" arr
                      index))))
  | Array_unsafe_set, [ arr; index; value ] ->
      Some (make_array_unsafe_set arr index value)
  | Array_set, [ arr; index; value ] ->
      Some
        (Core.bind arr (fun arr_id ->
             let arr = Core.var ~ty:(Core.type_of_expr arr) arr_id in
             Core.bind index (fun index_id ->
                 let index = Core.var ~ty:(Core.type_of_expr index) index_id in
                 Core.bind value (fun value_id ->
                     let value =
                       Core.var ~ty:(Core.type_of_expr value) value_id
                     in
                     let len = make_array_length arr in
                     let range_check =
                       or_expr
                         (Core.prim ~ty:Stype.bool
                            (Pcomparison { operand_type = I32; operator = Lt })
                            [
                              index; Core.const (C_int { v = 0l; repr = None });
                            ])
                         (Core.prim ~ty:Stype.bool
                            (Pcomparison { operand_type = I32; operator = Ge })
                            [ index; len ])
                     in
                     Core.sequence2
                       (Core.if_ ~loc range_check
                          ~ifso:(Core.prim ~ty:Stype.unit Ppanic []))
                       (make_array_unsafe_set arr index value)))))
  | (ArrayView_length | BytesView_length), arr :: [] ->
      Some (make_arrayview_length ~loc arr)
  | (ArrayView_unsafe_get | BytesView_unsafe_get), [ arr; index ] ->
      Some
        (Core.bind arr (fun arr_id ->
             let arr = Core.var ~ty:(Core.type_of_expr arr) arr_id in
             let elem_ty = ty in
             let buf_label_name =
               if intrinsic = ArrayView_unsafe_get then "buf" else "bytes"
             in
             make_array_unsafe_get ~elem_ty ~buf_label_name arr
               (make_int_add (make_arrayview_start arr) index)))
  | ArrayView_unsafe_set, [ arr; index; value ] ->
      Some
        (Core.bind arr (fun arr_id ->
             let arr = Core.var ~ty:(Core.type_of_expr arr) arr_id in
             make_array_unsafe_set arr
               (make_int_add (make_arrayview_start arr) index)
               value))
  | (ArrayView_unsafe_as_view | BytesView_unsafe_as_view), [ arr; start; end_ ]
    ->
      Some
        (Core.bind arr (fun arr_id ->
             Core.bind start (fun start_id ->
                 let arr = Core.var ~ty arr_id in
                 let start = Core.var ~ty:Stype.int start_id in
                 let make_label name =
                   ({ label_name = name; loc_ = Rloc.no_location } : Core.label)
                 in
                 let some_tag = Builtin.constr_some.cs_tag in
                 let some_binder = Ident.fresh "*some" in
                 let some_action =
                   Core.prim ~ty:Stype.int
                     (Penum_field { index = 0; tag = some_tag })
                     [
                       Core.var
                         ~ty:
                           (Type.make_constr_type (Core.type_of_expr end_)
                              ~tag:some_tag)
                         some_binder;
                     ]
                 in
                 let cases =
                   [
                     (Builtin.constr_some.cs_tag, Some some_binder, some_action);
                   ]
                 in
                 let end_ : Core.expr =
                   Core.switch_constr
                     ~default:(Some (make_array_length arr))
                     end_ cases
                 in
                 let elem_ty =
                   match ty with
                   | T_constr { tys = elem_ty :: []; _ } -> elem_ty
                   | T_constr { type_constructor = p; _ }
                     when Type_path.is_bytesview p ->
                       Stype.byte
                   | _ -> assert false
                 in
                 let buf_label_name =
                   if intrinsic = ArrayView_unsafe_as_view then "buf"
                   else "bytes"
                 in
                 let buf_field : Core.field_def =
                   {
                     label = make_label buf_label_name;
                     pos = 0;
                     is_mut = false;
                     expr = make_array_buffer arr ~elem_ty ~buf_label_name;
                   }
                 in
                 let start_field : Core.field_def =
                   {
                     label = make_label "start";
                     pos = 1;
                     is_mut = false;
                     expr = make_int_add start (make_arrayview_start arr);
                   }
                 in
                 let len_field : Core.field_def =
                   {
                     label = make_label "len";
                     pos = 2;
                     is_mut = false;
                     expr = make_int_sub end_ start;
                   }
                 in
                 let fields : Core.field_def list =
                   [ buf_field; start_field; len_field ]
                 in
                 Core.record ~ty fields)))
  | _ -> None

let make_length (env : Global_env.t) (expr : Core.expr) =
  let ty = Stype.type_repr (Core.type_of_expr expr) in
  match ty with
  | T_constr { type_constructor = T_fixedarray; _ } ->
      Core.prim ~ty:Stype.int Primitive.Pfixedarray_length [ expr ]
  | T_constr { type_constructor = p; _ }
    when Basic_type_path.equal p Basic_type_path.Builtin.type_path_arrayview ->
      Option.get
        (try_apply_intrinsic ArrayView_length [ expr ] ~loc:Rloc.no_location
           ~ty:Stype.int)
  | T_constr { type_constructor = p; _ } when Type_path.is_bytesview p ->
      Option.get
        (try_apply_intrinsic BytesView_length [ expr ] ~loc:Rloc.no_location
           ~ty:Stype.int)
  | T_constr { type_constructor = p; _ } -> (
      let method_ =
        List.hd
          (Global_env.find_dot_method env ~type_name:p ~method_name:"length")
      in
      let fallback () =
        let func_id = Ident.of_qual_ident method_.id in
        let func_ty, _, ty_args_ =
          Poly_type.instantiate_method_no_constraint method_
        in
        (match func_ty with
        | Tarrow { params_ty = ty_self :: []; _ } -> Ctype.unify_exn ty_self ty
        | _ -> assert false);
        Core.apply ~prim:None ~ty_args_
          ~kind:(Normal { func_ty })
          ~ty:Stype.int func_id [ expr ]
          [@@local]
      in
      match method_.prim with
      | Some (Pintrinsic intrinsic) -> (
          match
            try_apply_intrinsic intrinsic [ expr ] ~loc:Rloc.no_location
              ~ty:Stype.int
          with
          | Some result -> result
          | None -> fallback ())
      | Some prim -> Core.prim ~ty:Stype.int prim [ expr ]
      | None -> fallback ())
  | _ -> assert false

let make_array_unsafe_as_view arr ~(ty : Stype.t) ~loc_ =
  let make_label name =
    ({ label_name = name; loc_ = Rloc.no_location } : Core.label)
  in
  let elem_ty =
    match Stype.type_repr ty with
    | T_constr { tys = elem_ty :: []; _ } -> elem_ty
    | _ -> assert false
  in
  let buf = make_array_buffer arr ~elem_ty ~buf_label_name:"buf" in
  let len = make_array_length arr in
  let start = Core.const (C_int { v = 0l; repr = None }) in
  let buf_field : Core.field_def =
    { label = make_label "buf"; pos = 0; is_mut = false; expr = buf }
  in
  let start_field : Core.field_def =
    { label = make_label "start"; pos = 1; is_mut = false; expr = start }
  in
  let len_field : Core.field_def =
    { label = make_label "len"; pos = 2; is_mut = false; expr = len }
  in
  let fields : Core.field_def list = [ buf_field; start_field; len_field ] in
  Core.record ~ty fields ~loc:loc_

let make_op_as_view (env : Global_env.t) (expr : Core.expr)
    (drop_head_num : int) (drop_tail_num : int) =
  let wrap_some expr =
    let expr_ty = Core.type_of_expr expr in
    let ty : Stype.t =
      T_constr
        {
          type_constructor = Basic_type_path.Builtin.type_path_option;
          tys = [ expr_ty ];
          generic_ = false;
          is_suberror_ = false;
        }
    in
    let tag : Basic_constr_info.constr_tag = Builtin.constr_some.cs_tag in
    Core.constr ~ty tag [ expr ]
  in
  let ty = Stype.type_repr (Core.type_of_expr expr) in
  let start_index_expr =
    Core.const (C_int { v = Int32.of_int drop_head_num; repr = None })
  in
  let end_index_expr =
    lazy
      (let len_expr = make_length env expr in
       let n =
         Core.const (C_int { v = Int32.of_int drop_tail_num; repr = None })
       in
       Core.prim ~ty:Stype.int
         (Parith { operand_type = I32; operator = Sub })
         [ len_expr; n ])
  in
  let args =
    lazy [ expr; start_index_expr; wrap_some (Lazy.force end_index_expr) ]
  in
  let call_method p =
    let method_ =
      Global_env.find_dot_method env ~type_name:p ~method_name:"op_as_view"
    in
    match method_ with
    | method_ :: [] ->
        let func_id = Ident.of_qual_ident method_.id in
        let func_ty, _, ty_args_ =
          Poly_type.instantiate_method_no_constraint method_
        in
        let ret_ty =
          match func_ty with
          | Tarrow
              { params_ty = ty_self :: _; ret_ty; err_ty = _; is_async = _ } ->
              Ctype.unify_exn ty_self ty;
              ret_ty
          | _ -> assert false
        in
        if
          Type_path.equal p Type_path.Builtin.type_path_stringview
          || Type_path.equal p Type_path.Builtin.type_path_string
        then
          let end_index =
            if drop_tail_num = 0 then
              let ty : Stype.t =
                T_constr
                  {
                    type_constructor = Type_path.Builtin.type_path_option;
                    tys = [ Stype.int ];
                    generic_ = false;
                    is_suberror_ = false;
                  }
              in
              Core.constr ~ty Builtin.constr_none.cs_tag []
            else
              wrap_some
                (Core.const
                   (C_int { v = Int32.of_int (-drop_tail_num); repr = None }))
          in
          Core.apply ~prim:None ~ty_args_
            ~kind:(Normal { func_ty })
            ~ty:ret_ty func_id
            [ expr; start_index_expr; end_index ]
        else
          Core.apply ~prim:None ~ty_args_
            ~kind:(Normal { func_ty })
            ~ty:ret_ty func_id (Lazy.force args)
    | _ -> assert false
      [@@inline]
  in
  match ty with
  | T_constr { type_constructor = p; _ }
    when Type_path.equal p Type_path.Builtin.type_path_arrayview ->
      Option.get
        (try_apply_intrinsic ArrayView_unsafe_as_view (Lazy.force args)
           ~loc:Rloc.no_location ~ty)
  | T_constr { type_constructor = p; _ } when Type_path.is_bytesview p ->
      Option.get
        (try_apply_intrinsic BytesView_unsafe_as_view (Lazy.force args)
           ~loc:Rloc.no_location ~ty)
  | T_constr { type_constructor = p; _ } -> call_method p
  | T_builtin T_string -> call_method Type_path.Builtin.type_path_string
  | _ -> assert false

let make_string_codeunit_get (expr : Core.expr) (index : int) ~(rev : bool) =
  let index_expr = Core.const (C_int { v = Int32.of_int index; repr = None }) in
  let index_expr_after_rev =
    if rev then
      let len_expr = Core.prim ~ty:Stype.int Primitive.Pstringlength [ expr ] in
      let one = Core.const (C_int { v = 1l; repr = None }) in
      let psub a b =
        Core.prim ~ty:Stype.int
          (Parith { operand_type = I32; operator = Sub })
          [ a; b ]
      in
      psub (psub len_expr one) index_expr
    else index_expr
  in
  let prim : Primitive.prim = Pgetstringitem { safe = false } in
  Core.prim ~ty:Stype.char prim [ expr; index_expr_after_rev ]

let make_op_get (env : Global_env.t) (expr : Core.expr) (index : int)
    ~(rev : bool) =
  let ty = Stype.type_repr (Core.type_of_expr expr) in
  let index_expr = Core.const (C_int { v = Int32.of_int index; repr = None }) in
  let index_expr_after_rev =
    lazy
      (if rev then
         let len_expr = make_length env expr in
         let one = Core.const (C_int { v = 1l; repr = None }) in
         let psub a b =
           Core.prim ~ty:Stype.int
             (Parith { operand_type = I32; operator = Sub })
             [ a; b ]
         in
         psub (psub len_expr one) index_expr
       else index_expr)
  in
  let args = lazy [ expr; Lazy.force index_expr_after_rev ] in
  let call_method type_name method_name elem_ty args =
    match[@warning "-fragile-match"]
      Global_env.find_dot_method env ~type_name ~method_name
    with
    | method_ :: [] ->
        let func_id = Ident.of_qual_ident method_.id in
        let func_ty, _, ty_args_ =
          Poly_type.instantiate_method_no_constraint method_
        in
        (match func_ty with
        | Tarrow { ret_ty; _ } -> Ctype.unify_exn ret_ty elem_ty
        | _ -> assert false);
        Core.apply ~prim:None ~ty_args_
          ~kind:(Normal { func_ty })
          ~ty:elem_ty func_id args
    | _ -> assert false
      [@@inline]
  in
  match ty with
  | T_constr { type_constructor = T_fixedarray; tys = elem_ty :: [] } ->
      let prim : Primitive.prim =
        Pfixedarray_get_item { kind = (if rev then Rev_unsafe else Unsafe) }
      in
      Core.prim ~ty:elem_ty prim [ expr; index_expr ]
  | T_constr { type_constructor = p; tys = elem_ty :: [] }
    when Basic_type_path.equal p Basic_type_path.Builtin.type_path_arrayview ->
      Option.get
        (try_apply_intrinsic ArrayView_unsafe_get (Lazy.force args)
           ~loc:Rloc.no_location ~ty:elem_ty)
  | T_constr { type_constructor = p; tys = elem_ty :: [] }
    when Basic_type_path.equal p Basic_type_path.Builtin.type_path_array ->
      Option.get
        (try_apply_intrinsic Array_unsafe_get (Lazy.force args)
           ~loc:Rloc.no_location ~ty:elem_ty)
  | T_constr { type_constructor = p; _ } when Type_path.is_bytesview p ->
      Option.get
        (try_apply_intrinsic BytesView_unsafe_get (Lazy.force args)
           ~loc:Rloc.no_location ~ty:Stype.byte)
  | T_constr { type_constructor = p; _ }
    when Type_path.equal p Type_path.Builtin.type_path_stringview ->
      if rev then call_method p "rev_get" Stype.char [ expr; index_expr ]
      else call_method p "op_get" Stype.char (Lazy.force args)
  | T_builtin T_string ->
      let p = Type_path.Builtin.type_path_string in
      if rev then call_method p "rev_get" Stype.char [ expr; index_expr ]
      else call_method p "codepoint_at" Stype.char (Lazy.force args)
  | T_constr { type_constructor = p; tys = elem_ty :: [] } ->
      call_method p "op_get" elem_ty (Lazy.force args)
  | _ -> assert false

let make_array_make (exprs : Core.expr list) ~loc_ ~ty =
  (let make_label name =
     ({ label_name = name; loc_ = Rloc.no_location } : Core.label)
   in
   let elem_ty =
     match Stype.type_repr ty with
     | T_constr { type_constructor = _; tys = elem_ty :: [] } -> elem_ty
     | _ -> assert false
   in
   let buf_ty : Stype.t =
     T_constr
       {
         type_constructor = Type_path.Builtin.type_path_fixedarray;
         tys =
           [
             T_constr
               {
                 type_constructor = Type_path.Builtin.type_path_maybe_uninit;
                 tys = [ elem_ty ];
                 generic_ = false;
                 is_suberror_ = false;
               };
           ];
         generic_ = false;
         is_suberror_ = false;
       }
   in
   let fields : Core.field_def list =
     [
       {
         label = make_label "buf";
         pos = 0;
         is_mut = true;
         expr =
           Core.prim ~ty:buf_ty
             (Pfixedarray_make { kind = EverySingleElem })
             exprs;
       };
       {
         label = make_label "len";
         pos = 1;
         is_mut = true;
         expr =
           Core.const
             (C_int { v = Int32.of_int (List.length exprs); repr = None });
       };
     ]
   in
   Core.record ~ty ~loc:loc_ fields
    : Core.expr)

let all_fields_immutable (global_env : Global_env.t) (tp : Type_path.t)
    ~(allow_enum : bool) =
  match tp with
  | Tuple _ -> true
  | _ when Type_path.equal tp Type_path.Builtin.type_path_arrayview -> true
  | _ -> (
      match Global_env.find_all_type_by_path global_env tp with
      | Some t -> (
          match t.ty_desc with
          | Record_type { fields; _ } ->
              Lst.for_all fields (fun { mut; _ } -> not mut)
          | Variant_type cs when allow_enum ->
              Basic_lst.for_all cs (fun c ->
                  Fn_arity.is_immutable_constr_arity c.cs_arity_)
          | _ -> false)
      | None -> false)

let arrayview_ty_params : Tvar_env.t = Tvar_env.tvar_env_1

let arrayview_ty : Stype.t =
  Stype.T_constr
    {
      type_constructor = Type_path.Builtin.type_path_arrayview;
      tys = Tvar_env.get_types arrayview_ty_params;
      generic_ = true;
      is_suberror_ = false;
    }

let arrayview_all_labels = [ "buf"; "start"; "len" ]

let field_buf : Typedecl_info.field =
  {
    field_name = "buf";
    pos = 0;
    ty_field =
      Stype.T_constr
        {
          type_constructor = Type_path.Builtin.type_path_fixedarray;
          tys = [ Stype.param0 ];
          generic_ = true;
          is_suberror_ = false;
        };
    ty_record = arrayview_ty;
    mut = false;
    vis = Read_write;
    all_labels = arrayview_all_labels;
    ty_params_ = arrayview_ty_params;
    label_loc_ = Loc.no_location;
    loc_ = Loc.no_location;
  }

let field_start : Typedecl_info.field =
  {
    field_name = "start";
    pos = 1;
    ty_field = Stype.int;
    ty_record = arrayview_ty;
    mut = false;
    vis = Read_write;
    all_labels = arrayview_all_labels;
    ty_params_ = arrayview_ty_params;
    label_loc_ = Loc.no_location;
    loc_ = Loc.no_location;
  }

let field_len : Typedecl_info.field =
  {
    field_name = "len";
    pos = 2;
    ty_field = Stype.int;
    ty_record = arrayview_ty;
    mut = false;
    vis = Read_write;
    all_labels = arrayview_all_labels;
    ty_params_ = arrayview_ty_params;
    label_loc_ = Loc.no_location;
    loc_ = Loc.no_location;
  }

let arrayview_fields = [ field_buf; field_start; field_len ]

let get_fields_exn (global_env : Global_env.t) (tp : Type_path.t) =
  match tp with
  | Tuple _ -> []
  | _ when Type_path.equal tp Type_path.Builtin.type_path_arrayview ->
      arrayview_fields
  | _ -> (
      match Global_env.find_all_type_by_path global_env tp with
      | Some t -> (
          match t.ty_desc with
          | Record_type { fields; _ } -> fields
          | _ -> assert false)
      | None -> assert false)

let make_length_test (global_env : Global_env.t) (expr : Core.expr) (num : int)
    ~(kind : [ `Geq | `Eq ]) ~ifso ~ifnot =
  let expr_ty = Stype.type_repr (Core.type_of_expr expr) in
  let call_method p =
    let num_pats_expr =
      Core.const (C_int { v = Int32.of_int num; repr = None })
    in
    let test_expr =
      let method_ =
        List.hd
          (match kind with
          | `Geq ->
              Global_env.find_dot_method global_env ~type_name:p
                ~method_name:"length_ge"
          | `Eq ->
              Global_env.find_dot_method global_env ~type_name:p
                ~method_name:"length_eq")
      in
      Core.apply
        ~kind:(Normal { func_ty = method_.typ })
        ~ty:Stype.bool
        (Ident.of_qual_ident method_.id)
        [ expr; num_pats_expr ]
    in
    Core.if_ test_expr ~ifso ~ifnot
      [@@inline]
  in
  match expr_ty with
  | T_constr { type_constructor = p; _ }
    when Type_path.equal p Type_path.Builtin.type_path_stringview ->
      call_method p
  | T_builtin T_string -> call_method Type_path.Builtin.type_path_string
  | _ ->
      let arr_len_expr = make_length global_env expr in
      let num_pats_expr =
        Core.const (C_int { v = Int32.of_int num; repr = None })
      in
      let test_expr =
        let prim =
          if kind = `Geq then Primitive.ge_int else Primitive.equal_int
        in
        Core.prim prim [ arr_len_expr; num_pats_expr ] ~ty:Stype.bool
      in
      Core.if_ test_expr ~ifso ~ifnot
