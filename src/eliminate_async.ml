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
module Qual_ident = Basic_qual_ident
module Type_path = Basic_type_path
module Constr_info = Basic_constr_info
module Lst = Basic_lst
module Vec = Basic_vec

exception Need_cps

let need_cps_visitor =
  object (self)
    inherit [_] Core.Iter.iter as super

    method! visit_apply_kind _ kind =
      match kind with
      | Async _ -> raise_notrace Need_cps
      | Normal _ | Join -> ()

    method! visit_Cexpr_prim ctx prim args ty loc =
      (match prim with
      | Pget_current_continuation -> raise_notrace Need_cps
      | _ -> ());
      super#visit_Cexpr_prim ctx prim args ty loc

    method! visit_Cexpr_letfn ctx name fn body ty kind loc =
      match kind with
      | Tail_join | Nontail_join ->
          super#visit_Cexpr_letfn ctx name fn body ty kind loc
      | Nonrec | Rec -> self#visit_expr ctx body

    method! visit_Cexpr_letrec ctx _bindings body _ty _loc =
      self#visit_expr ctx body

    method! visit_Cexpr_function _ _func _ty _is_raw _loc = ()
  end

let need_cps expr =
  try
    need_cps_visitor#visit_expr () expr;
    false
  with Need_cps -> true

type continuation =
  | Identity
  | Return of { cont : Ident.t; cont_ty : Stype.t }
  | Simple of { state_id : int }
  | Complex of (Core.expr -> Core.expr)

type loop_info = { loop_state : int; break_cont : continuation }

type state = {
  state_id : int;
  state_name : string;
  params : Core.param list;
  captures : (Ident.t * Stype.t) list;
  body : Core.expr;
}

type ctx = {
  join_to_state : int Ident.Hash.t;
  loop_info : loop_info Label.Hash.t;
  return_cont : continuation;
  return_err_cont : continuation;
  driver_id : Ident.t;
  driver_ty : Stype.t;
  cont_params : Core.param list;
  state_ty : Stype.t;
  states : state Vec.t;
  ty_params : Tvar_env.t;
  toplevel_id : Type_path.toplevel_id;
  global_env : Global_env.t;
}

let make_state ctx state_id args =
  let state = Vec.get ctx.states state_id in
  let state_name = state.state_name in
  Core.constr ~ty:ctx.state_ty
    (Constr_tag_regular
       {
         total = Constr_info.Index_set.empty;
         index = state_id;
         repr_ = Non_constant;
         name_ = state_name;
       })
    (args @ Lst.map state.captures (fun (id, ty) -> Core.var ~ty id))

let rename_visitor =
  object
    inherit [_] Core.Map.map
    method! visit_var subst var = Ident.Hash.find_default subst var var
  end

let resume ctx ?(loc = Rloc.no_location) (k : continuation) (v : Core.expr) =
  match k with
  | Identity -> v
  | Return { cont; cont_ty } ->
      Core.apply ~loc cont [ v ] ~ty:Stype.unit
        ~kind:(Normal { func_ty = cont_ty })
  | Simple { state_id } ->
      Core.apply ~loc
        ~kind:(Normal { func_ty = ctx.driver_ty })
        ~ty:Stype.unit ctx.driver_id
        [ make_state ctx state_id [ v ] ]
  | Complex f -> f v

let ( let* ) wanting_continuation callback =
  wanting_continuation (Complex callback)
[@@dead "+let*"]

let id_cont ty =
  let param = Ident.fresh "*param" in
  Core.function_
    ~ty:(Builtin.type_arrow [ ty ] Stype.unit ~err_ty:None ~is_async:false)
    ~is_async:false
    [ { binder = param; ty = Stype.unit; loc_ = Rloc.no_location } ]
    (Core.unit ())

let add_captures_of_cont ctx captures k =
  match k with
  | Identity -> captures
  | Return { cont; cont_ty } -> Ident.Map.add captures cont cont_ty
  | Simple { state_id } ->
      let { captures = state_captures; _ } = Vec.get ctx.states state_id in
      Lst.fold_left state_captures captures (fun captures ->
          fun (id, ty) -> Ident.Map.add captures id ty)
  | Complex _ -> assert false

let add_recursive_state ctx ~state_name ~captures (params : Core.param list)
    body =
  let state_id = Vec.length ctx.states in
  let state_name = state_name ~state_id in
  let captures =
    Lst.map_append ctx.cont_params captures (fun p -> (p.binder, p.ty))
  in
  let state = { state_id; state_name; params; captures; body = Core.unit () } in
  Vec.push ctx.states state;
  let body = body state_id in
  Vec.set ctx.states state_id { state with body };
  state

let add_new_state ctx ~state_name params body =
  let state_id = Vec.length ctx.states in
  let state_name = state_name ~state_id in
  let captures =
    let exclude = Ident.Set.singleton ctx.driver_id in
    Ident.Map.bindings
      (Core_util.free_vars ~exclude { params; body; is_async = false })
  in
  let state = { state_id; state_name; params; captures; body } in
  Vec.push ctx.states state;
  state

let cont_is_simple cont =
  match cont with Identity | Return _ | Simple _ -> true | Complex _ -> false

let make_cont_simple ctx ~ty k =
  match k with
  | Identity | Return _ | Simple _ -> k
  | Complex f ->
      let param = Ident.fresh "*cont_param" in
      let state_name ~state_id =
        (("State_" ^ Int.to_string state_id : Stdlib.String.t) [@merlin.hide])
      in
      let state =
        match Stype.type_repr ty with
        | T_constr
            { type_constructor = T_error_value_result; tys = [ ok_ty; err_ty ] }
          ->
            let result_ty = Stype.make_result_ty ~ok_ty ~err_ty in
            let handle_error =
              let ok_var = Ident.fresh "*ok" in
              let err_var = Ident.fresh "*err" in
              let branch tag branch_ty var =
                ( tag,
                  Some var,
                  Core.prim ~ty
                    (Pmake_value_or_error { tag })
                    [
                      Core.prim ~ty:branch_ty
                        (Penum_field { index = 0; tag })
                        [
                          Core.var
                            ~ty:(Type.make_constr_type result_ty ~tag)
                            var;
                        ];
                    ] )
              in
              Core.switch_constr
                (Core.var ~ty:result_ty param)
                [
                  branch Builtin.constr_ok.cs_tag ok_ty ok_var;
                  branch Builtin.constr_err.cs_tag err_ty err_var;
                ]
                ~default:None
            in
            add_new_state ctx ~state_name
              [ { binder = param; ty = result_ty; loc_ = Rloc.no_location } ]
              (f handle_error)
        | ty ->
            add_new_state ctx ~state_name
              [ { binder = param; ty; loc_ = Rloc.no_location } ]
              (f (Core.var ~ty param))
      in
      Simple { state_id = state.state_id }

let cont_to_expr ctx ~ty k =
  let ty = Stype.type_repr ty in
  let cont_ty arg_ty =
    Builtin.type_arrow [ arg_ty ] Stype.unit ~err_ty:None ~is_async:false
      [@@inline]
  in
  match make_cont_simple ctx ~ty k with
  | Identity -> id_cont ty
  | Return { cont; cont_ty } -> Core.var ~ty:cont_ty cont
  | Simple { state_id } as k -> (
      let param = Ident.fresh "*cont_param" in
      match[@warning "-fragile-match"] (Vec.get ctx.states state_id).params with
      | { ty = param_ty; _ } :: [] ->
          Core.function_ ~ty:(cont_ty param_ty) ~is_async:false
            [ { binder = param; ty = param_ty; loc_ = Rloc.no_location } ]
            (resume ctx k (Core.var ~ty:param_ty param))
      | _ -> assert false)
  | Complex _ -> assert false

let rec transform_expr ctx (expr : Core.expr) ~error_cont k =
  let go expr k = transform_expr ctx expr ~error_cont k [@@inline] in
  let go_list exprs k =
    transform_expr_list ctx exprs ~error_cont k
      [@@inline]
  in
  let go_no_cps expr = transform_expr_no_cps ctx ~error_cont expr [@@inline] in
  match expr with
  | Cexpr_apply { kind = Async _ as kind; func; args; ty; ty_args_; prim; loc_ }
    ->
      let cont_args =
        match Stype.type_repr ty with
        | T_constr
            { type_constructor = T_error_value_result; tys = [ ok_ty; err_ty ] }
          ->
            [
              cont_to_expr ctx ~ty:ok_ty k;
              cont_to_expr ctx ~ty:err_ty error_cont;
            ]
        | ty -> [ cont_to_expr ctx ~ty k ]
      in
      go_list args (fun args ->
          Core.apply ~loc:loc_ ~kind ~ty:Stype.unit ~ty_args_ ~prim func
            (args @ cont_args))
  | Cexpr_prim { prim = Pget_current_continuation; args; ty; loc_ } -> (
      match[@warning "-fragile-match"] args with
      | cc_callback :: [] -> (
          let* cc_callback = go cc_callback in
          let conts =
            match Stype.type_repr ty with
            | T_constr
                {
                  type_constructor = T_error_value_result;
                  tys = [ ok_ty; err_ty ];
                } ->
                [
                  cont_to_expr ctx ~ty:ok_ty k;
                  cont_to_expr ctx ~ty:err_ty error_cont;
                ]
            | ty -> [ cont_to_expr ctx ~ty k ]
          in
          match cc_callback with
          | Cexpr_function { func = { params; body; is_async = _ }; ty = _ } ->
              Lst.fold_right2 params conts body (fun p ->
                  fun cont -> fun body -> Core.let_ p.binder cont body)
          | _ ->
              let func_ty = Core.type_of_expr cc_callback in
              Core.bind cc_callback (fun cc_callback ->
                  Core.apply ~loc:loc_
                    ~kind:(Normal { func_ty })
                    ~ty:Stype.unit cc_callback conts))
      | _ -> assert false)
  | Cexpr_prim { prim = Prun_async; args; ty; loc_ } -> (
      match[@warning "-fragile-match"] args with
      | async_fn :: [] -> (
          let* async_fn = go async_fn in
          let id_cont = cont_to_expr ctx ~ty Identity in
          match async_fn with
          | Cexpr_function
              { func = { params = param :: []; body; is_async = _ }; ty = _ } ->
              resume ctx k (Core.let_ param.binder id_cont body)
          | _ ->
              let func_ty = Core.type_of_expr async_fn in
              Core.bind async_fn (fun async_fn ->
                  resume ctx k
                    (Core.apply ~loc:loc_
                       ~kind:(Normal { func_ty })
                       ~ty:Stype.unit async_fn [ id_cont ])))
      | _ -> assert false)
  | Cexpr_letfn
      { name; fn; body; ty; kind = (Tail_join | Nontail_join) as kind; loc_ } ->
      if need_cps fn.body || need_cps body then (
        let k = make_cont_simple ctx ~ty k in
        let join_body = go fn.body k in
        let state =
          add_new_state ctx
            ~state_name:(fun ~state_id:_ -> Ident.to_string name)
            fn.params join_body
        in
        Ident.Hash.add ctx.join_to_state name state.state_id;
        go body k)
      else
        let join_body = go_no_cps fn.body in
        let body = go_no_cps body in
        resume ctx k
          (Core.letfn ~loc:loc_ ~kind name { fn with body = join_body } body)
  | Cexpr_apply { kind = Join; func; args; ty = _; ty_args_; prim; loc_ } ->
      go_list args (fun args ->
          match Ident.Hash.find_opt ctx.join_to_state func with
          | Some state_id ->
              Core.return ~ty:Stype.unit ~return_kind:Single_value
                (Core.apply ~loc:loc_
                   ~kind:(Normal { func_ty = ctx.driver_ty })
                   ~ty:Stype.unit ~ty_args_ ~prim ctx.driver_id
                   [ make_state ctx state_id args ])
          | None ->
              Core.apply ~loc:loc_ ~kind:Join ~ty:Stype.unit ~ty_args_ ~prim
                func args)
  | Cexpr_loop { params; body; args; label; ty; loc_ } ->
      go_list args (fun args ->
          if need_cps body then
            let k = make_cont_simple ctx ~ty k in
            let state_name ~state_id =
              ((Label.basename label ^ "_" ^ Int.to_string state_id
                : Stdlib.String.t)
                [@merlin.hide])
            in
            let state =
              let captures =
                Ident.Map.bindings (get_captures_of_loop ctx params body k)
              in
              add_recursive_state ctx ~state_name params ~captures
                (fun loop_state ->
                  Label.Hash.add ctx.loop_info label
                    { loop_state; break_cont = k };
                  go body k)
            in
            Core.apply
              ~kind:(Normal { func_ty = ctx.driver_ty })
              ~ty:Stype.unit ctx.driver_id
              [ make_state ctx state.state_id args ]
          else
            resume ctx k
              (Core.loop ~loc:loc_ params (go_no_cps body) args label))
  | Cexpr_break { arg; label; ty; loc_ } -> (
      match Label.Hash.find_opt ctx.loop_info label with
      | None -> Core.break ~loc_ (Option.map go_no_cps arg) label ty
      | Some { break_cont; _ } ->
          let arg = match arg with None -> Core.unit () | Some arg -> arg in
          Core.return ~ty:Stype.unit ~return_kind:Single_value
            (resume ctx ~loc:loc_ break_cont arg))
  | Cexpr_continue { args; label; ty; loc_ } -> (
      match Label.Hash.find_opt ctx.loop_info label with
      | None ->
          let args = Lst.map args go_no_cps in
          Core.continue ~loc:loc_ args label ty
      | Some { loop_state; break_cont = _ } ->
          go_list args (fun args ->
              Core.return ~ty:Stype.unit ~return_kind:Single_value
                (Core.apply ~loc:loc_
                   ~kind:(Normal { func_ty = ctx.driver_ty })
                   ~ty:Stype.unit ctx.driver_id
                   [ make_state ctx loop_state args ])))
  | Cexpr_return { expr; return_kind; ty = _; loc_ } ->
      let* expr = go expr in
      let return_cont =
        match return_kind with
        | Single_value | Error_result { is_error = false; _ } -> ctx.return_cont
        | Error_result { is_error = true; _ } -> ctx.return_err_cont
      in
      Core.return ~ty:Stype.unit ~return_kind:Single_value
        (resume ctx ~loc:loc_ return_cont expr)
  | Cexpr_if { cond; ifso; ifnot; ty; loc_ } ->
      let* cond = go cond in
      if
        cont_is_simple k || need_cps ifso
        || match ifnot with None -> false | Some ifnot -> need_cps ifnot
      then
        let k = make_cont_simple ctx ~ty k in
        let ifso = go ifso k in
        let ifnot =
          match ifnot with
          | None -> resume ctx k (Core.unit ())
          | Some ifnot -> go ifnot k
        in
        Core.if_ ~loc:loc_ cond ~ifso ~ifnot
      else
        let ifso = go_no_cps ifso in
        let ifnot = Option.map go_no_cps ifnot in
        resume ctx k (Core.if_ ~loc:loc_ cond ~ifso ?ifnot)
  | Cexpr_switch_constr { obj; cases; default; ty; loc_ } ->
      let* obj = go obj in
      let need_cps =
        cont_is_simple k
        || Lst.exists cases (fun (_, _, action) -> need_cps action)
        || match default with None -> false | Some default -> need_cps default
      in
      if need_cps then
        let k = make_cont_simple ctx ~ty k in
        let cases =
          Lst.map cases (fun (tag, binder, action) ->
              (tag, binder, go action k))
        in
        let default =
          match default with
          | None -> None
          | Some default -> Some (go default k)
        in
        Core.switch_constr ~loc:loc_ obj cases ~default
      else
        let cases =
          Lst.map cases (fun (tag, binder, action) ->
              (tag, binder, go_no_cps action))
        in
        let default = Option.map go_no_cps default in
        resume ctx k (Core.switch_constr ~loc:loc_ obj cases ~default)
  | Cexpr_switch_constant { obj; cases; default; ty; loc_ } ->
      let* obj = go obj in
      let need_cps =
        cont_is_simple k
        || Lst.exists cases (fun (_, action) -> need_cps action)
        || need_cps default
      in
      if need_cps then
        let k = make_cont_simple ctx ~ty k in
        let cases = Lst.map cases (fun (c, action) -> (c, go action k)) in
        let default = go default k in
        Core.switch_constant ~loc:loc_ obj cases ~default
      else
        let cases = Lst.map cases (fun (c, action) -> (c, go_no_cps action)) in
        let default = go_no_cps default in
        resume ctx k (Core.switch_constant ~loc:loc_ obj cases ~default)
  | Cexpr_prim { prim = Pmake_value_or_error { tag }; args; ty = _ } -> (
      match[@warning "-fragile-match"] args with
      | arg :: [] ->
          if Basic_constr_info.equal tag Builtin.constr_ok.cs_tag then go arg k
          else go arg error_cont
      | _ -> assert false)
  | Cexpr_handle_error
      {
        obj =
          Cexpr_apply
            {
              func;
              args;
              kind = Normal _ as kind;
              ty_args_;
              prim;
              ty = apply_ty;
              loc_ = apply_loc_;
            } as obj;
        handle_kind;
        ty;
        loc_;
      } -> (
      let no_need_to_transform () =
        go_list args (fun args ->
            let apply =
              Core.apply ~loc:apply_loc_ ~ty:apply_ty ~ty_args_ ~prim ~kind func
                args
            in
            resume ctx k (Core.handle_error ~loc:loc_ ~ty apply handle_kind))
          [@@local]
      in
      match handle_kind with
      | To_result -> no_need_to_transform ()
      | Return_err _ -> transform_expr ctx obj ~error_cont:ctx.return_err_cont k
      | Joinapply join -> (
          match Ident.Hash.find_opt ctx.join_to_state join with
          | None -> no_need_to_transform ()
          | Some state_id ->
              transform_expr ctx obj ~error_cont:(Simple { state_id }) k))
  | Cexpr_handle_error { obj; handle_kind; ty } ->
      let k, error_cont =
        match handle_kind with
        | To_result ->
            let k = make_cont_simple ctx ~ty k in
            let make_cont (ok_or_err : Typedecl_info.constructor) =
              Complex
                (fun payload ->
                  resume ctx k (Core.constr ~ty ok_or_err.cs_tag [ payload ]))
            in
            (make_cont Builtin.constr_ok, make_cont Builtin.constr_err)
        | Return_err _ -> (k, ctx.return_err_cont)
        | Joinapply join -> (
            match Ident.Hash.find_opt ctx.join_to_state join with
            | Some state_id -> (k, Simple { state_id })
            | None -> (k, Complex (fun v -> Core.join_apply ~ty join [ v ])))
      in
      transform_expr ctx ~error_cont obj k
  | Cexpr_let { name; rhs; body; ty = _; loc_ } ->
      let* rhs = go rhs in
      Core.let_ ~loc:loc_ name rhs (go body k)
  | Cexpr_letfn { name; kind = (Rec | Nonrec) as kind; fn; body; ty = _; loc_ }
    ->
      let toplevel_id =
        Type_path.map_toplevel_id ctx.toplevel_id (fun id ->
            ((id ^ "." ^ Ident.to_string name : Stdlib.String.t) [@merlin.hide]))
      in
      Core.letfn ~loc:loc_ ~kind name
        (transform_fn ~global_env:ctx.global_env ~toplevel_id ctx.ty_params fn)
        (go body k)
  | Cexpr_letrec { bindings; body; ty = _; loc_ } ->
      let bindings =
        Lst.map bindings (fun (name, fn) ->
            let toplevel_id =
              Type_path.map_toplevel_id ctx.toplevel_id (fun id ->
                  ((id ^ "." ^ Ident.to_string name
                    : Stdlib.String.t)
                    [@merlin.hide]))
            in
            ( name,
              transform_fn ~global_env:ctx.global_env ~toplevel_id ctx.ty_params
                fn ))
      in
      Core.letrec ~loc:loc_ bindings (go body k)
  | Cexpr_sequence { exprs; last_expr; ty = _; loc_ } ->
      let rec loop exprs =
        match exprs with
        | [] -> go last_expr k
        | expr :: exprs ->
            let* expr = go expr in
            Core.sequence2 ~loc:loc_ expr (loop exprs)
      in
      loop exprs
  | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ -> resume ctx k expr
  | Cexpr_function { func; ty; loc_ } ->
      let toplevel_id =
        Type_path.map_toplevel_id ctx.toplevel_id (fun id ->
            ((id ^ ".lambda/" ^ Int.to_string (Basic_uuid.next ())
              : Stdlib.String.t)
              [@merlin.hide]))
      in
      let ({ params; body; is_async } : Core.fn) =
        transform_fn ~global_env:ctx.global_env ~toplevel_id ctx.ty_params func
      in
      resume ctx k (Core.function_ ~loc:loc_ ~ty ~is_async params body)
  | Cexpr_as { expr; trait; obj_type; loc_ } ->
      let* expr = go expr in
      resume ctx k (Core.as_ ~loc:loc_ ~trait ~obj_type expr)
  | Cexpr_assign { var; expr; ty = _; loc_ } ->
      let* expr = go expr in
      resume ctx k (Core.assign ~loc:loc_ var expr)
  | Cexpr_field { record; accessor; pos; ty; loc_ } ->
      let* record = go record in
      resume ctx k (Core.field ~loc:loc_ ~ty record accessor ~pos)
  | Cexpr_mutate { record; label; field; pos; ty = _; loc_ } ->
      let* record = go record in
      let* field = go field in
      resume ctx k (Core.mutate ~loc:loc_ record label field ~pos)
  | Cexpr_prim { prim; args; ty; loc_ } ->
      go_list args (fun args ->
          handle_error_if_necessary ctx k ~error_cont
            (Core.prim ~loc:loc_ ~ty prim args))
  | Cexpr_and { lhs; rhs; loc_ } ->
      let* lhs = go lhs in
      let* rhs = go rhs in
      resume ctx k (Core.and_ ~loc:loc_ lhs rhs)
  | Cexpr_or { lhs; rhs; loc_ } ->
      let* lhs = go lhs in
      let* rhs = go rhs in
      resume ctx k (Core.or_ ~loc:loc_ lhs rhs)
  | Cexpr_apply
      { kind = Normal _ as kind; func; args; ty; ty_args_; prim; loc_ } ->
      go_list args (fun args ->
          handle_error_if_necessary ctx k ~error_cont
            (Core.apply ~loc:loc_ ~kind ~ty ~ty_args_ ~prim func args))
  | Cexpr_constr { tag; args; ty; loc_ } ->
      go_list args (fun args ->
          resume ctx k (Core.constr ~loc:loc_ ~ty tag args))
  | Cexpr_tuple { exprs; ty; loc_ } ->
      go_list exprs (fun exprs -> resume ctx k (Core.tuple ~loc:loc_ ~ty exprs))
  | Cexpr_record { fields; ty; loc_ } ->
      go_list
        (Lst.map fields (fun field -> field.expr))
        (fun field_exprs ->
          let fields =
            Lst.map2 fields field_exprs (fun field ->
                fun expr -> { field with expr })
          in
          resume ctx k (Core.record ~loc:loc_ ~ty fields))
  | Cexpr_record_update { record; fields; fields_num; ty = _; loc_ } ->
      let* record = go record in
      go_list
        (Lst.map fields (fun field -> field.expr))
        (fun field_exprs ->
          let fields =
            Lst.map2 fields field_exprs (fun field ->
                fun expr -> { field with expr })
          in
          resume ctx k (Core.record_update ~loc:loc_ record fields fields_num))
  | Cexpr_array { exprs; ty; loc_ } ->
      go_list exprs (fun exprs -> resume ctx k (Core.array ~loc:loc_ ~ty exprs))

and transform_expr_list ctx (exprs : Core.expr list) ~error_cont k =
  let rec go processed_rev exprs k =
    match exprs with
    | [] -> k (List.rev processed_rev)
    | expr :: exprs when need_cps expr ->
        let name_for_processed =
          Lst.map processed_rev (fun (processed : Core.expr) ->
              match processed with
              | Cexpr_var { id = Pident _ | Pdot _ | Plocal_method _; _ }
              | Cexpr_unit _ | Cexpr_const _ ->
                  None
              | _ -> Some (Ident.fresh "*arg"))
        in
        let new_processed =
          Lst.map2 name_for_processed processed_rev (fun name_opt ->
              fun expr ->
               match name_opt with
               | Some id -> Core.var ~ty:(Core.type_of_expr expr) id
               | None -> expr)
        in
        let rest =
          let* expr = transform_expr ctx ~error_cont expr in
          go (expr :: new_processed) exprs k
        in
        Lst.fold_left2 name_for_processed processed_rev rest (fun name_opt ->
            fun expr ->
             fun rest ->
              match name_opt with
              | None -> rest
              | Some id -> Core.let_ id expr rest)
    | expr :: exprs ->
        let expr = transform_expr_no_cps ctx ~error_cont expr in
        go (expr :: processed_rev) exprs k
  in
  go [] exprs k

and transform_expr_no_cps ctx ~error_cont expr =
  transform_expr ctx expr ~error_cont Identity

and handle_error_if_necessary ctx k ~error_cont expr =
  match Stype.type_repr (Core.type_of_expr expr) with
  | T_constr
      { type_constructor = T_error_value_result; tys = [ ok_ty; err_ty ] } ->
      let err_join = Ident.fresh "*handle_err" in
      let join_param = Ident.fresh "*err" in
      Core.letfn ~kind:Nontail_join err_join
        {
          params =
            [ { binder = join_param; ty = err_ty; loc_ = Rloc.no_location } ];
          body =
            Core.return ~ty:Stype.unit ~return_kind:Single_value
              (resume ctx error_cont (Core.var join_param ~ty:err_ty));
          is_async = false;
        }
        (resume ctx k (Core.handle_error ~ty:ok_ty expr (Joinapply err_join)))
  | _ -> resume ctx k expr

and transform_nested_functions_in_expr ~global_env ~toplevel_id ty_params expr =
  let visitor =
    object (self)
      inherit [_] Core.Map.map as super

      method! visit_Cexpr_prim ctx prim args ty loc =
        match prim with
        | Prun_async -> (
            match[@warning "-fragile-match"] args with
            | async_fn :: [] -> (
                let async_fn = self#visit_expr ctx async_fn in
                let cont = id_cont Stype.unit in
                match async_fn with
                | Cexpr_function
                    {
                      func = { params = param :: []; body; is_async = _ };
                      ty = _;
                    } ->
                    Core.let_ param.binder cont body
                | _ ->
                    let func_ty = Core.type_of_expr async_fn in
                    Core.bind async_fn (fun async_fn ->
                        Core.apply ~loc
                          ~kind:(Normal { func_ty })
                          ~ty:Stype.unit async_fn [ cont ]))
            | _ -> assert false)
        | _ -> super#visit_Cexpr_prim ctx prim args ty loc

      method! visit_Cexpr_letfn (ty_params, toplevel_id) name fn body ty kind
          loc =
        let toplevel_id =
          Type_path.map_toplevel_id toplevel_id (fun id ->
              (id ^ "." ^ Ident.to_string name : Stdlib.String.t))
        in
        super#visit_Cexpr_letfn (ty_params, toplevel_id) name fn body ty kind
          loc

      method! visit_Cexpr_letrec (ty_params, toplevel_id) bindings body _ty loc
          =
        let bindings =
          Lst.map bindings (fun (name, fn) ->
              let toplevel_id =
                Type_path.map_toplevel_id toplevel_id (fun id ->
                    (id ^ "." ^ Ident.to_string name : Stdlib.String.t))
              in
              (name, self#visit_fn (ty_params, toplevel_id) fn))
        in
        let body = self#visit_expr (ty_params, toplevel_id) body in
        Core.letrec ~loc bindings body

      method! visit_Cexpr_function (ty_params, toplevel_id) func ty loc =
        let toplevel_id =
          Type_path.map_toplevel_id toplevel_id (fun id ->
              let suffix =
                if func.is_async then
                  ("/" ^ Int.to_string (Basic_uuid.next ()) : Stdlib.String.t)
                else ""
              in
              (id ^ ".lambda" ^ suffix : Stdlib.String.t))
        in
        super#visit_Cexpr_function (ty_params, toplevel_id) func ty loc

      method! visit_fn (ty_params, toplevel_id) fn =
        transform_fn ~global_env ~toplevel_id ty_params fn
    end
  in
  visitor#visit_expr (ty_params, toplevel_id) expr

and get_captures_of_loop ctx params body k =
  let captures =
    ref
      (Core_util.free_vars
         ~exclude:(Ident.Set.singleton ctx.driver_id)
         { params; body; is_async = false })
  in
  captures := add_captures_of_cont ctx !captures k;
  let visitor =
    object (self)
      inherit [_] Core.Iter.iter as super

      method! visit_Cexpr_return ((ctx, captures) as env) arg _kind _ty _loc_ =
        captures := add_captures_of_cont ctx !captures ctx.return_cont;
        self#visit_expr env arg

      method! visit_Cexpr_break ((ctx, captures) as env) arg label _ty _loc =
        (match arg with Some x -> self#visit_expr env x | _ -> ());
        match Label.Hash.find_opt ctx.loop_info label with
        | Some { break_cont; loop_state = _ } ->
            captures := add_captures_of_cont ctx !captures break_cont
        | _ -> ()

      method! visit_Cexpr_continue ((ctx, captures) as env) args label _ty _loc
          =
        Lst.iter args ~f:(self#visit_expr env);
        match Label.Hash.find_opt ctx.loop_info label with
        | Some { loop_state; break_cont = _ } ->
            captures :=
              add_captures_of_cont ctx !captures
                (Simple { state_id = loop_state })
        | _ -> ()

      method! visit_Cexpr_apply ((ctx, captures) as env) fn args kind ty
          ty_args_ prim loc =
        match kind with
        | Join -> (
            Lst.iter args ~f:(self#visit_expr env);
            match Ident.Hash.find_opt ctx.join_to_state fn with
            | Some state_id ->
                captures :=
                  add_captures_of_cont ctx !captures (Simple { state_id })
            | _ -> ())
        | Normal _ | Async _ ->
            super#visit_Cexpr_apply env fn args kind ty ty_args_ prim loc

      method! visit_Cexpr_handle_error ((ctx, captures) as env) obj kind _ty
          _loc =
        self#visit_expr env obj;
        match kind with
        | To_result -> ()
        | Return_err _ ->
            captures := add_captures_of_cont ctx !captures ctx.return_err_cont
        | Joinapply join -> (
            match Ident.Hash.find_opt ctx.join_to_state join with
            | Some state_id ->
                captures :=
                  add_captures_of_cont ctx !captures (Simple { state_id })
            | _ -> ())

      method! visit_fn _ _fn = ()
    end
  in
  visitor#visit_expr (ctx, captures) body;
  !captures

and transform_fn ~global_env ~toplevel_id ty_params (fn : Core.fn) =
  if fn.is_async then (
    let return_cont, return_err_cont, (cont_params : Core.param list) =
      let cont_param = Ident.fresh "*cont" in
      match Stype.type_repr (Core.type_of_expr fn.body) with
      | T_constr
          { type_constructor = T_error_value_result; tys = [ ok_ty; err_ty ] }
        ->
          let cont_ty =
            Builtin.type_arrow [ ok_ty ] Stype.unit ~err_ty:None ~is_async:false
          in
          let err_cont_param = Ident.fresh "*err_cont" in
          let err_cont_ty =
            Builtin.type_arrow [ err_ty ] Stype.unit ~err_ty:None
              ~is_async:false
          in
          ( Return { cont = cont_param; cont_ty },
            Return { cont = err_cont_param; cont_ty = err_cont_ty },
            [
              { binder = cont_param; ty = cont_ty; loc_ = Rloc.no_location };
              {
                binder = err_cont_param;
                ty = err_cont_ty;
                loc_ = Rloc.no_location;
              };
            ] )
      | ty ->
          let cont_ty =
            Builtin.type_arrow [ ty ] Stype.unit ~err_ty:None ~is_async:false
          in
          ( Return { cont = cont_param; cont_ty },
            Complex (fun _ -> assert false),
            [ { binder = cont_param; ty = cont_ty; loc_ = Rloc.no_location } ]
          )
    in
    let state_ty_name = "State" in
    let state_ty_path = Type_path.local_type toplevel_id state_ty_name in
    let tys = Tvar_env.get_types ty_params in
    let state_ty : Stype.t =
      T_constr
        {
          type_constructor = state_ty_path;
          tys;
          generic_ = not (Tvar_env.is_empty ty_params);
          is_suberror_ = false;
        }
    in
    let driver_id = Ident.fresh "*async_driver" in
    let ctx =
      {
        join_to_state = Ident.Hash.create 7;
        loop_info = Label.Hash.create 7;
        return_cont;
        return_err_cont;
        state_ty;
        states = Vec.empty ();
        ty_params;
        driver_id;
        driver_ty =
          Builtin.type_arrow [ state_ty ] Stype.unit ~err_ty:None
            ~is_async:false;
        cont_params;
        toplevel_id;
        global_env;
      }
    in
    let body =
      transform_expr ctx fn.body ~error_cont:return_err_cont ctx.return_cont
    in
    let params = fn.params @ cont_params in
    let state_param = Ident.fresh "*state" in
    let state_constrs =
      Vec.map_into_list ctx.states ~unorder:(fun state ->
          (let { state_id; state_name; params; captures; body = _ } = state in
           let args =
             Lst.map_append params (Lst.map captures snd) (fun p -> p.ty)
           in
           {
             constr_name = state_name;
             cs_args = args;
             cs_res = state_ty;
             cs_tag =
               Constr_tag_regular
                 {
                   total =
                     Constr_info.Index_set.singleton 0
                       (Vec.length ctx.states - 1);
                   index = state_id;
                   name_ = state_name;
                   repr_ = Non_constant;
                 };
             cs_vis = Read_write;
             cs_ty_params_ = ty_params;
             cs_arity_ = Fn_arity.simple (List.length args);
             cs_constr_loc_ = Loc.no_location;
             cs_loc_ = Loc.no_location;
           }
            : Typedecl_info.constructor))
    in
    Typing_info.add_type
      (Global_env.get_toplevel_types global_env)
      (Local_type.mangle_name toplevel_id state_ty_name)
      (Local_type.to_generic_typedecl_info Loc.no_location
         {
           name = state_ty_name;
           toplevel_id;
           kind = Enum state_constrs;
           loc_ = Rloc.no_location;
           is_only_tag_enum = false;
           ty_params_ = ty_params;
         });
    let body =
      let branches =
        Vec.map_into_list ctx.states
          ~unorder:(fun { state_id; state_name; params; captures; body } ->
            let tag : Basic_constr_info.constr_tag =
              Constr_tag_regular
                {
                  total =
                    Constr_info.Index_set.singleton 0 (Vec.length ctx.states - 1);
                  index = state_id;
                  repr_ = Non_constant;
                  name_ = state_name;
                }
            in
            let renaming = Ident.Hash.create 17 in
            let capture_params =
              Lst.map captures (fun (capture, ty) ->
                  let capture' = Ident.rename capture in
                  Ident.Hash.add renaming capture capture';
                  (capture', ty))
            in
            match
              Lst.map_append params capture_params (fun p -> (p.binder, p.ty))
            with
            | [] -> (tag, None, body)
            | params ->
                let binder =
                  Ident.fresh
                    (("*" ^ state_name : Stdlib.String.t) [@merlin.hide])
                in
                let state_obj =
                  Core.var ~ty:(Type.make_constr_type state_ty ~tag) binder
                in
                let body = rename_visitor#visit_expr renaming body in
                let body =
                  Lst.fold_left_with_offset params body 0 (fun (param, ty) ->
                      fun body ->
                       fun index ->
                        Core.let_ param
                          (Core.prim ~ty
                             (Penum_field { index; tag })
                             [ state_obj ])
                          body)
                in
                (tag, Some binder, body))
      in
      match branches with
      | [] -> body
      | branches ->
          Core.letfn ~kind:Rec driver_id
            {
              is_async = false;
              params =
                [
                  {
                    binder = state_param;
                    ty = state_ty;
                    loc_ = Rloc.no_location;
                  };
                ];
              body =
                Core.switch_constr
                  (Core.var state_param ~ty:state_ty)
                  branches ~default:None;
            }
            body
    in
    { is_async = false; params; body })
  else
    {
      fn with
      body =
        transform_nested_functions_in_expr ~global_env ~toplevel_id ty_params
          fn.body;
    }

let eliminate_async ~global_env (prog : Core.program) =
  Lst.map prog (fun top ->
      (match top with
       | Ctop_expr top_expr ->
           Ctop_expr
             {
               top_expr with
               expr =
                 transform_nested_functions_in_expr ~global_env
                   ~toplevel_id:
                     (T_regular
                        { pkg = !Basic_config.current_package; name = "*init" })
                   Tvar_env.empty top_expr.expr;
             }
       | Ctop_let top_let -> (
           match[@warning "-fragile-match"] top_let.binder with
           | Pdot qual_name ->
               Ctop_let
                 {
                   top_let with
                   expr =
                     transform_nested_functions_in_expr ~global_env
                       ~toplevel_id:(Qual_ident.to_toplevel_id qual_name)
                       Tvar_env.empty top_let.expr;
                 }
           | _ -> assert false)
       | Ctop_fn ({ func; subtops; ty_params_; binder; _ } as top_fn) -> (
           assert (subtops = []);
           match[@warning "-fragile-match"] binder with
           | Pdot qual_name ->
               Ctop_fn
                 {
                   top_fn with
                   func =
                     transform_fn ~global_env
                       ~toplevel_id:(Qual_ident.to_toplevel_id qual_name)
                       ty_params_ func;
                 }
           | _ -> assert false)
       | Ctop_stub _ -> top
        : Core.top_item))
