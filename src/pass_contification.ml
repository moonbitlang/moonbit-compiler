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
module Ident = Basic_core_ident

type contify_result =
  | Never_called
  | Contifiable
  | Not_contifiable of { tail_called : bool }

let is_contifiable (ident : Ident.t) (expr : Core.expr) ~treat_return_as_tail =
  let has_other_occurence = ref false in
  let tail_called = ref false in
  let rec analyze_usage (expr : Core.expr) ~is_tail ~in_nested_call =
    let got expr = analyze_usage expr ~is_tail ~in_nested_call [@@inline] in
    let gon expr =
      analyze_usage expr ~is_tail:false ~in_nested_call
        [@@inline]
    in
    if (not !has_other_occurence) || not !tail_called then
      match expr with
      | Cexpr_apply { func; args; _ } when Ident.equal func ident ->
          if is_tail then tail_called := true else has_other_occurence := true;
          Lst.iter args ~f:gon
      | Cexpr_let { name = _; rhs; body; _ } ->
          gon rhs;
          got body
      | Cexpr_letrec { bindings; body; _ } ->
          Lst.iter bindings ~f:(fun (_, fn) ->
              analyze_usage ~in_nested_call:true ~is_tail:false fn.body);
          got body
      | Cexpr_letfn { name = _; fn; body; kind; _ } -> (
          match kind with
          | Tail_join | Nontail_join ->
              got fn.body;
              got body
          | Nonrec | Rec ->
              analyze_usage ~in_nested_call:true ~is_tail:false fn.body;
              got body)
      | Cexpr_function { func; _ } ->
          analyze_usage ~in_nested_call:true ~is_tail:false func.body
      | Cexpr_if { cond; ifso; ifnot; _ } -> (
          gon cond;
          got ifso;
          match ifnot with None -> () | Some e -> got e)
      | Cexpr_sequence { exprs; last_expr; _ } ->
          Lst.iter exprs ~f:gon;
          got last_expr
      | Cexpr_switch_constr { obj; cases; default; _ } -> (
          gon obj;
          Lst.iter cases ~f:(fun (_, _, expr) -> got expr);
          match default with None -> () | Some default -> got default)
      | Cexpr_switch_constant { obj; cases; default; _ } ->
          gon obj;
          Lst.iter cases ~f:(fun (_, expr) -> got expr);
          got default
      | Cexpr_and { lhs; rhs; _ } | Cexpr_or { lhs; rhs; _ } ->
          gon lhs;
          got rhs
      | Cexpr_loop { body; args; _ } ->
          Lst.iter args ~f:gon;
          got body
      | Cexpr_return { expr; _ } ->
          if treat_return_as_tail && not in_nested_call then
            analyze_usage expr ~is_tail:true ~in_nested_call:false
          else gon expr
      | Cexpr_break { arg; _ } -> (
          match arg with None -> () | Some arg -> gon arg)
      | Cexpr_tuple { exprs = args; _ }
      | Cexpr_constr { args; _ }
      | Cexpr_array { exprs = args; _ }
      | Cexpr_continue { args; _ }
      | Cexpr_prim { args; _ }
      | Cexpr_apply { args; _ } ->
          Lst.iter args ~f:gon
      | Cexpr_record { fields; _ } ->
          Lst.iter fields ~f:(fun def -> gon def.expr)
      | Cexpr_record_update { record; fields; _ } ->
          gon record;
          Lst.iter fields ~f:(fun def -> gon def.expr)
      | Cexpr_field { record = expr; _ }
      | Cexpr_assign { expr; _ }
      | Cexpr_as { expr; _ }
      | Cexpr_handle_error { obj = expr; _ } ->
          gon expr
      | Cexpr_mutate { record; field; _ } ->
          gon record;
          gon field
      | Cexpr_const _ | Cexpr_unit _ -> ()
      | Cexpr_var { id; _ } ->
          if Ident.equal id ident then has_other_occurence := true
  in
  analyze_usage expr ~is_tail:true ~in_nested_call:false;
  if !has_other_occurence then Not_contifiable { tail_called = !tail_called }
  else if !tail_called then Contifiable
  else Never_called

let apply2joinapply ident join body =
  let map =
    object (self)
      inherit [_] Core.Map.map as super

      method! visit_Cexpr_apply () func args kind ty ty_args_ prim loc_ =
        match kind with
        | Normal _ when Ident.equal func ident ->
            Core.apply ~loc:loc_ ~ty_args_ ~kind:Join ~ty ~prim join
              (Lst.map args (self#visit_expr ()))
        | _ -> super#visit_Cexpr_apply () func args kind ty ty_args_ prim loc_
    end
  in
  map#visit_expr () body

let tail_apply2continue ident label (expr : Core.expr) =
  let rec go (expr : Core.expr) ~is_tail =
    let got expr = go expr ~is_tail [@@inline] in
    let gon expr = go expr ~is_tail:false [@@inline] in
    match expr with
    | Cexpr_apply { func; args; ty; loc_; kind; prim; ty_args_ } ->
        if is_tail && Ident.equal func ident then
          Core.continue ~loc:loc_ (Lst.map args gon) label ty
        else
          Core.apply ~loc:loc_ ~ty_args_ ~kind ~ty ~prim func (Lst.map args gon)
    | Cexpr_let { name; rhs; body; ty = _; loc_ } ->
        Core.let_ ~loc:loc_ name (gon rhs) (got body)
    | Cexpr_letrec { bindings; body; ty = _; loc_ } ->
        Core.letrec ~loc:loc_ bindings (got body)
    | Cexpr_letfn { name; fn; body; kind; ty = _; loc_ } -> (
        match kind with
        | Tail_join | Nontail_join ->
            Core.letfn ~loc:loc_ ~kind name
              { fn with body = got fn.body }
              (got body)
        | Nonrec | Rec -> Core.letfn ~loc:loc_ ~kind name fn (got body))
    | Cexpr_if { cond; ifso; ifnot; ty = _; loc_ } ->
        Core.if_ ~loc:loc_ (gon cond) ~ifso:(got ifso)
          ?ifnot:(Option.map got ifnot)
    | Cexpr_sequence { exprs; last_expr; ty = _; loc_ } ->
        Core.sequence ~loc:loc_ (Lst.map exprs gon) (got last_expr)
    | Cexpr_switch_constr { obj; cases; default; ty = _; loc_ } ->
        Core.switch_constr ~loc:loc_ ~default:(Option.map got default) (gon obj)
          (Lst.map cases (fun (tag, binder, action) ->
               (tag, binder, got action)))
    | Cexpr_switch_constant { obj; cases; default; ty = _; loc_ } ->
        Core.switch_constant ~loc:loc_ ~default:(got default) (gon obj)
          (Lst.map cases (fun (c, action) -> (c, got action)))
    | Cexpr_and { lhs; rhs; loc_ } -> Core.and_ ~loc:loc_ (gon lhs) (got rhs)
    | Cexpr_or { lhs; rhs; loc_ } -> Core.or_ ~loc:loc_ (gon lhs) (got rhs)
    | Cexpr_loop { params; body; args; label; ty = _; loc_ } ->
        Core.loop ~loc:loc_ params (got body) (Lst.map args gon) label
    | Cexpr_return { expr; return_kind; ty; loc_ } ->
        Core.return ~loc:loc_ ~return_kind ~ty (go expr ~is_tail:true)
    | Cexpr_break { arg; loc_; label; ty } ->
        Core.break ~loc_ (Option.map got arg) label ty
    | Cexpr_tuple { exprs; loc_; ty } ->
        Core.tuple ~loc:loc_ ~ty (Lst.map exprs gon)
    | Cexpr_constr { tag; args; loc_; ty } ->
        Core.constr ~loc:loc_ ~ty tag (Lst.map args gon)
    | Cexpr_record { fields; loc_; ty } ->
        Core.record ~loc:loc_ ~ty
          (Lst.map fields (fun def -> { def with expr = gon def.expr }))
    | Cexpr_record_update { record; fields; fields_num; loc_; ty = _ } ->
        Core.record_update ~loc:loc_ record
          (Lst.map fields (fun def -> { def with expr = gon def.expr }))
          fields_num
    | Cexpr_field { record; accessor; pos; loc_; ty } ->
        Core.field ~loc:loc_ ~ty ~pos (gon record) accessor
    | Cexpr_mutate { record; label; field; pos; loc_; ty = _ } ->
        Core.mutate ~loc:loc_ ~pos (gon record) label (gon field)
    | Cexpr_array { exprs; loc_; ty } ->
        Core.array ~loc:loc_ ~ty (Lst.map exprs gon)
    | Cexpr_assign { var; expr; loc_; ty = _ } ->
        Core.assign ~loc:loc_ var (gon expr)
    | Cexpr_continue { args; loc_; label; ty } ->
        Core.continue ~loc:loc_ args label ty
    | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ | Cexpr_function _ -> expr
    | Cexpr_prim { prim; args; ty; loc_ } ->
        Core.prim ~loc:loc_ ~ty prim (Lst.map args gon)
    | Cexpr_as { expr; trait; obj_type; loc_ } ->
        Core.as_ ~loc:loc_ (gon expr) ~trait ~obj_type
    | Cexpr_handle_error { obj; handle_kind; ty; loc_ } ->
        Core.handle_error ~loc:loc_ (gon obj) handle_kind ~ty
  in
  go expr ~is_tail:true

let loopify (ident : Ident.t) (params : Core.param list) (body : Core.expr)
    ~is_async =
  (let label = Label.fresh (Ident.base_name ident) in
   let fresh_params =
     Lst.map params (fun p -> { p with binder = Ident.rename p.binder })
   in
   let args =
     Lst.map fresh_params (fun { binder = id; ty; loc_ } ->
         Core.var ~loc:loc_ ~ty id)
   in
   {
     params = fresh_params;
     body =
       Core.loop ~loc:(Core.loc_of_expr body) params
         (tail_apply2continue ident label body)
         args label;
     is_async;
   }
    : Core.fn)

let transform_return (fn : Core.fn) =
  { fn with body = Core_util.transform_return_in_fn_body fn.body }

let contifier =
  (object (self)
     inherit [_] Core.Map.map as super

     method! visit_Ctop_fn () ({ binder; func; _ } as decl) =
       match is_contifiable binder func.body ~treat_return_as_tail:true with
       | Contifiable | Not_contifiable { tail_called = true } ->
           super#visit_Ctop_fn ()
             {
               decl with
               func =
                 loopify binder func.params func.body ~is_async:func.is_async;
             }
       | Never_called | Not_contifiable { tail_called = false } ->
           super#visit_Ctop_fn () decl

     method! visit_Cexpr_letfn () name fn body _ty kind loc_ =
       let fn = self#visit_fn () fn in
       let body = self#visit_expr () body in
       match kind with
       | Nonrec -> (
           match is_contifiable name body ~treat_return_as_tail:false with
           | Contifiable ->
               let body = apply2joinapply name name body in
               Core.joinlet_tail ~loc:loc_ name fn.params
                 (Core_util.transform_return_in_fn_body fn.body)
                 body
           | Never_called -> body
           | Not_contifiable _ -> Core.letfn ~loc:loc_ ~kind name fn body)
       | Rec -> (
           match is_contifiable name fn.body ~treat_return_as_tail:true with
           | Contifiable -> (
               match is_contifiable name body ~treat_return_as_tail:false with
               | Contifiable ->
                   let fn =
                     transform_return
                       (loopify name fn.params fn.body ~is_async:fn.is_async)
                   in
                   let body = apply2joinapply name name body in
                   Core.letfn ~loc:loc_ ~kind:Tail_join name fn body
               | Never_called -> body
               | Not_contifiable _ ->
                   Core.letfn ~loc:loc_ ~kind:Nonrec name
                     (loopify name fn.params fn.body ~is_async:fn.is_async)
                     body)
           | Not_contifiable { tail_called = true } ->
               Core.letfn ~loc:loc_ ~kind:Rec name
                 (loopify name fn.params fn.body ~is_async:fn.is_async)
                 body
           | Never_called -> Core.letfn ~loc:loc_ ~kind:Nonrec name fn body
           | Not_contifiable { tail_called = false } ->
               Core.letfn ~loc:loc_ ~kind name fn body)
       | Tail_join | Nontail_join -> Core.letfn ~loc:loc_ ~kind name fn body
   end
    : < visit_top_item : unit -> Core.top_item -> Core.top_item ; .. >)

let contify (prog : Core.program) =
  Lst.map prog (fun top_item -> contifier#visit_top_item () top_item)
