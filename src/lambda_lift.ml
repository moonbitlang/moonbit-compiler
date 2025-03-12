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
module Map = Ident.Map
module Lst = Basic_lst
module Type_path = Basic_type_path

type lift_to_top = Subtop | Toplevel of { name_hint : string }

type lifter_context = {
  lift_to_top : lift_to_top;
  subtops : Core.subtop_fun_decl Vec.t;
  rename_table : Ident.t Ident.Map.t;
  exclude : Ident.Set.t;
  non_well_knowns : Ident.Hashset.t;
  convert_info : (Ident.t * Stype.t) Ident.Map.t;
}

type capture_info =
  | Single of Ident.t * Stype.t
  | Multiple of (Ident.t * Stype.t) list

type convert_context = {
  env_ty : Stype.t;
  captures : capture_info;
  self_binders : Ident.t list;
}

let free_vars ctx ~exclude fn =
  let fvs = Map.bindings (Core_util.free_vars ~exclude fn) in
  let dedup_map = Ident.Hash.create 17 in
  Lst.iter fvs ~f:(fun ((id, _ty) as v) ->
      let id, ty = Ident.Map.find_default ctx.convert_info id v in
      if not (Ident.Hash.mem dedup_map id || Ident.Set.mem exclude id) then
        Ident.Hash.add dedup_map id ty);
  Ident.Hash.to_list dedup_map

let convert_fn (ctx : lifter_context) (fn : Core.fn)
    (convert_ctx : convert_context)
    ~(visit_expr : lifter_context -> Core.expr -> Core.expr) =
  let { env_ty; captures; self_binders } = convert_ctx in
  let env_binder =
    match captures with
    | Single (cap_id, _) -> Ident.rename cap_id
    | Multiple _ -> Ident.fresh "*env"
  in
  let env : Core.param =
    { binder = env_binder; ty = env_ty; loc_ = Rloc.no_location }
  in
  let env_var = Core.var env_binder ~ty:env_ty in
  let new_rename_table =
    match captures with
    | Single (cap_id, _) -> Ident.Map.add ctx.rename_table cap_id env_binder
    | Multiple captures ->
        Lst.fold_left captures ctx.rename_table (fun acc ->
            fun (cap_id, _) ->
             let new_cap_id = Ident.rename cap_id in
             Ident.Map.add acc cap_id new_cap_id)
  in
  let new_convert_info =
    Lst.fold_left self_binders ctx.convert_info (fun acc ->
        fun self_binder -> Ident.Map.add acc self_binder (env_binder, env_ty))
  in
  let new_ctx =
    {
      ctx with
      rename_table = new_rename_table;
      convert_info = new_convert_info;
    }
  in
  let body = visit_expr new_ctx fn.body in
  let body =
    match captures with
    | Single _ -> body
    | Multiple captures ->
        Lst.fold_left_with_offset captures body 0 (fun (cap_id, cap_ty) ->
            fun acc ->
             fun i ->
              let pos =
                Parsing_syntax.Index
                  { tuple_index = i; loc_ = Rloc.no_location }
              in
              let rhs = Core.field ~ty:cap_ty ~pos:i env_var pos in
              let new_cap_id = Ident.Map.find_exn new_rename_table cap_id in
              Core.let_ new_cap_id rhs acc)
  in
  ({ params = env :: fn.params; body; is_async = fn.is_async } : Core.fn)

let add_subtop (ctx : lifter_context) binder fn
    ~(convert_ctx : convert_context option)
    ~(visit_expr : lifter_context -> Core.expr -> Core.expr) =
  let new_exclude =
    match convert_ctx with
    | Some _ -> ctx.exclude
    | None -> Ident.Set.add ctx.exclude binder
  in
  let visit_fn ctx fn =
    match convert_ctx with
    | Some convert_ctx -> convert_fn ctx fn convert_ctx ~visit_expr
    | None -> { fn with body = visit_expr ctx fn.body }
      [@@inline]
  in
  match ctx.lift_to_top with
  | Subtop ->
      let new_ctx = { ctx with exclude = new_exclude } in
      let fn = visit_fn ctx fn in
      Vec.push new_ctx.subtops { binder; fn };
      new_ctx
  | Toplevel { name_hint } ->
      let new_binder = Ident.make_global_binder ~name_hint binder in
      let new_rename_table = Ident.Map.add ctx.rename_table binder new_binder in
      let new_ctx =
        { ctx with rename_table = new_rename_table; exclude = new_exclude }
      in
      let fn = visit_fn new_ctx fn in
      let subtop : Core.subtop_fun_decl = { binder = new_binder; fn } in
      Vec.push new_ctx.subtops subtop;
      new_ctx

let add_subtops ctx bindings ~(convert_ctx : convert_context option)
    ~(visit_expr : lifter_context -> Core.expr -> Core.expr) =
  let new_exclude =
    Lst.fold_left bindings ctx.exclude (fun acc ->
        fun (binder, _) -> Ident.Set.add acc binder)
  in
  let visit_fn ctx fn =
    match convert_ctx with
    | Some convert_ctx -> convert_fn ctx fn convert_ctx ~visit_expr
    | None -> { fn with body = visit_expr ctx fn.body }
      [@@inline]
  in
  match ctx.lift_to_top with
  | Subtop ->
      let new_ctx = { ctx with exclude = new_exclude } in
      Lst.iter bindings ~f:(fun (binder, fn) ->
          let fn = visit_fn new_ctx fn in
          Vec.push new_ctx.subtops { binder; fn });
      new_ctx
  | Toplevel { name_hint } ->
      let new_rename_table =
        Lst.fold_left bindings ctx.rename_table (fun acc ->
            fun (binder, _) ->
             let new_binder = Ident.make_global_binder ~name_hint binder in
             Ident.Map.add acc binder new_binder)
      in
      let new_ctx =
        { ctx with rename_table = new_rename_table; exclude = new_exclude }
      in
      Lst.iter bindings ~f:(fun (binder, fn) ->
          let new_binder = Ident.Map.find_exn new_rename_table binder in
          let fn = visit_fn new_ctx fn in
          let subtop : Core.subtop_fun_decl = { binder = new_binder; fn } in
          Vec.push ctx.subtops subtop);
      new_ctx

let lift_well_known (ctx : lifter_context) ~(kind : Core.letfn_kind)
    ~(freevars : (Ident.t * Stype.t) list) ~(fns : (Ident.t * Core.fn) list)
    ~(body : Core.expr) ~loc
    ~(visit_expr : lifter_context -> Core.expr -> Core.expr) =
  let binders = Lst.map fns fst in
  let self_binders = if kind = Rec then binders else [] in
  match freevars with
  | (capture, capture_ty) :: [] ->
      let renamed_capture =
        Ident.Map.find_default ctx.rename_table capture capture
      in
      let convert_ctx =
        {
          env_ty = capture_ty;
          captures = Single (capture, capture_ty);
          self_binders;
        }
      in
      let new_ctx =
        match fns with
        | [] -> assert false
        | (binder, fn) :: [] ->
            add_subtop ctx binder ~convert_ctx:(Some convert_ctx) fn ~visit_expr
        | fns -> add_subtops ctx fns ~convert_ctx:(Some convert_ctx) ~visit_expr
      in
      let new_convert_info =
        Lst.fold_left binders new_ctx.convert_info (fun acc ->
            fun binder -> Ident.Map.add acc binder (renamed_capture, capture_ty))
      in
      let new_ctx_with_convert_info =
        { new_ctx with convert_info = new_convert_info }
      in
      visit_expr new_ctx_with_convert_info body
  | captures ->
      let captures_len = List.length captures in
      let renamed_captures =
        Lst.map captures (fun (capture, capture_ty) ->
            (Ident.Map.find_default ctx.rename_table capture capture, capture_ty))
      in
      let tuple_ty =
        Stype.T_constr
          {
            type_constructor = Type_path.tuple captures_len;
            tys = Lst.map captures snd;
            generic_ = false;
            is_suberror_ = false;
          }
      in
      let convert_ctx =
        { env_ty = tuple_ty; captures = Multiple captures; self_binders }
      in
      let new_ctx =
        match fns with
        | [] -> assert false
        | (binder, fn) :: [] ->
            add_subtop ctx binder ~convert_ctx:(Some convert_ctx) fn ~visit_expr
        | fns -> add_subtops ctx fns ~convert_ctx:(Some convert_ctx) ~visit_expr
      in
      let tuple =
        Core.tuple ~ty:tuple_ty
          (Lst.map renamed_captures (fun (c, ty) -> Core.var c ~ty))
      in
      let tuple_binder = Ident.fresh "*env" in
      let new_convert_info =
        Lst.fold_left binders new_ctx.convert_info (fun acc ->
            fun binder -> Ident.Map.add acc binder (tuple_binder, tuple_ty))
      in
      let new_ctx_with_convert_info =
        { new_ctx with convert_info = new_convert_info }
      in
      let body = visit_expr new_ctx_with_convert_info body in
      Core.let_ tuple_binder tuple body ~loc

let lifter =
  object (self)
    inherit [_] Core.Map.map as super

    method! visit_Cexpr_let ctx binder rhs body ty loc =
      match (binder, rhs) with
      | Pident _, Cexpr_function { func; is_raw_ = false; _ } ->
          let freevars = free_vars ctx ~exclude:ctx.exclude func in
          if freevars = [] then
            let new_ctx =
              add_subtop ctx binder func ~convert_ctx:None
                ~visit_expr:self#visit_expr
            in
            self#visit_expr new_ctx body
          else if not (Ident.Hashset.mem ctx.non_well_knowns binder) then
            lift_well_known ctx ~kind:Nonrec ~freevars
              ~fns:[ (binder, func) ]
              ~body ~loc ~visit_expr:self#visit_expr
          else super#visit_Cexpr_let ctx binder rhs body ty loc
      | Pdot _, _ | Plocal_method _, _ | Pident _, _ | Pmutable_ident _, _ ->
          super#visit_Cexpr_let ctx binder rhs body ty loc

    method! visit_Cexpr_letfn ctx binder fn body ty kind loc =
      match kind with
      | Nonrec | Rec ->
          let freevars =
            free_vars ctx ~exclude:(Ident.Set.add ctx.exclude binder) fn
          in
          if freevars = [] then
            let new_ctx =
              add_subtop ctx binder fn ~convert_ctx:None
                ~visit_expr:self#visit_expr
            in
            self#visit_expr new_ctx body
          else if not (Ident.Hashset.mem ctx.non_well_knowns binder) then
            lift_well_known ctx ~kind ~freevars
              ~fns:[ (binder, fn) ]
              ~body ~loc ~visit_expr:self#visit_expr
          else super#visit_Cexpr_letfn ctx binder fn body ty kind loc
      | Tail_join | Nontail_join ->
          super#visit_Cexpr_letfn ctx binder fn body ty kind loc

    method! visit_Cexpr_letrec ctx bindings body ty loc =
      let exclude =
        Lst.fold_left bindings ctx.exclude (fun acc ->
            fun (binder, _) -> Ident.Set.add acc binder)
      in
      let free_vars, _ =
        Lst.fold_right bindings ([], exclude) (fun (_, fn) ->
            fun (acc, exclude) ->
             let new_free_vars = free_vars ctx ~exclude fn in
             ( new_free_vars @ acc,
               Ident.Set.add_list exclude (Lst.map new_free_vars fst) ))
      in
      if free_vars = [] then
        let new_ctx =
          add_subtops ctx bindings ~visit_expr:self#visit_expr ~convert_ctx:None
        in
        self#visit_expr new_ctx body
      else if
        Lst.for_all bindings (fun (binder, _) ->
            not (Ident.Hashset.mem ctx.non_well_knowns binder))
      then
        lift_well_known ctx ~kind:Rec ~freevars:free_vars ~fns:bindings ~body
          ~loc ~visit_expr:self#visit_expr
      else super#visit_Cexpr_letrec ctx bindings body ty loc

    method! visit_var ctx var = Ident.Map.find_default ctx.rename_table var var

    method! visit_Cexpr_apply ctx func args kind ty ty_args_ prim loc =
      match kind with
      | Join -> super#visit_Cexpr_apply ctx func args kind ty ty_args_ prim loc
      | Normal _ | Async _ ->
          let new_func = Ident.Map.find_default ctx.rename_table func func in
          let args = Lst.map args (self#visit_expr ctx) in
          let new_args =
            match Ident.Map.find_opt ctx.convert_info func with
            | Some (env_id, env_ty) ->
                let actual_env_id =
                  Ident.Map.find_default ctx.rename_table env_id env_id
                in
                Core.var ~ty:env_ty actual_env_id :: args
            | None -> args
          in
          Core.apply ~loc ~ty ~ty_args_ ~prim ~kind new_func new_args
  end

let non_well_known_collector =
  object
    inherit [_] Core.Iter.iter

    method! visit_Cexpr_var (ctx : Ident.Hashset.t) id _ty _ty_args _prim _loc =
      Ident.Hashset.add ctx id
  end

let lift_expr ~lift_to_top (expr : Core.expr) =
  (let non_well_knowns = Ident.Hashset.create 17 in
   non_well_known_collector#visit_expr non_well_knowns expr;
   let ctx =
     {
       lift_to_top;
       subtops = Vec.empty ();
       rename_table = Ident.Map.empty;
       exclude = Ident.Set.empty;
       non_well_knowns;
       convert_info = Ident.Map.empty;
     }
   in
   let expr = lifter#visit_expr ctx expr in
   (expr, ctx.subtops)
    : Core.expr * Core.subtop_fun_decl Vec.t)

let subtop_to_top ({ binder; fn } : Core.subtop_fun_decl) ~loc_ =
  (Ctop_fn
     {
       binder;
       func = fn;
       subtops = [];
       ty_params_ = Tvar_env.empty;
       is_pub_ = false;
       loc_;
     }
    : Core.top_item)

let lift_item (acc : Core.top_item Vec.t) (item : Core.top_item) =
  match item with
  | Ctop_expr { expr; is_main; loc_ } ->
      let expr, subtops =
        lift_expr expr ~lift_to_top:(Toplevel { name_hint = "*init*" })
      in
      Vec.push acc (Ctop_expr { expr; is_main; loc_ });
      Vec.iter subtops (fun subtop -> Vec.push acc (subtop_to_top subtop ~loc_))
  | Ctop_let { binder; expr; is_pub_; loc_ } ->
      let expr, subtops =
        lift_expr expr
          ~lift_to_top:(Toplevel { name_hint = Ident.base_name binder })
      in
      Vec.push acc (Ctop_let { binder; expr; is_pub_; loc_ });
      Vec.iter subtops (fun subtop -> Vec.push acc (subtop_to_top subtop ~loc_))
  | Ctop_fn { binder; func; subtops = _; ty_params_; is_pub_; loc_ } ->
      if Tvar_env.is_empty ty_params_ then (
        let expr, subtops =
          lift_expr func.body
            ~lift_to_top:(Toplevel { name_hint = Ident.base_name binder })
        in
        Vec.iter subtops (fun subtop ->
            Vec.push acc (subtop_to_top subtop ~loc_));
        Vec.push acc
          (Ctop_fn
             {
               binder;
               func = { func with body = expr };
               subtops = [];
               ty_params_;
               is_pub_;
               loc_;
             }))
      else
        let expr, subtops = lift_expr func.body ~lift_to_top:Subtop in
        Vec.push acc
          (Ctop_fn
             {
               binder;
               func = { func with body = expr };
               subtops = Vec.to_list subtops;
               ty_params_;
               is_pub_;
               loc_;
             })
  | Ctop_stub _ -> Vec.push acc item

let lift_program (prog : Core.program) =
  let acc = Vec.empty () in
  Lst.iter prog ~f:(fun item -> lift_item acc item);
  Vec.to_list acc
