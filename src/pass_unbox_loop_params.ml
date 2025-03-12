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
module Lst = Basic_lst

type analyze_ctx = {
  loop_params : Ident.Hashset.t;
  loop_info : Label.Hashset.t;
  need_transform : bool ref;
  global_env : Global_env.t;
}

type transform_ctx = {
  loop_params : Ident.Hashset.t;
  fields_of_vars : (Ident.t * Stype.t) array Ident.Hash.t;
  loop_info : Label.Hashset.t;
  global_env : Global_env.t;
}

let analyze_loop_params =
  object
    inherit [_] Core.Iter.iter as super

    method! visit_var (ctx : analyze_ctx) var =
      Ident.Hashset.remove ctx.loop_params var

    method! visit_Cexpr_field ctx record accessor pos ty loc =
      match record with
      | Cexpr_var _ -> ()
      | _ -> super#visit_Cexpr_field ctx record accessor pos ty loc

    method! visit_Cexpr_loop ctx params body args label ty loc_ =
      match params with
      | { ty = T_constr { type_constructor; _ }; binder } :: []
        when Core_util.all_fields_immutable ctx.global_env type_constructor
               ~allow_enum:false ->
          Label.Hashset.add ctx.loop_info label;
          Ident.Hashset.add ctx.loop_params binder;
          super#visit_Cexpr_loop ctx params body args label ty loc_;
          if Ident.Hashset.mem ctx.loop_params binder then
            ctx.need_transform := true
          else Label.Hashset.remove ctx.loop_info label
      | _ -> super#visit_Cexpr_loop ctx params body args label ty loc_
  end

let transform =
  object (self)
    inherit [_] Core.Map.map as super

    method! visit_Cexpr_field ctx record accessor pos ty loc =
      match record with
      | Cexpr_var { id; _ } -> (
          match Ident.Hash.find_opt ctx.fields_of_vars id with
          | Some fields ->
              let id, ty = fields.(pos) in
              Core.var ~loc ~ty id
          | None -> super#visit_Cexpr_field ctx record accessor pos ty loc)
      | _ -> super#visit_Cexpr_field ctx record accessor pos ty loc

    method! visit_Cexpr_loop ctx params body args label ty loc_ =
      match params with
      | {
          ty = T_constr { type_constructor = Tuple n; tys } as ty_tuple;
          binder;
        }
        :: []
        when Label.Hashset.mem ctx.loop_info label
             && Ident.Hashset.mem ctx.loop_params binder -> (
          let base_name = Ident.base_name binder in
          let field_vars = Array.make n (binder, Stype.unit) in
          let new_params =
            Lst.mapi tys (fun i ->
                fun ty ->
                 (let name =
                    ((base_name ^ "_" ^ Int.to_string i
                      : Stdlib.String.t)
                      [@merlin.hide])
                  in
                  let binder = Ident.fresh name in
                  field_vars.(i) <- (binder, ty);
                  { binder; ty; loc_ = Rloc.no_location }
                   : Core.param))
          in
          Ident.Hash.add ctx.fields_of_vars binder field_vars;
          match args with
          | Cexpr_tuple { exprs; _ } :: [] ->
              let args = Lst.map exprs (self#visit_expr ctx) in
              Core.loop new_params (self#visit_expr ctx body) args label
                ~loc:loc_
          | arg :: [] ->
              Core.bind (self#visit_expr ctx arg) (fun id ->
                  let args =
                    Lst.mapi tys (fun i ->
                        fun ty ->
                         let var : Core.expr = Core.var id ~ty:ty_tuple in
                         let index : Parsing_syntax.accessor =
                           Index { tuple_index = i; loc_ = Rloc.no_location }
                         in
                         Core.field var ~ty ~pos:i index)
                  in
                  Core.loop new_params (self#visit_expr ctx body) args label
                    ~loc:loc_)
          | _ -> assert false)
      | { ty = T_constr { type_constructor; _ } as ty_record; binder } :: []
        when Label.Hashset.mem ctx.loop_info label
             && Ident.Hashset.mem ctx.loop_params binder -> (
          let base_name = Ident.base_name binder in
          let fields =
            Core_util.get_fields_exn ctx.global_env type_constructor
          in
          let _, fields =
            Poly_type.instantiate_record ~ty_record:(`Known ty_record) fields
          in
          let field_vars =
            Array.make (List.length fields) (binder, Stype.unit)
          in
          let new_params =
            Lst.map fields (fun field ->
                (let name =
                   ((base_name ^ "_" ^ field.field_name
                     : Stdlib.String.t)
                     [@merlin.hide])
                 in
                 let binder = Ident.fresh name in
                 field_vars.(field.pos) <- (binder, field.ty_field);
                 { binder; ty = field.ty_field; loc_ = Rloc.no_location }
                  : Core.param))
          in
          Ident.Hash.add ctx.fields_of_vars binder field_vars;
          match args with
          | arg :: [] ->
              Core.bind (self#visit_expr ctx arg) (fun arg_id ->
                  let arg = Core.var arg_id ~ty:(Core.type_of_expr arg) in
                  let new_args =
                    Lst.map fields (fun field ->
                        let index : Parsing_syntax.accessor =
                          Label
                            {
                              label_name = field.field_name;
                              loc_ = Rloc.no_location;
                            }
                        in
                        Core.field arg ~ty:field.ty_field ~pos:field.pos index)
                  in
                  Core.loop new_params (self#visit_expr ctx body) new_args label
                    ~loc:loc_)
          | _ -> assert false)
      | _ -> super#visit_Cexpr_loop ctx params body args label ty loc_

    method! visit_Cexpr_continue ctx args label ty loc_ =
      if Label.Hashset.mem ctx.loop_info label then
        match args with
        | Cexpr_tuple { exprs; _ } :: [] ->
            let new_args = Lst.map exprs (self#visit_expr ctx) in
            Core.continue new_args label ty
        | Cexpr_record { fields; _ } :: [] ->
            let new_args =
              Lst.map fields (fun { expr; _ } -> self#visit_expr ctx expr)
            in
            Core.continue new_args label ty
        | arg :: [] ->
            Core.bind (self#visit_expr ctx arg) (fun arg_id ->
                let arg_ty = Core.type_of_expr arg in
                let arg = Core.var arg_id ~ty:arg_ty in
                match arg_ty with
                | T_constr { type_constructor = Tuple _; tys } ->
                    let new_args =
                      Lst.mapi tys (fun i ->
                          fun ty ->
                           let index : Parsing_syntax.accessor =
                             Index { tuple_index = i; loc_ = Rloc.no_location }
                           in
                           Core.field arg ~pos:i ~ty index)
                    in
                    Core.continue new_args label ty
                | T_constr { type_constructor; _ } ->
                    let fields =
                      Core_util.get_fields_exn ctx.global_env type_constructor
                    in
                    let _, fields =
                      Poly_type.instantiate_record ~ty_record:(`Known arg_ty)
                        fields
                    in
                    let new_args =
                      Lst.map fields (fun field ->
                          let index : Parsing_syntax.accessor =
                            Label
                              {
                                label_name = field.field_name;
                                loc_ = Rloc.no_location;
                              }
                          in
                          Core.field arg ~ty:field.ty_field ~pos:field.pos index)
                    in
                    Core.continue new_args label ty
                | _ -> assert false)
        | _ -> assert false
      else super#visit_Cexpr_continue ctx args label ty loc_
  end

let unbox_loop_params (global_env : Global_env.t) (prog : Core.program) =
  Lst.map prog (fun top ->
      let analyze_ctx =
        {
          loop_params = Ident.Hashset.create 16;
          loop_info = Label.Hashset.create 16;
          need_transform = ref false;
          global_env;
        }
      in
      analyze_loop_params#visit_top_item analyze_ctx top;
      if !(analyze_ctx.need_transform) then
        let transform_ctx =
          {
            loop_params = analyze_ctx.loop_params;
            fields_of_vars = Ident.Hash.create 16;
            loop_info = analyze_ctx.loop_info;
            global_env;
          }
        in
        transform#visit_top_item transform_ctx top
      else top)
