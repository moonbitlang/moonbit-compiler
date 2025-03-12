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

type alias_to =
  | Constant of Core.constant
  | Variable of Ident.t * Stype.t array * Primitive.prim option

type context = {
  alias_tbl : alias_to Ident.Hash.t;
  top_items : Core.top_item Vec.t;
  name_hint : string;
}

let alias_obj =
  object (self)
    inherit [_] Core.Map.map as super

    method! visit_Cexpr_var (context : context) var typ ty_args_ prim loc_ =
      match Ident.Hash.find_opt context.alias_tbl var with
      | Some (Constant c) -> Core.const c ~loc:loc_
      | Some (Variable (v, ty_args_, prim)) ->
          Core.var v ~loc:loc_ ~ty_args_ ~ty:typ ~prim
      | None -> Core.var var ~loc:loc_ ~ty_args_ ~ty:typ ~prim

    method! visit_Cexpr_apply (context : context) func args kind typ ty_args_
        prim loc_ =
      match Ident.Hash.find_opt context.alias_tbl func with
      | Some (Constant _) -> assert false
      | Some (Variable (v, ty_args_, prim)) ->
          let args = Lst.map args (self#visit_expr context) in
          Core.apply v args ~loc:loc_ ~ty_args_ ~ty:typ ~kind ~prim
      | None ->
          super#visit_Cexpr_apply context func args kind typ ty_args_ prim loc_

    method! visit_Cexpr_let context binder rhs body _typ loc_ =
      let alias_tbl = context.alias_tbl in
      let rhs = self#visit_expr context rhs in
      match binder with
      | Pident _ -> (
          match rhs with
          | Cexpr_var
              {
                id = (Pident _ | Pdot _ | Plocal_method _) as id;
                ty_args_;
                prim;
                _;
              } ->
              (match Ident.Hash.find_opt alias_tbl id with
              | Some a -> Ident.Hash.add alias_tbl binder a
              | None ->
                  Ident.Hash.add alias_tbl binder
                    (Variable (id, ty_args_, prim)));
              self#visit_expr context body
          | Cexpr_const
              {
                c =
                  (C_bool _ | C_char _ | C_int _ | C_int64 _ | C_double _) as c;
                _;
              } ->
              Ident.Hash.add alias_tbl binder (Constant c);
              self#visit_expr context body
          | Cexpr_const { c = C_string _; _ } ->
              let new_binder =
                Ident.make_global_binder ~name_hint:context.name_hint binder
              in
              let top_item =
                Core.Ctop_let
                  {
                    binder = new_binder;
                    expr = rhs;
                    is_pub_ = false;
                    loc_ = Loc.no_location;
                  }
              in
              Vec.push context.top_items top_item;
              Ident.Hash.add alias_tbl binder
                (Variable (new_binder, [||], None));
              self#visit_expr context body
          | _ ->
              let body = self#visit_expr context body in
              Core.let_ binder rhs body ~loc:loc_)
      | _ ->
          let body = self#visit_expr context body in
          Core.let_ binder rhs body ~loc:loc_
  end

let remove_let_alias (prog : Core.program) =
  let alias_tbl = Ident.Hash.create 17 in
  let top_items = Vec.empty () in
  let ctx = { alias_tbl; top_items; name_hint = "" } in
  Lst.iter prog ~f:(fun top_item ->
      match top_item with
      | Ctop_expr _ ->
          let item =
            alias_obj#visit_top_item { ctx with name_hint = "*init*" } top_item
          in
          Vec.push top_items item
      | Ctop_fn { binder; _ } | Ctop_stub { binder; _ } | Ctop_let { binder; _ }
        -> (
          let ctx = { ctx with name_hint = Ident.base_name binder } in
          let item = alias_obj#visit_top_item ctx top_item in
          match item with
          | Ctop_let
              {
                binder;
                expr = Cexpr_var { id = Pdot _ as id; ty_args_; prim; _ };
                is_pub_;
              } ->
              Ident.Hash.add ctx.alias_tbl binder
                (Variable (id, ty_args_, prim));
              if is_pub_ then Vec.push top_items item
          | Ctop_let
              {
                binder;
                expr =
                  Cexpr_const
                    {
                      c =
                        (C_bool _ | C_char _ | C_int _ | C_int64 _ | C_double _)
                        as c;
                      _;
                    };
                is_pub_;
              } ->
              Ident.Hash.add ctx.alias_tbl binder (Constant c);
              if is_pub_ then Vec.push top_items item
          | _ -> Vec.push top_items item));
  Vec.to_list top_items
