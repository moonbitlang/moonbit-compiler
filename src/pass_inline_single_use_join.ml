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

let count_join_usage =
  object
    inherit [_] Core.Iter.iter as super

    method! visit_Cexpr_apply used_count_tbl func args kind ty ty_args loc =
      Ident.Hash.update_if_exists used_count_tbl func (( + ) 1);
      super#visit_Cexpr_apply used_count_tbl func args kind ty ty_args loc

    method! visit_Cexpr_var used_count_tbl var _ty _ty_args _prim _loc =
      Ident.Hash.update_if_exists used_count_tbl var (( + ) 1000)

    method! visit_Cexpr_letfn used_count_tbl name fn body ty kind loc =
      (match kind with
      | Tail_join | Nonrec -> Ident.Hash.add used_count_tbl name 0
      | Rec | Nontail_join -> ());
      super#visit_Cexpr_letfn used_count_tbl name fn body ty kind loc
  end

type ctx = {
  used_count_tbl : int Ident.Hash.t;
  func_def_tbl : Core.fn Ident.Hash.t;
}

let inline_single_use_join =
  object (self)
    inherit [_] Core.Map.map as super

    method! visit_Cexpr_apply ctx func args kind ty ty_args prim loc =
      match Ident.Hash.find_opt ctx.func_def_tbl func with
      | Some (def : Core.fn) ->
          Lst.fold_left2 def.params args def.body (fun param ->
              fun arg ->
               fun body ->
                let arg = self#visit_expr ctx arg in
                Core.let_ param.binder arg body)
      | _ -> super#visit_Cexpr_apply ctx func args kind ty ty_args prim loc

    method! visit_Cexpr_letfn ctx name fn body ty kind loc =
      match Ident.Hash.find_opt ctx.used_count_tbl name with
      | Some 0 -> self#visit_expr ctx body
      | Some 1 ->
          let fn_body =
            match kind with
            | Nonrec -> Core_util.transform_return_in_fn_body fn.body
            | Tail_join -> fn.body
            | Nontail_join | Rec -> assert false
          in
          Ident.Hash.add ctx.func_def_tbl name
            { fn with body = self#visit_expr ctx fn_body };
          self#visit_expr ctx body
      | _ -> super#visit_Cexpr_letfn ctx name fn body ty kind loc
  end

let inline_single_use_join prog =
  let used_count_tbl = Ident.Hash.create 17 in
  let func_def_tbl = Ident.Hash.create 17 in
  Lst.map prog (fun top_item ->
      Ident.Hash.clear used_count_tbl;
      count_join_usage#visit_top_item used_count_tbl top_item;
      if Ident.Hash.length used_count_tbl = 0 then top_item
      else
        let ctx = { used_count_tbl; func_def_tbl } in
        let o = inline_single_use_join#visit_top_item ctx top_item in
        Ident.Hash.clear func_def_tbl;
        o)
