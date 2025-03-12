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


module T = Basic_ident.Ordered_hash
module VI = Basic_vec_int
module Vec = Basic_vec
module Ident = Basic_ident
module Lst = Basic_lst

let add_error = Diagnostics.add_error

type 'a cxt = { tbl : 'a T.t; mutable vars : Ident.Set.t }

let obj =
  object
    inherit [_] Typedtree.iter

    method! visit_var (cxt : _ cxt) v =
      if T.mem cxt.tbl v.var_id then cxt.vars <- Ident.Set.add cxt.vars v.var_id
  end

let vars_of_impl (e : Typedtree.impl) (tbl : _ T.t) =
  (let vars = { tbl; vars = Ident.Set.empty } in
   obj#visit_impl vars e;
   vars.vars
    : Ident.Set.t)

type 'a impl_info = { impl : Typedtree.impl; info : 'a }
type binder_vars = { binder : Ident.t; vars : Ident.Set.t }

let topo_sort ~diagnostics (defs : Typedtree.output) =
  (let (Output { value_defs = impls; type_defs; trait_defs; trait_alias }) =
     defs
   in
   let add_cycle cycle =
     let idents, locs = List.split cycle in
     let errors = Errors.cycle_definitions ~cycle:idents ~locs in
     Lst.iter errors ~f:(add_error diagnostics)
   in
   let tbl = T.create 17 in
   let extract_binder i (impl : Typedtree.impl) =
     (match impl with
      | Timpl_stub_decl { binder; _ }
      | Timpl_fun_decl { fun_decl = { fn_binder = binder; _ }; _ } ->
          T.add tbl binder.binder_id i;
          { impl; info = binder.binder_id }
      | Timpl_letdef { binder; _ } ->
          let binder = binder.binder_id in
          T.add tbl binder i;
          { impl; info = binder }
      | Timpl_expr t ->
          let binder = Ident.of_qual_ident t.expr_id in
          T.add tbl binder i;
          { impl; info = binder }
       : Ident.t impl_info)
   in
   let extract_vars (impl_info : Ident.t impl_info) =
     (let impl = impl_info.impl in
      match impl with
      | Timpl_fun_decl _ ->
          {
            impl;
            info = { binder = impl_info.info; vars = vars_of_impl impl tbl };
          }
      | Timpl_stub_decl _ ->
          { impl; info = { binder = impl_info.info; vars = Ident.Set.empty } }
      | Timpl_letdef { loc_; _ } ->
          let binder = impl_info.info in
          let vars = vars_of_impl impl tbl in
          if Ident.Set.exists vars (fun v -> Ident.equal binder v) then
            add_cycle [ (binder, loc_) ];
          { impl; info = { binder; vars } }
      | Timpl_expr _ ->
          {
            impl;
            info = { binder = impl_info.info; vars = vars_of_impl impl tbl };
          }
       : binder_vars impl_info)
   in
   let impl_array =
     let impl_infos = Basic_arr.of_list_mapi impls extract_binder in
     Array.map extract_vars impl_infos
   in
   let nodes_num = Array.length impl_array in
   let adjacency_array = Array.init nodes_num (fun _ -> VI.empty ()) in
   let add_edge (binder : Ident.t) (var : Ident.t) =
     let src = T.find_value tbl binder in
     let tgt = T.find_value tbl var in
     VI.push adjacency_array.(src) tgt
   in
   Array.iter
     (fun impl ->
       Ident.Set.iter impl.info.vars (fun var -> add_edge impl.info.binder var))
     impl_array;
   let scc = Basic_scc.graph adjacency_array in
   let has_cycle (c : VI.t) =
     (let is_letdef impl =
        match impl with Typedtree.Timpl_letdef _ -> true | _ -> false
      in
      VI.length c > 1 && VI.exists (fun i -> is_letdef impl_array.(i).impl) c
       : bool)
   in
   let handle_cycle (c : VI.t) =
     (let cycle =
        VI.map_into_list c ~unorder:(fun i ->
            let impl = impl_array.(i) in
            (impl.info.binder, Typedtree.loc_of_impl impl.impl))
      in
      add_cycle cycle
       : unit)
   in
   Vec.iter scc (fun c -> if has_cycle c then handle_cycle c);
   let value_defs =
     Lst.concat
       (Vec.map_into_list scc ~unorder:(fun c ->
            VI.map_into_list c ~unorder:(fun i -> impl_array.(i).impl)))
   in
   Output { value_defs; type_defs; trait_defs; trait_alias }
    : Typedtree.output)
