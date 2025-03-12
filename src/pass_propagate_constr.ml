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

let bind_exprs (exprs : Core.expr list) (cont : Core.expr list -> Core.expr) =
  (let rec go exprs k =
     match exprs with
     | [] -> k []
     | e1 :: es ->
         Core.bind e1 (fun e1_id ->
             let new_e1 = Core.var ~ty:(Core.type_of_expr e1) e1_id in
             go es (fun es -> k (new_e1 :: es)))
   in
   go exprs cont
    : Core.expr)

type ctor_info = { mutable has_benefit_to_inline : bool }
type used_count_tbl = { ctor : ctor_info Ident.Hash.t; genv : Global_env.t }

let all_field_immutable (genv : Global_env.t) ty =
  match Stype.type_repr ty with
  | T_constr { type_constructor; _ } ->
      Core_util.all_fields_immutable genv type_constructor ~allow_enum:true
  | _ -> false

let analyze =
  object
    inherit [_] Core.Iter.iter as super

    method! visit_Cexpr_let used_count_tbl name rhs body ty loc =
      (match rhs with
      | (Cexpr_tuple _ | Cexpr_record _ | Cexpr_constr _)
        when all_field_immutable used_count_tbl.genv (Core.type_of_expr rhs) ->
          Ident.Hash.add used_count_tbl.ctor name
            { has_benefit_to_inline = false }
      | _ -> ());
      super#visit_Cexpr_let used_count_tbl name rhs body ty loc

    method! visit_Cexpr_switch_constr used_count_tbl obj cases default_case ty
        loc =
      (match obj with
      | Cexpr_var { id; _ } -> (
          match Ident.Hash.find_opt used_count_tbl.ctor id with
          | Some ctor_info -> ctor_info.has_benefit_to_inline <- true
          | None -> ())
      | _ -> ());
      super#visit_Cexpr_switch_constr used_count_tbl obj cases default_case ty
        loc

    method! visit_Cexpr_field used_count_tbl record accessor pos ty loc_ =
      (match record with
      | Cexpr_var { id; _ } -> (
          match Ident.Hash.find_opt used_count_tbl.ctor id with
          | Some ctor_info -> ctor_info.has_benefit_to_inline <- true
          | _ -> ())
      | _ -> ());
      super#visit_Cexpr_field used_count_tbl record accessor pos ty loc_
  end

type ctx = { used_count_tbl : used_count_tbl; def_tbl : Core.expr Ident.Hash.t }

let transform =
  object (self)
    inherit [_] Core.Map.map as super

    method! visit_Cexpr_let ctx name rhs body ty loc =
      match Ident.Hash.find_opt ctx.used_count_tbl.ctor name with
      | Some ctor_info when ctor_info.has_benefit_to_inline -> (
          match rhs with
          | Cexpr_tuple { exprs; ty = ty_tuple; loc_ } ->
              bind_exprs
                (Lst.map exprs (self#visit_expr ctx))
                (fun new_exprs ->
                  let new_tuple = Core.tuple new_exprs ~ty:ty_tuple ~loc:loc_ in
                  Ident.Hash.add ctx.def_tbl name new_tuple;
                  super#visit_Cexpr_let ctx name new_tuple body ty loc)
          | Cexpr_record { fields; ty = ty_record; loc_ } ->
              let exprs =
                Lst.map fields (fun field -> self#visit_expr ctx field.expr)
              in
              bind_exprs exprs (fun new_exprs ->
                  let new_fields =
                    Lst.map2 fields new_exprs (fun field ->
                        fun expr -> { field with expr })
                  in
                  let new_record =
                    Core.record new_fields ~ty:ty_record ~loc:loc_
                  in
                  Ident.Hash.add ctx.def_tbl name new_record;
                  super#visit_Cexpr_let ctx name new_record body ty loc)
          | Cexpr_constr { tag; args; ty = ty_constr; loc_ } ->
              bind_exprs
                (Lst.map args (self#visit_expr ctx))
                (fun new_args ->
                  let new_constr =
                    Core.constr tag new_args ~ty:ty_constr ~loc:loc_
                  in
                  Ident.Hash.add ctx.def_tbl name new_constr;
                  super#visit_Cexpr_let ctx name new_constr body ty loc)
          | _ -> assert false)
      | _ -> super#visit_Cexpr_let ctx name rhs body ty loc

    method! visit_Cexpr_switch_constr ctx obj cases default_case ty loc =
      let new_obj =
        match obj with
        | Cexpr_var { id; _ } -> (
            match Ident.Hash.find_opt ctx.def_tbl id with
            | Some expr -> expr
            | None -> obj)
        | _ -> obj
      in
      super#visit_Cexpr_switch_constr ctx new_obj cases default_case ty loc

    method! visit_Cexpr_field ctx record accessor pos ty loc_ =
      let new_record =
        match record with
        | Cexpr_var { id; _ } -> (
            match Ident.Hash.find_opt ctx.def_tbl id with
            | Some expr -> expr
            | None -> record)
        | _ -> record
      in
      super#visit_Cexpr_field ctx new_record accessor pos ty loc_
  end

let propagate_constr (genv : Global_env.t) prog =
  let used_count_tbl = { ctor = Ident.Hash.create 17; genv } in
  let def_tbl = Ident.Hash.create 17 in
  Lst.map prog (fun top_item ->
      Ident.Hash.clear used_count_tbl.ctor;
      analyze#visit_top_item used_count_tbl top_item;
      if Ident.Hash.length used_count_tbl.ctor = 0 then top_item
      else
        let ctx = { used_count_tbl; def_tbl } in
        let o = transform#visit_top_item ctx top_item in
        Ident.Hash.clear def_tbl;
        o)
