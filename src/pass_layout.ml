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

let minus_one = Mcore.const (Constant.C_int { v = -1l; repr = None })

let eq_minus_one_expr e =
  Mcore.prim ~ty:Mtype.T_bool Primitive.eq_int [ e; minus_one ]

let null ty = Mcore.prim ~ty Pnull []
let is_null e = Mcore.prim ~ty:Mtype.T_bool Pis_null [ e ]
let as_non_null e ty = Mcore.prim ~ty Primitive.Pas_non_null [ e ]
let upcast e ty = Mcore.prim ~ty Primitive.Pidentity [ e ]

let max_i32_plus_one =
  Mcore.const (Constant.C_int64 { v = 0x1_0000_0000L; repr = None })

let eq_max_i32_plus_one_expr e =
  Mcore.prim ~ty:Mtype.T_bool Primitive.eq_int64 [ e; max_i32_plus_one ]

let i32_to_i64 e =
  Mcore.prim ~ty:Mtype.T_int64
    (Primitive.Pconvert { from = I32; to_ = I64; kind = Convert })
    [ e ]

let i64_to_i32 e =
  Mcore.prim ~ty:Mtype.T_int
    (Primitive.Pconvert { from = I64; to_ = I32; kind = Convert })
    [ e ]

type kind = Int | Int64 | Null

let replace_switch_constr ctx ~obj ~ty_obj
    ~(cases : (Tag.t * Ident.t option * Mcore.expr) list) ~default ~loc_
    ~(kind : kind) ~(visit_expr : kind Ident.Map.t -> Mcore.expr -> Mcore.expr)
    =
  let test_none =
    match kind with
    | Int -> eq_minus_one_expr
    | Int64 -> eq_max_i32_plus_one_expr
    | Null -> is_null
  in
  let obj = visit_expr ctx obj in
  Mcore.bind ~loc:loc_ obj (fun obj_binder ->
      let obj_var = Mcore.var ~prim:None ~ty:ty_obj obj_binder in
      match (cases, default) with
      | [ (tag1, _param1, action1); (_tag2, param2, action2) ], None ->
          assert (tag1.index = 0);
          let ifso = visit_expr ctx action1 in
          let ifnot =
            match param2 with
            | Some binder ->
                let new_ctx = Ident.Map.add ctx binder kind in
                Mcore.let_ ~loc:loc_ binder obj_var (visit_expr new_ctx action2)
            | None -> visit_expr ctx action2
          in
          Mcore.if_ (test_none obj_var) ~ifso ~ifnot
      | (tag, param, action1) :: [], Some action2 ->
          if tag.index = 0 then
            let ifso = visit_expr ctx action1 in
            let ifnot = visit_expr ctx action2 in
            Mcore.if_ (test_none obj_var) ~ifso ~ifnot
          else
            let ifso = visit_expr ctx action2 in
            let ifnot =
              match param with
              | Some binder ->
                  let new_ctx = Ident.Map.add ctx binder kind in
                  Mcore.let_ ~loc:loc_ binder obj_var
                    (visit_expr new_ctx action1)
              | None -> visit_expr ctx action1
            in
            Mcore.if_ (test_none obj_var) ~ifso ~ifnot
      | (tag, param, action) :: [], None -> (
          if tag.index = 0 then visit_expr ctx action
          else
            match param with
            | Some binder ->
                let new_ctx = Ident.Map.add ctx binder kind in
                Mcore.let_ ~loc:loc_ binder obj_var (visit_expr new_ctx action)
            | None -> visit_expr ctx action)
      | _ -> assert false)

let optimizer =
  object (self)
    inherit [_] Mcore.Map.map as super

    method! visit_Cexpr_prim (ctx : kind Ident.Map.t) prim args ty loc_ =
      match (prim, args) with
      | Penum_field _, (Cexpr_var { id; ty = enum_ty; _ } as v) :: [] -> (
          match Ident.Map.find_opt ctx id with
          | Some Int -> v
          | Some Int64 -> i64_to_i32 v
          | Some Null -> as_non_null v ty
          | None -> (
              match Mtype.get_optimized_option_kind enum_ty with
              | Optimized_to_int -> v
              | Optimized_to_int64 -> i64_to_i32 v
              | Optimized_ref -> as_non_null v ty
              | Not_optimized -> super#visit_Cexpr_prim ctx prim args ty loc_))
      | Pcast { kind = Enum_to_constr }, v :: [] -> (
          match Mtype.get_optimized_option_kind (Mcore.type_of_expr v) with
          | Optimized_to_int | Optimized_ref | Optimized_to_int64 ->
              self#visit_expr ctx v
          | Not_optimized -> super#visit_Cexpr_prim ctx prim args ty loc_)
      | _ -> super#visit_Cexpr_prim ctx prim args ty loc_

    method! visit_Cexpr_constr ctx tag arg ty loc_ =
      let index = tag.index in
      match Mtype.get_optimized_option_kind ty with
      | Optimized_to_int -> (
          match (index, arg) with
          | 0, _ -> minus_one
          | 1, a :: [] -> upcast (self#visit_expr ctx a) ty
          | _ -> assert false)
      | Optimized_to_int64 -> (
          match (index, arg) with
          | 0, _ -> max_i32_plus_one
          | 1, a :: [] -> i32_to_i64 (self#visit_expr ctx a)
          | _ -> assert false)
      | Optimized_ref -> (
          match (index, arg) with
          | 0, _ -> null ty
          | 1, a :: [] -> upcast (self#visit_expr ctx a) ty
          | _ -> assert false)
      | Not_optimized -> super#visit_Cexpr_constr ctx tag arg ty loc_

    method! visit_Cexpr_switch_constr ctx obj cases default ty loc_ =
      let ty_obj = Mcore.type_of_expr obj in
      let replace_switch_constr kind =
        replace_switch_constr ctx ~obj ~ty_obj ~cases ~default ~loc_ ~kind
          ~visit_expr:self#visit_expr
          [@@inline]
      in
      match Mtype.get_optimized_option_kind ty_obj with
      | Optimized_to_int -> replace_switch_constr Int
      | Optimized_to_int64 -> replace_switch_constr Int64
      | Optimized_ref -> replace_switch_constr Null
      | Not_optimized ->
          super#visit_Cexpr_switch_constr ctx obj cases default ty loc_
  end

let optimize_layout (prog : Mcore.t) =
  ({
     Mcore.body =
       Lst.map prog.body (fun top_item ->
           optimizer#visit_top_item Ident.Map.empty top_item);
     main =
       (match prog.main with
       | Some (expr, loc) ->
           Some (optimizer#visit_expr Ident.Map.empty expr, loc)
       | None -> None);
     types = prog.types;
     object_methods = prog.object_methods;
   }
    : Mcore.t)
