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


module Config = Basic_config
module Ast = Dwarfsm_ast
module Lst = Basic_lst
module Hash_int = Basic_hash_int
module Fn_address = Basic_fn_address

type instr = Ast.instr

let i32_const = Wasmlinear_constr.i32_const
and int_const = Wasmlinear_constr.int_const
and if_ = Wasmlinear_constr.if_
and block = Wasmlinear_constr.block
and br_ = Wasmlinear_constr.br_
and br_table = Wasmlinear_constr.br_table
and ( @: ) = Wasmlinear_constr.( @: )
and ( @> ) = Wasmlinear_constr.( @> )

let add_cst i (rest : instr list) =
  match (i, rest) with
  | _, Drop :: rest -> rest
  | _, _ -> i32_const (Int32.of_int i) @: rest

let compile_arith (ty : Primitive.operand_type) (op : Primitive.arith_operator)
    =
  (match ty with
   | I32 -> (
       match op with
       | Add -> I32_add
       | Sub -> I32_sub
       | Mul -> I32_mul
       | Div -> I32_div_s
       | Mod -> I32_rem_s
       | Neg | Sqrt | Abs -> assert false)
   | I64 -> (
       match op with
       | Add -> I64_add
       | Sub -> I64_sub
       | Mul -> I64_mul
       | Div -> I64_div_s
       | Mod -> I64_rem_s
       | Neg | Sqrt | Abs -> assert false)
   | U32 -> (
       match op with
       | Div -> I32_div_u
       | Mod -> I32_rem_u
       | Add -> I32_add
       | Sub -> I32_sub
       | Mul -> I32_mul
       | Neg | Sqrt | Abs -> assert false)
   | U64 -> (
       match op with
       | Div -> I64_div_u
       | Mod -> I64_rem_u
       | Add -> I64_add
       | Sub -> I64_sub
       | Mul -> I64_mul
       | Neg | Sqrt | Abs -> assert false)
   | F32 -> (
       match op with
       | Add -> F32_add
       | Sub -> F32_sub
       | Mul -> F32_mul
       | Div -> F32_div
       | Mod -> assert false
       | Sqrt -> F32_sqrt
       | Neg -> F32_neg
       | Abs -> F32_abs)
   | F64 -> (
       match op with
       | Add -> F64_add
       | Sub -> F64_sub
       | Mul -> F64_mul
       | Div -> F64_div
       | Mod -> assert false
       | Sqrt -> F64_sqrt
       | Neg -> F64_neg
       | Abs -> F64_abs)
   | U16 -> assert false
   | U8 -> assert false
   | I16 -> assert false
    : instr)

let compile_convert (kind : Primitive.convert_kind)
    (from : Primitive.operand_type) (to_ : Primitive.operand_type)
    (rest : instr list) =
  match kind with
  | Convert -> (
      match from with
      | I32 -> (
          match to_ with
          | I32 -> assert false
          | I64 -> I64_extend_i32_s @: rest
          | F32 -> F32_convert_i32_s @: rest
          | F64 -> F64_convert_i32_s @: rest
          | U8 -> I32_const 255l @: I32_and @: rest
          | U16 | I16 -> I32_const 65535l @: I32_and @: rest
          | U64 | U32 -> assert false)
      | I64 -> (
          match to_ with
          | I32 -> I32_wrap_i64 @: rest
          | I64 -> assert false
          | F32 -> F32_convert_i64_s @: rest
          | F64 -> F64_convert_i64_s @: rest
          | U8 -> I32_wrap_i64 @: I32_const 255l @: I32_and @: rest
          | U16 | I16 -> I32_wrap_i64 @: I32_const 65535l @: I32_and @: rest
          | U64 | U32 -> assert false)
      | F32 -> (
          match to_ with
          | I32 -> I32_trunc_f32_s @: rest
          | I64 -> I64_trunc_f32_s @: rest
          | U32 -> I32_trunc_f32_u @: rest
          | U64 -> I64_trunc_f32_u @: rest
          | F32 -> assert false
          | F64 -> F64_promote_f32 @: rest
          | U8 -> I32_trunc_f32_s @: I32_const 255l @: I32_and @: rest
          | U16 | I16 -> assert false)
      | F64 -> (
          match to_ with
          | I32 -> I32_trunc_f64_s @: rest
          | I64 -> I64_trunc_f64_s @: rest
          | U32 -> I32_trunc_f64_u @: rest
          | U64 -> I64_trunc_f64_u @: rest
          | F32 -> F32_demote_f64 @: rest
          | F64 -> assert false
          | U8 -> I32_trunc_f64_s @: I32_const 255l @: I32_and @: rest
          | U16 | I16 -> assert false)
      | U8 -> (
          match to_ with
          | I32 | U16 | I16 -> rest
          | I64 -> I64_extend_i32_s @: rest
          | F64 -> F64_convert_i32_s @: rest
          | F32 -> F32_convert_i32_s @: rest
          | U64 | U32 | U8 -> assert false)
      | U16 -> (
          match to_ with
          | I32 -> rest
          | I64 -> I64_extend_i32_s @: rest
          | F64 -> assert false
          | F32 -> assert false
          | U8 -> I32_const 255l @: I32_and @: rest
          | U64 | U32 | U16 | I16 -> assert false)
      | I16 -> (
          match to_ with
          | I32 -> I32_extend_16_s @: rest
          | I64 -> I32_extend_16_s @: I64_extend_i32_s @: rest
          | F64 -> assert false
          | F32 -> assert false
          | U8 -> I32_const 255l @: I32_and @: rest
          | U64 | U32 | U16 | I16 -> assert false)
      | U32 -> (
          match to_ with
          | U32 -> assert false
          | F32 -> F32_convert_i32_u @: rest
          | F64 -> F64_convert_i32_u @: rest
          | U64 | I64 -> I64_extend_i32_u @: rest
          | I32 | U8 | U16 | I16 -> assert false)
      | U64 -> (
          match to_ with
          | F32 -> F32_convert_i64_u @: rest
          | F64 -> F64_convert_i64_u @: rest
          | U32 | I32 -> I32_wrap_i64 @: rest
          | U64 | I64 | U8 | U16 | I16 -> assert false))
  | Saturate -> (
      match from with
      | F32 -> (
          match to_ with
          | I32 -> I32_trunc_sat_f32_s @: rest
          | U32 -> I32_trunc_sat_f32_u @: rest
          | I64 -> I64_trunc_sat_f32_s @: rest
          | U64 -> I64_trunc_sat_f32_u @: rest
          | U8 -> I32_trunc_sat_f32_u @: I32_const 255l @: I32_and @: rest
          | F32 | F64 | U16 | I16 -> assert false)
      | F64 -> (
          match to_ with
          | I64 -> I64_trunc_sat_f64_s @: rest
          | U64 -> I64_trunc_sat_f64_u @: rest
          | I32 -> I32_trunc_sat_f64_s @: rest
          | U32 -> I32_trunc_sat_f64_u @: rest
          | U8 -> I32_trunc_sat_f64_u @: I32_const 255l @: I32_and @: rest
          | F32 | F64 | U16 | I16 -> assert false)
      | I32 | U32 | I64 | U64 | U8 | I16 | U16 -> assert false)
  | Reinterpret -> (
      match from with
      | F32 -> (
          match to_ with
          | I32 -> I32_reinterpret_f32 @: rest
          | I64 | F32 | F64 | U64 | U32 | U8 | U16 | I16 -> assert false)
      | F64 -> (
          match to_ with
          | I64 -> I64_reinterpret_f64 @: rest
          | I32 | F32 | F64 | U64 | U32 | U8 | U16 | I16 -> assert false)
      | I64 -> (
          match to_ with
          | F64 -> F64_reinterpret_i64 @: rest
          | U64 -> rest
          | I32 | I64 | F32 | U32 | U8 | U16 | I16 -> assert false)
      | U64 -> (
          match to_ with
          | I64 -> rest
          | I32 | F32 | F64 | U64 | U32 | U8 | U16 | I16 -> assert false)
      | I32 -> (
          match to_ with
          | F32 -> F32_reinterpret_i32 @: rest
          | U32 -> rest
          | I32 | I64 | U8 | F64 | U64 | U16 | I16 -> assert false)
      | U32 -> (
          match to_ with
          | I32 -> rest
          | U32 | I64 | U8 | F32 | F64 | U64 | U16 | I16 -> assert false)
      | U8 | U16 | I16 -> assert false)

let compile_bitwise (ty : Primitive.operand_type)
    (op : Primitive.bitwise_operator) (rest : instr list) =
  match ty with
  | I32 -> (
      match op with
      | Not -> int_const (-1) @: I32_xor @: rest
      | And -> I32_and @: rest
      | Or -> I32_or @: rest
      | Xor -> I32_xor @: rest
      | Shl -> I32_shl @: rest
      | Shr -> I32_shr_s @: rest
      | Clz -> I32_clz @: rest
      | Ctz -> I32_ctz @: rest
      | Popcnt -> I32_popcnt @: rest)
  | I64 -> (
      match op with
      | Not -> I64_const (Int64.of_int (-1)) @: I64_xor @: rest
      | And -> I64_and @: rest
      | Or -> I64_or @: rest
      | Xor -> I64_xor @: rest
      | Shl -> I64_extend_i32_s @: I64_shl @: rest
      | Shr -> I64_extend_i32_s @: I64_shr_s @: rest
      | Clz -> I64_clz @: I32_wrap_i64 @: rest
      | Ctz -> I64_ctz @: I32_wrap_i64 @: rest
      | Popcnt -> I64_popcnt @: I32_wrap_i64 @: rest)
  | F32 -> assert false
  | F64 -> assert false
  | U16 -> assert false
  | U8 -> assert false
  | I16 -> assert false
  | U32 -> (
      match op with
      | Not -> int_const (-1) @: I32_xor @: rest
      | And -> I32_and @: rest
      | Or -> I32_or @: rest
      | Xor -> I32_xor @: rest
      | Shl -> I32_shl @: rest
      | Shr -> I32_shr_u @: rest
      | Clz -> I32_clz @: rest
      | Ctz -> I32_ctz @: rest
      | Popcnt -> I32_popcnt @: rest)
  | U64 -> (
      match op with
      | Not -> I64_const (Int64.of_int (-1)) @: I64_xor @: rest
      | And -> I64_and @: rest
      | Or -> I64_or @: rest
      | Xor -> I64_xor @: rest
      | Shl -> I64_extend_i32_s @: I64_shl @: rest
      | Shr -> I64_extend_i32_s @: I64_shr_u @: rest
      | Clz -> I64_clz @: I32_wrap_i64 @: rest
      | Ctz -> I64_ctz @: I32_wrap_i64 @: rest
      | Popcnt -> I64_popcnt @: I32_wrap_i64 @: rest)

let compile_comparison (ty : Primitive.operand_type) (op : Primitive.comparison)
    =
  (match ty with
   | I32 -> (
       match op with
       | Lt -> I32_lt_s
       | Le -> I32_le_s
       | Gt -> I32_gt_s
       | Ge -> I32_ge_s
       | Eq -> I32_eq
       | Ne -> I32_ne)
   | I64 -> (
       match op with
       | Lt -> I64_lt_s
       | Le -> I64_le_s
       | Gt -> I64_gt_s
       | Ge -> I64_ge_s
       | Eq -> I64_eq
       | Ne -> I64_ne)
   | F32 -> (
       match op with
       | Lt -> F32_lt
       | Le -> F32_le
       | Gt -> F32_gt
       | Ge -> F32_ge
       | Eq -> F32_eq
       | Ne -> F32_ne)
   | F64 -> (
       match op with
       | Lt -> F64_lt
       | Le -> F64_le
       | Gt -> F64_gt
       | Ge -> F64_ge
       | Eq -> F64_eq
       | Ne -> F64_ne)
   | U8 -> (
       match op with
       | Lt -> I32_lt_s
       | Le -> I32_le_s
       | Gt -> I32_gt_s
       | Ge -> I32_ge_s
       | Eq -> I32_eq
       | Ne -> I32_ne)
   | U32 -> (
       match op with
       | Lt -> I32_lt_u
       | Le -> I32_le_u
       | Gt -> I32_gt_u
       | Ge -> I32_ge_u
       | Eq -> I32_eq
       | Ne -> I32_ne)
   | U64 -> (
       match op with
       | Lt -> I64_lt_u
       | Le -> I64_le_u
       | Gt -> I64_gt_u
       | Ge -> I64_ge_u
       | Eq -> I64_eq
       | Ne -> I64_ne)
   | U16 -> assert false
   | I16 -> assert false
    : instr)

let compile_int_switch ~(result_ty : Ast.valtype list) ~(obj : instr)
    ~(cases : (int * instr list) list) ~(default : instr list)
    (rest : instr list) =
  let cases_len = List.length cases in
  let min_index, max_index =
    Lst.fold_left cases (Int.max_int, Int.min_int) (fun (min, max) ->
        fun (tag, _) -> (Int.min min tag, Int.max max tag))
  in
  if cases_len >= 3 && max_index - min_index + 1 = cases_len then
    let exit_label = Label.fresh "switch_int" in
    let default_label = Label.fresh "switch_default" in
    let label_table = Hash_int.create 17 in
    let labels =
      List.init cases_len (fun i ->
          let label =
            Label.fresh ("switch_int_" ^ Int.to_string i : Stdlib.String.t)
          in
          Hash_int.add label_table i label;
          label)
    in
    let br_table =
      let br_table = br_table labels default_label in
      if min_index = 0 then obj @: [ br_table ]
      else
        let min_index = Int32.of_int min_index in
        obj @: i32_const min_index @: I32_sub @: [ br_table ]
    in
    let blocks =
      Lst.fold_left cases br_table (fun acc ->
          fun (lhs, act) ->
           let label = Hash_int.find_exn label_table (lhs - min_index) in
           block ~label acc @: act @> [ br_ exit_label ])
    in
    let blocks_with_default =
      block ~label:default_label blocks @: default @> [ br_ exit_label ]
    in
    block ~label:exit_label ~result_ty blocks_with_default @: rest
  else
    let switches =
      Lst.fold_right cases default (fun (lhs, act) ->
          fun acc ->
           let lhs = Int32.of_int lhs in
           if_ ~result_ty
             ~condition:(obj @: i32_const lhs @: [ I32_eq ])
             ~then_:act ~else_:acc ())
    in
    switches @> rest

let compile_source_pos ~pkg (pos : Lexing.position) =
  let file = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Ast.Source_pos { pkg; file; line; col }

let prepend_source_pos ~get_pos (loc : Loc.t option) (v : instr list) =
  if !Config.debug then
    match loc with
    | None -> v
    | Some loc ->
        let pos : Lexing.position = get_pos loc in
        if pos.pos_cnum = -1 then v
        else compile_source_pos ~pkg:(Loc.package loc) pos :: v
  else v

let prepend_start_source_pos (loc : Loc.t option) (v : instr list) =
  prepend_source_pos ~get_pos:Loc.get_start loc v

let prepend_end_source_pos (loc : Loc.t option) (v : instr list) =
  prepend_source_pos ~get_pos:Loc.get_end loc v

let compile_fn_source_name (fn_addr : Fn_address.t) =
  if !Config.debug then
    let name = Fn_address.source_name fn_addr in
    Some name
  else None

let compile_prologue_end () = if !Config.debug then [ Ast.Prologue_end ] else []
