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


module Ident = Clam_ident
module Ident_set = Ident.Set
module Lst = Basic_lst
module Ltype = Ltype_gc
module Ltype_util = Ltype_gc_util
module Tid = Basic_ty_ident
module UInt32 = Basic_uint32
module UInt64 = Basic_uint64
module Ast = Dwarfsm_ast

type instr = Dwarfsm_ast.instr

let empty_binder = Wasmlinear_constr.empty_binder
and int_const = Wasmlinear_constr.int_const
and i32_const = Wasmlinear_constr.i32_const
and i64_const = Wasmlinear_constr.i64_const
and f32_const = Wasmlinear_constr.f32_const
and f64_const = Wasmlinear_constr.f64_const
and ( @: ) = Wasmlinear_constr.( @: )
and ( @> ) = Wasmlinear_constr.( @> )
and call = Wasmlinear_constr.call
and if_ = Wasmlinear_constr.if_
and block = Wasmlinear_constr.block
and loop = Wasmlinear_constr.loop
and try_table = Wasmlinear_constr.try_table
and catch = Wasmlinear_constr.catch
and br_ = Wasmlinear_constr.br_
and br = Wasmlinear_constr.br
and func = Wasmlinear_constr.func
and i32 = Wasmlinear_constr.i32
and i64 = Wasmlinear_constr.i64
and f32 = Wasmlinear_constr.f32
and f64 = Wasmlinear_constr.f64
and export = Wasmlinear_constr.export
and import = Wasmlinear_constr.import
and memory = Wasmlinear_constr.memory
and start = Wasmlinear_constr.start
and immut = Wasmlinear_constr.immut
and mut = Wasmlinear_constr.mut
and externref = Wasmlinear_constr.externref
and funcref = Wasmlinear_constr.funcref

let global = Wasmgc_constr.global
and ref_ = Wasmgc_constr.ref_
and null = Wasmgc_constr.null
and ref_null = Wasmgc_constr.ref_null
and anyref = Wasmgc_constr.anyref
and ref_extern_ = Wasmgc_constr.ref_extern_
and array_get_u = Wasmgc_constr.array_get_u
and array_set = Wasmgc_constr.array_set
and array_set_ = Wasmgc_constr.array_set_
and array_get_u_ = Wasmgc_constr.array_get_u_
and struct_new = Wasmgc_constr.struct_new
and ref_func = Wasmgc_constr.ref_func
and generic_get = Wasmgc_constr.generic_get
and ref_cast = Wasmgc_constr.ref_cast
and global_set = Wasmgc_constr.global_set
and global_get_ = Wasmgc_constr.global_get_
and local_set = Wasmgc_constr.local_set
and local_get_ = Wasmgc_constr.local_get_
and result = Wasmgc_constr.result
and param = Wasmgc_constr.param
and local = Wasmgc_constr.local
and struct_get = Wasmgc_constr.struct_get
and struct_set = Wasmgc_constr.struct_set
and call_ref = Wasmgc_constr.call_ref
and local_get = Wasmgc_constr.local_get
and array_copy = Wasmgc_constr.array_copy
and array_fill = Wasmgc_constr.array_fill
and array_new = Wasmgc_constr.array_new
and array_new_ = Wasmgc_constr.array_new_
and array_new_default = Wasmgc_constr.array_new_default
and array_get = Wasmgc_constr.array_get
and array_get_s = Wasmgc_constr.array_get_s
and array_new_fixed = Wasmgc_constr.array_new_fixed
and struct_new_default = Wasmgc_constr.struct_new_default
and ref_cast_ = Wasmgc_constr.ref_cast_

let addr_to_string = Basic_fn_address.to_wasm_name
let add_cst = Wasmir_util.add_cst
let add_dummy_i32 rest = add_cst 0 rest

type loop_info = { params : Ident.t list; break_used : bool ref }

type ctx = {
  locals : Ident_set.t ref;
  loop_info : loop_info Label.Map.t;
  join_points : Ident.t list Join.Hash.t;
  return_type : Ltype.t list;
}

let ( +> ) (ctx : ctx) id = ctx.locals := Ident_set.add !(ctx.locals) id
let i32_zero : instr = int_const 0
let i32_minus_one : instr = int_const (-1)
let i64_zero : instr = i64_const 0L
let f32_zero : instr = F32_const ("0", Float.zero)
let f64_zero : instr = F64_const ("0", Float.zero)

let zero (t : Ltype.t) =
  (match t with
   | I64 -> i64_zero
   | F32 -> f32_zero
   | F64 -> f64_zero
   | I32 { kind = I32_Option_Char } -> i32_minus_one
   | I32 _ -> i32_zero
   | Ref_extern -> ref_null "extern"
   | Ref_string -> ref_null "$moonbit.string"
   | Ref_bytes -> ref_null "$moonbit.bytes"
   | Ref_lazy_init { tid } | Ref { tid } | Ref_nullable { tid } ->
       ref_null (Tid.to_wasm_name tid)
   | Ref_func -> ref_null "func"
   | Ref_any -> ref_null "any"
    : instr)

let compilePrimitive (fn : Primitive.prim) (rest : instr list) =
  (match fn with
   | Pccall { func_name = "add_string"; _ } ->
       call "$moonbit.add_string" @: rest
   | Pprintln -> call "$moonbit.println" @: add_dummy_i32 rest
   | Pfixedarray_length -> Array_len @: rest
   | Pgetbytesitem { safe = _ } -> array_get_u_ "$moonbit.bytes" @: rest
   | Psetbytesitem { safe = _ } ->
       array_set_ "$moonbit.bytes" @: add_dummy_i32 rest
   | Pgetstringitem { safe = _ } ->
       (if !Basic_config.use_js_builtin_string then
          call "$moonbit.js_string.charCodeAt"
        else array_get_u_ "$moonbit.string")
       :: rest
   | Pbyteslength -> Array_len @: rest
   | Pstringlength ->
       (if !Basic_config.use_js_builtin_string then
          call "$moonbit.js_string.length"
        else Array_len)
       :: rest
   | Pstringequal -> call "$moonbit.string_equal" @: rest
   | Pbytesequal -> call "$moonbit.bytes_equal" @: rest
   | Pccall { func_name = "int_is_pos"; _ } ->
       int_const 0 @: Ast.I32_gt_s @: rest
   | Pccall { func_name = "int_is_neg"; _ } ->
       int_const 0 @: Ast.I32_lt_s @: rest
   | Pccall { func_name = "int_is_non_pos"; _ } ->
       int_const 0 @: Ast.I32_le_s @: rest
   | Pccall { func_name = "int_is_non_neg"; _ } ->
       int_const 0 @: Ast.I32_ge_s @: rest
   | Parith { operand_type; operator } ->
       Wasmir_util.compile_arith operand_type operator :: rest
   | Pconvert { kind; from; to_ } ->
       Wasmir_util.compile_convert kind from to_ rest
   | Pbitwise { operand_type; operator } ->
       Wasmir_util.compile_bitwise operand_type operator rest
   | Pcomparison { operand_type; operator } ->
       Wasmir_util.compile_comparison operand_type operator :: rest
   | Prefeq -> Ref_eq @: rest
   | Pignore -> Drop @: int_const 0 @: rest
   | Pidentity -> rest
   | Pccall { func_name = "default_int" | "default_char" | "default_bool"; _ }
     ->
       i32_zero :: rest
   | Pccall { func_name = "default_int64"; _ } -> i64_zero :: rest
   | Pccall { func_name = "default_float"; _ } -> f64_zero :: rest
   | Pnot -> I32_eqz @: rest
   | Ppanic ->
       if !Basic_config.test_mode then call "$throw" @: [ Unreachable ]
       else [ Unreachable ]
   | Punreachable -> [ Unreachable ]
   | Pnull -> ref_null "none" @: rest
   | Pnull_string_extern -> ref_null "extern" @: rest
   | Pas_non_null -> Ref_as_non_null @: rest
   | Pis_null -> Ref_is_null @: rest
   | Praw_func_to_func_ref -> rest
   | Pfixedarray_make _ | Pfixedarray_get_item _ | Pfixedarray_set_item _
   | Penum_field _ | Pset_enum_field _ | Parray_make | Pintrinsic _ | Pcompare _
   | Pcatch | Pcast _ | Pclosure_to_extern_ref | Pmake_value_or_error _
   | Perror_to_string | Pany_to_string | Pcall_object_method _ | Pmakebytes
   | Pget_current_continuation | Prun_async | Pccall _ ->
       failwith
         (("Unsupported primitive " ^ S.to_string (Primitive.sexp_of_prim fn)
           : Stdlib.String.t)
           [@merlin.hide])
    : instr list)

let compileClosure ~(global_ctx : Global_ctx2.t) (it : Clam.closure) rest =
  (match (it.captures, it.address) with
   | [], Normal fn_addr ->
       Global_ctx2.add_func_ref global_ctx fn_addr;
       let tid = it.tid in
       ref_func fn_addr @: struct_new tid @: rest
   | captures, Normal address ->
       Global_ctx2.add_func_ref global_ctx address;
       let tid_capture = it.tid in
       let captures = Lst.concat_map captures generic_get in
       ref_func address @: captures @> struct_new tid_capture @: rest
   | _, Well_known_mut_rec -> assert false
    : instr list)

let rec compileExpr ~(tail : bool) ~ctx ~global_ctx ~type_defs
    (body : Clam.lambda) (rest : instr list) =
  (let body_loc = Clam_util.loc_of_lambda body in
   let rest = Wasmir_util.prepend_end_source_pos body_loc rest in
   Wasmir_util.prepend_start_source_pos body_loc
     (compileExpr0 ~tail ~ctx ~global_ctx ~type_defs body rest)
    : instr list)

and compileExpr0 ~(tail : bool) ~ctx ~global_ctx ~type_defs (body : Clam.lambda)
    (rest : instr list) =
  (let got body rest =
     compileExpr ~ctx ~global_ctx ~tail ~type_defs body rest
       [@@inline]
   in
   let gon body rest =
     compileExpr ~ctx ~global_ctx ~tail:false ~type_defs body rest
       [@@inline]
   in
   let gos_non_tail args rest = Lst.fold_right args rest gon [@@inline] in
   let new_ident ~ty name =
     let ptr = Ident.fresh name ~ty in
     ctx +> ptr;
     ptr
       [@@inline]
   in
   let new_label name = Label.fresh name [@@inline] in
   match body with
   | Levent { expr; _ } -> got expr rest
   | Lconst constant -> (
       match constant with
       | C_int i -> add_cst (Int32.to_int i.v) rest
       | C_byte b -> add_cst b.v rest
       | C_int64 i ->
           let iv = i.v in
           i64_const iv :: rest
       | C_uint { v; _ } ->
           let v = UInt32.to_int32 v in
           i32_const v :: rest
       | C_uint64 { v; _ } ->
           let v = UInt64.to_int64 v in
           i64_const v :: rest
       | C_bool false -> add_cst 0 rest
       | C_bool true -> add_cst 1 rest
       | C_char c ->
           let c = Uchar.to_int c in
           int_const c :: rest
       | C_string s -> Global_ctx2.compile_string_literal ~global_ctx s @> rest
       | C_bytes { v; _ } ->
           Global_ctx2.compile_bytes_literal ~global_ctx v @> rest
       | C_float { v; _ } -> f32_const v @: rest
       | C_double f ->
           let fv = f.v in
           f64_const fv @: rest
       | C_bigint _ -> assert false)
   | Lvar { var } -> generic_get var @> rest
   | Llet { name = Pdot _ as name; e; body } ->
       let rest = got body rest in
       gon e (global_set name @: rest)
   | Llet { name = (Pident _ | Pmutable_ident _) as name; e; body } ->
       ctx +> name;
       let rest = got body rest in
       gon e (local_set name @: rest)
   | Lassign { var; e } -> gon e (local_set var @: int_const 0 @: rest)
   | Lsequence { exprs; last_expr } ->
       let rest = got last_expr rest in
       Lst.fold_left (List.rev exprs) rest (fun acc ->
           fun expr ->
            let acc = Drop @: acc in
            gon expr acc)
   | Lif { pred; ifso; ifnot; type_ } -> (
       match (type_, tail) with
       | I32 { kind = I32_Unit }, false ->
           let ifso = gon ifso [ Drop ] in
           let ifnot = gon ifnot [ Drop ] in
           let branches =
             if_ ~then_:ifso ~else_:ifnot () @> int_const 0 @: rest
           in
           gon pred branches
       | _ ->
           let ifso = got ifso [] in
           let ifnot = got ifnot [] in
           let result_ty = [ result type_ ] in
           gon pred (if_ ~result_ty ~then_:ifso ~else_:ifnot () @> rest))
   | Lclosure closure -> compileClosure ~global_ctx closure rest
   | Lget_raw_func addr ->
       Global_ctx2.add_func_ref global_ctx addr;
       ref_func addr @: rest
   | Lget_field { kind; obj; index; tid } ->
       let index =
         match kind with
         | Tuple | Struct -> index
         | Enum -> index + 1
         | Object { number_of_methods } -> index + number_of_methods
       in
       gon obj (struct_get tid index @: rest)
   | Lclosure_field { obj; index; tid } ->
       let index = index + 1 in
       gon obj (struct_get tid index @: rest)
   | Lset_field { kind; obj; field; index; tid } ->
       let index = match kind with Enum -> index + 1 | Struct -> index in
       gon obj (gon field (struct_set tid index @: int_const 0 @: rest))
   | Lapply { fn = Dynamic fn; args; prim = _ } -> (
       let tid =
         match Ident.get_type fn with
         | Ref_lazy_init { tid } -> tid
         | Ltype.Ref { tid } -> tid
         | _ -> assert false
       in
       let tid_code_ptr = Tid.code_pointer_of_closure tid in
       let args = gos_non_tail (Clam.Lvar { var = fn } :: args) [] in
       match fn with
       | Pident _ | Pdot _ ->
           args @> generic_get fn @> struct_get tid 0 @: call_ref tid_code_ptr
           @: rest
       | Pmutable_ident _ ->
           let fn_tmp =
             new_ident ~ty:(Ref_lazy_init { tid = tid_code_ptr }) "*fn_tmp"
           in
           generic_get fn @> struct_get tid 0 @: local_set fn_tmp @: args
           @> local_get fn_tmp @: call_ref tid_code_ptr @: rest)
   | Lapply { fn = Object { obj; method_index; method_ty = _ }; args; prim = _ }
     ->
       let tid =
         match Ident.get_type obj with
         | Ref_lazy_init { tid } -> tid
         | Ref { tid } -> tid
         | _ -> assert false
       in
       let tid_code_ptr = Tid.method_of_object tid method_index in
       gos_non_tail
         (Clam.Lvar { var = obj } :: args)
         (generic_get obj
         @> struct_get tid method_index
         @: call_ref tid_code_ptr @: rest)
   | Lapply { fn = _; args; prim = Some (FixedArray_copy { src_tid; dst_tid }) }
     ->
       let rest = add_dummy_i32 rest in
       gos_non_tail args (array_copy ~dst_tid ~src_tid @: rest)
   | Lapply { fn = _; args; prim = Some (FixedArray_fill { tid = arr_tid }) } ->
       let rest = add_dummy_i32 rest in
       gos_non_tail args (array_fill arr_tid @: rest)
   | Lapply { fn = _; args; prim = Some Char_to_string } ->
       gos_non_tail args (call "$moonbit.js_string.fromCodePoint" @: rest)
   | Lapply { fn = StaticFn addr; args; prim = _ } ->
       gos_non_tail args (call (addr_to_string addr) @: rest)
   | Lstub_call { fn = func_stubs; params_ty; return_ty; args } ->
       let name =
         match func_stubs with
         | Import { module_name; func_name } ->
             Global_ctx2.add_import global_ctx
               { module_name; func_name; params_ty; return_ty }
         | Internal { func_name } ->
             (func_name : Stdlib.String.t) [@merlin.hide]
         | Inline_code_sexp { language; func_body } ->
             if language <> "wasm" then
               failwith
                 (("extern \"" ^ language
                   ^ "\" is not supported in wasm-gc backend"
                   : Stdlib.String.t)
                   [@merlin.hide]);
             Global_ctx2.add_inline global_ctx func_body
         | Inline_code_text _ -> assert false
       in
       let rest =
         match return_ty with None -> add_dummy_i32 rest | _ -> rest
       in
       gos_non_tail args (call name @: rest)
   | Lprim
       { fn = Parith { operand_type = I32; operator = Neg }; args = a :: [] } ->
       let a = gon a [] in
       int_const 0 @: a @> I32_sub @: rest
   | Lprim
       { fn = Parith { operand_type = I64; operator = Neg }; args = a :: [] } ->
       let a = gon a [] in
       i64_const 0L @: a @> I64_sub @: rest
   | Lprim { fn = Pcompare operand_type; args = [ a1; a2 ] } ->
       let compile_compare (ty : Primitive.operand_type) x y rest =
         match ty with
         | I32 -> x @: y @: I32_ge_s @: x @: y @: I32_le_s @: I32_sub @: rest
         | U32 -> x @: y @: I32_ge_u @: x @: y @: I32_le_u @: I32_sub @: rest
         | I64 -> x @: y @: I64_ge_s @: x @: y @: I64_le_s @: I32_sub @: rest
         | U64 -> x @: y @: I64_ge_u @: x @: y @: I64_le_u @: I32_sub @: rest
         | F32 -> x @: y @: F32_ge @: x @: y @: F32_le @: I32_sub @: rest
         | F64 -> x @: y @: F64_ge @: x @: y @: F64_le @: I32_sub @: rest
         | U8 | U16 | I16 -> assert false
           [@@inline]
       in
       let compile_arg (arg : Clam.lambda) k =
         match arg with
         | Lvar _ | Lconst _ -> (
             match[@warning "-fragile-match"] gon arg [] with
             | v :: [] -> k v
             | _ -> assert false)
         | _ ->
             let ty : Ltype.t =
               match operand_type with
               | I32 | U32 -> Ltype.i32_int
               | I64 | U64 -> I64
               | F32 -> F32
               | F64 -> F64
               | U8 | U16 | I16 -> assert false
             in
             let tmp = Ident.fresh ~ty "tmp" in
             ctx +> tmp;
             gon arg (local_set tmp @: k (local_get tmp))
       in
       compile_arg a1 (fun v1 ->
           compile_arg a2 (fun v2 -> compile_compare operand_type v1 v2 rest))
   | Lmake_array { kind = LenAndInit; tid; elems } -> (
       match[@warning "-fragile-match"] elems with
       | [ len; init ] ->
           let init = gon init [] in
           let len = gon len [] in
           init @> len @> array_new tid @: rest
       | _ -> assert false)
   | Lmake_array { kind = Uninit; tid; elems } -> (
       match[@warning "-fragile-match"] elems with
       | len :: [] -> (
           let len = gon len [] in
           match Global_ctx2.find_default_elem global_ctx type_defs tid with
           | Some id -> global_get_ id @: len @> array_new tid @: rest
           | None -> len @> array_new_default ~id:tid () @: rest)
       | _ -> assert false)
   | Larray_get_item { arr; index; tid; kind; extra } -> (
       let has_default_elem tid =
         match Global_ctx2.find_default_elem global_ctx type_defs tid with
         | Some _ -> true
         | None -> false
       in
       let is_non_nullable_elem tid =
         match Tid.Hash.find_opt type_defs tid with
         | Some (Ref_array { elem = Ref_nullable _ }) -> false
         | _ -> true
       in
       let rest =
         match extra with
         | Need_non_null_cast
           when (not (has_default_elem tid)) && is_non_nullable_elem tid ->
             array_get tid @: Ref_as_non_null @: rest
         | Need_signed_info { signed = true } -> array_get_s tid @: rest
         | Need_signed_info { signed = false } -> array_get_u tid @: rest
         | _ -> array_get tid @: rest
       in
       match kind with
       | Safe | Unsafe -> gon arr (gon index rest)
       | Rev_unsafe ->
           let arr = gon arr [] in
           let rev_index = gon index [] in
           arr @> arr @> Array_len @: rev_index @> I32_sub @: int_const 1
           @: I32_sub @: rest)
   | Larray_set_item { tid; kind = Null; arr; index; item = _ } -> (
       let elem_type = Ltype_util.get_arr_elem tid type_defs in
       match elem_type with
       | I32 _ | I64 | F32 | F64 -> add_dummy_i32 rest
       | Ref _ | Ref_string | Ref_bytes | Ref_func | Ref_any | Ref_nullable _
       | Ref_lazy_init _ | Ref_extern ->
           let arr = gon arr [] in
           let index = gon index [] in
           let zero =
             match Global_ctx2.find_default_elem global_ctx type_defs tid with
             | Some id -> [ global_get_ id ]
             | None -> [ zero elem_type ]
           in
           let rest = add_dummy_i32 rest in
           arr @> index @> zero @> array_set tid @: rest)
   | Larray_set_item { tid; kind = Value | Unsafe; arr; index; item } ->
       let item = Option.get item in
       gos_non_tail [ arr; index; item ] (array_set tid @: int_const 0 @: rest)
   | Larray_set_item { tid; kind = Default; arr; index; item = _ } ->
       let default =
         match Global_ctx2.find_default_elem global_ctx type_defs tid with
         | Some id -> [ global_get_ id ]
         | None ->
             let elem_type = Ltype_util.get_arr_elem tid type_defs in
             [ zero elem_type ]
       in
       gos_non_tail [ arr; index ]
         (default @> array_set tid @: add_dummy_i32 rest)
   | Lallocate { kind = Enum { tag }; fields = []; tid = _ } ->
       let tag = tag.index in
       Global_ctx2.compile_constant_constr ~global_ctx ~tag :: rest
   | Lallocate { kind; fields; tid } ->
       let fields = gos_non_tail fields [] in
       let fields =
         match kind with
         | Enum { tag = { index; _ } } -> int_const index @: fields
         | Object { methods } ->
             Lst.map_append methods fields (fun method_addr ->
                 Global_ctx2.add_func_ref global_ctx method_addr;
                 ref_func method_addr)
         | Tuple | Struct -> fields
       in
       fields @> struct_new tid @: rest
   | Lmake_array { kind = EverySingleElem; tid; elems } ->
       let exception Not_constant in
       let n = List.length elems in
       let normal () =
         if n > 100 then
           let init = gon (List.hd elems) [] in
           let arr_id = new_ident ~ty:(Ref { tid }) "arr" in
           let rest = local_get arr_id @: rest in
           let set_elem =
             Lst.flat_mapi_append (List.tl elems) ~init:rest ~f:(fun i ->
                 fun a ->
                  let arg = gon a [] in
                  let i = i + 1 in
                  local_get arr_id @: int_const i @: arg @> [ array_set tid ])
           in
           init @> int_const n @: array_new tid @: local_set arr_id @: set_elem
         else gos_non_tail elems (array_new_fixed ~id:tid ~size:n @: rest)
           [@@local]
       in
       if n >= 5 then
         let t = Ltype_util.get_arr_elem tid type_defs in
         match t with
         | I32 { kind = I32_Int | I32_Char | I32_Bool | I32_Unit | I32_Tag }
           -> (
             match
               Lst.map elems (fun x ->
                   match Clam_util.no_located x with
                   | Lconst (C_int { v; repr = _ }) -> v
                   | _ -> raise_notrace Not_constant)
             with
             | xs ->
                 Global_ctx2.compile_int32_array_literal ~global_ctx xs tid
                 @> rest
             | exception Not_constant -> normal ())
         | I64 -> (
             match
               Lst.map elems (fun x ->
                   match Clam_util.no_located x with
                   | Lconst (C_int64 { v; repr = _ }) -> v
                   | _ -> raise_notrace Not_constant)
             with
             | xs ->
                 Global_ctx2.compile_int64_array_literal ~global_ctx xs tid
                 @> rest
             | exception Not_constant -> normal ())
         | I32 { kind = I32_Byte } -> (
             let buf = Buffer.create 16 in
             match
               Lst.map elems (fun x ->
                   match Clam_util.no_located x with
                   | Lconst (C_int { v; repr = _ }) ->
                       Buffer.add_int8 buf (Int32.to_int v)
                   | _ -> raise_notrace Not_constant)
             with
             | _ ->
                 Global_ctx2.compile_bytes_literal ~global_ctx
                   (Buffer.contents buf)
                 @> rest
             | exception Not_constant -> normal ())
         | _ -> normal ()
       else normal ()
   | Lprim { fn = Pclosure_to_extern_ref; args = Lvar { var; _ } :: [] } ->
       let closure_ffi_name =
         Global_ctx2.add_import global_ctx Global_ctx2.Import.make_closure
       in
       let tid =
         match Ident.get_type var with
         | Ref_lazy_init { tid } | Ltype.Ref { tid } -> tid
         | _ -> assert false
       in
       let tid_base = tid in
       generic_get var @> struct_get tid_base 0 @: generic_get var
       @> call closure_ffi_name @: rest
   | Lcast { expr; target_type = Ref_any } -> got expr rest
   | Lcast { expr; target_type } ->
       gon expr (ref_cast (result target_type) @: rest)
   | Lcatch { body; on_exception; type_ } ->
       assert !Basic_config.test_mode;
       let body = gon body [] in
       let on_exception = gon on_exception [] in
       let label_exit = new_label "exit" in
       let label_catch = new_label "catch" in
       let tag = Ltype.tag_name in
       block ~label:label_exit
         ~result_ty:[ result type_ ]
         (block ~label:label_catch
            [
              try_table
                ~catchs:[ catch ~tag ~label:label_catch ]
                (body @> [ br_ label_exit ]);
            ]
         @: on_exception)
       @: rest
   | Lprim { fn = Pmakebytes; args } -> (
       match[@warning "-fragile-match"] args with
       | [ size; v ] ->
           let size = gon size [] in
           let v = gon v [] in
           v @> size @> array_new_ "$moonbit.bytes" @: rest
       | _ -> assert false)
   | Lprim { fn; args } -> gos_non_tail args (compilePrimitive fn rest)
   | Lswitch { obj; cases; default; type_ } ->
       let push_drop =
         match type_ with I32 { kind = I32_Unit } -> not tail | _ -> false
       in
       let compile_branch branch =
         if push_drop then gon branch [ Drop ] else got branch []
           [@@inline]
       in
       let tag = new_ident ~ty:Ltype.i32_tag "tag" in
       let obj_tag = local_get tag in
       let default = compile_branch default in
       let cases =
         Lst.map cases (fun (tag, action) -> (tag.index, compile_branch action))
       in
       let switches =
         Wasmir_util.compile_int_switch
           ~result_ty:(if push_drop then [] else [ result type_ ])
           ~obj:obj_tag ~cases ~default
           (if push_drop then add_cst 0 rest else rest)
       in
       let tid_enum = Ltype.tid_enum in
       generic_get obj @> struct_get tid_enum 0 @: local_set tag @: switches
   | Lswitchint { obj; cases; default; type_ } ->
       let push_drop =
         match type_ with I32 { kind = I32_Unit } -> not tail | _ -> false
       in
       let compile_branch branch =
         if push_drop then gon branch [ Drop ] else got branch []
           [@@inline]
       in
       Wasmir_util.compile_int_switch
         ~result_ty:(if push_drop then [] else [ result type_ ])
         ~obj:(local_get obj)
         ~cases:(Lst.map cases (fun (c, action) -> (c, compile_branch action)))
         ~default:(compile_branch default)
         (if push_drop then add_cst 0 rest else rest)
   | Lswitchstring { obj; cases; default; type_ } ->
       let push_drop =
         match type_ with I32 { kind = I32_Unit } -> not tail | _ -> false
       in
       let result_ty = if push_drop then [] else [ result type_ ] in
       let compile_branch branch =
         if push_drop then gon branch [ Drop ] else got branch []
           [@@inline]
       in
       let tag = new_ident ~ty:Ref_string "tag" in
       let obj_tag = local_get tag in
       let switches =
         Lst.fold_right cases (compile_branch default)
           (fun ((tag : string), act) ->
             fun acc ->
              let act = compile_branch act in
              let tagExpr =
                Global_ctx2.compile_string_literal ~global_ctx tag
              in
              obj_tag @: tagExpr
              @> call "$moonbit.string_equal"
              @: if_ ~result_ty ~then_:act ~else_:acc ())
       in
       let rest = if push_drop then add_cst 0 rest else rest in
       gon obj (local_set tag @: switches @> rest)
   | Lletrec { names = name :: []; fns = closure :: []; body } ->
       ctx +> name;
       let rest = got body rest in
       compileClosure ~global_ctx closure (local_set name @: rest)
   | Lletrec { names; fns; body } ->
       Lst.iter names ~f:(fun name -> ctx +> name);
       let names_fns = List.combine names fns in
       let rest = got body rest in
       let alloc_closure =
         Lst.concat_map names_fns (fun (name, { address; _ }) ->
             let tid_capture =
               match address with
               | Normal address -> Tid.capture_of_function address
               | Well_known_mut_rec -> (
                   match Ident.get_type name with
                   | Ref { tid } -> tid
                   | _ -> assert false)
             in
             struct_new_default tid_capture @: [ local_set name ])
       in
       let write_closure =
         Lst.concat_map names_fns (fun (name, { captures; address; tid }) ->
             let tid_capture = tid in
             match address with
             | Normal address ->
                 Global_ctx2.add_func_ref global_ctx address;
                 let closure = generic_get name @> [ ref_cast_ tid_capture ] in
                 closure @> ref_func address
                 @: struct_set tid_capture 0
                    :: Lst.concat
                         (Lst.mapi captures (fun i ->
                              fun c ->
                               let i = i + 1 in
                               closure @> generic_get c
                               @> [ struct_set tid_capture i ]))
             | Well_known_mut_rec ->
                 Lst.concat
                   (Lst.mapi captures (fun i ->
                        fun c ->
                         let closure =
                           generic_get name @> [ ref_cast_ tid_capture ]
                         in
                         closure @> generic_get c
                         @> [ struct_set tid_capture i ])))
       in
       alloc_closure @> write_closure @> rest
   | Ljoinlet { name; e; body; params; kind = _; type_ } ->
       Lst.iter params ~f:(fun param -> ctx +> param);
       let result_param_types =
         Lst.map params (fun id -> result (Ident.get_type id))
       in
       let result_types = Lst.map type_ result in
       let label_ = Join.to_wasm_label name in
       Join.Hash.add ctx.join_points name params;
       let def e =
         Lst.fold_left params e (fun acc ->
             fun param_name -> local_set param_name @: acc)
       in
       if tail then
         let body = got body [ Return ] in
         let e = def (got e rest) in
         block ~label_ ~result_ty:result_param_types body @: e
       else
         let outer_label = Label.to_wasm_name (Label.fresh "outer") in
         let body = gon body [ br outer_label ] in
         let e = def (gon e []) in
         block ~label_:outer_label ~result_ty:result_types
           (block ~label_ ~result_ty:result_param_types body @: e)
         @: rest
   | Ljoinapply { name; args } ->
       let label = Join.to_wasm_label name in
       let jump = [ br label ] in
       gos_non_tail args jump
   | Lbreak { arg; label = loop_label } -> (
       let label = Label.to_wasm_label_break loop_label in
       let loop_info = Label.Map.find_exn ctx.loop_info loop_label in
       loop_info.break_used := true;
       match arg with None -> [ br label ] | Some arg -> gon arg [ br label ])
   | Lcontinue { args; label = loop_label } -> (
       let label = Label.to_wasm_label_loop loop_label in
       match Label.Map.find_exn ctx.loop_info loop_label with
       | { params; _ } ->
           if !Basic_config.use_block_params then gos_non_tail args [ br label ]
           else
             let set_params =
               Lst.fold_left params [] (fun acc ->
                   fun param -> local_set param @: acc)
             in
             gos_non_tail args (set_params @> [ br label ]))
   | Lreturn e -> (
       match ctx.return_type with
       | [] -> [ Return ]
       | _ :: _ -> if tail then got e rest else got e [ Return ])
   | Lloop { params; body; args; label = loop_label; type_ } ->
       Lst.iter params ~f:(fun param -> ctx +> param);
       let break_label = Label.to_wasm_label_break loop_label in
       let label_ = Label.to_wasm_label_loop loop_label in
       let args = gos_non_tail args [] in
       let no_value =
         match type_ with I32 { kind = I32_Unit } -> true | _ -> false
       in
       let break_used = ref false in
       let new_loop_info =
         Label.Map.add ctx.loop_info loop_label { params; break_used }
       in
       let new_ctx = { ctx with loop_info = new_loop_info } in
       let body =
         compileExpr ~global_ctx ~ctx:new_ctx body [] ~tail:false ~type_defs
       in
       let body = if no_value then body @> [ Drop ] else body in
       let result_ty = if no_value then [] else [ result type_ ] in
       let set_params =
         Lst.fold_left params [] (fun acc ->
             fun param -> local_set param @: acc)
       in
       let loop =
         if !Basic_config.use_block_params then
           let params =
             Lst.map params (fun p ->
                 let t = Ident.get_type p in
                 param t)
           in
           args @> [ loop ~label_ ~params ~result_ty (set_params @> body) ]
         else args @> set_params @> [ loop ~label_ ~result_ty body ]
       in
       let loop =
         if !break_used then [ block ~label_:break_label ~result_ty loop ]
         else loop
       in
       if no_value then loop @> int_const 0 @: rest else loop @> rest
    : instr list)

let compileFunc ~(global_ctx : Global_ctx2.t) (top : Clam.top_func_item)
    ~type_defs =
  (let addr = top.binder in
   let ({ body = fn_body; params = fn_params; return_type_; _ } : Clam.fn) =
     top.fn
   in
   let ctx =
     {
       locals = ref Ident_set.empty;
       loop_info = Label.Map.empty;
       join_points = Join.Hash.create 17;
       return_type = return_type_;
     }
   in
   let body =
     let rest = match return_type_ with [] -> [ Ast.Drop ] | _ :: _ -> [] in
     compileExpr ~ctx ~global_ctx fn_body rest ~tail:true ~type_defs
   in
   let locals =
     Ident_set.map_to_list !(ctx.locals) (fun id ->
         let ty = Ident.get_type id in
         local ~id ty)
   in
   let params =
     Lst.map fn_params (fun id ->
         let ty = Ident.get_type id in
         param ~id ty)
   in
   let results = Lst.map return_type_ result in
   let export = match top.fn_kind_ with Top_pub s -> s | Top_private -> "" in
   let source_name = Wasmir_util.compile_fn_source_name top.binder in
   let prologue_end = Wasmir_util.compile_prologue_end () in
   let type_ =
     match top.tid with None -> None | Some tid -> Some (Tid.to_wasm_name tid)
   in
   match return_type_ with
   | I32 { kind = I32_Unit } :: [] when export <> "" ->
       let addr_ = addr_to_string addr in
       let name_wrapper =
         Basic_fn_address.fresh (Basic_fn_address.to_string addr ^ "_wrapper")
       in
       (let args =
          Lst.map params (fun p ->
              match[@warning "-fragile-match"] p.id with
              | { id = Some name; _ } -> local_get_ name
              | _ -> assert false)
        in
        func ~name:name_wrapper ~export ~params (args @> call addr_ @: [ Drop ]))
       @ func ~name:addr ~source_name ~type_ ~params ~results ~locals
           (prologue_end @> body)
   | _ ->
       func ~name:addr ~source_name ~export ~type_ ~params ~results ~locals
         (prologue_end @> body)
    : Ast.modulefield list)

let compile (prog : Clam.prog) =
  (let global_ctx = Global_ctx2.create () in
   let fns : Ast.modulefield list =
     Lst.concat_map prog.fns (fun top ->
         compileFunc ~global_ctx top ~type_defs:prog.type_defs)
   in
   let compile_expr addr body =
     let ctx =
       {
         locals = ref Ident_set.empty;
         loop_info = Label.Map.empty;
         join_points = Join.Hash.create 17;
         return_type = [ Ltype.i32_unit ];
       }
     in
     let body =
       compileExpr ~ctx ~global_ctx body [ Drop ] ~tail:false
         ~type_defs:prog.type_defs
     in
     let locals =
       Ident_set.map_to_list !(ctx.locals) (fun id ->
           let ty = Ident.get_type id in
           local ~id ty)
     in
     let prologue_end = Wasmir_util.compile_prologue_end () in
     func ~name:addr ~locals (prologue_end @> body)
   in
   let main =
     match prog.main with
     | Some main_code ->
         let main = Basic_fn_address.main () in
         let main_code = compile_expr main main_code in
         main_code @ export ~name:"_start" ~func:main ()
     | None -> []
   in
   let init = Basic_fn_address.init () in
   let init_code = compile_expr init prog.init in
   let types = Transl_type.compile_group_type_defs prog.type_defs in
   let data_section = Global_ctx2.compile_to_data global_ctx in
   let table = Global_ctx2.compile_func_ref_declare global_ctx in
   let custom_imports = Global_ctx2.compile_imports global_ctx in
   let inline_wasm = Global_ctx2.compile_inlines global_ctx in
   let runtime =
     if !Basic_config.use_js_builtin_string then
       Runtime_gc_js_string_api.runtime_gc
     else Runtime_gc.runtime_gc
   in
   let runtime = runtime.fields in
   let globals =
     Lst.map prog.globals (fun (id, initial) ->
         match initial with
         | Some (C_bool false) -> global ~id (immut i32) [ i32_const 0l ]
         | Some (C_bool true) ->
             let i = 1l in
             global ~id (immut i32) [ i32_const i ]
         | Some (C_char c) ->
             let i = Uchar.to_int c in
             global ~id (immut i32) [ int_const i ]
         | Some (C_int { v; _ }) -> global ~id (immut i32) [ i32_const v ]
         | Some (C_byte { v; _ }) ->
             global ~id (immut i32) [ i32_const (Int32.of_int v) ]
         | Some (C_int64 { v; _ }) -> global ~id (immut i64) [ i64_const v ]
         | Some (C_uint { v; _ }) ->
             let v = UInt32.to_int32 v in
             global ~id (immut i32) [ i32_const v ]
         | Some (C_uint64 { v; _ }) ->
             let v = UInt64.to_int64 v in
             global ~id (immut i64) [ i64_const v ]
         | Some (C_double { v; _ }) -> global ~id (immut f64) [ f64_const v ]
         | Some (C_float { v; _ }) -> global ~id (immut f32) [ f32_const v ]
         | Some (C_string s) ->
             let v = Global_ctx2.compile_string_literal ~global_ctx s in
             global ~id (immut ref_extern_) v
         | Some (C_bytes _) -> assert false
         | Some (C_bigint _) -> assert false
         | None -> (
             match Ident.get_type id with
             | I32 _ -> global ~id (mut i32) [ i32_const 0l ]
             | I64 -> global ~id (mut i64) [ i64_const 0L ]
             | F32 -> global ~id (mut f32) [ Ast.F32_const ("0", Float.zero) ]
             | F64 -> global ~id (mut f64) [ Ast.F64_const ("0", Float.zero) ]
             | Ref_lazy_init { tid } | Ref { tid } | Ref_nullable { tid } ->
                 global ~id
                   (mut (ref_ ~null ~id:tid ()))
                   [ ref_null (Tid.to_wasm_name tid) ]
             | Ref_extern -> global ~id (mut externref) [ ref_null "extern" ]
             | Ref_string ->
                 if !Basic_config.use_js_builtin_string then
                   global ~id (mut externref) [ ref_null "extern" ]
                 else
                   global ~id
                     (mut (ref_ ~null ~id_:"$moonbit.string" ()))
                     [ ref_null "$moonbit.string" ]
             | Ref_bytes ->
                 global ~id
                   (mut (ref_ ~null ~id_:"$moonbit.bytes" ()))
                   [ ref_null "$moonbit.bytes" ]
             | Ref_func -> global ~id (mut funcref) [ ref_null "func" ]
             | Ref_any -> global ~id (mut anyref) [ ref_null "any" ]))
   in
   let global_ctx_section, import_js_string =
     Global_ctx2.compile_to_globals global_ctx
   in
   let tags =
     if !Basic_config.test_mode then
       let exntag = Ltype.tag_name in
       let tag = Wasmlinear_constr.import_tag in
       let func = Wasmlinear_constr.import_func in
       import ~module_:"exception" ~name:"tag" (tag exntag)
       @ import ~module_:"exception" ~name:"throw" (func "$throw")
     else []
   in
   let mem =
     let export = !Basic_config.export_memory_name in
     let import =
       match
         (!Basic_config.import_memory_module, !Basic_config.import_memory_name)
       with
       | Some module_name, Some mem_name -> Some (module_name, mem_name)
       | _ -> None
     in
     let limits : Ast.limits =
       let min =
         match !Basic_config.memory_limits_min with
         | None -> 1l
         | Some min -> Int32.of_int min
       in
       match !Basic_config.memory_limits_max with
       | None -> { min; max = None }
       | Some max -> { min; max = Some (Int32.of_int max) }
     in
     let shared = if !Basic_config.shared_memory then true else false in
     memory ~name:Ast.default_memory_name ~export ~import ~shared limits
   in
   let fields =
     data_section @ tags @ custom_imports @ import_js_string @ runtime @ mem
     @ types @ table @ globals @ global_ctx_section @ inline_wasm @ fns
     @ start ~init @ init_code @ main
   in
   { id = empty_binder (); fields }
    : Ast.module_)
