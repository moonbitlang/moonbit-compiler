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


module Byteseq = Basic_byteseq
module Lst = Basic_lst
module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Hash_int = Basic_hash_int
module Encode_context = Dwarfsm_encode_context
module Ast = Dwarfsm_ast

type binder = Ast.binder
type label = Ast.label
type typeidx = Ast.typeidx
type fieldidx = Ast.fieldidx
type funcidx = Ast.funcidx
type labelidx = Ast.labelidx
type localidx = Ast.localidx
type tableidx = Ast.tableidx
type memidx = Ast.memidx
type globalidx = Ast.globalidx
type dataidx = Ast.dataidx
type tagidx = Ast.tagidx
type heaptype = Ast.heaptype
type storagetype = Ast.storagetype
type valtype = Ast.valtype
type reftype = Ast.reftype
type fieldtype = Ast.fieldtype
type field = Ast.field
type param = Ast.param
type comptype = Ast.comptype
type functype = Ast.functype
type subtype = Ast.subtype
type typedef = Ast.typedef
type rectype = Ast.rectype
type limits = Ast.limits
type tabletype = Ast.tabletype
type memtype = Ast.memtype
type globaltype = Ast.globaltype
type typeuse = Ast.typeuse
type import = Ast.import
type local = Ast.local
type memarg = Ast.memarg
type catch = Ast.catch
type instr = Ast.instr
type global = Ast.global
type table = Ast.table
type mem = Ast.mem
type export = Ast.export
type func = Ast.func
type elem = Ast.elem
type data = Ast.data
type tag = Ast.tag

module Implicits (Arg : sig
  val ctx : Encode_context.context
  val add_code_pos : (int ref * int -> Ast.source_pos -> unit) option
  val set_prologue_end : (int ref * int -> unit) option
  val custom_sections : (unit -> (string * Byteseq.t) list) option
  val emit_names : bool
end) =
struct
  let ( ^^= ) = Byteseq.O.( ^^= )

  let int_uleb128 = Basic_encoders.int_uleb128
  and byte = Basic_encoders.byte
  and int_sleb128 = Basic_encoders.int_sleb128
  and int32_uleb128 = Basic_encoders.int32_uleb128
  and float_le = Basic_encoders.float_le
  and float32_le = Basic_encoders.float32_le
  and int32_sleb128 = Basic_encoders.int32_sleb128
  and int64_sleb128 = Basic_encoders.int64_sleb128
  and with_length_preceded = Basic_encoders.with_length_preceded

  let ctx = Arg.ctx
  let spaces = ctx.spaces
  let curr_fn_rel_pc_base = ref (ref 0)

  let set_codepos offset (pos : Ast.source_pos) =
    match Arg.add_code_pos with
    | None -> ()
    | Some add_code_pos -> add_code_pos (!curr_fn_rel_pc_base, offset) pos

  let set_prologue_end offset =
    match Arg.set_prologue_end with
    | None -> ()
    | Some set_prologue_end -> set_prologue_end (!curr_fn_rel_pc_base, offset)

  let inline_types : string Hash_string.t = Hash_string.create 0
  let inline_types_buf = ref Byteseq.empty
  let num_distinct_inline_types = ref 0
  let curr_func_idx = ref ~-1
  let curr_labels : label list ref = ref []
  let reset_labels () = curr_labels := []

  let enter_label l f =
    let prev_labels = !curr_labels in
    curr_labels := l :: prev_labels;
    let r = f () in
    curr_labels := prev_labels;
    r

  let ( ^^ ) = Byteseq.concat
  let magic = Byteseq.of_string "\000asm"
  let version = Byteseq.of_string "\001\000\000\000"

  let vec_inner vec f =
    Vec.fold_left ~f:(fun t -> fun x -> t ^^ f x) Byteseq.empty vec

  let vec vec f = int_uleb128 (Vec.length vec) ^^ vec_inner vec f
  let byte_vec x = int_uleb128 (String.length x) ^^ Byteseq.of_string x
  let name x = byte_vec x
  let mut = function Ast.Var -> byte 0x01 | Ast.Const -> byte 0x00

  let resolve_binder (space : Encode_context.space) (binder : binder) =
    let i =
      match binder with
      | { index; _ } when index <> -1 -> index
      | { id = Some name; _ } -> Hash_string.find_exn space.map name
      | _ -> assert false
    in
    binder.index <- i;
    i

  let binder space binder =
    let i = resolve_binder space binder in
    int_sleb128 i

  let resolve_var_ (space : Encode_context.space) (v : Ast.var) =
    match v with
    | Unresolve var_name -> Hash_string.find_exn space.map var_name
    | Resolved { index; _ } -> index

  let resolve_var (space : Encode_context.space) (v : Ast.var) =
    int_sleb128 (resolve_var_ space v)

  let typeidx (i : typeidx) = resolve_var spaces.types i.var

  let structandfieldidx (t : typeidx) (f : fieldidx) =
    let typeidx = resolve_var_ spaces.types t.var in
    let space = Hash_int.find_exn spaces.fields typeidx in
    let fieldidx = resolve_var_ space f.var in
    int_sleb128 typeidx ^^ int_sleb128 fieldidx

  let funcidx (i : funcidx) = resolve_var spaces.funcs i.var

  let labelidx (i : labelidx) =
    match i.var with
    | Resolved { index; _ } -> int_sleb128 index
    | Unresolve name ->
        let rec aux i (l : label list) =
          match l with
          | Some name' :: _ when name = name' -> i
          | [] -> -1
          | _ :: l -> aux (i + 1) l
        in
        int_sleb128 (aux 0 !curr_labels)

  let localidx (i : localidx) =
    resolve_var (Hash_int.find_exn spaces.locals !curr_func_idx) i.var

  let tableidx (i : tableidx) = resolve_var spaces.tables i.var
  let memidx (i : memidx) = resolve_var spaces.mems i.var
  let globalidx (i : globalidx) = resolve_var spaces.globals i.var
  let dataidx (i : dataidx) = resolve_var spaces.datas i.var
  let tagidx (i : tagidx) = resolve_var spaces.tags i.var

  let heaptype (ht : heaptype) =
    match ht with
    | Type i -> typeidx i
    | Absheaptype Any -> byte 0x6e
    | Absheaptype Eq -> byte 0x6d
    | Absheaptype I31 -> byte 0x6c
    | Absheaptype Struct -> byte 0x6b
    | Absheaptype Array -> byte 0x6a
    | Absheaptype None -> byte 0x71
    | Absheaptype Func -> byte 0x70
    | Absheaptype NoFunc -> byte 0x73
    | Absheaptype Extern -> byte 0x6f
    | Absheaptype NoExtern -> byte 0x72

  let reftype (t : reftype) =
    match t with
    | Ref (NonNull, ht) -> byte 0x64 ^^ heaptype ht
    | Ref (Nullable, Type i) -> byte 0x63 ^^ typeidx i
    | Ref (Nullable, ht) -> heaptype ht

  let valtype (t : valtype) =
    match t with
    | Numtype I32 -> byte 0x7f
    | Numtype I64 -> byte 0x7e
    | Numtype F32 -> byte 0x7d
    | Numtype F64 -> byte 0x7c
    | Vectype V128 -> byte 0x7b
    | Reftype rt -> reftype rt

  let storagetype (st : storagetype) =
    match st with
    | Packedtype I8 -> byte 0x78
    | Packedtype I16 -> byte 0x77
    | Valtype vt -> valtype vt

  let fieldtype (ft : fieldtype) = storagetype ft.type_ ^^ mut ft.mut
  let field (field : field) = fieldtype field.fieldtype
  let param (p : param) = valtype p.type_

  let functype (t : functype) =
    let (Func (pts, rts)) = t in
    byte 0x60 ^^ vec (Vec.of_list pts) param ^^ vec (Vec.of_list rts) valtype

  let comptype (ct : comptype) =
    match ct with
    | Arraytype (Array ft) -> byte 0x5e ^^ fieldtype ft
    | Structtype (Struct ft) -> byte 0x5f ^^ vec (Vec.of_list ft) field
    | Functype ft -> functype ft

  let subtype (st : subtype) =
    if st.super = [] && st.final then comptype st.type_
    else
      byte (if st.final then 0x4f else 0x50)
      ^^ vec (Vec.of_list st.super) typeidx
      ^^ comptype st.type_

  let typedef ((id, st) : typedef) =
    let encoded = Byteseq.to_string (subtype st) in
    (match st.type_ with
    | Functype (Func _) ->
        let encoded_typeidx = Byteseq.to_string (binder spaces.types id) in
        Hash_string.replace inline_types encoded encoded_typeidx
    | _ -> ());
    Byteseq.of_string encoded

  let rectype (rt : rectype) =
    match rt with
    | tdef :: [] -> typedef tdef
    | tdefs -> byte 0x4e ^^ vec (Vec.of_list tdefs) typedef

  let limits (lim : limits) =
    match lim with
    | { min; max = None } -> byte 0x00 ^^ int32_uleb128 min
    | { min; max = Some max } ->
        byte 0x01 ^^ int32_uleb128 min ^^ int32_uleb128 max

  let tabletype (t : tabletype) = reftype t.element_type ^^ limits t.limits

  let memtype (t : memtype) =
    match (t.limits.max, t.shared) with
    | Some max, true ->
        byte 0x03 ^^ int32_uleb128 t.limits.min ^^ int32_uleb128 max
    | _ -> limits t.limits

  let globaltype (t : globaltype) = valtype t.type_ ^^ mut t.mut

  let typeuse (t : typeuse) =
    match t with
    | Use (id, _, _) -> typeidx id
    | Inline (pts, rts) ->
        let encoded = Byteseq.to_string (functype (Func (pts, rts))) in
        let encoded_typeidx =
          Hash_string.find_or_update inline_types encoded ~update:(fun _ ->
              let index = spaces.types.next_index in
              spaces.types.next_index <- index + 1;
              incr num_distinct_inline_types;
              inline_types_buf ^^= Byteseq.of_string encoded;
              Byteseq.to_string (int_sleb128 index))
        in
        Byteseq.of_string encoded_typeidx

  let import (ip : import) =
    name ip.module_ ^^ name ip.name
    ^^
    match ip.desc with
    | Func (_, i) -> byte 0x00 ^^ typeuse i
    | Table (_, t) -> byte 0x01 ^^ tabletype t
    | Memory (_, t) -> byte 0x02 ^^ memtype t
    | Global (_, t) -> byte 0x03 ^^ globaltype t
    | Tag (_, i) -> byte 0x04 ^^ byte 0x00 ^^ typeuse i

  let local (l : local) = valtype l.type_

  let locals (ls : local list) =
    let rec partition prev_encoded ls =
      match (ls, prev_encoded) with
      | [], Some (count, prev_encoded) ->
          [ int_uleb128 count ^^ Byteseq.of_string prev_encoded ]
      | [], None -> []
      | l :: ls, None ->
          let encoded = Byteseq.to_string (local l) in
          partition (Some (1, encoded)) ls
      | l :: ls, Some (count, prev_encoded) ->
          let encoded = Byteseq.to_string (local l) in
          if String.equal encoded prev_encoded then
            partition (Some (count + 1, prev_encoded)) ls
          else
            (int_uleb128 count ^^ Byteseq.of_string prev_encoded)
            :: partition (Some (1, encoded)) ls
    in
    vec (Vec.of_list (partition None ls)) Fun.id

  let blocktype (t : typeuse) =
    match t with
    | Inline ([], []) -> byte 0x40
    | Inline ([], t :: []) -> valtype t
    | t -> typeuse t

  let memarg (t : memarg) = int32_uleb128 t.align ^^ int32_uleb128 t.offset

  let catch (c : catch) =
    match c with Catch (x1, x2) -> byte 0x00 ^^ tagidx x1 ^^ labelidx x2

  let rec instr ~base (i : instr) =
    match i with
    | Any_convert_extern -> byte 0xfb ^^ int_uleb128 26
    | Array_copy (x, y) -> byte 0xfb ^^ int_uleb128 17 ^^ typeidx x ^^ typeidx y
    | Array_fill x -> byte 0xfb ^^ int_uleb128 16 ^^ typeidx x
    | Array_get x -> byte 0xfb ^^ int_uleb128 11 ^^ typeidx x
    | Array_get_u x -> byte 0xfb ^^ int_uleb128 13 ^^ typeidx x
    | Array_get_s x -> byte 0xfb ^^ int_uleb128 12 ^^ typeidx x
    | Array_len -> byte 0xfb ^^ int_uleb128 15
    | Array_new x -> byte 0xfb ^^ int_uleb128 6 ^^ typeidx x
    | Array_new_data (x, y) ->
        byte 0xfb ^^ int_sleb128 9 ^^ typeidx x ^^ dataidx y
    | Array_new_default x -> byte 0xfb ^^ int_uleb128 7 ^^ typeidx x
    | Array_new_fixed (x, n) ->
        byte 0xfb ^^ int_uleb128 8 ^^ typeidx x ^^ int32_uleb128 n
    | Array_set x -> byte 0xfb ^^ int_uleb128 14 ^^ typeidx x
    | Block { label; typeuse; instrs } ->
        enter_label label (fun () ->
            let preceded = byte 0x02 ^^ blocktype typeuse in
            let base = base + Byteseq.length preceded in
            preceded ^^ instr_list ~base instrs ^^ byte 0x0b)
    | Br l -> byte 0x0c ^^ labelidx l
    | Br_if l -> byte 0x0d ^^ labelidx l
    | Br_table (ls, l) ->
        byte 0x0e ^^ vec (Vec.of_list ls) labelidx ^^ labelidx l
    | Call x -> byte 0x10 ^^ funcidx x
    | Call_indirect (x, y) -> byte 0x11 ^^ typeuse y ^^ tableidx x
    | Call_ref x -> byte 0x14 ^^ typeidx x
    | Drop -> byte 0x1a
    | Extern_convert_any -> byte 0xfb ^^ int_uleb128 27
    | F64_add -> byte 0xa0
    | F64_const (_, z) -> byte 0x44 ^^ float_le z
    | F64_convert_i32_s -> byte 0xb7
    | F64_convert_i32_u -> byte 0xb8
    | F64_convert_i64_s -> byte 0xb9
    | F64_convert_i64_u -> byte 0xba
    | F64_div -> byte 0xa3
    | F64_eq -> byte 0x61
    | F64_gt -> byte 0x64
    | F64_ge -> byte 0x66
    | F64_le -> byte 0x65
    | F64_load m -> byte 0x2b ^^ memarg m
    | F64_lt -> byte 0x63
    | F64_mul -> byte 0xa2
    | F64_ne -> byte 0x62
    | F64_neg -> byte 0x9a
    | F64_reinterpret_i64 -> byte 0xbf
    | F64_store m -> byte 0x39 ^^ memarg m
    | F64_sub -> byte 0xa1
    | F64_sqrt -> byte 0x9f
    | F64_abs -> byte 0x99
    | F64_ceil -> byte 0x9B
    | F64_floor -> byte 0x9C
    | F64_trunc -> byte 0x9D
    | F64_nearest -> byte 0x9E
    | F32_add -> byte 0x92
    | F32_const (_, z) -> byte 0x43 ^^ float32_le z
    | F32_convert_i32_s -> byte 0xb2
    | F32_convert_i32_u -> byte 0xb3
    | F32_convert_i64_s -> byte 0xb4
    | F32_convert_i64_u -> byte 0xb5
    | F32_demote_f64 -> byte 0xb6
    | F32_div -> byte 0x95
    | F32_eq -> byte 0x5b
    | F32_ge -> byte 0x60
    | F32_gt -> byte 0x5e
    | F32_le -> byte 0x5f
    | F32_load m -> byte 0x2a ^^ memarg m
    | F32_lt -> byte 0x5d
    | F32_mul -> byte 0x94
    | F32_ne -> byte 0x5c
    | F32_neg -> byte 0x8c
    | F32_reinterpret_i32 -> byte 0xbe
    | F32_sqrt -> byte 0x91
    | F32_store m -> byte 0x38 ^^ memarg m
    | F32_sub -> byte 0x93
    | F32_abs -> byte 0x8B
    | F32_ceil -> byte 0x8D
    | F32_floor -> byte 0x8E
    | F32_trunc -> byte 0x8F
    | F32_nearest -> byte 0x90
    | F64_promote_f32 -> byte 0xbb
    | I32_reinterpret_f32 -> byte 0xbc
    | I32_trunc_f32_s -> byte 0xa8
    | I32_trunc_f32_u -> byte 0xa9
    | I64_trunc_f32_s -> byte 0xae
    | I64_trunc_f32_u -> byte 0xaf
    | Global_get g -> byte 0x23 ^^ globalidx g
    | Global_set g -> byte 0x24 ^^ globalidx g
    | I32_add -> byte 0x6a
    | I32_and -> byte 0x71
    | I32_clz -> byte 0x67
    | I32_const n -> byte 0x41 ^^ int32_sleb128 n
    | I32_ctz -> byte 0x68
    | I32_div_s -> byte 0x6d
    | I32_div_u -> byte 0x6e
    | I32_eq -> byte 0x46
    | I32_eqz -> byte 0x45
    | I32_ge_s -> byte 0x4e
    | I32_ge_u -> byte 0x4f
    | I32_gt_s -> byte 0x4a
    | I32_gt_u -> byte 0x4b
    | I32_le_s -> byte 0x4c
    | I32_le_u -> byte 0x4d
    | I32_load m -> byte 0x28 ^^ memarg m
    | I32_load16_u m -> byte 0x2f ^^ memarg m
    | I32_load16_s m -> byte 0x2E ^^ memarg m
    | I32_load8_u m -> byte 0x2d ^^ memarg m
    | I32_load8_s m -> byte 0x2C ^^ memarg m
    | I32_lt_s -> byte 0x48
    | I32_lt_u -> byte 0x49
    | I32_mul -> byte 0x6c
    | I32_ne -> byte 0x47
    | I32_or -> byte 0x72
    | I32_popcnt -> byte 0x69
    | I32_rem_s -> byte 0x6f
    | I32_rem_u -> byte 0x70
    | I32_shl -> byte 0x74
    | I32_shr_s -> byte 0x75
    | I32_shr_u -> byte 0x76
    | I32_rotl -> byte 0x77
    | I32_store m -> byte 0x36 ^^ memarg m
    | I32_store16 m -> byte 0x3b ^^ memarg m
    | I32_store8 m -> byte 0x3a ^^ memarg m
    | I32_sub -> byte 0x6b
    | I32_trunc_f64_s -> byte 0xaa
    | I32_trunc_f64_u -> byte 0xab
    | I32_wrap_i64 -> byte 0xa7
    | I32_xor -> byte 0x73
    | I32_extend_8_s -> byte 0xC0
    | I32_extend_16_s -> byte 0xC1
    | I32_trunc_sat_f32_s -> byte 0xFC ^^ int_uleb128 0
    | I32_trunc_sat_f32_u -> byte 0xFC ^^ int_uleb128 1
    | I32_trunc_sat_f64_s -> byte 0xFC ^^ int_uleb128 2
    | I32_trunc_sat_f64_u -> byte 0xFC ^^ int_uleb128 3
    | I32_atomic_load m -> byte 0xfe ^^ byte 0x10 ^^ memarg m
    | I32_atomic_load16_u m -> byte 0xfe ^^ byte 0x13 ^^ memarg m
    | I32_atomic_load8_u m -> byte 0xfe ^^ byte 0x12 ^^ memarg m
    | I32_atomic_store m -> byte 0xfe ^^ byte 0x17 ^^ memarg m
    | I32_atomic_store16 m -> byte 0xfe ^^ byte 0x1a ^^ memarg m
    | I32_atomic_store8 m -> byte 0xfe ^^ byte 0x19 ^^ memarg m
    | I32_atomic_rmw_cmpxchg m -> byte 0xfe ^^ byte 0x48 ^^ memarg m
    | I32_atomic_rmw8_cmpxchg_u m -> byte 0xfe ^^ byte 0x4a ^^ memarg m
    | I32_atomic_rmw16_cmpxchg_u m -> byte 0xfe ^^ byte 0x4b ^^ memarg m
    | I64_add -> byte 0x7c
    | I64_and -> byte 0x83
    | I64_clz -> byte 0x79
    | I64_const n -> byte 0x42 ^^ int64_sleb128 n
    | I64_ctz -> byte 0x7a
    | I64_div_s -> byte 0x7f
    | I64_div_u -> byte 0x80
    | I64_eq -> byte 0x51
    | I64_extend_i32_s -> byte 0xac
    | I64_extend_i32_u -> byte 0xad
    | I64_ge_s -> byte 0x59
    | I64_gt_s -> byte 0x55
    | I64_le_s -> byte 0x57
    | I64_ge_u -> byte 0x5a
    | I64_gt_u -> byte 0x56
    | I64_le_u -> byte 0x58
    | I64_load m -> byte 0x29 ^^ memarg m
    | I64_load32_u m -> byte 0x35 ^^ memarg m
    | I64_load32_s m -> byte 0x34 ^^ memarg m
    | I64_load16_u m -> byte 0x33 ^^ memarg m
    | I64_load16_s m -> byte 0x32 ^^ memarg m
    | I64_load8_u m -> byte 0x31 ^^ memarg m
    | I64_load8_s m -> byte 0x30 ^^ memarg m
    | I64_lt_s -> byte 0x53
    | I64_lt_u -> byte 0x54
    | I64_mul -> byte 0x7e
    | I64_ne -> byte 0x52
    | I64_or -> byte 0x84
    | I64_popcnt -> byte 0x7b
    | I64_reinterpret_f64 -> byte 0xbd
    | I64_rem_s -> byte 0x81
    | I64_rem_u -> byte 0x82
    | I64_shl -> byte 0x86
    | I64_shr_s -> byte 0x87
    | I64_shr_u -> byte 0x88
    | I64_store m -> byte 0x37 ^^ memarg m
    | I64_store32 m -> byte 0x3E ^^ memarg m
    | I64_store16 m -> byte 0x3D ^^ memarg m
    | I64_store8 m -> byte 0x3C ^^ memarg m
    | I64_sub -> byte 0x7d
    | I64_trunc_f64_s -> byte 0xb0
    | I64_trunc_f64_u -> byte 0xb1
    | I64_xor -> byte 0x85
    | I64_extend_8_s -> byte 0xC2
    | I64_extend_16_s -> byte 0xC3
    | I64_extend_32_s -> byte 0xC4
    | I64_trunc_sat_f32_s -> byte 0xFC ^^ int_uleb128 4
    | I64_trunc_sat_f32_u -> byte 0xFC ^^ int_uleb128 5
    | I64_trunc_sat_f64_s -> byte 0xFC ^^ int_uleb128 6
    | I64_trunc_sat_f64_u -> byte 0xFC ^^ int_uleb128 7
    | V128_load m -> byte 0xFD ^^ int_uleb128 0 ^^ memarg m
    | V128_store m -> byte 0xFD ^^ int_uleb128 11 ^^ memarg m
    | F64x2_add -> byte 0xFD ^^ int_uleb128 240
    | F64x2_mul -> byte 0xFD ^^ int_uleb128 242
    | F32x4_add -> byte 0xFD ^^ int_uleb128 228
    | F32x4_mul -> byte 0xFD ^^ int_uleb128 230
    | If { label; typeuse; then_; else_ = [] } ->
        enter_label label (fun () ->
            let preceded = byte 0x04 ^^ blocktype typeuse in
            let base = base + Byteseq.length preceded in
            preceded ^^ instr_list ~base then_ ^^ byte 0x0b)
    | If { label; typeuse; then_; else_ } ->
        enter_label label (fun () ->
            let base0 = base in
            let preceded = byte 0x04 ^^ blocktype typeuse in
            let base = base0 + Byteseq.length preceded in
            let preceded = preceded ^^ instr_list ~base then_ ^^ byte 0x05 in
            let base = base0 + Byteseq.length preceded in
            preceded ^^ instr_list ~base else_ ^^ byte 0x0b)
    | Local_get x -> byte 0x20 ^^ localidx x
    | Local_set x -> byte 0x21 ^^ localidx x
    | Local_tee x -> byte 0x22 ^^ localidx x
    | Loop { label; typeuse; instrs } ->
        enter_label label (fun () ->
            let preceded = byte 0x03 ^^ blocktype typeuse in
            let base = base + Byteseq.length preceded in
            preceded ^^ instr_list ~base instrs ^^ byte 0x0b)
    | Memory_init x -> byte 0xfc ^^ int_uleb128 8 ^^ dataidx x ^^ byte 0x00
    | Memory_copy -> byte 0xfc ^^ int_uleb128 10 ^^ byte 0x00 ^^ byte 0x00
    | Memory_grow -> byte 0x40 ^^ byte 0x00
    | Memory_size -> byte 0x3f ^^ byte 0x00
    | Memory_fill -> byte 0xfc ^^ int_uleb128 11 ^^ byte 0x00
    | Memory_atomic_notify m -> byte 0xfe ^^ byte 0x00 ^^ memarg m
    | Memory_atomic_wait32 m -> byte 0xfe ^^ byte 0x01 ^^ memarg m
    | Ref_eq -> byte 0xd3
    | Ref_as_non_null -> byte 0xd4
    | Ref_cast (Ref (NonNull, ht)) -> byte 0xfb ^^ int_uleb128 22 ^^ heaptype ht
    | Ref_cast (Ref (Nullable, ht)) ->
        byte 0xfb ^^ int_uleb128 23 ^^ heaptype ht
    | Ref_func x -> byte 0xd2 ^^ funcidx x
    | Ref_is_null -> byte 0xd1
    | Ref_null t -> byte 0xd0 ^^ heaptype t
    | Ref_i31 -> byte 0xfb1c
    | Return -> byte 0x0f
    | Struct_get (x, y) -> byte 0xfb ^^ int_uleb128 2 ^^ structandfieldidx x y
    | Struct_new x -> byte 0xfb ^^ int_uleb128 0 ^^ typeidx x
    | Struct_new_default x -> byte 0xfb ^^ int_uleb128 1 ^^ typeidx x
    | Struct_set (x, y) -> byte 0xfb ^^ int_sleb128 5 ^^ structandfieldidx x y
    | Table_get table_idx -> byte 0x25 ^^ tableidx table_idx
    | Unreachable -> byte 0x00
    | Throw x -> byte 0x08 ^^ tagidx x
    | Try_table { label; typeuse; catchs; instrs } ->
        let catch_seq = vec (Vec.of_list catchs) catch in
        enter_label label (fun () ->
            byte 0x1f ^^ blocktype typeuse ^^ catch_seq
            ^^ instr_list ~base instrs ^^ byte 0x0b)
    | Select -> byte 0x1b
    | No_op -> Byteseq.empty
    | Source_pos pos ->
        set_codepos base pos;
        Byteseq.empty
    | Prologue_end ->
        set_prologue_end base;
        Byteseq.empty

  and instr_list ~base (l : instr list) =
    List.fold_left
      (fun t ->
        fun i ->
         let base = base + Byteseq.length t in
         t ^^ instr ~base i)
      Byteseq.empty l

  and expr (e : instr list) = instr_list ~base:0 e ^^ byte 0x0b

  let global (g : global) = globaltype g.type_ ^^ expr g.init

  let table (t : table) =
    match t.init with
    | [] -> tabletype t.type_
    | init -> byte 0x40 ^^ byte 0x00 ^^ tabletype t.type_ ^^ expr init

  let mem (m : mem) = memtype m.type_

  let export (ep : export) =
    name ep.name
    ^^
    match ep.desc with
    | Func i -> byte 0x00 ^^ funcidx i
    | Table i -> byte 0x01 ^^ tableidx i
    | Memory i -> byte 0x02 ^^ memidx i
    | Global i -> byte 0x03 ^^ globalidx i

  let encode_code ~base (fn : func) =
    let encoded_locals = locals fn.locals in
    curr_func_idx := fn.id.index;
    curr_fn_rel_pc_base := ref 0;
    let encoded_code = expr fn.code in
    curr_func_idx := ~-1;
    reset_labels ();
    let encoded = encoded_locals ^^ encoded_code in
    let fn_len = Byteseq.length encoded in
    let encoded_fn_len = int_uleb128 fn_len in
    let pc_before_locals = base + Byteseq.length encoded_fn_len in
    let pc_after_locals = pc_before_locals + Byteseq.length encoded_locals in
    !curr_fn_rel_pc_base := pc_after_locals;
    let pc_end = pc_after_locals + Byteseq.length encoded_code in
    fn.aux.low_pc <- pc_before_locals;
    fn.aux.high_pc <- pc_end;
    encoded_fn_len ^^ encoded

  let elem (el : elem) =
    let funcrefitem = function
      | Ast.Ref_func i :: [] -> funcidx i
      | _ -> assert false
    in
    match el with
    | { mode = EMActive (t, e); type_ = Ref (Nullable, Absheaptype Func); _ }
      when resolve_var_ spaces.tables t.var = 0 ->
        int_uleb128 0 ^^ expr e ^^ vec (Vec.of_list el.list) funcrefitem
    | { mode = EMPassive; type_ = Ref (NonNull, Absheaptype Func); _ } ->
        int_uleb128 1 ^^ byte 0x00 ^^ vec (Vec.of_list el.list) funcrefitem
    | { mode = EMActive (t, e); type_ = Ref (NonNull, Absheaptype Func); _ } ->
        int_uleb128 2 ^^ tableidx t ^^ expr e ^^ byte 0x00
        ^^ vec (Vec.of_list el.list) funcrefitem
    | { mode = EMDeclarative; type_ = Ref (NonNull, Absheaptype Func); _ } ->
        int_uleb128 3 ^^ byte 0x00 ^^ vec (Vec.of_list el.list) funcrefitem
    | { mode = EMActive (t, e); type_ = Ref (Nullable, Absheaptype Func); _ }
      when resolve_var_ spaces.tables t.var = 0 ->
        int_uleb128 4 ^^ expr e ^^ vec (Vec.of_list el.list) funcrefitem
    | { mode = EMPassive; type_; list; _ } ->
        int_uleb128 5 ^^ reftype type_ ^^ vec (Vec.of_list list) expr
    | { mode = EMActive (t, e); type_; list; _ } ->
        int_uleb128 6 ^^ tableidx t ^^ expr e ^^ reftype type_
        ^^ vec (Vec.of_list list) expr
    | { mode = EMDeclarative; type_; list; _ } ->
        int_uleb128 7 ^^ reftype type_ ^^ vec (Vec.of_list list) expr

  let data (d : data) =
    match d.mode with
    | DMPassive -> int_uleb128 1 ^^ byte_vec d.data_str
    | DMActive (i, e) -> memidx i ^^ expr e ^^ byte_vec d.data_str

  let tag (t : tag) = byte 0x00 ^^ typeuse t.type_

  let section id content =
    byte id ^^ with_length_preceded ~f:int_uleb128 content

  let customsection id content = section 0x00 (name id ^^ content)

  let namesec () =
    let namesubsection id content =
      if Byteseq.is_empty content then Byteseq.empty
      else byte id ^^ with_length_preceded ~f:int_uleb128 content
    in
    let namemap (space : Encode_context.space) =
      let nameassoc idx name' =
        int_uleb128 idx ^^ name (String.sub name' 1 (String.length name' - 1))
      in
      let pairs = Vec.empty () in
      Hash_string.iter space.map (fun entry -> Vec.push pairs entry);
      Vec.sort pairs (fun (_, idx1) -> fun (_, idx2) -> idx1 - idx2);
      vec pairs (fun (name, idx) -> nameassoc idx name)
    in
    let indirectnamemap spaces =
      let pairs = Vec.empty () in
      Hash_int.iter spaces (fun entry -> Vec.push pairs entry);
      Vec.sort pairs (fun (idx1, _) -> fun (idx2, _) -> idx1 - idx2);
      vec pairs (fun (idx, space) -> int_uleb128 idx ^^ namemap space)
    in
    customsection "name"
      (namesubsection 1 (namemap spaces.funcs)
      ^^ namesubsection 2 (indirectnamemap spaces.locals)
      ^^ namesubsection 4 (namemap spaces.types)
      ^^ namesubsection 6 (namemap spaces.mems)
      ^^ namesubsection 7 (namemap spaces.globals)
      ^^ namesubsection 8 (namemap spaces.elems)
      ^^ namesubsection 9 (namemap spaces.datas)
      ^^ namesubsection 10 (indirectnamemap spaces.fields)
      ^^ namesubsection 11 (namemap spaces.tags))

  let output =
    let typesec_content = vec_inner ctx.types rectype in
    let importsec = section 2 (vec ctx.imports import) in
    let tablesec = section 4 (vec ctx.tables table) in
    let memsec = section 5 (vec ctx.mems mem) in
    let globalsec = section 6 (vec ctx.globals global) in
    let exportsec = section 7 (vec ctx.exports export) in
    let startsec =
      match ctx.start with
      | Some start -> section 8 (funcidx start)
      | None -> Byteseq.empty
    in
    let elemsec = section 9 (vec ctx.elems elem) in
    let tagsec =
      if Vec.length ctx.tags = 0 then Byteseq.empty
      else section 13 (vec ctx.tags tag)
    in
    let funcsec_buf = ref Byteseq.empty in
    let codesec_buf = ref Byteseq.empty in
    let () =
      funcsec_buf ^^= int_uleb128 (Vec.length ctx.funcs);
      codesec_buf ^^= int_uleb128 (Vec.length ctx.funcs);
      Vec.iter ctx.funcs (fun fn ->
          funcsec_buf ^^= typeuse fn.type_;
          codesec_buf ^^= encode_code ~base:(Byteseq.length !codesec_buf) fn)
    in
    let funcsec = section 3 !funcsec_buf in
    let codesec = section 10 !codesec_buf in
    let datacountsec = section 12 (int_uleb128 (Vec.length ctx.datas)) in
    let datasec = section 11 (vec ctx.datas data) in
    let typesec =
      section 1
        (int_uleb128 (Vec.length ctx.types + !num_distinct_inline_types)
        ^^ typesec_content ^^ !inline_types_buf)
    in
    let buf =
      ref
        (magic ^^ version ^^ typesec ^^ importsec ^^ funcsec ^^ tablesec
       ^^ memsec ^^ tagsec ^^ globalsec ^^ exportsec ^^ startsec ^^ elemsec
       ^^ datacountsec ^^ codesec ^^ datasec)
    in
    if Arg.emit_names then buf ^^= namesec ();
    let () =
      match Arg.custom_sections with
      | None -> ()
      | Some custom_sections ->
          let secs = custom_sections () in
          Lst.iter secs ~f:(fun (name, content) ->
              buf ^^= customsection name content)
    in
    !buf
end

let encode ?add_code_pos ?custom_sections ~emit_names ctx =
  let module I = Implicits (struct
    let ctx = ctx
    let add_code_pos = add_code_pos
    let set_prologue_end = None
    let custom_sections = custom_sections
    let emit_names = emit_names
  end) in
  I.output
