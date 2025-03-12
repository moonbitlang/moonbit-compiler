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


module Tid = Basic_ty_ident
module Ltype = Ltype_gc
module Lst = Basic_lst
module Ast = Dwarfsm_ast

let i32 = Wasmlinear_constr.i32
and externref = Wasmlinear_constr.externref

let ref_ = Wasmgc_constr.ref_
and anyref = Wasmgc_constr.anyref
and funcref = Wasmgc_constr.funcref
and null = Wasmgc_constr.null
and comp_array = Wasmgc_constr.comp_array
and comp_struct = Wasmgc_constr.comp_struct
and comp_func = Wasmgc_constr.comp_func
and typedef = Wasmgc_constr.typedef
and field_mut = Wasmgc_constr.field_mut
and field_immut = Wasmgc_constr.field_immut
and final = Wasmgc_constr.final
and sub = Wasmgc_constr.sub
and field = Wasmgc_constr.field
and param = Wasmgc_constr.param
and result = Wasmgc_constr.result
and ltype_to_valtype = Wasmgc_constr.ltype_to_valtype
and param_ = Wasmgc_constr.param_

let need_late_init_ltype_to_valtype (t : Ltype.t) =
  (match t with
   | Ref_extern -> externref
   | F32 -> Numtype F32
   | F64 -> Numtype F64
   | I64 -> Numtype I64
   | I32 _ -> Numtype I32
   | Ref_bytes -> ref_ ~null ~id_:"$moonbit.bytes" ()
   | Ref_string ->
       if !Basic_config.use_js_builtin_string then externref
       else ref_ ~null ~id_:"$moonbit.string" ()
   | Ref_nullable { tid } ->
       if !Basic_config.use_js_builtin_string && Tid.equal tid Ltype.tid_string
       then externref
       else ref_ ~null ~id:tid ()
   | Ref_lazy_init { tid } | Ref { tid } -> ref_ ~null ~id:tid ()
   | Ref_func -> funcref
   | Ref_any -> anyref
    : Ast.valtype)

let compile_typedef ~(type_defs : Ltype.type_defs) (id : Tid.t)
    (def : Ltype.def) =
  let array = comp_array in
  let mut = field_mut in
  let immut = field_immut in
  let struct_ = comp_struct in
  let func = comp_func in
  match def with
  | Ref_constructor { args = [] } | Ref_struct { fields = [] } -> []
  | Ref_array { elem = I32 { kind = I32_Int16 | I32_UInt16 } } ->
      let mut_i16 : Ast.fieldtype = { mut = Var; type_ = Packedtype I16 } in
      [ typedef ~id (array mut_i16) ]
  | Ref_array { elem } ->
      let elem = ltype_to_valtype elem in
      [ typedef ~id (array (mut elem)) ]
  | Ref_struct { fields } ->
      let fields =
        Lst.map fields (fun (field_ty, mutable_) ->
            let field_ty = ltype_to_valtype field_ty in
            if mutable_ then field (mut field_ty) else field (immut field_ty))
      in
      [ typedef ~id (struct_ fields) ]
  | Ref_late_init_struct { fields } ->
      let fields =
        Lst.map fields (fun p ->
            field (mut (need_late_init_ltype_to_valtype p)))
      in
      [ typedef ~id (struct_ fields) ]
  | Ref_constructor { args } ->
      let tid_enum = Ltype.tid_enum in
      let fields =
        Lst.map args (fun (field_ty, mutable_) ->
            let field_ty = ltype_to_valtype field_ty in
            if mutable_ then field (mut field_ty) else field (immut field_ty))
      in
      [
        typedef ~id
          (sub ~final ~super:[ tid_enum ]
             (struct_ ([ field (immut i32) ] @ fields)));
      ]
  | Ref_closure_abstract { fn_sig = { params; ret } } ->
      let tid_code_ptr = Tid.code_pointer_of_closure id in
      let params = Lst.map params (fun p -> param p) in
      let result = Lst.map ret (fun p -> result p) in
      [
        typedef ~id:tid_code_ptr
          (func ([ param_ (ref_ ~id ()) ] @ params) result);
        typedef ~id
          (sub (struct_ [ field (mut (ref_ ~null ~id:tid_code_ptr ())) ]));
      ]
  | Ref_object { methods } ->
      let method_tids =
        Lst.mapi methods (fun method_index ->
            fun _ -> Tid.method_of_object id method_index)
      in
      let method_sigs =
        Lst.map2 method_tids methods (fun method_tid ->
            fun { params; ret } ->
             let params = Lst.map params (fun p -> param p) in
             let result = Lst.map ret (fun p -> result p) in
             typedef ~id:method_tid
               (func ([ param_ (ref_ ~id ()) ] @ params) result))
      in
      let method_fields =
        Lst.map method_tids (fun method_tid ->
            field (immut (ref_ ~id:method_tid ())))
      in
      [ typedef ~id (sub (struct_ method_fields)) ] @ method_sigs
  | Ref_concrete_object { abstract_obj_tid; self } -> (
      match[@warning "-fragile-match"]
        Tid.Hash.find_exn type_defs abstract_obj_tid
      with
      | Ref_object { methods } ->
          let methods =
            Lst.mapi methods (fun method_index ->
                fun _ ->
                 let tid_method =
                   Tid.method_of_object abstract_obj_tid method_index
                 in
                 field (immut (ref_ ~id:tid_method ())))
          in
          [
            typedef ~id
              (sub ~super:[ abstract_obj_tid ]
                 (struct_ (methods @ [ field (immut (ltype_to_valtype self)) ])));
          ]
      | _ -> assert false)
  | Ref_closure { fn_sig_tid = _; captures = [] } -> []
  | Ref_closure { fn_sig_tid; captures } ->
      let tid_capture = id in
      let tid_base = fn_sig_tid in
      let tid_code_ptr = Tid.code_pointer_of_closure fn_sig_tid in
      let captures =
        Lst.map captures (fun p ->
            field (mut (need_late_init_ltype_to_valtype p)))
      in
      [
        typedef ~id:tid_capture
          (sub ~super:[ tid_base ]
             (struct_
                ([ field (mut (ref_ ~null ~id:tid_code_ptr ())) ] @ captures)));
      ]

let compile_group_type_defs (type_defs : Ltype.type_defs) =
  let grouped_types = Grouped_typedefs.group_typedefs type_defs in
  Lst.concat_map grouped_types (fun def ->
      match def with
      | Rec defs ->
          let types = ref [] in
          let rec_types = ref [] in
          Lst.iter defs ~f:(fun (tid, def) ->
              let typedefs = compile_typedef tid def ~type_defs in
              match typedefs with
              | [] -> ()
              | norec :: [] -> types := norec :: !types
              | _ -> rec_types := typedefs @ !rec_types);
          let rec_types = !rec_types in
          let types = List.rev !types in
          [ Ast.Rectype (rec_types @ types) ]
      | Nonrec (tid, def) ->
          [ Ast.Rectype (compile_typedef tid def ~type_defs) ])
