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
module Itype = Dwarfsm_itype
module Lst = Basic_lst
module Ast = Dwarfsm_ast
module Tid = Basic_ty_ident
module Ident = Clam_ident
module Ltype = Ltype_gc
module Ltype_util = Ltype_gc_util

type instr = Ast.instr

let binder = Wasmlinear_constr.binder
and empty_binder = Wasmlinear_constr.empty_binder
and externref = Wasmlinear_constr.externref
and ( @: ) = Wasmlinear_constr.( @: )

let anyref = Ast.Reftype (Ref (Nullable, Absheaptype Any))
let funcref = Ast.Reftype (Ref (Nullable, Absheaptype Func))
let ref_extern_ = Ast.Reftype (Ref (NonNull, Absheaptype Extern))
let null = Ast.Nullable
let typeidx s = ({ var = Unresolve s } : Ast.typeidx)
let dataidx s = ({ var = Unresolve s } : Ast.dataidx)
let localidx s = ({ var = Unresolve s } : Ast.localidx)
let globalidx s = ({ var = Unresolve s } : Ast.globalidx)

let ref_ ?(null = Ast.NonNull) ?id ?(id_ = "") () =
  let id =
    match id with
    | None -> if id_ = "" then assert false else typeidx id_
    | Some id -> typeidx (Tid.to_wasm_name id)
  in
  Ast.Reftype (Ref (null, Type id))

let field_immut (type_ : Ast.valtype) =
  ({ mut = Const; type_ = Ast.Valtype type_ } : Ast.fieldtype)

let field_mut (type_ : Ast.valtype) =
  ({ mut = Var; type_ = Ast.Valtype type_ } : Ast.fieldtype)

let final = ()

let sub ?final ?(super = []) (subtype : Ast.subtype) =
  (let final = match final with None -> false | Some () -> true in
   let super = Lst.map super (fun tid -> typeidx (Tid.to_wasm_name tid)) in
   { subtype with final; super }
    : Ast.subtype)

let typedef ?id subtype =
  (let id =
     match id with
     | None -> empty_binder ()
     | Some id -> binder (Tid.to_wasm_name id)
   in
   (id, subtype)
    : Ast.typedef)

let field ?id (fieldtype : Ast.fieldtype) =
  (let id =
     match id with
     | None -> empty_binder ()
     | Some id -> binder (Tid.to_wasm_name id)
   in
   { id; fieldtype }
    : Ast.field)

let comp_struct (fields : Ast.field list) =
  ({ final = true; super = []; type_ = Structtype (Struct fields) }
    : Ast.subtype)

let comp_func params results =
  ({
     final = true;
     super = [];
     type_ = Wasmlinear_constr.mk_functype params results;
   }
    : Ast.subtype)

let comp_array (fieldtype : Ast.fieldtype) =
  ({ final = true; super = []; type_ = Arraytype (Array fieldtype) }
    : Ast.subtype)

let global ?id ?id_ (type_ : Ast.globaltype) (init : instr list) =
  (let id =
     match id with
     | None -> ( match id_ with None -> assert false | Some s -> binder s)
     | Some id -> binder (Ident.to_wasm_name id)
   in
   Global { id; type_; init }
    : Ast.modulefield)

let import_global ~id_ ~mod_ ~s (type_ : Ast.globaltype) =
  (let id = binder id_ in
   let module_ = mod_ in
   let name = s in
   Ast.Import { module_; name; desc = Global (id, type_) }
    : Ast.modulefield)

let data ~(name : string) data_str =
  [ Ast.Data { id = binder name; mode = Ast.DMPassive; data_str } ]

let elem_declare_func (fns : Basic_fn_address.t list) =
  [
    Ast.Elem
      {
        id = empty_binder ();
        mode = Ast.EMDeclarative;
        type_ = Ref (NonNull, Absheaptype Func);
        list = Lst.map fns (fun name -> [ Wasmlinear_constr.ref_func name ]);
      };
  ]

let source_type_ (typ : Ltype.t) =
  (if !Config.debug then
     match typ with
     | I32 { kind = I32_Int } -> Some Int
     | I32 { kind = I32_Char } -> Some Char
     | I32 { kind = I32_Bool } -> Some Bool
     | I32 { kind = I32_Unit } -> Some Unit
     | I32 { kind = I32_Byte } -> Some Byte
     | I32 { kind = I32_Int16 } -> Some Int16
     | I32 { kind = I32_UInt16 } -> Some UInt16
     | I64 -> Some Int64
     | F64 -> Some Double
     | F32 -> Some Float
     | _ -> None
   else None
    : Itype.t option)

let source_name_ (source_name : string) =
  if !Config.debug then Some source_name else None

let ltype_to_valtype (typ : Ltype.t) =
  (match typ with
   | Ref_extern -> externref
   | F32 -> Numtype F32
   | F64 -> Numtype F64
   | I64 -> Numtype I64
   | I32 _ -> Numtype I32
   | Ref_bytes -> ref_ ~id_:"$moonbit.bytes" ()
   | Ref_string ->
       if !Basic_config.use_js_builtin_string then ref_extern_
       else ref_ ~id_:"$moonbit.string" ()
   | Ref { tid } -> ref_ ~id:tid ()
   | Ref_nullable { tid } ->
       if !Basic_config.use_js_builtin_string && Tid.equal tid Ltype.tid_string
       then externref
       else ref_ ~null ~id:tid ()
   | Ref_lazy_init { tid } -> ref_ ~null ~id:tid ()
   | Ref_func -> funcref
   | Ref_any -> anyref
    : Ast.valtype)

let result = ltype_to_valtype

let param ?id ?id_ (ltype : Ltype.t) =
  (let source_name =
     match id with
     | None -> None
     | Some id -> source_name_ (Ident.source_name id)
   in
   let id =
     match id with
     | None -> ( match id_ with None -> empty_binder () | Some s -> binder s)
     | Some s -> binder (Ident.to_wasm_name s)
   in
   let source_type = source_type_ ltype in
   let type_ = result ltype in
   { id; source_name; source_type; type_ }
    : Ast.param)

let param_ ?id (valtype : Ast.valtype) =
  (let source_name = None in
   let id = match id with None -> empty_binder () | Some s -> binder s in
   let source_type = None in
   let type_ = valtype in
   { id; source_name; source_type; type_ }
    : Ast.param)

let local = param

let array_new_data ?id ?id_ data =
  let data = dataidx data in
  let id =
    match id with
    | None -> ( match id_ with None -> assert false | Some s -> typeidx s)
    | Some tid -> typeidx (Tid.to_wasm_name tid)
  in
  Ast.Array_new_data (id, data)

let array_new_default ?id ?id_ () =
  let id =
    match id with
    | None -> ( match id_ with None -> assert false | Some s -> typeidx s)
    | Some id -> typeidx (Tid.to_wasm_name id)
  in
  Ast.Array_new_default id

let array_new_fixed ~id ~size =
  let size = Int32.of_int size in
  let id = typeidx (Tid.to_wasm_name id) in
  Ast.Array_new_fixed (id, size)

let struct_new tid =
  let tid = typeidx (Tid.to_wasm_name tid) in
  Ast.Struct_new tid

let ref_null str =
  let ht : Ast.heaptype =
    match str with
    | "any" -> Absheaptype Any
    | "eq" -> Absheaptype Eq
    | "i31" -> Absheaptype I31
    | "struct" -> Absheaptype Struct
    | "array" -> Absheaptype Array
    | "none" -> Absheaptype None
    | "func" -> Absheaptype Func
    | "nofunc" -> Absheaptype NoFunc
    | "extern" -> Absheaptype Extern
    | "noextern" -> Absheaptype NoExtern
    | "" -> assert false
    | s when s.[0] = '$' -> Type (typeidx s)
    | _ -> assert false
  in
  Ast.Ref_null ht

let local_get (id : Ident.t) =
  (Local_get (localidx (Ident.to_wasm_name id)) : Ast.instr)

let local_get_ (id : string) = (Local_get (localidx id) : Ast.instr)

let local_set (id : Ident.t) =
  (Local_set (localidx (Ident.to_wasm_name id)) : Ast.instr)

let global_get (id : Ident.t) =
  (Global_get (globalidx (Ident.to_wasm_name id)) : Ast.instr)

let global_get_ (id : string) = (Global_get (globalidx id) : Ast.instr)

let global_set (id : Ident.t) =
  (Global_set (globalidx (Ident.to_wasm_name id)) : Ast.instr)

let generic_get (id : Ident.t) =
  (match id with
   | Pident _ | Pmutable_ident _ -> [ local_get id ]
   | Pdot { ty; _ } ->
       if Ltype_util.is_non_nullable_ref_type ty then
         global_get id @: [ Ast.Ref_as_non_null ]
       else [ global_get id ]
    : Ast.instr list)

let ref_cast valtype =
  match valtype with
  | Ast.Reftype reftype -> Ast.Ref_cast reftype
  | _ -> assert false

let ref_cast_ (tid : Tid.t) =
  let tid = Tid.to_wasm_name tid in
  Ast.Ref_cast (Ref (NonNull, Type (typeidx tid)))

let array_new tid = (Array_new (typeidx (Tid.to_wasm_name tid)) : Ast.instr)
let array_new_ id = (Array_new (typeidx id) : Ast.instr)
let memory_init id = Ast.Memory_init { var = Unresolve id }

let struct_get tid index =
  Ast.Struct_get
    ( typeidx (Tid.to_wasm_name tid),
      { var = Ast.Resolved { var_name = ""; index } } )

let struct_set tid index =
  Ast.Struct_set
    ( typeidx (Tid.to_wasm_name tid),
      { var = Ast.Resolved { var_name = ""; index } } )

let struct_new_default tid =
  Ast.Struct_new_default (typeidx (Tid.to_wasm_name tid))

let ref_func addr =
  (Ast.Ref_func { var = Unresolve (Basic_fn_address.to_wasm_name addr) }
    : Ast.instr)

let array_set tid = Ast.Array_set (typeidx (Tid.to_wasm_name tid))
let array_set_ id = Ast.Array_set (typeidx id)
let array_get tid = Ast.Array_get (typeidx (Tid.to_wasm_name tid))
let array_get_u tid = Ast.Array_get_u (typeidx (Tid.to_wasm_name tid))
let array_get_u_ id = Ast.Array_get_u (typeidx id)
let array_get_s tid = Ast.Array_get_s (typeidx (Tid.to_wasm_name tid))
let call_ref tid = Ast.Call_ref (typeidx (Tid.to_wasm_name tid))

let array_copy ~dst_tid ~src_tid =
  (Array_copy
     (typeidx (Tid.to_wasm_name dst_tid), typeidx (Tid.to_wasm_name src_tid))
    : instr)

let array_fill tid = (Array_fill (typeidx (Tid.to_wasm_name tid)) : instr)
let br_if (id : string) = (Br_if { var = Unresolve id } : instr)
