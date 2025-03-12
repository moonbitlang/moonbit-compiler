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
module Ident = Clam1_ident
module Ast = Dwarfsm_ast
module Lst = Basic_lst

type instr = Ast.instr

let empty_binder () = ({ id = None; index = -1 } : Dwarfsm_ast.binder)

let comp_to_sub (comp : Ast.comptype) =
  ({ final = true; super = []; type_ = comp } : Ast.subtype)

let mk_functype (params : Ast.param list) (results : Ast.valtype list) =
  (Functype (Func (params, results)) : Ast.comptype)

let esc_then_decode s =
  let s = String.escaped s in
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    (try
       let c =
         if s.[!i] <> '\\' then s.[!i]
         else
           match
             incr i;
             s.[!i]
           with
           | 'n' -> '\n'
           | 'r' -> '\r'
           | 't' -> '\t'
           | '\\' -> '\\'
           | '\'' -> '\''
           | '"' -> '"'
           | 'u' ->
               let j = !i + 2 in
               i := String.index_from s j '}';
               let n = int_of_string ("0x" ^ String.sub s j (!i - j)) in
               Buffer.add_utf_8_uchar b (Uchar.of_int n);
               raise Exit
           | h ->
               incr i;
               Char.chr
                 (int_of_string ("0x" ^ String.make 1 h ^ String.make 1 s.[!i]))
       in
       Buffer.add_char b c
     with Exit -> ());
    incr i
  done;
  Buffer.contents b

let localidx s = ({ var = Unresolve s } : Ast.localidx)
let labelidx s = ({ var = Unresolve s } : Ast.labelidx)
let binder s = ({ id = Some s; index = -1 } : Dwarfsm_ast.binder)

let global (id : Ident.t) (type_ : Ast.globaltype) (init : instr list) =
  (let id = binder (Ident.to_wasm_name id) in
   Global { id; type_; init }
    : Ast.modulefield)

let immut (type_ : Ast.valtype) = ({ mut = Const; type_ } : Ast.globaltype)
let mut (type_ : Ast.valtype) = ({ mut = Var; type_ } : Ast.globaltype)
let i32 = Ast.Numtype I32
let i64 = Ast.Numtype I64
let f32 = Ast.Numtype F32
let f64 = Ast.Numtype F64
let funcref = Ast.Reftype (Ref (Nullable, Absheaptype Func))
let externref = Ast.Reftype (Ref (Nullable, Absheaptype Extern))

let data ~(offset : int32) data_str =
  [
    Ast.Data
      {
        id = empty_binder ();
        mode =
          Ast.DMActive
            ( { var = Unresolve Ast.default_memory_name },
              [ Ast.I32_const offset ] );
        data_str;
      };
  ]

let table ~id ~type_ ~fn_names () =
  let n = Int32.of_int (List.length fn_names) in
  let name = id in
  let id = binder id in
  [
    Ast.Table
      {
        id;
        type_ = { limits = { min = n; max = Some n }; element_type = type_ };
        init = [];
      };
    Ast.Elem
      {
        id = { id = None; index = -1 };
        type_;
        mode = Ast.EMActive ({ var = Unresolve name }, [ I32_const 0l ]);
        list = fn_names;
      };
  ]

let import ~module_ ~name desc = [ Ast.Import { module_; name; desc } ]

let import_tag ?(params = []) ?(results = []) name =
  (Ast.Tag (binder name, Inline (params, results)) : Ast.importdesc)

let import_func ?(params = []) ?(results = []) name =
  (Ast.Func (binder name, Inline (params, results)) : Ast.importdesc)

let export ~name ?func ?(func_ = "") () =
  let funcname : Ast.funcidx =
    match func with
    | None -> if func_ = "" then assert false else { var = Unresolve func_ }
    | Some addr -> { var = Unresolve (Basic_fn_address.to_wasm_name addr) }
  in
  [ Ast.Export { name; desc = Func funcname } ]

let memory ~name ~export ~import ~shared limits =
  let id = binder name in
  let esc_then_decode = esc_then_decode in
  let export =
    match export with
    | None -> []
    | Some mem_name -> (
        let s = esc_then_decode mem_name in
        match[@warning "-fragile-match"] id.id with
        | Some var_name ->
            [
              Ast.Export
                { name = s; desc = Memory { var = Unresolve var_name } };
            ]
        | _ -> assert false)
  in
  let memory =
    match import with
    | None -> Ast.Mem { id; type_ = { limits; shared } }
    | Some (module_name, mem_name) ->
        let s1 = esc_then_decode module_name in
        let s2 = esc_then_decode mem_name in
        Ast.Import
          { module_ = s1; name = s2; desc = Memory (id, { limits; shared }) }
  in
  memory :: export

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
     | U32 -> Some Uint
     | U64 -> Some UInt64
     | _ -> None
   else None
    : Itype.t option)

let source_name_ (source_name : string) =
  if !Config.debug then Some source_name else None

let result (typ : Ltype.t) =
  (match typ with
   | Ref_any -> Numtype I32
   | Ref _ -> Numtype I32
   | Ref_lazy_init _ -> Numtype I32
   | Ref_nullable _ -> Numtype I32
   | Ref_string -> Numtype I32
   | Ref_bytes -> Numtype I32
   | I32 { kind = I32_Option_Char } -> Numtype I32
   | I32 { kind = I32_Tag } -> Numtype I32
   | I32 { kind = I32_Int } -> Numtype I32
   | I32 { kind = I32_Char } -> Numtype I32
   | I32 { kind = I32_Bool } -> Numtype I32
   | I32 { kind = I32_Unit } -> Numtype I32
   | I32 { kind = I32_Byte } -> Numtype I32
   | I32 { kind = I32_Int16 } -> Numtype I32
   | I32 { kind = I32_UInt16 } -> Numtype I32
   | I64 -> Numtype I64
   | F64 -> Numtype F64
   | F32 -> Numtype F32
   | Ref_extern -> Reftype (Ref (Nullable, Absheaptype Extern))
   | Ref_func -> Reftype (Ref (Nullable, Absheaptype Func))
   | Raw_func _ -> Numtype I32
   | U32 -> Numtype I32
   | U64 -> Numtype I64
    : Ast.valtype)

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

let local = param

let func ?name ?(name_ = "") ?(source_name = None) ?(export = "")
    ?(type_ = None) ?(params = []) ?(results = []) ?(locals = [])
    (body : instr list) =
  (let id =
     match name with
     | None -> if name_ = "" then empty_binder () else binder name_
     | Some addr -> binder (Basic_fn_address.to_wasm_name addr)
   in
   let export =
     if export = "" then []
     else
       match[@warning "-fragile-match"] id.id with
       | Some name ->
           [
             Ast.Export
               {
                 name = esc_then_decode export;
                 desc = Func { var = Unresolve name };
               };
           ]
       | _ -> assert false
   in
   let type_ : Ast.typeuse =
     match type_ with
     | None -> Inline (params, results)
     | Some id -> Use ({ var = Unresolve id }, params, results)
   in
   [
     Ast.Func
       ({
          id;
          source_name;
          type_;
          locals;
          code = body;
          aux = { low_pc = 0; high_pc = 0 };
        }
         : Ast.func);
   ]
   @ export
    : Ast.modulefield list)

let start ~(init : Basic_fn_address.t) =
  [ Ast.Start { var = Unresolve (Basic_fn_address.to_wasm_name init) } ]

let ( @: ) (instr : instr) (rest : instr list) =
  match (instr, rest) with
  | (F32_const _ | I64_const _ | I32_const _ | Local_get _), Drop :: rest ->
      rest
  | ( Local_set ({ var = Unresolve id1 } as localidx),
      Local_get { var = Unresolve id2 } :: rest )
    when String.equal id1 id2 ->
      Local_tee localidx :: rest
  | Br _, _ -> [ instr ]
  | _ -> instr :: rest

let rec ( @> ) (instrs1 : instr list) (instrs2 : instr list) =
  match instrs1 with
  | [] -> instrs2
  | instr :: [] -> instr @: instrs2
  | instr :: rest -> instr @: rest @> instrs2

let local_get (id : string) =
  let id = localidx id in
  Ast.Local_get id

let local_set (id : string) =
  let id = localidx id in
  Ast.Local_set id

let global_get (id : string) =
  let id : Ast.globalidx = { var = Unresolve id } in
  Ast.Global_get id

let generic_get (id : Ident.t) =
  match id with
  | Pident _ | Pmutable_ident _ -> local_get (Ident.to_wasm_name id)
  | Pdot _ -> global_get (Ident.to_wasm_name id)

let global_set (id : string) =
  let id : Ast.globalidx = { var = Unresolve id } in
  Ast.Global_set id

let local_tee (id : string) =
  let id = localidx id in
  Ast.Local_tee id

let call fnname =
  let fnname : Ast.funcidx = { var = Unresolve fnname } in
  Ast.Call fnname

let call_indirect (typ : string) =
  let tableidx : Ast.tableidx = { var = Unresolve Ast.default_table_name } in
  let typ = Ast.Use ({ var = Unresolve typ }, [], []) in
  Ast.Call_indirect (tableidx, typ)

let int_const (i : int) = Ast.I32_const (Int32.of_int i)
let i32_const (i : int32) = Ast.I32_const i
let i64_const (i : int64) = Ast.I64_const i

let float_to_string f =
  let str = Printf.sprintf "%h" f in
  match str with "-infinity" -> "-inf" | "infinity" -> "inf" | s -> s

let f32_const (i : float) = Ast.F32_const (float_to_string i, i)
let f64_const (i : float) = Ast.F64_const (float_to_string i, i)

let mk_memarg ?(offset = 0) ?(align = 0) () =
  ({ offset = Int32.of_int offset; align = Int32.of_int align } : Ast.memarg)

let i32_load ?(offset = 0) ?(align = 0) () =
  Ast.I32_load (mk_memarg ~offset ~align ())

let i32_load8_u ?(offset = 0) ?(align = 0) () =
  Ast.I32_load8_u (mk_memarg ~offset ~align ())
[@@dead "+i32_load8_u"]

let i32_load16_u ?(offset = 0) ?(align = 0) () =
  Ast.I32_load16_u (mk_memarg ~offset ~align ())
[@@dead "+i32_load16_u"]

let i32_load16_s ?(offset = 0) ?(align = 0) () =
  Ast.I32_load16_s (mk_memarg ~offset ~align ())
[@@dead "+i32_load16_u"]

let i32_store ?(offset = 0) ?(align = 0) () =
  Ast.I32_store (mk_memarg ~offset ~align ())

let i32_store8 ?(offset = 0) ?(align = 0) () =
  Ast.I32_store8 (mk_memarg ~offset ~align ())
[@@dead "+i32_store8"]

let i32_store16 ?(offset = 0) ?(align = 0) () =
  Ast.I32_store16 (mk_memarg ~offset ~align ())
[@@dead "+i32_store16"]

let i64_load ?(offset = 0) ?(align = 0) () =
  Ast.I64_load (mk_memarg ~offset ~align ())

let i64_store ?(offset = 0) ?(align = 0) () =
  Ast.I64_store (mk_memarg ~offset ~align ())

let f32_load ?(offset = 0) ?(align = 0) () =
  Ast.F32_load (mk_memarg ~offset ~align ())

let f32_store ?(offset = 0) ?(align = 0) () =
  Ast.F32_store (mk_memarg ~offset ~align ())

let f64_load ?(offset = 0) ?(align = 0) () =
  Ast.F64_load (mk_memarg ~offset ~align ())

let f64_store ?(offset = 0) ?(align = 0) () =
  Ast.F64_store (mk_memarg ~offset ~align ())

let ref_null (t : Ast.valtype) =
  match t with
  | Reftype (Ref (Nullable, ht)) -> Ast.Ref_null ht
  | _ -> assert false

let ref_func addr =
  let fnname : Ast.funcidx =
    { var = Unresolve (Basic_fn_address.to_wasm_name addr) }
  in
  Ast.Ref_func fnname

let if_ ?label ?(result_ty = []) ?condition ~then_ ~else_ () =
  let condition = match condition with None -> [] | Some instrs -> instrs in
  condition
  @> [ Ast.If { label; typeuse = Inline ([], result_ty); then_; else_ } ]

let block ?label ?label_ ?(result_ty = []) instrs =
  let label =
    match label with None -> label_ | Some l -> Some (Label.to_wasm_name l)
  in
  Ast.Block { label; typeuse = Inline ([], result_ty); instrs }

let loop ?label ?label_ ?(params = []) ?(result_ty = []) instrs =
  let label =
    match label with None -> label_ | Some l -> Some (Label.to_wasm_name l)
  in
  Ast.Loop { label; typeuse = Inline (params, result_ty); instrs }

let br label =
  let idx = labelidx label in
  Ast.Br idx

let br_ label = br (Label.to_wasm_name label)

let br_table labels label =
  let label = labelidx (Label.to_wasm_name label) in
  let labels = Lst.map labels (fun l -> labelidx (Label.to_wasm_name l)) in
  Ast.Br_table (labels, label)

let unreachable = Ast.Unreachable

let table_get id =
  let id : Ast.tableidx = { var = Unresolve id } in
  Ast.Table_get id

let catch ~(tag : string) ~(label : Label.t) =
  let labelidx = labelidx (Label.to_wasm_name label) in
  let tagidx : Ast.tagidx = { var = Unresolve tag } in
  Ast.Catch (tagidx, labelidx)

let try_table ?label ?result_ty ?(catchs = []) instrs =
  let label =
    match label with None -> None | Some l -> Some (Label.to_wasm_name l)
  in
  let result_ty = match result_ty with None -> [] | Some r -> r in
  Ast.Try_table { label; typeuse = Inline ([], result_ty); catchs; instrs }
