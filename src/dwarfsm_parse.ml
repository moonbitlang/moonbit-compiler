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


module Itype = Dwarfsm_itype
module Lst = Basic_lst
module Ast = Dwarfsm_ast

type binder = Dwarfsm_ast.binder
type heaptype = Dwarfsm_ast.heaptype
type storagetype = Dwarfsm_ast.storagetype
type valtype = Dwarfsm_ast.valtype
type reftype = Dwarfsm_ast.reftype
type fieldtype = Dwarfsm_ast.fieldtype
type field = Dwarfsm_ast.field
type param = Dwarfsm_ast.param
type comptype = Dwarfsm_ast.comptype
type subtype = Dwarfsm_ast.subtype
type typeuse = Dwarfsm_ast.typeuse
type limits = Dwarfsm_ast.limits
type tabletype = Dwarfsm_ast.tabletype
type local = Dwarfsm_ast.local
type memarg = Dwarfsm_ast.memarg
type instr = Dwarfsm_ast.instr
type catch = Dwarfsm_ast.catch
type expr = Dwarfsm_ast.expr
type modulefield = Dwarfsm_ast.modulefield
type importdesc = Dwarfsm_ast.importdesc
type module_ = Dwarfsm_ast.module_

exception Parse_failure of { input : W.t; syntax : string }
exception Unclosed_structured_instr of { instr : string }

let () =
  Printexc.register_printer (function
    | Parse_failure { input; syntax } ->
        Some
          (Stdlib.String.concat ""
             [
               "Dwarfsm parse failure: unable to parse ";
               syntax;
               " with input ";
               W.to_string input;
             ] [@merlin.hide])
    | Unclosed_structured_instr { instr } ->
        Some
          (("Dwarfsm parse failure: unclosed structured instruction " ^ instr
            : Stdlib.String.t)
            [@merlin.hide])
    | _ -> None)

let is_id x = String.starts_with ~prefix:"$" x

let is_nat x =
  String.length x >= 1
  &&
  let c = Char.code x.[0] in
  Char.code '0' <= c && c <= Char.code '9'

let symbol = function
  | W.Atom x -> x
  | w -> raise (Parse_failure { input = w; syntax = "symbol" })

let is_symbol = function W.Atom _ -> true | _ -> false

let decode_string s =
  let b = Buffer.create (String.length s) in
  let i = ref 1 in
  while !i < String.length s - 1 do
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

let string = function
  | W.Atom x -> if String.starts_with ~prefix:"\"" x then decode_string x else x
  | w -> raise (Parse_failure { input = w; syntax = "string" })

let decode_int ~of_string s =
  if
    String.starts_with ~prefix:"0x" s
    || (String.length s >= 1 && (s.[0] = '+' || s.[0] = '-'))
  then of_string s
  else of_string ("0u" ^ s)

let i32 : W.t -> int32 =
 fun w ->
  let s = symbol w in
  decode_int ~of_string:Int32.of_string s

let i64 : W.t -> int64 =
 fun w ->
  let s = symbol w in
  decode_int ~of_string:Int64.of_string s

let f64 : W.t -> float = fun w -> Float.of_string (symbol w)
let u32 : W.t -> int32 = fun w -> Int32.of_string ("0u" ^ symbol w)

let vopt p v =
  match p v with exception Parse_failure _ -> (None, v) | x, v -> (Some x, v)

let repeat_rev p v =
  let rec aux l v =
    match v with
    | [] -> (l, [])
    | x :: v' -> (
        match p x with
        | exception Parse_failure _ -> (l, v)
        | x -> aux (x :: l) v')
  in
  aux [] v

let repeat p v =
  let l, v = repeat_rev p v in
  (List.rev l, v)

let binder : W.t list -> binder * W.t list = function
  | W.Atom x :: v when is_id x -> ({ id = Some x; index = -1 }, v)
  | v -> ({ id = None; index = -1 }, v)

let label : W.t list -> Ast.label * W.t list = function
  | W.Atom x :: v when is_id x -> (Some x, v)
  | v -> (None, v)

let fieldidx v =
  (let var : Ast.var =
     match v with
     | W.Atom x when is_id x -> Unresolve x
     | W.Atom x when is_nat x ->
         Resolved { var_name = ""; index = int_of_string x }
     | w -> raise (Parse_failure { input = w; syntax = "fieldidx" })
   in
   { var }
    : Ast.fieldidx)

let var v =
  (match v with
   | W.Atom x when is_id x -> Unresolve x
   | w -> raise (Parse_failure { input = w; syntax = "var" })
    : Ast.var)

let memidx v = ({ var = var v } : Ast.memidx)
let dataidx v = ({ var = var v } : Ast.dataidx)
let tableidx v = ({ var = var v } : Ast.tableidx)
let funcidx v = ({ var = var v } : Ast.funcidx)
let typeidx v = ({ var = var v } : Ast.typeidx)
let globalidx v = ({ var = var v } : Ast.globalidx)

let localidx v =
  (let var : Ast.var =
     match v with
     | W.Atom x when is_id x -> Unresolve x
     | W.Atom x when is_nat x ->
         Resolved { var_name = ""; index = int_of_string x }
     | w -> raise (Parse_failure { input = w; syntax = "localidx" })
   in
   { var }
    : Ast.localidx)

let labelidx v =
  (let var : Ast.var =
     match v with
     | W.Atom x when is_id x -> Unresolve x
     | W.Atom x when is_nat x ->
         Resolved { var_name = ""; index = int_of_string x }
     | w -> raise (Parse_failure { input = w; syntax = "localidx" })
   in
   { var }
    : Ast.labelidx)

let tagidx v = ({ var = var v } : Ast.tagidx)

let heaptype : W.t -> heaptype = function
  | (Atom "any" : W.t) -> Absheaptype Any
  | (Atom "eq" : W.t) -> Absheaptype Eq
  | (Atom "i31" : W.t) -> Absheaptype I31
  | (Atom "struct" : W.t) -> Absheaptype Struct
  | (Atom "array" : W.t) -> Absheaptype Array
  | (Atom "none" : W.t) -> Absheaptype None
  | (Atom "func" : W.t) -> Absheaptype Func
  | (Atom "nofunc" : W.t) -> Absheaptype NoFunc
  | (Atom "extern" : W.t) -> Absheaptype Extern
  | (Atom "noextern" : W.t) -> Absheaptype NoExtern
  | w -> Type (typeidx w)

let storagetype : W.t -> storagetype = function
  | (Atom "i8" : W.t) -> Packedtype I8
  | (Atom "i16" : W.t) -> Packedtype I16
  | (Atom "i32" : W.t) -> Valtype (Numtype I32)
  | (Atom "i64" : W.t) -> Valtype (Numtype I64)
  | (Atom "f32" : W.t) -> Valtype (Numtype F32)
  | (Atom "f64" : W.t) -> Valtype (Numtype F64)
  | (List [ Atom "ref"; w ] : W.t) ->
      Valtype (Reftype (Ref (NonNull, heaptype w)))
  | (List [ Atom "ref"; Atom "null"; w ] : W.t) ->
      Valtype (Reftype (Ref (Nullable, heaptype w)))
  | (Atom "anyref" : W.t) -> Valtype (Reftype (Ref (Nullable, Absheaptype Any)))
  | (Atom "i31ref" : W.t) -> Valtype (Reftype (Ref (Nullable, Absheaptype I31)))
  | (Atom "structref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype Struct)))
  | (Atom "arrayref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype Array)))
  | (Atom "nullref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype None)))
  | (Atom "funcref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype Func)))
  | (Atom "nullfuncref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype NoFunc)))
  | (Atom "externref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype Extern)))
  | (Atom "nullexternref" : W.t) ->
      Valtype (Reftype (Ref (Nullable, Absheaptype NoExtern)))
  | w -> raise (Parse_failure { input = w; syntax = "storagetype" })

let valtype : W.t -> valtype = function
  | (Atom "i32" : W.t) -> Numtype I32
  | (Atom "i64" : W.t) -> Numtype I64
  | (Atom "f32" : W.t) -> Numtype F32
  | (Atom "f64" : W.t) -> Numtype F64
  | (Atom "v128" : W.t) -> Vectype V128
  | (List [ Atom "ref"; w ] : W.t) -> Reftype (Ref (NonNull, heaptype w))
  | (List [ Atom "ref"; Atom "null"; w ] : W.t) ->
      Reftype (Ref (Nullable, heaptype w))
  | (Atom "anyref" : W.t) -> Reftype (Ref (Nullable, Absheaptype Any))
  | (Atom "i31ref" : W.t) -> Reftype (Ref (Nullable, Absheaptype I31))
  | (Atom "structref" : W.t) -> Reftype (Ref (Nullable, Absheaptype Struct))
  | (Atom "arrayref" : W.t) -> Reftype (Ref (Nullable, Absheaptype Array))
  | (Atom "nullref" : W.t) -> Reftype (Ref (Nullable, Absheaptype None))
  | (Atom "funcref" : W.t) -> Reftype (Ref (Nullable, Absheaptype Func))
  | (Atom "nullfuncref" : W.t) -> Reftype (Ref (Nullable, Absheaptype NoFunc))
  | (Atom "externref" : W.t) -> Reftype (Ref (Nullable, Absheaptype Extern))
  | (Atom "nullexternref" : W.t) ->
      Reftype (Ref (Nullable, Absheaptype NoExtern))
  | w -> raise (Parse_failure { input = w; syntax = "valtype" })

let reftype : W.t -> reftype = function
  | (List [ Atom "ref"; w ] : W.t) -> Ref (NonNull, heaptype w)
  | (List [ Atom "ref"; Atom "null"; w ] : W.t) -> Ref (Nullable, heaptype w)
  | (Atom "anyref" : W.t) -> Ref (Nullable, Absheaptype Any)
  | (Atom "i31ref" : W.t) -> Ref (Nullable, Absheaptype I31)
  | (Atom "structref" : W.t) -> Ref (Nullable, Absheaptype Struct)
  | (Atom "arrayref" : W.t) -> Ref (Nullable, Absheaptype Array)
  | (Atom "nullref" : W.t) -> Ref (Nullable, Absheaptype None)
  | (Atom "funcref" : W.t) -> Ref (Nullable, Absheaptype Func)
  | (Atom "nullfuncref" : W.t) -> Ref (Nullable, Absheaptype NoFunc)
  | (Atom "externref" : W.t) -> Ref (Nullable, Absheaptype Extern)
  | (Atom "nullexternref" : W.t) -> Ref (Nullable, Absheaptype NoExtern)
  | w -> raise (Parse_failure { input = w; syntax = "reftype" })

let mut p =
  (function (List [ Atom "mut"; w ] : W.t) -> (Var, p w) | w -> (Const, p w)
    : W.t -> Ast.mut * 'a)

let source_name_opt = function
  | (List [ Atom "source_name"; w ] : W.t) :: v -> (Some (string w), v)
  | v -> (None, v)

let source_type_inner : W.t -> Itype.t = function
  | (Atom "int" : W.t) -> Int
  | (Atom "uint" : W.t) -> Uint
  | (Atom "char" : W.t) -> Char
  | (Atom "bool" : W.t) -> Bool
  | (Atom "unit" : W.t) -> Unit
  | (Atom "byte" : W.t) -> Byte
  | (Atom "int16" : W.t) -> Int16
  | (Atom "uint16" : W.t) -> UInt16
  | (Atom "int64" : W.t) -> Int64
  | (Atom "uint64" : W.t) -> UInt64
  | (Atom "float" : W.t) -> Float
  | (Atom "double" : W.t) -> Double
  | w -> raise (Parse_failure { input = w; syntax = "source_type" })

let source_type_opt = function
  | (List [ Atom "source_type"; w ] : W.t) :: v ->
      (Some (source_type_inner w), v)
  | v -> (None, v)

let fieldtype : W.t -> fieldtype =
 fun w ->
  let mut, type_ = mut storagetype w in
  { mut; type_ }

let assert_list_singleton xs fail = match xs with x :: [] -> x | _ -> fail ()
[@@inline always]

let assert_list_empty xs fail = match xs with [] -> () | _ -> fail ()
[@@inline always]

let assert_list_nonempty xs fail =
  match xs with x :: xs -> (x, xs) | _ -> fail ()
[@@inline always]

let field : W.t -> field =
 fun w ->
  let fail () =
    raise (Parse_failure { input = w; syntax = "field" })
      [@@local]
  in
  match w with
  | (List [ Atom "field"; name; ty ] : W.t) -> (
      match name with
      | Atom x when is_id x ->
          { id = { id = Some x; index = -1 }; fieldtype = fieldtype ty }
      | _ -> fail ())
  | (List [ Atom "field"; ty ] : W.t) ->
      { id = { id = None; index = -1 }; fieldtype = fieldtype ty }
  | _ -> fail ()

let rec params : W.t list -> param list * W.t list =
 fun v ->
  match v with
  | ((List (Atom "param" :: v) : W.t) as w) :: v2 -> (
      let tail, v3 = params v2 in
      let id, v = binder v in
      match id.id with
      | Some _ ->
          let source_name, v = source_name_opt v in
          let source_type, v = source_type_opt v in
          let fail () =
            raise (Parse_failure { input = w; syntax = "params" })
              [@@inline]
          in
          let w = assert_list_singleton v fail in
          let type_ = valtype w in
          ({ id; source_name; type_; source_type } :: tail, v3)
      | None ->
          let rec go tail v =
            match v with
            | w :: v ->
                let ty = valtype w in
                let param : param =
                  { id; source_name = None; type_ = ty; source_type = None }
                in
                go (param :: tail) v
            | [] -> tail
          in
          (go tail (List.rev v), v3))
  | v -> ([], v)

let result : W.t -> valtype =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "result" }) in
  match w with
  | (List (Atom "result" :: v) : W.t) ->
      let w = assert_list_singleton v fail in
      valtype w
  | _ -> fail ()

let comptype : W.t -> comptype = function
  | (List [ Atom "array"; w ] : W.t) -> Arraytype (Array (fieldtype w))
  | (List (Atom "struct" :: v) : W.t) -> Structtype (Struct (Lst.map v field))
  | (List (Atom "func" :: v) : W.t) ->
      let params, v = params v in
      let results = Lst.map v result in
      Functype (Func (params, results))
  | w -> raise (Parse_failure { input = w; syntax = "comptype" })

let rec subtype : W.t -> subtype =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "subtype" }) in
  match w with
  | (List (Atom "sub" :: v) : W.t) ->
      let final, v = final v in
      let super, v = repeat typeidx v in
      let w = assert_list_singleton v fail in
      { final; super; type_ = comptype w }
  | w -> { final = true; super = []; type_ = comptype w }

and final : W.t list -> bool * W.t list = function
  | (Atom "final" : W.t) :: v -> (true, v)
  | v -> (false, v)

let typedef : W.t -> binder * subtype =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "typedef" }) in
  match w with
  | (List (Atom "type" :: v) : W.t) ->
      let id, v = binder v in
      let w = assert_list_singleton v fail in
      (id, subtype w)
  | _ -> fail ()

let typeuse : W.t list -> typeuse * W.t list = function
  | (List [ Atom "type"; w ] : W.t) :: v ->
      let idx = typeidx w in
      let params, v = params v in
      let results, v = repeat result v in
      (Use (idx, params, results), v)
  | v ->
      let params, v = params v in
      let results, v = repeat result v in
      (Inline (params, results), v)

let limits : W.t list -> limits * W.t list = function
  | w :: w2 :: v when is_nat (symbol w) && is_nat (symbol w2) ->
      ({ min = u32 w; max = Some (u32 w2) }, v)
  | w :: v when is_nat (symbol w) -> ({ min = u32 w; max = None }, v)
  | v -> raise (Parse_failure { input = W.List v; syntax = "limits" })

let shared : W.t list -> bool * W.t list = function
  | (Atom "shared" : W.t) :: v -> (true, v)
  | (Atom "unshared" : W.t) :: v -> (false, v)
  | v -> (false, v)

let local : W.t -> local =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "local" }) in
  match w with
  | (List (Atom "local" :: v) : W.t) ->
      let id, v = binder v in
      let source_name, v = source_name_opt v in
      let source_type, w = source_type_opt v in
      let w = assert_list_singleton w fail in
      let type_ = valtype w in
      { id; source_name; type_; source_type }
  | _ -> fail ()

let offset_opt : W.t list -> int32 option * W.t list = function
  | Atom s :: v when String.starts_with ~prefix:"offset=" s ->
      let lit = String.sub s 7 (String.length s - 7) in
      (Some (Int32.of_string ("0u" ^ lit)), v)
  | v -> (None, v)

let int32_log2 (i : int32) =
  (match i with
   | 1l -> 0l
   | 2l -> 1l
   | 4l -> 2l
   | 8l -> 3l
   | 16l -> 4l
   | 32l -> 5l
   | 64l -> 6l
   | 128l -> 7l
   | 256l -> 8l
   | 512l -> 9l
   | 1024l -> 10l
   | 2048l -> 11l
   | 4096l -> 12l
   | _ -> assert false
    : int32)

let align_opt : W.t list -> int32 option * W.t list = function
  | Atom s :: v when String.starts_with ~prefix:"align=" s ->
      let lit = String.sub s 6 (String.length s - 6) in
      (Some (int32_log2 (Int32.of_string ("0u" ^ lit))), v)
  | v -> (None, v)

let memarg : W.t list -> memarg * W.t list =
 fun v ->
  let offset, v = offset_opt v in
  let align, v = align_opt v in
  let offset = Option.value ~default:0l offset in
  let align = Option.value ~default:0l align in
  ({ offset; align }, v)

let memarg_n : int32 -> W.t list -> memarg * W.t list =
 fun n ->
  fun v ->
   let offset, v = offset_opt v in
   let align, v = align_opt v in
   let offset = Option.value ~default:0l offset in
   let align = Option.value ~default:n align in
   ({ offset; align }, v)

let rec plaininstr : W.t list -> instr * W.t list = function
  | (Atom "any.convert_extern" : W.t) :: v -> (Any_convert_extern, v)
  | (Atom "array.copy" : W.t) :: (w : W.t) :: (w2 : W.t) :: v ->
      (Array_copy (typeidx w, typeidx w2), v)
  | (Atom "array.fill" : W.t) :: (w : W.t) :: v -> (Array_fill (typeidx w), v)
  | (Atom "array.get" : W.t) :: (w : W.t) :: v -> (Array_get (typeidx w), v)
  | (Atom "array.get_s" : W.t) :: (w : W.t) :: v -> (Array_get_s (typeidx w), v)
  | (Atom "array.get_u" : W.t) :: (w : W.t) :: v -> (Array_get_u (typeidx w), v)
  | (Atom "array.len" : W.t) :: v -> (Array_len, v)
  | (Atom "array.new" : W.t) :: (w : W.t) :: v -> (Array_new (typeidx w), v)
  | (Atom "array.new_data" : W.t) :: (w : W.t) :: (w2 : W.t) :: v ->
      (Array_new_data (typeidx w, dataidx w2), v)
  | (Atom "array.new_fixed" : W.t) :: (w : W.t) :: (w2 : W.t) :: v ->
      (Array_new_fixed (typeidx w, u32 w2), v)
  | (Atom "array.new_default" : W.t) :: (w : W.t) :: v ->
      (Array_new_default (typeidx w), v)
  | (Atom "array.set" : W.t) :: (w : W.t) :: v -> (Array_set (typeidx w), v)
  | (Atom "block" : W.t) :: v ->
      let label, v = label v in
      let type_, v = typeuse v in
      let (instrs, v), end_ =
        instrlist ?label ~allow_else:false ~allow_end:true v
      in
      let () =
        match end_ with
        | `Else -> assert false
        | `End -> ()
        | `Nil -> raise (Unclosed_structured_instr { instr = "block" })
      in
      (Block { label; typeuse = type_; instrs }, v)
  | (Atom "br" : W.t) :: (w : W.t) :: v -> (Br (labelidx w), v)
  | (Atom "br_if" : W.t) :: (w : W.t) :: v -> (Br_if (labelidx w), v)
  | (Atom "br_table" : W.t) :: v ->
      let labels, v = repeat labelidx v in
      let labels, last_label = Basic_lst.split_at_last labels in
      (Br_table (labels, last_label), v)
  | (Atom "call" : W.t) :: (w : W.t) :: v -> (Call (funcidx w), v)
  | (Atom "call_indirect" : W.t) :: (w : W.t) :: v when is_symbol w ->
      let type_, v = typeuse v in
      (Call_indirect (tableidx w, type_), v)
  | (Atom "call_indirect" : W.t) :: v ->
      let type_, v = typeuse v in
      (Call_indirect ({ var = Unresolve Ast.default_table_name }, type_), v)
  | (Atom "call_ref" : W.t) :: (w : W.t) :: v -> (Call_ref (typeidx w), v)
  | (Atom "drop" : W.t) :: v -> (Drop, v)
  | (Atom "extern.convert_any" : W.t) :: v -> (Extern_convert_any, v)
  | (Atom "f64.add" : W.t) :: v -> (F64_add, v)
  | (Atom "f64.const" : W.t) :: (w : W.t) :: v ->
      (F64_const (string w, f64 w), v)
  | (Atom "f64.convert_i32_s" : W.t) :: v -> (F64_convert_i32_s, v)
  | (Atom "f64.convert_i32_u" : W.t) :: v -> (F64_convert_i32_u, v)
  | (Atom "f64.convert_i64_s" : W.t) :: v -> (F64_convert_i64_s, v)
  | (Atom "f64.convert_i64_u" : W.t) :: v -> (F64_convert_i64_u, v)
  | (Atom "f64.div" : W.t) :: v -> (F64_div, v)
  | (Atom "f64.eq" : W.t) :: v -> (F64_eq, v)
  | (Atom "f64.ge" : W.t) :: v -> (F64_ge, v)
  | (Atom "f64.gt" : W.t) :: v -> (F64_gt, v)
  | (Atom "f64.le" : W.t) :: v -> (F64_le, v)
  | (Atom "f64.load" : W.t) :: v ->
      let memarg, v = memarg v in
      (F64_load memarg, v)
  | (Atom "f64.lt" : W.t) :: v -> (F64_lt, v)
  | (Atom "f64.mul" : W.t) :: v -> (F64_mul, v)
  | (Atom "f64.ne" : W.t) :: v -> (F64_ne, v)
  | (Atom "f64.neg" : W.t) :: v -> (F64_neg, v)
  | (Atom "f64.reinterpret_i64" : W.t) :: v -> (F64_reinterpret_i64, v)
  | (Atom "f64.store" : W.t) :: v ->
      let memarg, v = memarg v in
      (F64_store memarg, v)
  | (Atom "f64.sub" : W.t) :: v -> (F64_sub, v)
  | (Atom "f64.sqrt" : W.t) :: v -> (F64_sqrt, v)
  | (Atom "f64.abs" : W.t) :: v -> (F64_abs, v)
  | (Atom "f64.ceil" : W.t) :: v -> (F64_ceil, v)
  | (Atom "f64.floor" : W.t) :: v -> (F64_floor, v)
  | (Atom "f64.trunc" : W.t) :: v -> (F64_trunc, v)
  | (Atom "f64.nearest" : W.t) :: v -> (F64_nearest, v)
  | (Atom "f32.add" : W.t) :: v -> (F32_add, v)
  | (Atom "f32.const" : W.t) :: (w : W.t) :: v ->
      (F32_const (string w, f64 w), v)
  | (Atom "f32.convert_i32_s" : W.t) :: v -> (F32_convert_i32_s, v)
  | (Atom "f32.convert_i32_u" : W.t) :: v -> (F32_convert_i32_u, v)
  | (Atom "f32.convert_i64_s" : W.t) :: v -> (F32_convert_i64_s, v)
  | (Atom "f32.convert_i64_u" : W.t) :: v -> (F32_convert_i64_u, v)
  | (Atom "f32.demote_f64" : W.t) :: v -> (F32_demote_f64, v)
  | (Atom "f32.div" : W.t) :: v -> (F32_div, v)
  | (Atom "f32.eq" : W.t) :: v -> (F32_eq, v)
  | (Atom "f32.ge" : W.t) :: v -> (F32_ge, v)
  | (Atom "f32.gt" : W.t) :: v -> (F32_gt, v)
  | (Atom "f32.le" : W.t) :: v -> (F32_le, v)
  | (Atom "f32.load" : W.t) :: v ->
      let memarg, v = memarg v in
      (F32_load memarg, v)
  | (Atom "f32.lt" : W.t) :: v -> (F32_lt, v)
  | (Atom "f32.mul" : W.t) :: v -> (F32_mul, v)
  | (Atom "f32.ne" : W.t) :: v -> (F32_ne, v)
  | (Atom "f32.neg" : W.t) :: v -> (F32_neg, v)
  | (Atom "f32.reinterpret_i32" : W.t) :: v -> (F32_reinterpret_i32, v)
  | (Atom "f32.sqrt" : W.t) :: v -> (F32_sqrt, v)
  | (Atom "f32.store" : W.t) :: v ->
      let memarg, v = memarg v in
      (F32_store memarg, v)
  | (Atom "f32.sub" : W.t) :: v -> (F32_sub, v)
  | (Atom "f32.abs" : W.t) :: v -> (F32_abs, v)
  | (Atom "f32.ceil" : W.t) :: v -> (F32_ceil, v)
  | (Atom "f32.floor" : W.t) :: v -> (F32_floor, v)
  | (Atom "f32.trunc" : W.t) :: v -> (F32_trunc, v)
  | (Atom "f32.nearest" : W.t) :: v -> (F32_nearest, v)
  | (Atom "f64.promote_f32" : W.t) :: v -> (F64_promote_f32, v)
  | (Atom "i32.reinterpret_f32" : W.t) :: v -> (I32_reinterpret_f32, v)
  | (Atom "i32.trunc_f32_s" : W.t) :: v -> (I32_trunc_f32_s, v)
  | (Atom "i32.trunc_f32_u" : W.t) :: v -> (I32_trunc_f32_u, v)
  | (Atom "i64.trunc_f32_s" : W.t) :: v -> (I64_trunc_f32_s, v)
  | (Atom "i64.trunc_f32_u" : W.t) :: v -> (I64_trunc_f32_u, v)
  | (Atom "global.get" : W.t) :: (w : W.t) :: v -> (Global_get (globalidx w), v)
  | (Atom "global.set" : W.t) :: (w : W.t) :: v -> (Global_set (globalidx w), v)
  | (Atom "i32.add" : W.t) :: v -> (I32_add, v)
  | (Atom "i32.and" : W.t) :: v -> (I32_and, v)
  | (Atom "i32.clz" : W.t) :: v -> (I32_clz, v)
  | (Atom "i32.const" : W.t) :: (w : W.t) :: v -> (I32_const (i32 w), v)
  | (Atom "i32.ctz" : W.t) :: v -> (I32_ctz, v)
  | (Atom "i32.div_s" : W.t) :: v -> (I32_div_s, v)
  | (Atom "i32.div_u" : W.t) :: v -> (I32_div_u, v)
  | (Atom "i32.eq" : W.t) :: v -> (I32_eq, v)
  | (Atom "i32.eqz" : W.t) :: v -> (I32_eqz, v)
  | (Atom "i32.ge_s" : W.t) :: v -> (I32_ge_s, v)
  | (Atom "i32.ge_u" : W.t) :: v -> (I32_ge_u, v)
  | (Atom "i32.gt_s" : W.t) :: v -> (I32_gt_s, v)
  | (Atom "i32.gt_u" : W.t) :: v -> (I32_gt_u, v)
  | (Atom "i32.le_s" : W.t) :: v -> (I32_le_s, v)
  | (Atom "i32.le_u" : W.t) :: v -> (I32_le_u, v)
  | (Atom "i32.load" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_load memarg, v)
  | (Atom "i32.load16_u" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_load16_u memarg, v)
  | (Atom "i32.load8_u" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_load8_u memarg, v)
  | (Atom "i32.load8_s" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_load8_s memarg, v)
  | (Atom "i32.load16_s" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_load16_s memarg, v)
  | (Atom "i32.lt_s" : W.t) :: v -> (I32_lt_s, v)
  | (Atom "i32.lt_u" : W.t) :: v -> (I32_lt_u, v)
  | (Atom "i32.mul" : W.t) :: v -> (I32_mul, v)
  | (Atom "i32.ne" : W.t) :: v -> (I32_ne, v)
  | (Atom "i32.or" : W.t) :: v -> (I32_or, v)
  | (Atom "i32.popcnt" : W.t) :: v -> (I32_popcnt, v)
  | (Atom "i32.rem_s" : W.t) :: v -> (I32_rem_s, v)
  | (Atom "i32.rem_u" : W.t) :: v -> (I32_rem_u, v)
  | (Atom "i32.shl" : W.t) :: v -> (I32_shl, v)
  | (Atom "i32.rotl" : W.t) :: v -> (I32_rotl, v)
  | (Atom "i32.shr_s" : W.t) :: v -> (I32_shr_s, v)
  | (Atom "i32.shr_u" : W.t) :: v -> (I32_shr_u, v)
  | (Atom "i32.store" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_store memarg, v)
  | (Atom "i32.store8" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_store8 memarg, v)
  | (Atom "i32.store16" : W.t) :: v ->
      let memarg, v = memarg v in
      (I32_store16 memarg, v)
  | (Atom "i32.sub" : W.t) :: v -> (I32_sub, v)
  | (Atom "i32.wrap_i64" : W.t) :: v -> (I32_wrap_i64, v)
  | (Atom "i32.trunc_f64_s" : W.t) :: v -> (I32_trunc_f64_s, v)
  | (Atom "i32.trunc_f64_u" : W.t) :: v -> (I32_trunc_f64_u, v)
  | (Atom "i32.xor" : W.t) :: v -> (I32_xor, v)
  | (Atom "i32.extend8_s" : W.t) :: v -> (I32_extend_8_s, v)
  | (Atom "i32.extend16_s" : W.t) :: v -> (I32_extend_16_s, v)
  | (Atom "i32.trunc_sat_f32_s" : W.t) :: v -> (I32_trunc_sat_f32_s, v)
  | (Atom "i32.trunc_sat_f32_u" : W.t) :: v -> (I32_trunc_sat_f32_u, v)
  | (Atom "i32.trunc_sat_f64_s" : W.t) :: v -> (I32_trunc_sat_f64_s, v)
  | (Atom "i32.trunc_sat_f64_u" : W.t) :: v -> (I32_trunc_sat_f64_u, v)
  | (Atom "i32.atomic.load" : W.t) :: v ->
      let memarg, v = memarg_n 2l v in
      (I32_atomic_load memarg, v)
  | (Atom "i32.atomic.load8_u" : W.t) :: v ->
      let memarg, v = memarg_n 0l v in
      (I32_atomic_load8_u memarg, v)
  | (Atom "i32.atomic.load16_u" : W.t) :: v ->
      let memarg, v = memarg_n 1l v in
      (I32_atomic_load16_u memarg, v)
  | (Atom "i32.atomic.store" : W.t) :: v ->
      let memarg, v = memarg_n 2l v in
      (I32_atomic_store memarg, v)
  | (Atom "i32.atomic.store8" : W.t) :: v ->
      let memarg, v = memarg_n 0l v in
      (I32_atomic_store8 memarg, v)
  | (Atom "i32.atomic.store16" : W.t) :: v ->
      let memarg, v = memarg_n 1l v in
      (I32_atomic_store16 memarg, v)
  | (Atom "i32.atomic.rmw.cmpxchg" : W.t) :: v ->
      let memarg, v = memarg_n 2l v in
      (I32_atomic_rmw_cmpxchg memarg, v)
  | (Atom "i32.atomic.rmw8.cmpxchg_u" : W.t) :: v ->
      let memarg, v = memarg_n 0l v in
      (I32_atomic_rmw8_cmpxchg_u memarg, v)
  | (Atom "i32.atomic.rmw16.cmpxchg_u" : W.t) :: v ->
      let memarg, v = memarg_n 1l v in
      (I32_atomic_rmw16_cmpxchg_u memarg, v)
  | (Atom "i64.add" : W.t) :: v -> (I64_add, v)
  | (Atom "i64.and" : W.t) :: v -> (I64_and, v)
  | (Atom "i64.clz" : W.t) :: v -> (I64_clz, v)
  | (Atom "i64.const" : W.t) :: (w : W.t) :: v -> (I64_const (i64 w), v)
  | (Atom "i64.ctz" : W.t) :: v -> (I64_ctz, v)
  | (Atom "i64.div_s" : W.t) :: v -> (I64_div_s, v)
  | (Atom "i64.div_u" : W.t) :: v -> (I64_div_u, v)
  | (Atom "i64.eq" : W.t) :: v -> (I64_eq, v)
  | (Atom "i64.extend_i32_s" : W.t) :: v -> (I64_extend_i32_s, v)
  | (Atom "i64.extend_i32_u" : W.t) :: v -> (I64_extend_i32_u, v)
  | (Atom "i64.ge_s" : W.t) :: v -> (I64_ge_s, v)
  | (Atom "i64.gt_s" : W.t) :: v -> (I64_gt_s, v)
  | (Atom "i64.le_s" : W.t) :: v -> (I64_le_s, v)
  | (Atom "i64.le_u" : W.t) :: v -> (I64_le_u, v)
  | (Atom "i64.ge_u" : W.t) :: v -> (I64_ge_u, v)
  | (Atom "i64.gt_u" : W.t) :: v -> (I64_gt_u, v)
  | (Atom "i64.load" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load memarg, v)
  | (Atom "i64.load32_u" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load32_u memarg, v)
  | (Atom "i64.load32_s" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load32_s memarg, v)
  | (Atom "i64.load16_u" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load16_u memarg, v)
  | (Atom "i64.load16_s" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load16_s memarg, v)
  | (Atom "i64.load8_u" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load8_u memarg, v)
  | (Atom "i64.load8_s" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_load8_s memarg, v)
  | (Atom "i64.lt_s" : W.t) :: v -> (I64_lt_s, v)
  | (Atom "i64.lt_u" : W.t) :: v -> (I64_lt_u, v)
  | (Atom "i64.mul" : W.t) :: v -> (I64_mul, v)
  | (Atom "i64.ne" : W.t) :: v -> (I64_ne, v)
  | (Atom "i64.or" : W.t) :: v -> (I64_or, v)
  | (Atom "i64.popcnt" : W.t) :: v -> (I64_popcnt, v)
  | (Atom "i64.reinterpret_f64" : W.t) :: v -> (I64_reinterpret_f64, v)
  | (Atom "i64.rem_s" : W.t) :: v -> (I64_rem_s, v)
  | (Atom "i64.rem_u" : W.t) :: v -> (I64_rem_u, v)
  | (Atom "i64.shl" : W.t) :: v -> (I64_shl, v)
  | (Atom "i64.shr_s" : W.t) :: v -> (I64_shr_s, v)
  | (Atom "i64.shr_u" : W.t) :: v -> (I64_shr_u, v)
  | (Atom "i64.store" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_store memarg, v)
  | (Atom "i64.store8" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_store8 memarg, v)
  | (Atom "i64.store16" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_store16 memarg, v)
  | (Atom "i64.store32" : W.t) :: v ->
      let memarg, v = memarg v in
      (I64_store32 memarg, v)
  | (Atom "i64.sub" : W.t) :: v -> (I64_sub, v)
  | (Atom "i64.trunc_f64_s" : W.t) :: v -> (I64_trunc_f64_s, v)
  | (Atom "i64.trunc_f64_u" : W.t) :: v -> (I64_trunc_f64_u, v)
  | (Atom "i64.xor" : W.t) :: v -> (I64_xor, v)
  | (Atom "i64.extend8_s" : W.t) :: v -> (I64_extend_8_s, v)
  | (Atom "i64.extend16_s" : W.t) :: v -> (I64_extend_16_s, v)
  | (Atom "i64.extend32_s" : W.t) :: v -> (I64_extend_32_s, v)
  | (Atom "i64.trunc_sat_f32_s" : W.t) :: v -> (I64_trunc_sat_f32_s, v)
  | (Atom "i64.trunc_sat_f32_u" : W.t) :: v -> (I64_trunc_sat_f32_u, v)
  | (Atom "i64.trunc_sat_f64_s" : W.t) :: v -> (I64_trunc_sat_f64_s, v)
  | (Atom "i64.trunc_sat_f64_u" : W.t) :: v -> (I64_trunc_sat_f64_u, v)
  | (Atom "v128.load" : W.t) :: v ->
      let memarg, v = memarg v in
      (V128_load memarg, v)
  | (Atom "v128.store" : W.t) :: v ->
      let memarg, v = memarg v in
      (V128_store memarg, v)
  | (Atom "f64x2.add" : W.t) :: v -> (F64x2_add, v)
  | (Atom "f64x2.mul" : W.t) :: v -> (F64x2_mul, v)
  | (Atom "f32x4.add" : W.t) :: v -> (F32x4_add, v)
  | (Atom "f32x4.mul" : W.t) :: v -> (F32x4_mul, v)
  | (Atom "if" : W.t) :: v -> (
      let label, v = label v in
      let type_, v = typeuse v in
      let (instrs, v), (end_ : [ `Else | `End | `Nil ]) =
        instrlist ?label ~allow_else:true ~allow_end:true v
      in
      match end_ with
      | `Else ->
          let (instrs2, v), end_ =
            instrlist ?label ~allow_else:false ~allow_end:true v
          in
          let () =
            match end_ with
            | `Else -> assert false
            | `End -> ()
            | `Nil -> raise (Unclosed_structured_instr { instr = "if" })
          in
          (If { label; typeuse = type_; then_ = instrs; else_ = instrs2 }, v)
      | `End -> (If { label; typeuse = type_; then_ = instrs; else_ = [] }, v)
      | `Nil -> raise (Unclosed_structured_instr { instr = "if" }))
  | (Atom "local.get" : W.t) :: (w : W.t) :: v -> (Local_get (localidx w), v)
  | (Atom "local.set" : W.t) :: (w : W.t) :: v -> (Local_set (localidx w), v)
  | (Atom "local.tee" : W.t) :: (w : W.t) :: v -> (Local_tee (localidx w), v)
  | (Atom "loop" : W.t) :: v ->
      let label, v = label v in
      let type_, v = typeuse v in
      let (instrs, v), _end =
        instrlist ?label ~allow_else:false ~allow_end:true v
      in
      let () =
        match _end with
        | `Else -> assert false
        | `End -> ()
        | `Nil -> raise (Unclosed_structured_instr { instr = "loop" })
      in
      (Loop { label; typeuse = type_; instrs }, v)
  | (Atom "memory.init" : W.t) :: (w : W.t) :: v -> (Memory_init (dataidx w), v)
  | (Atom "memory.copy" : W.t) :: v -> (Memory_copy, v)
  | (Atom "memory.grow" : W.t) :: v -> (Memory_grow, v)
  | (Atom "memory.size" : W.t) :: v -> (Memory_size, v)
  | (Atom "memory.fill" : W.t) :: v -> (Memory_fill, v)
  | (Atom "memory.atomic.wait32" : W.t) :: v ->
      let memarg, v = memarg_n 2l v in
      (Memory_atomic_wait32 memarg, v)
  | (Atom "memory.atomic.notify" : W.t) :: v ->
      let memarg, v = memarg_n 2l v in
      (Memory_atomic_notify memarg, v)
  | (Atom "ref.eq" : W.t) :: v -> (Ref_eq, v)
  | (Atom "ref.as_non_null" : W.t) :: v -> (Ref_as_non_null, v)
  | (Atom "ref.cast" : W.t) :: (w : W.t) :: v -> (Ref_cast (reftype w), v)
  | (Atom "ref.func" : W.t) :: (w : W.t) :: v -> (Ref_func (funcidx w), v)
  | (Atom "ref.is_null" : W.t) :: v -> (Ref_is_null, v)
  | (Atom "ref.null" : W.t) :: (w : W.t) :: v -> (Ref_null (heaptype w), v)
  | (Atom "return" : W.t) :: v -> (Return, v)
  | (Atom "struct.get" : W.t) :: (w : W.t) :: (w2 : W.t) :: v ->
      (Struct_get (typeidx w, fieldidx w2), v)
  | (Atom "struct.new" : W.t) :: (w : W.t) :: v -> (Struct_new (typeidx w), v)
  | (Atom "struct.new_default" : W.t) :: (w : W.t) :: v ->
      (Struct_new_default (typeidx w), v)
  | (Atom "struct.set" : W.t) :: (w : W.t) :: (w2 : W.t) :: v ->
      (Struct_set (typeidx w, fieldidx w2), v)
  | (Atom "table.get" : W.t) :: (w : W.t) :: v -> (Table_get (tableidx w), v)
  | (Atom "unreachable" : W.t) :: v -> (Unreachable, v)
  | (Atom "throw" : W.t) :: (w : W.t) :: v -> (Throw (tagidx w), v)
  | (Atom "select" : W.t) :: v -> (Select, v)
  | (Atom "nop" : W.t) :: v -> (No_op, v)
  | (Atom "source_pos" : W.t)
    :: (w : W.t)
    :: (w1 : W.t)
    :: (w2 : W.t)
    :: (w3 : W.t)
    :: v ->
      ( Source_pos
          {
            pkg = string w;
            file = string w1;
            line = int_of_string ("0u" ^ symbol w2);
            col = int_of_string ("0u" ^ symbol w3);
          },
        v )
  | (Atom "prologue_end" : W.t) :: v -> (Prologue_end, v)
  | v -> raise (Parse_failure { input = W.List v; syntax = "plaininstr" })

and foldedinstr : W.t -> instr list = function
  | (List (Atom "block" :: v) : W.t) ->
      let label, v = label v in
      let type_, v = typeuse v in
      let instrs = expr v in
      [ Block { label; typeuse = type_; instrs } ]
  | (List (Atom "loop" :: v) : W.t) ->
      let label, v = label v in
      let type_, v = typeuse v in
      let instrs = expr v in
      [ Loop { label; typeuse = type_; instrs } ]
  | (List (Atom "if" :: v) : W.t) as w ->
      let fail () = raise (Parse_failure { input = w; syntax = "if" }) in
      let label, v = label v in
      let type_, v = typeuse v in
      let instrs, v = repeat foldedinstr v in
      let then_, v =
        match v with
        | (List (Atom "then" :: v2) : W.t) :: v -> (expr v2, v)
        | v -> ([], v)
      in
      let else_, v =
        match v with
        | (List (Atom "else" :: v2) : W.t) :: v -> (expr v2, v)
        | v -> ([], v)
      in
      assert_list_empty v fail;
      Lst.concat instrs @ [ If { label; typeuse = type_; then_; else_ } ]
  | (List (Atom "try_table" :: v) : W.t) ->
      let label, v = label v in
      let type_, v = typeuse v in
      let catchs, v = repeat catch v in
      let instrs = expr v in
      [ Try_table { label; typeuse = type_; catchs; instrs } ]
  | (List (w :: v) : W.t) ->
      let instr, v = plaininstr (w :: v) in
      Basic_lst.flat_map_append ~f:foldedinstr v ~init:[ instr ]
  | w -> raise (Parse_failure { input = w; syntax = "foldedinstr" })

and catch : W.t -> catch = function
  | (List [ Atom "catch"; w1; w2 ] : W.t) -> Catch (tagidx w1, labelidx w2)
  | w -> raise (Parse_failure { input = w; syntax = "catch" })

and instrlist :
    ?label:string ->
    allow_else:bool ->
    allow_end:bool ->
    W.t list ->
    (instr list * W.t list) * [ `Else | `End | `Nil ] =
 fun ?label ->
  fun ~allow_else ->
   fun ~allow_end ->
    fun v ->
     let end_ = ref `Nil in
     let rec aux v acc =
       match v with
       | [] ->
           end_ := `Nil;
           (acc, [])
       | (Atom "else" : W.t) :: v when allow_else ->
           end_ := `Else;
           (acc, v)
       | (Atom "end" : W.t) :: v when allow_end ->
           end_ := `End;
           let v =
             match (label, v) with
             | Some l, w :: v when symbol w = l -> v
             | _ -> v
           in
           (acc, v)
       | ((List _ : W.t) as w) :: v ->
           aux v (List.rev_append (foldedinstr w) acc)
       | v ->
           let instr, v = plaininstr v in
           aux v (instr :: acc)
     in
     let instrs, v = aux v [] in
     ((List.rev instrs, v), !end_)

and expr : W.t list -> instr list =
 fun v ->
  let (l, v), end_ = instrlist ~allow_else:false ~allow_end:false v in
  assert (v = []);
  assert (end_ = `Nil);
  l

let offset : W.t -> instr list = function
  | (List (Atom "offset" :: v) : W.t) -> expr v
  | w -> foldedinstr w

let inline_import_opt : W.t list -> (string * string) option * W.t list =
  function
  | (List [ Atom "import"; m; n ] : W.t) :: v -> (Some (string m, string n), v)
  | v -> (None, v)

let inline_export_opt : W.t list -> string option * W.t list = function
  | (List [ Atom "export"; w ] : W.t) :: v -> (Some (string w), v)
  | v -> (None, v)

let elemexpr : W.t -> expr = function
  | (List (Atom "item" :: v) : W.t) -> expr v
  | Atom _ as w ->
      let index = funcidx w in
      [ Ref_func index ]
  | w -> foldedinstr w

let importdesc : W.t -> importdesc =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "importdesc" }) in
  match w with
  | (List (Atom "func" :: v) : W.t) ->
      let id, v = binder v in
      let type_, v = typeuse v in
      assert_list_empty v fail;
      Func (id, type_)
  | (List (Atom "table" :: v) : W.t) ->
      let id, v = binder v in
      let limits, v = limits v in
      let w = assert_list_singleton v fail in
      let element_type = reftype w in
      let type_ : tabletype = { limits; element_type } in
      Table (id, type_)
  | (List (Atom "memory" :: v) : W.t) ->
      let id, v = binder v in
      let limits, v = limits v in
      let shared, v = shared v in
      assert_list_empty v fail;
      Memory (id, { limits; shared })
  | (List (Atom "global" :: v) : W.t) ->
      let id, v = binder v in
      let w = assert_list_singleton v fail in
      let mut, type_ = mut valtype w in
      Global (id, { mut; type_ })
  | (List (Atom "tag" :: v) : W.t) ->
      let id, v = binder v in
      let type_, v = typeuse v in
      assert_list_empty v fail;
      Tag (id, type_)
  | _ -> fail ()

let exportdesc : W.t -> Ast.exportdesc =
 fun w ->
  let fail () = raise (Parse_failure { input = w; syntax = "exportdesc" }) in
  match w with
  | (List [ Atom "func"; w ] : W.t) -> Func (funcidx w)
  | (List [ Atom "table"; w ] : W.t) -> Table (tableidx w)
  | (List [ Atom "memory"; w ] : W.t) -> Memory (memidx w)
  | (List [ Atom "global"; w ] : W.t) -> Global (globalidx w)
  | _ -> fail ()

let modulefield : W.t -> modulefield list = function
  | (List (Atom "rec" :: v) : W.t) -> [ Rectype (Lst.map v typedef) ]
  | (List (Atom "type" :: _) : W.t) as w -> [ Rectype [ typedef w ] ]
  | (List [ Atom "import"; mod_; name; desc ] : W.t) ->
      let desc = importdesc desc in
      [ Import { module_ = string mod_; name = string name; desc } ]
  | (List (Atom "func" :: v) : W.t) as w -> (
      let fail () = raise (Parse_failure { input = w; syntax = "func" }) in
      let id, v = binder v in
      let source_name, v = source_name_opt v in
      let inline_import, v = inline_import_opt v in
      let inline_export, v = inline_export_opt v in
      let type_, v = typeuse v in
      let r =
        match (inline_import, v) with
        | Some (mod_, name), [] ->
            [ Ast.Import { module_ = mod_; name; desc = Func (id, type_) } ]
        | None, v ->
            let locals, v = repeat local v in
            ([
               Func
                 {
                   id;
                   type_;
                   locals;
                   code = expr v;
                   source_name;
                   aux = { low_pc = 0; high_pc = 0 };
                 };
             ]
              : Ast.modulefield list)
        | _ -> fail ()
      in
      match inline_export with
      | Some name -> (
          match[@warning "-fragile-match"] id.id with
          | Some var_name ->
              r @ [ Export { name; desc = Func { var = Unresolve var_name } } ]
          | _ -> assert false)
      | _ -> r)
  | (List (Atom "table" :: v) : W.t) as w -> (
      let fail () = raise (Parse_failure { input = w; syntax = "table" }) in
      let id, v = binder v in
      let limits, v = vopt limits v in
      let w, v = assert_list_nonempty v fail in
      let element_type = reftype w in
      match (limits, v) with
      | Some limits, v ->
          let init = expr v in
          [ Table { id; type_ = { limits; element_type }; init } ]
      | None, (List (Atom "elem" :: v) : W.t) :: [] -> (
          let n = Int32.of_int (List.length v) in
          let limits : limits = { min = n; max = Some n } in
          let list = Lst.map v elemexpr in
          match[@warning "-fragile-match"] id.id with
          | Some name ->
              [
                Table { id; type_ = { limits; element_type }; init = [] };
                Elem
                  {
                    id = { id = None; index = -1 };
                    type_ = element_type;
                    mode =
                      Ast.EMActive ({ var = Unresolve name }, [ I32_const 0l ]);
                    list;
                  };
              ]
          | _ -> assert false)
      | _ -> fail ())
  | (List (Atom "memory" :: v) : W.t) as w ->
      let fail () = raise (Parse_failure { input = w; syntax = "memory" }) in
      let id, v = binder v in
      let inline_export, v = inline_export_opt v in
      let inline_import, v = inline_import_opt v in
      let limits, v = limits v in
      let shared, v = shared v in
      assert_list_empty v fail;
      let import : Ast.modulefield =
        match inline_import with
        | None -> Mem { id; type_ = { limits; shared } }
        | Some (mod_, name) ->
            Import
              { module_ = mod_; name; desc = Memory (id, { limits; shared }) }
      in
      let export : Ast.modulefield list =
        match inline_export with
        | Some name -> (
            match[@warning "-fragile-match"] id.id with
            | Some var_name ->
                [ Export { name; desc = Memory { var = Unresolve var_name } } ]
            | _ -> assert false)
        | _ -> []
      in
      import :: export
  | (List (Atom "global" :: v) : W.t) as w ->
      let fail () = raise (Parse_failure { input = w; syntax = "global" }) in
      let id, v = binder v in
      let inline_export, v = inline_export_opt v in
      let inline_import, v = inline_import_opt v in
      let w, v = assert_list_nonempty v fail in
      let mut, type_ = mut valtype w in
      let export : Ast.modulefield list =
        match inline_export with
        | Some name -> (
            match[@warning "-fragile-match"] id.id with
            | Some var_name ->
                [ Export { name; desc = Global { var = Unresolve var_name } } ]
            | _ -> assert false)
        | _ -> []
      in
      let import : Ast.modulefield =
        match inline_import with
        | None -> Global { id; type_ = { mut; type_ }; init = expr v }
        | Some (mod_, name) ->
            Import { module_ = mod_; name; desc = Global (id, { mut; type_ }) }
      in
      import :: export
  | (List [ Atom "export"; name; desc ] : W.t) ->
      let name = string name in
      let desc = exportdesc desc in
      [ Export { name; desc } ]
  | (List [ Atom "start"; w ] : W.t) -> [ Start (funcidx w) ]
  | (List (Atom "elem" :: v) : W.t) as w ->
      let fail () = raise (Parse_failure { input = w; syntax = "elem" }) in
      let id, v = binder v in
      let mode, v =
        match v with
        | (List [ Atom "table"; w ] : W.t) :: (w2 : W.t) :: v ->
            (Ast.EMActive (tableidx w, offset w2), v)
        | (Atom "declare" : W.t) :: v -> (Ast.EMDeclarative, v)
        | v -> (Ast.EMPassive, v)
      in
      let w, v = assert_list_nonempty v fail in
      let type_ =
        match w with
        | (Atom "func" : W.t) -> Ast.Ref (NonNull, Absheaptype Func)
        | w -> reftype w
      in
      let list = Lst.map v elemexpr in
      [ Elem { id; mode; type_; list } ]
  | (List (Atom "data" :: v) : W.t) as w ->
      let fail () = raise (Parse_failure { input = w; syntax = "data" }) in
      let id, v = binder v in
      let mode, v =
        match v with
        | (List [ Atom "memory"; w ] : W.t) :: (w2 : W.t) :: v ->
            (Ast.DMActive (memidx w, offset w2), v)
        | (List (Atom "offset" :: v2) : W.t) :: v ->
            ( Ast.DMActive ({ var = Unresolve Ast.default_memory_name }, expr v2),
              v )
        | v -> (Ast.DMPassive, v)
      in
      let w = assert_list_singleton v fail in
      let data_str = string w in
      [ Data { id; mode; data_str } ]
  | (List (Atom "tag" :: w :: v) : W.t) as w' ->
      let fail () = raise (Parse_failure { input = w'; syntax = "tag" }) in
      let type_, v = typeuse v in
      assert_list_empty v fail;
      let id, _ = binder [ w ] in
      [ Tag { id; type_ } ]
  | w -> raise (Parse_failure { input = w; syntax = "modulefield" })

let module_ : W.t list -> module_ = function
  | (List (Atom "module" :: v) : W.t) :: [] ->
      let id, v = binder v in
      { id; fields = List.concat_map modulefield v }
  | v ->
      { id = { id = None; index = -1 }; fields = List.concat_map modulefield v }
