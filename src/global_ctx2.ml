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


module Hashset_int = Basic_hashset_int
module Tid = Basic_ty_ident
module Ltype = Ltype_gc
module Vec = Basic_vec
module Lst = Basic_lst
module Fn_addr = Basic_fn_address
module Fn_addr_hashset = Fn_addr.Hashset
module Ast = Dwarfsm_ast

let int_const = Wasmlinear_constr.int_const
and ( @: ) = Wasmlinear_constr.( @: )
and immut = Wasmlinear_constr.immut
and import = Wasmlinear_constr.import
and esc_then_decode = Wasmlinear_constr.esc_then_decode
and global_get = Wasmlinear_constr.global_get
and call = Wasmlinear_constr.call
and binder = Wasmlinear_constr.binder

let global = Wasmgc_constr.global
and ref_ = Wasmgc_constr.ref_
and ref_extern_ = Wasmgc_constr.ref_extern_
and data = Wasmgc_constr.data
and elem_declare_func = Wasmgc_constr.elem_declare_func
and param = Wasmgc_constr.param
and result = Wasmgc_constr.result
and array_new_default = Wasmgc_constr.array_new_default
and array_new_fixed = Wasmgc_constr.array_new_fixed
and array_new_data = Wasmgc_constr.array_new_data
and struct_new = Wasmgc_constr.struct_new
and import_global = Wasmgc_constr.import_global
and global_get_ = Wasmgc_constr.global_get_

let make_string_constant_id index =
  ("$moonbit.js_string_constant." ^ Int.to_string index : Stdlib.String.t)

type default_info = Arr of Tid.t | Enum | Js_string

let string_default_name = "$moonbit.string.default"
let bytes_default_name = "$moonbit.bytes.default"
let enum_default_name = "$moonbit.enum.default"

module Import = struct
  type t = {
    module_name : string;
    func_name : string;
    params_ty : Ltype.t list;
    return_ty : Ltype.t option;
  }

  include struct
    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun {
             module_name = module_name__002_;
             func_name = func_name__004_;
             params_ty = params_ty__006_;
             return_ty = return_ty__008_;
           }
       ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__009_ =
             Moon_sexp_conv.sexp_of_option Ltype.sexp_of_t return_ty__008_
           in
           (S.List [ S.Atom "return_ty"; arg__009_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__007_ =
             Moon_sexp_conv.sexp_of_list Ltype.sexp_of_t params_ty__006_
           in
           (S.List [ S.Atom "params_ty"; arg__007_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_string func_name__004_ in
           (S.List [ S.Atom "func_name"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_string module_name__002_ in
           (S.List [ S.Atom "module_name"; arg__003_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         S.List bnds__001_
        : t -> S.t)

    let _ = sexp_of_t

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
     fun hsv ->
      fun arg ->
       let hsv =
         let hsv =
           let hsv =
             let hsv = hsv in
             Ppx_base.hash_fold_string hsv arg.module_name
           in
           Ppx_base.hash_fold_string hsv arg.func_name
         in
         Ppx_base.hash_fold_list Ltype.hash_fold_t hsv arg.params_ty
       in
       Ppx_base.hash_fold_option Ltype.hash_fold_t hsv arg.return_ty

    let _ = hash_fold_t

    let (hash : t -> Ppx_base.hash_value) =
      let func arg =
        Ppx_base.get_hash_value
          (let hsv = Ppx_base.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x

    let _ = hash

    let equal =
      (fun a__010_ ->
         fun b__011_ ->
          if Stdlib.( == ) a__010_ b__011_ then true
          else
            Stdlib.( && )
              (Stdlib.( = ) (a__010_.module_name : string) b__011_.module_name)
              (Stdlib.( && )
                 (Stdlib.( = ) (a__010_.func_name : string) b__011_.func_name)
                 (Stdlib.( && )
                    (Ppx_base.equal_list
                       (fun a__012_ ->
                         fun b__013_ -> Ltype.equal a__012_ b__013_)
                       a__010_.params_ty b__011_.params_ty)
                    (match (a__010_.return_ty, b__011_.return_ty) with
                    | None, None -> true
                    | None, Some _ -> false
                    | Some _, None -> false
                    | Some __option_x, Some __option_y ->
                        (fun a__014_ ->
                          fun (b__015_ [@merlin.hide]) ->
                           (Ltype.equal a__014_ b__015_ [@merlin.hide]))
                          __option_x __option_y)))
        : t -> t -> bool)

    let _ = equal
  end

  let make_closure : t =
    {
      module_name = "moonbit:ffi";
      func_name = "make_closure";
      params_ty = [ Ref_func; Ref_any ];
      return_ty = Some Ref_extern;
    }
end

module Import_hash = Basic_hashf.Make (Import)

type t = {
  const_table : Const_table.t;
  tags : Hashset_int.t;
  imports : string Import_hash.t;
  inlines : (string * W.t) Vec.t;
  defaults : (string * default_info) Tid.Hash.t;
  func_refs : Fn_addr_hashset.t;
}

let make_constr_id__ (tag : int) = "$moonbit.constr/" ^ Int.to_string tag

let create () =
  {
    const_table = Const_table.create ();
    tags = Hashset_int.create 17;
    imports = Import_hash.create 17;
    inlines = Vec.empty ();
    defaults = Tid.Hash.create 17;
    func_refs = Fn_addr_hashset.create 17;
  }

let compile_string_literal ~(global_ctx : t) (s : string) =
  (if !Basic_config.use_js_builtin_string then
     let index =
       Const_table.find_js_builtin_str_const global_ctx.const_table s
     in
     let name = make_string_constant_id index in
     [ global_get name ]
   else if s = "" then (
     let tid = Ltype.tid_string in
     if not (Tid.Hash.mem global_ctx.defaults tid) then
       Tid.Hash.add global_ctx.defaults tid
         (string_default_name, Arr Ltype.tid_string);
     [ global_get string_default_name ])
   else
     let utf16 = Basic_strutil.string_utf16_of_utf8 s in
     let offset, index =
       Const_table.find_str_const global_ctx.const_table utf16
     in
     let len = String.length utf16 asr 1 in
     int_const index @: int_const offset @: int_const len
     @: [ call "$moonbit.string_literal" ]
    : Ast.instr list)

let compile_bytes_literal ~(global_ctx : t) (utf8str : string) =
  (if utf8str = "" then (
     let tid = Ltype.tid_bytes in
     if not (Tid.Hash.mem global_ctx.defaults tid) then
       Tid.Hash.add global_ctx.defaults tid
         (bytes_default_name, Arr Ltype.tid_bytes);
     [ global_get bytes_default_name ])
   else
     let offset = Const_table.find_bytes_const global_ctx.const_table utf8str in
     let len = String.length utf8str in
     int_const offset @: int_const len
     @: [ array_new_data ~id_:"$moonbit.bytes" "$moonbit.const_data" ]
    : Ast.instr list)

let compile_int32_array_literal ~(global_ctx : t) (xs : int32 list)
    (tid : Tid.t) =
  (let buf = Buffer.create 16 in
   let len = List.length xs in
   Basic_lst.iter xs ~f:(fun x -> Buffer.add_int32_le buf x);
   let s = Buffer.contents buf in
   let offset = Const_table.find_array_const global_ctx.const_table s in
   int_const offset @: int_const len
   @: [ array_new_data ~id:tid "$moonbit.const_data" ]
    : Ast.instr list)

let compile_int64_array_literal ~(global_ctx : t) (xs : int64 list)
    (tid : Tid.t) =
  (let buf = Buffer.create 16 in
   let len = List.length xs in
   Basic_lst.iter xs ~f:(fun x -> Buffer.add_int64_le buf x);
   let s = Buffer.contents buf in
   let offset = Const_table.find_array_const global_ctx.const_table s in
   int_const offset @: int_const len
   @: [ array_new_data ~id:tid "$moonbit.const_data" ]
    : Ast.instr list)

let compile_constant_constr ~(global_ctx : t) ~(tag : int) =
  (Hashset_int.add global_ctx.tags tag;
   let id = make_constr_id__ tag in
   global_get_ id
    : Ast.instr)

let compile_defaults (global_ctx : t) =
  let default_names = Vec.empty () in
  let used_default_js_string = ref false in
  Tid.Hash.iter global_ctx.defaults (fun (_, (name, default_info)) ->
      if default_info = Js_string then used_default_js_string := true
      else Vec.push default_names (name, default_info));
  ( Vec.map_into_list default_names ~unorder:(fun (name, default_info) ->
        match default_info with
        | Arr tid ->
            global ~id_:name
              (immut (ref_ ~id:tid ()))
              [ array_new_fixed ~id:tid ~size:0 ]
        | Js_string -> assert false
        | Enum ->
            let tid = Ltype.tid_enum in
            global ~id_:name
              (immut (ref_ ~id:tid ()))
              (int_const (-1) @: [ struct_new tid ])),
    !used_default_js_string )

let compile_to_globals (global_ctx : t) =
  let constant_constr =
    Hashset_int.fold global_ctx.tags [] (fun i ->
        fun acc ->
         let id_ = make_constr_id__ i in
         let tid_enum = Ltype.tid_enum in
         global ~id_
           (immut (ref_ ~id:tid_enum ()))
           (int_const i @: [ struct_new tid_enum ])
         :: acc)
  in
  let defaults, used_default_js_string = compile_defaults global_ctx in
  if !Basic_config.use_js_builtin_string then (
    let constant_strings = Vec.empty () in
    Const_table.iter_constant_string_with_index global_ctx.const_table (fun s ->
        fun i ->
         let name = make_string_constant_id i in
         let const_string_module_name =
           esc_then_decode !Basic_config.const_string_module_name
         in
         let global =
           import_global ~id_:name ~mod_:const_string_module_name ~s
             (immut ref_extern_)
         in
         Vec.push constant_strings global);
    let constant_strings = Vec.to_list constant_strings in
    let constant_strings =
      if used_default_js_string then
        let s = "" in
        let const_string_module_name =
          esc_then_decode !Basic_config.const_string_module_name
        in
        let name = string_default_name in
        import_global ~id_:name ~mod_:const_string_module_name ~s
          (immut ref_extern_)
        :: constant_strings
      else constant_strings
    in
    (constant_constr @ defaults, constant_strings))
  else
    let string_pool_len = Const_table.get_string_count global_ctx.const_table in
    let string_pool =
      global ~id_:"$moonbit.string_pool"
        (immut (ref_ ~id_:"$moonbit.string_pool_type" ()))
        (int_const string_pool_len
        @: [ array_new_default ~id_:"$moonbit.string_pool_type" () ])
    in
    ((string_pool :: constant_constr) @ defaults, [])

let compile_to_data (global_ctx : t) =
  let s = Const_table.to_wat_string global_ctx.const_table in
  if s = "" then [] else data ~name:"$moonbit.const_data" s

let compile_func_ref_declare (global_ctx : t) =
  let fns =
    Fn_addr_hashset.fold global_ctx.func_refs [] (fun addr ->
        fun acc -> addr :: acc)
  in
  if fns = [] then [] else elem_declare_func fns

let add_import (global_ctx : t) (import : Import.t) =
  Import_hash.find_or_update global_ctx.imports import ~update:(fun _ ->
      let id = Import_hash.length global_ctx.imports in
      let module_name = Basic_strutil.mangle_wasm_name import.module_name in
      let func_name = Basic_strutil.mangle_wasm_name import.func_name in
      Stdlib.String.concat ""
        [ "$"; module_name; "."; func_name; "."; Int.to_string id ])

let add_inline (global_ctx : t) (func_body : W.t) =
  let uuid = Basic_uuid.next () in
  let name = ("$inline$" ^ Int.to_string uuid : Stdlib.String.t) in
  Vec.push global_ctx.inlines (name, func_body);
  name

let add_func_ref (global_ctx : t) (address : Fn_addr.t) =
  Fn_addr_hashset.add global_ctx.func_refs address

let compile_imports (global_ctx : t) =
  (let func = Wasmlinear_constr.import_func in
   Import_hash.fold global_ctx.imports []
     (fun { module_name; func_name; params_ty; return_ty } ->
       fun fname ->
        fun acc ->
         let module_ = esc_then_decode module_name in
         let name = esc_then_decode func_name in
         let params = Lst.map params_ty (fun t -> param t) in
         let results =
           match return_ty with
           | Some return_type -> [ result return_type ]
           | None -> []
         in
         import ~module_ ~name (func fname ~params ~results) @ acc)
    : Ast.modulefield list)

let compile_inlines (global_ctx : t) =
  Vec.map_into_list global_ctx.inlines ~unorder:(fun (name, body) ->
      match Dwarfsm_parse.modulefield body with
      | Ast.Func func :: [] -> Ast.Func { func with id = binder name }
      | _ -> assert false)

let find_default_elem (global_ctx : t) (type_defs : Ltype.def Tid.Hash.t)
    (tid : Tid.t) =
  let add_if_not_exist tid default_name default =
    if not (Tid.Hash.mem global_ctx.defaults tid) then
      Tid.Hash.add global_ctx.defaults tid (default_name, default)
      [@@inline]
  in
  match Tid.Hash.find_opt type_defs tid with
  | Some (Ref_array { elem = Ref_string }) ->
      let default =
        if !Basic_config.use_js_builtin_string then Js_string
        else Arr Ltype.tid_string
      in
      add_if_not_exist Ltype.tid_string string_default_name default;
      Some string_default_name
  | Some (Ref_array { elem = Ref_bytes }) ->
      add_if_not_exist Ltype.tid_bytes bytes_default_name (Arr Ltype.tid_bytes);
      Some bytes_default_name
  | Some (Ref_array { elem = Ref { tid = elem_tid } })
    when Tid.equal elem_tid Ltype.tid_enum ->
      add_if_not_exist Ltype.tid_enum enum_default_name Enum;
      Some enum_default_name
  | _ -> None
