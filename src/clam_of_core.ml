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
module Ltype = Ltype_gc
module Ltype_util = Ltype_gc_util
module Transl_mtype = Transl_mtype_gc
module Addr_table = Addr_table_gc
module Fn_address = Basic_fn_address
module Tid = Basic_ty_ident
module Ident_hashset = Basic_core_ident.Hashset
module Lst = Basic_lst
module Core_ident = Basic_core_ident

type binds = Clam.top_func_item list

let base = ref Loc.no_location
let binds_init : binds ref = ref []
let new_top (x : Clam.top_func_item) = binds_init := x :: !binds_init
let clam_unit : Clam.lambda = Lconst (C_int { v = 0l; repr = None })
let local_non_well_knowns : Core_ident.Hashset.t = Core_ident.Hashset.create 17

let fix_var =
  object
    inherit [_] Clam.map

    method! visit_var (self, replace) var =
      if Ident.equal var self then replace else var
  end

let fix_single_var ~self ~replace body =
  fix_var#visit_lambda (self, replace) body

let transl_params ~mtype_defs ~type_defs (params : Mcore.param list) =
  (Lst.map params (fun { binder; ty } ->
       Ident.of_core_ident binder
         ~ty:(Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs ty))
    : Ident.t list)

let transl_ident (id : Core_ident.t) ~(addr_tbl : Addr_table.t) ~mtype_defs
    ~type_defs mty =
  (match Addr_table.find_opt addr_tbl id with
   | Some (Toplevel { name_as_closure = Some id; _ }) -> id
   | Some (Toplevel fn_info) ->
       let ty = Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs mty in
       let id = Ident.of_core_ident ~ty id in
       fn_info.name_as_closure <- Some id;
       id
   | Some (Local (_addr, ty)) -> Ident.of_core_ident ~ty id
   | None ->
       let ty = Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs mty in
       Ident.of_core_ident ~ty id
    : Ident.t)

let rec transl_expr ~name_hint ~mtype_defs ~addr_tbl ~type_defs ~object_methods
    (x : Mcore.expr) =
  (let transl_type mtype =
     Transl_mtype.mtype_to_ltype mtype ~type_defs ~mtype_defs
       [@@inline]
   in
   let transl_type_as_named_exn mtype =
     match transl_type mtype with
     | Ref_lazy_init { tid } | Ltype.Ref { tid } | Ref_nullable { tid } -> tid
     | Ref_bytes -> Ltype.tid_bytes
     | _ -> assert false
       [@@inline]
   in
   let transl_constr_type tag mtype =
     Transl_mtype.constr_to_ltype ~tag mtype
       [@@inline]
   in
   let go x =
     transl_expr ~name_hint ~mtype_defs ~addr_tbl ~type_defs ~object_methods x
       [@@inline]
   in
   let bind (rhs : Mcore.expr) cont =
     (match rhs with
      | Cexpr_var { id; ty; _ } ->
          let ty = transl_type ty in
          cont (Ident.of_core_ident ~ty id)
      | _ ->
          let ty = transl_type (Mcore.type_of_expr rhs) in
          let name = Ident.fresh ~ty "*bind" in
          Llet { name; e = go rhs; body = cont name }
       : Clam.lambda)
       [@@inline]
   in
   let append_name_hint new_name =
     (name_hint ^ "." ^ Core_ident.base_name new_name : Stdlib.String.t)
       [@@inline]
   in
   let handle_abstract_closure_type ~(name : Core_ident.t option)
       (fn : Mcore.fn) (address : Fn_address.t) =
     (let params = transl_params ~mtype_defs ~type_defs fn.params in
      let return_type_ =
        [
          Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs
            (Mcore.type_of_expr fn.body);
        ]
      in
      let sig_ : Ltype.fn_sig =
        { params = Lst.map params Ident.get_type; ret = return_type_ }
      in
      let abs_closure_tid =
        match Ltype.FnSigHash.find_opt type_defs.fn_sig_tbl sig_ with
        | Some tid -> tid
        | None ->
            let tid =
              Name_mangle.make_type_name
                (T_func
                   {
                     params = Lst.map fn.params (fun p -> p.ty);
                     return = Mcore.type_of_expr fn.body;
                   })
            in
            Ltype.FnSigHash.add type_defs.fn_sig_tbl sig_ tid;
            Tid.Hash.add type_defs.defs tid
              (Ltype.Ref_closure_abstract { fn_sig = sig_ });
            tid
      in
      let abs_closure_ty = Ltype.Ref { tid = abs_closure_tid } in
      (match name with
      | Some name ->
          Addr_table.add_local_fn_addr_and_type addr_tbl name address
            abs_closure_ty
      | None -> ());
      abs_closure_tid
       : Tid.t)
   in
   let expr : Clam.lambda =
     match x with
     | Cexpr_const { c; ty = _ } -> Lconst c
     | Cexpr_unit _ -> clam_unit
     | Cexpr_var { id; ty; prim = _ } ->
         let var = transl_ident id ty ~addr_tbl ~mtype_defs ~type_defs in
         Lvar { var }
     | Cexpr_and { lhs; rhs } ->
         Lif
           {
             pred = go lhs;
             ifso = go rhs;
             ifnot = Lconst (C_bool false);
             type_ = Ltype.i32_bool;
           }
     | Cexpr_or { lhs; rhs } ->
         Lif
           {
             pred = go lhs;
             ifso = Lconst (C_bool true);
             ifnot = go rhs;
             type_ = Ltype.i32_bool;
           }
     | Cexpr_prim { prim; args; ty } -> (
         let make_prim fn =
           Clam.Lprim { fn; args = Lst.map args go }
             [@@inline]
         in
         match prim with
         | Parray_make -> assert false
         | Pfixedarray_make { kind } ->
             Lmake_array
               {
                 kind;
                 tid = transl_type_as_named_exn ty;
                 elems = Lst.map args go;
               }
         | Pfixedarray_get_item { kind } -> (
             let arr_type = Mcore.type_of_expr (List.hd args) in
             if arr_type = T_bytes then
               match kind with
               | Safe ->
                   Lprim
                     {
                       fn = Pgetbytesitem { safe = true };
                       args = Lst.map args go;
                     }
               | Unsafe ->
                   Lprim
                     {
                       fn = Pgetbytesitem { safe = false };
                       args = Lst.map args go;
                     }
               | Rev_unsafe -> assert false
             else
               let extra : Clam.get_item_extra =
                 let elem = Mtype.get_fixedarray_elem_exn arr_type in
                 if Mtype.is_uninit elem then Need_non_null_cast
                 else if elem = T_int16 then Need_signed_info { signed = true }
                 else if elem = T_uint16 then
                   Need_signed_info { signed = false }
                 else No_extra
               in
               match[@warning "-fragile-match"] Lst.map args go with
               | [ arr; index ] ->
                   Larray_get_item
                     {
                       arr;
                       index;
                       tid = transl_type_as_named_exn arr_type;
                       kind;
                       extra;
                     }
               | _ -> assert false)
         | Pfixedarray_set_item { set_kind } -> (
             let arr_type = Mcore.type_of_expr (List.hd args) in
             match[@warning "-fragile-match"] Lst.map args go with
             | arr :: index :: item ->
                 let item =
                   match item with
                   | [] -> None
                   | item :: [] -> Some item
                   | _ -> assert false
                 in
                 Larray_set_item
                   {
                     tid = transl_type_as_named_exn arr_type;
                     kind = set_kind;
                     arr;
                     index;
                     item;
                   }
             | _ -> assert false)
         | Prefeq ->
             let mty = Mcore.type_of_expr (List.hd args) in
             let lty = transl_type mty in
             make_prim
               (match lty with
               | I32 _ -> Pcomparison { operator = Eq; operand_type = I32 }
               | I64 -> Pcomparison { operator = Eq; operand_type = I64 }
               | F32 -> Pcomparison { operator = Eq; operand_type = F32 }
               | F64 -> Pcomparison { operator = Eq; operand_type = F64 }
               | Ref_string when !Basic_config.use_js_builtin_string ->
                   Pstringequal
               | Ref_nullable { tid }
                 when !Basic_config.use_js_builtin_string
                      && Tid.equal tid Ltype.tid_string ->
                   Pstringequal
               | Ref _ | Ref_lazy_init _ | Ref_nullable _ | Ref_extern
               | Ref_string | Ref_bytes | Ref_func | Ref_any ->
                   Prefeq)
         | Pcast { kind } -> (
             match[@warning "-fragile-match"] args with
             | arg :: [] -> (
                 match kind with
                 | Constr_to_enum | Make_newtype -> go arg
                 | Unfold_rec_newtype | Enum_to_constr ->
                     let target_type = transl_type ty in
                     Lcast { expr = go arg; target_type })
             | _ -> assert false)
         | Penum_field { index; tag = _ } -> (
             let tid =
               Name_mangle.make_type_name (Mcore.type_of_expr (List.hd args))
             in
             match[@warning "-fragile-match"] Lst.map args go with
             | obj :: [] -> Lget_field { kind = Enum; obj; index; tid }
             | _ -> assert false)
         | Pset_enum_field { index; tag = _ } -> (
             let tid =
               Name_mangle.make_type_name (Mcore.type_of_expr (List.hd args))
             in
             match[@warning "-fragile-match"] Lst.map args go with
             | [ obj; field ] ->
                 Lset_field { kind = Enum; obj; field; index; tid }
             | _ -> assert false)
         | Pcatch -> (
             match[@warning "-fragile-match"] args with
             | [ body; on_exception ] ->
                 Lcatch
                   {
                     body = go body;
                     on_exception = go on_exception;
                     type_ = transl_type (Mcore.type_of_expr body);
                   }
             | _ -> assert false)
         | Pmake_value_or_error { tag } ->
             let tag = Tag.of_core_tag_no_ext tag in
             let t = transl_constr_type tag ty in
             Lallocate
               { kind = Enum { tag }; tid = t; fields = Lst.map args go }
         | Pnull when !Basic_config.use_js_builtin_string -> (
             let ty = transl_type ty in
             match ty with
             | Ref_nullable { tid } when Tid.equal tid Ltype.tid_string ->
                 make_prim Pnull_string_extern
             | _ -> make_prim Pnull)
         | Pcall_object_method { method_index; _ } -> (
             let method_ty =
               transl_type
                 (T_func
                    { params = Lst.map args Mcore.type_of_expr; return = ty })
             in
             match[@warning "-fragile-match"] args with
             | self :: args ->
                 bind self (fun obj ->
                     let args = Lst.map args go in
                     Lapply
                       {
                         fn = Object { obj; method_index; method_ty };
                         args;
                         prim = None;
                       })
             | _ -> assert false)
         | _ -> make_prim prim)
     | Cexpr_let { name; rhs; body; ty = _ } ->
         let ty_rhs = Mcore.type_of_expr rhs in
         let name = Ident.of_core_ident ~ty:(transl_type ty_rhs) name in
         Llet { name; e = go rhs; body = go body }
     | Cexpr_letfn { name; fn; body; kind; ty } -> (
         match kind with
         | Nonrec ->
             let name_hint = append_name_hint name in
             let address = Fn_address.fresh (name_hint ^ ".fn") in
             if Core_ident.Hashset.mem local_non_well_knowns name then
               let abs_closure_tid =
                 handle_abstract_closure_type ~name:(Some name) fn address
               in
               let closure, ty =
                 closure_of_fn ~addr_tbl ~type_defs ~object_methods ~address
                   ~name_hint ~mtype_defs ~self:None ~abs_closure_tid fn
               in
               let body = go body in
               let name = Ident.of_core_ident name ~ty in
               Llet { name; e = Lclosure closure; body }
             else
               let tuple, binder =
                 well_known_closure_of_fn ~mtype_defs ~addr_tbl ~type_defs
                   ~object_methods ~name_hint ~address ~self:name ~is_rec:false
                   fn
               in
               let body = go body in
               Llet { name = binder; e = tuple; body }
         | Rec ->
             let name_hint = append_name_hint name in
             let address = Fn_address.fresh (name_hint ^ ".fn") in
             if Core_ident.Hashset.mem local_non_well_knowns name then
               let abs_closure_tid =
                 handle_abstract_closure_type ~name:(Some name) fn address
               in
               let closure, ty =
                 closure_of_fn ~addr_tbl ~type_defs ~object_methods ~name_hint
                   ~mtype_defs ~self:(Some name) ~address ~abs_closure_tid fn
               in
               let body = go body in
               let name = Ident.of_core_ident name ~ty in
               Llet { name; e = Lclosure closure; body }
             else
               let tuple, binder =
                 well_known_closure_of_fn ~mtype_defs ~addr_tbl ~type_defs
                   ~object_methods ~name_hint ~address ~self:name ~is_rec:true
                   fn
               in
               let body = go body in
               Llet { name = binder; e = tuple; body }
         | Tail_join | Nontail_join ->
             let name = Join.of_core_ident name in
             let params = transl_params ~mtype_defs ~type_defs fn.params in
             let join_body = go fn.body in
             let body = go body in
             let kind : Clam.join_kind =
               match kind with
               | Tail_join -> Tail_join
               | Nontail_join -> Nontail_join
               | _ -> assert false
             in
             let type_ = [ transl_type ty ] in
             Ljoinlet { name; params; e = join_body; body; kind; type_ })
     | Cexpr_function { func; is_raw_; ty = _ } ->
         let address = Fn_address.fresh (name_hint ^ ".fn") in
         if is_raw_ then (
           let params = transl_params ~mtype_defs ~type_defs func.params in
           let return = Mcore.type_of_expr func.body in
           let return_type_ =
             if return = T_unit then [] else [ transl_type return ]
           in
           let body = go func.body in
           new_top
             {
               binder = address;
               fn_kind_ = Top_private;
               tid = None;
               fn = { params; body; return_type_ };
             };
           Lget_raw_func address)
         else
           let abs_closure_tid =
             handle_abstract_closure_type ~name:None func address
           in
           let closure, _ =
             closure_of_fn ~addr_tbl ~type_defs ~mtype_defs ~object_methods
               ~name_hint ~address ~self:None ~abs_closure_tid func
           in
           Lclosure closure
     | Cexpr_apply { func; args; kind = Join; ty = _; prim = _ } ->
         let name = Join.of_core_ident func in
         let args = Lst.map args go in
         Ljoinapply { name; args }
     | Cexpr_apply
         { func; args = core_args; kind = Normal { func_ty }; ty = _; prim }
       -> (
         let args = Lst.map core_args go in
         match Addr_table.find_opt addr_tbl func with
         | Some (Toplevel { addr; _ }) ->
             let prim : Clam.intrinsic option =
               match prim with
               | Some (Pintrinsic FixedArray_copy) -> (
                   match[@warning "-fragile-match"] core_args with
                   | dst :: _ :: src :: _ ->
                       let dst_tid =
                         transl_type_as_named_exn (Mcore.type_of_expr dst)
                       in
                       let src_tid =
                         transl_type_as_named_exn (Mcore.type_of_expr src)
                       in
                       Some (FixedArray_copy { src_tid; dst_tid })
                   | _ -> assert false)
               | Some (Pintrinsic FixedArray_fill) -> (
                   match[@warning "-fragile-match"] core_args with
                   | arr :: _ ->
                       let arr_tid =
                         transl_type_as_named_exn (Mcore.type_of_expr arr)
                       in
                       Some (FixedArray_fill { tid = arr_tid })
                   | _ -> assert false)
               | Some (Pintrinsic Char_to_string)
                 when !Basic_config.use_js_builtin_string ->
                   Some Char_to_string
               | Some (Pintrinsic _) -> None
               | Some _ -> assert false
               | None -> None
             in
             Lapply { fn = StaticFn addr; args; prim }
         | Some (Local (addr, self_ty)) ->
             let self = Ident.of_core_ident func ~ty:self_ty in
             Lapply
               {
                 fn = StaticFn addr;
                 args = Lvar { var = self } :: args;
                 prim = None;
               }
         | None ->
             let fn = Ident.of_core_ident ~ty:(transl_type func_ty) func in
             Lapply { fn = Dynamic fn; args; prim = None })
     | Cexpr_object { self; methods_key; ty = _ } ->
         let ({ trait; type_ } : Object_util.object_key) = methods_key in
         let tid = Tid.concrete_object_type ~trait ~type_name:type_ in
         let methods = Object_util.Hash.find_exn object_methods methods_key in
         Lallocate { tid; kind = Object { methods }; fields = [ go self ] }
     | Cexpr_letrec { bindings; body; ty = _ } ->
         let addresses =
           Lst.map bindings (fun (name, fn) ->
               let address = Fn_address.fresh (append_name_hint name ^ ".fn") in
               if Core_ident.Hashset.mem local_non_well_knowns name then
                 let abs_closure_tid =
                   handle_abstract_closure_type ~name:(Some name) fn address
                 in
                 (address, Some abs_closure_tid)
               else
                 let ty = Ltype.Ref { tid = Tid.capture_of_function address } in
                 Addr_table.add_local_fn_addr_and_type addr_tbl name address ty;
                 (address, None))
         in
         let fns, names =
           Lst.split_map2 bindings addresses (fun (name, fn) ->
               fun (address, abs_closure_tid_opt) ->
                if Core_ident.Hashset.mem local_non_well_knowns name then
                  let abs_closure_tid = Option.get abs_closure_tid_opt in
                  let closure, ty =
                    closure_of_fn ~addr_tbl ~type_defs ~mtype_defs
                      ~object_methods ~address
                      ~name_hint:(append_name_hint name) ~self:(Some name)
                      ~abs_closure_tid fn
                  in
                  let self = Ident.of_core_ident name ~ty in
                  (closure, self)
                else
                  well_known_closure_of_mut_rec_fn ~addr_tbl ~type_defs
                    ~mtype_defs ~object_methods ~address
                    ~name_hint:(append_name_hint name) ~self:name fn)
         in
         Lletrec { names; fns; body = go body }
     | Cexpr_constr { tag; args; ty } ->
         let t = transl_constr_type tag ty in
         Lallocate { kind = Enum { tag }; tid = t; fields = Lst.map args go }
     | Cexpr_tuple { exprs; ty } ->
         let t = transl_type_as_named_exn ty in
         Lallocate { kind = Tuple; tid = t; fields = Lst.map exprs go }
     | Cexpr_record { fields; ty } ->
         if fields = [] then clam_unit
         else
           let fields = Lst.map fields (fun { expr; _ } -> go expr) in
           let t = transl_type_as_named_exn ty in
           Lallocate { kind = Struct; tid = t; fields }
     | Cexpr_record_update { record; fields; fields_num; ty } ->
         let t = transl_type_as_named_exn ty in
         bind record (fun record_id ->
             let record_clam : Clam.lambda = Lvar { var = record_id } in
             let get_new_field i =
               match Lst.find_first fields (fun { pos; _ } -> pos = i) with
               | Some { expr; _ } -> go expr
               | None ->
                   Lget_field
                     { kind = Struct; obj = record_clam; index = i; tid = t }
             in
             Lallocate
               {
                 kind = Struct;
                 tid = t;
                 fields = List.init fields_num get_new_field;
               })
     | Cexpr_field { record; accessor; pos; ty = _ } ->
         let obj = go record in
         let t = transl_type_as_named_exn (Mcore.type_of_expr record) in
         let kind : Clam.get_field_kind =
           match accessor with
           | Newtype -> assert false
           | Label _ -> Struct
           | Index _ -> Tuple
         in
         Lget_field { kind; obj; index = pos; tid = t }
     | Cexpr_mutate { record; label = _; field; pos; ty = _ } ->
         let t = transl_type_as_named_exn (Mcore.type_of_expr record) in
         Lset_field
           {
             obj = go record;
             field = go field;
             index = pos;
             tid = t;
             kind = Struct;
           }
     | Cexpr_array { exprs; ty } ->
         let tid = transl_type_as_named_exn ty in
         Lmake_array { kind = EverySingleElem; tid; elems = Lst.map exprs go }
     | Cexpr_assign { var; expr; ty = _ } ->
         let var =
           Ident.of_core_ident var ~ty:(transl_type (Mcore.type_of_expr expr))
         in
         Lassign { var; e = go expr }
     | Cexpr_sequence { exprs; last_expr; ty = _ } ->
         let exprs = Lst.map exprs go in
         let last_expr = go last_expr in
         Lsequence { exprs; last_expr }
     | Cexpr_if { cond; ifso; ifnot; ty } -> (
         let pred = go cond in
         let ifso = go ifso in
         match ifnot with
         | Some ifnot ->
             Lif { pred; ifso; ifnot = go ifnot; type_ = transl_type ty }
         | None -> Lif { pred; ifso; ifnot = clam_unit; type_ = Ltype.i32_unit }
         )
     | Cexpr_handle_error { obj; handle_kind = To_result; _ } -> go obj
     | Cexpr_handle_error { obj; handle_kind; ty } ->
         let obj_ty = Mcore.type_of_expr obj in
         bind obj (fun obj_id ->
             let obj : Clam.lambda = Lvar { var = obj_id } in
             let ok_tag = Tag.of_core_tag_no_ext Builtin.constr_ok.cs_tag in
             let action_ok : Clam.lambda =
               let constr_tid = transl_constr_type ok_tag obj_ty in
               Lget_field
                 {
                   kind = Enum;
                   obj =
                     Lcast
                       { expr = obj; target_type = Ref { tid = constr_tid } };
                   index = 0;
                   tid = constr_tid;
                 }
             in
             let cases = [ (ok_tag, action_ok) ] in
             let default : Clam.lambda =
               let err_tag = Tag.of_core_tag_no_ext Builtin.constr_err.cs_tag in
               let constr_tid = transl_constr_type err_tag obj_ty in
               match handle_kind with
               | Joinapply err_join ->
                   let err_value : Clam.lambda =
                     Lget_field
                       {
                         kind = Enum;
                         obj =
                           Lcast
                             {
                               expr = obj;
                               target_type = Ref { tid = constr_tid };
                             };
                         index = 0;
                         tid = constr_tid;
                       }
                   in
                   Ljoinapply
                     {
                       name = Join.of_core_ident err_join;
                       args = [ err_value ];
                     }
               | Return_err _ -> Lreturn obj
               | To_result -> assert false
             in
             Lswitch { obj = obj_id; cases; default; type_ = transl_type ty })
     | Cexpr_switch_constr { obj; cases; default; ty } ->
         let obj_ty = Mcore.type_of_expr obj in
         bind obj (fun obj_id ->
             let obj : Clam.lambda = Lvar { var = obj_id } in
             let transl_action tag binder body =
               (match binder with
                | None -> go body
                | Some binder ->
                    let constr_tid = transl_constr_type tag obj_ty in
                    Clam.Llet
                      {
                        name =
                          Ident.of_core_ident binder
                            ~ty:(Ref { tid = constr_tid });
                        e =
                          Lcast
                            {
                              expr = obj;
                              target_type = Ref { tid = constr_tid };
                            };
                        body = go body;
                      }
                 : Clam.lambda)
                 [@@inline]
             in
             let cases, default =
               Lst.fold_right cases
                 ([], Option.map go default)
                 (fun (tag, binder, action) ->
                   fun (cases, default) ->
                    let action = transl_action tag binder action in
                    match default with
                    | Some _ -> ((tag, action) :: cases, default)
                    | None -> (cases, Some action))
             in
             match (cases, default) with
             | [], Some default -> default
             | [], None -> Lprim { fn = Ppanic; args = [] }
             | _ :: _, None -> assert false
             | _, Some default ->
                 Lswitch
                   { obj = obj_id; cases; default; type_ = transl_type ty })
     | Cexpr_switch_constant { obj; cases; default; ty } ->
         bind obj (fun obj_id ->
             let obj : Clam.lambda = Lvar { var = obj_id } in
             match cases with
             | (C_string _, _) :: _ ->
                 let cases =
                   Lst.map cases (fun (c, action) ->
                       match[@warning "-fragile-match"] c with
                       | C_string s -> (s, go action)
                       | _ -> assert false)
                 in
                 Lswitchstring
                   { obj; cases; default = go default; type_ = transl_type ty }
             | (C_int _, _) :: _ ->
                 let cases =
                   Lst.map cases (fun (c, action) ->
                       match[@warning "-fragile-match"] c with
                       | C_int { v; repr = _ } -> (Int32.to_int v, go action)
                       | _ -> assert false)
                 in
                 Lswitchint
                   {
                     obj = obj_id;
                     cases;
                     default = go default;
                     type_ = transl_type ty;
                   }
             | (c, _) :: _ ->
                 let default = go default in
                 let equal =
                   match c with
                   | C_char _ -> Primitive.equal_char
                   | C_int _ -> Primitive.equal_int
                   | C_byte _ -> Primitive.equal_int
                   | C_int64 _ -> Primitive.equal_int64
                   | C_uint _ -> Primitive.equal_uint
                   | C_uint64 _ -> Primitive.equal_uint64
                   | C_float _ -> Primitive.equal_float
                   | C_double _ -> Primitive.equal_float64
                   | C_bool _ -> Primitive.equal_bool
                   | C_bigint _ -> assert false
                   | C_string _ -> assert false
                   | C_bytes _ -> Primitive.Pbytesequal
                 in
                 Lst.fold_right cases default (fun (c, action) ->
                     fun rest ->
                      let pred : Clam.lambda =
                        Lprim { fn = equal; args = [ obj; Lconst c ] }
                      in
                      Lif
                        {
                          pred;
                          ifso = go action;
                          ifnot = rest;
                          type_ = transl_type ty;
                        })
             | [] -> go default)
     | Cexpr_loop { params; body; args; label; ty } ->
         let params = transl_params ~mtype_defs ~type_defs params in
         let body = go body in
         let args = Lst.map args go in
         let type_ = transl_type ty in
         Lloop { params; body; args; label; type_ }
     | Cexpr_break { arg; label; ty = _ } ->
         Lbreak { arg = Option.map go arg; label }
     | Cexpr_continue { args; label; ty = _ } ->
         Lcontinue { args = Lst.map args go; label }
     | Cexpr_return { expr; return_kind; ty = _ } -> (
         match return_kind with
         | Single_value -> Lreturn (go expr)
         | Error_result { is_error; return_ty } ->
             let tag =
               Tag.of_core_tag_no_ext
                 (if is_error then Builtin.constr_err.cs_tag
                  else Builtin.constr_ok.cs_tag)
             in
             let t = transl_constr_type tag return_ty in
             Lreturn
               (Lallocate { kind = Enum { tag }; tid = t; fields = [ go expr ] })
         )
   in
   if !Basic_config.debug then
     let expr_loc = Mcore.loc_of_expr x in
     Clam.event ~loc_:(Rloc.to_loc ~base:!base expr_loc) expr
   else expr
    : Clam.lambda)

and closure_of_fn ~mtype_defs ~addr_tbl ~type_defs ~object_methods ~name_hint
    ~(address : Fn_address.t) ~(self : Core_ident.t option)
    ~(abs_closure_tid : Tid.t) (fn : Mcore.fn) =
  (let exclude =
     match self with
     | Some self -> Core_ident.Set.singleton self
     | None -> Core_ident.Set.empty
   in
   let fvs =
     Core_ident.Map.fold (Mcore_util.free_vars ~exclude fn) [] (fun id ->
         fun mty ->
          fun acc -> transl_ident id mty ~addr_tbl ~mtype_defs ~type_defs :: acc)
   in
   let params = transl_params ~mtype_defs ~type_defs fn.params in
   let return_type_ =
     [
       Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs
         (Mcore.type_of_expr fn.body);
     ]
   in
   let abs_closure_ty = Ltype.Ref { tid = abs_closure_tid } in
   match fvs with
   | [] ->
       let env_id = Ident.fresh ~ty:abs_closure_ty "*env" in
       let body =
         transl_expr ~mtype_defs ~addr_tbl ~name_hint ~type_defs ~object_methods
           fn.body
       in
       let body =
         match self with
         | Some self ->
             let name = Ident.of_core_ident self ~ty:abs_closure_ty in
             let env = Clam.Lvar { var = env_id } in
             Clam.Llet { name; e = env; body }
         | None -> body
       in
       let fn : Clam.fn = { params = env_id :: params; body; return_type_ } in
       new_top
         {
           binder = address;
           fn_kind_ = Clam.Top_private;
           fn;
           tid = Some (Tid.code_pointer_of_closure abs_closure_tid);
         };
       ( { captures = fvs; address = Normal address; tid = abs_closure_tid },
         abs_closure_ty )
   | _ ->
       let closure_cap_tid = Tid.capture_of_function address in
       let closure_type_def =
         Ltype.Ref_closure
           {
             fn_sig_tid = abs_closure_tid;
             captures = Lst.map fvs Ident.get_type;
           }
       in
       let closure_ty = Ltype.Ref { tid = closure_cap_tid } in
       Tid.Hash.add type_defs.defs closure_cap_tid closure_type_def;
       let self =
         match self with
         | Some self -> Some (Ident.of_core_ident self ~ty:abs_closure_ty)
         | None -> None
       in
       let env_id = Ident.fresh ~ty:abs_closure_ty "*env" in
       let env = Clam.Lvar { var = env_id } in
       let casted_env_id = Ident.fresh "*casted_env" ~ty:closure_ty in
       let casted_env = Clam.Lvar { var = casted_env_id } in
       let body =
         transl_expr ~mtype_defs ~addr_tbl ~name_hint ~type_defs ~object_methods
           fn.body
       in
       let body_with_captures =
         Lst.fold_left_with_offset fvs body 0 (fun fv ->
             fun rest ->
              fun i ->
               let rhs =
                 Clam.Lclosure_field
                   { obj = casted_env; index = i; tid = closure_cap_tid }
               in
               let rhs : Clam.lambda =
                 if Ltype_util.is_non_nullable_ref_type (Ident.get_type fv) then
                   Lprim { fn = Pas_non_null; args = [ rhs ] }
                 else rhs
               in
               Clam.Llet { name = fv; e = rhs; body = rest })
       in
       let body_with_cast =
         Clam.Llet
           {
             name = casted_env_id;
             e =
               Lcast { expr = env; target_type = Ref { tid = closure_cap_tid } };
             body = body_with_captures;
           }
       in
       let body =
         match self with
         | Some self ->
             Clam.Llet { name = self; e = env; body = body_with_cast }
         | None -> body_with_cast
       in
       let fn : Clam.fn = { params = env_id :: params; body; return_type_ } in
       new_top
         {
           binder = address;
           fn_kind_ = Clam.Top_private;
           fn;
           tid = Some (Tid.code_pointer_of_closure abs_closure_tid);
         };
       ( { captures = fvs; address = Normal address; tid = closure_cap_tid },
         abs_closure_ty )
    : Clam.closure * Ltype.t)

and well_known_closure_of_fn ~mtype_defs ~addr_tbl ~type_defs ~object_methods
    ~name_hint ~(address : Fn_address.t) ~(self : Core_ident.t) ~(is_rec : bool)
    (fn : Mcore.fn) =
  (let params = transl_params ~mtype_defs ~type_defs fn.params in
   let return_type_ =
     [
       Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs
         (Mcore.type_of_expr fn.body);
     ]
   in
   let exclude =
     if is_rec then Core_ident.Set.singleton self else Core_ident.Set.empty
   in
   let fvs =
     Core_ident.Map.fold (Mcore_util.free_vars ~exclude fn) [] (fun id ->
         fun mty ->
          fun acc -> transl_ident id mty ~addr_tbl ~mtype_defs ~type_defs :: acc)
   in
   let insert_let_self_for_recursive_case self rhs ty body =
     if is_rec then
       let self = Ident.of_core_ident ~ty self in
       Clam.Llet { name = self; e = rhs; body }
     else body
       [@@inline]
   in
   let update_type_table ty =
     Addr_table.add_local_fn_addr_and_type addr_tbl self address ty
       [@@inline]
   in
   match fvs with
   | [] ->
       let env = Ident.fresh ~ty:Ltype.i32_unit "*env" in
       Addr_table.add_local_fn_addr_and_type addr_tbl self address
         Ltype.i32_unit;
       let body =
         transl_expr ~mtype_defs ~addr_tbl ~name_hint ~type_defs ~object_methods
           fn.body
       in
       let body =
         insert_let_self_for_recursive_case self clam_unit Ltype.i32_unit body
       in
       let fn : Clam.fn = { params = env :: params; body; return_type_ } in
       new_top { binder = address; fn_kind_ = Clam.Top_private; fn; tid = None };
       (clam_unit, Ident.of_core_ident ~ty:Ltype.i32_unit self)
   | fv :: [] ->
       let fv_ty = Ident.get_type fv in
       let fv_var = Clam.Lvar { var = fv } in
       update_type_table fv_ty;
       let body =
         transl_expr ~mtype_defs ~addr_tbl ~name_hint ~type_defs ~object_methods
           fn.body
       in
       let body =
         if is_rec then
           let self = Ident.of_core_ident ~ty:Ltype.i32_unit self in
           fix_single_var ~self ~replace:fv body
         else body
       in
       let fn : Clam.fn = { params = fv :: params; body; return_type_ } in
       new_top { binder = address; fn_kind_ = Clam.Top_private; fn; tid = None };
       (fv_var, Ident.of_core_ident self ~ty:fv_ty)
   | _ :: _ :: _ ->
       let ty_tuple_def =
         Ltype.Ref_struct
           { fields = Lst.map fvs (fun id -> (Ident.get_type id, false)) }
       in
       let ty_tuple_tid = Tid.capture_of_function address in
       Basic_ty_ident.Hash.add type_defs.defs ty_tuple_tid ty_tuple_def;
       let ty_tuple = Ltype.Ref { tid = ty_tuple_tid } in
       let env_id = Ident.fresh ~ty:ty_tuple "*env" in
       let env = Clam.Lvar { var = env_id } in
       update_type_table ty_tuple;
       let body =
         transl_expr ~mtype_defs ~addr_tbl ~name_hint ~type_defs ~object_methods
           fn.body
       in
       let body =
         Lst.fold_left_with_offset fvs body 0 (fun fv ->
             fun rest ->
              fun index ->
               let rhs =
                 Clam.Lget_field
                   { kind = Tuple; obj = env; index; tid = ty_tuple_tid }
               in
               Clam.Llet { name = fv; e = rhs; body = rest })
       in
       let body = insert_let_self_for_recursive_case self env ty_tuple body in
       let fn : Clam.fn = { params = env_id :: params; body; return_type_ } in
       new_top { binder = address; fn_kind_ = Clam.Top_private; fn; tid = None };
       ( Lallocate
           {
             kind = Struct;
             tid = ty_tuple_tid;
             fields = Lst.map fvs (fun fv -> Clam.Lvar { var = fv });
           },
         Ident.of_core_ident ~ty:ty_tuple self )
    : Clam.lambda * Ident.t)

and well_known_closure_of_mut_rec_fn ~mtype_defs ~addr_tbl ~type_defs
    ~object_methods ~self ~name_hint ~address (fn : Mcore.fn) =
  (let fvs =
     Core_ident.Map.fold
       (Mcore_util.free_vars ~exclude:(Core_ident.Set.singleton self) fn)
       []
       (fun id ->
         fun mty ->
          fun acc -> transl_ident id mty ~addr_tbl ~mtype_defs ~type_defs :: acc)
   in
   let params = transl_params ~mtype_defs ~type_defs fn.params in
   let return_type_ =
     [
       Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs
         (Mcore.type_of_expr fn.body);
     ]
   in
   let tid = Tid.capture_of_function address in
   let closure_type =
     Ltype.Ref_late_init_struct { fields = Lst.map fvs Ident.get_type }
   in
   Tid.Hash.add type_defs.defs tid closure_type;
   let name_hint =
     (name_hint ^ "." ^ Core_ident.base_name self : Stdlib.String.t)
   in
   let ty : Ltype.t = Ref { tid } in
   let self = Ident.of_core_ident ~ty self in
   let body =
     transl_expr ~mtype_defs ~addr_tbl ~type_defs ~name_hint ~object_methods
       fn.body
   in
   let env_id = Ident.fresh ~ty "*env" in
   let env = Clam.Lvar { var = env_id } in
   let body =
     Lst.fold_left_with_offset fvs body 0 (fun fv ->
         fun rest ->
          fun index ->
           let rhs = Clam.Lget_field { obj = env; index; tid; kind = Tuple } in
           let rhs : Clam.lambda =
             if Ltype_util.is_non_nullable_ref_type (Ident.get_type fv) then
               Lprim { fn = Pas_non_null; args = [ rhs ] }
             else rhs
           in
           Clam.Llet { name = fv; e = rhs; body = rest })
   in
   let body = fix_single_var ~self ~replace:env_id body in
   let fn : Clam.fn = { params = env_id :: params; body; return_type_ } in
   new_top { binder = address; fn_kind_ = Clam.Top_private; fn; tid = None };
   ({ captures = fvs; address = Well_known_mut_rec; tid }, self)
    : Clam.closure * Ident.t)

let make_top_fn_item ~mtype_defs ~addr_tbl ~type_defs ~object_methods
    (fn_address : Fn_address.t) params
    (body : [ `Clam of Clam.lambda | `Core of Mcore.expr ]) return_type_
    ~export_info_ =
  (let fn_kind_ =
     match export_info_ with
     | Some export_name -> Clam.Top_pub export_name
     | None -> Clam.Top_private
   in
   let fn_body =
     match body with
     | `Core body ->
         transl_expr ~addr_tbl ~type_defs ~object_methods
           ~name_hint:(Fn_address.to_string fn_address)
           ~mtype_defs body
     | `Clam body -> body
   in
   {
     binder = fn_address;
     fn_kind_;
     fn = { params; body = fn_body; return_type_ };
     tid = None;
   }
    : Clam.top_func_item)

let make_top_closure_item (addr : Fn_address.t) (params : Ident.t list)
    (return_type_ : Ltype.t list) ~(closure_ty : Ltype.t) =
  (let closure_address = Fn_address.make_closure_wrapper addr in
   let closure_body =
     let args = Lst.map params (fun p -> Clam.Lvar { var = p }) in
     Clam.Lapply { fn = Clam.StaticFn addr; args; prim = None }
   in
   let env_id = Ident.fresh ~ty:closure_ty "*env" in
   match[@warning "-fragile-match"] closure_ty with
   | Ref { tid } ->
       {
         binder = closure_address;
         fn_kind_ = Top_private;
         fn = { params = env_id :: params; body = closure_body; return_type_ };
         tid = Some (Tid.code_pointer_of_closure tid);
       }
   | _ -> assert false
    : Clam.top_func_item)

let make_object_wrapper ~mtype_defs ~type_defs ~addr_tbl ~object_methods
    ~abstract_obj_tid ~concrete_obj_tid ~self_lty ~method_index ~trait
    ~number_of_methods
    ({ method_id; method_prim; method_ty } : Object_util.object_method_item) =
  let params_ty, ret_ty =
    match method_ty with
    | T_func { params; return } -> (params, return)
    | T_maybe_uninit _ -> assert false
    | T_optimized_option _ -> assert false
    | T_double -> assert false
    | T_uint64 -> assert false
    | T_constr _ -> assert false
    | T_fixedarray _ -> assert false
    | T_bytes -> assert false
    | T_uint16 -> assert false
    | T_byte -> assert false
    | T_uint -> assert false
    | T_bool -> assert false
    | T_char -> assert false
    | T_int -> assert false
    | T_unit -> assert false
    | T_int16 -> assert false
    | T_string -> assert false
    | T_tuple _ -> assert false
    | T_trait _ -> assert false
    | T_int64 -> assert false
    | T_float -> assert false
    | T_any _ -> assert false
    | T_raw_func _ -> assert false
    | T_error_value_result _ -> assert false
  in
  match[@warning "-fragile-match"] params_ty with
  | self_ty :: params_ty -> (
      let self_id = Core_ident.fresh "*self" in
      let params_id = Lst.map params_ty (fun _ -> Core_ident.fresh "*param") in
      let body =
        let args =
          Mcore.var ~prim:None ~ty:self_ty self_id
          :: Lst.map2 params_id params_ty (fun id ->
                 fun ty -> Mcore.var ~prim:None ~ty id)
        in
        match method_prim with
        | Some (Pintrinsic _) | None ->
            Mcore.apply ~prim:method_prim ~ty:ret_ty
              ~kind:(Normal { func_ty = method_ty })
              method_id args
        | Some prim -> Mcore.prim ~ty:ret_ty prim args
      in
      let body =
        transl_expr ~name_hint:"" ~mtype_defs ~addr_tbl ~type_defs
          ~object_methods body
      in
      let self_id = Ident.of_core_ident ~ty:self_lty self_id in
      let obj_ty : Ltype.t = Ref { tid = abstract_obj_tid } in
      let obj_id = Ident.fresh ~ty:obj_ty "*obj" in
      let casted_obj_ty : Ltype.t = Ref { tid = concrete_obj_tid } in
      let body : Clam.lambda =
        Llet
          {
            name = self_id;
            e =
              Lget_field
                {
                  obj =
                    Lcast
                      {
                        expr = Lvar { var = obj_id };
                        target_type = casted_obj_ty;
                      };
                  tid = concrete_obj_tid;
                  index = 0;
                  kind = Object { number_of_methods };
                };
            body;
          }
      in
      let fn : Clam.fn =
        {
          params =
            obj_id
            :: Lst.map2 params_ty params_id (fun ty ->
                   fun id ->
                    let ty =
                      Transl_mtype.mtype_to_ltype ~type_defs ~mtype_defs ty
                    in
                    Ident.of_core_ident ~ty id);
          return_type_ =
            [ Transl_mtype_gc.mtype_to_ltype ~type_defs ~mtype_defs ret_ty ];
          body;
        }
      in
      match[@warning "-fragile-match"] method_id with
      | Pdot qual_name ->
          let addr = Fn_address.make_object_wrapper qual_name ~trait in
          new_top
            {
              binder = addr;
              fn_kind_ = Top_private;
              fn;
              tid = Some (Tid.method_of_object abstract_obj_tid method_index);
            };
          addr
      | _ -> assert false)
  | _ -> assert false

let transl_top_func ~(mtype_defs : Mtype.defs) ~(addr_tbl : Addr_table.t)
    ~type_defs ~object_methods (func : Mcore.fn) (binder : Core_ident.t) is_pub_
    =
  match[@warning "-fragile-match"] Addr_table.find_exn addr_tbl binder with
  | Toplevel { addr; params; return; _ } ->
      let fn_item =
        make_top_fn_item ~mtype_defs ~addr_tbl ~type_defs ~object_methods addr
          params (`Core func.body) return ~export_info_:is_pub_
      in
      new_top fn_item
  | _ -> assert false

let sequence (exprs : Clam.lambda list) (last_expr : Clam.lambda) =
  let is_unit x = Basic_prelude.phys_equal (Clam_util.no_located x) clam_unit in
  let rec loop rev_items = function
    | [] -> (
        match rev_items with
        | prev_expr :: rev_items when is_unit last_expr ->
            if rev_items = [] then prev_expr
            else
              Clam.Lsequence
                { exprs = List.rev rev_items; last_expr = prev_expr }
        | _ :: _ -> Lsequence { exprs = List.rev rev_items; last_expr }
        | [] -> last_expr)
    | expr :: remain ->
        if is_unit expr then loop rev_items remain
        else loop (expr :: rev_items) remain
  in
  loop [] exprs

type translate_result = {
  globals : (Ident.t * Constant.t option) list;
  init : Clam.lambda;
  test : Clam.lambda;
}

let transl_top_item ~(mtype_defs : Mtype.defs) ~(addr_tbl : Addr_table.t)
    ~(type_defs : Ltype.type_defs_with_context) ~object_methods
    (top : Mcore.top_item) (acc : translate_result) =
  (match top with
   | Ctop_expr { expr; loc_ } ->
       let name_hint = "*init*" in
       base := loc_;
       let clam_expr =
         transl_expr ~mtype_defs ~name_hint ~addr_tbl ~type_defs ~object_methods
           expr
       in
       { acc with init = sequence [ clam_expr ] acc.init }
   | Ctop_let { binder; expr; is_pub_ = _; loc_ } -> (
       base := loc_;
       match expr with
       | Cexpr_function { func; ty = _ } ->
           transl_top_func ~mtype_defs ~addr_tbl ~type_defs ~object_methods func
             binder None;
           acc
       | _ -> (
           let ty = Mcore.type_of_expr expr in
           let name =
             Ident.of_core_ident
               ~ty:(Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs ty)
               binder
           in
           let expr =
             transl_expr ~name_hint:(Ident.to_string name) ~mtype_defs ~addr_tbl
               ~type_defs ~object_methods expr
           in
           match Clam_util.no_located expr with
           | Lconst
               ((C_bool _ | C_char _ | C_int _ | C_int64 _ | C_double _) as c)
             ->
               { acc with globals = (name, Some c) :: acc.globals }
           | Lconst (C_string _ as c) when !Basic_config.use_js_builtin_string
             ->
               { acc with globals = (name, Some c) :: acc.globals }
           | _ ->
               {
                 acc with
                 globals = (name, None) :: acc.globals;
                 init = Llet { name; e = expr; body = acc.init };
               }))
   | Ctop_fn { binder; func; export_info_; loc_ } ->
       base := loc_;
       transl_top_func ~mtype_defs ~addr_tbl ~type_defs ~object_methods func
         binder export_info_;
       acc
   | Ctop_stub { binder; func_stubs; params_ty; return_ty; export_info_; loc_ }
     -> (
       base := loc_;
       match[@warning "-fragile-match"] Addr_table.find_exn addr_tbl binder with
       | Toplevel { addr; params; return; _ } ->
           let args =
             Lst.map2 params params_ty (fun id ->
                 fun ty ->
                  (let var : Clam.lambda = Lvar { var = id } in
                   if Mtype.is_func ty then
                     Lprim { fn = Pclosure_to_extern_ref; args = [ var ] }
                   else var
                    : Clam.lambda))
           in
           let params_ty =
             Lst.map2 params params_ty (fun id ->
                 fun ty ->
                  (if Mtype.is_func ty then Ref_extern else Ident.get_type id
                    : Ltype.t))
           in
           let return_ty =
             match return_ty with
             | None -> None
             | Some _ -> (
                 match[@warning "-fragile-match"] return with
                 | return_ltype :: [] -> Some return_ltype
                 | _ -> assert false)
           in
           let body : Clam.lambda =
             Levent
               {
                 expr =
                   Lstub_call { fn = func_stubs; args; params_ty; return_ty };
                 loc_;
               }
           in
           let ret = Option.value return_ty ~default:Ltype.i32_unit in
           let fn_item =
             make_top_fn_item ~mtype_defs ~addr_tbl ~type_defs ~object_methods
               addr params (`Clam body) [ ret ] ~export_info_
           in
           new_top fn_item;
           acc
       | _ -> assert false)
    : translate_result)

let collect_top_func ~mtype_defs ~type_defs (item : Mcore.top_item)
    ~(addr_tbl : Addr_table.t) =
  match item with
  | Ctop_expr _ -> ()
  | Ctop_let { binder; expr = Cexpr_function { func; _ } }
  | Ctop_fn { binder; func; _ } ->
      let return =
        [
          Transl_mtype.mtype_to_ltype ~mtype_defs ~type_defs
            (Mcore.type_of_expr func.body);
        ]
      in
      let params = transl_params ~mtype_defs func.params ~type_defs in
      Addr_table.add_toplevel_fn_addr addr_tbl binder ~params ~return
  | Ctop_stub { binder; params_ty; return_ty; _ } ->
      let params =
        Lst.map params_ty (fun ty ->
            Ident.fresh "*param"
              ~ty:(Transl_mtype.mtype_to_ltype ty ~type_defs ~mtype_defs))
      in
      let return =
        match return_ty with
        | Some return_ty ->
            Transl_mtype.mtype_to_ltype return_ty ~type_defs ~mtype_defs
        | None -> Ltype.i32_unit
      in
      Addr_table.add_toplevel_fn_addr addr_tbl binder ~params ~return:[ return ]
  | Ctop_let _ -> ()

let non_well_knowns_obj =
  object
    inherit [_] Mcore.Iter.iter

    method! visit_Cexpr_var (ctx : Core_ident.Hashset.t) id _ty _prim _loc =
      Core_ident.Hashset.add ctx id
  end

let collect_local_non_well_knowns (ctx : Core_ident.Hashset.t) (prog : Mcore.t)
    =
  Lst.iter prog.body ~f:(non_well_knowns_obj#visit_top_item ctx);
  match prog.main with
  | None -> ()
  | Some (main, _) -> non_well_knowns_obj#visit_expr ctx main

let transl_prog ({ body; main; types; object_methods = _ } as prog : Mcore.t) =
  (binds_init := [];
   Ident_hashset.reset local_non_well_knowns;
   collect_local_non_well_knowns local_non_well_knowns prog;
   let type_defs = Transl_mtype.transl_mtype_defs types in
   let addr_tbl = Addr_table.create 17 in
   Lst.iter body ~f:(collect_top_func ~type_defs ~mtype_defs:types ~addr_tbl);
   let object_methods = Object_util.Hash.create 17 in
   Object_util.Hash.iter2 prog.object_methods (fun obj_key ->
       fun { self_ty; methods } ->
        let ({ trait; type_ } : Object_util.object_key) = obj_key in
        let abstract_obj_tid = Tid.of_type_path trait in
        let concrete_obj_tid =
          Tid.concrete_object_type ~trait ~type_name:type_
        in
        let self_lty =
          Transl_mtype_gc.mtype_to_ltype ~type_defs ~mtype_defs:types self_ty
        in
        Tid.Hash.add type_defs.defs concrete_obj_tid
          (Ref_concrete_object { abstract_obj_tid; self = self_lty });
        let number_of_methods = List.length methods in
        let addrs =
          Lst.mapi methods (fun method_index ->
              fun method_item ->
               make_object_wrapper ~type_defs ~mtype_defs:types ~addr_tbl
                 ~object_methods ~abstract_obj_tid ~concrete_obj_tid ~self_lty
                 ~method_index ~trait ~number_of_methods method_item)
        in
        Object_util.Hash.add object_methods obj_key addrs);
   (match main with
   | Some (main, loc_) ->
       collect_top_func ~type_defs ~mtype_defs:types
         (Ctop_expr { expr = main; loc_ })
         ~addr_tbl
   | None -> ());
   let acc = { globals = []; init = clam_unit; test = clam_unit } in
   let main =
     match main with
     | None -> None
     | Some (main, loc_) ->
         base := loc_;
         Some
           (transl_expr ~mtype_defs:types ~name_hint:"*main*" ~addr_tbl
              ~type_defs ~object_methods main)
   in
   let { globals; init; test } =
     Lst.fold_right body acc
       (transl_top_item ~mtype_defs:types ~addr_tbl ~type_defs ~object_methods)
   in
   let start = sequence [ init ] test in
   let globals, init, fns =
     Addr_table.fold addr_tbl (globals, start, !binds_init) (fun _fn_binder ->
         fun fn_info ->
          fun (globals, start, fns) ->
           match fn_info with
           | Toplevel { name_as_closure = Some name; params; return; addr } -> (
               let closure_ty : Ltype.t =
                 match Ident.get_type name with
                 | Ref_any ->
                     let sig_ : Ltype.fn_sig =
                       { params = Lst.map params Ident.get_type; ret = return }
                     in
                     Ref
                       {
                         tid =
                           Ltype.FnSigHash.find_exn type_defs.fn_sig_tbl sig_;
                       }
                 | ty -> ty
               in
               let closure_item =
                 make_top_closure_item addr params return ~closure_ty
               in
               let address = closure_item.binder in
               match[@warning "-fragile-match"] closure_ty with
               | Ref { tid } ->
                   let fn_closure =
                     Clam.Lclosure
                       { captures = []; address = Normal address; tid }
                   in
                   ( (name, None) :: globals,
                     Clam.Llet { name; e = fn_closure; body = start },
                     closure_item :: fns )
               | _ -> assert false)
           | Toplevel { name_as_closure = None; _ } | Local _ ->
               (globals, start, fns))
   in
   { fns; init; main; globals; type_defs = type_defs.defs }
    : Clam.prog)
