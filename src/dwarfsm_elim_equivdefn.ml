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


module Ast = Dwarfsm_ast
module String_table = Basic_hash_string
module StringSet = Basic_set_string

type fn_table = Ast.func String_table.t
type index_table = string String_table.t
type unchecked_typs = Ast.typeidx list String_table.t
type unchecked_fns = Ast.funcidx list String_table.t
type fnsig = Ast.param list * Ast.result list * Ast.local list

include struct
  let _ = fun (_ : fnsig) -> ()

  let equal_fnsig =
    (fun a__001_ ->
       fun b__002_ ->
        let t__003_, t__004_, t__005_ = a__001_ in
        let t__006_, t__007_, t__008_ = b__002_ in
        Stdlib.( && )
          (Ppx_base.equal_list
             (fun a__009_ -> fun b__010_ -> Ast.equal_param a__009_ b__010_)
             t__003_ t__006_)
          (Stdlib.( && )
             (Ppx_base.equal_list
                (fun a__011_ -> fun b__012_ -> Ast.equal_result a__011_ b__012_)
                t__004_ t__007_)
             (Ppx_base.equal_list
                (fun a__013_ -> fun b__014_ -> Ast.equal_local a__013_ b__014_)
                t__005_ t__008_))
      : fnsig -> fnsig -> bool)

  let _ = equal_fnsig

  let (hash_fold_fnsig : Ppx_base.state -> fnsig -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let e0, e1, e2 = arg in
     let hsv = Ppx_base.hash_fold_list Ast.hash_fold_param hsv e0 in
     let hsv = Ppx_base.hash_fold_list Ast.hash_fold_result hsv e1 in
     let hsv = Ppx_base.hash_fold_list Ast.hash_fold_local hsv e2 in
     hsv

  let _ = hash_fold_fnsig

  let (hash_fnsig : fnsig -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_fnsig hsv arg)
    in
    fun x -> func x

  let _ = hash_fnsig
end

module SubtypHash = struct
  type t = Ast.subtype

  let equal = Ast.equal_subtype
  let hash = Ast.hash_subtype
  let sexp_of_t _ = assert false
end

module SubtypMap = Basic_hashf.Make (SubtypHash)

module FnSigHash = struct
  type t = fnsig

  let equal = equal_fnsig
  let hash = hash_fnsig
  let sexp_of_t _ = assert false
end

module FnSigMap = Basic_hashf.Make (FnSigHash)

let equal_funcidx = Ast.equal_funcidx
and equal_typeuse = Ast.equal_typeuse
and equal_catchs = Ast.equal_catchs
and equal_instr = Ast.equal_instr

module Implicits (Arg : sig
  val unchecked_fns : unchecked_fns
  val unchecked_typs : unchecked_typs
  val type_equiv_relation : index_table
  val func_equiv_relation : index_table
  val fns : fn_table
  val subtyp_set : string SubtypMap.t
  val func_set : Ast.func list FnSigMap.t
end) =
struct
  let unchecked_fns = Arg.unchecked_fns
  let unchecked_typs = Arg.unchecked_typs
  let type_equiv_relation = Arg.type_equiv_relation
  let func_equiv_relation = Arg.func_equiv_relation
  let fns = Arg.fns
  let subtyp_set = Arg.subtyp_set
  let func_set = Arg.func_set

  let add_unchecked_typ name idx =
    match String_table.find_opt unchecked_typs name with
    | None -> String_table.add unchecked_typs name [ idx ]
    | Some fns -> String_table.replace unchecked_typs name (idx :: fns)

  let add_unchecked_fn name idx =
    match String_table.find_opt unchecked_fns name with
    | None -> String_table.add unchecked_fns name [ idx ]
    | Some fns -> String_table.replace unchecked_fns name (idx :: fns)

  let rewrite_typeidx (typeidx : Ast.typeidx) =
    match[@warning "-fragile-match"] typeidx.var with
    | Unresolve name -> (
        match String_table.find_opt type_equiv_relation name with
        | None -> add_unchecked_typ name typeidx
        | Some rootname -> typeidx.var <- Unresolve rootname)
    | _ -> assert false

  let rewrite_funcidx (funcidx : Ast.funcidx) =
    match[@warning "-fragile-match"] funcidx.var with
    | Unresolve name -> (
        match String_table.find_opt func_equiv_relation name with
        | None -> add_unchecked_fn name funcidx
        | Some rootname -> funcidx.var <- Unresolve rootname)
    | _ -> assert false

  let rewrite_valtype (valtype : Ast.valtype) =
    match valtype with
    | Reftype (Ref (_, Type typeidx)) -> rewrite_typeidx typeidx
    | _ -> ()

  let rewrite_local (local : Ast.local) = rewrite_valtype local.type_

  let rewrite_subtype (subtype : Ast.subtype) =
    let rewrite_fieldtype (fieldtype : Ast.fieldtype) =
      match fieldtype.type_ with
      | Valtype valtype -> rewrite_valtype valtype
      | _ -> ()
    in
    let rewrite_field (field : Ast.field) = rewrite_fieldtype field.fieldtype in
    match subtype.type_ with
    | Arraytype (Array fieldtype) -> rewrite_fieldtype fieldtype
    | Structtype (Struct fieldlist) -> Basic_lst.iter fieldlist ~f:rewrite_field
    | Functype (Func (params, results)) ->
        Basic_lst.iter params ~f:rewrite_local;
        Basic_lst.iter results ~f:rewrite_valtype

  let rewrite_typeuse (typeuse : Ast.typeuse) =
    match typeuse with
    | Use (typeidx, params, results) ->
        rewrite_typeidx typeidx;
        Basic_lst.iter params ~f:rewrite_local;
        Basic_lst.iter results ~f:rewrite_valtype
    | Inline (params, results) ->
        Basic_lst.iter params ~f:rewrite_local;
        Basic_lst.iter results ~f:rewrite_valtype

  let rewrite_fnsig (fnsig : fnsig) =
    let params, results, locals = fnsig in
    Basic_lst.iter params ~f:rewrite_local;
    Basic_lst.iter results ~f:rewrite_valtype;
    Basic_lst.iter locals ~f:rewrite_local

  let rewrite_typedef (typedef : Ast.typedef) =
    let binder, subtype = typedef in
    rewrite_subtype subtype;
    match[@warning "-fragile-match"] binder with
    | { id = Some name; _ } -> (
        match SubtypMap.find_opt subtyp_set subtype with
        | None ->
            SubtypMap.add subtyp_set subtype name;
            String_table.add type_equiv_relation name name;
            Some (binder, subtype)
        | Some rootname ->
            String_table.add type_equiv_relation name rootname;
            (match String_table.find_opt unchecked_typs name with
            | None -> ()
            | Some typeidxes ->
                Basic_lst.iter typeidxes ~f:(fun (idx : Ast.typeidx) ->
                    idx.var <- Unresolve rootname));
            None)
    | _ -> assert false

  let get_fnsig (func : Ast.func) =
    match func.type_ with
    | Use (typeidx, params, results) ->
        rewrite_typeidx typeidx;
        (params, results, func.locals)
    | Inline (params, results) -> (params, results, func.locals)

  let rec equal_instrs_with ~(visited_l : StringSet.t)
      ~(visited_r : StringSet.t) (lhs : Ast.instr list) (rhs : Ast.instr list) =
    match (lhs, rhs) with
    | [], [] -> true
    | instr_l :: lhs, instr_r :: rhs ->
        (match (instr_l, instr_r) with
        | ( Call ({ var = Unresolve name_l } as funcidx_l),
            Call ({ var = Unresolve name_r } as funcidx_r) )
        | ( Ref_func ({ var = Unresolve name_l; _ } as funcidx_l),
            Ref_func ({ var = Unresolve name_r; _ } as funcidx_r) ) -> (
            (StringSet.mem visited_l name_l && StringSet.mem visited_r name_r)
            || equal_funcidx funcidx_l funcidx_r
            ||
            let left = String_table.find_opt fns name_l in
            let right = String_table.find_opt fns name_r in
            match (left, right) with
            | Some left, Some right -> (
                match[@warning "-fragile-match"] (left.id, right.id) with
                | { id = Some name_l; _ }, { id = Some name_r; _ } ->
                    let visited_l = StringSet.add visited_l name_l in
                    let visited_r = StringSet.add visited_r name_r in
                    rewrite_instrs left.code;
                    rewrite_instrs right.code;
                    let lhs = left.code in
                    let rhs = right.code in
                    equal_instrs_with ~visited_l ~visited_r lhs rhs
                | _ -> assert false)
            | _ -> false)
        | ( Try_table
              { typeuse = typeuse_l; catchs = catchs_l; instrs = code_l; _ },
            Try_table
              { typeuse = typeuse_r; catchs = catchs_r; instrs = code_r; _ } )
          ->
            equal_typeuse typeuse_l typeuse_r
            && equal_catchs catchs_l catchs_r
            && equal_instrs_with ~visited_l ~visited_r code_l code_r
        | ( Loop { typeuse = typeuse_l; instrs = code_l; _ },
            Loop { typeuse = typeuse_r; instrs = code_r; _ } ) ->
            equal_typeuse typeuse_l typeuse_r
            && equal_instrs_with ~visited_l ~visited_r code_l code_r
        | ( Block { typeuse = typeuse_l; instrs = code_l; _ },
            Block { typeuse = typeuse_r; instrs = code_r; _ } ) ->
            equal_typeuse typeuse_l typeuse_r
            && equal_instrs_with ~visited_l ~visited_r code_l code_r
        | ( If { typeuse = typeuse_l; then_ = then_l; else_ = else_l; _ },
            If { typeuse = typeuse_r; then_ = then_r; else_ = else_r; _ } ) ->
            equal_typeuse typeuse_l typeuse_r
            && equal_instrs_with ~visited_l ~visited_r then_l then_r
            && equal_instrs_with ~visited_l ~visited_r else_l else_r
        | _ -> equal_instr instr_l instr_r)
        && equal_instrs_with ~visited_l ~visited_r lhs rhs
    | _ -> false

  and rewrite_instrs (instrs : Ast.instr list) =
    match instrs with
    | [] -> ()
    | instr :: instrs ->
        (match instr with
        | Array_copy (typeidx1, typeidx2) ->
            rewrite_typeidx typeidx1;
            rewrite_typeidx typeidx2
        | Array_fill typeidx -> rewrite_typeidx typeidx
        | Array_get typeidx -> rewrite_typeidx typeidx
        | Array_get_s typeidx -> rewrite_typeidx typeidx
        | Array_get_u typeidx -> rewrite_typeidx typeidx
        | Array_new typeidx -> rewrite_typeidx typeidx
        | Array_new_data (typeidx, _) -> rewrite_typeidx typeidx
        | Array_new_default typeidx -> rewrite_typeidx typeidx
        | Array_new_fixed (typeidx, _) -> rewrite_typeidx typeidx
        | Array_set typeidx -> rewrite_typeidx typeidx
        | Call funcidx -> rewrite_funcidx funcidx
        | Call_indirect (_, typeuse) -> rewrite_typeuse typeuse
        | Call_ref typeidx -> rewrite_typeidx typeidx
        | Ref_cast (Ref (_, Type typeidx)) -> rewrite_typeidx typeidx
        | Ref_func funcidx -> rewrite_funcidx funcidx
        | Ref_null (Type typeidx) -> rewrite_typeidx typeidx
        | Struct_get (typeidx, _) -> rewrite_typeidx typeidx
        | Struct_new typeidx -> rewrite_typeidx typeidx
        | Struct_new_default typeidx -> rewrite_typeidx typeidx
        | Struct_set (typeidx, _) -> rewrite_typeidx typeidx
        | Try_table { typeuse; instrs; _ }
        | Loop { typeuse; instrs; _ }
        | Block { typeuse; instrs; _ } ->
            rewrite_typeuse typeuse;
            rewrite_instrs instrs
        | If { typeuse; then_; else_; _ } ->
            rewrite_typeuse typeuse;
            rewrite_instrs then_;
            rewrite_instrs else_
        | _ -> ());
        rewrite_instrs instrs

  let rewrite_func (func : Ast.func) =
    (match[@warning "-fragile-match"] func.id with
     | { id = Some name; _ } -> (
         let fnsig = get_fnsig func in
         rewrite_fnsig fnsig;
         rewrite_instrs func.code;
         match FnSigMap.find_opt func_set fnsig with
         | None ->
             FnSigMap.add func_set fnsig [ func ];
             String_table.add func_equiv_relation name name;
             Some func
         | Some funclist ->
             let res = ref (Some func) in
             let rec aux (funcs : Ast.func list) =
               match funcs with
               | [] ->
                   FnSigMap.replace func_set fnsig (func :: funclist);
                   String_table.add func_equiv_relation name name
               | f :: funcs -> (
                   match[@warning "-fragile-match"] f.id with
                   | { id = Some fname; _ } ->
                       if
                         equal_instrs_with
                           ~visited_l:(StringSet.singleton fname)
                           ~visited_r:(StringSet.singleton name) f.code
                           func.code
                       then (
                         res := None;
                         match[@warning "-fragile-match"] f.id with
                         | { id = Some rootname; _ } -> (
                             String_table.add func_equiv_relation name rootname;
                             match String_table.find_opt unchecked_fns name with
                             | None -> ()
                             | Some funcidxes ->
                                 Basic_lst.iter funcidxes
                                   ~f:(fun (idx : Ast.funcidx) ->
                                     idx.var <- Unresolve rootname))
                         | _ -> assert false)
                       else aux funcs
                   | _ -> assert false)
             in
             aux funclist;
             !res)
     | _ -> assert false
      : Ast.func option)

  let elim_equiv_defn (module_ : Ast.module_) =
    (let rec aux (mfs : Ast.modulefield list) =
       match mfs with
       | [] -> []
       | mf :: mfs -> (
           match mf with
           | Import { desc = Func (funcidx, _); _ } -> (
               match[@warning "-fragile-match"] funcidx with
               | { id = Some name; _ } ->
                   String_table.add func_equiv_relation name name;
                   mf :: aux mfs
               | _ -> assert false)
           | Global { type_ = { type_ = valtype; _ }; init; _ } ->
               rewrite_valtype valtype;
               rewrite_instrs init;
               mf :: aux mfs
           | Rectype (typedef :: []) -> (
               match rewrite_typedef typedef with
               | None -> aux mfs
               | Some typedef -> Ast.Rectype [ typedef ] :: aux mfs)
           | Rectype typedefs ->
               let typedefs =
                 List.concat_map
                   (fun typedef ->
                     match rewrite_typedef typedef with
                     | None -> []
                     | Some typedef -> [ typedef ])
                   typedefs
               in
               Ast.Rectype typedefs :: aux mfs
           | Func func -> (
               match rewrite_func func with
               | None -> aux mfs
               | Some func -> Ast.Func func :: aux mfs)
           | Export { desc = Func funcidx; _ } | Start funcidx ->
               rewrite_funcidx funcidx;
               mf :: aux mfs
           | Elem { list; _ } ->
               Basic_lst.iter list ~f:rewrite_instrs;
               mf :: aux mfs
           | _ -> mf :: aux mfs)
     in
     let module_ = { module_ with fields = aux module_.fields } in
     module_
      : Ast.module_)
end

let create_fn_table (mfs : Ast.modulefield list) =
  (let fns = String_table.create 400 in
   let aux (mf : Ast.modulefield) =
     match mf with
     | Func ({ id = { id = Some name; _ }; _ } as func) ->
         String_table.add fns name func
     | _ -> ()
   in
   Basic_lst.iter mfs ~f:aux;
   fns
    : fn_table)

let elim_equiv_defn (module_ : Ast.module_) =
  (let module I = Implicits (struct
     let type_equiv_relation : index_table = String_table.create 500
     let func_equiv_relation : index_table = String_table.create 500
     let unchecked_fns : unchecked_fns = String_table.create 500
     let unchecked_typs : unchecked_typs = String_table.create 500
     let subtyp_set = SubtypMap.create 500
     let func_set = FnSigMap.create 500
     let fns = create_fn_table module_.fields
   end) in
   Dwarfsm_local_resolve.resolve module_;
   I.elim_equiv_defn module_
    : Ast.module_)
