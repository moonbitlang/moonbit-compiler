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


module StringSet = Basic_set_string
module Lst = Basic_lst
module Ast = Dwarfsm_ast
type program_item =
  | Binding of (string * Ast.modulefield)
  | Rec_type of (string list * Ast.modulefield)
  | Raw of Ast.modulefield
let extract_item keep_types (mf : Ast.modulefield) =
  (match mf with
   | Ast.Func { id = { id = Some name;_};_} -> Binding (name, mf)
   | Ast.Global { id = { id = Some name;_};_} | Ast.Data
     { id = { id = Some name;_};_} | Ast.Import
     { desc = Func ({ id = Some name;_}, _);_} | Ast.Import
     { desc = Global ({ id = Some name;_}, _);_} | Ast.Rectype
     (({ id = Some name;_}, _)::[]) ->
       if keep_types then Raw mf else Binding (name, mf)
   | Ast.Rectype typs ->
       let names =
         Lst.fold_right typs []
           (fun t ->
              fun acc ->
                match t with
                | ({ id = Some name;_}, _) -> name :: acc
                | _ -> acc) in
       Rec_type (names, mf)
   | _ -> Raw mf : program_item)
let get_all_ident_var ~except:(except : StringSet.t)  (cell : Ast.var) =
  (match cell with
   | Resolved { var_name;_} | Unresolve var_name ->
       if not (StringSet.mem except var_name)
       then StringSet.singleton var_name
       else StringSet.empty : StringSet.t)
let get_all_ident_binder ~except:(except : StringSet.t)
  (binder : Ast.binder) =
  (match binder with
   | { id = Some name;_} ->
       if not (StringSet.mem except name)
       then StringSet.singleton name
       else StringSet.empty
   | _ -> StringSet.empty : StringSet.t)
let get_all_ident_heaptype ~except:(except : StringSet.t)
  (ht : Ast.heaptype) =
  match ht with
  | Type { var = v } -> get_all_ident_var ~except v
  | _ -> StringSet.empty
let get_all_ident_valtype ~except:(except : StringSet.t)  (vt : Ast.valtype)
  =
  match vt with
  | Reftype (Ref (_, ht)) -> get_all_ident_heaptype ~except ht
  | _ -> StringSet.empty
let get_all_ident_storagetype ~except:(except : StringSet.t)
  (stt : Ast.storagetype) =
  match stt with
  | Valtype vt -> get_all_ident_valtype ~except vt
  | Packedtype _ -> StringSet.empty
let get_all_ident_local ~except:(except : StringSet.t)  (local : Ast.local) =
  StringSet.union (get_all_ident_binder ~except local.id)
    (get_all_ident_valtype ~except local.type_)
let get_all_ident_locals ~except:(except : StringSet.t)
  (locals : Ast.local list) =
  List.fold_left
    (fun acc ->
       fun local -> StringSet.union (get_all_ident_local ~except local) acc)
    StringSet.empty locals
let rec get_all_ident_typeuse ~except:(except : StringSet.t)
  (fntype : Ast.typeuse) =
  let get_all_ident_vts ~except:(except : StringSet.t)
    (vts : Ast.valtype list) =
    List.fold_left
      (fun acc ->
         fun vt -> StringSet.union (get_all_ident_valtype ~except vt) acc)
      StringSet.empty vts in
  match fntype with
  | Use ({ var = id }, params, results) ->
      StringSet.union (get_all_ident_var ~except id)
        (get_all_ident_typeuse ~except (Inline (params, results)))
  | Inline (params, results) ->
      StringSet.union (get_all_ident_vts ~except results)
        (get_all_ident_locals ~except params)
let get_all_ident_comptype ~except:(except : StringSet.t)
  (ct : Ast.comptype) =
  let unwarp (field : Ast.field) = (field.fieldtype).type_ in
  match ct with
  | Arraytype (Array ft) -> get_all_ident_storagetype ~except ft.type_
  | Structtype (Struct fl) ->
      let sttl = Lst.map fl unwarp in
      List.fold_left
        (fun acc ->
           fun stt ->
             StringSet.union (get_all_ident_storagetype ~except stt) acc)
        StringSet.empty sttl
  | Functype (Func (params, results)) ->
      get_all_ident_typeuse ~except (Inline (params, results))
let get_all_ident_subtype ~except:(except : StringSet.t)  (st : Ast.subtype)
  =
  StringSet.union (get_all_ident_comptype ~except st.type_)
    (Lst.fold_left st.super StringSet.empty
       (fun acc ->
          fun super ->
            StringSet.union acc (get_all_ident_var ~except super.var)))
let get_all_ident_typedef ~except:(except : StringSet.t)  (td : Ast.typedef)
  =
  let (v, st) = td in
  StringSet.union (get_all_ident_binder ~except v)
    (get_all_ident_subtype ~except st)
let get_all_ident_rectype ~except:(except : StringSet.t)  (rt : Ast.rectype)
  =
  List.fold_left
    (fun acc ->
       fun typedef ->
         StringSet.union (get_all_ident_typedef ~except typedef) acc)
    StringSet.empty rt
let get_all_ident_importdesc ~except:(except : StringSet.t)
  (importdesc : Ast.importdesc) =
  match importdesc with
  | Func (v, typeuse) ->
      StringSet.union (get_all_ident_binder ~except v)
        (get_all_ident_typeuse ~except typeuse)
  | Table (v, tabletype) ->
      let Ref (_, ht) = tabletype.element_type in
      StringSet.union (get_all_ident_binder ~except v)
        (get_all_ident_heaptype ~except ht)
  | Memory (v, _) -> get_all_ident_binder ~except v
  | Global (v, gt) ->
      StringSet.union (get_all_ident_binder ~except v)
        (get_all_ident_valtype ~except gt.type_)
  | Tag (v, typeuse) ->
      StringSet.union (get_all_ident_binder ~except v)
        (get_all_ident_typeuse ~except typeuse)
let rec get_all_ident_catchs ~except:(except : StringSet.t)
  (catchs : Ast.catch list) =
  match catchs with
  | [] -> StringSet.empty
  | (Catch ({ var = var1 }, { var = var2 }))::catchs ->
      StringSet.union (get_all_ident_var ~except var1)
        (StringSet.union (get_all_ident_var ~except var2)
           (get_all_ident_catchs ~except catchs))
let get_all_ident_instrs ~except:(except : StringSet.t)
  (instrs : Ast.instr list) =
  let identifiers = ref StringSet.empty in
  let rec aux (instrs : Ast.instr list) =
    match instrs with
    | [] -> ()
    | (Call { var })::instrs | (Call_ref { var })::instrs | (Array_fill
      { var })::instrs | (Array_get { var })::instrs | (Array_get_u
      { var })::instrs | (Array_new { var })::instrs | (Array_new_default
      { var })::instrs | (Array_new_fixed ({ var }, _))::instrs | (Array_set
      { var })::instrs | (Ref_func { var })::instrs | (Struct_new
      { var })::instrs | (Struct_new_default { var })::instrs | (Struct_get
      ({ var }, _))::instrs | (Struct_set ({ var }, _))::instrs | (Global_get
      { var })::instrs | (Global_set { var })::instrs | (Table_get
      { var })::instrs | (Memory_init { var })::instrs | (Throw
      { var })::instrs ->
        (identifiers :=
           (StringSet.union (!identifiers) (get_all_ident_var ~except var));
         aux instrs)
    | (Call_indirect (_, typ))::instrs ->
        (identifiers :=
           (StringSet.union (!identifiers)
              (get_all_ident_typeuse ~except typ));
         aux instrs)
    | (Array_new_data ({ var = v1 }, { var }))::instrs ->
        (identifiers :=
           (StringSet.union (!identifiers)
              (StringSet.union (get_all_ident_var ~except v1)
                 (get_all_ident_var ~except var)));
         aux instrs)
    | (Array_copy ({ var = v1 }, { var = v2 }))::instrs ->
        (identifiers :=
           (StringSet.union (!identifiers)
              (StringSet.union (get_all_ident_var ~except v1)
                 (get_all_ident_var ~except v2)));
         aux instrs)
    | (Ref_null ht)::instrs | (Ref_cast (Ref (_, ht)))::instrs ->
        (identifiers :=
           (StringSet.union (!identifiers)
              (get_all_ident_heaptype ~except ht));
         aux instrs)
    | (Try_table { typeuse; catchs; instrs;_})::rest ->
        (identifiers :=
           (StringSet.union (!identifiers)
              (StringSet.union (get_all_ident_typeuse ~except typeuse)
                 (get_all_ident_catchs ~except catchs)));
         aux instrs;
         aux rest)
    | (If { then_; else_;_})::rest -> (aux then_; aux else_; aux rest)
    | (Block { instrs;_})::rest -> (aux instrs; aux rest)
    | (Loop { instrs;_})::rest -> (aux instrs; aux rest)
    | _::instrs -> aux instrs in
  aux instrs; !identifiers
let get_all_ident ~except:(except : StringSet.t)  (mf : Ast.modulefield) =
  (match mf with
   | Rectype rectype -> get_all_ident_rectype ~except rectype
   | Import import -> get_all_ident_importdesc ~except import.desc
   | Func func ->
       StringSet.union (get_all_ident_binder ~except func.id)
         (StringSet.union (get_all_ident_locals ~except func.locals)
            (StringSet.union (get_all_ident_typeuse ~except func.type_)
               (get_all_ident_instrs ~except func.code)))
   | Global global ->
       StringSet.union (get_all_ident_instrs ~except global.init)
         (StringSet.union
            (get_all_ident_valtype ~except (global.type_).type_)
            (get_all_ident_binder ~except global.id))
   | Data data -> get_all_ident_binder ~except data.id
   | _ -> StringSet.empty : StringSet.t)
let get_all_ident_elem ~except:(except : StringSet.t)
  (expr : Ast.instr list) =
  match expr with
  | (Ref_func { var = Unresolve name })::[] ->
      if not (StringSet.mem except name)
      then StringSet.singleton name
      else StringSet.empty
  | _ -> assert false
let get_exported (mfs : Ast.modulefield list) =
  (let exports = ref StringSet.empty in
   let go mf =
     match mf with
     | Ast.Export { desc = Func { var = Unresolve name };_} | Ast.Export
       { desc = Table { var = Unresolve name };_} | Ast.Export
       { desc = Memory { var = Unresolve name };_} | Ast.Export
       { desc = Global { var = Unresolve name };_} | Ast.Start
       { var = Unresolve name } -> exports := (StringSet.add (!exports) name)
     | Ast.Elem { list = elems;_} ->
         exports :=
           (StringSet.union (!exports)
              (List.fold_left StringSet.union StringSet.empty
                 (Lst.map elems (get_all_ident_elem ~except:StringSet.empty))))
     | _ -> () in
   Basic_lst.iter mfs ~f:go; !exports : StringSet.t)
let slice_prog keep_types (body : Ast.modulefield list) =
  (Lst.map body (extract_item keep_types) : program_item list)
let dependency_of_exports
  (get_all_ident : except:StringSet.t -> Ast.modulefield -> StringSet.t)
  (items : program_item list) (exported : StringSet.t) =
  (let tbl = Hashtbl.create 50 in
   let collect_item (item : program_item) =
     match item with
     | Binding (x, mf) -> Hashtbl.add tbl x mf
     | Rec_type (names, mf) ->
         Lst.iter names ~f:(fun name -> Hashtbl.add tbl name mf)
     | Raw _ -> () in
   Lst.iter items ~f:collect_item;
   (let deps_of_exported = ref exported in
    let update_bit = ref true in
    let visited = ref StringSet.empty in
    while !update_bit do
      (update_bit := false;
       StringSet.iter (!deps_of_exported)
         (fun name ->
            if not (StringSet.mem (!visited) name)
            then
              (visited := (StringSet.add (!visited) name);
               (match Hashtbl.find_opt tbl name with
                | Some mf ->
                    let new_deps =
                      get_all_ident ~except:(!deps_of_exported) mf in
                    (update_bit :=
                       ((!update_bit) || (not (StringSet.is_empty new_deps)));
                     deps_of_exported :=
                       (StringSet.union (!deps_of_exported) new_deps))
                | None -> ()))))
      done;
    !deps_of_exported) : StringSet.t)
let rebuild_program (items : program_item list) usage =
  (Lst.fold_right items []
     (fun item ->
        fun acc ->
          match item with
          | Binding (name, mf) ->
              if StringSet.mem usage name then mf :: acc else acc
          | Rec_type (names, mf) ->
              if Lst.exists names (fun name -> StringSet.mem usage name)
              then mf :: acc
              else acc
          | Raw mf -> mf :: acc) : Ast.modulefield list)
let shrink ?(keep_types= false)  (mod_ : Ast.module_) =
  (let code = mod_.fields in
   let items = slice_prog keep_types code in
   let exported = get_exported code in
   let usage = dependency_of_exports get_all_ident items exported in
   {
     id = (Wasmlinear_constr.empty_binder ());
     fields = (rebuild_program items usage)
   } : Ast.module_)
