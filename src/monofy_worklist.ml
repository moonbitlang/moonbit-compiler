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


module Ident = Basic_core_ident
module Qual_ident = Basic_qual_ident
module Q = Qual_ident_tbl
module Arr = Basic_arr
module Type_path = Basic_type_path
module Object_hash = Object_util.Hash
module Vec_int = Basic_vec_int
module Vec = Basic_vec

type t = {
  todo : Monofy_instances.t Q.t;
  complete : Monofy_instances.t Q.t;
  objects : Object_util.t;
  used_error_to_string : bool ref;
  error_types : Ident.t option Type_path.Hash.t;
  dependency : (int * Ident.Hashset.t) Ident.Hash.t;
  obj_to_string : Method_env.method_info option Type_path.Hash.t;
}

let make () =
  ({
     todo = Q.create 17;
     complete = Q.create 17;
     objects = Object_hash.create 17;
     used_error_to_string = ref false;
     error_types = Type_path.Hash.create 17;
     dependency = Ident.Hash.create 17;
     obj_to_string = Type_path.Hash.create 17;
   }
    : t)

let add_value_if_not_exist (worklist : t) (qual_name : Q.key)
    (tys : Type_args.t) =
  (let monofy_ident (qual_name : Qual_ident.t) (tys : Type_args.t) =
     (let q : Basic_qual_ident.t =
        if Arr.is_empty tys then qual_name
        else
          Basic_qual_ident.map qual_name ~f:(fun s ->
              Stdlib.String.concat "" [ s; "|"; Type_args.mangle tys; "|" ])
      in
      Ident.of_qual_ident q
       : Ident.t)
   in
   let complete_item =
     match Q.find_opt worklist.complete qual_name with
     | Some items -> Monofy_instances.has_type_args items tys
     | None -> false
   in
   let id = monofy_ident qual_name tys in
   (if not complete_item then
      match Q.find_opt worklist.todo qual_name with
      | Some items ->
          if not (Monofy_instances.has_type_args items tys) then
            Q.replace worklist.todo qual_name
              ({ binder = id; types = tys; old_binder = qual_name } :: items)
      | None ->
          Q.add worklist.todo qual_name
            [ { binder = id; types = tys; old_binder = qual_name } ]);
   id
    : Ident.t)

let add_to_complete (wl : t) (qual_name : Q.key)
    (new_items : Monofy_instances.t) =
  match Q.find_opt wl.complete qual_name with
  | Some complete_items ->
      Q.replace wl.complete qual_name (new_items @ complete_items)
  | None -> Q.add wl.complete qual_name new_items

let get_todo_items_and_mark_as_analyzed (wl : t) =
  (let items = ref [] in
   Q.iter2 wl.todo (fun qual_name ->
       fun item ->
        add_to_complete wl qual_name item;
        items := item @ !items);
   Q.clear wl.todo;
   !items
    : Monofy_instances.t)

let find_analyzed_items (wl : t) (qual_name : Q.key) =
  (Q.find_default wl.complete qual_name [] : Monofy_instances.t)

let find_new_binder_exn (wl : t) (qual_name : Q.key) (tys : Type_args.t) =
  (let complete_item =
     match Q.find_opt wl.complete qual_name with
     | Some items -> Monofy_instances.find_item items tys
     | None -> None
   in
   match complete_item with
   | Some item -> item
   | None -> (
       let items = Q.find_exn wl.todo qual_name in
       match Monofy_instances.find_item items tys with
       | Some item -> item
       | None -> assert false)
    : Ident.t)

let find_new_binder_opt (wl : t) (qual_name : Q.key) (tys : Type_args.t) =
  (let complete_item =
     match Q.find_opt wl.complete qual_name with
     | Some items -> Monofy_instances.find_item items tys
     | None -> None
   in
   match complete_item with
   | Some item -> Some item
   | None -> (
       match Q.find_opt wl.todo qual_name with
       | Some items -> Monofy_instances.find_item items tys
       | None -> None)
    : Ident.t option)

let has_object (worklist : t) ~(trait : Type_path.t) ~(type_ : string) =
  (Object_hash.mem worklist.objects { trait; type_ } : bool)

let add_object_methods (worklist : t) ~(trait : Type_path.t) ~(type_ : string)
    ~self_ty ~(methods : Object_util.object_method_item list) =
  Object_hash.add worklist.objects { trait; type_ } { self_ty; methods }

let get_all_object_methods (worklist : t) = worklist.objects

let add_error_type (worklist : t) (tag : Basic_constr_info.constr_tag) =
  let type_path =
    match tag with
    | Extensible_tag { pkg; type_name; _ } ->
        Type_path.toplevel_type ~pkg type_name
    | _ -> assert false
  in
  ignore
    (Type_path.Hash.add_or_update worklist.error_types type_path
       ~update:(fun t -> t)
       None)

let check_impl_show_trait ~(monofy_env : Monofy_env.t) ~type_name =
  if
    Monofy_env.find_method_opt monofy_env ~type_name ~method_name:"output"
      ~trait:Type_path.Builtin.trait_show
    <> None
  then
    Monofy_env.find_method_opt monofy_env ~type_name ~method_name:"to_string"
      ~trait:Type_path.Builtin.trait_show
  else None

let add_error_to_string (worklist : t) ~(monofy_env : Monofy_env.t) =
  Type_path.Hash.iter worklist.error_types (fun (type_name, _) ->
      let err_ty : Stype.t =
        T_constr
          {
            type_constructor = type_name;
            tys = [];
            generic_ = false;
            is_suberror_ = true;
          }
      in
      match check_impl_show_trait ~monofy_env ~type_name with
      | Some method_ ->
          let new_ident =
            add_value_if_not_exist worklist method_.id [| err_ty |]
          in
          Type_path.Hash.replace worklist.error_types type_name (Some new_ident)
      | None -> ())

let get_used_error_to_string (worklist : t) =
  (!(worklist.used_error_to_string) : bool)

let set_used_error_to_string (worklist : t) =
  (worklist.used_error_to_string := true : unit)

let error_to_string_binder =
  Ident.of_qual_ident (Basic_qual_ident.make ~pkg:"Error" ~name:"to_string")

let decode_type_from_tag_str (s : string) =
  match String.split_on_char '.' s with
  | [ pkg; type_name; _ ] -> Some (Type_path.toplevel_type ~pkg type_name)
  | [ type_name; _ ] -> Some (Type_path.toplevel_type ~pkg:"" type_name)
  | _ -> None

let make_error_to_string (worklist : t) ~(tags : int Basic_hash_string.t) =
  (let err_ty : Mtype.t = T_constr Mtype.error_mid in
   let e_id = Ident.fresh "*e" in
   let param : Mcore.param =
     { binder = e_id; ty = err_ty; loc_ = Rloc.no_location }
   in
   if Basic_hash_string.length tags = 0 then
     Ctop_fn
       {
         binder = error_to_string_binder;
         func = { params = [ param ]; body = Mcore.prim ~ty:T_string Ppanic [] };
         export_info_ = None;
         loc_ = Loc.no_location;
       }
   else
     let match_obj = Mcore.var e_id ~ty:err_ty ~prim:None in
     let cases = Basic_vec.empty () in
     let make_case (tag_name : string) (index : int) =
       let tag : Tag.t = { index; is_constant_ = false; name_ = tag_name } in
       let default () =
         let str = Mcore.const (C_string tag_name) in
         Basic_vec.push cases (tag, None, str)
           [@@local]
       in
       match decode_type_from_tag_str tag_name with
       | Some type_name -> (
           match Type_path.Hash.find_opt worklist.error_types type_name with
           | Some (Some func) ->
               let method_ty : Mtype.t =
                 T_func
                   { params = [ T_constr Mtype.error_mid ]; return = T_string }
               in
               let kind = Mcore.Normal { func_ty = method_ty } in
               let args = [ match_obj ] in
               let action =
                 Mcore.apply ~prim:None ~ty:T_string ~kind func args
               in
               Basic_vec.push cases (tag, None, action)
           | _ -> default ())
       | None -> default ()
     in
     Basic_hash_string.iter tags (fun (tag, idx) -> make_case tag idx);
     let cases = Basic_vec.to_list cases in
     let body =
       Mcore.switch_constr ~loc:Rloc.no_location ~default:None match_obj cases
     in
     let func : Mcore.fn = { params = [ param ]; body } in
     Ctop_fn
       {
         binder = error_to_string_binder;
         func;
         export_info_ = None;
         loc_ = Loc.no_location;
       }
    : Mcore.top_item)

let add_dependency (worklist : t) (ident : Ident.t) (dep : Ident.t) =
  (match Ident.Hash.find_opt worklist.dependency ident with
   | Some (_, deps) -> Ident.Hashset.add deps dep
   | None ->
       let deps = Ident.Hashset.create 17 in
       Ident.Hashset.add deps dep;
       let index = Ident.Hash.length worklist.dependency in
       Ident.Hash.add worklist.dependency ident (index, deps)
    : unit)

let order_globals (worklist : t)
    (globals : (Ident.t * Mcore.expr * bool * Loc.t) Vec.t) =
  (let indexed_globals = Basic_hash_int.create 17 in
   let res = Vec.empty () in
   Vec.iter globals (fun ((id, _, _, _) as global) ->
       match Ident.Hash.find_opt worklist.dependency id with
       | Some (index, _) -> Basic_hash_int.add indexed_globals index global
       | None -> Vec.push res global);
   if Vec.length res = Vec.length globals then res
   else
     let nodes_num = Ident.Hash.length worklist.dependency in
     let dep_graph = Array.init nodes_num (fun _ -> Vec_int.empty ()) in
     Ident.Hash.iter worklist.dependency (fun (_, (src_index, deps)) ->
         Ident.Hashset.iter deps (fun d ->
             match Ident.Hash.find_opt worklist.dependency d with
             | Some (tgt_index, _) ->
                 Vec_int.push dep_graph.(src_index) tgt_index
             | None -> ()));
     let scc = Basic_scc.graph dep_graph in
     Vec.iter scc (fun c ->
         Vec_int.iter c (fun i ->
             match Basic_hash_int.find_opt indexed_globals i with
             | Some global -> Vec.push res global
             | None -> ()));
     res
    : (Ident.t * Mcore.expr * bool * Loc.t) Vec.t)

let add_any_to_string (worklist : t) (monofy_env : Monofy_env.t) (ty : Stype.t)
    =
  let ty = Stype.type_repr ty in
  let local_cache : (Method_env.method_info * Stype.t) Basic_vec.t =
    Basic_vec.empty ()
  in
  let resolve_by_type_path (tp : Type_path.t) =
    (match Type_path.Hash.find_opt worklist.obj_to_string tp with
     | Some (Some mi) ->
         Basic_vec.push local_cache (mi, ty);
         true
     | Some None -> false
     | None -> (
         match check_impl_show_trait ~monofy_env ~type_name:tp with
         | Some mi ->
             Basic_vec.push local_cache (mi, ty);
             Type_path.Hash.add worklist.obj_to_string tp (Some mi);
             true
         | None ->
             Type_path.Hash.add worklist.obj_to_string tp None;
             false)
      : bool)
  in
  let rec resolve_by_type (ty : Stype.t) =
    match ty with
    | T_constr { type_constructor = tp; tys; _ } ->
        resolve_by_type_path tp && Basic_lst.for_all tys resolve_by_type
    | T_builtin b -> resolve_by_type_path (Stype.tpath_of_builtin b)
    | T_trait _ | Tarrow _ | Tvar _ | Tparam _ | T_blackhole -> false
  in
  if resolve_by_type ty then
    Basic_vec.iter local_cache (fun (mi, ty) ->
        ignore (add_value_if_not_exist worklist mi.id [| ty |]))

let transl_any_to_string (worklist : t) (ty : Stype.t) =
  (let ty = Stype.type_repr ty in
   let resolve_by_type_path (tp : Type_path.t) =
     match Type_path.Hash.find_opt worklist.obj_to_string tp with
     | Some (Some mi) -> (
         match find_new_binder_opt worklist mi.id [| ty |] with
         | Some id -> Some (id, mi.prim)
         | None -> None)
     | _ -> None
   in
   match ty with
   | T_constr { type_constructor = tp; _ } -> resolve_by_type_path tp
   | T_builtin b -> resolve_by_type_path (Stype.tpath_of_builtin b)
   | T_trait _ | Tarrow _ | Tvar _ | Tparam _ | T_blackhole -> None
    : (Ident.t * Primitive.prim option) option)
