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


module Vec = Basic_vec
module Arr = Basic_arr
module Type_path = Basic_type_path
module Hash_string = Basic_hash_string

type mi_view = {
  values : Value_info.toplevel Hash_string.t;
  external_types : Typedecl_info.t Hash_string.t;
  external_traits : Trait_decl.t Hash_string.t;
  external_constrs : Typedecl_info.constructor list Hash_string.t;
  external_type_alias : Type_alias.t Hash_string.t;
  method_env : Method_env.t;
  ext_method_env : Ext_method_env.t;
  trait_impls : Trait_impl.t;
  name : string;
}

include struct
  let _ = fun (_ : mi_view) -> ()

  let sexp_of_mi_view =
    (fun {
           values = values__002_;
           external_types = external_types__004_;
           external_traits = external_traits__006_;
           external_constrs = external_constrs__008_;
           external_type_alias = external_type_alias__010_;
           method_env = method_env__012_;
           ext_method_env = ext_method_env__014_;
           trait_impls = trait_impls__016_;
           name = name__018_;
         }
     ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__019_ = Moon_sexp_conv.sexp_of_string name__018_ in
         (S.List [ S.Atom "name"; arg__019_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__017_ = Trait_impl.sexp_of_t trait_impls__016_ in
         (S.List [ S.Atom "trait_impls"; arg__017_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__015_ = Ext_method_env.sexp_of_t ext_method_env__014_ in
         (S.List [ S.Atom "ext_method_env"; arg__015_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__013_ = Method_env.sexp_of_t method_env__012_ in
         (S.List [ S.Atom "method_env"; arg__013_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__011_ =
           Hash_string.sexp_of_t Type_alias.sexp_of_t external_type_alias__010_
         in
         (S.List [ S.Atom "external_type_alias"; arg__011_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__009_ =
           Hash_string.sexp_of_t
             (Moon_sexp_conv.sexp_of_list Typedecl_info.sexp_of_constructor)
             external_constrs__008_
         in
         (S.List [ S.Atom "external_constrs"; arg__009_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__007_ =
           Hash_string.sexp_of_t Trait_decl.sexp_of_t external_traits__006_
         in
         (S.List [ S.Atom "external_traits"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ =
           Hash_string.sexp_of_t Typedecl_info.sexp_of_t external_types__004_
         in
         (S.List [ S.Atom "external_types"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ =
           Hash_string.sexp_of_t Value_info.sexp_of_toplevel values__002_
         in
         (S.List [ S.Atom "values"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : mi_view -> S.t)

  let _ = sexp_of_mi_view
end

type trait_impl_info = {
  impl : Trait_impl.impl;
  methods : (string * Method_env.method_info) Vec.t;
}

type type_info = {
  path : Type_path.t;
  decl : Typedecl_info.t option;
  methods : (string * Method_env.method_info) Vec.t;
  impls : trait_impl_info Vec.t;
}

type trait_info = {
  path : Type_path.t;
  decl : Trait_decl.t;
  default_impls : (string * Method_env.method_info) Vec.t;
  impls : trait_impl_info Vec.t;
}

type mi_readable_view = {
  name : string;
  values : (string * Value_info.toplevel) Vec.t;
  types : type_info array;
  traits : trait_info array;
  type_alias : Type_alias.t array;
}

module Trait_impl_hash = Basic_hashf.Make (struct
  type t = Type_path.t * Type_path.t

  include struct
    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun (arg0__020_, arg1__021_) ->
         let res0__022_ = Type_path.sexp_of_t arg0__020_
         and res1__023_ = Type_path.sexp_of_t arg1__021_ in
         S.List [ res0__022_; res1__023_ ]
        : t -> S.t)

    let _ = sexp_of_t

    let equal =
      (fun a__024_ ->
         fun b__025_ ->
          let t__026_, t__027_ = a__024_ in
          let t__028_, t__029_ = b__025_ in
          Stdlib.( && )
            (Type_path.equal t__026_ t__028_)
            (Type_path.equal t__027_ t__029_)
        : t -> t -> bool)

    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
     fun hsv ->
      fun arg ->
       let e0, e1 = arg in
       let hsv = Type_path.hash_fold_t hsv e0 in
       let hsv = Type_path.hash_fold_t hsv e1 in
       hsv

    let _ = hash_fold_t

    let (hash : t -> Ppx_base.hash_value) =
      let func arg =
        Ppx_base.get_hash_value
          (let hsv = Ppx_base.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x

    let _ = hash
  end
end)

let to_readable_view (mi : mi_view) =
  let types = Type_path.Hash.create 17 in
  Hash_string.iter2 mi.external_types (fun _ ->
      fun decl ->
       Type_path.Hash.add types decl.ty_constr
         {
           path = decl.ty_constr;
           decl = Some decl;
           methods = Vec.empty ();
           impls = Vec.empty ();
         });
  let get_type_info_or_add_as_foreign path =
    match Type_path.Hash.find_opt types path with
    | Some info -> info
    | None ->
        let info =
          { path; decl = None; methods = Vec.empty (); impls = Vec.empty () }
        in
        Type_path.Hash.add types path info;
        info
  in
  let traits = Type_path.Hash.create 17 in
  Hash_string.iter2 mi.external_traits (fun _ ->
      fun trait_decl ->
       Type_path.Hash.add traits trait_decl.name
         {
           path = trait_decl.name;
           decl = trait_decl;
           default_impls = Vec.empty ();
           impls = Vec.empty ();
         });
  let values = Vec.empty () in
  Hash_string.iter2 mi.values (fun name ->
      fun value_info ->
       match value_info.direct_use_loc_ with
       | Explicit_import _ | Implicit_import_all _ -> ()
       | Not_direct_use -> Vec.push values (name, value_info));
  Method_env.iter mi.method_env (fun type_name ->
      fun method_name ->
       fun method_info ->
        match method_info.id with
        | Qmethod _ ->
            let type_info = get_type_info_or_add_as_foreign type_name in
            Vec.push type_info.methods (method_name, method_info)
        | _ -> ());
  let trait_impls = Trait_impl_hash.create 17 in
  Trait_impl.iter mi.trait_impls (fun ~trait ->
      fun ~type_name ->
       fun impl ->
        if not impl.is_implicit_ then
          Trait_impl_hash.add trait_impls (trait, type_name)
            { impl; methods = Vec.empty () });
  Ext_method_env.iter mi.ext_method_env
    (fun ({ trait; self_type; method_name }, method_info) ->
      if Type_path.equal self_type Type_path.Builtin.default_impl_placeholder
      then
        let trait_info = Type_path.Hash.find_exn traits trait in
        Vec.push trait_info.default_impls (method_name, method_info)
      else
        match Trait_impl_hash.find_opt trait_impls (trait, self_type) with
        | None -> ()
        | Some impl_info -> Vec.push impl_info.methods (method_name, method_info));
  Trait_impl_hash.iter2 trait_impls (fun (trait, type_name) ->
      fun impl ->
       Vec.sort impl.methods (fun (name1, _) ->
           fun (name2, _) -> String.compare name1 name2);
       match Type_path.Hash.find_opt types type_name with
       | Some { decl = Some _; impls; _ } -> Vec.push impls impl
       | _ -> (
           match Type_path.Hash.find_opt traits trait with
           | Some trait_info -> Vec.push trait_info.impls impl
           | None ->
               let type_info = get_type_info_or_add_as_foreign type_name in
               Vec.push type_info.impls impl));
  let type_alias = Hash_string.to_array_map mi.external_type_alias snd in
  let types = Type_path.Hash.to_array_map types snd in
  Array.sort
    (fun (t1 : type_info) ->
      fun (t2 : type_info) ->
       match (t1.decl, t2.decl) with
       | Some _, None -> -1
       | None, Some _ -> 1
       | Some _, Some _ | None, None -> Type_path.compare t1.path t2.path)
    types;
  Arr.iter types (fun type_info ->
      let { path = _; decl = _; methods; impls } = type_info in
      Vec.sort methods (fun (name1, _) ->
          fun (name2, _) -> String.compare name1 name2);
      Vec.sort impls (fun impl1 ->
          fun impl2 -> Type_path.compare impl1.impl.trait impl2.impl.trait));
  let traits = Type_path.Hash.to_array_map traits snd in
  Array.sort (fun tr1 -> fun tr2 -> Type_path.compare tr1.path tr2.path) traits;
  Arr.iter traits (fun trait_info ->
      let { path = _; decl = _; default_impls; impls } = trait_info in
      Vec.sort default_impls (fun (name1, _) ->
          fun (name2, _) -> String.compare name1 name2);
      Vec.sort impls (fun impl1 ->
          fun impl2 ->
           let ty1 =
             Stype.extract_tpath_exn (Stype.type_repr impl1.impl.self_ty)
           in
           let ty2 =
             Stype.extract_tpath_exn (Stype.type_repr impl2.impl.self_ty)
           in
           Type_path.compare ty1 ty2));
  Vec.sort values (fun (name1, _) ->
      fun (name2, _) -> String.compare name1 name2);
  Array.sort
    (fun (alias1 : Type_alias.t) ->
      fun (alias2 : Type_alias.t) -> String.compare alias1.name alias2.name)
    type_alias;
  { name = mi.name; values; types; traits; type_alias }
