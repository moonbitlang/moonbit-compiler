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


module Hash_string = Basic_hash_string
module Hashset_string = Basic_hashset_string
module Lst = Basic_lst
module Syntax = Parsing_syntax
module Type_path = Basic_type_path
module Longident = Basic_longident

type t = {
  types : Syntax.type_decl Hash_string.t;
  traits : Syntax.trait_decl Hash_string.t;
  trait_alias : Syntax.impl Hash_string.t;
  newtype_deps : string list Hash_string.t;
  type_alias_deps : string list Hash_string.t;
}

let find_type_opt (env : t) (name : string) =
  Hash_string.find_opt env.types name

let find_trait_opt (env : t) (name : string) =
  Hash_string.find_opt env.traits name

let find_trait_alias (env : t) (name : string) =
  Hash_string.find_opt env.trait_alias name

let find_type_alias_deps (env : t) (name : string) =
  Hash_string.find_opt env.type_alias_deps name

let make ~type_defs ~trait_defs ~trait_alias =
  let newtype_deps = Hash_string.create 17 in
  let type_alias_deps = Hash_string.create 17 in
  Hash_string.iter type_defs (fun (name, (decl : Syntax.type_decl)) ->
      let collect_use (exclude : Hashset_string.t) (typ : Syntax.typ) =
        let result = ref [] in
        let rec go (typ : Syntax.typ) =
          match typ with
          | Ptype_any _ -> ()
          | Ptype_arrow { ty_arg; ty_res; ty_err; is_async = _ } -> (
              Lst.iter ty_arg ~f:go;
              go ty_res;
              match ty_err with
              | Error_typ { ty = ty_err } -> go ty_err
              | No_error_typ | Default_error_typ _ -> ())
          | Ptype_tuple { tys } -> Lst.iter tys ~f:go
          | Ptype_name { constr_id; tys } -> (
              Lst.iter tys ~f:go;
              match constr_id.lid with
              | Lident name ->
                  if not (Hashset_string.mem exclude name) then
                    result := name :: !result
              | Ldot _ -> ())
          | Ptype_option { ty; _ } -> go ty
          | Ptype_object { lid = Lident name } ->
              if not (Hashset_string.mem exclude name) then
                result := name :: !result
          | Ptype_object { lid = Ldot _ } -> ()
        in
        go typ;
        !result
      in
      match decl.components with
      | Ptd_newtype typ ->
          let exclude = Hashset_string.create 17 in
          Lst.iter decl.params ~f:(fun tvar ->
              match tvar.tvar_name with
              | Some tvar_name -> Hashset_string.add exclude tvar_name
              | None -> ());
          Hash_string.add newtype_deps name (collect_use exclude typ)
      | Ptd_alias typ ->
          let exclude = Hashset_string.create 17 in
          Lst.iter decl.params ~f:(fun tvar ->
              match tvar.tvar_name with
              | Some tvar_name -> Hashset_string.add exclude tvar_name
              | None -> ());
          Hash_string.add type_alias_deps name (collect_use exclude typ)
      | Ptd_abstract | Ptd_extern | Ptd_variant _ | Ptd_record _ | Ptd_error _
        ->
          ());
  {
    types = type_defs;
    traits = trait_defs;
    newtype_deps;
    type_alias_deps;
    trait_alias;
  }

let types_to_list_map (type a) (env : t) (f : string -> Syntax.type_decl -> a) =
  (Hash_string.to_list_with env.types f : a list)

let iter_types (env : t) (f : string -> Syntax.type_decl -> unit) =
  Hash_string.iter2 env.types f

let iter_traits (env : t) (f : string -> Syntax.trait_decl -> unit) =
  (Hash_string.iter2 env.traits f : unit)

let iter_trait_alias (env : t) (f : string -> Syntax.impl -> unit) =
  (Hash_string.iter2 env.trait_alias f : unit)

let traits_to_list_map (type a) (env : t) (f : string -> Syntax.trait_decl -> a)
    =
  (Hash_string.to_list_with env.traits f : a list)

let newtype_in_cycle (env : t) (name : string) =
  (let visiting = Hashset_string.create 17 in
   let rec go (name : string) =
     if Hashset_string.mem visiting name then true
     else
       match Hash_string.find_opt env.newtype_deps name with
       | None -> false
       | Some deps ->
           Hashset_string.add visiting name;
           let result = Lst.exists deps (fun dep -> go dep) in
           Hashset_string.remove visiting name;
           result
   in
   go name
    : bool)

type trait_info =
  | Local_trait of Type_path.t * Syntax.trait_decl
  | Foreign_trait of Trait_decl.t

let typing_trait_name ~types (env : t) (trait : Syntax.type_name) =
  let find_foreign ~is_alias =
    match Global_env.All_types.find_trait types trait.name ~loc:trait.loc_ with
    | Ok decl ->
        let type_name : Typedtree.type_name =
          if is_alias then
            Tname_alias
              {
                name = decl.name;
                kind = Trait;
                alias_ = trait.name;
                loc_ = trait.loc_;
              }
          else Tname_path { name = decl.name; kind = Trait; loc_ = trait.loc_ }
        in
        Ok (Foreign_trait decl, type_name)
    | Error err -> Error err
      [@@local]
  in
  match Global_env.All_types.find_type_alias types trait.name with
  | Some { target = Trait_alias { trait = path }; _ } -> (
      match path with
      | Toplevel { pkg; id } when pkg = !Basic_config.current_package -> (
          match find_trait_opt env id with
          | Some decl ->
              let type_name : Typedtree.type_name =
                Tname_alias
                  {
                    name = path;
                    kind = Trait;
                    alias_ = trait.name;
                    loc_ = trait.loc_;
                  }
              in
              Ok (Local_trait (path, decl), type_name)
          | None -> Error Errors.swallow_error)
      | _ -> find_foreign ~is_alias:true)
  | Some { target = Type_alias _; _ } ->
      Error
        (Errors.not_a_trait
           ~name:(Longident.to_string trait.name)
           ~loc:trait.loc_)
  | None -> (
      match trait.name with
      | Lident name -> (
          match find_trait_opt env name with
          | Some decl ->
              let path =
                Type_path.toplevel_type ~pkg:!Basic_config.current_package name
              in
              let type_name : Typedtree.type_name =
                Tname_path { name = path; kind = Trait; loc_ = trait.loc_ }
              in
              Ok (Local_trait (path, decl), type_name)
          | None -> find_foreign ~is_alias:false)
      | Ldot _ -> find_foreign ~is_alias:false)
