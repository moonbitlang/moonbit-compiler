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


module Qual_ident = Basic_qual_ident
module Hash_string = Basic_hash_string
module Lst = Basic_lst

type types = {
  type_decls : Typedecl_info.t Hash_string.t;
  trait_decls : Trait_decl.t Hash_string.t;
}

include struct
  let _ = fun (_ : types) -> ()

  let sexp_of_types =
    (fun { type_decls = type_decls__002_; trait_decls = trait_decls__004_ } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__005_ =
           Hash_string.sexp_of_t Trait_decl.sexp_of_t trait_decls__004_
         in
         (S.List [ S.Atom "trait_decls"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ =
           Hash_string.sexp_of_t Typedecl_info.sexp_of_t type_decls__002_
         in
         (S.List [ S.Atom "type_decls"; arg__003_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : types -> S.t)

  let _ = sexp_of_types
end

type values = {
  values : Value_info.toplevel Hash_string.t;
  constructors : Typedecl_info.constructor list Hash_string.t;
  fields : Typedecl_info.field list Hash_string.t;
}

include struct
  let _ = fun (_ : values) -> ()

  let sexp_of_values =
    (fun {
           values = values__007_;
           constructors = constructors__009_;
           fields = fields__011_;
         }
     ->
       let bnds__006_ = ([] : _ Stdlib.List.t) in
       let bnds__006_ =
         let arg__012_ =
           Hash_string.sexp_of_t
             (Moon_sexp_conv.sexp_of_list Typedecl_info.sexp_of_field)
             fields__011_
         in
         (S.List [ S.Atom "fields"; arg__012_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__010_ =
           Hash_string.sexp_of_t
             (Moon_sexp_conv.sexp_of_list Typedecl_info.sexp_of_constructor)
             constructors__009_
         in
         (S.List [ S.Atom "constructors"; arg__010_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__008_ =
           Hash_string.sexp_of_t Value_info.sexp_of_toplevel values__007_
         in
         (S.List [ S.Atom "values"; arg__008_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       S.List bnds__006_
      : values -> S.t)

  let _ = sexp_of_values
end

type stype_defs = types Hash_string.t

include struct
  let _ = fun (_ : stype_defs) -> ()

  let sexp_of_stype_defs =
    (fun x__013_ -> Hash_string.sexp_of_t sexp_of_types x__013_
      : stype_defs -> S.t)

  let _ = sexp_of_stype_defs
end

let make_types () =
  { type_decls = Hash_string.create 17; trait_decls = Hash_string.create 17 }

let init_types types traits =
  let type_decls = Hash_string.create (Array.length types * 3 / 2) in
  Array.iter (fun (name, decl) -> Hash_string.add type_decls name decl) types;
  let trait_decls = Hash_string.create (Array.length types * 3 / 2) in
  Array.iter (fun (name, decl) -> Hash_string.add trait_decls name decl) traits;
  { type_decls; trait_decls }

let make_values () =
  {
    values = Hash_string.create 17;
    constructors = Hash_string.create 17;
    fields = Hash_string.create 17;
  }

let add_type (types : types) (name : string) (decl : Typedecl_info.t) =
  Hash_string.add types.type_decls name decl

let add_trait (types : types) (name : string) (decl : Trait_decl.t) =
  Hash_string.add types.trait_decls name decl

let add_value (info : values) (value : Value_info.toplevel) =
  Hash_string.add info.values (Qual_ident.base_name value.id) value

let add_constructor (info : values) (x : Typedecl_info.constructor) =
  ignore
    (Hash_string.add_or_update info.constructors x.constr_name [ x ]
       ~update:(fun cs -> x :: cs))

let add_field (info : values) ~(field : Typedecl_info.field) =
  ignore
    (Hash_string.add_or_update info.fields field.field_name [ field ]
       ~update:(fun fields -> field :: fields))

let find_type (info : types) name = Hash_string.find_opt info.type_decls name

let find_type_exn (info : types) name =
  Hash_string.find_exn info.type_decls name

let find_trait (info : types) name = Hash_string.find_opt info.trait_decls name

let find_trait_exn (info : types) name =
  Hash_string.find_exn info.trait_decls name

let find_value (info : values) name = Hash_string.find_opt info.values name

let find_constructor (info : values) name =
  Option.value ~default:[] (Hash_string.find_opt info.constructors name)

let find_all_fields (info : values) (name : string) =
  Hash_string.find_default info.fields name []
[@@inline]

let find_field (info : values) name =
  match find_all_fields info name with entry :: _ -> Some entry | [] -> None

let get_all_types (info : types) =
  (Hash_string.to_array info.type_decls : (string * Typedecl_info.t) array)

let get_all_traits (info : types) =
  (Hash_string.to_array info.trait_decls : (string * Trait_decl.t) array)

let iter_types (info : types) (f : string * Typedecl_info.t -> unit) =
  Hash_string.iter info.type_decls f

let get_pub_types (info : types) =
  (Hash_string.to_array_filter_map info.type_decls (fun (name, decl) ->
       match decl.ty_vis with
       | Vis_priv -> None
       | Vis_default -> Some (name, { decl with ty_desc = Abstract_type })
       | Vis_fully_pub | Vis_readonly ->
           let decl =
             match decl.ty_desc with
             | Record_type { has_private_field_ = true; fields } ->
                 let fields = Lst.filter fields (fun f -> f.vis <> Invisible) in
                 {
                   decl with
                   ty_desc = Record_type { has_private_field_ = true; fields };
                 }
             | _ -> decl
           in
           Some (name, decl))
    : (string * Typedecl_info.t) array)

let get_pub_traits (info : types) =
  (Hash_string.to_array_filter_map info.trait_decls (fun (name, trait) ->
       if trait.vis_ <> Vis_priv then Some (name, trait) else None)
    : (string * Trait_decl.t) array)

let iter_traits (info : types) (f : string * Trait_decl.t -> unit) =
  Hash_string.iter info.trait_decls f

let get_pub_values (info : values) =
  (Hash_string.to_array_filter_map info.values (fun (name, vd) ->
       if vd.pub then Some (name, vd) else None)
    : (string * Value_info.toplevel) array)

let iter_values (info : values) (f : Value_info.toplevel -> unit) =
  Hash_string.iter info.values (fun (_, entry) -> f entry)

let iter_constructors (info : values)
    (f : string * Typedecl_info.constructor -> unit) =
  Hash_string.iter info.constructors (fun (name, constrs) ->
      Basic_lst.iter constrs ~f:(fun constr -> f (name, constr)))

let iter_fields (info : values) (f : string * Typedecl_info.field -> unit) =
  Hash_string.iter info.fields (fun (field_name, fields) ->
      Basic_lst.iter fields ~f:(fun field_desc -> f (field_name, field_desc)))
