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


module Type_path = Basic_type_path
module Hash_string = Basic_hash_string
module Lst = Basic_lst

type location = Rloc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Rloc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type constructor = Typedecl_info.constructor

include struct
  let _ = fun (_ : constructor) -> ()

  let sexp_of_constructor =
    (Typedecl_info.sexp_of_constructor : constructor -> S.t)

  let _ = sexp_of_constructor
end

type field = Typedecl_info.field

include struct
  let _ = fun (_ : field) -> ()
  let sexp_of_field = (Typedecl_info.sexp_of_field : field -> S.t)
  let _ = sexp_of_field
end

type newtype_info = Typedecl_info.newtype_info

include struct
  let _ = fun (_ : newtype_info) -> ()

  let sexp_of_newtype_info =
    (Typedecl_info.sexp_of_newtype_info : newtype_info -> S.t)

  let _ = sexp_of_newtype_info
end

type local_type_kind =
  | Enum of constructor list
  | Struct of field list
  | Newtype of newtype_info
  | Placeholder

include struct
  let _ = fun (_ : local_type_kind) -> ()

  let sexp_of_local_type_kind =
    (function
     | Enum arg0__001_ ->
         let res0__002_ =
           Moon_sexp_conv.sexp_of_list sexp_of_constructor arg0__001_
         in
         S.List [ S.Atom "Enum"; res0__002_ ]
     | Struct arg0__003_ ->
         let res0__004_ =
           Moon_sexp_conv.sexp_of_list sexp_of_field arg0__003_
         in
         S.List [ S.Atom "Struct"; res0__004_ ]
     | Newtype arg0__005_ ->
         let res0__006_ = sexp_of_newtype_info arg0__005_ in
         S.List [ S.Atom "Newtype"; res0__006_ ]
     | Placeholder -> S.Atom "Placeholder"
      : local_type_kind -> S.t)

  let _ = sexp_of_local_type_kind
end

type t = {
  name : string;
  toplevel_id : Type_path.toplevel_id;
  kind : local_type_kind;
  loc_ : location;
  is_only_tag_enum : bool; [@sexp_drop_if fun x -> x = false]
  ty_params_ : Tvar_env.t;
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (let (drop_if__017_ : bool -> Stdlib.Bool.t) = fun x -> x = false in
     fun {
           name = name__008_;
           toplevel_id = toplevel_id__010_;
           kind = kind__012_;
           loc_ = loc___014_;
           is_only_tag_enum = is_only_tag_enum__018_;
           ty_params_ = ty_params___021_;
         } ->
       let bnds__007_ = ([] : _ Stdlib.List.t) in
       let bnds__007_ =
         let arg__022_ = Tvar_env.sexp_of_t ty_params___021_ in
         (S.List [ S.Atom "ty_params_"; arg__022_ ] :: bnds__007_
           : _ Stdlib.List.t)
       in
       let bnds__007_ =
         if drop_if__017_ is_only_tag_enum__018_ then bnds__007_
         else
           let arg__020_ = Moon_sexp_conv.sexp_of_bool is_only_tag_enum__018_ in
           let bnd__019_ = S.List [ S.Atom "is_only_tag_enum"; arg__020_ ] in
           (bnd__019_ :: bnds__007_ : _ Stdlib.List.t)
       in
       let bnds__007_ =
         let arg__015_ = sexp_of_location loc___014_ in
         (S.List [ S.Atom "loc_"; arg__015_ ] :: bnds__007_ : _ Stdlib.List.t)
       in
       let bnds__007_ =
         let arg__013_ = sexp_of_local_type_kind kind__012_ in
         (S.List [ S.Atom "kind"; arg__013_ ] :: bnds__007_ : _ Stdlib.List.t)
       in
       let bnds__007_ =
         let arg__011_ = Type_path.sexp_of_toplevel_id toplevel_id__010_ in
         (S.List [ S.Atom "toplevel_id"; arg__011_ ] :: bnds__007_
           : _ Stdlib.List.t)
       in
       let bnds__007_ =
         let arg__009_ = Moon_sexp_conv.sexp_of_string name__008_ in
         (S.List [ S.Atom "name"; arg__009_ ] :: bnds__007_ : _ Stdlib.List.t)
       in
       S.List bnds__007_
      : t -> S.t)

  let _ = sexp_of_t
end

let mangle_name (toplevel_id : Type_path.toplevel_id) (name : string) =
  let toplevel_id = Type_path.toplevel_id_to_string toplevel_id in
  (toplevel_id ^ "." ^ name : Stdlib.String.t)

let to_generic_typedecl_info base t =
  (let make_type_generic ty =
     (let rec go (ty : Stype.t) =
        (let ty = Stype.type_repr ty in
         match ty with
         | Tarrow { params_ty; ret_ty; err_ty; is_async } ->
             let params_ty, generic1 = gos params_ty in
             let ret_ty, generic2 = go ret_ty in
             let err_ty, generic3 =
               match err_ty with
               | Some err_ty ->
                   let t, g = go err_ty in
                   (Some t, g)
               | None -> (None, false)
             in
             let generic_ = generic1 || generic2 || generic3 in
             (Tarrow { params_ty; ret_ty; err_ty; is_async; generic_ }, generic_)
         | T_constr { type_constructor; tys; is_suberror_ } ->
             let tys, generic_ = gos tys in
             ( T_constr { type_constructor; tys; generic_; is_suberror_ },
               generic_ )
         | Tparam _ -> (ty, true)
         | _ -> (ty, false)
          : Stype.t * bool)
      and gos (tys : Stype.t list) =
        (Lst.fold_right tys ([], false) (fun ty ->
             fun (tys, generic1) ->
              let ty, generic2 = go ty in
              (ty :: tys, generic1 || generic2))
          : Stype.t list * bool)
      in
      fst (go ty)
       : Stype.t)
   in
   let make_field_generic (f : field) =
     {
       f with
       ty_params_ = t.ty_params_;
       ty_field = make_type_generic f.ty_field;
       ty_record = make_type_generic f.ty_record;
     }
   in
   let make_constr_generic (c : constructor) =
     {
       c with
       cs_ty_params_ = t.ty_params_;
       cs_res = make_type_generic c.cs_res;
       cs_args = Lst.map c.cs_args make_type_generic;
     }
   in
   let make_newtype_generic (n : newtype_info) =
     {
       n with
       underlying_typ = make_type_generic n.underlying_typ;
       newtype_constr = make_constr_generic n.newtype_constr;
     }
   in
   let ty_desc : Typedecl_info.type_components =
     if Tvar_env.is_empty t.ty_params_ then
       match t.kind with
       | Enum cs -> Variant_type cs
       | Struct fs -> Record_type { fields = fs; has_private_field_ = false }
       | Newtype n -> New_type n
       | Placeholder -> assert false
     else
       match t.kind with
       | Enum cs ->
           let cs = Lst.map cs make_constr_generic in
           Variant_type cs
       | Struct fs ->
           let fs = Lst.map fs make_field_generic in
           Record_type { fields = fs; has_private_field_ = false }
       | Newtype n -> New_type (make_newtype_generic n)
       | Placeholder -> assert false
   in
   {
     ty_constr =
       Type_path.toplevel_type
         ~pkg:!Basic_config.current_package
         (mangle_name t.toplevel_id t.name);
     ty_arity = 0;
     ty_desc;
     ty_vis = Vis_default;
     ty_params_ = t.ty_params_;
     ty_loc_ = Rloc.to_loc ~base t.loc_;
     ty_doc_ = Docstring.empty;
     ty_attrs = [];
     ty_is_only_tag_enum_ = t.is_only_tag_enum;
     ty_is_suberror_ = false;
   }
    : Typedecl_info.t)

type env = {
  types : t Hash_string.t;
  fields : field Hash_string.t;
  constructors : constructor Hash_string.t;
  base : Loc.t;
}

include struct
  let _ = fun (_ : env) -> ()

  let sexp_of_env =
    (fun {
           types = types__024_;
           fields = fields__026_;
           constructors = constructors__028_;
           base = base__030_;
         }
     ->
       let bnds__023_ = ([] : _ Stdlib.List.t) in
       let bnds__023_ =
         let arg__031_ = Loc.sexp_of_t base__030_ in
         (S.List [ S.Atom "base"; arg__031_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       let bnds__023_ =
         let arg__029_ =
           Hash_string.sexp_of_t sexp_of_constructor constructors__028_
         in
         (S.List [ S.Atom "constructors"; arg__029_ ] :: bnds__023_
           : _ Stdlib.List.t)
       in
       let bnds__023_ =
         let arg__027_ = Hash_string.sexp_of_t sexp_of_field fields__026_ in
         (S.List [ S.Atom "fields"; arg__027_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       let bnds__023_ =
         let arg__025_ = Hash_string.sexp_of_t sexp_of_t types__024_ in
         (S.List [ S.Atom "types"; arg__025_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       S.List bnds__023_
      : env -> S.t)

  let _ = sexp_of_env
end

let empty_env base =
  ({
     types = Hash_string.create 17;
     fields = Hash_string.create 17;
     constructors = Hash_string.create 17;
     base;
   }
    : env)

let find_type env name = Hash_string.find_opt env.types name
let add_type env type_ = Hash_string.add env.types type_.name type_
let update_type env type_ = Hash_string.replace env.types type_.name type_

let add_field env (field : field) =
  Hash_string.add env.fields field.field_name field

let add_constr env (constructor : constructor) =
  Hash_string.add env.constructors constructor.constr_name constructor

let find_constr env name = Hash_string.find_all env.constructors name
let find_field env name = Hash_string.find_all env.fields name
let iter_types env f = Hash_string.iter2 env.types f
let get_base_loc env = env.base

type derive_task = {
  decl : Parsing_syntax.type_decl;
  type_path : Type_path.t;
  directive : Parsing_syntax.deriving_directive;
  trait_path : Type_path.t;
}

type derive_tasks = derive_task list
