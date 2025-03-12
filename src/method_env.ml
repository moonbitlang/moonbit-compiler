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
module M = Basic_hash_string
module Vec = Basic_vec
module Arr = Basic_arr
module Lst = Basic_lst

module H = Basic_hashf.Make (struct
  type t = Type_path.t

  include struct
    let _ = fun (_ : t) -> ()
    let equal = (Type_path.equal : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      Type_path.hash_fold_t

    and (hash : t -> Ppx_base.hash_value) =
      let func = Type_path.hash in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash

    let sexp_of_t = (Type_path.sexp_of_t : t -> S.t)
    let _ = sexp_of_t
  end
end)

type method_name = string

include struct
  let _ = fun (_ : method_name) -> ()
  let sexp_of_method_name = (Moon_sexp_conv.sexp_of_string : method_name -> S.t)
  let _ = sexp_of_method_name
end

type type_name = Type_path.t

include struct
  let _ = fun (_ : type_name) -> ()
  let sexp_of_type_name = (Type_path.sexp_of_t : type_name -> S.t)
  let _ = sexp_of_type_name
end

type docstring = Docstring.t

include struct
  let _ = fun (_ : docstring) -> ()
  let sexp_of_docstring = (Docstring.sexp_of_t : docstring -> S.t)
  let _ = sexp_of_docstring
end

type method_kind =
  | Regular_method
  | Regular_method_qualified
  | Method_explicit_self of { self_ty : Stype.t }

include struct
  let _ = fun (_ : method_kind) -> ()

  let sexp_of_method_kind =
    (function
     | Regular_method -> S.Atom "Regular_method"
     | Regular_method_qualified -> S.Atom "Regular_method_qualified"
     | Method_explicit_self { self_ty = self_ty__004_ } ->
         let bnds__003_ = ([] : _ Stdlib.List.t) in
         let bnds__003_ =
           let arg__005_ = Stype.sexp_of_t self_ty__004_ in
           (S.List [ S.Atom "self_ty"; arg__005_ ] :: bnds__003_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Method_explicit_self" :: bnds__003_)
      : method_kind -> S.t)

  let _ = sexp_of_method_kind
end

type method_info = {
  id : Basic_qual_ident.t;
  prim : Primitive.prim option;
  typ : Stype.t;
  pub : bool;
  loc : Loc.t;
  doc_ : docstring; [@sexp_drop_if Docstring.is_empty]
  attrs : Checked_attributes.t;
  ty_params_ : Tvar_env.t; [@sexp_drop_if Tvar_env.is_empty]
  kind_ : method_kind;
      [@sexp_drop_if
        function
        | Regular_method | Regular_method_qualified -> true
        | Method_explicit_self _ -> false]
  arity_ : Fn_arity.t; [@sexp_drop_if Fn_arity.is_simple]
  param_names_ : string list;
}

include struct
  let _ = fun (_ : method_info) -> ()

  let sexp_of_method_info =
    (let (drop_if__018_ : docstring -> Stdlib.Bool.t) = Docstring.is_empty
     and (drop_if__025_ : Tvar_env.t -> Stdlib.Bool.t) = Tvar_env.is_empty
     and (drop_if__030_ : method_kind -> Stdlib.Bool.t) = function
       | Regular_method | Regular_method_qualified -> true
       | Method_explicit_self _ -> false
     and (drop_if__035_ : Fn_arity.t -> Stdlib.Bool.t) = Fn_arity.is_simple in
     fun {
           id = id__007_;
           prim = prim__009_;
           typ = typ__011_;
           pub = pub__013_;
           loc = loc__015_;
           doc_ = doc___019_;
           attrs = attrs__022_;
           ty_params_ = ty_params___026_;
           kind_ = kind___031_;
           arity_ = arity___036_;
           param_names_ = param_names___039_;
         } ->
       let bnds__006_ = ([] : _ Stdlib.List.t) in
       let bnds__006_ =
         let arg__040_ =
           Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
             param_names___039_
         in
         (S.List [ S.Atom "param_names_"; arg__040_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         if drop_if__035_ arity___036_ then bnds__006_
         else
           let arg__038_ = Fn_arity.sexp_of_t arity___036_ in
           let bnd__037_ = S.List [ S.Atom "arity_"; arg__038_ ] in
           (bnd__037_ :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         if drop_if__030_ kind___031_ then bnds__006_
         else
           let arg__033_ = sexp_of_method_kind kind___031_ in
           let bnd__032_ = S.List [ S.Atom "kind_"; arg__033_ ] in
           (bnd__032_ :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         if drop_if__025_ ty_params___026_ then bnds__006_
         else
           let arg__028_ = Tvar_env.sexp_of_t ty_params___026_ in
           let bnd__027_ = S.List [ S.Atom "ty_params_"; arg__028_ ] in
           (bnd__027_ :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__023_ = Checked_attributes.sexp_of_t attrs__022_ in
         (S.List [ S.Atom "attrs"; arg__023_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         if drop_if__018_ doc___019_ then bnds__006_
         else
           let arg__021_ = sexp_of_docstring doc___019_ in
           let bnd__020_ = S.List [ S.Atom "doc_"; arg__021_ ] in
           (bnd__020_ :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__016_ = Loc.sexp_of_t loc__015_ in
         (S.List [ S.Atom "loc"; arg__016_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__014_ = Moon_sexp_conv.sexp_of_bool pub__013_ in
         (S.List [ S.Atom "pub"; arg__014_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__012_ = Stype.sexp_of_t typ__011_ in
         (S.List [ S.Atom "typ"; arg__012_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__010_ =
           Moon_sexp_conv.sexp_of_option Primitive.sexp_of_prim prim__009_
         in
         (S.List [ S.Atom "prim"; arg__010_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__008_ = Basic_qual_ident.sexp_of_t id__007_ in
         (S.List [ S.Atom "id"; arg__008_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       S.List bnds__006_
      : method_info -> S.t)

  let _ = sexp_of_method_info
end

type method_table_entry =
  | Has_regular of { regular : method_info; impls : method_info list }
  | Only_impl of method_info list

let sexp_of_method_table_entry = function
  | Has_regular { regular = mi; impls = _ } | Only_impl (mi :: []) ->
      sexp_of_method_info mi
  | Only_impl mis -> List (Atom "Ambiguous" :: Lst.map mis sexp_of_method_info)

type t = method_table_entry M.t H.t

let sexp_of_t t = H.sexp_of_t (M.sexp_of_t sexp_of_method_table_entry) t
let empty () = (H.create 17 : t)

let find_regular_method (env : t) ~(type_name : type_name)
    ~(method_name : method_name) =
  (match H.find_opt env type_name with
   | Some method_table -> (
       match M.find_opt method_table method_name with
       | Some (Has_regular { regular; impls = _ }) -> Some regular
       | Some (Only_impl _) | None -> None)
   | None -> None
    : method_info option)

let find_method_opt (env : t) ~(type_name : type_name)
    ~(method_name : method_name) =
  (match H.find_opt env type_name with
   | Some method_table -> M.find_opt method_table method_name
   | None -> None
    : method_table_entry option)

let iter_methods_by_type (env : t) ~(type_name : type_name)
    (f : method_name -> method_info -> unit) =
  match H.find_opt env type_name with
  | Some method_table ->
      M.iter2 method_table (fun method_name ->
          fun entry ->
           match entry with
           | Has_regular { regular = mi; impls = _ } | Only_impl (mi :: []) ->
               f method_name mi
           | Only_impl _ -> ())
  | None -> ()

let add_method (env : t) ~(type_name : type_name) ~(method_name : method_name)
    ~(method_info : method_info) =
  (match H.find_opt env type_name with
   | Some method_table ->
       ignore
         (M.add_or_update method_table method_name
            (Has_regular { regular = method_info; impls = [] })
            ~update:(fun
                (Has_regular { regular = _; impls } | Only_impl impls) ->
              Has_regular { regular = method_info; impls }))
   | None ->
       let method_table = M.create 17 in
       M.add method_table method_name
         (Has_regular { regular = method_info; impls = [] });
       H.add env type_name method_table
    : unit)

let add_impl (env : t) ~(type_name : type_name) ~(method_name : method_name)
    ~(method_info : method_info) =
  (match H.find_opt env type_name with
   | Some method_table ->
       ignore
         (M.add_or_update method_table method_name (Only_impl [ method_info ])
            ~update:(fun entry ->
              match entry with
              | Has_regular { regular; impls } ->
                  Has_regular { regular; impls = method_info :: impls }
              | Only_impl mis -> Only_impl (method_info :: mis)))
   | None ->
       let method_table = M.create 17 in
       M.add method_table method_name (Only_impl [ method_info ]);
       H.add env type_name method_table
    : unit)

let to_value_info (m : method_info) =
  (Toplevel_value
     {
       id = m.id;
       typ = m.typ;
       pub = m.pub;
       kind = (match m.prim with None -> Normal | Some prim -> Prim prim);
       loc_ = m.loc;
       doc_ = m.doc_;
       attrs = m.attrs;
       ty_params_ = m.ty_params_;
       arity_ = Some m.arity_;
       param_names_ = m.param_names_;
       direct_use_loc_ = Not_direct_use;
     }
    : Value_info.t)

let iter (env : t) f =
  H.iter2 env (fun type_name ->
      fun tbl ->
       M.iter2 tbl (fun method_name ->
           fun entry ->
            match entry with
            | Has_regular { regular = mi; impls = _ } | Only_impl (mi :: []) ->
                f type_name method_name mi
            | Only_impl _ -> ()))

type method_array = (type_name * (method_name * method_table_entry) array) array

include struct
  let _ = fun (_ : method_array) -> ()

  let sexp_of_method_array =
    (fun x__049_ ->
       Moon_sexp_conv.sexp_of_array
         (fun (arg0__045_, arg1__046_) ->
           let res0__047_ = sexp_of_type_name arg0__045_
           and res1__048_ =
             Moon_sexp_conv.sexp_of_array
               (fun (arg0__041_, arg1__042_) ->
                 let res0__043_ = sexp_of_method_name arg0__041_
                 and res1__044_ = sexp_of_method_table_entry arg1__042_ in
                 S.List [ res0__043_; res1__044_ ])
               arg1__046_
           in
           S.List [ res0__047_; res1__048_ ])
         x__049_
      : method_array -> S.t)

  let _ = sexp_of_method_array
end

let export (env : t) ~export_private =
  (let result =
     Vec.make (H.length env) ~dummy:(Type_path.Builtin.type_path_int, [||])
   in
   H.iter2 env (fun self_type ->
       fun methods ->
        if
          export_private
          || (not (Type_path_util.is_foreign self_type))
          || !Basic_config.current_package = Basic_config.builtin_package
        then (
          let methods_vec =
            Vec.make (M.length methods) ~dummy:("", Only_impl [])
          in
          M.iter2 methods (fun method_name ->
              fun entry ->
               match entry with
               | Has_regular { regular; impls } -> (
                   if export_private || regular.pub then
                     Vec.push methods_vec
                       (method_name, Has_regular { regular; impls = [] })
                   else
                     match
                       Lst.filter impls (fun mi -> export_private || mi.pub)
                     with
                     | [] -> ()
                     | mis -> Vec.push methods_vec (method_name, Only_impl mis))
               | Only_impl mis -> (
                   match
                     Lst.filter mis (fun mi -> export_private || mi.pub)
                   with
                   | [] -> ()
                   | mis -> Vec.push methods_vec (method_name, Only_impl mis)));
          if Vec.length methods_vec > 0 then
            Vec.push result (self_type, Vec.to_array methods_vec)));
   Vec.to_array result
    : method_array)

let import (methods_by_type : method_array) =
  (let env = H.create (Array.length methods_by_type * 3 / 2) in
   Arr.iter methods_by_type (fun (type_name, methods) ->
       let tbl = M.create (Array.length methods * 3 / 2) in
       Arr.iter methods (fun (method_name, entry) ->
           M.add tbl method_name entry);
       H.add env type_name tbl);
   env
    : t)
