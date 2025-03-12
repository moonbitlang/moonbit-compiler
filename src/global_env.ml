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
module Lst = Basic_lst
module Hash_string = Basic_hash_string
module Longident = Basic_longident
module Type_path = Basic_type_path
module Syntax = Parsing_syntax

module All_types = struct
  type t = {
    toplevel : Typing_info.types;
    builtin : Typing_info.types;
    type_alias : Type_alias.t Hash_string.t;
    pkgs : Pkg.pkg_tbl;
  }

  include struct
    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun {
             toplevel = toplevel__002_;
             builtin = builtin__004_;
             type_alias = type_alias__006_;
             pkgs = pkgs__008_;
           }
       ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__009_ = Pkg.sexp_of_pkg_tbl pkgs__008_ in
           (S.List [ S.Atom "pkgs"; arg__009_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__007_ =
             Hash_string.sexp_of_t Type_alias.sexp_of_t type_alias__006_
           in
           (S.List [ S.Atom "type_alias"; arg__007_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__005_ = Typing_info.sexp_of_types builtin__004_ in
           (S.List [ S.Atom "builtin"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Typing_info.sexp_of_types toplevel__002_ in
           (S.List [ S.Atom "toplevel"; arg__003_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         S.List bnds__001_
        : t -> S.t)

    let _ = sexp_of_t
  end

  let make ~toplevel ~builtin ~type_alias ~pkgs =
    { toplevel; builtin; type_alias; pkgs }

  let get_pkg_tbl t = t.pkgs

  let find_type_alias_aux ~pkgs ~type_alias (x : Longident.t) =
    match x with
    | Lident name -> Hash_string.find_opt type_alias name
    | Ldot qual_name -> Pkg.find_type_alias pkgs qual_name

  let find_type_alias types x =
    find_type_alias_aux ~pkgs:types.pkgs ~type_alias:types.type_alias x

  let find_type_by_path_aux ~pkgs ~builtin_types ~toplevel_types
      (p : Type_path.t) =
    (match p with
     | T_unit | T_bool | T_byte | T_int16 | T_uint16 | T_char | T_int | T_int64
     | T_uint | T_uint64 | T_float | T_double | T_string | T_option | T_result
     | T_error_value_result | T_fixedarray | T_bytes | T_ref | T_error ->
         Typing_info.find_type builtin_types (Type_path_util.name p)
     | Toplevel { pkg; id } ->
         if pkg = !Basic_config.current_package then
           Typing_info.find_type toplevel_types id
         else Pkg.find_type_opt pkgs ~pkg id
     | Tuple _ -> None
     | Constr _ -> None
     | T_local _ -> None
      : Typedecl_info.t option)

  let find_type_by_path (types : t) (p : Type_path.t) =
    (find_type_by_path_aux ~pkgs:types.pkgs ~builtin_types:types.builtin
       ~toplevel_types:types.toplevel p
      : Typedecl_info.t option)

  let find_trait (env : t) (x : Longident.t) ~loc =
    (match x with
     | Lident name -> (
         match Typing_info.find_trait env.toplevel name with
         | Some trait -> Ok trait
         | None -> (
             match
               Pkg.find_trait env.pkgs
                 { pkg = Basic_config.builtin_package; id = name }
                 ~loc
             with
             | Ok trait -> Ok trait
             | Error _ -> (
                 match Typing_info.find_trait env.builtin name with
                 | Some trait -> Ok trait
                 | None -> Error (Errors.unbound_trait ~name:x ~loc))))
     | Ldot qual_name -> Pkg.find_trait env.pkgs qual_name ~loc
      : Trait_decl.t Local_diagnostics.info)

  let add_type_alias env name ty = Hash_string.replace env.type_alias name ty
  let add_type (t : t) = Typing_info.add_type t.toplevel
  let add_trait (t : t) = Typing_info.add_trait t.toplevel

  let find_trait_by_path_aux ~pkgs ~toplevel_types ~builtin_types
      (p : Type_path.t) =
    (match p with
     | T_unit | T_bool | T_byte | T_int16 | T_uint16 | T_char | T_int | T_int64
     | T_uint | T_uint64 | T_float | T_double | T_string | T_option | T_result
     | T_error_value_result | T_fixedarray | T_bytes | T_ref | T_error ->
         Typing_info.find_trait builtin_types (Type_path_util.name p)
     | Toplevel { pkg; id } when pkg = !Basic_config.current_package ->
         Typing_info.find_trait toplevel_types id
     | Toplevel { pkg; id } -> (
         match Pkg.find_trait pkgs { pkg; id } ~loc:Rloc.no_location with
         | Ok trait -> Some trait
         | Error _ -> None)
     | Tuple _ | Constr _ | T_local _ -> None
      : Trait_decl.t option)

  let find_trait_by_path (env : t) (p : Type_path.t) =
    find_trait_by_path_aux p ~pkgs:env.pkgs ~builtin_types:env.builtin
      ~toplevel_types:env.toplevel

  let find_type_or_trait (env : t) (name : Longident.t) ~loc =
    match name with
    | Lident id -> (
        match Typing_info.find_type env.toplevel id with
        | Some td -> `Type td
        | None -> (
            match Typing_info.find_trait env.toplevel id with
            | Some trait -> `Trait trait
            | None -> (
                match Pkg.find_type_or_trait_in_builtin env.pkgs id with
                | (`Type _ | `Trait _) as x -> x
                | `Not_found -> (
                    match Typing_info.find_type env.builtin id with
                    | Some td -> `Type td
                    | None -> (
                        match id with
                        | "Unit" -> `Predef Stype.unit
                        | "Bool" -> `Predef Stype.bool
                        | "Byte" -> `Predef Stype.byte
                        | "Int16" -> `Predef Stype.int16
                        | "UInt16" -> `Predef Stype.uint16
                        | "Char" -> `Predef Stype.char
                        | "Int" -> `Predef Stype.int
                        | "Int64" -> `Predef Stype.int64
                        | "UInt" -> `Predef Stype.uint
                        | "UInt64" -> `Predef Stype.uint64
                        | "Float" -> `Predef Stype.float
                        | "Double" -> `Predef Stype.double
                        | "String" -> `Predef Stype.string
                        | "Bytes" -> `Predef Stype.bytes
                        | _ -> `Error (Errors.unbound_type_or_trait ~name ~loc))
                    ))))
    | Ldot { pkg; id } ->
        (Pkg.find_type_or_trait env.pkgs ~pkg id ~loc
          :> [ `Type of _ | `Trait of _ | `Predef of _ | `Error of _ ])

  let find_foreign_value (env : t) ~pkg ~name =
    Pkg.find_regular_value env.pkgs ~pkg name
end

module Qual_ident_hash = Basic_hashf.Make (Basic_qual_ident)

type t = {
  builtin_types : Typing_info.types;
  builtin_values : Typing_info.values;
  toplevel_types : Typing_info.types;
  toplevel_values : Typing_info.values;
  type_alias : Type_alias.t Hash_string.t;
  pkg_tbl : Pkg.pkg_tbl;
  method_env : Method_env.t;
  ext_method_env : Ext_method_env.t;
  trait_impls : Trait_impl.t;
  mutable cur_local_type_env : Local_type.env option;
  all_local_type_env : Local_type.env Qual_ident_hash.t;
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun {
           builtin_types = builtin_types__011_;
           builtin_values = builtin_values__013_;
           toplevel_types = toplevel_types__015_;
           toplevel_values = toplevel_values__017_;
           type_alias = type_alias__019_;
           pkg_tbl = pkg_tbl__021_;
           method_env = method_env__023_;
           ext_method_env = ext_method_env__025_;
           trait_impls = trait_impls__027_;
           cur_local_type_env = cur_local_type_env__029_;
           all_local_type_env = all_local_type_env__031_;
         }
     ->
       let bnds__010_ = ([] : _ Stdlib.List.t) in
       let bnds__010_ =
         let arg__032_ =
           Qual_ident_hash.sexp_of_t Local_type.sexp_of_env
             all_local_type_env__031_
         in
         (S.List [ S.Atom "all_local_type_env"; arg__032_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__030_ =
           Moon_sexp_conv.sexp_of_option Local_type.sexp_of_env
             cur_local_type_env__029_
         in
         (S.List [ S.Atom "cur_local_type_env"; arg__030_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__028_ = Trait_impl.sexp_of_t trait_impls__027_ in
         (S.List [ S.Atom "trait_impls"; arg__028_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__026_ = Ext_method_env.sexp_of_t ext_method_env__025_ in
         (S.List [ S.Atom "ext_method_env"; arg__026_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__024_ = Method_env.sexp_of_t method_env__023_ in
         (S.List [ S.Atom "method_env"; arg__024_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__022_ = Pkg.sexp_of_pkg_tbl pkg_tbl__021_ in
         (S.List [ S.Atom "pkg_tbl"; arg__022_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__020_ =
           Hash_string.sexp_of_t Type_alias.sexp_of_t type_alias__019_
         in
         (S.List [ S.Atom "type_alias"; arg__020_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__018_ = Typing_info.sexp_of_values toplevel_values__017_ in
         (S.List [ S.Atom "toplevel_values"; arg__018_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__016_ = Typing_info.sexp_of_types toplevel_types__015_ in
         (S.List [ S.Atom "toplevel_types"; arg__016_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__014_ = Typing_info.sexp_of_values builtin_values__013_ in
         (S.List [ S.Atom "builtin_values"; arg__014_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__012_ = Typing_info.sexp_of_types builtin_types__011_ in
         (S.List [ S.Atom "builtin_types"; arg__012_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       S.List bnds__010_
      : t -> S.t)

  let _ = sexp_of_t
end

let make ~(types : All_types.t) ~builtin ~toplevel ~method_env ~ext_method_env
    ~trait_impls =
  ({
     builtin_types = types.builtin;
     builtin_values = builtin;
     toplevel_types = types.toplevel;
     toplevel_values = toplevel;
     type_alias = types.type_alias;
     pkg_tbl = types.pkgs;
     method_env;
     ext_method_env;
     trait_impls;
     cur_local_type_env = None;
     all_local_type_env = Qual_ident_hash.create 17;
   }
    : t)

let get_builtin_types (env : t) = env.builtin_types
let get_builtin_values (env : t) = env.builtin_values
let get_toplevel_types (env : t) = env.toplevel_types
let get_toplevel_values (env : t) = env.toplevel_values
let get_type_alias (env : t) = env.type_alias
let get_pkg_tbl (env : t) = env.pkg_tbl
let get_method_env (env : t) = env.method_env
let get_ext_method_env (env : t) = env.ext_method_env
let get_trait_impls (env : t) = env.trait_impls

let get_all_types (env : t) =
  ({
     builtin = env.builtin_types;
     toplevel = env.toplevel_types;
     type_alias = env.type_alias;
     pkgs = env.pkg_tbl;
   }
    : All_types.t)

let unknown_value id =
  (Toplevel_value
     {
       id;
       typ = Stype.new_type_var Tvar_error;
       pub = false;
       kind = Normal;
       loc_ = Loc.no_location;
       doc_ = Docstring.empty;
       attrs = [];
       ty_params_ = Tvar_env.empty;
       arity_ = None;
       param_names_ = [];
       direct_use_loc_ = Not_direct_use;
     }
    : Value_info.t)

let find_type_alias (env : t) (x : Longident.t) =
  (All_types.find_type_alias_aux ~pkgs:env.pkg_tbl ~type_alias:env.type_alias x
    : Type_alias.t option)

let find_type_by_path (env : t) (p : Type_path.t) =
  (All_types.find_type_by_path_aux p ~pkgs:env.pkg_tbl
     ~builtin_types:env.builtin_types ~toplevel_types:env.toplevel_types
    : Typedecl_info.t option)

let type_not_found_error (env : t) (p : Type_path.t) ~creating_value ~loc =
  (let pkg = Type_path.get_pkg p in
   if
     pkg <> !Basic_config.current_package
     &&
     match Pkg.find_pkg_opt (get_pkg_tbl env) ~pkg with
     | None -> true
     | _ -> false
   then
     let action = if creating_value then "create value" else "destruct value" in
     Errors.pkg_not_imported ~name:pkg
       ~action:(action ^ " of type " ^ Type_path_util.name p : Stdlib.String.t)
       ~loc
   else Errors.type_not_found ~tycon:(Type_path_util.name p) ~loc
    : Local_diagnostics.error)

let get_newtype_info (env : t) (ty : Stype.t) =
  (let find_local_newtype_info toplevel_id name =
     let find_in_local_env env name =
       match Local_type.find_type env name with
       | Some { kind = Newtype info; _ } -> Some info
       | _ -> None
         [@@inline]
     in
     match env.cur_local_type_env with
     | Some local_type_env -> find_in_local_env local_type_env name
     | None -> (
         match
           Qual_ident_hash.find_opt env.all_local_type_env
             (Qual_ident.from_toplevel_id toplevel_id)
         with
         | Some local_type_env -> find_in_local_env local_type_env name
         | _ -> None)
       [@@inline]
   in
   let ty = Stype.type_repr ty in
   match ty with
   | T_constr { type_constructor = p; _ } -> (
       match p with
       | T_local { toplevel_id; name } ->
           find_local_newtype_info toplevel_id name
       | _ -> (
           match find_type_by_path env p with
           | Some { ty_desc = New_type info; _ } -> Some info
           | _ -> None))
   | _ -> None
    : Typedecl_info.newtype_info option)

let is_newtype (env : t) (ty : Stype.t) =
  (match get_newtype_info env ty with Some _ -> true | _ -> false : bool)

let find_trait_by_path (env : t) (p : Type_path.t) =
  (All_types.find_trait_by_path_aux ~pkgs:env.pkg_tbl
     ~toplevel_types:env.toplevel_types ~builtin_types:env.builtin_types p
    : Trait_decl.t option)

let find_dot_method (env : t) ~(type_name : Type_path.t) ~(method_name : string)
    =
  let find_method_in_pkg pkg =
    if pkg = !Basic_config.current_package then
      Method_env.find_method_opt env.method_env ~type_name ~method_name
    else Pkg.find_method_opt env.pkg_tbl ~pkg ~type_name ~method_name
      [@@inline]
  in
  let pkg = Type_path.get_pkg type_name in
  let find_local_extension () =
    if pkg <> !Basic_config.current_package then
      match
        Method_env.find_method_opt env.method_env ~type_name ~method_name
      with
      | Some (Only_impl mis) -> mis
      | None | Some (Has_regular _) -> []
    else []
      [@@local]
  in
  match find_method_in_pkg pkg with
  | Some (Has_regular { regular; impls = _ }) -> [ regular ]
  | Some (Only_impl mis) ->
      if
        pkg <> Basic_config.builtin_package
        && Type_path.can_be_extended_from_builtin type_name
      then
        match find_method_in_pkg Basic_config.builtin_package with
        | Some (Has_regular { regular; impls = _ }) -> [ regular ]
        | Some (Only_impl mis') -> mis @ mis'
        | None -> mis
      else mis
  | None ->
      if
        pkg <> Basic_config.builtin_package
        && Type_path.can_be_extended_from_builtin type_name
      then
        match find_method_in_pkg Basic_config.builtin_package with
        | Some (Has_regular { regular; impls = _ }) -> [ regular ]
        | Some (Only_impl mis) -> mis
        | None -> find_local_extension ()
      else find_local_extension ()

let find_regular_method env ~type_name ~method_name =
  let find_method_in_pkg pkg =
    if pkg = !Basic_config.current_package then
      Method_env.find_regular_method env.method_env ~type_name ~method_name
    else Pkg.find_regular_method env.pkg_tbl ~pkg ~type_name ~method_name
      [@@inline]
  in
  let pkg = Type_path.get_pkg type_name in
  match find_method_in_pkg pkg with
  | Some _ as result -> result
  | None when Type_path.can_be_extended_from_builtin type_name ->
      find_method_in_pkg Basic_config.builtin_package
  | None -> None

let find_trait_method (env : t) ~(trait : Type_path.t)
    ~(type_name : Type_path.t) ~(method_name : string) =
  let exception Found of Method_env.method_info in
  let find_ext_method pkg ~trait ~type_name ~method_name =
    let result =
      if pkg = !Basic_config.current_package then
        Ext_method_env.find_method env.ext_method_env ~trait
          ~self_type:type_name ~method_name
      else
        Pkg.find_ext_method_opt env.pkg_tbl ~pkg ~trait ~self_type:type_name
          ~method_name
    in
    match result with None -> () | Some mi -> raise_notrace (Found mi)
      [@@inline]
  in
  try
    let pkg_of_trait = Type_path.get_pkg trait in
    find_ext_method pkg_of_trait ~trait ~type_name ~method_name;
    let pkg_of_type = Type_path.get_pkg type_name in
    if pkg_of_trait <> pkg_of_type then
      find_ext_method pkg_of_type ~trait ~type_name ~method_name;
    find_ext_method pkg_of_trait ~trait
      ~type_name:Type_path.Builtin.default_impl_placeholder ~method_name;
    find_regular_method env ~type_name ~method_name
  with Found mi -> Some mi

let find_trait_impl (env : t) ~(trait : Type_path.t) ~(type_name : Type_path.t)
    =
  let find_in_pkg pkg =
    if pkg = !Basic_config.current_package then
      Trait_impl.find_impl env.trait_impls ~trait ~type_name
    else Pkg.find_trait_impl env.pkg_tbl ~pkg ~trait ~type_name
      [@@inline]
  in
  let find_in_builtin () =
    if !Basic_config.current_package = Basic_config.builtin_package then None
    else
      Pkg.find_trait_impl env.pkg_tbl ~pkg:Basic_config.builtin_package ~trait
        ~type_name
      [@@inline]
  in
  let pkg_of_type = Type_path.get_pkg type_name in
  match find_in_pkg pkg_of_type with
  | Some _ as result -> result
  | None -> (
      let pkg_of_trait = Type_path.get_pkg trait in
      if pkg_of_type = pkg_of_trait then find_in_builtin ()
      else
        match find_in_pkg pkg_of_trait with
        | Some _ as result -> result
        | None -> find_in_builtin ())

let find_value (env : t) (x : Longident.t) ~(loc : Rloc.t) ~diagnostics =
  (match x with
   | Lident id -> (
       match Typing_info.find_value env.toplevel_values id with
       | Some v -> Toplevel_value v
       | None -> (
           match Pkg.find_value_in_builtin_package env.pkg_tbl id with
           | Some vd -> vd
           | None ->
               let id =
                 Qual_ident.make ~pkg:!Basic_config.current_package ~name:id
               in
               Local_diagnostics.add_error diagnostics
                 (Errors.unbound_value ~name:x ~loc);
               unknown_value id))
   | Ldot qual_name -> (
       match Pkg.find_value env.pkg_tbl qual_name ~loc ~diagnostics with
       | Some vd -> vd
       | None ->
           let id = Qual_ident.make ~pkg:qual_name.pkg ~name:qual_name.id in
           unknown_value id)
    : Value_info.t)

let find_value_by_qual_name (env : t) (q : Qual_ident.t) =
  (match q with
   | Qregular { pkg; name } | Qregular_implicit_pkg { pkg; name } ->
       if pkg = !Basic_config.current_package then
         match Typing_info.find_value env.toplevel_values name with
         | Some v -> Some (Toplevel_value v)
         | None -> None
       else Pkg.find_regular_value env.pkg_tbl ~pkg name
   | Qmethod { self_typ; name } -> (
       let pkg = Type_path.get_pkg self_typ in
       let result =
         if
           pkg = !Basic_config.current_package
           || !Basic_config.current_package = Basic_config.builtin_package
         then
           match
             Method_env.find_regular_method env.method_env ~type_name:self_typ
               ~method_name:name
           with
           | Some mi -> Some (Method_env.to_value_info mi)
           | None -> None
         else
           Option.map Method_env.to_value_info
             (Pkg.find_regular_method env.pkg_tbl ~pkg ~type_name:self_typ
                ~method_name:name)
       in
       match result with
       | Some _ -> result
       | None -> (
           match
             Pkg.find_regular_method env.pkg_tbl
               ~pkg:Basic_config.builtin_package ~type_name:self_typ
               ~method_name:name
           with
           | Some mi -> Some (Method_env.to_value_info mi)
           | None -> None))
   | Qext_method { trait; self_typ; name } -> (
       let pkg_of_trait = Type_path.get_pkg trait in
       let pkg_of_type = Type_path.get_pkg self_typ in
       let find_ext_method pkg =
         if pkg = !Basic_config.current_package then
           Ext_method_env.find_method env.ext_method_env ~trait
             ~self_type:self_typ ~method_name:name
         else
           Pkg.find_ext_method_opt env.pkg_tbl ~pkg ~trait ~self_type:self_typ
             ~method_name:name
           [@@inline]
       in
       match find_ext_method pkg_of_trait with
       | Some mi -> Some (Method_env.to_value_info mi)
       | None ->
           if String.equal pkg_of_trait pkg_of_type then None
           else
             Option.map Method_env.to_value_info (find_ext_method pkg_of_type))
    : Value_info.t option)

let find_constructor_or_constant (env : t) (x : string) ~loc =
  (let ambiguous_error (c1 : Typedecl_info.constructor)
       (c2 : Typedecl_info.constructor) =
     let first_ty = Type_path_util.name (Stype.extract_tpath_exn c1.cs_res) in
     let second_ty = Type_path_util.name (Stype.extract_tpath_exn c2.cs_res) in
     Error (Errors.ambiguous_constructor ~name:x ~first_ty ~second_ty ~loc)
       [@@local]
   in
   let find_toplevel_constructor_or_constant () =
     match Typing_info.find_constructor env.toplevel_values x with
     | constr :: [] -> Ok (`Constr constr)
     | c1 :: c2 :: _ -> ambiguous_error c1 c2
     | [] -> (
         match Typing_info.find_value env.toplevel_values x with
         | Some c -> Ok (`Constant c)
         | None -> (
             match
               Pkg.find_constructor_or_constant env.pkg_tbl
                 ~pkg:Basic_config.builtin_package x ~loc
             with
             | Ok _ as result -> result
             | Error _ -> (
                 match Typing_info.find_constructor env.builtin_values x with
                 | constr :: [] -> Ok (`Constr constr)
                 | c1 :: c2 :: _ -> ambiguous_error c1 c2
                 | [] -> Error (Errors.constr_not_found ~ty:None ~constr:x ~loc)
                 )))
       [@@local]
   in
   match env.cur_local_type_env with
   | Some local_type_env -> (
       match Local_type.find_constr local_type_env x with
       | constr :: [] -> Ok (`Constr constr)
       | c1 :: c2 :: _ -> ambiguous_error c1 c2
       | [] -> find_toplevel_constructor_or_constant ())
   | None -> find_toplevel_constructor_or_constant ()
    : [ `Constr of Typedecl_info.constructor | `Constant of Value_info.toplevel ]
      Local_diagnostics.info)

let try_pick_field (env : t) (x : string) =
  (match Typing_info.find_field env.toplevel_values x with
   | Some _ as result -> result
   | None -> Typing_info.find_field env.builtin_values x
    : Typedecl_info.field option)

let labels_of_record (env : t) (ty : Stype.t) ~loc
    ~(context : [ `Create | `Update | `Pattern ]) =
  (let ty = Stype.type_repr ty in
   let not_a_record kind =
     Error
       (Errors.not_a_record ~may_be_method:None
          ~ty:(Printer.type_to_string ty)
          ~kind ~loc)
   in
   match ty with
   | T_constr { type_constructor = Tuple _; _ } -> not_a_record "tuple"
   | T_constr { type_constructor = T_local { name; _ }; _ } -> (
       match[@warning "-fragile-match"] env.cur_local_type_env with
       | Some local_type_env -> (
           match Local_type.find_type local_type_env name with
           | Some { kind = Struct fs; _ } -> Ok fs
           | Some { kind = Enum _; _ } -> not_a_record "enum"
           | Some { kind = Newtype _; _ } -> not_a_record "newtype"
           | Some { kind = Placeholder; _ } | None -> assert false)
       | _ -> assert false)
   | T_constr { type_constructor = p; _ } -> (
       match find_type_by_path env p with
       | Some type_info -> (
           match type_info.ty_desc with
           | Record_type { fields = fs; has_private_field_ } ->
               if Stype.is_external ty then
                 match (type_info.ty_vis, context) with
                 | Vis_readonly, (`Create | `Update) ->
                     let name = Type_path_util.name p in
                     Error (Errors.readonly_type ~name ~loc)
                 | Vis_fully_pub, `Create when has_private_field_ ->
                     let name = Type_path_util.name p in
                     Error
                       (Errors.cannot_create_struct_with_priv_field ~name ~loc)
                 | (Vis_fully_pub | Vis_readonly), _ -> Ok fs
                 | (Vis_default | Vis_priv), _ -> assert false
               else Ok fs
           | Error_type _ | ErrorEnum_type _ -> not_a_record "error type"
           | New_type _ -> not_a_record "newtype"
           | Variant_type _ -> not_a_record "variant"
           | Extern_type | Abstract_type -> not_a_record "abstract")
       | None ->
           let creating_value =
             match context with `Create | `Update -> true | `Pattern -> false
           in
           Error (type_not_found_error env p ~creating_value ~loc))
   | Tvar { contents = Tnolink Tvar_error } | T_blackhole ->
       Error Errors.swallow_error
   | Tvar _ -> not_a_record "unknown"
   | Tarrow _ -> not_a_record "function"
   | Tparam _ -> not_a_record "type parameter"
   | T_trait _ -> not_a_record "trait"
   | T_builtin T_unit -> not_a_record "Unit"
   | T_builtin T_bool -> not_a_record "Bool"
   | T_builtin T_byte -> not_a_record "Byte"
   | T_builtin T_int16 -> not_a_record "Int16"
   | T_builtin T_uint16 -> not_a_record "Uint16"
   | T_builtin T_char -> not_a_record "Char"
   | T_builtin T_int -> not_a_record "Int"
   | T_builtin T_int64 -> not_a_record "Int64"
   | T_builtin T_uint -> not_a_record "Uint"
   | T_builtin T_uint64 -> not_a_record "Uint64"
   | T_builtin T_float -> not_a_record "Float"
   | T_builtin T_double -> not_a_record "Double"
   | T_builtin T_string -> not_a_record "String"
   | T_builtin T_bytes -> not_a_record "Bytes"
    : Typedecl_info.fields Local_diagnostics.info)

let constrs_of_typedecl_info (td : Typedecl_info.t) ~loc ~error_ty =
  let not_a_variant kind =
    Error (Errors.not_a_variant ~ty:(error_ty ()) ~kind ~loc)
  in
  match td.ty_desc with
  | Variant_type cs -> Ok cs
  | Error_type c -> Ok [ c ]
  | ErrorEnum_type cs -> Ok cs
  | New_type { newtype_constr = c; _ } -> Ok [ c ]
  | Record_type _ -> not_a_variant "record"
  | Extern_type | Abstract_type -> not_a_variant "abstract"

let constrs_of_variant (env : t) (ty : Stype.t) ~loc ~creating_value =
  (let ty = Stype.type_repr ty in
   let not_a_variant kind =
     Error (Errors.not_a_variant ~ty:(Printer.type_to_string ty) ~kind ~loc)
   in
   match ty with
   | T_constr { type_constructor = Tuple _; _ } -> not_a_variant "tuple"
   | T_constr { type_constructor = T_local { name; toplevel_id }; _ } -> (
       let local_type_env =
         match env.cur_local_type_env with
         | Some local_type_env -> local_type_env
         | None ->
             Qual_ident_hash.find_exn env.all_local_type_env
               (Qual_ident.from_toplevel_id toplevel_id)
       in
       match Local_type.find_type local_type_env name with
       | Some { kind = Enum cs; _ } -> Ok cs
       | Some { kind = Struct _; _ } -> not_a_variant "struct"
       | Some { kind = Newtype { newtype_constr; _ }; _ } ->
           Ok [ newtype_constr ]
       | Some { kind = Placeholder; _ } | None -> assert false)
   | T_constr { type_constructor = p; _ } -> (
       match find_type_by_path env p with
       | Some type_info ->
           constrs_of_typedecl_info type_info ~loc ~error_ty:(fun () ->
               Printer.type_to_string ty)
       | None -> Error (type_not_found_error env p ~creating_value ~loc))
   | Tvar { contents = Tnolink Tvar_error } | T_blackhole ->
       Error Errors.swallow_error
   | Tvar _ -> not_a_variant "unknown"
   | Tarrow _ -> not_a_variant "function"
   | Tparam _ -> not_a_variant "type parameter"
   | T_trait _ -> not_a_variant "trait"
   | T_builtin T_unit -> not_a_variant "unit"
   | T_builtin T_bool -> not_a_variant "bool"
   | T_builtin T_byte -> not_a_variant "byte"
   | T_builtin T_int16 -> not_a_variant "int16"
   | T_builtin T_uint16 -> not_a_variant "uint16"
   | T_builtin T_char -> not_a_variant "char"
   | T_builtin T_int -> not_a_variant "int"
   | T_builtin T_int64 -> not_a_variant "int64"
   | T_builtin T_uint -> not_a_variant "uint"
   | T_builtin T_uint64 -> not_a_variant "uint64"
   | T_builtin T_float -> not_a_variant "Float"
   | T_builtin T_double -> not_a_variant "double"
   | T_builtin T_string -> not_a_variant "string"
   | T_builtin T_bytes -> not_a_variant "bytes"
    : Typedecl_info.constructors Local_diagnostics.info)

let resolve_record (env : t) ~(labels : Syntax.label list) ~loc =
  (let first_label = List.hd labels in
   let first_label_name = first_label.label_name in
   let all_possibles =
     List.append
       (Typing_info.find_all_fields env.toplevel_values first_label_name)
       (Typing_info.find_all_fields env.builtin_values first_label_name)
   in
   let all_possibles =
     match env.cur_local_type_env with
     | Some local_type_env ->
         List.append
           (Local_type.find_field local_type_env first_label_name)
           all_possibles
     | None -> all_possibles
   in
   match all_possibles with
   | [] ->
       Error (Errors.unbound_field ~name:first_label_name ~loc:first_label.loc_)
   | _ -> (
       let check_field_desc (field_desc : Typedecl_info.field) =
         let all_labels = field_desc.all_labels in
         Lst.same_length all_labels labels
         && Lst.for_all labels (fun label ->
                Lst.exists all_labels (fun label' -> label' = label.label_name))
       in
       match Lst.filter all_possibles check_field_desc with
       | [] ->
           Error
             (Errors.cannot_resolve_record
                ~labels:(Lst.map labels (fun l -> l.label_name))
                ~loc)
       | field_desc :: [] -> (
           let ty_record = field_desc.ty_record in
           match ty_record with
           | T_constr
               { type_constructor = T_local { toplevel_id = _; name }; _ } -> (
               match[@warning "-fragile-match"] env.cur_local_type_env with
               | Some local_type_env -> (
                   match Local_type.find_type local_type_env name with
                   | Some { kind = Struct fields; _ } ->
                       Ok (field_desc.ty_params_, ty_record, fields)
                   | _ -> assert false)
               | _ -> assert false)
           | T_constr { type_constructor = p; _ } -> (
               match find_type_by_path env p with
               | Some
                   { ty_desc = Record_type { has_private_field_ = true; _ }; _ }
                 when Type_path_util.is_foreign p ->
                   let name = Type_path_util.name p in
                   Error
                     (Errors.cannot_create_struct_with_priv_field ~name ~loc)
               | Some { ty_desc = Record_type { fields }; _ } ->
                   Ok (field_desc.ty_params_, ty_record, fields)
               | _ -> assert false)
           | _ -> assert false)
       | field_descs ->
           let extract_name (fd : Typedecl_info.field) =
             match fd.ty_record with
             | T_constr { type_constructor; _ } ->
                 Type_path_util.name type_constructor
             | _ -> assert false
           in
           Error
             (Errors.ambiguous_record
                ~names:(Lst.map field_descs extract_name)
                ~loc))
    : (Tvar_env.t * Stype.t * Typedecl_info.fields) Local_diagnostics.info)

let export_mi (env : t) ~action ~pkg_name =
  Mi_format.export_mi ~action ~pkg_name ~types:env.toplevel_types
    ~type_alias:env.type_alias ~values:env.toplevel_values
    ~method_env:env.method_env ~ext_method_env:env.ext_method_env
    ~trait_impls:env.trait_impls

let report_unused_pkg ~diagnostics (global_env : t) =
  (Pkg.report_unused ~diagnostics global_env.pkg_tbl : unit)

let add_new_local_type_env (env : t) (toplevel_id : Qual_ident.t) base =
  let local_type_env = Local_type.empty_env base in
  env.cur_local_type_env <- Some local_type_env;
  Qual_ident_hash.add env.all_local_type_env toplevel_id local_type_env

let get_cur_local_type_env (env : t) = env.cur_local_type_env
let clear_cur_local_type_env (env : t) = env.cur_local_type_env <- None

let add_local_type (env : t) (local_type : Local_type.t) =
  match[@warning "-fragile-match"] env.cur_local_type_env with
  | Some local_type_env -> Local_type.add_type local_type_env local_type
  | _ -> assert false

let find_local_type (env : t) (name : string) =
  (match env.cur_local_type_env with
   | None -> None
   | Some local_type_env -> Local_type.find_type local_type_env name
    : Local_type.t option)

let update_local_type (env : t) (local_type : Local_type.t) =
  match env.cur_local_type_env with
  | None -> ()
  | Some local_type_env -> Local_type.update_type local_type_env local_type

let add_local_field (env : t) (field : Local_type.field) =
  match env.cur_local_type_env with
  | None -> ()
  | Some local_type_env -> Local_type.add_field local_type_env field

let add_local_constr (env : t) (constructor : Local_type.constructor) =
  match env.cur_local_type_env with
  | None -> ()
  | Some local_type_env -> Local_type.add_constr local_type_env constructor

let all_local_types (env : t) =
  let acc = Basic_vec.empty () in
  Qual_ident_hash.iter2 env.all_local_type_env (fun _ ->
      fun local_type_env ->
       let base = Local_type.get_base_loc local_type_env in
       Local_type.iter_types local_type_env (fun name ->
           fun t ->
            Basic_vec.push acc
              ( Local_type.mangle_name t.toplevel_id name,
                Local_type.to_generic_typedecl_info base t )));
  Basic_vec.to_array acc

let find_all_type_by_path (env : t) (p : Type_path.t) =
  (match p with
   | T_local { toplevel_id; name } -> (
       match
         Qual_ident_hash.find_opt env.all_local_type_env
           (Qual_ident.from_toplevel_id toplevel_id)
       with
       | None -> None
       | Some local_type_env ->
           Option.map
             (Local_type.to_generic_typedecl_info
                (Local_type.get_base_loc local_type_env))
             (Local_type.find_type local_type_env name))
   | _ ->
       All_types.find_type_by_path_aux p ~pkgs:env.pkg_tbl
         ~builtin_types:env.builtin_types ~toplevel_types:env.toplevel_types
    : Typedecl_info.t option)

let find_local_type_env (env : t) (id : Qual_ident.t) =
  (Qual_ident_hash.find_opt env.all_local_type_env id : Local_type.env option)

let find_all_local_type (env : t) (id : Qual_ident.t) (name : string) =
  (match find_local_type_env env id with
   | None -> None
   | Some local_type_env -> Local_type.find_type local_type_env name
    : Local_type.t option)

let get_all_local_type (env : t) (id : Qual_ident.t) =
  (match find_local_type_env env id with
   | None -> []
   | Some local_type_env ->
       let names = Basic_vec.empty () in
       Local_type.iter_types local_type_env (fun name ->
           fun _ -> Basic_vec.push names name);
       Basic_vec.to_list names
    : string list)

let add_local_type_after_typing (env : t) (local_type : Local_type.t) =
  let id = Qual_ident.from_toplevel_id local_type.toplevel_id in
  match Qual_ident_hash.find_opt env.all_local_type_env id with
  | None ->
      let local_type_env = Local_type.empty_env Loc.no_location in
      Qual_ident_hash.add env.all_local_type_env id local_type_env;
      Local_type.add_type local_type_env local_type
  | Some local_type_env -> Local_type.add_type local_type_env local_type
