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
module Config = Basic_config
module Strutil = Basic_strutil

type t =
  | Qregular of { pkg : string; name : string }
  | Qregular_implicit_pkg of { pkg : string; name : string }
  | Qmethod of { self_typ : Type_path.t; name : string }
  | Qext_method of {
      trait : Type_path.t;
      self_typ : Type_path.t;
      name : string;
    }

let hash_fold_t (hsv : Ppx_base.state) (t : t) =
  (match t with
   | Qregular { pkg; name } | Qregular_implicit_pkg { pkg; name } ->
       let hsv = Ppx_base.hash_fold_int hsv 0 in
       let hsv = Ppx_base.hash_fold_string hsv pkg in
       Ppx_base.hash_fold_string hsv name
   | Qmethod { self_typ; name } ->
       let hsv = Ppx_base.hash_fold_int hsv 2 in
       let hsv = Type_path.hash_fold_t hsv self_typ in
       Ppx_base.hash_fold_string hsv name
   | Qext_method { trait; self_typ; name } ->
       let hsv = Ppx_base.hash_fold_int hsv 3 in
       let hsv = Type_path.hash_fold_t hsv trait in
       let hsv = Type_path.hash_fold_t hsv self_typ in
       Ppx_base.hash_fold_string hsv name
    : Ppx_base.state)

let hash (t : t) = Ppx_base.get_hash_value (hash_fold_t (Ppx_base.create ()) t)

let compare (t1 : t) (t2 : t) =
  if Basic_prelude.phys_equal t1 t2 then 0
  else
    match (t1, t2) with
    | ( ( Qregular { pkg = pkg1; name = name1 }
        | Qregular_implicit_pkg { pkg = pkg1; name = name1 } ),
        ( Qregular { pkg = pkg2; name = name2 }
        | Qregular_implicit_pkg { pkg = pkg2; name = name2 } ) ) ->
        let n = String.compare pkg1 pkg2 in
        if n <> 0 then n else String.compare name1 name2
    | (Qregular _ | Qregular_implicit_pkg _), _ -> -1
    | _, (Qregular _ | Qregular_implicit_pkg _) -> 1
    | ( Qmethod { self_typ = self_typ1; name = name1 },
        Qmethod { self_typ = self_typ2; name = name2 } ) ->
        let n = Type_path.compare self_typ1 self_typ2 in
        if n <> 0 then n else String.compare name1 name2
    | Qmethod _, _ -> -1
    | _, Qmethod _ -> 1
    | ( Qext_method { trait = trait1; self_typ = self_typ1; name = name1 },
        Qext_method { trait = trait2; self_typ = self_typ2; name = name2 } ) ->
        let n = Type_path.compare trait1 trait2 in
        if n <> 0 then n
        else
          let n = Type_path.compare self_typ1 self_typ2 in
          if n <> 0 then n else String.compare name1 name2

let equal (t1 : t) (t2 : t) =
  if Basic_prelude.phys_equal t1 t2 then true
  else
    match (t1, t2) with
    | ( ( Qregular { pkg = pkg1; name = name1 }
        | Qregular_implicit_pkg { pkg = pkg1; name = name1 } ),
        ( Qregular { pkg = pkg2; name = name2 }
        | Qregular_implicit_pkg { pkg = pkg2; name = name2 } ) ) ->
        String.equal pkg1 pkg2 && String.equal name1 name2
    | (Qregular _ | Qregular_implicit_pkg _), _ -> false
    | _, (Qregular _ | Qregular_implicit_pkg _) -> false
    | ( Qmethod { self_typ = self_typ1; name = name1 },
        Qmethod { self_typ = self_typ2; name = name2 } ) ->
        Type_path.equal self_typ1 self_typ2 && String.equal name1 name2
    | Qmethod _, _ -> false
    | _, Qmethod _ -> false
    | ( Qext_method { trait = trait1; self_typ = self_typ1; name = name1 },
        Qext_method { trait = trait2; self_typ = self_typ2; name = name2 } ) ->
        Type_path.equal trait1 trait2
        && Type_path.equal self_typ1 self_typ2
        && String.equal name1 name2

let create_predef name = Qregular { pkg = "*predef*"; name }

let get_pkg t =
  match t with
  | Qregular { pkg; _ } -> pkg
  | Qregular_implicit_pkg { pkg; _ } -> pkg
  | Qmethod { self_typ; _ } -> Type_path.get_pkg self_typ
  | Qext_method { trait; _ } -> Type_path.get_pkg trait

let is_foreign id = get_pkg id <> !Config.current_package

let string_of_t t =
  match t with
  | Qregular { pkg; name } | Qregular_implicit_pkg { pkg; name } ->
      if pkg = "" then name
      else if pkg = Config.builtin_package then "$builtin." ^ name
      else Stdlib.String.concat "" [ "$"; pkg; "."; name ] [@merlin.hide]
  | Qmethod { self_typ; name } ->
      let typ_str = Type_path.short_name ~cur_pkg_name:None self_typ in
      ((typ_str ^ "::" ^ name : Stdlib.String.t) [@merlin.hide])
  | Qext_method { trait; self_typ; name } ->
      let trait_str = Type_path.short_name ~cur_pkg_name:None trait in
      let typ_str = Type_path.short_name ~cur_pkg_name:None self_typ in
      (Stdlib.String.concat ""
         [ trait_str; "::"; name; "|"; typ_str; "|" ] [@merlin.hide])

let sexp_of_t t = (S.Atom (string_of_t t) : S.t)

let base_name t =
  match t with
  | Qregular { name; _ }
  | Qregular_implicit_pkg { name; _ }
  | Qmethod { name; _ }
  | Qext_method { name; _ } ->
      name

let make ~pkg ~name = Qregular { pkg; name }
let make_implicit_pkg ~pkg ~name = Qregular_implicit_pkg { pkg; name }
let toplevel_value ~name = Qregular { pkg = !Config.current_package; name }
let meth ~self_typ ~name = Qmethod { self_typ; name }
let ext_meth ~trait ~self_typ ~name = Qext_method { trait; self_typ; name }

let map qual_name ~f =
  match qual_name with
  | Qregular { pkg; name } -> Qregular { pkg; name = f name }
  | Qregular_implicit_pkg { pkg; name } ->
      Qregular_implicit_pkg { pkg; name = f name }
  | Qmethod { self_typ; name } -> Qmethod { self_typ; name = f name }
  | Qext_method { trait; self_typ; name } ->
      Qext_method { trait; self_typ; name = f name }

let to_wasm_name t =
  match t with
  | Qregular { pkg; name } | Qregular_implicit_pkg { pkg; name } ->
      let name = Strutil.mangle_wasm_name name in
      if pkg = "" then ("$" ^ name : Stdlib.String.t) [@merlin.hide]
      else
        let pkg = Strutil.mangle_wasm_name pkg in
        (Stdlib.String.concat "" [ "$"; pkg; "."; name ] [@merlin.hide])
  | Qmethod { self_typ; name } ->
      let typ_str = Strutil.mangle_wasm_name (Type_path.export_name self_typ) in
      let name = Strutil.mangle_wasm_name name in
      (Stdlib.String.concat "" [ "$"; typ_str; "::"; name ] [@merlin.hide])
  | Qext_method { trait; self_typ; name } ->
      let trait_str = Strutil.mangle_wasm_name (Type_path.export_name trait) in
      let typ_str = Strutil.mangle_wasm_name (Type_path.export_name self_typ) in
      let name = Strutil.mangle_wasm_name name in
      (Stdlib.String.concat ""
         [ "$"; trait_str; "::"; typ_str; "::"; name ] [@merlin.hide])

let to_toplevel_id t =
  (match t with
   | Qregular { pkg; name } -> T_regular { pkg; name }
   | Qmethod { self_typ; name } -> T_method { self_typ; name }
   | Qext_method { trait; self_typ; name } ->
       T_ext_method { trait; self_typ; name }
   | Qregular_implicit_pkg _ -> assert false
    : Type_path.toplevel_id)

let from_toplevel_id (id : Type_path.toplevel_id) =
  match id with
  | T_regular { pkg; name } -> Qregular { pkg; name }
  | T_method { self_typ; name } -> Qmethod { self_typ; name }
  | T_ext_method { trait; self_typ; name } ->
      Qext_method { trait; self_typ; name }
