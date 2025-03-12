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


module Arr = Basic_arr
module Type_path = Basic_type_path

type t = Stype.t array

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun x__001_ -> Moon_sexp_conv.sexp_of_array Stype.sexp_of_t x__001_
      : t -> S.t)

  let _ = sexp_of_t
end

let rec equal_typ (x : Stype.t) (y : Stype.t) =
  let x = Stype.type_repr x in
  let y = Stype.type_repr y in
  match x with
  | Tarrow { params_ty = ts1; ret_ty = t1; err_ty = e1; is_async = a1 } -> (
      match y with
      | Tarrow { params_ty = ts2; ret_ty = t2; err_ty = e2; is_async = a2 } ->
          equal_typs ts1 ts2 && equal_typ t1 t2 && equal_typ_option e1 e2
          && a1 = a2
      | T_trait _ | T_constr _ | T_builtin _ -> false
      | Tvar _ | Tparam _ | T_blackhole -> assert false)
  | T_constr { type_constructor = c1; tys = ts1 } -> (
      match y with
      | T_constr { type_constructor = c2; tys = ts2 } ->
          Type_path.equal c1 c2 && equal_typs ts1 ts2
      | Tarrow _ | T_trait _ | T_builtin _ -> false
      | Tvar _ | Tparam _ | T_blackhole -> assert false)
  | T_trait t1 -> (
      match y with
      | T_trait t2 -> Type_path.equal t1 t2
      | Tarrow _ | T_constr _ | T_builtin _ -> false
      | Tvar _ | Tparam _ | T_blackhole -> assert false)
  | T_builtin a -> (
      match y with T_builtin b -> Stype.equal_builtin a b | _ -> false)
  | Tparam _ -> assert false
  | Tvar _ -> assert false
  | T_blackhole -> assert false

and equal_typs (xs : Stype.t list) (ys : Stype.t list) =
  match xs with
  | [] -> ys = []
  | x :: rest1 -> (
      match ys with
      | y :: rest2 -> equal_typ x y && equal_typs rest1 rest2
      | _ -> false)

and equal_typ_option (x : Stype.t option) (y : Stype.t option) =
  match x with
  | None -> y = None
  | Some x -> ( match y with Some y -> equal_typ x y | None -> false)

let equal (xs : t) (ys : t) = Arr.for_all2_no_exn xs ys equal_typ
let ( +> ) = Buffer.add_char
let ( +>> ) = Buffer.add_string

let append_typ_to_buffer (buf : Buffer.t) (ty : Stype.t) =
  let rec go (ty : Stype.t) =
    let ty = Stype.type_repr ty in
    match ty with
    | Tarrow { params_ty; ret_ty; err_ty; is_async = _ } -> (
        buf +> '<';
        (match params_ty with
        | [] -> ()
        | ty :: [] -> go ty
        | tys -> gos '*' tys);
        buf +> '>';
        buf +>> "=>";
        go ret_ty;
        match err_ty with
        | None -> ()
        | Some ty ->
            buf +> '!';
            go ty)
    | T_constr { type_constructor = Tuple _; tys } ->
        buf +> '<';
        gos '*' tys;
        buf +> '>'
    | T_constr { type_constructor = c; tys = args } -> (
        buf
        +>> Type_path.export_name ~cur_pkg_name:!Basic_config.current_package c;
        match args with
        | [] -> ()
        | tys ->
            buf +> '<';
            gos '*' tys;
            buf +> '>')
    | T_trait t ->
        buf
        +>> Type_path.export_name ~cur_pkg_name:!Basic_config.current_package t
    | T_builtin T_unit -> buf +>> "Unit"
    | T_builtin T_bool -> buf +>> "Bool"
    | T_builtin T_byte -> buf +>> "Byte"
    | T_builtin T_int16 -> buf +>> "Int16"
    | T_builtin T_uint16 -> buf +>> "UInt16"
    | T_builtin T_char -> buf +>> "Char"
    | T_builtin T_int -> buf +>> "Int"
    | T_builtin T_int64 -> buf +>> "Int64"
    | T_builtin T_uint -> buf +>> "UInt"
    | T_builtin T_uint64 -> buf +>> "UInt64"
    | T_builtin T_float -> buf +>> "Float"
    | T_builtin T_double -> buf +>> "Double"
    | T_builtin T_string -> buf +>> "String"
    | T_builtin T_bytes -> buf +>> "Bytes"
    | Tvar _ -> assert false
    | Tparam _ -> assert false
    | T_blackhole -> assert false
  and gos (sep : char) = function
    | [] -> ()
    | ty :: [] -> go ty
    | ty :: tys ->
        go ty;
        Buffer.add_char buf sep;
        gos sep tys
  in
  go ty

let mangle (tys : t) =
  (let buf = Buffer.create 16 in
   Array.iteri
     (fun i ->
       fun ty ->
        if i > 0 then Buffer.add_string buf "+";
        append_typ_to_buffer buf ty)
     tys;
   Buffer.contents buf
    : string)

let mangle_ty (ty : Stype.t) =
  let buf = Buffer.create 16 in
  append_typ_to_buffer buf ty;
  Buffer.contents buf
