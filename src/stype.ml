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


module Ident = Basic_ident
module Type_path = Basic_type_path

let sexp_of_list = Moon_sexp_conv.sexp_of_list

type builtin =
  | T_unit
  | T_bool
  | T_byte
  | T_char
  | T_int
  | T_int16
  | T_int64
  | T_uint
  | T_uint16
  | T_uint64
  | T_float
  | T_double
  | T_string
  | T_bytes

let equal_builtin (a : builtin) (b : builtin) = a = b

type t =
  | Tarrow of {
      params_ty : t list;
      ret_ty : t;
      err_ty : t option;
      is_async : bool;
      generic_ : bool;
    }
  | T_constr of {
      type_constructor : Type_path.t;
      tys : t list;
      generic_ : bool;
      is_suberror_ : bool;
    }
  | Tvar of tlink ref
  | Tparam of { index : int; name_ : string }
  | T_trait of Type_path.t
  | T_builtin of builtin
  | T_blackhole

and tlink = Tnolink of tvar_kind | Tlink of t
and tvar_kind = Tvar_normal | Tvar_error

let unit = T_builtin T_unit
let bool = T_builtin T_bool
let byte = T_builtin T_byte
let char = T_builtin T_char
let int16 = T_builtin T_int16
let int = T_builtin T_int
let int64 = T_builtin T_int64
let uint = T_builtin T_uint
let uint16 = T_builtin T_uint16
let uint64 = T_builtin T_uint64
let float = T_builtin T_float
let double = T_builtin T_double
let string = T_builtin T_string
let bytes = T_builtin T_bytes

let error =
  T_constr
    {
      type_constructor = Type_path.Builtin.type_path_error;
      tys = [];
      generic_ = false;
      is_suberror_ = false;
    }

let json =
  T_constr
    {
      type_constructor = Type_path.Builtin.type_path_json;
      tys = [];
      generic_ = false;
      is_suberror_ = false;
    }

let bigint =
  T_constr
    {
      type_constructor = Type_path.Builtin.type_path_bigint;
      tys = [];
      generic_ = false;
      is_suberror_ = false;
    }

let blackhole = T_blackhole
let tnolink_normal = Tnolink Tvar_normal
let tnolink_error = Tnolink Tvar_error

let new_type_var kind =
  (match kind with
   | Tvar_normal -> Tvar (ref tnolink_normal)
   | Tvar_error -> Tvar (ref tnolink_error)
    : t)

let rec sexp_of_t = function
  | Tarrow { params_ty; ret_ty; err_ty; is_async; generic_ } -> (
      let async = if is_async then [ S.Atom "async" ] else [] in
      match err_ty with
      | None ->
          (List
             (List.append
                (async : S.t list)
                (List.cons
                   (sexp_of_ts params_ty : S.t)
                   (List.cons
                      (Atom "->" : S.t)
                      (List.cons
                         (sexp_of_t ret_ty : S.t)
                         ([
                            List
                              (List.cons
                                 (Atom "generic_" : S.t)
                                 ([ Moon_sexp_conv.sexp_of_bool generic_ ]
                                   : S.t list));
                          ]
                           : S.t list)))))
            : S.t)
      | Some err_ty ->
          let exclamation = S.Atom "!" in
          (List
             (List.append
                (async : S.t list)
                (List.cons
                   (sexp_of_ts params_ty : S.t)
                   (List.cons
                      (Atom "->" : S.t)
                      (List.cons
                         (sexp_of_t ret_ty : S.t)
                         (List.cons
                            (exclamation : S.t)
                            (List.cons
                               (sexp_of_t err_ty : S.t)
                               ([
                                  List
                                    (List.cons
                                       (Atom "generic_" : S.t)
                                       ([ Moon_sexp_conv.sexp_of_bool generic_ ]
                                         : S.t list));
                                ]
                                 : S.t list)))))))
            : S.t))
  | T_constr
      {
        type_constructor =
          (T_option | T_result | T_fixedarray | T_bytes | T_ref) as p;
        tys = [];
        generic_ = _;
      } ->
      S.Atom (Type_path_util.name p)
  | T_constr
      {
        type_constructor =
          (T_option | T_result | T_fixedarray | T_bytes | T_ref) as p;
        tys;
        generic_;
      } ->
      let tys = match tys with [] -> [] | _ -> Basic_lst.map tys sexp_of_t in
      let s = Type_path_util.name p in
      (List
         (List.cons
            (Atom s : S.t)
            (List.append
               (tys : S.t list)
               ([
                  List
                    (List.cons
                       (Atom "generic_" : S.t)
                       ([ Moon_sexp_conv.sexp_of_bool generic_ ] : S.t list));
                ]
                 : S.t list)))
        : S.t)
  | T_constr { type_constructor = Tuple _; tys; generic_ } ->
      (List
         (List.cons
            (Atom "Tproduct" : S.t)
            (List.cons
               (sexp_of_ts tys : S.t)
               ([
                  List
                    (List.cons
                       (Atom "generic_" : S.t)
                       ([ Moon_sexp_conv.sexp_of_bool generic_ ] : S.t list));
                ]
                 : S.t list)))
        : S.t)
  | T_constr
      {
        type_constructor = T_error_value_result;
        tys = [ ok_ty; err_ty ];
        generic_ = _;
      } ->
      (List
         (List.cons
            (Atom "T_error_value_result" : S.t)
            (List.cons
               (sexp_of_t ok_ty : S.t)
               ([ sexp_of_t err_ty ] : S.t list)))
        : S.t)
  | T_constr { type_constructor; is_suberror_ = true; _ } ->
      (List
         (List.cons
            (Atom "T_suberror" : S.t)
            ([ Type_path.sexp_of_t type_constructor ] : S.t list))
        : S.t)
  | T_constr { type_constructor; tys; generic_ } ->
      (List
         (List.cons
            (Atom "T_constr" : S.t)
            (List.cons
               (Type_path.sexp_of_t type_constructor : S.t)
               (List.cons
                  (sexp_of_ts tys : S.t)
                  ([
                     List
                       (List.cons
                          (Atom "generic_" : S.t)
                          ([ Moon_sexp_conv.sexp_of_bool generic_ ] : S.t list));
                   ]
                    : S.t list))))
        : S.t)
  | Tvar { contents = Tnolink _ } ->
      (List (List.cons (Atom "Tvar" : S.t) ([ Atom "Tnolink" ] : S.t list))
        : S.t)
  | Tvar { contents = Tlink link } -> sexp_of_t link
  | Tparam { index = _; name_ } -> S.Atom name_
  | T_trait trait -> List [ Atom "T_trait"; Atom (Type_path_util.name trait) ]
  | T_builtin T_unit -> (Atom "Unit" : S.t)
  | T_builtin T_bool -> (Atom "Bool" : S.t)
  | T_builtin T_byte -> (Atom "Byte" : S.t)
  | T_builtin T_char -> (Atom "Char" : S.t)
  | T_builtin T_int -> (Atom "Int" : S.t)
  | T_builtin T_int16 -> (Atom "Int16" : S.t)
  | T_builtin T_int64 -> (Atom "Int64" : S.t)
  | T_builtin T_uint -> (Atom "UInt" : S.t)
  | T_builtin T_uint16 -> (Atom "UInt16" : S.t)
  | T_builtin T_uint64 -> (Atom "UInt64" : S.t)
  | T_builtin T_float -> (Atom "Float" : S.t)
  | T_builtin T_double -> (Atom "Double" : S.t)
  | T_builtin T_string -> (Atom "String" : S.t)
  | T_builtin T_bytes -> (Atom "Bytes" : S.t)
  | T_blackhole -> (Atom "T_blackhole" : S.t)

and sexp_of_ts ts = sexp_of_list sexp_of_t ts

module B = Type_path.Builtin

let tpath_of_builtin (t : builtin) =
  match t with
  | T_unit -> B.type_path_unit
  | T_bool -> B.type_path_bool
  | T_byte -> B.type_path_byte
  | T_char -> B.type_path_char
  | T_int -> B.type_path_int
  | T_int16 -> B.type_path_int16
  | T_int64 -> B.type_path_int64
  | T_uint -> B.type_path_uint
  | T_uint16 -> B.type_path_uint16
  | T_uint64 -> B.type_path_uint64
  | T_float -> B.type_path_float
  | T_double -> B.type_path_double
  | T_string -> B.type_path_string
  | T_bytes -> B.type_path_bytes

let extract_tpath t =
  match t with
  | T_constr { type_constructor; _ } -> Some type_constructor
  | T_builtin b -> Some (tpath_of_builtin b)
  | T_trait trait -> Some trait
  | _ -> None

let extract_tpath_exn t =
  match extract_tpath t with Some p -> p | None -> failwith __FUNCTION__

let arity_of_typ t =
  match t with
  | Tarrow { params_ty; _ } -> List.length params_ty
  | _ -> failwith "cannot get the arity of a non-function type"

let is_external t =
  let rec tpath_is_external (p : Type_path.t) =
    match p with
    | Toplevel { pkg; _ } -> pkg <> !Basic_config.current_package
    | Constr { ty; tag = _ } -> tpath_is_external ty
    | Tuple _ | T_unit | T_bool | T_byte | T_char | T_int | T_int16 | T_int64
    | T_uint | T_uint16 | T_uint64 | T_float | T_double | T_string | T_option
    | T_result | T_error_value_result | T_fixedarray | T_bytes | T_ref | T_error
      ->
        false
    | T_local _ -> false
  in
  match extract_tpath t with Some p -> tpath_is_external p | None -> false

let f desc =
  (T_constr
     {
       type_constructor = desc;
       tys = [];
       generic_ = false;
       is_suberror_ = false;
     }
    : t)

let type_sourceloc = f B.type_path_sourceloc
let type_argsloc = f B.type_path_argsloc

let type_iter_result =
  T_constr
    {
      type_constructor = B.type_path_iter_result;
      tys = [];
      generic_ = false;
      is_suberror_ = false;
    }

let param0 : t = Tparam { index = 0; name_ = "A" }
let param1 : t = Tparam { index = 1; name_ = "B" }
let param_self : t = Tparam { index = 0; name_ = "Self" }

let make_multi_value_result_ty ~ok_ty ~err_ty =
  T_constr
    {
      type_constructor = Type_path.Builtin.type_path_multi_value_result;
      tys = [ ok_ty; err_ty ];
      generic_ = false;
      is_suberror_ = false;
    }

let make_result_ty ~ok_ty ~err_ty =
  T_constr
    {
      type_constructor = Type_path.Builtin.type_path_result;
      tys = [ ok_ty; err_ty ];
      generic_ = false;
      is_suberror_ = false;
    }

let rec type_repr (ty : t) =
  (match ty with
   | Tvar ({ contents = Tlink (Tvar ({ contents = Tlink ty } as a0)) } as a1) ->
       let ty = type_repr ty in
       let link = Tlink ty in
       a0 := link;
       a1 := link;
       ty
   | Tvar { contents = Tlink ty } -> ty
   | Tarrow _ | T_constr _ | Tvar _ | Tparam _ | T_trait _ | T_builtin _
   | T_blackhole ->
       ty
    : t)
