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
module Lst = Basic_lst

module Hashed_type = struct
  type t =
    | Hconstr of { hash : int; [@sexp_drop] tycon : Type_path.t; tys : t list }
    | Harrow of {
        hash : int; [@sexp_drop]
        params : t list;
        ret : t;
        err : t option;
        is_async : bool;
      }
    | Hparam of { hash : int; [@sexp_drop] index : int }
    | Htrait of { hash : int; trait : Type_path.t }
    | Hunit
    | Hbool
    | Hbyte
    | Hint16
    | Huint16
    | Hchar
    | Hint
    | Hint64
    | Huint
    | Huint64
    | Hfloat
    | Hdouble
    | Hstring
    | Hbytes
    | Hblackhole

  include struct
    let _ = fun (_ : t) -> ()

    let rec sexp_of_t =
      (function
       | Hconstr { hash = hash__002_; tycon = tycon__004_; tys = tys__006_ } ->
           let bnds__001_ = ([] : _ Stdlib.List.t) in
           let bnds__001_ =
             let arg__007_ = Moon_sexp_conv.sexp_of_list sexp_of_t tys__006_ in
             (S.List [ S.Atom "tys"; arg__007_ ] :: bnds__001_
               : _ Stdlib.List.t)
           in
           let bnds__001_ =
             let arg__005_ = Type_path.sexp_of_t tycon__004_ in
             (S.List [ S.Atom "tycon"; arg__005_ ] :: bnds__001_
               : _ Stdlib.List.t)
           in
           let bnds__001_ =
             let arg__003_ = Moon_sexp_conv.sexp_of_int hash__002_ in
             (S.List [ S.Atom "hash"; arg__003_ ] :: bnds__001_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Hconstr" :: bnds__001_)
       | Harrow
           {
             hash = hash__009_;
             params = params__011_;
             ret = ret__013_;
             err = err__015_;
             is_async = is_async__017_;
           } ->
           let bnds__008_ = ([] : _ Stdlib.List.t) in
           let bnds__008_ =
             let arg__018_ = Moon_sexp_conv.sexp_of_bool is_async__017_ in
             (S.List [ S.Atom "is_async"; arg__018_ ] :: bnds__008_
               : _ Stdlib.List.t)
           in
           let bnds__008_ =
             let arg__016_ =
               Moon_sexp_conv.sexp_of_option sexp_of_t err__015_
             in
             (S.List [ S.Atom "err"; arg__016_ ] :: bnds__008_
               : _ Stdlib.List.t)
           in
           let bnds__008_ =
             let arg__014_ = sexp_of_t ret__013_ in
             (S.List [ S.Atom "ret"; arg__014_ ] :: bnds__008_
               : _ Stdlib.List.t)
           in
           let bnds__008_ =
             let arg__012_ =
               Moon_sexp_conv.sexp_of_list sexp_of_t params__011_
             in
             (S.List [ S.Atom "params"; arg__012_ ] :: bnds__008_
               : _ Stdlib.List.t)
           in
           let bnds__008_ =
             let arg__010_ = Moon_sexp_conv.sexp_of_int hash__009_ in
             (S.List [ S.Atom "hash"; arg__010_ ] :: bnds__008_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Harrow" :: bnds__008_)
       | Hparam { hash = hash__020_; index = index__022_ } ->
           let bnds__019_ = ([] : _ Stdlib.List.t) in
           let bnds__019_ =
             let arg__023_ = Moon_sexp_conv.sexp_of_int index__022_ in
             (S.List [ S.Atom "index"; arg__023_ ] :: bnds__019_
               : _ Stdlib.List.t)
           in
           let bnds__019_ =
             let arg__021_ = Moon_sexp_conv.sexp_of_int hash__020_ in
             (S.List [ S.Atom "hash"; arg__021_ ] :: bnds__019_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Hparam" :: bnds__019_)
       | Htrait { hash = hash__025_; trait = trait__027_ } ->
           let bnds__024_ = ([] : _ Stdlib.List.t) in
           let bnds__024_ =
             let arg__028_ = Type_path.sexp_of_t trait__027_ in
             (S.List [ S.Atom "trait"; arg__028_ ] :: bnds__024_
               : _ Stdlib.List.t)
           in
           let bnds__024_ =
             let arg__026_ = Moon_sexp_conv.sexp_of_int hash__025_ in
             (S.List [ S.Atom "hash"; arg__026_ ] :: bnds__024_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Htrait" :: bnds__024_)
       | Hunit -> S.Atom "Hunit"
       | Hbool -> S.Atom "Hbool"
       | Hbyte -> S.Atom "Hbyte"
       | Hint16 -> S.Atom "Hint16"
       | Huint16 -> S.Atom "Huint16"
       | Hchar -> S.Atom "Hchar"
       | Hint -> S.Atom "Hint"
       | Hint64 -> S.Atom "Hint64"
       | Huint -> S.Atom "Huint"
       | Huint64 -> S.Atom "Huint64"
       | Hfloat -> S.Atom "Hfloat"
       | Hdouble -> S.Atom "Hdouble"
       | Hstring -> S.Atom "Hstring"
       | Hbytes -> S.Atom "Hbytes"
       | Hblackhole -> S.Atom "Hblackhole"
        : t -> S.t)

    let _ = sexp_of_t

    let rec (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      (fun hsv ->
         fun arg ->
          match arg with
          | Hconstr _ir ->
              let hsv = Ppx_base.hash_fold_int hsv 0 in
              let hsv =
                let hsv =
                  let hsv = hsv in
                  Ppx_base.hash_fold_int hsv _ir.hash
                in
                Type_path.hash_fold_t hsv _ir.tycon
              in
              Ppx_base.hash_fold_list hash_fold_t hsv _ir.tys
          | Harrow _ir ->
              let hsv = Ppx_base.hash_fold_int hsv 1 in
              let hsv =
                let hsv =
                  let hsv =
                    let hsv =
                      let hsv = hsv in
                      Ppx_base.hash_fold_int hsv _ir.hash
                    in
                    Ppx_base.hash_fold_list hash_fold_t hsv _ir.params
                  in
                  hash_fold_t hsv _ir.ret
                in
                Ppx_base.hash_fold_option hash_fold_t hsv _ir.err
              in
              Ppx_base.hash_fold_bool hsv _ir.is_async
          | Hparam _ir ->
              let hsv = Ppx_base.hash_fold_int hsv 2 in
              let hsv =
                let hsv = hsv in
                Ppx_base.hash_fold_int hsv _ir.hash
              in
              Ppx_base.hash_fold_int hsv _ir.index
          | Htrait _ir ->
              let hsv = Ppx_base.hash_fold_int hsv 3 in
              let hsv =
                let hsv = hsv in
                Ppx_base.hash_fold_int hsv _ir.hash
              in
              Type_path.hash_fold_t hsv _ir.trait
          | Hunit -> Ppx_base.hash_fold_int hsv 4
          | Hbool -> Ppx_base.hash_fold_int hsv 5
          | Hbyte -> Ppx_base.hash_fold_int hsv 6
          | Hint16 -> Ppx_base.hash_fold_int hsv 7
          | Huint16 -> Ppx_base.hash_fold_int hsv 8
          | Hchar -> Ppx_base.hash_fold_int hsv 9
          | Hint -> Ppx_base.hash_fold_int hsv 10
          | Hint64 -> Ppx_base.hash_fold_int hsv 11
          | Huint -> Ppx_base.hash_fold_int hsv 12
          | Huint64 -> Ppx_base.hash_fold_int hsv 13
          | Hfloat -> Ppx_base.hash_fold_int hsv 14
          | Hdouble -> Ppx_base.hash_fold_int hsv 15
          | Hstring -> Ppx_base.hash_fold_int hsv 16
          | Hbytes -> Ppx_base.hash_fold_int hsv 17
          | Hblackhole -> Ppx_base.hash_fold_int hsv 18
        : Ppx_base.state -> t -> Ppx_base.state)

    and (hash : t -> Ppx_base.hash_value) =
      let func arg =
        Ppx_base.get_hash_value
          (let hsv = Ppx_base.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end

  let hash t =
    match t with
    | Hconstr { hash; _ }
    | Harrow { hash; _ }
    | Hparam { hash; _ }
    | Htrait { hash; _ } ->
        hash
    | Hunit | Hbool | Hbyte | Hint16 | Huint16 | Hchar | Hint | Hint64 | Huint
    | Huint64 | Hfloat | Hdouble | Hstring | Hbytes | Hblackhole ->
        hash t

  let hash_fold_t hsv t = Ppx_base.hash_fold_int hsv (hash t)

  let equal t1 t2 =
    match t1 with
    | Hconstr { hash = _; tycon = p1; tys = tys1 } -> (
        match t2 with
        | Hconstr { hash = _; tycon = p2; tys = tys2 } ->
            Type_path.equal p1 p2
            && Lst.for_all2_no_exn tys1 tys2 Basic_prelude.phys_equal
        | _ -> false)
    | Harrow { hash = _; params = ps1; ret = r1; err = e1; is_async = a1 } -> (
        match t2 with
        | Harrow { hash = _; params = ps2; ret = r2; err = e2; is_async = a2 }
          -> (
            Lst.for_all2_no_exn ps1 ps2 Basic_prelude.phys_equal
            && Basic_prelude.phys_equal r1 r2
            && a1 = a2
            &&
            match (e1, e2) with
            | None, None -> true
            | Some e1, Some e2 -> Basic_prelude.phys_equal e1 e2
            | None, Some _ | Some _, None -> false)
        | _ -> false)
    | Hparam { hash = _; index = i1 } -> (
        match t2 with Hparam { hash = _; index = i2 } -> i1 = i2 | _ -> false)
    | Htrait { hash = _; trait = trait1 } -> (
        match t2 with
        | Htrait { hash = _; trait = trait2 } -> Type_path.equal trait1 trait2
        | _ -> false)
    | Hunit -> ( match t2 with Hunit -> true | _ -> false)
    | Hbool -> ( match t2 with Hbool -> true | _ -> false)
    | Hbyte -> ( match t2 with Hbyte -> true | _ -> false)
    | Hint16 -> ( match t2 with Hint16 -> true | _ -> false)
    | Huint16 -> ( match t2 with Huint16 -> true | _ -> false)
    | Hchar -> ( match t2 with Hchar -> true | _ -> false)
    | Hint -> ( match t2 with Hint -> true | _ -> false)
    | Hint64 -> ( match t2 with Hint64 -> true | _ -> false)
    | Huint -> ( match t2 with Huint -> true | _ -> false)
    | Huint64 -> ( match t2 with Huint64 -> true | _ -> false)
    | Hfloat -> ( match t2 with Hfloat -> true | _ -> false)
    | Hdouble -> ( match t2 with Hdouble -> true | _ -> false)
    | Hstring -> ( match t2 with Hstring -> true | _ -> false)
    | Hbytes -> ( match t2 with Hbytes -> true | _ -> false)
    | Hblackhole -> ( match t2 with Hblackhole -> true | _ -> false)
end

include Hashed_type
module Hashset = Basic_hashsetf.Make (Hashed_type)

let equal (t1 : t) (t2 : t) = Basic_prelude.phys_equal t1 t2

type cache = Hashset.t

let make_cache () = (Hashset.create 42 : cache)

let constr cache tycon tys =
  let hsv = Ppx_base.create () in
  let hsv = Ppx_base.hash_fold_int hsv 0 in
  let hsv = Type_path.hash_fold_t hsv tycon in
  let hsv = Lst.fold_left tys hsv hash_fold_t in
  let t = Hconstr { hash = Ppx_base.get_hash_value hsv; tycon; tys } in
  Hashset.find_or_add cache t
[@@inline]

let arrow cache params ret err ~is_async =
  let hsv = Ppx_base.create () in
  let hsv = Ppx_base.hash_fold_int hsv 1 in
  let hsv = Ppx_base.hash_fold_bool hsv is_async in
  let hsv = Lst.fold_left params hsv hash_fold_t in
  let hsv = hash_fold_t hsv ret in
  let hsv = match err with None -> hsv | Some err -> hash_fold_t hsv err in
  let t =
    Harrow { hash = Ppx_base.get_hash_value hsv; params; ret; err; is_async }
  in
  Hashset.find_or_add cache t
[@@inline]

let param cache index =
  let hsv = Ppx_base.create () in
  let hsv = Ppx_base.hash_fold_int hsv 3 in
  let hsv = Ppx_base.hash_fold_int hsv index in
  let t = Hparam { hash = Ppx_base.get_hash_value hsv; index } in
  Hashset.find_or_add cache t
[@@inline]

let trait cache trait =
  let hsv = Ppx_base.create () in
  let hsv = Ppx_base.hash_fold_int hsv 3 in
  let hsv = Type_path.hash_fold_t hsv trait in
  let t = Htrait { hash = Ppx_base.get_hash_value hsv; trait } in
  Hashset.find_or_add cache t
[@@inline]

exception Unresolved_type_variable

let rec of_stype cache (sty : Stype.t) =
  let go sty = of_stype cache sty [@@inline] in
  match sty with
  | Tarrow { params_ty; ret_ty; err_ty; is_async } ->
      arrow cache (Lst.map params_ty go) (go ret_ty)
        (match err_ty with None -> None | Some e -> Some (go e))
        ~is_async
  | T_constr { type_constructor; tys } ->
      constr cache type_constructor (Lst.map tys go)
  | Tparam { index } -> param cache index
  | T_trait t -> trait cache t
  | Tvar { contents = Tlink sty' } -> go sty'
  | Tvar _ -> raise_notrace Unresolved_type_variable
  | T_builtin T_unit -> Hunit
  | T_builtin T_bool -> Hbool
  | T_builtin T_byte -> Hbyte
  | T_builtin T_char -> Hchar
  | T_builtin T_int16 -> Hint16
  | T_builtin T_uint16 -> Huint16
  | T_builtin T_int -> Hint
  | T_builtin T_int64 -> Hint64
  | T_builtin T_uint -> Huint
  | T_builtin T_uint64 -> Huint64
  | T_builtin T_float -> Hfloat
  | T_builtin T_double -> Hdouble
  | T_builtin T_string -> Hstring
  | T_builtin T_bytes -> Hbytes
  | T_blackhole -> Hblackhole
