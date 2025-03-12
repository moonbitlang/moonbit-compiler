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
module Ident = Basic_core_ident

type object_key = { trait : Type_path.t; type_ : string }

include struct
  let _ = fun (_ : object_key) -> ()

  let sexp_of_object_key =
    (fun { trait = trait__002_; type_ = type___004_ } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_string type___004_ in
         (S.List [ S.Atom "type_"; arg__005_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Type_path.sexp_of_t trait__002_ in
         (S.List [ S.Atom "trait"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : object_key -> S.t)

  let _ = sexp_of_object_key

  let equal_object_key =
    (fun a__006_ ->
       fun b__007_ ->
        if Stdlib.( == ) a__006_ b__007_ then true
        else
          Stdlib.( && )
            (Type_path.equal a__006_.trait b__007_.trait)
            (Stdlib.( = ) (a__006_.type_ : string) b__007_.type_)
      : object_key -> object_key -> bool)

  let _ = equal_object_key

  let (hash_fold_object_key : Ppx_base.state -> object_key -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       Type_path.hash_fold_t hsv arg.trait
     in
     Ppx_base.hash_fold_string hsv arg.type_

  let _ = hash_fold_object_key

  let (hash_object_key : object_key -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_object_key hsv arg)
    in
    fun x -> func x

  let _ = hash_object_key
end

module Hash = Basic_hashf.Make (struct
  type t = object_key

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_object_key : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_object_key : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      hash_fold_object_key

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_object_key in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

type object_method_item = {
  method_id : Ident.t;
  method_prim : Primitive.prim option;
  method_ty : Mtype.t;
}

include struct
  let _ = fun (_ : object_method_item) -> ()

  let sexp_of_object_method_item =
    (fun {
           method_id = method_id__011_;
           method_prim = method_prim__013_;
           method_ty = method_ty__015_;
         }
     ->
       let bnds__010_ = ([] : _ Stdlib.List.t) in
       let bnds__010_ =
         let arg__016_ = Mtype.sexp_of_t method_ty__015_ in
         (S.List [ S.Atom "method_ty"; arg__016_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__014_ =
           Moon_sexp_conv.sexp_of_option Primitive.sexp_of_prim
             method_prim__013_
         in
         (S.List [ S.Atom "method_prim"; arg__014_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__012_ = Ident.sexp_of_t method_id__011_ in
         (S.List [ S.Atom "method_id"; arg__012_ ] :: bnds__010_
           : _ Stdlib.List.t)
       in
       S.List bnds__010_
      : object_method_item -> S.t)

  let _ = sexp_of_object_method_item
end

type object_info = { self_ty : Mtype.t; methods : object_method_item list }

include struct
  let _ = fun (_ : object_info) -> ()

  let sexp_of_object_info =
    (fun { self_ty = self_ty__018_; methods = methods__020_ } ->
       let bnds__017_ = ([] : _ Stdlib.List.t) in
       let bnds__017_ =
         let arg__021_ =
           Moon_sexp_conv.sexp_of_list sexp_of_object_method_item methods__020_
         in
         (S.List [ S.Atom "methods"; arg__021_ ] :: bnds__017_
           : _ Stdlib.List.t)
       in
       let bnds__017_ =
         let arg__019_ = Mtype.sexp_of_t self_ty__018_ in
         (S.List [ S.Atom "self_ty"; arg__019_ ] :: bnds__017_
           : _ Stdlib.List.t)
       in
       S.List bnds__017_
      : object_info -> S.t)

  let _ = sexp_of_object_info
end

type t = object_info Hash.t

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun x__022_ -> Hash.sexp_of_t sexp_of_object_info x__022_ : t -> S.t)

  let _ = sexp_of_t
end
