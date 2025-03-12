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
module Arr = Basic_arr

type trait = Type_path.t

include struct
  let _ = fun (_ : trait) -> ()
  let sexp_of_trait = (Type_path.sexp_of_t : trait -> S.t)
  let _ = sexp_of_trait
end

type type_name = Type_path.t

include struct
  let _ = fun (_ : type_name) -> ()
  let sexp_of_type_name = (Type_path.sexp_of_t : type_name -> S.t)
  let _ = sexp_of_type_name
end

type method_name = string

include struct
  let _ = fun (_ : method_name) -> ()
  let sexp_of_method_name = (Moon_sexp_conv.sexp_of_string : method_name -> S.t)
  let _ = sexp_of_method_name
end

type key = {
  trait : Type_path.t;
  self_type : Type_path.t;
  method_name : string;
}

include struct
  let _ = fun (_ : key) -> ()

  let equal_key =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then true
        else
          Stdlib.( && )
            (Type_path.equal a__001_.trait b__002_.trait)
            (Stdlib.( && )
               (Type_path.equal a__001_.self_type b__002_.self_type)
               (Stdlib.( = ) (a__001_.method_name : string) b__002_.method_name))
      : key -> key -> bool)

  let _ = equal_key

  let (hash_fold_key : Ppx_base.state -> key -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv =
         let hsv = hsv in
         Type_path.hash_fold_t hsv arg.trait
       in
       Type_path.hash_fold_t hsv arg.self_type
     in
     Ppx_base.hash_fold_string hsv arg.method_name

  let _ = hash_fold_key

  let (hash_key : key -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_key hsv arg)
    in
    fun x -> func x

  let _ = hash_key

  let sexp_of_key =
    (fun {
           trait = trait__004_;
           self_type = self_type__006_;
           method_name = method_name__008_;
         }
     ->
       let bnds__003_ = ([] : _ Stdlib.List.t) in
       let bnds__003_ =
         let arg__009_ = Moon_sexp_conv.sexp_of_string method_name__008_ in
         (S.List [ S.Atom "method_name"; arg__009_ ] :: bnds__003_
           : _ Stdlib.List.t)
       in
       let bnds__003_ =
         let arg__007_ = Type_path.sexp_of_t self_type__006_ in
         (S.List [ S.Atom "self_type"; arg__007_ ] :: bnds__003_
           : _ Stdlib.List.t)
       in
       let bnds__003_ =
         let arg__005_ = Type_path.sexp_of_t trait__004_ in
         (S.List [ S.Atom "trait"; arg__005_ ] :: bnds__003_ : _ Stdlib.List.t)
       in
       S.List bnds__003_
      : key -> S.t)

  let _ = sexp_of_key

  let compare_key =
    (fun a__010_ ->
       fun b__011_ ->
        if Stdlib.( == ) a__010_ b__011_ then 0
        else
          match Type_path.compare a__010_.trait b__011_.trait with
          | 0 -> (
              match Type_path.compare a__010_.self_type b__011_.self_type with
              | 0 ->
                  Stdlib.compare
                    (a__010_.method_name : string)
                    b__011_.method_name
              | n -> n)
          | n -> n
      : key -> key -> int)

  let _ = compare_key
end

module Key = struct
  type t = key

  include struct
    let _ = fun (_ : t) -> ()
    let equal = (equal_key : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_key

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_key in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash

    let sexp_of_t = (sexp_of_key : t -> S.t)
    let _ = sexp_of_t
  end
end

module H = Basic_hashf.Make (Key)

type method_info = Method_env.method_info

include struct
  let _ = fun (_ : method_info) -> ()

  let sexp_of_method_info =
    (Method_env.sexp_of_method_info : method_info -> S.t)

  let _ = sexp_of_method_info
end

type t = method_info H.t

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun x__014_ -> H.sexp_of_t sexp_of_method_info x__014_ : t -> S.t)

  let _ = sexp_of_t
end

let empty () = H.create 17

let find_method (env : t) ~(trait : trait) ~(self_type : type_name)
    ~(method_name : method_name) =
  (H.find_opt env { trait; self_type; method_name } : method_info option)

let add_method (env : t) ~(trait : Type_path.t) ~(self_type : Type_path.t)
    ~(method_name : string) (meth : method_info) =
  H.add env { trait; self_type; method_name } meth

let iter (env : t) (f : key * method_info -> unit) = H.iter env f

let iter_pub (env : t) (f : key * method_info -> unit) =
  H.iter env (fun ((_, method_info) as entry) ->
      if method_info.pub then f entry)

let of_array (arr : (key * method_info) array) =
  let env = empty () in
  Arr.iter arr (fun (k, v) -> H.add env k v);
  env
