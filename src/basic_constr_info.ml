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


module Index_set = Basic_diet.Make (Int)

type constr_repr = Non_constant | Constant | Integer of int

type constr_tag =
  | Constr_tag_regular of {
      total : Index_set.t; [@ceh.ignore]
      index : int;
      repr_ : constr_repr; [@ceh.ignore]
      name_ : string; [@ceh.ignore]
    }
  | Extensible_tag of {
      pkg : string;
      type_name : string;
      name : string;
      total : Index_set.t; [@ceh.ignore]
      index : int; [@ceh.ignore]
    }

include struct
  let _ = fun (_ : constr_tag) -> ()

  let compare_constr_tag =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then 0
        else
          match (a__001_, b__002_) with
          | Constr_tag_regular _a__003_, Constr_tag_regular _b__004_ ->
              Stdlib.compare (_a__003_.index : int) _b__004_.index
          | Constr_tag_regular _, _ -> -1
          | _, Constr_tag_regular _ -> 1
          | Extensible_tag _a__005_, Extensible_tag _b__006_ -> (
              match Stdlib.compare (_a__005_.pkg : string) _b__006_.pkg with
              | 0 -> (
                  match
                    Stdlib.compare
                      (_a__005_.type_name : string)
                      _b__006_.type_name
                  with
                  | 0 -> Stdlib.compare (_a__005_.name : string) _b__006_.name
                  | n -> n)
              | n -> n)
      : constr_tag -> constr_tag -> int)

  let _ = compare_constr_tag

  let equal_constr_tag =
    (fun a__007_ ->
       fun b__008_ ->
        if Stdlib.( == ) a__007_ b__008_ then true
        else
          match (a__007_, b__008_) with
          | Constr_tag_regular _a__009_, Constr_tag_regular _b__010_ ->
              Stdlib.( = ) (_a__009_.index : int) _b__010_.index
          | Constr_tag_regular _, _ -> false
          | _, Constr_tag_regular _ -> false
          | Extensible_tag _a__011_, Extensible_tag _b__012_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__011_.pkg : string) _b__012_.pkg)
                (Stdlib.( && )
                   (Stdlib.( = )
                      (_a__011_.type_name : string)
                      _b__012_.type_name)
                   (Stdlib.( = ) (_a__011_.name : string) _b__012_.name))
      : constr_tag -> constr_tag -> bool)

  let _ = equal_constr_tag

  let (hash_fold_constr_tag : Ppx_base.state -> constr_tag -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Constr_tag_regular _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv =
              let hsv =
                let hsv =
                  let hsv = hsv in
                  hsv
                in
                Ppx_base.hash_fold_int hsv _ir.index
              in
              hsv
            in
            hsv
        | Extensible_tag _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv =
              let hsv =
                let hsv =
                  let hsv =
                    let hsv = hsv in
                    Ppx_base.hash_fold_string hsv _ir.pkg
                  in
                  Ppx_base.hash_fold_string hsv _ir.type_name
                in
                Ppx_base.hash_fold_string hsv _ir.name
              in
              hsv
            in
            hsv
      : Ppx_base.state -> constr_tag -> Ppx_base.state)

  let _ = hash_fold_constr_tag

  let (hash_constr_tag : constr_tag -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_constr_tag hsv arg)
    in
    fun x -> func x

  let _ = hash_constr_tag
end

let sexp_of_constr_tag (tag : constr_tag) =
  match tag with
  | Constr_tag_regular { name_; total = _; index = _; repr_ = _ } ->
      (List
         (List.cons
            (Atom "Constr_tag_regular" : S.t)
            ([ Atom name_ ] : S.t list))
        : S.t)
  | Extensible_tag { pkg; type_name; name; total = _; index = _ } ->
      (List
         (List.cons
            (Atom "Extensible_tag" : S.t)
            (List.cons
               (Atom pkg : S.t)
               (List.cons (Atom type_name : S.t) ([ Atom name ] : S.t list))))
        : S.t)

let equal = equal_constr_tag

let ext_tag_to_str ~(pkg : string) ~(type_name : string) ~(name : string) =
  if pkg = "" then (type_name ^ "." ^ name : Stdlib.String.t)
  else Stdlib.String.concat "" [ pkg; "."; type_name; "."; name ]

let get_name tag =
  match tag with
  | Constr_tag_regular { name_ = name; _ } | Extensible_tag { name; _ } -> name
