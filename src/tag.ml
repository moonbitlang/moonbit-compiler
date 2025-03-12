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


type t = {
  index : int;
  is_constant_ : bool; [@ceh.ignore]
  name_ : string; [@ceh.ignore]
}

include struct
  let _ = fun (_ : t) -> ()

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then 0
        else Stdlib.compare (a__001_.index : int) b__002_.index
      : t -> t -> int)

  let _ = compare

  let equal =
    (fun a__003_ ->
       fun b__004_ ->
        if Stdlib.( == ) a__003_ b__004_ then true
        else Stdlib.( = ) (a__003_.index : int) b__004_.index
      : t -> t -> bool)

  let _ = equal

  let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv =
         let hsv = hsv in
         Ppx_base.hash_fold_int hsv arg.index
       in
       hsv
     in
     hsv

  let _ = hash_fold_t

  let (hash : t -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_t hsv arg)
    in
    fun x -> func x

  let _ = hash
end

let sexp_of_t { name_; index = _ } =
  (List
     (List.cons (Atom "Constr_tag_regular" : S.t) ([ Atom name_ ] : S.t list))
    : S.t)

let of_core_tag (extensible_tag_info : int Basic_hash_string.t)
    (tag : Basic_constr_info.constr_tag) =
  match tag with
  | Constr_tag_regular { index; name_; repr_; _ } ->
      let is_constant_ =
        match repr_ with
        | Constant -> true
        | Non_constant -> false
        | Integer _ -> assert false
      in
      { index; name_; is_constant_ }
  | Extensible_tag { pkg; type_name; name; total = _; index = _ } -> (
      let str = Basic_constr_info.ext_tag_to_str ~pkg ~type_name ~name in
      match Basic_hash_string.find_opt extensible_tag_info str with
      | Some index -> { index; name_ = str; is_constant_ = false }
      | None ->
          let index = Basic_hash_string.length extensible_tag_info in
          Basic_hash_string.add extensible_tag_info str index;
          { index; name_ = str; is_constant_ = false })

let of_core_tag_no_ext (tag : Basic_constr_info.constr_tag) =
  match tag with
  | Constr_tag_regular { index; name_; repr_; _ } ->
      let is_constant_ =
        match repr_ with
        | Constant -> true
        | Non_constant -> false
        | Integer _ -> assert false
      in
      { index; name_; is_constant_ }
  | Extensible_tag _ -> assert false
