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

module Key = struct
  type t = Hashed_type.t * Type_path.t

  include struct
    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun (arg0__001_, arg1__002_) ->
         let res0__003_ = Hashed_type.sexp_of_t arg0__001_
         and res1__004_ = Type_path.sexp_of_t arg1__002_ in
         S.List [ res0__003_; res1__004_ ]
        : t -> S.t)

    let _ = sexp_of_t

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
     fun hsv ->
      fun arg ->
       let e0, e1 = arg in
       let hsv = Hashed_type.hash_fold_t hsv e0 in
       let hsv = Type_path.hash_fold_t hsv e1 in
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

    let equal =
      (fun a__005_ ->
         fun b__006_ ->
          let t__007_, t__008_ = a__005_ in
          let t__009_, t__010_ = b__006_ in
          Stdlib.( && )
            (Hashed_type.equal t__007_ t__009_)
            (Type_path.equal t__008_ t__010_)
        : t -> t -> bool)

    let _ = equal
  end
end

include Basic_hashf.Make (Key)

type 'err result = Success | Failure of 'err
type nonrec 'err t = 'err result t
