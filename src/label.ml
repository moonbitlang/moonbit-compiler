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


module Label = struct
  type t = { name : string; [@ceh.ignore] stamp : int }

  include struct
    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun { name = name__002_; stamp = stamp__004_ } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_int stamp__004_ in
           (S.List [ S.Atom "stamp"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_string name__002_ in
           (S.List [ S.Atom "name"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         S.List bnds__001_
        : t -> S.t)

    let _ = sexp_of_t

    let equal =
      (fun a__006_ ->
         fun b__007_ ->
          if Stdlib.( == ) a__006_ b__007_ then true
          else Stdlib.( = ) (a__006_.stamp : int) b__007_.stamp
        : t -> t -> bool)

    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
     fun hsv ->
      fun arg ->
       let hsv =
         let hsv = hsv in
         hsv
       in
       Ppx_base.hash_fold_int hsv arg.stamp

    let _ = hash_fold_t

    let (hash : t -> Ppx_base.hash_value) =
      let func arg =
        Ppx_base.get_hash_value
          (let hsv = Ppx_base.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x

    let _ = hash

    let compare =
      (fun a__008_ ->
         fun b__009_ ->
          if Stdlib.( == ) a__008_ b__009_ then 0
          else Stdlib.compare (a__008_.stamp : int) b__009_.stamp
        : t -> t -> int)

    let _ = compare
  end
end

include Label

let dummy : t = { name = ""; stamp = -1 }
let fresh name = { name; stamp = Basic_uuid.next () }
let rename t = { name = t.name; stamp = Basic_uuid.next () }

let to_wasm_name (t : t) =
  Stdlib.String.concat "" [ "$"; t.name; "/"; Int.to_string t.stamp ]

let to_string t = t.name ^ "/" ^ string_of_int t.stamp
let basename t = t.name

let to_wasm_label_loop t =
  let x = t.stamp in
  ("$loop:" ^ Int.to_string x : Stdlib.String.t)

let to_wasm_label_break t =
  let x = t.stamp in
  ("$break:" ^ Int.to_string x : Stdlib.String.t)

module Hash = Basic_hashf.Make (Label)
module Hashset = Basic_hashsetf.Make (Label)
module Map = Basic_mapf.Make (Label)
