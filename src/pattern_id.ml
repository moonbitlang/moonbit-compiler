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


type key = int list

module Pattern_id_map = Basic_mapf.Make (struct
  type t = int list

  include struct
    let _ = fun (_ : t) -> ()

    let compare =
      (fun a__001_ ->
         fun b__002_ ->
          Ppx_base.compare_list
            (fun a__003_ ->
              fun b__004_ -> Stdlib.compare (a__003_ : int) b__004_)
            a__001_ b__002_
        : t -> t -> int)

    let _ = compare

    let sexp_of_t =
      (fun x__005_ ->
         Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_int x__005_
        : t -> S.t)

    let _ = sexp_of_t

    let equal =
      (fun a__006_ ->
         fun b__007_ ->
          Ppx_base.equal_list
            (fun a__008_ -> fun b__009_ -> Stdlib.( = ) (a__008_ : int) b__009_)
            a__006_ b__007_
        : t -> t -> bool)

    let _ = equal
  end
end)

type t = bool Pattern_id_map.t ref

let create () = (ref Pattern_id_map.empty : t)

let is_unreachable (self : t) (pat_id : key) =
  Pattern_id_map.find_default !self pat_id true

let mark_reachable (self : t) (pat_id : key) =
  self := Pattern_id_map.add !self pat_id false
