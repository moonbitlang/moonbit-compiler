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


module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val iter : t -> (elt -> unit) -> unit
  val fold : t -> 'a -> (elt -> 'a -> 'a) -> 'a
  val for_all : t -> (elt -> bool) -> bool
  val exists : t -> (elt -> bool) -> bool
  val singleton : elt -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val choose : t -> elt
  val mem : t -> elt -> bool
  val add : t -> elt -> t
  val check_add : t -> duplicate_flag:bool ref -> elt -> t
  val remove : t -> elt -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val of_list : elt list -> t
  val of_sorted_array : elt array -> t
  val invariant : t -> bool
  val add_list : t -> elt list -> t
  val to_list : t -> elt list
  val map_to_list : t -> (elt -> 'a) -> 'a list
  val sexp_of_t : t -> S.t
  val filter : t -> (elt -> bool) -> t
end

module type OrderedType = sig
  type t

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> S.t
    val compare : t -> t -> int
    val equal : t -> t -> bool
  end
end
