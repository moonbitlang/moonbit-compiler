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
  type key
  type 'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val add : 'a t -> key -> 'a -> unit
  val add_or_update : 'a t -> key -> update:('a -> 'a) -> 'a -> 'a
  val remove : 'a t -> key -> unit
  val find_exn : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val find_opt : 'a t -> key -> 'a option
  val find_or_update : 'a t -> key -> update:(key -> 'a) -> 'a
  val find_key_opt : 'a t -> key -> key option
  val find_default : 'a t -> key -> 'a -> 'a
  val replace : 'a t -> key -> 'a -> unit
  val update_if_exists : 'a t -> key -> ('a -> 'a) -> unit
  val mem : 'a t -> key -> bool
  val iter : 'a t -> (key * 'a -> unit) -> unit
  val to_iter : 'a t -> (key * 'a) Basic_iter.t
  val iter2 : 'a t -> (key -> 'a -> unit) -> unit
  val fold : 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b
  val length : 'a t -> int
  val to_list_with : 'a t -> (key -> 'a -> 'c) -> 'c list
  val to_list : 'a t -> (key * 'a) list
  val to_array : 'a t -> (key * 'a) array
  val to_array_map : 'a t -> (key * 'a -> 'b) -> 'b array
  val to_array_filter_map : 'a t -> (key * 'a -> 'b option) -> 'b array
  val of_list : (key * 'a) list -> 'a t
  val of_list2 : key list -> 'a list -> 'a t
  val of_list_map : 'a list -> ('a -> key * 'b) -> 'b t
  val sexp_of_t : ('a -> S.t) -> 'a t -> S.t
end

module type HashedType = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> S.t
end
