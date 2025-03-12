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


type t = int

let equal = Int.equal

let sexp_of_t (i : int) =
  (Atom ("$join:" ^ Int.to_string i : Stdlib.String.t) : S.t)

let of_core_ident (id : Basic_core_ident.t) =
  match id with
  | Pident { stamp; _ } | Pmutable_ident { stamp; _ } -> stamp
  | Pdot _ -> assert false
  | Plocal_method _ -> assert false

let to_wasm_label x = ("$join:" ^ Int.to_string x : Stdlib.String.t)

module Map = Basic_map_int
module Hash = Basic_hash_int
