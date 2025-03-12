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


type qual_name = { pkg : string; id : string }

include struct
  let _ = fun (_ : qual_name) -> ()

  let equal_qual_name =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then true
        else
          Stdlib.( && )
            (Stdlib.( = ) (a__001_.pkg : string) b__002_.pkg)
            (Stdlib.( = ) (a__001_.id : string) b__002_.id)
      : qual_name -> qual_name -> bool)

  let _ = equal_qual_name
end

type t = Lident of string | Ldot of qual_name

include struct
  let _ = fun (_ : t) -> ()

  let equal =
    (fun a__003_ ->
       fun b__004_ ->
        if Stdlib.( == ) a__003_ b__004_ then true
        else
          match (a__003_, b__004_) with
          | Lident _a__005_, Lident _b__006_ ->
              Stdlib.( = ) (_a__005_ : string) _b__006_
          | Lident _, _ -> false
          | _, Lident _ -> false
          | Ldot _a__007_, Ldot _b__008_ -> equal_qual_name _a__007_ _b__008_
      : t -> t -> bool)

  let _ = equal
end

let to_string = function
  | Lident id -> id
  | Ldot { pkg; id } -> "@" ^ pkg ^ "." ^ id

let sexp_of_t (x : t) = (Atom (to_string x) : S.t)
