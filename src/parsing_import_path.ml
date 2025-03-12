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


type t = { path : string; alias : string option }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { path = path__002_; alias = alias__004_ } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__005_ =
           Moon_sexp_conv.sexp_of_option Moon_sexp_conv.sexp_of_string
             alias__004_
         in
         (S.List [ S.Atom "alias"; arg__005_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Moon_sexp_conv.sexp_of_string path__002_ in
         (S.List [ S.Atom "path"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : t -> S.t)

  let _ = sexp_of_t
end

let default_abbrev s =
  let s_len = String.length s in
  match String.rindex_opt s '/' with
  | None -> s
  | Some x -> String.sub s (x + 1) (s_len - x - 1)

let parse (s : string) =
  (match Basic_strutil.split_on_last ':' s with
   | "", s -> { path = s; alias = None }
   | path, alias -> { path; alias = Some alias }
    : t)
