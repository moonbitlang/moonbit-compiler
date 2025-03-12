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


type 'a t = Ok of 'a | Partial of 'a * Diagnostics.report list

let take_info (x : 'a t) ~(diagnostics : Diagnostics.t) =
  (match x with
   | Ok a -> a
   | Partial (a, err) ->
       Basic_lst.iter err ~f:(fun info ->
           Diagnostics.add_error diagnostics info);
       a
    : 'a)
