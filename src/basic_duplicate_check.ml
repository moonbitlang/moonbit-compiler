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


let check_duplicate_string_list (l : string list) =
  (match l with
   | [] -> false
   | _ :: [] -> false
   | [ x0; x1 ] -> x0 = x1
   | [ x0; x1; x2 ] -> x0 = x1 || x0 = x2 || x1 = x2
   | [ x0; x1; x2; x3 ] ->
       x0 = x1 || x0 = x2 || x0 = x3 || x1 = x2 || x1 = x3 || x2 = x3
   | _ ->
       let rec aux flag container = function
         | [] -> !flag
         | x :: rest ->
             let container =
               Basic_set_string.check_add container ~duplicate_flag:flag x
             in
             !flag || aux flag container rest
       in
       aux (ref false) Basic_set_string.empty l
    : bool)

let check_duplicate_by (l : 'a list) (f : 'a -> string) =
  (match l with
   | [] -> None
   | _ :: [] -> None
   | [ x0; x1 ] ->
       let a0 = f x0 in
       let a1 = f x1 in
       if a0 = a1 then Some x1 else None
   | [ x0; x1; x2 ] ->
       let a0 = f x0 in
       let a1 = f x1 in
       if a0 = a1 then Some x1
       else
         let a2 = f x2 in
         if a0 = a2 || a1 = a2 then Some x2 else None
   | [ x0; x1; x2; x3 ] ->
       let a0 = f x0 in
       let a1 = f x1 in
       if a0 = a1 then Some x1
       else
         let a2 = f x2 in
         if a0 = a2 || a1 = a2 then Some x2
         else
           let a3 = f x3 in
           if a0 = a3 || a1 = a3 || a2 = a3 then Some x3 else None
   | _ ->
       let rec aux flag container = function
         | [] -> None
         | x :: rest ->
             let container =
               Basic_set_string.check_add container ~duplicate_flag:flag (f x)
             in
             if !flag then Some x else aux flag container rest
       in
       aux (ref false) Basic_set_string.empty l
    : 'a option)
