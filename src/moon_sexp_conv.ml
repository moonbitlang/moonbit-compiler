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


external format_float : string -> float -> string = "caml_format_float"

let default_string_of_float =
  ref (fun x ->
      let y = format_float "%.15G" x in
      if float_of_string y = x then y else format_float "%.17G" x)

let sexp_of_unit () = (List [] : S.t)
let sexp_of_bool b = S.Atom (string_of_bool b)
let sexp_of_string str = S.Atom str
let sexp_of_bytes bytes = S.Atom (Bytes.to_string bytes)
let sexp_of_char c = S.Atom (String.make 1 c)
let sexp_of_int n = S.Atom (string_of_int n)
let sexp_of_float n = S.Atom (!default_string_of_float n)
let sexp_of_int32 n = S.Atom (Int32.to_string n)
let sexp_of_int64 n = S.Atom (Int64.to_string n)
let sexp_of_ref sexp_of__a rf = sexp_of__a !rf
let write_old_option_format = ref true

let sexp_of_option sexp_of__a s =
  (match s with
   | Some x when !write_old_option_format -> List [ sexp_of__a x ]
   | Some x -> List [ Atom "some"; sexp_of__a x ]
   | None when !write_old_option_format -> List []
   | None -> Atom "none"
    : S.t)

let sexp_of_list sexp_of__a lst =
  (List (List.rev (List.rev_map sexp_of__a lst)) : S.t)

let sexp_of_array sexp_of__a ar =
  (let lst_ref = ref [] in
   for i = Array.length ar - 1 downto 0 do
     lst_ref := sexp_of__a ar.(i) :: !lst_ref
   done;
   List !lst_ref
    : S.t)

let sexp_of_lazy_t sexp_of__a lv = sexp_of__a (Lazy.force lv)
