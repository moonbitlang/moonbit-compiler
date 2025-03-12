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


let sexp_of_array = Moon_sexp_conv.sexp_of_array
and sexp_of_bool = Moon_sexp_conv.sexp_of_bool
and sexp_of_bytes = Moon_sexp_conv.sexp_of_bytes
and sexp_of_char = Moon_sexp_conv.sexp_of_char
and sexp_of_float = Moon_sexp_conv.sexp_of_float
and sexp_of_int = Moon_sexp_conv.sexp_of_int
and sexp_of_int32 = Moon_sexp_conv.sexp_of_int32
and sexp_of_int64 = Moon_sexp_conv.sexp_of_int64
and sexp_of_list = Moon_sexp_conv.sexp_of_list
and sexp_of_option = Moon_sexp_conv.sexp_of_option
and sexp_of_ref = Moon_sexp_conv.sexp_of_ref
and sexp_of_string = Moon_sexp_conv.sexp_of_string
and sexp_of_unit = Moon_sexp_conv.sexp_of_unit

class ['self] sexp =
  object (self : 'self)
    method private visit_array :
        'env 'a. ('env -> 'a -> S.t) -> 'env -> 'a array -> S.t =
      fun f -> fun env -> sexp_of_array (f env)

    method private visit_bool : 'env. 'env -> bool -> S.t =
      fun _env -> sexp_of_bool

    method private visit_bytes : 'env. 'env -> bytes -> S.t =
      fun _env -> sexp_of_bytes

    method private visit_char : 'env. 'env -> char -> S.t =
      fun _env -> sexp_of_char

    method private visit_float : 'env. 'env -> float -> S.t =
      fun _env -> sexp_of_float

    method private visit_int : 'env. 'env -> int -> S.t =
      fun _env -> sexp_of_int

    method private visit_int32 : 'env. 'env -> int32 -> S.t =
      fun _env -> sexp_of_int32

    method private visit_int64 : 'env. 'env -> int64 -> S.t =
      fun _env -> sexp_of_int64

    method private visit_list :
        'env 'a. ('env -> 'a -> S.t) -> 'env -> 'a list -> S.t =
      fun f -> fun env -> sexp_of_list (f env)

    method private visit_option :
        'env 'a. ('env -> 'a -> S.t) -> 'env -> 'a option -> S.t =
      fun f -> fun env -> sexp_of_option (f env)

    method private visit_record : 'env. 'env -> (string * S.t) list -> S.t =
      fun _env ->
        fun flds ->
         S.List (Basic_lst.map flds (fun (lbl, v) -> S.List [ Atom lbl; v ]))

    method private visit_ref :
        'env 'a. ('env -> 'a -> S.t) -> 'env -> 'a ref -> S.t =
      fun f -> fun env -> sexp_of_ref (f env)

    method private visit_string : 'env. 'env -> string -> S.t =
      fun _env -> sexp_of_string

    method private visit_tuple : 'env. 'env -> S.t list -> S.t =
      fun _env -> fun xs -> S.List xs

    method private visit_unit : 'env. 'env -> unit -> S.t =
      fun _env -> sexp_of_unit

    method private visit_inline_tuple : 'env -> string -> S.t list -> S.t =
      fun _env ->
        fun ct ->
         fun xs ->
          let ct = self#visit_variant_ctor _env ct in
          match xs with [] -> ct | _ -> S.List (ct :: xs)

    method private visit_variant_ctor : 'env -> string -> S.t =
      fun _env ->
        fun ct ->
         let ct =
           match String.index_opt ct '_' with
           | Some i ->
               String.capitalize_ascii
                 (String.sub ct (i + 1) (String.length ct - i - 1))
           | None -> ct
         in
         S.Atom ct

    method private visit_inline_record :
        'env -> string -> (string * S.t) list -> S.t =
      fun _env ->
        fun ct ->
         fun flds ->
          let s = self#visit_variant_ctor _env ct in
          S.List (s :: Basic_lst.map flds snd)
  end
