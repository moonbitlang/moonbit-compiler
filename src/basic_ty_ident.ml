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


module Fn_address = Basic_fn_address
module Strutil = Basic_strutil
module Type_path = Basic_type_path
module Hashf = Basic_hashf
module Hashsetf = Basic_hashsetf

type t = string

include struct
  let _ = fun (_ : t) -> ()
  let sexp_of_t = (Moon_sexp_conv.sexp_of_string : t -> S.t)
  let _ = sexp_of_t

  let equal =
    (fun a__001_ -> fun b__002_ -> Stdlib.( = ) (a__001_ : string) b__002_
      : t -> t -> bool)

  let _ = equal

  let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
    Ppx_base.hash_fold_string

  and (hash : t -> Ppx_base.hash_value) =
    let func = Ppx_base.hash_string in
    fun x -> func x

  let _ = hash_fold_t
  and _ = hash
end

let of_string (s : string) = (s : t)
let to_string (t : t) = (t : string)

let to_wasm_name (t : t) =
  ("$" ^ Strutil.mangle_wasm_name (to_string t) : string)

let of_type_path (p : Type_path.t) =
  (Type_path.export_name ~cur_pkg_name:!Basic_config.current_package p : t)

let capture_of_function (addr : Fn_address.t) =
  (of_string (Fn_address.to_string addr ^ "-cap") : t)

let code_pointer_of_closure (closure_tid : t) =
  (of_string (to_string closure_tid ^ "-sig") : t)

let method_of_object (object_tid : t) method_index =
  ((object_tid ^ ".method_" ^ Int.to_string method_index : Stdlib.String.t) : t)

let concrete_object_type ~trait ~type_name =
  ((type_name ^ ".as_" ^ of_type_path trait : Stdlib.String.t) : t)

module Hash = Hashf.Make (struct
  type nonrec t = t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_t

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

module Hashset = Hashsetf.Make (struct
  type nonrec t = t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_t

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)
