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


module Hash_string = Basic_hash_string

type const_kind = Str | Bytes | Arr

include struct
  let _ = fun (_ : const_kind) -> ()

  let sexp_of_const_kind =
    (function
     | Str -> S.Atom "Str"
     | Bytes -> S.Atom "Bytes"
     | Arr -> S.Atom "Arr"
      : const_kind -> S.t)

  let _ = sexp_of_const_kind
  let equal_const_kind = (Stdlib.( = ) : const_kind -> const_kind -> bool)
  let _ = equal_const_kind

  let (hash_fold_const_kind : Ppx_base.state -> const_kind -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv
          (match arg with Str -> 0 | Bytes -> 1 | Arr -> 2)
      : Ppx_base.state -> const_kind -> Ppx_base.state)

  let _ = hash_fold_const_kind

  let (hash_const_kind : const_kind -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_const_kind hsv arg)
    in
    fun x -> func x

  let _ = hash_const_kind
end

type const = { kind : const_kind; value : string }

include struct
  let _ = fun (_ : const) -> ()

  let sexp_of_const =
    (fun { kind = kind__004_; value = value__006_ } ->
       let bnds__003_ = ([] : _ Stdlib.List.t) in
       let bnds__003_ =
         let arg__007_ = Moon_sexp_conv.sexp_of_string value__006_ in
         (S.List [ S.Atom "value"; arg__007_ ] :: bnds__003_ : _ Stdlib.List.t)
       in
       let bnds__003_ =
         let arg__005_ = sexp_of_const_kind kind__004_ in
         (S.List [ S.Atom "kind"; arg__005_ ] :: bnds__003_ : _ Stdlib.List.t)
       in
       S.List bnds__003_
      : const -> S.t)

  let _ = sexp_of_const

  let equal_const =
    (fun a__008_ ->
       fun b__009_ ->
        if Stdlib.( == ) a__008_ b__009_ then true
        else
          Stdlib.( && )
            (equal_const_kind a__008_.kind b__009_.kind)
            (Stdlib.( = ) (a__008_.value : string) b__009_.value)
      : const -> const -> bool)

  let _ = equal_const

  let (hash_fold_const : Ppx_base.state -> const -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       hash_fold_const_kind hsv arg.kind
     in
     Ppx_base.hash_fold_string hsv arg.value

  let _ = hash_fold_const

  let (hash_const : const -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_const hsv arg)
    in
    fun x -> func x

  let _ = hash_const
end

module Hash_const = Basic_hashf.Make (struct
  type t = const

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_const : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_const : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_const

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_const in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

type t = {
  offset_table : int Hash_const.t;
  str_index_table : int Hash_string.t;
  mutable current_offset : int;
  buf : Buffer.t;
}

let create () =
  {
    offset_table = Hash_const.create 10;
    str_index_table = Hash_string.create 10;
    current_offset = 0;
    buf = Buffer.create 50;
  }

let get_string_count (t : t) = Hash_string.length t.str_index_table

let find_str_const (obj : t) (s : string) =
  (match Hash_string.find_opt obj.str_index_table s with
   | Some index ->
       let offset =
         Hash_const.find_exn obj.offset_table { value = s; kind = Str }
       in
       (offset, index)
   | None ->
       let index = Hash_string.length obj.str_index_table in
       let offset = obj.current_offset in
       Hash_string.add obj.str_index_table s index;
       Hash_const.add obj.offset_table { value = s; kind = Str } offset;
       obj.current_offset <- obj.current_offset + String.length s;
       Buffer.add_string obj.buf s;
       (offset, index)
    : int * int)

let find_js_builtin_str_const (obj : t) (s : string) =
  (match Hash_string.find_opt obj.str_index_table s with
   | Some index -> index
   | None ->
       let index = Hash_string.length obj.str_index_table in
       Hash_string.add obj.str_index_table s index;
       index
    : int)

let find_const (obj : t) (s : string) (kind : const_kind) =
  (let const = { kind; value = s } in
   match Hash_const.find_opt obj.offset_table const with
   | Some i -> i
   | None ->
       let offset = obj.current_offset in
       let size = String.length s in
       obj.current_offset <- obj.current_offset + size;
       Buffer.add_string obj.buf s;
       Hash_const.add obj.offset_table const offset;
       offset
    : int)

let find_array_const (obj : t) (s : string) = (find_const obj s Arr : int)
let find_bytes_const (obj : t) (s : string) = (find_const obj s Bytes : int)
let to_wat_string (obj : t) = Buffer.contents obj.buf

let iter_constant_string_with_index (t : t) (f : string -> int -> unit) =
  Hash_string.iter2 t.str_index_table f
