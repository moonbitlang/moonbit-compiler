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


module Strutil = Basic_strutil
module Qual_ident = Basic_qual_ident
module Type_path = Basic_type_path

module Key = struct
  type t = Pdot of Qual_ident.t | Pident of { stamp : int; name : string }

  include struct
    let _ = fun (_ : t) -> ()

    let compare =
      (fun a__001_ ->
         fun b__002_ ->
          if Stdlib.( == ) a__001_ b__002_ then 0
          else
            match (a__001_, b__002_) with
            | Pdot _a__003_, Pdot _b__004_ ->
                Qual_ident.compare _a__003_ _b__004_
            | Pdot _, _ -> -1
            | _, Pdot _ -> 1
            | Pident _a__005_, Pident _b__006_ -> (
                match Stdlib.compare (_a__005_.stamp : int) _b__006_.stamp with
                | 0 -> Stdlib.compare (_a__005_.name : string) _b__006_.name
                | n -> n)
        : t -> t -> int)

    let _ = compare

    let equal =
      (fun a__007_ ->
         fun b__008_ ->
          if Stdlib.( == ) a__007_ b__008_ then true
          else
            match (a__007_, b__008_) with
            | Pdot _a__009_, Pdot _b__010_ -> Qual_ident.equal _a__009_ _b__010_
            | Pdot _, _ -> false
            | _, Pdot _ -> false
            | Pident _a__011_, Pident _b__012_ ->
                Stdlib.( && )
                  (Stdlib.( = ) (_a__011_.stamp : int) _b__012_.stamp)
                  (Stdlib.( = ) (_a__011_.name : string) _b__012_.name)
        : t -> t -> bool)

    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      (fun hsv ->
         fun arg ->
          match arg with
          | Pdot _a0 ->
              let hsv = Ppx_base.hash_fold_int hsv 0 in
              let hsv = hsv in
              Qual_ident.hash_fold_t hsv _a0
          | Pident _ir ->
              let hsv = Ppx_base.hash_fold_int hsv 1 in
              let hsv =
                let hsv = hsv in
                Ppx_base.hash_fold_int hsv _ir.stamp
              in
              Ppx_base.hash_fold_string hsv _ir.name
        : Ppx_base.state -> t -> Ppx_base.state)

    let _ = hash_fold_t

    let (hash : t -> Ppx_base.hash_value) =
      let func arg =
        Ppx_base.get_hash_value
          (let hsv = Ppx_base.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x

    let _ = hash
  end

  let to_string (x : t) =
    match x with
    | Pdot qual_name -> Qual_ident.string_of_t qual_name
    | Pident { name; stamp } -> name ^ "/" ^ Stdlib.string_of_int stamp

  let sexp_of_t x = (Atom (to_string x) : S.t)
end

include Key
module Hash = Basic_hashf.Make (Key)
module Hashset = Basic_hashsetf.Make (Key)

let id = ref 0
let reset () = id := 0
let of_qual_ident qual_name = Pdot qual_name

let fresh name =
  incr id;
  Pident { name; stamp = !id }

let make_closure_wrapper original_fn =
  match[@warning "-fragile-match"] original_fn with
  | Pdot qual_name ->
      Pdot
        (Qual_ident.map qual_name ~f:(fun name ->
             ((name ^ ".dyncall" : Stdlib.String.t) [@merlin.hide])))
  | _ -> assert false

let make_object_wrapper fn ~trait =
  let trait_name =
    Type_path.export_name ~cur_pkg_name:!Basic_config.current_package trait
  in
  Pdot
    (Qual_ident.map fn ~f:(fun name ->
         (name ^ ".dyncall_as_" ^ trait_name : Stdlib.String.t)))

let source_name t =
  match t with
  | Pdot qual_name -> Qual_ident.string_of_t qual_name
  | Pident { name; _ } -> name

let to_wasm_name (x : t) =
  match x with
  | Pdot qual_name -> Qual_ident.to_wasm_name qual_name
  | Pident { name; stamp } ->
      Stdlib.String.concat ""
        [ "$"; Strutil.mangle_wasm_name name; "/"; Int.to_string stamp ]
      [@merlin.hide]

let init () =
  incr id;
  Pident { name = "*init*"; stamp = !id }

let main () =
  incr id;
  Pident { name = "*main*"; stamp = !id }
