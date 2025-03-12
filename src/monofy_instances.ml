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


module Ident = Basic_core_ident
module Lst = Basic_lst

type value_item = {
  types : Type_args.t;
  binder : Ident.t;
  old_binder : Basic_qual_ident.t;
}

and t = value_item list

include struct
  let _ = fun (_ : value_item) -> ()
  let _ = fun (_ : t) -> ()

  let rec sexp_of_value_item =
    (fun {
           types = types__002_;
           binder = binder__004_;
           old_binder = old_binder__006_;
         }
     ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__007_ = Basic_qual_ident.sexp_of_t old_binder__006_ in
         (S.List [ S.Atom "old_binder"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = Ident.sexp_of_t binder__004_ in
         (S.List [ S.Atom "binder"; arg__005_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Type_args.sexp_of_t types__002_ in
         (S.List [ S.Atom "types"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : value_item -> S.t)

  and sexp_of_t =
    (fun x__008_ -> Moon_sexp_conv.sexp_of_list sexp_of_value_item x__008_
      : t -> S.t)

  let _ = sexp_of_value_item
  and _ = sexp_of_t
end

let find_item (items : t) (tys : Type_args.t) =
  (Lst.find_opt items (fun item ->
       if Type_args.equal tys item.types then Some item.binder else None)
    : Ident.t option)

let has_type_args (items : t) (tys : Type_args.t) =
  (Lst.exists items (fun item -> Type_args.equal tys item.types) : bool)
