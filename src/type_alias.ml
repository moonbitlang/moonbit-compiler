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


module Type_path = Basic_type_path

type alias_target =
  | Type_alias of Stype.t
  | Trait_alias of { trait : Type_path.t }

include struct
  let _ = fun (_ : alias_target) -> ()

  let sexp_of_alias_target =
    (function
     | Type_alias arg0__001_ ->
         let res0__002_ = Stype.sexp_of_t arg0__001_ in
         S.List [ S.Atom "Type_alias"; res0__002_ ]
     | Trait_alias { trait = trait__004_ } ->
         let bnds__003_ = ([] : _ Stdlib.List.t) in
         let bnds__003_ =
           let arg__005_ = Type_path.sexp_of_t trait__004_ in
           (S.List [ S.Atom "trait"; arg__005_ ] :: bnds__003_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Trait_alias" :: bnds__003_)
      : alias_target -> S.t)

  let _ = sexp_of_alias_target
end

type t = {
  name : string;
  arity : int;
  ty_params : Tvar_env.t;
  target : alias_target;
  is_pub : bool;
  doc_ : Docstring.t;
  loc_ : Loc.t; [@sexp_drop_if fun _ -> not !Basic_config.show_loc]
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (let (drop_if__020_ : Loc.t -> Stdlib.Bool.t) =
      fun _ -> not !Basic_config.show_loc
     in
     fun {
           name = name__007_;
           arity = arity__009_;
           ty_params = ty_params__011_;
           target = target__013_;
           is_pub = is_pub__015_;
           doc_ = doc___017_;
           loc_ = loc___021_;
         } ->
       let bnds__006_ = ([] : _ Stdlib.List.t) in
       let bnds__006_ =
         if drop_if__020_ loc___021_ then bnds__006_
         else
           let arg__023_ = Loc.sexp_of_t loc___021_ in
           let bnd__022_ = S.List [ S.Atom "loc_"; arg__023_ ] in
           (bnd__022_ :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__018_ = Docstring.sexp_of_t doc___017_ in
         (S.List [ S.Atom "doc_"; arg__018_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__016_ = Moon_sexp_conv.sexp_of_bool is_pub__015_ in
         (S.List [ S.Atom "is_pub"; arg__016_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__014_ = sexp_of_alias_target target__013_ in
         (S.List [ S.Atom "target"; arg__014_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__012_ = Tvar_env.sexp_of_t ty_params__011_ in
         (S.List [ S.Atom "ty_params"; arg__012_ ] :: bnds__006_
           : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__010_ = Moon_sexp_conv.sexp_of_int arity__009_ in
         (S.List [ S.Atom "arity"; arg__010_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       let bnds__006_ =
         let arg__008_ = Moon_sexp_conv.sexp_of_string name__007_ in
         (S.List [ S.Atom "name"; arg__008_ ] :: bnds__006_ : _ Stdlib.List.t)
       in
       S.List bnds__006_
      : t -> S.t)

  let _ = sexp_of_t
end
