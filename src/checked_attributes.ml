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


module Lst = Basic_lst

type location = Rloc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Rloc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type attribute =
  | Tattr_alert of { loc_ : location; category : string; message : string }
  | Tattr_intrinsic of { loc_ : location; intrinsic : string }

include struct
  let _ = fun (_ : attribute) -> ()

  let sexp_of_attribute =
    (function
     | Tattr_alert
         {
           loc_ = loc___002_;
           category = category__004_;
           message = message__006_;
         } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__007_ = Moon_sexp_conv.sexp_of_string message__006_ in
           (S.List [ S.Atom "message"; arg__007_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_string category__004_ in
           (S.List [ S.Atom "category"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = sexp_of_location loc___002_ in
           (S.List [ S.Atom "loc_"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Tattr_alert" :: bnds__001_)
     | Tattr_intrinsic { loc_ = loc___009_; intrinsic = intrinsic__011_ } ->
         let bnds__008_ = ([] : _ Stdlib.List.t) in
         let bnds__008_ =
           let arg__012_ = Moon_sexp_conv.sexp_of_string intrinsic__011_ in
           (S.List [ S.Atom "intrinsic"; arg__012_ ] :: bnds__008_
             : _ Stdlib.List.t)
         in
         let bnds__008_ =
           let arg__010_ = sexp_of_location loc___009_ in
           (S.List [ S.Atom "loc_"; arg__010_ ] :: bnds__008_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Tattr_intrinsic" :: bnds__008_)
      : attribute -> S.t)

  let _ = sexp_of_attribute
end

type t = attribute list

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun x__013_ -> Moon_sexp_conv.sexp_of_list sexp_of_attribute x__013_
      : t -> S.t)

  let _ = sexp_of_t
end

let check ~local_diagnostics
    ~(context : [ `TopLet | `TopFun | `TopTypeDecl | `Impl | `Trait ])
    (attrs : Attribute.t list) =
  (Lst.fold_left attrs [] (fun acc ->
       fun attr ->
        match attr with
        | { Attribute.loc_; raw = _; parsed = Some expr } -> (
            let warn_unused_attribute name =
              Local_diagnostics.add_warning local_diagnostics
                { loc = loc_; kind = Unused_attribute name }
            in
            match expr with
            | Apply
                ( { qual = None; name = "intrinsic" },
                  Expr (String { string_val = intrinsic; _ }) :: [] ) -> (
                match context with
                | `TopLet | `TopTypeDecl | `Impl | `Trait ->
                    warn_unused_attribute "intrinsic";
                    acc
                | `TopFun -> Tattr_intrinsic { loc_; intrinsic } :: acc)
            | Apply
                ( { qual = None; name = "deprecated" },
                  Expr (String message) :: [] ) -> (
                match context with
                | `TopLet | `TopTypeDecl | `Impl | `Trait ->
                    warn_unused_attribute "deprecated";
                    acc
                | `TopFun ->
                    Tattr_alert
                      {
                        loc_;
                        category = "deprecated";
                        message = message.string_val;
                      }
                    :: acc)
            | Apply
                ( { qual = None; name = "alert" },
                  [
                    Expr (Ident { qual = None; name = category });
                    Expr (String message);
                  ] ) -> (
                match context with
                | `TopLet | `TopTypeDecl | `Impl | `Trait ->
                    warn_unused_attribute "alert";
                    acc
                | `TopFun ->
                    Tattr_alert { loc_; category; message = message.string_val }
                    :: acc)
            | _ -> acc)
        | _ -> acc)
    : t)

let check_alerts ~diagnostics t loc =
  Lst.iter t ~f:(fun pragma ->
      match pragma with
      | Tattr_alert { category; message; loc_ = _ } ->
          Local_diagnostics.add_alert diagnostics { category; message; loc }
      | _ -> ())
