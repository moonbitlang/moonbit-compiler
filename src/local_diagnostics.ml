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

type error = { loc : Rloc.t; message : string; error_code : Error_code.t }

include struct
  let _ = fun (_ : error) -> ()

  let sexp_of_error =
    (fun {
           loc = loc__002_;
           message = message__004_;
           error_code = error_code__006_;
         }
     ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__007_ = Error_code.sexp_of_t error_code__006_ in
         (S.List [ S.Atom "error_code"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_string message__004_ in
         (S.List [ S.Atom "message"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Rloc.sexp_of_t loc__002_ in
         (S.List [ S.Atom "loc"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : error -> S.t)

  let _ = sexp_of_error

  let compare_error =
    (fun a__008_ ->
       fun b__009_ ->
        if Stdlib.( == ) a__008_ b__009_ then 0
        else
          match Rloc.compare a__008_.loc b__009_.loc with
          | 0 -> (
              match
                Stdlib.compare (a__008_.message : string) b__009_.message
              with
              | 0 -> Error_code.compare a__008_.error_code b__009_.error_code
              | n -> n)
          | n -> n
      : error -> error -> int)

  let _ = compare_error

  let equal_error =
    (fun a__010_ ->
       fun b__011_ ->
        if Stdlib.( == ) a__010_ b__011_ then true
        else
          Stdlib.( && )
            (Rloc.equal a__010_.loc b__011_.loc)
            (Stdlib.( && )
               (Stdlib.( = ) (a__010_.message : string) b__011_.message)
               (Error_code.equal a__010_.error_code b__011_.error_code))
      : error -> error -> bool)

  let _ = equal_error
end

type warning = { loc : Rloc.t; kind : Warnings.kind }

include struct
  let _ = fun (_ : warning) -> ()

  let sexp_of_warning =
    (fun { loc = loc__013_; kind = kind__015_ } ->
       let bnds__012_ = ([] : _ Stdlib.List.t) in
       let bnds__012_ =
         let arg__016_ = Warnings.sexp_of_kind kind__015_ in
         (S.List [ S.Atom "kind"; arg__016_ ] :: bnds__012_ : _ Stdlib.List.t)
       in
       let bnds__012_ =
         let arg__014_ = Rloc.sexp_of_t loc__013_ in
         (S.List [ S.Atom "loc"; arg__014_ ] :: bnds__012_ : _ Stdlib.List.t)
       in
       S.List bnds__012_
      : warning -> S.t)

  let _ = sexp_of_warning

  let compare_warning =
    (fun a__017_ ->
       fun b__018_ ->
        if Stdlib.( == ) a__017_ b__018_ then 0
        else
          match Rloc.compare a__017_.loc b__018_.loc with
          | 0 -> Warnings.compare_kind a__017_.kind b__018_.kind
          | n -> n
      : warning -> warning -> int)

  let _ = compare_warning

  let equal_warning =
    (fun a__019_ ->
       fun b__020_ ->
        if Stdlib.( == ) a__019_ b__020_ then true
        else
          Stdlib.( && )
            (Rloc.equal a__019_.loc b__020_.loc)
            (Warnings.equal_kind a__019_.kind b__020_.kind)
      : warning -> warning -> bool)

  let _ = equal_warning
end

type alert = { loc : Rloc.t; category : string; message : string }

module Error_set = Basic_setf.Make (struct
  type t = error

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_error : t -> S.t)
    let _ = sexp_of_t
    let compare = (compare_error : t -> t -> int)
    let _ = compare
    let equal = (equal_error : t -> t -> bool)
    let _ = equal
  end
end)

module Warning_set = Basic_setf.Make (struct
  type t = warning

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_warning : t -> S.t)
    let _ = sexp_of_t
    let compare = (compare_warning : t -> t -> int)
    let _ = compare
    let equal = (equal_warning : t -> t -> bool)
    let _ = equal
  end
end)

type t = {
  base : Loc.t;
  mutable errors : Error_set.t;
  mutable alerts : alert list;
  mutable warnings : Warning_set.t;
}

let make ~base =
  { base; errors = Error_set.empty; alerts = []; warnings = Warning_set.empty }

let swallow_error =
  {
    loc = Rloc.no_location;
    message = "";
    error_code = Error_code.swallow_error;
  }

let add_to_global t (diagnostics : Diagnostics.t) =
  Error_set.iter t.errors (fun e ->
      let loc = Rloc.to_loc ~base:t.base e.loc in
      Diagnostics.add_error diagnostics
        { loc; message = e.message; error_code = e.error_code });
  Warning_set.iter t.warnings (fun w ->
      let loc = Rloc.to_loc ~base:t.base w.loc in
      Diagnostics.add_warning diagnostics { loc; kind = w.kind });
  Basic_lst.iter t.alerts ~f:(fun a ->
      let loc = Rloc.to_loc ~base:t.base a.loc in
      Diagnostics.add_alert diagnostics
        { loc; category = a.category; message = a.message })

let add_warning (x : t) (w : warning) =
  x.warnings <- Warning_set.add x.warnings w

let add_error (x : t) (w : error) =
  if not (Basic_prelude.phys_equal w swallow_error) then
    x.errors <- Error_set.add x.errors w

let add_alert (x : t) (a : alert) = x.alerts <- a :: x.alerts
let has_fatal_errors (x : t) = not (Error_set.is_empty x.errors)

let merge (x : t) ~into =
  Error_set.iter x.errors (add_error into);
  Warning_set.iter x.warnings (add_warning into);
  Lst.iter x.alerts ~f:(add_alert into)

type 'a partial_info = Ok of 'a | Partial of 'a * error list

let take_partial_info (x : 'a partial_info) ~(diagnostics : t) =
  (match x with
   | Ok a -> a
   | Partial (a, err) ->
       Basic_lst.iter err ~f:(fun info -> add_error diagnostics info);
       a
    : 'a)

type 'a info = ('a, error) Result.t
