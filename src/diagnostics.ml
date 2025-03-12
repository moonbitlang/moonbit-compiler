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


type warning = Warnings.t
type alert = Alerts.t
type loc = Loc.t
type report = { loc : Loc.t; message : string; error_code : Error_code.t }

include struct
  let _ = fun (_ : report) -> ()

  let sexp_of_report =
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
         let arg__003_ = Loc.sexp_of_t loc__002_ in
         (S.List [ S.Atom "loc"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : report -> S.t)

  let _ = sexp_of_report

  let compare_report =
    (fun a__008_ ->
       fun b__009_ ->
        if Stdlib.( == ) a__008_ b__009_ then 0
        else
          match Loc.compare a__008_.loc b__009_.loc with
          | 0 -> (
              match
                Stdlib.compare (a__008_.message : string) b__009_.message
              with
              | 0 -> Error_code.compare a__008_.error_code b__009_.error_code
              | n -> n)
          | n -> n
      : report -> report -> int)

  let _ = compare_report

  let equal_report =
    (fun a__010_ ->
       fun b__011_ ->
        if Stdlib.( == ) a__010_ b__011_ then true
        else
          Stdlib.( && )
            (Loc.equal a__010_.loc b__011_.loc)
            (Stdlib.( && )
               (Stdlib.( = ) (a__010_.message : string) b__011_.message)
               (Error_code.equal a__010_.error_code b__011_.error_code))
      : report -> report -> bool)

  let _ = equal_report
end

let report_to_json ~is_error report =
  (let level = if is_error then "error" else "warning" in
   `Assoc
     [
       ("$message_type", `String "diagnostic");
       ("level", `String level);
       ("loc", Loc.t_to_json report.loc);
       ("message", `String report.message);
       ("error_code", Error_code_utils.to_json report.error_code);
     ]
    : Json.t)

module Error_set = Basic_setf.Make (struct
  type t = report

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_report : t -> S.t)
    let _ = sexp_of_t
    let compare = (compare_report : t -> t -> int)
    let _ = compare
    let equal = (equal_report : t -> t -> bool)
    let _ = equal
  end
end)

module Warning_set = Basic_setf.Make (struct
  type t = Warnings.t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (Warnings.sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let compare = (Warnings.compare : t -> t -> int)
    let _ = compare
    let equal = (Warnings.equal : t -> t -> bool)
    let _ = equal
  end
end)

module Alert_set = Basic_setf.Make (struct
  type t = Alerts.t

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (Alerts.sexp_of_t : t -> S.t)
    let _ = sexp_of_t
    let compare = (Alerts.compare : t -> t -> int)
    let _ = compare
    let equal = (Alerts.equal : t -> t -> bool)
    let _ = equal
  end
end)

exception Fatal_error

type t = {
  mutable errors : Error_set.t;
  mutable alerts : Alert_set.t;
  mutable warnings : Warning_set.t;
}

let has_fatal_errors (x : t) = not (Error_set.is_empty x.errors)

let make () =
  {
    errors = Error_set.empty;
    alerts = Alert_set.empty;
    warnings = Warning_set.empty;
  }

let add_warning (x : t) (w : warning) =
  x.warnings <- Warning_set.add x.warnings w

let add_error (x : t) (w : report) = x.errors <- Error_set.add x.errors w

let add_alert (x : t) (a : alert) =
  Alerts.register_alert a.category;
  x.alerts <- Alert_set.add x.alerts a

let iter_reports (x : t) (f : is_error:bool -> report -> unit) =
  Alert_set.iter x.alerts (fun a ->
      let is_error = Alerts.is_error a in
      if is_error || Alerts.is_active a then
        f ~is_error
          {
            loc = a.loc;
            message = Alerts.message ~as_error:is_error a;
            error_code = Error_code.alert;
          });
  Warning_set.iter x.warnings (fun w ->
      let is_error = Warnings.is_error w.kind in
      if is_error || Warnings.is_active w.kind then
        f ~is_error
          {
            loc = w.loc;
            message = Warnings.message ~as_error:is_error w.kind;
            error_code = Error_code_utils.warning (Warnings.number w.kind);
          });
  Error_set.iter x.errors (fun report -> f ~is_error:true report)

let render_report ~is_error ({ loc; message; error_code } as report) =
  match !Basic_config.error_format with
  | Basic_config.Human ->
      Printf.sprintf "%s [%s] %s\n" (Loc.loc_range_string loc)
        (Error_code_utils.to_string error_code)
        message
  | Basic_config.Json ->
      Printf.sprintf "%s\n" (Json.to_string (report_to_json ~is_error report))

let emit_report ~is_error report =
  output_string stderr (render_report ~is_error report)

let emit_errors (x : t) =
  iter_reports x (fun ~is_error ->
      fun report -> if is_error then emit_report ~is_error:true report)

let check_diagnostics (x : t) =
  let has_error = ref false in
  iter_reports x (fun ~is_error ->
      fun report ->
       if is_error then has_error := true;
       emit_report ~is_error report);
  if !has_error then raise Fatal_error

let reset (x : t) =
  x.errors <- Error_set.empty;
  x.warnings <- Warning_set.empty

let merge_into (dst : t) (src : t) =
  dst.errors <- Error_set.union dst.errors src.errors;
  dst.warnings <- Warning_set.union dst.warnings src.warnings

let remove_diagnostics_inside_loc (x : t) (loc : Loc.t) =
  let not_in_loc l =
    not
      (Loc.relation_of_loc loc l = Loc.Includes
      || Loc.relation_of_loc loc l = Loc.Equal)
  in
  x.errors <- Error_set.filter x.errors (fun e -> not_in_loc e.loc);
  x.warnings <- Warning_set.filter x.warnings (fun w -> not_in_loc w.loc);
  x.alerts <- Alert_set.filter x.alerts (fun a -> not_in_loc a.loc)
