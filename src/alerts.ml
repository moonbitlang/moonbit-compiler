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


type alert_id = string

include struct
  let _ = fun (_ : alert_id) -> ()
  let sexp_of_alert_id = (Moon_sexp_conv.sexp_of_string : alert_id -> S.t)
  let _ = sexp_of_alert_id

  let compare_alert_id =
    (fun a__001_ -> fun b__002_ -> Stdlib.compare (a__001_ : string) b__002_
      : alert_id -> alert_id -> int)

  let _ = compare_alert_id

  let equal_alert_id =
    (fun a__003_ -> fun b__004_ -> Stdlib.( = ) (a__003_ : string) b__004_
      : alert_id -> alert_id -> bool)

  let _ = equal_alert_id
end

type alert_state = Warning | Error | Disabled

include struct
  let _ = fun (_ : alert_state) -> ()

  let sexp_of_alert_state =
    (function
     | Warning -> S.Atom "Warning"
     | Error -> S.Atom "Error"
     | Disabled -> S.Atom "Disabled"
      : alert_state -> S.t)

  let _ = sexp_of_alert_state
  let compare_alert_state = (Stdlib.compare : alert_state -> alert_state -> int)
  let _ = compare_alert_state
  let equal_alert_state = (Stdlib.( = ) : alert_state -> alert_state -> bool)
  let _ = equal_alert_state
end

type t = { loc : Loc.t; category : alert_id; message : string }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { loc = loc__010_; category = category__012_; message = message__014_ }
     ->
       let bnds__009_ = ([] : _ Stdlib.List.t) in
       let bnds__009_ =
         let arg__015_ = Moon_sexp_conv.sexp_of_string message__014_ in
         (S.List [ S.Atom "message"; arg__015_ ] :: bnds__009_
           : _ Stdlib.List.t)
       in
       let bnds__009_ =
         let arg__013_ = sexp_of_alert_id category__012_ in
         (S.List [ S.Atom "category"; arg__013_ ] :: bnds__009_
           : _ Stdlib.List.t)
       in
       let bnds__009_ =
         let arg__011_ = Loc.sexp_of_t loc__010_ in
         (S.List [ S.Atom "loc"; arg__011_ ] :: bnds__009_ : _ Stdlib.List.t)
       in
       S.List bnds__009_
      : t -> S.t)

  let _ = sexp_of_t

  let compare =
    (fun a__016_ ->
       fun b__017_ ->
        if Stdlib.( == ) a__016_ b__017_ then 0
        else
          match Loc.compare a__016_.loc b__017_.loc with
          | 0 -> (
              match compare_alert_id a__016_.category b__017_.category with
              | 0 -> Stdlib.compare (a__016_.message : string) b__017_.message
              | n -> n)
          | n -> n
      : t -> t -> int)

  let _ = compare

  let equal =
    (fun a__018_ ->
       fun b__019_ ->
        if Stdlib.( == ) a__018_ b__019_ then true
        else
          Stdlib.( && )
            (Loc.equal a__018_.loc b__019_.loc)
            (Stdlib.( && )
               (equal_alert_id a__018_.category b__019_.category)
               (Stdlib.( = ) (a__018_.message : string) b__019_.message))
      : t -> t -> bool)

  let _ = equal
end

let message ~as_error t =
  (let padding len m =
     String.make (Int.max 0 (len - String.length m)) '0' ^ m
   in
   let id = t.category in
   let leading =
     if as_error then "Error (Alert " ^ padding 3 id ^ "): "
     else "Warning (Alert " ^ padding 3 id ^ "): "
   in
   leading ^ t.message
    : string)

let alerts = Hashtbl.create 10
let post_process = ref None

let is_active x =
  match Hashtbl.find_opt alerts x.category with
  | Some (Warning | Error) -> true
  | _ -> false

let is_error x =
  match Hashtbl.find_opt alerts x.category with
  | Some Error -> true
  | _ -> false

let register_alert id =
  if not (Hashtbl.mem alerts id) then (
    Hashtbl.add alerts id Warning;
    match !post_process with Some f -> f id | None -> ())

let parse_options s =
  (let enable id =
     match Hashtbl.find_opt alerts id with
     | Some Disabled | None -> Hashtbl.replace alerts id Warning
     | _ -> ()
   in
   let disable id = Hashtbl.replace alerts id Disabled in
   let enable_as_error id = Hashtbl.replace alerts id Error in
   let error msg = raise (Arg.Bad ("Ill-formed list of alerts: " ^ msg)) in
   let unknown_token c = error ("Unexpected token '" ^ String.make 1 c ^ "'") in
   let rec ident i acc =
     if i < String.length s then
       let c = s.[i] in
       match c with
       | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
           ident (i + 1) (acc ^ String.make 1 c)
       | _ -> (i, acc)
     else (i, acc)
   in
   let rec loop i =
     if i < String.length s then (
       let f =
         match s.[i] with
         | '+' -> enable
         | '-' -> disable
         | '@' -> enable_as_error
         | _ -> unknown_token s.[i]
       in
       let i, id = ident (i + 1) "" in
       (match id with
       | "all" | "All" ->
           Hashtbl.iter (fun k -> fun _ -> f k) alerts;
           post_process := Some f
       | _ -> f id);
       loop i)
   in
   loop 0
    : unit)

let default_alerts = "+all-raise-throw-unsafe+deprecated"
let () = parse_options default_alerts
