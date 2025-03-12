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


module Comment = Lex_comment
module Strutil = Basic_strutil
module Lst = Basic_lst

type pragma_prop = Prop_string of string | Prop_atom of string

include struct
  let _ = fun (_ : pragma_prop) -> ()

  let sexp_of_pragma_prop =
    (function
     | Prop_string arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_string arg0__001_ in
         S.List [ S.Atom "Prop_string"; res0__002_ ]
     | Prop_atom arg0__003_ ->
         let res0__004_ = Moon_sexp_conv.sexp_of_string arg0__003_ in
         S.List [ S.Atom "Prop_atom"; res0__004_ ]
      : pragma_prop -> S.t)

  let _ = sexp_of_pragma_prop
end

type pragma =
  | Pragma_alert of { category : string; message : string }
  | Pragma_intrinsic of string
  | Pragma_gen_js of pragma_prop list
  | Pragma_coverage_skip

include struct
  let _ = fun (_ : pragma) -> ()

  let sexp_of_pragma =
    (function
     | Pragma_alert { category = category__006_; message = message__008_ } ->
         let bnds__005_ = ([] : _ Stdlib.List.t) in
         let bnds__005_ =
           let arg__009_ = Moon_sexp_conv.sexp_of_string message__008_ in
           (S.List [ S.Atom "message"; arg__009_ ] :: bnds__005_
             : _ Stdlib.List.t)
         in
         let bnds__005_ =
           let arg__007_ = Moon_sexp_conv.sexp_of_string category__006_ in
           (S.List [ S.Atom "category"; arg__007_ ] :: bnds__005_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Pragma_alert" :: bnds__005_)
     | Pragma_intrinsic arg0__010_ ->
         let res0__011_ = Moon_sexp_conv.sexp_of_string arg0__010_ in
         S.List [ S.Atom "Pragma_intrinsic"; res0__011_ ]
     | Pragma_gen_js arg0__012_ ->
         let res0__013_ =
           Moon_sexp_conv.sexp_of_list sexp_of_pragma_prop arg0__012_
         in
         S.List [ S.Atom "Pragma_gen_js"; res0__013_ ]
     | Pragma_coverage_skip -> S.Atom "Pragma_coverage_skip"
      : pragma -> S.t)

  let _ = sexp_of_pragma
end

type t = { comment : string list; pragmas : pragma list; loc : Loc.t }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { comment = comment__015_; pragmas = pragmas__017_; loc = loc__019_ }
     ->
       let bnds__014_ = ([] : _ Stdlib.List.t) in
       let bnds__014_ =
         let arg__020_ = Loc.sexp_of_t loc__019_ in
         (S.List [ S.Atom "loc"; arg__020_ ] :: bnds__014_ : _ Stdlib.List.t)
       in
       let bnds__014_ =
         let arg__018_ =
           Moon_sexp_conv.sexp_of_list sexp_of_pragma pragmas__017_
         in
         (S.List [ S.Atom "pragmas"; arg__018_ ] :: bnds__014_
           : _ Stdlib.List.t)
       in
       let bnds__014_ =
         let arg__016_ =
           Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
             comment__015_
         in
         (S.List [ S.Atom "comment"; arg__016_ ] :: bnds__014_
           : _ Stdlib.List.t)
       in
       S.List bnds__014_
      : t -> S.t)

  let _ = sexp_of_t
end

exception Parse_error of Loc.t * string

type state = { doc : string; loc : Loc.t; mutable pos : int }

let peek state =
  (if state.pos < String.length state.doc then state.doc.[state.pos] else '\000'
    : char)

let consume state =
  if state.pos < String.length state.doc then (
    let c = state.doc.[state.pos] in
    state.pos <- state.pos + 1;
    c)
  else '\000'

let skip state =
  if state.pos < String.length state.doc then state.pos <- state.pos + 1

let parse_ident s =
  let rec aux acc =
    match peek s with
    | ' ' | '\t' | '\n' | '\000' -> acc
    | _ -> aux (acc ^ String.make 1 (consume s))
  in
  aux ""

let parse_string s =
  let rec aux is_first acc =
    match peek s with
    | '"' ->
        skip s;
        if is_first then aux false acc else acc
    | '\n' | '\000' ->
        raise
          (Parse_error (s.loc, "unexpected end of string in pragma properties"))
    | _ -> aux false (acc ^ String.make 1 (consume s))
  in
  aux true ""

let parse_pragma_props s =
  let rec aux acc =
    match peek s with
    | ' ' | '\t' ->
        skip s;
        aux acc
    | '"' -> aux (Prop_string (parse_string s) :: acc)
    | '\n' | '\000' -> List.rev acc
    | _ -> aux (Prop_atom (parse_ident s) :: acc)
  in
  aux []

let parse_pragma ~loc str =
  let s = { doc = str; loc; pos = 0 } in
  let rec parse () =
    (match peek s with
     | ' ' | '\t' ->
         skip s;
         parse ()
     | '@' -> (
         skip s;
         let pragma_name = parse_ident s in
         let peek_s = peek s in
         match peek_s with
         | ' ' | '\t' | '\000' -> Some (pragma_name, parse_pragma_props s)
         | _ -> None)
     | _ -> None
      : (string * pragma_prop list) option)
  in
  match parse () with
  | Some (id, props) -> (
      match (id, props) with
      | "alert", [ Prop_atom category; Prop_string message ] ->
          Some (Pragma_alert { category; message })
      | "intrinsic", Prop_atom intrinsic :: [] ->
          Some (Pragma_intrinsic intrinsic)
      | "gen_js", props -> Some (Pragma_gen_js props)
      | ("return" | "param"), _ -> None
      | "coverage.skip", _ -> Some Pragma_coverage_skip
      | _ ->
          raise
            (Parse_error
               (s.loc, "unexpected id `" ^ id ^ "' or invalid properties")))
  | None -> None

let empty = { comment = []; pragmas = []; loc = Loc.no_location }
let is_empty = function { comment = []; pragmas = []; _ } -> true | _ -> false
let pragmas t = t.pragmas
let comment_string t = String.concat "\n" t.comment
let loc (t : t) = t.loc

let of_comments ~diagnostics (comments : Comment.with_loc) =
  let comments =
    Lst.map comments (fun (loc, { Comment.content; _ }) ->
        let content = Basic_strutil.drop_while (fun c -> c = '/') content in
        (loc, content))
  in
  let comments =
    match comments with
    | (loc, str) :: xs when String.starts_with ~prefix:"|" str ->
        (loc, String.sub str 1 (String.length str - 1)) :: xs
    | xs -> xs
  in
  let comments_rev = List.rev comments in
  let startp : Lexing.position =
    match comments with
    | [] -> Lexing.dummy_pos
    | (loc, _) :: _ -> Loc.get_start loc
  in
  let endp : Lexing.position =
    match comments_rev with
    | [] -> Lexing.dummy_pos
    | (loc, _) :: _ -> Loc.get_end loc
  in
  let pragmas =
    let rec aux (acc : 'a list) (comments : (Loc.t * string) list) =
      (match comments with
       | [] -> acc
       | (_, line) :: comments when Strutil.trim line = "" -> aux acc comments
       | (loc, line) :: comments -> (
           try
             match parse_pragma ~loc line with
             | None -> acc
             | Some x -> aux (x :: acc) comments
           with Parse_error (loc, message) ->
             Diagnostics.add_warning diagnostics
               { kind = Warnings.Unexpected_pragmas message; loc };
             aux acc comments)
        : pragma list)
    in
    aux [] comments_rev
  in
  {
    comment = Lst.map comments snd;
    pragmas;
    loc = Loc.of_menhir (startp, endp);
  }

let make ~pragmas ~loc docs = { comment = docs; pragmas; loc }

let check_alerts ~diagnostics pragmas loc =
  Basic_lst.iter pragmas ~f:(fun pragma ->
      match pragma with
      | Pragma_alert { category; message } ->
          Local_diagnostics.add_alert diagnostics { category; message; loc }
      | _ -> ())
