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


type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

include struct
  let _ = fun (_ : position) -> ()

  let sexp_of_position =
    (fun {
           pos_fname = pos_fname__002_;
           pos_lnum = pos_lnum__004_;
           pos_bol = pos_bol__006_;
           pos_cnum = pos_cnum__008_;
         }
     ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__009_ = Moon_sexp_conv.sexp_of_int pos_cnum__008_ in
         (S.List [ S.Atom "pos_cnum"; arg__009_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__007_ = Moon_sexp_conv.sexp_of_int pos_bol__006_ in
         (S.List [ S.Atom "pos_bol"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_int pos_lnum__004_ in
         (S.List [ S.Atom "pos_lnum"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = Moon_sexp_conv.sexp_of_string pos_fname__002_ in
         (S.List [ S.Atom "pos_fname"; arg__003_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : position -> S.t)

  let _ = sexp_of_position
end

type t = { pkg : string; start : position; _end : position }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { pkg = pkg__011_; start = start__013_; _end = _end__015_ } ->
       let bnds__010_ = ([] : _ Stdlib.List.t) in
       let bnds__010_ =
         let arg__016_ = sexp_of_position _end__015_ in
         (S.List [ S.Atom "_end"; arg__016_ ] :: bnds__010_ : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__014_ = sexp_of_position start__013_ in
         (S.List [ S.Atom "start"; arg__014_ ] :: bnds__010_ : _ Stdlib.List.t)
       in
       let bnds__010_ =
         let arg__012_ = Moon_sexp_conv.sexp_of_string pkg__011_ in
         (S.List [ S.Atom "pkg"; arg__012_ ] :: bnds__010_ : _ Stdlib.List.t)
       in
       S.List bnds__010_
      : t -> S.t)

  let _ = sexp_of_t
end

let column_of_pos (x : position) = x.pos_cnum - x.pos_bol + 1 [@@inline]
let package loc = loc.pkg
let filename loc = loc.start.pos_fname
let get_start loc = loc.start
let get_end loc = loc._end

let position_to_json (p : position) =
  (`Assoc [ ("line", `Int p.pos_lnum); ("col", `Int (column_of_pos p)) ]
    : Json.t)

let sexp_of_t (l : t) =
  (S.Atom
     (Stdlib.String.concat ""
        [
          Int.to_string l.start.pos_lnum;
          ":";
          Int.to_string (column_of_pos l.start);
          "-";
          Int.to_string l._end.pos_lnum;
          ":";
          Int.to_string (column_of_pos l._end);
        ])
    : S.t)

let hide_loc _ = not !Basic_config.show_loc

type 'a loced = { v : 'a; loc_ : t [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : 'a loced) -> ()

  let sexp_of_loced : 'a. ('a -> S.t) -> 'a loced -> S.t =
    let (drop_if__022_ : t -> Stdlib.Bool.t) = hide_loc in
    fun _of_a__017_ ->
      fun { v = v__019_; loc_ = loc___023_ } ->
       let bnds__018_ = ([] : _ Stdlib.List.t) in
       let bnds__018_ =
         if drop_if__022_ loc___023_ then bnds__018_
         else
           let arg__025_ = sexp_of_t loc___023_ in
           let bnd__024_ = S.List [ S.Atom "loc_"; arg__025_ ] in
           (bnd__024_ :: bnds__018_ : _ Stdlib.List.t)
       in
       let bnds__018_ =
         let arg__020_ = _of_a__017_ v__019_ in
         (S.List [ S.Atom "v"; arg__020_ ] :: bnds__018_ : _ Stdlib.List.t)
       in
       S.List bnds__018_

  let _ = sexp_of_loced
end

let no_location =
  { pkg = ""; start = Lexing.dummy_pos; _end = Lexing.dummy_pos }

let no_location_with_pkg pkg =
  { pkg; start = Lexing.dummy_pos; _end = Lexing.dummy_pos }

let is_no_location loc =
  Basic_prelude.phys_equal loc no_location
  || (loc.pkg = "" && loc.start.pos_cnum = -1 && loc._end.pos_cnum = -1)

let pos_compare (left : position) (right : position) =
  (if Basic_prelude.phys_equal left right then 0
   else if right.pos_lnum <> left.pos_lnum then
     Int.compare left.pos_lnum right.pos_lnum
   else
     Int.compare (left.pos_cnum - left.pos_bol) (right.pos_cnum - right.pos_bol)
    : int)

let merge (l1 : t) (l2 : t) =
  (let { start = s1; _end = e1; pkg = pkg1 } = l1 in
   let { start = s2; _end = e2; pkg = pkg2 } = l2 in
   let start = if pos_compare s1 s2 < 0 then s1 else s2 in
   assert (pkg1 = pkg2);
   let _end = if pos_compare e1 e2 > 0 then e1 else e2 in
   { pkg = pkg1; start; _end }
    : t)

let collapse (loc : t) = ({ loc with _end = loc.start } : t)
let pkg_path_tbl = Pkg_path_tbl.create ()

let t_to_json (l : t) =
  (let path =
     Pkg_path_tbl.resolve_source pkg_path_tbl ~pkg:l.pkg ~file:l.start.pos_fname
   in
   `Assoc
     [
       ("path", `String path);
       ("start", position_to_json l.start);
       ("end", position_to_json l._end);
     ]
    : Json.t)

let to_string (loc : t) =
  let path =
    Pkg_path_tbl.resolve_source pkg_path_tbl ~pkg:loc.pkg
      ~file:loc.start.pos_fname
  in
  Stdlib.String.concat ""
    [
      path;
      ":";
      Int.to_string loc.start.pos_lnum;
      ":";
      Int.to_string (column_of_pos loc.start);
    ]

let loc_range_string (loc : t) =
  (let path =
     Pkg_path_tbl.resolve_source pkg_path_tbl ~pkg:loc.pkg
       ~file:loc.start.pos_fname
   in
   Stdlib.String.concat ""
     [
       path;
       ":";
       Int.to_string loc.start.pos_lnum;
       ":";
       Int.to_string (column_of_pos loc.start);
       "-";
       Int.to_string loc._end.pos_lnum;
       ":";
       Int.to_string (column_of_pos loc._end);
     ]
    : string)

let loc_range_string_no_filename (loc : t) =
  (Stdlib.String.concat ""
     [
       Int.to_string loc.start.pos_lnum;
       ":";
       Int.to_string (column_of_pos loc.start);
       "-";
       Int.to_string loc._end.pos_lnum;
       ":";
       Int.to_string (column_of_pos loc._end);
     ]
    : string)

let line_number l = l.start.pos_lnum
let column_number l = column_of_pos l.start

let of_menhir ((start, _end) : Lexing.position * Lexing.position) =
  { pkg = !Basic_config.current_package; start; _end }

let line_number_end (l : t) = l._end.pos_lnum
let column_number_end (l : t) = column_of_pos l._end

let compare (l1 : t) (l2 : t) =
  match pos_compare l1.start l2.start with
  | 0 -> pos_compare l1._end l2._end
  | c -> c

let equal (x : t) y = compare x y = 0

type loc_relation =
  | Smaller
  | Greater
  | Equal
  | Includes
  | Included
  | Overlap
  | Unrelated

let relation_of_loc (loc1 : t) (loc2 : t) =
  (let { start = s1; _end = e1; _ } = loc1
   and { start = s2; _end = e2; _ } = loc2 in
   if s1.pos_fname <> s2.pos_fname then Unrelated
   else if pos_compare s1 s2 = 0 && pos_compare e1 e2 = 0 then Equal
   else if pos_compare e1 s2 <= 0 then Smaller
   else if pos_compare s1 e2 >= 0 then Greater
   else
     let s1_vs_s2 = pos_compare s1 s2 in
     let e2_vs_e1 = pos_compare e2 e1 in
     match s1_vs_s2 + e2_vs_e1 with
     | 0 -> if s1_vs_s2 = 0 && e2_vs_e1 = 0 then Equal else Overlap
     | ord when ord < 0 -> Includes
     | _ -> Included
    : loc_relation)

let trim_first_char loc =
  let start' = { loc.start with pos_cnum = loc.start.pos_cnum + 1 } in
  { loc with start = start' }
[@@dead "+trim_first_char"]

let only_last_n_char loc n =
  let start' = { loc._end with pos_cnum = loc._end.pos_cnum - n } in
  { loc with start = start' }

let only_first_n_char loc n =
  let _end' = { loc.start with pos_cnum = loc.start.pos_cnum + n } in
  { loc with _end = _end' }

let length loc = loc._end.pos_cnum - loc.start.pos_cnum [@@dead "+length"]
