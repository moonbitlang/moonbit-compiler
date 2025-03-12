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


type rpos = int

include struct
  let _ = fun (_ : rpos) -> ()
  let sexp_of_rpos = (Moon_sexp_conv.sexp_of_int : rpos -> S.t)
  let _ = sexp_of_rpos

  let compare_rpos =
    (fun a__001_ -> fun b__002_ -> Stdlib.compare (a__001_ : int) b__002_
      : rpos -> rpos -> int)

  let _ = compare_rpos

  let equal_rpos =
    (fun a__003_ -> fun b__004_ -> Stdlib.( = ) (a__003_ : int) b__004_
      : rpos -> rpos -> bool)

  let _ = equal_rpos
end

let get_line rpos = if rpos = -1 then 0 else (rpos lsr 16) land 0xFFFF
let get_col rpos = if rpos = -1 then 0 else (rpos land 0xFFFF) + 1

type t = { start : rpos; _end : rpos }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { start = start__006_; _end = _end__008_ } ->
       let bnds__005_ = ([] : _ Stdlib.List.t) in
       let bnds__005_ =
         let arg__009_ = sexp_of_rpos _end__008_ in
         (S.List [ S.Atom "_end"; arg__009_ ] :: bnds__005_ : _ Stdlib.List.t)
       in
       let bnds__005_ =
         let arg__007_ = sexp_of_rpos start__006_ in
         (S.List [ S.Atom "start"; arg__007_ ] :: bnds__005_ : _ Stdlib.List.t)
       in
       S.List bnds__005_
      : t -> S.t)

  let _ = sexp_of_t

  let compare =
    (fun a__010_ ->
       fun b__011_ ->
        if Stdlib.( == ) a__010_ b__011_ then 0
        else
          match compare_rpos a__010_.start b__011_.start with
          | 0 -> compare_rpos a__010_._end b__011_._end
          | n -> n
      : t -> t -> int)

  let _ = compare

  let equal =
    (fun a__012_ ->
       fun b__013_ ->
        if Stdlib.( == ) a__012_ b__013_ then true
        else
          Stdlib.( && )
            (equal_rpos a__012_.start b__013_.start)
            (equal_rpos a__012_._end b__013_._end)
      : t -> t -> bool)

  let _ = equal
end

let hide_loc _ = not !Basic_config.show_loc

type 'a loced = { v : 'a; loc_ : t [@sexp_drop_if hide_loc] }

include struct
  let _ = fun (_ : 'a loced) -> ()

  let sexp_of_loced : 'a. ('a -> S.t) -> 'a loced -> S.t =
    let (drop_if__019_ : t -> Stdlib.Bool.t) = hide_loc in
    fun _of_a__014_ ->
      fun { v = v__016_; loc_ = loc___020_ } ->
       let bnds__015_ = ([] : _ Stdlib.List.t) in
       let bnds__015_ =
         if drop_if__019_ loc___020_ then bnds__015_
         else
           let arg__022_ = sexp_of_t loc___020_ in
           let bnd__021_ = S.List [ S.Atom "loc_"; arg__022_ ] in
           (bnd__021_ :: bnds__015_ : _ Stdlib.List.t)
       in
       let bnds__015_ =
         let arg__017_ = _of_a__014_ v__016_ in
         (S.List [ S.Atom "v"; arg__017_ ] :: bnds__015_ : _ Stdlib.List.t)
       in
       S.List bnds__015_

  let _ = sexp_of_loced
end

let lex_pos_to_pos ~(base : Lexing.position) (pos : Lexing.position) =
  (let line = pos.pos_lnum - base.pos_lnum in
   let col = pos.pos_cnum - pos.pos_bol in
   (line lsl 16) lor col
    : rpos)

let pos_to_lex_pos ~(base : Lexing.position) (rpos : rpos) =
  (let col = rpos land 0xFFFF in
   let line = (rpos lsr 16) land 0xFFFF in
   {
     pos_fname = base.pos_fname;
     pos_lnum = base.pos_lnum + line;
     pos_bol = 0;
     pos_cnum = col;
   }
    : Lexing.position)

let no_location = { start = -1; _end = -1 }

let is_no_location l =
  Basic_prelude.phys_equal l no_location || (l.start = -1 && l._end = -1)

let of_loc ~(base : Lexing.position) (loc : Loc.t) =
  if Loc.is_no_location loc then no_location
  else
    let start = lex_pos_to_pos ~base (Loc.get_start loc) in
    let _end = lex_pos_to_pos ~base (Loc.get_end loc) in
    { start; _end }

let to_loc_base_pos ~(base : Lexing.position) (rloc : t) =
  if is_no_location rloc || base.pos_cnum = -1 then Loc.no_location
  else
    Loc.of_menhir
      (pos_to_lex_pos ~base rloc.start, pos_to_lex_pos ~base rloc._end)

let to_loc ~(base : Loc.t) (rloc : t) =
  if is_no_location rloc || Loc.is_no_location base then Loc.no_location
  else
    let pkg = Loc.package base in
    let base = Loc.get_start base in
    Basic_ref.protect Basic_config.current_package pkg (fun () ->
        Loc.of_menhir
          (pos_to_lex_pos ~base rloc.start, pos_to_lex_pos ~base rloc._end))

let merge (l1 : t) (l2 : t) =
  (let { start = s1; _end = e1 } = l1 in
   let { start = s2; _end = e2 } = l2 in
   let start = if compare_rpos s1 s2 < 0 then s1 else s2 in
   let _end = if compare_rpos e1 e2 > 0 then e1 else e2 in
   { start; _end }
    : t)

let of_menhir ~base ((start, _end) : Lexing.position * Lexing.position) =
  of_loc ~base (Loc.of_menhir (start, _end))

let of_lex_pos ~base start _end =
  let start = lex_pos_to_pos ~base start in
  let _end = lex_pos_to_pos ~base _end in
  { start; _end }

let of_pos (start, _end) = { start; _end }
let get_start l = l.start
let get_end l = l._end
let shift_col p i = p + i
let line_number ~(base : Lexing.position) l = get_line l.start + base.pos_lnum
let column_number l = get_col l.start

let line_number_end ~(base : Lexing.position) l =
  get_line l._end + base.pos_lnum

let column_number_end l = get_col l._end

let loc_range_string ~(base : Loc.t) l =
  let base_pos = Loc.get_start base in
  let start_line = line_number ~base:base_pos l in
  let end_line = line_number_end ~base:base_pos l in
  let start_col = column_number l in
  let end_col = column_number_end l in
  let path =
    Pkg_path_tbl.resolve_source Loc.pkg_path_tbl ~pkg:(Loc.package base)
      ~file:(Loc.filename base)
  in
  Stdlib.String.concat ""
    [
      path;
      ":";
      Int.to_string start_line;
      ":";
      Int.to_string start_col;
      "-";
      Int.to_string end_line;
      ":";
      Int.to_string end_col;
    ]

let sexp_of_rpos (p : rpos) =
  (let line = get_line p in
   let col = get_col p in
   S.Atom (Int.to_string line ^ ":" ^ Int.to_string col : Stdlib.String.t)
    : S.t)

let sexp_of_t (l : t) =
  (let start_line = get_line l.start in
   let end_line = get_line l._end in
   let start_col = get_col l.start in
   let end_col = get_col l._end in
   S.Atom
     (Stdlib.String.concat ""
        [
          Int.to_string start_line;
          ":";
          Int.to_string start_col;
          "-";
          Int.to_string end_line;
          ":";
          Int.to_string end_col;
        ])
    : S.t)

let trim_first_char loc = { loc with start = loc.start + 1 }
[@@dead "+trim_first_char"]

let trim_last_char loc = { loc with _end = loc._end - 1 }

let only_last_n_char loc n =
  let start' = shift_col loc._end (-n) in
  { loc with start = start' }
