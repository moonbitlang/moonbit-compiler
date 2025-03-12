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


type attr_id = { qual : string option; name : string }

let string_of_id = function
  | { qual = None; name } -> name
  | { qual = Some qual; name } -> qual ^ "." ^ name

let sexp_of_attr_id id = S.Atom (string_of_id id)

type attr_expr =
  | Ident of attr_id
  | String of Lex_literal.string_literal
  | Apply of attr_id * attr_prop list

and attr_prop = Labeled of string * attr_expr | Expr of attr_expr

include struct
  let _ = fun (_ : attr_expr) -> ()
  let _ = fun (_ : attr_prop) -> ()

  let rec sexp_of_attr_expr =
    (function
     | Ident arg0__001_ ->
         let res0__002_ = sexp_of_attr_id arg0__001_ in
         S.List [ S.Atom "Ident"; res0__002_ ]
     | String arg0__003_ ->
         let res0__004_ = Lex_literal.sexp_of_string_literal arg0__003_ in
         S.List [ S.Atom "String"; res0__004_ ]
     | Apply (arg0__005_, arg1__006_) ->
         let res0__007_ = sexp_of_attr_id arg0__005_
         and res1__008_ =
           Moon_sexp_conv.sexp_of_list sexp_of_attr_prop arg1__006_
         in
         S.List [ S.Atom "Apply"; res0__007_; res1__008_ ]
      : attr_expr -> S.t)

  and sexp_of_attr_prop =
    (function
     | Labeled (arg0__009_, arg1__010_) ->
         let res0__011_ = Moon_sexp_conv.sexp_of_string arg0__009_
         and res1__012_ = sexp_of_attr_expr arg1__010_ in
         S.List [ S.Atom "Labeled"; res0__011_; res1__012_ ]
     | Expr arg0__013_ ->
         let res0__014_ = sexp_of_attr_expr arg0__013_ in
         S.List [ S.Atom "Expr"; res0__014_ ]
      : attr_prop -> S.t)

  let _ = sexp_of_attr_expr
  and _ = sexp_of_attr_prop
end

type t = { loc_ : Rloc.t; raw : string; parsed : attr_expr option }

let sexp_of_t { loc_; raw; parsed } =
  let loc_sexp =
    if !Basic_config.show_loc then [ Rloc.sexp_of_t loc_ ] else []
  in
  match parsed with
  | None -> S.List (List.append loc_sexp [ S.Atom raw ])
  | Some expr -> S.List (List.append loc_sexp [ sexp_of_attr_expr expr ])
