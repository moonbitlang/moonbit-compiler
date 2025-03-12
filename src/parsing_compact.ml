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


module Syntax = Parsing_syntax

type expr = Syntax.expr
type pattern = Syntax.pattern
type typ = Syntax.typ
type binder = Syntax.binder

type semi_expr_prop =
  | Stmt_expr of { expr : expr }
  | Stmt_let of { pat : pattern; expr : expr; loc : Rloc.t }
  | Stmt_guard of { cond : expr; otherwise : expr option; loc : Rloc.t }
  | Stmt_guard_let of {
      pat : pattern;
      expr : expr;
      otherwise : Syntax.case list option;
      loc : Rloc.t;
    }
  | Stmt_letmut of {
      binder : binder;
      ty_opt : typ option;
      expr : expr;
      loc : Rloc.t;
    }
  | Stmt_func of { binder : binder; func : Syntax.func; loc : Rloc.t }

let rec compact_rev (ls : semi_expr_prop list) (semi_list_loc : Rloc.t) =
  (match ls with
   | Stmt_expr { expr } :: rest -> collect_rev rest expr
   | rest ->
       let loc_ =
         match rest with
         | [] -> semi_list_loc
         | ( Stmt_let { loc; _ }
           | Stmt_guard { loc; _ }
           | Stmt_guard_let { loc; _ }
           | Stmt_letmut { loc; _ }
           | Stmt_func { loc; _ } )
           :: _ ->
             loc
         | Stmt_expr _ :: _ -> assert false
       in
       collect_rev rest (Syntax.Pexpr_unit { loc_; faked = true })
    : expr)

and collect_rev rest cont =
  match rest with
  | [] -> cont
  | x :: xs -> (
      match x with
      | Stmt_func x -> collect_letrec x.loc [ (x.binder, x.func) ] xs cont
      | Stmt_expr _ ->
          collect_sequence (Syntax.loc_of_expression cont) [] rest cont
      | _ ->
          let acc : Syntax.expr =
            match x with
            | Stmt_let { pat; expr; loc } ->
                let loc_ = Rloc.merge loc (Syntax.loc_of_expression cont) in
                Pexpr_let { pattern = pat; expr; loc_; body = cont }
            | Stmt_guard { cond; otherwise; loc } ->
                let loc_ = Rloc.merge loc (Syntax.loc_of_expression cont) in
                Pexpr_guard { cond; otherwise; loc_; body = cont }
            | Stmt_guard_let { pat; expr; otherwise; loc } ->
                let loc_ = Rloc.merge loc (Syntax.loc_of_expression cont) in
                Pexpr_guard_let { pat; expr; otherwise; loc_; body = cont }
            | Stmt_letmut { binder; ty_opt; expr; loc } ->
                let loc_ = Rloc.merge loc (Syntax.loc_of_expression cont) in
                Pexpr_letmut { binder; ty = ty_opt; expr; loc_; body = cont }
            | Stmt_expr _ | Stmt_func _ -> assert false
          in
          collect_rev xs acc)

and collect_sequence loc_ (acc : Syntax.expr list) todo cont =
  match todo with
  | Stmt_expr { expr = expr1 } :: rest ->
      let loc_ = Rloc.merge (Syntax.loc_of_expression expr1) loc_ in
      collect_sequence loc_ (expr1 :: acc) rest cont
  | _ ->
      collect_rev todo (Pexpr_sequence { exprs = acc; last_expr = cont; loc_ })

and collect_letrec firstloc acc todo cont =
  match todo with
  | Stmt_func x :: rest ->
      collect_letrec x.loc ((x.binder, x.func) :: acc) rest cont
  | _ ->
      collect_rev todo
        (match acc with
        | (name, func) :: [] ->
            Pexpr_letfn
              {
                name;
                func;
                body = cont;
                loc_ = Rloc.merge firstloc (Syntax.loc_of_expression cont);
              }
        | _ ->
            Pexpr_letrec
              {
                bindings = acc;
                body = cont;
                loc_ = Rloc.merge firstloc (Syntax.loc_of_expression cont);
              })
