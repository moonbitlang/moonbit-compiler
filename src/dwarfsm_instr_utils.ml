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


module Ast = Dwarfsm_ast

let rec map (instrs : Ast.instr list) (f : Ast.instr -> Ast.instr) =
  match instrs with
  | [] -> []
  | instr :: instrs ->
      (match instr with
      | If { label; typeuse; then_; else_ } ->
          let then_ = map then_ f in
          let else_ = map else_ f in
          f (If { label; typeuse; then_; else_ })
      | Block { label; typeuse; instrs } ->
          let instrs = map instrs f in
          f (Block { label; typeuse; instrs })
      | Loop { label; typeuse; instrs } ->
          let instrs = map instrs f in
          f (Loop { label; typeuse; instrs })
      | Try_table { label; typeuse; catchs; instrs } ->
          let instrs = map instrs f in
          f (Try_table { label; typeuse; catchs; instrs })
      | _ -> f instr)
      :: map instrs f

let rec concat_map (instrs : Ast.instr list) (f : Ast.instr -> Ast.instr list) =
  match instrs with
  | [] -> []
  | instr :: instrs ->
      let res =
        match instr with
        | If { label; typeuse; then_; else_ } ->
            let then_ = concat_map then_ f in
            let else_ = concat_map else_ f in
            f (If { label; typeuse; then_; else_ })
        | Block { label; typeuse; instrs } ->
            let instrs = concat_map instrs f in
            f (Block { label; typeuse; instrs })
        | Loop { label; typeuse; instrs } ->
            let instrs = concat_map instrs f in
            f (Loop { label; typeuse; instrs })
        | Try_table { label; typeuse; catchs; instrs } ->
            let instrs = concat_map instrs f in
            f (Try_table { label; typeuse; catchs; instrs })
        | _ -> f instr
      in
      res @ concat_map instrs f
[@@dead "+concat_map"]

let rec iter (instrs : Ast.instr list) (f : Ast.instr -> unit) =
  (match instrs with
   | [] -> ()
   | instr :: instrs ->
       f instr;
       (match instr with
       | If { then_; else_; _ } ->
           iter then_ f;
           iter else_ f
       | Block { instrs; _ } | Loop { instrs; _ } | Try_table { instrs; _ } ->
           iter instrs f
       | _ -> ());
       iter instrs f
    : unit)
