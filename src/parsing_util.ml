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
module Operators = Parsing_operators
module Lst = Basic_lst
module Vec = Basic_vec
module Menhir_token = Lex_menhir_token
module Unicode_lex = Lex_unicode_lex

type constant = Syntax.constant
type label = Syntax.label
type var = Syntax.var
type expr = Syntax.expr
type argument = Syntax.argument
type typ = Syntax.typ
type pattern = Syntax.pattern
type field_def = Syntax.field_def
type field_pat = Syntax.field_pat

let loc_of_expression = Syntax.loc_of_expression

let get_start = Rloc.get_start
and get_end = Rloc.get_end

let i (start, end_) =
  let base = Parsing_menhir_state.get_base_pos () in
  Rloc.of_lex_pos ~base start end_

let enter_next_block () = Parsing_menhir_state.update_state ()
let aloc = Loc.of_menhir
let make_Pexpr_constant ~loc_ c = (Pexpr_constant { c; loc_ } : expr)
let make_Pexpr_ident ~loc_ id = (Pexpr_ident { id; loc_ } : expr)
let make_Pexpr_tuple ~loc_ exprs = (Pexpr_tuple { exprs; loc_ } : expr)

let make_Pexpr_array ~loc_ (elems : Syntax.spreadable_elem list) =
  (let exception To_spread in
   try
     Pexpr_array
       {
         exprs =
           Basic_lst.map elems (function
             | Elem_regular e -> e
             | Elem_spread _ -> raise_notrace To_spread);
         loc_;
       }
   with To_spread -> Pexpr_array_spread { elems; loc_ }
    : expr)

let make_interps c =
  (Lst.map c (fun lit ->
       match lit with
       | Lex_literal.Interp_lit { c; repr; loc_ } ->
           let loc_ =
             Rloc.of_loc ~base:(Parsing_menhir_state.get_base_pos ()) loc_
           in
           Syntax.Interp_lit { str = c; repr; loc_ }
       | Interp_source s -> Syntax.Interp_source s)
    : Syntax.interp_elem list)

let make_Pexpr_interp ~loc_ c =
  (let elems = make_interps c in
   Pexpr_interp { elems; loc_ }
    : expr)

let make_Pexpr_record ~loc_ ~trailing type_name fs =
  (Pexpr_record { type_name; fields = fs; loc_; trailing } : expr)

let make_Ppat_alias ~loc_ (pat, alias) =
  (Ppat_alias { pat; alias; loc_ } : pattern)

let make_Ppat_constant ~loc_ c = (Ppat_constant { c; loc_ } : pattern)
let make_Ppat_tuple ~loc_ pats = (Ppat_tuple { pats; loc_ } : pattern)

let make_Ppat_constr ~loc_ (constr, args, is_open) =
  (Ppat_constr { constr; args; is_open; loc_ } : pattern)

let make_Ptype_tuple ~loc_ tys = (Ptype_tuple { tys; loc_ } : typ)

let make_Ptype_option ~loc_ ~constr_loc ty =
  (Ptype_option { loc_; question_loc = constr_loc; ty } : typ)

let make_field_pat ~loc_ label pattern is_pun =
  (Field_pat { label; pattern; is_pun; loc_ } : field_pat)

let make_uplus ~loc_ name (arg : expr) =
  (match (name, arg) with
   | "+", Pexpr_constant { c = Const_int _; _ } -> arg
   | "+", Pexpr_constant { c = Const_double _; _ } -> arg
   | _ ->
       Pexpr_apply
         {
           loc_;
           func = make_Pexpr_ident ~loc_ { var_name = Lident "~+"; loc_ };
           args = [ { arg_value = arg; arg_kind = Positional } ];
           attr = No_attr;
         }
    : expr)

let make_uminus ~loc_ name (arg : expr) =
  match (name, arg) with
  | "-", Pexpr_constant { c = Const_int x; loc_ = _ } ->
      make_Pexpr_constant ~loc_
        (Const_int
           (if x.[0] = '-' then String.sub x 1 (String.length x - 1)
            else "-" ^ x))
  | "-", Pexpr_constant { c = Const_int64 x; loc_ = _ } ->
      make_Pexpr_constant ~loc_
        (Const_int64
           (if x.[0] = '-' then String.sub x 1 (String.length x - 1)
            else "-" ^ x))
  | "-", Pexpr_constant { c = Const_double x; loc_ = _ } ->
      make_Pexpr_constant ~loc_
        (Const_double
           (if x.[0] = '-' then String.sub x 1 (String.length x - 1)
            else "-" ^ x))
  | _ -> Pexpr_unary { op = { var_name = Lident name; loc_ }; expr = arg; loc_ }

let bracket_loc index =
  let loc_ = loc_of_expression index in
  let start = get_start loc_ in
  let start = Rloc.shift_col start (-1) in
  let _end = get_end loc_ in
  let _end = Rloc.shift_col _end 1 in
  Rloc.of_pos (start, _end)

let desugar_array_get ~loc_ obj index =
  (Pexpr_dot_apply
     {
       self = obj;
       method_name =
         {
           label_name = (Operators.find_exn "_[_]").method_name;
           loc_ = bracket_loc index;
         };
       args = [ { arg_value = index; arg_kind = Positional } ];
       return_self = false;
       attr = No_attr;
       loc_;
     }
    : expr)

let desugar_array_get_slice ~loc_ ~index_loc_ obj start_index end_index =
  (let start_label : label =
     { label_name = "start"; loc_ = Rloc.no_location }
   in
   let end_label : label = { label_name = "end"; loc_ = Rloc.no_location } in
   let end_arg : argument list =
     match end_index with
     | None -> []
     | Some arg_value -> [ { arg_value; arg_kind = Labelled end_label } ]
   in
   let args : argument list =
     match start_index with
     | None -> end_arg
     | Some start_index ->
         { arg_value = start_index; arg_kind = Labelled start_label } :: end_arg
   in
   Pexpr_dot_apply
     {
       self = obj;
       method_name =
         {
           label_name = (Operators.find_exn "_[_:_]").method_name;
           loc_ = index_loc_;
         };
       args;
       return_self = false;
       attr = No_attr;
       loc_;
     }
    : expr)

let desugar_array_set ~loc_ obj index value =
  (Pexpr_dot_apply
     {
       self = obj;
       method_name =
         {
           label_name = (Operators.find_exn "_[_]=_").method_name;
           loc_ = bracket_loc index;
         };
       args =
         [
           { arg_value = index; arg_kind = Positional };
           { arg_value = value; arg_kind = Positional };
         ];
       return_self = false;
       attr = No_attr;
       loc_;
     }
    : expr)

let desugar_array_augmented_set ~loc_ op array index value =
  (let arr_name =
     let i = Basic_uuid.next () in
     ("*array_" ^ Int.to_string i : Stdlib.String.t)
   in
   let arr_loc = loc_of_expression array in
   let arr_var : var = { var_name = Lident arr_name; loc_ = arr_loc } in
   let arr_ident : expr = Pexpr_ident { id = arr_var; loc_ = arr_loc } in
   let idx_name =
     let i = Basic_uuid.next () in
     ("*index_" ^ Int.to_string i : Stdlib.String.t)
   in
   let idx_loc = loc_of_expression index in
   let idx_var : var = { var_name = Lident idx_name; loc_ = idx_loc } in
   let idx_ident : expr = Pexpr_ident { id = idx_var; loc_ = idx_loc } in
   Pexpr_let
     {
       pattern = Ppat_var { binder_name = arr_name; loc_ = Rloc.no_location };
       expr = array;
       body =
         Pexpr_let
           {
             pattern =
               Ppat_var { binder_name = idx_name; loc_ = Rloc.no_location };
             expr = index;
             body =
               desugar_array_set ~loc_ arr_ident idx_ident
                 (Pexpr_infix
                    {
                      op;
                      lhs = desugar_array_get ~loc_ arr_ident idx_ident;
                      rhs = value;
                      loc_;
                    });
             loc_;
           };
       loc_;
     }
    : expr)

let make_assign_opt ~loc_ (lhs : expr) (rhs : expr) =
  (match lhs with
   | Pexpr_ident { id; _ } ->
       Some (Pexpr_assign { var = id; expr = rhs; augmented_by = None; loc_ })
   | Pexpr_field { record; accessor; _ } ->
       Some
         (Pexpr_mutate
            { record; accessor; field = rhs; augmented_by = None; loc_ })
   | Pexpr_array_get { array; index; loc_ } ->
       Some (Pexpr_array_set { array; index; value = rhs; loc_ })
   | _ -> None
    : expr option)

let make_augmented_assign_opt ~loc_ (op : var) (lhs : expr) (rhs : expr) =
  (match lhs with
   | Pexpr_ident { id; _ } ->
       Some
         (Pexpr_assign { var = id; expr = rhs; augmented_by = Some op; loc_ })
   | Pexpr_field { record; accessor; _ } ->
       Some
         (Pexpr_mutate
            { record; accessor; field = rhs; augmented_by = Some op; loc_ })
   | Pexpr_array_get { array; index; loc_ } ->
       Some (Pexpr_array_augmented_set { op; array; index; value = rhs; loc_ })
   | _ -> None
    : expr option)

let make_field_def ~loc_ label expr is_pun =
  (Field_def { label; expr; is_pun; loc_ } : field_def)

let label_to_expr ~loc_ (l : label) =
  make_Pexpr_ident ~loc_ { var_name = Lident l.label_name; loc_ }

let label_to_pat ~loc_ (l : label) =
  (Ppat_var { binder_name = l.label_name; loc_ } : pattern)

let make_int (i : string) =
  (match Basic_strutil.ends_with_then_chop i "L" with
   | Some i -> (
       match Basic_strutil.ends_with_then_chop i "U" with
       | Some u64 -> Const_uint64 u64
       | None -> Const_int64 i)
   | None -> (
       match Basic_strutil.ends_with_then_chop i "U" with
       | Some u32 -> Const_uint u32
       | None -> (
           match Basic_strutil.ends_with_then_chop i "N" with
           | Some bigint -> Const_bigint bigint
           | None -> Const_int i))
    : constant)

let make_float (f : string) = (Const_double f : constant)

let make_attribute ~loc_ str =
  let diagnostics = Diagnostics.make () in
  let start_pos = Lexing.dummy_pos in
  let tokens =
    Unicode_lex.tokens_of_string ~start_pos ~name:start_pos.pos_fname
      ~diagnostics ~comment:false str
  in
  let lexbuf : Lexing.lexbuf = Lexing.from_string "" in
  let next = ref 0 in
  let lexer (_lexbuf : Lexing.lexbuf) =
    (match Vec.get tokens !next with
     | Menhir_token.EOF, _, _ -> Menhir_token.EOF
     | tok, spos, epos ->
         incr next;
         lexbuf.lex_start_p <- spos;
         lexbuf.lex_curr_p <- epos;
         tok
      : Menhir_token.token)
  in
  match Attribute_parser.attribute lexer lexbuf with
  | exception Attribute_parser.Error ->
      let tok, spos, epos =
        let i = if !next = 0 then 0 else !next - 1 in
        Vec.get tokens i
      in
      let expect_string =
        Lex_menhir_token_util.token_kind_to_expect_string
          (Menhir_token.kind_of_token tok)
      in
      Diagnostics.add_warning diagnostics
        {
          loc = Loc.of_menhir (spos, epos);
          kind =
            Invalid_attribute
              (("unexpected " ^ expect_string ^ " here"
                : Stdlib.String.t)
                [@merlin.hide]);
        };
      { Attribute.loc_; raw = str; parsed = None }
  | expr -> { Attribute.loc_; raw = str; parsed = Some expr }
