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


module Parse = Parsing_parse
module Syntax = Parsing_syntax
module Ident = Basic_longident
module Lst = Basic_lst

type tracing_item = { display : string; loc : Loc.t; expr : Syntax.expr }
type env = { mutable tracing_list : tracing_item list; mutable base : Loc.t }

module Ast = Ast_builder

let generate_trace (item : tracing_item) =
  let start_signal = "######MOONBIT_VALUE_TRACING_START######" in
  let end_signal = "######MOONBIT_VALUE_TRACING_END######" in
  let abs_loc = item.loc in
  let line = Loc.line_number abs_loc in
  let start_column = Loc.column_number abs_loc in
  let end_column = Loc.column_number_end abs_loc in
  let make_string_const str =
    Syntax.Const_string { string_val = str; string_repr = str }
  in
  Ast.sequence
    (Ast.apply
       (Ast.ident (Ident.Ldot { pkg = "builtin"; id = "println" }))
       [ Ast.const_string start_signal ]
       No_attr)
    (Ast.sequence
       (Ast.apply
          (Ast.ident (Ident.Ldot { pkg = "builtin"; id = "println" }))
          [
            Ast.dot_apply
              (Ast.annotation
                 (Ast.map
                    [
                      Ast.map_elem (make_string_const "name")
                        (Ast.const_string item.display);
                      Ast.map_elem
                        (make_string_const "value")
                        (Ast.constr "String"
                           [
                             Ast.apply
                               (Ast.ident (Ident.Lident "any_to_string"))
                               [ item.expr ] No_attr;
                           ]);
                      Ast.map_elem (make_string_const "line")
                        (Ast.const_string (string_of_int line));
                      Ast.map_elem
                        (make_string_const "start_column")
                        (Ast.const_string (string_of_int start_column));
                      Ast.map_elem
                        (make_string_const "end_column")
                        (Ast.const_string (string_of_int end_column));
                    ])
                 (Ast.typ "Json" []))
              "stringify" [];
          ]
          No_attr)
       (Ast.apply
          (Ast.ident (Ident.Ldot { pkg = "builtin"; id = "println" }))
          [ Ast.const_string end_signal ]
          No_attr))

let wrap_trace (item : tracing_item) expr =
  let generated_ast = generate_trace item in
  Ast.sequence generated_ast expr

let wrap_traces (items : tracing_item list) expr =
  List.fold_left (fun expr -> fun item -> wrap_trace item expr) expr items

let binder2tracing_item ~base (binder : Syntax.binder) =
  (let id = Ident.Lident binder.binder_name in
   {
     display = binder.binder_name;
     expr = Ast.ident id;
     loc = Rloc.to_loc ~base binder.loc_;
   }
    : tracing_item)

let impl_visitor =
  object (self)
    inherit [_] Syntax.map as super

    method! visit_binder env binder =
      let item = binder2tracing_item ~base:env.base binder in
      env.tracing_list <- item :: env.tracing_list;
      super#visit_binder env binder

    method! visit_Pexpr_let env pattern expr body loc_ =
      env.tracing_list <- [];
      let pattern = self#visit_pattern env pattern in
      let tracing_list = env.tracing_list in
      let expr = self#visit_expr env expr in
      let body = self#visit_expr env body in
      let body = wrap_traces tracing_list body in
      Syntax.Pexpr_let { pattern; expr; body; loc_ }

    method! visit_Pexpr_guard_let env pat expr otherwise body loc_ =
      env.tracing_list <- [];
      let pat = self#visit_pattern env pat in
      let vars = env.tracing_list in
      let expr = self#visit_expr env expr in
      let otherwise =
        match otherwise with
        | Some xs ->
            Some
              (Lst.map xs (fun { pattern = pat; guard; body = expr } ->
                   (env.tracing_list <- [];
                    let pat = self#visit_pattern env pat in
                    let vars = env.tracing_list in
                    let expr = wrap_traces vars (self#visit_expr env expr) in
                    { pattern = pat; guard; body = expr }
                     : Syntax.case)))
        | None -> None
      in
      let body = wrap_traces vars (self#visit_expr env body) in
      Syntax.Pexpr_guard_let { pat; expr; otherwise; body; loc_ }

    method! visit_Pexpr_letmut env binder ty expr body loc_ =
      let expr = self#visit_expr env expr in
      let body =
        wrap_trace
          (binder2tracing_item ~base:env.base binder)
          (self#visit_expr env body)
      in
      Syntax.Pexpr_letmut { binder; ty; expr; body; loc_ }

    method! visit_Pexpr_match env expr cases match_loc_ loc_ =
      let expr = self#visit_expr env expr in
      let cases =
        Lst.map cases (fun { pattern = pat; guard; body = expr } ->
            (env.tracing_list <- [];
             let pat = self#visit_pattern env pat in
             let bindings = env.tracing_list in
             let expr = wrap_traces bindings (self#visit_expr env expr) in
             { pattern = pat; guard; body = expr }
              : Syntax.case))
      in
      Syntax.Pexpr_match { expr; cases; match_loc_; loc_ }

    method! visit_Pexpr_assign env var expr augmented_by loc_ =
      let assign_expr =
        super#visit_Pexpr_assign env var expr augmented_by loc_
      in
      let tracing_item =
        {
          display = Ident.to_string var.var_name;
          expr = Ast.ident var.var_name;
          loc = Rloc.to_loc ~base:env.base var.loc_;
        }
      in
      let trace_expr = generate_trace tracing_item in
      Ast.sequence assign_expr trace_expr

    method! visit_impl env impl =
      env.base <- Syntax.loc_of_impl impl;
      super#visit_impl env impl
  end

let instrument (outputs : Parse.output list) =
  (let env = { base = Loc.no_location; tracing_list = [] } in
   match outputs with
   | [] -> []
   | output :: outputs ->
       let any_to_string_intrinsic =
         Syntax.Ptop_funcdef
           {
             fun_decl =
               {
                 type_name = None;
                 has_error = None;
                 decl_params =
                   Some
                     [
                       Positional
                         {
                           binder =
                             { binder_name = "a"; loc_ = Rloc.no_location };
                           ty =
                             Some
                               (Ptype_name
                                  {
                                    constr_id =
                                      {
                                        lid = Ident.Lident "A";
                                        loc_ = Rloc.no_location;
                                      };
                                    tys = [];
                                    loc_ = Rloc.no_location;
                                  });
                         };
                     ];
                 params_loc_ = Rloc.no_location;
                 quantifiers =
                   [
                     {
                       tvar_name = "A";
                       tvar_constraints = [];
                       loc_ = Rloc.no_location;
                     };
                   ];
                 vis = Vis_default;
                 is_async = false;
                 return_type =
                   Some
                     ( Ptype_name
                         {
                           constr_id =
                             {
                               lid = Ident.Lident "String";
                               loc_ = Rloc.no_location;
                             };
                           tys = [];
                           loc_ = Rloc.no_location;
                         },
                       No_error_typ );
                 name =
                   { binder_name = "any_to_string"; loc_ = Rloc.no_location };
                 doc_ = Docstring.empty;
                 attrs = [];
               };
             loc_ = Loc.no_location;
             decl_body =
               Decl_stubs
                 (Embedded
                    {
                      language = None;
                      code =
                        Code_string
                          {
                            string_val = "%any.to_string";
                            string_repr = "%any.to_string";
                          };
                    });
           }
       in
       let output =
         { output with ast = any_to_string_intrinsic :: output.ast }
       in
       Lst.map (output :: outputs) (fun output ->
           let impls = impl_visitor#visit_impls env output.ast in
           { output with ast = impls })
    : Parse.output list)
