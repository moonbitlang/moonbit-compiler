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
module Lst = Basic_lst
module Vec = Basic_vec

let ghost_loc_ = Rloc.no_location

let typ (ty_name : string) (tys : Syntax.typ list) =
  (Ptype_name
     {
       constr_id = { lid = Lident ty_name; loc_ = ghost_loc_ };
       tys;
       loc_ = ghost_loc_;
     }
    : Syntax.typ)

let typ_any : Syntax.typ = Ptype_any { loc_ = ghost_loc_ }

let extract_option_inner (ty : Syntax.typ) =
  match ty with
  | Ptype_option { ty; _ } -> Some ty
  | Ptype_name { constr_id = { lid = Lident "Option"; _ }; tys = ty :: []; _ }
    ->
      Some ty
  | _ -> None

let type_is_option (ty : Syntax.typ) =
  match ty with
  | Ptype_option _ | Ptype_name { constr_id = { lid = Lident "Option"; _ }; _ }
    ->
      true
  | _ -> false

let label label_name = ({ label_name; loc_ = ghost_loc_ } : Syntax.label)

let type_name name =
  ({ name; is_object = false; loc_ = ghost_loc_ } : Syntax.type_name)

let hole : Syntax.expr = Pexpr_hole { loc_ = ghost_loc_; kind = Synthesized }

let const_uint_pat i =
  (Ppat_constant { c = Const_uint i; loc_ = ghost_loc_ } : Syntax.pattern)

let const_uint i =
  (Pexpr_constant { c = Const_uint i; loc_ = ghost_loc_ } : Syntax.expr)

let const_int i =
  (Pexpr_constant { c = Const_int i; loc_ = ghost_loc_ } : Syntax.expr)

let const_bool b =
  (Pexpr_constant { c = Const_bool b; loc_ = ghost_loc_ } : Syntax.expr)

let const_string s =
  (Pexpr_constant
     { c = Const_string { string_val = s; string_repr = s }; loc_ = ghost_loc_ }
    : Syntax.expr)

let method_ type_ method_name =
  (Pexpr_method
     {
       type_name = type_name type_;
       method_name = label method_name;
       loc_ = ghost_loc_;
     }
    : Syntax.expr)

let apply func args attr =
  (let args =
     Lst.map args (fun arg ->
         ({ arg_value = arg; arg_kind = Positional } : Syntax.argument))
   in
   Pexpr_apply { func; args; attr; loc_ = ghost_loc_ }
    : Syntax.expr)

let annotation ?(loc_ = ghost_loc_) expr ty =
  (Pexpr_constraint { expr; ty; loc_ } : Syntax.expr)

let apply_trait_method ~assertions ~typ ~loc ~msg trait method_name args attr =
  (let assertion : Syntax.static_assertion =
     {
       assert_type = typ;
       assert_trait = trait;
       assert_loc = loc;
       assert_msg = msg;
     }
   in
   Vec.push assertions assertion;
   apply (method_ trait method_name) args attr
    : Syntax.expr)

let assign var expr =
  Syntax.Pexpr_assign
    {
      var = { var_name = Basic_longident.Lident var; loc_ = ghost_loc_ };
      expr;
      loc_ = ghost_loc_;
      augmented_by = None;
    }

let dot_apply self name args =
  (let args =
     Lst.map args (fun arg ->
         ({ arg_value = arg; arg_kind = Positional } : Syntax.argument))
   in
   let method_name : Syntax.label = { label_name = name; loc_ = ghost_loc_ } in
   Pexpr_dot_apply
     {
       self;
       method_name;
       args;
       attr = No_attr;
       loc_ = ghost_loc_;
       return_self = false;
     }
    : Syntax.expr)

let dotdot_apply self name args =
  (let args =
     Lst.map args (fun arg ->
         ({ arg_value = arg; arg_kind = Positional } : Syntax.argument))
   in
   let method_name : Syntax.label = { label_name = name; loc_ = ghost_loc_ } in
   Pexpr_dot_apply
     {
       self;
       method_name;
       args;
       attr = No_attr;
       loc_ = ghost_loc_;
       return_self = true;
     }
    : Syntax.expr)

let constr ?(extra_info = Syntax.No_extra_info) name args =
  (let constr : Syntax.expr =
     Pexpr_constr
       {
         constr =
           {
             extra_info;
             constr_name = { name; loc_ = ghost_loc_ };
             loc_ = ghost_loc_;
           };
         loc_ = ghost_loc_;
       }
   in
   match args with [] -> constr | args -> apply constr args No_attr
    : Syntax.expr)

let apply_label func args =
  (let args : Syntax.argument list =
     Lst.map args (fun (arg : Syntax.expr * Syntax.label option) ->
         (let kind : Syntax.argument_kind =
            match snd arg with
            | None -> Syntax.Positional
            | Some label -> Labelled label
          in
          { arg_value = fst arg; arg_kind = kind }
           : Syntax.argument))
   in
   Pexpr_apply { func; args; attr = No_attr; loc_ = ghost_loc_ }
    : Syntax.expr)

let constr_label name args =
  (let constr : Syntax.expr =
     Pexpr_constr
       {
         constr =
           {
             extra_info = No_extra_info;
             constr_name = { name; loc_ = ghost_loc_ };
             loc_ = ghost_loc_;
           };
         loc_ = ghost_loc_;
       }
   in
   match args with [] -> constr | args -> apply_label constr args
    : Syntax.expr)

let record fields =
  (Pexpr_record
     {
       type_name = None;
       trailing = Trailing_none;
       loc_ = ghost_loc_;
       fields =
         Lst.map fields (fun (label_name, value) ->
             (Field_def
                {
                  label = label label_name;
                  expr = value;
                  is_pun = false;
                  loc_ = ghost_loc_;
                }
               : Syntax.field_def));
     }
    : Syntax.expr)

let field record label_name =
  (Pexpr_field
     {
       record;
       accessor = Label { label_name; loc_ = ghost_loc_ };
       loc_ = ghost_loc_;
     }
    : Syntax.expr)

let newtype_field nt =
  (Pexpr_field { record = nt; accessor = Newtype; loc_ = ghost_loc_ }
    : Syntax.expr)

let tuple exprs = (Pexpr_tuple { exprs; loc_ = ghost_loc_ } : Syntax.expr)
let unit : Syntax.expr = Pexpr_unit { loc_ = ghost_loc_; faked = false }

let infix_op op lhs rhs =
  (Pexpr_infix
     {
       op = { var_name = Lident op; loc_ = ghost_loc_ };
       lhs;
       rhs;
       loc_ = ghost_loc_;
     }
    : Syntax.expr)

let array exprs = (Pexpr_array { exprs; loc_ = ghost_loc_ } : Syntax.expr)

let map_string_expr entries =
  (let elems =
     Lst.map entries (fun (k, v) ->
         (Map_expr_elem
            {
              key = Const_string { string_val = k; string_repr = k };
              expr = v;
              key_loc_ = ghost_loc_;
              loc_ = ghost_loc_;
            }
           : Syntax.map_expr_elem))
   in
   Pexpr_map { elems; loc_ = ghost_loc_ }
    : Syntax.expr)

let pany : Syntax.pattern = Ppat_any { loc_ = ghost_loc_ }

let pvar name =
  (Ppat_var { binder_name = name; loc_ = ghost_loc_ } : Syntax.pattern)

let array_pat p = (Pattern p : Syntax.array_pattern)
let ptuple pats = (Ppat_tuple { pats; loc_ = ghost_loc_ } : Syntax.pattern)

let pconstr constr args =
  (let args =
     match args with
     | [] -> None
     | args ->
         Some
           (Lst.map args (fun (label_opt, pat) ->
                (match label_opt with
                 | None -> Constr_pat_arg { pat; kind = Positional }
                 | Some label_name ->
                     Constr_pat_arg
                       {
                         pat;
                         kind = Labelled { label_name; loc_ = ghost_loc_ };
                       }
                  : Syntax.constr_pat_arg)))
   in
   Ppat_constr
     {
       constr =
         {
           extra_info = No_extra_info;
           constr_name = { name = constr; loc_ = ghost_loc_ };
           loc_ = ghost_loc_;
         };
       args;
       is_open = false;
       loc_ = ghost_loc_;
     }
    : Syntax.pattern)

let pconstant c = (Ppat_constant { c; loc_ = ghost_loc_ } : Syntax.pattern)

let por pats =
  (match pats with
   | [] -> assert false
   | pat :: [] -> pat
   | pat :: pats ->
       Lst.fold_left pats pat (fun acc ->
           fun pat -> Ppat_or { pat1 = acc; pat2 = pat; loc_ = ghost_loc_ })
    : Syntax.pattern)

let var name =
  (Pexpr_ident
     { id = { var_name = Lident name; loc_ = ghost_loc_ }; loc_ = ghost_loc_ }
    : Syntax.expr)

let let_ pattern expr body =
  (Pexpr_let { pattern; expr; body; loc_ = ghost_loc_ } : Syntax.expr)

let letmut name ty expr body =
  (Pexpr_letmut
     {
       binder = { binder_name = name; loc_ = ghost_loc_ };
       ty;
       expr;
       body;
       loc_ = ghost_loc_;
     }
    : Syntax.expr)

let sequence expr1 expr2 =
  (match expr2 with
   | Syntax.Pexpr_sequence { exprs; last_expr; _ } ->
       Pexpr_sequence { exprs = expr1 :: exprs; last_expr; loc_ = ghost_loc_ }
   | _ ->
       Pexpr_sequence
         { exprs = [ expr1 ]; last_expr = expr2; loc_ = ghost_loc_ }
    : Syntax.expr)

let rec seq exprs =
  (match exprs with
   | [] -> unit
   | expr :: [] -> expr
   | expr :: exprs -> sequence expr (seq exprs)
    : Syntax.expr)

let if_ cond ifso ifnot =
  (Pexpr_if { cond; ifso; ifnot = Some ifnot; loc_ = ghost_loc_ } : Syntax.expr)

let match_ expr (cases : (Syntax.pattern * Syntax.expr) list) =
  (let cases =
     Lst.map cases (fun (pattern, expr) ->
         ({ pattern; guard = None; body = expr } : Syntax.case))
   in
   Pexpr_match { expr; cases; match_loc_ = ghost_loc_; loc_ = ghost_loc_ }
    : Syntax.expr)

let rest_raise_json_decode_error_ (path : Syntax.expr) (msg : Syntax.expr) =
  (let err_value =
     let t = tuple [ path; msg ] in
     constr "JsonDecodeError" [ t ] ~extra_info:(Syntax.Package "json")
   in
   (pvar "_", Pexpr_raise { err_value; loc_ = ghost_loc_ })
    : Syntax.pattern * Syntax.expr)

let raise_json_decode_error (path : Syntax.expr) (msg : Syntax.expr) =
  (let _, raise_expr = rest_raise_json_decode_error_ path msg in
   raise_expr
    : Syntax.expr)

let json_path_add_key path field_name =
  (apply
     (method_ (Ldot { pkg = "json"; id = "JsonPath" }) "add_key")
     [ path; const_string field_name ]
     No_attr
    : Syntax.expr)

let json_path_add_key_expr path expr =
  (apply
     (method_ (Ldot { pkg = "json"; id = "JsonPath" }) "add_key")
     [ path; expr ] No_attr
    : Syntax.expr)

let json_path_add_index path index =
  (apply
     (method_ (Ldot { pkg = "json"; id = "JsonPath" }) "add_index")
     [ path; const_int (string_of_int index) ]
     No_attr
    : Syntax.expr)

let ident long_ident =
  (Pexpr_ident
     { id = { var_name = long_ident; loc_ = ghost_loc_ }; loc_ = ghost_loc_ }
    : Syntax.expr)

let map (elems : Syntax.map_expr_elem list) =
  (Pexpr_map { elems; loc_ = ghost_loc_ } : Syntax.expr)

let map_elem key expr =
  (Map_expr_elem { key; expr; key_loc_ = ghost_loc_; loc_ = ghost_loc_ }
    : Syntax.map_expr_elem)

let panic : Syntax.expr = apply (var "panic") [] No_attr
