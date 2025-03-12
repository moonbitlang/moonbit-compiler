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


module Qual_ident = Basic_qual_ident
module Type_path = Basic_type_path
module Syntax = Parsing_syntax

type typed_fn_annotation = {
  params_ty : (Stype.t * Typedtree.typ option) list;
  ret_ty : Stype.t;
  err_ty : Stype.t option;
  is_async : bool;
  ret_annotation : Typeutil.ret_annotation;
}

type value =
  | Wl_top_expr of {
      expr : Syntax.expr;
      is_main : bool;
      id : Qual_ident.t;
      loc_ : Loc.t;
      local_types : Syntax.local_type_decl list;
    }
  | Wl_top_letdef of {
      binder : Syntax.binder;
      expr : Syntax.expr;
      is_pub : bool;
      loc_ : Loc.t;
      doc_ : Docstring.t;
      attrs : Checked_attributes.t;
      konstraint : Typedtree.typ option;
      id : Qual_ident.t;
      typ : Stype.t;
    }
  | Wl_top_funcdef of {
      fun_binder : Syntax.binder;
      decl_params : Syntax.parameter list;
      params_loc : Rloc.t;
      is_pub : bool;
      doc : Docstring.t;
      attrs : Checked_attributes.t;
      decl_body : Syntax.decl_body;
      loc_ : Loc.t;
      id : Qual_ident.t;
      kind : Typedtree.fun_decl_kind;
      arity : Fn_arity.t;
      tvar_env : Tvar_env.t;
      constraint_names : Typedtree.type_name list;
      typed_fn_annotation : typed_fn_annotation;
    }
  | Wl_derive of {
      ty_decl : Typedecl_info.t;
      syn_decl : Syntax.type_decl;
      directive : Syntax.deriving_directive;
      trait_path : Type_path.t;
      loc_ : Loc.t;
    }

type const_decl = {
  binder : Syntax.binder;
  expr : Syntax.expr;
  is_pub : bool;
  loc_ : Loc.t;
  doc_ : Docstring.t;
  attrs : Checked_attributes.t;
  konstraint : Typedtree.typ option;
  id : Qual_ident.t;
  typ : Stype.t;
}

type t = {
  global_env : Global_env.t;
  values : value Basic_vec.t;
  const_decls : Typedtree.impl list;
  type_decls : Typedtree.type_decl list;
  trait_decls : Typedtree.trait_decl list;
  trait_alias : Typedtree.trait_alias_decl list;
}
