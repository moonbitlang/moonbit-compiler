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


module Lst = Basic_lst
module Ident = Basic_ident
module Type_path = Basic_type_path
module Syntax = Parsing_syntax

type location = Rloc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Rloc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type absolute_loc = Loc.t

include struct
  let _ = fun (_ : absolute_loc) -> ()
  let sexp_of_absolute_loc = (Loc.sexp_of_t : absolute_loc -> S.t)
  let _ = sexp_of_absolute_loc
end

type stype = Stype.t

include struct
  let _ = fun (_ : stype) -> ()
  let sexp_of_stype = (Stype.sexp_of_t : stype -> S.t)
  let _ = sexp_of_stype
end

type constraints = Tvar_env.type_constraint list

include struct
  let _ = fun (_ : constraints) -> ()

  let sexp_of_constraints =
    (fun x__001_ ->
       Moon_sexp_conv.sexp_of_list Tvar_env.sexp_of_type_constraint x__001_
      : constraints -> S.t)

  let _ = sexp_of_constraints
end

type constant = Constant.t

include struct
  let _ = fun (_ : constant) -> ()
  let sexp_of_constant = (Constant.sexp_of_t : constant -> S.t)
  let _ = sexp_of_constant
end

type tvar_env = Tvar_env.t

include struct
  let _ = fun (_ : tvar_env) -> ()
  let sexp_of_tvar_env = (Tvar_env.sexp_of_t : tvar_env -> S.t)
  let _ = sexp_of_tvar_env
end

type fn_arity = Fn_arity.t

include struct
  let _ = fun (_ : fn_arity) -> ()
  let sexp_of_fn_arity = (Fn_arity.sexp_of_t : fn_arity -> S.t)
  let _ = sexp_of_fn_arity
end

type docstring = Docstring.t

include struct
  let _ = fun (_ : docstring) -> ()
  let sexp_of_docstring = (Docstring.sexp_of_t : docstring -> S.t)
  let _ = sexp_of_docstring
end

type attributes = Checked_attributes.t

include struct
  let _ = fun (_ : attributes) -> ()
  let sexp_of_attributes = (Checked_attributes.sexp_of_t : attributes -> S.t)
  let _ = sexp_of_attributes
end

type ident = Ident.t

include struct
  let _ = fun (_ : ident) -> ()
  let sexp_of_ident = (Ident.sexp_of_t : ident -> S.t)
  let _ = sexp_of_ident
end

type expr_id = Basic_qual_ident.t

include struct
  let _ = fun (_ : expr_id) -> ()
  let sexp_of_expr_id = (Basic_qual_ident.sexp_of_t : expr_id -> S.t)
  let _ = sexp_of_expr_id
end

let hide_loc _ = not !Basic_config.show_loc

type constrid_loc = Syntax.constrid_loc

include struct
  let _ = fun (_ : constrid_loc) -> ()
  let sexp_of_constrid_loc = (Syntax.sexp_of_constrid_loc : constrid_loc -> S.t)
  let _ = sexp_of_constrid_loc
end

type constr_tag = Basic_constr_info.constr_tag

include struct
  let _ = fun (_ : constr_tag) -> ()

  let sexp_of_constr_tag =
    (Basic_constr_info.sexp_of_constr_tag : constr_tag -> S.t)

  let _ = sexp_of_constr_tag
end

type syntax_binder = Syntax.binder

include struct
  let _ = fun (_ : syntax_binder) -> ()
  let sexp_of_syntax_binder = (Syntax.sexp_of_binder : syntax_binder -> S.t)
  let _ = sexp_of_syntax_binder
end

type syntax_hole = Syntax.hole

include struct
  let _ = fun (_ : syntax_hole) -> ()
  let sexp_of_syntax_hole = (Syntax.sexp_of_hole : syntax_hole -> S.t)
  let _ = sexp_of_syntax_hole
end

type field_info = Typedecl_info.field

include struct
  let _ = fun (_ : field_info) -> ()
  let sexp_of_field_info = (Typedecl_info.sexp_of_field : field_info -> S.t)
  let _ = sexp_of_field_info
end

type var = { var_id : Ident.t; loc_ : location }

let sexp_of_var (x : var) =
  let s = Ident.sexp_of_t x.var_id in
  if hide_loc () then s
  else
    let loc = Rloc.sexp_of_t x.loc_ in
    (List (List.cons (s : S.t) ([ loc ] : S.t list)) : S.t)

type binder = { binder_id : Ident.t; loc_ : location }

let sexp_of_binder (x : binder) =
  let s = Ident.sexp_of_t x.binder_id in
  if hide_loc () then s
  else
    let loc = Rloc.sexp_of_t x.loc_ in
    (List (List.cons (s : S.t) ([ loc ] : S.t list)) : S.t)

type loop_label = { label : Label.t; loc_ : location }

include struct
  let _ = fun (_ : loop_label) -> ()

  let sexp_of_loop_label =
    (fun { label = label__003_; loc_ = loc___005_ } ->
       let bnds__002_ = ([] : _ Stdlib.List.t) in
       let bnds__002_ =
         let arg__006_ = sexp_of_location loc___005_ in
         (S.List [ S.Atom "loc_"; arg__006_ ] :: bnds__002_ : _ Stdlib.List.t)
       in
       let bnds__002_ =
         let arg__004_ = Label.sexp_of_t label__003_ in
         (S.List [ S.Atom "label"; arg__004_ ] :: bnds__002_ : _ Stdlib.List.t)
       in
       S.List bnds__002_
      : loop_label -> S.t)

  let _ = sexp_of_loop_label
end

type loop_label_binder = { label : Label.t; loc_ : location }

include struct
  let _ = fun (_ : loop_label_binder) -> ()

  let sexp_of_loop_label_binder =
    (fun { label = label__008_; loc_ = loc___010_ } ->
       let bnds__007_ = ([] : _ Stdlib.List.t) in
       let bnds__007_ =
         let arg__011_ = sexp_of_location loc___010_ in
         (S.List [ S.Atom "loc_"; arg__011_ ] :: bnds__007_ : _ Stdlib.List.t)
       in
       let bnds__007_ =
         let arg__009_ = Label.sexp_of_t label__008_ in
         (S.List [ S.Atom "label"; arg__009_ ] :: bnds__007_ : _ Stdlib.List.t)
       in
       S.List bnds__007_
      : loop_label_binder -> S.t)

  let _ = sexp_of_loop_label_binder
end

type func_stubs = Stub_type.t

include struct
  let _ = fun (_ : func_stubs) -> ()
  let sexp_of_func_stubs = (Stub_type.sexp_of_t : func_stubs -> S.t)
  let _ = sexp_of_func_stubs
end

type apply_kind = Infix | Dot | Dot_return_self | Normal

include struct
  let _ = fun (_ : apply_kind) -> ()

  let sexp_of_apply_kind =
    (function
     | Infix -> S.Atom "Infix"
     | Dot -> S.Atom "Dot"
     | Dot_return_self -> S.Atom "Dot_return_self"
     | Normal -> S.Atom "Normal"
      : apply_kind -> S.t)

  let _ = sexp_of_apply_kind
end

type fn_kind = Syntax.fn_kind

include struct
  let _ = fun (_ : fn_kind) -> ()
  let sexp_of_fn_kind = (Syntax.sexp_of_fn_kind : fn_kind -> S.t)
  let _ = sexp_of_fn_kind
end

type pat_binder = { binder : binder; binder_typ : stype }

include struct
  let _ = fun (_ : pat_binder) -> ()

  let sexp_of_pat_binder =
    (fun { binder = binder__013_; binder_typ = binder_typ__015_ } ->
       let bnds__012_ = ([] : _ Stdlib.List.t) in
       let bnds__012_ =
         let arg__016_ = sexp_of_stype binder_typ__015_ in
         (S.List [ S.Atom "binder_typ"; arg__016_ ] :: bnds__012_
           : _ Stdlib.List.t)
       in
       let bnds__012_ =
         let arg__014_ = sexp_of_binder binder__013_ in
         (S.List [ S.Atom "binder"; arg__014_ ] :: bnds__012_ : _ Stdlib.List.t)
       in
       S.List bnds__012_
      : pat_binder -> S.t)

  let _ = sexp_of_pat_binder
end

type pat_binders = pat_binder list

include struct
  let _ = fun (_ : pat_binders) -> ()

  let sexp_of_pat_binders =
    (fun x__017_ -> Moon_sexp_conv.sexp_of_list sexp_of_pat_binder x__017_
      : pat_binders -> S.t)

  let _ = sexp_of_pat_binders
end

type value_kind =
  | Prim of Primitive.prim
  | Mutable
  | Normal
  | Value_constr of constr_tag

include struct
  let _ = fun (_ : value_kind) -> ()

  let sexp_of_value_kind =
    (function
     | Prim arg0__018_ ->
         let res0__019_ = Primitive.sexp_of_prim arg0__018_ in
         S.List [ S.Atom "Prim"; res0__019_ ]
     | Mutable -> S.Atom "Mutable"
     | Normal -> S.Atom "Normal"
     | Value_constr arg0__020_ ->
         let res0__021_ = sexp_of_constr_tag arg0__020_ in
         S.List [ S.Atom "Value_constr"; res0__021_ ]
      : value_kind -> S.t)

  let _ = sexp_of_value_kind
end

type type_name_kind = Type | Trait

include struct
  let _ = fun (_ : type_name_kind) -> ()

  let sexp_of_type_name_kind =
    (function Type -> S.Atom "Type" | Trait -> S.Atom "Trait"
      : type_name_kind -> S.t)

  let _ = sexp_of_type_name_kind
end

type type_constr_loc = {
  name : Type_path.t;
  kind : type_name_kind;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : type_constr_loc) -> ()

  let sexp_of_type_constr_loc =
    (let (drop_if__028_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { name = name__023_; kind = kind__025_; loc_ = loc___029_ } ->
       let bnds__022_ = ([] : _ Stdlib.List.t) in
       let bnds__022_ =
         if drop_if__028_ loc___029_ then bnds__022_
         else
           let arg__031_ = sexp_of_location loc___029_ in
           let bnd__030_ = S.List [ S.Atom "loc_"; arg__031_ ] in
           (bnd__030_ :: bnds__022_ : _ Stdlib.List.t)
       in
       let bnds__022_ =
         let arg__026_ = sexp_of_type_name_kind kind__025_ in
         (S.List [ S.Atom "kind"; arg__026_ ] :: bnds__022_ : _ Stdlib.List.t)
       in
       let bnds__022_ =
         let arg__024_ = Type_path.sexp_of_t name__023_ in
         (S.List [ S.Atom "name"; arg__024_ ] :: bnds__022_ : _ Stdlib.List.t)
       in
       S.List bnds__022_
      : type_constr_loc -> S.t)

  let _ = sexp_of_type_constr_loc
end

type type_path_loc = {
  name : Type_path.t;
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  let _ = fun (_ : type_path_loc) -> ()

  let sexp_of_type_path_loc =
    (let (drop_if__036_ : location -> Stdlib.Bool.t) = hide_loc in
     fun { name = name__033_; loc_ = loc___037_ } ->
       let bnds__032_ = ([] : _ Stdlib.List.t) in
       let bnds__032_ =
         if drop_if__036_ loc___037_ then bnds__032_
         else
           let arg__039_ = sexp_of_location loc___037_ in
           let bnd__038_ = S.List [ S.Atom "loc_"; arg__039_ ] in
           (bnd__038_ :: bnds__032_ : _ Stdlib.List.t)
       in
       let bnds__032_ =
         let arg__034_ = Type_path.sexp_of_t name__033_ in
         (S.List [ S.Atom "name"; arg__034_ ] :: bnds__032_ : _ Stdlib.List.t)
       in
       S.List bnds__032_
      : type_path_loc -> S.t)

  let _ = sexp_of_type_path_loc
end

type type_name =
  | Tname_tvar of {
      index : int;
      name_ : string;
      loc_ : location; [@sexp_drop_if hide_loc]
    }
  | Tname_path of type_constr_loc
  | Tname_alias of {
      name : Type_path.t;
      kind : type_name_kind;
      alias_ : Basic_longident.t;
      loc_ : location;
    }

let loc_of_type_name (x : type_name) =
  match x with
  | Tname_tvar { loc_; _ } -> loc_
  | Tname_path { loc_; _ } -> loc_
  | Tname_alias { loc_; _ } -> loc_

let sexp_of_type_name (x : type_name) =
  let loc_s : S.t list =
    if hide_loc () then []
    else [ Atom "loc_"; sexp_of_location (loc_of_type_name x) ]
  in
  match x with
  | Tname_tvar { index; name_ } ->
      (List
         (List.cons
            (Atom "Tname_tvar" : S.t)
            (List.cons
               (List
                  (List.cons
                     (Atom "index" : S.t)
                     ([ Moon_sexp_conv.sexp_of_int index ] : S.t list))
                 : S.t)
               (List.cons
                  (List
                     (List.cons
                        (Atom "name_" : S.t)
                        ([ Moon_sexp_conv.sexp_of_string name_ ] : S.t list))
                    : S.t)
                  (loc_s : S.t list))))
        : S.t)
  | Tname_path { name; kind = Type } ->
      (List
         (List.cons
            (Atom "Tname_path" : S.t)
            (List.cons
               (List
                  (List.cons
                     (Atom "name" : S.t)
                     ([ Type_path.sexp_of_t name ] : S.t list))
                 : S.t)
               (loc_s : S.t list)))
        : S.t)
  | Tname_path { name; kind = Trait } ->
      (List
         (List.cons
            (Atom "Tname_trait" : S.t)
            (List.cons
               (List
                  (List.cons
                     (Atom "name" : S.t)
                     ([ Type_path.sexp_of_t name ] : S.t list))
                 : S.t)
               (loc_s : S.t list)))
        : S.t)
  | Tname_alias { name; kind = Type; alias_ } ->
      let alias = Basic_longident.to_string alias_ in
      (List
         (List.cons
            (Atom "Tname_path" : S.t)
            (List.cons
               (List
                  (List.cons
                     (Atom "name" : S.t)
                     ([ Type_path.sexp_of_t name ] : S.t list))
                 : S.t)
               (List.cons
                  (List
                     (List.cons
                        (Atom "alias" : S.t)
                        ([ Atom alias ] : S.t list))
                    : S.t)
                  (loc_s : S.t list))))
        : S.t)
  | Tname_alias { name; kind = Trait; alias_ } ->
      let alias = Basic_longident.to_string alias_ in
      (List
         (List.cons
            (Atom "Tname_trait" : S.t)
            (List.cons
               (List
                  (List.cons
                     (Atom "name" : S.t)
                     ([ Type_path.sexp_of_t name ] : S.t list))
                 : S.t)
               (List.cons
                  (List
                     (List.cons
                        (Atom "alias" : S.t)
                        ([ Atom alias ] : S.t list))
                    : S.t)
                  (loc_s : S.t list))))
        : S.t)

class ['a] iterbase =
  object (self)
    method private visit_attributes : 'a -> attributes -> unit =
      fun _ -> fun _ -> ()

    method private visit_ident : 'a -> ident -> unit = fun _ -> fun _ -> ()
    method visit_var : 'a -> var -> unit = fun _ -> fun _ -> ()
    method visit_binder : 'a -> binder -> unit = fun _ -> fun _ -> ()

    method visit_type_constraint : 'a -> Tvar_env.type_constraint -> unit =
      fun _ -> fun _ -> ()

    method visit_visibility : 'a -> Typedecl_info.visibility -> unit =
      fun _ -> fun _ -> ()

    method private visit_type_component_visibility :
        'a -> Typedecl_info.type_component_visibility -> unit =
      fun _ -> fun _ -> ()

    method visit_constraints : 'a -> constraints -> unit =
      fun ctx ->
        fun cs -> Lst.iter cs ~f:(fun c -> self#visit_type_constraint ctx c)

    method visit_argument_kind : 'a -> Syntax.argument_kind -> unit =
      fun _ -> fun _ -> ()

    method visit_fn_arity : 'a -> fn_arity -> unit = fun _ -> fun _ -> ()
    method visit_fn_kind : 'a -> fn_kind -> unit = fun _ -> fun _ -> ()
    method visit_prim : 'a -> Primitive.prim -> unit = fun _ -> fun _ -> ()
    method visit_location : 'a -> location -> unit = fun _ -> fun _ -> ()

    method visit_absolute_loc : 'a -> absolute_loc -> unit =
      fun _ -> fun _ -> ()

    method visit_label : 'a -> Syntax.label -> unit = fun _ -> fun _ -> ()
    method visit_accessor : 'a -> Syntax.accessor -> unit = fun _ -> fun _ -> ()

    method visit_constr_name : 'a -> Syntax.constr_name -> unit =
      fun _ -> fun _ -> ()

    method visit_constr_tag : 'a -> constr_tag -> unit = fun _ -> fun _ -> ()
    method visit_func_stubs : 'a -> func_stubs -> unit = fun _ -> fun _ -> ()
    method visit_pat_binders : 'a -> pat_binders -> unit = fun _ -> fun _ -> ()
    method visit_stype : 'a -> stype -> unit = fun _ -> fun _ -> ()

    method visit_constrid_loc : 'a -> constrid_loc -> unit =
      fun _ -> fun _ -> ()

    method visit_syntax_binder : 'a -> syntax_binder -> unit =
      fun _ -> fun _ -> ()

    method visit_hole : 'a -> Parsing_syntax.hole -> unit = fun _ -> fun _ -> ()
    method visit_apply_kind : 'a -> apply_kind -> unit = fun _ -> fun _ -> ()

    method visit_type_constr_loc : 'a -> type_constr_loc -> unit =
      fun _ -> fun _ -> ()

    method visit_type_name : 'a -> type_name -> unit = fun _ -> fun _ -> ()

    method visit_type_path_loc : 'a -> type_path_loc -> unit =
      fun _ -> fun _ -> ()

    method visit_docstring : 'a -> docstring -> unit = fun _ -> fun _ -> ()

    method visit_tvar_env : 'a -> tvar_env -> unit =
      fun ctx ->
        fun env ->
         Tvar_env.iter env (fun { name = _; typ; constraints } ->
             self#visit_stype ctx typ;
             self#visit_constraints ctx constraints)

    method visit_field_info : 'a -> field_info -> unit = fun _ -> fun _ -> ()

    method private visit_syntax_hole : 'a -> Syntax.hole -> unit =
      fun _ -> fun _ -> ()

    method visit_loop_label : 'a -> loop_label -> unit = fun _ -> fun _ -> ()

    method visit_loop_label_binder : 'a -> loop_label_binder -> unit =
      fun _ -> fun _ -> ()
  end

class ['a] mapbase =
  object (self)
    method private visit_attributes : 'a -> attributes -> attributes =
      fun _ -> fun x -> x

    method private visit_ident : 'a -> ident -> ident = fun _ -> fun e -> e
    method visit_var : 'a -> var -> var = fun _ -> fun e -> e
    method visit_binder : 'a -> binder -> binder = fun _ -> fun e -> e

    method visit_type_constraint :
        'a -> Tvar_env.type_constraint -> Tvar_env.type_constraint =
      fun _ -> fun e -> e

    method visit_visibility :
        'a -> Typedecl_info.visibility -> Typedecl_info.visibility =
      fun _ -> fun e -> e

    method private visit_type_component_visibility :
        'a ->
        Typedecl_info.type_component_visibility ->
        Typedecl_info.type_component_visibility =
      fun _ -> fun e -> e

    method visit_constraints : 'a -> constraints -> constraints =
      fun ctx ->
        fun cs -> Lst.map cs (fun c -> self#visit_type_constraint ctx c)

    method visit_argument_kind :
        'a -> Syntax.argument_kind -> Syntax.argument_kind =
      fun _ -> fun e -> e

    method visit_fn_arity : 'a -> fn_arity -> fn_arity = fun _ -> fun e -> e
    method visit_fn_kind : 'a -> fn_kind -> fn_kind = fun _ -> fun e -> e

    method visit_prim : 'a -> Primitive.prim -> Primitive.prim =
      fun _ -> fun e -> e

    method visit_location : 'a -> location -> location = fun _ -> fun e -> e

    method visit_absolute_loc : 'a -> absolute_loc -> absolute_loc =
      fun _ -> fun e -> e

    method visit_label : 'a -> Syntax.label -> Syntax.label =
      fun _ -> fun e -> e

    method visit_accessor : 'a -> Syntax.accessor -> Syntax.accessor =
      fun _ -> fun e -> e

    method visit_constr_name : 'a -> Syntax.constr_name -> Syntax.constr_name =
      fun _ -> fun e -> e

    method visit_constr_tag : 'a -> constr_tag -> constr_tag =
      fun _ -> fun e -> e

    method visit_func_stubs : 'a -> func_stubs -> func_stubs =
      fun _ -> fun e -> e

    method visit_pat_binders : 'a -> pat_binders -> pat_binders =
      fun _ -> fun e -> e

    method visit_stype : 'a -> stype -> stype = fun _ -> fun e -> e

    method visit_constrid_loc : 'a -> constrid_loc -> constrid_loc =
      fun _ -> fun e -> e

    method visit_syntax_binder : 'a -> syntax_binder -> syntax_binder =
      fun _ -> fun e -> e

    method visit_hole : 'a -> syntax_hole -> syntax_hole = fun _ -> fun e -> e

    method visit_apply_kind : 'a -> apply_kind -> apply_kind =
      fun _ -> fun e -> e

    method visit_type_name : 'a -> type_name -> type_name = fun _ -> fun e -> e

    method visit_type_path_loc : 'a -> type_path_loc -> type_path_loc =
      fun _ -> fun e -> e

    method visit_type_constr_loc : 'a -> type_constr_loc -> type_constr_loc =
      fun _ -> fun e -> e

    method visit_tvar_env : 'a -> tvar_env -> tvar_env = fun _ -> fun e -> e
    method visit_docstring : 'a -> docstring -> docstring = fun _ -> fun e -> e

    method visit_field_info : 'a -> field_info -> field_info =
      fun _ -> fun e -> e

    method private visit_syntax_hole : 'a -> Syntax.hole -> Syntax.hole =
      fun _ -> fun e -> e

    method visit_loop_label : 'a -> loop_label -> loop_label =
      fun _ -> fun e -> e

    method visit_loop_label_binder :
        'a -> loop_label_binder -> loop_label_binder =
      fun _ -> fun e -> e
  end

class virtual ['a] sexpbase =
  object
    inherit [_] Sexp_visitors.sexp

    method private visit_attributes : 'a -> attributes -> S.t =
      fun _ -> fun x -> sexp_of_attributes x

    method private visit_ident : 'a -> ident -> S.t =
      fun _ -> fun x -> sexp_of_ident x

    method visit_var : 'a -> var -> S.t = fun _ -> fun x -> sexp_of_var x

    method visit_value_kind : 'a -> value_kind -> S.t =
      fun _ -> fun x -> sexp_of_value_kind x

    method visit_visibility : 'a -> Typedecl_info.visibility -> S.t =
      fun _ -> fun x -> Typedecl_info.sexp_of_visibility x

    method visit_type_component_visibility :
        'a -> Typedecl_info.type_component_visibility -> S.t =
      fun _ -> fun x -> Typedecl_info.sexp_of_type_component_visibility x

    method visit_stype : 'a -> stype -> S.t = fun _ -> fun x -> sexp_of_stype x

    method visit_prim : 'a -> Primitive.prim -> S.t =
      fun _ -> fun x -> Primitive.sexp_of_prim x

    method visit_pat_binders : 'a -> pat_binders -> S.t =
      fun _ -> fun x -> sexp_of_pat_binders x

    method visit_location : 'a -> location -> S.t =
      fun _ -> fun x -> sexp_of_location x

    method visit_absolute_loc : 'a -> absolute_loc -> S.t =
      fun _ -> fun x -> sexp_of_absolute_loc x

    method visit_label : 'a -> Syntax.label -> S.t =
      fun _ -> fun x -> Syntax.sexp_of_label x

    method visit_func_stubs : 'a -> func_stubs -> S.t =
      fun _ -> fun x -> sexp_of_func_stubs x

    method visit_constr_name : 'a -> Syntax.constr_name -> S.t =
      fun _ -> fun x -> Syntax.sexp_of_constr_name x

    method visit_constrid_loc : 'a -> constrid_loc -> S.t =
      fun _ -> fun x -> sexp_of_constrid_loc x

    method visit_type_constraint : 'a -> Tvar_env.type_constraint -> S.t =
      fun _ -> fun x -> Tvar_env.sexp_of_type_constraint x

    method visit_argument_kind : 'a -> Syntax.argument_kind -> S.t =
      fun _ -> fun x -> Syntax.sexp_of_argument_kind x

    method visit_fn_arity : 'a -> fn_arity -> S.t =
      fun _ -> fun x -> sexp_of_fn_arity x

    method visit_fn_kind : 'a -> fn_kind -> S.t =
      fun _ -> fun x -> sexp_of_fn_kind x

    method visit_syntax_binder : 'a -> syntax_binder -> S.t =
      fun _ -> fun x -> sexp_of_syntax_binder x

    method visit_hole : 'a -> syntax_hole -> S.t =
      fun _ -> fun x -> sexp_of_syntax_hole x

    method visit_constraints : 'a -> constraints -> S.t =
      fun _ -> fun x -> sexp_of_constraints x

    method visit_constr_tag : 'a -> constr_tag -> S.t =
      fun _ -> fun x -> sexp_of_constr_tag x

    method visit_constant : 'a -> constant -> S.t =
      fun _ -> fun x -> sexp_of_constant x

    method visit_binder : 'a -> binder -> S.t =
      fun _ -> fun x -> sexp_of_binder x

    method visit_apply_kind : 'a -> apply_kind -> S.t =
      fun _ -> fun x -> sexp_of_apply_kind x

    method visit_accessor : 'a -> Syntax.accessor -> S.t =
      fun _ -> fun x -> Syntax.sexp_of_accessor x

    method visit_type_constr_loc : 'a -> type_constr_loc -> S.t =
      fun _ -> fun x -> sexp_of_type_constr_loc x

    method visit_type_name : 'a -> type_name -> S.t =
      fun _ -> fun x -> sexp_of_type_name x

    method visit_type_path_loc : 'a -> type_path_loc -> S.t =
      fun _ -> fun x -> sexp_of_type_path_loc x

    method visit_tvar_env : 'a -> tvar_env -> S.t =
      fun _ -> fun x -> sexp_of_tvar_env x

    method visit_docstring : 'a -> docstring -> S.t =
      fun _ -> fun x -> sexp_of_docstring x

    method visit_field_info : 'a -> field_info -> S.t =
      fun _ -> fun x -> sexp_of_field_info x

    method private visit_syntax_hole : 'a -> Syntax.hole -> S.t =
      fun _ -> fun x -> sexp_of_syntax_hole x

    method visit_expr_id : 'a -> expr_id -> S.t =
      fun _ -> fun x -> sexp_of_expr_id x

    method visit_loop_label : 'a -> loop_label -> S.t =
      fun _ -> fun x -> sexp_of_loop_label x

    method visit_loop_label_binder : 'a -> loop_label_binder -> S.t =
      fun _ -> fun x -> sexp_of_loop_label_binder x
  end

type stub_decl = {
  binder : binder;
  params : params;
  ret : typ option;
  func_stubs : stub_body;
  is_pub : bool;
  arity_ : fn_arity;
  kind_ : fun_decl_kind; [@dead "stub_decl.kind_"]
  loc_ : absolute_loc;
  mutable doc_ : docstring; [@dead "stub_decl.doc_"]
}

and stub_body = Intrinsic | Func_stub of func_stubs

and expr =
  | Texpr_apply of {
      func : expr;
      args : argument list;
      ty : stype;
      kind_ : apply_kind;
      loc_ : location;
    }
  | Texpr_method of {
      meth : var;
      ty_args_ : stype array;
      arity_ : fn_arity option;
      type_name : type_name;
      prim : Primitive.prim option;
      ty : stype;
      loc_ : location;
    }
  | Texpr_unresolved_method of {
      trait_name : type_name;
      method_name : string;
      self_type : stype;
      arity_ : fn_arity option;
      ty : stype;
      loc_ : location;
    }
  | Texpr_ident of {
      id : var;
      ty_args_ : stype array;
      arity_ : fn_arity option;
      kind : value_kind;
      ty : stype;
      loc_ : location;
    }
  | Texpr_as of {
      expr : expr;
      trait : type_name;
      ty : stype;
      is_implicit : bool;
      loc_ : location;
    }
  | Texpr_array of {
      exprs : expr list;
      ty : stype;
      is_fixed_array : bool;
      loc_ : location;
    }
  | Texpr_constant of {
      c : constant;
      ty : stype;
      name_ : var option;
      loc_ : location;
    }
  | Texpr_constr of {
      type_name : type_name option;
      constr : Syntax.constr_name;
      tag : constr_tag;
      ty : stype;
      arity_ : fn_arity;
      loc_ : location;
    }
  | Texpr_while of {
      loop_cond : expr;
      loop_body : expr;
      while_else : expr option;
      ty : stype;
      label : loop_label_binder option;
      loc_ : location;
    }
  | Texpr_function of { func : fn; ty : stype; is_raw_ : bool; loc_ : location }
  | Texpr_if of {
      cond : expr;
      ifso : expr;
      ifnot : expr option;
      ty : stype;
      loc_ : location;
    }
  | Texpr_letfn of {
      binder : binder;
      fn : fn;
      body : expr;
      ty : stype;
      is_rec : bool;
      loc_ : location;
    }
  | Texpr_letrec of {
      bindings : (binder * fn) list;
      body : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_let of {
      pat : pat;
      rhs : expr;
      pat_binders : pat_binders;
      body : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_sequence of {
      exprs : expr list;
      last_expr : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_tuple of { exprs : expr list; ty : stype; loc_ : location }
  | Texpr_record of {
      type_name : type_name option;
      fields : field_def list;
      ty : stype;
      loc_ : location;
    }
  | Texpr_record_update of {
      type_name : type_name option;
      record : expr;
      all_fields : field_info list;
      fields : field_def list;
      ty : stype;
      loc_ : location;
    }
  | Texpr_field of {
      record : expr;
      accessor : Syntax.accessor;
      pos : int;
      ty : stype;
      loc_ : location;
    }
  | Texpr_mutate of {
      record : expr;
      label : Syntax.label;
      field : expr;
      pos : int;
      augmented_by : expr option;
      ty : stype;
      loc_ : location;
    }
  | Texpr_match of {
      expr : expr;
      cases : match_case list;
      ty : stype;
      match_loc_ : location;
      loc_ : location;
    }
  | Texpr_letmut of {
      binder : binder;
      konstraint : typ option;
      expr : expr;
      body : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_assign of {
      var : var;
      expr : expr;
      augmented_by : expr option;
      ty : stype;
      loc_ : location;
    }
  | Texpr_hole of { ty : stype; loc_ : location; kind : syntax_hole }
  | Texpr_unit of { loc_ : location }
  | Texpr_break of {
      arg : expr option;
      ty : stype;
      label : loop_label option;
      loc_ : location;
    }
  | Texpr_continue of {
      args : expr list;
      ty : stype;
      label : loop_label option;
      loc_ : location;
    }
  | Texpr_loop of {
      params : param list;
      body : expr;
      args : expr list;
      ty : stype;
      label : loop_label_binder option;
      loc_ : location;
    }
  | Texpr_for of {
      binders : (binder * expr) list;
      condition : expr option;
      steps : (var * expr) list;
      body : expr;
      for_else : expr option;
      ty : stype;
      label : loop_label_binder option;
      loc_ : location;
    }
  | Texpr_foreach of {
      binders : binder option list;
      elem_tys : stype list;
      expr : expr;
      body : expr;
      else_block : expr option;
      ty : stype;
      label : loop_label_binder option;
      loc_ : location;
    }
  | Texpr_return of { return_value : expr option; ty : stype; loc_ : location }
  | Texpr_raise of { error_value : expr; ty : stype; loc_ : location }
  | Texpr_try of {
      body : expr;
      catch : match_case list;
      catch_all : bool;
      try_else : match_case list option;
      ty : stype;
      err_ty : stype;
      catch_loc_ : location;
      else_loc_ : location;
      loc_ : location;
    }
  | Texpr_exclamation of {
      expr : expr;
      ty : stype;
      convert_to_result : bool;
      loc_ : location;
    }
  | Texpr_constraint of {
      expr : expr;
      konstraint : typ;
      ty : stype;
      loc_ : location;
    }
  | Texpr_pipe of { lhs : expr; rhs : pipe_rhs; ty : stype; loc_ : location }
  | Texpr_interp of { elems : interp_elem list; ty : stype; loc_ : location }
  | Texpr_guard of {
      cond : expr;
      otherwise : expr option;
      body : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_guard_let of {
      pat : pat;
      rhs : expr;
      pat_binders : pat_binders;
      otherwise : match_case list option;
      body : expr;
      ty : stype;
      loc_ : location;
    }
  | Texpr_is of {
      expr : expr;
      pat : pat;
      pat_binders : pat_binders;
      loc_ : location;
    }
  | Texpr_array_as_view of { array : expr; ty : stype; loc_ : location }
  | Texpr_and of { lhs : expr; rhs : expr; loc_ : location }
  | Texpr_or of { lhs : expr; rhs : expr; loc_ : location }

and argument = { arg_value : expr; arg_kind : Syntax.argument_kind }

and pipe_rhs =
  | Pipe_partial_apply of { func : expr; args : argument list; loc_ : location }
  | Pipe_invalid of { expr : expr; ty : stype; loc_ : location }

and interp_elem =
  | Interp_lit of string
  | Interp_expr of { expr : expr; to_string : expr; loc_ : location }

and field_def =
  | Field_def of {
      label : Syntax.label;
      expr : expr;
      is_mut : bool;
      is_pun : bool;
      pos : int;
    }

and field_pat =
  | Field_pat of { label : Syntax.label; pat : pat; is_pun : bool; pos : int }

and constr_pat_args = constr_pat_arg list

and constr_pat_arg =
  | Constr_pat_arg of { pat : pat; kind : Syntax.argument_kind; pos : int }

and param =
  | Param of {
      binder : binder;
      konstraint : typ option;
      ty : stype;
      kind : param_kind;
    }
  | Discard_positional_param of {
      ty : stype;
      konstraint : typ option;
      loc_ : location;
    }

and param_kind =
  | Positional
  | Labelled
  | Optional of expr
  | Autofill
  | Question_optional

and params = param list

and fn = {
  params : params;
  params_loc_ : location;
  body : expr;
  ret_constraint : (typ * error_typ) option;
  ty : stype;
  kind_ : fn_kind;
}

and match_case = {
  pat : pat;
  action : expr;
  guard : expr option;
  pat_binders : pat_binders;
}

and error_typ =
  | Error_typ of { ty : typ }
  | Default_error_typ of { loc_ : location }
  | No_error_typ

and typ =
  | Tany of { ty : stype; loc_ : location }
  | Tarrow of {
      params : typ list;
      return : typ;
      err_ty : error_typ;
      ty : stype;
      loc_ : location;
    }
  | T_tuple of { params : typ list; ty : stype; loc_ : location }
  | Tname of {
      constr : constrid_loc;
      params : typ list;
      ty : stype;
      is_alias_ : bool;
      loc_ : location;
    }
  | Tobject of {
      constr : constrid_loc;
      ty : stype;
      is_alias_ : bool;
      loc_ : location;
    }

and pat =
  | Tpat_alias of { pat : pat; alias : binder; ty : stype; loc_ : location }
  | Tpat_any of { ty : stype; loc_ : location }
  | Tpat_array of { pats : array_pattern; ty : stype; loc_ : location }
  | Tpat_constant of {
      c : constant;
      ty : stype;
      name_ : var option;
      loc_ : location;
    }
  | Tpat_constr of {
      type_name : type_name option;
      constr : Syntax.constr_name;
      args : constr_pat_args;
      tag : constr_tag;
      ty : stype;
      used_error_subtyping : bool;
      arity_ : fn_arity;
      all_args_ : stype list;
      loc_ : location;
    }
  | Tpat_or of { pat1 : pat; pat2 : pat; ty : stype; loc_ : location }
  | Tpat_tuple of { pats : pat list; ty : stype; loc_ : location }
  | Tpat_var of { binder : binder; ty : stype; loc_ : location }
  | Tpat_record of {
      fields : field_pat list;
      is_closed : bool;
      ty : stype;
      loc_ : location;
    }
  | Tpat_constraint of {
      pat : pat;
      konstraint : typ;
      ty : stype;
      loc_ : location;
    }
  | Tpat_map of {
      elems : (constant * pat) list;
      op_get_info_ : ident * stype * stype array;
      ty : stype;
      loc_ : location;
    }
  | Tpat_range of {
      lhs : pat;
      rhs : pat;
      inclusive : bool;
      ty : stype;
      loc_ : location;
    }

and array_pattern =
  | Closed of pat list
  | Open of pat list * pat list * (binder * stype) option

and fun_decl = {
  kind : fun_decl_kind;
  fn_binder : binder;
  fn : fn;
  is_pub : bool;
  ty_params_ : tvar_env;
  constraint_names : type_name list;
  mutable doc_ : docstring; [@dead "fun_decl.doc_"]
  attrs : attributes;
}

and fun_decl_kind =
  | Fun_kind_regular
  | Fun_kind_method of type_name option
  | Fun_kind_default_impl of type_name
  | Fun_kind_impl of { self_ty : typ; trait : type_name }

and impl =
  | Timpl_expr of {
      expr : expr;
      is_main : bool; [@sexp_drop_if not]
      expr_id : (expr_id[@visitors.opaque]);
      loc_ : absolute_loc;
      is_generated_ : bool;
    }
  | Timpl_fun_decl of {
      fun_decl : fun_decl;
      arity_ : fn_arity;
      loc_ : absolute_loc;
      is_generated_ : bool;
    }
  | Timpl_letdef of {
      binder : binder;
      konstraint : typ option;
      expr : expr;
      is_pub : bool;
      loc_ : absolute_loc;
      mutable doc_ : docstring;
      attrs : attributes;
      is_generated_ : bool;
    }
  | Timpl_stub_decl of stub_decl

and impls = impl list

and type_decl = {
  td_binder : type_constr_loc;
  td_params : tvar_env;
  td_desc : type_desc;
  td_vis : Typedecl_info.visibility;
  td_loc_ : absolute_loc;
  td_doc_ : docstring; [@dead "type_decl.td_doc_"]
  td_deriving_ : type_name list;
}

and exception_decl =
  | No_payload
  | Single_payload of typ
  | Enum_payload of constr_decl list

and type_desc =
  | Td_abstract
  | Td_error of exception_decl
  | Td_newtype of typ
  | Td_variant of constr_decl list
  | Td_record of field_decl list
  | Td_alias of typ

and constr_decl_arg = {
  carg_typ : typ;
  carg_mut : bool;
  carg_label : Syntax.label option;
}

and constr_decl = {
  constr_name : Syntax.label;
  constr_tag : constr_tag;
  constr_args : constr_decl_arg list;
  constr_arity_ : fn_arity;
  constr_loc_ : location; [@dead "constr_decl.constr_loc_"]
}

and field_decl = {
  field_label : Syntax.label;
  field_typ : typ;
  field_mut : bool;
  field_vis : Typedecl_info.type_component_visibility;
  field_loc_ : location; [@dead "field_decl.field_loc_"]
}

and trait_decl = {
  trait_name : type_constr_loc;
  trait_supers : type_name list; [@dead "trait_decl.trait_supers"]
  trait_methods : method_decl list;
  trait_vis : Typedecl_info.visibility;
  trait_loc_ : absolute_loc;
  trait_doc_ : docstring; [@dead "trait_decl.trait_doc_"]
  trait_attrs : attributes; [@dead "trait_decl.trait_attrs"]
}

and trait_alias_decl = {
  trait_alias_name : type_constr_loc;
  trait_alias_target : type_name;
  trait_alias_is_pub : bool;
  trait_alias_loc_ : absolute_loc;
  trait_alias_doc_ : docstring; [@dead "trait_alias_decl.trait_alias_doc_"]
}

and method_decl = {
  method_name : syntax_binder;
  method_params : (Syntax.label option * typ) list;
  method_ret : typ option;
  method_err : typ option;
  method_loc_ : location; [@dead "method_decl.method_loc_"]
}

and output =
  | Output of {
      value_defs : impls;
      type_defs : type_decl list;
      trait_defs : trait_decl list;
      trait_alias : trait_alias_decl list;
    }

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] sexp =
    object (self : 'self)
      inherit [_] sexpbase

      method visit_stub_decl : _ -> stub_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_binder env _visitors_this.binder in
           let _visitors_r1 = self#visit_params env _visitors_this.params in
           let _visitors_r2 =
             self#visit_option self#visit_typ env _visitors_this.ret
           in
           let _visitors_r3 =
             self#visit_stub_body env _visitors_this.func_stubs
           in
           let _visitors_r4 = self#visit_bool env _visitors_this.is_pub in
           let _visitors_r5 = self#visit_fn_arity env _visitors_this.arity_ in
           let _visitors_r6 =
             self#visit_fun_decl_kind env _visitors_this.kind_
           in
           let _visitors_r7 = self#visit_absolute_loc env _visitors_this.loc_ in
           let _visitors_r8 = self#visit_docstring env _visitors_this.doc_ in
           self#visit_record env
             [
               ("binder", _visitors_r0);
               ("params", _visitors_r1);
               ("ret", _visitors_r2);
               ("func_stubs", _visitors_r3);
               ("is_pub", _visitors_r4);
               ("arity_", _visitors_r5);
               ("kind_", _visitors_r6);
               ("loc_", _visitors_r7);
               ("doc_", _visitors_r8);
             ]

      method visit_Intrinsic : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Intrinsic" []

      method visit_Func_stub : _ -> func_stubs -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
           self#visit_inline_tuple env "Func_stub" [ _visitors_r0 ]

      method visit_stub_body : _ -> stub_body -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Intrinsic -> self#visit_Intrinsic env
           | Func_stub _visitors_c0 -> self#visit_Func_stub env _visitors_c0

      method visit_Texpr_apply :
          _ -> expr -> argument list -> stype -> apply_kind -> location -> S.t =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_fty ->
             fun _visitors_fkind_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_ffunc in
               let _visitors_r1 =
                 self#visit_list self#visit_argument env _visitors_fargs
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 = self#visit_apply_kind env _visitors_fkind_ in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_apply"
                 [
                   ("func", _visitors_r0);
                   ("args", _visitors_r1);
                   ("ty", _visitors_r2);
                   ("kind_", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_method :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          type_name ->
          Primitive.prim option ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fmeth ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_ftype_name ->
              fun _visitors_fprim ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fmeth in
                 let _visitors_r1 =
                   self#visit_array self#visit_stype env _visitors_fty_args_
                 in
                 let _visitors_r2 =
                   self#visit_option self#visit_fn_arity env _visitors_farity_
                 in
                 let _visitors_r3 =
                   self#visit_type_name env _visitors_ftype_name
                 in
                 let _visitors_r4 =
                   self#visit_option self#visit_prim env _visitors_fprim
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Texpr_method"
                   [
                     ("meth", _visitors_r0);
                     ("ty_args_", _visitors_r1);
                     ("arity_", _visitors_r2);
                     ("type_name", _visitors_r3);
                     ("prim", _visitors_r4);
                     ("ty", _visitors_r5);
                     ("loc_", _visitors_r6);
                   ]

      method visit_Texpr_unresolved_method :
          _ ->
          type_name ->
          string ->
          stype ->
          fn_arity option ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_ftrait_name ->
           fun _visitors_fmethod_name ->
            fun _visitors_fself_type ->
             fun _visitors_farity_ ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_type_name env _visitors_ftrait_name
                in
                let _visitors_r1 =
                  self#visit_string env _visitors_fmethod_name
                in
                let _visitors_r2 = self#visit_stype env _visitors_fself_type in
                let _visitors_r3 =
                  self#visit_option self#visit_fn_arity env _visitors_farity_
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_unresolved_method"
                  [
                    ("trait_name", _visitors_r0);
                    ("method_name", _visitors_r1);
                    ("self_type", _visitors_r2);
                    ("arity_", _visitors_r3);
                    ("ty", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_ident :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          value_kind ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fid ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_fkind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  self#visit_array self#visit_stype env _visitors_fty_args_
                in
                let _visitors_r2 =
                  self#visit_option self#visit_fn_arity env _visitors_farity_
                in
                let _visitors_r3 = self#visit_value_kind env _visitors_fkind in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_ident"
                  [
                    ("id", _visitors_r0);
                    ("ty_args_", _visitors_r1);
                    ("arity_", _visitors_r2);
                    ("kind", _visitors_r3);
                    ("ty", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_as :
          _ -> expr -> type_name -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftrait ->
            fun _visitors_fty ->
             fun _visitors_fis_implicit ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 = self#visit_bool env _visitors_fis_implicit in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_as"
                 [
                   ("expr", _visitors_r0);
                   ("trait", _visitors_r1);
                   ("ty", _visitors_r2);
                   ("is_implicit", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_array :
          _ -> expr list -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_fis_fixed_array ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_expr env _visitors_fexprs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_bool env _visitors_fis_fixed_array
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_array"
                [
                  ("exprs", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("is_fixed_array", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_constant :
          _ -> constant -> stype -> var option -> location -> S.t =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constant env _visitors_fc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_option self#visit_var env _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_constant"
                [
                  ("c", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("name_", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_tag ->
          stype ->
          fn_arity ->
          location ->
          S.t =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_ftag ->
             fun _visitors_fty ->
              fun _visitors_farity_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_option self#visit_type_name env
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  self#visit_constr_name env _visitors_fconstr
                in
                let _visitors_r2 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 = self#visit_fn_arity env _visitors_farity_ in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_constr"
                  [
                    ("type_name", _visitors_r0);
                    ("constr", _visitors_r1);
                    ("tag", _visitors_r2);
                    ("ty", _visitors_r3);
                    ("arity_", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_while :
          _ ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          S.t =
        fun env ->
          fun _visitors_floop_cond ->
           fun _visitors_floop_body ->
            fun _visitors_fwhile_else ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                let _visitors_r2 =
                  self#visit_option self#visit_expr env _visitors_fwhile_else
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  self#visit_option self#visit_loop_label_binder env
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_while"
                  [
                    ("loop_cond", _visitors_r0);
                    ("loop_body", _visitors_r1);
                    ("while_else", _visitors_r2);
                    ("ty", _visitors_r3);
                    ("label", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_function : _ -> fn -> stype -> bool -> location -> S.t
          =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fty ->
            fun _visitors_fis_raw_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_fn env _visitors_ffunc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 = self#visit_bool env _visitors_fis_raw_ in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_function"
                [
                  ("func", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("is_raw_", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_if :
          _ -> expr -> expr -> expr option -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 = self#visit_expr env _visitors_fifso in
               let _visitors_r2 =
                 self#visit_option self#visit_expr env _visitors_fifnot
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_if"
                 [
                   ("cond", _visitors_r0);
                   ("ifso", _visitors_r1);
                   ("ifnot", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_letfn :
          _ -> binder -> fn -> expr -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_ffn ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_fis_rec ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_fn env _visitors_ffn in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 = self#visit_bool env _visitors_fis_rec in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_letfn"
                  [
                    ("binder", _visitors_r0);
                    ("fn", _visitors_r1);
                    ("body", _visitors_r2);
                    ("ty", _visitors_r3);
                    ("is_rec", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_letrec :
          _ -> (binder * fn) list -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fbindings ->
           fun _visitors_fbody ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list
                  (fun env ->
                    fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 = self#visit_binder env _visitors_c0 in
                     let _visitors_r1 = self#visit_fn env _visitors_c1 in
                     self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                  env _visitors_fbindings
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_letrec"
                [
                  ("bindings", _visitors_r0);
                  ("body", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_let :
          _ -> pat -> expr -> pat_binders -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pat env _visitors_fpat in
                let _visitors_r1 = self#visit_expr env _visitors_frhs in
                let _visitors_r2 =
                  self#visit_pat_binders env _visitors_fpat_binders
                in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_let"
                  [
                    ("pat", _visitors_r0);
                    ("rhs", _visitors_r1);
                    ("pat_binders", _visitors_r2);
                    ("body", _visitors_r3);
                    ("ty", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_sequence :
          _ -> expr list -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_expr env _visitors_fexprs
              in
              let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_sequence"
                [
                  ("exprs", _visitors_r0);
                  ("last_expr", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_tuple : _ -> expr list -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               self#visit_list self#visit_expr env _visitors_fexprs
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_tuple"
               [
                 ("exprs", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_record :
          _ -> type_name option -> field_def list -> stype -> location -> S.t =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_ffields ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_option self#visit_type_name env _visitors_ftype_name
              in
              let _visitors_r1 =
                self#visit_list self#visit_field_def env _visitors_ffields
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_record"
                [
                  ("type_name", _visitors_r0);
                  ("fields", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_record_update :
          _ ->
          type_name option ->
          expr ->
          field_info list ->
          field_def list ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_frecord ->
            fun _visitors_fall_fields ->
             fun _visitors_ffields ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_option self#visit_type_name env
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  self#visit_list self#visit_field_info env
                    _visitors_fall_fields
                in
                let _visitors_r3 =
                  self#visit_list self#visit_field_def env _visitors_ffields
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_record_update"
                  [
                    ("type_name", _visitors_r0);
                    ("record", _visitors_r1);
                    ("all_fields", _visitors_r2);
                    ("fields", _visitors_r3);
                    ("ty", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_field :
          _ -> expr -> Syntax.accessor -> int -> stype -> location -> S.t =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_faccessor ->
            fun _visitors_fpos ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 = self#visit_int env _visitors_fpos in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_field"
                 [
                   ("record", _visitors_r0);
                   ("accessor", _visitors_r1);
                   ("pos", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_mutate :
          _ ->
          expr ->
          Syntax.label ->
          expr ->
          int ->
          expr option ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_flabel ->
            fun _visitors_ffield ->
             fun _visitors_fpos ->
              fun _visitors_faugmented_by ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 = self#visit_label env _visitors_flabel in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 = self#visit_int env _visitors_fpos in
                 let _visitors_r4 =
                   self#visit_option self#visit_expr env _visitors_faugmented_by
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Texpr_mutate"
                   [
                     ("record", _visitors_r0);
                     ("label", _visitors_r1);
                     ("field", _visitors_r2);
                     ("pos", _visitors_r3);
                     ("augmented_by", _visitors_r4);
                     ("ty", _visitors_r5);
                     ("loc_", _visitors_r6);
                   ]

      method visit_Texpr_match :
          _ -> expr -> match_case list -> stype -> location -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fcases ->
            fun _visitors_fty ->
             fun _visitors_fmatch_loc_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 self#visit_list self#visit_match_case env _visitors_fcases
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 self#visit_location env _visitors_fmatch_loc_
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_match"
                 [
                   ("expr", _visitors_r0);
                   ("cases", _visitors_r1);
                   ("ty", _visitors_r2);
                   ("match_loc_", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_letmut :
          _ -> binder -> typ option -> expr -> expr -> stype -> location -> S.t
          =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 =
                  self#visit_option self#visit_typ env _visitors_fkonstraint
                in
                let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_letmut"
                  [
                    ("binder", _visitors_r0);
                    ("konstraint", _visitors_r1);
                    ("expr", _visitors_r2);
                    ("body", _visitors_r3);
                    ("ty", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_assign :
          _ -> var -> expr -> expr option -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fexpr ->
            fun _visitors_faugmented_by ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fvar in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 self#visit_option self#visit_expr env _visitors_faugmented_by
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_assign"
                 [
                   ("var", _visitors_r0);
                   ("expr", _visitors_r1);
                   ("augmented_by", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_hole : _ -> stype -> location -> syntax_hole -> S.t =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            fun _visitors_fkind ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 = self#visit_location env _visitors_floc_ in
             let _visitors_r2 = self#visit_syntax_hole env _visitors_fkind in
             self#visit_inline_record env "Texpr_hole"
               [
                 ("ty", _visitors_r0);
                 ("loc_", _visitors_r1);
                 ("kind", _visitors_r2);
               ]

      method visit_Texpr_unit : _ -> location -> S.t =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           self#visit_inline_record env "Texpr_unit" [ ("loc_", _visitors_r0) ]

      method visit_Texpr_break :
          _ -> expr option -> stype -> loop_label option -> location -> S.t =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_option self#visit_expr env _visitors_farg
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_option self#visit_loop_label env _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_break"
                [
                  ("arg", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("label", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_continue :
          _ -> expr list -> stype -> loop_label option -> location -> S.t =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_expr env _visitors_fargs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_option self#visit_loop_label env _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_continue"
                [
                  ("args", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("label", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_loop :
          _ ->
          param list ->
          expr ->
          expr list ->
          stype ->
          loop_label_binder option ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_list self#visit_param env _visitors_fparams
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 =
                  self#visit_list self#visit_expr env _visitors_fargs
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  self#visit_option self#visit_loop_label_binder env
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Texpr_loop"
                  [
                    ("params", _visitors_r0);
                    ("body", _visitors_r1);
                    ("args", _visitors_r2);
                    ("ty", _visitors_r3);
                    ("label", _visitors_r4);
                    ("loc_", _visitors_r5);
                  ]

      method visit_Texpr_for :
          _ ->
          (binder * expr) list ->
          expr option ->
          (var * expr) list ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_fcondition ->
            fun _visitors_fsteps ->
             fun _visitors_fbody ->
              fun _visitors_ffor_else ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    self#visit_list
                      (fun env ->
                        fun (_visitors_c0, _visitors_c1) ->
                         let _visitors_r0 =
                           self#visit_binder env _visitors_c0
                         in
                         let _visitors_r1 = self#visit_expr env _visitors_c1 in
                         self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                      env _visitors_fbinders
                  in
                  let _visitors_r1 =
                    self#visit_option self#visit_expr env _visitors_fcondition
                  in
                  let _visitors_r2 =
                    self#visit_list
                      (fun env ->
                        fun (_visitors_c0, _visitors_c1) ->
                         let _visitors_r0 = self#visit_var env _visitors_c0 in
                         let _visitors_r1 = self#visit_expr env _visitors_c1 in
                         self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                      env _visitors_fsteps
                  in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    self#visit_option self#visit_expr env _visitors_ffor_else
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    self#visit_option self#visit_loop_label_binder env
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Texpr_for"
                    [
                      ("binders", _visitors_r0);
                      ("condition", _visitors_r1);
                      ("steps", _visitors_r2);
                      ("body", _visitors_r3);
                      ("for_else", _visitors_r4);
                      ("ty", _visitors_r5);
                      ("label", _visitors_r6);
                      ("loc_", _visitors_r7);
                    ]

      method visit_Texpr_foreach :
          _ ->
          binder option list ->
          stype list ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_felem_tys ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_felse_block ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    self#visit_list
                      (self#visit_option self#visit_binder)
                      env _visitors_fbinders
                  in
                  let _visitors_r1 =
                    self#visit_list self#visit_stype env _visitors_felem_tys
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    self#visit_option self#visit_expr env _visitors_felse_block
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    self#visit_option self#visit_loop_label_binder env
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Texpr_foreach"
                    [
                      ("binders", _visitors_r0);
                      ("elem_tys", _visitors_r1);
                      ("expr", _visitors_r2);
                      ("body", _visitors_r3);
                      ("else_block", _visitors_r4);
                      ("ty", _visitors_r5);
                      ("label", _visitors_r6);
                      ("loc_", _visitors_r7);
                    ]

      method visit_Texpr_return : _ -> expr option -> stype -> location -> S.t =
        fun env ->
          fun _visitors_freturn_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               self#visit_option self#visit_expr env _visitors_freturn_value
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_return"
               [
                 ("return_value", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_raise : _ -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_ferror_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ferror_value in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_raise"
               [
                 ("error_value", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_try :
          _ ->
          expr ->
          match_case list ->
          bool ->
          match_case list option ->
          stype ->
          stype ->
          location ->
          location ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fcatch ->
            fun _visitors_fcatch_all ->
             fun _visitors_ftry_else ->
              fun _visitors_fty ->
               fun _visitors_ferr_ty ->
                fun _visitors_fcatch_loc_ ->
                 fun _visitors_felse_loc_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fbody in
                   let _visitors_r1 =
                     self#visit_list self#visit_match_case env _visitors_fcatch
                   in
                   let _visitors_r2 =
                     self#visit_bool env _visitors_fcatch_all
                   in
                   let _visitors_r3 =
                     self#visit_option
                       (self#visit_list self#visit_match_case)
                       env _visitors_ftry_else
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 = self#visit_stype env _visitors_ferr_ty in
                   let _visitors_r6 =
                     self#visit_location env _visitors_fcatch_loc_
                   in
                   let _visitors_r7 =
                     self#visit_location env _visitors_felse_loc_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   self#visit_inline_record env "Texpr_try"
                     [
                       ("body", _visitors_r0);
                       ("catch", _visitors_r1);
                       ("catch_all", _visitors_r2);
                       ("try_else", _visitors_r3);
                       ("ty", _visitors_r4);
                       ("err_ty", _visitors_r5);
                       ("catch_loc_", _visitors_r6);
                       ("else_loc_", _visitors_r7);
                       ("loc_", _visitors_r8);
                     ]

      method visit_Texpr_exclamation :
          _ -> expr -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_fconvert_to_result ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_bool env _visitors_fconvert_to_result
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_exclamation"
                [
                  ("expr", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("convert_to_result", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_constraint :
          _ -> expr -> typ -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_constraint"
                [
                  ("expr", _visitors_r0);
                  ("konstraint", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_pipe :
          _ -> expr -> pipe_rhs -> stype -> location -> S.t =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_flhs in
              let _visitors_r1 = self#visit_pipe_rhs env _visitors_frhs in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_pipe"
                [
                  ("lhs", _visitors_r0);
                  ("rhs", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_interp :
          _ -> interp_elem list -> stype -> location -> S.t =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               self#visit_list self#visit_interp_elem env _visitors_felems
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_interp"
               [
                 ("elems", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_guard :
          _ -> expr -> expr option -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fotherwise ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 =
                 self#visit_option self#visit_expr env _visitors_fotherwise
               in
               let _visitors_r2 = self#visit_expr env _visitors_fbody in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Texpr_guard"
                 [
                   ("cond", _visitors_r0);
                   ("otherwise", _visitors_r1);
                   ("body", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Texpr_guard_let :
          _ ->
          pat ->
          expr ->
          pat_binders ->
          match_case list option ->
          expr ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pat env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 =
                   self#visit_pat_binders env _visitors_fpat_binders
                 in
                 let _visitors_r3 =
                   self#visit_option
                     (self#visit_list self#visit_match_case)
                     env _visitors_fotherwise
                 in
                 let _visitors_r4 = self#visit_expr env _visitors_fbody in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Texpr_guard_let"
                   [
                     ("pat", _visitors_r0);
                     ("rhs", _visitors_r1);
                     ("pat_binders", _visitors_r2);
                     ("otherwise", _visitors_r3);
                     ("body", _visitors_r4);
                     ("ty", _visitors_r5);
                     ("loc_", _visitors_r6);
                   ]

      method visit_Texpr_is : _ -> expr -> pat -> pat_binders -> location -> S.t
          =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fpat ->
            fun _visitors_fpat_binders ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 =
                self#visit_pat_binders env _visitors_fpat_binders
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Texpr_is"
                [
                  ("expr", _visitors_r0);
                  ("pat", _visitors_r1);
                  ("pat_binders", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Texpr_array_as_view : _ -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_farray ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_farray in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_array_as_view"
               [
                 ("array", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_and : _ -> expr -> expr -> location -> S.t =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_and"
               [
                 ("lhs", _visitors_r0);
                 ("rhs", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Texpr_or : _ -> expr -> expr -> location -> S.t =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Texpr_or"
               [
                 ("lhs", _visitors_r0);
                 ("rhs", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_expr : _ -> expr -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Texpr_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 kind_ = _visitors_fkind_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_apply env _visitors_ffunc _visitors_fargs
                 _visitors_fty _visitors_fkind_ _visitors_floc_
           | Texpr_method
               {
                 meth = _visitors_fmeth;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 type_name = _visitors_ftype_name;
                 prim = _visitors_fprim;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_method env _visitors_fmeth _visitors_fty_args_
                 _visitors_farity_ _visitors_ftype_name _visitors_fprim
                 _visitors_fty _visitors_floc_
           | Texpr_unresolved_method
               {
                 trait_name = _visitors_ftrait_name;
                 method_name = _visitors_fmethod_name;
                 self_type = _visitors_fself_type;
                 arity_ = _visitors_farity_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_unresolved_method env _visitors_ftrait_name
                 _visitors_fmethod_name _visitors_fself_type _visitors_farity_
                 _visitors_fty _visitors_floc_
           | Texpr_ident
               {
                 id = _visitors_fid;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 kind = _visitors_fkind;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_ident env _visitors_fid _visitors_fty_args_
                 _visitors_farity_ _visitors_fkind _visitors_fty _visitors_floc_
           | Texpr_as
               {
                 expr = _visitors_fexpr;
                 trait = _visitors_ftrait;
                 ty = _visitors_fty;
                 is_implicit = _visitors_fis_implicit;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_as env _visitors_fexpr _visitors_ftrait
                 _visitors_fty _visitors_fis_implicit _visitors_floc_
           | Texpr_array
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 is_fixed_array = _visitors_fis_fixed_array;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array env _visitors_fexprs _visitors_fty
                 _visitors_fis_fixed_array _visitors_floc_
           | Texpr_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Texpr_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constr env _visitors_ftype_name
                 _visitors_fconstr _visitors_ftag _visitors_fty
                 _visitors_farity_ _visitors_floc_
           | Texpr_while
               {
                 loop_cond = _visitors_floop_cond;
                 loop_body = _visitors_floop_body;
                 while_else = _visitors_fwhile_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_while env _visitors_floop_cond
                 _visitors_floop_body _visitors_fwhile_else _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_function
               {
                 func = _visitors_ffunc;
                 ty = _visitors_fty;
                 is_raw_ = _visitors_fis_raw_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_function env _visitors_ffunc _visitors_fty
                 _visitors_fis_raw_ _visitors_floc_
           | Texpr_if
               {
                 cond = _visitors_fcond;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_if env _visitors_fcond _visitors_fifso
                 _visitors_fifnot _visitors_fty _visitors_floc_
           | Texpr_letfn
               {
                 binder = _visitors_fbinder;
                 fn = _visitors_ffn;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 is_rec = _visitors_fis_rec;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letfn env _visitors_fbinder _visitors_ffn
                 _visitors_fbody _visitors_fty _visitors_fis_rec _visitors_floc_
           | Texpr_letrec
               {
                 bindings = _visitors_fbindings;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letrec env _visitors_fbindings _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fbody _visitors_fty
                 _visitors_floc_
           | Texpr_sequence
               {
                 exprs = _visitors_fexprs;
                 last_expr = _visitors_flast_expr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_sequence env _visitors_fexprs
                 _visitors_flast_expr _visitors_fty _visitors_floc_
           | Texpr_tuple
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_tuple env _visitors_fexprs _visitors_fty
                 _visitors_floc_
           | Texpr_record
               {
                 type_name = _visitors_ftype_name;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record env _visitors_ftype_name
                 _visitors_ffields _visitors_fty _visitors_floc_
           | Texpr_record_update
               {
                 type_name = _visitors_ftype_name;
                 record = _visitors_frecord;
                 all_fields = _visitors_fall_fields;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record_update env _visitors_ftype_name
                 _visitors_frecord _visitors_fall_fields _visitors_ffields
                 _visitors_fty _visitors_floc_
           | Texpr_field
               {
                 record = _visitors_frecord;
                 accessor = _visitors_faccessor;
                 pos = _visitors_fpos;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_field env _visitors_frecord _visitors_faccessor
                 _visitors_fpos _visitors_fty _visitors_floc_
           | Texpr_mutate
               {
                 record = _visitors_frecord;
                 label = _visitors_flabel;
                 field = _visitors_ffield;
                 pos = _visitors_fpos;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_mutate env _visitors_frecord _visitors_flabel
                 _visitors_ffield _visitors_fpos _visitors_faugmented_by
                 _visitors_fty _visitors_floc_
           | Texpr_match
               {
                 expr = _visitors_fexpr;
                 cases = _visitors_fcases;
                 ty = _visitors_fty;
                 match_loc_ = _visitors_fmatch_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_match env _visitors_fexpr _visitors_fcases
                 _visitors_fty _visitors_fmatch_loc_ _visitors_floc_
           | Texpr_letmut
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letmut env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_assign
               {
                 var = _visitors_fvar;
                 expr = _visitors_fexpr;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_assign env _visitors_fvar _visitors_fexpr
                 _visitors_faugmented_by _visitors_fty _visitors_floc_
           | Texpr_hole
               {
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
                 kind = _visitors_fkind;
               } ->
               self#visit_Texpr_hole env _visitors_fty _visitors_floc_
                 _visitors_fkind
           | Texpr_unit { loc_ = _visitors_floc_ } ->
               self#visit_Texpr_unit env _visitors_floc_
           | Texpr_break
               {
                 arg = _visitors_farg;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_break env _visitors_farg _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_continue
               {
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_continue env _visitors_fargs _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_loop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_loop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_for
               {
                 binders = _visitors_fbinders;
                 condition = _visitors_fcondition;
                 steps = _visitors_fsteps;
                 body = _visitors_fbody;
                 for_else = _visitors_ffor_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_for env _visitors_fbinders _visitors_fcondition
                 _visitors_fsteps _visitors_fbody _visitors_ffor_else
                 _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_foreach
               {
                 binders = _visitors_fbinders;
                 elem_tys = _visitors_felem_tys;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 else_block = _visitors_felse_block;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_foreach env _visitors_fbinders
                 _visitors_felem_tys _visitors_fexpr _visitors_fbody
                 _visitors_felse_block _visitors_fty _visitors_flabel
                 _visitors_floc_
           | Texpr_return
               {
                 return_value = _visitors_freturn_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_return env _visitors_freturn_value _visitors_fty
                 _visitors_floc_
           | Texpr_raise
               {
                 error_value = _visitors_ferror_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_raise env _visitors_ferror_value _visitors_fty
                 _visitors_floc_
           | Texpr_try
               {
                 body = _visitors_fbody;
                 catch = _visitors_fcatch;
                 catch_all = _visitors_fcatch_all;
                 try_else = _visitors_ftry_else;
                 ty = _visitors_fty;
                 err_ty = _visitors_ferr_ty;
                 catch_loc_ = _visitors_fcatch_loc_;
                 else_loc_ = _visitors_felse_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_try env _visitors_fbody _visitors_fcatch
                 _visitors_fcatch_all _visitors_ftry_else _visitors_fty
                 _visitors_ferr_ty _visitors_fcatch_loc_ _visitors_felse_loc_
                 _visitors_floc_
           | Texpr_exclamation
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 convert_to_result = _visitors_fconvert_to_result;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_exclamation env _visitors_fexpr _visitors_fty
                 _visitors_fconvert_to_result _visitors_floc_
           | Texpr_constraint
               {
                 expr = _visitors_fexpr;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constraint env _visitors_fexpr
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Texpr_pipe
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_pipe env _visitors_flhs _visitors_frhs
                 _visitors_fty _visitors_floc_
           | Texpr_interp
               {
                 elems = _visitors_felems;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_interp env _visitors_felems _visitors_fty
                 _visitors_floc_
           | Texpr_guard
               {
                 cond = _visitors_fcond;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard env _visitors_fcond _visitors_fotherwise
                 _visitors_fbody _visitors_fty _visitors_floc_
           | Texpr_guard_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fotherwise _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_is
               {
                 expr = _visitors_fexpr;
                 pat = _visitors_fpat;
                 pat_binders = _visitors_fpat_binders;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_is env _visitors_fexpr _visitors_fpat
                 _visitors_fpat_binders _visitors_floc_
           | Texpr_array_as_view
               {
                 array = _visitors_farray;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array_as_view env _visitors_farray _visitors_fty
                 _visitors_floc_
           | Texpr_and
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_and env _visitors_flhs _visitors_frhs
                 _visitors_floc_
           | Texpr_or
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_or env _visitors_flhs _visitors_frhs
                 _visitors_floc_

      method visit_argument : _ -> argument -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
           let _visitors_r1 =
             self#visit_argument_kind env _visitors_this.arg_kind
           in
           self#visit_record env
             [ ("arg_value", _visitors_r0); ("arg_kind", _visitors_r1) ]

      method visit_Pipe_partial_apply :
          _ -> expr -> argument list -> location -> S.t =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ffunc in
             let _visitors_r1 =
               self#visit_list self#visit_argument env _visitors_fargs
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Pipe_partial_apply"
               [
                 ("func", _visitors_r0);
                 ("args", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Pipe_invalid : _ -> expr -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Pipe_invalid"
               [
                 ("expr", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_pipe_rhs : _ -> pipe_rhs -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Pipe_partial_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_partial_apply env _visitors_ffunc _visitors_fargs
                 _visitors_floc_
           | Pipe_invalid
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_invalid env _visitors_fexpr _visitors_fty
                 _visitors_floc_

      method visit_Interp_lit : _ -> string -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_string env _visitors_c0 in
           self#visit_inline_tuple env "Interp_lit" [ _visitors_r0 ]

      method visit_Interp_expr : _ -> expr -> expr -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fto_string ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_expr env _visitors_fto_string in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Interp_expr"
               [
                 ("expr", _visitors_r0);
                 ("to_string", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_interp_elem : _ -> interp_elem -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Interp_lit _visitors_c0 -> self#visit_Interp_lit env _visitors_c0
           | Interp_expr
               {
                 expr = _visitors_fexpr;
                 to_string = _visitors_fto_string;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Interp_expr env _visitors_fexpr _visitors_fto_string
                 _visitors_floc_

      method visit_Field_def :
          _ -> Syntax.label -> expr -> bool -> bool -> int -> S.t =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fexpr ->
            fun _visitors_fis_mut ->
             fun _visitors_fis_pun ->
              fun _visitors_fpos ->
               let _visitors_r0 = self#visit_label env _visitors_flabel in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 = self#visit_bool env _visitors_fis_mut in
               let _visitors_r3 = self#visit_bool env _visitors_fis_pun in
               let _visitors_r4 = self#visit_int env _visitors_fpos in
               self#visit_inline_record env "Field_def"
                 [
                   ("label", _visitors_r0);
                   ("expr", _visitors_r1);
                   ("is_mut", _visitors_r2);
                   ("is_pun", _visitors_r3);
                   ("pos", _visitors_r4);
                 ]

      method visit_field_def : _ -> field_def -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_def
               {
                 label = _visitors_flabel;
                 expr = _visitors_fexpr;
                 is_mut = _visitors_fis_mut;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_def env _visitors_flabel _visitors_fexpr
                 _visitors_fis_mut _visitors_fis_pun _visitors_fpos

      method visit_Field_pat : _ -> Syntax.label -> pat -> bool -> int -> S.t =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fpat ->
            fun _visitors_fis_pun ->
             fun _visitors_fpos ->
              let _visitors_r0 = self#visit_label env _visitors_flabel in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 = self#visit_bool env _visitors_fis_pun in
              let _visitors_r3 = self#visit_int env _visitors_fpos in
              self#visit_inline_record env "Field_pat"
                [
                  ("label", _visitors_r0);
                  ("pat", _visitors_r1);
                  ("is_pun", _visitors_r2);
                  ("pos", _visitors_r3);
                ]

      method visit_field_pat : _ -> field_pat -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_pat
               {
                 label = _visitors_flabel;
                 pat = _visitors_fpat;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_pat env _visitors_flabel _visitors_fpat
                 _visitors_fis_pun _visitors_fpos

      method visit_constr_pat_args : _ -> constr_pat_args -> S.t =
        fun env -> self#visit_list self#visit_constr_pat_arg env

      method visit_Constr_pat_arg :
          _ -> pat -> Syntax.argument_kind -> int -> S.t =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkind ->
            fun _visitors_fpos ->
             let _visitors_r0 = self#visit_pat env _visitors_fpat in
             let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
             let _visitors_r2 = self#visit_int env _visitors_fpos in
             self#visit_inline_record env "Constr_pat_arg"
               [
                 ("pat", _visitors_r0);
                 ("kind", _visitors_r1);
                 ("pos", _visitors_r2);
               ]

      method visit_constr_pat_arg : _ -> constr_pat_arg -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Constr_pat_arg
               {
                 pat = _visitors_fpat;
                 kind = _visitors_fkind;
                 pos = _visitors_fpos;
               } ->
               self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind
                 _visitors_fpos

      method visit_Param :
          _ -> binder -> typ option -> stype -> param_kind -> S.t =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                self#visit_option self#visit_typ env _visitors_fkonstraint
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_param_kind env _visitors_fkind in
              self#visit_inline_record env "Param"
                [
                  ("binder", _visitors_r0);
                  ("konstraint", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("kind", _visitors_r3);
                ]

      method visit_Discard_positional_param :
          _ -> stype -> typ option -> location -> S.t =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_fkonstraint ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 =
               self#visit_option self#visit_typ env _visitors_fkonstraint
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Discard_positional_param"
               [
                 ("ty", _visitors_r0);
                 ("konstraint", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_param : _ -> param -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Param
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 kind = _visitors_fkind;
               } ->
               self#visit_Param env _visitors_fbinder _visitors_fkonstraint
                 _visitors_fty _visitors_fkind
           | Discard_positional_param
               {
                 ty = _visitors_fty;
                 konstraint = _visitors_fkonstraint;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Discard_positional_param env _visitors_fty
                 _visitors_fkonstraint _visitors_floc_

      method visit_Positional : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Positional" []

      method visit_Labelled : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Labelled" []

      method visit_Optional : _ -> expr -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_expr env _visitors_c0 in
           self#visit_inline_tuple env "Optional" [ _visitors_r0 ]

      method visit_Autofill : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Autofill" []

      method visit_Question_optional : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Question_optional" []

      method visit_param_kind : _ -> param_kind -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Positional -> self#visit_Positional env
           | Labelled -> self#visit_Labelled env
           | Optional _visitors_c0 -> self#visit_Optional env _visitors_c0
           | Autofill -> self#visit_Autofill env
           | Question_optional -> self#visit_Question_optional env

      method visit_params : _ -> params -> S.t =
        fun env -> self#visit_list self#visit_param env

      method visit_fn : _ -> fn -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_params env _visitors_this.params in
           let _visitors_r1 =
             self#visit_location env _visitors_this.params_loc_
           in
           let _visitors_r2 = self#visit_expr env _visitors_this.body in
           let _visitors_r3 =
             self#visit_option
               (fun env ->
                 fun (_visitors_c0, _visitors_c1) ->
                  let _visitors_r0 = self#visit_typ env _visitors_c0 in
                  let _visitors_r1 = self#visit_error_typ env _visitors_c1 in
                  self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
               env _visitors_this.ret_constraint
           in
           let _visitors_r4 = self#visit_stype env _visitors_this.ty in
           let _visitors_r5 = self#visit_fn_kind env _visitors_this.kind_ in
           self#visit_record env
             [
               ("params", _visitors_r0);
               ("params_loc_", _visitors_r1);
               ("body", _visitors_r2);
               ("ret_constraint", _visitors_r3);
               ("ty", _visitors_r4);
               ("kind_", _visitors_r5);
             ]

      method visit_match_case : _ -> match_case -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_pat env _visitors_this.pat in
           let _visitors_r1 = self#visit_expr env _visitors_this.action in
           let _visitors_r2 =
             self#visit_option self#visit_expr env _visitors_this.guard
           in
           let _visitors_r3 =
             self#visit_pat_binders env _visitors_this.pat_binders
           in
           self#visit_record env
             [
               ("pat", _visitors_r0);
               ("action", _visitors_r1);
               ("guard", _visitors_r2);
               ("pat_binders", _visitors_r3);
             ]

      method visit_Error_typ : _ -> typ -> S.t =
        fun env ->
          fun _visitors_fty ->
           let _visitors_r0 = self#visit_typ env _visitors_fty in
           self#visit_inline_record env "Error_typ" [ ("ty", _visitors_r0) ]

      method visit_Default_error_typ : _ -> location -> S.t =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           self#visit_inline_record env "Default_error_typ"
             [ ("loc_", _visitors_r0) ]

      method visit_No_error_typ : _ -> S.t =
        fun env -> self#visit_inline_tuple env "No_error_typ" []

      method visit_error_typ : _ -> error_typ -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Error_typ { ty = _visitors_fty } ->
               self#visit_Error_typ env _visitors_fty
           | Default_error_typ { loc_ = _visitors_floc_ } ->
               self#visit_Default_error_typ env _visitors_floc_
           | No_error_typ -> self#visit_No_error_typ env

      method visit_Tany : _ -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            self#visit_inline_record env "Tany"
              [ ("ty", _visitors_r0); ("loc_", _visitors_r1) ]

      method visit_Tarrow :
          _ -> typ list -> typ -> error_typ -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_freturn ->
            fun _visitors_ferr_ty ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_typ env _visitors_fparams
               in
               let _visitors_r1 = self#visit_typ env _visitors_freturn in
               let _visitors_r2 = self#visit_error_typ env _visitors_ferr_ty in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Tarrow"
                 [
                   ("params", _visitors_r0);
                   ("return", _visitors_r1);
                   ("err_ty", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_T_tuple : _ -> typ list -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               self#visit_list self#visit_typ env _visitors_fparams
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "T_tuple"
               [
                 ("params", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Tname :
          _ -> constrid_loc -> typ list -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fparams ->
            fun _visitors_fty ->
             fun _visitors_fis_alias_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr
               in
               let _visitors_r1 =
                 self#visit_list self#visit_typ env _visitors_fparams
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 = self#visit_bool env _visitors_fis_alias_ in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Tname"
                 [
                   ("constr", _visitors_r0);
                   ("params", _visitors_r1);
                   ("ty", _visitors_r2);
                   ("is_alias_", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_Tobject :
          _ -> constrid_loc -> stype -> bool -> location -> S.t =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fty ->
            fun _visitors_fis_alias_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_constrid_loc env _visitors_fconstr
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 = self#visit_bool env _visitors_fis_alias_ in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tobject"
                [
                  ("constr", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("is_alias_", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_typ : _ -> typ -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tany { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tany env _visitors_fty _visitors_floc_
           | Tarrow
               {
                 params = _visitors_fparams;
                 return = _visitors_freturn;
                 err_ty = _visitors_ferr_ty;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tarrow env _visitors_fparams _visitors_freturn
                 _visitors_ferr_ty _visitors_fty _visitors_floc_
           | T_tuple
               {
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_T_tuple env _visitors_fparams _visitors_fty
                 _visitors_floc_
           | Tname
               {
                 constr = _visitors_fconstr;
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tname env _visitors_fconstr _visitors_fparams
                 _visitors_fty _visitors_fis_alias_ _visitors_floc_
           | Tobject
               {
                 constr = _visitors_fconstr;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tobject env _visitors_fconstr _visitors_fty
                 _visitors_fis_alias_ _visitors_floc_

      method visit_Tpat_alias : _ -> pat -> binder -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_falias ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_binder env _visitors_falias in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_alias"
                [
                  ("pat", _visitors_r0);
                  ("alias", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_any : _ -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            self#visit_inline_record env "Tpat_any"
              [ ("ty", _visitors_r0); ("loc_", _visitors_r1) ]

      method visit_Tpat_array : _ -> array_pattern -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_array_pattern env _visitors_fpats in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Tpat_array"
               [
                 ("pats", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Tpat_constant :
          _ -> constant -> stype -> var option -> location -> S.t =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_constant env _visitors_fc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                self#visit_option self#visit_var env _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_constant"
                [
                  ("c", _visitors_r0);
                  ("ty", _visitors_r1);
                  ("name_", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_pat_args ->
          constr_tag ->
          stype ->
          bool ->
          fn_arity ->
          stype list ->
          location ->
          S.t =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_fargs ->
             fun _visitors_ftag ->
              fun _visitors_fty ->
               fun _visitors_fused_error_subtyping ->
                fun _visitors_farity_ ->
                 fun _visitors_fall_args_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     self#visit_option self#visit_type_name env
                       _visitors_ftype_name
                   in
                   let _visitors_r1 =
                     self#visit_constr_name env _visitors_fconstr
                   in
                   let _visitors_r2 =
                     self#visit_constr_pat_args env _visitors_fargs
                   in
                   let _visitors_r3 =
                     self#visit_constr_tag env _visitors_ftag
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 =
                     self#visit_bool env _visitors_fused_error_subtyping
                   in
                   let _visitors_r6 =
                     self#visit_fn_arity env _visitors_farity_
                   in
                   let _visitors_r7 =
                     self#visit_list self#visit_stype env _visitors_fall_args_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   self#visit_inline_record env "Tpat_constr"
                     [
                       ("type_name", _visitors_r0);
                       ("constr", _visitors_r1);
                       ("args", _visitors_r2);
                       ("tag", _visitors_r3);
                       ("ty", _visitors_r4);
                       ("used_error_subtyping", _visitors_r5);
                       ("arity_", _visitors_r6);
                       ("all_args_", _visitors_r7);
                       ("loc_", _visitors_r8);
                     ]

      method visit_Tpat_or : _ -> pat -> pat -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fpat1 ->
           fun _visitors_fpat2 ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat1 in
              let _visitors_r1 = self#visit_pat env _visitors_fpat2 in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_or"
                [
                  ("pat1", _visitors_r0);
                  ("pat2", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_tuple : _ -> pat list -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               self#visit_list self#visit_pat env _visitors_fpats
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Tpat_tuple"
               [
                 ("pats", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Tpat_var : _ -> binder -> stype -> location -> S.t =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_binder env _visitors_fbinder in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Tpat_var"
               [
                 ("binder", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

      method visit_Tpat_record :
          _ -> field_pat list -> bool -> stype -> location -> S.t =
        fun env ->
          fun _visitors_ffields ->
           fun _visitors_fis_closed ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list self#visit_field_pat env _visitors_ffields
              in
              let _visitors_r1 = self#visit_bool env _visitors_fis_closed in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_record"
                [
                  ("fields", _visitors_r0);
                  ("is_closed", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_constraint : _ -> pat -> typ -> stype -> location -> S.t
          =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_constraint"
                [
                  ("pat", _visitors_r0);
                  ("konstraint", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_map :
          _ ->
          (constant * pat) list ->
          ident * stype * stype array ->
          stype ->
          location ->
          S.t =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fop_get_info_ ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_list
                  (fun env ->
                    fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 = self#visit_constant env _visitors_c0 in
                     let _visitors_r1 = self#visit_pat env _visitors_c1 in
                     self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                  env _visitors_felems
              in
              let _visitors_r1 =
                (fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                  let _visitors_r0 = self#visit_ident env _visitors_c0 in
                  let _visitors_r1 = self#visit_stype env _visitors_c1 in
                  let _visitors_r2 =
                    self#visit_array self#visit_stype env _visitors_c2
                  in
                  self#visit_tuple env
                    [ _visitors_r0; _visitors_r1; _visitors_r2 ])
                  _visitors_fop_get_info_
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              self#visit_inline_record env "Tpat_map"
                [
                  ("elems", _visitors_r0);
                  ("op_get_info_", _visitors_r1);
                  ("ty", _visitors_r2);
                  ("loc_", _visitors_r3);
                ]

      method visit_Tpat_range :
          _ -> pat -> pat -> bool -> stype -> location -> S.t =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_finclusive ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pat env _visitors_flhs in
               let _visitors_r1 = self#visit_pat env _visitors_frhs in
               let _visitors_r2 = self#visit_bool env _visitors_finclusive in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Tpat_range"
                 [
                   ("lhs", _visitors_r0);
                   ("rhs", _visitors_r1);
                   ("inclusive", _visitors_r2);
                   ("ty", _visitors_r3);
                   ("loc_", _visitors_r4);
                 ]

      method visit_pat : _ -> pat -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tpat_alias
               {
                 pat = _visitors_fpat;
                 alias = _visitors_falias;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_alias env _visitors_fpat _visitors_falias
                 _visitors_fty _visitors_floc_
           | Tpat_any { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tpat_any env _visitors_fty _visitors_floc_
           | Tpat_array
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_array env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Tpat_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 args = _visitors_fargs;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 used_error_subtyping = _visitors_fused_error_subtyping;
                 arity_ = _visitors_farity_;
                 all_args_ = _visitors_fall_args_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constr env _visitors_ftype_name _visitors_fconstr
                 _visitors_fargs _visitors_ftag _visitors_fty
                 _visitors_fused_error_subtyping _visitors_farity_
                 _visitors_fall_args_ _visitors_floc_
           | Tpat_or
               {
                 pat1 = _visitors_fpat1;
                 pat2 = _visitors_fpat2;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_or env _visitors_fpat1 _visitors_fpat2
                 _visitors_fty _visitors_floc_
           | Tpat_tuple
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_tuple env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_var
               {
                 binder = _visitors_fbinder;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_var env _visitors_fbinder _visitors_fty
                 _visitors_floc_
           | Tpat_record
               {
                 fields = _visitors_ffields;
                 is_closed = _visitors_fis_closed;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_record env _visitors_ffields _visitors_fis_closed
                 _visitors_fty _visitors_floc_
           | Tpat_constraint
               {
                 pat = _visitors_fpat;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constraint env _visitors_fpat
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Tpat_map
               {
                 elems = _visitors_felems;
                 op_get_info_ = _visitors_fop_get_info_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_map env _visitors_felems _visitors_fop_get_info_
                 _visitors_fty _visitors_floc_
           | Tpat_range
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 inclusive = _visitors_finclusive;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_range env _visitors_flhs _visitors_frhs
                 _visitors_finclusive _visitors_fty _visitors_floc_

      method visit_Closed : _ -> pat list -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_list self#visit_pat env _visitors_c0 in
           self#visit_inline_tuple env "Closed" [ _visitors_r0 ]

      method visit_Open :
          _ -> pat list -> pat list -> (binder * stype) option -> S.t =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            fun _visitors_c2 ->
             let _visitors_r0 =
               self#visit_list self#visit_pat env _visitors_c0
             in
             let _visitors_r1 =
               self#visit_list self#visit_pat env _visitors_c1
             in
             let _visitors_r2 =
               self#visit_option
                 (fun env ->
                   fun (_visitors_c0, _visitors_c1) ->
                    let _visitors_r0 = self#visit_binder env _visitors_c0 in
                    let _visitors_r1 = self#visit_stype env _visitors_c1 in
                    self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                 env _visitors_c2
             in
             self#visit_inline_tuple env "Open"
               [ _visitors_r0; _visitors_r1; _visitors_r2 ]

      method visit_array_pattern : _ -> array_pattern -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
           | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
               self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

      method visit_fun_decl : _ -> fun_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_fun_decl_kind env _visitors_this.kind
           in
           let _visitors_r1 = self#visit_binder env _visitors_this.fn_binder in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 = self#visit_bool env _visitors_this.is_pub in
           let _visitors_r4 =
             self#visit_tvar_env env _visitors_this.ty_params_
           in
           let _visitors_r5 =
             self#visit_list self#visit_type_name env
               _visitors_this.constraint_names
           in
           let _visitors_r6 = self#visit_docstring env _visitors_this.doc_ in
           let _visitors_r7 = self#visit_attributes env _visitors_this.attrs in
           self#visit_record env
             [
               ("kind", _visitors_r0);
               ("fn_binder", _visitors_r1);
               ("fn", _visitors_r2);
               ("is_pub", _visitors_r3);
               ("ty_params_", _visitors_r4);
               ("constraint_names", _visitors_r5);
               ("doc_", _visitors_r6);
               ("attrs", _visitors_r7);
             ]

      method visit_Fun_kind_regular : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Fun_kind_regular" []

      method visit_Fun_kind_method : _ -> type_name option -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             self#visit_option self#visit_type_name env _visitors_c0
           in
           self#visit_inline_tuple env "Fun_kind_method" [ _visitors_r0 ]

      method visit_Fun_kind_default_impl : _ -> type_name -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_type_name env _visitors_c0 in
           self#visit_inline_tuple env "Fun_kind_default_impl" [ _visitors_r0 ]

      method visit_Fun_kind_impl : _ -> typ -> type_name -> S.t =
        fun env ->
          fun _visitors_fself_ty ->
           fun _visitors_ftrait ->
            let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
            let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
            self#visit_inline_record env "Fun_kind_impl"
              [ ("self_ty", _visitors_r0); ("trait", _visitors_r1) ]

      method visit_fun_decl_kind : _ -> fun_decl_kind -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Fun_kind_regular -> self#visit_Fun_kind_regular env
           | Fun_kind_method _visitors_c0 ->
               self#visit_Fun_kind_method env _visitors_c0
           | Fun_kind_default_impl _visitors_c0 ->
               self#visit_Fun_kind_default_impl env _visitors_c0
           | Fun_kind_impl
               { self_ty = _visitors_fself_ty; trait = _visitors_ftrait } ->
               self#visit_Fun_kind_impl env _visitors_fself_ty _visitors_ftrait

      method visit_Timpl_expr :
          _ -> expr -> bool -> _ -> absolute_loc -> bool -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fis_main ->
            fun _visitors_fexpr_id ->
             fun _visitors_floc_ ->
              fun _visitors_fis_generated_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_bool env _visitors_fis_main in
               let _visitors_r2 = self#visit_expr_id env _visitors_fexpr_id in
               let _visitors_r3 = self#visit_absolute_loc env _visitors_floc_ in
               let _visitors_r4 =
                 self#visit_bool env _visitors_fis_generated_
               in
               self#visit_inline_record env "Timpl_expr"
                 [
                   ("expr", _visitors_r0);
                   ("is_main", _visitors_r1);
                   ("expr_id", _visitors_r2);
                   ("loc_", _visitors_r3);
                   ("is_generated_", _visitors_r4);
                 ]

      method visit_Timpl_fun_decl :
          _ -> fun_decl -> fn_arity -> absolute_loc -> bool -> S.t =
        fun env ->
          fun _visitors_ffun_decl ->
           fun _visitors_farity_ ->
            fun _visitors_floc_ ->
             fun _visitors_fis_generated_ ->
              let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
              let _visitors_r1 = self#visit_fn_arity env _visitors_farity_ in
              let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
              let _visitors_r3 = self#visit_bool env _visitors_fis_generated_ in
              self#visit_inline_record env "Timpl_fun_decl"
                [
                  ("fun_decl", _visitors_r0);
                  ("arity_", _visitors_r1);
                  ("loc_", _visitors_r2);
                  ("is_generated_", _visitors_r3);
                ]

      method visit_Timpl_letdef :
          _ ->
          binder ->
          typ option ->
          expr ->
          bool ->
          absolute_loc ->
          docstring ->
          attributes ->
          bool ->
          S.t =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fis_pub ->
              fun _visitors_floc_ ->
               fun _visitors_fdoc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fis_generated_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_option self#visit_typ env _visitors_fkonstraint
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 = self#visit_bool env _visitors_fis_pub in
                  let _visitors_r4 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  let _visitors_r6 =
                    self#visit_attributes env _visitors_fattrs
                  in
                  let _visitors_r7 =
                    self#visit_bool env _visitors_fis_generated_
                  in
                  self#visit_inline_record env "Timpl_letdef"
                    [
                      ("binder", _visitors_r0);
                      ("konstraint", _visitors_r1);
                      ("expr", _visitors_r2);
                      ("is_pub", _visitors_r3);
                      ("loc_", _visitors_r4);
                      ("doc_", _visitors_r5);
                      ("attrs", _visitors_r6);
                      ("is_generated_", _visitors_r7);
                    ]

      method visit_Timpl_stub_decl : _ -> stub_decl -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_stub_decl env _visitors_c0 in
           self#visit_inline_tuple env "Timpl_stub_decl" [ _visitors_r0 ]

      method visit_impl : _ -> impl -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Timpl_expr
               {
                 expr = _visitors_fexpr;
                 is_main = _visitors_fis_main;
                 expr_id = _visitors_fexpr_id;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_expr env _visitors_fexpr _visitors_fis_main
                 _visitors_fexpr_id _visitors_floc_ _visitors_fis_generated_
           | Timpl_fun_decl
               {
                 fun_decl = _visitors_ffun_decl;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_fun_decl env _visitors_ffun_decl
                 _visitors_farity_ _visitors_floc_ _visitors_fis_generated_
           | Timpl_letdef
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 is_pub = _visitors_fis_pub;
                 loc_ = _visitors_floc_;
                 doc_ = _visitors_fdoc_;
                 attrs = _visitors_fattrs;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_letdef env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fis_pub
                 _visitors_floc_ _visitors_fdoc_ _visitors_fattrs
                 _visitors_fis_generated_
           | Timpl_stub_decl _visitors_c0 ->
               self#visit_Timpl_stub_decl env _visitors_c0

      method visit_impls : _ -> impls -> S.t =
        fun env -> self#visit_list self#visit_impl env

      method visit_type_decl : _ -> type_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.td_binder
           in
           let _visitors_r1 =
             self#visit_tvar_env env _visitors_this.td_params
           in
           let _visitors_r2 = self#visit_type_desc env _visitors_this.td_desc in
           let _visitors_r3 = self#visit_visibility env _visitors_this.td_vis in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.td_loc_
           in
           let _visitors_r5 = self#visit_docstring env _visitors_this.td_doc_ in
           let _visitors_r6 =
             self#visit_list self#visit_type_name env
               _visitors_this.td_deriving_
           in
           self#visit_record env
             [
               ("td_binder", _visitors_r0);
               ("td_params", _visitors_r1);
               ("td_desc", _visitors_r2);
               ("td_vis", _visitors_r3);
               ("td_loc_", _visitors_r4);
               ("td_doc_", _visitors_r5);
               ("td_deriving_", _visitors_r6);
             ]

      method visit_No_payload : _ -> S.t =
        fun env -> self#visit_inline_tuple env "No_payload" []

      method visit_Single_payload : _ -> typ -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           self#visit_inline_tuple env "Single_payload" [ _visitors_r0 ]

      method visit_Enum_payload : _ -> constr_decl list -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             self#visit_list self#visit_constr_decl env _visitors_c0
           in
           self#visit_inline_tuple env "Enum_payload" [ _visitors_r0 ]

      method visit_exception_decl : _ -> exception_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | No_payload -> self#visit_No_payload env
           | Single_payload _visitors_c0 ->
               self#visit_Single_payload env _visitors_c0
           | Enum_payload _visitors_c0 ->
               self#visit_Enum_payload env _visitors_c0

      method visit_Td_abstract : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Td_abstract" []

      method visit_Td_error : _ -> exception_decl -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
           self#visit_inline_tuple env "Td_error" [ _visitors_r0 ]

      method visit_Td_newtype : _ -> typ -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           self#visit_inline_tuple env "Td_newtype" [ _visitors_r0 ]

      method visit_Td_variant : _ -> constr_decl list -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             self#visit_list self#visit_constr_decl env _visitors_c0
           in
           self#visit_inline_tuple env "Td_variant" [ _visitors_r0 ]

      method visit_Td_record : _ -> field_decl list -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             self#visit_list self#visit_field_decl env _visitors_c0
           in
           self#visit_inline_tuple env "Td_record" [ _visitors_r0 ]

      method visit_Td_alias : _ -> typ -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           self#visit_inline_tuple env "Td_alias" [ _visitors_r0 ]

      method visit_type_desc : _ -> type_desc -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Td_abstract -> self#visit_Td_abstract env
           | Td_error _visitors_c0 -> self#visit_Td_error env _visitors_c0
           | Td_newtype _visitors_c0 -> self#visit_Td_newtype env _visitors_c0
           | Td_variant _visitors_c0 -> self#visit_Td_variant env _visitors_c0
           | Td_record _visitors_c0 -> self#visit_Td_record env _visitors_c0
           | Td_alias _visitors_c0 -> self#visit_Td_alias env _visitors_c0

      method visit_constr_decl_arg : _ -> constr_decl_arg -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_typ env _visitors_this.carg_typ in
           let _visitors_r1 = self#visit_bool env _visitors_this.carg_mut in
           let _visitors_r2 =
             self#visit_option self#visit_label env _visitors_this.carg_label
           in
           self#visit_record env
             [
               ("carg_typ", _visitors_r0);
               ("carg_mut", _visitors_r1);
               ("carg_label", _visitors_r2);
             ]

      method visit_constr_decl : _ -> constr_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.constr_name in
           let _visitors_r1 =
             self#visit_constr_tag env _visitors_this.constr_tag
           in
           let _visitors_r2 =
             self#visit_list self#visit_constr_decl_arg env
               _visitors_this.constr_args
           in
           let _visitors_r3 =
             self#visit_fn_arity env _visitors_this.constr_arity_
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.constr_loc_
           in
           self#visit_record env
             [
               ("constr_name", _visitors_r0);
               ("constr_tag", _visitors_r1);
               ("constr_args", _visitors_r2);
               ("constr_arity_", _visitors_r3);
               ("constr_loc_", _visitors_r4);
             ]

      method visit_field_decl : _ -> field_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.field_label in
           let _visitors_r1 = self#visit_typ env _visitors_this.field_typ in
           let _visitors_r2 = self#visit_bool env _visitors_this.field_mut in
           let _visitors_r3 =
             self#visit_type_component_visibility env _visitors_this.field_vis
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.field_loc_
           in
           self#visit_record env
             [
               ("field_label", _visitors_r0);
               ("field_typ", _visitors_r1);
               ("field_mut", _visitors_r2);
               ("field_vis", _visitors_r3);
               ("field_loc_", _visitors_r4);
             ]

      method visit_trait_decl : _ -> trait_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_name
           in
           let _visitors_r1 =
             self#visit_list self#visit_type_name env
               _visitors_this.trait_supers
           in
           let _visitors_r2 =
             self#visit_list self#visit_method_decl env
               _visitors_this.trait_methods
           in
           let _visitors_r3 =
             self#visit_visibility env _visitors_this.trait_vis
           in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.trait_loc_
           in
           let _visitors_r5 =
             self#visit_docstring env _visitors_this.trait_doc_
           in
           let _visitors_r6 =
             self#visit_attributes env _visitors_this.trait_attrs
           in
           self#visit_record env
             [
               ("trait_name", _visitors_r0);
               ("trait_supers", _visitors_r1);
               ("trait_methods", _visitors_r2);
               ("trait_vis", _visitors_r3);
               ("trait_loc_", _visitors_r4);
               ("trait_doc_", _visitors_r5);
               ("trait_attrs", _visitors_r6);
             ]

      method visit_trait_alias_decl : _ -> trait_alias_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_alias_name
           in
           let _visitors_r1 =
             self#visit_type_name env _visitors_this.trait_alias_target
           in
           let _visitors_r2 =
             self#visit_bool env _visitors_this.trait_alias_is_pub
           in
           let _visitors_r3 =
             self#visit_absolute_loc env _visitors_this.trait_alias_loc_
           in
           let _visitors_r4 =
             self#visit_docstring env _visitors_this.trait_alias_doc_
           in
           self#visit_record env
             [
               ("trait_alias_name", _visitors_r0);
               ("trait_alias_target", _visitors_r1);
               ("trait_alias_is_pub", _visitors_r2);
               ("trait_alias_loc_", _visitors_r3);
               ("trait_alias_doc_", _visitors_r4);
             ]

      method visit_method_decl : _ -> method_decl -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_syntax_binder env _visitors_this.method_name
           in
           let _visitors_r1 =
             self#visit_list
               (fun env ->
                 fun (_visitors_c0, _visitors_c1) ->
                  let _visitors_r0 =
                    self#visit_option self#visit_label env _visitors_c0
                  in
                  let _visitors_r1 = self#visit_typ env _visitors_c1 in
                  self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
               env _visitors_this.method_params
           in
           let _visitors_r2 =
             self#visit_option self#visit_typ env _visitors_this.method_ret
           in
           let _visitors_r3 =
             self#visit_option self#visit_typ env _visitors_this.method_err
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.method_loc_
           in
           self#visit_record env
             [
               ("method_name", _visitors_r0);
               ("method_params", _visitors_r1);
               ("method_ret", _visitors_r2);
               ("method_err", _visitors_r3);
               ("method_loc_", _visitors_r4);
             ]

      method visit_Output :
          _ ->
          impls ->
          type_decl list ->
          trait_decl list ->
          trait_alias_decl list ->
          S.t =
        fun env ->
          fun _visitors_fvalue_defs ->
           fun _visitors_ftype_defs ->
            fun _visitors_ftrait_defs ->
             fun _visitors_ftrait_alias ->
              let _visitors_r0 = self#visit_impls env _visitors_fvalue_defs in
              let _visitors_r1 =
                self#visit_list self#visit_type_decl env _visitors_ftype_defs
              in
              let _visitors_r2 =
                self#visit_list self#visit_trait_decl env _visitors_ftrait_defs
              in
              let _visitors_r3 =
                self#visit_list self#visit_trait_alias_decl env
                  _visitors_ftrait_alias
              in
              self#visit_inline_record env "Output"
                [
                  ("value_defs", _visitors_r0);
                  ("type_defs", _visitors_r1);
                  ("trait_defs", _visitors_r2);
                  ("trait_alias", _visitors_r3);
                ]

      method visit_output : _ -> output -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Output
               {
                 value_defs = _visitors_fvalue_defs;
                 type_defs = _visitors_ftype_defs;
                 trait_defs = _visitors_ftrait_defs;
                 trait_alias = _visitors_ftrait_alias;
               } ->
               self#visit_Output env _visitors_fvalue_defs _visitors_ftype_defs
                 _visitors_ftrait_defs _visitors_ftrait_alias
    end

  [@@@VISITORS.END]
end

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] iter =
    object (self : 'self)
      inherit [_] iterbase

      method visit_stub_decl : _ -> stub_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_binder env _visitors_this.binder in
           let _visitors_r1 = self#visit_params env _visitors_this.params in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_typ env) t
               | None -> ())
               _visitors_this.ret
           in
           let _visitors_r3 =
             self#visit_stub_body env _visitors_this.func_stubs
           in
           let _visitors_r4 =
             (fun _visitors_this -> ()) _visitors_this.is_pub
           in
           let _visitors_r5 = self#visit_fn_arity env _visitors_this.arity_ in
           let _visitors_r6 =
             self#visit_fun_decl_kind env _visitors_this.kind_
           in
           let _visitors_r7 = self#visit_absolute_loc env _visitors_this.loc_ in
           let _visitors_r8 = self#visit_docstring env _visitors_this.doc_ in
           ()

      method visit_Intrinsic : _ -> unit = fun env -> ()

      method visit_Func_stub : _ -> func_stubs -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
           ()

      method visit_stub_body : _ -> stub_body -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Intrinsic -> self#visit_Intrinsic env
           | Func_stub _visitors_c0 -> self#visit_Func_stub env _visitors_c0

      method visit_Texpr_apply :
          _ -> expr -> argument list -> stype -> apply_kind -> location -> unit
          =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_fty ->
             fun _visitors_fkind_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_ffunc in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_argument env))
                   _visitors_fargs
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 = self#visit_apply_kind env _visitors_fkind_ in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_method :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          type_name ->
          Primitive.prim option ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_fmeth ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_ftype_name ->
              fun _visitors_fprim ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fmeth in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_arr.iter _visitors_this (self#visit_stype env))
                     _visitors_fty_args_
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_fn_arity env) t
                     | None -> ())
                     _visitors_farity_
                 in
                 let _visitors_r3 =
                   self#visit_type_name env _visitors_ftype_name
                 in
                 let _visitors_r4 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_prim env) t
                     | None -> ())
                     _visitors_fprim
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 ()

      method visit_Texpr_unresolved_method :
          _ ->
          type_name ->
          string ->
          stype ->
          fn_arity option ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_ftrait_name ->
           fun _visitors_fmethod_name ->
            fun _visitors_fself_type ->
             fun _visitors_farity_ ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_type_name env _visitors_ftrait_name
                in
                let _visitors_r1 =
                  (fun _visitors_this -> ()) _visitors_fmethod_name
                in
                let _visitors_r2 = self#visit_stype env _visitors_fself_type in
                let _visitors_r3 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_fn_arity env) t
                    | None -> ())
                    _visitors_farity_
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_ident :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          value_kind ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_fid ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_fkind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_arr.iter _visitors_this (self#visit_stype env))
                    _visitors_fty_args_
                in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_fn_arity env) t
                    | None -> ())
                    _visitors_farity_
                in
                let _visitors_r3 = (fun _visitors_this -> ()) _visitors_fkind in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_as :
          _ -> expr -> type_name -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftrait ->
            fun _visitors_fty ->
             fun _visitors_fis_implicit ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 (fun _visitors_this -> ()) _visitors_fis_implicit
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_array :
          _ -> expr list -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_fis_fixed_array ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> ()) _visitors_fis_fixed_array
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_constant :
          _ -> constant -> stype -> var option -> location -> unit =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_var env) t
                  | None -> ())
                  _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_tag ->
          stype ->
          fn_arity ->
          location ->
          unit =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_ftag ->
             fun _visitors_fty ->
              fun _visitors_farity_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_type_name env) t
                    | None -> ())
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  self#visit_constr_name env _visitors_fconstr
                in
                let _visitors_r2 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 = self#visit_fn_arity env _visitors_farity_ in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_while :
          _ ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          unit =
        fun env ->
          fun _visitors_floop_cond ->
           fun _visitors_floop_body ->
            fun _visitors_fwhile_else ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_expr env) t
                    | None -> ())
                    _visitors_fwhile_else
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_loop_label_binder env) t
                    | None -> ())
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_function : _ -> fn -> stype -> bool -> location -> unit
          =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fty ->
            fun _visitors_fis_raw_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_fn env _visitors_ffunc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> ()) _visitors_fis_raw_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_if :
          _ -> expr -> expr -> expr option -> stype -> location -> unit =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 = self#visit_expr env _visitors_fifso in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_expr env) t
                   | None -> ())
                   _visitors_fifnot
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_letfn :
          _ -> binder -> fn -> expr -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_ffn ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_fis_rec ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_fn env _visitors_ffn in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this -> ()) _visitors_fis_rec
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_letrec :
          _ -> (binder * fn) list -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_fbindings ->
           fun _visitors_fbody ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 = self#visit_binder env _visitors_c0 in
                      let _visitors_r1 = self#visit_fn env _visitors_c1 in
                      ()))
                  _visitors_fbindings
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_let :
          _ -> pat -> expr -> pat_binders -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pat env _visitors_fpat in
                let _visitors_r1 = self#visit_expr env _visitors_frhs in
                let _visitors_r2 =
                  self#visit_pat_binders env _visitors_fpat_binders
                in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_sequence :
          _ -> expr list -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_tuple : _ -> expr list -> stype -> location -> unit =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                 _visitors_fexprs
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_record :
          _ -> type_name option -> field_def list -> stype -> location -> unit =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_ffields ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_type_name env) t
                  | None -> ())
                  _visitors_ftype_name
              in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                  _visitors_ffields
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_record_update :
          _ ->
          type_name option ->
          expr ->
          field_info list ->
          field_def list ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_frecord ->
            fun _visitors_fall_fields ->
             fun _visitors_ffields ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_type_name env) t
                    | None -> ())
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_field_info env))
                    _visitors_fall_fields
                in
                let _visitors_r3 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_field :
          _ -> expr -> Syntax.accessor -> int -> stype -> location -> unit =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_faccessor ->
            fun _visitors_fpos ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_fpos in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_mutate :
          _ ->
          expr ->
          Syntax.label ->
          expr ->
          int ->
          expr option ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_flabel ->
            fun _visitors_ffield ->
             fun _visitors_fpos ->
              fun _visitors_faugmented_by ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 = self#visit_label env _visitors_flabel in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 = (fun _visitors_this -> ()) _visitors_fpos in
                 let _visitors_r4 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_faugmented_by
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 ()

      method visit_Texpr_match :
          _ -> expr -> match_case list -> stype -> location -> location -> unit
          =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fcases ->
            fun _visitors_fty ->
             fun _visitors_fmatch_loc_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_match_case env))
                   _visitors_fcases
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 self#visit_location env _visitors_fmatch_loc_
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_letmut :
          _ -> binder -> typ option -> expr -> expr -> stype -> location -> unit
          =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_typ env) t
                    | None -> ())
                    _visitors_fkonstraint
                in
                let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_assign :
          _ -> var -> expr -> expr option -> stype -> location -> unit =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fexpr ->
            fun _visitors_faugmented_by ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fvar in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_expr env) t
                   | None -> ())
                   _visitors_faugmented_by
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_hole : _ -> stype -> location -> syntax_hole -> unit =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            fun _visitors_fkind ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 = self#visit_location env _visitors_floc_ in
             let _visitors_r2 = self#visit_syntax_hole env _visitors_fkind in
             ()

      method visit_Texpr_unit : _ -> location -> unit =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           ()

      method visit_Texpr_break :
          _ -> expr option -> stype -> loop_label option -> location -> unit =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_expr env) t
                  | None -> ())
                  _visitors_farg
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_loop_label env) t
                  | None -> ())
                  _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_continue :
          _ -> expr list -> stype -> loop_label option -> location -> unit =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                  _visitors_fargs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_loop_label env) t
                  | None -> ())
                  _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_loop :
          _ ->
          param list ->
          expr ->
          expr list ->
          stype ->
          loop_label_binder option ->
          location ->
          unit =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_param env))
                    _visitors_fparams
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_loop_label_binder env) t
                    | None -> ())
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                ()

      method visit_Texpr_for :
          _ ->
          (binder * expr) list ->
          expr option ->
          (var * expr) list ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          unit =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_fcondition ->
            fun _visitors_fsteps ->
             fun _visitors_fbody ->
              fun _visitors_ffor_else ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this
                        ~f:(fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            self#visit_binder env _visitors_c0
                          in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          ()))
                      _visitors_fbinders
                  in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_expr env) t
                      | None -> ())
                      _visitors_fcondition
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this
                        ~f:(fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 = self#visit_var env _visitors_c0 in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          ()))
                      _visitors_fsteps
                  in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_expr env) t
                      | None -> ())
                      _visitors_ffor_else
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_loop_label_binder env) t
                      | None -> ())
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  ()

      method visit_Texpr_foreach :
          _ ->
          binder option list ->
          stype list ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          unit =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_felem_tys ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_felse_block ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(fun _visitors_this ->
                          match _visitors_this with
                          | Some t -> (self#visit_binder env) t
                          | None -> ()))
                      _visitors_fbinders
                  in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_stype env))
                      _visitors_felem_tys
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_expr env) t
                      | None -> ())
                      _visitors_felse_block
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_loop_label_binder env) t
                      | None -> ())
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  ()

      method visit_Texpr_return : _ -> expr option -> stype -> location -> unit
          =
        fun env ->
          fun _visitors_freturn_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_expr env) t
                 | None -> ())
                 _visitors_freturn_value
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_raise : _ -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_ferror_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ferror_value in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_try :
          _ ->
          expr ->
          match_case list ->
          bool ->
          match_case list option ->
          stype ->
          stype ->
          location ->
          location ->
          location ->
          unit =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fcatch ->
            fun _visitors_fcatch_all ->
             fun _visitors_ftry_else ->
              fun _visitors_fty ->
               fun _visitors_ferr_ty ->
                fun _visitors_fcatch_loc_ ->
                 fun _visitors_felse_loc_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fbody in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this
                         ~f:(self#visit_match_case env))
                       _visitors_fcatch
                   in
                   let _visitors_r2 =
                     (fun _visitors_this -> ()) _visitors_fcatch_all
                   in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           (fun _visitors_this ->
                             Basic_lst.iter _visitors_this
                               ~f:(self#visit_match_case env))
                             t
                       | None -> ())
                       _visitors_ftry_else
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 = self#visit_stype env _visitors_ferr_ty in
                   let _visitors_r6 =
                     self#visit_location env _visitors_fcatch_loc_
                   in
                   let _visitors_r7 =
                     self#visit_location env _visitors_felse_loc_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   ()

      method visit_Texpr_exclamation :
          _ -> expr -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_fconvert_to_result ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> ()) _visitors_fconvert_to_result
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_constraint :
          _ -> expr -> typ -> stype -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_pipe :
          _ -> expr -> pipe_rhs -> stype -> location -> unit =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_flhs in
              let _visitors_r1 = self#visit_pipe_rhs env _visitors_frhs in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_interp :
          _ -> interp_elem list -> stype -> location -> unit =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_interp_elem env))
                 _visitors_felems
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_guard :
          _ -> expr -> expr option -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fotherwise ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_expr env) t
                   | None -> ())
                   _visitors_fotherwise
               in
               let _visitors_r2 = self#visit_expr env _visitors_fbody in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Texpr_guard_let :
          _ ->
          pat ->
          expr ->
          pat_binders ->
          match_case list option ->
          expr ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pat env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 =
                   self#visit_pat_binders env _visitors_fpat_binders
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t ->
                         (fun _visitors_this ->
                           Basic_lst.iter _visitors_this
                             ~f:(self#visit_match_case env))
                           t
                     | None -> ())
                     _visitors_fotherwise
                 in
                 let _visitors_r4 = self#visit_expr env _visitors_fbody in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 ()

      method visit_Texpr_is :
          _ -> expr -> pat -> pat_binders -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fpat ->
            fun _visitors_fpat_binders ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 =
                self#visit_pat_binders env _visitors_fpat_binders
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Texpr_array_as_view : _ -> expr -> stype -> location -> unit
          =
        fun env ->
          fun _visitors_farray ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_farray in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_and : _ -> expr -> expr -> location -> unit =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Texpr_or : _ -> expr -> expr -> location -> unit =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_expr : _ -> expr -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Texpr_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 kind_ = _visitors_fkind_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_apply env _visitors_ffunc _visitors_fargs
                 _visitors_fty _visitors_fkind_ _visitors_floc_
           | Texpr_method
               {
                 meth = _visitors_fmeth;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 type_name = _visitors_ftype_name;
                 prim = _visitors_fprim;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_method env _visitors_fmeth _visitors_fty_args_
                 _visitors_farity_ _visitors_ftype_name _visitors_fprim
                 _visitors_fty _visitors_floc_
           | Texpr_unresolved_method
               {
                 trait_name = _visitors_ftrait_name;
                 method_name = _visitors_fmethod_name;
                 self_type = _visitors_fself_type;
                 arity_ = _visitors_farity_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_unresolved_method env _visitors_ftrait_name
                 _visitors_fmethod_name _visitors_fself_type _visitors_farity_
                 _visitors_fty _visitors_floc_
           | Texpr_ident
               {
                 id = _visitors_fid;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 kind = _visitors_fkind;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_ident env _visitors_fid _visitors_fty_args_
                 _visitors_farity_ _visitors_fkind _visitors_fty _visitors_floc_
           | Texpr_as
               {
                 expr = _visitors_fexpr;
                 trait = _visitors_ftrait;
                 ty = _visitors_fty;
                 is_implicit = _visitors_fis_implicit;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_as env _visitors_fexpr _visitors_ftrait
                 _visitors_fty _visitors_fis_implicit _visitors_floc_
           | Texpr_array
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 is_fixed_array = _visitors_fis_fixed_array;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array env _visitors_fexprs _visitors_fty
                 _visitors_fis_fixed_array _visitors_floc_
           | Texpr_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Texpr_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constr env _visitors_ftype_name
                 _visitors_fconstr _visitors_ftag _visitors_fty
                 _visitors_farity_ _visitors_floc_
           | Texpr_while
               {
                 loop_cond = _visitors_floop_cond;
                 loop_body = _visitors_floop_body;
                 while_else = _visitors_fwhile_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_while env _visitors_floop_cond
                 _visitors_floop_body _visitors_fwhile_else _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_function
               {
                 func = _visitors_ffunc;
                 ty = _visitors_fty;
                 is_raw_ = _visitors_fis_raw_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_function env _visitors_ffunc _visitors_fty
                 _visitors_fis_raw_ _visitors_floc_
           | Texpr_if
               {
                 cond = _visitors_fcond;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_if env _visitors_fcond _visitors_fifso
                 _visitors_fifnot _visitors_fty _visitors_floc_
           | Texpr_letfn
               {
                 binder = _visitors_fbinder;
                 fn = _visitors_ffn;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 is_rec = _visitors_fis_rec;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letfn env _visitors_fbinder _visitors_ffn
                 _visitors_fbody _visitors_fty _visitors_fis_rec _visitors_floc_
           | Texpr_letrec
               {
                 bindings = _visitors_fbindings;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letrec env _visitors_fbindings _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fbody _visitors_fty
                 _visitors_floc_
           | Texpr_sequence
               {
                 exprs = _visitors_fexprs;
                 last_expr = _visitors_flast_expr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_sequence env _visitors_fexprs
                 _visitors_flast_expr _visitors_fty _visitors_floc_
           | Texpr_tuple
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_tuple env _visitors_fexprs _visitors_fty
                 _visitors_floc_
           | Texpr_record
               {
                 type_name = _visitors_ftype_name;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record env _visitors_ftype_name
                 _visitors_ffields _visitors_fty _visitors_floc_
           | Texpr_record_update
               {
                 type_name = _visitors_ftype_name;
                 record = _visitors_frecord;
                 all_fields = _visitors_fall_fields;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record_update env _visitors_ftype_name
                 _visitors_frecord _visitors_fall_fields _visitors_ffields
                 _visitors_fty _visitors_floc_
           | Texpr_field
               {
                 record = _visitors_frecord;
                 accessor = _visitors_faccessor;
                 pos = _visitors_fpos;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_field env _visitors_frecord _visitors_faccessor
                 _visitors_fpos _visitors_fty _visitors_floc_
           | Texpr_mutate
               {
                 record = _visitors_frecord;
                 label = _visitors_flabel;
                 field = _visitors_ffield;
                 pos = _visitors_fpos;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_mutate env _visitors_frecord _visitors_flabel
                 _visitors_ffield _visitors_fpos _visitors_faugmented_by
                 _visitors_fty _visitors_floc_
           | Texpr_match
               {
                 expr = _visitors_fexpr;
                 cases = _visitors_fcases;
                 ty = _visitors_fty;
                 match_loc_ = _visitors_fmatch_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_match env _visitors_fexpr _visitors_fcases
                 _visitors_fty _visitors_fmatch_loc_ _visitors_floc_
           | Texpr_letmut
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letmut env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_assign
               {
                 var = _visitors_fvar;
                 expr = _visitors_fexpr;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_assign env _visitors_fvar _visitors_fexpr
                 _visitors_faugmented_by _visitors_fty _visitors_floc_
           | Texpr_hole
               {
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
                 kind = _visitors_fkind;
               } ->
               self#visit_Texpr_hole env _visitors_fty _visitors_floc_
                 _visitors_fkind
           | Texpr_unit { loc_ = _visitors_floc_ } ->
               self#visit_Texpr_unit env _visitors_floc_
           | Texpr_break
               {
                 arg = _visitors_farg;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_break env _visitors_farg _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_continue
               {
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_continue env _visitors_fargs _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_loop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_loop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_for
               {
                 binders = _visitors_fbinders;
                 condition = _visitors_fcondition;
                 steps = _visitors_fsteps;
                 body = _visitors_fbody;
                 for_else = _visitors_ffor_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_for env _visitors_fbinders _visitors_fcondition
                 _visitors_fsteps _visitors_fbody _visitors_ffor_else
                 _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_foreach
               {
                 binders = _visitors_fbinders;
                 elem_tys = _visitors_felem_tys;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 else_block = _visitors_felse_block;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_foreach env _visitors_fbinders
                 _visitors_felem_tys _visitors_fexpr _visitors_fbody
                 _visitors_felse_block _visitors_fty _visitors_flabel
                 _visitors_floc_
           | Texpr_return
               {
                 return_value = _visitors_freturn_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_return env _visitors_freturn_value _visitors_fty
                 _visitors_floc_
           | Texpr_raise
               {
                 error_value = _visitors_ferror_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_raise env _visitors_ferror_value _visitors_fty
                 _visitors_floc_
           | Texpr_try
               {
                 body = _visitors_fbody;
                 catch = _visitors_fcatch;
                 catch_all = _visitors_fcatch_all;
                 try_else = _visitors_ftry_else;
                 ty = _visitors_fty;
                 err_ty = _visitors_ferr_ty;
                 catch_loc_ = _visitors_fcatch_loc_;
                 else_loc_ = _visitors_felse_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_try env _visitors_fbody _visitors_fcatch
                 _visitors_fcatch_all _visitors_ftry_else _visitors_fty
                 _visitors_ferr_ty _visitors_fcatch_loc_ _visitors_felse_loc_
                 _visitors_floc_
           | Texpr_exclamation
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 convert_to_result = _visitors_fconvert_to_result;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_exclamation env _visitors_fexpr _visitors_fty
                 _visitors_fconvert_to_result _visitors_floc_
           | Texpr_constraint
               {
                 expr = _visitors_fexpr;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constraint env _visitors_fexpr
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Texpr_pipe
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_pipe env _visitors_flhs _visitors_frhs
                 _visitors_fty _visitors_floc_
           | Texpr_interp
               {
                 elems = _visitors_felems;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_interp env _visitors_felems _visitors_fty
                 _visitors_floc_
           | Texpr_guard
               {
                 cond = _visitors_fcond;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard env _visitors_fcond _visitors_fotherwise
                 _visitors_fbody _visitors_fty _visitors_floc_
           | Texpr_guard_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fotherwise _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_is
               {
                 expr = _visitors_fexpr;
                 pat = _visitors_fpat;
                 pat_binders = _visitors_fpat_binders;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_is env _visitors_fexpr _visitors_fpat
                 _visitors_fpat_binders _visitors_floc_
           | Texpr_array_as_view
               {
                 array = _visitors_farray;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array_as_view env _visitors_farray _visitors_fty
                 _visitors_floc_
           | Texpr_and
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_and env _visitors_flhs _visitors_frhs
                 _visitors_floc_
           | Texpr_or
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_or env _visitors_flhs _visitors_frhs
                 _visitors_floc_

      method visit_argument : _ -> argument -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
           let _visitors_r1 =
             self#visit_argument_kind env _visitors_this.arg_kind
           in
           ()

      method visit_Pipe_partial_apply :
          _ -> expr -> argument list -> location -> unit =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ffunc in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_argument env))
                 _visitors_fargs
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Pipe_invalid : _ -> expr -> stype -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_pipe_rhs : _ -> pipe_rhs -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Pipe_partial_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_partial_apply env _visitors_ffunc _visitors_fargs
                 _visitors_floc_
           | Pipe_invalid
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_invalid env _visitors_fexpr _visitors_fty
                 _visitors_floc_

      method visit_Interp_lit : _ -> string -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = (fun _visitors_this -> ()) _visitors_c0 in
           ()

      method visit_Interp_expr : _ -> expr -> expr -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fto_string ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_expr env _visitors_fto_string in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_interp_elem : _ -> interp_elem -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Interp_lit _visitors_c0 -> self#visit_Interp_lit env _visitors_c0
           | Interp_expr
               {
                 expr = _visitors_fexpr;
                 to_string = _visitors_fto_string;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Interp_expr env _visitors_fexpr _visitors_fto_string
                 _visitors_floc_

      method visit_Field_def :
          _ -> Syntax.label -> expr -> bool -> bool -> int -> unit =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fexpr ->
            fun _visitors_fis_mut ->
             fun _visitors_fis_pun ->
              fun _visitors_fpos ->
               let _visitors_r0 = self#visit_label env _visitors_flabel in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 (fun _visitors_this -> ()) _visitors_fis_mut
               in
               let _visitors_r3 =
                 (fun _visitors_this -> ()) _visitors_fis_pun
               in
               let _visitors_r4 = (fun _visitors_this -> ()) _visitors_fpos in
               ()

      method visit_field_def : _ -> field_def -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_def
               {
                 label = _visitors_flabel;
                 expr = _visitors_fexpr;
                 is_mut = _visitors_fis_mut;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_def env _visitors_flabel _visitors_fexpr
                 _visitors_fis_mut _visitors_fis_pun _visitors_fpos

      method visit_Field_pat : _ -> Syntax.label -> pat -> bool -> int -> unit =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fpat ->
            fun _visitors_fis_pun ->
             fun _visitors_fpos ->
              let _visitors_r0 = self#visit_label env _visitors_flabel in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 = (fun _visitors_this -> ()) _visitors_fis_pun in
              let _visitors_r3 = (fun _visitors_this -> ()) _visitors_fpos in
              ()

      method visit_field_pat : _ -> field_pat -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_pat
               {
                 label = _visitors_flabel;
                 pat = _visitors_fpat;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_pat env _visitors_flabel _visitors_fpat
                 _visitors_fis_pun _visitors_fpos

      method visit_constr_pat_args : _ -> constr_pat_args -> unit =
        fun env ->
          fun _visitors_this ->
           Basic_lst.iter _visitors_this ~f:(self#visit_constr_pat_arg env)

      method visit_Constr_pat_arg :
          _ -> pat -> Syntax.argument_kind -> int -> unit =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkind ->
            fun _visitors_fpos ->
             let _visitors_r0 = self#visit_pat env _visitors_fpat in
             let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
             let _visitors_r2 = (fun _visitors_this -> ()) _visitors_fpos in
             ()

      method visit_constr_pat_arg : _ -> constr_pat_arg -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Constr_pat_arg
               {
                 pat = _visitors_fpat;
                 kind = _visitors_fkind;
                 pos = _visitors_fpos;
               } ->
               self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind
                 _visitors_fpos

      method visit_Param :
          _ -> binder -> typ option -> stype -> param_kind -> unit =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_typ env) t
                  | None -> ())
                  _visitors_fkonstraint
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_param_kind env _visitors_fkind in
              ()

      method visit_Discard_positional_param :
          _ -> stype -> typ option -> location -> unit =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_fkonstraint ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_typ env) t
                 | None -> ())
                 _visitors_fkonstraint
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_param : _ -> param -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Param
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 kind = _visitors_fkind;
               } ->
               self#visit_Param env _visitors_fbinder _visitors_fkonstraint
                 _visitors_fty _visitors_fkind
           | Discard_positional_param
               {
                 ty = _visitors_fty;
                 konstraint = _visitors_fkonstraint;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Discard_positional_param env _visitors_fty
                 _visitors_fkonstraint _visitors_floc_

      method visit_Positional : _ -> unit = fun env -> ()
      method visit_Labelled : _ -> unit = fun env -> ()

      method visit_Optional : _ -> expr -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_expr env _visitors_c0 in
           ()

      method visit_Autofill : _ -> unit = fun env -> ()
      method visit_Question_optional : _ -> unit = fun env -> ()

      method visit_param_kind : _ -> param_kind -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Positional -> self#visit_Positional env
           | Labelled -> self#visit_Labelled env
           | Optional _visitors_c0 -> self#visit_Optional env _visitors_c0
           | Autofill -> self#visit_Autofill env
           | Question_optional -> self#visit_Question_optional env

      method visit_params : _ -> params -> unit =
        fun env ->
          fun _visitors_this ->
           Basic_lst.iter _visitors_this ~f:(self#visit_param env)

      method visit_fn : _ -> fn -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_params env _visitors_this.params in
           let _visitors_r1 =
             self#visit_location env _visitors_this.params_loc_
           in
           let _visitors_r2 = self#visit_expr env _visitors_this.body in
           let _visitors_r3 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t ->
                   (fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 = self#visit_typ env _visitors_c0 in
                     let _visitors_r1 = self#visit_error_typ env _visitors_c1 in
                     ())
                     t
               | None -> ())
               _visitors_this.ret_constraint
           in
           let _visitors_r4 = self#visit_stype env _visitors_this.ty in
           let _visitors_r5 = self#visit_fn_kind env _visitors_this.kind_ in
           ()

      method visit_match_case : _ -> match_case -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_pat env _visitors_this.pat in
           let _visitors_r1 = self#visit_expr env _visitors_this.action in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_expr env) t
               | None -> ())
               _visitors_this.guard
           in
           let _visitors_r3 =
             self#visit_pat_binders env _visitors_this.pat_binders
           in
           ()

      method visit_Error_typ : _ -> typ -> unit =
        fun env ->
          fun _visitors_fty ->
           let _visitors_r0 = self#visit_typ env _visitors_fty in
           ()

      method visit_Default_error_typ : _ -> location -> unit =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           ()

      method visit_No_error_typ : _ -> unit = fun env -> ()

      method visit_error_typ : _ -> error_typ -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Error_typ { ty = _visitors_fty } ->
               self#visit_Error_typ env _visitors_fty
           | Default_error_typ { loc_ = _visitors_floc_ } ->
               self#visit_Default_error_typ env _visitors_floc_
           | No_error_typ -> self#visit_No_error_typ env

      method visit_Tany : _ -> stype -> location -> unit =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            ()

      method visit_Tarrow :
          _ -> typ list -> typ -> error_typ -> stype -> location -> unit =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_freturn ->
            fun _visitors_ferr_ty ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                   _visitors_fparams
               in
               let _visitors_r1 = self#visit_typ env _visitors_freturn in
               let _visitors_r2 = self#visit_error_typ env _visitors_ferr_ty in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_T_tuple : _ -> typ list -> stype -> location -> unit =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                 _visitors_fparams
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Tname :
          _ -> constrid_loc -> typ list -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fparams ->
            fun _visitors_fty ->
             fun _visitors_fis_alias_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                   _visitors_fparams
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 (fun _visitors_this -> ()) _visitors_fis_alias_
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_Tobject :
          _ -> constrid_loc -> stype -> bool -> location -> unit =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fty ->
            fun _visitors_fis_alias_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_constrid_loc env _visitors_fconstr
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> ()) _visitors_fis_alias_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_typ : _ -> typ -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tany { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tany env _visitors_fty _visitors_floc_
           | Tarrow
               {
                 params = _visitors_fparams;
                 return = _visitors_freturn;
                 err_ty = _visitors_ferr_ty;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tarrow env _visitors_fparams _visitors_freturn
                 _visitors_ferr_ty _visitors_fty _visitors_floc_
           | T_tuple
               {
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_T_tuple env _visitors_fparams _visitors_fty
                 _visitors_floc_
           | Tname
               {
                 constr = _visitors_fconstr;
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tname env _visitors_fconstr _visitors_fparams
                 _visitors_fty _visitors_fis_alias_ _visitors_floc_
           | Tobject
               {
                 constr = _visitors_fconstr;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tobject env _visitors_fconstr _visitors_fty
                 _visitors_fis_alias_ _visitors_floc_

      method visit_Tpat_alias : _ -> pat -> binder -> stype -> location -> unit
          =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_falias ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_binder env _visitors_falias in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_any : _ -> stype -> location -> unit =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            ()

      method visit_Tpat_array : _ -> array_pattern -> stype -> location -> unit
          =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_array_pattern env _visitors_fpats in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Tpat_constant :
          _ -> constant -> stype -> var option -> location -> unit =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_var env) t
                  | None -> ())
                  _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_pat_args ->
          constr_tag ->
          stype ->
          bool ->
          fn_arity ->
          stype list ->
          location ->
          unit =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_fargs ->
             fun _visitors_ftag ->
              fun _visitors_fty ->
               fun _visitors_fused_error_subtyping ->
                fun _visitors_farity_ ->
                 fun _visitors_fall_args_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_type_name env) t
                       | None -> ())
                       _visitors_ftype_name
                   in
                   let _visitors_r1 =
                     self#visit_constr_name env _visitors_fconstr
                   in
                   let _visitors_r2 =
                     self#visit_constr_pat_args env _visitors_fargs
                   in
                   let _visitors_r3 =
                     self#visit_constr_tag env _visitors_ftag
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 =
                     (fun _visitors_this -> ()) _visitors_fused_error_subtyping
                   in
                   let _visitors_r6 =
                     self#visit_fn_arity env _visitors_farity_
                   in
                   let _visitors_r7 =
                     (fun _visitors_this ->
                       Basic_lst.iter _visitors_this ~f:(self#visit_stype env))
                       _visitors_fall_args_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   ()

      method visit_Tpat_or : _ -> pat -> pat -> stype -> location -> unit =
        fun env ->
          fun _visitors_fpat1 ->
           fun _visitors_fpat2 ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat1 in
              let _visitors_r1 = self#visit_pat env _visitors_fpat2 in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_tuple : _ -> pat list -> stype -> location -> unit =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_pat env))
                 _visitors_fpats
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Tpat_var : _ -> binder -> stype -> location -> unit =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_binder env _visitors_fbinder in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             ()

      method visit_Tpat_record :
          _ -> field_pat list -> bool -> stype -> location -> unit =
        fun env ->
          fun _visitors_ffields ->
           fun _visitors_fis_closed ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_field_pat env))
                  _visitors_ffields
              in
              let _visitors_r1 =
                (fun _visitors_this -> ()) _visitors_fis_closed
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_constraint :
          _ -> pat -> typ -> stype -> location -> unit =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_map :
          _ ->
          (constant * pat) list ->
          ident * stype * stype array ->
          stype ->
          location ->
          unit =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fop_get_info_ ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> ()) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_pat env _visitors_c1 in
                      ()))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                  let _visitors_r0 = self#visit_ident env _visitors_c0 in
                  let _visitors_r1 = self#visit_stype env _visitors_c1 in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_arr.iter _visitors_this (self#visit_stype env))
                      _visitors_c2
                  in
                  ())
                  _visitors_fop_get_info_
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              ()

      method visit_Tpat_range :
          _ -> pat -> pat -> bool -> stype -> location -> unit =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_finclusive ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pat env _visitors_flhs in
               let _visitors_r1 = self#visit_pat env _visitors_frhs in
               let _visitors_r2 =
                 (fun _visitors_this -> ()) _visitors_finclusive
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               ()

      method visit_pat : _ -> pat -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tpat_alias
               {
                 pat = _visitors_fpat;
                 alias = _visitors_falias;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_alias env _visitors_fpat _visitors_falias
                 _visitors_fty _visitors_floc_
           | Tpat_any { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tpat_any env _visitors_fty _visitors_floc_
           | Tpat_array
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_array env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Tpat_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 args = _visitors_fargs;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 used_error_subtyping = _visitors_fused_error_subtyping;
                 arity_ = _visitors_farity_;
                 all_args_ = _visitors_fall_args_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constr env _visitors_ftype_name _visitors_fconstr
                 _visitors_fargs _visitors_ftag _visitors_fty
                 _visitors_fused_error_subtyping _visitors_farity_
                 _visitors_fall_args_ _visitors_floc_
           | Tpat_or
               {
                 pat1 = _visitors_fpat1;
                 pat2 = _visitors_fpat2;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_or env _visitors_fpat1 _visitors_fpat2
                 _visitors_fty _visitors_floc_
           | Tpat_tuple
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_tuple env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_var
               {
                 binder = _visitors_fbinder;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_var env _visitors_fbinder _visitors_fty
                 _visitors_floc_
           | Tpat_record
               {
                 fields = _visitors_ffields;
                 is_closed = _visitors_fis_closed;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_record env _visitors_ffields _visitors_fis_closed
                 _visitors_fty _visitors_floc_
           | Tpat_constraint
               {
                 pat = _visitors_fpat;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constraint env _visitors_fpat
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Tpat_map
               {
                 elems = _visitors_felems;
                 op_get_info_ = _visitors_fop_get_info_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_map env _visitors_felems _visitors_fop_get_info_
                 _visitors_fty _visitors_floc_
           | Tpat_range
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 inclusive = _visitors_finclusive;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_range env _visitors_flhs _visitors_frhs
                 _visitors_finclusive _visitors_fty _visitors_floc_

      method visit_Closed : _ -> pat list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_pat env))
               _visitors_c0
           in
           ()

      method visit_Open :
          _ -> pat list -> pat list -> (binder * stype) option -> unit =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            fun _visitors_c2 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_pat env))
                 _visitors_c0
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_pat env))
                 _visitors_c1
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     (fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_binder env _visitors_c0 in
                       let _visitors_r1 = self#visit_stype env _visitors_c1 in
                       ())
                       t
                 | None -> ())
                 _visitors_c2
             in
             ()

      method visit_array_pattern : _ -> array_pattern -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
           | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
               self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

      method visit_fun_decl : _ -> fun_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_fun_decl_kind env _visitors_this.kind
           in
           let _visitors_r1 = self#visit_binder env _visitors_this.fn_binder in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 =
             (fun _visitors_this -> ()) _visitors_this.is_pub
           in
           let _visitors_r4 =
             self#visit_tvar_env env _visitors_this.ty_params_
           in
           let _visitors_r5 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_type_name env))
               _visitors_this.constraint_names
           in
           let _visitors_r6 = self#visit_docstring env _visitors_this.doc_ in
           let _visitors_r7 = self#visit_attributes env _visitors_this.attrs in
           ()

      method visit_Fun_kind_regular : _ -> unit = fun env -> ()

      method visit_Fun_kind_method : _ -> type_name option -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_type_name env) t
               | None -> ())
               _visitors_c0
           in
           ()

      method visit_Fun_kind_default_impl : _ -> type_name -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_type_name env _visitors_c0 in
           ()

      method visit_Fun_kind_impl : _ -> typ -> type_name -> unit =
        fun env ->
          fun _visitors_fself_ty ->
           fun _visitors_ftrait ->
            let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
            let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
            ()

      method visit_fun_decl_kind : _ -> fun_decl_kind -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Fun_kind_regular -> self#visit_Fun_kind_regular env
           | Fun_kind_method _visitors_c0 ->
               self#visit_Fun_kind_method env _visitors_c0
           | Fun_kind_default_impl _visitors_c0 ->
               self#visit_Fun_kind_default_impl env _visitors_c0
           | Fun_kind_impl
               { self_ty = _visitors_fself_ty; trait = _visitors_ftrait } ->
               self#visit_Fun_kind_impl env _visitors_fself_ty _visitors_ftrait

      method visit_Timpl_expr :
          _ -> expr -> bool -> _ -> absolute_loc -> bool -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fis_main ->
            fun _visitors_fexpr_id ->
             fun _visitors_floc_ ->
              fun _visitors_fis_generated_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this -> ()) _visitors_fis_main
               in
               let _visitors_r2 =
                 (fun _visitors_this -> ()) _visitors_fexpr_id
               in
               let _visitors_r3 = self#visit_absolute_loc env _visitors_floc_ in
               let _visitors_r4 =
                 (fun _visitors_this -> ()) _visitors_fis_generated_
               in
               ()

      method visit_Timpl_fun_decl :
          _ -> fun_decl -> fn_arity -> absolute_loc -> bool -> unit =
        fun env ->
          fun _visitors_ffun_decl ->
           fun _visitors_farity_ ->
            fun _visitors_floc_ ->
             fun _visitors_fis_generated_ ->
              let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
              let _visitors_r1 = self#visit_fn_arity env _visitors_farity_ in
              let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
              let _visitors_r3 =
                (fun _visitors_this -> ()) _visitors_fis_generated_
              in
              ()

      method visit_Timpl_letdef :
          _ ->
          binder ->
          typ option ->
          expr ->
          bool ->
          absolute_loc ->
          docstring ->
          attributes ->
          bool ->
          unit =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fis_pub ->
              fun _visitors_floc_ ->
               fun _visitors_fdoc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fis_generated_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_typ env) t
                      | None -> ())
                      _visitors_fkonstraint
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 =
                    (fun _visitors_this -> ()) _visitors_fis_pub
                  in
                  let _visitors_r4 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  let _visitors_r6 =
                    self#visit_attributes env _visitors_fattrs
                  in
                  let _visitors_r7 =
                    (fun _visitors_this -> ()) _visitors_fis_generated_
                  in
                  ()

      method visit_Timpl_stub_decl : _ -> stub_decl -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_stub_decl env _visitors_c0 in
           ()

      method visit_impl : _ -> impl -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Timpl_expr
               {
                 expr = _visitors_fexpr;
                 is_main = _visitors_fis_main;
                 expr_id = _visitors_fexpr_id;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_expr env _visitors_fexpr _visitors_fis_main
                 _visitors_fexpr_id _visitors_floc_ _visitors_fis_generated_
           | Timpl_fun_decl
               {
                 fun_decl = _visitors_ffun_decl;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_fun_decl env _visitors_ffun_decl
                 _visitors_farity_ _visitors_floc_ _visitors_fis_generated_
           | Timpl_letdef
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 is_pub = _visitors_fis_pub;
                 loc_ = _visitors_floc_;
                 doc_ = _visitors_fdoc_;
                 attrs = _visitors_fattrs;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_letdef env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fis_pub
                 _visitors_floc_ _visitors_fdoc_ _visitors_fattrs
                 _visitors_fis_generated_
           | Timpl_stub_decl _visitors_c0 ->
               self#visit_Timpl_stub_decl env _visitors_c0

      method visit_impls : _ -> impls -> unit =
        fun env ->
          fun _visitors_this ->
           Basic_lst.iter _visitors_this ~f:(self#visit_impl env)

      method visit_type_decl : _ -> type_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.td_binder
           in
           let _visitors_r1 =
             self#visit_tvar_env env _visitors_this.td_params
           in
           let _visitors_r2 = self#visit_type_desc env _visitors_this.td_desc in
           let _visitors_r3 = self#visit_visibility env _visitors_this.td_vis in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.td_loc_
           in
           let _visitors_r5 = self#visit_docstring env _visitors_this.td_doc_ in
           let _visitors_r6 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_type_name env))
               _visitors_this.td_deriving_
           in
           ()

      method visit_No_payload : _ -> unit = fun env -> ()

      method visit_Single_payload : _ -> typ -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           ()

      method visit_Enum_payload : _ -> constr_decl list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_constr_decl env))
               _visitors_c0
           in
           ()

      method visit_exception_decl : _ -> exception_decl -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | No_payload -> self#visit_No_payload env
           | Single_payload _visitors_c0 ->
               self#visit_Single_payload env _visitors_c0
           | Enum_payload _visitors_c0 ->
               self#visit_Enum_payload env _visitors_c0

      method visit_Td_abstract : _ -> unit = fun env -> ()

      method visit_Td_error : _ -> exception_decl -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
           ()

      method visit_Td_newtype : _ -> typ -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           ()

      method visit_Td_variant : _ -> constr_decl list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_constr_decl env))
               _visitors_c0
           in
           ()

      method visit_Td_record : _ -> field_decl list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_field_decl env))
               _visitors_c0
           in
           ()

      method visit_Td_alias : _ -> typ -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           ()

      method visit_type_desc : _ -> type_desc -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Td_abstract -> self#visit_Td_abstract env
           | Td_error _visitors_c0 -> self#visit_Td_error env _visitors_c0
           | Td_newtype _visitors_c0 -> self#visit_Td_newtype env _visitors_c0
           | Td_variant _visitors_c0 -> self#visit_Td_variant env _visitors_c0
           | Td_record _visitors_c0 -> self#visit_Td_record env _visitors_c0
           | Td_alias _visitors_c0 -> self#visit_Td_alias env _visitors_c0

      method visit_constr_decl_arg : _ -> constr_decl_arg -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_typ env _visitors_this.carg_typ in
           let _visitors_r1 =
             (fun _visitors_this -> ()) _visitors_this.carg_mut
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_label env) t
               | None -> ())
               _visitors_this.carg_label
           in
           ()

      method visit_constr_decl : _ -> constr_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.constr_name in
           let _visitors_r1 =
             self#visit_constr_tag env _visitors_this.constr_tag
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_constr_decl_arg env))
               _visitors_this.constr_args
           in
           let _visitors_r3 =
             self#visit_fn_arity env _visitors_this.constr_arity_
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.constr_loc_
           in
           ()

      method visit_field_decl : _ -> field_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.field_label in
           let _visitors_r1 = self#visit_typ env _visitors_this.field_typ in
           let _visitors_r2 =
             (fun _visitors_this -> ()) _visitors_this.field_mut
           in
           let _visitors_r3 =
             self#visit_type_component_visibility env _visitors_this.field_vis
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.field_loc_
           in
           ()

      method visit_trait_decl : _ -> trait_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_name
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_type_name env))
               _visitors_this.trait_supers
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_method_decl env))
               _visitors_this.trait_methods
           in
           let _visitors_r3 =
             self#visit_visibility env _visitors_this.trait_vis
           in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.trait_loc_
           in
           let _visitors_r5 =
             self#visit_docstring env _visitors_this.trait_doc_
           in
           let _visitors_r6 =
             self#visit_attributes env _visitors_this.trait_attrs
           in
           ()

      method visit_trait_alias_decl : _ -> trait_alias_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_alias_name
           in
           let _visitors_r1 =
             self#visit_type_name env _visitors_this.trait_alias_target
           in
           let _visitors_r2 =
             (fun _visitors_this -> ()) _visitors_this.trait_alias_is_pub
           in
           let _visitors_r3 =
             self#visit_absolute_loc env _visitors_this.trait_alias_loc_
           in
           let _visitors_r4 =
             self#visit_docstring env _visitors_this.trait_alias_doc_
           in
           ()

      method visit_method_decl : _ -> method_decl -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_syntax_binder env _visitors_this.method_name
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this
                 ~f:(fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_label env) t
                       | None -> ())
                       _visitors_c0
                   in
                   let _visitors_r1 = self#visit_typ env _visitors_c1 in
                   ()))
               _visitors_this.method_params
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_typ env) t
               | None -> ())
               _visitors_this.method_ret
           in
           let _visitors_r3 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_typ env) t
               | None -> ())
               _visitors_this.method_err
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.method_loc_
           in
           ()

      method visit_Output :
          _ ->
          impls ->
          type_decl list ->
          trait_decl list ->
          trait_alias_decl list ->
          unit =
        fun env ->
          fun _visitors_fvalue_defs ->
           fun _visitors_ftype_defs ->
            fun _visitors_ftrait_defs ->
             fun _visitors_ftrait_alias ->
              let _visitors_r0 = self#visit_impls env _visitors_fvalue_defs in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_type_decl env))
                  _visitors_ftype_defs
              in
              let _visitors_r2 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_trait_decl env))
                  _visitors_ftrait_defs
              in
              let _visitors_r3 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(self#visit_trait_alias_decl env))
                  _visitors_ftrait_alias
              in
              ()

      method visit_output : _ -> output -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Output
               {
                 value_defs = _visitors_fvalue_defs;
                 type_defs = _visitors_ftype_defs;
                 trait_defs = _visitors_ftrait_defs;
                 trait_alias = _visitors_ftrait_alias;
               } ->
               self#visit_Output env _visitors_fvalue_defs _visitors_ftype_defs
                 _visitors_ftrait_defs _visitors_ftrait_alias
    end

  [@@@VISITORS.END]
end

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] map =
    object (self : 'self)
      inherit [_] mapbase

      method visit_stub_decl : _ -> stub_decl -> stub_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_binder env _visitors_this.binder in
           let _visitors_r1 = self#visit_params env _visitors_this.params in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_typ env) t)
               | None -> None)
               _visitors_this.ret
           in
           let _visitors_r3 =
             self#visit_stub_body env _visitors_this.func_stubs
           in
           let _visitors_r4 =
             (fun _visitors_this -> _visitors_this) _visitors_this.is_pub
           in
           let _visitors_r5 = self#visit_fn_arity env _visitors_this.arity_ in
           let _visitors_r6 =
             self#visit_fun_decl_kind env _visitors_this.kind_
           in
           let _visitors_r7 = self#visit_absolute_loc env _visitors_this.loc_ in
           let _visitors_r8 = self#visit_docstring env _visitors_this.doc_ in
           {
             binder = _visitors_r0;
             params = _visitors_r1;
             ret = _visitors_r2;
             func_stubs = _visitors_r3;
             is_pub = _visitors_r4;
             arity_ = _visitors_r5;
             kind_ = _visitors_r6;
             loc_ = _visitors_r7;
             doc_ = _visitors_r8;
           }

      method visit_Intrinsic : _ -> stub_body = fun env -> Intrinsic

      method visit_Func_stub : _ -> func_stubs -> stub_body =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_func_stubs env _visitors_c0 in
           Func_stub _visitors_r0

      method visit_stub_body : _ -> stub_body -> stub_body =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Intrinsic -> self#visit_Intrinsic env
           | Func_stub _visitors_c0 -> self#visit_Func_stub env _visitors_c0

      method visit_Texpr_apply :
          _ -> expr -> argument list -> stype -> apply_kind -> location -> expr
          =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_fty ->
             fun _visitors_fkind_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_ffunc in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_argument env))
                   _visitors_fargs
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 = self#visit_apply_kind env _visitors_fkind_ in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_apply
                 {
                   func = _visitors_r0;
                   args = _visitors_r1;
                   ty = _visitors_r2;
                   kind_ = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_method :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          type_name ->
          Primitive.prim option ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_fmeth ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_ftype_name ->
              fun _visitors_fprim ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fmeth in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_arr.map _visitors_this (self#visit_stype env))
                     _visitors_fty_args_
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_fn_arity env) t)
                     | None -> None)
                     _visitors_farity_
                 in
                 let _visitors_r3 =
                   self#visit_type_name env _visitors_ftype_name
                 in
                 let _visitors_r4 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_prim env) t)
                     | None -> None)
                     _visitors_fprim
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 Texpr_method
                   {
                     meth = _visitors_r0;
                     ty_args_ = _visitors_r1;
                     arity_ = _visitors_r2;
                     type_name = _visitors_r3;
                     prim = _visitors_r4;
                     ty = _visitors_r5;
                     loc_ = _visitors_r6;
                   }

      method visit_Texpr_unresolved_method :
          _ ->
          type_name ->
          string ->
          stype ->
          fn_arity option ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_ftrait_name ->
           fun _visitors_fmethod_name ->
            fun _visitors_fself_type ->
             fun _visitors_farity_ ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_type_name env _visitors_ftrait_name
                in
                let _visitors_r1 =
                  (fun _visitors_this -> _visitors_this) _visitors_fmethod_name
                in
                let _visitors_r2 = self#visit_stype env _visitors_fself_type in
                let _visitors_r3 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_fn_arity env) t)
                    | None -> None)
                    _visitors_farity_
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_unresolved_method
                  {
                    trait_name = _visitors_r0;
                    method_name = _visitors_r1;
                    self_type = _visitors_r2;
                    arity_ = _visitors_r3;
                    ty = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_ident :
          _ ->
          var ->
          stype array ->
          fn_arity option ->
          value_kind ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_fid ->
           fun _visitors_fty_args_ ->
            fun _visitors_farity_ ->
             fun _visitors_fkind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_arr.map _visitors_this (self#visit_stype env))
                    _visitors_fty_args_
                in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_fn_arity env) t)
                    | None -> None)
                    _visitors_farity_
                in
                let _visitors_r3 =
                  (fun _visitors_this -> _visitors_this) _visitors_fkind
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_ident
                  {
                    id = _visitors_r0;
                    ty_args_ = _visitors_r1;
                    arity_ = _visitors_r2;
                    kind = _visitors_r3;
                    ty = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_as :
          _ -> expr -> type_name -> stype -> bool -> location -> expr =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftrait ->
            fun _visitors_fty ->
             fun _visitors_fis_implicit ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_implicit
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_as
                 {
                   expr = _visitors_r0;
                   trait = _visitors_r1;
                   ty = _visitors_r2;
                   is_implicit = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_array :
          _ -> expr list -> stype -> bool -> location -> expr =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_fis_fixed_array ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_fixed_array
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_array
                {
                  exprs = _visitors_r0;
                  ty = _visitors_r1;
                  is_fixed_array = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_constant :
          _ -> constant -> stype -> var option -> location -> expr =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_fc
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_var env) t)
                  | None -> None)
                  _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_constant
                {
                  c = _visitors_r0;
                  ty = _visitors_r1;
                  name_ = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_tag ->
          stype ->
          fn_arity ->
          location ->
          expr =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_ftag ->
             fun _visitors_fty ->
              fun _visitors_farity_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_type_name env) t)
                    | None -> None)
                    _visitors_ftype_name
                in
                let _visitors_r1 =
                  self#visit_constr_name env _visitors_fconstr
                in
                let _visitors_r2 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 = self#visit_fn_arity env _visitors_farity_ in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_constr
                  {
                    type_name = _visitors_r0;
                    constr = _visitors_r1;
                    tag = _visitors_r2;
                    ty = _visitors_r3;
                    arity_ = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_while :
          _ ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          expr =
        fun env ->
          fun _visitors_floop_cond ->
           fun _visitors_floop_body ->
            fun _visitors_fwhile_else ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_floop_cond in
                let _visitors_r1 = self#visit_expr env _visitors_floop_body in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_expr env) t)
                    | None -> None)
                    _visitors_fwhile_else
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_loop_label_binder env) t)
                    | None -> None)
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_while
                  {
                    loop_cond = _visitors_r0;
                    loop_body = _visitors_r1;
                    while_else = _visitors_r2;
                    ty = _visitors_r3;
                    label = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_function : _ -> fn -> stype -> bool -> location -> expr
          =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fty ->
            fun _visitors_fis_raw_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_fn env _visitors_ffunc in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_raw_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_function
                {
                  func = _visitors_r0;
                  ty = _visitors_r1;
                  is_raw_ = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_if :
          _ -> expr -> expr -> expr option -> stype -> location -> expr =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 = self#visit_expr env _visitors_fifso in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_expr env) t)
                   | None -> None)
                   _visitors_fifnot
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_if
                 {
                   cond = _visitors_r0;
                   ifso = _visitors_r1;
                   ifnot = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_letfn :
          _ -> binder -> fn -> expr -> stype -> bool -> location -> expr =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_ffn ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_fis_rec ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_fn env _visitors_ffn in
                let _visitors_r2 = self#visit_expr env _visitors_fbody in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_rec
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_letfn
                  {
                    binder = _visitors_r0;
                    fn = _visitors_r1;
                    body = _visitors_r2;
                    ty = _visitors_r3;
                    is_rec = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_letrec :
          _ -> (binder * fn) list -> expr -> stype -> location -> expr =
        fun env ->
          fun _visitors_fbindings ->
           fun _visitors_fbody ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 = self#visit_binder env _visitors_c0 in
                      let _visitors_r1 = self#visit_fn env _visitors_c1 in
                      (_visitors_r0, _visitors_r1)))
                  _visitors_fbindings
              in
              let _visitors_r1 = self#visit_expr env _visitors_fbody in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_letrec
                {
                  bindings = _visitors_r0;
                  body = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_let :
          _ -> pat -> expr -> pat_binders -> expr -> stype -> location -> expr =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_pat env _visitors_fpat in
                let _visitors_r1 = self#visit_expr env _visitors_frhs in
                let _visitors_r2 =
                  self#visit_pat_binders env _visitors_fpat_binders
                in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_let
                  {
                    pat = _visitors_r0;
                    rhs = _visitors_r1;
                    pat_binders = _visitors_r2;
                    body = _visitors_r3;
                    ty = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_sequence :
          _ -> expr list -> expr -> stype -> location -> expr =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_expr env))
                  _visitors_fexprs
              in
              let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_sequence
                {
                  exprs = _visitors_r0;
                  last_expr = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_tuple : _ -> expr list -> stype -> location -> expr =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_expr env))
                 _visitors_fexprs
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_tuple
               { exprs = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Texpr_record :
          _ -> type_name option -> field_def list -> stype -> location -> expr =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_ffields ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_type_name env) t)
                  | None -> None)
                  _visitors_ftype_name
              in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_field_def env))
                  _visitors_ffields
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_record
                {
                  type_name = _visitors_r0;
                  fields = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_record_update :
          _ ->
          type_name option ->
          expr ->
          field_info list ->
          field_def list ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_frecord ->
            fun _visitors_fall_fields ->
             fun _visitors_ffields ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_type_name env) t)
                    | None -> None)
                    _visitors_ftype_name
                in
                let _visitors_r1 = self#visit_expr env _visitors_frecord in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_field_info env))
                    _visitors_fall_fields
                in
                let _visitors_r3 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_field_def env))
                    _visitors_ffields
                in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_record_update
                  {
                    type_name = _visitors_r0;
                    record = _visitors_r1;
                    all_fields = _visitors_r2;
                    fields = _visitors_r3;
                    ty = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_field :
          _ -> expr -> Syntax.accessor -> int -> stype -> location -> expr =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_faccessor ->
            fun _visitors_fpos ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_frecord in
               let _visitors_r1 = self#visit_accessor env _visitors_faccessor in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_fpos
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_field
                 {
                   record = _visitors_r0;
                   accessor = _visitors_r1;
                   pos = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_mutate :
          _ ->
          expr ->
          Syntax.label ->
          expr ->
          int ->
          expr option ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_frecord ->
           fun _visitors_flabel ->
            fun _visitors_ffield ->
             fun _visitors_fpos ->
              fun _visitors_faugmented_by ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 = self#visit_label env _visitors_flabel in
                 let _visitors_r2 = self#visit_expr env _visitors_ffield in
                 let _visitors_r3 =
                   (fun _visitors_this -> _visitors_this) _visitors_fpos
                 in
                 let _visitors_r4 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_faugmented_by
                 in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 Texpr_mutate
                   {
                     record = _visitors_r0;
                     label = _visitors_r1;
                     field = _visitors_r2;
                     pos = _visitors_r3;
                     augmented_by = _visitors_r4;
                     ty = _visitors_r5;
                     loc_ = _visitors_r6;
                   }

      method visit_Texpr_match :
          _ -> expr -> match_case list -> stype -> location -> location -> expr
          =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fcases ->
            fun _visitors_fty ->
             fun _visitors_fmatch_loc_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_match_case env))
                   _visitors_fcases
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 self#visit_location env _visitors_fmatch_loc_
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_match
                 {
                   expr = _visitors_r0;
                   cases = _visitors_r1;
                   ty = _visitors_r2;
                   match_loc_ = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_letmut :
          _ -> binder -> typ option -> expr -> expr -> stype -> location -> expr
          =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_typ env) t)
                    | None -> None)
                    _visitors_fkonstraint
                in
                let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                let _visitors_r3 = self#visit_expr env _visitors_fbody in
                let _visitors_r4 = self#visit_stype env _visitors_fty in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_letmut
                  {
                    binder = _visitors_r0;
                    konstraint = _visitors_r1;
                    expr = _visitors_r2;
                    body = _visitors_r3;
                    ty = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_assign :
          _ -> var -> expr -> expr option -> stype -> location -> expr =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fexpr ->
            fun _visitors_faugmented_by ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_var env _visitors_fvar in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_expr env) t)
                   | None -> None)
                   _visitors_faugmented_by
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_assign
                 {
                   var = _visitors_r0;
                   expr = _visitors_r1;
                   augmented_by = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_hole : _ -> stype -> location -> syntax_hole -> expr =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            fun _visitors_fkind ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 = self#visit_location env _visitors_floc_ in
             let _visitors_r2 = self#visit_syntax_hole env _visitors_fkind in
             Texpr_hole
               { ty = _visitors_r0; loc_ = _visitors_r1; kind = _visitors_r2 }

      method visit_Texpr_unit : _ -> location -> expr =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           Texpr_unit { loc_ = _visitors_r0 }

      method visit_Texpr_break :
          _ -> expr option -> stype -> loop_label option -> location -> expr =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_expr env) t)
                  | None -> None)
                  _visitors_farg
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_loop_label env) t)
                  | None -> None)
                  _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_break
                {
                  arg = _visitors_r0;
                  ty = _visitors_r1;
                  label = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_continue :
          _ -> expr list -> stype -> loop_label option -> location -> expr =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_fty ->
            fun _visitors_flabel ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_expr env))
                  _visitors_fargs
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_loop_label env) t)
                  | None -> None)
                  _visitors_flabel
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_continue
                {
                  args = _visitors_r0;
                  ty = _visitors_r1;
                  label = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_loop :
          _ ->
          param list ->
          expr ->
          expr list ->
          stype ->
          loop_label_binder option ->
          location ->
          expr =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_fty ->
              fun _visitors_flabel ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_param env))
                    _visitors_fparams
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r3 = self#visit_stype env _visitors_fty in
                let _visitors_r4 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_loop_label_binder env) t)
                    | None -> None)
                    _visitors_flabel
                in
                let _visitors_r5 = self#visit_location env _visitors_floc_ in
                Texpr_loop
                  {
                    params = _visitors_r0;
                    body = _visitors_r1;
                    args = _visitors_r2;
                    ty = _visitors_r3;
                    label = _visitors_r4;
                    loc_ = _visitors_r5;
                  }

      method visit_Texpr_for :
          _ ->
          (binder * expr) list ->
          expr option ->
          (var * expr) list ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          expr =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_fcondition ->
            fun _visitors_fsteps ->
             fun _visitors_fbody ->
              fun _visitors_ffor_else ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this
                        (fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            self#visit_binder env _visitors_c0
                          in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          (_visitors_r0, _visitors_r1)))
                      _visitors_fbinders
                  in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_expr env) t)
                      | None -> None)
                      _visitors_fcondition
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this
                        (fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 = self#visit_var env _visitors_c0 in
                          let _visitors_r1 = self#visit_expr env _visitors_c1 in
                          (_visitors_r0, _visitors_r1)))
                      _visitors_fsteps
                  in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_expr env) t)
                      | None -> None)
                      _visitors_ffor_else
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_loop_label_binder env) t)
                      | None -> None)
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  Texpr_for
                    {
                      binders = _visitors_r0;
                      condition = _visitors_r1;
                      steps = _visitors_r2;
                      body = _visitors_r3;
                      for_else = _visitors_r4;
                      ty = _visitors_r5;
                      label = _visitors_r6;
                      loc_ = _visitors_r7;
                    }

      method visit_Texpr_foreach :
          _ ->
          binder option list ->
          stype list ->
          expr ->
          expr ->
          expr option ->
          stype ->
          loop_label_binder option ->
          location ->
          expr =
        fun env ->
          fun _visitors_fbinders ->
           fun _visitors_felem_tys ->
            fun _visitors_fexpr ->
             fun _visitors_fbody ->
              fun _visitors_felse_block ->
               fun _visitors_fty ->
                fun _visitors_flabel ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (fun _visitors_this ->
                          match _visitors_this with
                          | Some t -> Some ((self#visit_binder env) t)
                          | None -> None))
                      _visitors_fbinders
                  in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_stype env))
                      _visitors_felem_tys
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 = self#visit_expr env _visitors_fbody in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_expr env) t)
                      | None -> None)
                      _visitors_felse_block
                  in
                  let _visitors_r5 = self#visit_stype env _visitors_fty in
                  let _visitors_r6 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_loop_label_binder env) t)
                      | None -> None)
                      _visitors_flabel
                  in
                  let _visitors_r7 = self#visit_location env _visitors_floc_ in
                  Texpr_foreach
                    {
                      binders = _visitors_r0;
                      elem_tys = _visitors_r1;
                      expr = _visitors_r2;
                      body = _visitors_r3;
                      else_block = _visitors_r4;
                      ty = _visitors_r5;
                      label = _visitors_r6;
                      loc_ = _visitors_r7;
                    }

      method visit_Texpr_return : _ -> expr option -> stype -> location -> expr
          =
        fun env ->
          fun _visitors_freturn_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_expr env) t)
                 | None -> None)
                 _visitors_freturn_value
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_return
               {
                 return_value = _visitors_r0;
                 ty = _visitors_r1;
                 loc_ = _visitors_r2;
               }

      method visit_Texpr_raise : _ -> expr -> stype -> location -> expr =
        fun env ->
          fun _visitors_ferror_value ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ferror_value in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_raise
               {
                 error_value = _visitors_r0;
                 ty = _visitors_r1;
                 loc_ = _visitors_r2;
               }

      method visit_Texpr_try :
          _ ->
          expr ->
          match_case list ->
          bool ->
          match_case list option ->
          stype ->
          stype ->
          location ->
          location ->
          location ->
          expr =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fcatch ->
            fun _visitors_fcatch_all ->
             fun _visitors_ftry_else ->
              fun _visitors_fty ->
               fun _visitors_ferr_ty ->
                fun _visitors_fcatch_loc_ ->
                 fun _visitors_felse_loc_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 = self#visit_expr env _visitors_fbody in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this (self#visit_match_case env))
                       _visitors_fcatch
                   in
                   let _visitors_r2 =
                     (fun _visitors_this -> _visitors_this) _visitors_fcatch_all
                   in
                   let _visitors_r3 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           Some
                             ((fun _visitors_this ->
                                Basic_lst.map _visitors_this
                                  (self#visit_match_case env))
                                t)
                       | None -> None)
                       _visitors_ftry_else
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 = self#visit_stype env _visitors_ferr_ty in
                   let _visitors_r6 =
                     self#visit_location env _visitors_fcatch_loc_
                   in
                   let _visitors_r7 =
                     self#visit_location env _visitors_felse_loc_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   Texpr_try
                     {
                       body = _visitors_r0;
                       catch = _visitors_r1;
                       catch_all = _visitors_r2;
                       try_else = _visitors_r3;
                       ty = _visitors_r4;
                       err_ty = _visitors_r5;
                       catch_loc_ = _visitors_r6;
                       else_loc_ = _visitors_r7;
                       loc_ = _visitors_r8;
                     }

      method visit_Texpr_exclamation :
          _ -> expr -> stype -> bool -> location -> expr =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_fconvert_to_result ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this)
                  _visitors_fconvert_to_result
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_exclamation
                {
                  expr = _visitors_r0;
                  ty = _visitors_r1;
                  convert_to_result = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_constraint :
          _ -> expr -> typ -> stype -> location -> expr =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_constraint
                {
                  expr = _visitors_r0;
                  konstraint = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_pipe :
          _ -> expr -> pipe_rhs -> stype -> location -> expr =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_flhs in
              let _visitors_r1 = self#visit_pipe_rhs env _visitors_frhs in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_pipe
                {
                  lhs = _visitors_r0;
                  rhs = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_interp :
          _ -> interp_elem list -> stype -> location -> expr =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_interp_elem env))
                 _visitors_felems
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_interp
               { elems = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Texpr_guard :
          _ -> expr -> expr option -> expr -> stype -> location -> expr =
        fun env ->
          fun _visitors_fcond ->
           fun _visitors_fotherwise ->
            fun _visitors_fbody ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fcond in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_expr env) t)
                   | None -> None)
                   _visitors_fotherwise
               in
               let _visitors_r2 = self#visit_expr env _visitors_fbody in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Texpr_guard
                 {
                   cond = _visitors_r0;
                   otherwise = _visitors_r1;
                   body = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Texpr_guard_let :
          _ ->
          pat ->
          expr ->
          pat_binders ->
          match_case list option ->
          expr ->
          stype ->
          location ->
          expr =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_frhs ->
            fun _visitors_fpat_binders ->
             fun _visitors_fotherwise ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_pat env _visitors_fpat in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 =
                   self#visit_pat_binders env _visitors_fpat_binders
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t ->
                         Some
                           ((fun _visitors_this ->
                              Basic_lst.map _visitors_this
                                (self#visit_match_case env))
                              t)
                     | None -> None)
                     _visitors_fotherwise
                 in
                 let _visitors_r4 = self#visit_expr env _visitors_fbody in
                 let _visitors_r5 = self#visit_stype env _visitors_fty in
                 let _visitors_r6 = self#visit_location env _visitors_floc_ in
                 Texpr_guard_let
                   {
                     pat = _visitors_r0;
                     rhs = _visitors_r1;
                     pat_binders = _visitors_r2;
                     otherwise = _visitors_r3;
                     body = _visitors_r4;
                     ty = _visitors_r5;
                     loc_ = _visitors_r6;
                   }

      method visit_Texpr_is :
          _ -> expr -> pat -> pat_binders -> location -> expr =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fpat ->
            fun _visitors_fpat_binders ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 =
                self#visit_pat_binders env _visitors_fpat_binders
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Texpr_is
                {
                  expr = _visitors_r0;
                  pat = _visitors_r1;
                  pat_binders = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Texpr_array_as_view : _ -> expr -> stype -> location -> expr
          =
        fun env ->
          fun _visitors_farray ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_farray in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_array_as_view
               { array = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Texpr_and : _ -> expr -> expr -> location -> expr =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_and
               { lhs = _visitors_r0; rhs = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Texpr_or : _ -> expr -> expr -> location -> expr =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_flhs in
             let _visitors_r1 = self#visit_expr env _visitors_frhs in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Texpr_or
               { lhs = _visitors_r0; rhs = _visitors_r1; loc_ = _visitors_r2 }

      method visit_expr : _ -> expr -> expr =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Texpr_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 kind_ = _visitors_fkind_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_apply env _visitors_ffunc _visitors_fargs
                 _visitors_fty _visitors_fkind_ _visitors_floc_
           | Texpr_method
               {
                 meth = _visitors_fmeth;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 type_name = _visitors_ftype_name;
                 prim = _visitors_fprim;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_method env _visitors_fmeth _visitors_fty_args_
                 _visitors_farity_ _visitors_ftype_name _visitors_fprim
                 _visitors_fty _visitors_floc_
           | Texpr_unresolved_method
               {
                 trait_name = _visitors_ftrait_name;
                 method_name = _visitors_fmethod_name;
                 self_type = _visitors_fself_type;
                 arity_ = _visitors_farity_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_unresolved_method env _visitors_ftrait_name
                 _visitors_fmethod_name _visitors_fself_type _visitors_farity_
                 _visitors_fty _visitors_floc_
           | Texpr_ident
               {
                 id = _visitors_fid;
                 ty_args_ = _visitors_fty_args_;
                 arity_ = _visitors_farity_;
                 kind = _visitors_fkind;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_ident env _visitors_fid _visitors_fty_args_
                 _visitors_farity_ _visitors_fkind _visitors_fty _visitors_floc_
           | Texpr_as
               {
                 expr = _visitors_fexpr;
                 trait = _visitors_ftrait;
                 ty = _visitors_fty;
                 is_implicit = _visitors_fis_implicit;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_as env _visitors_fexpr _visitors_ftrait
                 _visitors_fty _visitors_fis_implicit _visitors_floc_
           | Texpr_array
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 is_fixed_array = _visitors_fis_fixed_array;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array env _visitors_fexprs _visitors_fty
                 _visitors_fis_fixed_array _visitors_floc_
           | Texpr_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Texpr_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constr env _visitors_ftype_name
                 _visitors_fconstr _visitors_ftag _visitors_fty
                 _visitors_farity_ _visitors_floc_
           | Texpr_while
               {
                 loop_cond = _visitors_floop_cond;
                 loop_body = _visitors_floop_body;
                 while_else = _visitors_fwhile_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_while env _visitors_floop_cond
                 _visitors_floop_body _visitors_fwhile_else _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_function
               {
                 func = _visitors_ffunc;
                 ty = _visitors_fty;
                 is_raw_ = _visitors_fis_raw_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_function env _visitors_ffunc _visitors_fty
                 _visitors_fis_raw_ _visitors_floc_
           | Texpr_if
               {
                 cond = _visitors_fcond;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_if env _visitors_fcond _visitors_fifso
                 _visitors_fifnot _visitors_fty _visitors_floc_
           | Texpr_letfn
               {
                 binder = _visitors_fbinder;
                 fn = _visitors_ffn;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 is_rec = _visitors_fis_rec;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letfn env _visitors_fbinder _visitors_ffn
                 _visitors_fbody _visitors_fty _visitors_fis_rec _visitors_floc_
           | Texpr_letrec
               {
                 bindings = _visitors_fbindings;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letrec env _visitors_fbindings _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fbody _visitors_fty
                 _visitors_floc_
           | Texpr_sequence
               {
                 exprs = _visitors_fexprs;
                 last_expr = _visitors_flast_expr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_sequence env _visitors_fexprs
                 _visitors_flast_expr _visitors_fty _visitors_floc_
           | Texpr_tuple
               {
                 exprs = _visitors_fexprs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_tuple env _visitors_fexprs _visitors_fty
                 _visitors_floc_
           | Texpr_record
               {
                 type_name = _visitors_ftype_name;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record env _visitors_ftype_name
                 _visitors_ffields _visitors_fty _visitors_floc_
           | Texpr_record_update
               {
                 type_name = _visitors_ftype_name;
                 record = _visitors_frecord;
                 all_fields = _visitors_fall_fields;
                 fields = _visitors_ffields;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_record_update env _visitors_ftype_name
                 _visitors_frecord _visitors_fall_fields _visitors_ffields
                 _visitors_fty _visitors_floc_
           | Texpr_field
               {
                 record = _visitors_frecord;
                 accessor = _visitors_faccessor;
                 pos = _visitors_fpos;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_field env _visitors_frecord _visitors_faccessor
                 _visitors_fpos _visitors_fty _visitors_floc_
           | Texpr_mutate
               {
                 record = _visitors_frecord;
                 label = _visitors_flabel;
                 field = _visitors_ffield;
                 pos = _visitors_fpos;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_mutate env _visitors_frecord _visitors_flabel
                 _visitors_ffield _visitors_fpos _visitors_faugmented_by
                 _visitors_fty _visitors_floc_
           | Texpr_match
               {
                 expr = _visitors_fexpr;
                 cases = _visitors_fcases;
                 ty = _visitors_fty;
                 match_loc_ = _visitors_fmatch_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_match env _visitors_fexpr _visitors_fcases
                 _visitors_fty _visitors_fmatch_loc_ _visitors_floc_
           | Texpr_letmut
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_letmut env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_assign
               {
                 var = _visitors_fvar;
                 expr = _visitors_fexpr;
                 augmented_by = _visitors_faugmented_by;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_assign env _visitors_fvar _visitors_fexpr
                 _visitors_faugmented_by _visitors_fty _visitors_floc_
           | Texpr_hole
               {
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
                 kind = _visitors_fkind;
               } ->
               self#visit_Texpr_hole env _visitors_fty _visitors_floc_
                 _visitors_fkind
           | Texpr_unit { loc_ = _visitors_floc_ } ->
               self#visit_Texpr_unit env _visitors_floc_
           | Texpr_break
               {
                 arg = _visitors_farg;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_break env _visitors_farg _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_continue
               {
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_continue env _visitors_fargs _visitors_fty
                 _visitors_flabel _visitors_floc_
           | Texpr_loop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_loop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_for
               {
                 binders = _visitors_fbinders;
                 condition = _visitors_fcondition;
                 steps = _visitors_fsteps;
                 body = _visitors_fbody;
                 for_else = _visitors_ffor_else;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_for env _visitors_fbinders _visitors_fcondition
                 _visitors_fsteps _visitors_fbody _visitors_ffor_else
                 _visitors_fty _visitors_flabel _visitors_floc_
           | Texpr_foreach
               {
                 binders = _visitors_fbinders;
                 elem_tys = _visitors_felem_tys;
                 expr = _visitors_fexpr;
                 body = _visitors_fbody;
                 else_block = _visitors_felse_block;
                 ty = _visitors_fty;
                 label = _visitors_flabel;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_foreach env _visitors_fbinders
                 _visitors_felem_tys _visitors_fexpr _visitors_fbody
                 _visitors_felse_block _visitors_fty _visitors_flabel
                 _visitors_floc_
           | Texpr_return
               {
                 return_value = _visitors_freturn_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_return env _visitors_freturn_value _visitors_fty
                 _visitors_floc_
           | Texpr_raise
               {
                 error_value = _visitors_ferror_value;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_raise env _visitors_ferror_value _visitors_fty
                 _visitors_floc_
           | Texpr_try
               {
                 body = _visitors_fbody;
                 catch = _visitors_fcatch;
                 catch_all = _visitors_fcatch_all;
                 try_else = _visitors_ftry_else;
                 ty = _visitors_fty;
                 err_ty = _visitors_ferr_ty;
                 catch_loc_ = _visitors_fcatch_loc_;
                 else_loc_ = _visitors_felse_loc_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_try env _visitors_fbody _visitors_fcatch
                 _visitors_fcatch_all _visitors_ftry_else _visitors_fty
                 _visitors_ferr_ty _visitors_fcatch_loc_ _visitors_felse_loc_
                 _visitors_floc_
           | Texpr_exclamation
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 convert_to_result = _visitors_fconvert_to_result;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_exclamation env _visitors_fexpr _visitors_fty
                 _visitors_fconvert_to_result _visitors_floc_
           | Texpr_constraint
               {
                 expr = _visitors_fexpr;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_constraint env _visitors_fexpr
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Texpr_pipe
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_pipe env _visitors_flhs _visitors_frhs
                 _visitors_fty _visitors_floc_
           | Texpr_interp
               {
                 elems = _visitors_felems;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_interp env _visitors_felems _visitors_fty
                 _visitors_floc_
           | Texpr_guard
               {
                 cond = _visitors_fcond;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard env _visitors_fcond _visitors_fotherwise
                 _visitors_fbody _visitors_fty _visitors_floc_
           | Texpr_guard_let
               {
                 pat = _visitors_fpat;
                 rhs = _visitors_frhs;
                 pat_binders = _visitors_fpat_binders;
                 otherwise = _visitors_fotherwise;
                 body = _visitors_fbody;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_guard_let env _visitors_fpat _visitors_frhs
                 _visitors_fpat_binders _visitors_fotherwise _visitors_fbody
                 _visitors_fty _visitors_floc_
           | Texpr_is
               {
                 expr = _visitors_fexpr;
                 pat = _visitors_fpat;
                 pat_binders = _visitors_fpat_binders;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_is env _visitors_fexpr _visitors_fpat
                 _visitors_fpat_binders _visitors_floc_
           | Texpr_array_as_view
               {
                 array = _visitors_farray;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_array_as_view env _visitors_farray _visitors_fty
                 _visitors_floc_
           | Texpr_and
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_and env _visitors_flhs _visitors_frhs
                 _visitors_floc_
           | Texpr_or
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Texpr_or env _visitors_flhs _visitors_frhs
                 _visitors_floc_

      method visit_argument : _ -> argument -> argument =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_expr env _visitors_this.arg_value in
           let _visitors_r1 =
             self#visit_argument_kind env _visitors_this.arg_kind
           in
           { arg_value = _visitors_r0; arg_kind = _visitors_r1 }

      method visit_Pipe_partial_apply :
          _ -> expr -> argument list -> location -> pipe_rhs =
        fun env ->
          fun _visitors_ffunc ->
           fun _visitors_fargs ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_ffunc in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_argument env))
                 _visitors_fargs
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Pipe_partial_apply
               { func = _visitors_r0; args = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Pipe_invalid : _ -> expr -> stype -> location -> pipe_rhs =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Pipe_invalid
               { expr = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_pipe_rhs : _ -> pipe_rhs -> pipe_rhs =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Pipe_partial_apply
               {
                 func = _visitors_ffunc;
                 args = _visitors_fargs;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_partial_apply env _visitors_ffunc _visitors_fargs
                 _visitors_floc_
           | Pipe_invalid
               {
                 expr = _visitors_fexpr;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Pipe_invalid env _visitors_fexpr _visitors_fty
                 _visitors_floc_

      method visit_Interp_lit : _ -> string -> interp_elem =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_c0
           in
           Interp_lit _visitors_r0

      method visit_Interp_expr : _ -> expr -> expr -> location -> interp_elem =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fto_string ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_expr env _visitors_fexpr in
             let _visitors_r1 = self#visit_expr env _visitors_fto_string in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Interp_expr
               {
                 expr = _visitors_r0;
                 to_string = _visitors_r1;
                 loc_ = _visitors_r2;
               }

      method visit_interp_elem : _ -> interp_elem -> interp_elem =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Interp_lit _visitors_c0 -> self#visit_Interp_lit env _visitors_c0
           | Interp_expr
               {
                 expr = _visitors_fexpr;
                 to_string = _visitors_fto_string;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Interp_expr env _visitors_fexpr _visitors_fto_string
                 _visitors_floc_

      method visit_Field_def :
          _ -> Syntax.label -> expr -> bool -> bool -> int -> field_def =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fexpr ->
            fun _visitors_fis_mut ->
             fun _visitors_fis_pun ->
              fun _visitors_fpos ->
               let _visitors_r0 = self#visit_label env _visitors_flabel in
               let _visitors_r1 = self#visit_expr env _visitors_fexpr in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_mut
               in
               let _visitors_r3 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_pun
               in
               let _visitors_r4 =
                 (fun _visitors_this -> _visitors_this) _visitors_fpos
               in
               Field_def
                 {
                   label = _visitors_r0;
                   expr = _visitors_r1;
                   is_mut = _visitors_r2;
                   is_pun = _visitors_r3;
                   pos = _visitors_r4;
                 }

      method visit_field_def : _ -> field_def -> field_def =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_def
               {
                 label = _visitors_flabel;
                 expr = _visitors_fexpr;
                 is_mut = _visitors_fis_mut;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_def env _visitors_flabel _visitors_fexpr
                 _visitors_fis_mut _visitors_fis_pun _visitors_fpos

      method visit_Field_pat :
          _ -> Syntax.label -> pat -> bool -> int -> field_pat =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_fpat ->
            fun _visitors_fis_pun ->
             fun _visitors_fpos ->
              let _visitors_r0 = self#visit_label env _visitors_flabel in
              let _visitors_r1 = self#visit_pat env _visitors_fpat in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_pun
              in
              let _visitors_r3 =
                (fun _visitors_this -> _visitors_this) _visitors_fpos
              in
              Field_pat
                {
                  label = _visitors_r0;
                  pat = _visitors_r1;
                  is_pun = _visitors_r2;
                  pos = _visitors_r3;
                }

      method visit_field_pat : _ -> field_pat -> field_pat =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Field_pat
               {
                 label = _visitors_flabel;
                 pat = _visitors_fpat;
                 is_pun = _visitors_fis_pun;
                 pos = _visitors_fpos;
               } ->
               self#visit_Field_pat env _visitors_flabel _visitors_fpat
                 _visitors_fis_pun _visitors_fpos

      method visit_constr_pat_args : _ -> constr_pat_args -> constr_pat_args =
        fun env ->
          fun _visitors_this ->
           Basic_lst.map _visitors_this (self#visit_constr_pat_arg env)

      method visit_Constr_pat_arg :
          _ -> pat -> Syntax.argument_kind -> int -> constr_pat_arg =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkind ->
            fun _visitors_fpos ->
             let _visitors_r0 = self#visit_pat env _visitors_fpat in
             let _visitors_r1 = self#visit_argument_kind env _visitors_fkind in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_fpos
             in
             Constr_pat_arg
               { pat = _visitors_r0; kind = _visitors_r1; pos = _visitors_r2 }

      method visit_constr_pat_arg : _ -> constr_pat_arg -> constr_pat_arg =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Constr_pat_arg
               {
                 pat = _visitors_fpat;
                 kind = _visitors_fkind;
                 pos = _visitors_fpos;
               } ->
               self#visit_Constr_pat_arg env _visitors_fpat _visitors_fkind
                 _visitors_fpos

      method visit_Param :
          _ -> binder -> typ option -> stype -> param_kind -> param =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_binder env _visitors_fbinder in
              let _visitors_r1 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_typ env) t)
                  | None -> None)
                  _visitors_fkonstraint
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_param_kind env _visitors_fkind in
              Param
                {
                  binder = _visitors_r0;
                  konstraint = _visitors_r1;
                  ty = _visitors_r2;
                  kind = _visitors_r3;
                }

      method visit_Discard_positional_param :
          _ -> stype -> typ option -> location -> param =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_fkonstraint ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_stype env _visitors_fty in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_typ env) t)
                 | None -> None)
                 _visitors_fkonstraint
             in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Discard_positional_param
               {
                 ty = _visitors_r0;
                 konstraint = _visitors_r1;
                 loc_ = _visitors_r2;
               }

      method visit_param : _ -> param -> param =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Param
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 kind = _visitors_fkind;
               } ->
               self#visit_Param env _visitors_fbinder _visitors_fkonstraint
                 _visitors_fty _visitors_fkind
           | Discard_positional_param
               {
                 ty = _visitors_fty;
                 konstraint = _visitors_fkonstraint;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Discard_positional_param env _visitors_fty
                 _visitors_fkonstraint _visitors_floc_

      method visit_Positional : _ -> param_kind = fun env -> Positional
      method visit_Labelled : _ -> param_kind = fun env -> Labelled

      method visit_Optional : _ -> expr -> param_kind =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_expr env _visitors_c0 in
           Optional _visitors_r0

      method visit_Autofill : _ -> param_kind = fun env -> Autofill

      method visit_Question_optional : _ -> param_kind =
        fun env -> Question_optional

      method visit_param_kind : _ -> param_kind -> param_kind =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Positional -> self#visit_Positional env
           | Labelled -> self#visit_Labelled env
           | Optional _visitors_c0 -> self#visit_Optional env _visitors_c0
           | Autofill -> self#visit_Autofill env
           | Question_optional -> self#visit_Question_optional env

      method visit_params : _ -> params -> params =
        fun env ->
          fun _visitors_this ->
           Basic_lst.map _visitors_this (self#visit_param env)

      method visit_fn : _ -> fn -> fn =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_params env _visitors_this.params in
           let _visitors_r1 =
             self#visit_location env _visitors_this.params_loc_
           in
           let _visitors_r2 = self#visit_expr env _visitors_this.body in
           let _visitors_r3 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t ->
                   Some
                     ((fun (_visitors_c0, _visitors_c1) ->
                        let _visitors_r0 = self#visit_typ env _visitors_c0 in
                        let _visitors_r1 =
                          self#visit_error_typ env _visitors_c1
                        in
                        (_visitors_r0, _visitors_r1))
                        t)
               | None -> None)
               _visitors_this.ret_constraint
           in
           let _visitors_r4 = self#visit_stype env _visitors_this.ty in
           let _visitors_r5 = self#visit_fn_kind env _visitors_this.kind_ in
           {
             params = _visitors_r0;
             params_loc_ = _visitors_r1;
             body = _visitors_r2;
             ret_constraint = _visitors_r3;
             ty = _visitors_r4;
             kind_ = _visitors_r5;
           }

      method visit_match_case : _ -> match_case -> match_case =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_pat env _visitors_this.pat in
           let _visitors_r1 = self#visit_expr env _visitors_this.action in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_expr env) t)
               | None -> None)
               _visitors_this.guard
           in
           let _visitors_r3 =
             self#visit_pat_binders env _visitors_this.pat_binders
           in
           {
             pat = _visitors_r0;
             action = _visitors_r1;
             guard = _visitors_r2;
             pat_binders = _visitors_r3;
           }

      method visit_Error_typ : _ -> typ -> error_typ =
        fun env ->
          fun _visitors_fty ->
           let _visitors_r0 = self#visit_typ env _visitors_fty in
           Error_typ { ty = _visitors_r0 }

      method visit_Default_error_typ : _ -> location -> error_typ =
        fun env ->
          fun _visitors_floc_ ->
           let _visitors_r0 = self#visit_location env _visitors_floc_ in
           Default_error_typ { loc_ = _visitors_r0 }

      method visit_No_error_typ : _ -> error_typ = fun env -> No_error_typ

      method visit_error_typ : _ -> error_typ -> error_typ =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Error_typ { ty = _visitors_fty } ->
               self#visit_Error_typ env _visitors_fty
           | Default_error_typ { loc_ = _visitors_floc_ } ->
               self#visit_Default_error_typ env _visitors_floc_
           | No_error_typ -> self#visit_No_error_typ env

      method visit_Tany : _ -> stype -> location -> typ =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            Tany { ty = _visitors_r0; loc_ = _visitors_r1 }

      method visit_Tarrow :
          _ -> typ list -> typ -> error_typ -> stype -> location -> typ =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_freturn ->
            fun _visitors_ferr_ty ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_typ env))
                   _visitors_fparams
               in
               let _visitors_r1 = self#visit_typ env _visitors_freturn in
               let _visitors_r2 = self#visit_error_typ env _visitors_ferr_ty in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Tarrow
                 {
                   params = _visitors_r0;
                   return = _visitors_r1;
                   err_ty = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_T_tuple : _ -> typ list -> stype -> location -> typ =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_typ env))
                 _visitors_fparams
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             T_tuple
               { params = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Tname :
          _ -> constrid_loc -> typ list -> stype -> bool -> location -> typ =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fparams ->
            fun _visitors_fty ->
             fun _visitors_fis_alias_ ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_constrid_loc env _visitors_fconstr
               in
               let _visitors_r1 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_typ env))
                   _visitors_fparams
               in
               let _visitors_r2 = self#visit_stype env _visitors_fty in
               let _visitors_r3 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_alias_
               in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Tname
                 {
                   constr = _visitors_r0;
                   params = _visitors_r1;
                   ty = _visitors_r2;
                   is_alias_ = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_Tobject :
          _ -> constrid_loc -> stype -> bool -> location -> typ =
        fun env ->
          fun _visitors_fconstr ->
           fun _visitors_fty ->
            fun _visitors_fis_alias_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                self#visit_constrid_loc env _visitors_fconstr
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_alias_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tobject
                {
                  constr = _visitors_r0;
                  ty = _visitors_r1;
                  is_alias_ = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_typ : _ -> typ -> typ =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tany { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tany env _visitors_fty _visitors_floc_
           | Tarrow
               {
                 params = _visitors_fparams;
                 return = _visitors_freturn;
                 err_ty = _visitors_ferr_ty;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tarrow env _visitors_fparams _visitors_freturn
                 _visitors_ferr_ty _visitors_fty _visitors_floc_
           | T_tuple
               {
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_T_tuple env _visitors_fparams _visitors_fty
                 _visitors_floc_
           | Tname
               {
                 constr = _visitors_fconstr;
                 params = _visitors_fparams;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tname env _visitors_fconstr _visitors_fparams
                 _visitors_fty _visitors_fis_alias_ _visitors_floc_
           | Tobject
               {
                 constr = _visitors_fconstr;
                 ty = _visitors_fty;
                 is_alias_ = _visitors_fis_alias_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tobject env _visitors_fconstr _visitors_fty
                 _visitors_fis_alias_ _visitors_floc_

      method visit_Tpat_alias : _ -> pat -> binder -> stype -> location -> pat =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_falias ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_binder env _visitors_falias in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_alias
                {
                  pat = _visitors_r0;
                  alias = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_any : _ -> stype -> location -> pat =
        fun env ->
          fun _visitors_fty ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_stype env _visitors_fty in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            Tpat_any { ty = _visitors_r0; loc_ = _visitors_r1 }

      method visit_Tpat_array : _ -> array_pattern -> stype -> location -> pat =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_array_pattern env _visitors_fpats in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Tpat_array
               { pats = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Tpat_constant :
          _ -> constant -> stype -> var option -> location -> pat =
        fun env ->
          fun _visitors_fc ->
           fun _visitors_fty ->
            fun _visitors_fname_ ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this -> _visitors_this) _visitors_fc
              in
              let _visitors_r1 = self#visit_stype env _visitors_fty in
              let _visitors_r2 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_var env) t)
                  | None -> None)
                  _visitors_fname_
              in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_constant
                {
                  c = _visitors_r0;
                  ty = _visitors_r1;
                  name_ = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_constr :
          _ ->
          type_name option ->
          Syntax.constr_name ->
          constr_pat_args ->
          constr_tag ->
          stype ->
          bool ->
          fn_arity ->
          stype list ->
          location ->
          pat =
        fun env ->
          fun _visitors_ftype_name ->
           fun _visitors_fconstr ->
            fun _visitors_fargs ->
             fun _visitors_ftag ->
              fun _visitors_fty ->
               fun _visitors_fused_error_subtyping ->
                fun _visitors_farity_ ->
                 fun _visitors_fall_args_ ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_type_name env) t)
                       | None -> None)
                       _visitors_ftype_name
                   in
                   let _visitors_r1 =
                     self#visit_constr_name env _visitors_fconstr
                   in
                   let _visitors_r2 =
                     self#visit_constr_pat_args env _visitors_fargs
                   in
                   let _visitors_r3 =
                     self#visit_constr_tag env _visitors_ftag
                   in
                   let _visitors_r4 = self#visit_stype env _visitors_fty in
                   let _visitors_r5 =
                     (fun _visitors_this -> _visitors_this)
                       _visitors_fused_error_subtyping
                   in
                   let _visitors_r6 =
                     self#visit_fn_arity env _visitors_farity_
                   in
                   let _visitors_r7 =
                     (fun _visitors_this ->
                       Basic_lst.map _visitors_this (self#visit_stype env))
                       _visitors_fall_args_
                   in
                   let _visitors_r8 = self#visit_location env _visitors_floc_ in
                   Tpat_constr
                     {
                       type_name = _visitors_r0;
                       constr = _visitors_r1;
                       args = _visitors_r2;
                       tag = _visitors_r3;
                       ty = _visitors_r4;
                       used_error_subtyping = _visitors_r5;
                       arity_ = _visitors_r6;
                       all_args_ = _visitors_r7;
                       loc_ = _visitors_r8;
                     }

      method visit_Tpat_or : _ -> pat -> pat -> stype -> location -> pat =
        fun env ->
          fun _visitors_fpat1 ->
           fun _visitors_fpat2 ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat1 in
              let _visitors_r1 = self#visit_pat env _visitors_fpat2 in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_or
                {
                  pat1 = _visitors_r0;
                  pat2 = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_tuple : _ -> pat list -> stype -> location -> pat =
        fun env ->
          fun _visitors_fpats ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_pat env))
                 _visitors_fpats
             in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Tpat_tuple
               { pats = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Tpat_var : _ -> binder -> stype -> location -> pat =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fty ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_binder env _visitors_fbinder in
             let _visitors_r1 = self#visit_stype env _visitors_fty in
             let _visitors_r2 = self#visit_location env _visitors_floc_ in
             Tpat_var
               { binder = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

      method visit_Tpat_record :
          _ -> field_pat list -> bool -> stype -> location -> pat =
        fun env ->
          fun _visitors_ffields ->
           fun _visitors_fis_closed ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_field_pat env))
                  _visitors_ffields
              in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_closed
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_record
                {
                  fields = _visitors_r0;
                  is_closed = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_constraint : _ -> pat -> typ -> stype -> location -> pat
          =
        fun env ->
          fun _visitors_fpat ->
           fun _visitors_fkonstraint ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_pat env _visitors_fpat in
              let _visitors_r1 = self#visit_typ env _visitors_fkonstraint in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_constraint
                {
                  pat = _visitors_r0;
                  konstraint = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_map :
          _ ->
          (constant * pat) list ->
          ident * stype * stype array ->
          stype ->
          location ->
          pat =
        fun env ->
          fun _visitors_felems ->
           fun _visitors_fop_get_info_ ->
            fun _visitors_fty ->
             fun _visitors_floc_ ->
              let _visitors_r0 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> _visitors_this) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_pat env _visitors_c1 in
                      (_visitors_r0, _visitors_r1)))
                  _visitors_felems
              in
              let _visitors_r1 =
                (fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                  let _visitors_r0 = self#visit_ident env _visitors_c0 in
                  let _visitors_r1 = self#visit_stype env _visitors_c1 in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_arr.map _visitors_this (self#visit_stype env))
                      _visitors_c2
                  in
                  (_visitors_r0, _visitors_r1, _visitors_r2))
                  _visitors_fop_get_info_
              in
              let _visitors_r2 = self#visit_stype env _visitors_fty in
              let _visitors_r3 = self#visit_location env _visitors_floc_ in
              Tpat_map
                {
                  elems = _visitors_r0;
                  op_get_info_ = _visitors_r1;
                  ty = _visitors_r2;
                  loc_ = _visitors_r3;
                }

      method visit_Tpat_range :
          _ -> pat -> pat -> bool -> stype -> location -> pat =
        fun env ->
          fun _visitors_flhs ->
           fun _visitors_frhs ->
            fun _visitors_finclusive ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_pat env _visitors_flhs in
               let _visitors_r1 = self#visit_pat env _visitors_frhs in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_finclusive
               in
               let _visitors_r3 = self#visit_stype env _visitors_fty in
               let _visitors_r4 = self#visit_location env _visitors_floc_ in
               Tpat_range
                 {
                   lhs = _visitors_r0;
                   rhs = _visitors_r1;
                   inclusive = _visitors_r2;
                   ty = _visitors_r3;
                   loc_ = _visitors_r4;
                 }

      method visit_pat : _ -> pat -> pat =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Tpat_alias
               {
                 pat = _visitors_fpat;
                 alias = _visitors_falias;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_alias env _visitors_fpat _visitors_falias
                 _visitors_fty _visitors_floc_
           | Tpat_any { ty = _visitors_fty; loc_ = _visitors_floc_ } ->
               self#visit_Tpat_any env _visitors_fty _visitors_floc_
           | Tpat_array
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_array env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_constant
               {
                 c = _visitors_fc;
                 ty = _visitors_fty;
                 name_ = _visitors_fname_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constant env _visitors_fc _visitors_fty
                 _visitors_fname_ _visitors_floc_
           | Tpat_constr
               {
                 type_name = _visitors_ftype_name;
                 constr = _visitors_fconstr;
                 args = _visitors_fargs;
                 tag = _visitors_ftag;
                 ty = _visitors_fty;
                 used_error_subtyping = _visitors_fused_error_subtyping;
                 arity_ = _visitors_farity_;
                 all_args_ = _visitors_fall_args_;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constr env _visitors_ftype_name _visitors_fconstr
                 _visitors_fargs _visitors_ftag _visitors_fty
                 _visitors_fused_error_subtyping _visitors_farity_
                 _visitors_fall_args_ _visitors_floc_
           | Tpat_or
               {
                 pat1 = _visitors_fpat1;
                 pat2 = _visitors_fpat2;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_or env _visitors_fpat1 _visitors_fpat2
                 _visitors_fty _visitors_floc_
           | Tpat_tuple
               {
                 pats = _visitors_fpats;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_tuple env _visitors_fpats _visitors_fty
                 _visitors_floc_
           | Tpat_var
               {
                 binder = _visitors_fbinder;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_var env _visitors_fbinder _visitors_fty
                 _visitors_floc_
           | Tpat_record
               {
                 fields = _visitors_ffields;
                 is_closed = _visitors_fis_closed;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_record env _visitors_ffields _visitors_fis_closed
                 _visitors_fty _visitors_floc_
           | Tpat_constraint
               {
                 pat = _visitors_fpat;
                 konstraint = _visitors_fkonstraint;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_constraint env _visitors_fpat
                 _visitors_fkonstraint _visitors_fty _visitors_floc_
           | Tpat_map
               {
                 elems = _visitors_felems;
                 op_get_info_ = _visitors_fop_get_info_;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_map env _visitors_felems _visitors_fop_get_info_
                 _visitors_fty _visitors_floc_
           | Tpat_range
               {
                 lhs = _visitors_flhs;
                 rhs = _visitors_frhs;
                 inclusive = _visitors_finclusive;
                 ty = _visitors_fty;
                 loc_ = _visitors_floc_;
               } ->
               self#visit_Tpat_range env _visitors_flhs _visitors_frhs
                 _visitors_finclusive _visitors_fty _visitors_floc_

      method visit_Closed : _ -> pat list -> array_pattern =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_pat env))
               _visitors_c0
           in
           Closed _visitors_r0

      method visit_Open :
          _ -> pat list -> pat list -> (binder * stype) option -> array_pattern
          =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            fun _visitors_c2 ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_pat env))
                 _visitors_c0
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_pat env))
                 _visitors_c1
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t ->
                     Some
                       ((fun (_visitors_c0, _visitors_c1) ->
                          let _visitors_r0 =
                            self#visit_binder env _visitors_c0
                          in
                          let _visitors_r1 =
                            self#visit_stype env _visitors_c1
                          in
                          (_visitors_r0, _visitors_r1))
                          t)
                 | None -> None)
                 _visitors_c2
             in
             Open (_visitors_r0, _visitors_r1, _visitors_r2)

      method visit_array_pattern : _ -> array_pattern -> array_pattern =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Closed _visitors_c0 -> self#visit_Closed env _visitors_c0
           | Open (_visitors_c0, _visitors_c1, _visitors_c2) ->
               self#visit_Open env _visitors_c0 _visitors_c1 _visitors_c2

      method visit_fun_decl : _ -> fun_decl -> fun_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_fun_decl_kind env _visitors_this.kind
           in
           let _visitors_r1 = self#visit_binder env _visitors_this.fn_binder in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 =
             (fun _visitors_this -> _visitors_this) _visitors_this.is_pub
           in
           let _visitors_r4 =
             self#visit_tvar_env env _visitors_this.ty_params_
           in
           let _visitors_r5 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_type_name env))
               _visitors_this.constraint_names
           in
           let _visitors_r6 = self#visit_docstring env _visitors_this.doc_ in
           let _visitors_r7 = self#visit_attributes env _visitors_this.attrs in
           {
             kind = _visitors_r0;
             fn_binder = _visitors_r1;
             fn = _visitors_r2;
             is_pub = _visitors_r3;
             ty_params_ = _visitors_r4;
             constraint_names = _visitors_r5;
             doc_ = _visitors_r6;
             attrs = _visitors_r7;
           }

      method visit_Fun_kind_regular : _ -> fun_decl_kind =
        fun env -> Fun_kind_regular

      method visit_Fun_kind_method : _ -> type_name option -> fun_decl_kind =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_type_name env) t)
               | None -> None)
               _visitors_c0
           in
           Fun_kind_method _visitors_r0

      method visit_Fun_kind_default_impl : _ -> type_name -> fun_decl_kind =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_type_name env _visitors_c0 in
           Fun_kind_default_impl _visitors_r0

      method visit_Fun_kind_impl : _ -> typ -> type_name -> fun_decl_kind =
        fun env ->
          fun _visitors_fself_ty ->
           fun _visitors_ftrait ->
            let _visitors_r0 = self#visit_typ env _visitors_fself_ty in
            let _visitors_r1 = self#visit_type_name env _visitors_ftrait in
            Fun_kind_impl { self_ty = _visitors_r0; trait = _visitors_r1 }

      method visit_fun_decl_kind : _ -> fun_decl_kind -> fun_decl_kind =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Fun_kind_regular -> self#visit_Fun_kind_regular env
           | Fun_kind_method _visitors_c0 ->
               self#visit_Fun_kind_method env _visitors_c0
           | Fun_kind_default_impl _visitors_c0 ->
               self#visit_Fun_kind_default_impl env _visitors_c0
           | Fun_kind_impl
               { self_ty = _visitors_fself_ty; trait = _visitors_ftrait } ->
               self#visit_Fun_kind_impl env _visitors_fself_ty _visitors_ftrait

      method visit_Timpl_expr :
          _ -> expr -> bool -> _ -> absolute_loc -> bool -> impl =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_fis_main ->
            fun _visitors_fexpr_id ->
             fun _visitors_floc_ ->
              fun _visitors_fis_generated_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_main
               in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_fexpr_id
               in
               let _visitors_r3 = self#visit_absolute_loc env _visitors_floc_ in
               let _visitors_r4 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_generated_
               in
               Timpl_expr
                 {
                   expr = _visitors_r0;
                   is_main = _visitors_r1;
                   expr_id = _visitors_r2;
                   loc_ = _visitors_r3;
                   is_generated_ = _visitors_r4;
                 }

      method visit_Timpl_fun_decl :
          _ -> fun_decl -> fn_arity -> absolute_loc -> bool -> impl =
        fun env ->
          fun _visitors_ffun_decl ->
           fun _visitors_farity_ ->
            fun _visitors_floc_ ->
             fun _visitors_fis_generated_ ->
              let _visitors_r0 = self#visit_fun_decl env _visitors_ffun_decl in
              let _visitors_r1 = self#visit_fn_arity env _visitors_farity_ in
              let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
              let _visitors_r3 =
                (fun _visitors_this -> _visitors_this) _visitors_fis_generated_
              in
              Timpl_fun_decl
                {
                  fun_decl = _visitors_r0;
                  arity_ = _visitors_r1;
                  loc_ = _visitors_r2;
                  is_generated_ = _visitors_r3;
                }

      method visit_Timpl_letdef :
          _ ->
          binder ->
          typ option ->
          expr ->
          bool ->
          absolute_loc ->
          docstring ->
          attributes ->
          bool ->
          impl =
        fun env ->
          fun _visitors_fbinder ->
           fun _visitors_fkonstraint ->
            fun _visitors_fexpr ->
             fun _visitors_fis_pub ->
              fun _visitors_floc_ ->
               fun _visitors_fdoc_ ->
                fun _visitors_fattrs ->
                 fun _visitors_fis_generated_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_typ env) t)
                      | None -> None)
                      _visitors_fkonstraint
                  in
                  let _visitors_r2 = self#visit_expr env _visitors_fexpr in
                  let _visitors_r3 =
                    (fun _visitors_this -> _visitors_this) _visitors_fis_pub
                  in
                  let _visitors_r4 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  let _visitors_r5 = self#visit_docstring env _visitors_fdoc_ in
                  let _visitors_r6 =
                    self#visit_attributes env _visitors_fattrs
                  in
                  let _visitors_r7 =
                    (fun _visitors_this -> _visitors_this)
                      _visitors_fis_generated_
                  in
                  Timpl_letdef
                    {
                      binder = _visitors_r0;
                      konstraint = _visitors_r1;
                      expr = _visitors_r2;
                      is_pub = _visitors_r3;
                      loc_ = _visitors_r4;
                      doc_ = _visitors_r5;
                      attrs = _visitors_r6;
                      is_generated_ = _visitors_r7;
                    }

      method visit_Timpl_stub_decl : _ -> stub_decl -> impl =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_stub_decl env _visitors_c0 in
           Timpl_stub_decl _visitors_r0

      method visit_impl : _ -> impl -> impl =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Timpl_expr
               {
                 expr = _visitors_fexpr;
                 is_main = _visitors_fis_main;
                 expr_id = _visitors_fexpr_id;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_expr env _visitors_fexpr _visitors_fis_main
                 _visitors_fexpr_id _visitors_floc_ _visitors_fis_generated_
           | Timpl_fun_decl
               {
                 fun_decl = _visitors_ffun_decl;
                 arity_ = _visitors_farity_;
                 loc_ = _visitors_floc_;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_fun_decl env _visitors_ffun_decl
                 _visitors_farity_ _visitors_floc_ _visitors_fis_generated_
           | Timpl_letdef
               {
                 binder = _visitors_fbinder;
                 konstraint = _visitors_fkonstraint;
                 expr = _visitors_fexpr;
                 is_pub = _visitors_fis_pub;
                 loc_ = _visitors_floc_;
                 doc_ = _visitors_fdoc_;
                 attrs = _visitors_fattrs;
                 is_generated_ = _visitors_fis_generated_;
               } ->
               self#visit_Timpl_letdef env _visitors_fbinder
                 _visitors_fkonstraint _visitors_fexpr _visitors_fis_pub
                 _visitors_floc_ _visitors_fdoc_ _visitors_fattrs
                 _visitors_fis_generated_
           | Timpl_stub_decl _visitors_c0 ->
               self#visit_Timpl_stub_decl env _visitors_c0

      method visit_impls : _ -> impls -> impls =
        fun env ->
          fun _visitors_this ->
           Basic_lst.map _visitors_this (self#visit_impl env)

      method visit_type_decl : _ -> type_decl -> type_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.td_binder
           in
           let _visitors_r1 =
             self#visit_tvar_env env _visitors_this.td_params
           in
           let _visitors_r2 = self#visit_type_desc env _visitors_this.td_desc in
           let _visitors_r3 = self#visit_visibility env _visitors_this.td_vis in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.td_loc_
           in
           let _visitors_r5 = self#visit_docstring env _visitors_this.td_doc_ in
           let _visitors_r6 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_type_name env))
               _visitors_this.td_deriving_
           in
           {
             td_binder = _visitors_r0;
             td_params = _visitors_r1;
             td_desc = _visitors_r2;
             td_vis = _visitors_r3;
             td_loc_ = _visitors_r4;
             td_doc_ = _visitors_r5;
             td_deriving_ = _visitors_r6;
           }

      method visit_No_payload : _ -> exception_decl = fun env -> No_payload

      method visit_Single_payload : _ -> typ -> exception_decl =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           Single_payload _visitors_r0

      method visit_Enum_payload : _ -> constr_decl list -> exception_decl =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_constr_decl env))
               _visitors_c0
           in
           Enum_payload _visitors_r0

      method visit_exception_decl : _ -> exception_decl -> exception_decl =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | No_payload -> self#visit_No_payload env
           | Single_payload _visitors_c0 ->
               self#visit_Single_payload env _visitors_c0
           | Enum_payload _visitors_c0 ->
               self#visit_Enum_payload env _visitors_c0

      method visit_Td_abstract : _ -> type_desc = fun env -> Td_abstract

      method visit_Td_error : _ -> exception_decl -> type_desc =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_exception_decl env _visitors_c0 in
           Td_error _visitors_r0

      method visit_Td_newtype : _ -> typ -> type_desc =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           Td_newtype _visitors_r0

      method visit_Td_variant : _ -> constr_decl list -> type_desc =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_constr_decl env))
               _visitors_c0
           in
           Td_variant _visitors_r0

      method visit_Td_record : _ -> field_decl list -> type_desc =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_field_decl env))
               _visitors_c0
           in
           Td_record _visitors_r0

      method visit_Td_alias : _ -> typ -> type_desc =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typ env _visitors_c0 in
           Td_alias _visitors_r0

      method visit_type_desc : _ -> type_desc -> type_desc =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Td_abstract -> self#visit_Td_abstract env
           | Td_error _visitors_c0 -> self#visit_Td_error env _visitors_c0
           | Td_newtype _visitors_c0 -> self#visit_Td_newtype env _visitors_c0
           | Td_variant _visitors_c0 -> self#visit_Td_variant env _visitors_c0
           | Td_record _visitors_c0 -> self#visit_Td_record env _visitors_c0
           | Td_alias _visitors_c0 -> self#visit_Td_alias env _visitors_c0

      method visit_constr_decl_arg : _ -> constr_decl_arg -> constr_decl_arg =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_typ env _visitors_this.carg_typ in
           let _visitors_r1 =
             (fun _visitors_this -> _visitors_this) _visitors_this.carg_mut
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_label env) t)
               | None -> None)
               _visitors_this.carg_label
           in
           {
             carg_typ = _visitors_r0;
             carg_mut = _visitors_r1;
             carg_label = _visitors_r2;
           }

      method visit_constr_decl : _ -> constr_decl -> constr_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.constr_name in
           let _visitors_r1 =
             self#visit_constr_tag env _visitors_this.constr_tag
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_constr_decl_arg env))
               _visitors_this.constr_args
           in
           let _visitors_r3 =
             self#visit_fn_arity env _visitors_this.constr_arity_
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.constr_loc_
           in
           {
             constr_name = _visitors_r0;
             constr_tag = _visitors_r1;
             constr_args = _visitors_r2;
             constr_arity_ = _visitors_r3;
             constr_loc_ = _visitors_r4;
           }

      method visit_field_decl : _ -> field_decl -> field_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_label env _visitors_this.field_label in
           let _visitors_r1 = self#visit_typ env _visitors_this.field_typ in
           let _visitors_r2 =
             (fun _visitors_this -> _visitors_this) _visitors_this.field_mut
           in
           let _visitors_r3 =
             self#visit_type_component_visibility env _visitors_this.field_vis
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.field_loc_
           in
           {
             field_label = _visitors_r0;
             field_typ = _visitors_r1;
             field_mut = _visitors_r2;
             field_vis = _visitors_r3;
             field_loc_ = _visitors_r4;
           }

      method visit_trait_decl : _ -> trait_decl -> trait_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_name
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_type_name env))
               _visitors_this.trait_supers
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_method_decl env))
               _visitors_this.trait_methods
           in
           let _visitors_r3 =
             self#visit_visibility env _visitors_this.trait_vis
           in
           let _visitors_r4 =
             self#visit_absolute_loc env _visitors_this.trait_loc_
           in
           let _visitors_r5 =
             self#visit_docstring env _visitors_this.trait_doc_
           in
           let _visitors_r6 =
             self#visit_attributes env _visitors_this.trait_attrs
           in
           {
             trait_name = _visitors_r0;
             trait_supers = _visitors_r1;
             trait_methods = _visitors_r2;
             trait_vis = _visitors_r3;
             trait_loc_ = _visitors_r4;
             trait_doc_ = _visitors_r5;
             trait_attrs = _visitors_r6;
           }

      method visit_trait_alias_decl : _ -> trait_alias_decl -> trait_alias_decl
          =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_type_constr_loc env _visitors_this.trait_alias_name
           in
           let _visitors_r1 =
             self#visit_type_name env _visitors_this.trait_alias_target
           in
           let _visitors_r2 =
             (fun _visitors_this -> _visitors_this)
               _visitors_this.trait_alias_is_pub
           in
           let _visitors_r3 =
             self#visit_absolute_loc env _visitors_this.trait_alias_loc_
           in
           let _visitors_r4 =
             self#visit_docstring env _visitors_this.trait_alias_doc_
           in
           {
             trait_alias_name = _visitors_r0;
             trait_alias_target = _visitors_r1;
             trait_alias_is_pub = _visitors_r2;
             trait_alias_loc_ = _visitors_r3;
             trait_alias_doc_ = _visitors_r4;
           }

      method visit_method_decl : _ -> method_decl -> method_decl =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_syntax_binder env _visitors_this.method_name
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_label env) t)
                       | None -> None)
                       _visitors_c0
                   in
                   let _visitors_r1 = self#visit_typ env _visitors_c1 in
                   (_visitors_r0, _visitors_r1)))
               _visitors_this.method_params
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_typ env) t)
               | None -> None)
               _visitors_this.method_ret
           in
           let _visitors_r3 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_typ env) t)
               | None -> None)
               _visitors_this.method_err
           in
           let _visitors_r4 =
             self#visit_location env _visitors_this.method_loc_
           in
           {
             method_name = _visitors_r0;
             method_params = _visitors_r1;
             method_ret = _visitors_r2;
             method_err = _visitors_r3;
             method_loc_ = _visitors_r4;
           }

      method visit_Output :
          _ ->
          impls ->
          type_decl list ->
          trait_decl list ->
          trait_alias_decl list ->
          output =
        fun env ->
          fun _visitors_fvalue_defs ->
           fun _visitors_ftype_defs ->
            fun _visitors_ftrait_defs ->
             fun _visitors_ftrait_alias ->
              let _visitors_r0 = self#visit_impls env _visitors_fvalue_defs in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_type_decl env))
                  _visitors_ftype_defs
              in
              let _visitors_r2 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_trait_decl env))
                  _visitors_ftrait_defs
              in
              let _visitors_r3 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_trait_alias_decl env))
                  _visitors_ftrait_alias
              in
              Output
                {
                  value_defs = _visitors_r0;
                  type_defs = _visitors_r1;
                  trait_defs = _visitors_r2;
                  trait_alias = _visitors_r3;
                }

      method visit_output : _ -> output -> output =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Output
               {
                 value_defs = _visitors_fvalue_defs;
                 type_defs = _visitors_ftype_defs;
                 trait_defs = _visitors_ftrait_defs;
                 trait_alias = _visitors_ftrait_alias;
               } ->
               self#visit_Output env _visitors_fvalue_defs _visitors_ftype_defs
                 _visitors_ftrait_defs _visitors_ftrait_alias
    end

  [@@@VISITORS.END]
end

include struct
  let _ = fun (_ : stub_decl) -> ()
  let _ = fun (_ : stub_body) -> ()
  let _ = fun (_ : expr) -> ()
  let _ = fun (_ : argument) -> ()
  let _ = fun (_ : pipe_rhs) -> ()
  let _ = fun (_ : interp_elem) -> ()
  let _ = fun (_ : field_def) -> ()
  let _ = fun (_ : field_pat) -> ()
  let _ = fun (_ : constr_pat_args) -> ()
  let _ = fun (_ : constr_pat_arg) -> ()
  let _ = fun (_ : param) -> ()
  let _ = fun (_ : param_kind) -> ()
  let _ = fun (_ : params) -> ()
  let _ = fun (_ : fn) -> ()
  let _ = fun (_ : match_case) -> ()
  let _ = fun (_ : error_typ) -> ()
  let _ = fun (_ : typ) -> ()
  let _ = fun (_ : pat) -> ()
  let _ = fun (_ : array_pattern) -> ()
  let _ = fun (_ : fun_decl) -> ()
  let _ = fun (_ : fun_decl_kind) -> ()
  let _ = fun (_ : impl) -> ()
  let _ = fun (_ : impls) -> ()
  let _ = fun (_ : type_decl) -> ()
  let _ = fun (_ : exception_decl) -> ()
  let _ = fun (_ : type_desc) -> ()
  let _ = fun (_ : constr_decl_arg) -> ()
  let _ = fun (_ : constr_decl) -> ()
  let _ = fun (_ : field_decl) -> ()
  let _ = fun (_ : trait_decl) -> ()
  let _ = fun (_ : trait_alias_decl) -> ()
  let _ = fun (_ : method_decl) -> ()
  let _ = fun (_ : output) -> ()
end

let filter_fields ctor fields =
  Lst.fold_right fields [] (fun field ->
      fun acc ->
       match field with
       | ("params_loc_" | "loc_" | "match_loc_" | "else_loc_" | "catch_loc_"), _
         when not !Basic_config.show_loc ->
           acc
       | "is_pub", S.Atom "false" -> acc
       | "ty_params_", List [] -> acc
       | "is_pun", Atom "false" -> acc
       | "doc_", Atom "" -> acc
       | "type_name", List [] -> acc
       | "continue_block", List [] -> acc
       | "is_generated_", Atom "false" -> acc
       | "is_generated_", Atom "true" ->
           ("is_generated_", S.List [ Atom "is_generated"; Atom "true" ]) :: acc
       | "args", List [] when ctor = "Texpr_continue" -> acc
       | "arg", List [] when ctor = "Texpr_break" -> acc
       | "is_main", S.Atom "false" -> acc
       | "arity_", List [] -> acc
       | "arity_", List (List param_kinds :: [])
         when Lst.for_all param_kinds (function Atom "_" -> true | _ -> false)
         ->
           acc
       | "arity_", List param_kinds
         when Lst.for_all param_kinds (function Atom "_" -> true | _ -> false)
         ->
           acc
       | "kind", Atom "Positional" when ctor = "Param" -> acc
       | "kind", Atom "Kind_regular"
       | "kind", List [ Atom "Kind_method"; List [] ] ->
           acc
       | ("doc_" | "intf_doc_"), List [] -> acc
       | "kind_", Atom "Lambda" -> acc
       | "constr_", List [] -> acc
       | "augmented_by", List [] -> acc
       | "is_alias_", Atom "false" -> acc
       | "used_error_subtyping", Atom "false" -> acc
       | "name_", List [] -> acc
       | "label", List [] -> acc
       | "constraint_names", List [] -> acc
       | "is_raw_", Atom "false" -> acc
       | "is_raw_", Atom "true" -> ("is_raw_", List [ Atom "raw" ]) :: acc
       | "guard", List [] -> acc
       | _ -> field :: acc)

type loc_ctx = Use_absolute_loc of absolute_loc | Use_relative_loc

let sexp =
  object (self)
    inherit [_] sexp as super

    method! visit_inline_record env ctor fields =
      super#visit_inline_record env ctor (filter_fields ctor fields)

    method! visit_docstring env docstring =
      let comment = Docstring.comment_string docstring in
      if comment = "" && Docstring.pragmas docstring = [] then S.List []
      else if Docstring.pragmas docstring = [] then S.Atom comment
      else super#visit_docstring env docstring

    method! visit_record env fields =
      super#visit_record env (filter_fields "" fields)

    method! visit_argument env arg =
      match arg.arg_kind with
      | Positional -> self#visit_expr env arg.arg_value
      | kind ->
          S.List
            [
              self#visit_expr env arg.arg_value;
              Syntax.sexp_of_argument_kind kind;
            ]

    method! visit_constr_pat_args env args =
      if
        Lst.for_all args (fun (Constr_pat_arg { kind; _ }) ->
            match kind with
            | Positional -> true
            | Labelled _ | Labelled_pun _ | Labelled_option _
            | Labelled_option_pun _ ->
                false)
      then
        S.List
          (Lst.map args (fun (Constr_pat_arg { pat; _ }) ->
               self#visit_pat env pat))
      else super#visit_constr_pat_args env args

    method! visit_constr_decl_arg env arg =
      let typ = self#visit_typ env arg.carg_typ in
      match arg.carg_label with
      | None -> typ
      | Some label ->
          List
            (Atom "Labelled" :: self#visit_label env label :: typ
            :: (if arg.carg_mut then [ List [ Atom "mut" ] ] else []))

    method! visit_location env loc =
      match env with
      | Use_absolute_loc base -> sexp_of_absolute_loc (Rloc.to_loc ~base loc)
      | Use_relative_loc -> super#visit_location env loc
  end

let sexp_of_impls ~use_absolute_loc impls =
  (let impls =
     Lst.map impls (fun impl ->
         let ctx =
           if use_absolute_loc then
             let base =
               match impl with
               | Timpl_expr { loc_; _ }
               | Timpl_fun_decl { loc_; _ }
               | Timpl_letdef { loc_; _ }
               | Timpl_stub_decl { loc_; _ } ->
                   loc_
             in
             Use_absolute_loc base
           else Use_relative_loc
         in
         Basic_compress_stamp.normalize (sexp#visit_impl ctx impl))
   in
   S.List impls
    : S.t)

let sexp_of_output ?(use_absolute_loc = false) (Output { value_defs; _ }) =
  sexp_of_impls ~use_absolute_loc value_defs

let loc_of_impl impl =
  match impl with
  | Timpl_expr { loc_; _ }
  | Timpl_fun_decl { loc_; _ }
  | Timpl_letdef { loc_; _ }
  | Timpl_stub_decl { loc_; _ } ->
      loc_

let loc_of_pat (pat : pat) =
  match pat with
  | Tpat_alias { loc_; _ }
  | Tpat_any { loc_; _ }
  | Tpat_array { loc_; _ }
  | Tpat_constant { loc_; _ }
  | Tpat_constr { loc_; _ }
  | Tpat_or { loc_; _ }
  | Tpat_tuple { loc_; _ }
  | Tpat_var { loc_; _ }
  | Tpat_range { loc_; _ }
  | Tpat_record { loc_; _ }
  | Tpat_constraint { loc_; _ }
  | Tpat_map { loc_; _ } ->
      loc_

let loc_of_typed_expr te =
  match te with
  | Texpr_apply { loc_; _ }
  | Texpr_array { loc_; _ }
  | Texpr_constant { loc_; _ }
  | Texpr_constr { loc_; _ }
  | Texpr_while { loc_; _ }
  | Texpr_function { loc_; _ }
  | Texpr_ident { loc_; _ }
  | Texpr_method { loc_; _ }
  | Texpr_unresolved_method { loc_; _ }
  | Texpr_as { loc_; _ }
  | Texpr_if { loc_; _ }
  | Texpr_letfn { loc_; _ }
  | Texpr_letrec { loc_; _ }
  | Texpr_let { loc_; _ }
  | Texpr_sequence { loc_; _ }
  | Texpr_tuple { loc_; _ }
  | Texpr_record { loc_; _ }
  | Texpr_record_update { loc_; _ }
  | Texpr_field { loc_; _ }
  | Texpr_mutate { loc_; _ }
  | Texpr_match { loc_; _ }
  | Texpr_letmut { loc_; _ }
  | Texpr_assign { loc_; _ }
  | Texpr_hole { loc_; _ }
  | Texpr_break { loc_; _ }
  | Texpr_continue { loc_; _ }
  | Texpr_loop { loc_; _ }
  | Texpr_for { loc_; _ }
  | Texpr_foreach { loc_; _ }
  | Texpr_unit { loc_; _ }
  | Texpr_return { loc_; _ }
  | Texpr_raise { loc_; _ }
  | Texpr_try { loc_; _ }
  | Texpr_exclamation { loc_; _ }
  | Texpr_constraint { loc_; _ }
  | Texpr_pipe { loc_; _ }
  | Texpr_interp { loc_; _ }
  | Texpr_guard { loc_; _ }
  | Texpr_guard_let { loc_; _ }
  | Texpr_array_as_view { loc_; _ }
  | Texpr_is { loc_; _ }
  | Texpr_and { loc_; _ }
  | Texpr_or { loc_; _ } ->
      loc_

let loc_of_typ ty =
  match ty with
  | Tany { loc_; _ }
  | Tname { loc_; _ }
  | Tarrow { loc_; _ }
  | T_tuple { loc_; _ }
  | Tobject { loc_; _ } ->
      loc_

let loc_of_pipe_rhs = function
  | Pipe_invalid { loc_; _ } | Pipe_partial_apply { loc_; _ } -> loc_
[@@dead "+loc_of_pipe_rhs"]
