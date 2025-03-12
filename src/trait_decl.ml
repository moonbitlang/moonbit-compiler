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
module Type_path = Basic_type_path
module Lst = Basic_lst

type method_not_object_safe_reason =
  | First_param_not_self
  | Self_in_return
  | Multiple_self

include struct
  let _ = fun (_ : method_not_object_safe_reason) -> ()

  let sexp_of_method_not_object_safe_reason =
    (function
     | First_param_not_self -> S.Atom "First_param_not_self"
     | Self_in_return -> S.Atom "Self_in_return"
     | Multiple_self -> S.Atom "Multiple_self"
      : method_not_object_safe_reason -> S.t)

  let _ = sexp_of_method_not_object_safe_reason
end

type not_object_safe_reason =
  | Bad_method of string * method_not_object_safe_reason
  | Bad_super_trait of Type_path.t

include struct
  let _ = fun (_ : not_object_safe_reason) -> ()

  let sexp_of_not_object_safe_reason =
    (function
     | Bad_method (arg0__001_, arg1__002_) ->
         let res0__003_ = Moon_sexp_conv.sexp_of_string arg0__001_
         and res1__004_ = sexp_of_method_not_object_safe_reason arg1__002_ in
         S.List [ S.Atom "Bad_method"; res0__003_; res1__004_ ]
     | Bad_super_trait arg0__005_ ->
         let res0__006_ = Type_path.sexp_of_t arg0__005_ in
         S.List [ S.Atom "Bad_super_trait"; res0__006_ ]
      : not_object_safe_reason -> S.t)

  let _ = sexp_of_not_object_safe_reason
end

type object_safety_status = not_object_safe_reason list

include struct
  let _ = fun (_ : object_safety_status) -> ()

  let sexp_of_object_safety_status =
    (fun x__007_ ->
       Moon_sexp_conv.sexp_of_list sexp_of_not_object_safe_reason x__007_
      : object_safety_status -> S.t)

  let _ = sexp_of_object_safety_status
end

let type_not_object_safe ~name ~reasons ~loc =
  let reasons =
    Lst.map reasons (fun reason ->
        match reason with
        | Bad_method (method_name, First_param_not_self) ->
            ("The first parameter of method " ^ method_name ^ " is not `Self`"
              : Stdlib.String.t)
              [@merlin.hide]
        | Bad_method (method_name, Self_in_return) ->
            ("`Self` occur in the return type of method " ^ method_name
              : Stdlib.String.t)
              [@merlin.hide]
        | Bad_method (method_name, Multiple_self) ->
            ("`Self` occur multiple times in the type of method " ^ method_name
              : Stdlib.String.t)
              [@merlin.hide]
        | Bad_super_trait super ->
            ("Its super trait " ^ Type_path_util.name super
             ^ " does not allow objects"
              : Stdlib.String.t)
              [@merlin.hide])
  in
  Errors.type_not_object_safe ~name ~reasons ~loc

type type_path = Type_path.t

include struct
  let _ = fun (_ : type_path) -> ()
  let sexp_of_type_path = (Type_path.sexp_of_t : type_path -> S.t)
  let _ = sexp_of_type_path
end

type tvar_env = Tvar_env.t

include struct
  let _ = fun (_ : tvar_env) -> ()
  let sexp_of_tvar_env = (Tvar_env.sexp_of_t : tvar_env -> S.t)
  let _ = sexp_of_tvar_env
end

type typ = Stype.t

include struct
  let _ = fun (_ : typ) -> ()
  let sexp_of_typ = (Stype.sexp_of_t : typ -> S.t)
  let _ = sexp_of_typ
end

type location = Loc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Loc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type docstring = Docstring.t

include struct
  let _ = fun (_ : docstring) -> ()
  let sexp_of_docstring = (Docstring.sexp_of_t : docstring -> S.t)
  let _ = sexp_of_docstring
end

type method_decl = {
  method_name : string;
  method_typ : typ;
  method_arity : Fn_arity.t; [@sexp_drop_if Fn_arity.is_simple]
  method_loc_ : location;
}

include struct
  let _ = fun (_ : method_decl) -> ()

  let sexp_of_method_decl =
    (let (drop_if__014_ : Fn_arity.t -> Stdlib.Bool.t) = Fn_arity.is_simple in
     fun {
           method_name = method_name__009_;
           method_typ = method_typ__011_;
           method_arity = method_arity__015_;
           method_loc_ = method_loc___018_;
         } ->
       let bnds__008_ = ([] : _ Stdlib.List.t) in
       let bnds__008_ =
         let arg__019_ = sexp_of_location method_loc___018_ in
         (S.List [ S.Atom "method_loc_"; arg__019_ ] :: bnds__008_
           : _ Stdlib.List.t)
       in
       let bnds__008_ =
         if drop_if__014_ method_arity__015_ then bnds__008_
         else
           let arg__017_ = Fn_arity.sexp_of_t method_arity__015_ in
           let bnd__016_ = S.List [ S.Atom "method_arity"; arg__017_ ] in
           (bnd__016_ :: bnds__008_ : _ Stdlib.List.t)
       in
       let bnds__008_ =
         let arg__012_ = sexp_of_typ method_typ__011_ in
         (S.List [ S.Atom "method_typ"; arg__012_ ] :: bnds__008_
           : _ Stdlib.List.t)
       in
       let bnds__008_ =
         let arg__010_ = Moon_sexp_conv.sexp_of_string method_name__009_ in
         (S.List [ S.Atom "method_name"; arg__010_ ] :: bnds__008_
           : _ Stdlib.List.t)
       in
       S.List bnds__008_
      : method_decl -> S.t)

  let _ = sexp_of_method_decl
end

type t = {
  name : type_path;
  supers : type_path list; [@sexp_drop_if function [] -> true | _ -> false]
  closure : type_path list;
  closure_methods : (type_path * method_decl) list;
  methods : method_decl list;
  vis_ : Typedecl_info.visibility;
  loc_ : location;
  doc_ : docstring;
      [@default Docstring.empty] [@sexp_drop_if Docstring.is_empty]
  attrs : Checked_attributes.t;
  object_safety_ : object_safety_status;
      [@sexp_drop_if function [] -> true | _ -> false]
}

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (let (drop_if__024_ : type_path list -> Stdlib.Bool.t) = function
       | [] -> true
       | _ -> false
     and (drop_if__043_ : docstring -> Stdlib.Bool.t) = Docstring.is_empty
     and (drop_if__050_ : object_safety_status -> Stdlib.Bool.t) = function
       | [] -> true
       | _ -> false
     in
     fun {
           name = name__021_;
           supers = supers__025_;
           closure = closure__028_;
           closure_methods = closure_methods__030_;
           methods = methods__036_;
           vis_ = vis___038_;
           loc_ = loc___040_;
           doc_ = doc___044_;
           attrs = attrs__047_;
           object_safety_ = object_safety___051_;
         } ->
       let bnds__020_ = ([] : _ Stdlib.List.t) in
       let bnds__020_ =
         if drop_if__050_ object_safety___051_ then bnds__020_
         else
           let arg__053_ = sexp_of_object_safety_status object_safety___051_ in
           let bnd__052_ = S.List [ S.Atom "object_safety_"; arg__053_ ] in
           (bnd__052_ :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__048_ = Checked_attributes.sexp_of_t attrs__047_ in
         (S.List [ S.Atom "attrs"; arg__048_ ] :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         if drop_if__043_ doc___044_ then bnds__020_
         else
           let arg__046_ = sexp_of_docstring doc___044_ in
           let bnd__045_ = S.List [ S.Atom "doc_"; arg__046_ ] in
           (bnd__045_ :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__041_ = sexp_of_location loc___040_ in
         (S.List [ S.Atom "loc_"; arg__041_ ] :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__039_ = Typedecl_info.sexp_of_visibility vis___038_ in
         (S.List [ S.Atom "vis_"; arg__039_ ] :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__037_ =
           Moon_sexp_conv.sexp_of_list sexp_of_method_decl methods__036_
         in
         (S.List [ S.Atom "methods"; arg__037_ ] :: bnds__020_
           : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__031_ =
           Moon_sexp_conv.sexp_of_list
             (fun (arg0__032_, arg1__033_) ->
               let res0__034_ = sexp_of_type_path arg0__032_
               and res1__035_ = sexp_of_method_decl arg1__033_ in
               S.List [ res0__034_; res1__035_ ])
             closure_methods__030_
         in
         (S.List [ S.Atom "closure_methods"; arg__031_ ] :: bnds__020_
           : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__029_ =
           Moon_sexp_conv.sexp_of_list sexp_of_type_path closure__028_
         in
         (S.List [ S.Atom "closure"; arg__029_ ] :: bnds__020_
           : _ Stdlib.List.t)
       in
       let bnds__020_ =
         if drop_if__024_ supers__025_ then bnds__020_
         else
           let arg__027_ =
             (Moon_sexp_conv.sexp_of_list sexp_of_type_path) supers__025_
           in
           let bnd__026_ = S.List [ S.Atom "supers"; arg__027_ ] in
           (bnd__026_ :: bnds__020_ : _ Stdlib.List.t)
       in
       let bnds__020_ =
         let arg__022_ = sexp_of_type_path name__021_ in
         (S.List [ S.Atom "name"; arg__022_ ] :: bnds__020_ : _ Stdlib.List.t)
       in
       S.List bnds__020_
      : t -> S.t)

  let _ = sexp_of_t
end

let get_methods_object_safety (decl : Syntax.trait_decl) =
  Lst.fold_right decl.trait_methods []
    (fun (Trait_method { name; params; return_type; _ }) ->
      fun acc ->
       match params with
       | {
           tmparam_typ = Ptype_name { constr_id = { lid = Lident "Self" }; _ };
           _;
         }
         :: ps -> (
           let exception Invalid_method of method_not_object_safe_reason in
           let rec check_no_self reason (ty : Syntax.typ) =
             match ty with
             | Ptype_name { constr_id; tys } ->
                 (match constr_id.lid with
                 | Lident "Self" -> raise_notrace (Invalid_method reason)
                 | _ -> ());
                 Basic_lst.iter tys ~f:(check_no_self reason)
             | Ptype_tuple { tys } ->
                 Basic_lst.iter tys ~f:(check_no_self reason)
             | Ptype_arrow { ty_arg; ty_res; ty_err; is_async } -> (
                 assert (not is_async);
                 Basic_lst.iter ty_arg ~f:(check_no_self reason);
                 check_no_self reason ty_res;
                 match ty_err with
                 | No_error_typ | Default_error_typ _ -> ()
                 | Error_typ { ty = ty_err } -> check_no_self reason ty_err)
             | Ptype_any _ -> ()
             | Ptype_option { ty; _ } -> check_no_self reason ty
             | Ptype_object _ -> ()
           in
           try
             Lst.iter ps ~f:(fun p -> check_no_self Multiple_self p.tmparam_typ);
             (match return_type with
             | None -> ()
             | Some (return_type, err_type) -> (
                 check_no_self Self_in_return return_type;
                 match err_type with
                 | No_error_typ | Default_error_typ _ -> ()
                 | Error_typ { ty = ty_err } ->
                     check_no_self Self_in_return ty_err));
             acc
           with Invalid_method reason ->
             Bad_method (name.binder_name, reason) :: acc)
       | _ -> Bad_method (name.binder_name, First_param_not_self) :: acc)

let check_object_safety ~name ~loc status =
  match status with
  | [] -> None
  | reasons -> Some (type_not_object_safe ~name ~reasons ~loc)

let find_method (decl : t) method_name ~loc =
  (match
     Lst.find_first decl.methods (fun meth_decl ->
         meth_decl.method_name = method_name)
   with
   | Some decl -> Ok decl
   | None ->
       Error
         (Errors.method_not_found_in_trait ~trait:decl.name ~method_name ~loc)
    : method_decl Local_diagnostics.info)
