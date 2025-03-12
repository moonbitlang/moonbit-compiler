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
module Type_path = Basic_type_path
module Ident = Basic_ident
module Constr_info = Basic_constr_info
module Map_string = Basic_map_string

type var_kind = Top_func | Top_let | Local_func | Local

include struct
  let _ = fun (_ : var_kind) -> ()

  let sexp_of_var_kind =
    (function
     | Top_func -> S.Atom "Top_func"
     | Top_let -> S.Atom "Top_let"
     | Local_func -> S.Atom "Local_func"
     | Local -> S.Atom "Local"
      : var_kind -> S.t)

  let _ = sexp_of_var_kind
end

type constr_arg_kind = Positional of int | Labelled of string

include struct
  let _ = fun (_ : constr_arg_kind) -> ()

  let sexp_of_constr_arg_kind =
    (function
     | Positional arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_int arg0__001_ in
         S.List [ S.Atom "Positional"; res0__002_ ]
     | Labelled arg0__003_ ->
         let res0__004_ = Moon_sexp_conv.sexp_of_string arg0__003_ in
         S.List [ S.Atom "Labelled"; res0__004_ ]
      : constr_arg_kind -> S.t)

  let _ = sexp_of_constr_arg_kind

  let equal_constr_arg_kind =
    (fun a__005_ ->
       fun b__006_ ->
        if Stdlib.( == ) a__005_ b__006_ then true
        else
          match (a__005_, b__006_) with
          | Positional _a__007_, Positional _b__008_ ->
              Stdlib.( = ) (_a__007_ : int) _b__008_
          | Positional _, _ -> false
          | _, Positional _ -> false
          | Labelled _a__009_, Labelled _b__010_ ->
              Stdlib.( = ) (_a__009_ : string) _b__010_
      : constr_arg_kind -> constr_arg_kind -> bool)

  let _ = equal_constr_arg_kind

  let (hash_fold_constr_arg_kind :
        Ppx_base.state -> constr_arg_kind -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Positional _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            Ppx_base.hash_fold_int hsv _a0
        | Labelled _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            Ppx_base.hash_fold_string hsv _a0
      : Ppx_base.state -> constr_arg_kind -> Ppx_base.state)

  let _ = hash_fold_constr_arg_kind

  let (hash_constr_arg_kind : constr_arg_kind -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_constr_arg_kind hsv arg)
    in
    fun x -> func x

  let _ = hash_constr_arg_kind
end

type entity =
  | Struct_field of { ty : Type_path.t; name : string }
  | Construct_struct of { ty : Type_path.t }
  | Enum_constr of { ty : Type_path.t; name : string }
  | Var of { id : Ident.t; kind : var_kind [@ceh.ignore] }
  | Direct_pkg_use of string
  | Trait_method of { trait : Type_path.t; method_name : string }
  | Constr_argument of {
      ty : Type_path.t;
      tag : Constr_info.constr_tag;
      kind : constr_arg_kind;
    }
  | Fn_optional_arg of { fn : Ident.t; label : string }

include struct
  let _ = fun (_ : entity) -> ()

  let sexp_of_entity =
    (function
     | Struct_field { ty = ty__012_; name = name__014_ } ->
         let bnds__011_ = ([] : _ Stdlib.List.t) in
         let bnds__011_ =
           let arg__015_ = Moon_sexp_conv.sexp_of_string name__014_ in
           (S.List [ S.Atom "name"; arg__015_ ] :: bnds__011_ : _ Stdlib.List.t)
         in
         let bnds__011_ =
           let arg__013_ = Type_path.sexp_of_t ty__012_ in
           (S.List [ S.Atom "ty"; arg__013_ ] :: bnds__011_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Struct_field" :: bnds__011_)
     | Construct_struct { ty = ty__017_ } ->
         let bnds__016_ = ([] : _ Stdlib.List.t) in
         let bnds__016_ =
           let arg__018_ = Type_path.sexp_of_t ty__017_ in
           (S.List [ S.Atom "ty"; arg__018_ ] :: bnds__016_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Construct_struct" :: bnds__016_)
     | Enum_constr { ty = ty__020_; name = name__022_ } ->
         let bnds__019_ = ([] : _ Stdlib.List.t) in
         let bnds__019_ =
           let arg__023_ = Moon_sexp_conv.sexp_of_string name__022_ in
           (S.List [ S.Atom "name"; arg__023_ ] :: bnds__019_ : _ Stdlib.List.t)
         in
         let bnds__019_ =
           let arg__021_ = Type_path.sexp_of_t ty__020_ in
           (S.List [ S.Atom "ty"; arg__021_ ] :: bnds__019_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Enum_constr" :: bnds__019_)
     | Var { id = id__025_; kind = kind__027_ } ->
         let bnds__024_ = ([] : _ Stdlib.List.t) in
         let bnds__024_ =
           let arg__028_ = sexp_of_var_kind kind__027_ in
           (S.List [ S.Atom "kind"; arg__028_ ] :: bnds__024_ : _ Stdlib.List.t)
         in
         let bnds__024_ =
           let arg__026_ = Ident.sexp_of_t id__025_ in
           (S.List [ S.Atom "id"; arg__026_ ] :: bnds__024_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Var" :: bnds__024_)
     | Direct_pkg_use arg0__029_ ->
         let res0__030_ = Moon_sexp_conv.sexp_of_string arg0__029_ in
         S.List [ S.Atom "Direct_pkg_use"; res0__030_ ]
     | Trait_method { trait = trait__032_; method_name = method_name__034_ } ->
         let bnds__031_ = ([] : _ Stdlib.List.t) in
         let bnds__031_ =
           let arg__035_ = Moon_sexp_conv.sexp_of_string method_name__034_ in
           (S.List [ S.Atom "method_name"; arg__035_ ] :: bnds__031_
             : _ Stdlib.List.t)
         in
         let bnds__031_ =
           let arg__033_ = Type_path.sexp_of_t trait__032_ in
           (S.List [ S.Atom "trait"; arg__033_ ] :: bnds__031_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Trait_method" :: bnds__031_)
     | Constr_argument { ty = ty__037_; tag = tag__039_; kind = kind__041_ } ->
         let bnds__036_ = ([] : _ Stdlib.List.t) in
         let bnds__036_ =
           let arg__042_ = sexp_of_constr_arg_kind kind__041_ in
           (S.List [ S.Atom "kind"; arg__042_ ] :: bnds__036_ : _ Stdlib.List.t)
         in
         let bnds__036_ =
           let arg__040_ = Constr_info.sexp_of_constr_tag tag__039_ in
           (S.List [ S.Atom "tag"; arg__040_ ] :: bnds__036_ : _ Stdlib.List.t)
         in
         let bnds__036_ =
           let arg__038_ = Type_path.sexp_of_t ty__037_ in
           (S.List [ S.Atom "ty"; arg__038_ ] :: bnds__036_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Constr_argument" :: bnds__036_)
     | Fn_optional_arg { fn = fn__044_; label = label__046_ } ->
         let bnds__043_ = ([] : _ Stdlib.List.t) in
         let bnds__043_ =
           let arg__047_ = Moon_sexp_conv.sexp_of_string label__046_ in
           (S.List [ S.Atom "label"; arg__047_ ] :: bnds__043_
             : _ Stdlib.List.t)
         in
         let bnds__043_ =
           let arg__045_ = Ident.sexp_of_t fn__044_ in
           (S.List [ S.Atom "fn"; arg__045_ ] :: bnds__043_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Fn_optional_arg" :: bnds__043_)
      : entity -> S.t)

  let _ = sexp_of_entity

  let equal_entity =
    (fun a__048_ ->
       fun b__049_ ->
        if Stdlib.( == ) a__048_ b__049_ then true
        else
          match (a__048_, b__049_) with
          | Struct_field _a__050_, Struct_field _b__051_ ->
              Stdlib.( && )
                (Type_path.equal _a__050_.ty _b__051_.ty)
                (Stdlib.( = ) (_a__050_.name : string) _b__051_.name)
          | Struct_field _, _ -> false
          | _, Struct_field _ -> false
          | Construct_struct _a__052_, Construct_struct _b__053_ ->
              Type_path.equal _a__052_.ty _b__053_.ty
          | Construct_struct _, _ -> false
          | _, Construct_struct _ -> false
          | Enum_constr _a__054_, Enum_constr _b__055_ ->
              Stdlib.( && )
                (Type_path.equal _a__054_.ty _b__055_.ty)
                (Stdlib.( = ) (_a__054_.name : string) _b__055_.name)
          | Enum_constr _, _ -> false
          | _, Enum_constr _ -> false
          | Var _a__056_, Var _b__057_ -> Ident.equal _a__056_.id _b__057_.id
          | Var _, _ -> false
          | _, Var _ -> false
          | Direct_pkg_use _a__058_, Direct_pkg_use _b__059_ ->
              Stdlib.( = ) (_a__058_ : string) _b__059_
          | Direct_pkg_use _, _ -> false
          | _, Direct_pkg_use _ -> false
          | Trait_method _a__060_, Trait_method _b__061_ ->
              Stdlib.( && )
                (Type_path.equal _a__060_.trait _b__061_.trait)
                (Stdlib.( = )
                   (_a__060_.method_name : string)
                   _b__061_.method_name)
          | Trait_method _, _ -> false
          | _, Trait_method _ -> false
          | Constr_argument _a__062_, Constr_argument _b__063_ ->
              Stdlib.( && )
                (Type_path.equal _a__062_.ty _b__063_.ty)
                (Stdlib.( && )
                   (Constr_info.equal_constr_tag _a__062_.tag _b__063_.tag)
                   (equal_constr_arg_kind _a__062_.kind _b__063_.kind))
          | Constr_argument _, _ -> false
          | _, Constr_argument _ -> false
          | Fn_optional_arg _a__064_, Fn_optional_arg _b__065_ ->
              Stdlib.( && )
                (Ident.equal _a__064_.fn _b__065_.fn)
                (Stdlib.( = ) (_a__064_.label : string) _b__065_.label)
      : entity -> entity -> bool)

  let _ = equal_entity

  let (hash_fold_entity : Ppx_base.state -> entity -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Struct_field _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv =
              let hsv = hsv in
              Type_path.hash_fold_t hsv _ir.ty
            in
            Ppx_base.hash_fold_string hsv _ir.name
        | Construct_struct _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            Type_path.hash_fold_t hsv _ir.ty
        | Enum_constr _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv =
              let hsv = hsv in
              Type_path.hash_fold_t hsv _ir.ty
            in
            Ppx_base.hash_fold_string hsv _ir.name
        | Var _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 3 in
            let hsv =
              let hsv = hsv in
              Ident.hash_fold_t hsv _ir.id
            in
            hsv
        | Direct_pkg_use _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 4 in
            let hsv = hsv in
            Ppx_base.hash_fold_string hsv _a0
        | Trait_method _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 5 in
            let hsv =
              let hsv = hsv in
              Type_path.hash_fold_t hsv _ir.trait
            in
            Ppx_base.hash_fold_string hsv _ir.method_name
        | Constr_argument _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 6 in
            let hsv =
              let hsv =
                let hsv = hsv in
                Type_path.hash_fold_t hsv _ir.ty
              in
              Constr_info.hash_fold_constr_tag hsv _ir.tag
            in
            hash_fold_constr_arg_kind hsv _ir.kind
        | Fn_optional_arg _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 7 in
            let hsv =
              let hsv = hsv in
              Ident.hash_fold_t hsv _ir.fn
            in
            Ppx_base.hash_fold_string hsv _ir.label
      : Ppx_base.state -> entity -> Ppx_base.state)

  let _ = hash_fold_entity

  let (hash_entity : entity -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_entity hsv arg)
    in
    fun x -> func x

  let _ = hash_entity
end

module H = Basic_hashf.Make (struct
  type t = entity

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_entity : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_entity : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_entity

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_entity in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

type entity_usage = Unused | Only_read | Only_write | Read_write

let join_usage u1 u2 =
  match (u1, u2) with
  | Unused, u | u, Unused -> u
  | Read_write, _ | _, Read_write -> Read_write
  | Only_read, Only_write | Only_write, Only_read -> Read_write
  | Only_read, Only_read | Only_write, Only_write -> u1

let optional_arg_use_default : entity_usage = Only_read
let optional_arg_supplied : entity_usage = Only_write

type type_usage = Unused | Used_in_priv | Used_in_pub

let join_type_usage u1 u2 =
  match (u1, u2) with
  | Unused, u | u, Unused -> u
  | Used_in_pub, _ | _, Used_in_pub -> Used_in_pub
  | Used_in_priv, Used_in_priv -> Used_in_priv

type entity_info = { is_mut : bool; mutable usage : entity_usage; loc : Loc.t }

type type_info = {
  decl :
    [ `Type of Typedtree.type_decl
    | `Trait of Typedtree.trait_decl
    | `Trait_alias of Typedtree.trait_alias_decl ];
  mutable usage : type_usage;
  loc : Loc.t;
}

type ctx = {
  entities : entity_info H.t;
  types : type_info Type_path.Hash.t;
  excluding : Ident.Set.t;
}

let register_entity ctx entity ~is_mut ~loc =
  H.add ctx.entities entity { is_mut; loc; usage = Unused }

let name_should_be_ignored name =
  String.length name = 0 || match name.[0] with '_' | '*' -> true | _ -> false

let register_binder ctx (binder : Typedtree.binder) ~base_loc ~is_mut ~kind =
  if not (name_should_be_ignored (Ident.base_name binder.binder_id)) then
    let loc = Rloc.to_loc ~base:base_loc binder.loc_ in
    H.add ctx.entities
      (Var { id = binder.binder_id; kind })
      { is_mut; loc; usage = Unused }

let register_pat_binders ctx (pat_binders : Typedtree.pat_binders) ~base_loc =
  Lst.iter pat_binders ~f:(fun pb ->
      register_binder ctx pb.binder ~base_loc ~is_mut:false ~kind:Local)

let update_usage ctx entity usage =
  match H.find_opt ctx.entities entity with
  | None -> ()
  | Some info -> info.usage <- join_usage info.usage usage

let update_var_usage ctx (var : Ident.t) usage =
  if not (Ident.Set.mem ctx.excluding var) then
    let entity =
      match var with
      | Plocal_method { trait; method_name; _ }
      | Pdot (Qext_method { trait; name = method_name; self_typ = _ })
      | Pdyntrait_method { trait; method_name; _ } ->
          Trait_method { trait; method_name }
      | Pident _ | Pdot _ -> Var { id = var; kind = Local }
    in
    match H.find_opt ctx.entities entity with
    | None -> ()
    | Some info -> info.usage <- join_usage info.usage usage

let update_direct_use_usage ctx (name : string) =
  let entity = Direct_pkg_use name in
  match H.find_opt ctx.entities entity with
  | None -> ()
  | Some info -> info.usage <- join_usage info.usage Only_read

let register_type ctx type_name decl ~loc =
  Type_path.Hash.add ctx.types type_name { decl; loc; usage = Unused }

let update_type_usage ctx type_name usage =
  match Type_path.Hash.find_opt ctx.types type_name with
  | None -> ()
  | Some info -> info.usage <- join_type_usage info.usage usage

let process_alias ctx (alias : Basic_longident.t) =
  match alias with
  | Lident name ->
      let alias_path =
        Type_path.toplevel_type ~pkg:!Basic_config.current_package name
      in
      update_type_usage ctx alias_path Used_in_pub
  | Ldot _ -> ()

let rec process_typ ~in_pub_sig ctx (typ : Typedtree.typ) =
  let go ty = process_typ ~in_pub_sig ctx ty [@@inline] in
  match typ with
  | Tany _ -> ()
  | Tarrow { params; return; err_ty; ty = _ } -> (
      Lst.iter params ~f:go;
      go return;
      match err_ty with
      | Error_typ { ty } -> go ty
      | Default_error_typ _ | No_error_typ -> ())
  | T_tuple { params; ty = _ } -> Lst.iter params ~f:go
  | Tname { constr; params; ty; is_alias_ } -> (
      Lst.iter params ~f:go;
      if is_alias_ then process_alias ctx constr.lid
      else
        match Stype.type_repr ty with
        | T_constr { type_constructor = ty; tys = _ } | T_trait ty ->
            let usage = if in_pub_sig then Used_in_pub else Used_in_priv in
            update_type_usage ctx ty usage
        | T_builtin _ | Tarrow _ | Tparam _ | Tvar _ | T_blackhole -> ())
  | Tobject { constr; ty; is_alias_ } -> (
      if is_alias_ then process_alias ctx constr.lid
      else
        match Stype.type_repr ty with
        | T_trait ty ->
            let usage = if in_pub_sig then Used_in_pub else Used_in_priv in
            update_type_usage ctx ty usage
        | _ -> ())

let process_typ_opt ctx ~in_pub_sig konstraint =
  match konstraint with
  | None -> ()
  | Some typ -> process_typ ctx ~in_pub_sig typ

let process_type_name ctx ~in_pub_sig (type_name : Typedtree.type_name) =
  match type_name with
  | Tname_tvar _ -> ()
  | Tname_path { name; kind = _ } ->
      let usage = if in_pub_sig then Used_in_pub else Used_in_priv in
      update_type_usage ctx name usage
  | Tname_alias { alias_; name = _; kind = _ } -> process_alias ctx alias_

let rec process_pat ctx (pat : Typedtree.pat) =
  let go pat = process_pat ctx pat [@@inline] in
  match pat with
  | Tpat_alias { pat; alias = _; ty = _ } -> go pat
  | Tpat_var _ | Tpat_any _ -> ()
  | Tpat_array { pats; ty = _ } -> (
      match pats with
      | Closed pats -> Lst.iter pats ~f:go
      | Open (pats_l, pats_r, _) ->
          Lst.iter pats_l ~f:go;
          Lst.iter pats_r ~f:go)
  | Tpat_constant { name_ = None; _ } -> ()
  | Tpat_constant { name_ = Some { var_id }; _ } ->
      update_var_usage ctx var_id Only_read
  | Tpat_range { lhs; rhs; inclusive = _; ty = _ } ->
      go lhs;
      go rhs
  | Tpat_constr { type_name; constr; args; ty; tag; used_error_subtyping = _ }
    -> (
      (match type_name with
      | Some type_name -> process_type_name ~in_pub_sig:false ctx type_name
      | _ -> ());
      match Stype.type_repr ty with
      | T_constr { type_constructor = ty; _ } ->
          update_type_usage ctx ty Used_in_priv;
          update_usage ctx (Enum_constr { ty; name = constr.name }) Only_read;
          let last_positional_index = ref (-1) in
          Lst.iter args ~f:(fun (Constr_pat_arg { pat; kind; pos = _ }) ->
              go pat;
              let kind : constr_arg_kind =
                match kind with
                | Positional ->
                    incr last_positional_index;
                    Positional !last_positional_index
                | Labelled label
                | Labelled_pun label
                | Labelled_option { label; question_loc = _ }
                | Labelled_option_pun { label; question_loc = _ } ->
                    Labelled label.label_name
              in
              match pat with
              | Tpat_any _ -> ()
              | _ ->
                  update_usage ctx (Constr_argument { ty; tag; kind }) Only_read)
      | _ ->
          Lst.iter args ~f:(fun (Constr_pat_arg { pat; kind = _; pos = _ }) ->
              go pat))
  | Tpat_or { pat1; pat2; ty = _ } ->
      go pat1;
      go pat2
  | Tpat_tuple { pats; ty = _ } -> Lst.iter pats ~f:go
  | Tpat_record { fields; is_closed = _; ty } ->
      let ty = Stype.type_repr ty in
      Lst.iter fields ~f:(fun (Field_pat { label; pat; is_pun = _; pos = _ }) ->
          go pat;
          match ty with
          | T_constr { type_constructor = ty; _ } ->
              update_type_usage ctx ty Used_in_priv;
              update_usage ctx
                (Struct_field { ty; name = label.label_name })
                Only_read
          | _ -> ())
  | Tpat_constraint { pat; konstraint; ty = _ } ->
      process_typ ctx ~in_pub_sig:false konstraint;
      go pat
  | Tpat_map { elems; op_get_info_ = op_get_id, _, _; ty = _ } ->
      update_var_usage ctx op_get_id Only_read;
      Lst.iter elems ~f:(fun (_, pat) -> go pat)

let rec process_expr ~base_loc ctx (expr : Typedtree.expr) =
  let go expr = process_expr ~base_loc ctx expr [@@inline] in
  match expr with
  | Texpr_apply { func; args; ty = _ } -> (
      go func;
      Lst.iter args ~f:(fun arg -> go arg.arg_value);
      match func with
      | Texpr_method { meth = { var_id = fn }; arity_ = Some arity; _ }
      | Texpr_ident { id = { var_id = fn }; arity_ = Some arity; _ }
        when not (Fn_arity.is_simple arity) ->
          Fn_arity.iter arity (fun param_kind ->
              match param_kind with
              | Positional _ | Labelled _ -> ()
              | Optional { label; _ }
              | Autofill { label }
              | Question_optional { label } -> (
                  let entity = Fn_optional_arg { fn; label } in
                  match
                    Lst.find_first args (fun arg ->
                        match arg.arg_kind with
                        | Labelled { label_name }
                        | Labelled_pun { label_name }
                        | Labelled_option
                            { label = { label_name }; question_loc = _ }
                        | Labelled_option_pun
                            { label = { label_name }; question_loc = _ } ->
                            label_name = label
                        | Positional -> false)
                  with
                  | Some
                      {
                        arg_kind = Labelled_option _ | Labelled_option_pun _;
                        _;
                      } ->
                      update_usage ctx entity Read_write
                  | Some _ -> update_usage ctx entity optional_arg_supplied
                  | None -> update_usage ctx entity optional_arg_use_default))
      | _ -> ())
  | Texpr_method { type_name; meth; prim = _; ty = _ } ->
      process_type_name ctx ~in_pub_sig:false type_name;
      update_var_usage ctx meth.var_id Only_read
  | Texpr_unresolved_method { trait_name; method_name; self_type = _; ty = _ }
    -> (
      process_type_name ~in_pub_sig:false ctx trait_name;
      match trait_name with
      | Tname_path { name; _ } | Tname_alias { name; _ } ->
          update_usage ctx
            (Trait_method { trait = name; method_name })
            Only_read
      | Tname_tvar _ -> ())
  | Texpr_ident { id; kind = _; ty = _ } -> (
      update_var_usage ctx id.var_id Only_read;
      match id.var_id with
      | Pdot (Qregular_implicit_pkg { pkg = _; name }) ->
          update_direct_use_usage ctx name
      | _ -> ())
  | Texpr_as { expr; trait; ty = _; is_implicit = _ } ->
      process_type_name ctx ~in_pub_sig:false trait;
      go expr
  | Texpr_array { exprs; ty = _; is_fixed_array = _ } -> Lst.iter exprs ~f:go
  | Texpr_constant { name_ = None; _ } -> ()
  | Texpr_constant { name_ = Some var; _ } ->
      update_var_usage ctx var.var_id Only_read
  | Texpr_constr { type_name; constr; tag = _; ty } -> (
      (match type_name with
      | Some type_name -> process_type_name ~in_pub_sig:false ctx type_name
      | _ -> ());
      let constr_opt =
        match Stype.type_repr ty with
        | T_constr { type_constructor; _ } -> Some type_constructor
        | Tarrow { ret_ty; _ } -> (
            match Stype.type_repr ret_ty with
            | T_constr { type_constructor; _ } -> Some type_constructor
            | _ -> None)
        | _ -> None
      in
      match constr_opt with
      | None -> ()
      | Some ty ->
          update_type_usage ctx ty Used_in_priv;
          update_usage ctx (Enum_constr { ty; name = constr.name }) Only_write)
  | Texpr_while { loop_cond; loop_body; while_else; ty = _; label = _ } -> (
      go loop_cond;
      go loop_body;
      match while_else with Some while_else -> go while_else | _ -> ())
  | Texpr_function { func; ty = _ } ->
      process_fn ctx ~base_loc ~in_pub_sig:false func
  | Texpr_if { cond; ifso; ifnot; ty = _ } -> (
      go cond;
      go ifso;
      match ifnot with Some ifnot -> go ifnot | _ -> ())
  | Texpr_letfn { binder; fn; body; ty = _; is_rec = _ } ->
      process_fn ctx ~base_loc ~in_pub_sig:false fn;
      register_binder ctx binder ~base_loc ~is_mut:false ~kind:Local_func;
      go body
  | Texpr_letrec { bindings; body; ty = _ } ->
      Lst.iter bindings ~f:(fun (binder, _) ->
          register_binder ctx binder ~base_loc ~is_mut:false ~kind:Local_func);
      go body;
      let filter_used fns =
        List.partition
          (fun ((binder : Typedtree.binder), _) ->
            match
              H.find_opt ctx.entities
                (Var { id = binder.binder_id; kind = Local })
            with
            | None -> true
            | Some { usage = Unused; _ } -> false
            | Some { usage = Only_read | Only_write | Read_write; _ } -> true)
          fns
      in
      let rec process_used ~used ~unused =
        match used with
        | [] ->
            let excluding =
              Lst.fold_left unused ctx.excluding (fun excluding ->
                  fun ((binder : Typedtree.binder), _) ->
                   Ident.Set.add excluding binder.binder_id)
            in
            let ctx = { ctx with excluding } in
            Lst.iter unused ~f:(fun (_, fn) ->
                process_fn ctx ~base_loc ~in_pub_sig:false fn)
        | used ->
            Lst.iter used ~f:(fun (_, fn) ->
                process_fn ctx ~base_loc ~in_pub_sig:false fn);
            let used, unused = filter_used unused in
            process_used ~used ~unused
      in
      let used, unused = filter_used bindings in
      process_used ~used ~unused
  | Texpr_let { pat; rhs; pat_binders; body; ty = _ } ->
      process_pat ctx pat;
      go rhs;
      register_pat_binders ctx pat_binders ~base_loc;
      go body
  | Texpr_sequence { exprs; last_expr; ty = _ } ->
      Lst.iter exprs ~f:go;
      go last_expr
  | Texpr_tuple { exprs; ty = _ } -> Lst.iter exprs ~f:go
  | Texpr_record { type_name; fields; ty } ->
      (match type_name with
      | None -> ()
      | Some type_name -> process_type_name ctx ~in_pub_sig:false type_name);
      (match Stype.type_repr ty with
      | T_constr { type_constructor = ty; _ } ->
          update_type_usage ctx ty Used_in_priv;
          update_usage ctx (Construct_struct { ty }) Only_write
      | _ -> ());
      Lst.iter fields ~f:(fun (Field_def { expr; _ }) -> go expr)
  | Texpr_record_update { type_name; record; fields; all_fields = _; ty = _ } ->
      (match type_name with
      | None -> ()
      | Some type_name -> process_type_name ctx ~in_pub_sig:false type_name);
      go record;
      Lst.iter fields ~f:(fun (Field_def { expr; _ }) -> go expr)
  | Texpr_field { record; accessor = Index _; pos = _; ty = _ } -> go record
  | Texpr_field { record; accessor = Newtype; pos = _; ty = _ } -> (
      go record;
      match Stype.type_repr (Typedtree_util.type_of_typed_expr record) with
      | T_constr { type_constructor = Toplevel { pkg = _; id } as ty; _ } ->
          update_usage ctx (Enum_constr { ty; name = id }) Only_read
      | T_constr
          { type_constructor = T_local { toplevel_id = _; name } as ty; _ } ->
          update_usage ctx (Enum_constr { ty; name }) Only_read
      | _ -> ())
  | Texpr_field { record; accessor = Label label; pos = _; ty = _ } -> (
      go record;
      match Stype.type_repr (Typedtree_util.type_of_typed_expr record) with
      | T_constr { type_constructor = ty; _ } ->
          let entity =
            match record with
            | Texpr_ident { kind = Value_constr tag; _ } ->
                Constr_argument { ty; tag; kind = Labelled label.label_name }
            | _ -> Struct_field { ty; name = label.label_name }
          in
          update_usage ctx entity Only_read
      | _ -> ())
  | Texpr_mutate { record; label; field; augmented_by; pos = _; ty = _ } -> (
      go record;
      go field;
      (match augmented_by with Some augmented_by -> go augmented_by | _ -> ());
      match Stype.type_repr (Typedtree_util.type_of_typed_expr record) with
      | T_constr { type_constructor = ty; _ } ->
          let entity =
            match record with
            | Texpr_ident { kind = Value_constr tag; _ } ->
                Constr_argument { ty; tag; kind = Labelled label.label_name }
            | _ -> Struct_field { ty; name = label.label_name }
          in
          update_usage ctx entity Only_write
      | _ -> ())
  | Texpr_match { expr; cases; ty = _ } ->
      go expr;
      process_match_cases ctx ~base_loc cases
  | Texpr_is { expr; pat; pat_binders } ->
      go expr;
      process_pat ctx pat;
      register_pat_binders ctx pat_binders ~base_loc
  | Texpr_letmut { binder; konstraint; expr; body; ty = _ } ->
      go expr;
      process_typ_opt ctx ~in_pub_sig:false konstraint;
      register_binder ctx binder ~base_loc ~is_mut:true ~kind:Local;
      go body
  | Texpr_assign { var; expr; augmented_by; ty = _ } ->
      update_var_usage ctx var.var_id Only_write;
      (match augmented_by with Some augmented_by -> go augmented_by | _ -> ());
      go expr
  | Texpr_hole _ | Texpr_unit _ -> ()
  | Texpr_break { arg; ty = _; label = _ } -> (
      match arg with Some arg -> go arg | _ -> ())
  | Texpr_continue { args; ty = _; label = _ } -> Lst.iter args ~f:go
  | Texpr_loop { params; body; args; ty = _; label = _ } ->
      Lst.iter args ~f:go;
      process_params ctx ~base_loc ~in_pub_sig:false params;
      go body
  | Texpr_for { binders; condition; steps; body; for_else; ty = _; label = _ }
    -> (
      Lst.iter binders ~f:(fun (binder, init) ->
          register_binder ctx binder ~base_loc ~is_mut:false ~kind:Local;
          go init);
      (match condition with Some condition -> go condition | _ -> ());
      Lst.iter steps ~f:(fun (_, update) -> go update);
      go body;
      match for_else with Some for_else -> go for_else | _ -> ())
  | Texpr_foreach
      { binders; expr; body; else_block; elem_tys = _; ty = _; label = _ } -> (
      go expr;
      Lst.iter binders ~f:(fun binder_opt ->
          match binder_opt with
          | None -> ()
          | Some binder ->
              register_binder ctx binder ~base_loc ~is_mut:false ~kind:Local);
      go body;
      match else_block with Some else_block -> go else_block | _ -> ())
  | Texpr_return { return_value; ty = _ } -> (
      match return_value with Some return_value -> go return_value | _ -> ())
  | Texpr_raise { error_value; ty = _ } -> go error_value
  | Texpr_try { body; catch; try_else; catch_all = _; ty = _; err_ty = _ } -> (
      go body;
      process_match_cases ctx ~base_loc catch;
      match try_else with
      | None -> ()
      | Some try_else -> process_match_cases ctx ~base_loc try_else)
  | Texpr_exclamation { expr; ty = _; convert_to_result = _ } -> go expr
  | Texpr_constraint { expr; konstraint; ty = _ } ->
      process_typ ctx ~in_pub_sig:false konstraint;
      go expr
  | Texpr_pipe { lhs; rhs; ty = _ } -> (
      go lhs;
      match rhs with
      | Pipe_partial_apply { func; args } ->
          go func;
          Lst.iter args ~f:(fun arg -> go arg.arg_value)
      | Pipe_invalid { expr; ty = _ } -> go expr)
  | Texpr_interp { elems; ty = _ } ->
      Lst.iter elems ~f:(fun elem ->
          match elem with
          | Interp_lit _ -> ()
          | Interp_expr { expr; to_string } ->
              go expr;
              go to_string)
  | Texpr_guard { cond; otherwise; body; ty = _ } ->
      go cond;
      (match otherwise with Some otherwise -> go otherwise | _ -> ());
      go body
  | Texpr_guard_let { pat; rhs; pat_binders; otherwise; body; ty = _ } ->
      process_pat ctx pat;
      go rhs;
      (match otherwise with
      | None -> ()
      | Some otherwise -> process_match_cases ctx ~base_loc otherwise);
      register_pat_binders ctx pat_binders ~base_loc;
      go body
  | Texpr_array_as_view { array; _ } -> go array
  | Texpr_and { lhs; rhs; _ } | Texpr_or { lhs; rhs; _ } ->
      go lhs;
      go rhs

and process_fn ~base_loc ~in_pub_sig ctx (func : Typedtree.fn) =
  process_params ctx ~base_loc ~in_pub_sig func.params;
  (match func.ret_constraint with
  | None -> ()
  | Some (ret_ty, err_ty) -> (
      process_typ ctx ~in_pub_sig ret_ty;
      match err_ty with
      | Default_error_typ _ | No_error_typ -> ()
      | Error_typ { ty } -> process_typ ctx ~in_pub_sig ty));
  process_expr ctx ~base_loc func.body

and process_params ctx ~base_loc ~in_pub_sig (params : Typedtree.params) =
  Lst.iter params ~f:(fun p ->
      match p with
      | Discard_positional_param { konstraint; _ } ->
          process_typ_opt ctx ~in_pub_sig konstraint
      | Param { binder; konstraint; kind; ty = _ } -> (
          register_binder ctx binder ~base_loc ~is_mut:false ~kind:Local;
          process_typ_opt ctx ~in_pub_sig konstraint;
          match kind with
          | Positional | Labelled | Autofill | Question_optional -> ()
          | Optional default -> process_expr ctx ~base_loc default))

and process_match_cases ctx ~base_loc (cases : Typedtree.match_case list) =
  Lst.iter cases ~f:(fun { pat; action; pat_binders; guard } ->
      process_pat ctx pat;
      register_pat_binders ctx pat_binders ~base_loc;
      (match guard with
      | None -> ()
      | Some guard -> process_expr ctx ~base_loc guard);
      process_expr ctx ~base_loc action)

let register_trait_decl ctx (trait : Typedtree.trait_decl) =
  match trait.trait_vis with
  | Vis_priv | Vis_default ->
      let base = trait.trait_loc_ in
      let loc = Rloc.to_loc ~base trait.trait_name.loc_ in
      register_type ctx trait.trait_name.name (`Trait trait) ~loc;
      Lst.iter trait.trait_methods ~f:(fun meth ->
          let loc = Rloc.to_loc ~base meth.method_name.loc_ in
          let trait = trait.trait_name.name in
          let method_name = meth.method_name.binder_name in
          if not (name_should_be_ignored method_name) then
            register_entity ctx
              (Trait_method { trait; method_name })
              ~is_mut:false ~loc)
  | Vis_readonly | Vis_fully_pub -> ()

let register_trait_alias ctx (decl : Typedtree.trait_alias_decl) =
  if not decl.trait_alias_is_pub then
    let base = decl.trait_alias_loc_ in
    let loc = Rloc.to_loc ~base decl.trait_alias_name.loc_ in
    register_type ctx decl.trait_alias_name.name (`Trait_alias decl) ~loc

let register_type_decl ctx (decl : Typedtree.type_decl) =
  let base = decl.td_loc_ in
  let is_pub =
    match decl.td_vis with
    | Vis_readonly | Vis_fully_pub -> true
    | Vis_default | Vis_priv ->
        let loc = Rloc.to_loc ~base decl.td_binder.loc_ in
        register_type ctx decl.td_binder.name (`Type decl) ~loc;
        false
  in
  let is_error = match decl.td_desc with Td_error _ -> true | _ -> false in
  match decl.td_desc with
  | Td_abstract | Td_error No_payload -> ()
  | Td_alias _ -> ()
  | Td_newtype _ | Td_error (Single_payload _) -> (
      match decl.td_vis with
      | Vis_fully_pub -> ()
      | Vis_readonly | Vis_priv | Vis_default ->
          let ty = decl.td_binder.name in
          let id =
            match ty with
            | Toplevel { pkg = _; id } -> id
            | T_local { toplevel_id = _; name } -> name
            | _ -> assert false
          in
          let entity = Enum_constr { ty; name = id } in
          register_entity ctx entity ~is_mut:true
            ~loc:(Rloc.to_loc ~base decl.td_binder.loc_);
          if is_pub || is_error then update_usage ctx entity Only_read)
  | Td_variant constrs | Td_error (Enum_payload constrs) ->
      let ty = decl.td_binder.name in
      let can_construct_outside, can_read_outside =
        match decl.td_vis with
        | Vis_fully_pub -> (true, true)
        | Vis_readonly -> (false, true)
        | Vis_priv | Vis_default -> (false, false)
      in
      if not can_construct_outside then
        Lst.iter constrs ~f:(fun constr ->
            let entity =
              Enum_constr { ty; name = constr.constr_name.label_name }
            in
            register_entity ctx entity ~is_mut:true
              ~loc:(Rloc.to_loc ~base constr.constr_name.loc_);
            if can_read_outside || is_error then
              update_usage ctx entity Only_read;
            Fn_arity.iter2 constr.constr_arity_ constr.constr_args (fun kind ->
                fun arg ->
                 let register_arg kind =
                   let entity =
                     Constr_argument { ty; tag = constr.constr_tag; kind }
                   in
                   register_entity ctx entity ~is_mut:arg.carg_mut
                     ~loc:
                       (Rloc.to_loc ~base (Typedtree.loc_of_typ arg.carg_typ));
                   if can_read_outside || is_error then
                     update_usage ctx entity Only_read
                     [@@local]
                 in
                 match kind with
                 | Positional index -> register_arg (Positional index)
                 | Labelled { label; is_mut = _ } ->
                     if not (name_should_be_ignored label) then
                       register_arg (Labelled label)
                 | Optional _ | Autofill _ | Question_optional _ -> assert false))
  | Td_record fields ->
      let ty = decl.td_binder.name in
      (match decl.td_vis with
      | Vis_fully_pub
        when Lst.for_all fields (fun field -> field.field_vis <> Invisible) ->
          ()
      | _ ->
          register_entity ctx
            (Construct_struct { ty })
            ~is_mut:false
            ~loc:(Rloc.to_loc ~base decl.td_binder.loc_));
      Lst.iter fields ~f:(fun field ->
          let register_entity () =
            let entity =
              Struct_field { ty; name = field.field_label.label_name }
            in
            register_entity ctx entity ~is_mut:field.field_mut
              ~loc:(Rloc.to_loc ~base field.field_label.loc_);
            entity
          in
          match field.field_vis with
          | Read_write -> ()
          | _ when name_should_be_ignored field.field_label.label_name -> ()
          | Readable ->
              let entity = register_entity () in
              update_usage ctx entity Only_read
          | Invisible -> ignore (register_entity ()))

let register_impl ctx (impl : Typedtree.impl) =
  match impl with
  | Timpl_expr _ -> ()
  | Timpl_letdef { binder; is_pub; loc_; _ } ->
      if not is_pub then
        register_binder ctx binder ~base_loc:loc_ ~is_mut:false ~kind:Top_let
  | Timpl_fun_decl { fun_decl = { fn_binder = binder; is_pub; _ }; loc_; _ }
  | Timpl_stub_decl { binder; is_pub; loc_; _ } ->
      let is_pub =
        match binder.binder_id with
        | Pdot (Qregular { pkg = _; name }) ->
            is_pub || String.starts_with name ~prefix:"__test_"
        | Pdot (Qmethod _ | Qext_method _) -> true
        | Pident _ | Plocal_method _ | Pdyntrait_method _
        | Pdot (Qregular_implicit_pkg _) ->
            assert false
      in
      if not is_pub then (
        register_binder ctx binder ~base_loc:loc_ ~is_mut:false ~kind:Top_func;
        match impl with
        | Timpl_fun_decl { arity_; _ } when not (Fn_arity.is_simple arity_) ->
            Fn_arity.iter arity_ (fun param_kind ->
                match param_kind with
                | Positional _ | Labelled _ -> ()
                | Optional { label; loc_; _ }
                | Question_optional { label; loc_ } ->
                    if not (name_should_be_ignored label) then
                      let entity =
                        Fn_optional_arg { fn = binder.binder_id; label }
                      in
                      register_entity ctx entity ~is_mut:true ~loc:loc_
                | Autofill { label; loc_ } ->
                    if not (name_should_be_ignored label) then
                      let entity =
                        Fn_optional_arg { fn = binder.binder_id; label }
                      in
                      register_entity ctx entity ~is_mut:false ~loc:loc_)
        | _ -> ())

let register_direct_use ctx (import_items : Pkg_config_util.import_items) =
  Map_string.iter import_items (fun _pkg ->
      fun item ->
       Lst.iter item.direct_uses ~f:(fun (name, loc) ->
           let entity = Direct_pkg_use name in
           register_entity ctx entity ~is_mut:false ~loc))

let process_tvar_env ctx ~in_pub_sig (tvar_env : Tvar_env.t) =
  let usage = if in_pub_sig then Used_in_pub else Used_in_priv in
  Tvar_env.iter tvar_env (fun tparam ->
      Lst.iter tparam.constraints ~f:(fun tvc ->
          update_type_usage ctx tvc.trait usage))

let process_trait_decl ctx (trait : Typedtree.trait_decl) =
  let in_pub_sig =
    match trait.trait_vis with
    | Vis_readonly | Vis_fully_pub -> true
    | Vis_priv | Vis_default -> false
  in
  let go_typ ty = process_typ ctx ~in_pub_sig ty in
  Lst.iter trait.trait_methods ~f:(fun meth ->
      Lst.iter meth.method_params ~f:(fun (_, typ) -> go_typ typ);
      (match meth.method_ret with
      | Some method_ret -> go_typ method_ret
      | _ -> ());
      match meth.method_err with
      | Some method_err -> go_typ method_err
      | _ -> ())

let process_trait_alias ctx (decl : Typedtree.trait_alias_decl) =
  process_type_name ctx ~in_pub_sig:decl.trait_alias_is_pub
    decl.trait_alias_target

let process_type_decl ctx (decl : Typedtree.type_decl) =
  let is_pub =
    match decl.td_vis with
    | Vis_readonly | Vis_fully_pub -> true
    | Vis_default | Vis_priv -> false
  in
  process_tvar_env ctx ~in_pub_sig:is_pub decl.td_params;
  let go_typ ty = process_typ ctx ~in_pub_sig:is_pub ty in
  match decl.td_desc with
  | Td_abstract | Td_error No_payload -> ()
  | Td_newtype typ | Td_error (Single_payload typ) | Td_alias typ -> go_typ typ
  | Td_variant constrs | Td_error (Enum_payload constrs) ->
      Lst.iter constrs ~f:(fun constr ->
          Lst.iter constr.constr_args ~f:(fun carg -> go_typ carg.carg_typ))
  | Td_record fields -> Lst.iter fields ~f:(fun field -> go_typ field.field_typ)

let rec collect_named_types ctx ~acc ty =
  let go ty = collect_named_types ctx ~acc ty [@@inline] in
  match Stype.type_repr ty with
  | T_builtin _ | Tvar _ | T_blackhole | T_trait _ | Tparam _ -> ()
  | Tarrow { params_ty; ret_ty; err_ty; is_async = _ } -> (
      Lst.iter params_ty ~f:go;
      go ret_ty;
      match err_ty with Some err_ty -> go err_ty | _ -> ())
  | T_constr { type_constructor = p; tys } -> (
      Lst.iter tys ~f:go;
      match Type_path.Hash.find_opt ctx.types p with
      | Some { decl = `Type decl; _ } -> (
          match Type_path.Hash.find_opt acc p with
          | None -> (
              Type_path.Hash.add acc p decl;
              let go typ = go (Typedtree_util.stype_of_typ typ) [@@inline] in
              match decl.td_desc with
              | Td_abstract | Td_error No_payload -> ()
              | Td_error (Single_payload typ) | Td_newtype typ | Td_alias typ ->
                  go typ
              | Td_error (Enum_payload constrs) | Td_variant constrs ->
                  Lst.iter constrs ~f:(fun constr ->
                      Lst.iter constr.constr_args ~f:(fun carg ->
                          go carg.carg_typ))
              | Td_record fields ->
                  Lst.iter fields ~f:(fun field -> go field.field_typ))
          | Some _ -> ())
      | None | Some { decl = `Trait _ | `Trait_alias _; _ } -> ())

let process_impl ctx (impl : Typedtree.impl) =
  match impl with
  | Timpl_expr { expr; loc_; expr_id = _; is_main = _ } ->
      process_expr ctx ~base_loc:loc_ expr
  | Timpl_fun_decl
      {
        fun_decl =
          {
            kind;
            fn;
            is_pub;
            ty_params_;
            fn_binder = _;
            constraint_names;
            attrs = _;
          };
        loc_;
      } ->
      Lst.iter constraint_names ~f:(fun c ->
          process_type_name ctx ~in_pub_sig:is_pub c);
      (match kind with
      | Fun_kind_regular | Fun_kind_method None -> ()
      | Fun_kind_method (Some type_name) | Fun_kind_default_impl type_name ->
          process_type_name ctx ~in_pub_sig:is_pub type_name
      | Fun_kind_impl { self_ty; trait } ->
          process_type_name ctx ~in_pub_sig:is_pub trait;
          process_typ ctx ~in_pub_sig:is_pub self_ty);
      process_tvar_env ctx ~in_pub_sig:is_pub ty_params_;
      process_fn ctx ~in_pub_sig:is_pub ~base_loc:loc_ fn
  | Timpl_letdef { konstraint; expr; is_pub; loc_; binder = _; attrs = _ } ->
      process_typ_opt ctx ~in_pub_sig:is_pub konstraint;
      process_expr ctx ~base_loc:loc_ expr
  | Timpl_stub_decl { params; ret; is_pub; binder = _; func_stubs = _ } ->
      let all_accessible_types = Type_path.Hash.create 17 in
      Lst.iter params
        ~f:(fun
            ( Param { konstraint; binder = _; ty; kind = _ }
            | Discard_positional_param { konstraint; ty; _ } )
          ->
          process_typ_opt ctx ~in_pub_sig:is_pub konstraint;
          collect_named_types ctx ty ~acc:all_accessible_types);
      (match ret with
      | Some ret ->
          process_typ ctx ~in_pub_sig:is_pub ret;
          collect_named_types ctx
            (Typedtree_util.stype_of_typ ret)
            ~acc:all_accessible_types
      | None -> ());
      Type_path.Hash.iter2 all_accessible_types (fun ty ->
          fun decl ->
           match decl with
           | { td_vis = Vis_fully_pub; _ } -> ()
           | { td_desc = Td_abstract | Td_alias _; _ } -> ()
           | { td_desc = Td_record fields; _ } ->
               Lst.iter fields ~f:(fun field ->
                   update_usage ctx
                     (Struct_field { ty; name = field.field_label.label_name })
                     Read_write);
               update_usage ctx (Construct_struct { ty }) Only_write
           | {
            td_desc = Td_newtype _ | Td_error (No_payload | Single_payload _);
            _;
           } -> (
               match[@warning "-fragile-match"] ty with
               | Toplevel { pkg = _; id } ->
                   update_usage ctx (Enum_constr { ty; name = id }) Read_write
               | _ -> assert false)
           | {
            td_desc = Td_variant constrs | Td_error (Enum_payload constrs);
            _;
           } ->
               Lst.iter constrs ~f:(fun constr ->
                   let tag = constr.constr_tag in
                   update_usage ctx
                     (Enum_constr { ty; name = constr.constr_name.label_name })
                     Read_write;
                   ignore
                     (Lst.fold_left constr.constr_args 0
                        (fun positional_index ->
                          fun carg ->
                           match carg.carg_label with
                           | None ->
                               update_usage ctx
                                 (Constr_argument
                                    {
                                      ty;
                                      tag;
                                      kind = Positional positional_index;
                                    })
                                 Only_read;
                               positional_index + 1
                           | Some label ->
                               update_usage ctx
                                 (Constr_argument
                                    {
                                      ty;
                                      tag;
                                      kind = Labelled label.label_name;
                                    })
                                 Read_write;
                               positional_index))))

let analyze_usage ~(import_items : Pkg_config_util.import_items)
    (prog : Typedtree.output) =
  let ctx =
    {
      entities = H.create 17;
      types = Type_path.Hash.create 17;
      excluding = Ident.Set.empty;
    }
  in
  let (Output { value_defs; type_defs; trait_defs; trait_alias }) = prog in
  Lst.iter trait_defs ~f:(register_trait_decl ctx);
  Lst.iter trait_alias ~f:(register_trait_alias ctx);
  Lst.iter type_defs ~f:(register_type_decl ctx);
  Lst.iter value_defs ~f:(register_impl ctx);
  register_direct_use ctx import_items;
  Lst.iter trait_defs ~f:(process_trait_decl ctx);
  Lst.iter trait_alias ~f:(process_trait_alias ctx);
  Lst.iter type_defs ~f:(process_type_decl ctx);
  Lst.iter value_defs ~f:(process_impl ctx);
  ctx

let report_unused ~diagnostics ~ctx ~report_unused_abstract_type =
  let unused_types : Type_path.Hashset.t = Type_path.Hashset.create 17 in
  Type_path.Hash.iter2 ctx.types (fun ty ->
      fun info ->
       let report w =
         Diagnostics.add_warning diagnostics { loc = info.loc; kind = w }
       in
       let vis =
         match info.decl with
         | `Type td -> td.td_vis
         | `Trait tr -> tr.trait_vis
         | `Trait_alias decl ->
             if decl.trait_alias_is_pub then Vis_fully_pub else Vis_priv
       in
       match (vis, info.usage) with
       | (Vis_fully_pub | Vis_readonly), _ -> assert false
       | _, Unused ->
           Type_path.Hashset.add unused_types ty;
           report (Unused_type_declaration (Type_path_util.name ty))
       | Vis_priv, (Used_in_priv | Used_in_pub) | Vis_default, Used_in_pub -> ()
       | Vis_default, Used_in_priv ->
           if report_unused_abstract_type then
             let name = Type_path_util.name ty in
             let kind =
               match info.decl with
               | `Type _ -> "type"
               | `Trait _ | `Trait_alias _ -> "trait"
             in
             report (Unused_abstract_type { name; kind }));
  let constr_is_read ty tag =
    let entity = Enum_constr { ty; name = Constr_info.get_name tag } in
    match (H.find_exn ctx.entities entity).usage with
    | Read_write | Only_read -> true
    | Only_write | Unused -> false
  in
  H.iter2 ctx.entities (fun entity ->
      fun info ->
       let report w =
         Diagnostics.add_warning diagnostics { loc = info.loc; kind = w }
       in
       match (entity, info.usage) with
       | _, Read_write -> ()
       | _, Only_read when not info.is_mut -> ()
       | Struct_field { ty; name = _ }, _
       | Construct_struct { ty }, _
       | Enum_constr { ty; name = _ }, _
         when Type_path.Hashset.mem unused_types ty ->
           ()
       | Struct_field { ty = _; name }, (Unused | Only_write) ->
           report (Unused_field name)
       | Struct_field { ty = _; name }, Only_read ->
           report
             (Unused_mutability
                (("field '" ^ name ^ "'" : Stdlib.String.t) [@merlin.hide]))
       | Construct_struct { ty }, Unused ->
           report (Struct_never_constructed (Type_path_util.name ty))
       | Construct_struct _, (Only_read | Only_write) -> ()
       | Enum_constr { ty = _; name }, Unused ->
           report (Unused_constructor { constr = name; kind = Unused })
       | Enum_constr { ty = _; name }, Only_write ->
           report (Unused_constructor { constr = name; kind = No_read })
       | Enum_constr { ty = _; name }, Only_read ->
           report (Unused_constructor { constr = name; kind = No_construct })
       | Trait_method { trait = _; method_name }, Unused ->
           report (Unused_func method_name)
       | Trait_method _, (Only_read | Only_write) -> ()
       | Var { id; kind }, (Unused | Only_write) -> (
           let var_name = Ident.base_name id in
           match kind with
           | Local -> report (Unused_var { var_name; is_toplevel = false })
           | Top_let -> report (Unused_var { var_name; is_toplevel = true })
           | Local_func | Top_func -> report (Unused_func var_name))
       | Var { id; kind = _ }, Only_read ->
           report
             (Unused_mutability
                (("'" ^ Ident.base_name id ^ "'"
                  : Stdlib.String.t)
                  [@merlin.hide]))
       | Constr_argument { ty; tag; kind = _ }, _
         when not (constr_is_read ty tag) ->
           ()
       | ( Constr_argument { ty = _; tag; kind = Positional index },
           (Unused | Only_write) ) ->
           let constr = Constr_info.get_name tag in
           report (Unused_constr_arg { constr; index })
       | Constr_argument { ty = _; tag; kind = Labelled label }, Unused ->
           let constr = Constr_info.get_name tag in
           report (Unused_constr_field { constr; label; is_mutated = false })
       | Constr_argument { ty = _; tag; kind = Labelled label }, Only_write ->
           let constr = Constr_info.get_name tag in
           report (Unused_constr_field { constr; label; is_mutated = true })
       | Constr_argument { ty = _; tag; kind }, Only_read -> (
           match[@warning "-fragile-match"] kind with
           | Labelled label ->
               report
                 (Unused_mutability
                    (Stdlib.String.concat ""
                       [
                         "field '";
                         label;
                         "' of constructor ";
                         Constr_info.get_name tag;
                       ] [@merlin.hide]))
           | _ -> assert false)
       | Fn_optional_arg { fn = _; label }, usage ->
           if usage = optional_arg_supplied then
             report (Optional_arg_always_supplied label)
           else if usage = optional_arg_use_default then
             report (Optional_arg_never_supplied label)
       | Direct_pkg_use name, Unused -> report (Unused_import_value name)
       | Direct_pkg_use _, _ -> ())

let analyze_unused ~diagnostics ~import_items
    ~(build_context : Typeutil.build_context)
    ~(import_kind : Pkg_config_util.import_kind) (prog : Typedtree.output) =
  let ctx = analyze_usage ~import_items prog in
  let report_unused_abstract_type =
    match build_context with
    | Exec _ | SingleFile -> false
    | Lib -> (
        match import_kind with Wbtest | Bbtest -> false | Normal -> true)
  in
  report_unused ~diagnostics ~ctx ~report_unused_abstract_type
