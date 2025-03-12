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
module Vec = Basic_vec
module Type_path = Basic_type_path
module Syntax = Parsing_syntax
module Operators = Parsing_operators

let fresh_name name =
  Stdlib.String.concat "" [ "*"; name; "_"; Int.to_string (Basic_uuid.next ()) ]

module S = Ast_builder

let rec type_has_hole (typ : Syntax.typ) =
  match typ with
  | Ptype_any _ -> true
  | Ptype_arrow { ty_arg; ty_res; ty_err; is_async = _ } -> (
      Lst.exists ty_arg type_has_hole
      || type_has_hole ty_res
      ||
      match ty_err with
      | Error_typ { ty; _ } -> type_has_hole ty
      | Default_error_typ _ | No_error_typ -> false)
  | Ptype_tuple { tys } -> Lst.exists tys type_has_hole
  | Ptype_name { tys; _ } -> Lst.exists tys type_has_hole
  | Ptype_option { ty; _ } -> type_has_hole ty
  | Ptype_object _ -> false

type deriver =
  Syntax.deriving_directive ->
  Syntax.type_decl ->
  params:string list ->
  assertions:Syntax.static_assertion Vec.t ->
  diagnostics:Local_diagnostics.t ->
  Syntax.expr

let derive_default (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~params:(_params : string list)
    ~(assertions : Syntax.static_assertion Vec.t)
    ~(diagnostics : Local_diagnostics.t) =
  let rec default_of_typ ~msg (typ : Syntax.typ) =
    (if type_has_hole typ then S.hole
     else
       match typ with
       | Ptype_tuple { tys; _ } -> S.tuple (Lst.map tys (default_of_typ ~msg))
       | typ ->
           S.annotation
             (S.apply_trait_method ~assertions ~typ ~msg
                ~loc:(Syntax.loc_of_type_expression typ)
                trait.name "default" [] No_attr)
             typ
      : Syntax.expr)
  in
  match decl.components with
  | Ptd_abstract ->
      Local_diagnostics.add_error diagnostics
        (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
           ~reason:"target type is abstract" ~loc:trait.loc_);
      S.hole
  | Ptd_extern ->
      Local_diagnostics.add_error diagnostics
        (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
           ~reason:"target type is external" ~loc:trait.loc_);
      S.hole
  | Ptd_newtype typ ->
      S.constr decl.tycon
        [
          default_of_typ
            ~msg:
              (("derive(Default) for newtype " ^ decl.tycon
                : Stdlib.String.t)
                [@merlin.hide])
            typ;
        ]
  | Ptd_error No_payload -> S.constr decl.tycon []
  | Ptd_error (Single_payload typ) ->
      S.constr decl.tycon
        [
          default_of_typ
            ~msg:
              (("derive(Default) for error type " ^ decl.tycon
                : Stdlib.String.t)
                [@merlin.hide])
            typ;
        ]
  | Ptd_error (Enum_payload constrs) | Ptd_variant constrs -> (
      match
        Lst.filter constrs (fun constr ->
            match constr.constr_args with None -> true | _ -> false)
      with
      | [] ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"cannot find a constant constructor as default"
               ~loc:trait.loc_);
          S.hole
      | constr :: [] -> S.constr constr.constr_name.name []
      | { constr_name = constr1; _ } :: { constr_name = constr2; _ } :: rest ->
          let candidate_str =
            match rest with
            | [] ->
                (constr1.name ^ " and " ^ constr2.name
                  : Stdlib.String.t)
                  [@merlin.hide]
            | _ ->
                Stdlib.String.concat ""
                  [ constr1.name; ", "; constr2.name; ", ..." ] [@merlin.hide]
          in
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:
                 ((candidate_str ^ " are both candidates as default constructor"
                   : Stdlib.String.t)
                   [@merlin.hide])
               ~loc:trait.loc_);
          S.hole)
  | Ptd_record fields ->
      S.record
        (Lst.map fields (fun field ->
             ( field.field_name.label,
               default_of_typ
                 ~msg:
                   (Stdlib.String.concat ""
                      [
                        "derive(Default) for field ";
                        field.field_name.label;
                        " of type ";
                        decl.tycon;
                      ] [@merlin.hide])
                 field.field_ty )))
  | Ptd_alias _ -> S.hole

let derive_eq (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~(params : string list) ~assertions ~diagnostics =
  let rec all_eq xs ys zs ~eq_for_elem =
    match (xs, ys, zs) with
    | [], [], [] -> S.const_bool true
    | x :: [], y :: [], z :: [] -> eq_for_elem x y z
    | x :: xs, y :: ys, z :: zs ->
        S.infix_op "&&" (eq_for_elem x y z) (all_eq xs ys zs ~eq_for_elem)
    | _ -> assert false
  in
  let rec eq_of_typ ~msg (typ : Syntax.typ) (lhs : Syntax.expr)
      (rhs : Syntax.expr) =
    if type_has_hole typ then S.const_bool true
    else
      match typ with
      | Ptype_tuple { tys; _ } ->
          let names1 =
            Lst.mapi tys (fun i ->
                fun _ ->
                 fresh_name
                   (("x" ^ Int.to_string i : Stdlib.String.t) [@merlin.hide]))
          in
          let names2 =
            Lst.mapi tys (fun i ->
                fun _ ->
                 fresh_name
                   (("y" ^ Int.to_string i : Stdlib.String.t) [@merlin.hide]))
          in
          S.let_
            (S.ptuple (Lst.map names1 S.pvar))
            lhs
            (S.let_
               (S.ptuple (Lst.map names2 S.pvar))
               rhs
               (all_eq tys names1 names2 ~eq_for_elem:(fun ty ->
                    fun lhs ->
                     fun rhs -> eq_of_typ ~msg ty (S.var lhs) (S.var rhs))))
      | typ ->
          S.apply_trait_method ~assertions ~typ
            ~loc:(Syntax.loc_of_type_expression typ)
            ~msg trait.name Operators.op_equal_info.method_name [ lhs; rhs ]
            No_attr
  in
  match[@warning "-fragile-match"] params with
  | [ lhs; rhs ] -> (
      let lhs = S.var lhs in
      let rhs = S.var rhs in
      match decl.components with
      | Ptd_abstract ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is abstract" ~loc:trait.loc_);
          S.hole
      | Ptd_extern ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is external" ~loc:trait.loc_);
          S.hole
      | Ptd_newtype typ ->
          let lhs = S.newtype_field lhs in
          let rhs = S.newtype_field rhs in
          eq_of_typ
            ~msg:
              (("derive(Eq) for newtype " ^ decl.tycon
                : Stdlib.String.t)
                [@merlin.hide])
            typ lhs rhs
      | Ptd_error No_payload -> S.const_bool true
      | Ptd_error (Single_payload typ) ->
          let x = fresh_name "x" in
          let y = fresh_name "y" in
          let msg =
            (("derive(Eq) for error type " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          S.let_
            (S.pconstr decl.tycon [ (None, S.pvar x) ])
            lhs
            (S.let_
               (S.pconstr decl.tycon [ (None, S.pvar y) ])
               rhs
               (eq_of_typ ~msg typ (S.var x) (S.var y)))
      | Ptd_variant constrs | Ptd_error (Enum_payload constrs) ->
          let fallback_cases =
            match constrs with
            | _ :: _ :: _ -> [ (S.pany, S.const_bool false) ]
            | _ -> []
          in
          let cases =
            Lst.map_append constrs fallback_cases
              (fun { constr_name; constr_args; _ } ->
                let constr_name = constr_name.name in
                let make_result pat_args1 pat_args2 action =
                  ( S.ptuple
                      [
                        S.pconstr constr_name pat_args1;
                        S.pconstr constr_name pat_args2;
                      ],
                    action )
                    [@@inline]
                in
                match constr_args with
                | None -> make_result [] [] (S.const_bool true)
                | Some args ->
                    let msg =
                      (Stdlib.String.concat ""
                         [
                           "derive(Eq) for constructor ";
                           constr_name;
                           " of type ";
                           decl.tycon;
                         ] [@merlin.hide])
                    in
                    let names1 =
                      Lst.mapi args (fun i ->
                          fun _ ->
                           fresh_name
                             (("x" ^ Int.to_string i
                               : Stdlib.String.t)
                               [@merlin.hide]))
                    in
                    let names2 =
                      Lst.mapi args (fun i ->
                          fun _ ->
                           fresh_name
                             (("y" ^ Int.to_string i
                               : Stdlib.String.t)
                               [@merlin.hide]))
                    in
                    let action =
                      all_eq args names1 names2 ~eq_for_elem:(fun cparam ->
                          fun lhs ->
                           fun rhs ->
                            eq_of_typ ~msg cparam.cparam_typ (S.var lhs)
                              (S.var rhs))
                    in
                    let pat_args1 =
                      Lst.map2 args names1 (fun cparam ->
                          fun name ->
                           match cparam.cparam_label with
                           | None -> (None, S.pvar name)
                           | Some label -> (Some label.label_name, S.pvar name))
                    in
                    let pat_args2 =
                      Lst.map2 args names2 (fun cparam ->
                          fun name ->
                           match cparam.cparam_label with
                           | None -> (None, S.pvar name)
                           | Some label -> (Some label.label_name, S.pvar name))
                    in
                    make_result pat_args1 pat_args2 action)
          in
          S.match_ (S.tuple [ lhs; rhs ]) cases
      | Ptd_record [] -> S.const_bool true
      | Ptd_record (field0 :: fields) ->
          let eq_of_field (field : Syntax.field_decl) =
            let field_name = field.field_name.label in
            eq_of_typ
              ~msg:
                (Stdlib.String.concat ""
                   [
                     "derive(Eq) for field ";
                     field_name;
                     " of type ";
                     decl.tycon;
                   ] [@merlin.hide])
              field.field_ty (S.field lhs field_name) (S.field rhs field_name)
          in
          Lst.fold_left fields (eq_of_field field0) (fun acc ->
              fun field -> S.infix_op "&&" acc (eq_of_field field))
      | Ptd_alias _ -> S.hole)
  | _ -> assert false

let derive_compare (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~(params : string list) ~assertions ~diagnostics =
  let rec compare_all tys values1 values2 ~cmp_for_elem =
    match (tys, values1, values2) with
    | [], [], [] -> S.const_int "0"
    | ty :: [], v1 :: [], v2 :: [] -> cmp_for_elem ty v1 v2
    | ty :: tys, v1 :: values1, v2 :: values2 ->
        let tmp = fresh_name "ord" in
        S.match_ (cmp_for_elem ty v1 v2)
          [
            ( S.pconstant (Const_int "0"),
              compare_all tys values1 values2 ~cmp_for_elem );
            (S.pvar tmp, S.var tmp);
          ]
    | _ -> assert false
  in
  let rec compare_of_typ ~msg (typ : Syntax.typ) (lhs : Syntax.expr)
      (rhs : Syntax.expr) =
    if type_has_hole typ then S.const_int "0"
    else
      match typ with
      | Ptype_tuple { tys; _ } ->
          let names1 =
            Lst.mapi tys (fun i ->
                fun _ ->
                 fresh_name
                   (("x" ^ Int.to_string i : Stdlib.String.t) [@merlin.hide]))
          in
          let names2 =
            Lst.mapi tys (fun i ->
                fun _ ->
                 fresh_name
                   (("y" ^ Int.to_string i : Stdlib.String.t) [@merlin.hide]))
          in
          S.let_
            (S.ptuple (Lst.map names1 S.pvar))
            lhs
            (S.let_
               (S.ptuple (Lst.map names2 S.pvar))
               rhs
               (compare_all tys names1 names2 ~cmp_for_elem:(fun typ ->
                    fun name1 ->
                     fun name2 ->
                      compare_of_typ ~msg typ (S.var name1) (S.var name2))))
      | typ ->
          S.apply_trait_method ~assertions ~typ
            ~loc:(Syntax.loc_of_type_expression typ)
            ~msg trait.name "compare" [ lhs; rhs ] No_attr
  in
  match[@warning "-fragile-match"] params with
  | [ lhs; rhs ] -> (
      let lhs = S.var lhs in
      let rhs = S.var rhs in
      Vec.push assertions
        {
          assert_type =
            S.typ decl.tycon
              (Lst.map decl.params (fun { tvar_name; _ } ->
                   match tvar_name with
                   | None -> S.typ_any
                   | Some tvar_name -> S.typ tvar_name []));
          assert_trait = Lident "Eq";
          assert_loc = trait.loc_;
          assert_msg =
            ("derive(Compare) of type " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide];
        };
      match decl.components with
      | Ptd_abstract ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is abstract" ~loc:trait.loc_);
          S.hole
      | Ptd_extern ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is external" ~loc:trait.loc_);
          S.hole
      | Ptd_newtype typ ->
          let lhs = S.newtype_field lhs in
          let rhs = S.newtype_field rhs in
          compare_of_typ
            ~msg:
              (("derive(Compare) for newtype " ^ decl.tycon
                : Stdlib.String.t)
                [@merlin.hide])
            typ lhs rhs
      | Ptd_error No_payload -> S.const_int "0"
      | Ptd_error (Single_payload typ) ->
          let x = fresh_name "x" in
          let y = fresh_name "y" in
          let msg =
            (("derive(Compare) for error type " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          S.let_
            (S.pconstr decl.tycon [ (None, S.pvar x) ])
            lhs
            (S.let_
               (S.pconstr decl.tycon [ (None, S.pvar y) ])
               rhs
               (compare_of_typ ~msg typ (S.var x) (S.var y)))
      | Ptd_variant constrs | Ptd_error (Enum_payload constrs) ->
          let constr_count = List.length constrs in
          let cases =
            Lst.mapi constrs (fun constr_index ->
                fun { constr_name; constr_args; _ } ->
                 let constr_name = constr_name.name in
                 let constr_args = Option.value constr_args ~default:[] in
                 let arg_names =
                   Lst.mapi constr_args (fun i ->
                       fun _ ->
                        fresh_name
                          (("x" ^ Int.to_string i
                            : Stdlib.String.t)
                            [@merlin.hide]))
                 in
                 let arg_pats =
                   Lst.map2 arg_names constr_args (fun name ->
                       fun { cparam_label; _ } ->
                        match cparam_label with
                        | None -> (None, S.pvar name)
                        | Some label -> (Some label.label_name, S.pvar name))
                 in
                 let eq_case =
                   let arg_names' =
                     Lst.mapi constr_args (fun i ->
                         fun _ ->
                          fresh_name
                            (("y" ^ Int.to_string i
                              : Stdlib.String.t)
                              [@merlin.hide]))
                   in
                   let arg_pats' =
                     Lst.map2 arg_names' constr_args (fun name ->
                         fun { cparam_label; _ } ->
                          match cparam_label with
                          | None -> (None, S.pvar name)
                          | Some label -> (Some label.label_name, S.pvar name))
                   in
                   let msg =
                     (Stdlib.String.concat ""
                        [
                          "derive(Compare) for constructor ";
                          constr_name;
                          " of type ";
                          decl.tycon;
                        ] [@merlin.hide])
                   in
                   ( S.pconstr constr_name arg_pats',
                     compare_all constr_args arg_names arg_names'
                       ~cmp_for_elem:(fun { cparam_typ; _ } ->
                         fun name1 ->
                          fun name2 ->
                           compare_of_typ ~msg cparam_typ (S.var name1)
                             (S.var name2)) )
                 in
                 let lt_case = (S.pany, S.const_int "-1") in
                 let cases =
                   if constr_count = 1 then [ eq_case ]
                   else if constr_index = 0 then [ eq_case; lt_case ]
                   else
                     let pat =
                       S.por
                         (Lst.map (Lst.take constrs constr_index)
                            (fun { constr_name; _ } ->
                              S.pconstr constr_name.name [ (None, S.pany) ]))
                     in
                     let gt_case = (pat, S.const_int "1") in
                     if constr_index = constr_count - 1 then
                       [ gt_case; eq_case ]
                     else [ gt_case; eq_case; lt_case ]
                 in
                 (S.pconstr constr_name arg_pats, S.match_ rhs cases))
          in
          S.match_ lhs cases
      | Ptd_record fields ->
          let lhs_fields =
            Lst.map fields (fun field -> S.field lhs field.field_name.label)
          in
          let rhs_fields =
            Lst.map fields (fun field -> S.field rhs field.field_name.label)
          in
          let cmp_of_field (field : Syntax.field_decl) lhs rhs =
            let field_name = field.field_name.label in
            compare_of_typ
              ~msg:
                (Stdlib.String.concat ""
                   [
                     "derive(Compare) for field ";
                     field_name;
                     " of type ";
                     decl.tycon;
                   ] [@merlin.hide])
              field.field_ty lhs rhs
          in
          compare_all fields lhs_fields rhs_fields ~cmp_for_elem:cmp_of_field
      | Ptd_alias _ -> S.hole)
  | _ -> assert false

let derive_show (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~(params : string list) ~assertions ~diagnostics =
  match[@warning "-fragile-match"] params with
  | [ obj; logger ] -> (
      let obj = S.var obj in
      let logger = S.var logger in
      let write_string ?(is_last = false) str self =
        if is_last then S.dot_apply self "write_string" [ S.const_string str ]
        else S.dotdot_apply self "write_string" [ S.const_string str ]
      in
      let rec write_list tys objs ~write_elem self =
        match (tys, objs) with
        | [], [] -> self
        | ty :: [], obj :: [] -> write_elem ty obj self
        | ty :: tys, obj :: objs ->
            write_list tys objs ~write_elem
              (write_string ", " (write_elem ty obj self))
        | _ -> assert false
      in
      let rec write_object ~msg (typ : Syntax.typ) obj self =
        if type_has_hole typ then self
        else
          match typ with
          | Ptype_arrow _ -> write_string "<function>" self
          | Ptype_tuple { tys; _ } ->
              let names =
                Lst.mapi tys (fun i ->
                    fun _ ->
                     fresh_name
                       (("x" ^ Int.to_string i
                         : Stdlib.String.t)
                         [@merlin.hide]))
              in
              S.let_
                (S.ptuple (Lst.map names S.pvar))
                obj
                (write_string ")"
                   (write_list tys names
                      ~write_elem:(fun typ ->
                        fun name ->
                         fun self -> write_object ~msg typ (S.var name) self)
                      (write_string "(" self)))
          | _ ->
              let assertion : Syntax.static_assertion =
                {
                  assert_type = typ;
                  assert_trait = trait.name;
                  assert_loc = Syntax.loc_of_type_expression typ;
                  assert_msg = msg;
                }
              in
              Vec.push assertions assertion;
              S.dotdot_apply self "write_object" [ obj ]
      in
      match decl.components with
      | Ptd_abstract ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is abstract" ~loc:trait.loc_);
          S.hole
      | Ptd_extern ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is external" ~loc:trait.loc_);
          S.hole
      | Ptd_newtype typ ->
          let msg =
            (("derive(Show) for newtype " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          write_string ~is_last:true ")"
            (write_object ~msg typ (S.newtype_field obj)
               (write_string (decl.tycon ^ "(") logger))
      | Ptd_error No_payload -> write_string ~is_last:true decl.tycon logger
      | Ptd_error (Single_payload typ) ->
          let var = fresh_name "err_payload" in
          let msg =
            (("derive(Show) for error type " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          S.let_
            (S.pconstr decl.tycon [ (None, S.pvar var) ])
            obj
            (write_string ~is_last:true ")"
               (write_object ~msg typ (S.var var)
                  (write_string (decl.tycon ^ "(") logger)))
      | Ptd_error (Enum_payload constrs) | Ptd_variant constrs ->
          S.match_ obj
            (Lst.map constrs (fun { constr_name; constr_args; _ } ->
                 let constr_name = constr_name.name in
                 match constr_args with
                 | None ->
                     ( S.pconstr constr_name [],
                       write_string ~is_last:true constr_name logger )
                 | Some args ->
                     let vars = Lst.map args (fun _ -> fresh_name "arg") in
                     let action =
                       let msg =
                         (Stdlib.String.concat ""
                            [
                              "derive(Show) for constructor ";
                              constr_name;
                              " of type ";
                              decl.tycon;
                            ] [@merlin.hide])
                       in
                       write_string ~is_last:true ")"
                         (write_list args vars
                            ~write_elem:(fun constr_param ->
                              fun var ->
                               fun self ->
                                match constr_param.cparam_label with
                                | None ->
                                    write_object ~msg constr_param.cparam_typ
                                      (S.var var) self
                                | Some label ->
                                    write_object ~msg constr_param.cparam_typ
                                      (S.var var)
                                      (write_string (label.label_name ^ "=")
                                         self))
                            (write_string (constr_name ^ "(") logger))
                     in
                     ( S.pconstr constr_name
                         (Lst.map2 args vars (fun cparam ->
                              fun var ->
                               let label =
                                 Option.map
                                   (fun (l : Syntax.label) -> l.label_name)
                                   cparam.cparam_label
                               in
                               (label, S.pvar var))),
                       action )))
      | Ptd_record fields ->
          let write_field (field : Syntax.field_decl) _ self =
            let field_name = field.field_name.label in
            let msg =
              (Stdlib.String.concat ""
                 [
                   "derive(Show) for field ";
                   field_name;
                   " of type ";
                   decl.tycon;
                 ] [@merlin.hide])
            in
            write_object ~msg field.field_ty (S.field obj field_name)
              (write_string (field_name ^ ": ") self)
          in
          write_string ~is_last:true "}"
            (write_list fields fields ~write_elem:write_field
               (write_string "{" logger))
      | Ptd_alias _ -> S.hole)
  | _ -> assert false

let derive_hash (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~(params : string list) ~assertions ~diagnostics =
  match[@warning "-fragile-match"] params with
  | [ obj; hasher ] -> (
      let obj = S.var obj in
      let hasher = S.var hasher in
      let rec hash_of_typ ~msg (typ : Syntax.typ) (obj : Syntax.expr) =
        if type_has_hole typ then S.unit
        else
          match typ with
          | Ptype_tuple { tys; _ } ->
              let names =
                Lst.mapi tys (fun i ->
                    fun _ ->
                     fresh_name
                       (("x" ^ Int.to_string i
                         : Stdlib.String.t)
                         [@merlin.hide]))
              in
              S.let_
                (S.ptuple (Lst.map names S.pvar))
                obj
                (S.seq
                   (Lst.map2 tys names (fun typ ->
                        fun name -> hash_of_typ ~msg typ (S.var name))))
          | typ ->
              S.apply_trait_method ~assertions ~typ
                ~loc:(Syntax.loc_of_type_expression typ)
                ~msg trait.name "hash_combine" [ obj; hasher ] No_attr
      in
      match decl.components with
      | Ptd_abstract ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is abstract" ~loc:trait.loc_);
          S.hole
      | Ptd_extern ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is external" ~loc:trait.loc_);
          S.hole
      | Ptd_newtype typ ->
          let msg =
            (("derive(Hash) for newtype " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          hash_of_typ ~msg typ (S.newtype_field obj)
      | Ptd_error No_payload ->
          S.dot_apply hasher "combine_int" [ S.const_int "0" ]
      | Ptd_error (Single_payload typ) ->
          let var = fresh_name "err_payload" in
          let msg =
            (("derive(Hash) for error type " ^ decl.tycon
              : Stdlib.String.t)
              [@merlin.hide])
          in
          S.let_
            (S.pconstr decl.tycon [ (None, S.pvar var) ])
            obj
            (S.sequence
               (S.dot_apply hasher "combine_int" [ S.const_int "0" ])
               (hash_of_typ ~msg typ (S.var var)))
      | Ptd_error (Enum_payload constrs) | Ptd_variant constrs ->
          S.match_ obj
            (Lst.mapi constrs (fun constr_index ->
                 fun { constr_name; constr_args; _ } ->
                  let constr_name = constr_name.name in
                  let args = Option.value ~default:[] constr_args in
                  let vars = Lst.map args (fun _ -> fresh_name "arg") in
                  let action =
                    let msg =
                      (Stdlib.String.concat ""
                         [
                           "derive(Hash) for constructor ";
                           constr_name;
                           " of type ";
                           decl.tycon;
                         ] [@merlin.hide])
                    in
                    S.seq
                      (S.dot_apply hasher "combine_int"
                         [ S.const_int (string_of_int constr_index) ]
                      :: Lst.map2 args vars (fun cparam ->
                             fun var ->
                              hash_of_typ ~msg cparam.cparam_typ (S.var var)))
                  in
                  ( S.pconstr constr_name
                      (Lst.map2 args vars (fun cparam ->
                           fun var ->
                            let label =
                              Option.map
                                (fun (l : Syntax.label) -> l.label_name)
                                cparam.cparam_label
                            in
                            (label, S.pvar var))),
                    action )))
      | Ptd_record fields ->
          S.seq
            (Lst.map fields (fun field ->
                 let field_name = field.field_name.label in
                 hash_of_typ
                   ~msg:
                     (Stdlib.String.concat ""
                        [
                          "derive(Hash) for field ";
                          field_name;
                          " of type ";
                          decl.tycon;
                        ] [@merlin.hide])
                   field.field_ty (S.field obj field_name)))
      | Ptd_alias _ -> S.hole)
  | _ -> assert false

module DeriveJson = struct
  type rename_case =
    | Unchanged
    | Lowercase
    | Uppercase
    | PascalCase
    | CamelCase
    | SnakeCase
    | ScreamingSnakeCase
    | KebabCase
    | ScreamingKebabCase

  let parse_rename_directive name =
    try
      let res =
        match name with
        | "lowercase" -> Lowercase
        | "UPPERCASE" -> Uppercase
        | "PascalCase" -> PascalCase
        | "camelCase" -> CamelCase
        | "snake_case" -> SnakeCase
        | "SCREAMING_SNAKE_CASE" -> ScreamingSnakeCase
        | "kebab-case" -> KebabCase
        | "SCREAMING-KEBAB-CASE" -> ScreamingKebabCase
        | _ -> assert false
      in
      Some res
    with _ -> None

  let parse_rename_directive_item_exn (item : Derive_args.meta_item) =
    (match parse_rename_directive (Derive_args.item_string_exn item) with
     | Some res -> res
     | None ->
         Derive_args.fail_item item
           "Expected one of `lowercase`, `UPPERCASE`, `PascalCase`, \
            `camelCase`, `snake_case`, `SCREAMING_SNAKE_CASE`, `kebab-case`, \
            `SCREAMING-KEBAB-CASE`"
      : rename_case)

  let rename_name_parts (case : rename_case) (name_parts : string list) =
    (match case with
     | Unchanged -> failwith "Unchanged case should not be used here"
     | Lowercase -> String.lowercase_ascii (String.concat "" name_parts)
     | Uppercase -> String.uppercase_ascii (String.concat "" name_parts)
     | PascalCase ->
         String.concat "" (Lst.map name_parts String.capitalize_ascii)
     | CamelCase ->
         let camel_case_name =
           List.mapi
             (fun i ->
               fun part ->
                if i = 0 then String.lowercase_ascii part
                else String.capitalize_ascii part)
             name_parts
         in
         String.concat "" camel_case_name
     | SnakeCase -> String.lowercase_ascii (String.concat "_" name_parts)
     | ScreamingSnakeCase ->
         String.uppercase_ascii (String.concat "_" name_parts)
     | KebabCase -> String.lowercase_ascii (String.concat "-" name_parts)
     | ScreamingKebabCase ->
         String.uppercase_ascii (String.concat "-" name_parts)
      : string)

  let rename_name_from_snake (case : rename_case) (name : string) =
    (match case with
     | Unchanged -> name
     | _ ->
         let name_parts = String.split_on_char '_' name in
         rename_name_parts case name_parts
      : string)

  let rename_name_from_pascal (case : rename_case) (name : string) =
    (let rec split_pascal_name (name : string) (last_start : int) (idx : int)
         (acc : string list) =
       let is_upper ch = ch >= 'A' && ch <= 'Z' in
       if idx >= String.length name then
         let last_segment =
           if last_start < idx then
             [ String.sub name last_start (idx - last_start) ]
           else []
         in
         List.rev (last_segment @ acc)
       else
         let ch = name.[idx] in
         if ch = '_' then split_pascal_name name (idx + 1) (idx + 1) acc
         else if not (is_upper ch) then
           let next_idx = idx + 1 in
           if next_idx >= String.length name then
             let sub = String.sub name last_start (idx - last_start + 1) in
             split_pascal_name name next_idx next_idx (sub :: acc)
           else
             let next_ch = name.[next_idx] in
             if is_upper next_ch || next_ch = '_' then
               let sub = String.sub name last_start (idx - last_start + 1) in
               split_pascal_name name next_idx next_idx (sub :: acc)
             else split_pascal_name name last_start next_idx acc
         else split_pascal_name name last_start (idx + 1) acc
     in
     match case with
     | Unchanged -> name
     | _ ->
         let name_parts = split_pascal_name name 0 0 [] in
         rename_name_parts case name_parts
      : string)

  let default_tag_name = "$tag"

  type json_repr =
    | Tagged of { tag_name : string }
    | ExtTagged
    | AdjTagged of { tag_name : string; contents_name : string }
    | Untagged

  module StringMap = Basic_map_string
  module IntMap = Basic_map_int

  let default_struct_repr = Untagged
  let default_enum_repr = Tagged { tag_name = default_tag_name }

  let parse_repr (item : Derive_args.meta_item)
      (diag : string -> Rloc.t -> unit) =
    (let args = Derive_args.item_value_args_exn item in
     let untagged = ref false in
     let tag = ref None in
     let contents = ref None in
     let ext_tagged = ref false in
     Derive_args.parse args diag (fun item ->
         match () with
         | _ when Derive_args.item_label_is "tag" item ->
             tag := Some (Derive_args.item_string_exn item)
         | _ when Derive_args.item_label_is "contents" item ->
             contents := Some (Derive_args.item_string_exn item)
         | _ when Derive_args.item_label_is "ext_tagged" item ->
             Derive_args.item_no_value_exn item;
             ext_tagged := true
         | _ when Derive_args.item_label_is "untagged" item ->
             Derive_args.item_no_value_exn item;
             untagged := true
         | _ ->
             Derive_args.fail_item item
               "Expected one of `tag`, `content`, `ext_tagged` or `untagged`");
     match (!untagged, !tag, !contents, !ext_tagged) with
     | true, None, None, false -> Untagged
     | false, Some tag, None, false -> Tagged { tag_name = tag }
     | false, Some tag, Some content, false ->
         AdjTagged { tag_name = tag; contents_name = content }
     | false, None, None, true -> ExtTagged
     | _ ->
         Derive_args.fail_item item
           "Expected one of: `tag=...`, both `tag=...` and `contents=...`, \
            `ext_tagged`, `untagged`."
      : json_repr)

  type field_value_source =
    | SerializedField of string
    | SerializedFieldIndex of int
    | Constant of Syntax.expr
    | Json of json_layout

  and struct_field = {
    field_name : string;
    value_source : field_value_source;
    skip_serialize_if : (Syntax.expr -> Syntax.expr) option;
  }

  and struct_field_kind = Field of struct_field
  and object_layout = struct_field_kind list

  and json_layout =
    | Value of field_value_source
    | Array of json_layout list
    | Object of object_layout

  type field_layout_args = {
    rename : string option;
    default : bool;
    default_expr : Syntax.expr option;
  }

  let default_field_layout_args =
    { rename = None; default = false; default_expr = None }

  type struct_layout_args = {
    repr : json_repr;
    rename_fields : rename_case;
    rename_struct : rename_case;
    struct_name_override : string option;
    field_layout : field_layout_args StringMap.t;
    default : bool;
  }

  let default_struct_layout_args =
    {
      repr = default_struct_repr;
      rename_fields = Unchanged;
      rename_struct = Unchanged;
      struct_name_override = None;
      field_layout = StringMap.empty;
      default = false;
    }

  type enum_ctor_repr = CtorTuple | CtorObject

  type enum_ctor_layout_args = {
    ctor_name_override : string option;
    named_field_layout : field_layout_args StringMap.t;
    positional_field_layout : field_layout_args IntMap.t;
  }

  type enum_layout_args = {
    repr : json_repr;
    repr_ctors : enum_ctor_repr;
    rename_ctors : rename_case;
    rename_fields : rename_case;
    ctor_layout_args : enum_ctor_layout_args StringMap.t;
    default : bool;
  }

  let default_ctor_layout_args =
    {
      ctor_name_override = None;
      named_field_layout = StringMap.empty;
      positional_field_layout = IntMap.empty;
    }

  let default_enum_layout_args =
    {
      repr = default_enum_repr;
      repr_ctors = CtorObject;
      rename_ctors = Unchanged;
      rename_fields = Unchanged;
      ctor_layout_args = StringMap.empty;
      default = false;
    }

  let parse_field_attr attr diag =
    Derive_args.parse_fold attr diag default_field_layout_args (fun layout ->
        fun item ->
         match () with
         | _ when Derive_args.item_label_is "rename" item ->
             let name = Derive_args.item_string_exn item in
             { layout with rename = Some name }
         | _ when Derive_args.item_label_is "default" item -> (
             match item.value with
             | Nothing -> { layout with default = true }
             | Value expr ->
                 { layout with default = true; default_expr = Some expr }
             | _ ->
                 Derive_args.fail_item item
                   "Expected `default` or a `default=<expression>`")
         | _ -> Derive_args.fail_item item "Unknown attribute")

  let parse_struct_attr attr diag =
    Derive_args.parse_fold attr diag default_struct_layout_args (fun layout ->
        fun item ->
         match () with
         | _ when Derive_args.item_label_is "default" item ->
             Derive_args.item_no_value_exn item;
             { layout with default = true }
         | _ when Derive_args.item_label_is "repr" item ->
             let repr = parse_repr item diag in
             { layout with repr }
         | _ when Derive_args.item_label_is "rename_fields" item ->
             let rename_fields = parse_rename_directive_item_exn item in
             { layout with rename_fields }
         | _ when Derive_args.item_label_is "rename_struct" item ->
             let rename_struct = parse_rename_directive_item_exn item in
             { layout with rename_struct }
         | _ when Derive_args.item_label_is "rename_all" item ->
             let rename_directive = parse_rename_directive_item_exn item in
             {
               layout with
               rename_fields = rename_directive;
               rename_struct = rename_directive;
             }
         | _ when Derive_args.item_label_is "fields" item ->
             let args = Derive_args.item_value_args_exn item in
             let field_layout =
               Derive_args.parse_fold args diag StringMap.empty (fun map ->
                   fun item ->
                    match Derive_args.item_label item with
                    | Some field_name ->
                        let args = Derive_args.item_value_args_exn item in
                        let layout_args = parse_field_attr args diag in
                        StringMap.add map field_name layout_args
                    | _ -> Derive_args.fail_item item "Expected field name")
             in
             { layout with field_layout }
         | _ -> Derive_args.fail_item item "Unknown attribute")

  let parse_enum_ctor_layout_attr diag layout item =
    match Derive_args.item_label item with
    | Some ctor_name ->
        let args = Derive_args.item_value_args_exn item in
        let ctor_layout_args =
          Derive_args.parse_fold args diag default_ctor_layout_args
            (fun layout ->
              fun item ->
               match () with
               | _ when Derive_args.item_label_is "rename" item ->
                   let name = Derive_args.item_string_exn item in
                   { layout with ctor_name_override = Some name }
               | _ when Derive_args.item_label_is "fields" item ->
                   let args = Derive_args.item_value_args_exn item in
                   let named, positional =
                     Derive_args.parse_fold args diag
                       (StringMap.empty, IntMap.empty)
                       (fun (named, positional) ->
                         fun item ->
                          match Derive_args.item_label item with
                          | Some field_name -> (
                              let args = Derive_args.item_value_args_exn item in
                              let field_layout_args =
                                parse_field_attr args diag
                              in
                              match int_of_string_opt field_name with
                              | Some i ->
                                  ( named,
                                    IntMap.add positional i field_layout_args )
                              | None ->
                                  ( StringMap.add named field_name
                                      field_layout_args,
                                    positional ))
                          | _ ->
                              Derive_args.fail_item item "Expected field name")
                   in
                   {
                     layout with
                     named_field_layout = named;
                     positional_field_layout = positional;
                   }
               | _ -> Derive_args.fail_item item "Unknown attribute")
        in
        StringMap.add layout ctor_name ctor_layout_args
    | _ -> Derive_args.fail_item item "Expected constructor name"

  let parse_enum_attr (attr : Syntax.deriving_directive) diag =
    let res =
      Derive_args.parse_fold attr.args diag default_enum_layout_args
        (fun layout ->
          fun item ->
           match () with
           | _ when Derive_args.item_label_is "default" item ->
               Derive_args.item_no_value_exn item;
               { layout with default = true }
           | _ when Derive_args.item_label_is "repr" item ->
               let repr = parse_repr item diag in
               { layout with repr }
           | _ when Derive_args.item_label_is "case_repr" item ->
               let args = Derive_args.item_value_args_exn item in
               let repr_ctors =
                 Derive_args.parse_fold args diag CtorObject (fun _ ->
                     fun item ->
                      match () with
                      | _ when Derive_args.item_label_is "tuple" item ->
                          CtorTuple
                      | _ when Derive_args.item_label_is "object" item ->
                          CtorObject
                      | _ ->
                          Derive_args.fail_item item
                            "Expected `tuple` or `object`")
               in
               { layout with repr_ctors }
           | _ when Derive_args.item_label_is "rename_cases" item ->
               let rename_ctors = parse_rename_directive_item_exn item in
               { layout with rename_ctors }
           | _ when Derive_args.item_label_is "rename_fields" item ->
               let rename_fields = parse_rename_directive_item_exn item in
               { layout with rename_fields }
           | _ when Derive_args.item_label_is "rename_all" item ->
               let rename_directive = parse_rename_directive_item_exn item in
               {
                 layout with
                 rename_fields = rename_directive;
                 rename_ctors = rename_directive;
               }
           | _ when Derive_args.item_label_is "cases" item ->
               let args = Derive_args.item_value_args_exn item in
               let ctor_layout_args_list =
                 Derive_args.parse_fold args diag StringMap.empty
                   (parse_enum_ctor_layout_attr diag)
               in
               { layout with ctor_layout_args = ctor_layout_args_list }
           | _ -> Derive_args.fail_item item "Unknown attribute")
    in
    match (res.repr, res.repr_ctors) with
    | Tagged _, CtorTuple ->
        diag "Internally tagged enum is incompatible with case_repr(tuple)"
          attr.loc_;
        raise Derive_args.MetaParseErrorNoEmit
    | _ -> res

  let option_skip_de_p (value : Syntax.expr) =
    (S.match_ value
       [
         (S.pconstr "None" [], S.const_bool true); (S.pany, S.const_bool false);
       ]
      : Syntax.expr)

  let layout_struct_payload (fields : (Syntax.typ * string) list)
      (layout : struct_layout_args) =
    (let field_renames =
       Lst.map fields (fun (_ty, f) ->
           let field_layout =
             StringMap.find_default layout.field_layout f
               default_field_layout_args
           in
           match field_layout.rename with
           | Some s -> s
           | None -> rename_name_from_snake layout.rename_fields f)
     in
     Lst.map2 fields field_renames (fun (ty, name) ->
         fun rename ->
          Field
            {
              field_name = rename;
              value_source = SerializedField name;
              skip_serialize_if =
                (if S.type_is_option ty then Some option_skip_de_p else None);
            })
      : object_layout)

  let layout_struct_like (serialized_name : string) (repr : json_repr)
      (payload : object_layout) =
    (match repr with
     | Untagged -> payload
     | Tagged tag ->
         Field
           {
             field_name = tag.tag_name;
             value_source = Constant (S.const_string serialized_name);
             skip_serialize_if = None;
           }
         :: payload
     | ExtTagged ->
         [
           Field
             {
               field_name = serialized_name;
               value_source = Json (Object payload);
               skip_serialize_if = None;
             };
         ]
     | AdjTagged { tag_name; contents_name } ->
         [
           Field
             {
               field_name = tag_name;
               value_source = Constant (S.const_string serialized_name);
               skip_serialize_if = None;
             };
           Field
             {
               field_name = contents_name;
               value_source = Json (Object payload);
               skip_serialize_if = None;
             };
         ]
      : object_layout)

  let layout_struct (name : string) (fields : (Syntax.typ * string) list)
      (layout : struct_layout_args) =
    (let payload = layout_struct_payload fields layout in
     let name =
       match layout.struct_name_override with
       | Some s -> s
       | _ -> rename_name_from_pascal layout.rename_struct name
     in
     let payload = layout_struct_like name layout.repr payload in
     Object payload
      : json_layout)

  let layout_enum_payload_object (fields : string option list)
      (layout : enum_layout_args) =
    (let ctor_layout =
       StringMap.find_default layout.ctor_layout_args "default"
         default_ctor_layout_args
     in
     let field_renames = Vec.empty () in
     let _ =
       Lst.fold_left fields (0, 0) (fun (absolute, positional) ->
           fun f ->
            let positional_layout =
              IntMap.find_default ctor_layout.positional_field_layout absolute
                default_field_layout_args
            in
            let named_layout =
              match f with
              | None -> default_field_layout_args
              | Some s ->
                  StringMap.find_default ctor_layout.named_field_layout s
                    default_field_layout_args
            in
            let rename, is_positional =
              match (positional_layout.rename, named_layout.rename, f) with
              | _, Some s, _ -> (s, false)
              | Some s, _, _ -> (s, false)
              | _, _, Some s ->
                  (rename_name_from_snake layout.rename_fields s, false)
              | None, None, None -> (string_of_int positional, true)
            in
            Vec.push field_renames rename;
            (absolute + 1, if is_positional then positional + 1 else positional))
     in
     let field_renames = Vec.to_list field_renames in
     Lst.mapi field_renames (fun i ->
         fun r ->
          Field
            {
              field_name = r;
              value_source = SerializedFieldIndex i;
              skip_serialize_if = None;
            })
      : object_layout)

  let enum_ctor_renamed (layout_args : enum_layout_args)
      (original_name : string) =
    (let ctor_override =
       StringMap.find_default layout_args.ctor_layout_args original_name
         default_ctor_layout_args
     in
     match ctor_override.ctor_name_override with
     | Some s -> s
     | None -> rename_name_from_pascal layout_args.rename_ctors original_name
      : string)

  let layout_enum_ctor_object (name : string) (enum_args : enum_layout_args)
      (fields : string option list) =
    (let payload = layout_enum_payload_object fields enum_args in
     let ctor_serialized_name = enum_ctor_renamed enum_args name in
     let payload =
       layout_struct_like ctor_serialized_name enum_args.repr payload
     in
     Object payload
      : json_layout)

  let layout_enum_payload_tuple (name : string) (fields : string option list)
      (layout : enum_layout_args) =
    (let raw_payloads =
       List.mapi (fun i -> fun _ -> Value (SerializedFieldIndex i)) fields
     in
     match layout.repr with
     | Tagged _ -> Value (Constant (S.const_string name)) :: raw_payloads
     | _ -> raw_payloads
      : json_layout list)

  let layout_tuple_like (name : string) (enum_args : enum_layout_args)
      (payload : json_layout list) =
    (match enum_args.repr with
     | Untagged -> Array payload
     | Tagged _ ->
         failwith
           "Unreachable: Tuple-like ctor does not support internally tagged \
            payload. Error should be intercepted earlier."
     | ExtTagged ->
         Object
           [
             Field
               {
                 field_name = name;
                 value_source = Json (Array payload);
                 skip_serialize_if = None;
               };
           ]
     | AdjTagged { tag_name; contents_name } ->
         Object
           [
             Field
               {
                 field_name = tag_name;
                 value_source = Constant (S.const_string name);
                 skip_serialize_if = None;
               };
             Field
               {
                 field_name = contents_name;
                 value_source = Json (Array payload);
                 skip_serialize_if = None;
               };
           ]
      : json_layout)

  let layout_enum_ctor_tuple (name : string) (enum_args : enum_layout_args)
      (fields : string option list) =
    (let ctor_serialized_name = enum_ctor_renamed enum_args name in
     let payload =
       layout_enum_payload_tuple ctor_serialized_name fields enum_args
     in
     layout_tuple_like ctor_serialized_name enum_args payload
      : json_layout)

  let layout_enum (ctors : (string * string option list) list)
      (options : enum_layout_args) =
    let layout_ctor (name, fields) =
      match options.repr_ctors with
      | CtorObject -> layout_enum_ctor_object name options fields
      | CtorTuple -> layout_enum_ctor_tuple name options fields
    in
    Lst.map ctors layout_ctor

  let serialize_layout
      ?(named_fields : (Syntax.expr * Syntax.typ) StringMap.t = StringMap.empty)
      ?(indexed_fields : (Syntax.expr * Syntax.typ) array = [||])
      (layout : json_layout) ~(assertions : Syntax.static_assertion Vec.t)
      ~(trait : Syntax.type_name) ~(type_description : string) =
    let rec ser_layout_internal (layout : json_layout) =
      match layout with
      | Value v -> ser_value v
      | Array values -> ser_array values
      | Object fields ->
          let map = S.var "$map" in
          let write_objects = ser_object fields map in
          let string_to_json_map_typ =
            S.typ "Map" [ S.typ "String" []; S.typ "Json" [] ]
          in
          S.let_ (S.pvar "$map")
            (S.annotation (S.map_string_expr []) string_to_json_map_typ)
            (S.sequence write_objects (S.constr "Object" [ map ]))
    and ser_object (fields : object_layout) (write_to_map : Syntax.expr) =
      S.seq
        (Lst.map fields (fun field ->
             match field with
             | Field { field_name; value_source; skip_serialize_if = None } ->
                 let value = ser_value value_source in
                 S.dot_apply write_to_map "set"
                   [ S.const_string field_name; value ]
             | Field { field_name; value_source; skip_serialize_if = Some skip }
               ->
                 S.if_
                   (skip (get_source value_source))
                   S.unit
                   (S.dot_apply write_to_map "set"
                      [ S.const_string field_name; ser_value value_source ])))
    and ser_array (layout : json_layout list) =
      S.array (Lst.map layout (fun l -> ser_layout_internal l))
    and to_json_of_typ_wrap ~msg (typ : Syntax.typ) (obj : Syntax.expr) =
      if type_has_hole typ then S.hole
      else
        match S.extract_option_inner typ with
        | Some option_ty ->
            S.match_ obj
              [
                (S.pconstr "None" [], S.constr "Null" []);
                ( S.pconstr "Some" [ (None, S.pvar "$inner") ],
                  to_json_of_typ ~msg option_ty (S.var "$inner") );
              ]
        | _ -> to_json_of_typ ~msg typ obj
    and to_json_of_typ ~msg typ obj =
      S.apply_trait_method ~assertions ~typ
        ~loc:(Syntax.loc_of_type_expression typ)
        ~msg:
          (Stdlib.String.concat ""
             [
               "derive ";
               Basic_longident.to_string trait.name;
               " for ";
               type_description;
               " ";
               msg;
             ])
        trait.name "to_json" [ obj ] No_attr
    and get_source (source : field_value_source) =
      match source with
      | SerializedField field_name ->
          let field, _ = StringMap.find_exn named_fields field_name in
          field
      | SerializedFieldIndex index ->
          let field, _ = indexed_fields.(index) in
          field
      | Constant c -> c
      | Json _ ->
          failwith
            "Unexpected JSON layout in get_source, which is intended to get \
             the value before serialization"
    and ser_value (source : field_value_source) =
      match source with
      | SerializedField field_name ->
          let field, typ = StringMap.find_exn named_fields field_name in
          to_json_of_typ_wrap
            ~msg:(("field " ^ field_name : Stdlib.String.t) [@merlin.hide])
            typ field
      | SerializedFieldIndex index ->
          let field, typ = indexed_fields.(index) in
          to_json_of_typ_wrap
            ~msg:
              (("field index " ^ Int.to_string index
                : Stdlib.String.t)
                [@merlin.hide])
            typ field
      | Constant c -> S.dot_apply c "to_json" []
      | Json layout -> ser_layout_internal layout
    in
    ser_layout_internal layout

  let derive_to_json (directive : Syntax.deriving_directive)
      (decl : Syntax.type_decl) ~(params : string list) ~assertions ~diagnostics
      =
    let trait = directive.type_name_ in
    let diag =
      Derive_args.mk_diag_emitter ~host_type:decl.tycon ~trait_name:trait.name
        diagnostics
    in
    match[@warning "-fragile-match"] params with
    | self :: [] -> (
        let self = S.var self in
        let do_parse () =
          match decl.components with
          | Ptd_abstract ->
              diag "target type is abstract" trait.loc_;
              S.hole
          | Ptd_extern ->
              diag "target type is external" trait.loc_;
              S.hole
          | Ptd_newtype typ -> (
              match typ with
              | _ when type_has_hole typ -> S.hole
              | typ ->
                  S.apply_trait_method ~assertions ~typ
                    ~loc:(Syntax.loc_of_type_expression typ)
                    ~msg:
                      (("derive(ToJson) for newtype " ^ decl.tycon
                        : Stdlib.String.t)
                        [@merlin.hide])
                    trait.name "to_json"
                    [ S.newtype_field self ]
                    No_attr)
          | Ptd_error No_payload ->
              let tag = decl.tycon in
              let attrs = parse_enum_attr directive diag in
              let layout = layout_enum [ (tag, []) ] attrs in
              let layout = List.hd layout in
              serialize_layout layout ~assertions ~trait
                ~type_description:decl.tycon
          | Ptd_error (Single_payload typ) ->
              let tag = decl.tycon in
              let var = fresh_name "err_payload" in
              let attrs = parse_enum_attr directive diag in
              let layout = layout_enum [ (tag, [ None ]) ] attrs in
              let layout = List.hd layout in
              let ser_code =
                serialize_layout layout
                  ~indexed_fields:[| (S.var var, typ) |]
                  ~assertions ~trait ~type_description:decl.tycon
              in
              S.let_ (S.pconstr decl.tycon [ (None, S.pvar var) ]) self ser_code
          | Ptd_error (Enum_payload ctors) | Ptd_variant ctors ->
              let attrs = parse_enum_attr directive diag in
              let enum_layout_raw =
                Lst.map ctors (fun { constr_name; constr_args; _ } ->
                    let arg_names =
                      match constr_args with
                      | None -> []
                      | Some arg_list ->
                          Lst.map arg_list (fun { cparam_label; _ } ->
                              Option.map
                                (fun (lbl : Syntax.label) -> lbl.label_name)
                                cparam_label)
                    in
                    (constr_name.name, arg_names))
              in
              let layout = layout_enum enum_layout_raw attrs in
              S.match_ self
                (Lst.map2 ctors layout (fun { constr_name; constr_args; _ } ->
                     fun layout ->
                      let constr_name = constr_name.name in
                      let args = Option.value ~default:[] constr_args in
                      let vars = Lst.map args (fun _ -> fresh_name "arg") in
                      let args_with_types =
                        Array.of_list
                          (Lst.map2 vars args (fun name ->
                               fun arg -> (S.var name, arg.cparam_typ)))
                      in
                      let layout_code =
                        serialize_layout ~indexed_fields:args_with_types layout
                          ~assertions ~trait
                          ~type_description:
                            (Stdlib.String.concat ""
                               [
                                 "enum ";
                                 constr_name;
                                 " constructor ";
                                 decl.tycon;
                               ] [@merlin.hide])
                      in
                      ( S.pconstr constr_name
                          (Lst.map2 args vars (fun cparam ->
                               fun var ->
                                let label =
                                  Option.map
                                    (fun (l : Syntax.label) -> l.label_name)
                                    cparam.cparam_label
                                in
                                (label, S.pvar var))),
                        layout_code )))
          | Ptd_record fields ->
              let type_name = decl.tycon in
              let attrs = parse_struct_attr directive.args diag in
              let field_desc =
                Lst.map fields (fun field ->
                    (field.field_ty, field.field_name.label))
              in
              let layout = layout_struct type_name field_desc attrs in
              let named_field_types =
                StringMap.add_list
                  (Lst.map fields (fun field ->
                       ( field.field_name.label,
                         (S.field self field.field_name.label, field.field_ty)
                       )))
                  StringMap.empty
              in
              serialize_layout ~named_fields:named_field_types layout
                ~assertions ~trait ~type_description:decl.tycon
          | Ptd_alias _ -> S.hole
        in
        match Derive_args.wrap_parse do_parse with
        | Some v -> v
        | None -> S.hole)
    | _ -> assert false

  let deserialize_layout
      ?(named_fields : (Syntax.typ * Syntax.expr option) StringMap.t =
        StringMap.empty)
      ?(positional_fields : (Syntax.typ * Syntax.expr option) array = [||])
      (layout : json_layout) (input_json : Syntax.expr)
      (input_json_path : Syntax.expr)
      (construct :
        (string * Syntax.expr) list -> Syntax.expr list -> Syntax.expr)
      (error : Syntax.expr -> string -> Syntax.expr)
      ~(assertions : Syntax.static_assertion Vec.t) ~(trait : Syntax.type_name)
      ~(type_description : string) =
    (let named_field_vars =
       StringMap.mapi named_fields (fun key ->
           fun _ -> fresh_name ("de_" ^ key))
     in
     let positional_field_vars =
       Array.map (fun _ -> fresh_name "de_arg") positional_fields
     in
     let decl_vars expr =
       let expr =
         Lst.fold_left (StringMap.bindings named_field_vars) expr (fun expr ->
             fun (_, var) -> S.letmut var None (S.constr "None" []) expr)
       in
       Lst.fold_left (Array.to_list positional_field_vars) expr (fun expr ->
           fun var -> S.letmut var None (S.constr "None" []) expr)
     in
     let rec de_layout_internal (layout : json_layout) (input_v : Syntax.expr)
         input_json_path =
       match layout with
       | Value v -> de_value_source from_json_of_typ v input_v input_json_path
       | Array values -> de_array values input_v input_json_path
       | Object fields -> de_object fields input_v input_json_path
     and de_value_source de_fn (v : field_value_source) (input_v : Syntax.expr)
         input_json_path =
       match v with
       | SerializedField field_name ->
           let field_typ, _ = StringMap.find_exn named_fields field_name in
           let field_deserialized =
             de_fn
               ~msg:(("field " ^ field_name : Stdlib.String.t) [@merlin.hide])
               field_typ input_v input_json_path
           in
           let named_field_var =
             StringMap.find_exn named_field_vars field_name
           in
           S.assign named_field_var (S.constr "Some" [ field_deserialized ])
       | SerializedFieldIndex idx ->
           let field_typ, _ = positional_fields.(idx) in
           let field_deserialized =
             de_fn
               ~msg:
                 (("field index " ^ Int.to_string idx
                   : Stdlib.String.t)
                   [@merlin.hide])
               field_typ input_v input_json_path
           in
           let positional_field_var = positional_field_vars.(idx) in
           S.assign positional_field_var
             (S.constr "Some" [ field_deserialized ])
       | Constant _ -> S.unit
       | Json inner -> de_layout_internal inner input_v input_json_path
     and de_array (layout : json_layout list) (input_v : Syntax.expr)
         input_json_path =
       let vars = Lst.map layout (fun _ -> fresh_name "arg") in
       let pattern_match_array =
         Syntax.Ppat_array
           {
             pats = Closed (Lst.map vars (fun v -> S.array_pat (S.pvar v)));
             loc_ = Rloc.no_location;
           }
       in
       let array_deserialized =
         Lst.map2i layout vars (fun index ->
             fun layout ->
              fun var_name ->
               de_layout_internal layout (S.var var_name)
                 (S.json_path_add_index input_json_path index))
       in
       S.match_ input_v
         [
           (pattern_match_array, S.seq array_deserialized);
           ( S.pany,
             error input_json_path
               ("Expected array to deserialize " ^ type_description
                 : Stdlib.String.t) );
         ]
     and de_object (fields : object_layout) (input_v : Syntax.expr)
         input_json_path =
       let map = S.var "_map" in
       let pattern_match_object =
         S.pconstr "Object" [ (None, S.pvar "_map") ]
       in
       let object_deserialized =
         Lst.map fields (fun field ->
             match field with
             | Field { field_name; value_source; _ } ->
                 let original_field =
                   S.dot_apply map "get" [ S.const_string field_name ]
                 in
                 let de_field =
                   de_value_source struct_field_from_json_of_typ value_source
                     (S.var "_v")
                     (S.json_path_add_key input_json_path field_name)
                 in
                 S.match_ original_field
                   [
                     (S.pconstr "None" [], S.unit);
                     (S.pconstr "Some" [ (None, S.pvar "_v") ], de_field);
                   ])
       in
       S.match_ input_v
         [
           (pattern_match_object, S.seq object_deserialized);
           ( S.pany,
             error input_json_path
               ("Expected object to deserialize " ^ type_description
                 : Stdlib.String.t) );
         ]
     and struct_field_from_json_of_typ ~msg typ input_v input_json_path =
       match S.extract_option_inner typ with
       | Some inner ->
           let de_res = from_json_of_typ ~msg inner input_v input_json_path in
           S.constr "Some" [ de_res ]
       | None -> from_json_of_typ ~msg typ input_v input_json_path
     and from_json_of_typ ~msg typ input_v input_json_path =
       if type_has_hole typ then S.hole
       else
         S.apply_trait_method ~assertions ~typ
           ~msg:
             (Stdlib.String.concat ""
                [
                  "derive ";
                  Basic_longident.to_string trait.name;
                  " for ";
                  type_description;
                  " ";
                  msg;
                ])
           ~loc:(Syntax.loc_of_type_expression typ)
           trait.name "from_json"
           [ input_v; input_json_path ]
           Exclamation
     in
     let de_exprs = de_layout_internal layout input_json input_json_path in
     let comptime_assert_default_ty (ty : Syntax.typ) (default : Syntax.expr) =
       let loc = Syntax.loc_of_expression default in
       let res = S.annotation ~loc_:loc default ty in
       res
     in
     let named_fields_assign_default =
       Lst.map (StringMap.bindings named_field_vars) (fun (key, var) ->
           let ty, default = StringMap.find_exn named_fields key in
           let on_none =
             match default with
             | Some default -> comptime_assert_default_ty ty default
             | None when S.type_is_option ty -> S.constr "None" []
             | None ->
                 error input_json_path
                   (("Missing field " ^ key : Stdlib.String.t) [@merlin.hide])
           in
           S.match_ (S.var var)
             [
               (S.pconstr "Some" [ (None, S.pvar "v") ], S.var "v");
               (S.pconstr "None" [], on_none);
             ])
     in
     let positional_fields_assign_default =
       Lst.mapi (Array.to_list positional_field_vars) (fun i ->
           fun var ->
            let ty, default = positional_fields.(i) in
            let on_none =
              match default with
              | Some default -> comptime_assert_default_ty ty default
              | None ->
                  error input_json_path
                    (("Missing positional field " ^ Int.to_string i
                      : Stdlib.String.t)
                      [@merlin.hide])
            in
            S.match_ (S.var var)
              [
                (S.pconstr "Some" [ (None, S.pvar "v") ], S.var "v");
                (S.pconstr "None" [], on_none);
              ])
     in
     let named_fields_values =
       Lst.map (StringMap.bindings named_field_vars) (fun (key, var) ->
           (key, S.var var))
     in
     let positional_fields_values =
       Lst.map (Array.to_list positional_field_vars) S.var
     in
     let construct = construct named_fields_values positional_fields_values in
     let construct =
       Lst.fold_left2 named_fields_assign_default
         (StringMap.bindings named_field_vars) construct (fun value ->
           fun (_, orig_var_name) ->
            fun acc -> S.let_ (S.pvar orig_var_name) value acc)
     in
     let construct =
       Lst.fold_left2 positional_fields_assign_default
         (Array.to_list positional_field_vars) construct (fun value ->
           fun orig_var_name ->
            fun acc -> S.let_ (S.pvar orig_var_name) value acc)
     in
     let res = S.seq [ de_exprs; construct ] in
     decl_vars res
      : Syntax.expr)

  let default_of ~(enabled : bool) (explicit_value : Syntax.expr option)
      (typ : Syntax.typ) ~assertions ~field_name =
    (match explicit_value with
     | _ when not enabled -> None
     | Some v -> Some v
     | None ->
         Some
           (S.apply_trait_method ~assertions ~typ
              ~msg:
                (("derive(FromJson) for default value of " ^ field_name
                  : Stdlib.String.t)
                  [@merlin.hide])
              ~loc:(Syntax.loc_of_type_expression typ)
              (Basic_longident.Ldot
                 { pkg = "moonbitlang/core/builtin"; id = "Default" })
              "default" [] No_attr)
      : Syntax.expr option)

  let deserialize_enum (args : enum_layout_args)
      (ctors : (string * (Syntax.typ * string option) list) list)
      (input_json : Syntax.expr) (input_json_path : Syntax.expr)
      ~(assertions : Syntax.static_assertion Vec.t) ~(trait : Syntax.type_name)
      ~(type_name : string) =
    (let error_expr path msg =
       S.raise_json_decode_error path
         (S.infix_op "+" msg
            (S.const_string
               (" when deserializing " ^ type_name : Stdlib.String.t)))
     in
     let error path msg =
       S.raise_json_decode_error path
         (S.const_string
            (msg ^ " when derserializing " ^ type_name : Stdlib.String.t))
     in
     let with_path_add key_expr perform =
       S.let_ (S.pvar "$path")
         (S.json_path_add_key_expr input_json_path key_expr)
         (perform (S.var "$path"))
     in
     let extract_object (input_v : Syntax.expr)
         (perform : Syntax.expr -> Syntax.expr) =
       let map_name = fresh_name "map" in
       let pattern = S.pconstr "Object" [ (None, S.pvar map_name) ] in
       let deserialized = perform (S.var map_name) in
       S.match_ input_v
         [
           (pattern, deserialized);
           (S.pany, error input_json_path "Expected object");
         ]
     in
     let extract_array (input_v : Syntax.expr)
         (perform : Syntax.expr -> Syntax.expr) =
       let array_name = fresh_name "array" in
       let pattern = S.pconstr "Array" [ (None, S.pvar array_name) ] in
       let deserialized = perform (S.var array_name) in
       S.match_ input_v
         [
           (pattern, deserialized);
           (S.pany, error input_json_path "Expected array");
         ]
     in
     let extract_string (context : string) (e : Syntax.expr) =
       S.match_ e
         [
           (S.pconstr "String" [ (None, S.pvar "s") ], S.var "s");
           ( S.pany,
             error input_json_path
               ("Expected string at " ^ context : Stdlib.String.t) );
         ]
     in
     let extract_field (tag_name : string) =
       extract_object input_json (fun map ->
           let tag = S.dot_apply map "get" [ S.const_string tag_name ] in
           S.match_ tag
             [
               (S.pconstr "Some" [ (None, S.pvar "field") ], S.var "field");
               ( S.pany,
                 error input_json_path
                   ("Missing tag at field " ^ tag_name : Stdlib.String.t) );
             ])
     in
     let extract_tag_and_payload_ext_tagged_k
         (and_then : Syntax.expr -> Syntax.expr -> Syntax.expr -> Syntax.expr) =
       let result =
         extract_object input_json (fun map ->
             let size = S.dot_apply map "size" [] in
             S.match_ size
               [
                 ( Syntax.Ppat_constant
                     { c = Syntax.Const_int "0"; loc_ = Rloc.no_location },
                   error input_json_path "Missing tag in externally-tagged enum"
                 );
                 ( Syntax.Ppat_constant
                     { c = Syntax.Const_int "1"; loc_ = Rloc.no_location },
                   S.dot_apply
                     (S.dot_apply (S.dot_apply map "iter" []) "peek" [])
                     "unwrap" [] );
                 ( S.pany,
                   error input_json_path
                     "More than one key found in externally-tagged enum" );
               ])
       in
       S.let_
         (S.ptuple [ S.pvar "k"; S.pvar "v" ])
         result
         (with_path_add (S.var "k") (fun path ->
              and_then (S.var "k") (S.var "v") path))
     in
     let extract_tag_array () =
       extract_array input_json (fun input_json ->
           let tag = S.dot_apply input_json "get" [ S.const_int "0" ] in
           S.match_ tag
             [
               ( S.pconstr "Some"
                   [ (None, S.pconstr "String" [ (None, S.pvar "tag") ]) ],
                 S.var "tag" );
               ( S.pconstr "Some" [ (None, S.pany) ],
                 error input_json_path "Expected string tag at array[0]" );
               (S.pany, error input_json_path "Missing tag in array[0]");
             ])
     in
     let extract_tag_and_payload_k (kind : json_repr)
         (ctor_reprs : enum_ctor_repr)
         (and_then : Syntax.expr -> Syntax.expr -> Syntax.expr -> Syntax.expr) =
       match (kind, ctor_reprs) with
       | Tagged { tag_name }, CtorObject ->
           and_then
             (extract_string tag_name (extract_field tag_name))
             input_json input_json_path
       | Tagged _, CtorTuple ->
           and_then (extract_tag_array ()) input_json input_json_path
       | AdjTagged { tag_name; contents_name }, _ ->
           with_path_add (S.const_string contents_name) (fun path ->
               and_then
                 (extract_string tag_name (extract_field tag_name))
                 (extract_field contents_name)
                 path)
       | ExtTagged, _ -> extract_tag_and_payload_ext_tagged_k and_then
       | Untagged, _ ->
           failwith
             "Unreachable: untagged constructor should be intercepted earlier"
     in
     let layout_ctor_payload name fields args =
       match args.repr_ctors with
       | CtorObject -> Object (layout_enum_payload_object fields args)
       | CtorTuple -> Array (layout_enum_payload_tuple name fields args)
     in
     let positional_fields_desc variant_desc variant_args =
       let default_enabled_for_type = args.default in
       Lst.mapi variant_desc (fun i ->
           fun (ty, name) ->
            let positional_layout =
              IntMap.find_default variant_args.positional_field_layout i
                default_field_layout_args
            in
            let default_enabled_positional = positional_layout.default in
            let default_expr_positional = positional_layout.default_expr in
            let named_layout =
              match name with
              | None -> default_field_layout_args
              | Some name ->
                  StringMap.find_default variant_args.named_field_layout name
                    default_field_layout_args
            in
            let default_enabled_named = named_layout.default in
            let default_expr_named = named_layout.default_expr in
            let default_enabled =
              default_enabled_for_type || default_enabled_positional
              || default_enabled_named
            in
            let default_value =
              match (default_expr_positional, default_expr_named) with
              | Some expr, None -> Some expr
              | None, Some expr -> Some expr
              | Some _, Some _ ->
                  failwith
                    "TODO: default_expr_positional and default_expr_named \
                     should not both be set"
              | None, None -> None
            in
            let default_value =
              default_of ~enabled:default_enabled default_value ty ~assertions
                ~field_name:
                  (match name with
                  | None ->
                      ("index " ^ Int.to_string i
                        : Stdlib.String.t)
                        [@merlin.hide]
                  | Some name -> name)
            in
            (ty, default_value))
     in
     let deserialize_tagged () =
       extract_tag_and_payload_k args.repr args.repr_ctors (fun tag ->
           fun payload ->
            fun payload_path ->
             let ctor_cases =
               Lst.map ctors (fun (ctor_name, ctor_desc) ->
                   let ctor_tag = enum_ctor_renamed args ctor_name in
                   let payload_layout =
                     layout_ctor_payload ctor_tag (Lst.map ctor_desc snd) args
                   in
                   let ctor_args =
                     StringMap.find_default args.ctor_layout_args ctor_name
                       default_ctor_layout_args
                   in
                   let ctor_deserialized =
                     deserialize_layout
                       ~positional_fields:
                         (Array.of_list
                            (positional_fields_desc ctor_desc ctor_args))
                       payload_layout payload payload_path
                       (fun _named ->
                         fun positional_fields ->
                          if _named <> [] then
                            failwith
                              "Unreachable: Tagged enum deserialize \
                               constructor should not have named fields";
                          S.constr_label ctor_name
                            (Lst.map2 positional_fields ctor_desc (fun value ->
                                 fun desc ->
                                  let label = Option.map S.label (snd desc) in
                                  (value, label))))
                       (fun path -> fun msg -> error path msg)
                       ~assertions ~trait
                       ~type_description:
                         (Stdlib.String.concat ""
                            [ "enum "; type_name; " constructor "; ctor_name ])
                   in
                   ( S.pconstant
                       (Syntax.Const_string
                          { string_val = ctor_tag; string_repr = ctor_tag }),
                     ctor_deserialized ))
             in
             let err_case =
               error_expr input_json_path
                 (S.infix_op "+"
                    (S.const_string "Unknown enum constructor ")
                    (S.var "tag"))
             in
             S.match_ tag (ctor_cases @ [ (S.pvar "tag", err_case) ]))
     in
     let deserialize_untagged () =
       let ctor_deserializers =
         Lst.map ctors (fun (ctor_name, ctor_desc) ->
             let ctor_tag = enum_ctor_renamed args ctor_name in
             let ctror_layout =
               layout_ctor_payload ctor_tag (Lst.map ctor_desc snd) args
             in
             let ctor_deserialized =
               deserialize_layout
                 ~positional_fields:
                   (Array.of_list
                      (Lst.map ctor_desc (fun (ty, _) -> (ty, None))))
                 ctror_layout input_json input_json_path
                 (fun _named ->
                   fun positional_fields ->
                    if _named <> [] then
                      failwith
                        "Unreachable: Untagged enum deserialize constructor \
                         should not have named fields";
                    S.constr_label ctor_tag
                      (Lst.map2 positional_fields ctor_desc (fun value ->
                           fun desc ->
                            let label = Option.map S.label (snd desc) in
                            (value, label))))
                 (fun path -> fun msg -> error path msg)
                 ~assertions ~trait
                 ~type_description:
                   (Stdlib.String.concat ""
                      [ "enum "; type_name; " constructor "; ctor_tag ])
             in
             Syntax.Pexpr_try
               {
                 body =
                   Syntax.Pexpr_return
                     {
                       return_value = Some ctor_deserialized;
                       loc_ = Rloc.no_location;
                     };
                 catch = [ { pattern = S.pany; guard = None; body = S.unit } ];
                 catch_all = true;
                 try_else = None;
                 try_loc_ = Rloc.no_location;
                 catch_loc_ = Rloc.no_location;
                 else_loc_ = Rloc.no_location;
                 loc_ = Rloc.no_location;
               })
       in
       let err_case =
         error input_json_path
           ("Unable to find deserializable constructor of enum " ^ type_name
             : Stdlib.String.t)
       in
       S.sequence (S.seq ctor_deserializers) err_case
     in
     match args.repr with
     | Untagged -> deserialize_untagged ()
     | Tagged _ | AdjTagged _ | ExtTagged -> deserialize_tagged ()
      : Syntax.expr)

  let derive_from_json (directive : Syntax.deriving_directive)
      (decl : Syntax.type_decl) ~(params : string list) ~assertions ~diagnostics
      =
    match[@warning "-fragile-match"] params with
    | [ json; path ] -> (
        let json, path = (S.var json, S.var path) in
        let trait = directive.type_name_ in
        let diag =
          Derive_args.mk_diag_emitter ~host_type:decl.tycon
            ~trait_name:trait.name diagnostics
        in
        let do_parse () =
          match decl.components with
          | Ptd_abstract ->
              Local_diagnostics.add_error diagnostics
                (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
                   ~reason:"target type is abstract" ~loc:trait.loc_);
              S.hole
          | Ptd_extern ->
              Local_diagnostics.add_error diagnostics
                (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
                   ~reason:"target type is external" ~loc:trait.loc_);
              S.hole
          | Ptd_newtype typ ->
              if type_has_hole typ then S.hole
              else
                S.constr decl.tycon
                  [
                    S.apply_trait_method ~assertions ~typ
                      ~msg:
                        (("derive(FromJson) for newtype " ^ decl.tycon
                          : Stdlib.String.t)
                          [@merlin.hide])
                      ~loc:(Syntax.loc_of_type_expression typ)
                      trait.name "from_json" [ json; path ] Exclamation;
                  ]
          | Ptd_error (Single_payload typ) ->
              let attrs = parse_enum_attr directive diag in
              deserialize_enum attrs
                [ (decl.tycon, [ (typ, None) ]) ]
                json path ~assertions ~trait ~type_name:decl.tycon
          | Ptd_error No_payload ->
              let attrs = parse_enum_attr directive diag in
              deserialize_enum attrs
                [ (decl.tycon, []) ]
                json path ~assertions ~trait ~type_name:decl.tycon
          | Ptd_error (Enum_payload ctors) | Ptd_variant ctors ->
              let attrs = parse_enum_attr directive diag in
              let enum_layout_raw =
                Lst.map ctors (fun { constr_name; constr_args; _ } ->
                    let arg_names =
                      match constr_args with
                      | None -> []
                      | Some arg_list ->
                          Lst.map arg_list
                            (fun { cparam_label; cparam_typ; _ } ->
                              ( cparam_typ,
                                Option.map
                                  (fun (lbl : Syntax.label) -> lbl.label_name)
                                  cparam_label ))
                    in
                    (constr_name.name, arg_names))
              in
              deserialize_enum attrs enum_layout_raw json path ~assertions
                ~trait ~type_name:decl.tycon
          | Ptd_record fields ->
              let attrs = parse_struct_attr directive.args diag in
              let struct_layout =
                layout_struct decl.tycon
                  (Lst.map fields (fun field ->
                       (field.field_ty, field.field_name.label)))
                  attrs
              in
              let type_enable_default = attrs.default in
              let fields_info =
                Lst.map fields (fun field ->
                    let field_default =
                      StringMap.find_default attrs.field_layout
                        field.field_name.label default_field_layout_args
                    in
                    let default_enabled =
                      field_default.default || type_enable_default
                    in
                    let default_expr = field_default.default_expr in
                    let default_value =
                      default_of ~enabled:default_enabled default_expr
                        field.field_ty ~assertions
                        ~field_name:field.field_name.label
                    in
                    (field.field_name.label, (field.field_ty, default_value)))
              in
              deserialize_layout
                ~named_fields:(StringMap.of_list fields_info)
                struct_layout json path
                (fun named_fields ->
                  fun _positional ->
                   if _positional <> [] then
                     failwith
                       "Unreachable: Record deserialize constructor should not \
                        have positional fields";
                   S.record
                     (Lst.map fields (fun field ->
                          let field_name = field.field_name.label in
                          let field_expr =
                            Lst.assoc_str_exn named_fields field_name
                          in
                          (field_name, field_expr))))
                (fun path ->
                  fun msg -> S.raise_json_decode_error path (S.const_string msg))
                ~assertions ~trait ~type_description:decl.tycon
          | Ptd_alias _ -> S.hole
        in
        match Derive_args.wrap_parse do_parse with
        | Some v -> v
        | None -> S.hole)
    | _ -> assert false
end

let derive_to_json = DeriveJson.derive_to_json
let derive_from_json = DeriveJson.derive_from_json

let derive_arbitrary (trait : Syntax.type_name) (decl : Syntax.type_decl)
    ~(params : string list) ~assertions ~diagnostics =
  match[@warning "-fragile-match"] params with
  | [ size; rng ] -> (
      let size, rng = (S.var size, S.var rng) in
      let rec arbitrary_of_typ ~msg (typ : Syntax.typ) =
        if type_has_hole typ then S.hole
        else
          match typ with
          | Ptype_tuple { tys; _ } ->
              S.tuple (Lst.map tys (arbitrary_of_typ ~msg))
          | typ ->
              S.apply_trait_method ~assertions ~typ ~msg
                ~loc:(Syntax.loc_of_type_expression typ)
                trait.name "arbitrary" [ size; rng ] No_attr
      in
      match decl.components with
      | Ptd_abstract ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is abstract" ~loc:trait.loc_);
          S.hole
      | Ptd_extern ->
          Local_diagnostics.add_error diagnostics
            (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
               ~reason:"target type is external" ~loc:trait.loc_);
          S.hole
      | Ptd_newtype typ ->
          S.constr decl.tycon
            [
              arbitrary_of_typ
                ~msg:
                  (("derive(Arbitrary) for newtype " ^ decl.tycon
                    : Stdlib.String.t)
                    [@merlin.hide])
                typ;
            ]
      | Ptd_error No_payload -> S.constr decl.tycon []
      | Ptd_error (Single_payload typ) ->
          S.constr decl.tycon
            [
              arbitrary_of_typ
                ~msg:
                  (("derive(Arbitrary) for error type " ^ decl.tycon
                    : Stdlib.String.t)
                    [@merlin.hide])
                typ;
            ]
      | Ptd_error (Enum_payload constrs) | Ptd_variant constrs -> (
          match constrs with
          | [] ->
              Local_diagnostics.add_error diagnostics
                (Errors.cannot_derive ~tycon:decl.tycon ~trait:trait.name
                   ~reason:"cannot find a constant constructor" ~loc:trait.loc_);
              S.hole
          | constr :: [] ->
              let constr_name = constr.constr_name.name in
              let args = Option.value ~default:[] constr.constr_args in
              S.constr_label constr_name
                (Lst.map args (fun arg ->
                     ( arbitrary_of_typ
                         ~msg:
                           (Stdlib.String.concat ""
                              [
                                "derive(Arbitrary) for constructor ";
                                constr_name;
                                " of type ";
                                decl.tycon;
                              ] [@merlin.hide])
                         arg.cparam_typ,
                       arg.cparam_label )))
          | multi ->
              let exp =
                S.infix_op "%"
                  (S.infix_op "+"
                     (S.dot_apply rng "next_uint" [])
                     (S.const_uint "1"))
                  (S.const_uint (string_of_int (List.length multi)))
              in
              (fun cases ->
                let cases = cases @ [ (S.pany, S.panic) ] in
                S.match_ exp cases)
                (Lst.mapi multi (fun i ->
                     fun { constr_name; constr_args; _ } ->
                      let constr_name = constr_name.name in
                      let args = Option.value ~default:[] constr_args in
                      let action =
                        S.constr_label constr_name
                          (Lst.map args (fun arg ->
                               ( arbitrary_of_typ
                                   ~msg:
                                     (Stdlib.String.concat ""
                                        [
                                          "derive(Arbitrary) for constructor ";
                                          constr_name;
                                          " of type ";
                                          decl.tycon;
                                        ] [@merlin.hide])
                                   arg.cparam_typ,
                                 arg.cparam_label )))
                      in
                      (S.const_uint_pat (string_of_int i), action))))
      | Ptd_record fields ->
          S.record
            (Lst.map fields (fun field ->
                 let field_name = field.field_name.label in
                 ( field_name,
                   arbitrary_of_typ
                     ~msg:
                       (Stdlib.String.concat ""
                          [
                            "derive(Arbitrary) for field ";
                            field_name;
                            " of type ";
                            decl.tycon;
                          ] [@merlin.hide])
                     field.field_ty )))
      | Ptd_alias _ -> S.hole)
  | _ -> assert false

let deny_all_derive_args_then f (directive : Syntax.deriving_directive)
    (decl : Syntax.type_decl) ~(params : string list) ~assertions ~diagnostics =
  let trait = directive.type_name_ in
  if
    Derive_args.deny_all_args ~host_type:decl.tycon ~trait_name:trait.name
      diagnostics directive
  then S.hole
  else f directive.type_name_ decl ~params ~assertions ~diagnostics

let derivers : (string * deriver) list Type_path.Hash.t =
  Type_path.Hash.of_list
    [
      ( Type_path.Builtin.trait_default,
        [ ("default", deny_all_derive_args_then derive_default) ] );
      ( Type_path.Builtin.trait_eq,
        [
          ( Operators.op_equal_info.method_name,
            deny_all_derive_args_then derive_eq );
        ] );
      ( Type_path.Builtin.trait_compare,
        [ ("compare", deny_all_derive_args_then derive_compare) ] );
      ( Type_path.Builtin.trait_show,
        [ ("output", deny_all_derive_args_then derive_show) ] );
      ( Type_path.Builtin.trait_hash,
        [ ("hash_combine", deny_all_derive_args_then derive_hash) ] );
      ( Type_path.toplevel_type ~pkg:"moonbitlang/core/json" "FromJson",
        [ ("from_json", derive_from_json) ] );
      ( Type_path.toplevel_type ~pkg:Basic_config.builtin_package "ToJson",
        [ ("to_json", derive_to_json) ] );
      ( Type_path.toplevel_type ~pkg:"moonbitlang/core/quickcheck" "Arbitrary",
        [ ("arbitrary", deny_all_derive_args_then derive_arbitrary) ] );
    ]
