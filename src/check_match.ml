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


module Vec = Basic_vec
module Lst = Basic_lst
module Path = Pat_path
module Db = Patmatch_db

let mark_reachable = Pattern_id.mark_reachable
let is_unreachable = Pattern_id.is_unreachable

let report_unreachable ~diagnostics ({ action; _ } : Typedtree.match_case) =
  Local_diagnostics.add_warning diagnostics
    { kind = Unreachable; loc = Typedtree.loc_of_typed_expr action }

let report_unused_pat ~diagnostics pat =
  Local_diagnostics.add_warning diagnostics
    { kind = Unused_pat; loc = Typedtree.loc_of_pat pat }

let lint_cases ~diagnostics ~book ~cases_reachability
    (cases : Typedtree.match_case list) =
  (let rec visit_cases (cases : Typedtree.match_case list) index =
     match cases with
     | [] -> ()
     | case :: cases ->
         if cases_reachability.(index) then
           let pat_id = [ index ] in
           visit_pat case.pat pat_id
         else report_unreachable ~diagnostics case;
         visit_cases cases (index + 1)
   and visit_pat pat pat_id =
     if is_unreachable book pat_id then report_unused_pat ~diagnostics pat
     else
       match pat with
       | Tpat_any _ | Tpat_var _ | Tpat_constant _ -> ()
       | Tpat_alias { pat; _ } | Tpat_constraint { pat; _ } ->
           visit_pat pat (0 :: pat_id)
       | Tpat_range _ -> ()
       | Tpat_or { pat1; pat2; _ } ->
           visit_pat pat1 (0 :: pat_id);
           visit_pat pat2 (1 :: pat_id)
       | Tpat_tuple { pats; _ } ->
           Lst.iteri pats ~f:(fun i -> fun pat -> visit_pat pat (i :: pat_id))
       | Tpat_record { fields; _ } ->
           Lst.iteri fields ~f:(fun i ->
               fun (Field_pat { pat; _ }) -> visit_pat pat (i :: pat_id))
       | Tpat_constr { args = []; _ } -> ()
       | Tpat_constr { args; _ } ->
           Lst.iteri args ~f:(fun arg_index ->
               fun (Constr_pat_arg { pat; _ }) ->
                visit_pat pat (arg_index :: pat_id))
       | Tpat_array { pats; _ } -> (
           match pats with
           | Closed pats ->
               Lst.iteri pats ~f:(fun i ->
                   fun pat -> visit_pat pat (i :: pat_id))
           | Open (pats1, pats2, _dotdot_binder) ->
               let len1 = List.length pats1 in
               Lst.iteri pats1 ~f:(fun i ->
                   fun pat -> visit_pat pat (i :: pat_id));
               Lst.iteri pats2 ~f:(fun i ->
                   fun pat -> visit_pat pat ((i + len1) :: pat_id)))
       | Tpat_map { elems; _ } ->
           Lst.iteri elems ~f:(fun i ->
               fun (_, pat) -> visit_pat pat (i :: pat_id))
   in
   visit_cases cases 0
    : unit)

let try_combine_array_pat_to_string (pat : Typedtree.array_pattern) =
  (let exception Not_all_char_literal in
   try
     match pat with
     | Closed pats ->
         let vec = Basic_vec_int.empty () in
         Lst.iter pats ~f:(fun p ->
             match p with
             | Tpat_constant { c = C_char c; _ } ->
                 Basic_vec_int.push vec (Uchar.to_int c)
             | _ -> raise_notrace Not_all_char_literal);
         let str = Basic_utf8.string_of_vec vec ~offset:0 ~len:vec.len in
         Some str
     | Open _ -> raise_notrace Not_all_char_literal
   with Not_all_char_literal -> None
    : string option)

let analyze_cases (ty : Stype.t) (cases : Typedtree.match_case list)
    (genv : Global_env.t) ~(catch_all_loc : Rloc.t option)
    ~(only_check_unused_pat : bool) =
  let book = Pattern_id.create () in
  let cases_reachability = Array.make (List.length cases) false in
  let missing_cases = Vec.empty () in
  let dispatch ~db ~ok_cont ~fail_cont (r : Db.static_eval_result) ~pat_id =
    match r with
    | For_sure_yes ->
        mark_reachable book pat_id;
        ok_cont db
    | For_sure_no -> fail_cont db
    | Uncertain r ->
        mark_reachable book pat_id;
        ok_cont r.ok_db;
        fail_cont r.fail_db
      [@@inline]
  in
  let rec process_pat ~(db : Db.t) ~(path : Path.t) (pat : Typedtree.pat)
      ~(pat_id : Pattern_id.key) ~(ok_cont : Db.t -> unit)
      ~(fail_cont : Db.t -> unit) =
    match (pat : Typedtree.pat) with
    | Tpat_any _ | Tpat_var _ ->
        mark_reachable book pat_id;
        ok_cont db
    | Tpat_alias { pat; _ } | Tpat_constraint { pat; _ } ->
        mark_reachable book pat_id;
        process_pat ~db ~path pat ~pat_id:(0 :: pat_id) ~ok_cont ~fail_cont
    | Tpat_or { pat1; pat2; _ } ->
        mark_reachable book pat_id;
        process_pat ~db ~path pat1 ~pat_id:(0 :: pat_id) ~ok_cont
          ~fail_cont:(fun db ->
            process_pat ~db ~path pat2 ~pat_id:(1 :: pat_id) ~ok_cont ~fail_cont)
    | Tpat_constant { c; _ } ->
        dispatch ~ok_cont ~fail_cont ~pat_id ~db
          (match c with
          | C_string str ->
              Db.eval_string_constant db path str ~check_match:true
          | _ -> Db.eval_constant db path c)
    | Tpat_range { lhs; rhs; inclusive; ty = _ } ->
        let lo =
          match lhs with
          | Tpat_constant { c; _ } -> Some c
          | Tpat_any _ -> None
          | _ -> assert false
        in
        let hi =
          match rhs with
          | Tpat_constant { c; _ } -> Some c
          | Tpat_any _ -> None
          | _ -> assert false
        in
        dispatch ~ok_cont ~fail_cont ~pat_id ~db
          (Db.eval_range db path lo hi ~inclusive)
    | Tpat_tuple { pats; _ } ->
        mark_reachable book pat_id;
        let rec go db pats index =
          match pats with
          | [] -> ok_cont db
          | pat :: pats ->
              process_pat ~db ~path:(Field index :: path) pat
                ~pat_id:(index :: pat_id)
                ~ok_cont:(fun db -> go db pats (index + 1))
                ~fail_cont
        in
        go db pats 0
    | Tpat_record { fields; _ } ->
        mark_reachable book pat_id;
        let rec go db fields index =
          match fields with
          | [] -> ok_cont db
          | Typedtree.Field_pat { pat; pos; _ } :: pats ->
              process_pat ~db ~path:(Field pos :: path) pat
                ~pat_id:(index :: pat_id)
                ~ok_cont:(fun db -> go db pats (index + 1))
                ~fail_cont
        in
        go db fields 0
    | Tpat_constr
        { tag; args; used_error_subtyping; type_name = _; constr = _; ty = _ }
      ->
        let arg_path arg_index =
          (match tag with
           | Constr_tag_regular { index; total = _ } ->
               Constr_field { tag_index = index; arg_index } :: path
           | Extensible_tag ext_tag ->
               if used_error_subtyping then
                 Error_constr_field { tag; arg_index } :: path
               else
                 Constr_field { tag_index = ext_tag.index; arg_index } :: path
            : Path.t)
        in
        let ok_cont db =
          let rec loop db arg_index (args : Typedtree.constr_pat_arg list) =
            match args with
            | [] -> ok_cont db
            | Constr_pat_arg { pat; kind = _; pos } :: args ->
                process_pat ~db ~path:(arg_path pos) pat
                  ~pat_id:(arg_index :: pat_id)
                  ~ok_cont:(fun db -> loop db (arg_index + 1) args)
                  ~fail_cont
          in
          loop db 0 args
        in
        dispatch ~ok_cont ~fail_cont ~pat_id ~db
          (Db.eval_constructor db path tag ~used_error_subtyping)
    | Tpat_array { pats; _ } -> (
        let has_dotdot, pats_with_info, num_pats =
          match pats with
          | Closed pats ->
              ( false,
                Lst.mapi pats (fun i -> fun pat -> (pat, Path.Field i, i)),
                List.length pats )
          | Open (pats1, pats2, _dotdot_binder) ->
              let len1 = List.length pats1 in
              let len2 = List.length pats2 in
              let pats2 =
                Lst.mapi pats2 (fun i ->
                    fun pat -> (pat, Path.Last_field (len2 - 1 - i), i + len1))
              in
              ( true,
                Lst.mapi_append pats1
                  (fun i -> fun pat -> (pat, Path.Field i, i))
                  pats2,
                len1 + len2 )
        in
        let ok_cont db =
          let rec go db pats =
            match pats with
            | [] -> ok_cont db
            | (pat, field, index) :: pats ->
                process_pat ~db ~path:(field :: path) pat
                  ~pat_id:(index :: pat_id)
                  ~ok_cont:(fun db -> go db pats)
                  ~fail_cont
          in
          go db pats_with_info
        in
        let default () =
          let r =
            if has_dotdot then Db.eval_geq_array_len db path num_pats
            else Db.eval_eq_array_len db path num_pats
          in
          dispatch r ~db ~ok_cont ~fail_cont ~pat_id
            [@@inline]
        in
        let default_string () =
          let r =
            if has_dotdot then Db.eval_geq_array_len_string db path num_pats
            else Db.eval_eq_array_len_string db path num_pats
          in
          dispatch r ~db ~ok_cont ~fail_cont ~pat_id
            [@@inline]
        in
        match Stype.type_repr (Typedtree_util.type_of_pat pat) with
        | T_builtin T_string -> (
            match try_combine_array_pat_to_string pats with
            | Some str -> (
                match Db.eval_string_constant db path str ~check_match:true with
                | (For_sure_no | For_sure_yes) as r ->
                    dispatch ~ok_cont ~fail_cont ~pat_id ~db r
                | Uncertain _ -> default_string ())
            | None -> default_string ())
        | _ -> default ())
    | Tpat_map { elems; op_get_info_ = _, op_get_ty, _; ty = _ } -> (
        mark_reachable book pat_id;
        match[@warning "-fragile-match"] op_get_ty with
        | Tarrow { ret_ty = ty_op_get_result; _ } ->
            let rec go pat_index db elems =
              match elems with
              | [] -> ok_cont db
              | (key, pat) :: elems ->
                  process_pat pat
                    ~db:(Db.eval_map_elem db path key ~elem_ty:ty_op_get_result)
                    ~path:(Map_elem { key } :: path)
                    ~pat_id:(pat_index :: pat_id)
                    ~ok_cont:(fun db -> go (pat_index + 1) db elems)
                    ~fail_cont
            in
            go 0 db elems
        | _ -> assert false)
  in
  let rec process_cases (db : Db.t) (cases : Typedtree.match_case list) index =
    match cases with
    | [] -> Vec.push missing_cases db
    | { pat; guard = None; _ } :: cases ->
        let pat_id = [ index ] in
        let ok_cont _db = cases_reachability.(index) <- true in
        let fail_cont (db : Db.t) = process_cases db cases (index + 1) in
        process_pat ~db ~path:[] pat ~pat_id ~ok_cont ~fail_cont
    | { pat; guard = Some _; _ } :: cases ->
        let pat_id = [ index ] in
        process_pat ~db ~path:[] pat ~pat_id
          ~ok_cont:(fun _ -> cases_reachability.(index) <- true)
          ~fail_cont:(fun _ -> ());
        process_cases db cases (index + 1)
  in
  process_cases Db.empty cases 0;
  let patterns = Basic_vec.empty () in
  (if
     Vec.length missing_cases > 0
     && catch_all_loc = None && not only_check_unused_pat
   then
     let empty_match = cases = [] in
     Vec.iter missing_cases (fun db ->
         let ps =
           Patmatch_static_info.synthesize_missing_case_pattern db ~genv
             ~empty_match ty
         in
         Basic_lst.iter ps ~f:(fun p -> Vec.push patterns p)));
  (book, cases_reachability, patterns, Vec.length missing_cases = 0)

let get_missing_case_patterns (ty : Stype.t) (cases : Typedtree.match_case list)
    (genv : Global_env.t) =
  let _, _, missing_cases, _ =
    analyze_cases ty cases genv ~catch_all_loc:None ~only_check_unused_pat:false
  in
  missing_cases

let check_match ~(diagnostics : Local_diagnostics.t) (ty : Stype.t)
    (cases : Typedtree.match_case list) (genv : Global_env.t)
    ~(catch_all_loc : Rloc.t option) ~(is_let_match : bool)
    ~(only_check_unused_pat : bool) (loc : Rloc.t) =
  let book, cases_reachability, missing_case_patterns, missing_case_db_is_empty
      =
    analyze_cases ty cases genv ~catch_all_loc ~only_check_unused_pat
  in
  (if Vec.length missing_case_patterns > 0 && not only_check_unused_pat then
     let has_guard = Lst.exists cases (fun { guard; _ } -> guard <> None) in
     Local_diagnostics.add_warning diagnostics
       {
         kind = Partial_match { hint_cases = [ "" ]; is_let_match; has_guard };
         loc;
       });
  (match catch_all_loc with
  | None -> ()
  | Some loc ->
      if missing_case_db_is_empty then
        Local_diagnostics.add_warning diagnostics
          { kind = Useless_catch_all; loc });
  lint_cases ~diagnostics ~book ~cases_reachability cases

let rec irrefutable_pattern (pat : Typedtree.pat) =
  match pat with
  | Tpat_any _ | Tpat_var _ -> true
  | Tpat_alias { pat; _ } | Tpat_constraint { pat; _ } ->
      irrefutable_pattern pat
  | Tpat_record { fields; _ } ->
      List.for_all
        (fun (Typedtree.Field_pat { pat; _ }) -> irrefutable_pattern pat)
        fields
  | Tpat_tuple { pats; _ } -> List.for_all irrefutable_pattern pats
  | Tpat_or _ | Tpat_constr _ | Tpat_array _ | Tpat_constant _ | Tpat_map _
  | Tpat_range _ ->
      false

let obj =
  object
    inherit [_] Typedtree.iter as super

    method! visit_Texpr_guard_let ((diagnostics, genv) as _ctx) pat rhs
        pat_binders otherwise body ty loc_ =
      super#visit_Texpr_guard_let _ctx pat rhs pat_binders otherwise body ty
        loc_;
      check_match ~diagnostics ~catch_all_loc:None
        (Typedtree_util.type_of_typed_expr rhs)
        [
          {
            pat;
            action = Texpr_unit { loc_ = Rloc.no_location };
            pat_binders;
            guard = None;
          };
        ]
        genv Rloc.no_location ~is_let_match:false ~only_check_unused_pat:true;
      match otherwise with
      | None -> ()
      | Some else_cases ->
          let guard_case =
            { Typedtree.pat; action = body; pat_binders; guard = None }
          in
          let match_loc_ =
            Rloc.of_pos
              ( Rloc.get_start loc_,
                Rloc.get_end (Typedtree.loc_of_typed_expr rhs) )
          in
          check_match ~diagnostics ~catch_all_loc:None
            (Typedtree_util.type_of_typed_expr rhs)
            (guard_case :: else_cases) genv match_loc_ ~is_let_match:false
            ~only_check_unused_pat:false

    method! visit_Texpr_match ((diagnostics, genv) as _ctx) expr cases typ
        match_loc loc =
      super#visit_Texpr_match _ctx expr cases typ match_loc loc;
      check_match ~diagnostics ~catch_all_loc:None
        (Typedtree_util.type_of_typed_expr expr)
        cases genv match_loc ~is_let_match:false ~only_check_unused_pat:false

    method! visit_Texpr_try ((diagnostics, genv) as ctx) body catch catch_all
        try_else ty err_ty catch_loc else_loc loc =
      super#visit_Texpr_try ctx body catch catch_all try_else ty err_ty
        catch_loc else_loc loc;
      check_match ~diagnostics
        ~catch_all_loc:(if catch_all then Some catch_loc else None)
        err_ty catch genv catch_loc ~is_let_match:false
        ~only_check_unused_pat:false;
      match try_else with
      | None -> ()
      | Some try_else ->
          check_match ~diagnostics ~catch_all_loc:None
            (Typedtree_util.type_of_typed_expr body)
            try_else genv else_loc ~is_let_match:false
            ~only_check_unused_pat:false

    method! visit_Texpr_let ((diagnostics, genv) as ctx) (pat : Typedtree.pat)
        rhs pat_binders body ty loc =
      super#visit_Texpr_let ctx pat rhs pat_binders body ty loc;
      if not (irrefutable_pattern pat) then
        check_match ~diagnostics ~catch_all_loc:None
          (Typedtree_util.type_of_typed_expr rhs)
          [ { pat; action = body; pat_binders; guard = None } ]
          genv (Typedtree.loc_of_pat pat) ~is_let_match:true
          ~only_check_unused_pat:false

    method! visit_Texpr_is ((diagnostics, genv) as ctx) expr pat pat_binders loc
        =
      super#visit_Texpr_is ctx expr pat pat_binders loc;
      check_match ~diagnostics ~catch_all_loc:None
        (Typedtree_util.type_of_typed_expr expr)
        [
          {
            pat;
            action = Texpr_unit { loc_ = Rloc.no_location };
            pat_binders;
            guard = None;
          };
        ]
        genv loc ~is_let_match:false ~only_check_unused_pat:true
  end

let analyze ~diagnostics (genv, output) =
  let (Output { value_defs; _ } : Typedtree.output) = output in
  Lst.iter value_defs ~f:(fun impl ->
      let base = Typedtree.loc_of_impl impl in
      let local_diagnostics = Local_diagnostics.make ~base in
      obj#visit_impl (local_diagnostics, genv) impl;
      Local_diagnostics.add_to_global local_diagnostics diagnostics)
