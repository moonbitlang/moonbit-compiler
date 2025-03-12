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


module Path = Pat_path
module StaticInfo = Patmatch_static_info
module Lst = Basic_lst

type t = StaticInfo.t Path.Map.t

type 'a eval_result = 'a Basic_case_set_intf.static_matching_result =
  | For_sure_yes
  | For_sure_no
  | Uncertain of { ok_db : 'a; fail_db : 'a }

type static_eval_result = t eval_result

let empty : t = Path.Map.empty

let make_result (self : t) (path : Path.t)
    ~info:(matched : StaticInfo.t eval_result) =
  (match matched with
   | For_sure_yes -> For_sure_yes
   | For_sure_no -> For_sure_no
   | Uncertain { ok_db; fail_db } ->
       Uncertain
         {
           ok_db = Path.Map.add self path ok_db;
           fail_db = Path.Map.add self path fail_db;
         }
    : static_eval_result)

let eval_range self path lo hi ~inclusive =
  (make_result self path
     ~info:
       (StaticInfo.eval_range (Path.Map.find_opt self path) lo hi ~inclusive)
    : static_eval_result)

let eval_constructor self path constr_tag ~used_error_subtyping =
  (make_result self path
     ~info:
       (StaticInfo.eval_constructor
          (Path.Map.find_opt self path)
          constr_tag ~used_error_subtyping)
    : static_eval_result)

let eval_eq_array_len self path len =
  (make_result self path
     ~info:(StaticInfo.eval_eq_array_len (Path.Map.find_opt self path) len)
    : static_eval_result)

let eval_geq_array_len self path len =
  (make_result self path
     ~info:(StaticInfo.eval_geq_array_len (Path.Map.find_opt self path) len)
    : static_eval_result)

let eval_eq_array_len_string self path len =
  (make_result self path
     ~info:
       (StaticInfo.eval_eq_array_len_string (Path.Map.find_opt self path) len)
    : static_eval_result)

let eval_geq_array_len_string self path len =
  (make_result self path
     ~info:
       (StaticInfo.eval_geq_array_len_string (Path.Map.find_opt self path) len)
    : static_eval_result)

let eval_map_elem self path key ~elem_ty =
  (Path.Map.add self path
     (StaticInfo.eval_map_elem (Path.Map.find_opt self path) key ~elem_ty)
    : t)

type partial_result = For_sure_no | Uncertain

let eval_entire_array_pat (self : t) (path : Pat_path.t)
    (pats : (Typedtree.pat * Pat_path.access) list) (op : [ `Ge | `Eq ])
    (num_pats : int) =
  (let exception For_sure_no in
   try
     (match op with
     | `Ge -> (
         match
           StaticInfo.eval_geq_array_len (Path.Map.find_opt self path) num_pats
         with
         | For_sure_no -> raise_notrace For_sure_no
         | Uncertain _ | For_sure_yes -> ())
     | `Eq -> (
         match
           StaticInfo.eval_eq_array_len (Path.Map.find_opt self path) num_pats
         with
         | For_sure_no -> raise_notrace For_sure_no
         | Uncertain _ | For_sure_yes -> ()));
     Lst.iter pats ~f:(fun (pat, pat_path) ->
         match pat with
         | Tpat_constant { c; _ } -> (
             let path = pat_path :: path in
             match StaticInfo.eval_constant (Path.Map.find_opt self path) c with
             | For_sure_no -> raise_notrace For_sure_no
             | Uncertain _ | For_sure_yes -> ())
         | _ -> ());
     Uncertain
   with For_sure_no -> For_sure_no
    : partial_result)

let eval_entire_array_pat_string (self : t) (path : Pat_path.t)
    (pats : (Typedtree.pat * Pat_path.access) list) (op : [ `Ge | `Eq ])
    (num_pats : int) =
  (let exception For_sure_no in
   try
     (match op with
     | `Ge -> (
         match
           StaticInfo.eval_geq_array_len_string
             (Path.Map.find_opt self path)
             num_pats
         with
         | For_sure_no -> raise_notrace For_sure_no
         | Uncertain _ | For_sure_yes -> ())
     | `Eq -> (
         match
           StaticInfo.eval_eq_array_len_string
             (Path.Map.find_opt self path)
             num_pats
         with
         | For_sure_no -> raise_notrace For_sure_no
         | Uncertain _ | For_sure_yes -> ()));
     Lst.iter pats ~f:(fun (pat, pat_path) ->
         match pat with
         | Tpat_constant { c; _ } -> (
             let path = pat_path :: path in
             match StaticInfo.eval_constant (Path.Map.find_opt self path) c with
             | For_sure_no -> raise_notrace For_sure_no
             | Uncertain _ | For_sure_yes -> ())
         | _ -> ());
     Uncertain
   with For_sure_no -> For_sure_no
    : partial_result)

let eval_string_literal (self : t) (path : Pat_path.t) (str : string)
    ~check_match =
  (let path_db = Path.Map.find_opt self path in
   match StaticInfo.eval_constant path_db (C_string str) with
   | For_sure_no -> For_sure_no
   | For_sure_yes -> For_sure_yes
   | Uncertain { ok_db; fail_db } -> (
       let exception For_sure_no in
       try
         let is_all_yes = ref true in
         let chars = Basic_utf8.from_string str in
         let char_len = Basic_vec_int.length chars in
         (match StaticInfo.eval_eq_array_len_string path_db char_len with
         | For_sure_no -> raise_notrace For_sure_no
         | Uncertain _ -> is_all_yes := false
         | For_sure_yes -> ());
         (if check_match then
            Basic_vec_int.iteri chars (fun i ->
                fun c ->
                 let c_const = Constant.C_char (Uchar.of_int c) in
                 let path = Path.Field i :: path in
                 match
                   StaticInfo.eval_constant
                     (Path.Map.find_opt self path)
                     c_const
                 with
                 | For_sure_no -> raise_notrace For_sure_no
                 | Uncertain _ -> is_all_yes := false
                 | For_sure_yes -> ())
          else
            let utf16_index = ref 0 in
            Basic_vec_int.iteri chars (fun i ->
                fun c ->
                 let c_const = Constant.C_char (Uchar.of_int c) in
                 let cur_utf16_index = !utf16_index in
                 let path =
                   if c > 0xFFFF then Path.Field i :: path
                   else
                     Path.Codeunit_field
                       { index = cur_utf16_index; rev = false }
                     :: path
                 in
                 utf16_index := !utf16_index + if c > 0xFFFF then 2 else 1;
                 match
                   StaticInfo.eval_constant
                     (Path.Map.find_opt self path)
                     c_const
                 with
                 | For_sure_no -> raise_notrace For_sure_no
                 | Uncertain _ -> is_all_yes := false
                 | For_sure_yes -> ()));
         if !is_all_yes then For_sure_yes
         else
           Uncertain
             {
               ok_db = Path.Map.add self path ok_db;
               fail_db = Path.Map.add self path fail_db;
             }
       with For_sure_no -> For_sure_no)
    : static_eval_result)

let eval_constant self path (constant : Constant.t) =
  (make_result self path
     ~info:(StaticInfo.eval_constant (Path.Map.find_opt self path) constant)
    : static_eval_result)

let eval_string_constant self path (str : string) ~check_match =
  (eval_string_literal self path str ~check_match : static_eval_result)
