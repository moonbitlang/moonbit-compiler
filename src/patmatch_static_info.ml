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


module Type_path = Basic_type_path
module Constr_info = Basic_constr_info
module Lst = Basic_lst
module Iter = Basic_iter
module Syntax = Parsing_syntax
module UInt32 = Basic_uint32
module UInt64 = Basic_uint64
module Bigint = Basic_bigint
module Loc = Rloc

module type DB = Basic_case_set_intf.CASE_SET

module MakeSetEncodedCaseSet (CaseType : Basic_set_intf.OrderedType) :
  DB with type case = CaseType.t = struct
  module Set = Basic_setf.Make (CaseType)

  type case = CaseType.t
  type t = Exclude of Set.t | Include of case

  let full = Exclude Set.empty

  let eval self case =
    (match self with
     | Include mycase ->
         if CaseType.equal mycase case then For_sure_yes else For_sure_no
     | Exclude cases ->
         if Set.mem cases case then For_sure_no
         else
           Uncertain
             { ok_db = Include case; fail_db = Exclude (Set.add cases case) }
      : t Basic_case_set_intf.static_matching_result)
end

module BoolDB = struct
  type t = Db_false | Db_true | Db_unknown

  let full = Db_unknown

  let eval (self : t) (case : bool) =
    (match self with
     | Db_false -> if case then For_sure_no else For_sure_yes
     | Db_true -> if case then For_sure_yes else For_sure_no
     | Db_unknown ->
         if case then Uncertain { ok_db = Db_true; fail_db = Db_false }
         else Uncertain { ok_db = Db_false; fail_db = Db_true }
      : t Basic_case_set_intf.static_matching_result)
end

module MakeIntCaseSet (Elt : sig
  include Basic_diet_intf.ELT

  val min_int : t
  val max_int : t
end) =
struct
  module Diet = Basic_diet.Make (Elt)

  type t = Diet.t

  let full = Diet.singleton Elt.min_int Elt.max_int

  let eval (self : Diet.t) (case : Elt.t) =
    (let ok_db = Diet.singleton case case in
     let fail_db () = Diet.diff self ok_db in
     if Diet.mem case self then
       if Diet.has_single_element self then For_sure_yes
       else Uncertain { ok_db; fail_db = fail_db () }
     else For_sure_no
      : Diet.t Basic_case_set_intf.static_matching_result)

  let eval_range (self : Diet.t) (lo : Elt.t option) (hi : Elt.t option)
      ~inclusive =
    (let lo = Option.value lo ~default:Elt.min_int in
     let hi =
       match hi with
       | None -> Elt.max_int
       | Some hi -> if inclusive then hi else Elt.pred hi
     in
     let fail_db = Diet.diff self (Diet.singleton lo hi) in
     let ok_db = Diet.diff self fail_db in
     match (Diet.is_empty ok_db, Diet.is_empty fail_db) with
     | true, true -> assert false
     | true, false -> For_sure_no
     | false, true -> For_sure_yes
     | false, false -> Uncertain { ok_db; fail_db }
      : Diet.t Basic_case_set_intf.static_matching_result)
end

module IntCaseSet = MakeIntCaseSet (Int32)
module UIntCaseSet = MakeIntCaseSet (UInt32)
module Int64CaseSet = MakeIntCaseSet (Int64)
module UInt64CaseSet = MakeIntCaseSet (UInt64)

let full_arr_len = IntCaseSet.Diet.singleton 0l Int32.max_int

let full_uchar =
  IntCaseSet.Diet.singleton
    (Int32.of_int (Uchar.to_int Uchar.min))
    (Int32.of_int (Uchar.to_int Uchar.max))

let full_byte = IntCaseSet.Diet.singleton 0l 255l

module DoubleCaseSet = MakeSetEncodedCaseSet (struct
  include Float

  let sexp_of_t = Moon_sexp_conv.sexp_of_float
end)

module BigintCaseSet = MakeSetEncodedCaseSet (struct
  include Bigint

  let sexp_of_t t = Moon_sexp_conv.sexp_of_string (Bigint.to_string t)
end)

module StringCaseSet = MakeSetEncodedCaseSet (struct
  include String

  let sexp_of_t = Moon_sexp_conv.sexp_of_string
end)

module ConstantSet = Basic_setf.Make (Constant)

type t =
  | Bool of BoolDB.t
  | Uchar of IntCaseSet.t
  | Int of IntCaseSet.t
  | Byte of IntCaseSet.t
  | UInt of UIntCaseSet.t
  | Int64 of Int64CaseSet.t
  | UInt64 of UInt64CaseSet.t
  | Bigint of BigintCaseSet.t
  | Float of DoubleCaseSet.t
  | Double of DoubleCaseSet.t
  | String of { literals : StringCaseSet.t; arr_len : IntCaseSet.t }
  | Bytes of StringCaseSet.t
  | Constr of Constr_info.Index_set.t
  | ErrorConstr of StringCaseSet.t
  | Array of { arr_len : IntCaseSet.t }
  | Map of { fetched : ConstantSet.t; elem_ty : Stype.t }

let full_by_constant = function
  | Constant.C_bool _ -> Bool BoolDB.full
  | C_char _ -> Uchar full_uchar
  | C_int _ -> Int IntCaseSet.full
  | C_byte _ -> Byte full_byte
  | C_int64 _ -> Int64 Int64CaseSet.full
  | C_uint _ -> UInt UIntCaseSet.full
  | C_uint64 _ -> UInt64 UInt64CaseSet.full
  | C_bigint _ -> Bigint BigintCaseSet.full
  | C_float _ -> Float DoubleCaseSet.full
  | C_double _ -> Double DoubleCaseSet.full
  | C_string _ ->
      String { literals = StringCaseSet.full; arr_len = full_arr_len }
  | C_bytes _ -> Bytes StringCaseSet.full

let full_by_extensible_constr = ErrorConstr StringCaseSet.full

let eval_constant self_opt constant =
  (let self =
     match self_opt with Some self -> self | None -> full_by_constant constant
   in
   match (self, constant) with
   | Bool cases, Constant.C_bool case -> (
       match BoolDB.eval cases case with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Bool ok_db; fail_db = Bool fail_db })
   | Uchar cases, C_char case -> (
       match IntCaseSet.eval cases (Int32.of_int (Uchar.to_int case)) with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Uchar ok_db; fail_db = Uchar fail_db })
   | Int cases, C_int case -> (
       match IntCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Int ok_db; fail_db = Int fail_db })
   | Byte cases, C_byte case -> (
       match IntCaseSet.eval cases (Int32.of_int case.v) with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Byte ok_db; fail_db = Byte fail_db })
   | Int64 cases, C_int64 case -> (
       match Int64CaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Int64 ok_db; fail_db = Int64 fail_db })
   | UInt cases, C_uint case -> (
       match UIntCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = UInt ok_db; fail_db = UInt fail_db })
   | UInt64 cases, C_uint64 case -> (
       match UInt64CaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = UInt64 ok_db; fail_db = UInt64 fail_db })
   | Bigint cases, C_bigint case -> (
       match BigintCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Bigint ok_db; fail_db = Bigint fail_db })
   | Double cases, C_double case -> (
       match DoubleCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Double ok_db; fail_db = Double fail_db })
   | Float cases, C_float case -> (
       match DoubleCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Float ok_db; fail_db = Float fail_db })
   | String { literals; arr_len }, C_string case -> (
       match StringCaseSet.eval literals case with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } -> (
           match
             IntCaseSet.eval arr_len (Int32.of_int (Basic_utf8.length case))
           with
           | For_sure_no -> For_sure_no
           | For_sure_yes ->
               Uncertain
                 {
                   ok_db = String { literals = ok_db; arr_len };
                   fail_db = String { literals = fail_db; arr_len };
                 }
           | Uncertain { ok_db = ok_db_len; fail_db = _ } ->
               Uncertain
                 {
                   ok_db = String { literals = ok_db; arr_len = ok_db_len };
                   fail_db = String { literals = fail_db; arr_len };
                 }))
   | Bytes cases, C_bytes case -> (
       match StringCaseSet.eval cases case.v with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Bytes ok_db; fail_db = Bytes fail_db })
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_range self_opt lo hi ~inclusive =
  (let self =
     match self_opt with
     | Some self -> self
     | None -> (
         match lo with
         | Some c -> full_by_constant c
         | None -> (
             match hi with Some c -> full_by_constant c | None -> assert false))
   in
   match self with
   | Uchar cases -> (
       let lo =
         match lo with
         | Some (C_char c) -> Some (Int32.of_int (Uchar.to_int c))
         | _ -> None
       in
       let hi =
         match hi with
         | Some (C_char c) -> Some (Int32.of_int (Uchar.to_int c))
         | _ -> None
       in
       match IntCaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Uchar ok_db; fail_db = Uchar fail_db })
   | Byte cases -> (
       let lo =
         match lo with
         | Some (C_byte { v; repr = _ }) -> Some (Int32.of_int v)
         | _ -> None
       in
       let hi =
         match hi with
         | Some (C_byte { v; repr = _ }) -> Some (Int32.of_int v)
         | _ -> None
       in
       match IntCaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Byte ok_db; fail_db = Byte fail_db })
   | Int cases -> (
       let lo =
         match lo with Some (C_int { v; repr = _ }) -> Some v | _ -> None
       in
       let hi =
         match hi with Some (C_int { v; repr = _ }) -> Some v | _ -> None
       in
       match IntCaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Int ok_db; fail_db = Int fail_db })
   | Int64 cases -> (
       let lo =
         match lo with Some (C_int64 { v; repr = _ }) -> Some v | _ -> None
       in
       let hi =
         match hi with Some (C_int64 { v; repr = _ }) -> Some v | _ -> None
       in
       match Int64CaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = Int64 ok_db; fail_db = Int64 fail_db })
   | UInt cases -> (
       let lo =
         match lo with Some (C_uint { v; repr = _ }) -> Some v | _ -> None
       in
       let hi =
         match hi with Some (C_uint { v; repr = _ }) -> Some v | _ -> None
       in
       match UIntCaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = UInt ok_db; fail_db = UInt fail_db })
   | UInt64 cases -> (
       let lo =
         match lo with Some (C_uint64 { v; repr = _ }) -> Some v | _ -> None
       in
       let hi =
         match hi with Some (C_uint64 { v; repr = _ }) -> Some v | _ -> None
       in
       match UInt64CaseSet.eval_range cases lo hi ~inclusive with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain { ok_db = UInt64 ok_db; fail_db = UInt64 fail_db })
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_constructor self_opt (constr_tag : Constr_info.constr_tag)
    ~(used_error_subtyping : bool) =
  (let eval_constr_index cases index =
     (if not (Constr_info.Index_set.mem index cases) then For_sure_no
      else if Constr_info.Index_set.has_single_element cases then For_sure_yes
      else
        let ok_db = Constr_info.Index_set.singleton index index in
        let fail_db = Constr_info.Index_set.diff cases ok_db in
        Uncertain { ok_db = Constr ok_db; fail_db = Constr fail_db }
       : t Basic_case_set_intf.static_matching_result)
       [@@local]
   in
   match constr_tag with
   | Constr_tag_regular { index = case; total; _ } -> (
       let self =
         match self_opt with Some self -> self | None -> Constr total
       in
       match self with
       | Constr cases -> eval_constr_index cases case
       | _ -> For_sure_no)
   | Extensible_tag { pkg; type_name; name; index; total } -> (
       let tag_str = Basic_constr_info.ext_tag_to_str ~pkg ~type_name ~name in
       let self =
         match self_opt with
         | Some self -> self
         | None ->
             if used_error_subtyping then full_by_extensible_constr
             else Constr total
       in
       match self with
       | ErrorConstr cases -> (
           match StringCaseSet.eval cases tag_str with
           | For_sure_yes -> For_sure_yes
           | For_sure_no -> For_sure_no
           | Uncertain { ok_db; fail_db } ->
               Uncertain
                 { ok_db = ErrorConstr ok_db; fail_db = ErrorConstr fail_db })
       | Constr cases -> eval_constr_index cases index
       | _ -> For_sure_no)
    : t Basic_case_set_intf.static_matching_result)

let eval_eq_array_len self_opt len =
  (let len = Int32.of_int len in
   let self =
     match self_opt with
     | Some self -> self
     | None -> Array { arr_len = full_arr_len }
   in
   match self with
   | Array { arr_len } ->
       let ok_db = Array { arr_len = IntCaseSet.Diet.singleton len len } in
       let fail_db () =
         Array
           {
             arr_len =
               IntCaseSet.Diet.diff arr_len (IntCaseSet.Diet.singleton len len);
           }
       in
       if IntCaseSet.Diet.mem len arr_len then
         if IntCaseSet.Diet.has_single_element arr_len then For_sure_yes
         else Uncertain { ok_db; fail_db = fail_db () }
       else For_sure_no
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_geq_array_len self_opt len =
  (let len = Int32.of_int len in
   let self =
     match self_opt with
     | Some self -> self
     | None -> Array { arr_len = full_arr_len }
   in
   match self with
   | Array { arr_len } -> (
       match IntCaseSet.eval_range arr_len (Some len) None ~inclusive:false with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain
             {
               ok_db = Array { arr_len = ok_db };
               fail_db = Array { arr_len = fail_db };
             })
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_eq_array_len_string self_opt len =
  (let len = Int32.of_int len in
   let self =
     match self_opt with
     | Some self -> self
     | None -> String { arr_len = full_arr_len; literals = StringCaseSet.full }
   in
   match self with
   | String { arr_len; literals } ->
       let ok_db =
         String { arr_len = IntCaseSet.Diet.singleton len len; literals }
       in
       let fail_db () =
         String
           {
             arr_len =
               IntCaseSet.Diet.diff arr_len (IntCaseSet.Diet.singleton len len);
             literals;
           }
       in
       if IntCaseSet.Diet.mem len arr_len then
         if IntCaseSet.Diet.has_single_element arr_len then For_sure_yes
         else Uncertain { ok_db; fail_db = fail_db () }
       else For_sure_no
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_geq_array_len_string self_opt len =
  (let len = Int32.of_int len in
   let self =
     match self_opt with
     | Some self -> self
     | None -> String { arr_len = full_arr_len; literals = StringCaseSet.full }
   in
   match self with
   | String { arr_len; literals } -> (
       match IntCaseSet.eval_range arr_len (Some len) None ~inclusive:false with
       | For_sure_yes -> For_sure_yes
       | For_sure_no -> For_sure_no
       | Uncertain { ok_db; fail_db } ->
           Uncertain
             {
               ok_db = String { arr_len = ok_db; literals };
               fail_db = String { arr_len = fail_db; literals };
             })
   | _ -> For_sure_no
    : t Basic_case_set_intf.static_matching_result)

let eval_map_elem self_opt key ~elem_ty =
  (match self_opt with
   | None -> Map { fetched = ConstantSet.singleton key; elem_ty }
   | Some (Map { fetched; elem_ty }) ->
       Map { fetched = ConstantSet.add fetched key; elem_ty }
   | Some _ -> assert false
    : t)

let synthesize_missing_case_pattern (db : t Pat_path.Map.t)
    ~(genv : Global_env.t) ~(empty_match : bool) (ty : Stype.t) =
  (let pat_any : Syntax.pattern = Ppat_any { loc_ = Loc.no_location } in
   let pat_const c =
     (Ppat_constant { c; loc_ = Loc.no_location } : Syntax.pattern)
   in
   let pat_range lo hi =
     (Ppat_range
        { lhs = lo; rhs = hi; inclusive = false; loc_ = Loc.no_location }
       : Syntax.pattern)
   in
   let pat_or ps =
     match ps with
     | [] -> None
     | p0 :: ps ->
         Some
           (Lst.fold_right ps p0 (fun pat1 ->
                fun pat2 ->
                 (Ppat_or { pat1; pat2; loc_ = Loc.no_location }
                   : Syntax.pattern)))
   in
   let arr_len_is_full db =
     IntCaseSet.Diet.is_empty (IntCaseSet.Diet.diff full_arr_len db)
   in
   let rec pat_for_path path ty ~empty_match =
     let ty = Stype.type_repr ty in
     match Pat_path.Map.find_opt db path with
     | Some (Bool Db_unknown) ->
         if empty_match then
           pat_or [ pat_const (Const_bool true); pat_const (Const_bool false) ]
         else Some pat_any
     | Some (Bool Db_true) -> Some (pat_const (Const_bool true))
     | Some (Bool Db_false) -> Some (pat_const (Const_bool false))
     | Some (Byte cases) -> (
         match Iter.head (IntCaseSet.Diet.iter cases) with
         | Some v ->
             let byte_repr = Printf.sprintf "\\x%02x" (Int32.to_int v) in
             Some
               (pat_const (Const_byte { byte_val = Int32.to_int v; byte_repr }))
         | None -> None)
     | Some (Uchar cases) -> (
         match Iter.head (IntCaseSet.Diet.iter cases) with
         | Some code ->
             let c = Uchar.of_int (Int32.to_int code) in
             Some
               (pat_const
                  (Const_char
                     {
                       char_val = c;
                       char_repr =
                         String.escaped (Basic_uchar_utils.uchar_to_string c);
                     }))
         | None -> None)
     | Some (Int cases) ->
         pat_or
           (Iter.to_rev_list
              (Iter.map
                 ~f:(fun (lo, hi) ->
                   (if Int32.equal lo hi then
                      pat_const (Const_int (Int32.to_string lo))
                    else
                      let lo =
                        if Int32.equal lo Int32.min_int then pat_any
                        else pat_const (Const_int (Int32.to_string lo))
                      in
                      let hi =
                        if Int32.equal hi Int32.max_int then pat_any
                        else
                          pat_const
                            (Const_int (Int32.to_string (Int32.succ hi)))
                      in
                      pat_range lo hi
                     : Syntax.pattern))
                 (IntCaseSet.Diet.iter_range cases)))
     | Some (Int64 cases) ->
         pat_or
           (Iter.to_rev_list
              (Iter.map
                 ~f:(fun (lo, hi) ->
                   (if Int64.equal lo hi then
                      pat_const (Const_int (Int64.to_string lo))
                    else
                      let lo =
                        if Int64.equal lo Int64.min_int then pat_any
                        else pat_const (Const_int (Int64.to_string lo))
                      in
                      let hi =
                        if Int64.equal hi Int64.max_int then pat_any
                        else
                          pat_const
                            (Const_int (Int64.to_string (Int64.succ hi)))
                      in
                      pat_range lo hi
                     : Syntax.pattern))
                 (Int64CaseSet.Diet.iter_range cases)))
     | Some (UInt cases) ->
         pat_or
           (Iter.to_rev_list
              (Iter.map
                 ~f:(fun (lo, hi) ->
                   (if UInt32.equal lo hi then
                      pat_const (Const_int (UInt32.to_string lo))
                    else
                      let lo =
                        if UInt32.equal lo UInt32.min_int then pat_any
                        else pat_const (Const_int (UInt32.to_string lo))
                      in
                      let hi =
                        if UInt32.equal hi UInt32.max_int then pat_any
                        else
                          pat_const
                            (Const_int (UInt32.to_string (UInt32.succ hi)))
                      in
                      pat_range lo hi
                     : Syntax.pattern))
                 (UIntCaseSet.Diet.iter_range cases)))
     | Some (UInt64 cases) ->
         pat_or
           (Iter.to_rev_list
              (Iter.map
                 ~f:(fun (lo, hi) ->
                   (if UInt64.equal lo hi then
                      pat_const (Const_int (UInt64.to_string lo))
                    else
                      let lo =
                        if UInt64.equal lo UInt64.min_int then pat_any
                        else pat_const (Const_int (UInt64.to_string lo))
                      in
                      let hi =
                        if UInt64.equal hi UInt64.max_int then pat_any
                        else
                          pat_const
                            (Const_int (UInt64.to_string (UInt64.succ hi)))
                      in
                      pat_range lo hi
                     : Syntax.pattern))
                 (UInt64CaseSet.Diet.iter_range cases)))
     | Some (Float _cases) -> Some pat_any
     | Some (Double _cases) -> Some pat_any
     | Some (String { arr_len; _ }) when arr_len_is_full arr_len -> Some pat_any
     | Some (Bytes _cases) -> Some pat_any
     | Some (ErrorConstr _cases) -> Some pat_any
     | Some (Bigint _cases) -> Some pat_any
     | Some (Constr constr_cases) ->
         let module Hash_int = Basic_hash_int in
         let constrs =
           Result.get_ok
             (Global_env.constrs_of_variant genv ty ~loc:Loc.no_location
                ~creating_value:false)
         in
         let index_to_constr =
           Hash_int.of_list_map constrs (fun constr ->
               let index =
                 match constr.cs_tag with
                 | Constr_tag_regular { index; _ } -> index
                 | Extensible_tag { index; _ } -> index
               in
               (index, constr))
         in
         pat_or
           (Iter.to_rev_list
              (Iter.take 3
                 (Iter.filter_map
                    ~f:(fun index ->
                      pat_for_constr ~ty path
                        (Hash_int.find_exn index_to_constr index)
                        ~with_payload:true)
                    (Constr_info.Index_set.iter constr_cases))))
     | Some (Array { arr_len } | String { arr_len; _ }) ->
         let elem_ty =
           match ty with
           | T_constr { type_constructor; tys = elem_ty :: [] }
             when Type_path.can_use_array_pattern type_constructor ->
               elem_ty
           | T_constr { type_constructor; _ }
             when Type_path.is_bytesview type_constructor ->
               Stype.byte
           | T_builtin T_string -> Stype.char
           | T_constr { type_constructor = p; _ }
             when Type_path.equal p Type_path.Builtin.type_path_stringview ->
               Stype.char
           | _ -> assert false
         in
         pat_or
           (Iter.to_rev_list
              (Iter.filter_map
                 ~f:(fun (min, max) ->
                   let len = Int32.to_int min in
                   let rec loop ~rev index acc =
                     if index >= len then
                       if rev then Some acc else Some (List.rev acc)
                     else
                       let p =
                         if rev then Pat_path.Last_field index else Field index
                       in
                       match
                         pat_for_path ~empty_match:false (p :: path) elem_ty
                       with
                       | None -> None
                       | Some elem_pat ->
                           loop ~rev (index + 1) (Syntax.Pattern elem_pat :: acc)
                   in
                   let make_open_arr_pat head_pats tail_pats =
                     (let rec trim pats n =
                        if n <= 0 then (pats, 0)
                        else
                          match pats with
                          | [] -> ([], n)
                          | Syntax.Pattern (Ppat_any _) :: pats ->
                              trim pats (n - 1)
                          | _ -> (pats, n)
                      in
                      let trimmed_tail_pats, rest_len = trim tail_pats len in
                      match trimmed_tail_pats with
                      | [] -> Open (head_pats, [], None)
                      | _ :: _ -> (
                          let trimmed_head_pats, _ =
                            trim (List.rev head_pats) rest_len
                          in
                          match List.rev trimmed_head_pats with
                          | [] -> Open ([], trimmed_tail_pats, None)
                          | trimmed_head_pats ->
                              Open (trimmed_head_pats, trimmed_tail_pats, None))
                       : Syntax.array_patterns)
                   in
                   if Int32.equal max Int32.max_int then
                     match loop ~rev:false 0 [] with
                     | None -> None
                     | Some head_pats -> (
                         match loop ~rev:true 0 [] with
                         | None -> None
                         | Some tail_pats ->
                             Some
                               (Syntax.Ppat_array
                                  {
                                    pats = make_open_arr_pat head_pats tail_pats;
                                    loc_ = Loc.no_location;
                                  }))
                   else
                     match loop ~rev:false 0 [] with
                     | Some pats ->
                         Some
                           (Syntax.Ppat_array
                              { pats = Closed pats; loc_ = Loc.no_location })
                     | None -> None)
                 (IntCaseSet.Diet.iter_range arr_len)))
     | Some (Map { fetched; elem_ty }) ->
         let missing_case_for_present_fields = ref [] in
         if
           ConstantSet.for_all fetched (fun key ->
               match
                 pat_for_path ~empty_match:false
                   (Pat_path.Map_elem { key } :: path)
                   elem_ty
               with
               | None -> false
               | Some (Ppat_any _) -> true
               | Some pat ->
                   let pat, match_absent =
                     match pat with
                     | Ppat_constr
                         {
                           constr = { constr_name = { name = "Some" }; _ };
                           args = Some (Constr_pat_arg { pat; kind = _ } :: []);
                           is_open = _;
                         } ->
                         (pat, false)
                     | _ -> (pat, true)
                   in
                   missing_case_for_present_fields :=
                     Syntax.Map_pat_elem
                       {
                         key = Typeutil.typed_constant_to_syntax_constant key;
                         pat;
                         match_absent;
                         key_loc_ = Loc.no_location;
                         loc_ = Loc.no_location;
                       }
                     :: !missing_case_for_present_fields;
                   true)
         then
           Some
             (Ppat_map
                {
                  elems = !missing_case_for_present_fields;
                  is_closed = false;
                  loc_ = Loc.no_location;
                })
         else
           Some
             (Ppat_map { elems = []; is_closed = false; loc_ = Loc.no_location })
     | None -> (
         match ty with
         | T_constr { type_constructor = Tuple _; tys } ->
             let rec loop index prev_pats_rev tys =
               match tys with
               | []
                 when Lst.for_all prev_pats_rev (function
                        | Syntax.Ppat_any _ -> true
                        | _ -> false) ->
                   Some pat_any
               | [] ->
                   Some
                     (Ppat_tuple
                        {
                          pats = List.rev prev_pats_rev;
                          loc_ = Loc.no_location;
                        })
               | ty :: tys -> (
                   match pat_for_path ~empty_match (Field index :: path) ty with
                   | None -> None
                   | Some elem_pat ->
                       loop (index + 1) (elem_pat :: prev_pats_rev) tys)
             in
             loop 0 [] tys
         | T_constr { type_constructor; tys = _ }
           when Type_path.equal type_constructor
                  Type_path.Builtin.type_path_fixedarray ->
             Some
               (Ppat_array
                  { pats = Open ([], [], None); loc_ = Loc.no_location })
         | T_constr { type_constructor; tys = _ } -> (
             match Global_env.find_type_by_path genv type_constructor with
             | None -> Some pat_any
             | Some
                 {
                   ty_desc =
                     ( Variant_type (c :: [])
                     | ErrorEnum_type (c :: [])
                     | Error_type c
                     | New_type { newtype_constr = c; _ } );
                   _;
                 } ->
                 pat_for_constr ~ty path c ~with_payload:(not empty_match)
             | Some { ty_desc = Variant_type [] | ErrorEnum_type []; _ } -> None
             | Some { ty_desc = Variant_type cs | ErrorEnum_type cs; _ }
               when empty_match ->
                 pat_or
                   (Lst.fold_left cs [] (fun acc ->
                        fun c ->
                         match
                           pat_for_constr ~ty path c ~with_payload:false
                         with
                         | None -> acc
                         | Some p -> p :: acc))
             | Some
                 {
                   ty_desc =
                     ( Extern_type | Abstract_type | Variant_type _
                     | ErrorEnum_type _ );
                   _;
                 } ->
                 Some pat_any
             | Some { ty_desc = Record_type { fields }; _ } ->
                 let _, fields =
                   Poly_type.instantiate_record ~ty_record:(`Known ty) fields
                 in
                 let rec loop prev_field_pats_rev fields =
                   match fields with
                   | [] ->
                       let fields, any_count =
                         Lst.fold_left prev_field_pats_rev ([], 0)
                           (fun (fields, any_count) ->
                             fun (field_pat : Syntax.field_pat) ->
                              match field_pat with
                              | Field_pat { pattern = Ppat_any _; _ } ->
                                  (fields, any_count + 1)
                              | _ -> (field_pat :: fields, any_count))
                       in
                       if fields = [] then Some pat_any
                       else
                         Some
                           (Ppat_record
                              {
                                fields;
                                is_closed = any_count = 0;
                                loc_ = Loc.no_location;
                              })
                   | (field : Typedecl_info.field) :: fields -> (
                       match
                         pat_for_path ~empty_match:false
                           (Field field.pos :: path) field.ty_field
                       with
                       | None -> None
                       | Some pattern ->
                           loop
                             (Syntax.Field_pat
                                {
                                  label =
                                    {
                                      label_name = field.field_name;
                                      loc_ = Loc.no_location;
                                    };
                                  pattern;
                                  is_pun = false;
                                  loc_ = Loc.no_location;
                                }
                             :: prev_field_pats_rev)
                             fields)
                 in
                 loop [] fields)
         | _ -> Some pat_any)
   and pat_for_constr ~ty path (constr : Typedecl_info.constructor)
       ~(with_payload : bool) =
     let index =
       match constr.cs_tag with
       | Constr_tag_regular { index; _ } -> index
       | Extensible_tag _ -> 0
     in
     let arity = constr.cs_arity_ in
     let ty_res, arg_tys = Poly_type.instantiate_constr constr in
     Ctype.unify_exn ty_res ty;
     let constr_name : Syntax.constr_name =
       { name = constr.constr_name; loc_ = Loc.no_location }
     in
     let constr : Syntax.constructor =
       { constr_name; extra_info = No_extra_info; loc_ = Loc.no_location }
     in
     let mk_pat ~is_open args =
       (Some (Ppat_constr { constr; args; loc_ = Loc.no_location; is_open })
         : Syntax.pattern option)
     in
     match arg_tys with
     | [] -> mk_pat ~is_open:false None
     | arg_tys ->
         let param_kinds = Fn_arity.to_list_map arity Fun.id in
         let rec loop ~is_open arg_index prev_pats_rev arg_tys param_kinds =
           match (arg_tys, param_kinds) with
           | [], _ ->
               if with_payload then
                 mk_pat ~is_open (Some (List.rev prev_pats_rev))
               else
                 mk_pat ~is_open:false
                   (Some [ Constr_pat_arg { pat = pat_any; kind = Positional } ])
           | arg_ty :: arg_tys, param_kind :: param_kinds -> (
               let kind : Syntax.argument_kind =
                 match (param_kind : Fn_arity.param_kind) with
                 | Positional _ -> Positional
                 | Labelled { label = label_name; _ }
                 | Optional { label = label_name; _ }
                 | Autofill { label = label_name }
                 | Question_optional { label = label_name } ->
                     Labelled { label_name; loc_ = Loc.no_location }
               in
               let arg_path : Pat_path.t =
                 Constr_field { tag_index = index; arg_index } :: path
               in
               match
                 (kind, pat_for_path ~empty_match:false arg_path arg_ty)
               with
               | _, None -> None
               | ( ( Labelled _ | Labelled_pun _ | Labelled_option _
                   | Labelled_option_pun _ ),
                   Some (Ppat_any _) ) ->
                   loop ~is_open:true (arg_index + 1) prev_pats_rev arg_tys
                     param_kinds
               | _, Some arg_pat ->
                   loop ~is_open (arg_index + 1)
                     (Constr_pat_arg { pat = arg_pat; kind } :: prev_pats_rev)
                     arg_tys param_kinds)
           | _ -> assert false
         in
         loop ~is_open:false 0 [] arg_tys param_kinds
   in
   match pat_for_path ~empty_match [] ty with
   | None -> []
   | Some pat ->
       let rec break_or_pat (p : Syntax.pattern) =
         match p with
         | Ppat_or { pat1; pat2 } -> pat1 :: break_or_pat pat2
         | _ -> [ p ]
       in
       break_or_pat pat
    : Syntax.pattern list)
