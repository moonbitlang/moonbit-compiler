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
module Syntax = Parsing_syntax
module Vec = Basic_vec

type param_kind =
  | Positional of int
  | Labelled of {
      label : string;
      is_mut : bool;
      loc_ : Loc.t;
          [@ceh.ignore] [@sexp_drop_if fun _ -> not !Basic_config.show_loc]
    }
  | Optional of {
      label : string;
      depends_on : int list;
      loc_ : Loc.t;
          [@ceh.ignore] [@sexp_drop_if fun _ -> not !Basic_config.show_loc]
    }
  | Autofill of {
      label : string;
      loc_ : Loc.t;
          [@ceh.ignore] [@sexp_drop_if fun _ -> not !Basic_config.show_loc]
    }
  | Question_optional of {
      label : string;
      loc_ : Loc.t;
          [@ceh.ignore] [@sexp_drop_if fun _ -> not !Basic_config.show_loc]
    }

include struct
  let _ = fun (_ : param_kind) -> ()

  let sexp_of_param_kind =
    (let (drop_if__009_ : Loc.t -> Stdlib.Bool.t) =
      fun _ -> not !Basic_config.show_loc
     and (drop_if__019_ : Loc.t -> Stdlib.Bool.t) =
      fun _ -> not !Basic_config.show_loc
     and (drop_if__027_ : Loc.t -> Stdlib.Bool.t) =
      fun _ -> not !Basic_config.show_loc
     and (drop_if__035_ : Loc.t -> Stdlib.Bool.t) =
      fun _ -> not !Basic_config.show_loc
     in
     function
     | Positional arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_int arg0__001_ in
         S.List [ S.Atom "Positional"; res0__002_ ]
     | Labelled
         { label = label__004_; is_mut = is_mut__006_; loc_ = loc___010_ } ->
         let bnds__003_ = ([] : _ Stdlib.List.t) in
         let bnds__003_ =
           if drop_if__009_ loc___010_ then bnds__003_
           else
             let arg__012_ = Loc.sexp_of_t loc___010_ in
             let bnd__011_ = S.List [ S.Atom "loc_"; arg__012_ ] in
             (bnd__011_ :: bnds__003_ : _ Stdlib.List.t)
         in
         let bnds__003_ =
           let arg__007_ = Moon_sexp_conv.sexp_of_bool is_mut__006_ in
           (S.List [ S.Atom "is_mut"; arg__007_ ] :: bnds__003_
             : _ Stdlib.List.t)
         in
         let bnds__003_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_string label__004_ in
           (S.List [ S.Atom "label"; arg__005_ ] :: bnds__003_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Labelled" :: bnds__003_)
     | Optional
         {
           label = label__014_;
           depends_on = depends_on__016_;
           loc_ = loc___020_;
         } ->
         let bnds__013_ = ([] : _ Stdlib.List.t) in
         let bnds__013_ =
           if drop_if__019_ loc___020_ then bnds__013_
           else
             let arg__022_ = Loc.sexp_of_t loc___020_ in
             let bnd__021_ = S.List [ S.Atom "loc_"; arg__022_ ] in
             (bnd__021_ :: bnds__013_ : _ Stdlib.List.t)
         in
         let bnds__013_ =
           let arg__017_ =
             Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_int
               depends_on__016_
           in
           (S.List [ S.Atom "depends_on"; arg__017_ ] :: bnds__013_
             : _ Stdlib.List.t)
         in
         let bnds__013_ =
           let arg__015_ = Moon_sexp_conv.sexp_of_string label__014_ in
           (S.List [ S.Atom "label"; arg__015_ ] :: bnds__013_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Optional" :: bnds__013_)
     | Autofill { label = label__024_; loc_ = loc___028_ } ->
         let bnds__023_ = ([] : _ Stdlib.List.t) in
         let bnds__023_ =
           if drop_if__027_ loc___028_ then bnds__023_
           else
             let arg__030_ = Loc.sexp_of_t loc___028_ in
             let bnd__029_ = S.List [ S.Atom "loc_"; arg__030_ ] in
             (bnd__029_ :: bnds__023_ : _ Stdlib.List.t)
         in
         let bnds__023_ =
           let arg__025_ = Moon_sexp_conv.sexp_of_string label__024_ in
           (S.List [ S.Atom "label"; arg__025_ ] :: bnds__023_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Autofill" :: bnds__023_)
     | Question_optional { label = label__032_; loc_ = loc___036_ } ->
         let bnds__031_ = ([] : _ Stdlib.List.t) in
         let bnds__031_ =
           if drop_if__035_ loc___036_ then bnds__031_
           else
             let arg__038_ = Loc.sexp_of_t loc___036_ in
             let bnd__037_ = S.List [ S.Atom "loc_"; arg__038_ ] in
             (bnd__037_ :: bnds__031_ : _ Stdlib.List.t)
         in
         let bnds__031_ =
           let arg__033_ = Moon_sexp_conv.sexp_of_string label__032_ in
           (S.List [ S.Atom "label"; arg__033_ ] :: bnds__031_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Question_optional" :: bnds__031_)
      : param_kind -> S.t)

  let _ = sexp_of_param_kind

  let equal_param_kind =
    (fun a__039_ ->
       fun b__040_ ->
        if Stdlib.( == ) a__039_ b__040_ then true
        else
          match (a__039_, b__040_) with
          | Positional _a__041_, Positional _b__042_ ->
              Stdlib.( = ) (_a__041_ : int) _b__042_
          | Positional _, _ -> false
          | _, Positional _ -> false
          | Labelled _a__043_, Labelled _b__044_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__043_.label : string) _b__044_.label)
                (Stdlib.( = ) (_a__043_.is_mut : bool) _b__044_.is_mut)
          | Labelled _, _ -> false
          | _, Labelled _ -> false
          | Optional _a__045_, Optional _b__046_ ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__045_.label : string) _b__046_.label)
                (Ppx_base.equal_list
                   (fun a__047_ ->
                     fun (b__048_ [@merlin.hide]) ->
                      (Stdlib.( = ) (a__047_ : int) b__048_ [@merlin.hide]))
                   _a__045_.depends_on _b__046_.depends_on)
          | Optional _, _ -> false
          | _, Optional _ -> false
          | Autofill _a__049_, Autofill _b__050_ ->
              Stdlib.( = ) (_a__049_.label : string) _b__050_.label
          | Autofill _, _ -> false
          | _, Autofill _ -> false
          | Question_optional _a__051_, Question_optional _b__052_ ->
              Stdlib.( = ) (_a__051_.label : string) _b__052_.label
      : param_kind -> param_kind -> bool)

  let _ = equal_param_kind
end

type t = Simple of int | Complex of param_kind list

include struct
  let _ = fun (_ : t) -> ()

  let equal =
    (fun a__053_ ->
       fun b__054_ ->
        if Stdlib.( == ) a__053_ b__054_ then true
        else
          match (a__053_, b__054_) with
          | Simple _a__055_, Simple _b__056_ ->
              Stdlib.( = ) (_a__055_ : int) _b__056_
          | Simple _, _ -> false
          | _, Simple _ -> false
          | Complex _a__057_, Complex _b__058_ ->
              Ppx_base.equal_list
                (fun a__059_ ->
                  fun (b__060_ [@merlin.hide]) ->
                   (equal_param_kind a__059_ b__060_ [@merlin.hide]))
                _a__057_ _b__058_
      : t -> t -> bool)

  let _ = equal
end

let arity_0 = Simple 0
let arity_1 = Simple 1
let arity_2 = Simple 2
let arity_3 = Simple 3
let arity_4 = Simple 4
let arity_5 = Simple 5
let arity_6 = Simple 6
let arity_7 = Simple 7

let simple n =
  match n with
  | 0 -> arity_0
  | 1 -> arity_1
  | 2 -> arity_2
  | 3 -> arity_3
  | 4 -> arity_4
  | 5 -> arity_5
  | 6 -> arity_6
  | 7 -> arity_7
  | _ -> Simple n

let is_simple = function Simple _ -> true | Complex _ -> false

let count_positional = function
  | Simple n -> n
  | Complex kinds ->
      Lst.fold_left kinds 0 (fun count ->
          fun kind ->
           match kind with
           | Positional _ -> count + 1
           | Labelled _ | Optional _ | Autofill _ | Question_optional _ -> count)

let analyze_default_expr_deps_visitor =
  let module H = Basic_hashset_string in
  object
    inherit [_] Syntax.iter

    method! visit_var unseen_vars var =
      match var.var_name with
      | Lident name -> H.remove unseen_vars name
      | Ldot _ -> ()
  end

let analyze_default_expr_deps (params : string list) (expr : Syntax.expr) =
  let module H = Basic_hashset_string in
  let unseen = H.create 17 in
  Lst.iter params ~f:(H.add unseen);
  analyze_default_expr_deps_visitor#visit_expr unseen expr;
  Lst.filter_mapi params (fun name ->
      fun index -> if H.mem unseen name then None else Some index)

let from_params ~(base : Loc.t) (params : Syntax.parameter list) =
  (let module H = Basic_hash_string in
   let seen_labels = H.create 17 in
   let positional_index = ref 0 in
   let errors = ref [] in
   let prev_params = Vec.empty () in
   let kinds =
     Lst.map params (fun p ->
         match p with
         | Discard_positional _ ->
             let i = !positional_index in
             incr positional_index;
             Positional i
         | Positional { binder; _ }
         | Labelled { binder; _ }
         | Optional { binder; _ }
         | Question_optional { binder; _ } ->
             let ({ binder_name = label; loc_ = loc } : Syntax.binder) =
               binder
             in
             let check_duplicate () =
               match H.find_opt seen_labels label with
               | None -> H.add seen_labels label loc
               | Some first_loc ->
                   errors :=
                     Errors.duplicated_label_in_decl ~label
                       ~first_loc:(Rloc.to_loc ~base first_loc)
                       ~second_loc:loc
                     :: !errors
                 [@@inline]
             in
             let kind =
               match p with
               | Positional _ ->
                   let i = !positional_index in
                   incr positional_index;
                   Positional i
               | Labelled _ ->
                   check_duplicate ();
                   Labelled
                     { label; is_mut = false; loc_ = Rloc.to_loc ~base loc }
               | Optional { default; _ } -> (
                   check_duplicate ();
                   match default with
                   | Pexpr_hole { kind = Incomplete } ->
                       Autofill { label; loc_ = Rloc.to_loc ~base loc }
                   | _ ->
                       Optional
                         {
                           label;
                           depends_on =
                             analyze_default_expr_deps (Vec.to_list prev_params)
                               default;
                           loc_ = Rloc.to_loc ~base loc;
                         })
               | Question_optional _ ->
                   check_duplicate ();
                   Question_optional { label; loc_ = Rloc.to_loc ~base loc }
               | Discard_positional _ ->
                   failwith
                     "Unreachable: Discard_positional and Positional are \
                      handled above"
             in
             Vec.push prev_params label;
             kind)
   in
   let t =
     if H.length seen_labels = 0 then Simple !positional_index
     else Complex kinds
   in
   match !errors with [] -> Ok t | errs -> Partial (t, errs)
    : t Local_diagnostics.partial_info)

let from_constr_params ~(base : Loc.t) (params : Syntax.constr_param list) =
  (let module H = Basic_hash_string in
   let seen_labels = H.create 17 in
   let positional_index = ref 0 in
   let errors = ref [] in
   let kinds =
     Lst.map params (fun p ->
         match p.cparam_label with
         | None ->
             let i = !positional_index in
             incr positional_index;
             Positional i
         | Some { label_name = label; loc_ } ->
             (match H.find_opt seen_labels label with
             | None -> H.add seen_labels label loc_
             | Some first_loc ->
                 errors :=
                   Errors.duplicated_label_in_decl ~label
                     ~first_loc:(Rloc.to_loc ~base first_loc)
                     ~second_loc:loc_
                   :: !errors);
             Labelled
               { label; is_mut = p.cparam_mut; loc_ = Rloc.to_loc ~base loc_ })
   in
   let t =
     if H.length seen_labels = 0 then Simple !positional_index
     else Complex kinds
   in
   match !errors with [] -> Ok t | errs -> Partial (t, errs)
    : t Local_diagnostics.partial_info)

let from_trait_method_params ~(base : Loc.t)
    (params : Syntax.trait_method_param list) =
  (let module H = Basic_hash_string in
   let seen_labels = H.create 17 in
   let positional_index = ref 0 in
   let errors = ref [] in
   let kinds =
     Lst.map params (fun p ->
         match p.tmparam_label with
         | None ->
             let i = !positional_index in
             incr positional_index;
             Positional i
         | Some { label_name = label; loc_ } ->
             (match H.find_opt seen_labels label with
             | None -> H.add seen_labels label loc_
             | Some first_loc ->
                 errors :=
                   Errors.duplicated_label_in_decl ~label
                     ~first_loc:(Rloc.to_loc ~base first_loc)
                     ~second_loc:loc_
                   :: !errors);
             Labelled { label; is_mut = false; loc_ = Rloc.to_loc ~base loc_ })
   in
   let t =
     if H.length seen_labels = 0 then Simple !positional_index
     else Complex kinds
   in
   match !errors with [] -> Ok t | errs -> Partial (t, errs)
    : t Local_diagnostics.partial_info)

let iter arity f =
  match arity with
  | Simple n ->
      for i = 0 to n - 1 do
        f (Positional i)
      done
  | Complex params -> Lst.iter params ~f

let iter2 arity items f =
  match arity with
  | Simple _ ->
      List.iteri (fun index -> fun item -> f (Positional index) item) items
  | Complex params -> Lst.iter2 params items f

let to_list_map arity f =
  match arity with
  | Simple n -> List.init n (fun i -> f (Positional i))
  | Complex params -> Lst.map params f

let to_list_map2 arity items f =
  match arity with
  | Simple _ ->
      Lst.mapi items (fun index -> fun item -> f (Positional index) item)
  | Complex params -> Lst.map2 params items f

let find_constr_label arity typs ~label =
  match arity with
  | Simple _ -> None
  | Complex params ->
      let rec go offset params typs =
        match (params, typs) with
        | [], [] -> None
        | [], _ | _, [] -> assert false
        | Labelled { label = label'; is_mut } :: _, ty :: _ when label' = label
          ->
            Some (ty, offset, is_mut)
        | _ :: params, _ :: typs -> go (offset + 1) params typs
      in
      go 0 params typs

let find_label arity ~label:target =
  match arity with
  | Simple _ -> None
  | Complex params ->
      Lst.find_first params (fun param_kind ->
          match param_kind with
          | Labelled { label; _ }
          | Optional { label; _ }
          | Autofill { label }
          | Question_optional { label } ->
              label = target
          | Positional _ -> false)

let sexp_of_t arity =
  S.List
    (to_list_map arity (fun kind ->
         match kind with
         | Positional _ -> S.Atom "_"
         | Labelled { label; is_mut = false } ->
             S.Atom ((label ^ "~" : Stdlib.String.t) [@merlin.hide])
         | Labelled { label; is_mut = true } ->
             S.List
               [
                 Atom "mut";
                 Atom ((label ^ "~" : Stdlib.String.t) [@merlin.hide]);
               ]
         | Optional { label; _ } ->
             S.Atom ((label ^ "?=" : Stdlib.String.t) [@merlin.hide])
         | Autofill { label } -> S.List [ Atom "auto"; Atom label ]
         | Question_optional { label } ->
             Atom ((label ^ "?" : Stdlib.String.t) [@merlin.hide])))

type arg_kind = Positional of int | Labelled of string

include struct
  let _ = fun (_ : arg_kind) -> ()

  let sexp_of_arg_kind =
    (function
     | Positional arg0__061_ ->
         let res0__062_ = Moon_sexp_conv.sexp_of_int arg0__061_ in
         S.List [ S.Atom "Positional"; res0__062_ ]
     | Labelled arg0__063_ ->
         let res0__064_ = Moon_sexp_conv.sexp_of_string arg0__063_ in
         S.List [ S.Atom "Labelled"; res0__064_ ]
      : arg_kind -> S.t)

  let _ = sexp_of_arg_kind

  let equal_arg_kind =
    (fun a__065_ ->
       fun b__066_ ->
        if Stdlib.( == ) a__065_ b__066_ then true
        else
          match (a__065_, b__066_) with
          | Positional _a__067_, Positional _b__068_ ->
              Stdlib.( = ) (_a__067_ : int) _b__068_
          | Positional _, _ -> false
          | _, Positional _ -> false
          | Labelled _a__069_, Labelled _b__070_ ->
              Stdlib.( = ) (_a__069_ : string) _b__070_
      : arg_kind -> arg_kind -> bool)

  let _ = equal_arg_kind

  let (hash_fold_arg_kind : Ppx_base.state -> arg_kind -> Ppx_base.state) =
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
      : Ppx_base.state -> arg_kind -> Ppx_base.state)

  let _ = hash_fold_arg_kind

  let (hash_arg_kind : arg_kind -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_arg_kind hsv arg)
    in
    fun x -> func x

  let _ = hash_arg_kind
end

module Hash = Basic_hashf.Make (struct
  type t = arg_kind

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_arg_kind : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_arg_kind : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      hash_fold_arg_kind

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_arg_kind in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

module Hashset = Basic_hashsetf.Make (struct
  type t = arg_kind

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_arg_kind : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_arg_kind : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
      hash_fold_arg_kind

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_arg_kind in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

let to_hashtbl (type a) (type b) arity (items : a list)
    (f : param_kind -> a -> b) =
  let tbl : b Hash.t = Hash.create 17 in
  iter2 arity items (fun kind ->
      fun item ->
       match kind with
       | Positional i -> Hash.add tbl (Positional i) (f kind item)
       | Labelled { label; _ }
       | Optional { label; _ }
       | Autofill { label }
       | Question_optional { label } ->
           Hash.add tbl (Labelled label) (f kind item));
  tbl

let is_immutable_constr_arity arity =
  match arity with
  | Simple _ -> true
  | Complex params ->
      Basic_lst.for_all params (fun p ->
          match p with Labelled { is_mut; _ } -> not is_mut | _ -> true)
