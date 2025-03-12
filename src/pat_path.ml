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


module Path_def = struct
  type t = access list

  and access =
    | Field of int
    | Constr_field of { tag_index : int; arg_index : int }
    | Last_field of int
    | Field_slice
    | Casted_constr of { tag_index : int }
    | Map_elem of { key : Constant.t }
    | Error_constr_field of {
        tag : Basic_constr_info.constr_tag;
        arg_index : int;
      }
    | Codeunit_field of { index : int; rev : bool }

  include struct
    let _ = fun (_ : t) -> ()
    let _ = fun (_ : access) -> ()

    let rec equal =
      (fun a__001_ ->
         fun b__002_ ->
          Ppx_base.equal_list
            (fun a__003_ -> fun b__004_ -> equal_access a__003_ b__004_)
            a__001_ b__002_
        : t -> t -> bool)

    and equal_access =
      (fun a__005_ ->
         fun b__006_ ->
          if Stdlib.( == ) a__005_ b__006_ then true
          else
            match (a__005_, b__006_) with
            | Field _a__007_, Field _b__008_ ->
                Stdlib.( = ) (_a__007_ : int) _b__008_
            | Field _, _ -> false
            | _, Field _ -> false
            | Constr_field _a__009_, Constr_field _b__010_ ->
                Stdlib.( && )
                  (Stdlib.( = ) (_a__009_.tag_index : int) _b__010_.tag_index)
                  (Stdlib.( = ) (_a__009_.arg_index : int) _b__010_.arg_index)
            | Constr_field _, _ -> false
            | _, Constr_field _ -> false
            | Last_field _a__011_, Last_field _b__012_ ->
                Stdlib.( = ) (_a__011_ : int) _b__012_
            | Last_field _, _ -> false
            | _, Last_field _ -> false
            | Field_slice, Field_slice -> true
            | Field_slice, _ -> false
            | _, Field_slice -> false
            | Casted_constr _a__013_, Casted_constr _b__014_ ->
                Stdlib.( = ) (_a__013_.tag_index : int) _b__014_.tag_index
            | Casted_constr _, _ -> false
            | _, Casted_constr _ -> false
            | Map_elem _a__015_, Map_elem _b__016_ ->
                Constant.equal _a__015_.key _b__016_.key
            | Map_elem _, _ -> false
            | _, Map_elem _ -> false
            | Error_constr_field _a__017_, Error_constr_field _b__018_ ->
                Stdlib.( && )
                  (Basic_constr_info.equal_constr_tag _a__017_.tag _b__018_.tag)
                  (Stdlib.( = ) (_a__017_.arg_index : int) _b__018_.arg_index)
            | Error_constr_field _, _ -> false
            | _, Error_constr_field _ -> false
            | Codeunit_field _a__019_, Codeunit_field _b__020_ ->
                Stdlib.( && )
                  (Stdlib.( = ) (_a__019_.index : int) _b__020_.index)
                  (Stdlib.( = ) (_a__019_.rev : bool) _b__020_.rev)
        : access -> access -> bool)

    let _ = equal
    and _ = equal_access

    let rec compare =
      (fun a__021_ ->
         fun b__022_ ->
          Ppx_base.compare_list
            (fun a__023_ -> fun b__024_ -> compare_access a__023_ b__024_)
            a__021_ b__022_
        : t -> t -> int)

    and compare_access =
      (fun a__025_ ->
         fun b__026_ ->
          if Stdlib.( == ) a__025_ b__026_ then 0
          else
            match (a__025_, b__026_) with
            | Field _a__027_, Field _b__028_ ->
                Stdlib.compare (_a__027_ : int) _b__028_
            | Field _, _ -> -1
            | _, Field _ -> 1
            | Constr_field _a__029_, Constr_field _b__030_ -> (
                match
                  Stdlib.compare (_a__029_.tag_index : int) _b__030_.tag_index
                with
                | 0 ->
                    Stdlib.compare (_a__029_.arg_index : int) _b__030_.arg_index
                | n -> n)
            | Constr_field _, _ -> -1
            | _, Constr_field _ -> 1
            | Last_field _a__031_, Last_field _b__032_ ->
                Stdlib.compare (_a__031_ : int) _b__032_
            | Last_field _, _ -> -1
            | _, Last_field _ -> 1
            | Field_slice, Field_slice -> 0
            | Field_slice, _ -> -1
            | _, Field_slice -> 1
            | Casted_constr _a__033_, Casted_constr _b__034_ ->
                Stdlib.compare (_a__033_.tag_index : int) _b__034_.tag_index
            | Casted_constr _, _ -> -1
            | _, Casted_constr _ -> 1
            | Map_elem _a__035_, Map_elem _b__036_ ->
                Constant.compare _a__035_.key _b__036_.key
            | Map_elem _, _ -> -1
            | _, Map_elem _ -> 1
            | Error_constr_field _a__037_, Error_constr_field _b__038_ -> (
                match
                  Basic_constr_info.compare_constr_tag _a__037_.tag _b__038_.tag
                with
                | 0 ->
                    Stdlib.compare (_a__037_.arg_index : int) _b__038_.arg_index
                | n -> n)
            | Error_constr_field _, _ -> -1
            | _, Error_constr_field _ -> 1
            | Codeunit_field _a__039_, Codeunit_field _b__040_ -> (
                match Stdlib.compare (_a__039_.index : int) _b__040_.index with
                | 0 -> Stdlib.compare (_a__039_.rev : bool) _b__040_.rev
                | n -> n)
        : access -> access -> int)

    let _ = compare
    and _ = compare_access

    let rec sexp_of_t =
      (fun x__041_ -> Moon_sexp_conv.sexp_of_list sexp_of_access x__041_
        : t -> S.t)

    and sexp_of_access =
      (function
       | Field arg0__042_ ->
           let res0__043_ = Moon_sexp_conv.sexp_of_int arg0__042_ in
           S.List [ S.Atom "Field"; res0__043_ ]
       | Constr_field
           { tag_index = tag_index__045_; arg_index = arg_index__047_ } ->
           let bnds__044_ = ([] : _ Stdlib.List.t) in
           let bnds__044_ =
             let arg__048_ = Moon_sexp_conv.sexp_of_int arg_index__047_ in
             (S.List [ S.Atom "arg_index"; arg__048_ ] :: bnds__044_
               : _ Stdlib.List.t)
           in
           let bnds__044_ =
             let arg__046_ = Moon_sexp_conv.sexp_of_int tag_index__045_ in
             (S.List [ S.Atom "tag_index"; arg__046_ ] :: bnds__044_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Constr_field" :: bnds__044_)
       | Last_field arg0__049_ ->
           let res0__050_ = Moon_sexp_conv.sexp_of_int arg0__049_ in
           S.List [ S.Atom "Last_field"; res0__050_ ]
       | Field_slice -> S.Atom "Field_slice"
       | Casted_constr { tag_index = tag_index__052_ } ->
           let bnds__051_ = ([] : _ Stdlib.List.t) in
           let bnds__051_ =
             let arg__053_ = Moon_sexp_conv.sexp_of_int tag_index__052_ in
             (S.List [ S.Atom "tag_index"; arg__053_ ] :: bnds__051_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Casted_constr" :: bnds__051_)
       | Map_elem { key = key__055_ } ->
           let bnds__054_ = ([] : _ Stdlib.List.t) in
           let bnds__054_ =
             let arg__056_ = Constant.sexp_of_t key__055_ in
             (S.List [ S.Atom "key"; arg__056_ ] :: bnds__054_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Map_elem" :: bnds__054_)
       | Error_constr_field { tag = tag__058_; arg_index = arg_index__060_ } ->
           let bnds__057_ = ([] : _ Stdlib.List.t) in
           let bnds__057_ =
             let arg__061_ = Moon_sexp_conv.sexp_of_int arg_index__060_ in
             (S.List [ S.Atom "arg_index"; arg__061_ ] :: bnds__057_
               : _ Stdlib.List.t)
           in
           let bnds__057_ =
             let arg__059_ = Basic_constr_info.sexp_of_constr_tag tag__058_ in
             (S.List [ S.Atom "tag"; arg__059_ ] :: bnds__057_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Error_constr_field" :: bnds__057_)
       | Codeunit_field { index = index__063_; rev = rev__065_ } ->
           let bnds__062_ = ([] : _ Stdlib.List.t) in
           let bnds__062_ =
             let arg__066_ = Moon_sexp_conv.sexp_of_bool rev__065_ in
             (S.List [ S.Atom "rev"; arg__066_ ] :: bnds__062_
               : _ Stdlib.List.t)
           in
           let bnds__062_ =
             let arg__064_ = Moon_sexp_conv.sexp_of_int index__063_ in
             (S.List [ S.Atom "index"; arg__064_ ] :: bnds__062_
               : _ Stdlib.List.t)
           in
           S.List (S.Atom "Codeunit_field" :: bnds__062_)
        : access -> S.t)

    let _ = sexp_of_t
    and _ = sexp_of_access
  end
end

include Path_def
module Map = Basic_mapf.Make (Path_def)
