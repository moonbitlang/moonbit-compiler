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


module Ty_ident = Basic_ty_ident
module Hashf = Basic_hashf

type int_kind =
  | I32_Int
  | I32_Char
  | I32_Bool
  | I32_Unit
  | I32_Byte
  | I32_Int16
  | I32_UInt16
  | I32_Tag
  | I32_Option_Char

include struct
  let _ = fun (_ : int_kind) -> ()

  let sexp_of_int_kind =
    (function
     | I32_Int -> S.Atom "I32_Int"
     | I32_Char -> S.Atom "I32_Char"
     | I32_Bool -> S.Atom "I32_Bool"
     | I32_Unit -> S.Atom "I32_Unit"
     | I32_Byte -> S.Atom "I32_Byte"
     | I32_Int16 -> S.Atom "I32_Int16"
     | I32_UInt16 -> S.Atom "I32_UInt16"
     | I32_Tag -> S.Atom "I32_Tag"
     | I32_Option_Char -> S.Atom "I32_Option_Char"
      : int_kind -> S.t)

  let _ = sexp_of_int_kind
end

type t =
  | I32 of { kind : int_kind [@ceh.ignore] }
  | I64
  | F32
  | F64
  | Ref of { tid : Ty_ident.t }
  | Ref_lazy_init of { tid : Ty_ident.t }
  | Ref_nullable of { tid : Ty_ident.t }
  | Ref_extern
  | Ref_string
  | Ref_bytes
  | Ref_func
  | Ref_any

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (function
     | I32 { kind = kind__002_ } ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__003_ = sexp_of_int_kind kind__002_ in
           (S.List [ S.Atom "kind"; arg__003_ ] :: bnds__001_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "I32" :: bnds__001_)
     | I64 -> S.Atom "I64"
     | F32 -> S.Atom "F32"
     | F64 -> S.Atom "F64"
     | Ref { tid = tid__005_ } ->
         let bnds__004_ = ([] : _ Stdlib.List.t) in
         let bnds__004_ =
           let arg__006_ = Ty_ident.sexp_of_t tid__005_ in
           (S.List [ S.Atom "tid"; arg__006_ ] :: bnds__004_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref" :: bnds__004_)
     | Ref_lazy_init { tid = tid__008_ } ->
         let bnds__007_ = ([] : _ Stdlib.List.t) in
         let bnds__007_ =
           let arg__009_ = Ty_ident.sexp_of_t tid__008_ in
           (S.List [ S.Atom "tid"; arg__009_ ] :: bnds__007_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_lazy_init" :: bnds__007_)
     | Ref_nullable { tid = tid__011_ } ->
         let bnds__010_ = ([] : _ Stdlib.List.t) in
         let bnds__010_ =
           let arg__012_ = Ty_ident.sexp_of_t tid__011_ in
           (S.List [ S.Atom "tid"; arg__012_ ] :: bnds__010_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_nullable" :: bnds__010_)
     | Ref_extern -> S.Atom "Ref_extern"
     | Ref_string -> S.Atom "Ref_string"
     | Ref_bytes -> S.Atom "Ref_bytes"
     | Ref_func -> S.Atom "Ref_func"
     | Ref_any -> S.Atom "Ref_any"
      : t -> S.t)

  let _ = sexp_of_t

  let equal =
    (fun a__013_ ->
       fun b__014_ ->
        if Stdlib.( == ) a__013_ b__014_ then true
        else
          match (a__013_, b__014_) with
          | I32 _a__015_, I32 _b__016_ -> true
          | I32 _, _ -> false
          | _, I32 _ -> false
          | I64, I64 -> true
          | I64, _ -> false
          | _, I64 -> false
          | F32, F32 -> true
          | F32, _ -> false
          | _, F32 -> false
          | F64, F64 -> true
          | F64, _ -> false
          | _, F64 -> false
          | Ref _a__017_, Ref _b__018_ ->
              Ty_ident.equal _a__017_.tid _b__018_.tid
          | Ref _, _ -> false
          | _, Ref _ -> false
          | Ref_lazy_init _a__019_, Ref_lazy_init _b__020_ ->
              Ty_ident.equal _a__019_.tid _b__020_.tid
          | Ref_lazy_init _, _ -> false
          | _, Ref_lazy_init _ -> false
          | Ref_nullable _a__021_, Ref_nullable _b__022_ ->
              Ty_ident.equal _a__021_.tid _b__022_.tid
          | Ref_nullable _, _ -> false
          | _, Ref_nullable _ -> false
          | Ref_extern, Ref_extern -> true
          | Ref_extern, _ -> false
          | _, Ref_extern -> false
          | Ref_string, Ref_string -> true
          | Ref_string, _ -> false
          | _, Ref_string -> false
          | Ref_bytes, Ref_bytes -> true
          | Ref_bytes, _ -> false
          | _, Ref_bytes -> false
          | Ref_func, Ref_func -> true
          | Ref_func, _ -> false
          | _, Ref_func -> false
          | Ref_any, Ref_any -> true
      : t -> t -> bool)

  let _ = equal

  let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | I32 _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            hsv
        | I64 -> Ppx_base.hash_fold_int hsv 1
        | F32 -> Ppx_base.hash_fold_int hsv 2
        | F64 -> Ppx_base.hash_fold_int hsv 3
        | Ref _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 4 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_lazy_init _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 5 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_nullable _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 6 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_extern -> Ppx_base.hash_fold_int hsv 7
        | Ref_string -> Ppx_base.hash_fold_int hsv 8
        | Ref_bytes -> Ppx_base.hash_fold_int hsv 9
        | Ref_func -> Ppx_base.hash_fold_int hsv 10
        | Ref_any -> Ppx_base.hash_fold_int hsv 11
      : Ppx_base.state -> t -> Ppx_base.state)

  let _ = hash_fold_t

  let (hash : t -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_t hsv arg)
    in
    fun x -> func x

  let _ = hash
end

let sexp_of_t t =
  match t with I32 { kind } -> sexp_of_int_kind kind | _ -> sexp_of_t t

type fn_sig = { params : t list; ret : t list }

include struct
  let _ = fun (_ : fn_sig) -> ()

  let sexp_of_fn_sig =
    (fun { params = params__024_; ret = ret__026_ } ->
       let bnds__023_ = ([] : _ Stdlib.List.t) in
       let bnds__023_ =
         let arg__027_ = Moon_sexp_conv.sexp_of_list sexp_of_t ret__026_ in
         (S.List [ S.Atom "ret"; arg__027_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       let bnds__023_ =
         let arg__025_ = Moon_sexp_conv.sexp_of_list sexp_of_t params__024_ in
         (S.List [ S.Atom "params"; arg__025_ ] :: bnds__023_ : _ Stdlib.List.t)
       in
       S.List bnds__023_
      : fn_sig -> S.t)

  let _ = sexp_of_fn_sig

  let equal_fn_sig =
    (fun a__028_ ->
       fun b__029_ ->
        if Stdlib.( == ) a__028_ b__029_ then true
        else
          Stdlib.( && )
            (Ppx_base.equal_list
               (fun a__030_ -> fun b__031_ -> equal a__030_ b__031_)
               a__028_.params b__029_.params)
            (Ppx_base.equal_list
               (fun a__032_ -> fun b__033_ -> equal a__032_ b__033_)
               a__028_.ret b__029_.ret)
      : fn_sig -> fn_sig -> bool)

  let _ = equal_fn_sig

  let (hash_fold_fn_sig : Ppx_base.state -> fn_sig -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       Ppx_base.hash_fold_list hash_fold_t hsv arg.params
     in
     Ppx_base.hash_fold_list hash_fold_t hsv arg.ret

  let _ = hash_fold_fn_sig

  let (hash_fn_sig : fn_sig -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_fn_sig hsv arg)
    in
    fun x -> func x

  let _ = hash_fn_sig
end

let sexp_of_fn_sig (t : fn_sig) =
  match t with
  | { params; ret = ret :: [] } ->
      let params = Basic_lst.map params sexp_of_t in
      let ret = sexp_of_t ret in
      (List
         (List.cons
            (List
               (List.cons
                  (Atom "params" : S.t)
                  ([ List (params : S.t list) ] : S.t list))
              : S.t)
            ([ List (List.cons (Atom "ret" : S.t) ([ ret ] : S.t list)) ]
              : S.t list))
        : S.t)
  | _ -> sexp_of_fn_sig t

type def =
  | Ref_array of { elem : t }
  | Ref_struct of { fields : (t * bool) list }
  | Ref_late_init_struct of { fields : t list }
  | Ref_constructor of { args : (t * bool) list [@list] }
  | Ref_closure_abstract of { fn_sig : fn_sig }
  | Ref_object of { methods : fn_sig list }
  | Ref_concrete_object of { abstract_obj_tid : Basic_ty_ident.t; self : t }
  | Ref_closure of { fn_sig_tid : Ty_ident.t; captures : t list }

include struct
  let _ = fun (_ : def) -> ()

  let sexp_of_def =
    (function
     | Ref_array { elem = elem__035_ } ->
         let bnds__034_ = ([] : _ Stdlib.List.t) in
         let bnds__034_ =
           let arg__036_ = sexp_of_t elem__035_ in
           (S.List [ S.Atom "elem"; arg__036_ ] :: bnds__034_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_array" :: bnds__034_)
     | Ref_struct { fields = fields__038_ } ->
         let bnds__037_ = ([] : _ Stdlib.List.t) in
         let bnds__037_ =
           let arg__039_ =
             Moon_sexp_conv.sexp_of_list
               (fun (arg0__040_, arg1__041_) ->
                 let res0__042_ = sexp_of_t arg0__040_
                 and res1__043_ = Moon_sexp_conv.sexp_of_bool arg1__041_ in
                 S.List [ res0__042_; res1__043_ ])
               fields__038_
           in
           (S.List [ S.Atom "fields"; arg__039_ ] :: bnds__037_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_struct" :: bnds__037_)
     | Ref_late_init_struct { fields = fields__045_ } ->
         let bnds__044_ = ([] : _ Stdlib.List.t) in
         let bnds__044_ =
           let arg__046_ = Moon_sexp_conv.sexp_of_list sexp_of_t fields__045_ in
           (S.List [ S.Atom "fields"; arg__046_ ] :: bnds__044_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_late_init_struct" :: bnds__044_)
     | Ref_constructor { args = args__049_ } ->
         let bnds__047_ = ([] : _ Stdlib.List.t) in
         let bnds__047_ =
           if match args__049_ with [] -> true | _ -> false then bnds__047_
           else
             let arg__055_ =
               (Moon_sexp_conv.sexp_of_list (fun (arg0__050_, arg1__051_) ->
                    let res0__052_ = sexp_of_t arg0__050_
                    and res1__053_ = Moon_sexp_conv.sexp_of_bool arg1__051_ in
                    S.List [ res0__052_; res1__053_ ]))
                 args__049_
             in
             let bnd__054_ = S.List [ S.Atom "args"; arg__055_ ] in
             (bnd__054_ :: bnds__047_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_constructor" :: bnds__047_)
     | Ref_closure_abstract { fn_sig = fn_sig__057_ } ->
         let bnds__056_ = ([] : _ Stdlib.List.t) in
         let bnds__056_ =
           let arg__058_ = sexp_of_fn_sig fn_sig__057_ in
           (S.List [ S.Atom "fn_sig"; arg__058_ ] :: bnds__056_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_closure_abstract" :: bnds__056_)
     | Ref_object { methods = methods__060_ } ->
         let bnds__059_ = ([] : _ Stdlib.List.t) in
         let bnds__059_ =
           let arg__061_ =
             Moon_sexp_conv.sexp_of_list sexp_of_fn_sig methods__060_
           in
           (S.List [ S.Atom "methods"; arg__061_ ] :: bnds__059_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_object" :: bnds__059_)
     | Ref_concrete_object
         { abstract_obj_tid = abstract_obj_tid__063_; self = self__065_ } ->
         let bnds__062_ = ([] : _ Stdlib.List.t) in
         let bnds__062_ =
           let arg__066_ = sexp_of_t self__065_ in
           (S.List [ S.Atom "self"; arg__066_ ] :: bnds__062_ : _ Stdlib.List.t)
         in
         let bnds__062_ =
           let arg__064_ = Basic_ty_ident.sexp_of_t abstract_obj_tid__063_ in
           (S.List [ S.Atom "abstract_obj_tid"; arg__064_ ] :: bnds__062_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_concrete_object" :: bnds__062_)
     | Ref_closure { fn_sig_tid = fn_sig_tid__068_; captures = captures__070_ }
       ->
         let bnds__067_ = ([] : _ Stdlib.List.t) in
         let bnds__067_ =
           let arg__071_ =
             Moon_sexp_conv.sexp_of_list sexp_of_t captures__070_
           in
           (S.List [ S.Atom "captures"; arg__071_ ] :: bnds__067_
             : _ Stdlib.List.t)
         in
         let bnds__067_ =
           let arg__069_ = Ty_ident.sexp_of_t fn_sig_tid__068_ in
           (S.List [ S.Atom "fn_sig_tid"; arg__069_ ] :: bnds__067_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_closure" :: bnds__067_)
      : def -> S.t)

  let _ = sexp_of_def
end

let i32_int = I32 { kind = I32_Int }
let i32_char = I32 { kind = I32_Char }
let i32_bool = I32 { kind = I32_Bool }
let i32_unit = I32 { kind = I32_Unit }
let i32_byte = I32 { kind = I32_Byte }
let i32_int16 = I32 { kind = I32_Int16 }
let i32_uint16 = I32 { kind = I32_UInt16 }
let i32_tag = I32 { kind = I32_Tag }
let i32_option_char = I32 { kind = I32_Option_Char }
let tid_string = Ty_ident.of_string "moonbit.string"
let tid_bytes = Ty_ident.of_string "moonbit.bytes"
let tid_enum = Ty_ident.of_string "moonbit.enum"
let tid_array_i32 = Ty_ident.of_string "moonbit.array_i32"
let tid_array_i64 = Ty_ident.of_string "moonbit.array_i64"
let tid_array_f64 = Ty_ident.of_string "moonbit.array_f64"
let tag_name = "$moonbit.tag"
let ref_enum = Ref { tid = tid_enum }
let ref_array_i32 = Ref { tid = tid_array_i32 }
let ref_array_i64 = Ref { tid = tid_array_i64 }
let ref_array_f64 = Ref { tid = tid_array_f64 }
let def_array_i32 = Ref_array { elem = I32 { kind = I32_Int } }
let def_array_i64 = Ref_array { elem = I64 }
let def_array_f64 = Ref_array { elem = F64 }

let predefs =
  [
    (tid_array_i32, def_array_i32);
    (tid_array_i64, def_array_i64);
    (tid_array_f64, def_array_f64);
  ]

module FnSigHash = Hashf.Make (struct
  type t = fn_sig

  include struct
    let _ = fun (_ : t) -> ()
    let sexp_of_t = (sexp_of_fn_sig : t -> S.t)
    let _ = sexp_of_t
    let equal = (equal_fn_sig : t -> t -> bool)
    let _ = equal

    let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) = hash_fold_fn_sig

    and (hash : t -> Ppx_base.hash_value) =
      let func = hash_fn_sig in
      fun x -> func x

    let _ = hash_fold_t
    and _ = hash
  end
end)

module Hash = Hashf.Make (struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let equal = equal
  let hash = hash
end)

type type_defs = def Ty_ident.Hash.t

include struct
  let _ = fun (_ : type_defs) -> ()

  let sexp_of_type_defs =
    (fun x__074_ -> Ty_ident.Hash.sexp_of_t sexp_of_def x__074_
      : type_defs -> S.t)

  let _ = sexp_of_type_defs
end

type type_defs_with_context = {
  defs : type_defs;
  fn_sig_tbl : Ty_ident.t FnSigHash.t;
}

include struct
  let _ = fun (_ : type_defs_with_context) -> ()

  let sexp_of_type_defs_with_context =
    (fun { defs = defs__076_; fn_sig_tbl = fn_sig_tbl__078_ } ->
       let bnds__075_ = ([] : _ Stdlib.List.t) in
       let bnds__075_ =
         let arg__079_ =
           FnSigHash.sexp_of_t Ty_ident.sexp_of_t fn_sig_tbl__078_
         in
         (S.List [ S.Atom "fn_sig_tbl"; arg__079_ ] :: bnds__075_
           : _ Stdlib.List.t)
       in
       let bnds__075_ =
         let arg__077_ = sexp_of_type_defs defs__076_ in
         (S.List [ S.Atom "defs"; arg__077_ ] :: bnds__075_ : _ Stdlib.List.t)
       in
       S.List bnds__075_
      : type_defs_with_context -> S.t)

  let _ = sexp_of_type_defs_with_context
end
