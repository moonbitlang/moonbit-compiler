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
  | U32
  | I64
  | U64
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
  | Raw_func of { tid : Ty_ident.t }

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
     | U32 -> S.Atom "U32"
     | I64 -> S.Atom "I64"
     | U64 -> S.Atom "U64"
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
     | Raw_func { tid = tid__014_ } ->
         let bnds__013_ = ([] : _ Stdlib.List.t) in
         let bnds__013_ =
           let arg__015_ = Ty_ident.sexp_of_t tid__014_ in
           (S.List [ S.Atom "tid"; arg__015_ ] :: bnds__013_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Raw_func" :: bnds__013_)
      : t -> S.t)

  let _ = sexp_of_t

  let equal =
    (fun a__016_ ->
       fun b__017_ ->
        if Stdlib.( == ) a__016_ b__017_ then true
        else
          match (a__016_, b__017_) with
          | I32 _a__018_, I32 _b__019_ -> true
          | I32 _, _ -> false
          | _, I32 _ -> false
          | U32, U32 -> true
          | U32, _ -> false
          | _, U32 -> false
          | I64, I64 -> true
          | I64, _ -> false
          | _, I64 -> false
          | U64, U64 -> true
          | U64, _ -> false
          | _, U64 -> false
          | F32, F32 -> true
          | F32, _ -> false
          | _, F32 -> false
          | F64, F64 -> true
          | F64, _ -> false
          | _, F64 -> false
          | Ref _a__020_, Ref _b__021_ ->
              Ty_ident.equal _a__020_.tid _b__021_.tid
          | Ref _, _ -> false
          | _, Ref _ -> false
          | Ref_lazy_init _a__022_, Ref_lazy_init _b__023_ ->
              Ty_ident.equal _a__022_.tid _b__023_.tid
          | Ref_lazy_init _, _ -> false
          | _, Ref_lazy_init _ -> false
          | Ref_nullable _a__024_, Ref_nullable _b__025_ ->
              Ty_ident.equal _a__024_.tid _b__025_.tid
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
          | Ref_any, _ -> false
          | _, Ref_any -> false
          | Raw_func _a__026_, Raw_func _b__027_ ->
              Ty_ident.equal _a__026_.tid _b__027_.tid
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
        | U32 -> Ppx_base.hash_fold_int hsv 1
        | I64 -> Ppx_base.hash_fold_int hsv 2
        | U64 -> Ppx_base.hash_fold_int hsv 3
        | F32 -> Ppx_base.hash_fold_int hsv 4
        | F64 -> Ppx_base.hash_fold_int hsv 5
        | Ref _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 6 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_lazy_init _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 7 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_nullable _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 8 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
        | Ref_extern -> Ppx_base.hash_fold_int hsv 9
        | Ref_string -> Ppx_base.hash_fold_int hsv 10
        | Ref_bytes -> Ppx_base.hash_fold_int hsv 11
        | Ref_func -> Ppx_base.hash_fold_int hsv 12
        | Ref_any -> Ppx_base.hash_fold_int hsv 13
        | Raw_func _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 14 in
            let hsv = hsv in
            Ty_ident.hash_fold_t hsv _ir.tid
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

let i32_int = I32 { kind = I32_Int }
let i32_char = I32 { kind = I32_Char }
let i32_bool = I32 { kind = I32_Bool }
let i32_unit = I32 { kind = I32_Unit }
let i32_byte = I32 { kind = I32_Byte }
let i32_int16 = I32 { kind = I32_Int16 }
let i32_uint16 = I32 { kind = I32_UInt16 }
let i32_tag = I32 { kind = I32_Tag }
let i32_option_char = I32 { kind = I32_Option_Char }

type return_type =
  | Ret_nothing
  | Ret_single of t
  | Ret_error of { ok_ty : t; err_ty : t }

include struct
  let _ = fun (_ : return_type) -> ()

  let sexp_of_return_type =
    (function
     | Ret_nothing -> S.Atom "Ret_nothing"
     | Ret_single arg0__028_ ->
         let res0__029_ = sexp_of_t arg0__028_ in
         S.List [ S.Atom "Ret_single"; res0__029_ ]
     | Ret_error { ok_ty = ok_ty__031_; err_ty = err_ty__033_ } ->
         let bnds__030_ = ([] : _ Stdlib.List.t) in
         let bnds__030_ =
           let arg__034_ = sexp_of_t err_ty__033_ in
           (S.List [ S.Atom "err_ty"; arg__034_ ] :: bnds__030_
             : _ Stdlib.List.t)
         in
         let bnds__030_ =
           let arg__032_ = sexp_of_t ok_ty__031_ in
           (S.List [ S.Atom "ok_ty"; arg__032_ ] :: bnds__030_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ret_error" :: bnds__030_)
      : return_type -> S.t)

  let _ = sexp_of_return_type

  let equal_return_type =
    (fun a__035_ ->
       fun b__036_ ->
        if Stdlib.( == ) a__035_ b__036_ then true
        else
          match (a__035_, b__036_) with
          | Ret_nothing, Ret_nothing -> true
          | Ret_nothing, _ -> false
          | _, Ret_nothing -> false
          | Ret_single _a__037_, Ret_single _b__038_ -> equal _a__037_ _b__038_
          | Ret_single _, _ -> false
          | _, Ret_single _ -> false
          | Ret_error _a__039_, Ret_error _b__040_ ->
              Stdlib.( && )
                (equal _a__039_.ok_ty _b__040_.ok_ty)
                (equal _a__039_.err_ty _b__040_.err_ty)
      : return_type -> return_type -> bool)

  let _ = equal_return_type

  let (hash_fold_return_type : Ppx_base.state -> return_type -> Ppx_base.state)
      =
    (fun hsv ->
       fun arg ->
        match arg with
        | Ret_nothing -> Ppx_base.hash_fold_int hsv 0
        | Ret_single _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_t hsv _a0
        | Ret_error _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv =
              let hsv = hsv in
              hash_fold_t hsv _ir.ok_ty
            in
            hash_fold_t hsv _ir.err_ty
      : Ppx_base.state -> return_type -> Ppx_base.state)

  let _ = hash_fold_return_type

  let (hash_return_type : return_type -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_return_type hsv arg)
    in
    fun x -> func x

  let _ = hash_return_type
end

type fn_sig = { params : t list; ret : return_type }

include struct
  let _ = fun (_ : fn_sig) -> ()

  let sexp_of_fn_sig =
    (fun { params = params__042_; ret = ret__044_ } ->
       let bnds__041_ = ([] : _ Stdlib.List.t) in
       let bnds__041_ =
         let arg__045_ = sexp_of_return_type ret__044_ in
         (S.List [ S.Atom "ret"; arg__045_ ] :: bnds__041_ : _ Stdlib.List.t)
       in
       let bnds__041_ =
         let arg__043_ = Moon_sexp_conv.sexp_of_list sexp_of_t params__042_ in
         (S.List [ S.Atom "params"; arg__043_ ] :: bnds__041_ : _ Stdlib.List.t)
       in
       S.List bnds__041_
      : fn_sig -> S.t)

  let _ = sexp_of_fn_sig

  let equal_fn_sig =
    (fun a__046_ ->
       fun b__047_ ->
        if Stdlib.( == ) a__046_ b__047_ then true
        else
          Stdlib.( && )
            (Ppx_base.equal_list
               (fun a__048_ -> fun b__049_ -> equal a__048_ b__049_)
               a__046_.params b__047_.params)
            (equal_return_type a__046_.ret b__047_.ret)
      : fn_sig -> fn_sig -> bool)

  let _ = equal_fn_sig

  let (hash_fold_fn_sig : Ppx_base.state -> fn_sig -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       Ppx_base.hash_fold_list hash_fold_t hsv arg.params
     in
     hash_fold_return_type hsv arg.ret

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
  | { params; ret = Ret_single ret } ->
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
  | Ref_concrete_object of { abstract_obj_tid : Ty_ident.t; self : t }
  | Ref_closure of { fn_sig_tid : Ty_ident.t; captures : t list }

include struct
  let _ = fun (_ : def) -> ()

  let sexp_of_def =
    (function
     | Ref_array { elem = elem__051_ } ->
         let bnds__050_ = ([] : _ Stdlib.List.t) in
         let bnds__050_ =
           let arg__052_ = sexp_of_t elem__051_ in
           (S.List [ S.Atom "elem"; arg__052_ ] :: bnds__050_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_array" :: bnds__050_)
     | Ref_struct { fields = fields__054_ } ->
         let bnds__053_ = ([] : _ Stdlib.List.t) in
         let bnds__053_ =
           let arg__055_ =
             Moon_sexp_conv.sexp_of_list
               (fun (arg0__056_, arg1__057_) ->
                 let res0__058_ = sexp_of_t arg0__056_
                 and res1__059_ = Moon_sexp_conv.sexp_of_bool arg1__057_ in
                 S.List [ res0__058_; res1__059_ ])
               fields__054_
           in
           (S.List [ S.Atom "fields"; arg__055_ ] :: bnds__053_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_struct" :: bnds__053_)
     | Ref_late_init_struct { fields = fields__061_ } ->
         let bnds__060_ = ([] : _ Stdlib.List.t) in
         let bnds__060_ =
           let arg__062_ = Moon_sexp_conv.sexp_of_list sexp_of_t fields__061_ in
           (S.List [ S.Atom "fields"; arg__062_ ] :: bnds__060_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_late_init_struct" :: bnds__060_)
     | Ref_constructor { args = args__065_ } ->
         let bnds__063_ = ([] : _ Stdlib.List.t) in
         let bnds__063_ =
           if match args__065_ with [] -> true | _ -> false then bnds__063_
           else
             let arg__071_ =
               (Moon_sexp_conv.sexp_of_list (fun (arg0__066_, arg1__067_) ->
                    let res0__068_ = sexp_of_t arg0__066_
                    and res1__069_ = Moon_sexp_conv.sexp_of_bool arg1__067_ in
                    S.List [ res0__068_; res1__069_ ]))
                 args__065_
             in
             let bnd__070_ = S.List [ S.Atom "args"; arg__071_ ] in
             (bnd__070_ :: bnds__063_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_constructor" :: bnds__063_)
     | Ref_closure_abstract { fn_sig = fn_sig__073_ } ->
         let bnds__072_ = ([] : _ Stdlib.List.t) in
         let bnds__072_ =
           let arg__074_ = sexp_of_fn_sig fn_sig__073_ in
           (S.List [ S.Atom "fn_sig"; arg__074_ ] :: bnds__072_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_closure_abstract" :: bnds__072_)
     | Ref_object { methods = methods__076_ } ->
         let bnds__075_ = ([] : _ Stdlib.List.t) in
         let bnds__075_ =
           let arg__077_ =
             Moon_sexp_conv.sexp_of_list sexp_of_fn_sig methods__076_
           in
           (S.List [ S.Atom "methods"; arg__077_ ] :: bnds__075_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_object" :: bnds__075_)
     | Ref_concrete_object
         { abstract_obj_tid = abstract_obj_tid__079_; self = self__081_ } ->
         let bnds__078_ = ([] : _ Stdlib.List.t) in
         let bnds__078_ =
           let arg__082_ = sexp_of_t self__081_ in
           (S.List [ S.Atom "self"; arg__082_ ] :: bnds__078_ : _ Stdlib.List.t)
         in
         let bnds__078_ =
           let arg__080_ = Ty_ident.sexp_of_t abstract_obj_tid__079_ in
           (S.List [ S.Atom "abstract_obj_tid"; arg__080_ ] :: bnds__078_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_concrete_object" :: bnds__078_)
     | Ref_closure { fn_sig_tid = fn_sig_tid__084_; captures = captures__086_ }
       ->
         let bnds__083_ = ([] : _ Stdlib.List.t) in
         let bnds__083_ =
           let arg__087_ =
             Moon_sexp_conv.sexp_of_list sexp_of_t captures__086_
           in
           (S.List [ S.Atom "captures"; arg__087_ ] :: bnds__083_
             : _ Stdlib.List.t)
         in
         let bnds__083_ =
           let arg__085_ = Ty_ident.sexp_of_t fn_sig_tid__084_ in
           (S.List [ S.Atom "fn_sig_tid"; arg__085_ ] :: bnds__083_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Ref_closure" :: bnds__083_)
      : def -> S.t)

  let _ = sexp_of_def
end

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
    (fun x__090_ -> Ty_ident.Hash.sexp_of_t sexp_of_def x__090_
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
    (fun { defs = defs__092_; fn_sig_tbl = fn_sig_tbl__094_ } ->
       let bnds__091_ = ([] : _ Stdlib.List.t) in
       let bnds__091_ =
         let arg__095_ =
           FnSigHash.sexp_of_t Ty_ident.sexp_of_t fn_sig_tbl__094_
         in
         (S.List [ S.Atom "fn_sig_tbl"; arg__095_ ] :: bnds__091_
           : _ Stdlib.List.t)
       in
       let bnds__091_ =
         let arg__093_ = sexp_of_type_defs defs__092_ in
         (S.List [ S.Atom "defs"; arg__093_ ] :: bnds__091_ : _ Stdlib.List.t)
       in
       S.List bnds__091_
      : type_defs_with_context -> S.t)

  let _ = sexp_of_type_defs_with_context
end
