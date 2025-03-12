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


module Itype = Dwarfsm_itype

type binder = { id : string option; mutable index : int }

type var =
  | Unresolve of string
  | Resolved of { var_name : string; [@ceh.ignore] index : int }

include struct
  let _ = fun (_ : var) -> ()

  let equal_var =
    (fun a__001_ ->
       fun b__002_ ->
        if Stdlib.( == ) a__001_ b__002_ then true
        else
          match (a__001_, b__002_) with
          | Unresolve _a__003_, Unresolve _b__004_ ->
              Stdlib.( = ) (_a__003_ : string) _b__004_
          | Unresolve _, _ -> false
          | _, Unresolve _ -> false
          | Resolved _a__005_, Resolved _b__006_ ->
              Stdlib.( = ) (_a__005_.index : int) _b__006_.index
      : var -> var -> bool)

  let _ = equal_var

  let (hash_fold_var : Ppx_base.state -> var -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Unresolve _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            Ppx_base.hash_fold_string hsv _a0
        | Resolved _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv =
              let hsv = hsv in
              hsv
            in
            Ppx_base.hash_fold_int hsv _ir.index
      : Ppx_base.state -> var -> Ppx_base.state)

  let _ = hash_fold_var

  let (hash_var : var -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_var hsv arg)
    in
    fun x -> func x

  let _ = hash_var
end

type label = string option
type typeidx = { mutable var : var }

include struct
  let _ = fun (_ : typeidx) -> ()

  let equal_typeidx =
    (fun a__007_ ->
       fun b__008_ ->
        if Stdlib.( == ) a__007_ b__008_ then true
        else equal_var a__007_.var b__008_.var
      : typeidx -> typeidx -> bool)

  let _ = equal_typeidx
end

type funcidx = { mutable var : var }

include struct
  let _ = fun (_ : funcidx) -> ()

  let equal_funcidx =
    (fun a__009_ ->
       fun b__010_ ->
        if Stdlib.( == ) a__009_ b__010_ then true
        else equal_var a__009_.var b__010_.var
      : funcidx -> funcidx -> bool)

  let _ = equal_funcidx
end

type tableidx = { mutable var : var }

include struct
  let _ = fun (_ : tableidx) -> ()

  let equal_tableidx =
    (fun a__011_ ->
       fun b__012_ ->
        if Stdlib.( == ) a__011_ b__012_ then true
        else equal_var a__011_.var b__012_.var
      : tableidx -> tableidx -> bool)

  let _ = equal_tableidx
end

type memidx = { mutable var : var }

include struct
  let _ = fun (_ : memidx) -> ()

  let equal_memidx =
    (fun a__013_ ->
       fun b__014_ ->
        if Stdlib.( == ) a__013_ b__014_ then true
        else equal_var a__013_.var b__014_.var
      : memidx -> memidx -> bool)

  let _ = equal_memidx
end

type globalidx = { mutable var : var }

include struct
  let _ = fun (_ : globalidx) -> ()

  let equal_globalidx =
    (fun a__015_ ->
       fun b__016_ ->
        if Stdlib.( == ) a__015_ b__016_ then true
        else equal_var a__015_.var b__016_.var
      : globalidx -> globalidx -> bool)

  let _ = equal_globalidx
end

type dataidx = { mutable var : var }

include struct
  let _ = fun (_ : dataidx) -> ()

  let equal_dataidx =
    (fun a__017_ ->
       fun b__018_ ->
        if Stdlib.( == ) a__017_ b__018_ then true
        else equal_var a__017_.var b__018_.var
      : dataidx -> dataidx -> bool)

  let _ = equal_dataidx
end

type localidx = { mutable var : var }

include struct
  let _ = fun (_ : localidx) -> ()

  let equal_localidx =
    (fun a__019_ ->
       fun b__020_ ->
        if Stdlib.( == ) a__019_ b__020_ then true
        else equal_var a__019_.var b__020_.var
      : localidx -> localidx -> bool)

  let _ = equal_localidx
end

type labelidx = { mutable var : var }

include struct
  let _ = fun (_ : labelidx) -> ()

  let equal_labelidx =
    (fun a__021_ ->
       fun b__022_ ->
        if Stdlib.( == ) a__021_ b__022_ then true
        else equal_var a__021_.var b__022_.var
      : labelidx -> labelidx -> bool)

  let _ = equal_labelidx
end

type fieldidx = { mutable var : var }

include struct
  let _ = fun (_ : fieldidx) -> ()

  let equal_fieldidx =
    (fun a__023_ ->
       fun b__024_ ->
        if Stdlib.( == ) a__023_ b__024_ then true
        else equal_var a__023_.var b__024_.var
      : fieldidx -> fieldidx -> bool)

  let _ = equal_fieldidx
end

type tagidx = { mutable var : var }

include struct
  let _ = fun (_ : tagidx) -> ()

  let equal_tagidx =
    (fun a__025_ ->
       fun b__026_ ->
        if Stdlib.( == ) a__025_ b__026_ then true
        else equal_var a__025_.var b__026_.var
      : tagidx -> tagidx -> bool)

  let _ = equal_tagidx
end

let hash_fold_typeidx state (i : typeidx) = hash_fold_var state i.var

type numtype = I32 | I64 | F32 | F64

include struct
  let _ = fun (_ : numtype) -> ()
  let equal_numtype = (Stdlib.( = ) : numtype -> numtype -> bool)
  let _ = equal_numtype

  let (hash_fold_numtype : Ppx_base.state -> numtype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv
          (match arg with I32 -> 0 | I64 -> 1 | F32 -> 2 | F64 -> 3)
      : Ppx_base.state -> numtype -> Ppx_base.state)

  let _ = hash_fold_numtype

  let (hash_numtype : numtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_numtype hsv arg)
    in
    fun x -> func x

  let _ = hash_numtype
end

type vectype = V128

include struct
  let _ = fun (_ : vectype) -> ()
  let equal_vectype = (Stdlib.( = ) : vectype -> vectype -> bool)
  let _ = equal_vectype

  let (hash_fold_vectype : Ppx_base.state -> vectype -> Ppx_base.state) =
    (fun hsv -> fun arg -> match arg with V128 -> hsv
      : Ppx_base.state -> vectype -> Ppx_base.state)

  let _ = hash_fold_vectype

  let (hash_vectype : vectype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_vectype hsv arg)
    in
    fun x -> func x

  let _ = hash_vectype
end

type absheaptype =
  | Any
  | Eq
  | I31
  | Struct
  | Array
  | None
  | Func
  | NoFunc
  | Extern
  | NoExtern

include struct
  let _ = fun (_ : absheaptype) -> ()
  let equal_absheaptype = (Stdlib.( = ) : absheaptype -> absheaptype -> bool)
  let _ = equal_absheaptype

  let (hash_fold_absheaptype : Ppx_base.state -> absheaptype -> Ppx_base.state)
      =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv
          (match arg with
          | Any -> 0
          | Eq -> 1
          | I31 -> 2
          | Struct -> 3
          | Array -> 4
          | None -> 5
          | Func -> 6
          | NoFunc -> 7
          | Extern -> 8
          | NoExtern -> 9)
      : Ppx_base.state -> absheaptype -> Ppx_base.state)

  let _ = hash_fold_absheaptype

  let (hash_absheaptype : absheaptype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_absheaptype hsv arg)
    in
    fun x -> func x

  let _ = hash_absheaptype
end

type heaptype = Type of typeidx | Absheaptype of absheaptype

include struct
  let _ = fun (_ : heaptype) -> ()

  let equal_heaptype =
    (fun a__033_ ->
       fun b__034_ ->
        if Stdlib.( == ) a__033_ b__034_ then true
        else
          match (a__033_, b__034_) with
          | Type _a__035_, Type _b__036_ -> equal_typeidx _a__035_ _b__036_
          | Type _, _ -> false
          | _, Type _ -> false
          | Absheaptype _a__037_, Absheaptype _b__038_ ->
              equal_absheaptype _a__037_ _b__038_
      : heaptype -> heaptype -> bool)

  let _ = equal_heaptype

  let (hash_fold_heaptype : Ppx_base.state -> heaptype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Type _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            hash_fold_typeidx hsv _a0
        | Absheaptype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_absheaptype hsv _a0
      : Ppx_base.state -> heaptype -> Ppx_base.state)

  let _ = hash_fold_heaptype

  let (hash_heaptype : heaptype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_heaptype hsv arg)
    in
    fun x -> func x

  let _ = hash_heaptype
end

type null = Nullable | NonNull

include struct
  let _ = fun (_ : null) -> ()
  let equal_null = (Stdlib.( = ) : null -> null -> bool)
  let _ = equal_null

  let (hash_fold_null : Ppx_base.state -> null -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv
          (match arg with Nullable -> 0 | NonNull -> 1)
      : Ppx_base.state -> null -> Ppx_base.state)

  let _ = hash_fold_null

  let (hash_null : null -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_null hsv arg)
    in
    fun x -> func x

  let _ = hash_null
end

type reftype = Ref of null * heaptype

include struct
  let _ = fun (_ : reftype) -> ()

  let equal_reftype =
    (fun a__041_ ->
       fun b__042_ ->
        if Stdlib.( == ) a__041_ b__042_ then true
        else
          match (a__041_, b__042_) with
          | Ref (_a__043_, _a__045_), Ref (_b__044_, _b__046_) ->
              Stdlib.( && )
                (equal_null _a__043_ _b__044_)
                (equal_heaptype _a__045_ _b__046_)
      : reftype -> reftype -> bool)

  let _ = equal_reftype

  let (hash_fold_reftype : Ppx_base.state -> reftype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Ref (_a0, _a1) ->
            let hsv = hsv in
            let hsv =
              let hsv = hsv in
              hash_fold_null hsv _a0
            in
            hash_fold_heaptype hsv _a1
      : Ppx_base.state -> reftype -> Ppx_base.state)

  let _ = hash_fold_reftype

  let (hash_reftype : reftype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_reftype hsv arg)
    in
    fun x -> func x

  let _ = hash_reftype
end

type valtype = Numtype of numtype | Vectype of vectype | Reftype of reftype

include struct
  let _ = fun (_ : valtype) -> ()

  let equal_valtype =
    (fun a__047_ ->
       fun b__048_ ->
        if Stdlib.( == ) a__047_ b__048_ then true
        else
          match (a__047_, b__048_) with
          | Numtype _a__049_, Numtype _b__050_ ->
              equal_numtype _a__049_ _b__050_
          | Numtype _, _ -> false
          | _, Numtype _ -> false
          | Vectype _a__051_, Vectype _b__052_ ->
              equal_vectype _a__051_ _b__052_
          | Vectype _, _ -> false
          | _, Vectype _ -> false
          | Reftype _a__053_, Reftype _b__054_ ->
              equal_reftype _a__053_ _b__054_
      : valtype -> valtype -> bool)

  let _ = equal_valtype

  let (hash_fold_valtype : Ppx_base.state -> valtype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Numtype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            hash_fold_numtype hsv _a0
        | Vectype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_vectype hsv _a0
        | Reftype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv = hsv in
            hash_fold_reftype hsv _a0
      : Ppx_base.state -> valtype -> Ppx_base.state)

  let _ = hash_fold_valtype

  let (hash_valtype : valtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_valtype hsv arg)
    in
    fun x -> func x

  let _ = hash_valtype
end

type local = {
  id : binder; [@ceh.ignore]
  source_name : string option; [@ceh.ignore]
  type_ : valtype;
  source_type : Itype.t option; [@ceh.ignore]
}

include struct
  let _ = fun (_ : local) -> ()

  let equal_local =
    (fun a__055_ ->
       fun b__056_ ->
        if Stdlib.( == ) a__055_ b__056_ then true
        else equal_valtype a__055_.type_ b__056_.type_
      : local -> local -> bool)

  let _ = equal_local

  let (hash_fold_local : Ppx_base.state -> local -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv =
         let hsv =
           let hsv = hsv in
           hsv
         in
         hsv
       in
       hash_fold_valtype hsv arg.type_
     in
     hsv

  let _ = hash_fold_local

  let (hash_local : local -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_local hsv arg)
    in
    fun x -> func x

  let _ = hash_local
end

type param = local

include struct
  let _ = fun (_ : param) -> ()
  let equal_param = (equal_local : param -> param -> bool)
  let _ = equal_param

  let (hash_fold_param : Ppx_base.state -> param -> Ppx_base.state) =
    hash_fold_local

  and (hash_param : param -> Ppx_base.hash_value) =
    let func = hash_local in
    fun x -> func x

  let _ = hash_fold_param
  and _ = hash_param
end

type result = valtype

include struct
  let _ = fun (_ : result) -> ()
  let equal_result = (equal_valtype : result -> result -> bool)
  let _ = equal_result

  let (hash_fold_result : Ppx_base.state -> result -> Ppx_base.state) =
    hash_fold_valtype

  and (hash_result : result -> Ppx_base.hash_value) =
    let func = hash_valtype in
    fun x -> func x

  let _ = hash_fold_result
  and _ = hash_result
end

type functype = Func of param list * result list

include struct
  let _ = fun (_ : functype) -> ()

  let equal_functype =
    (fun a__061_ ->
       fun b__062_ ->
        if Stdlib.( == ) a__061_ b__062_ then true
        else
          match (a__061_, b__062_) with
          | Func (_a__063_, _a__065_), Func (_b__064_, _b__066_) ->
              Stdlib.( && )
                (Ppx_base.equal_list
                   (fun a__067_ ->
                     fun (b__068_ [@merlin.hide]) ->
                      (equal_param a__067_ b__068_ [@merlin.hide]))
                   _a__063_ _b__064_)
                (Ppx_base.equal_list
                   (fun a__069_ ->
                     fun (b__070_ [@merlin.hide]) ->
                      (equal_result a__069_ b__070_ [@merlin.hide]))
                   _a__065_ _b__066_)
      : functype -> functype -> bool)

  let _ = equal_functype

  let (hash_fold_functype : Ppx_base.state -> functype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Func (_a0, _a1) ->
            let hsv = hsv in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_list hash_fold_param hsv _a0
            in
            Ppx_base.hash_fold_list hash_fold_result hsv _a1
      : Ppx_base.state -> functype -> Ppx_base.state)

  let _ = hash_fold_functype

  let (hash_functype : functype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_functype hsv arg)
    in
    fun x -> func x

  let _ = hash_functype
end

type packedtype = I8 | I16

include struct
  let _ = fun (_ : packedtype) -> ()
  let equal_packedtype = (Stdlib.( = ) : packedtype -> packedtype -> bool)
  let _ = equal_packedtype

  let (hash_fold_packedtype : Ppx_base.state -> packedtype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv (match arg with I8 -> 0 | I16 -> 1)
      : Ppx_base.state -> packedtype -> Ppx_base.state)

  let _ = hash_fold_packedtype

  let (hash_packedtype : packedtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_packedtype hsv arg)
    in
    fun x -> func x

  let _ = hash_packedtype
end

type storagetype = Valtype of valtype | Packedtype of packedtype

include struct
  let _ = fun (_ : storagetype) -> ()

  let equal_storagetype =
    (fun a__073_ ->
       fun b__074_ ->
        if Stdlib.( == ) a__073_ b__074_ then true
        else
          match (a__073_, b__074_) with
          | Valtype _a__075_, Valtype _b__076_ ->
              equal_valtype _a__075_ _b__076_
          | Valtype _, _ -> false
          | _, Valtype _ -> false
          | Packedtype _a__077_, Packedtype _b__078_ ->
              equal_packedtype _a__077_ _b__078_
      : storagetype -> storagetype -> bool)

  let _ = equal_storagetype

  let (hash_fold_storagetype : Ppx_base.state -> storagetype -> Ppx_base.state)
      =
    (fun hsv ->
       fun arg ->
        match arg with
        | Valtype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            hash_fold_valtype hsv _a0
        | Packedtype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_packedtype hsv _a0
      : Ppx_base.state -> storagetype -> Ppx_base.state)

  let _ = hash_fold_storagetype

  let (hash_storagetype : storagetype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_storagetype hsv arg)
    in
    fun x -> func x

  let _ = hash_storagetype
end

type mut = Const | Var

include struct
  let _ = fun (_ : mut) -> ()
  let equal_mut = (Stdlib.( = ) : mut -> mut -> bool)
  let _ = equal_mut

  let (hash_fold_mut : Ppx_base.state -> mut -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        Ppx_base.hash_fold_int hsv (match arg with Const -> 0 | Var -> 1)
      : Ppx_base.state -> mut -> Ppx_base.state)

  let _ = hash_fold_mut

  let (hash_mut : mut -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_mut hsv arg)
    in
    fun x -> func x

  let _ = hash_mut
end

type fieldtype = { mut : mut; type_ : storagetype }

include struct
  let _ = fun (_ : fieldtype) -> ()

  let equal_fieldtype =
    (fun a__081_ ->
       fun b__082_ ->
        if Stdlib.( == ) a__081_ b__082_ then true
        else
          Stdlib.( && )
            (equal_mut a__081_.mut b__082_.mut)
            (equal_storagetype a__081_.type_ b__082_.type_)
      : fieldtype -> fieldtype -> bool)

  let _ = equal_fieldtype

  let (hash_fold_fieldtype : Ppx_base.state -> fieldtype -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       hash_fold_mut hsv arg.mut
     in
     hash_fold_storagetype hsv arg.type_

  let _ = hash_fold_fieldtype

  let (hash_fieldtype : fieldtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_fieldtype hsv arg)
    in
    fun x -> func x

  let _ = hash_fieldtype
end

type field = { id : binder; [@ceh.ignore] fieldtype : fieldtype }

include struct
  let _ = fun (_ : field) -> ()

  let equal_field =
    (fun a__083_ ->
       fun b__084_ ->
        if Stdlib.( == ) a__083_ b__084_ then true
        else equal_fieldtype a__083_.fieldtype b__084_.fieldtype
      : field -> field -> bool)

  let _ = equal_field

  let (hash_fold_field : Ppx_base.state -> field -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv = hsv in
       hsv
     in
     hash_fold_fieldtype hsv arg.fieldtype

  let _ = hash_fold_field

  let (hash_field : field -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_field hsv arg)
    in
    fun x -> func x

  let _ = hash_field
end

type arraytype = Array of fieldtype

include struct
  let _ = fun (_ : arraytype) -> ()

  let equal_arraytype =
    (fun a__085_ ->
       fun b__086_ ->
        if Stdlib.( == ) a__085_ b__086_ then true
        else
          match (a__085_, b__086_) with
          | Array _a__087_, Array _b__088_ -> equal_fieldtype _a__087_ _b__088_
      : arraytype -> arraytype -> bool)

  let _ = equal_arraytype

  let (hash_fold_arraytype : Ppx_base.state -> arraytype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Array _a0 ->
            let hsv = hsv in
            let hsv = hsv in
            hash_fold_fieldtype hsv _a0
      : Ppx_base.state -> arraytype -> Ppx_base.state)

  let _ = hash_fold_arraytype

  let (hash_arraytype : arraytype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_arraytype hsv arg)
    in
    fun x -> func x

  let _ = hash_arraytype
end

type structtype = Struct of field list

include struct
  let _ = fun (_ : structtype) -> ()

  let equal_structtype =
    (fun a__089_ ->
       fun b__090_ ->
        if Stdlib.( == ) a__089_ b__090_ then true
        else
          match (a__089_, b__090_) with
          | Struct _a__091_, Struct _b__092_ ->
              Ppx_base.equal_list
                (fun a__093_ ->
                  fun (b__094_ [@merlin.hide]) ->
                   (equal_field a__093_ b__094_ [@merlin.hide]))
                _a__091_ _b__092_
      : structtype -> structtype -> bool)

  let _ = equal_structtype

  let (hash_fold_structtype : Ppx_base.state -> structtype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Struct _a0 ->
            let hsv = hsv in
            let hsv = hsv in
            Ppx_base.hash_fold_list hash_fold_field hsv _a0
      : Ppx_base.state -> structtype -> Ppx_base.state)

  let _ = hash_fold_structtype

  let (hash_structtype : structtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_structtype hsv arg)
    in
    fun x -> func x

  let _ = hash_structtype
end

type comptype =
  | Arraytype of arraytype
  | Structtype of structtype
  | Functype of functype

include struct
  let _ = fun (_ : comptype) -> ()

  let equal_comptype =
    (fun a__095_ ->
       fun b__096_ ->
        if Stdlib.( == ) a__095_ b__096_ then true
        else
          match (a__095_, b__096_) with
          | Arraytype _a__097_, Arraytype _b__098_ ->
              equal_arraytype _a__097_ _b__098_
          | Arraytype _, _ -> false
          | _, Arraytype _ -> false
          | Structtype _a__099_, Structtype _b__100_ ->
              equal_structtype _a__099_ _b__100_
          | Structtype _, _ -> false
          | _, Structtype _ -> false
          | Functype _a__101_, Functype _b__102_ ->
              equal_functype _a__101_ _b__102_
      : comptype -> comptype -> bool)

  let _ = equal_comptype

  let (hash_fold_comptype : Ppx_base.state -> comptype -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Arraytype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv = hsv in
            hash_fold_arraytype hsv _a0
        | Structtype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_structtype hsv _a0
        | Functype _a0 ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv = hsv in
            hash_fold_functype hsv _a0
      : Ppx_base.state -> comptype -> Ppx_base.state)

  let _ = hash_fold_comptype

  let (hash_comptype : comptype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_comptype hsv arg)
    in
    fun x -> func x

  let _ = hash_comptype
end

type subtype = { final : bool; super : typeidx list; type_ : comptype }

include struct
  let _ = fun (_ : subtype) -> ()

  let equal_subtype =
    (fun a__103_ ->
       fun b__104_ ->
        if Stdlib.( == ) a__103_ b__104_ then true
        else
          Stdlib.( && )
            (Stdlib.( = ) (a__103_.final : bool) b__104_.final)
            (Stdlib.( && )
               (Ppx_base.equal_list
                  (fun a__105_ -> fun b__106_ -> equal_typeidx a__105_ b__106_)
                  a__103_.super b__104_.super)
               (equal_comptype a__103_.type_ b__104_.type_))
      : subtype -> subtype -> bool)

  let _ = equal_subtype

  let (hash_fold_subtype : Ppx_base.state -> subtype -> Ppx_base.state) =
   fun hsv ->
    fun arg ->
     let hsv =
       let hsv =
         let hsv = hsv in
         Ppx_base.hash_fold_bool hsv arg.final
       in
       Ppx_base.hash_fold_list hash_fold_typeidx hsv arg.super
     in
     hash_fold_comptype hsv arg.type_

  let _ = hash_fold_subtype

  let (hash_subtype : subtype -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_subtype hsv arg)
    in
    fun x -> func x

  let _ = hash_subtype
end

type typedef = binder * subtype
type rectype = typedef list
type limits = { min : int32; max : int32 option }
type memtype = { limits : limits; shared : bool }
type tabletype = { limits : limits; element_type : reftype }
type globaltype = { mut : mut; type_ : valtype }

type typeuse =
  | Use of typeidx * param list * valtype list
  | Inline of param list * valtype list

include struct
  let _ = fun (_ : typeuse) -> ()

  let equal_typeuse =
    (fun a__107_ ->
       fun b__108_ ->
        if Stdlib.( == ) a__107_ b__108_ then true
        else
          match (a__107_, b__108_) with
          | ( Use (_a__109_, _a__111_, _a__113_),
              Use (_b__110_, _b__112_, _b__114_) ) ->
              Stdlib.( && )
                (equal_typeidx _a__109_ _b__110_)
                (Stdlib.( && )
                   (Ppx_base.equal_list
                      (fun a__115_ ->
                        fun (b__116_ [@merlin.hide]) ->
                         (equal_param a__115_ b__116_ [@merlin.hide]))
                      _a__111_ _b__112_)
                   (Ppx_base.equal_list
                      (fun a__117_ ->
                        fun (b__118_ [@merlin.hide]) ->
                         (equal_valtype a__117_ b__118_ [@merlin.hide]))
                      _a__113_ _b__114_))
          | Use _, _ -> false
          | _, Use _ -> false
          | Inline (_a__119_, _a__121_), Inline (_b__120_, _b__122_) ->
              Stdlib.( && )
                (Ppx_base.equal_list
                   (fun a__123_ ->
                     fun (b__124_ [@merlin.hide]) ->
                      (equal_param a__123_ b__124_ [@merlin.hide]))
                   _a__119_ _b__120_)
                (Ppx_base.equal_list
                   (fun a__125_ ->
                     fun (b__126_ [@merlin.hide]) ->
                      (equal_valtype a__125_ b__126_ [@merlin.hide]))
                   _a__121_ _b__122_)
      : typeuse -> typeuse -> bool)

  let _ = equal_typeuse

  let (hash_fold_typeuse : Ppx_base.state -> typeuse -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Use (_a0, _a1, _a2) ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv =
              let hsv =
                let hsv = hsv in
                hash_fold_typeidx hsv _a0
              in
              Ppx_base.hash_fold_list hash_fold_param hsv _a1
            in
            Ppx_base.hash_fold_list hash_fold_valtype hsv _a2
        | Inline (_a0, _a1) ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_list hash_fold_param hsv _a0
            in
            Ppx_base.hash_fold_list hash_fold_valtype hsv _a1
      : Ppx_base.state -> typeuse -> Ppx_base.state)

  let _ = hash_fold_typeuse

  let (hash_typeuse : typeuse -> Ppx_base.hash_value) =
    let func arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_typeuse hsv arg)
    in
    fun x -> func x

  let _ = hash_typeuse
end

type importdesc =
  | Func of binder * typeuse
  | Table of binder * tabletype
  | Memory of binder * memtype
  | Global of binder * globaltype
  | Tag of binder * typeuse

type import = { module_ : string; name : string; desc : importdesc }
type memarg = { align : int32; offset : int32 }

include struct
  let _ = fun (_ : memarg) -> ()

  let equal_memarg =
    (fun a__127_ ->
       fun b__128_ ->
        if Stdlib.( == ) a__127_ b__128_ then true
        else
          Stdlib.( && )
            (Stdlib.( = ) (a__127_.align : int32) b__128_.align)
            (Stdlib.( = ) (a__127_.offset : int32) b__128_.offset)
      : memarg -> memarg -> bool)

  let _ = equal_memarg
end

type source_pos = { pkg : string; file : string; line : int; col : int }

include struct
  let _ = fun (_ : source_pos) -> ()

  let equal_source_pos =
    (fun a__129_ ->
       fun b__130_ ->
        if Stdlib.( == ) a__129_ b__130_ then true
        else
          Stdlib.( && )
            (Stdlib.( = ) (a__129_.pkg : string) b__130_.pkg)
            (Stdlib.( && )
               (Stdlib.( = ) (a__129_.file : string) b__130_.file)
               (Stdlib.( && )
                  (Stdlib.( = ) (a__129_.line : int) b__130_.line)
                  (Stdlib.( = ) (a__129_.col : int) b__130_.col)))
      : source_pos -> source_pos -> bool)

  let _ = equal_source_pos
end

type catch = Catch of tagidx * labelidx

include struct
  let _ = fun (_ : catch) -> ()

  let equal_catch =
    (fun a__131_ ->
       fun b__132_ ->
        if Stdlib.( == ) a__131_ b__132_ then true
        else
          match (a__131_, b__132_) with
          | Catch (_a__133_, _a__135_), Catch (_b__134_, _b__136_) ->
              Stdlib.( && )
                (equal_tagidx _a__133_ _b__134_)
                (equal_labelidx _a__135_ _b__136_)
      : catch -> catch -> bool)

  let _ = equal_catch
end

type catchs = catch list

include struct
  let _ = fun (_ : catchs) -> ()

  let equal_catchs =
    (fun a__137_ ->
       fun b__138_ ->
        Ppx_base.equal_list
          (fun a__139_ -> fun b__140_ -> equal_catch a__139_ b__140_)
          a__137_ b__138_
      : catchs -> catchs -> bool)

  let _ = equal_catchs
end

class ['a] mapbase =
  object
    method visit_typeidx : 'a -> typeidx -> typeidx = fun _ -> fun e -> e
    method visit_dataidx : 'a -> dataidx -> dataidx = fun _ -> fun e -> e
    method visit_labelidx : 'a -> labelidx -> labelidx = fun _ -> fun e -> e
    method visit_funcidx : 'a -> funcidx -> funcidx = fun _ -> fun e -> e
    method visit_tableidx : 'a -> tableidx -> tableidx = fun _ -> fun e -> e
    method visit_globalidx : 'a -> globalidx -> globalidx = fun _ -> fun e -> e
    method visit_localidx : 'a -> localidx -> localidx = fun _ -> fun e -> e
    method visit_fieldidx : 'a -> fieldidx -> fieldidx = fun _ -> fun e -> e
    method visit_tagidx : 'a -> tagidx -> tagidx = fun _ -> fun e -> e
    method visit_memarg : 'a -> memarg -> memarg = fun _ -> fun e -> e
    method visit_label : 'a -> label -> label = fun _ -> fun e -> e
    method visit_reftype : 'a -> reftype -> reftype = fun _ -> fun e -> e
    method visit_heaptype : 'a -> heaptype -> heaptype = fun _ -> fun e -> e
    method visit_typeuse : 'a -> typeuse -> typeuse = fun _ -> fun e -> e
    method visit_catch : 'a -> catch -> catch = fun _ -> fun e -> e
    method visit_catchs : 'a -> catchs -> catchs = fun _ -> fun e -> e
  end

type instr =
  | Any_convert_extern
  | Array_copy of typeidx * typeidx
  | Array_fill of typeidx
  | Array_get of typeidx
  | Array_get_u of typeidx
  | Array_get_s of typeidx
  | Array_len
  | Array_new of typeidx
  | Array_new_data of typeidx * dataidx
  | Array_new_default of typeidx
  | Array_new_fixed of typeidx * int32
  | Array_set of typeidx
  | Block of {
      label : label; [@ceh.ignore]
      typeuse : typeuse;
      instrs : instr list;
    }
  | Br of labelidx
  | Br_if of labelidx
  | Br_table of labelidx list * labelidx
  | Call of funcidx
  | Call_indirect of tableidx * typeuse
  | Call_ref of typeidx
  | Drop
  | Extern_convert_any
  | F64_add
  | F64_const of string * float
  | F64_convert_i32_s
  | F64_convert_i32_u
  | F64_convert_i64_s
  | F64_convert_i64_u
  | F64_div
  | F64_eq
  | F64_ge
  | F64_gt
  | F64_le
  | F64_load of memarg
  | F64_lt
  | F64_mul
  | F64_ne
  | F64_neg
  | F64_reinterpret_i64
  | F64_store of memarg
  | F64_sub
  | F64_sqrt
  | F64_abs
  | F64_trunc
  | F64_floor
  | F64_ceil
  | F64_nearest
  | F32_add
  | F32_const of string * float
  | F32_convert_i32_s
  | F32_convert_i32_u
  | F32_convert_i64_s
  | F32_convert_i64_u
  | F32_demote_f64
  | F32_div
  | F32_eq
  | F32_ge
  | F32_gt
  | F32_le
  | F32_load of memarg
  | F32_lt
  | F32_mul
  | F32_ne
  | F32_neg
  | F32_reinterpret_i32
  | F32_sqrt
  | F32_store of memarg
  | F32_sub
  | F32_abs
  | F32_trunc
  | F32_floor
  | F32_ceil
  | F32_nearest
  | F64_promote_f32
  | I32_reinterpret_f32
  | I32_trunc_f32_s
  | I32_trunc_f32_u
  | I64_trunc_f32_s
  | I64_trunc_f32_u
  | Global_get of globalidx
  | Global_set of globalidx
  | I32_add
  | I32_and
  | I32_clz
  | I32_const of int32
  | I32_ctz
  | I32_div_s
  | I32_div_u
  | I32_eq
  | I32_eqz
  | I32_ge_s
  | I32_ge_u
  | I32_gt_s
  | I32_gt_u
  | I32_le_s
  | I32_le_u
  | I32_load of memarg
  | I32_load16_u of memarg
  | I32_load16_s of memarg
  | I32_load8_u of memarg
  | I32_load8_s of memarg
  | I32_lt_s
  | I32_lt_u
  | I32_mul
  | I32_ne
  | I32_or
  | I32_popcnt
  | I32_rem_s
  | I32_rem_u
  | I32_shl
  | I32_shr_s
  | I32_shr_u
  | I32_rotl
  | I32_store of memarg
  | I32_store16 of memarg
  | I32_store8 of memarg
  | I32_sub
  | I32_trunc_f64_s
  | I32_trunc_f64_u
  | I32_wrap_i64
  | I32_xor
  | I32_extend_8_s
  | I32_extend_16_s
  | I32_trunc_sat_f32_s
  | I32_trunc_sat_f32_u
  | I32_trunc_sat_f64_s
  | I32_trunc_sat_f64_u
  | I32_atomic_load of memarg
  | I32_atomic_load8_u of memarg
  | I32_atomic_load16_u of memarg
  | I32_atomic_store of memarg
  | I32_atomic_store8 of memarg
  | I32_atomic_store16 of memarg
  | I32_atomic_rmw_cmpxchg of memarg
  | I32_atomic_rmw8_cmpxchg_u of memarg
  | I32_atomic_rmw16_cmpxchg_u of memarg
  | I64_add
  | I64_and
  | I64_clz
  | I64_const of int64
  | I64_ctz
  | I64_div_s
  | I64_div_u
  | I64_eq
  | I64_extend_i32_s
  | I64_extend_i32_u
  | I64_ge_s
  | I64_gt_s
  | I64_le_s
  | I64_ge_u
  | I64_gt_u
  | I64_le_u
  | I64_load of memarg
  | I64_load32_u of memarg
  | I64_load32_s of memarg
  | I64_load16_u of memarg
  | I64_load16_s of memarg
  | I64_load8_u of memarg
  | I64_load8_s of memarg
  | I64_lt_s
  | I64_lt_u
  | I64_mul
  | I64_ne
  | I64_or
  | I64_popcnt
  | I64_reinterpret_f64
  | I64_rem_s
  | I64_rem_u
  | I64_shl
  | I64_shr_s
  | I64_shr_u
  | I64_store of memarg
  | I64_store32 of memarg
  | I64_store16 of memarg
  | I64_store8 of memarg
  | I64_sub
  | I64_trunc_f64_s
  | I64_trunc_f64_u
  | I64_xor
  | I64_extend_8_s
  | I64_extend_16_s
  | I64_extend_32_s
  | I64_trunc_sat_f32_s
  | I64_trunc_sat_f32_u
  | I64_trunc_sat_f64_s
  | I64_trunc_sat_f64_u
  | V128_load of memarg
  | V128_store of memarg
  | F64x2_add
  | F64x2_mul
  | F32x4_add
  | F32x4_mul
  | If of {
      label : label; [@ceh.ignore]
      typeuse : typeuse;
      then_ : instr list;
      else_ : instr list;
    }
  | Local_get of localidx
  | Local_set of localidx
  | Local_tee of localidx
  | Loop of {
      label : label; [@ceh.ignore]
      typeuse : typeuse;
      instrs : instr list;
    }
  | Memory_init of dataidx
  | Memory_copy
  | Memory_grow
  | Memory_size
  | Memory_fill
  | Memory_atomic_wait32 of memarg
  | Memory_atomic_notify of memarg
  | Ref_eq
  | Ref_as_non_null
  | Ref_cast of reftype
  | Ref_func of funcidx
  | Ref_is_null
  | Ref_null of heaptype
  | Return
  | Struct_get of typeidx * fieldidx
  | Struct_new of typeidx
  | Struct_new_default of typeidx
  | Struct_set of typeidx * fieldidx
  | Table_get of tableidx
  | Unreachable
  | Throw of tagidx
  | Try_table of {
      label : label; [@ceh.ignore]
      typeuse : typeuse;
      catchs : catchs;
      instrs : instr list;
    }
  | Select
  | No_op
  | Source_pos of (source_pos[@visitors.opaque])
  | Prologue_end

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] map =
    object (self : 'self)
      inherit [_] mapbase

      method visit_Any_convert_extern : _ -> instr =
        fun env -> Any_convert_extern

      method visit_Array_copy : _ -> typeidx -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
            let _visitors_r1 = self#visit_typeidx env _visitors_c1 in
            Array_copy (_visitors_r0, _visitors_r1)

      method visit_Array_fill : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_fill _visitors_r0

      method visit_Array_get : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_get _visitors_r0

      method visit_Array_get_u : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_get_u _visitors_r0

      method visit_Array_get_s : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_get_s _visitors_r0

      method visit_Array_len : _ -> instr = fun env -> Array_len

      method visit_Array_new : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_new _visitors_r0

      method visit_Array_new_data : _ -> typeidx -> dataidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
            let _visitors_r1 = self#visit_dataidx env _visitors_c1 in
            Array_new_data (_visitors_r0, _visitors_r1)

      method visit_Array_new_default : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_new_default _visitors_r0

      method visit_Array_new_fixed : _ -> typeidx -> int32 -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_c1
            in
            Array_new_fixed (_visitors_r0, _visitors_r1)

      method visit_Array_set : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Array_set _visitors_r0

      method visit_Block : _ -> label -> typeuse -> instr list -> instr =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_ftypeuse ->
            fun _visitors_finstrs ->
             let _visitors_r0 = self#visit_label env _visitors_flabel in
             let _visitors_r1 = self#visit_typeuse env _visitors_ftypeuse in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_instr env))
                 _visitors_finstrs
             in
             Block
               {
                 label = _visitors_r0;
                 typeuse = _visitors_r1;
                 instrs = _visitors_r2;
               }

      method visit_Br : _ -> labelidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_labelidx env _visitors_c0 in
           Br _visitors_r0

      method visit_Br_if : _ -> labelidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_labelidx env _visitors_c0 in
           Br_if _visitors_r0

      method visit_Br_table : _ -> labelidx list -> labelidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.map _visitors_this (self#visit_labelidx env))
                _visitors_c0
            in
            let _visitors_r1 = self#visit_labelidx env _visitors_c1 in
            Br_table (_visitors_r0, _visitors_r1)

      method visit_Call : _ -> funcidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_funcidx env _visitors_c0 in
           Call _visitors_r0

      method visit_Call_indirect : _ -> tableidx -> typeuse -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_tableidx env _visitors_c0 in
            let _visitors_r1 = self#visit_typeuse env _visitors_c1 in
            Call_indirect (_visitors_r0, _visitors_r1)

      method visit_Call_ref : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Call_ref _visitors_r0

      method visit_Drop : _ -> instr = fun env -> Drop

      method visit_Extern_convert_any : _ -> instr =
        fun env -> Extern_convert_any

      method visit_F64_add : _ -> instr = fun env -> F64_add

      method visit_F64_const : _ -> string -> float -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 =
              (fun _visitors_this -> _visitors_this) _visitors_c0
            in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_c1
            in
            F64_const (_visitors_r0, _visitors_r1)

      method visit_F64_convert_i32_s : _ -> instr = fun env -> F64_convert_i32_s
      method visit_F64_convert_i32_u : _ -> instr = fun env -> F64_convert_i32_u
      method visit_F64_convert_i64_s : _ -> instr = fun env -> F64_convert_i64_s
      method visit_F64_convert_i64_u : _ -> instr = fun env -> F64_convert_i64_u
      method visit_F64_div : _ -> instr = fun env -> F64_div
      method visit_F64_eq : _ -> instr = fun env -> F64_eq
      method visit_F64_ge : _ -> instr = fun env -> F64_ge
      method visit_F64_gt : _ -> instr = fun env -> F64_gt
      method visit_F64_le : _ -> instr = fun env -> F64_le

      method visit_F64_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           F64_load _visitors_r0

      method visit_F64_lt : _ -> instr = fun env -> F64_lt
      method visit_F64_mul : _ -> instr = fun env -> F64_mul
      method visit_F64_ne : _ -> instr = fun env -> F64_ne
      method visit_F64_neg : _ -> instr = fun env -> F64_neg

      method visit_F64_reinterpret_i64 : _ -> instr =
        fun env -> F64_reinterpret_i64

      method visit_F64_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           F64_store _visitors_r0

      method visit_F64_sub : _ -> instr = fun env -> F64_sub
      method visit_F64_sqrt : _ -> instr = fun env -> F64_sqrt
      method visit_F64_abs : _ -> instr = fun env -> F64_abs
      method visit_F64_trunc : _ -> instr = fun env -> F64_trunc
      method visit_F64_floor : _ -> instr = fun env -> F64_floor
      method visit_F64_ceil : _ -> instr = fun env -> F64_ceil
      method visit_F64_nearest : _ -> instr = fun env -> F64_nearest
      method visit_F32_add : _ -> instr = fun env -> F32_add

      method visit_F32_const : _ -> string -> float -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 =
              (fun _visitors_this -> _visitors_this) _visitors_c0
            in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_c1
            in
            F32_const (_visitors_r0, _visitors_r1)

      method visit_F32_convert_i32_s : _ -> instr = fun env -> F32_convert_i32_s
      method visit_F32_convert_i32_u : _ -> instr = fun env -> F32_convert_i32_u
      method visit_F32_convert_i64_s : _ -> instr = fun env -> F32_convert_i64_s
      method visit_F32_convert_i64_u : _ -> instr = fun env -> F32_convert_i64_u
      method visit_F32_demote_f64 : _ -> instr = fun env -> F32_demote_f64
      method visit_F32_div : _ -> instr = fun env -> F32_div
      method visit_F32_eq : _ -> instr = fun env -> F32_eq
      method visit_F32_ge : _ -> instr = fun env -> F32_ge
      method visit_F32_gt : _ -> instr = fun env -> F32_gt
      method visit_F32_le : _ -> instr = fun env -> F32_le

      method visit_F32_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           F32_load _visitors_r0

      method visit_F32_lt : _ -> instr = fun env -> F32_lt
      method visit_F32_mul : _ -> instr = fun env -> F32_mul
      method visit_F32_ne : _ -> instr = fun env -> F32_ne
      method visit_F32_neg : _ -> instr = fun env -> F32_neg

      method visit_F32_reinterpret_i32 : _ -> instr =
        fun env -> F32_reinterpret_i32

      method visit_F32_sqrt : _ -> instr = fun env -> F32_sqrt

      method visit_F32_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           F32_store _visitors_r0

      method visit_F32_sub : _ -> instr = fun env -> F32_sub
      method visit_F32_abs : _ -> instr = fun env -> F32_abs
      method visit_F32_trunc : _ -> instr = fun env -> F32_trunc
      method visit_F32_floor : _ -> instr = fun env -> F32_floor
      method visit_F32_ceil : _ -> instr = fun env -> F32_ceil
      method visit_F32_nearest : _ -> instr = fun env -> F32_nearest
      method visit_F64_promote_f32 : _ -> instr = fun env -> F64_promote_f32

      method visit_I32_reinterpret_f32 : _ -> instr =
        fun env -> I32_reinterpret_f32

      method visit_I32_trunc_f32_s : _ -> instr = fun env -> I32_trunc_f32_s
      method visit_I32_trunc_f32_u : _ -> instr = fun env -> I32_trunc_f32_u
      method visit_I64_trunc_f32_s : _ -> instr = fun env -> I64_trunc_f32_s
      method visit_I64_trunc_f32_u : _ -> instr = fun env -> I64_trunc_f32_u

      method visit_Global_get : _ -> globalidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_globalidx env _visitors_c0 in
           Global_get _visitors_r0

      method visit_Global_set : _ -> globalidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_globalidx env _visitors_c0 in
           Global_set _visitors_r0

      method visit_I32_add : _ -> instr = fun env -> I32_add
      method visit_I32_and : _ -> instr = fun env -> I32_and
      method visit_I32_clz : _ -> instr = fun env -> I32_clz

      method visit_I32_const : _ -> int32 -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_c0
           in
           I32_const _visitors_r0

      method visit_I32_ctz : _ -> instr = fun env -> I32_ctz
      method visit_I32_div_s : _ -> instr = fun env -> I32_div_s
      method visit_I32_div_u : _ -> instr = fun env -> I32_div_u
      method visit_I32_eq : _ -> instr = fun env -> I32_eq
      method visit_I32_eqz : _ -> instr = fun env -> I32_eqz
      method visit_I32_ge_s : _ -> instr = fun env -> I32_ge_s
      method visit_I32_ge_u : _ -> instr = fun env -> I32_ge_u
      method visit_I32_gt_s : _ -> instr = fun env -> I32_gt_s
      method visit_I32_gt_u : _ -> instr = fun env -> I32_gt_u
      method visit_I32_le_s : _ -> instr = fun env -> I32_le_s
      method visit_I32_le_u : _ -> instr = fun env -> I32_le_u

      method visit_I32_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_load _visitors_r0

      method visit_I32_load16_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_load16_u _visitors_r0

      method visit_I32_load16_s : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_load16_s _visitors_r0

      method visit_I32_load8_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_load8_u _visitors_r0

      method visit_I32_load8_s : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_load8_s _visitors_r0

      method visit_I32_lt_s : _ -> instr = fun env -> I32_lt_s
      method visit_I32_lt_u : _ -> instr = fun env -> I32_lt_u
      method visit_I32_mul : _ -> instr = fun env -> I32_mul
      method visit_I32_ne : _ -> instr = fun env -> I32_ne
      method visit_I32_or : _ -> instr = fun env -> I32_or
      method visit_I32_popcnt : _ -> instr = fun env -> I32_popcnt
      method visit_I32_rem_s : _ -> instr = fun env -> I32_rem_s
      method visit_I32_rem_u : _ -> instr = fun env -> I32_rem_u
      method visit_I32_shl : _ -> instr = fun env -> I32_shl
      method visit_I32_shr_s : _ -> instr = fun env -> I32_shr_s
      method visit_I32_shr_u : _ -> instr = fun env -> I32_shr_u
      method visit_I32_rotl : _ -> instr = fun env -> I32_rotl

      method visit_I32_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_store _visitors_r0

      method visit_I32_store16 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_store16 _visitors_r0

      method visit_I32_store8 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_store8 _visitors_r0

      method visit_I32_sub : _ -> instr = fun env -> I32_sub
      method visit_I32_trunc_f64_s : _ -> instr = fun env -> I32_trunc_f64_s
      method visit_I32_trunc_f64_u : _ -> instr = fun env -> I32_trunc_f64_u
      method visit_I32_wrap_i64 : _ -> instr = fun env -> I32_wrap_i64
      method visit_I32_xor : _ -> instr = fun env -> I32_xor
      method visit_I32_extend_8_s : _ -> instr = fun env -> I32_extend_8_s
      method visit_I32_extend_16_s : _ -> instr = fun env -> I32_extend_16_s

      method visit_I32_trunc_sat_f32_s : _ -> instr =
        fun env -> I32_trunc_sat_f32_s

      method visit_I32_trunc_sat_f32_u : _ -> instr =
        fun env -> I32_trunc_sat_f32_u

      method visit_I32_trunc_sat_f64_s : _ -> instr =
        fun env -> I32_trunc_sat_f64_s

      method visit_I32_trunc_sat_f64_u : _ -> instr =
        fun env -> I32_trunc_sat_f64_u

      method visit_I32_atomic_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_load _visitors_r0

      method visit_I32_atomic_load8_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_load8_u _visitors_r0

      method visit_I32_atomic_load16_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_load16_u _visitors_r0

      method visit_I32_atomic_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_store _visitors_r0

      method visit_I32_atomic_store8 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_store8 _visitors_r0

      method visit_I32_atomic_store16 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_store16 _visitors_r0

      method visit_I32_atomic_rmw_cmpxchg : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_rmw_cmpxchg _visitors_r0

      method visit_I32_atomic_rmw8_cmpxchg_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_rmw8_cmpxchg_u _visitors_r0

      method visit_I32_atomic_rmw16_cmpxchg_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I32_atomic_rmw16_cmpxchg_u _visitors_r0

      method visit_I64_add : _ -> instr = fun env -> I64_add
      method visit_I64_and : _ -> instr = fun env -> I64_and
      method visit_I64_clz : _ -> instr = fun env -> I64_clz

      method visit_I64_const : _ -> int64 -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_c0
           in
           I64_const _visitors_r0

      method visit_I64_ctz : _ -> instr = fun env -> I64_ctz
      method visit_I64_div_s : _ -> instr = fun env -> I64_div_s
      method visit_I64_div_u : _ -> instr = fun env -> I64_div_u
      method visit_I64_eq : _ -> instr = fun env -> I64_eq
      method visit_I64_extend_i32_s : _ -> instr = fun env -> I64_extend_i32_s
      method visit_I64_extend_i32_u : _ -> instr = fun env -> I64_extend_i32_u
      method visit_I64_ge_s : _ -> instr = fun env -> I64_ge_s
      method visit_I64_gt_s : _ -> instr = fun env -> I64_gt_s
      method visit_I64_le_s : _ -> instr = fun env -> I64_le_s
      method visit_I64_ge_u : _ -> instr = fun env -> I64_ge_u
      method visit_I64_gt_u : _ -> instr = fun env -> I64_gt_u
      method visit_I64_le_u : _ -> instr = fun env -> I64_le_u

      method visit_I64_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load _visitors_r0

      method visit_I64_load32_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load32_u _visitors_r0

      method visit_I64_load32_s : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load32_s _visitors_r0

      method visit_I64_load16_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load16_u _visitors_r0

      method visit_I64_load16_s : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load16_s _visitors_r0

      method visit_I64_load8_u : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load8_u _visitors_r0

      method visit_I64_load8_s : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_load8_s _visitors_r0

      method visit_I64_lt_s : _ -> instr = fun env -> I64_lt_s
      method visit_I64_lt_u : _ -> instr = fun env -> I64_lt_u
      method visit_I64_mul : _ -> instr = fun env -> I64_mul
      method visit_I64_ne : _ -> instr = fun env -> I64_ne
      method visit_I64_or : _ -> instr = fun env -> I64_or
      method visit_I64_popcnt : _ -> instr = fun env -> I64_popcnt

      method visit_I64_reinterpret_f64 : _ -> instr =
        fun env -> I64_reinterpret_f64

      method visit_I64_rem_s : _ -> instr = fun env -> I64_rem_s
      method visit_I64_rem_u : _ -> instr = fun env -> I64_rem_u
      method visit_I64_shl : _ -> instr = fun env -> I64_shl
      method visit_I64_shr_s : _ -> instr = fun env -> I64_shr_s
      method visit_I64_shr_u : _ -> instr = fun env -> I64_shr_u

      method visit_I64_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_store _visitors_r0

      method visit_I64_store32 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_store32 _visitors_r0

      method visit_I64_store16 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_store16 _visitors_r0

      method visit_I64_store8 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           I64_store8 _visitors_r0

      method visit_I64_sub : _ -> instr = fun env -> I64_sub
      method visit_I64_trunc_f64_s : _ -> instr = fun env -> I64_trunc_f64_s
      method visit_I64_trunc_f64_u : _ -> instr = fun env -> I64_trunc_f64_u
      method visit_I64_xor : _ -> instr = fun env -> I64_xor
      method visit_I64_extend_8_s : _ -> instr = fun env -> I64_extend_8_s
      method visit_I64_extend_16_s : _ -> instr = fun env -> I64_extend_16_s
      method visit_I64_extend_32_s : _ -> instr = fun env -> I64_extend_32_s

      method visit_I64_trunc_sat_f32_s : _ -> instr =
        fun env -> I64_trunc_sat_f32_s

      method visit_I64_trunc_sat_f32_u : _ -> instr =
        fun env -> I64_trunc_sat_f32_u

      method visit_I64_trunc_sat_f64_s : _ -> instr =
        fun env -> I64_trunc_sat_f64_s

      method visit_I64_trunc_sat_f64_u : _ -> instr =
        fun env -> I64_trunc_sat_f64_u

      method visit_V128_load : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           V128_load _visitors_r0

      method visit_V128_store : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           V128_store _visitors_r0

      method visit_F64x2_add : _ -> instr = fun env -> F64x2_add
      method visit_F64x2_mul : _ -> instr = fun env -> F64x2_mul
      method visit_F32x4_add : _ -> instr = fun env -> F32x4_add
      method visit_F32x4_mul : _ -> instr = fun env -> F32x4_mul

      method visit_If :
          _ -> label -> typeuse -> instr list -> instr list -> instr =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_ftypeuse ->
            fun _visitors_fthen_ ->
             fun _visitors_felse_ ->
              let _visitors_r0 = self#visit_label env _visitors_flabel in
              let _visitors_r1 = self#visit_typeuse env _visitors_ftypeuse in
              let _visitors_r2 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_instr env))
                  _visitors_fthen_
              in
              let _visitors_r3 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_instr env))
                  _visitors_felse_
              in
              If
                {
                  label = _visitors_r0;
                  typeuse = _visitors_r1;
                  then_ = _visitors_r2;
                  else_ = _visitors_r3;
                }

      method visit_Local_get : _ -> localidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_localidx env _visitors_c0 in
           Local_get _visitors_r0

      method visit_Local_set : _ -> localidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_localidx env _visitors_c0 in
           Local_set _visitors_r0

      method visit_Local_tee : _ -> localidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_localidx env _visitors_c0 in
           Local_tee _visitors_r0

      method visit_Loop : _ -> label -> typeuse -> instr list -> instr =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_ftypeuse ->
            fun _visitors_finstrs ->
             let _visitors_r0 = self#visit_label env _visitors_flabel in
             let _visitors_r1 = self#visit_typeuse env _visitors_ftypeuse in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_instr env))
                 _visitors_finstrs
             in
             Loop
               {
                 label = _visitors_r0;
                 typeuse = _visitors_r1;
                 instrs = _visitors_r2;
               }

      method visit_Memory_init : _ -> dataidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_dataidx env _visitors_c0 in
           Memory_init _visitors_r0

      method visit_Memory_copy : _ -> instr = fun env -> Memory_copy
      method visit_Memory_grow : _ -> instr = fun env -> Memory_grow
      method visit_Memory_size : _ -> instr = fun env -> Memory_size
      method visit_Memory_fill : _ -> instr = fun env -> Memory_fill

      method visit_Memory_atomic_wait32 : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           Memory_atomic_wait32 _visitors_r0

      method visit_Memory_atomic_notify : _ -> memarg -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_memarg env _visitors_c0 in
           Memory_atomic_notify _visitors_r0

      method visit_Ref_eq : _ -> instr = fun env -> Ref_eq
      method visit_Ref_as_non_null : _ -> instr = fun env -> Ref_as_non_null

      method visit_Ref_cast : _ -> reftype -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_reftype env _visitors_c0 in
           Ref_cast _visitors_r0

      method visit_Ref_func : _ -> funcidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_funcidx env _visitors_c0 in
           Ref_func _visitors_r0

      method visit_Ref_is_null : _ -> instr = fun env -> Ref_is_null

      method visit_Ref_null : _ -> heaptype -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_heaptype env _visitors_c0 in
           Ref_null _visitors_r0

      method visit_Return : _ -> instr = fun env -> Return

      method visit_Struct_get : _ -> typeidx -> fieldidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
            let _visitors_r1 = self#visit_fieldidx env _visitors_c1 in
            Struct_get (_visitors_r0, _visitors_r1)

      method visit_Struct_new : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Struct_new _visitors_r0

      method visit_Struct_new_default : _ -> typeidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
           Struct_new_default _visitors_r0

      method visit_Struct_set : _ -> typeidx -> fieldidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           fun _visitors_c1 ->
            let _visitors_r0 = self#visit_typeidx env _visitors_c0 in
            let _visitors_r1 = self#visit_fieldidx env _visitors_c1 in
            Struct_set (_visitors_r0, _visitors_r1)

      method visit_Table_get : _ -> tableidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_tableidx env _visitors_c0 in
           Table_get _visitors_r0

      method visit_Unreachable : _ -> instr = fun env -> Unreachable

      method visit_Throw : _ -> tagidx -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_tagidx env _visitors_c0 in
           Throw _visitors_r0

      method visit_Try_table :
          _ -> label -> typeuse -> catchs -> instr list -> instr =
        fun env ->
          fun _visitors_flabel ->
           fun _visitors_ftypeuse ->
            fun _visitors_fcatchs ->
             fun _visitors_finstrs ->
              let _visitors_r0 = self#visit_label env _visitors_flabel in
              let _visitors_r1 = self#visit_typeuse env _visitors_ftypeuse in
              let _visitors_r2 = self#visit_catchs env _visitors_fcatchs in
              let _visitors_r3 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_instr env))
                  _visitors_finstrs
              in
              Try_table
                {
                  label = _visitors_r0;
                  typeuse = _visitors_r1;
                  catchs = _visitors_r2;
                  instrs = _visitors_r3;
                }

      method visit_Select : _ -> instr = fun env -> Select
      method visit_No_op : _ -> instr = fun env -> No_op

      method visit_Source_pos : _ -> _ -> instr =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_c0
           in
           Source_pos _visitors_r0

      method visit_Prologue_end : _ -> instr = fun env -> Prologue_end

      method visit_instr : _ -> instr -> instr =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Any_convert_extern -> self#visit_Any_convert_extern env
           | Array_copy (_visitors_c0, _visitors_c1) ->
               self#visit_Array_copy env _visitors_c0 _visitors_c1
           | Array_fill _visitors_c0 -> self#visit_Array_fill env _visitors_c0
           | Array_get _visitors_c0 -> self#visit_Array_get env _visitors_c0
           | Array_get_u _visitors_c0 -> self#visit_Array_get_u env _visitors_c0
           | Array_get_s _visitors_c0 -> self#visit_Array_get_s env _visitors_c0
           | Array_len -> self#visit_Array_len env
           | Array_new _visitors_c0 -> self#visit_Array_new env _visitors_c0
           | Array_new_data (_visitors_c0, _visitors_c1) ->
               self#visit_Array_new_data env _visitors_c0 _visitors_c1
           | Array_new_default _visitors_c0 ->
               self#visit_Array_new_default env _visitors_c0
           | Array_new_fixed (_visitors_c0, _visitors_c1) ->
               self#visit_Array_new_fixed env _visitors_c0 _visitors_c1
           | Array_set _visitors_c0 -> self#visit_Array_set env _visitors_c0
           | Block
               {
                 label = _visitors_flabel;
                 typeuse = _visitors_ftypeuse;
                 instrs = _visitors_finstrs;
               } ->
               self#visit_Block env _visitors_flabel _visitors_ftypeuse
                 _visitors_finstrs
           | Br _visitors_c0 -> self#visit_Br env _visitors_c0
           | Br_if _visitors_c0 -> self#visit_Br_if env _visitors_c0
           | Br_table (_visitors_c0, _visitors_c1) ->
               self#visit_Br_table env _visitors_c0 _visitors_c1
           | Call _visitors_c0 -> self#visit_Call env _visitors_c0
           | Call_indirect (_visitors_c0, _visitors_c1) ->
               self#visit_Call_indirect env _visitors_c0 _visitors_c1
           | Call_ref _visitors_c0 -> self#visit_Call_ref env _visitors_c0
           | Drop -> self#visit_Drop env
           | Extern_convert_any -> self#visit_Extern_convert_any env
           | F64_add -> self#visit_F64_add env
           | F64_const (_visitors_c0, _visitors_c1) ->
               self#visit_F64_const env _visitors_c0 _visitors_c1
           | F64_convert_i32_s -> self#visit_F64_convert_i32_s env
           | F64_convert_i32_u -> self#visit_F64_convert_i32_u env
           | F64_convert_i64_s -> self#visit_F64_convert_i64_s env
           | F64_convert_i64_u -> self#visit_F64_convert_i64_u env
           | F64_div -> self#visit_F64_div env
           | F64_eq -> self#visit_F64_eq env
           | F64_ge -> self#visit_F64_ge env
           | F64_gt -> self#visit_F64_gt env
           | F64_le -> self#visit_F64_le env
           | F64_load _visitors_c0 -> self#visit_F64_load env _visitors_c0
           | F64_lt -> self#visit_F64_lt env
           | F64_mul -> self#visit_F64_mul env
           | F64_ne -> self#visit_F64_ne env
           | F64_neg -> self#visit_F64_neg env
           | F64_reinterpret_i64 -> self#visit_F64_reinterpret_i64 env
           | F64_store _visitors_c0 -> self#visit_F64_store env _visitors_c0
           | F64_sub -> self#visit_F64_sub env
           | F64_sqrt -> self#visit_F64_sqrt env
           | F64_abs -> self#visit_F64_abs env
           | F64_trunc -> self#visit_F64_trunc env
           | F64_floor -> self#visit_F64_floor env
           | F64_ceil -> self#visit_F64_ceil env
           | F64_nearest -> self#visit_F64_nearest env
           | F32_add -> self#visit_F32_add env
           | F32_const (_visitors_c0, _visitors_c1) ->
               self#visit_F32_const env _visitors_c0 _visitors_c1
           | F32_convert_i32_s -> self#visit_F32_convert_i32_s env
           | F32_convert_i32_u -> self#visit_F32_convert_i32_u env
           | F32_convert_i64_s -> self#visit_F32_convert_i64_s env
           | F32_convert_i64_u -> self#visit_F32_convert_i64_u env
           | F32_demote_f64 -> self#visit_F32_demote_f64 env
           | F32_div -> self#visit_F32_div env
           | F32_eq -> self#visit_F32_eq env
           | F32_ge -> self#visit_F32_ge env
           | F32_gt -> self#visit_F32_gt env
           | F32_le -> self#visit_F32_le env
           | F32_load _visitors_c0 -> self#visit_F32_load env _visitors_c0
           | F32_lt -> self#visit_F32_lt env
           | F32_mul -> self#visit_F32_mul env
           | F32_ne -> self#visit_F32_ne env
           | F32_neg -> self#visit_F32_neg env
           | F32_reinterpret_i32 -> self#visit_F32_reinterpret_i32 env
           | F32_sqrt -> self#visit_F32_sqrt env
           | F32_store _visitors_c0 -> self#visit_F32_store env _visitors_c0
           | F32_sub -> self#visit_F32_sub env
           | F32_abs -> self#visit_F32_abs env
           | F32_trunc -> self#visit_F32_trunc env
           | F32_floor -> self#visit_F32_floor env
           | F32_ceil -> self#visit_F32_ceil env
           | F32_nearest -> self#visit_F32_nearest env
           | F64_promote_f32 -> self#visit_F64_promote_f32 env
           | I32_reinterpret_f32 -> self#visit_I32_reinterpret_f32 env
           | I32_trunc_f32_s -> self#visit_I32_trunc_f32_s env
           | I32_trunc_f32_u -> self#visit_I32_trunc_f32_u env
           | I64_trunc_f32_s -> self#visit_I64_trunc_f32_s env
           | I64_trunc_f32_u -> self#visit_I64_trunc_f32_u env
           | Global_get _visitors_c0 -> self#visit_Global_get env _visitors_c0
           | Global_set _visitors_c0 -> self#visit_Global_set env _visitors_c0
           | I32_add -> self#visit_I32_add env
           | I32_and -> self#visit_I32_and env
           | I32_clz -> self#visit_I32_clz env
           | I32_const _visitors_c0 -> self#visit_I32_const env _visitors_c0
           | I32_ctz -> self#visit_I32_ctz env
           | I32_div_s -> self#visit_I32_div_s env
           | I32_div_u -> self#visit_I32_div_u env
           | I32_eq -> self#visit_I32_eq env
           | I32_eqz -> self#visit_I32_eqz env
           | I32_ge_s -> self#visit_I32_ge_s env
           | I32_ge_u -> self#visit_I32_ge_u env
           | I32_gt_s -> self#visit_I32_gt_s env
           | I32_gt_u -> self#visit_I32_gt_u env
           | I32_le_s -> self#visit_I32_le_s env
           | I32_le_u -> self#visit_I32_le_u env
           | I32_load _visitors_c0 -> self#visit_I32_load env _visitors_c0
           | I32_load16_u _visitors_c0 ->
               self#visit_I32_load16_u env _visitors_c0
           | I32_load16_s _visitors_c0 ->
               self#visit_I32_load16_s env _visitors_c0
           | I32_load8_u _visitors_c0 -> self#visit_I32_load8_u env _visitors_c0
           | I32_load8_s _visitors_c0 -> self#visit_I32_load8_s env _visitors_c0
           | I32_lt_s -> self#visit_I32_lt_s env
           | I32_lt_u -> self#visit_I32_lt_u env
           | I32_mul -> self#visit_I32_mul env
           | I32_ne -> self#visit_I32_ne env
           | I32_or -> self#visit_I32_or env
           | I32_popcnt -> self#visit_I32_popcnt env
           | I32_rem_s -> self#visit_I32_rem_s env
           | I32_rem_u -> self#visit_I32_rem_u env
           | I32_shl -> self#visit_I32_shl env
           | I32_shr_s -> self#visit_I32_shr_s env
           | I32_shr_u -> self#visit_I32_shr_u env
           | I32_rotl -> self#visit_I32_rotl env
           | I32_store _visitors_c0 -> self#visit_I32_store env _visitors_c0
           | I32_store16 _visitors_c0 -> self#visit_I32_store16 env _visitors_c0
           | I32_store8 _visitors_c0 -> self#visit_I32_store8 env _visitors_c0
           | I32_sub -> self#visit_I32_sub env
           | I32_trunc_f64_s -> self#visit_I32_trunc_f64_s env
           | I32_trunc_f64_u -> self#visit_I32_trunc_f64_u env
           | I32_wrap_i64 -> self#visit_I32_wrap_i64 env
           | I32_xor -> self#visit_I32_xor env
           | I32_extend_8_s -> self#visit_I32_extend_8_s env
           | I32_extend_16_s -> self#visit_I32_extend_16_s env
           | I32_trunc_sat_f32_s -> self#visit_I32_trunc_sat_f32_s env
           | I32_trunc_sat_f32_u -> self#visit_I32_trunc_sat_f32_u env
           | I32_trunc_sat_f64_s -> self#visit_I32_trunc_sat_f64_s env
           | I32_trunc_sat_f64_u -> self#visit_I32_trunc_sat_f64_u env
           | I32_atomic_load _visitors_c0 ->
               self#visit_I32_atomic_load env _visitors_c0
           | I32_atomic_load8_u _visitors_c0 ->
               self#visit_I32_atomic_load8_u env _visitors_c0
           | I32_atomic_load16_u _visitors_c0 ->
               self#visit_I32_atomic_load16_u env _visitors_c0
           | I32_atomic_store _visitors_c0 ->
               self#visit_I32_atomic_store env _visitors_c0
           | I32_atomic_store8 _visitors_c0 ->
               self#visit_I32_atomic_store8 env _visitors_c0
           | I32_atomic_store16 _visitors_c0 ->
               self#visit_I32_atomic_store16 env _visitors_c0
           | I32_atomic_rmw_cmpxchg _visitors_c0 ->
               self#visit_I32_atomic_rmw_cmpxchg env _visitors_c0
           | I32_atomic_rmw8_cmpxchg_u _visitors_c0 ->
               self#visit_I32_atomic_rmw8_cmpxchg_u env _visitors_c0
           | I32_atomic_rmw16_cmpxchg_u _visitors_c0 ->
               self#visit_I32_atomic_rmw16_cmpxchg_u env _visitors_c0
           | I64_add -> self#visit_I64_add env
           | I64_and -> self#visit_I64_and env
           | I64_clz -> self#visit_I64_clz env
           | I64_const _visitors_c0 -> self#visit_I64_const env _visitors_c0
           | I64_ctz -> self#visit_I64_ctz env
           | I64_div_s -> self#visit_I64_div_s env
           | I64_div_u -> self#visit_I64_div_u env
           | I64_eq -> self#visit_I64_eq env
           | I64_extend_i32_s -> self#visit_I64_extend_i32_s env
           | I64_extend_i32_u -> self#visit_I64_extend_i32_u env
           | I64_ge_s -> self#visit_I64_ge_s env
           | I64_gt_s -> self#visit_I64_gt_s env
           | I64_le_s -> self#visit_I64_le_s env
           | I64_ge_u -> self#visit_I64_ge_u env
           | I64_gt_u -> self#visit_I64_gt_u env
           | I64_le_u -> self#visit_I64_le_u env
           | I64_load _visitors_c0 -> self#visit_I64_load env _visitors_c0
           | I64_load32_u _visitors_c0 ->
               self#visit_I64_load32_u env _visitors_c0
           | I64_load32_s _visitors_c0 ->
               self#visit_I64_load32_s env _visitors_c0
           | I64_load16_u _visitors_c0 ->
               self#visit_I64_load16_u env _visitors_c0
           | I64_load16_s _visitors_c0 ->
               self#visit_I64_load16_s env _visitors_c0
           | I64_load8_u _visitors_c0 -> self#visit_I64_load8_u env _visitors_c0
           | I64_load8_s _visitors_c0 -> self#visit_I64_load8_s env _visitors_c0
           | I64_lt_s -> self#visit_I64_lt_s env
           | I64_lt_u -> self#visit_I64_lt_u env
           | I64_mul -> self#visit_I64_mul env
           | I64_ne -> self#visit_I64_ne env
           | I64_or -> self#visit_I64_or env
           | I64_popcnt -> self#visit_I64_popcnt env
           | I64_reinterpret_f64 -> self#visit_I64_reinterpret_f64 env
           | I64_rem_s -> self#visit_I64_rem_s env
           | I64_rem_u -> self#visit_I64_rem_u env
           | I64_shl -> self#visit_I64_shl env
           | I64_shr_s -> self#visit_I64_shr_s env
           | I64_shr_u -> self#visit_I64_shr_u env
           | I64_store _visitors_c0 -> self#visit_I64_store env _visitors_c0
           | I64_store32 _visitors_c0 -> self#visit_I64_store32 env _visitors_c0
           | I64_store16 _visitors_c0 -> self#visit_I64_store16 env _visitors_c0
           | I64_store8 _visitors_c0 -> self#visit_I64_store8 env _visitors_c0
           | I64_sub -> self#visit_I64_sub env
           | I64_trunc_f64_s -> self#visit_I64_trunc_f64_s env
           | I64_trunc_f64_u -> self#visit_I64_trunc_f64_u env
           | I64_xor -> self#visit_I64_xor env
           | I64_extend_8_s -> self#visit_I64_extend_8_s env
           | I64_extend_16_s -> self#visit_I64_extend_16_s env
           | I64_extend_32_s -> self#visit_I64_extend_32_s env
           | I64_trunc_sat_f32_s -> self#visit_I64_trunc_sat_f32_s env
           | I64_trunc_sat_f32_u -> self#visit_I64_trunc_sat_f32_u env
           | I64_trunc_sat_f64_s -> self#visit_I64_trunc_sat_f64_s env
           | I64_trunc_sat_f64_u -> self#visit_I64_trunc_sat_f64_u env
           | V128_load _visitors_c0 -> self#visit_V128_load env _visitors_c0
           | V128_store _visitors_c0 -> self#visit_V128_store env _visitors_c0
           | F64x2_add -> self#visit_F64x2_add env
           | F64x2_mul -> self#visit_F64x2_mul env
           | F32x4_add -> self#visit_F32x4_add env
           | F32x4_mul -> self#visit_F32x4_mul env
           | If
               {
                 label = _visitors_flabel;
                 typeuse = _visitors_ftypeuse;
                 then_ = _visitors_fthen_;
                 else_ = _visitors_felse_;
               } ->
               self#visit_If env _visitors_flabel _visitors_ftypeuse
                 _visitors_fthen_ _visitors_felse_
           | Local_get _visitors_c0 -> self#visit_Local_get env _visitors_c0
           | Local_set _visitors_c0 -> self#visit_Local_set env _visitors_c0
           | Local_tee _visitors_c0 -> self#visit_Local_tee env _visitors_c0
           | Loop
               {
                 label = _visitors_flabel;
                 typeuse = _visitors_ftypeuse;
                 instrs = _visitors_finstrs;
               } ->
               self#visit_Loop env _visitors_flabel _visitors_ftypeuse
                 _visitors_finstrs
           | Memory_init _visitors_c0 -> self#visit_Memory_init env _visitors_c0
           | Memory_copy -> self#visit_Memory_copy env
           | Memory_grow -> self#visit_Memory_grow env
           | Memory_size -> self#visit_Memory_size env
           | Memory_fill -> self#visit_Memory_fill env
           | Memory_atomic_wait32 _visitors_c0 ->
               self#visit_Memory_atomic_wait32 env _visitors_c0
           | Memory_atomic_notify _visitors_c0 ->
               self#visit_Memory_atomic_notify env _visitors_c0
           | Ref_eq -> self#visit_Ref_eq env
           | Ref_as_non_null -> self#visit_Ref_as_non_null env
           | Ref_cast _visitors_c0 -> self#visit_Ref_cast env _visitors_c0
           | Ref_func _visitors_c0 -> self#visit_Ref_func env _visitors_c0
           | Ref_is_null -> self#visit_Ref_is_null env
           | Ref_null _visitors_c0 -> self#visit_Ref_null env _visitors_c0
           | Return -> self#visit_Return env
           | Struct_get (_visitors_c0, _visitors_c1) ->
               self#visit_Struct_get env _visitors_c0 _visitors_c1
           | Struct_new _visitors_c0 -> self#visit_Struct_new env _visitors_c0
           | Struct_new_default _visitors_c0 ->
               self#visit_Struct_new_default env _visitors_c0
           | Struct_set (_visitors_c0, _visitors_c1) ->
               self#visit_Struct_set env _visitors_c0 _visitors_c1
           | Table_get _visitors_c0 -> self#visit_Table_get env _visitors_c0
           | Unreachable -> self#visit_Unreachable env
           | Throw _visitors_c0 -> self#visit_Throw env _visitors_c0
           | Try_table
               {
                 label = _visitors_flabel;
                 typeuse = _visitors_ftypeuse;
                 catchs = _visitors_fcatchs;
                 instrs = _visitors_finstrs;
               } ->
               self#visit_Try_table env _visitors_flabel _visitors_ftypeuse
                 _visitors_fcatchs _visitors_finstrs
           | Select -> self#visit_Select env
           | No_op -> self#visit_No_op env
           | Source_pos _visitors_c0 -> self#visit_Source_pos env _visitors_c0
           | Prologue_end -> self#visit_Prologue_end env
    end

  [@@@VISITORS.END]
end

include struct
  let _ = fun (_ : instr) -> ()

  let rec equal_instr =
    (fun a__141_ ->
       fun b__142_ ->
        if Stdlib.( == ) a__141_ b__142_ then true
        else
          match (a__141_, b__142_) with
          | Any_convert_extern, Any_convert_extern -> true
          | Any_convert_extern, _ -> false
          | _, Any_convert_extern -> false
          | Array_copy (_a__143_, _a__145_), Array_copy (_b__144_, _b__146_) ->
              Stdlib.( && )
                (equal_typeidx _a__143_ _b__144_)
                (equal_typeidx _a__145_ _b__146_)
          | Array_copy _, _ -> false
          | _, Array_copy _ -> false
          | Array_fill _a__147_, Array_fill _b__148_ ->
              equal_typeidx _a__147_ _b__148_
          | Array_fill _, _ -> false
          | _, Array_fill _ -> false
          | Array_get _a__149_, Array_get _b__150_ ->
              equal_typeidx _a__149_ _b__150_
          | Array_get _, _ -> false
          | _, Array_get _ -> false
          | Array_get_u _a__151_, Array_get_u _b__152_ ->
              equal_typeidx _a__151_ _b__152_
          | Array_get_u _, _ -> false
          | _, Array_get_u _ -> false
          | Array_get_s _a__153_, Array_get_s _b__154_ ->
              equal_typeidx _a__153_ _b__154_
          | Array_get_s _, _ -> false
          | _, Array_get_s _ -> false
          | Array_len, Array_len -> true
          | Array_len, _ -> false
          | _, Array_len -> false
          | Array_new _a__155_, Array_new _b__156_ ->
              equal_typeidx _a__155_ _b__156_
          | Array_new _, _ -> false
          | _, Array_new _ -> false
          | ( Array_new_data (_a__157_, _a__159_),
              Array_new_data (_b__158_, _b__160_) ) ->
              Stdlib.( && )
                (equal_typeidx _a__157_ _b__158_)
                (equal_dataidx _a__159_ _b__160_)
          | Array_new_data _, _ -> false
          | _, Array_new_data _ -> false
          | Array_new_default _a__161_, Array_new_default _b__162_ ->
              equal_typeidx _a__161_ _b__162_
          | Array_new_default _, _ -> false
          | _, Array_new_default _ -> false
          | ( Array_new_fixed (_a__163_, _a__165_),
              Array_new_fixed (_b__164_, _b__166_) ) ->
              Stdlib.( && )
                (equal_typeidx _a__163_ _b__164_)
                (Stdlib.( = ) (_a__165_ : int32) _b__166_)
          | Array_new_fixed _, _ -> false
          | _, Array_new_fixed _ -> false
          | Array_set _a__167_, Array_set _b__168_ ->
              equal_typeidx _a__167_ _b__168_
          | Array_set _, _ -> false
          | _, Array_set _ -> false
          | Block _a__169_, Block _b__170_ ->
              Stdlib.( && )
                (equal_typeuse _a__169_.typeuse _b__170_.typeuse)
                (Ppx_base.equal_list
                   (fun a__171_ ->
                     fun (b__172_ [@merlin.hide]) ->
                      (equal_instr a__171_ b__172_ [@merlin.hide]))
                   _a__169_.instrs _b__170_.instrs)
          | Block _, _ -> false
          | _, Block _ -> false
          | Br _a__173_, Br _b__174_ -> equal_labelidx _a__173_ _b__174_
          | Br _, _ -> false
          | _, Br _ -> false
          | Br_if _a__175_, Br_if _b__176_ -> equal_labelidx _a__175_ _b__176_
          | Br_if _, _ -> false
          | _, Br_if _ -> false
          | Br_table (_a__177_, _a__179_), Br_table (_b__178_, _b__180_) ->
              Stdlib.( && )
                (Ppx_base.equal_list
                   (fun a__181_ ->
                     fun (b__182_ [@merlin.hide]) ->
                      (equal_labelidx a__181_ b__182_ [@merlin.hide]))
                   _a__177_ _b__178_)
                (equal_labelidx _a__179_ _b__180_)
          | Br_table _, _ -> false
          | _, Br_table _ -> false
          | Call _a__183_, Call _b__184_ -> equal_funcidx _a__183_ _b__184_
          | Call _, _ -> false
          | _, Call _ -> false
          | ( Call_indirect (_a__185_, _a__187_),
              Call_indirect (_b__186_, _b__188_) ) ->
              Stdlib.( && )
                (equal_tableidx _a__185_ _b__186_)
                (equal_typeuse _a__187_ _b__188_)
          | Call_indirect _, _ -> false
          | _, Call_indirect _ -> false
          | Call_ref _a__189_, Call_ref _b__190_ ->
              equal_typeidx _a__189_ _b__190_
          | Call_ref _, _ -> false
          | _, Call_ref _ -> false
          | Drop, Drop -> true
          | Drop, _ -> false
          | _, Drop -> false
          | Extern_convert_any, Extern_convert_any -> true
          | Extern_convert_any, _ -> false
          | _, Extern_convert_any -> false
          | F64_add, F64_add -> true
          | F64_add, _ -> false
          | _, F64_add -> false
          | F64_const (_a__191_, _a__193_), F64_const (_b__192_, _b__194_) ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__191_ : string) _b__192_)
                (Stdlib.( = ) (_a__193_ : float) _b__194_)
          | F64_const _, _ -> false
          | _, F64_const _ -> false
          | F64_convert_i32_s, F64_convert_i32_s -> true
          | F64_convert_i32_s, _ -> false
          | _, F64_convert_i32_s -> false
          | F64_convert_i32_u, F64_convert_i32_u -> true
          | F64_convert_i32_u, _ -> false
          | _, F64_convert_i32_u -> false
          | F64_convert_i64_s, F64_convert_i64_s -> true
          | F64_convert_i64_s, _ -> false
          | _, F64_convert_i64_s -> false
          | F64_convert_i64_u, F64_convert_i64_u -> true
          | F64_convert_i64_u, _ -> false
          | _, F64_convert_i64_u -> false
          | F64_div, F64_div -> true
          | F64_div, _ -> false
          | _, F64_div -> false
          | F64_eq, F64_eq -> true
          | F64_eq, _ -> false
          | _, F64_eq -> false
          | F64_ge, F64_ge -> true
          | F64_ge, _ -> false
          | _, F64_ge -> false
          | F64_gt, F64_gt -> true
          | F64_gt, _ -> false
          | _, F64_gt -> false
          | F64_le, F64_le -> true
          | F64_le, _ -> false
          | _, F64_le -> false
          | F64_load _a__195_, F64_load _b__196_ ->
              equal_memarg _a__195_ _b__196_
          | F64_load _, _ -> false
          | _, F64_load _ -> false
          | F64_lt, F64_lt -> true
          | F64_lt, _ -> false
          | _, F64_lt -> false
          | F64_mul, F64_mul -> true
          | F64_mul, _ -> false
          | _, F64_mul -> false
          | F64_ne, F64_ne -> true
          | F64_ne, _ -> false
          | _, F64_ne -> false
          | F64_neg, F64_neg -> true
          | F64_neg, _ -> false
          | _, F64_neg -> false
          | F64_reinterpret_i64, F64_reinterpret_i64 -> true
          | F64_reinterpret_i64, _ -> false
          | _, F64_reinterpret_i64 -> false
          | F64_store _a__197_, F64_store _b__198_ ->
              equal_memarg _a__197_ _b__198_
          | F64_store _, _ -> false
          | _, F64_store _ -> false
          | F64_sub, F64_sub -> true
          | F64_sub, _ -> false
          | _, F64_sub -> false
          | F64_sqrt, F64_sqrt -> true
          | F64_sqrt, _ -> false
          | _, F64_sqrt -> false
          | F64_abs, F64_abs -> true
          | F64_abs, _ -> false
          | _, F64_abs -> false
          | F64_trunc, F64_trunc -> true
          | F64_trunc, _ -> false
          | _, F64_trunc -> false
          | F64_floor, F64_floor -> true
          | F64_floor, _ -> false
          | _, F64_floor -> false
          | F64_ceil, F64_ceil -> true
          | F64_ceil, _ -> false
          | _, F64_ceil -> false
          | F64_nearest, F64_nearest -> true
          | F64_nearest, _ -> false
          | _, F64_nearest -> false
          | F32_add, F32_add -> true
          | F32_add, _ -> false
          | _, F32_add -> false
          | F32_const (_a__199_, _a__201_), F32_const (_b__200_, _b__202_) ->
              Stdlib.( && )
                (Stdlib.( = ) (_a__199_ : string) _b__200_)
                (Stdlib.( = ) (_a__201_ : float) _b__202_)
          | F32_const _, _ -> false
          | _, F32_const _ -> false
          | F32_convert_i32_s, F32_convert_i32_s -> true
          | F32_convert_i32_s, _ -> false
          | _, F32_convert_i32_s -> false
          | F32_convert_i32_u, F32_convert_i32_u -> true
          | F32_convert_i32_u, _ -> false
          | _, F32_convert_i32_u -> false
          | F32_convert_i64_s, F32_convert_i64_s -> true
          | F32_convert_i64_s, _ -> false
          | _, F32_convert_i64_s -> false
          | F32_convert_i64_u, F32_convert_i64_u -> true
          | F32_convert_i64_u, _ -> false
          | _, F32_convert_i64_u -> false
          | F32_demote_f64, F32_demote_f64 -> true
          | F32_demote_f64, _ -> false
          | _, F32_demote_f64 -> false
          | F32_div, F32_div -> true
          | F32_div, _ -> false
          | _, F32_div -> false
          | F32_eq, F32_eq -> true
          | F32_eq, _ -> false
          | _, F32_eq -> false
          | F32_ge, F32_ge -> true
          | F32_ge, _ -> false
          | _, F32_ge -> false
          | F32_gt, F32_gt -> true
          | F32_gt, _ -> false
          | _, F32_gt -> false
          | F32_le, F32_le -> true
          | F32_le, _ -> false
          | _, F32_le -> false
          | F32_load _a__203_, F32_load _b__204_ ->
              equal_memarg _a__203_ _b__204_
          | F32_load _, _ -> false
          | _, F32_load _ -> false
          | F32_lt, F32_lt -> true
          | F32_lt, _ -> false
          | _, F32_lt -> false
          | F32_mul, F32_mul -> true
          | F32_mul, _ -> false
          | _, F32_mul -> false
          | F32_ne, F32_ne -> true
          | F32_ne, _ -> false
          | _, F32_ne -> false
          | F32_neg, F32_neg -> true
          | F32_neg, _ -> false
          | _, F32_neg -> false
          | F32_reinterpret_i32, F32_reinterpret_i32 -> true
          | F32_reinterpret_i32, _ -> false
          | _, F32_reinterpret_i32 -> false
          | F32_sqrt, F32_sqrt -> true
          | F32_sqrt, _ -> false
          | _, F32_sqrt -> false
          | F32_store _a__205_, F32_store _b__206_ ->
              equal_memarg _a__205_ _b__206_
          | F32_store _, _ -> false
          | _, F32_store _ -> false
          | F32_sub, F32_sub -> true
          | F32_sub, _ -> false
          | _, F32_sub -> false
          | F32_abs, F32_abs -> true
          | F32_abs, _ -> false
          | _, F32_abs -> false
          | F32_trunc, F32_trunc -> true
          | F32_trunc, _ -> false
          | _, F32_trunc -> false
          | F32_floor, F32_floor -> true
          | F32_floor, _ -> false
          | _, F32_floor -> false
          | F32_ceil, F32_ceil -> true
          | F32_ceil, _ -> false
          | _, F32_ceil -> false
          | F32_nearest, F32_nearest -> true
          | F32_nearest, _ -> false
          | _, F32_nearest -> false
          | F64_promote_f32, F64_promote_f32 -> true
          | F64_promote_f32, _ -> false
          | _, F64_promote_f32 -> false
          | I32_reinterpret_f32, I32_reinterpret_f32 -> true
          | I32_reinterpret_f32, _ -> false
          | _, I32_reinterpret_f32 -> false
          | I32_trunc_f32_s, I32_trunc_f32_s -> true
          | I32_trunc_f32_s, _ -> false
          | _, I32_trunc_f32_s -> false
          | I32_trunc_f32_u, I32_trunc_f32_u -> true
          | I32_trunc_f32_u, _ -> false
          | _, I32_trunc_f32_u -> false
          | I64_trunc_f32_s, I64_trunc_f32_s -> true
          | I64_trunc_f32_s, _ -> false
          | _, I64_trunc_f32_s -> false
          | I64_trunc_f32_u, I64_trunc_f32_u -> true
          | I64_trunc_f32_u, _ -> false
          | _, I64_trunc_f32_u -> false
          | Global_get _a__207_, Global_get _b__208_ ->
              equal_globalidx _a__207_ _b__208_
          | Global_get _, _ -> false
          | _, Global_get _ -> false
          | Global_set _a__209_, Global_set _b__210_ ->
              equal_globalidx _a__209_ _b__210_
          | Global_set _, _ -> false
          | _, Global_set _ -> false
          | I32_add, I32_add -> true
          | I32_add, _ -> false
          | _, I32_add -> false
          | I32_and, I32_and -> true
          | I32_and, _ -> false
          | _, I32_and -> false
          | I32_clz, I32_clz -> true
          | I32_clz, _ -> false
          | _, I32_clz -> false
          | I32_const _a__211_, I32_const _b__212_ ->
              Stdlib.( = ) (_a__211_ : int32) _b__212_
          | I32_const _, _ -> false
          | _, I32_const _ -> false
          | I32_ctz, I32_ctz -> true
          | I32_ctz, _ -> false
          | _, I32_ctz -> false
          | I32_div_s, I32_div_s -> true
          | I32_div_s, _ -> false
          | _, I32_div_s -> false
          | I32_div_u, I32_div_u -> true
          | I32_div_u, _ -> false
          | _, I32_div_u -> false
          | I32_eq, I32_eq -> true
          | I32_eq, _ -> false
          | _, I32_eq -> false
          | I32_eqz, I32_eqz -> true
          | I32_eqz, _ -> false
          | _, I32_eqz -> false
          | I32_ge_s, I32_ge_s -> true
          | I32_ge_s, _ -> false
          | _, I32_ge_s -> false
          | I32_ge_u, I32_ge_u -> true
          | I32_ge_u, _ -> false
          | _, I32_ge_u -> false
          | I32_gt_s, I32_gt_s -> true
          | I32_gt_s, _ -> false
          | _, I32_gt_s -> false
          | I32_gt_u, I32_gt_u -> true
          | I32_gt_u, _ -> false
          | _, I32_gt_u -> false
          | I32_le_s, I32_le_s -> true
          | I32_le_s, _ -> false
          | _, I32_le_s -> false
          | I32_le_u, I32_le_u -> true
          | I32_le_u, _ -> false
          | _, I32_le_u -> false
          | I32_load _a__213_, I32_load _b__214_ ->
              equal_memarg _a__213_ _b__214_
          | I32_load _, _ -> false
          | _, I32_load _ -> false
          | I32_load16_u _a__215_, I32_load16_u _b__216_ ->
              equal_memarg _a__215_ _b__216_
          | I32_load16_u _, _ -> false
          | _, I32_load16_u _ -> false
          | I32_load16_s _a__217_, I32_load16_s _b__218_ ->
              equal_memarg _a__217_ _b__218_
          | I32_load16_s _, _ -> false
          | _, I32_load16_s _ -> false
          | I32_load8_u _a__219_, I32_load8_u _b__220_ ->
              equal_memarg _a__219_ _b__220_
          | I32_load8_u _, _ -> false
          | _, I32_load8_u _ -> false
          | I32_load8_s _a__221_, I32_load8_s _b__222_ ->
              equal_memarg _a__221_ _b__222_
          | I32_load8_s _, _ -> false
          | _, I32_load8_s _ -> false
          | I32_lt_s, I32_lt_s -> true
          | I32_lt_s, _ -> false
          | _, I32_lt_s -> false
          | I32_lt_u, I32_lt_u -> true
          | I32_lt_u, _ -> false
          | _, I32_lt_u -> false
          | I32_mul, I32_mul -> true
          | I32_mul, _ -> false
          | _, I32_mul -> false
          | I32_ne, I32_ne -> true
          | I32_ne, _ -> false
          | _, I32_ne -> false
          | I32_or, I32_or -> true
          | I32_or, _ -> false
          | _, I32_or -> false
          | I32_popcnt, I32_popcnt -> true
          | I32_popcnt, _ -> false
          | _, I32_popcnt -> false
          | I32_rem_s, I32_rem_s -> true
          | I32_rem_s, _ -> false
          | _, I32_rem_s -> false
          | I32_rem_u, I32_rem_u -> true
          | I32_rem_u, _ -> false
          | _, I32_rem_u -> false
          | I32_shl, I32_shl -> true
          | I32_shl, _ -> false
          | _, I32_shl -> false
          | I32_shr_s, I32_shr_s -> true
          | I32_shr_s, _ -> false
          | _, I32_shr_s -> false
          | I32_shr_u, I32_shr_u -> true
          | I32_shr_u, _ -> false
          | _, I32_shr_u -> false
          | I32_rotl, I32_rotl -> true
          | I32_rotl, _ -> false
          | _, I32_rotl -> false
          | I32_store _a__223_, I32_store _b__224_ ->
              equal_memarg _a__223_ _b__224_
          | I32_store _, _ -> false
          | _, I32_store _ -> false
          | I32_store16 _a__225_, I32_store16 _b__226_ ->
              equal_memarg _a__225_ _b__226_
          | I32_store16 _, _ -> false
          | _, I32_store16 _ -> false
          | I32_store8 _a__227_, I32_store8 _b__228_ ->
              equal_memarg _a__227_ _b__228_
          | I32_store8 _, _ -> false
          | _, I32_store8 _ -> false
          | I32_sub, I32_sub -> true
          | I32_sub, _ -> false
          | _, I32_sub -> false
          | I32_trunc_f64_s, I32_trunc_f64_s -> true
          | I32_trunc_f64_s, _ -> false
          | _, I32_trunc_f64_s -> false
          | I32_trunc_f64_u, I32_trunc_f64_u -> true
          | I32_trunc_f64_u, _ -> false
          | _, I32_trunc_f64_u -> false
          | I32_wrap_i64, I32_wrap_i64 -> true
          | I32_wrap_i64, _ -> false
          | _, I32_wrap_i64 -> false
          | I32_xor, I32_xor -> true
          | I32_xor, _ -> false
          | _, I32_xor -> false
          | I32_extend_8_s, I32_extend_8_s -> true
          | I32_extend_8_s, _ -> false
          | _, I32_extend_8_s -> false
          | I32_extend_16_s, I32_extend_16_s -> true
          | I32_extend_16_s, _ -> false
          | _, I32_extend_16_s -> false
          | I32_trunc_sat_f32_s, I32_trunc_sat_f32_s -> true
          | I32_trunc_sat_f32_s, _ -> false
          | _, I32_trunc_sat_f32_s -> false
          | I32_trunc_sat_f32_u, I32_trunc_sat_f32_u -> true
          | I32_trunc_sat_f32_u, _ -> false
          | _, I32_trunc_sat_f32_u -> false
          | I32_trunc_sat_f64_s, I32_trunc_sat_f64_s -> true
          | I32_trunc_sat_f64_s, _ -> false
          | _, I32_trunc_sat_f64_s -> false
          | I32_trunc_sat_f64_u, I32_trunc_sat_f64_u -> true
          | I32_trunc_sat_f64_u, _ -> false
          | _, I32_trunc_sat_f64_u -> false
          | I32_atomic_load _a__229_, I32_atomic_load _b__230_ ->
              equal_memarg _a__229_ _b__230_
          | I32_atomic_load _, _ -> false
          | _, I32_atomic_load _ -> false
          | I32_atomic_load8_u _a__231_, I32_atomic_load8_u _b__232_ ->
              equal_memarg _a__231_ _b__232_
          | I32_atomic_load8_u _, _ -> false
          | _, I32_atomic_load8_u _ -> false
          | I32_atomic_load16_u _a__233_, I32_atomic_load16_u _b__234_ ->
              equal_memarg _a__233_ _b__234_
          | I32_atomic_load16_u _, _ -> false
          | _, I32_atomic_load16_u _ -> false
          | I32_atomic_store _a__235_, I32_atomic_store _b__236_ ->
              equal_memarg _a__235_ _b__236_
          | I32_atomic_store _, _ -> false
          | _, I32_atomic_store _ -> false
          | I32_atomic_store8 _a__237_, I32_atomic_store8 _b__238_ ->
              equal_memarg _a__237_ _b__238_
          | I32_atomic_store8 _, _ -> false
          | _, I32_atomic_store8 _ -> false
          | I32_atomic_store16 _a__239_, I32_atomic_store16 _b__240_ ->
              equal_memarg _a__239_ _b__240_
          | I32_atomic_store16 _, _ -> false
          | _, I32_atomic_store16 _ -> false
          | I32_atomic_rmw_cmpxchg _a__241_, I32_atomic_rmw_cmpxchg _b__242_ ->
              equal_memarg _a__241_ _b__242_
          | I32_atomic_rmw_cmpxchg _, _ -> false
          | _, I32_atomic_rmw_cmpxchg _ -> false
          | ( I32_atomic_rmw8_cmpxchg_u _a__243_,
              I32_atomic_rmw8_cmpxchg_u _b__244_ ) ->
              equal_memarg _a__243_ _b__244_
          | I32_atomic_rmw8_cmpxchg_u _, _ -> false
          | _, I32_atomic_rmw8_cmpxchg_u _ -> false
          | ( I32_atomic_rmw16_cmpxchg_u _a__245_,
              I32_atomic_rmw16_cmpxchg_u _b__246_ ) ->
              equal_memarg _a__245_ _b__246_
          | I32_atomic_rmw16_cmpxchg_u _, _ -> false
          | _, I32_atomic_rmw16_cmpxchg_u _ -> false
          | I64_add, I64_add -> true
          | I64_add, _ -> false
          | _, I64_add -> false
          | I64_and, I64_and -> true
          | I64_and, _ -> false
          | _, I64_and -> false
          | I64_clz, I64_clz -> true
          | I64_clz, _ -> false
          | _, I64_clz -> false
          | I64_const _a__247_, I64_const _b__248_ ->
              Stdlib.( = ) (_a__247_ : int64) _b__248_
          | I64_const _, _ -> false
          | _, I64_const _ -> false
          | I64_ctz, I64_ctz -> true
          | I64_ctz, _ -> false
          | _, I64_ctz -> false
          | I64_div_s, I64_div_s -> true
          | I64_div_s, _ -> false
          | _, I64_div_s -> false
          | I64_div_u, I64_div_u -> true
          | I64_div_u, _ -> false
          | _, I64_div_u -> false
          | I64_eq, I64_eq -> true
          | I64_eq, _ -> false
          | _, I64_eq -> false
          | I64_extend_i32_s, I64_extend_i32_s -> true
          | I64_extend_i32_s, _ -> false
          | _, I64_extend_i32_s -> false
          | I64_extend_i32_u, I64_extend_i32_u -> true
          | I64_extend_i32_u, _ -> false
          | _, I64_extend_i32_u -> false
          | I64_ge_s, I64_ge_s -> true
          | I64_ge_s, _ -> false
          | _, I64_ge_s -> false
          | I64_gt_s, I64_gt_s -> true
          | I64_gt_s, _ -> false
          | _, I64_gt_s -> false
          | I64_le_s, I64_le_s -> true
          | I64_le_s, _ -> false
          | _, I64_le_s -> false
          | I64_ge_u, I64_ge_u -> true
          | I64_ge_u, _ -> false
          | _, I64_ge_u -> false
          | I64_gt_u, I64_gt_u -> true
          | I64_gt_u, _ -> false
          | _, I64_gt_u -> false
          | I64_le_u, I64_le_u -> true
          | I64_le_u, _ -> false
          | _, I64_le_u -> false
          | I64_load _a__249_, I64_load _b__250_ ->
              equal_memarg _a__249_ _b__250_
          | I64_load _, _ -> false
          | _, I64_load _ -> false
          | I64_load32_u _a__251_, I64_load32_u _b__252_ ->
              equal_memarg _a__251_ _b__252_
          | I64_load32_u _, _ -> false
          | _, I64_load32_u _ -> false
          | I64_load32_s _a__253_, I64_load32_s _b__254_ ->
              equal_memarg _a__253_ _b__254_
          | I64_load32_s _, _ -> false
          | _, I64_load32_s _ -> false
          | I64_load16_u _a__255_, I64_load16_u _b__256_ ->
              equal_memarg _a__255_ _b__256_
          | I64_load16_u _, _ -> false
          | _, I64_load16_u _ -> false
          | I64_load16_s _a__257_, I64_load16_s _b__258_ ->
              equal_memarg _a__257_ _b__258_
          | I64_load16_s _, _ -> false
          | _, I64_load16_s _ -> false
          | I64_load8_u _a__259_, I64_load8_u _b__260_ ->
              equal_memarg _a__259_ _b__260_
          | I64_load8_u _, _ -> false
          | _, I64_load8_u _ -> false
          | I64_load8_s _a__261_, I64_load8_s _b__262_ ->
              equal_memarg _a__261_ _b__262_
          | I64_load8_s _, _ -> false
          | _, I64_load8_s _ -> false
          | I64_lt_s, I64_lt_s -> true
          | I64_lt_s, _ -> false
          | _, I64_lt_s -> false
          | I64_lt_u, I64_lt_u -> true
          | I64_lt_u, _ -> false
          | _, I64_lt_u -> false
          | I64_mul, I64_mul -> true
          | I64_mul, _ -> false
          | _, I64_mul -> false
          | I64_ne, I64_ne -> true
          | I64_ne, _ -> false
          | _, I64_ne -> false
          | I64_or, I64_or -> true
          | I64_or, _ -> false
          | _, I64_or -> false
          | I64_popcnt, I64_popcnt -> true
          | I64_popcnt, _ -> false
          | _, I64_popcnt -> false
          | I64_reinterpret_f64, I64_reinterpret_f64 -> true
          | I64_reinterpret_f64, _ -> false
          | _, I64_reinterpret_f64 -> false
          | I64_rem_s, I64_rem_s -> true
          | I64_rem_s, _ -> false
          | _, I64_rem_s -> false
          | I64_rem_u, I64_rem_u -> true
          | I64_rem_u, _ -> false
          | _, I64_rem_u -> false
          | I64_shl, I64_shl -> true
          | I64_shl, _ -> false
          | _, I64_shl -> false
          | I64_shr_s, I64_shr_s -> true
          | I64_shr_s, _ -> false
          | _, I64_shr_s -> false
          | I64_shr_u, I64_shr_u -> true
          | I64_shr_u, _ -> false
          | _, I64_shr_u -> false
          | I64_store _a__263_, I64_store _b__264_ ->
              equal_memarg _a__263_ _b__264_
          | I64_store _, _ -> false
          | _, I64_store _ -> false
          | I64_store32 _a__265_, I64_store32 _b__266_ ->
              equal_memarg _a__265_ _b__266_
          | I64_store32 _, _ -> false
          | _, I64_store32 _ -> false
          | I64_store16 _a__267_, I64_store16 _b__268_ ->
              equal_memarg _a__267_ _b__268_
          | I64_store16 _, _ -> false
          | _, I64_store16 _ -> false
          | I64_store8 _a__269_, I64_store8 _b__270_ ->
              equal_memarg _a__269_ _b__270_
          | I64_store8 _, _ -> false
          | _, I64_store8 _ -> false
          | I64_sub, I64_sub -> true
          | I64_sub, _ -> false
          | _, I64_sub -> false
          | I64_trunc_f64_s, I64_trunc_f64_s -> true
          | I64_trunc_f64_s, _ -> false
          | _, I64_trunc_f64_s -> false
          | I64_trunc_f64_u, I64_trunc_f64_u -> true
          | I64_trunc_f64_u, _ -> false
          | _, I64_trunc_f64_u -> false
          | I64_xor, I64_xor -> true
          | I64_xor, _ -> false
          | _, I64_xor -> false
          | I64_extend_8_s, I64_extend_8_s -> true
          | I64_extend_8_s, _ -> false
          | _, I64_extend_8_s -> false
          | I64_extend_16_s, I64_extend_16_s -> true
          | I64_extend_16_s, _ -> false
          | _, I64_extend_16_s -> false
          | I64_extend_32_s, I64_extend_32_s -> true
          | I64_extend_32_s, _ -> false
          | _, I64_extend_32_s -> false
          | I64_trunc_sat_f32_s, I64_trunc_sat_f32_s -> true
          | I64_trunc_sat_f32_s, _ -> false
          | _, I64_trunc_sat_f32_s -> false
          | I64_trunc_sat_f32_u, I64_trunc_sat_f32_u -> true
          | I64_trunc_sat_f32_u, _ -> false
          | _, I64_trunc_sat_f32_u -> false
          | I64_trunc_sat_f64_s, I64_trunc_sat_f64_s -> true
          | I64_trunc_sat_f64_s, _ -> false
          | _, I64_trunc_sat_f64_s -> false
          | I64_trunc_sat_f64_u, I64_trunc_sat_f64_u -> true
          | I64_trunc_sat_f64_u, _ -> false
          | _, I64_trunc_sat_f64_u -> false
          | V128_load _a__271_, V128_load _b__272_ ->
              equal_memarg _a__271_ _b__272_
          | V128_load _, _ -> false
          | _, V128_load _ -> false
          | V128_store _a__273_, V128_store _b__274_ ->
              equal_memarg _a__273_ _b__274_
          | V128_store _, _ -> false
          | _, V128_store _ -> false
          | F64x2_add, F64x2_add -> true
          | F64x2_add, _ -> false
          | _, F64x2_add -> false
          | F64x2_mul, F64x2_mul -> true
          | F64x2_mul, _ -> false
          | _, F64x2_mul -> false
          | F32x4_add, F32x4_add -> true
          | F32x4_add, _ -> false
          | _, F32x4_add -> false
          | F32x4_mul, F32x4_mul -> true
          | F32x4_mul, _ -> false
          | _, F32x4_mul -> false
          | If _a__275_, If _b__276_ ->
              Stdlib.( && )
                (equal_typeuse _a__275_.typeuse _b__276_.typeuse)
                (Stdlib.( && )
                   (Ppx_base.equal_list
                      (fun a__277_ ->
                        fun (b__278_ [@merlin.hide]) ->
                         (equal_instr a__277_ b__278_ [@merlin.hide]))
                      _a__275_.then_ _b__276_.then_)
                   (Ppx_base.equal_list
                      (fun a__279_ ->
                        fun (b__280_ [@merlin.hide]) ->
                         (equal_instr a__279_ b__280_ [@merlin.hide]))
                      _a__275_.else_ _b__276_.else_))
          | If _, _ -> false
          | _, If _ -> false
          | Local_get _a__281_, Local_get _b__282_ ->
              equal_localidx _a__281_ _b__282_
          | Local_get _, _ -> false
          | _, Local_get _ -> false
          | Local_set _a__283_, Local_set _b__284_ ->
              equal_localidx _a__283_ _b__284_
          | Local_set _, _ -> false
          | _, Local_set _ -> false
          | Local_tee _a__285_, Local_tee _b__286_ ->
              equal_localidx _a__285_ _b__286_
          | Local_tee _, _ -> false
          | _, Local_tee _ -> false
          | Loop _a__287_, Loop _b__288_ ->
              Stdlib.( && )
                (equal_typeuse _a__287_.typeuse _b__288_.typeuse)
                (Ppx_base.equal_list
                   (fun a__289_ ->
                     fun (b__290_ [@merlin.hide]) ->
                      (equal_instr a__289_ b__290_ [@merlin.hide]))
                   _a__287_.instrs _b__288_.instrs)
          | Loop _, _ -> false
          | _, Loop _ -> false
          | Memory_init _a__291_, Memory_init _b__292_ ->
              equal_dataidx _a__291_ _b__292_
          | Memory_init _, _ -> false
          | _, Memory_init _ -> false
          | Memory_copy, Memory_copy -> true
          | Memory_copy, _ -> false
          | _, Memory_copy -> false
          | Memory_grow, Memory_grow -> true
          | Memory_grow, _ -> false
          | _, Memory_grow -> false
          | Memory_size, Memory_size -> true
          | Memory_size, _ -> false
          | _, Memory_size -> false
          | Memory_fill, Memory_fill -> true
          | Memory_fill, _ -> false
          | _, Memory_fill -> false
          | Memory_atomic_wait32 _a__293_, Memory_atomic_wait32 _b__294_ ->
              equal_memarg _a__293_ _b__294_
          | Memory_atomic_wait32 _, _ -> false
          | _, Memory_atomic_wait32 _ -> false
          | Memory_atomic_notify _a__295_, Memory_atomic_notify _b__296_ ->
              equal_memarg _a__295_ _b__296_
          | Memory_atomic_notify _, _ -> false
          | _, Memory_atomic_notify _ -> false
          | Ref_eq, Ref_eq -> true
          | Ref_eq, _ -> false
          | _, Ref_eq -> false
          | Ref_as_non_null, Ref_as_non_null -> true
          | Ref_as_non_null, _ -> false
          | _, Ref_as_non_null -> false
          | Ref_cast _a__297_, Ref_cast _b__298_ ->
              equal_reftype _a__297_ _b__298_
          | Ref_cast _, _ -> false
          | _, Ref_cast _ -> false
          | Ref_func _a__299_, Ref_func _b__300_ ->
              equal_funcidx _a__299_ _b__300_
          | Ref_func _, _ -> false
          | _, Ref_func _ -> false
          | Ref_is_null, Ref_is_null -> true
          | Ref_is_null, _ -> false
          | _, Ref_is_null -> false
          | Ref_null _a__301_, Ref_null _b__302_ ->
              equal_heaptype _a__301_ _b__302_
          | Ref_null _, _ -> false
          | _, Ref_null _ -> false
          | Return, Return -> true
          | Return, _ -> false
          | _, Return -> false
          | Struct_get (_a__303_, _a__305_), Struct_get (_b__304_, _b__306_) ->
              Stdlib.( && )
                (equal_typeidx _a__303_ _b__304_)
                (equal_fieldidx _a__305_ _b__306_)
          | Struct_get _, _ -> false
          | _, Struct_get _ -> false
          | Struct_new _a__307_, Struct_new _b__308_ ->
              equal_typeidx _a__307_ _b__308_
          | Struct_new _, _ -> false
          | _, Struct_new _ -> false
          | Struct_new_default _a__309_, Struct_new_default _b__310_ ->
              equal_typeidx _a__309_ _b__310_
          | Struct_new_default _, _ -> false
          | _, Struct_new_default _ -> false
          | Struct_set (_a__311_, _a__313_), Struct_set (_b__312_, _b__314_) ->
              Stdlib.( && )
                (equal_typeidx _a__311_ _b__312_)
                (equal_fieldidx _a__313_ _b__314_)
          | Struct_set _, _ -> false
          | _, Struct_set _ -> false
          | Table_get _a__315_, Table_get _b__316_ ->
              equal_tableidx _a__315_ _b__316_
          | Table_get _, _ -> false
          | _, Table_get _ -> false
          | Unreachable, Unreachable -> true
          | Unreachable, _ -> false
          | _, Unreachable -> false
          | Throw _a__317_, Throw _b__318_ -> equal_tagidx _a__317_ _b__318_
          | Throw _, _ -> false
          | _, Throw _ -> false
          | Try_table _a__319_, Try_table _b__320_ ->
              Stdlib.( && )
                (equal_typeuse _a__319_.typeuse _b__320_.typeuse)
                (Stdlib.( && )
                   (equal_catchs _a__319_.catchs _b__320_.catchs)
                   (Ppx_base.equal_list
                      (fun a__321_ ->
                        fun (b__322_ [@merlin.hide]) ->
                         (equal_instr a__321_ b__322_ [@merlin.hide]))
                      _a__319_.instrs _b__320_.instrs))
          | Try_table _, _ -> false
          | _, Try_table _ -> false
          | Select, Select -> true
          | Select, _ -> false
          | _, Select -> false
          | No_op, No_op -> true
          | No_op, _ -> false
          | _, No_op -> false
          | Source_pos _a__323_, Source_pos _b__324_ ->
              equal_source_pos _a__323_ _b__324_
          | Source_pos _, _ -> false
          | _, Source_pos _ -> false
          | Prologue_end, Prologue_end -> true
      : instr -> instr -> bool)

  let _ = equal_instr
end

type expr = instr list

include struct
  let _ = fun (_ : expr) -> ()

  let equal_expr =
    (fun a__325_ ->
       fun b__326_ ->
        Ppx_base.equal_list
          (fun a__327_ -> fun b__328_ -> equal_instr a__327_ b__328_)
          a__325_ b__326_
      : expr -> expr -> bool)

  let _ = equal_expr
end

type extra_info = { mutable low_pc : int; mutable high_pc : int }

type func = {
  id : binder;
  type_ : typeuse;
  locals : local list;
  code : instr list;
  source_name : string option;
  aux : extra_info;
}

type table = { id : binder; type_ : tabletype; init : expr }

let default_table_name = "$moonbit.global"

type mem = { id : binder; type_ : memtype }

let default_memory_name = "$moonbit.memory"

type global = { id : binder; type_ : globaltype; init : expr }

type exportdesc =
  | Func of funcidx
  | Table of tableidx
  | Memory of memidx
  | Global of globalidx

type export = { name : string; desc : exportdesc }
type start = funcidx
type elemmode = EMPassive | EMActive of tableidx * expr | EMDeclarative
type elem = { id : binder; type_ : reftype; list : expr list; mode : elemmode }
type datamode = DMPassive | DMActive of memidx * expr
type data = { id : binder; data_str : string; mode : datamode }
type tag = { id : binder; type_ : typeuse }

type modulefield =
  | Rectype of rectype
  | Import of import
  | Func of func
  | Table of table
  | Mem of mem
  | Global of global
  | Export of export
  | Start of start
  | Elem of elem
  | Data of data
  | Tag of tag

type module_ = { id : binder; fields : modulefield list }
