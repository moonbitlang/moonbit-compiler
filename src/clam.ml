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


module Ltype = Ltype_gc
module Tid = Basic_ty_ident
module Lst = Basic_lst

type constant = Constant.t

include struct
  let _ = fun (_ : constant) -> ()
  let sexp_of_constant = (Constant.sexp_of_t : constant -> S.t)
  let _ = sexp_of_constant
end

type constr_tag = Tag.t

include struct
  let _ = fun (_ : constr_tag) -> ()
  let sexp_of_constr_tag = (Tag.sexp_of_t : constr_tag -> S.t)
  let _ = sexp_of_constr_tag
end

type prim = Primitive.prim

include struct
  let _ = fun (_ : prim) -> ()
  let sexp_of_prim = (Primitive.sexp_of_prim : prim -> S.t)
  let _ = sexp_of_prim
end

type ltype = Ltype.t

include struct
  let _ = fun (_ : ltype) -> ()
  let sexp_of_ltype = (Ltype.sexp_of_t : ltype -> S.t)
  let _ = sexp_of_ltype
end

type func_stubs = Stub_type.t

include struct
  let _ = fun (_ : func_stubs) -> ()
  let sexp_of_func_stubs = (Stub_type.sexp_of_t : func_stubs -> S.t)
  let _ = sexp_of_func_stubs
end

module Ident = Clam_ident

type location = Loc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Loc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type binder = Ident.t

include struct
  let _ = fun (_ : binder) -> ()
  let sexp_of_binder = (Ident.sexp_of_t : binder -> S.t)
  let _ = sexp_of_binder
end

type var = Ident.t

include struct
  let _ = fun (_ : var) -> ()
  let sexp_of_var = (Ident.sexp_of_t : var -> S.t)
  let _ = sexp_of_var
end

type address = Basic_fn_address.t

include struct
  let _ = fun (_ : address) -> ()
  let sexp_of_address = (Basic_fn_address.sexp_of_t : address -> S.t)
  let _ = sexp_of_address
end

type join = Join.t

include struct
  let _ = fun (_ : join) -> ()
  let sexp_of_join = (Join.sexp_of_t : join -> S.t)
  let _ = sexp_of_join
end

type label = Label.t

include struct
  let _ = fun (_ : label) -> ()
  let sexp_of_label = (Label.sexp_of_t : label -> S.t)
  let _ = sexp_of_label
end

type type_defs = Ltype.type_defs

include struct
  let _ = fun (_ : type_defs) -> ()
  let sexp_of_type_defs = (Ltype.sexp_of_type_defs : type_defs -> S.t)
  let _ = sexp_of_type_defs
end

type tid = Tid.t

include struct
  let _ = fun (_ : tid) -> ()
  let sexp_of_tid = (Tid.sexp_of_t : tid -> S.t)
  let _ = sexp_of_tid
end

type fn_kind = Top_pub of string | Top_private

include struct
  let _ = fun (_ : fn_kind) -> ()

  let sexp_of_fn_kind =
    (function
     | Top_pub arg0__001_ ->
         let res0__002_ = Moon_sexp_conv.sexp_of_string arg0__001_ in
         S.List [ S.Atom "Top_pub"; res0__002_ ]
     | Top_private -> S.Atom "Top_private"
      : fn_kind -> S.t)

  let _ = sexp_of_fn_kind
end

type join_kind = Tail_join | Nontail_join

include struct
  let _ = fun (_ : join_kind) -> ()

  let sexp_of_join_kind =
    (function
     | Tail_join -> S.Atom "Tail_join"
     | Nontail_join -> S.Atom "Nontail_join"
      : join_kind -> S.t)

  let _ = sexp_of_join_kind
end

type alloc_kind =
  | Tuple
  | Struct
  | Enum of { tag : constr_tag }
  | Object of { methods : address list }

include struct
  let _ = fun (_ : alloc_kind) -> ()

  let sexp_of_alloc_kind =
    (function
     | Tuple -> S.Atom "Tuple"
     | Struct -> S.Atom "Struct"
     | Enum { tag = tag__004_ } ->
         let bnds__003_ = ([] : _ Stdlib.List.t) in
         let bnds__003_ =
           let arg__005_ = sexp_of_constr_tag tag__004_ in
           (S.List [ S.Atom "tag"; arg__005_ ] :: bnds__003_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Enum" :: bnds__003_)
     | Object { methods = methods__007_ } ->
         let bnds__006_ = ([] : _ Stdlib.List.t) in
         let bnds__006_ =
           let arg__008_ =
             Moon_sexp_conv.sexp_of_list sexp_of_address methods__007_
           in
           (S.List [ S.Atom "methods"; arg__008_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Object" :: bnds__006_)
      : alloc_kind -> S.t)

  let _ = sexp_of_alloc_kind
end

type get_field_kind =
  | Tuple
  | Struct
  | Enum
  | Object of { number_of_methods : int }

include struct
  let _ = fun (_ : get_field_kind) -> ()

  let sexp_of_get_field_kind =
    (function
     | Tuple -> S.Atom "Tuple"
     | Struct -> S.Atom "Struct"
     | Enum -> S.Atom "Enum"
     | Object { number_of_methods = number_of_methods__010_ } ->
         let bnds__009_ = ([] : _ Stdlib.List.t) in
         let bnds__009_ =
           let arg__011_ = Moon_sexp_conv.sexp_of_int number_of_methods__010_ in
           (S.List [ S.Atom "number_of_methods"; arg__011_ ] :: bnds__009_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Object" :: bnds__009_)
      : get_field_kind -> S.t)

  let _ = sexp_of_get_field_kind
end

type set_field_kind = Struct | Enum

include struct
  let _ = fun (_ : set_field_kind) -> ()

  let sexp_of_set_field_kind =
    (function Struct -> S.Atom "Struct" | Enum -> S.Atom "Enum"
      : set_field_kind -> S.t)

  let _ = sexp_of_set_field_kind
end

type closure_address = Normal of address | Well_known_mut_rec

type get_item_extra =
  | Need_non_null_cast
  | Need_signed_info of { signed : bool }
  | No_extra

include struct
  let _ = fun (_ : get_item_extra) -> ()

  let sexp_of_get_item_extra =
    (function
     | Need_non_null_cast -> S.Atom "Need_non_null_cast"
     | Need_signed_info { signed = signed__013_ } ->
         let bnds__012_ = ([] : _ Stdlib.List.t) in
         let bnds__012_ =
           let arg__014_ = Moon_sexp_conv.sexp_of_bool signed__013_ in
           (S.List [ S.Atom "signed"; arg__014_ ] :: bnds__012_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Need_signed_info" :: bnds__012_)
     | No_extra -> S.Atom "No_extra"
      : get_item_extra -> S.t)

  let _ = sexp_of_get_item_extra
end

let sexp_of_closure_address addr =
  match addr with
  | Normal addr -> S.List [ sexp_of_address addr ]
  | Well_known_mut_rec -> S.List []

class ['a] mapbase =
  object
    method visit_var : 'a -> var -> var = fun _ -> fun e -> e

    method visit_prim : 'a -> Primitive.prim -> Primitive.prim =
      fun _ -> fun e -> e

    method visit_constr_tag : 'a -> constr_tag -> constr_tag =
      fun _ -> fun e -> e

    method visit_binder : 'a -> binder -> binder = fun _ -> fun e -> e
    method visit_address : 'a -> address -> address = fun _ -> fun e -> e
    method visit_join : 'a -> join -> join = fun _ -> fun e -> e
    method visit_label : 'a -> label -> label = fun _ -> fun e -> e
    method visit_ltype : 'a -> ltype -> ltype = fun _ -> fun e -> e

    method private visit_func_stubs : 'a -> func_stubs -> func_stubs =
      fun _ -> fun e -> e

    method private visit_alloc_kind : 'a -> alloc_kind -> alloc_kind =
      fun _ -> fun e -> e

    method private visit_get_field_kind : 'a -> get_field_kind -> get_field_kind
        =
      fun _ -> fun e -> e

    method private visit_set_field_kind : 'a -> set_field_kind -> set_field_kind
        =
      fun _ -> fun e -> e

    method private visit_closure_address :
        'a -> closure_address -> closure_address =
      fun _ -> fun e -> e
  end

class ['a] iterbase =
  object
    method visit_var : 'a -> var -> unit = fun _ -> fun _ -> ()
    method visit_prim : 'a -> Primitive.prim -> unit = fun _ -> fun _ -> ()
    method visit_constr_tag : 'a -> constr_tag -> unit = fun _ -> fun _ -> ()
    method visit_binder : 'a -> binder -> unit = fun _ -> fun _ -> ()
    method visit_address : 'a -> address -> unit = fun _ -> fun _ -> ()
    method visit_join : 'a -> join -> unit = fun _ -> fun _ -> ()
    method visit_label : 'a -> label -> unit = fun _ -> fun _ -> ()
    method visit_ltype : 'a -> ltype -> unit = fun _ -> fun _ -> ()

    method private visit_func_stubs : 'a -> func_stubs -> unit =
      fun _ -> fun _ -> ()

    method private visit_alloc_kind : 'a -> alloc_kind -> unit =
      fun _ -> fun _ -> ()

    method private visit_get_field_kind : 'a -> get_field_kind -> unit =
      fun _ -> fun _ -> ()

    method private visit_set_field_kind : 'a -> set_field_kind -> unit =
      fun _ -> fun _ -> ()

    method private visit_closure_address : 'a -> closure_address -> unit =
      fun _ -> fun _ -> ()
  end

class virtual ['a] sexpbase =
  object
    inherit [_] Sexp_visitors.sexp

    method visit_location : 'a -> location -> S.t =
      fun _ -> fun loc -> sexp_of_location loc

    method visit_var : 'a -> var -> S.t = fun _ -> fun x -> sexp_of_var x

    method visit_prim : 'a -> Primitive.prim -> S.t =
      fun _ -> fun x -> Primitive.sexp_of_prim x

    method visit_constr_tag : 'a -> constr_tag -> S.t =
      fun _ -> fun x -> sexp_of_constr_tag x

    method visit_binder : 'a -> binder -> S.t =
      fun _ -> fun x -> sexp_of_binder x

    method visit_constant : 'a -> constant -> S.t =
      fun _ -> fun x -> sexp_of_constant x

    method visit_address : 'a -> address -> S.t =
      fun _ -> fun x -> sexp_of_address x

    method visit_join : 'a -> join -> S.t = fun _ -> fun x -> sexp_of_join x
    method visit_label : 'a -> label -> S.t = fun _ -> fun x -> sexp_of_label x

    method visit_fn_kind : 'a -> fn_kind -> S.t =
      fun _ -> fun x -> sexp_of_fn_kind x

    method visit_ltype : 'a -> ltype -> S.t = fun _ -> fun x -> sexp_of_ltype x

    method visit_join_kind : 'a -> join_kind -> S.t =
      fun _ -> fun x -> sexp_of_join_kind x

    method visit_type_defs : 'a -> type_defs -> S.t =
      fun _ -> fun x -> sexp_of_type_defs x

    method visit_tid : 'a -> tid -> S.t = fun _ -> fun x -> sexp_of_tid x

    method private visit_func_stubs : 'a -> func_stubs -> S.t =
      fun _ -> fun x -> sexp_of_func_stubs x

    method private visit_alloc_kind : 'a -> alloc_kind -> S.t =
      fun _ -> fun x -> sexp_of_alloc_kind x

    method private visit_get_field_kind : 'a -> get_field_kind -> S.t =
      fun _ -> fun x -> sexp_of_get_field_kind x

    method private visit_set_field_kind : 'a -> set_field_kind -> S.t =
      fun _ -> fun x -> sexp_of_set_field_kind x

    method private visit_closure_address : 'a -> closure_address -> S.t =
      fun _ -> fun x -> sexp_of_closure_address x

    method private visit_make_array_kind :
        'a -> Primitive.make_array_kind -> S.t =
      fun _ -> fun x -> Primitive.sexp_of_make_array_kind x

    method private visit_array_get_kind : 'a -> Primitive.array_get_kind -> S.t
        =
      fun _ -> fun x -> Primitive.sexp_of_array_get_kind x

    method private visit_array_set_kind : 'a -> Primitive.array_set_kind -> S.t
        =
      fun _ -> fun x -> Primitive.sexp_of_array_set_kind x

    method private visit_get_item_extra : 'a -> get_item_extra -> S.t =
      fun _ -> fun x -> sexp_of_get_item_extra x
  end

type fn = { params : binder list; body : lambda; return_type_ : ltype list }

and top_func_item = {
  binder : address;
  fn_kind_ : fn_kind;
  fn : fn;
  tid : (tid option[@visitors.opaque]);
}

and prog = {
  fns : top_func_item list;
  main : lambda option;
  init : lambda;
  globals : (binder * constant option) list;
  type_defs : type_defs;
}

and closure = {
  captures : var list;
  address : closure_address;
  tid : (tid[@visitors.opaque]);
}

and aggregate = {
  kind : alloc_kind;
  tid : (tid[@visitors.opaque]);
  fields : lambda list;
}

and target =
  | Dynamic of var
  | StaticFn of address
  | Object of { obj : var; method_index : int; method_ty : ltype }

and intrinsic =
  | FixedArray_copy of {
      src_tid : (tid[@visitors.opaque]);
      dst_tid : (tid[@visitors.opaque]);
    }
  | FixedArray_fill of { tid : (tid[@visitors.opaque]) }
  | Char_to_string

and lambda =
  | Levent of { expr : lambda; loc_ : location }
  | Lallocate of aggregate
  | Lclosure of closure
  | Lget_raw_func of address
  | Lget_field of {
      obj : lambda;
      tid : (tid[@visitors.opaque]);
      index : int;
      kind : get_field_kind;
    }
  | Lclosure_field of {
      obj : lambda;
      tid : (tid[@visitors.opaque]);
      index : int;
    }
  | Lset_field of {
      obj : lambda;
      field : lambda;
      tid : (tid[@visitors.opaque]);
      index : int;
      kind : set_field_kind;
    }
  | Lmake_array of {
      tid : (tid[@visitors.opaque]);
      kind : (Primitive.make_array_kind[@visitors.opaque]);
      elems : lambda list;
    }
  | Larray_get_item of {
      tid : (tid[@visitors.opaque]);
      kind : (Primitive.array_get_kind[@visitors.opaque]);
      arr : lambda;
      index : lambda;
      extra : (get_item_extra[@visitors.opaque]);
    }
  | Larray_set_item of {
      tid : (tid[@visitors.opaque]);
      kind : (Primitive.array_set_kind[@visitors.opaque]);
      arr : lambda;
      index : lambda;
      item : lambda option;
    }
  | Lapply of { fn : target; prim : intrinsic option; args : lambda list }
  | Lstub_call of {
      fn : func_stubs;
      args : lambda list;
      params_ty : ltype list;
      return_ty : ltype option;
    }
  | Lconst of constant
  | Lloop of {
      params : binder list;
      body : lambda;
      args : lambda list;
      label : label;
      type_ : ltype;
    }
  | Lif of { pred : lambda; ifso : lambda; ifnot : lambda; type_ : ltype }
  | Llet of { name : binder; e : lambda; body : lambda }
  | Lletrec of { names : binder list; fns : closure list; body : lambda }
  | Lprim of { fn : prim; args : lambda list }
  | Lsequence of { exprs : lambda list; last_expr : lambda }
  | Ljoinlet of {
      name : join;
      params : binder list;
      e : lambda;
      body : lambda;
      kind : join_kind;
      type_ : ltype list;
    }
  | Ljoinapply of { name : join; args : lambda list }
  | Lbreak of { arg : lambda option; label : label }
  | Lcontinue of { args : lambda list; label : label }
  | Lreturn of lambda
  | Lswitch of {
      obj : var;
      cases : (constr_tag * lambda) list;
      default : lambda;
      type_ : ltype;
    }
  | Lswitchint of {
      obj : var;
      cases : (int * lambda) list;
      default : lambda;
      type_ : ltype;
    }
  | Lswitchstring of {
      obj : lambda;
      cases : (string * lambda) list;
      default : lambda;
      type_ : ltype;
    }
  | Lvar of { var : var }
  | Lassign of { var : var; e : lambda }
  | Lcatch of { body : lambda; on_exception : lambda; type_ : ltype }
  | Lcast of { expr : lambda; target_type : ltype }

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] iter =
    object (self : 'self)
      inherit [_] iterbase

      method visit_fn : _ -> fn -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_binder env))
               _visitors_this.params
           in
           let _visitors_r1 = self#visit_lambda env _visitors_this.body in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_ltype env))
               _visitors_this.return_type_
           in
           ()

      method visit_top_func_item : _ -> top_func_item -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_address env _visitors_this.binder in
           let _visitors_r1 =
             (fun _visitors_this -> ()) _visitors_this.fn_kind_
           in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 = (fun _visitors_this -> ()) _visitors_this.tid in
           ()

      method visit_prog : _ -> prog -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_top_func_item env))
               _visitors_this.fns
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> (self#visit_lambda env) t
               | None -> ())
               _visitors_this.main
           in
           let _visitors_r2 = self#visit_lambda env _visitors_this.init in
           let _visitors_r3 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this
                 ~f:(fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 = self#visit_binder env _visitors_c0 in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (fun _visitors_this -> ()) t
                       | None -> ())
                       _visitors_c1
                   in
                   ()))
               _visitors_this.globals
           in
           let _visitors_r4 =
             (fun _visitors_this -> ()) _visitors_this.type_defs
           in
           ()

      method visit_closure : _ -> closure -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_var env))
               _visitors_this.captures
           in
           let _visitors_r1 =
             self#visit_closure_address env _visitors_this.address
           in
           let _visitors_r2 = (fun _visitors_this -> ()) _visitors_this.tid in
           ()

      method visit_aggregate : _ -> aggregate -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_alloc_kind env _visitors_this.kind in
           let _visitors_r1 = (fun _visitors_this -> ()) _visitors_this.tid in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
               _visitors_this.fields
           in
           ()

      method visit_Dynamic : _ -> var -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_var env _visitors_c0 in
           ()

      method visit_StaticFn : _ -> address -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           ()

      method visit_Object : _ -> var -> int -> ltype -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fmethod_index ->
            fun _visitors_fmethod_ty ->
             let _visitors_r0 = self#visit_var env _visitors_fobj in
             let _visitors_r1 =
               (fun _visitors_this -> ()) _visitors_fmethod_index
             in
             let _visitors_r2 = self#visit_ltype env _visitors_fmethod_ty in
             ()

      method visit_target : _ -> target -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Dynamic _visitors_c0 -> self#visit_Dynamic env _visitors_c0
           | StaticFn _visitors_c0 -> self#visit_StaticFn env _visitors_c0
           | Object
               {
                 obj = _visitors_fobj;
                 method_index = _visitors_fmethod_index;
                 method_ty = _visitors_fmethod_ty;
               } ->
               self#visit_Object env _visitors_fobj _visitors_fmethod_index
                 _visitors_fmethod_ty

      method visit_FixedArray_copy : _ -> _ -> _ -> unit =
        fun env ->
          fun _visitors_fsrc_tid ->
           fun _visitors_fdst_tid ->
            let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fsrc_tid in
            let _visitors_r1 = (fun _visitors_this -> ()) _visitors_fdst_tid in
            ()

      method visit_FixedArray_fill : _ -> _ -> unit =
        fun env ->
          fun _visitors_ftid ->
           let _visitors_r0 = (fun _visitors_this -> ()) _visitors_ftid in
           ()

      method visit_Char_to_string : _ -> unit = fun env -> ()

      method visit_intrinsic : _ -> intrinsic -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | FixedArray_copy
               { src_tid = _visitors_fsrc_tid; dst_tid = _visitors_fdst_tid } ->
               self#visit_FixedArray_copy env _visitors_fsrc_tid
                 _visitors_fdst_tid
           | FixedArray_fill { tid = _visitors_ftid } ->
               self#visit_FixedArray_fill env _visitors_ftid
           | Char_to_string -> self#visit_Char_to_string env

      method visit_Levent : _ -> lambda -> location -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 = (fun _visitors_this -> ()) _visitors_floc_ in
            ()

      method visit_Lallocate : _ -> aggregate -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_aggregate env _visitors_c0 in
           ()

      method visit_Lclosure : _ -> closure -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_closure env _visitors_c0 in
           ()

      method visit_Lget_raw_func : _ -> address -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           ()

      method visit_Lget_field :
          _ -> lambda -> _ -> int -> get_field_kind -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 = (fun _visitors_this -> ()) _visitors_ftid in
              let _visitors_r2 = (fun _visitors_this -> ()) _visitors_findex in
              let _visitors_r3 =
                self#visit_get_field_kind env _visitors_fkind
              in
              ()

      method visit_Lclosure_field : _ -> lambda -> _ -> int -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             let _visitors_r0 = self#visit_lambda env _visitors_fobj in
             let _visitors_r1 = (fun _visitors_this -> ()) _visitors_ftid in
             let _visitors_r2 = (fun _visitors_this -> ()) _visitors_findex in
             ()

      method visit_Lset_field :
          _ -> lambda -> lambda -> _ -> int -> set_field_kind -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ffield ->
            fun _visitors_ftid ->
             fun _visitors_findex ->
              fun _visitors_fkind ->
               let _visitors_r0 = self#visit_lambda env _visitors_fobj in
               let _visitors_r1 = self#visit_lambda env _visitors_ffield in
               let _visitors_r2 = (fun _visitors_this -> ()) _visitors_ftid in
               let _visitors_r3 = (fun _visitors_this -> ()) _visitors_findex in
               let _visitors_r4 =
                 self#visit_set_field_kind env _visitors_fkind
               in
               ()

      method visit_Lmake_array : _ -> _ -> _ -> lambda list -> unit =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_felems ->
             let _visitors_r0 = (fun _visitors_this -> ()) _visitors_ftid in
             let _visitors_r1 = (fun _visitors_this -> ()) _visitors_fkind in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                 _visitors_felems
             in
             ()

      method visit_Larray_get_item :
          _ -> _ -> _ -> lambda -> lambda -> _ -> unit =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fextra ->
               let _visitors_r0 = (fun _visitors_this -> ()) _visitors_ftid in
               let _visitors_r1 = (fun _visitors_this -> ()) _visitors_fkind in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 = (fun _visitors_this -> ()) _visitors_fextra in
               ()

      method visit_Larray_set_item :
          _ -> _ -> _ -> lambda -> lambda -> lambda option -> unit =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fitem ->
               let _visitors_r0 = (fun _visitors_this -> ()) _visitors_ftid in
               let _visitors_r1 = (fun _visitors_this -> ()) _visitors_fkind in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> (self#visit_lambda env) t
                   | None -> ())
                   _visitors_fitem
               in
               ()

      method visit_Lapply :
          _ -> target -> intrinsic option -> lambda list -> unit =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fprim ->
            fun _visitors_fargs ->
             let _visitors_r0 = self#visit_target env _visitors_ffn in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (self#visit_intrinsic env) t
                 | None -> ())
                 _visitors_fprim
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                 _visitors_fargs
             in
             ()

      method visit_Lstub_call :
          _ -> func_stubs -> lambda list -> ltype list -> ltype option -> unit =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            fun _visitors_fparams_ty ->
             fun _visitors_freturn_ty ->
              let _visitors_r0 = self#visit_func_stubs env _visitors_ffn in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                  _visitors_fargs
              in
              let _visitors_r2 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this ~f:(self#visit_ltype env))
                  _visitors_fparams_ty
              in
              let _visitors_r3 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> (self#visit_ltype env) t
                  | None -> ())
                  _visitors_freturn_ty
              in
              ()

      method visit_Lconst : _ -> constant -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = (fun _visitors_this -> ()) _visitors_c0 in
           ()

      method visit_Lloop :
          _ -> binder list -> lambda -> lambda list -> label -> ltype -> unit =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_ftype_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_binder env))
                   _visitors_fparams
               in
               let _visitors_r1 = self#visit_lambda env _visitors_fbody in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                   _visitors_fargs
               in
               let _visitors_r3 = self#visit_label env _visitors_flabel in
               let _visitors_r4 = self#visit_ltype env _visitors_ftype_ in
               ()

      method visit_Lif : _ -> lambda -> lambda -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fpred ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fpred in
              let _visitors_r1 = self#visit_lambda env _visitors_fifso in
              let _visitors_r2 = self#visit_lambda env _visitors_fifnot in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              ()

      method visit_Llet : _ -> binder -> lambda -> lambda -> unit =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fe ->
            fun _visitors_fbody ->
             let _visitors_r0 = self#visit_binder env _visitors_fname in
             let _visitors_r1 = self#visit_lambda env _visitors_fe in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             ()

      method visit_Lletrec : _ -> binder list -> closure list -> lambda -> unit
          =
        fun env ->
          fun _visitors_fnames ->
           fun _visitors_ffns ->
            fun _visitors_fbody ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_binder env))
                 _visitors_fnames
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_closure env))
                 _visitors_ffns
             in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             ()

      method visit_Lprim : _ -> prim -> lambda list -> unit =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_prim env _visitors_ffn in
            let _visitors_r1 =
              (fun _visitors_this ->
                Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                _visitors_fargs
            in
            ()

      method visit_Lsequence : _ -> lambda list -> lambda -> unit =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                _visitors_fexprs
            in
            let _visitors_r1 = self#visit_lambda env _visitors_flast_expr in
            ()

      method visit_Ljoinlet :
          _ ->
          join ->
          binder list ->
          lambda ->
          lambda ->
          join_kind ->
          ltype list ->
          unit =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fparams ->
            fun _visitors_fe ->
             fun _visitors_fbody ->
              fun _visitors_fkind ->
               fun _visitors_ftype_ ->
                let _visitors_r0 = self#visit_join env _visitors_fname in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_binder env))
                    _visitors_fparams
                in
                let _visitors_r2 = self#visit_lambda env _visitors_fe in
                let _visitors_r3 = self#visit_lambda env _visitors_fbody in
                let _visitors_r4 = (fun _visitors_this -> ()) _visitors_fkind in
                let _visitors_r5 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_ltype env))
                    _visitors_ftype_
                in
                ()

      method visit_Ljoinapply : _ -> join -> lambda list -> unit =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_join env _visitors_fname in
            let _visitors_r1 =
              (fun _visitors_this ->
                Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                _visitors_fargs
            in
            ()

      method visit_Lbreak : _ -> lambda option -> label -> unit =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              (fun _visitors_this ->
                match _visitors_this with
                | Some t -> (self#visit_lambda env) t
                | None -> ())
                _visitors_farg
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            ()

      method visit_Lcontinue : _ -> lambda list -> label -> unit =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.iter _visitors_this ~f:(self#visit_lambda env))
                _visitors_fargs
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            ()

      method visit_Lreturn : _ -> lambda -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_lambda env _visitors_c0 in
           ()

      method visit_Lswitch :
          _ -> var -> (constr_tag * lambda) list -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        self#visit_constr_tag env _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      ()))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              ()

      method visit_Lswitchint :
          _ -> var -> (int * lambda) list -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> ()) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      ()))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              ()

      method visit_Lswitchstring :
          _ -> lambda -> (string * lambda) list -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.iter _visitors_this
                    ~f:(fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> ()) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      ()))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              ()

      method visit_Lvar : _ -> var -> unit =
        fun env ->
          fun _visitors_fvar ->
           let _visitors_r0 = self#visit_var env _visitors_fvar in
           ()

      method visit_Lassign : _ -> var -> lambda -> unit =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fe ->
            let _visitors_r0 = self#visit_var env _visitors_fvar in
            let _visitors_r1 = self#visit_lambda env _visitors_fe in
            ()

      method visit_Lcatch : _ -> lambda -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fon_exception ->
            fun _visitors_ftype_ ->
             let _visitors_r0 = self#visit_lambda env _visitors_fbody in
             let _visitors_r1 = self#visit_lambda env _visitors_fon_exception in
             let _visitors_r2 = self#visit_ltype env _visitors_ftype_ in
             ()

      method visit_Lcast : _ -> lambda -> ltype -> unit =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftarget_type ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 = self#visit_ltype env _visitors_ftarget_type in
            ()

      method visit_lambda : _ -> lambda -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Levent { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
               self#visit_Levent env _visitors_fexpr _visitors_floc_
           | Lallocate _visitors_c0 -> self#visit_Lallocate env _visitors_c0
           | Lclosure _visitors_c0 -> self#visit_Lclosure env _visitors_c0
           | Lget_raw_func _visitors_c0 ->
               self#visit_Lget_raw_func env _visitors_c0
           | Lget_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lget_field env _visitors_fobj _visitors_ftid
                 _visitors_findex _visitors_fkind
           | Lclosure_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
               } ->
               self#visit_Lclosure_field env _visitors_fobj _visitors_ftid
                 _visitors_findex
           | Lset_field
               {
                 obj = _visitors_fobj;
                 field = _visitors_ffield;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lset_field env _visitors_fobj _visitors_ffield
                 _visitors_ftid _visitors_findex _visitors_fkind
           | Lmake_array
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 elems = _visitors_felems;
               } ->
               self#visit_Lmake_array env _visitors_ftid _visitors_fkind
                 _visitors_felems
           | Larray_get_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 extra = _visitors_fextra;
               } ->
               self#visit_Larray_get_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fextra
           | Larray_set_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 item = _visitors_fitem;
               } ->
               self#visit_Larray_set_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fitem
           | Lapply
               {
                 fn = _visitors_ffn;
                 prim = _visitors_fprim;
                 args = _visitors_fargs;
               } ->
               self#visit_Lapply env _visitors_ffn _visitors_fprim
                 _visitors_fargs
           | Lstub_call
               {
                 fn = _visitors_ffn;
                 args = _visitors_fargs;
                 params_ty = _visitors_fparams_ty;
                 return_ty = _visitors_freturn_ty;
               } ->
               self#visit_Lstub_call env _visitors_ffn _visitors_fargs
                 _visitors_fparams_ty _visitors_freturn_ty
           | Lconst _visitors_c0 -> self#visit_Lconst env _visitors_c0
           | Lloop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 label = _visitors_flabel;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lloop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_flabel _visitors_ftype_
           | Lif
               {
                 pred = _visitors_fpred;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lif env _visitors_fpred _visitors_fifso
                 _visitors_fifnot _visitors_ftype_
           | Llet
               {
                 name = _visitors_fname;
                 e = _visitors_fe;
                 body = _visitors_fbody;
               } ->
               self#visit_Llet env _visitors_fname _visitors_fe _visitors_fbody
           | Lletrec
               {
                 names = _visitors_fnames;
                 fns = _visitors_ffns;
                 body = _visitors_fbody;
               } ->
               self#visit_Lletrec env _visitors_fnames _visitors_ffns
                 _visitors_fbody
           | Lprim { fn = _visitors_ffn; args = _visitors_fargs } ->
               self#visit_Lprim env _visitors_ffn _visitors_fargs
           | Lsequence
               { exprs = _visitors_fexprs; last_expr = _visitors_flast_expr } ->
               self#visit_Lsequence env _visitors_fexprs _visitors_flast_expr
           | Ljoinlet
               {
                 name = _visitors_fname;
                 params = _visitors_fparams;
                 e = _visitors_fe;
                 body = _visitors_fbody;
                 kind = _visitors_fkind;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Ljoinlet env _visitors_fname _visitors_fparams
                 _visitors_fe _visitors_fbody _visitors_fkind _visitors_ftype_
           | Ljoinapply { name = _visitors_fname; args = _visitors_fargs } ->
               self#visit_Ljoinapply env _visitors_fname _visitors_fargs
           | Lbreak { arg = _visitors_farg; label = _visitors_flabel } ->
               self#visit_Lbreak env _visitors_farg _visitors_flabel
           | Lcontinue { args = _visitors_fargs; label = _visitors_flabel } ->
               self#visit_Lcontinue env _visitors_fargs _visitors_flabel
           | Lreturn _visitors_c0 -> self#visit_Lreturn env _visitors_c0
           | Lswitch
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitch env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchint
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchint env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchstring
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchstring env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lvar { var = _visitors_fvar } -> self#visit_Lvar env _visitors_fvar
           | Lassign { var = _visitors_fvar; e = _visitors_fe } ->
               self#visit_Lassign env _visitors_fvar _visitors_fe
           | Lcatch
               {
                 body = _visitors_fbody;
                 on_exception = _visitors_fon_exception;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lcatch env _visitors_fbody _visitors_fon_exception
                 _visitors_ftype_
           | Lcast
               { expr = _visitors_fexpr; target_type = _visitors_ftarget_type }
             ->
               self#visit_Lcast env _visitors_fexpr _visitors_ftarget_type
    end

  [@@@VISITORS.END]
end

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] map =
    object (self : 'self)
      inherit [_] mapbase

      method visit_fn : _ -> fn -> fn =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_binder env))
               _visitors_this.params
           in
           let _visitors_r1 = self#visit_lambda env _visitors_this.body in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_ltype env))
               _visitors_this.return_type_
           in
           {
             params = _visitors_r0;
             body = _visitors_r1;
             return_type_ = _visitors_r2;
           }

      method visit_top_func_item : _ -> top_func_item -> top_func_item =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_address env _visitors_this.binder in
           let _visitors_r1 =
             (fun _visitors_this -> _visitors_this) _visitors_this.fn_kind_
           in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 =
             (fun _visitors_this -> _visitors_this) _visitors_this.tid
           in
           {
             binder = _visitors_r0;
             fn_kind_ = _visitors_r1;
             fn = _visitors_r2;
             tid = _visitors_r3;
           }

      method visit_prog : _ -> prog -> prog =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_top_func_item env))
               _visitors_this.fns
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               match _visitors_this with
               | Some t -> Some ((self#visit_lambda env) t)
               | None -> None)
               _visitors_this.main
           in
           let _visitors_r2 = self#visit_lambda env _visitors_this.init in
           let _visitors_r3 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 = self#visit_binder env _visitors_c0 in
                   let _visitors_r1 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t ->
                           Some ((fun _visitors_this -> _visitors_this) t)
                       | None -> None)
                       _visitors_c1
                   in
                   (_visitors_r0, _visitors_r1)))
               _visitors_this.globals
           in
           let _visitors_r4 =
             (fun _visitors_this -> _visitors_this) _visitors_this.type_defs
           in
           {
             fns = _visitors_r0;
             main = _visitors_r1;
             init = _visitors_r2;
             globals = _visitors_r3;
             type_defs = _visitors_r4;
           }

      method visit_closure : _ -> closure -> closure =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_var env))
               _visitors_this.captures
           in
           let _visitors_r1 =
             self#visit_closure_address env _visitors_this.address
           in
           let _visitors_r2 =
             (fun _visitors_this -> _visitors_this) _visitors_this.tid
           in
           {
             captures = _visitors_r0;
             address = _visitors_r1;
             tid = _visitors_r2;
           }

      method visit_aggregate : _ -> aggregate -> aggregate =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_alloc_kind env _visitors_this.kind in
           let _visitors_r1 =
             (fun _visitors_this -> _visitors_this) _visitors_this.tid
           in
           let _visitors_r2 =
             (fun _visitors_this ->
               Basic_lst.map _visitors_this (self#visit_lambda env))
               _visitors_this.fields
           in
           { kind = _visitors_r0; tid = _visitors_r1; fields = _visitors_r2 }

      method visit_Dynamic : _ -> var -> target =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_var env _visitors_c0 in
           Dynamic _visitors_r0

      method visit_StaticFn : _ -> address -> target =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           StaticFn _visitors_r0

      method visit_Object : _ -> var -> int -> ltype -> target =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fmethod_index ->
            fun _visitors_fmethod_ty ->
             let _visitors_r0 = self#visit_var env _visitors_fobj in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_fmethod_index
             in
             let _visitors_r2 = self#visit_ltype env _visitors_fmethod_ty in
             Object
               {
                 obj = _visitors_r0;
                 method_index = _visitors_r1;
                 method_ty = _visitors_r2;
               }

      method visit_target : _ -> target -> target =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Dynamic _visitors_c0 -> self#visit_Dynamic env _visitors_c0
           | StaticFn _visitors_c0 -> self#visit_StaticFn env _visitors_c0
           | Object
               {
                 obj = _visitors_fobj;
                 method_index = _visitors_fmethod_index;
                 method_ty = _visitors_fmethod_ty;
               } ->
               self#visit_Object env _visitors_fobj _visitors_fmethod_index
                 _visitors_fmethod_ty

      method visit_FixedArray_copy : _ -> _ -> _ -> intrinsic =
        fun env ->
          fun _visitors_fsrc_tid ->
           fun _visitors_fdst_tid ->
            let _visitors_r0 =
              (fun _visitors_this -> _visitors_this) _visitors_fsrc_tid
            in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_fdst_tid
            in
            FixedArray_copy { src_tid = _visitors_r0; dst_tid = _visitors_r1 }

      method visit_FixedArray_fill : _ -> _ -> intrinsic =
        fun env ->
          fun _visitors_ftid ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_ftid
           in
           FixedArray_fill { tid = _visitors_r0 }

      method visit_Char_to_string : _ -> intrinsic = fun env -> Char_to_string

      method visit_intrinsic : _ -> intrinsic -> intrinsic =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | FixedArray_copy
               { src_tid = _visitors_fsrc_tid; dst_tid = _visitors_fdst_tid } ->
               self#visit_FixedArray_copy env _visitors_fsrc_tid
                 _visitors_fdst_tid
           | FixedArray_fill { tid = _visitors_ftid } ->
               self#visit_FixedArray_fill env _visitors_ftid
           | Char_to_string -> self#visit_Char_to_string env

      method visit_Levent : _ -> lambda -> location -> lambda =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_floc_
            in
            Levent { expr = _visitors_r0; loc_ = _visitors_r1 }

      method visit_Lallocate : _ -> aggregate -> lambda =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_aggregate env _visitors_c0 in
           Lallocate _visitors_r0

      method visit_Lclosure : _ -> closure -> lambda =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_closure env _visitors_c0 in
           Lclosure _visitors_r0

      method visit_Lget_raw_func : _ -> address -> lambda =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           Lget_raw_func _visitors_r0

      method visit_Lget_field :
          _ -> lambda -> _ -> int -> get_field_kind -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this -> _visitors_this) _visitors_ftid
              in
              let _visitors_r2 =
                (fun _visitors_this -> _visitors_this) _visitors_findex
              in
              let _visitors_r3 =
                self#visit_get_field_kind env _visitors_fkind
              in
              Lget_field
                {
                  obj = _visitors_r0;
                  tid = _visitors_r1;
                  index = _visitors_r2;
                  kind = _visitors_r3;
                }

      method visit_Lclosure_field : _ -> lambda -> _ -> int -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             let _visitors_r0 = self#visit_lambda env _visitors_fobj in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_ftid
             in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_findex
             in
             Lclosure_field
               { obj = _visitors_r0; tid = _visitors_r1; index = _visitors_r2 }

      method visit_Lset_field :
          _ -> lambda -> lambda -> _ -> int -> set_field_kind -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ffield ->
            fun _visitors_ftid ->
             fun _visitors_findex ->
              fun _visitors_fkind ->
               let _visitors_r0 = self#visit_lambda env _visitors_fobj in
               let _visitors_r1 = self#visit_lambda env _visitors_ffield in
               let _visitors_r2 =
                 (fun _visitors_this -> _visitors_this) _visitors_ftid
               in
               let _visitors_r3 =
                 (fun _visitors_this -> _visitors_this) _visitors_findex
               in
               let _visitors_r4 =
                 self#visit_set_field_kind env _visitors_fkind
               in
               Lset_field
                 {
                   obj = _visitors_r0;
                   field = _visitors_r1;
                   tid = _visitors_r2;
                   index = _visitors_r3;
                   kind = _visitors_r4;
                 }

      method visit_Lmake_array : _ -> _ -> _ -> lambda list -> lambda =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_felems ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_ftid
             in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_fkind
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_lambda env))
                 _visitors_felems
             in
             Lmake_array
               { tid = _visitors_r0; kind = _visitors_r1; elems = _visitors_r2 }

      method visit_Larray_get_item :
          _ -> _ -> _ -> lambda -> lambda -> _ -> lambda =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fextra ->
               let _visitors_r0 =
                 (fun _visitors_this -> _visitors_this) _visitors_ftid
               in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fkind
               in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 =
                 (fun _visitors_this -> _visitors_this) _visitors_fextra
               in
               Larray_get_item
                 {
                   tid = _visitors_r0;
                   kind = _visitors_r1;
                   arr = _visitors_r2;
                   index = _visitors_r3;
                   extra = _visitors_r4;
                 }

      method visit_Larray_set_item :
          _ -> _ -> _ -> lambda -> lambda -> lambda option -> lambda =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fitem ->
               let _visitors_r0 =
                 (fun _visitors_this -> _visitors_this) _visitors_ftid
               in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fkind
               in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 =
                 (fun _visitors_this ->
                   match _visitors_this with
                   | Some t -> Some ((self#visit_lambda env) t)
                   | None -> None)
                   _visitors_fitem
               in
               Larray_set_item
                 {
                   tid = _visitors_r0;
                   kind = _visitors_r1;
                   arr = _visitors_r2;
                   index = _visitors_r3;
                   item = _visitors_r4;
                 }

      method visit_Lapply :
          _ -> target -> intrinsic option -> lambda list -> lambda =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fprim ->
            fun _visitors_fargs ->
             let _visitors_r0 = self#visit_target env _visitors_ffn in
             let _visitors_r1 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((self#visit_intrinsic env) t)
                 | None -> None)
                 _visitors_fprim
             in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_lambda env))
                 _visitors_fargs
             in
             Lapply
               { fn = _visitors_r0; prim = _visitors_r1; args = _visitors_r2 }

      method visit_Lstub_call :
          _ -> func_stubs -> lambda list -> ltype list -> ltype option -> lambda
          =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            fun _visitors_fparams_ty ->
             fun _visitors_freturn_ty ->
              let _visitors_r0 = self#visit_func_stubs env _visitors_ffn in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_lambda env))
                  _visitors_fargs
              in
              let _visitors_r2 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this (self#visit_ltype env))
                  _visitors_fparams_ty
              in
              let _visitors_r3 =
                (fun _visitors_this ->
                  match _visitors_this with
                  | Some t -> Some ((self#visit_ltype env) t)
                  | None -> None)
                  _visitors_freturn_ty
              in
              Lstub_call
                {
                  fn = _visitors_r0;
                  args = _visitors_r1;
                  params_ty = _visitors_r2;
                  return_ty = _visitors_r3;
                }

      method visit_Lconst : _ -> constant -> lambda =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this -> _visitors_this) _visitors_c0
           in
           Lconst _visitors_r0

      method visit_Lloop :
          _ -> binder list -> lambda -> lambda list -> label -> ltype -> lambda
          =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_ftype_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_binder env))
                   _visitors_fparams
               in
               let _visitors_r1 = self#visit_lambda env _visitors_fbody in
               let _visitors_r2 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_lambda env))
                   _visitors_fargs
               in
               let _visitors_r3 = self#visit_label env _visitors_flabel in
               let _visitors_r4 = self#visit_ltype env _visitors_ftype_ in
               Lloop
                 {
                   params = _visitors_r0;
                   body = _visitors_r1;
                   args = _visitors_r2;
                   label = _visitors_r3;
                   type_ = _visitors_r4;
                 }

      method visit_Lif : _ -> lambda -> lambda -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fpred ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fpred in
              let _visitors_r1 = self#visit_lambda env _visitors_fifso in
              let _visitors_r2 = self#visit_lambda env _visitors_fifnot in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              Lif
                {
                  pred = _visitors_r0;
                  ifso = _visitors_r1;
                  ifnot = _visitors_r2;
                  type_ = _visitors_r3;
                }

      method visit_Llet : _ -> binder -> lambda -> lambda -> lambda =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fe ->
            fun _visitors_fbody ->
             let _visitors_r0 = self#visit_binder env _visitors_fname in
             let _visitors_r1 = self#visit_lambda env _visitors_fe in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             Llet { name = _visitors_r0; e = _visitors_r1; body = _visitors_r2 }

      method visit_Lletrec :
          _ -> binder list -> closure list -> lambda -> lambda =
        fun env ->
          fun _visitors_fnames ->
           fun _visitors_ffns ->
            fun _visitors_fbody ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_binder env))
                 _visitors_fnames
             in
             let _visitors_r1 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_closure env))
                 _visitors_ffns
             in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             Lletrec
               { names = _visitors_r0; fns = _visitors_r1; body = _visitors_r2 }

      method visit_Lprim : _ -> prim -> lambda list -> lambda =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_prim env _visitors_ffn in
            let _visitors_r1 =
              (fun _visitors_this ->
                Basic_lst.map _visitors_this (self#visit_lambda env))
                _visitors_fargs
            in
            Lprim { fn = _visitors_r0; args = _visitors_r1 }

      method visit_Lsequence : _ -> lambda list -> lambda -> lambda =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.map _visitors_this (self#visit_lambda env))
                _visitors_fexprs
            in
            let _visitors_r1 = self#visit_lambda env _visitors_flast_expr in
            Lsequence { exprs = _visitors_r0; last_expr = _visitors_r1 }

      method visit_Ljoinlet :
          _ ->
          join ->
          binder list ->
          lambda ->
          lambda ->
          join_kind ->
          ltype list ->
          lambda =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fparams ->
            fun _visitors_fe ->
             fun _visitors_fbody ->
              fun _visitors_fkind ->
               fun _visitors_ftype_ ->
                let _visitors_r0 = self#visit_join env _visitors_fname in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_binder env))
                    _visitors_fparams
                in
                let _visitors_r2 = self#visit_lambda env _visitors_fe in
                let _visitors_r3 = self#visit_lambda env _visitors_fbody in
                let _visitors_r4 =
                  (fun _visitors_this -> _visitors_this) _visitors_fkind
                in
                let _visitors_r5 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_ltype env))
                    _visitors_ftype_
                in
                Ljoinlet
                  {
                    name = _visitors_r0;
                    params = _visitors_r1;
                    e = _visitors_r2;
                    body = _visitors_r3;
                    kind = _visitors_r4;
                    type_ = _visitors_r5;
                  }

      method visit_Ljoinapply : _ -> join -> lambda list -> lambda =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_join env _visitors_fname in
            let _visitors_r1 =
              (fun _visitors_this ->
                Basic_lst.map _visitors_this (self#visit_lambda env))
                _visitors_fargs
            in
            Ljoinapply { name = _visitors_r0; args = _visitors_r1 }

      method visit_Lbreak : _ -> lambda option -> label -> lambda =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              (fun _visitors_this ->
                match _visitors_this with
                | Some t -> Some ((self#visit_lambda env) t)
                | None -> None)
                _visitors_farg
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            Lbreak { arg = _visitors_r0; label = _visitors_r1 }

      method visit_Lcontinue : _ -> lambda list -> label -> lambda =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.map _visitors_this (self#visit_lambda env))
                _visitors_fargs
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            Lcontinue { args = _visitors_r0; label = _visitors_r1 }

      method visit_Lreturn : _ -> lambda -> lambda =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_lambda env _visitors_c0 in
           Lreturn _visitors_r0

      method visit_Lswitch :
          _ -> var -> (constr_tag * lambda) list -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        self#visit_constr_tag env _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      (_visitors_r0, _visitors_r1)))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              Lswitch
                {
                  obj = _visitors_r0;
                  cases = _visitors_r1;
                  default = _visitors_r2;
                  type_ = _visitors_r3;
                }

      method visit_Lswitchint :
          _ -> var -> (int * lambda) list -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> _visitors_this) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      (_visitors_r0, _visitors_r1)))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              Lswitchint
                {
                  obj = _visitors_r0;
                  cases = _visitors_r1;
                  default = _visitors_r2;
                  type_ = _visitors_r3;
                }

      method visit_Lswitchstring :
          _ -> lambda -> (string * lambda) list -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 =
                (fun _visitors_this ->
                  Basic_lst.map _visitors_this
                    (fun (_visitors_c0, _visitors_c1) ->
                      let _visitors_r0 =
                        (fun _visitors_this -> _visitors_this) _visitors_c0
                      in
                      let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                      (_visitors_r0, _visitors_r1)))
                  _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              Lswitchstring
                {
                  obj = _visitors_r0;
                  cases = _visitors_r1;
                  default = _visitors_r2;
                  type_ = _visitors_r3;
                }

      method visit_Lvar : _ -> var -> lambda =
        fun env ->
          fun _visitors_fvar ->
           let _visitors_r0 = self#visit_var env _visitors_fvar in
           Lvar { var = _visitors_r0 }

      method visit_Lassign : _ -> var -> lambda -> lambda =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fe ->
            let _visitors_r0 = self#visit_var env _visitors_fvar in
            let _visitors_r1 = self#visit_lambda env _visitors_fe in
            Lassign { var = _visitors_r0; e = _visitors_r1 }

      method visit_Lcatch : _ -> lambda -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fon_exception ->
            fun _visitors_ftype_ ->
             let _visitors_r0 = self#visit_lambda env _visitors_fbody in
             let _visitors_r1 = self#visit_lambda env _visitors_fon_exception in
             let _visitors_r2 = self#visit_ltype env _visitors_ftype_ in
             Lcatch
               {
                 body = _visitors_r0;
                 on_exception = _visitors_r1;
                 type_ = _visitors_r2;
               }

      method visit_Lcast : _ -> lambda -> ltype -> lambda =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftarget_type ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 = self#visit_ltype env _visitors_ftarget_type in
            Lcast { expr = _visitors_r0; target_type = _visitors_r1 }

      method visit_lambda : _ -> lambda -> lambda =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Levent { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
               self#visit_Levent env _visitors_fexpr _visitors_floc_
           | Lallocate _visitors_c0 -> self#visit_Lallocate env _visitors_c0
           | Lclosure _visitors_c0 -> self#visit_Lclosure env _visitors_c0
           | Lget_raw_func _visitors_c0 ->
               self#visit_Lget_raw_func env _visitors_c0
           | Lget_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lget_field env _visitors_fobj _visitors_ftid
                 _visitors_findex _visitors_fkind
           | Lclosure_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
               } ->
               self#visit_Lclosure_field env _visitors_fobj _visitors_ftid
                 _visitors_findex
           | Lset_field
               {
                 obj = _visitors_fobj;
                 field = _visitors_ffield;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lset_field env _visitors_fobj _visitors_ffield
                 _visitors_ftid _visitors_findex _visitors_fkind
           | Lmake_array
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 elems = _visitors_felems;
               } ->
               self#visit_Lmake_array env _visitors_ftid _visitors_fkind
                 _visitors_felems
           | Larray_get_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 extra = _visitors_fextra;
               } ->
               self#visit_Larray_get_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fextra
           | Larray_set_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 item = _visitors_fitem;
               } ->
               self#visit_Larray_set_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fitem
           | Lapply
               {
                 fn = _visitors_ffn;
                 prim = _visitors_fprim;
                 args = _visitors_fargs;
               } ->
               self#visit_Lapply env _visitors_ffn _visitors_fprim
                 _visitors_fargs
           | Lstub_call
               {
                 fn = _visitors_ffn;
                 args = _visitors_fargs;
                 params_ty = _visitors_fparams_ty;
                 return_ty = _visitors_freturn_ty;
               } ->
               self#visit_Lstub_call env _visitors_ffn _visitors_fargs
                 _visitors_fparams_ty _visitors_freturn_ty
           | Lconst _visitors_c0 -> self#visit_Lconst env _visitors_c0
           | Lloop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 label = _visitors_flabel;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lloop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_flabel _visitors_ftype_
           | Lif
               {
                 pred = _visitors_fpred;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lif env _visitors_fpred _visitors_fifso
                 _visitors_fifnot _visitors_ftype_
           | Llet
               {
                 name = _visitors_fname;
                 e = _visitors_fe;
                 body = _visitors_fbody;
               } ->
               self#visit_Llet env _visitors_fname _visitors_fe _visitors_fbody
           | Lletrec
               {
                 names = _visitors_fnames;
                 fns = _visitors_ffns;
                 body = _visitors_fbody;
               } ->
               self#visit_Lletrec env _visitors_fnames _visitors_ffns
                 _visitors_fbody
           | Lprim { fn = _visitors_ffn; args = _visitors_fargs } ->
               self#visit_Lprim env _visitors_ffn _visitors_fargs
           | Lsequence
               { exprs = _visitors_fexprs; last_expr = _visitors_flast_expr } ->
               self#visit_Lsequence env _visitors_fexprs _visitors_flast_expr
           | Ljoinlet
               {
                 name = _visitors_fname;
                 params = _visitors_fparams;
                 e = _visitors_fe;
                 body = _visitors_fbody;
                 kind = _visitors_fkind;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Ljoinlet env _visitors_fname _visitors_fparams
                 _visitors_fe _visitors_fbody _visitors_fkind _visitors_ftype_
           | Ljoinapply { name = _visitors_fname; args = _visitors_fargs } ->
               self#visit_Ljoinapply env _visitors_fname _visitors_fargs
           | Lbreak { arg = _visitors_farg; label = _visitors_flabel } ->
               self#visit_Lbreak env _visitors_farg _visitors_flabel
           | Lcontinue { args = _visitors_fargs; label = _visitors_flabel } ->
               self#visit_Lcontinue env _visitors_fargs _visitors_flabel
           | Lreturn _visitors_c0 -> self#visit_Lreturn env _visitors_c0
           | Lswitch
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitch env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchint
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchint env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchstring
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchstring env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lvar { var = _visitors_fvar } -> self#visit_Lvar env _visitors_fvar
           | Lassign { var = _visitors_fvar; e = _visitors_fe } ->
               self#visit_Lassign env _visitors_fvar _visitors_fe
           | Lcatch
               {
                 body = _visitors_fbody;
                 on_exception = _visitors_fon_exception;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lcatch env _visitors_fbody _visitors_fon_exception
                 _visitors_ftype_
           | Lcast
               { expr = _visitors_fexpr; target_type = _visitors_ftarget_type }
             ->
               self#visit_Lcast env _visitors_fexpr _visitors_ftarget_type
    end

  [@@@VISITORS.END]
end

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] sexp =
    object (self : 'self)
      inherit [_] sexpbase

      method visit_fn : _ -> fn -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_list self#visit_binder env _visitors_this.params
           in
           let _visitors_r1 = self#visit_lambda env _visitors_this.body in
           let _visitors_r2 =
             self#visit_list self#visit_ltype env _visitors_this.return_type_
           in
           self#visit_record env
             [
               ("params", _visitors_r0);
               ("body", _visitors_r1);
               ("return_type_", _visitors_r2);
             ]

      method visit_top_func_item : _ -> top_func_item -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_address env _visitors_this.binder in
           let _visitors_r1 = self#visit_fn_kind env _visitors_this.fn_kind_ in
           let _visitors_r2 = self#visit_fn env _visitors_this.fn in
           let _visitors_r3 =
             self#visit_option self#visit_tid env _visitors_this.tid
           in
           self#visit_record env
             [
               ("binder", _visitors_r0);
               ("fn_kind_", _visitors_r1);
               ("fn", _visitors_r2);
               ("tid", _visitors_r3);
             ]

      method visit_prog : _ -> prog -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_list self#visit_top_func_item env _visitors_this.fns
           in
           let _visitors_r1 =
             self#visit_option self#visit_lambda env _visitors_this.main
           in
           let _visitors_r2 = self#visit_lambda env _visitors_this.init in
           let _visitors_r3 =
             self#visit_list
               (fun env ->
                 fun (_visitors_c0, _visitors_c1) ->
                  let _visitors_r0 = self#visit_binder env _visitors_c0 in
                  let _visitors_r1 =
                    self#visit_option self#visit_constant env _visitors_c1
                  in
                  self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
               env _visitors_this.globals
           in
           let _visitors_r4 =
             self#visit_type_defs env _visitors_this.type_defs
           in
           self#visit_record env
             [
               ("fns", _visitors_r0);
               ("main", _visitors_r1);
               ("init", _visitors_r2);
               ("globals", _visitors_r3);
               ("type_defs", _visitors_r4);
             ]

      method visit_closure : _ -> closure -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_list self#visit_var env _visitors_this.captures
           in
           let _visitors_r1 =
             self#visit_closure_address env _visitors_this.address
           in
           let _visitors_r2 = self#visit_tid env _visitors_this.tid in
           self#visit_record env
             [
               ("captures", _visitors_r0);
               ("address", _visitors_r1);
               ("tid", _visitors_r2);
             ]

      method visit_aggregate : _ -> aggregate -> S.t =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 = self#visit_alloc_kind env _visitors_this.kind in
           let _visitors_r1 = self#visit_tid env _visitors_this.tid in
           let _visitors_r2 =
             self#visit_list self#visit_lambda env _visitors_this.fields
           in
           self#visit_record env
             [
               ("kind", _visitors_r0);
               ("tid", _visitors_r1);
               ("fields", _visitors_r2);
             ]

      method visit_Dynamic : _ -> var -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_var env _visitors_c0 in
           self#visit_inline_tuple env "Dynamic" [ _visitors_r0 ]

      method visit_StaticFn : _ -> address -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           self#visit_inline_tuple env "StaticFn" [ _visitors_r0 ]

      method visit_Object : _ -> var -> int -> ltype -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fmethod_index ->
            fun _visitors_fmethod_ty ->
             let _visitors_r0 = self#visit_var env _visitors_fobj in
             let _visitors_r1 = self#visit_int env _visitors_fmethod_index in
             let _visitors_r2 = self#visit_ltype env _visitors_fmethod_ty in
             self#visit_inline_record env "Object"
               [
                 ("obj", _visitors_r0);
                 ("method_index", _visitors_r1);
                 ("method_ty", _visitors_r2);
               ]

      method visit_target : _ -> target -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Dynamic _visitors_c0 -> self#visit_Dynamic env _visitors_c0
           | StaticFn _visitors_c0 -> self#visit_StaticFn env _visitors_c0
           | Object
               {
                 obj = _visitors_fobj;
                 method_index = _visitors_fmethod_index;
                 method_ty = _visitors_fmethod_ty;
               } ->
               self#visit_Object env _visitors_fobj _visitors_fmethod_index
                 _visitors_fmethod_ty

      method visit_FixedArray_copy : _ -> _ -> _ -> S.t =
        fun env ->
          fun _visitors_fsrc_tid ->
           fun _visitors_fdst_tid ->
            let _visitors_r0 = self#visit_tid env _visitors_fsrc_tid in
            let _visitors_r1 = self#visit_tid env _visitors_fdst_tid in
            self#visit_inline_record env "FixedArray_copy"
              [ ("src_tid", _visitors_r0); ("dst_tid", _visitors_r1) ]

      method visit_FixedArray_fill : _ -> _ -> S.t =
        fun env ->
          fun _visitors_ftid ->
           let _visitors_r0 = self#visit_tid env _visitors_ftid in
           self#visit_inline_record env "FixedArray_fill"
             [ ("tid", _visitors_r0) ]

      method visit_Char_to_string : _ -> S.t =
        fun env -> self#visit_inline_tuple env "Char_to_string" []

      method visit_intrinsic : _ -> intrinsic -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | FixedArray_copy
               { src_tid = _visitors_fsrc_tid; dst_tid = _visitors_fdst_tid } ->
               self#visit_FixedArray_copy env _visitors_fsrc_tid
                 _visitors_fdst_tid
           | FixedArray_fill { tid = _visitors_ftid } ->
               self#visit_FixedArray_fill env _visitors_ftid
           | Char_to_string -> self#visit_Char_to_string env

      method visit_Levent : _ -> lambda -> location -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_floc_ ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 = self#visit_location env _visitors_floc_ in
            self#visit_inline_record env "Levent"
              [ ("expr", _visitors_r0); ("loc_", _visitors_r1) ]

      method visit_Lallocate : _ -> aggregate -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_aggregate env _visitors_c0 in
           self#visit_inline_tuple env "Lallocate" [ _visitors_r0 ]

      method visit_Lclosure : _ -> closure -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_closure env _visitors_c0 in
           self#visit_inline_tuple env "Lclosure" [ _visitors_r0 ]

      method visit_Lget_raw_func : _ -> address -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_address env _visitors_c0 in
           self#visit_inline_tuple env "Lget_raw_func" [ _visitors_r0 ]

      method visit_Lget_field : _ -> lambda -> _ -> int -> get_field_kind -> S.t
          =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             fun _visitors_fkind ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 = self#visit_tid env _visitors_ftid in
              let _visitors_r2 = self#visit_int env _visitors_findex in
              let _visitors_r3 =
                self#visit_get_field_kind env _visitors_fkind
              in
              self#visit_inline_record env "Lget_field"
                [
                  ("obj", _visitors_r0);
                  ("tid", _visitors_r1);
                  ("index", _visitors_r2);
                  ("kind", _visitors_r3);
                ]

      method visit_Lclosure_field : _ -> lambda -> _ -> int -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ftid ->
            fun _visitors_findex ->
             let _visitors_r0 = self#visit_lambda env _visitors_fobj in
             let _visitors_r1 = self#visit_tid env _visitors_ftid in
             let _visitors_r2 = self#visit_int env _visitors_findex in
             self#visit_inline_record env "Lclosure_field"
               [
                 ("obj", _visitors_r0);
                 ("tid", _visitors_r1);
                 ("index", _visitors_r2);
               ]

      method visit_Lset_field :
          _ -> lambda -> lambda -> _ -> int -> set_field_kind -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_ffield ->
            fun _visitors_ftid ->
             fun _visitors_findex ->
              fun _visitors_fkind ->
               let _visitors_r0 = self#visit_lambda env _visitors_fobj in
               let _visitors_r1 = self#visit_lambda env _visitors_ffield in
               let _visitors_r2 = self#visit_tid env _visitors_ftid in
               let _visitors_r3 = self#visit_int env _visitors_findex in
               let _visitors_r4 =
                 self#visit_set_field_kind env _visitors_fkind
               in
               self#visit_inline_record env "Lset_field"
                 [
                   ("obj", _visitors_r0);
                   ("field", _visitors_r1);
                   ("tid", _visitors_r2);
                   ("index", _visitors_r3);
                   ("kind", _visitors_r4);
                 ]

      method visit_Lmake_array : _ -> _ -> _ -> lambda list -> S.t =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_felems ->
             let _visitors_r0 = self#visit_tid env _visitors_ftid in
             let _visitors_r1 =
               self#visit_make_array_kind env _visitors_fkind
             in
             let _visitors_r2 =
               self#visit_list self#visit_lambda env _visitors_felems
             in
             self#visit_inline_record env "Lmake_array"
               [
                 ("tid", _visitors_r0);
                 ("kind", _visitors_r1);
                 ("elems", _visitors_r2);
               ]

      method visit_Larray_get_item : _ -> _ -> _ -> lambda -> lambda -> _ -> S.t
          =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fextra ->
               let _visitors_r0 = self#visit_tid env _visitors_ftid in
               let _visitors_r1 =
                 self#visit_array_get_kind env _visitors_fkind
               in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 =
                 self#visit_get_item_extra env _visitors_fextra
               in
               self#visit_inline_record env "Larray_get_item"
                 [
                   ("tid", _visitors_r0);
                   ("kind", _visitors_r1);
                   ("arr", _visitors_r2);
                   ("index", _visitors_r3);
                   ("extra", _visitors_r4);
                 ]

      method visit_Larray_set_item :
          _ -> _ -> _ -> lambda -> lambda -> lambda option -> S.t =
        fun env ->
          fun _visitors_ftid ->
           fun _visitors_fkind ->
            fun _visitors_farr ->
             fun _visitors_findex ->
              fun _visitors_fitem ->
               let _visitors_r0 = self#visit_tid env _visitors_ftid in
               let _visitors_r1 =
                 self#visit_array_set_kind env _visitors_fkind
               in
               let _visitors_r2 = self#visit_lambda env _visitors_farr in
               let _visitors_r3 = self#visit_lambda env _visitors_findex in
               let _visitors_r4 =
                 self#visit_option self#visit_lambda env _visitors_fitem
               in
               self#visit_inline_record env "Larray_set_item"
                 [
                   ("tid", _visitors_r0);
                   ("kind", _visitors_r1);
                   ("arr", _visitors_r2);
                   ("index", _visitors_r3);
                   ("item", _visitors_r4);
                 ]

      method visit_Lapply :
          _ -> target -> intrinsic option -> lambda list -> S.t =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fprim ->
            fun _visitors_fargs ->
             let _visitors_r0 = self#visit_target env _visitors_ffn in
             let _visitors_r1 =
               self#visit_option self#visit_intrinsic env _visitors_fprim
             in
             let _visitors_r2 =
               self#visit_list self#visit_lambda env _visitors_fargs
             in
             self#visit_inline_record env "Lapply"
               [
                 ("fn", _visitors_r0);
                 ("prim", _visitors_r1);
                 ("args", _visitors_r2);
               ]

      method visit_Lstub_call :
          _ -> func_stubs -> lambda list -> ltype list -> ltype option -> S.t =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            fun _visitors_fparams_ty ->
             fun _visitors_freturn_ty ->
              let _visitors_r0 = self#visit_func_stubs env _visitors_ffn in
              let _visitors_r1 =
                self#visit_list self#visit_lambda env _visitors_fargs
              in
              let _visitors_r2 =
                self#visit_list self#visit_ltype env _visitors_fparams_ty
              in
              let _visitors_r3 =
                self#visit_option self#visit_ltype env _visitors_freturn_ty
              in
              self#visit_inline_record env "Lstub_call"
                [
                  ("fn", _visitors_r0);
                  ("args", _visitors_r1);
                  ("params_ty", _visitors_r2);
                  ("return_ty", _visitors_r3);
                ]

      method visit_Lconst : _ -> constant -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_constant env _visitors_c0 in
           self#visit_inline_tuple env "Lconst" [ _visitors_r0 ]

      method visit_Lloop :
          _ -> binder list -> lambda -> lambda list -> label -> ltype -> S.t =
        fun env ->
          fun _visitors_fparams ->
           fun _visitors_fbody ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_ftype_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_binder env _visitors_fparams
               in
               let _visitors_r1 = self#visit_lambda env _visitors_fbody in
               let _visitors_r2 =
                 self#visit_list self#visit_lambda env _visitors_fargs
               in
               let _visitors_r3 = self#visit_label env _visitors_flabel in
               let _visitors_r4 = self#visit_ltype env _visitors_ftype_ in
               self#visit_inline_record env "Lloop"
                 [
                   ("params", _visitors_r0);
                   ("body", _visitors_r1);
                   ("args", _visitors_r2);
                   ("label", _visitors_r3);
                   ("type_", _visitors_r4);
                 ]

      method visit_Lif : _ -> lambda -> lambda -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fpred ->
           fun _visitors_fifso ->
            fun _visitors_fifnot ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fpred in
              let _visitors_r1 = self#visit_lambda env _visitors_fifso in
              let _visitors_r2 = self#visit_lambda env _visitors_fifnot in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              self#visit_inline_record env "Lif"
                [
                  ("pred", _visitors_r0);
                  ("ifso", _visitors_r1);
                  ("ifnot", _visitors_r2);
                  ("type_", _visitors_r3);
                ]

      method visit_Llet : _ -> binder -> lambda -> lambda -> S.t =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fe ->
            fun _visitors_fbody ->
             let _visitors_r0 = self#visit_binder env _visitors_fname in
             let _visitors_r1 = self#visit_lambda env _visitors_fe in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             self#visit_inline_record env "Llet"
               [
                 ("name", _visitors_r0);
                 ("e", _visitors_r1);
                 ("body", _visitors_r2);
               ]

      method visit_Lletrec : _ -> binder list -> closure list -> lambda -> S.t =
        fun env ->
          fun _visitors_fnames ->
           fun _visitors_ffns ->
            fun _visitors_fbody ->
             let _visitors_r0 =
               self#visit_list self#visit_binder env _visitors_fnames
             in
             let _visitors_r1 =
               self#visit_list self#visit_closure env _visitors_ffns
             in
             let _visitors_r2 = self#visit_lambda env _visitors_fbody in
             self#visit_inline_record env "Lletrec"
               [
                 ("names", _visitors_r0);
                 ("fns", _visitors_r1);
                 ("body", _visitors_r2);
               ]

      method visit_Lprim : _ -> prim -> lambda list -> S.t =
        fun env ->
          fun _visitors_ffn ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_prim env _visitors_ffn in
            let _visitors_r1 =
              self#visit_list self#visit_lambda env _visitors_fargs
            in
            self#visit_inline_record env "Lprim"
              [ ("fn", _visitors_r0); ("args", _visitors_r1) ]

      method visit_Lsequence : _ -> lambda list -> lambda -> S.t =
        fun env ->
          fun _visitors_fexprs ->
           fun _visitors_flast_expr ->
            let _visitors_r0 =
              self#visit_list self#visit_lambda env _visitors_fexprs
            in
            let _visitors_r1 = self#visit_lambda env _visitors_flast_expr in
            self#visit_inline_record env "Lsequence"
              [ ("exprs", _visitors_r0); ("last_expr", _visitors_r1) ]

      method visit_Ljoinlet :
          _ ->
          join ->
          binder list ->
          lambda ->
          lambda ->
          join_kind ->
          ltype list ->
          S.t =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fparams ->
            fun _visitors_fe ->
             fun _visitors_fbody ->
              fun _visitors_fkind ->
               fun _visitors_ftype_ ->
                let _visitors_r0 = self#visit_join env _visitors_fname in
                let _visitors_r1 =
                  self#visit_list self#visit_binder env _visitors_fparams
                in
                let _visitors_r2 = self#visit_lambda env _visitors_fe in
                let _visitors_r3 = self#visit_lambda env _visitors_fbody in
                let _visitors_r4 = self#visit_join_kind env _visitors_fkind in
                let _visitors_r5 =
                  self#visit_list self#visit_ltype env _visitors_ftype_
                in
                self#visit_inline_record env "Ljoinlet"
                  [
                    ("name", _visitors_r0);
                    ("params", _visitors_r1);
                    ("e", _visitors_r2);
                    ("body", _visitors_r3);
                    ("kind", _visitors_r4);
                    ("type_", _visitors_r5);
                  ]

      method visit_Ljoinapply : _ -> join -> lambda list -> S.t =
        fun env ->
          fun _visitors_fname ->
           fun _visitors_fargs ->
            let _visitors_r0 = self#visit_join env _visitors_fname in
            let _visitors_r1 =
              self#visit_list self#visit_lambda env _visitors_fargs
            in
            self#visit_inline_record env "Ljoinapply"
              [ ("name", _visitors_r0); ("args", _visitors_r1) ]

      method visit_Lbreak : _ -> lambda option -> label -> S.t =
        fun env ->
          fun _visitors_farg ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              self#visit_option self#visit_lambda env _visitors_farg
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            self#visit_inline_record env "Lbreak"
              [ ("arg", _visitors_r0); ("label", _visitors_r1) ]

      method visit_Lcontinue : _ -> lambda list -> label -> S.t =
        fun env ->
          fun _visitors_fargs ->
           fun _visitors_flabel ->
            let _visitors_r0 =
              self#visit_list self#visit_lambda env _visitors_fargs
            in
            let _visitors_r1 = self#visit_label env _visitors_flabel in
            self#visit_inline_record env "Lcontinue"
              [ ("args", _visitors_r0); ("label", _visitors_r1) ]

      method visit_Lreturn : _ -> lambda -> S.t =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_lambda env _visitors_c0 in
           self#visit_inline_tuple env "Lreturn" [ _visitors_r0 ]

      method visit_Lswitch :
          _ -> var -> (constr_tag * lambda) list -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                self#visit_list
                  (fun env ->
                    fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 =
                       self#visit_constr_tag env _visitors_c0
                     in
                     let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                     self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                  env _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              self#visit_inline_record env "Lswitch"
                [
                  ("obj", _visitors_r0);
                  ("cases", _visitors_r1);
                  ("default", _visitors_r2);
                  ("type_", _visitors_r3);
                ]

      method visit_Lswitchint :
          _ -> var -> (int * lambda) list -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_var env _visitors_fobj in
              let _visitors_r1 =
                self#visit_list
                  (fun env ->
                    fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 = self#visit_int env _visitors_c0 in
                     let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                     self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                  env _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              self#visit_inline_record env "Lswitchint"
                [
                  ("obj", _visitors_r0);
                  ("cases", _visitors_r1);
                  ("default", _visitors_r2);
                  ("type_", _visitors_r3);
                ]

      method visit_Lswitchstring :
          _ -> lambda -> (string * lambda) list -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fobj ->
           fun _visitors_fcases ->
            fun _visitors_fdefault ->
             fun _visitors_ftype_ ->
              let _visitors_r0 = self#visit_lambda env _visitors_fobj in
              let _visitors_r1 =
                self#visit_list
                  (fun env ->
                    fun (_visitors_c0, _visitors_c1) ->
                     let _visitors_r0 = self#visit_string env _visitors_c0 in
                     let _visitors_r1 = self#visit_lambda env _visitors_c1 in
                     self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                  env _visitors_fcases
              in
              let _visitors_r2 = self#visit_lambda env _visitors_fdefault in
              let _visitors_r3 = self#visit_ltype env _visitors_ftype_ in
              self#visit_inline_record env "Lswitchstring"
                [
                  ("obj", _visitors_r0);
                  ("cases", _visitors_r1);
                  ("default", _visitors_r2);
                  ("type_", _visitors_r3);
                ]

      method visit_Lvar : _ -> var -> S.t =
        fun env ->
          fun _visitors_fvar ->
           let _visitors_r0 = self#visit_var env _visitors_fvar in
           self#visit_inline_record env "Lvar" [ ("var", _visitors_r0) ]

      method visit_Lassign : _ -> var -> lambda -> S.t =
        fun env ->
          fun _visitors_fvar ->
           fun _visitors_fe ->
            let _visitors_r0 = self#visit_var env _visitors_fvar in
            let _visitors_r1 = self#visit_lambda env _visitors_fe in
            self#visit_inline_record env "Lassign"
              [ ("var", _visitors_r0); ("e", _visitors_r1) ]

      method visit_Lcatch : _ -> lambda -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fbody ->
           fun _visitors_fon_exception ->
            fun _visitors_ftype_ ->
             let _visitors_r0 = self#visit_lambda env _visitors_fbody in
             let _visitors_r1 = self#visit_lambda env _visitors_fon_exception in
             let _visitors_r2 = self#visit_ltype env _visitors_ftype_ in
             self#visit_inline_record env "Lcatch"
               [
                 ("body", _visitors_r0);
                 ("on_exception", _visitors_r1);
                 ("type_", _visitors_r2);
               ]

      method visit_Lcast : _ -> lambda -> ltype -> S.t =
        fun env ->
          fun _visitors_fexpr ->
           fun _visitors_ftarget_type ->
            let _visitors_r0 = self#visit_lambda env _visitors_fexpr in
            let _visitors_r1 = self#visit_ltype env _visitors_ftarget_type in
            self#visit_inline_record env "Lcast"
              [ ("expr", _visitors_r0); ("target_type", _visitors_r1) ]

      method visit_lambda : _ -> lambda -> S.t =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Levent { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
               self#visit_Levent env _visitors_fexpr _visitors_floc_
           | Lallocate _visitors_c0 -> self#visit_Lallocate env _visitors_c0
           | Lclosure _visitors_c0 -> self#visit_Lclosure env _visitors_c0
           | Lget_raw_func _visitors_c0 ->
               self#visit_Lget_raw_func env _visitors_c0
           | Lget_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lget_field env _visitors_fobj _visitors_ftid
                 _visitors_findex _visitors_fkind
           | Lclosure_field
               {
                 obj = _visitors_fobj;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
               } ->
               self#visit_Lclosure_field env _visitors_fobj _visitors_ftid
                 _visitors_findex
           | Lset_field
               {
                 obj = _visitors_fobj;
                 field = _visitors_ffield;
                 tid = _visitors_ftid;
                 index = _visitors_findex;
                 kind = _visitors_fkind;
               } ->
               self#visit_Lset_field env _visitors_fobj _visitors_ffield
                 _visitors_ftid _visitors_findex _visitors_fkind
           | Lmake_array
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 elems = _visitors_felems;
               } ->
               self#visit_Lmake_array env _visitors_ftid _visitors_fkind
                 _visitors_felems
           | Larray_get_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 extra = _visitors_fextra;
               } ->
               self#visit_Larray_get_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fextra
           | Larray_set_item
               {
                 tid = _visitors_ftid;
                 kind = _visitors_fkind;
                 arr = _visitors_farr;
                 index = _visitors_findex;
                 item = _visitors_fitem;
               } ->
               self#visit_Larray_set_item env _visitors_ftid _visitors_fkind
                 _visitors_farr _visitors_findex _visitors_fitem
           | Lapply
               {
                 fn = _visitors_ffn;
                 prim = _visitors_fprim;
                 args = _visitors_fargs;
               } ->
               self#visit_Lapply env _visitors_ffn _visitors_fprim
                 _visitors_fargs
           | Lstub_call
               {
                 fn = _visitors_ffn;
                 args = _visitors_fargs;
                 params_ty = _visitors_fparams_ty;
                 return_ty = _visitors_freturn_ty;
               } ->
               self#visit_Lstub_call env _visitors_ffn _visitors_fargs
                 _visitors_fparams_ty _visitors_freturn_ty
           | Lconst _visitors_c0 -> self#visit_Lconst env _visitors_c0
           | Lloop
               {
                 params = _visitors_fparams;
                 body = _visitors_fbody;
                 args = _visitors_fargs;
                 label = _visitors_flabel;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lloop env _visitors_fparams _visitors_fbody
                 _visitors_fargs _visitors_flabel _visitors_ftype_
           | Lif
               {
                 pred = _visitors_fpred;
                 ifso = _visitors_fifso;
                 ifnot = _visitors_fifnot;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lif env _visitors_fpred _visitors_fifso
                 _visitors_fifnot _visitors_ftype_
           | Llet
               {
                 name = _visitors_fname;
                 e = _visitors_fe;
                 body = _visitors_fbody;
               } ->
               self#visit_Llet env _visitors_fname _visitors_fe _visitors_fbody
           | Lletrec
               {
                 names = _visitors_fnames;
                 fns = _visitors_ffns;
                 body = _visitors_fbody;
               } ->
               self#visit_Lletrec env _visitors_fnames _visitors_ffns
                 _visitors_fbody
           | Lprim { fn = _visitors_ffn; args = _visitors_fargs } ->
               self#visit_Lprim env _visitors_ffn _visitors_fargs
           | Lsequence
               { exprs = _visitors_fexprs; last_expr = _visitors_flast_expr } ->
               self#visit_Lsequence env _visitors_fexprs _visitors_flast_expr
           | Ljoinlet
               {
                 name = _visitors_fname;
                 params = _visitors_fparams;
                 e = _visitors_fe;
                 body = _visitors_fbody;
                 kind = _visitors_fkind;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Ljoinlet env _visitors_fname _visitors_fparams
                 _visitors_fe _visitors_fbody _visitors_fkind _visitors_ftype_
           | Ljoinapply { name = _visitors_fname; args = _visitors_fargs } ->
               self#visit_Ljoinapply env _visitors_fname _visitors_fargs
           | Lbreak { arg = _visitors_farg; label = _visitors_flabel } ->
               self#visit_Lbreak env _visitors_farg _visitors_flabel
           | Lcontinue { args = _visitors_fargs; label = _visitors_flabel } ->
               self#visit_Lcontinue env _visitors_fargs _visitors_flabel
           | Lreturn _visitors_c0 -> self#visit_Lreturn env _visitors_c0
           | Lswitch
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitch env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchint
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchint env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lswitchstring
               {
                 obj = _visitors_fobj;
                 cases = _visitors_fcases;
                 default = _visitors_fdefault;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lswitchstring env _visitors_fobj _visitors_fcases
                 _visitors_fdefault _visitors_ftype_
           | Lvar { var = _visitors_fvar } -> self#visit_Lvar env _visitors_fvar
           | Lassign { var = _visitors_fvar; e = _visitors_fe } ->
               self#visit_Lassign env _visitors_fvar _visitors_fe
           | Lcatch
               {
                 body = _visitors_fbody;
                 on_exception = _visitors_fon_exception;
                 type_ = _visitors_ftype_;
               } ->
               self#visit_Lcatch env _visitors_fbody _visitors_fon_exception
                 _visitors_ftype_
           | Lcast
               { expr = _visitors_fexpr; target_type = _visitors_ftarget_type }
             ->
               self#visit_Lcast env _visitors_fexpr _visitors_ftarget_type
    end

  [@@@VISITORS.END]
end

include struct
  let _ = fun (_ : fn) -> ()
  let _ = fun (_ : top_func_item) -> ()
  let _ = fun (_ : prog) -> ()
  let _ = fun (_ : closure) -> ()
  let _ = fun (_ : aggregate) -> ()
  let _ = fun (_ : target) -> ()
  let _ = fun (_ : intrinsic) -> ()
  let _ = fun (_ : lambda) -> ()
end

let sexp =
  object (self)
    inherit [_] sexp as super
    method! visit_Lconst env c = self#visit_constant env c

    method! visit_inline_record env ctor fields =
      let predicate field acc =
        match (ctor, field) with
        | "Ljoinapply", ("param_names", _) -> acc
        | _, ("loc_", _) -> acc
        | _, ("continue_block", S.List []) -> acc
        | _ -> field :: acc
      in
      super#visit_inline_record env ctor
        (Basic_lst.fold_right fields [] predicate)

    method! visit_Levent (_ : 'a) e loc_ =
      (if !Basic_config.show_loc then
         let e = self#visit_lambda () e in
         let loc_ = self#visit_location () loc_ in
         (List
            (List.cons
               (Atom "Levent" : S.t)
               (List.cons (e : S.t) ([ loc_ ] : S.t list)))
           : S.t)
       else self#visit_lambda () e
        : S.t)

    method! visit_Lprim env prim args =
      let args = Basic_lst.map args (fun x -> self#visit_lambda env x) in
      (List (List.cons (sexp_of_prim prim : S.t) (args : S.t list)) : S.t)

    method! visit_Lvar _ v = sexp_of_var v

    method! visit_Lapply env fn prim args =
      let fn = self#visit_target () fn in
      let args = Basic_lst.map args (fun x -> self#visit_lambda () x) in
      match prim with
      | None ->
          (List
             (List.cons
                (Atom "Lapply" : S.t)
                (List.cons (fn : S.t) ([ List (args : S.t list) ] : S.t list)))
            : S.t)
      | Some p ->
          let prim = self#visit_intrinsic env p in
          (List
             (List.cons
                (Atom "Lapply" : S.t)
                (List.cons
                   (fn : S.t)
                   (List.cons
                      (prim : S.t)
                      ([ List (args : S.t list) ] : S.t list))))
            : S.t)

    method! visit_Llet env name e body =
      let rec aux lam r =
        match lam with
        | Levent { expr; loc_ = _ } when not !Basic_config.show_loc ->
            aux expr r
        | Llet { name; e; body } -> aux body ((name, e) :: r)
        | _ ->
            let cont = self#visit_lambda env lam in
            let r =
              List.fold_left
                (fun acc ->
                  fun (name, e) ->
                   let e = self#visit_lambda env e in
                   (List
                      (List.cons (sexp_of_binder name : S.t) ([ e ] : S.t list))
                     : S.t)
                   :: acc)
                [] r
            in
            (List
               (List.cons
                  (Atom "Llet" : S.t)
                  (List.cons (List (r : S.t list) : S.t) ([ cont ] : S.t list)))
              : S.t)
      in
      aux body [ (name, e) ]

    method! visit_Lsequence env exprs last_expr =
      let rec aux lam (r : lambda list) =
        match lam with
        | Levent { expr; loc_ = _ } when not !Basic_config.show_loc ->
            aux expr r
        | Lsequence { exprs; last_expr } ->
            aux last_expr (List.rev_append exprs r)
        | _ ->
            let cont = self#visit_lambda env lam in
            let r =
              List.fold_left
                (fun acc -> fun a -> self#visit_lambda env a :: acc)
                [ cont ] r
            in
            (List (List.cons (Atom "Lsequence" : S.t) (r : S.t list)) : S.t)
      in
      aux last_expr (List.rev exprs)

    method! visit_Ljoinlet env name params e body kind _type_ =
      let name = Join.sexp_of_t name in
      let params =
        (fun x__015_ -> Moon_sexp_conv.sexp_of_list sexp_of_var x__015_) params
      in
      let e = self#visit_lambda env e in
      let body = self#visit_lambda env body in
      match kind with
      | Tail_join ->
          (List
             (List.cons
                (Atom "Ljoinlet" : S.t)
                (List.cons
                   (name : S.t)
                   (List.cons
                      (params : S.t)
                      (List.cons (e : S.t) ([ body ] : S.t list)))))
            : S.t)
      | Nontail_join ->
          (List
             (List.cons
                (Atom "Ljoinlet_nontail" : S.t)
                (List.cons
                   (name : S.t)
                   (List.cons
                      (params : S.t)
                      (List.cons (e : S.t) ([ body ] : S.t list)))))
            : S.t)

    method! visit_Lbreak env arg label =
      let label = Label.sexp_of_t label in
      match arg with
      | None ->
          (List (List.cons (Atom "Lbreak" : S.t) ([ label ] : S.t list)) : S.t)
      | Some arg ->
          let arg = self#visit_lambda env arg in
          (List
             (List.cons
                (Atom "Lbreak" : S.t)
                (List.cons (label : S.t) ([ arg ] : S.t list)))
            : S.t)

    method! visit_Lcontinue env args label =
      let label = Label.sexp_of_t label in
      let args = Basic_lst.map args (fun x -> self#visit_lambda env x) in
      (List
         (List.cons
            (Atom "Lcontinue" : S.t)
            (List.cons (label : S.t) (args : S.t list)))
        : S.t)

    method! visit_fn env fn =
      let params = Lst.map fn.params (fun p -> self#visit_binder env p) in
      let body = self#visit_lambda env fn.body in
      let return_type_ =
        match fn.return_type_ with
        | t :: [] -> self#visit_ltype env t
        | ts ->
            let ts = Lst.map ts (fun p -> self#visit_ltype env p) in
            (List (ts : S.t list) : S.t)
      in
      (List
         (List.cons
            (List
               (List.cons
                  (Atom "params" : S.t)
                  ([ List (params : S.t list) ] : S.t list))
              : S.t)
            (List.cons
               (List (List.cons (Atom "body" : S.t) ([ body ] : S.t list))
                 : S.t)
               ([
                  List
                    (List.cons
                       (Atom "return_type_" : S.t)
                       ([ return_type_ ] : S.t list));
                ]
                 : S.t list)))
        : S.t)
  end

let sexp_of_prog prog = sexp#visit_prog () prog

let event ~(loc_ : location) (expr : lambda) =
  if (not !Basic_config.debug) || Basic_prelude.phys_equal loc_ Loc.no_location
  then expr
  else Levent { loc_; expr }
