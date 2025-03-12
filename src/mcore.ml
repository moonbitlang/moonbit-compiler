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


module Ident = Basic_core_ident
module Constr_info = Basic_constr_info
module Lst = Basic_lst
module Syntax = Parsing_syntax

type constant = Constant.t

include struct
  let _ = fun (_ : constant) -> ()
  let sexp_of_constant = (Constant.sexp_of_t : constant -> S.t)
  let _ = sexp_of_constant
end

type prim = Primitive.prim

include struct
  let _ = fun (_ : prim) -> ()
  let sexp_of_prim = (Primitive.sexp_of_prim : prim -> S.t)
  let _ = sexp_of_prim
end

type constr = Syntax.constructor

include struct
  let _ = fun (_ : constr) -> ()
  let sexp_of_constr = (Syntax.sexp_of_constructor : constr -> S.t)
  let _ = sexp_of_constr
end

type constr_tag = Tag.t

include struct
  let _ = fun (_ : constr_tag) -> ()
  let sexp_of_constr_tag = (Tag.sexp_of_t : constr_tag -> S.t)
  let _ = sexp_of_constr_tag
end

type label = Syntax.label

include struct
  let _ = fun (_ : label) -> ()
  let sexp_of_label = (Syntax.sexp_of_label : label -> S.t)
  let _ = sexp_of_label
end

type accessor = Syntax.accessor

include struct
  let _ = fun (_ : accessor) -> ()
  let sexp_of_accessor = (Syntax.sexp_of_accessor : accessor -> S.t)
  let _ = sexp_of_accessor
end

type location = Rloc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Rloc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type absolute_loc = Loc.t

include struct
  let _ = fun (_ : absolute_loc) -> ()
  let sexp_of_absolute_loc = (Loc.sexp_of_t : absolute_loc -> S.t)
  let _ = sexp_of_absolute_loc
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

type loop_label = Label.t

include struct
  let _ = fun (_ : loop_label) -> ()
  let sexp_of_loop_label = (Label.sexp_of_t : loop_label -> S.t)
  let _ = sexp_of_loop_label
end

type typ = Mtype.t

include struct
  let _ = fun (_ : typ) -> ()
  let sexp_of_typ = (Mtype.sexp_of_t : typ -> S.t)
  let _ = sexp_of_typ
end

type func_stubs = Stub_type.t

include struct
  let _ = fun (_ : func_stubs) -> ()
  let sexp_of_func_stubs = (Stub_type.sexp_of_t : func_stubs -> S.t)
  let _ = sexp_of_func_stubs
end

type letfn_kind = Core.letfn_kind =
  | Nonrec
  | Rec [@dead "letfn_kind.Rec"]
  | Tail_join
  | Nontail_join [@dead "letfn_kind.Nontail_join"]

include struct
  let _ = fun (_ : letfn_kind) -> ()

  let sexp_of_letfn_kind =
    (function
     | Nonrec -> S.Atom "Nonrec"
     | Rec -> S.Atom "Rec"
     | Tail_join -> S.Atom "Tail_join"
     | Nontail_join -> S.Atom "Nontail_join"
      : letfn_kind -> S.t)

  let _ = sexp_of_letfn_kind
end

type return_kind =
  | Error_result of { is_error : bool; return_ty : typ }
  | Single_value

include struct
  let _ = fun (_ : return_kind) -> ()

  let sexp_of_return_kind =
    (function
     | Error_result { is_error = is_error__002_; return_ty = return_ty__004_ }
       ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__005_ = sexp_of_typ return_ty__004_ in
           (S.List [ S.Atom "return_ty"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_bool is_error__002_ in
           (S.List [ S.Atom "is_error"; arg__003_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Error_result" :: bnds__001_)
     | Single_value -> S.Atom "Single_value"
      : return_kind -> S.t)

  let _ = sexp_of_return_kind
end

type top_item =
  | Ctop_expr of { expr : expr; loc_ : absolute_loc }
  | Ctop_let of {
      binder : binder;
      expr : expr;
      is_pub_ : bool;
      loc_ : absolute_loc;
    }
  | Ctop_fn of top_fun_decl
  | Ctop_stub of {
      binder : binder;
      func_stubs : func_stubs;
      params_ty : typ list;
      return_ty : typ option;
      export_info_ : string option;
      loc_ : absolute_loc;
    }

and top_fun_decl = {
  binder : binder;
  func : fn;
  export_info_ : string option;
  loc_ : absolute_loc;
}

and handle_kind = To_result | Joinapply of var | Return_err of { ok_ty : typ }

and expr =
  | Cexpr_const of { c : constant; ty : typ; loc_ : location }
  | Cexpr_unit of { loc_ : location }
  | Cexpr_var of { id : var; prim : prim option; ty : typ; loc_ : location }
  | Cexpr_prim of { prim : prim; args : expr list; ty : typ; loc_ : location }
      [@build fun p -> fun args -> fun ty -> fun loc -> prim ~loc ~ty p args]
  | Cexpr_let of {
      name : binder;
      rhs : expr;
      body : expr;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_letfn of {
      name : binder;
      fn : fn;
      body : expr;
      ty : typ;
      kind : letfn_kind;
      loc_ : location;
    }
  | Cexpr_function of { func : fn; ty : typ; is_raw_ : bool; loc_ : location }
  | Cexpr_apply of {
      func : var;
      args : expr list;
      kind : apply_kind;
      prim : prim option;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_object of {
      methods_key : Object_util.object_key;
      self : expr;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_letrec of {
      bindings : (binder * fn) list;
      body : expr;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_constr of {
      tag : constr_tag;
      args : expr list;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_tuple of { exprs : expr list; ty : typ; loc_ : location }
  | Cexpr_record of { fields : field_def list; ty : typ; loc_ : location }
  | Cexpr_record_update of {
      record : expr;
      fields : field_def list;
      fields_num : int;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_field of {
      record : expr;
      accessor : accessor;
      pos : int;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_mutate of {
      record : expr;
      label : label;
      field : expr;
      pos : int;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_array of { exprs : expr list; ty : typ; loc_ : location }
  | Cexpr_assign of { var : var; expr : expr; ty : typ; loc_ : location }
  | Cexpr_sequence of {
      exprs : expr list;
      last_expr : expr;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_if of {
      cond : expr;
      ifso : expr;
      ifnot : expr option;
      ty : typ;
      loc_ : location;
    }
      [@build
        fun cond ->
          fun ifso ->
           fun ifnot -> fun ty -> fun loc -> if_ ~loc cond ~ifso ?ifnot]
  | Cexpr_switch_constr of {
      obj : expr;
      cases : (constr_tag * binder option * expr) list;
      default : expr option;
      ty : typ;
      loc_ : location;
    }
      [@build
        fun obj ->
          fun cases ->
           fun default ->
            fun ty -> fun loc -> switch_constr ~loc obj cases ~default]
  | Cexpr_switch_constant of {
      obj : expr;
      cases : (constant * expr) list;
      default : expr;
      ty : typ;
      loc_ : location;
    }
      [@build
        fun obj ->
          fun cases ->
           fun default ->
            fun ty -> fun loc -> switch_constant ~loc obj cases ~default]
  | Cexpr_loop of {
      params : param list;
      body : expr;
      args : expr list;
      label : loop_label;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_break of {
      arg : expr option;
      label : loop_label;
      ty : typ;
      loc_ : location;
    }
      [@build
        fun arg -> fun label -> fun ty -> fun loc_ -> break arg label ty ~loc_]
  | Cexpr_continue of {
      args : expr list;
      label : loop_label;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_handle_error of {
      obj : expr;
      handle_kind : handle_kind;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_return of {
      expr : expr;
      return_kind : return_kind;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_and of { lhs : expr; rhs : expr; loc_ : location }
  | Cexpr_or of { lhs : expr; rhs : expr; loc_ : location }

and fn = { params : param list; body : expr }
and param = { binder : binder; ty : typ; loc_ : location }
and apply_kind = Normal of { func_ty : typ } | Join
and field_def = { label : label; pos : int; is_mut : bool; expr : expr }

include struct
  let _ = fun (_ : top_item) -> ()
  let _ = fun (_ : top_fun_decl) -> ()
  let _ = fun (_ : handle_kind) -> ()
  let _ = fun (_ : expr) -> ()
  let _ = fun (_ : fn) -> ()
  let _ = fun (_ : param) -> ()
  let _ = fun (_ : apply_kind) -> ()
  let _ = fun (_ : field_def) -> ()
end

module Iter = struct
  class virtual ['a] iterbase =
    object
      method visit_prim : 'a -> Primitive.prim -> unit = fun _ -> fun _ -> ()
      method visit_constr_tag : 'a -> constr_tag -> unit = fun _ -> fun _ -> ()
      method visit_constr : 'a -> constr -> unit = fun _ -> fun _ -> ()
      method visit_label : 'a -> label -> unit = fun _ -> fun _ -> ()
      method visit_accessor : 'a -> accessor -> unit = fun _ -> fun _ -> ()
      method visit_location : 'a -> location -> unit = fun _ -> fun _ -> ()

      method visit_absolute_loc : 'a -> absolute_loc -> unit =
        fun _ -> fun _ -> ()

      method visit_binder : 'a -> binder -> unit = fun _ -> fun _ -> ()
      method visit_var : 'a -> var -> unit = fun _ -> fun _ -> ()
      method visit_loop_label : 'a -> loop_label -> unit = fun _ -> fun _ -> ()
      method visit_typ : 'a -> typ -> unit = fun _ -> fun _ -> ()
      method visit_func_stubs : 'a -> func_stubs -> unit = fun _ -> fun _ -> ()

      method visit_return_kind : 'a -> return_kind -> unit =
        fun _ -> fun _ -> ()

      method private visit_object_key : 'a -> Object_util.object_key -> unit =
        fun _ -> fun _ -> ()
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] iter =
      object (self : 'self)
        inherit [_] iterbase

        method visit_Ctop_expr : _ -> expr -> absolute_loc -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_absolute_loc env _visitors_floc_ in
              ()

        method visit_Ctop_let :
            _ -> binder -> expr -> bool -> absolute_loc -> unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pub_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fis_pub_
                in
                let _visitors_r3 =
                  self#visit_absolute_loc env _visitors_floc_
                in
                ()

        method visit_Ctop_fn : _ -> top_fun_decl -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_top_fun_decl env _visitors_c0 in
             ()

        method visit_Ctop_stub :
            _ ->
            binder ->
            func_stubs ->
            typ list ->
            typ option ->
            string option ->
            absolute_loc ->
            unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fexport_info_ ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_func_stubs env _visitors_ffunc_stubs
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
                      _visitors_fparams_ty
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_typ env) t
                      | None -> ())
                      _visitors_freturn_ty
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (fun _visitors_this -> ()) t
                      | None -> ())
                      _visitors_fexport_info_
                  in
                  let _visitors_r5 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  ()

        method visit_top_item : _ -> top_item -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_floc_
             | Ctop_let
                 {
                   binder = _visitors_fbinder;
                   expr = _visitors_fexpr;
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_let env _visitors_fbinder _visitors_fexpr
                   _visitors_fis_pub_ _visitors_floc_
             | Ctop_fn _visitors_c0 -> self#visit_Ctop_fn env _visitors_c0
             | Ctop_stub
                 {
                   binder = _visitors_fbinder;
                   func_stubs = _visitors_ffunc_stubs;
                   params_ty = _visitors_fparams_ty;
                   return_ty = _visitors_freturn_ty;
                   export_info_ = _visitors_fexport_info_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fexport_info_ _visitors_floc_

        method visit_top_fun_decl : _ -> top_fun_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> (fun _visitors_this -> ()) t
                 | None -> ())
                 _visitors_this.export_info_
             in
             let _visitors_r3 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             ()

        method visit_To_result : _ -> unit = fun env -> ()

        method visit_Joinapply : _ -> var -> unit =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_var env _visitors_c0 in
             ()

        method visit_Return_err : _ -> typ -> unit =
          fun env ->
            fun _visitors_fok_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_fok_ty in
             ()

        method visit_handle_kind : _ -> handle_kind -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | To_result -> self#visit_To_result env
             | Joinapply _visitors_c0 -> self#visit_Joinapply env _visitors_c0
             | Return_err { ok_ty = _visitors_fok_ty } ->
                 self#visit_Return_err env _visitors_fok_ty

        method visit_Cexpr_const : _ -> constant -> typ -> location -> unit =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = (fun _visitors_this -> ()) _visitors_fc in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_Cexpr_unit : _ -> location -> unit =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             ()

        method visit_Cexpr_var :
            _ -> var -> prim option -> typ -> location -> unit =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fprim ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_prim env) t
                    | None -> ())
                    _visitors_fprim
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_prim :
            _ -> prim -> expr list -> typ -> location -> unit =
          fun env ->
            fun _visitors_fprim ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_prim env _visitors_fprim in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_let :
            _ -> binder -> expr -> expr -> typ -> location -> unit =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_frhs ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fname in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 = self#visit_expr env _visitors_fbody in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_letfn :
            _ -> binder -> fn -> expr -> typ -> letfn_kind -> location -> unit =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffn ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_fkind ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 = self#visit_fn env _visitors_ffn in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 = self#visit_typ env _visitors_fty in
                  let _visitors_r4 =
                    (fun _visitors_this -> ()) _visitors_fkind
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  ()

        method visit_Cexpr_function : _ -> fn -> typ -> bool -> location -> unit
            =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fty ->
              fun _visitors_fis_raw_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_fn env _visitors_ffunc in
                let _visitors_r1 = self#visit_typ env _visitors_fty in
                let _visitors_r2 =
                  (fun _visitors_this -> ()) _visitors_fis_raw_
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            prim option ->
            typ ->
            location ->
            unit =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fprim ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_var env _visitors_ffunc in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                      _visitors_fargs
                  in
                  let _visitors_r2 =
                    self#visit_apply_kind env _visitors_fkind
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> (self#visit_prim env) t
                      | None -> ())
                      _visitors_fprim
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  ()

        method visit_Cexpr_object :
            _ -> Object_util.object_key -> expr -> typ -> location -> unit =
          fun env ->
            fun _visitors_fmethods_key ->
             fun _visitors_fself ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_object_key env _visitors_fmethods_key
                in
                let _visitors_r1 = self#visit_expr env _visitors_fself in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_letrec :
            _ -> (binder * fn) list -> expr -> typ -> location -> unit =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this
                      ~f:(fun (_visitors_c0, _visitors_c1) ->
                        let _visitors_r0 = self#visit_binder env _visitors_c0 in
                        let _visitors_r1 = self#visit_fn env _visitors_c1 in
                        ()))
                    _visitors_fbindings
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_constr :
            _ -> constr_tag -> expr list -> typ -> location -> unit =
          fun env ->
            fun _visitors_ftag ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_tuple : _ -> expr list -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_Cexpr_record :
            _ -> field_def list -> typ -> location -> unit =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                   _visitors_ffields
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_Cexpr_record_update :
            _ -> expr -> field_def list -> int -> typ -> location -> unit =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_ffields ->
              fun _visitors_ffields_num ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this ~f:(self#visit_field_def env))
                     _visitors_ffields
                 in
                 let _visitors_r2 =
                   (fun _visitors_this -> ()) _visitors_ffields_num
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_field :
            _ -> expr -> accessor -> int -> typ -> location -> unit =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_fpos ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 = (fun _visitors_this -> ()) _visitors_fpos in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_mutate :
            _ -> expr -> label -> expr -> int -> typ -> location -> unit =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_flabel ->
              fun _visitors_ffield ->
               fun _visitors_fpos ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_frecord in
                  let _visitors_r1 = self#visit_label env _visitors_flabel in
                  let _visitors_r2 = self#visit_expr env _visitors_ffield in
                  let _visitors_r3 =
                    (fun _visitors_this -> ()) _visitors_fpos
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  ()

        method visit_Cexpr_array : _ -> expr list -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_Cexpr_assign : _ -> var -> expr -> typ -> location -> unit
            =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_sequence :
            _ -> expr list -> expr -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                    _visitors_fexprs
                in
                let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_if :
            _ -> expr -> expr -> expr option -> typ -> location -> unit =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fcond in
                 let _visitors_r1 = self#visit_expr env _visitors_fifso in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_fifnot
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_switch_constr :
            _ ->
            expr ->
            (constr_tag * binder option * expr) list ->
            expr option ->
            typ ->
            location ->
            unit =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this
                       ~f:(fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                         let _visitors_r0 =
                           self#visit_constr_tag env _visitors_c0
                         in
                         let _visitors_r1 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t -> (self#visit_binder env) t
                             | None -> ())
                             _visitors_c1
                         in
                         let _visitors_r2 = self#visit_expr env _visitors_c2 in
                         ()))
                     _visitors_fcases
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_expr env) t
                     | None -> ())
                     _visitors_fdefault
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_switch_constant :
            _ ->
            expr ->
            (constant * expr) list ->
            expr ->
            typ ->
            location ->
            unit =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.iter _visitors_this
                       ~f:(fun (_visitors_c0, _visitors_c1) ->
                         let _visitors_r0 =
                           (fun _visitors_this -> ()) _visitors_c0
                         in
                         let _visitors_r1 = self#visit_expr env _visitors_c1 in
                         ()))
                     _visitors_fcases
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fdefault in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_loop :
            _ ->
            param list ->
            expr ->
            expr list ->
            loop_label ->
            typ ->
            location ->
            unit =
          fun env ->
            fun _visitors_fparams ->
             fun _visitors_fbody ->
              fun _visitors_fargs ->
               fun _visitors_flabel ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_param env))
                      _visitors_fparams
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fbody in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                      _visitors_fargs
                  in
                  let _visitors_r3 =
                    self#visit_loop_label env _visitors_flabel
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  ()

        method visit_Cexpr_break :
            _ -> expr option -> loop_label -> typ -> location -> unit =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> (self#visit_expr env) t
                    | None -> ())
                    _visitors_farg
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_continue :
            _ -> expr list -> loop_label -> typ -> location -> unit =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.iter _visitors_this ~f:(self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_handle_error :
            _ -> expr -> handle_kind -> typ -> location -> unit =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fhandle_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fobj in
                let _visitors_r1 =
                  self#visit_handle_kind env _visitors_fhandle_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_return :
            _ -> expr -> return_kind -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_freturn_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  self#visit_return_kind env _visitors_freturn_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                ()

        method visit_Cexpr_and : _ -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_Cexpr_or : _ -> expr -> expr -> location -> unit =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               ()

        method visit_expr : _ -> expr -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Cexpr_const
                 {
                   c = _visitors_fc;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_const env _visitors_fc _visitors_fty
                   _visitors_floc_
             | Cexpr_unit { loc_ = _visitors_floc_ } ->
                 self#visit_Cexpr_unit env _visitors_floc_
             | Cexpr_var
                 {
                   id = _visitors_fid;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fprim
                   _visitors_fty _visitors_floc_
             | Cexpr_prim
                 {
                   prim = _visitors_fprim;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_prim env _visitors_fprim _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_let
                 {
                   name = _visitors_fname;
                   rhs = _visitors_frhs;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_let env _visitors_fname _visitors_frhs
                   _visitors_fbody _visitors_fty _visitors_floc_
             | Cexpr_letfn
                 {
                   name = _visitors_fname;
                   fn = _visitors_ffn;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   kind = _visitors_fkind;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letfn env _visitors_fname _visitors_ffn
                   _visitors_fbody _visitors_fty _visitors_fkind _visitors_floc_
             | Cexpr_function
                 {
                   func = _visitors_ffunc;
                   ty = _visitors_fty;
                   is_raw_ = _visitors_fis_raw_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_function env _visitors_ffunc _visitors_fty
                   _visitors_fis_raw_ _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fprim _visitors_fty _visitors_floc_
             | Cexpr_object
                 {
                   methods_key = _visitors_fmethods_key;
                   self = _visitors_fself;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_object env _visitors_fmethods_key
                   _visitors_fself _visitors_fty _visitors_floc_
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_constr
                 {
                   tag = _visitors_ftag;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_constr env _visitors_ftag _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_tuple
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_tuple env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_record
                 {
                   fields = _visitors_ffields;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record env _visitors_ffields _visitors_fty
                   _visitors_floc_
             | Cexpr_record_update
                 {
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   fields_num = _visitors_ffields_num;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record_update env _visitors_frecord
                   _visitors_ffields _visitors_ffields_num _visitors_fty
                   _visitors_floc_
             | Cexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_fpos _visitors_fty
                   _visitors_floc_
             | Cexpr_mutate
                 {
                   record = _visitors_frecord;
                   label = _visitors_flabel;
                   field = _visitors_ffield;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_mutate env _visitors_frecord _visitors_flabel
                   _visitors_ffield _visitors_fpos _visitors_fty _visitors_floc_
             | Cexpr_array
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_array env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_fty _visitors_floc_
             | Cexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_fty _visitors_floc_
             | Cexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_fty _visitors_floc_
             | Cexpr_switch_constr
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constr env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_switch_constant
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constant env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_loop
                 {
                   params = _visitors_fparams;
                   body = _visitors_fbody;
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_loop env _visitors_fparams _visitors_fbody
                   _visitors_fargs _visitors_flabel _visitors_fty
                   _visitors_floc_
             | Cexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_break env _visitors_farg _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_handle_error
                 {
                   obj = _visitors_fobj;
                   handle_kind = _visitors_fhandle_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_handle_error env _visitors_fobj
                   _visitors_fhandle_kind _visitors_fty _visitors_floc_
             | Cexpr_return
                 {
                   expr = _visitors_fexpr;
                   return_kind = _visitors_freturn_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_return env _visitors_fexpr
                   _visitors_freturn_kind _visitors_fty _visitors_floc_
             | Cexpr_and
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_and env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Cexpr_or
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_or env _visitors_flhs _visitors_frhs
                   _visitors_floc_

        method visit_fn : _ -> fn -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this ~f:(self#visit_param env))
                 _visitors_this.params
             in
             let _visitors_r1 = self#visit_expr env _visitors_this.body in
             ()

        method visit_param : _ -> param -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_typ env _visitors_this.ty in
             let _visitors_r2 = self#visit_location env _visitors_this.loc_ in
             ()

        method visit_Normal : _ -> typ -> unit =
          fun env ->
            fun _visitors_ffunc_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_ffunc_ty in
             ()

        method visit_Join : _ -> unit = fun env -> ()

        method visit_apply_kind : _ -> apply_kind -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Normal { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Normal env _visitors_ffunc_ty
             | Join -> self#visit_Join env

        method visit_field_def : _ -> field_def -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_label env _visitors_this.label in
             let _visitors_r1 = (fun _visitors_this -> ()) _visitors_this.pos in
             let _visitors_r2 =
               (fun _visitors_this -> ()) _visitors_this.is_mut
             in
             let _visitors_r3 = self#visit_expr env _visitors_this.expr in
             ()
      end

    [@@@VISITORS.END]
  end

  include struct
    let _ = fun (_ : _unused) -> ()
  end
end

open struct
  class virtual ['a] sexpbase =
    object
      inherit [_] Sexp_visitors.sexp

      method visit_constant : 'a -> constant -> S.t =
        fun _ -> fun x -> sexp_of_constant x

      method visit_prim : 'a -> prim -> S.t = fun _ -> fun x -> sexp_of_prim x

      method visit_constr_tag : 'a -> constr_tag -> S.t =
        fun _ -> fun x -> sexp_of_constr_tag x

      method visit_constr : 'a -> constr -> S.t =
        fun _ -> fun x -> sexp_of_constr x

      method visit_label : 'a -> label -> S.t =
        fun _ -> fun x -> sexp_of_label x

      method visit_accessor : 'a -> accessor -> S.t =
        fun _ -> fun x -> sexp_of_accessor x

      method visit_location : 'a -> location -> S.t =
        fun _ -> fun x -> sexp_of_location x

      method visit_absolute_loc : 'a -> absolute_loc -> S.t =
        fun _ -> fun x -> sexp_of_absolute_loc x

      method visit_binder : 'a -> binder -> S.t =
        fun _ -> fun x -> sexp_of_binder x

      method visit_var : 'a -> var -> S.t = fun _ -> fun x -> sexp_of_var x

      method visit_loop_label : 'a -> loop_label -> S.t =
        fun _ -> fun x -> sexp_of_loop_label x

      method visit_typ : 'a -> typ -> S.t = fun _ -> fun x -> sexp_of_typ x

      method visit_func_stubs : 'a -> func_stubs -> S.t =
        fun _ -> fun x -> sexp_of_func_stubs x

      method visit_letfn_kind : 'a -> letfn_kind -> S.t =
        fun _ -> fun x -> sexp_of_letfn_kind x

      method visit_return_kind : 'a -> return_kind -> S.t =
        fun _ -> fun x -> sexp_of_return_kind x

      method private visit_test_name : 'a -> Syntax.test_name -> S.t =
        fun _ -> fun x -> Syntax.sexp_of_test_name x

      method private visit_object_key : 'a -> Object_util.object_key -> S.t =
        fun _ -> fun x -> Object_util.sexp_of_object_key x
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] sexp =
      object (self : 'self)
        inherit [_] sexpbase

        method visit_Ctop_expr : _ -> expr -> absolute_loc -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_absolute_loc env _visitors_floc_ in
              self#visit_inline_record env "Ctop_expr"
                [ ("expr", _visitors_r0); ("loc_", _visitors_r1) ]

        method visit_Ctop_let :
            _ -> binder -> expr -> bool -> absolute_loc -> S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pub_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_bool env _visitors_fis_pub_ in
                let _visitors_r3 =
                  self#visit_absolute_loc env _visitors_floc_
                in
                self#visit_inline_record env "Ctop_let"
                  [
                    ("binder", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("is_pub_", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Ctop_fn : _ -> top_fun_decl -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_top_fun_decl env _visitors_c0 in
             self#visit_inline_tuple env "Ctop_fn" [ _visitors_r0 ]

        method visit_Ctop_stub :
            _ ->
            binder ->
            func_stubs ->
            typ list ->
            typ option ->
            string option ->
            absolute_loc ->
            S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fexport_info_ ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_func_stubs env _visitors_ffunc_stubs
                  in
                  let _visitors_r2 =
                    self#visit_list self#visit_typ env _visitors_fparams_ty
                  in
                  let _visitors_r3 =
                    self#visit_option self#visit_typ env _visitors_freturn_ty
                  in
                  let _visitors_r4 =
                    self#visit_option self#visit_string env
                      _visitors_fexport_info_
                  in
                  let _visitors_r5 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  self#visit_inline_record env "Ctop_stub"
                    [
                      ("binder", _visitors_r0);
                      ("func_stubs", _visitors_r1);
                      ("params_ty", _visitors_r2);
                      ("return_ty", _visitors_r3);
                      ("export_info_", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_top_item : _ -> top_item -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_floc_
             | Ctop_let
                 {
                   binder = _visitors_fbinder;
                   expr = _visitors_fexpr;
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_let env _visitors_fbinder _visitors_fexpr
                   _visitors_fis_pub_ _visitors_floc_
             | Ctop_fn _visitors_c0 -> self#visit_Ctop_fn env _visitors_c0
             | Ctop_stub
                 {
                   binder = _visitors_fbinder;
                   func_stubs = _visitors_ffunc_stubs;
                   params_ty = _visitors_fparams_ty;
                   return_ty = _visitors_freturn_ty;
                   export_info_ = _visitors_fexport_info_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fexport_info_ _visitors_floc_

        method visit_top_fun_decl : _ -> top_fun_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               self#visit_option self#visit_string env
                 _visitors_this.export_info_
             in
             let _visitors_r3 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             self#visit_record env
               [
                 ("binder", _visitors_r0);
                 ("func", _visitors_r1);
                 ("export_info_", _visitors_r2);
                 ("loc_", _visitors_r3);
               ]

        method visit_To_result : _ -> S.t =
          fun env -> self#visit_inline_tuple env "To_result" []

        method visit_Joinapply : _ -> var -> S.t =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_var env _visitors_c0 in
             self#visit_inline_tuple env "Joinapply" [ _visitors_r0 ]

        method visit_Return_err : _ -> typ -> S.t =
          fun env ->
            fun _visitors_fok_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_fok_ty in
             self#visit_inline_record env "Return_err"
               [ ("ok_ty", _visitors_r0) ]

        method visit_handle_kind : _ -> handle_kind -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | To_result -> self#visit_To_result env
             | Joinapply _visitors_c0 -> self#visit_Joinapply env _visitors_c0
             | Return_err { ok_ty = _visitors_fok_ty } ->
                 self#visit_Return_err env _visitors_fok_ty

        method visit_Cexpr_const : _ -> constant -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_constant env _visitors_fc in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_const"
                 [
                   ("c", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Cexpr_unit : _ -> location -> S.t =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             self#visit_inline_record env "Cexpr_unit"
               [ ("loc_", _visitors_r0) ]

        method visit_Cexpr_var :
            _ -> var -> prim option -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fprim ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  self#visit_option self#visit_prim env _visitors_fprim
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_var"
                  [
                    ("id", _visitors_r0);
                    ("prim", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_prim :
            _ -> prim -> expr list -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fprim ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_prim env _visitors_fprim in
                let _visitors_r1 =
                  self#visit_list self#visit_expr env _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_prim"
                  [
                    ("prim", _visitors_r0);
                    ("args", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_let :
            _ -> binder -> expr -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_frhs ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fname in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 = self#visit_expr env _visitors_fbody in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_let"
                   [
                     ("name", _visitors_r0);
                     ("rhs", _visitors_r1);
                     ("body", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_letfn :
            _ -> binder -> fn -> expr -> typ -> letfn_kind -> location -> S.t =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffn ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_fkind ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 = self#visit_fn env _visitors_ffn in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 = self#visit_typ env _visitors_fty in
                  let _visitors_r4 =
                    self#visit_letfn_kind env _visitors_fkind
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Cexpr_letfn"
                    [
                      ("name", _visitors_r0);
                      ("fn", _visitors_r1);
                      ("body", _visitors_r2);
                      ("ty", _visitors_r3);
                      ("kind", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Cexpr_function : _ -> fn -> typ -> bool -> location -> S.t
            =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fty ->
              fun _visitors_fis_raw_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_fn env _visitors_ffunc in
                let _visitors_r1 = self#visit_typ env _visitors_fty in
                let _visitors_r2 = self#visit_bool env _visitors_fis_raw_ in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_function"
                  [
                    ("func", _visitors_r0);
                    ("ty", _visitors_r1);
                    ("is_raw_", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            prim option ->
            typ ->
            location ->
            S.t =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fprim ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_var env _visitors_ffunc in
                  let _visitors_r1 =
                    self#visit_list self#visit_expr env _visitors_fargs
                  in
                  let _visitors_r2 =
                    self#visit_apply_kind env _visitors_fkind
                  in
                  let _visitors_r3 =
                    self#visit_option self#visit_prim env _visitors_fprim
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Cexpr_apply"
                    [
                      ("func", _visitors_r0);
                      ("args", _visitors_r1);
                      ("kind", _visitors_r2);
                      ("prim", _visitors_r3);
                      ("ty", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Cexpr_object :
            _ -> Object_util.object_key -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fmethods_key ->
             fun _visitors_fself ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_object_key env _visitors_fmethods_key
                in
                let _visitors_r1 = self#visit_expr env _visitors_fself in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_object"
                  [
                    ("methods_key", _visitors_r0);
                    ("self", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_letrec :
            _ -> (binder * fn) list -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_list
                    (fun env ->
                      fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_binder env _visitors_c0 in
                       let _visitors_r1 = self#visit_fn env _visitors_c1 in
                       self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                    env _visitors_fbindings
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_letrec"
                  [
                    ("bindings", _visitors_r0);
                    ("body", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_constr :
            _ -> constr_tag -> expr list -> typ -> location -> S.t =
          fun env ->
            fun _visitors_ftag ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r1 =
                  self#visit_list self#visit_expr env _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_constr"
                  [
                    ("tag", _visitors_r0);
                    ("args", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_tuple : _ -> expr list -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_expr env _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_tuple"
                 [
                   ("exprs", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Cexpr_record :
            _ -> field_def list -> typ -> location -> S.t =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_field_def env _visitors_ffields
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_record"
                 [
                   ("fields", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Cexpr_record_update :
            _ -> expr -> field_def list -> int -> typ -> location -> S.t =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_ffields ->
              fun _visitors_ffields_num ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_list self#visit_field_def env _visitors_ffields
                 in
                 let _visitors_r2 = self#visit_int env _visitors_ffields_num in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_record_update"
                   [
                     ("record", _visitors_r0);
                     ("fields", _visitors_r1);
                     ("fields_num", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_field :
            _ -> expr -> accessor -> int -> typ -> location -> S.t =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_fpos ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 = self#visit_int env _visitors_fpos in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_field"
                   [
                     ("record", _visitors_r0);
                     ("accessor", _visitors_r1);
                     ("pos", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_mutate :
            _ -> expr -> label -> expr -> int -> typ -> location -> S.t =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_flabel ->
              fun _visitors_ffield ->
               fun _visitors_fpos ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_frecord in
                  let _visitors_r1 = self#visit_label env _visitors_flabel in
                  let _visitors_r2 = self#visit_expr env _visitors_ffield in
                  let _visitors_r3 = self#visit_int env _visitors_fpos in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Cexpr_mutate"
                    [
                      ("record", _visitors_r0);
                      ("label", _visitors_r1);
                      ("field", _visitors_r2);
                      ("pos", _visitors_r3);
                      ("ty", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Cexpr_array : _ -> expr list -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 self#visit_list self#visit_expr env _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_array"
                 [
                   ("exprs", _visitors_r0);
                   ("ty", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Cexpr_assign : _ -> var -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_assign"
                  [
                    ("var", _visitors_r0);
                    ("expr", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_sequence :
            _ -> expr list -> expr -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_list self#visit_expr env _visitors_fexprs
                in
                let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_sequence"
                  [
                    ("exprs", _visitors_r0);
                    ("last_expr", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_if :
            _ -> expr -> expr -> expr option -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fcond in
                 let _visitors_r1 = self#visit_expr env _visitors_fifso in
                 let _visitors_r2 =
                   self#visit_option self#visit_expr env _visitors_fifnot
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_if"
                   [
                     ("cond", _visitors_r0);
                     ("ifso", _visitors_r1);
                     ("ifnot", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_switch_constr :
            _ ->
            expr ->
            (constr_tag * binder option * expr) list ->
            expr option ->
            typ ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   self#visit_list
                     (fun env ->
                       fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                        let _visitors_r0 =
                          self#visit_constr_tag env _visitors_c0
                        in
                        let _visitors_r1 =
                          self#visit_option self#visit_binder env _visitors_c1
                        in
                        let _visitors_r2 = self#visit_expr env _visitors_c2 in
                        self#visit_tuple env
                          [ _visitors_r0; _visitors_r1; _visitors_r2 ])
                     env _visitors_fcases
                 in
                 let _visitors_r2 =
                   self#visit_option self#visit_expr env _visitors_fdefault
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_switch_constr"
                   [
                     ("obj", _visitors_r0);
                     ("cases", _visitors_r1);
                     ("default", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_switch_constant :
            _ ->
            expr ->
            (constant * expr) list ->
            expr ->
            typ ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   self#visit_list
                     (fun env ->
                       fun (_visitors_c0, _visitors_c1) ->
                        let _visitors_r0 =
                          self#visit_constant env _visitors_c0
                        in
                        let _visitors_r1 = self#visit_expr env _visitors_c1 in
                        self#visit_tuple env [ _visitors_r0; _visitors_r1 ])
                     env _visitors_fcases
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fdefault in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_switch_constant"
                   [
                     ("obj", _visitors_r0);
                     ("cases", _visitors_r1);
                     ("default", _visitors_r2);
                     ("ty", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_loop :
            _ ->
            param list ->
            expr ->
            expr list ->
            loop_label ->
            typ ->
            location ->
            S.t =
          fun env ->
            fun _visitors_fparams ->
             fun _visitors_fbody ->
              fun _visitors_fargs ->
               fun _visitors_flabel ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    self#visit_list self#visit_param env _visitors_fparams
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fbody in
                  let _visitors_r2 =
                    self#visit_list self#visit_expr env _visitors_fargs
                  in
                  let _visitors_r3 =
                    self#visit_loop_label env _visitors_flabel
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  self#visit_inline_record env "Cexpr_loop"
                    [
                      ("params", _visitors_r0);
                      ("body", _visitors_r1);
                      ("args", _visitors_r2);
                      ("label", _visitors_r3);
                      ("ty", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_Cexpr_break :
            _ -> expr option -> loop_label -> typ -> location -> S.t =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_option self#visit_expr env _visitors_farg
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_break"
                  [
                    ("arg", _visitors_r0);
                    ("label", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_continue :
            _ -> expr list -> loop_label -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_list self#visit_expr env _visitors_fargs
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_continue"
                  [
                    ("args", _visitors_r0);
                    ("label", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_handle_error :
            _ -> expr -> handle_kind -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fhandle_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fobj in
                let _visitors_r1 =
                  self#visit_handle_kind env _visitors_fhandle_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_handle_error"
                  [
                    ("obj", _visitors_r0);
                    ("handle_kind", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_return :
            _ -> expr -> return_kind -> typ -> location -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_freturn_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  self#visit_return_kind env _visitors_freturn_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_return"
                  [
                    ("expr", _visitors_r0);
                    ("return_kind", _visitors_r1);
                    ("ty", _visitors_r2);
                    ("loc_", _visitors_r3);
                  ]

        method visit_Cexpr_and : _ -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_and"
                 [
                   ("lhs", _visitors_r0);
                   ("rhs", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_Cexpr_or : _ -> expr -> expr -> location -> S.t =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               self#visit_inline_record env "Cexpr_or"
                 [
                   ("lhs", _visitors_r0);
                   ("rhs", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

        method visit_expr : _ -> expr -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Cexpr_const
                 {
                   c = _visitors_fc;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_const env _visitors_fc _visitors_fty
                   _visitors_floc_
             | Cexpr_unit { loc_ = _visitors_floc_ } ->
                 self#visit_Cexpr_unit env _visitors_floc_
             | Cexpr_var
                 {
                   id = _visitors_fid;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fprim
                   _visitors_fty _visitors_floc_
             | Cexpr_prim
                 {
                   prim = _visitors_fprim;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_prim env _visitors_fprim _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_let
                 {
                   name = _visitors_fname;
                   rhs = _visitors_frhs;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_let env _visitors_fname _visitors_frhs
                   _visitors_fbody _visitors_fty _visitors_floc_
             | Cexpr_letfn
                 {
                   name = _visitors_fname;
                   fn = _visitors_ffn;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   kind = _visitors_fkind;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letfn env _visitors_fname _visitors_ffn
                   _visitors_fbody _visitors_fty _visitors_fkind _visitors_floc_
             | Cexpr_function
                 {
                   func = _visitors_ffunc;
                   ty = _visitors_fty;
                   is_raw_ = _visitors_fis_raw_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_function env _visitors_ffunc _visitors_fty
                   _visitors_fis_raw_ _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fprim _visitors_fty _visitors_floc_
             | Cexpr_object
                 {
                   methods_key = _visitors_fmethods_key;
                   self = _visitors_fself;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_object env _visitors_fmethods_key
                   _visitors_fself _visitors_fty _visitors_floc_
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_constr
                 {
                   tag = _visitors_ftag;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_constr env _visitors_ftag _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_tuple
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_tuple env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_record
                 {
                   fields = _visitors_ffields;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record env _visitors_ffields _visitors_fty
                   _visitors_floc_
             | Cexpr_record_update
                 {
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   fields_num = _visitors_ffields_num;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record_update env _visitors_frecord
                   _visitors_ffields _visitors_ffields_num _visitors_fty
                   _visitors_floc_
             | Cexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_fpos _visitors_fty
                   _visitors_floc_
             | Cexpr_mutate
                 {
                   record = _visitors_frecord;
                   label = _visitors_flabel;
                   field = _visitors_ffield;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_mutate env _visitors_frecord _visitors_flabel
                   _visitors_ffield _visitors_fpos _visitors_fty _visitors_floc_
             | Cexpr_array
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_array env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_fty _visitors_floc_
             | Cexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_fty _visitors_floc_
             | Cexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_fty _visitors_floc_
             | Cexpr_switch_constr
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constr env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_switch_constant
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constant env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_loop
                 {
                   params = _visitors_fparams;
                   body = _visitors_fbody;
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_loop env _visitors_fparams _visitors_fbody
                   _visitors_fargs _visitors_flabel _visitors_fty
                   _visitors_floc_
             | Cexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_break env _visitors_farg _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_handle_error
                 {
                   obj = _visitors_fobj;
                   handle_kind = _visitors_fhandle_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_handle_error env _visitors_fobj
                   _visitors_fhandle_kind _visitors_fty _visitors_floc_
             | Cexpr_return
                 {
                   expr = _visitors_fexpr;
                   return_kind = _visitors_freturn_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_return env _visitors_fexpr
                   _visitors_freturn_kind _visitors_fty _visitors_floc_
             | Cexpr_and
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_and env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Cexpr_or
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_or env _visitors_flhs _visitors_frhs
                   _visitors_floc_

        method visit_fn : _ -> fn -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               self#visit_list self#visit_param env _visitors_this.params
             in
             let _visitors_r1 = self#visit_expr env _visitors_this.body in
             self#visit_record env
               [ ("params", _visitors_r0); ("body", _visitors_r1) ]

        method visit_param : _ -> param -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_typ env _visitors_this.ty in
             let _visitors_r2 = self#visit_location env _visitors_this.loc_ in
             self#visit_record env
               [
                 ("binder", _visitors_r0);
                 ("ty", _visitors_r1);
                 ("loc_", _visitors_r2);
               ]

        method visit_Normal : _ -> typ -> S.t =
          fun env ->
            fun _visitors_ffunc_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_ffunc_ty in
             self#visit_inline_record env "Normal" [ ("func_ty", _visitors_r0) ]

        method visit_Join : _ -> S.t =
          fun env -> self#visit_inline_tuple env "Join" []

        method visit_apply_kind : _ -> apply_kind -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Normal { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Normal env _visitors_ffunc_ty
             | Join -> self#visit_Join env

        method visit_field_def : _ -> field_def -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_label env _visitors_this.label in
             let _visitors_r1 = self#visit_int env _visitors_this.pos in
             let _visitors_r2 = self#visit_bool env _visitors_this.is_mut in
             let _visitors_r3 = self#visit_expr env _visitors_this.expr in
             self#visit_record env
               [
                 ("label", _visitors_r0);
                 ("pos", _visitors_r1);
                 ("is_mut", _visitors_r2);
                 ("expr", _visitors_r3);
               ]
      end

    [@@@VISITORS.END]
  end

  include struct
    let _ = fun (_ : _unused) -> ()
  end

  let predicate field =
    match field with
    | "loc_", _ when not !Basic_config.show_loc -> None
    | "loc_", S.Atom "0:0-0:0" -> None
    | "continue_block", S.List [] -> None
    | "prim", S.List [] -> None
    | "is_handle_error", S.Atom "false" -> None
    | "is_raw_", Atom "false" -> None
    | "is_raw_", Atom "true" -> Some ("is_raw_", S.List [ Atom "raw" ])
    | _ -> Some field

  let compose f g x = match g x with None -> None | Some x -> f x
  [@@warning "-unused-value-declaration"]

  type loc_ctx = Use_absolute_loc of absolute_loc | Use_relative_loc

  let sexp_visitor =
    object (self)
      inherit [_] sexp as super
      method! visit_Cexpr_const env c _ty _loc_ = self#visit_constant env c

      method! visit_inline_record env ctor fields =
        let predicate = match ctor with _ -> predicate in
        super#visit_inline_record env ctor (Lst.filter_map fields predicate)

      method! visit_Cexpr_prim env prim args _ty loc_ =
        let args = Lst.map args (fun arg -> self#visit_expr env arg) in
        let prim = self#visit_prim env prim in
        let loc_ =
          if !Basic_config.show_loc then [ self#visit_location env loc_ ]
          else []
        in
        (List
           (List.cons
              (prim : S.t)
              (List.append (args : S.t list) (loc_ : S.t list)))
          : S.t)

      method! visit_record env fields =
        super#visit_record env (Lst.filter_map fields predicate)

      method! visit_Cexpr_apply env func args kind prim ty loc_ =
        match kind with
        | Join ->
            let func = self#visit_var env func in
            let args = self#visit_list self#visit_expr env args in
            let ty = self#visit_typ env ty in
            let loc_ =
              if !Basic_config.show_loc then [ self#visit_location env loc_ ]
              else []
            in
            (List
               (List.cons
                  (Atom "Cexpr_join_apply" : S.t)
                  (List.cons
                     (func : S.t)
                     (List.cons
                        (args : S.t)
                        (List.cons (ty : S.t) (loc_ : S.t list)))))
              : S.t)
        | Normal _ -> super#visit_Cexpr_apply env func args kind prim ty loc_

      method! visit_Cexpr_letfn env name fn body ty letfn_kind loc_ =
        let name = self#visit_binder env name in
        let params = self#visit_list self#visit_param env fn.params in
        let join_body = self#visit_expr env fn.body in
        let body = self#visit_expr env body in
        let ty = self#visit_typ env ty in
        let loc_ =
          if !Basic_config.show_loc then [ self#visit_location env loc_ ]
          else []
        in
        match letfn_kind with
        | Rec ->
            (List
               (List.cons
                  (Atom "Cexpr_letfnrec" : S.t)
                  (List.cons
                     (name : S.t)
                     (List.cons
                        (params : S.t)
                        (List.cons
                           (join_body : S.t)
                           (List.cons
                              (body : S.t)
                              (List.cons (ty : S.t) (loc_ : S.t list)))))))
              : S.t)
        | Nonrec ->
            (List
               (List.cons
                  (Atom "Cexpr_letfn" : S.t)
                  (List.cons
                     (name : S.t)
                     (List.cons
                        (params : S.t)
                        (List.cons
                           (join_body : S.t)
                           (List.cons
                              (body : S.t)
                              (List.cons (ty : S.t) (loc_ : S.t list)))))))
              : S.t)
        | Tail_join ->
            (List
               (List.cons
                  (Atom "Cexpr_joinlet" : S.t)
                  (List.cons
                     (name : S.t)
                     (List.cons
                        (params : S.t)
                        (List.cons
                           (join_body : S.t)
                           (List.cons
                              (body : S.t)
                              (List.cons (ty : S.t) (loc_ : S.t list)))))))
              : S.t)
        | Nontail_join ->
            (List
               (List.cons
                  (Atom "Cexpr_joinlet_nontail" : S.t)
                  (List.cons
                     (name : S.t)
                     (List.cons
                        (params : S.t)
                        (List.cons
                           (join_body : S.t)
                           (List.cons
                              (body : S.t)
                              (List.cons (ty : S.t) (loc_ : S.t list)))))))
              : S.t)

      method! visit_location env loc =
        match env with
        | Use_absolute_loc base -> sexp_of_absolute_loc (Rloc.to_loc ~base loc)
        | Use_relative_loc -> super#visit_location env loc
    end
end

let type_of_expr = function
  | Cexpr_const { ty; _ }
  | Cexpr_var { ty; _ }
  | Cexpr_prim { ty; _ }
  | Cexpr_let { ty; _ }
  | Cexpr_letfn { ty; _ }
  | Cexpr_function { ty; _ }
  | Cexpr_apply { ty; _ }
  | Cexpr_object { ty; _ }
  | Cexpr_letrec { ty; _ }
  | Cexpr_constr { ty; _ }
  | Cexpr_tuple { ty; _ }
  | Cexpr_record { ty; _ }
  | Cexpr_record_update { ty; _ }
  | Cexpr_field { ty; _ }
  | Cexpr_mutate { ty; _ }
  | Cexpr_array { ty; _ }
  | Cexpr_assign { ty; _ }
  | Cexpr_sequence { ty; _ }
  | Cexpr_if { ty; _ }
  | Cexpr_switch_constr { ty; _ }
  | Cexpr_switch_constant { ty; _ }
  | Cexpr_break { ty; _ }
  | Cexpr_continue { ty; _ }
  | Cexpr_loop { ty; _ }
  | Cexpr_return { ty; _ }
  | Cexpr_handle_error { ty; _ } ->
      ty
  | Cexpr_and _ | Cexpr_or _ -> Mtype.T_bool
  | Cexpr_unit _ -> Mtype.T_unit

let loc_of_expr = function
  | Cexpr_const { loc_; _ }
  | Cexpr_unit { loc_ }
  | Cexpr_var { loc_; _ }
  | Cexpr_prim { loc_; _ }
  | Cexpr_let { loc_; _ }
  | Cexpr_letfn { loc_; _ }
  | Cexpr_function { loc_; _ }
  | Cexpr_apply { loc_; _ }
  | Cexpr_object { loc_; _ }
  | Cexpr_letrec { loc_; _ }
  | Cexpr_constr { loc_; _ }
  | Cexpr_tuple { loc_; _ }
  | Cexpr_record { loc_; _ }
  | Cexpr_record_update { loc_; _ }
  | Cexpr_field { loc_; _ }
  | Cexpr_mutate { loc_; _ }
  | Cexpr_array { loc_; _ }
  | Cexpr_assign { loc_; _ }
  | Cexpr_sequence { loc_; _ }
  | Cexpr_if { loc_; _ }
  | Cexpr_switch_constr { loc_; _ }
  | Cexpr_switch_constant { loc_; _ }
  | Cexpr_break { loc_; _ }
  | Cexpr_continue { loc_; _ }
  | Cexpr_loop { loc_; _ }
  | Cexpr_return { loc_; _ }
  | Cexpr_and { loc_; _ }
  | Cexpr_or { loc_; _ }
  | Cexpr_handle_error { loc_; _ } ->
      loc_

let ghost_loc_ = Rloc.no_location

let const ?(loc = ghost_loc_) (c : constant) =
  (let ty =
     match c with
     | C_int _ -> Mtype.T_int
     | C_byte _ -> Mtype.T_byte
     | C_int64 _ -> T_int64
     | C_uint _ -> T_uint
     | C_uint64 _ -> T_uint64
     | C_float _ -> T_float
     | C_double _ -> T_double
     | C_char _ -> T_char
     | C_bool _ -> T_bool
     | C_string _ -> T_string
     | C_bytes _ -> T_bytes
     | C_bigint _ -> assert false
   in
   Cexpr_const { c; ty; loc_ = loc }
    : expr)

let unit ?(loc = ghost_loc_) () = (Cexpr_unit { loc_ = loc } : expr)

let prim ?(loc = ghost_loc_) ~ty (prim : Primitive.prim) args =
  (match (prim, args) with
   | ( Parith { operand_type; operator },
       [ Cexpr_const { c = c1; _ }; Cexpr_const { c = c2; _ } ] ) -> (
       match Constant.eval_arith operand_type operator c1 c2 with
       | Some c -> const ~loc c
       | None -> Cexpr_prim { prim; args; ty; loc_ = loc })
   | ( Pbitwise { operand_type; operator },
       [ Cexpr_const { c = c1; _ }; Cexpr_const { c = c2; _ } ] ) -> (
       match Constant.eval_bitwise operand_type operator c1 c2 with
       | Some c -> const ~loc c
       | None -> Cexpr_prim { prim; args; ty; loc_ = loc })
   | ( Pcomparison { operand_type; operator },
       [ Cexpr_const { c = c1; _ }; Cexpr_const { c = c2; _ } ] ) -> (
       match Constant.eval_comparison operand_type operator c1 c2 with
       | Some c -> const ~loc c
       | None -> Cexpr_prim { prim; args; ty; loc_ = loc })
   | Pnot, Cexpr_const { c = C_bool b; _ } :: [] -> const ~loc (C_bool (not b))
   | Pcast { kind = Make_newtype }, arg :: [] -> arg
   | _ -> Cexpr_prim { prim; args; ty; loc_ = loc }
    : expr)

let eta_expand ~loc ~func_ty f =
  (match func_ty with
   | Mtype.T_func { params; return } as ty ->
       let params, args =
         Lst.map_split params (fun ty ->
             (let id = Ident.fresh "x" in
              ( { binder = id; ty; loc_ = ghost_loc_ },
                Cexpr_var { id; ty; loc_ = ghost_loc_; prim = None } )
               : param * expr))
       in
       let body : expr = f ~return_ty:return args in
       Cexpr_function
         { func = { params; body }; ty; is_raw_ = false; loc_ = loc }
   | T_maybe_uninit _ -> assert false
   | T_trait _ -> assert false
   | T_fixedarray _ -> assert false
   | T_optimized_option _ -> assert false
   | T_string -> assert false
   | T_float -> assert false
   | T_uint -> assert false
   | T_uint16 -> assert false
   | T_byte -> assert false
   | T_bool -> assert false
   | T_int -> assert false
   | T_raw_func _ -> assert false
   | T_char -> assert false
   | T_unit -> assert false
   | T_int16 -> assert false
   | T_int64 -> assert false
   | T_uint64 -> assert false
   | T_double -> assert false
   | T_bytes -> assert false
   | T_tuple _ -> assert false
   | T_constr _ -> assert false
   | T_any _ -> assert false
   | T_error_value_result _ -> assert false
    : expr)

let unsaturated_prim ?(loc = ghost_loc_) ~ty prim =
  eta_expand ~loc ~func_ty:ty (fun ~return_ty ->
      fun args -> Cexpr_prim { prim; args; ty = return_ty; loc_ = ghost_loc_ })

let if_ ?(loc = ghost_loc_) ~ifso ?ifnot cond =
  (match cond with
   | Cexpr_const { c = C_bool true; _ } -> ifso
   | Cexpr_const { c = C_bool false; _ } -> (
       match ifnot with Some ifnot -> ifnot | None -> unit ~loc ())
   | _ -> (
       match (ifso, ifnot) with
       | ( Cexpr_const { c = C_bool true; _ },
           Some (Cexpr_const { c = C_bool false; _ }) ) ->
           cond
       | ( Cexpr_const { c = C_bool false; _ },
           Some (Cexpr_const { c = C_bool true; _ }) ) ->
           prim ~loc ~ty:T_bool Pnot [ cond ]
       | _ ->
           let ty = type_of_expr ifso in
           Cexpr_if { cond; ifso; ifnot; ty; loc_ = loc })
    : expr)

let let_ ~loc (name : Ident.t) (rhs : expr) (body : expr) =
  (let ty = type_of_expr body in
   match (rhs, name) with
   | Cexpr_function { func; is_raw_ = false; _ }, Pident _ ->
       Cexpr_letfn { name; fn = func; kind = Nonrec; body; ty; loc_ = loc }
   | _ -> Cexpr_let { name; rhs; body; ty; loc_ = loc }
    : expr)

let switch_constr ~loc ~default obj cases =
  (match obj with
   | Cexpr_constr { tag; args; ty = _ } ->
       let rec select_case tag arg cases default =
         match cases with
         | [] -> (
             match default with Some default -> default | None -> assert false)
         | (tag', param, action) :: rest ->
             if Tag.equal tag' tag then
               match param with
               | None -> action
               | Some param -> let_ ~loc param obj action
             else select_case tag arg rest default
       in
       select_case tag args cases default
   | _ ->
       let ty =
         match cases with
         | (_, _, action0) :: _ -> type_of_expr action0
         | [] -> (
             match default with
             | Some default -> type_of_expr default
             | None -> assert false)
       in
       Cexpr_switch_constr { obj; cases; default; ty; loc_ = loc }
    : expr)

let switch_constant ~loc ~default obj cases =
  (match obj with
   | Cexpr_const { c; _ } ->
       let rec select_case c cases default =
         match cases with
         | [] -> default
         | (c', action) :: rest ->
             if Constant.equal c' c then action else select_case c rest default
       in
       select_case c cases default
   | _ ->
       let ty = type_of_expr default in
       Cexpr_switch_constant { obj; cases; default; ty; loc_ = loc }
    : expr)

let var ?(loc = ghost_loc_) ~(prim : Primitive.prim option) ~ty (id : Ident.t) =
  (Cexpr_var { id; prim; ty; loc_ = loc } : expr)

let break ~loc_ arg label ty = (Cexpr_break { arg; label; ty; loc_ } : expr)

let apply ?(loc = ghost_loc_) ~(prim : Primitive.prim option) ~ty ~kind
    (func : Ident.t) (args : expr list) =
  (Cexpr_apply { func; args; kind; prim; ty; loc_ = loc } : expr)

let make_object ?(loc = ghost_loc_) ~methods_key self ~ty =
  (Cexpr_object { self; methods_key; ty; loc_ = loc } : expr)

let bind ?(loc = ghost_loc_) (rhs : expr) (cont : Ident.t -> expr) =
  match rhs with
  | Cexpr_var { id = (Pident _ | Pdot _ | Plocal_method _) as id; _ } -> cont id
  | _ ->
      let id = Ident.fresh "bind" in
      let_ ~loc id rhs (cont id)

let field ~ty ~pos record accessor =
  (Cexpr_field { record; accessor; pos; ty; loc_ = ghost_loc_ } : expr)

let return ?(loc = ghost_loc_) expr ~return_kind ~ty =
  (Cexpr_return { expr; ty; return_kind; loc_ = loc } : expr)

let sequence ~loc (exprs : expr list) (last_expr : expr) =
  (let ty = type_of_expr last_expr in
   Cexpr_sequence { exprs; last_expr; ty; loc_ = loc }
    : expr)

let letfn ~loc ~kind name fn body =
  let ty = type_of_expr body in
  Cexpr_letfn { name; fn; kind; body; ty; loc_ = loc }

let letrec ~loc (bindings : (Ident.t * fn) list) body =
  (let ty = type_of_expr body in
   Cexpr_letrec { bindings; body; ty; loc_ = loc }
    : expr)

let fn params body = ({ params; body } : fn)

let rec tail_map (e : expr) (f : expr -> expr) =
  (let go e = tail_map e f [@@inline] in
   let go_opt e =
     match e with None -> None | Some e -> Some (go e)
       [@@inline]
   in
   match e with
   | Cexpr_let t -> let_ ~loc:t.loc_ t.name t.rhs (go t.body)
   | Cexpr_letfn t -> (
       match t.kind with
       | Tail_join | Nontail_join ->
           letfn ~loc:t.loc_ ~kind:t.kind t.name
             (fn t.fn.params (go t.fn.body))
             (go t.body)
       | Rec | Nonrec -> letfn ~loc:t.loc_ ~kind:t.kind t.name t.fn (go t.body))
   | Cexpr_letrec t -> letrec ~loc:t.loc_ t.bindings (go t.body)
   | Cexpr_sequence t -> sequence ~loc:t.loc_ t.exprs (go t.last_expr)
   | Cexpr_if t ->
       if_ ~loc:t.loc_ ~ifso:(go t.ifso) ?ifnot:(Option.map go t.ifnot) t.cond
   | Cexpr_switch_constr t ->
       let cases =
         Lst.map t.cases (fun (tag, arg, action) -> (tag, arg, go action))
       in
       let default = go_opt t.default in
       switch_constr ~loc:t.loc_ t.obj cases ~default
   | Cexpr_switch_constant t ->
       let cases = Lst.map t.cases (fun (c, action) -> (c, go action)) in
       let default = go t.default in
       switch_constant ~loc:t.loc_ ~default t.obj cases
   | Cexpr_return { expr; _ } -> go expr
   | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ | Cexpr_prim _ | Cexpr_and _
   | Cexpr_or _ | Cexpr_function _ | Cexpr_apply _ | Cexpr_object _
   | Cexpr_constr _ | Cexpr_tuple _ | Cexpr_record _ | Cexpr_record_update _
   | Cexpr_field _ | Cexpr_mutate _ | Cexpr_array _ | Cexpr_assign _
   | Cexpr_loop _ | Cexpr_break _ | Cexpr_continue _ | Cexpr_handle_error _ ->
       f e
    : expr)

let joinlet_tail ~(loc : Rloc.t) (name : Ident.t) (params : param list)
    (join_body : expr) (body : expr) =
  (let default () =
     (letfn ~loc name (fn params join_body) ~kind:Tail_join body : expr)
       [@@inline]
   in
   match (body : expr) with
   | Cexpr_apply { kind = Join; func; args; ty = _; _ }
     when Ident.equal name func ->
       Lst.fold_right2 params args join_body (fun param ->
           fun arg -> fun body -> let_ ~loc:ghost_loc_ param.binder arg body)
   | _ -> (
       match (params, join_body) with
       | [], (Cexpr_const _ | Cexpr_unit _) ->
           tail_map body (fun tail_expr ->
               match tail_expr with
               | Cexpr_apply { func; _ } when Ident.equal func name -> join_body
               | _ -> tail_expr)
       | _ -> default ())
    : expr)

let join_apply ~(loc : Rloc.t) ~ty join args =
  (Cexpr_apply { func = join; args; kind = Join; ty; prim = None; loc_ = loc }
    : expr)

module Map = struct
  class virtual ['a] mapbase =
    object
      method visit_prim : 'a -> Primitive.prim -> Primitive.prim =
        fun _ -> fun e -> e

      method visit_constr_tag : 'a -> constr_tag -> constr_tag =
        fun _ -> fun e -> e

      method visit_constr : 'a -> constr -> constr = fun _ -> fun e -> e
      method visit_label : 'a -> label -> label = fun _ -> fun e -> e
      method visit_accessor : 'a -> accessor -> accessor = fun _ -> fun e -> e
      method visit_location : 'a -> location -> location = fun _ -> fun e -> e

      method visit_absolute_loc : 'a -> absolute_loc -> absolute_loc =
        fun _ -> fun e -> e

      method visit_binder : 'a -> binder -> binder = fun _ -> fun e -> e
      method visit_var : 'a -> var -> var = fun _ -> fun e -> e

      method visit_loop_label : 'a -> loop_label -> loop_label =
        fun _ -> fun e -> e

      method visit_typ : 'a -> typ -> typ = fun _ -> fun e -> e

      method visit_func_stubs : 'a -> func_stubs -> func_stubs =
        fun _ -> fun e -> e

      method visit_return_kind : 'a -> return_kind -> return_kind =
        fun _ -> fun e -> e

      method private visit_object_key :
          'a -> Object_util.object_key -> Object_util.object_key =
        fun _ -> fun e -> e
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] map =
      object (self : 'self)
        inherit [_] mapbase

        method visit_Ctop_expr : _ -> expr -> absolute_loc -> top_item =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_floc_ ->
              let _visitors_r0 = self#visit_expr env _visitors_fexpr in
              let _visitors_r1 = self#visit_absolute_loc env _visitors_floc_ in
              Ctop_expr { expr = _visitors_r0; loc_ = _visitors_r1 }

        method visit_Ctop_let :
            _ -> binder -> expr -> bool -> absolute_loc -> top_item =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_fexpr ->
              fun _visitors_fis_pub_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_pub_
                in
                let _visitors_r3 =
                  self#visit_absolute_loc env _visitors_floc_
                in
                Ctop_let
                  {
                    binder = _visitors_r0;
                    expr = _visitors_r1;
                    is_pub_ = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Ctop_fn : _ -> top_fun_decl -> top_item =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_top_fun_decl env _visitors_c0 in
             Ctop_fn _visitors_r0

        method visit_Ctop_stub :
            _ ->
            binder ->
            func_stubs ->
            typ list ->
            typ option ->
            string option ->
            absolute_loc ->
            top_item =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fexport_info_ ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fbinder in
                  let _visitors_r1 =
                    self#visit_func_stubs env _visitors_ffunc_stubs
                  in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_typ env))
                      _visitors_fparams_ty
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_typ env) t)
                      | None -> None)
                      _visitors_freturn_ty
                  in
                  let _visitors_r4 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t ->
                          Some ((fun _visitors_this -> _visitors_this) t)
                      | None -> None)
                      _visitors_fexport_info_
                  in
                  let _visitors_r5 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  Ctop_stub
                    {
                      binder = _visitors_r0;
                      func_stubs = _visitors_r1;
                      params_ty = _visitors_r2;
                      return_ty = _visitors_r3;
                      export_info_ = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_top_item : _ -> top_item -> top_item =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr { expr = _visitors_fexpr; loc_ = _visitors_floc_ } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_floc_
             | Ctop_let
                 {
                   binder = _visitors_fbinder;
                   expr = _visitors_fexpr;
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_let env _visitors_fbinder _visitors_fexpr
                   _visitors_fis_pub_ _visitors_floc_
             | Ctop_fn _visitors_c0 -> self#visit_Ctop_fn env _visitors_c0
             | Ctop_stub
                 {
                   binder = _visitors_fbinder;
                   func_stubs = _visitors_ffunc_stubs;
                   params_ty = _visitors_fparams_ty;
                   return_ty = _visitors_freturn_ty;
                   export_info_ = _visitors_fexport_info_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fexport_info_ _visitors_floc_

        method visit_top_fun_decl : _ -> top_fun_decl -> top_fun_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               (fun _visitors_this ->
                 match _visitors_this with
                 | Some t -> Some ((fun _visitors_this -> _visitors_this) t)
                 | None -> None)
                 _visitors_this.export_info_
             in
             let _visitors_r3 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             {
               binder = _visitors_r0;
               func = _visitors_r1;
               export_info_ = _visitors_r2;
               loc_ = _visitors_r3;
             }

        method visit_To_result : _ -> handle_kind = fun env -> To_result

        method visit_Joinapply : _ -> var -> handle_kind =
          fun env ->
            fun _visitors_c0 ->
             let _visitors_r0 = self#visit_var env _visitors_c0 in
             Joinapply _visitors_r0

        method visit_Return_err : _ -> typ -> handle_kind =
          fun env ->
            fun _visitors_fok_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_fok_ty in
             Return_err { ok_ty = _visitors_r0 }

        method visit_handle_kind : _ -> handle_kind -> handle_kind =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | To_result -> self#visit_To_result env
             | Joinapply _visitors_c0 -> self#visit_Joinapply env _visitors_c0
             | Return_err { ok_ty = _visitors_fok_ty } ->
                 self#visit_Return_err env _visitors_fok_ty

        method visit_Cexpr_const : _ -> constant -> typ -> location -> expr =
          fun env ->
            fun _visitors_fc ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this -> _visitors_this) _visitors_fc
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_const
                 { c = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Cexpr_unit : _ -> location -> expr =
          fun env ->
            fun _visitors_floc_ ->
             let _visitors_r0 = self#visit_location env _visitors_floc_ in
             Cexpr_unit { loc_ = _visitors_r0 }

        method visit_Cexpr_var :
            _ -> var -> prim option -> typ -> location -> expr =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fprim ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fid in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_prim env) t)
                    | None -> None)
                    _visitors_fprim
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_var
                  {
                    id = _visitors_r0;
                    prim = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_prim :
            _ -> prim -> expr list -> typ -> location -> expr =
          fun env ->
            fun _visitors_fprim ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_prim env _visitors_fprim in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                (fun p -> fun args -> fun ty -> fun loc -> prim ~loc ~ty p args)
                  _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3

        method visit_Cexpr_let :
            _ -> binder -> expr -> expr -> typ -> location -> expr =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_frhs ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_binder env _visitors_fname in
                 let _visitors_r1 = self#visit_expr env _visitors_frhs in
                 let _visitors_r2 = self#visit_expr env _visitors_fbody in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 Cexpr_let
                   {
                     name = _visitors_r0;
                     rhs = _visitors_r1;
                     body = _visitors_r2;
                     ty = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Cexpr_letfn :
            _ -> binder -> fn -> expr -> typ -> letfn_kind -> location -> expr =
          fun env ->
            fun _visitors_fname ->
             fun _visitors_ffn ->
              fun _visitors_fbody ->
               fun _visitors_fty ->
                fun _visitors_fkind ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_binder env _visitors_fname in
                  let _visitors_r1 = self#visit_fn env _visitors_ffn in
                  let _visitors_r2 = self#visit_expr env _visitors_fbody in
                  let _visitors_r3 = self#visit_typ env _visitors_fty in
                  let _visitors_r4 =
                    (fun _visitors_this -> _visitors_this) _visitors_fkind
                  in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  Cexpr_letfn
                    {
                      name = _visitors_r0;
                      fn = _visitors_r1;
                      body = _visitors_r2;
                      ty = _visitors_r3;
                      kind = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Cexpr_function : _ -> fn -> typ -> bool -> location -> expr
            =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fty ->
              fun _visitors_fis_raw_ ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_fn env _visitors_ffunc in
                let _visitors_r1 = self#visit_typ env _visitors_fty in
                let _visitors_r2 =
                  (fun _visitors_this -> _visitors_this) _visitors_fis_raw_
                in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_function
                  {
                    func = _visitors_r0;
                    ty = _visitors_r1;
                    is_raw_ = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            prim option ->
            typ ->
            location ->
            expr =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fprim ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_var env _visitors_ffunc in
                  let _visitors_r1 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_expr env))
                      _visitors_fargs
                  in
                  let _visitors_r2 =
                    self#visit_apply_kind env _visitors_fkind
                  in
                  let _visitors_r3 =
                    (fun _visitors_this ->
                      match _visitors_this with
                      | Some t -> Some ((self#visit_prim env) t)
                      | None -> None)
                      _visitors_fprim
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  Cexpr_apply
                    {
                      func = _visitors_r0;
                      args = _visitors_r1;
                      kind = _visitors_r2;
                      prim = _visitors_r3;
                      ty = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Cexpr_object :
            _ -> Object_util.object_key -> expr -> typ -> location -> expr =
          fun env ->
            fun _visitors_fmethods_key ->
             fun _visitors_fself ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  self#visit_object_key env _visitors_fmethods_key
                in
                let _visitors_r1 = self#visit_expr env _visitors_fself in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_object
                  {
                    methods_key = _visitors_r0;
                    self = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_letrec :
            _ -> (binder * fn) list -> expr -> typ -> location -> expr =
          fun env ->
            fun _visitors_fbindings ->
             fun _visitors_fbody ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this
                      (fun (_visitors_c0, _visitors_c1) ->
                        let _visitors_r0 = self#visit_binder env _visitors_c0 in
                        let _visitors_r1 = self#visit_fn env _visitors_c1 in
                        (_visitors_r0, _visitors_r1)))
                    _visitors_fbindings
                in
                let _visitors_r1 = self#visit_expr env _visitors_fbody in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_letrec
                  {
                    bindings = _visitors_r0;
                    body = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_constr :
            _ -> constr_tag -> expr list -> typ -> location -> expr =
          fun env ->
            fun _visitors_ftag ->
             fun _visitors_fargs ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_constr_tag env _visitors_ftag in
                let _visitors_r1 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_constr
                  {
                    tag = _visitors_r0;
                    args = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_tuple : _ -> expr list -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_tuple
                 {
                   exprs = _visitors_r0;
                   ty = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Cexpr_record :
            _ -> field_def list -> typ -> location -> expr =
          fun env ->
            fun _visitors_ffields ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_field_def env))
                   _visitors_ffields
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_record
                 {
                   fields = _visitors_r0;
                   ty = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Cexpr_record_update :
            _ -> expr -> field_def list -> int -> typ -> location -> expr =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_ffields ->
              fun _visitors_ffields_num ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this (self#visit_field_def env))
                     _visitors_ffields
                 in
                 let _visitors_r2 =
                   (fun _visitors_this -> _visitors_this) _visitors_ffields_num
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 Cexpr_record_update
                   {
                     record = _visitors_r0;
                     fields = _visitors_r1;
                     fields_num = _visitors_r2;
                     ty = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Cexpr_field :
            _ -> expr -> accessor -> int -> typ -> location -> expr =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_faccessor ->
              fun _visitors_fpos ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_frecord in
                 let _visitors_r1 =
                   self#visit_accessor env _visitors_faccessor
                 in
                 let _visitors_r2 =
                   (fun _visitors_this -> _visitors_this) _visitors_fpos
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 Cexpr_field
                   {
                     record = _visitors_r0;
                     accessor = _visitors_r1;
                     pos = _visitors_r2;
                     ty = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Cexpr_mutate :
            _ -> expr -> label -> expr -> int -> typ -> location -> expr =
          fun env ->
            fun _visitors_frecord ->
             fun _visitors_flabel ->
              fun _visitors_ffield ->
               fun _visitors_fpos ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 = self#visit_expr env _visitors_frecord in
                  let _visitors_r1 = self#visit_label env _visitors_flabel in
                  let _visitors_r2 = self#visit_expr env _visitors_ffield in
                  let _visitors_r3 =
                    (fun _visitors_this -> _visitors_this) _visitors_fpos
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  Cexpr_mutate
                    {
                      record = _visitors_r0;
                      label = _visitors_r1;
                      field = _visitors_r2;
                      pos = _visitors_r3;
                      ty = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Cexpr_array : _ -> expr list -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_fty ->
              fun _visitors_floc_ ->
               let _visitors_r0 =
                 (fun _visitors_this ->
                   Basic_lst.map _visitors_this (self#visit_expr env))
                   _visitors_fexprs
               in
               let _visitors_r1 = self#visit_typ env _visitors_fty in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_array
                 {
                   exprs = _visitors_r0;
                   ty = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

        method visit_Cexpr_assign : _ -> var -> expr -> typ -> location -> expr
            =
          fun env ->
            fun _visitors_fvar ->
             fun _visitors_fexpr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_var env _visitors_fvar in
                let _visitors_r1 = self#visit_expr env _visitors_fexpr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_assign
                  {
                    var = _visitors_r0;
                    expr = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_sequence :
            _ -> expr list -> expr -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexprs ->
             fun _visitors_flast_expr ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_expr env))
                    _visitors_fexprs
                in
                let _visitors_r1 = self#visit_expr env _visitors_flast_expr in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_sequence
                  {
                    exprs = _visitors_r0;
                    last_expr = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_if :
            _ -> expr -> expr -> expr option -> typ -> location -> expr =
          fun env ->
            fun _visitors_fcond ->
             fun _visitors_fifso ->
              fun _visitors_fifnot ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fcond in
                 let _visitors_r1 = self#visit_expr env _visitors_fifso in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_fifnot
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 (fun cond ->
                   fun ifso ->
                    fun ifnot -> fun ty -> fun loc -> if_ ~loc cond ~ifso ?ifnot)
                   _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3
                   _visitors_r4

        method visit_Cexpr_switch_constr :
            _ ->
            expr ->
            (constr_tag * binder option * expr) list ->
            expr option ->
            typ ->
            location ->
            expr =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this
                       (fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                         let _visitors_r0 =
                           self#visit_constr_tag env _visitors_c0
                         in
                         let _visitors_r1 =
                           (fun _visitors_this ->
                             match _visitors_this with
                             | Some t -> Some ((self#visit_binder env) t)
                             | None -> None)
                             _visitors_c1
                         in
                         let _visitors_r2 = self#visit_expr env _visitors_c2 in
                         (_visitors_r0, _visitors_r1, _visitors_r2)))
                     _visitors_fcases
                 in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_expr env) t)
                     | None -> None)
                     _visitors_fdefault
                 in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 (fun obj ->
                   fun cases ->
                    fun default ->
                     fun ty -> fun loc -> switch_constr ~loc obj cases ~default)
                   _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3
                   _visitors_r4

        method visit_Cexpr_switch_constant :
            _ ->
            expr ->
            (constant * expr) list ->
            expr ->
            typ ->
            location ->
            expr =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fcases ->
              fun _visitors_fdefault ->
               fun _visitors_fty ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_expr env _visitors_fobj in
                 let _visitors_r1 =
                   (fun _visitors_this ->
                     Basic_lst.map _visitors_this
                       (fun (_visitors_c0, _visitors_c1) ->
                         let _visitors_r0 =
                           (fun _visitors_this -> _visitors_this) _visitors_c0
                         in
                         let _visitors_r1 = self#visit_expr env _visitors_c1 in
                         (_visitors_r0, _visitors_r1)))
                     _visitors_fcases
                 in
                 let _visitors_r2 = self#visit_expr env _visitors_fdefault in
                 let _visitors_r3 = self#visit_typ env _visitors_fty in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 (fun obj ->
                   fun cases ->
                    fun default ->
                     fun ty ->
                      fun loc -> switch_constant ~loc obj cases ~default)
                   _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3
                   _visitors_r4

        method visit_Cexpr_loop :
            _ ->
            param list ->
            expr ->
            expr list ->
            loop_label ->
            typ ->
            location ->
            expr =
          fun env ->
            fun _visitors_fparams ->
             fun _visitors_fbody ->
              fun _visitors_fargs ->
               fun _visitors_flabel ->
                fun _visitors_fty ->
                 fun _visitors_floc_ ->
                  let _visitors_r0 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_param env))
                      _visitors_fparams
                  in
                  let _visitors_r1 = self#visit_expr env _visitors_fbody in
                  let _visitors_r2 =
                    (fun _visitors_this ->
                      Basic_lst.map _visitors_this (self#visit_expr env))
                      _visitors_fargs
                  in
                  let _visitors_r3 =
                    self#visit_loop_label env _visitors_flabel
                  in
                  let _visitors_r4 = self#visit_typ env _visitors_fty in
                  let _visitors_r5 = self#visit_location env _visitors_floc_ in
                  Cexpr_loop
                    {
                      params = _visitors_r0;
                      body = _visitors_r1;
                      args = _visitors_r2;
                      label = _visitors_r3;
                      ty = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_Cexpr_break :
            _ -> expr option -> loop_label -> typ -> location -> expr =
          fun env ->
            fun _visitors_farg ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    match _visitors_this with
                    | Some t -> Some ((self#visit_expr env) t)
                    | None -> None)
                    _visitors_farg
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                (fun arg ->
                  fun label -> fun ty -> fun loc_ -> break arg label ty ~loc_)
                  _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3

        method visit_Cexpr_continue :
            _ -> expr list -> loop_label -> typ -> location -> expr =
          fun env ->
            fun _visitors_fargs ->
             fun _visitors_flabel ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 =
                  (fun _visitors_this ->
                    Basic_lst.map _visitors_this (self#visit_expr env))
                    _visitors_fargs
                in
                let _visitors_r1 = self#visit_loop_label env _visitors_flabel in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_continue
                  {
                    args = _visitors_r0;
                    label = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_handle_error :
            _ -> expr -> handle_kind -> typ -> location -> expr =
          fun env ->
            fun _visitors_fobj ->
             fun _visitors_fhandle_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fobj in
                let _visitors_r1 =
                  self#visit_handle_kind env _visitors_fhandle_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_handle_error
                  {
                    obj = _visitors_r0;
                    handle_kind = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_return :
            _ -> expr -> return_kind -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_freturn_kind ->
              fun _visitors_fty ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 =
                  self#visit_return_kind env _visitors_freturn_kind
                in
                let _visitors_r2 = self#visit_typ env _visitors_fty in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_return
                  {
                    expr = _visitors_r0;
                    return_kind = _visitors_r1;
                    ty = _visitors_r2;
                    loc_ = _visitors_r3;
                  }

        method visit_Cexpr_and : _ -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_and
                 { lhs = _visitors_r0; rhs = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Cexpr_or : _ -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               Cexpr_or
                 { lhs = _visitors_r0; rhs = _visitors_r1; loc_ = _visitors_r2 }

        method visit_expr : _ -> expr -> expr =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Cexpr_const
                 {
                   c = _visitors_fc;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_const env _visitors_fc _visitors_fty
                   _visitors_floc_
             | Cexpr_unit { loc_ = _visitors_floc_ } ->
                 self#visit_Cexpr_unit env _visitors_floc_
             | Cexpr_var
                 {
                   id = _visitors_fid;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fprim
                   _visitors_fty _visitors_floc_
             | Cexpr_prim
                 {
                   prim = _visitors_fprim;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_prim env _visitors_fprim _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_let
                 {
                   name = _visitors_fname;
                   rhs = _visitors_frhs;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_let env _visitors_fname _visitors_frhs
                   _visitors_fbody _visitors_fty _visitors_floc_
             | Cexpr_letfn
                 {
                   name = _visitors_fname;
                   fn = _visitors_ffn;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   kind = _visitors_fkind;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letfn env _visitors_fname _visitors_ffn
                   _visitors_fbody _visitors_fty _visitors_fkind _visitors_floc_
             | Cexpr_function
                 {
                   func = _visitors_ffunc;
                   ty = _visitors_fty;
                   is_raw_ = _visitors_fis_raw_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_function env _visitors_ffunc _visitors_fty
                   _visitors_fis_raw_ _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   prim = _visitors_fprim;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fprim _visitors_fty _visitors_floc_
             | Cexpr_object
                 {
                   methods_key = _visitors_fmethods_key;
                   self = _visitors_fself;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_object env _visitors_fmethods_key
                   _visitors_fself _visitors_fty _visitors_floc_
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_constr
                 {
                   tag = _visitors_ftag;
                   args = _visitors_fargs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_constr env _visitors_ftag _visitors_fargs
                   _visitors_fty _visitors_floc_
             | Cexpr_tuple
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_tuple env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_record
                 {
                   fields = _visitors_ffields;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record env _visitors_ffields _visitors_fty
                   _visitors_floc_
             | Cexpr_record_update
                 {
                   record = _visitors_frecord;
                   fields = _visitors_ffields;
                   fields_num = _visitors_ffields_num;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_record_update env _visitors_frecord
                   _visitors_ffields _visitors_ffields_num _visitors_fty
                   _visitors_floc_
             | Cexpr_field
                 {
                   record = _visitors_frecord;
                   accessor = _visitors_faccessor;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_field env _visitors_frecord
                   _visitors_faccessor _visitors_fpos _visitors_fty
                   _visitors_floc_
             | Cexpr_mutate
                 {
                   record = _visitors_frecord;
                   label = _visitors_flabel;
                   field = _visitors_ffield;
                   pos = _visitors_fpos;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_mutate env _visitors_frecord _visitors_flabel
                   _visitors_ffield _visitors_fpos _visitors_fty _visitors_floc_
             | Cexpr_array
                 {
                   exprs = _visitors_fexprs;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_array env _visitors_fexprs _visitors_fty
                   _visitors_floc_
             | Cexpr_assign
                 {
                   var = _visitors_fvar;
                   expr = _visitors_fexpr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_assign env _visitors_fvar _visitors_fexpr
                   _visitors_fty _visitors_floc_
             | Cexpr_sequence
                 {
                   exprs = _visitors_fexprs;
                   last_expr = _visitors_flast_expr;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_sequence env _visitors_fexprs
                   _visitors_flast_expr _visitors_fty _visitors_floc_
             | Cexpr_if
                 {
                   cond = _visitors_fcond;
                   ifso = _visitors_fifso;
                   ifnot = _visitors_fifnot;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_if env _visitors_fcond _visitors_fifso
                   _visitors_fifnot _visitors_fty _visitors_floc_
             | Cexpr_switch_constr
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constr env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_switch_constant
                 {
                   obj = _visitors_fobj;
                   cases = _visitors_fcases;
                   default = _visitors_fdefault;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_switch_constant env _visitors_fobj
                   _visitors_fcases _visitors_fdefault _visitors_fty
                   _visitors_floc_
             | Cexpr_loop
                 {
                   params = _visitors_fparams;
                   body = _visitors_fbody;
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_loop env _visitors_fparams _visitors_fbody
                   _visitors_fargs _visitors_flabel _visitors_fty
                   _visitors_floc_
             | Cexpr_break
                 {
                   arg = _visitors_farg;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_break env _visitors_farg _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_continue
                 {
                   args = _visitors_fargs;
                   label = _visitors_flabel;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_continue env _visitors_fargs _visitors_flabel
                   _visitors_fty _visitors_floc_
             | Cexpr_handle_error
                 {
                   obj = _visitors_fobj;
                   handle_kind = _visitors_fhandle_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_handle_error env _visitors_fobj
                   _visitors_fhandle_kind _visitors_fty _visitors_floc_
             | Cexpr_return
                 {
                   expr = _visitors_fexpr;
                   return_kind = _visitors_freturn_kind;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_return env _visitors_fexpr
                   _visitors_freturn_kind _visitors_fty _visitors_floc_
             | Cexpr_and
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_and env _visitors_flhs _visitors_frhs
                   _visitors_floc_
             | Cexpr_or
                 {
                   lhs = _visitors_flhs;
                   rhs = _visitors_frhs;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_or env _visitors_flhs _visitors_frhs
                   _visitors_floc_

        method visit_fn : _ -> fn -> fn =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_param env))
                 _visitors_this.params
             in
             let _visitors_r1 = self#visit_expr env _visitors_this.body in
             { params = _visitors_r0; body = _visitors_r1 }

        method visit_param : _ -> param -> param =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_typ env _visitors_this.ty in
             let _visitors_r2 = self#visit_location env _visitors_this.loc_ in
             { binder = _visitors_r0; ty = _visitors_r1; loc_ = _visitors_r2 }

        method visit_Normal : _ -> typ -> apply_kind =
          fun env ->
            fun _visitors_ffunc_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_ffunc_ty in
             Normal { func_ty = _visitors_r0 }

        method visit_Join : _ -> apply_kind = fun env -> Join

        method visit_apply_kind : _ -> apply_kind -> apply_kind =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Normal { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Normal env _visitors_ffunc_ty
             | Join -> self#visit_Join env

        method visit_field_def : _ -> field_def -> field_def =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_label env _visitors_this.label in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_this.pos
             in
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_this.is_mut
             in
             let _visitors_r3 = self#visit_expr env _visitors_this.expr in
             {
               label = _visitors_r0;
               pos = _visitors_r1;
               is_mut = _visitors_r2;
               expr = _visitors_r3;
             }
      end

    [@@@VISITORS.END]
  end

  include struct
    let _ = fun (_ : _unused) -> ()
  end
end

module Map_core = struct
  include struct
    class virtual ['self] map =
      object (self : 'self)
        method visit_binder : 'a -> binder -> binder = fun _ -> fun e -> e
        method visit_var : 'a -> var -> var = fun _ -> fun e -> e

        method visit_loop_label : 'a -> loop_label -> loop_label =
          fun _ -> fun e -> e

        method virtual visit_typ : 'a -> Stype.t -> typ
        method virtual visit_tag : 'a -> Constr_info.constr_tag -> constr_tag

        method private visit_Cexpr_const :
            _ -> constant -> Stype.t -> location -> expr =
          fun env ->
            fun c ->
             fun ty ->
              fun loc_ ->
               let ty = self#visit_typ env ty in
               Cexpr_const { c; ty; loc_ }

        method virtual visit_Cexpr_var
            : 'a ->
              var ->
              Stype.t ->
              Stype.t array ->
              prim option ->
              location ->
              expr

        method virtual visit_Cexpr_as
            : 'a ->
              Core.expr ->
              Basic_type_path.t ->
              Stype.t ->
              location ->
              expr

        method visit_Cexpr_prim :
            'a -> prim -> Core.expr list -> Stype.t -> location -> expr =
          fun env ->
            fun p ->
             fun args ->
              fun ty ->
               fun loc_ ->
                let args = Lst.map args (self#visit_expr env) in
                let ty = self#visit_typ env ty in
                prim p args ~ty ~loc:loc_

        method private visit_Cexpr_let :
            'a -> var -> Core.expr -> Core.expr -> Stype.t -> location -> expr =
          fun env ->
            fun name ->
             fun rhs ->
              fun body ->
               fun ty ->
                fun loc_ ->
                 let name = self#visit_binder env name in
                 let rhs = self#visit_expr env rhs in
                 let body = self#visit_expr env body in
                 let ty = self#visit_typ env ty in
                 Cexpr_let { name; rhs; body; ty; loc_ }

        method private visit_Cexpr_letfn :
            'a ->
            var ->
            Core.fn ->
            Core.expr ->
            Stype.t ->
            letfn_kind ->
            location ->
            expr =
          fun env ->
            fun name ->
             fun (fn : Core.fn) ->
              fun body ->
               fun ty ->
                fun kind ->
                 fun loc_ ->
                  let name = self#visit_binder env name in
                  let fn = self#visit_fn env fn in
                  let body = self#visit_expr env body in
                  let ty = self#visit_typ env ty in
                  Cexpr_letfn { name; fn; body; ty; kind; loc_ }

        method private visit_Cexpr_function :
            'a -> Core.fn -> Stype.t -> bool -> location -> expr =
          fun env ->
            fun func ->
             fun ty ->
              fun is_raw_ ->
               fun loc_ ->
                let func = self#visit_fn env func in
                let ty = self#visit_typ env ty in
                Cexpr_function { func; ty; is_raw_; loc_ }

        method virtual visit_Cexpr_apply
            : 'a ->
              var ->
              Core.expr list ->
              Core.apply_kind ->
              Stype.t ->
              Stype.t array ->
              prim option ->
              location ->
              expr

        method private visit_Cexpr_letrec :
            'a ->
            (var * Core.fn) list ->
            Core.expr ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun bindings ->
             fun body ->
              fun ty ->
               fun loc_ ->
                let bindings =
                  Lst.map bindings (fun (binder, fn) ->
                      let binder = self#visit_binder env binder in
                      let fn = self#visit_fn env fn in
                      (binder, fn))
                in
                let body = self#visit_expr env body in
                let ty = self#visit_typ env ty in
                Cexpr_letrec { bindings; body; ty; loc_ }

        method private visit_Cexpr_constr :
            'a ->
            Constr_info.constr_tag ->
            Core.expr list ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun tag ->
             fun args ->
              fun ty ->
               fun loc_ ->
                let args = Lst.map args (self#visit_expr env) in
                let ty = self#visit_typ env ty in
                let tag = self#visit_tag env tag in
                Cexpr_constr { tag; args; ty; loc_ }

        method private visit_Cexpr_tuple :
            'a -> Core.expr list -> Stype.t -> location -> expr =
          fun env ->
            fun exprs ->
             fun ty ->
              fun loc_ ->
               let exprs = Lst.map exprs (self#visit_expr env) in
               let ty = self#visit_typ env ty in
               Cexpr_tuple { exprs; ty; loc_ }

        method private visit_Cexpr_record :
            'a -> Core.field_def list -> Stype.t -> location -> expr =
          fun env ->
            fun fields ->
             fun ty ->
              fun loc_ ->
               let fields = Lst.map fields (self#visit_field_def env) in
               let ty = self#visit_typ env ty in
               Cexpr_record { fields; ty; loc_ }

        method private visit_Cexpr_record_update :
            'a ->
            Core.expr ->
            Core.field_def list ->
            int ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun record ->
             fun fields ->
              fun fields_num ->
               fun ty ->
                fun loc_ ->
                 let record = self#visit_expr env record in
                 let fields = Lst.map fields (self#visit_field_def env) in
                 let ty = self#visit_typ env ty in
                 Cexpr_record_update { record; fields; fields_num; ty; loc_ }

        method private visit_Cexpr_field :
            'a -> Core.expr -> accessor -> int -> Stype.t -> location -> expr =
          fun env ->
            fun record ->
             fun accessor ->
              fun pos ->
               fun ty ->
                fun loc_ ->
                 let record = self#visit_expr env record in
                 match accessor with
                 | Newtype -> record
                 | Index _ | Label _ ->
                     let ty = self#visit_typ env ty in
                     Cexpr_field { record; accessor; pos; ty; loc_ }

        method private visit_Cexpr_mutate :
            'a ->
            Core.expr ->
            label ->
            Core.expr ->
            int ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun record ->
             fun label ->
              fun field ->
               fun pos ->
                fun ty ->
                 fun loc_ ->
                  let record = self#visit_expr env record in
                  let field = self#visit_expr env field in
                  let ty = self#visit_typ env ty in
                  Cexpr_mutate { record; label; field; pos; ty; loc_ }

        method private visit_Cexpr_array :
            'a -> Core.expr list -> Stype.t -> location -> expr =
          fun env ->
            fun exprs ->
             fun ty ->
              fun loc_ ->
               let exprs = Lst.map exprs (self#visit_expr env) in
               let ty = self#visit_typ env ty in
               Cexpr_array { exprs; ty; loc_ }

        method private visit_Cexpr_assign :
            'a -> var -> Core.expr -> Stype.t -> location -> expr =
          fun env ->
            fun var ->
             fun expr ->
              fun ty ->
               fun loc_ ->
                let var = self#visit_var env var in
                let expr = self#visit_expr env expr in
                let ty = self#visit_typ env ty in
                Cexpr_assign { var; expr; ty; loc_ }

        method private visit_Cexpr_sequence :
            'a -> Core.expr list -> Core.expr -> Stype.t -> location -> expr =
          fun env ->
            fun exprs ->
             fun last_expr ->
              fun ty ->
               fun loc_ ->
                let exprs = Lst.map exprs (self#visit_expr env) in
                let last_expr = self#visit_expr env last_expr in
                let ty = self#visit_typ env ty in
                Cexpr_sequence { exprs; last_expr; ty; loc_ }

        method private visit_Cexpr_if :
            'a ->
            Core.expr ->
            Core.expr ->
            Core.expr option ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun cond ->
             fun ifso ->
              fun ifnot ->
               fun _ty ->
                fun loc_ ->
                 let cond = self#visit_expr env cond in
                 let ifso = self#visit_expr env ifso in
                 let ifnot = Option.map (self#visit_expr env) ifnot in
                 if_ ~loc:loc_ cond ~ifso ?ifnot

        method private visit_Cexpr_switch_constr :
            'a ->
            Core.expr ->
            (Constr_info.constr_tag * var option * Core.expr) list ->
            Core.expr option ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun obj ->
             fun cases ->
              fun default ->
               fun _ty ->
                fun loc_ ->
                 let obj = self#visit_expr env obj in
                 let cases =
                   Lst.map cases (fun (tag, binder, expr) ->
                       let binder = Option.map (self#visit_binder env) binder in
                       let expr = self#visit_expr env expr in
                       let tag = self#visit_tag env tag in
                       (tag, binder, expr))
                 in
                 let default = Option.map (self#visit_expr env) default in
                 switch_constr ~loc:loc_ obj cases ~default

        method private visit_Cexpr_switch_constant :
            'a ->
            Core.expr ->
            (constant * Core.expr) list ->
            Core.expr ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun obj ->
             fun cases ->
              fun default ->
               fun _ty ->
                fun loc_ ->
                 let obj = self#visit_expr env obj in
                 let cases =
                   Lst.map cases (fun (c, expr) ->
                       let expr = self#visit_expr env expr in
                       (c, expr))
                 in
                 let default = self#visit_expr env default in
                 switch_constant ~loc:loc_ obj cases ~default

        method private visit_Cexpr_loop :
            'a ->
            Core.param list ->
            Core.expr ->
            Core.expr list ->
            loop_label ->
            Stype.t ->
            location ->
            expr =
          fun env ->
            fun params ->
             fun body ->
              fun args ->
               fun label ->
                fun ty ->
                 fun loc_ ->
                  let params = Lst.map params (self#visit_param env) in
                  let body = self#visit_expr env body in
                  let args = Lst.map args (self#visit_expr env) in
                  let ty = self#visit_typ env ty in
                  let label = self#visit_loop_label env label in
                  Cexpr_loop { params; body; args; label; ty; loc_ }

        method private visit_handle_kind : 'a -> Core.handle_kind -> handle_kind
            =
          fun env ->
            fun e ->
             match e with
             | To_result -> To_result
             | Joinapply j -> Joinapply (self#visit_var env j)
             | Return_err { ok_ty } ->
                 Return_err { ok_ty = self#visit_typ env ok_ty }

        method private visit_Cexpr_handle_error :
            'a -> Core.expr -> Core.handle_kind -> Stype.t -> location -> expr =
          fun env ->
            fun obj ->
             fun handle_kind ->
              fun ty ->
               fun loc_ ->
                let obj = self#visit_expr env obj in
                let ty = self#visit_typ env ty in
                let handle_kind = self#visit_handle_kind env handle_kind in
                Cexpr_handle_error { obj; handle_kind; ty; loc_ }

        method private visit_return_kind : 'a -> Core.return_kind -> return_kind
            =
          fun env ->
            fun e ->
             match e with
             | Single_value -> Single_value
             | Error_result { is_error; return_ty } ->
                 Error_result
                   { is_error; return_ty = self#visit_typ env return_ty }

        method private visit_Cexpr_return :
            'a -> Core.expr -> Core.return_kind -> Stype.t -> location -> expr =
          fun env ->
            fun expr ->
             fun return_kind ->
              fun ty ->
               fun loc_ ->
                let expr = self#visit_expr env expr in
                let ty = self#visit_typ env ty in
                let return_kind = self#visit_return_kind env return_kind in
                Cexpr_return { expr; return_kind; ty; loc_ }

        method visit_expr : 'a -> Core.expr -> expr =
          fun env ->
            fun (expr : Core.expr) ->
             match expr with
             | Cexpr_const { c; ty; loc_ } ->
                 self#visit_Cexpr_const env c ty loc_
             | Cexpr_unit { loc_ } -> Cexpr_unit { loc_ }
             | Cexpr_var { id; ty; prim; ty_args_; loc_ } ->
                 self#visit_Cexpr_var env id ty ty_args_ prim loc_
             | Cexpr_as { expr; trait; obj_type; loc_ } ->
                 self#visit_Cexpr_as env expr trait obj_type loc_
             | Cexpr_prim { prim; args; ty; loc_ } ->
                 self#visit_Cexpr_prim env prim args ty loc_
             | Cexpr_let { name; rhs; body; ty; loc_ } ->
                 self#visit_Cexpr_let env name rhs body ty loc_
             | Cexpr_letfn { name; fn; body; ty; kind; loc_ } ->
                 self#visit_Cexpr_letfn env name fn body ty kind loc_
             | Cexpr_function { func; ty; is_raw_; loc_ } ->
                 self#visit_Cexpr_function env func ty is_raw_ loc_
             | Cexpr_apply { func; args; kind; ty; ty_args_; prim; loc_ } ->
                 self#visit_Cexpr_apply env func args kind ty ty_args_ prim loc_
             | Cexpr_letrec { bindings; body; ty; loc_ } ->
                 self#visit_Cexpr_letrec env bindings body ty loc_
             | Cexpr_constr { tag; args; ty; loc_ } ->
                 self#visit_Cexpr_constr env tag args ty loc_
             | Cexpr_tuple { exprs; ty; loc_ } ->
                 self#visit_Cexpr_tuple env exprs ty loc_
             | Cexpr_record { fields; ty; loc_ } ->
                 self#visit_Cexpr_record env fields ty loc_
             | Cexpr_record_update { record; fields; fields_num; ty; loc_ } ->
                 self#visit_Cexpr_record_update env record fields fields_num ty
                   loc_
             | Cexpr_field { record; accessor; pos; ty; loc_ } ->
                 self#visit_Cexpr_field env record accessor pos ty loc_
             | Cexpr_mutate { record; label; field; pos; ty; loc_ } ->
                 self#visit_Cexpr_mutate env record label field pos ty loc_
             | Cexpr_array { exprs; ty; loc_ } ->
                 self#visit_Cexpr_array env exprs ty loc_
             | Cexpr_assign { var; expr; ty; loc_ } ->
                 self#visit_Cexpr_assign env var expr ty loc_
             | Cexpr_sequence { exprs; last_expr; ty; loc_ } ->
                 self#visit_Cexpr_sequence env exprs last_expr ty loc_
             | Cexpr_if { cond; ifso; ifnot; ty; loc_ } ->
                 self#visit_Cexpr_if env cond ifso ifnot ty loc_
             | Cexpr_switch_constr { obj; cases; default; ty; loc_ } ->
                 self#visit_Cexpr_switch_constr env obj cases default ty loc_
             | Cexpr_switch_constant { obj; cases; default; ty; loc_ } ->
                 self#visit_Cexpr_switch_constant env obj cases default ty loc_
             | Cexpr_loop { params; body; args; label; ty; loc_ } ->
                 self#visit_Cexpr_loop env params body args label ty loc_
             | Cexpr_break { arg; label; ty; loc_ } ->
                 let ty = self#visit_typ env ty in
                 let label = self#visit_loop_label env label in
                 let arg = Option.map (self#visit_expr env) arg in
                 Cexpr_break { arg; label; ty; loc_ }
             | Cexpr_continue { args; label; ty; loc_ } ->
                 let ty = self#visit_typ env ty in
                 let label = self#visit_loop_label env label in
                 let args = Lst.map args (self#visit_expr env) in
                 Cexpr_continue { args; label; ty; loc_ }
             | Cexpr_handle_error { obj; handle_kind; ty; loc_ } ->
                 self#visit_Cexpr_handle_error env obj handle_kind ty loc_
             | Cexpr_return { expr; return_kind; ty; loc_ } ->
                 self#visit_Cexpr_return env expr return_kind ty loc_
             | Cexpr_and { lhs; rhs; loc_ } ->
                 let lhs = self#visit_expr env lhs in
                 let rhs = self#visit_expr env rhs in
                 Cexpr_and { lhs; rhs; loc_ }
             | Cexpr_or { lhs; rhs; loc_ } ->
                 let lhs = self#visit_expr env lhs in
                 let rhs = self#visit_expr env rhs in
                 Cexpr_or { lhs; rhs; loc_ }

        method private visit_fn : 'a -> Core.fn -> fn =
          fun env ->
            fun { params; body; is_async = _ } ->
             let params = Lst.map params (self#visit_param env) in
             let body = self#visit_expr env body in
             { params; body }

        method private visit_param : 'a -> Core.param -> param =
          fun env ->
            fun { binder; ty; loc_ } ->
             let binder = self#visit_binder env binder in
             let ty = self#visit_typ env ty in
             { binder; ty; loc_ }

        method visit_apply_kind : 'a -> Core.apply_kind -> apply_kind =
          fun env ->
            fun (_visitors_this : Core.apply_kind) ->
             match _visitors_this with
             | Normal { func_ty } ->
                 Normal { func_ty = self#visit_typ env func_ty }
             | Join -> Join
             | Async _ -> assert false

        method private visit_field_def : 'a -> Core.field_def -> field_def =
          fun env ->
            fun { label; pos; is_mut; expr } ->
             let expr = self#visit_expr env expr in
             { label; pos; is_mut; expr }
      end
  end
end

type t = {
  body : top_item list;
  main : (expr * absolute_loc) option;
  types : Mtype.defs;
  object_methods : Object_util.t;
}

let sexp_of_t ?(use_absolute_loc = false) prog =
  (let global_stamps = Hashtbl.create 20 in
   let body =
     S.List
       (Lst.map prog.body (fun top_item ->
            let ctx =
              if use_absolute_loc then
                let base =
                  match top_item with
                  | Ctop_expr { loc_; _ }
                  | Ctop_let { loc_; _ }
                  | Ctop_fn { loc_; _ }
                  | Ctop_stub { loc_; _ } ->
                      loc_
                in
                Use_absolute_loc base
              else Use_relative_loc
            in
            Basic_compress_stamp.normalize ~global_stamps
              (sexp_visitor#visit_top_item ctx top_item)))
   in
   let types = Mtype.sexp_of_defs prog.types in
   let object_methods = Object_util.sexp_of_t prog.object_methods in
   match prog.main with
   | Some expr ->
       let sexp_of_expr e =
         if use_absolute_loc then
           sexp_visitor#visit_expr (Use_absolute_loc (snd expr)) e
         else sexp_visitor#visit_expr Use_relative_loc e
       in
       let main =
         if !Basic_config.show_loc then
           Basic_compress_stamp.normalize ~global_stamps
             ((fun [@merlin.hide] (arg0__006_, arg1__007_) ->
                let res0__008_ = sexp_of_expr arg0__006_
                and res1__009_ = sexp_of_absolute_loc arg1__007_ in
                S.List [ res0__008_; res1__009_ ])
                expr)
         else
           Basic_compress_stamp.normalize ~global_stamps
             ((sexp_of_expr [@merlin.hide]) (fst expr))
       in
       (List
          (List.cons
             (List (List.cons (Atom "body" : S.t) ([ body ] : S.t list)) : S.t)
             (List.cons
                (List (List.cons (Atom "main" : S.t) ([ main ] : S.t list))
                  : S.t)
                (List.cons
                   (List (List.cons (Atom "types" : S.t) ([ types ] : S.t list))
                     : S.t)
                   ([
                      List
                        (List.cons
                           (Atom "object_methods" : S.t)
                           ([ object_methods ] : S.t list));
                    ]
                     : S.t list))))
         : S.t)
   | None ->
       (List
          (List.cons
             (List (List.cons (Atom "body" : S.t) ([ body ] : S.t list)) : S.t)
             (List.cons
                (List (List.cons (Atom "types" : S.t) ([ types ] : S.t list))
                  : S.t)
                ([
                   List
                     (List.cons
                        (Atom "object_methods" : S.t)
                        ([ object_methods ] : S.t list));
                 ]
                  : S.t list)))
         : S.t)
    : S.t)
