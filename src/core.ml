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
module Lst = Basic_lst
module Type_path = Basic_type_path
module Constr_info = Basic_constr_info
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

type constr_tag = Constr_info.constr_tag

include struct
  let _ = fun (_ : constr_tag) -> ()
  let sexp_of_constr_tag = (Constr_info.sexp_of_constr_tag : constr_tag -> S.t)
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

type typ = Stype.t

include struct
  let _ = fun (_ : typ) -> ()
  let sexp_of_typ = (Stype.sexp_of_t : typ -> S.t)
  let _ = sexp_of_typ
end

type type_path = Type_path.t

include struct
  let _ = fun (_ : type_path) -> ()
  let sexp_of_type_path = (Type_path.sexp_of_t : type_path -> S.t)
  let _ = sexp_of_type_path
end

type tvar_env = Tvar_env.t

include struct
  let _ = fun (_ : tvar_env) -> ()
  let sexp_of_tvar_env = (Tvar_env.sexp_of_t : tvar_env -> S.t)
  let _ = sexp_of_tvar_env
end

type func_stubs = Stub_type.t

include struct
  let _ = fun (_ : func_stubs) -> ()
  let sexp_of_func_stubs = (Stub_type.sexp_of_t : func_stubs -> S.t)
  let _ = sexp_of_func_stubs
end

type letfn_kind = Nonrec | Rec | Tail_join | Nontail_join

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
  | Ctop_expr of {
      expr : expr;
      is_main : bool; [@sexp_drop_if not]
      loc_ : absolute_loc;
    }
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
      is_pub_ : bool;
      loc_ : absolute_loc;
    }

and subtop_fun_decl = { binder : binder; fn : fn }

and top_fun_decl = {
  binder : binder;
  func : fn;
  subtops : subtop_fun_decl list;
  ty_params_ : tvar_env;
  is_pub_ : bool;
  loc_ : absolute_loc;
}

and handle_kind = To_result | Joinapply of var | Return_err of { ok_ty : typ }

and expr =
  | Cexpr_const of { c : constant; ty : typ; loc_ : location }
  | Cexpr_unit of { loc_ : location }
  | Cexpr_var of {
      id : var;
      ty : typ;
      ty_args_ : typ array;
      prim : prim option;
      loc_ : location;
    }
  | Cexpr_as of {
      expr : expr;
      trait : type_path;
      obj_type : typ;
      loc_ : location;
    }
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
  | Cexpr_letrec of {
      bindings : (binder * fn) list;
      body : expr;
      ty : typ;
      loc_ : location;
    }
  | Cexpr_apply of {
      func : var;
      args : expr list;
      kind : apply_kind;
      ty : typ;
      ty_args_ : typ array;
      prim : prim option;
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
      [@build
        fun record ->
          fun accessor ->
           fun pos -> fun ty -> fun loc -> field ~loc record accessor ~pos ~ty]
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
      [@build
        fun expr1 ->
          fun expr2 -> fun ty -> fun loc_ -> sequence ~loc:loc_ expr1 expr2]
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
      [@build fun lhs -> fun rhs -> fun loc -> and_ ~loc lhs rhs]
  | Cexpr_or of { lhs : expr; rhs : expr; loc_ : location }
      [@build fun lhs -> fun rhs -> fun loc -> or_ ~loc lhs rhs]

and fn = { params : param list; body : expr; is_async : bool }
and param = { binder : binder; ty : typ; loc_ : location }

and apply_kind =
  | Normal of { func_ty : typ }
  | Async of { func_ty : typ }
  | Join

and field_def = { label : label; pos : int; is_mut : bool; expr : expr }

include struct
  let _ = fun (_ : top_item) -> ()
  let _ = fun (_ : subtop_fun_decl) -> ()
  let _ = fun (_ : top_fun_decl) -> ()
  let _ = fun (_ : handle_kind) -> ()
  let _ = fun (_ : expr) -> ()
  let _ = fun (_ : fn) -> ()
  let _ = fun (_ : param) -> ()
  let _ = fun (_ : apply_kind) -> ()
  let _ = fun (_ : field_def) -> ()
end

type program = top_item list

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
      method visit_type_path : 'a -> type_path -> unit = fun _ -> fun _ -> ()
      method visit_tvar_env : 'a -> tvar_env -> unit = fun _ -> fun _ -> ()
      method visit_func_stubs : 'a -> func_stubs -> unit = fun _ -> fun _ -> ()

      method visit_return_kind : 'a -> return_kind -> unit =
        fun _ -> fun _ -> ()
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] iter =
      object (self : 'self)
        inherit [_] iterbase

        method visit_Ctop_expr : _ -> expr -> bool -> absolute_loc -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this -> ()) _visitors_fis_main
               in
               let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
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
            bool ->
            absolute_loc ->
            unit =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fis_pub_ ->
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
                    (fun _visitors_this -> ()) _visitors_fis_pub_
                  in
                  let _visitors_r5 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  ()

        method visit_top_item : _ -> top_item -> unit =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_floc_
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
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fis_pub_ _visitors_floc_

        method visit_subtop_fun_decl : _ -> subtop_fun_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.fn in
             ()

        method visit_top_fun_decl : _ -> top_fun_decl -> unit =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.iter _visitors_this
                   ~f:(self#visit_subtop_fun_decl env))
                 _visitors_this.subtops
             in
             let _visitors_r3 =
               self#visit_tvar_env env _visitors_this.ty_params_
             in
             let _visitors_r4 =
               (fun _visitors_this -> ()) _visitors_this.is_pub_
             in
             let _visitors_r5 =
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
            _ -> var -> typ -> typ array -> prim option -> location -> unit =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fty ->
              fun _visitors_fty_args_ ->
               fun _visitors_fprim ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fid in
                 let _visitors_r1 = self#visit_typ env _visitors_fty in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     Basic_arr.iter _visitors_this (self#visit_typ env))
                     _visitors_fty_args_
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> (self#visit_prim env) t
                     | None -> ())
                     _visitors_fprim
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 ()

        method visit_Cexpr_as :
            _ -> expr -> type_path -> typ -> location -> unit =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_fobj_type ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 = self#visit_type_path env _visitors_ftrait in
                let _visitors_r2 = self#visit_typ env _visitors_fobj_type in
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

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            typ ->
            typ array ->
            prim option ->
            location ->
            unit =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fty ->
                fun _visitors_fty_args_ ->
                 fun _visitors_fprim ->
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
                   let _visitors_r3 = self#visit_typ env _visitors_fty in
                   let _visitors_r4 =
                     (fun _visitors_this ->
                       Basic_arr.iter _visitors_this (self#visit_typ env))
                       _visitors_fty_args_
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> (self#visit_prim env) t
                       | None -> ())
                       _visitors_fprim
                   in
                   let _visitors_r6 = self#visit_location env _visitors_floc_ in
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
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fty
                   _visitors_fty_args_ _visitors_fprim _visitors_floc_
             | Cexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   obj_type = _visitors_fobj_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_fobj_type _visitors_floc_
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
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fty _visitors_fty_args_
                   _visitors_fprim _visitors_floc_
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
             let _visitors_r2 =
               (fun _visitors_this -> ()) _visitors_this.is_async
             in
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

        method visit_Async : _ -> typ -> unit =
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
             | Async { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Async env _visitors_ffunc_ty
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

      method visit_type_path : 'a -> type_path -> S.t =
        fun _ -> fun x -> sexp_of_type_path x

      method visit_tvar_env : 'a -> tvar_env -> S.t =
        fun _ -> fun x -> sexp_of_tvar_env x

      method private visit_test_name : 'a -> Syntax.test_name -> S.t =
        fun _ -> fun x -> Syntax.sexp_of_test_name x

      method visit_func_stubs : 'a -> func_stubs -> S.t =
        fun _ -> fun x -> sexp_of_func_stubs x

      method visit_letfn_kind : 'a -> letfn_kind -> S.t =
        fun _ -> fun x -> sexp_of_letfn_kind x

      method visit_return_kind : 'a -> return_kind -> S.t =
        fun _ -> fun x -> sexp_of_return_kind x
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] sexp =
      object (self : 'self)
        inherit [_] sexpbase

        method visit_Ctop_expr : _ -> expr -> bool -> absolute_loc -> S.t =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 = self#visit_bool env _visitors_fis_main in
               let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
               self#visit_inline_record env "Ctop_expr"
                 [
                   ("expr", _visitors_r0);
                   ("is_main", _visitors_r1);
                   ("loc_", _visitors_r2);
                 ]

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
            bool ->
            absolute_loc ->
            S.t =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fis_pub_ ->
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
                  let _visitors_r4 = self#visit_bool env _visitors_fis_pub_ in
                  let _visitors_r5 =
                    self#visit_absolute_loc env _visitors_floc_
                  in
                  self#visit_inline_record env "Ctop_stub"
                    [
                      ("binder", _visitors_r0);
                      ("func_stubs", _visitors_r1);
                      ("params_ty", _visitors_r2);
                      ("return_ty", _visitors_r3);
                      ("is_pub_", _visitors_r4);
                      ("loc_", _visitors_r5);
                    ]

        method visit_top_item : _ -> top_item -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_floc_
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
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fis_pub_ _visitors_floc_

        method visit_subtop_fun_decl : _ -> subtop_fun_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.fn in
             self#visit_record env
               [ ("binder", _visitors_r0); ("fn", _visitors_r1) ]

        method visit_top_fun_decl : _ -> top_fun_decl -> S.t =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               self#visit_list self#visit_subtop_fun_decl env
                 _visitors_this.subtops
             in
             let _visitors_r3 =
               self#visit_tvar_env env _visitors_this.ty_params_
             in
             let _visitors_r4 = self#visit_bool env _visitors_this.is_pub_ in
             let _visitors_r5 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             self#visit_record env
               [
                 ("binder", _visitors_r0);
                 ("func", _visitors_r1);
                 ("subtops", _visitors_r2);
                 ("ty_params_", _visitors_r3);
                 ("is_pub_", _visitors_r4);
                 ("loc_", _visitors_r5);
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
            _ -> var -> typ -> typ array -> prim option -> location -> S.t =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fty ->
              fun _visitors_fty_args_ ->
               fun _visitors_fprim ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fid in
                 let _visitors_r1 = self#visit_typ env _visitors_fty in
                 let _visitors_r2 =
                   self#visit_array self#visit_typ env _visitors_fty_args_
                 in
                 let _visitors_r3 =
                   self#visit_option self#visit_prim env _visitors_fprim
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 self#visit_inline_record env "Cexpr_var"
                   [
                     ("id", _visitors_r0);
                     ("ty", _visitors_r1);
                     ("ty_args_", _visitors_r2);
                     ("prim", _visitors_r3);
                     ("loc_", _visitors_r4);
                   ]

        method visit_Cexpr_as : _ -> expr -> type_path -> typ -> location -> S.t
            =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_fobj_type ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 = self#visit_type_path env _visitors_ftrait in
                let _visitors_r2 = self#visit_typ env _visitors_fobj_type in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                self#visit_inline_record env "Cexpr_as"
                  [
                    ("expr", _visitors_r0);
                    ("trait", _visitors_r1);
                    ("obj_type", _visitors_r2);
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

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            typ ->
            typ array ->
            prim option ->
            location ->
            S.t =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fty ->
                fun _visitors_fty_args_ ->
                 fun _visitors_fprim ->
                  fun _visitors_floc_ ->
                   let _visitors_r0 = self#visit_var env _visitors_ffunc in
                   let _visitors_r1 =
                     self#visit_list self#visit_expr env _visitors_fargs
                   in
                   let _visitors_r2 =
                     self#visit_apply_kind env _visitors_fkind
                   in
                   let _visitors_r3 = self#visit_typ env _visitors_fty in
                   let _visitors_r4 =
                     self#visit_array self#visit_typ env _visitors_fty_args_
                   in
                   let _visitors_r5 =
                     self#visit_option self#visit_prim env _visitors_fprim
                   in
                   let _visitors_r6 = self#visit_location env _visitors_floc_ in
                   self#visit_inline_record env "Cexpr_apply"
                     [
                       ("func", _visitors_r0);
                       ("args", _visitors_r1);
                       ("kind", _visitors_r2);
                       ("ty", _visitors_r3);
                       ("ty_args_", _visitors_r4);
                       ("prim", _visitors_r5);
                       ("loc_", _visitors_r6);
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
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fty
                   _visitors_fty_args_ _visitors_fprim _visitors_floc_
             | Cexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   obj_type = _visitors_fobj_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_fobj_type _visitors_floc_
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
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fty _visitors_fty_args_
                   _visitors_fprim _visitors_floc_
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
             let _visitors_r2 = self#visit_bool env _visitors_this.is_async in
             self#visit_record env
               [
                 ("params", _visitors_r0);
                 ("body", _visitors_r1);
                 ("is_async", _visitors_r2);
               ]

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

        method visit_Async : _ -> typ -> S.t =
          fun env ->
            fun _visitors_ffunc_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_ffunc_ty in
             self#visit_inline_record env "Async" [ ("func_ty", _visitors_r0) ]

        method visit_Join : _ -> S.t =
          fun env -> self#visit_inline_tuple env "Join" []

        method visit_apply_kind : _ -> apply_kind -> S.t =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Normal { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Normal env _visitors_ffunc_ty
             | Async { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Async env _visitors_ffunc_ty
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
    | "is_main", S.Atom "false" -> None
    | "subtops", S.List [] -> None
    | "prim", S.List [] -> None
    | "is_handle_error", S.Atom "false" -> None
    | "is_async", S.Atom "false" -> None
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

      method! visit_Cexpr_apply env func args kind ty ty_args prim loc_ =
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
        | Normal _ | Async _ ->
            super#visit_Cexpr_apply env func args kind ty ty_args prim loc_

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

let sexp_of_program ?(use_absolute_loc = false) prog =
  let global_stamps = Hashtbl.create 20 in
  S.List
    (Lst.map prog (fun top_item ->
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

let type_of_expr = function
  | Cexpr_const { ty; _ }
  | Cexpr_var { ty; _ }
  | Cexpr_prim { ty; _ }
  | Cexpr_let { ty; _ }
  | Cexpr_letfn { ty; _ }
  | Cexpr_function { ty; _ }
  | Cexpr_apply { ty; _ }
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
  | Cexpr_handle_error { ty; _ }
  | Cexpr_switch_constant { ty; _ }
  | Cexpr_loop { ty; _ }
  | Cexpr_break { ty; _ }
  | Cexpr_continue { ty; _ }
  | Cexpr_return { ty; _ } ->
      ty
  | Cexpr_as { trait; _ } -> Stype.T_trait trait
  | Cexpr_unit _ -> Stype.unit
  | Cexpr_and _ | Cexpr_or _ -> Stype.bool

let loc_of_expr = function
  | Cexpr_const { loc_; _ }
  | Cexpr_unit { loc_ }
  | Cexpr_var { loc_; _ }
  | Cexpr_prim { loc_; _ }
  | Cexpr_let { loc_; _ }
  | Cexpr_letfn { loc_; _ }
  | Cexpr_function { loc_; _ }
  | Cexpr_apply { loc_; _ }
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
  | Cexpr_handle_error { loc_; _ }
  | Cexpr_switch_constant { loc_; _ }
  | Cexpr_break { loc_; _ }
  | Cexpr_continue { loc_; _ }
  | Cexpr_as { loc_; _ }
  | Cexpr_loop { loc_; _ }
  | Cexpr_return { loc_; _ }
  | Cexpr_and { loc_; _ }
  | Cexpr_or { loc_; _ } ->
      loc_

let update_expr_loc e ~loc =
  let loc_ = loc in
  match e with
  | Cexpr_const t -> Cexpr_const { t with loc_ }
  | Cexpr_unit _ -> Cexpr_unit { loc_ }
  | Cexpr_var t -> Cexpr_var { t with loc_ }
  | Cexpr_as t -> Cexpr_as { t with loc_ }
  | Cexpr_prim t -> Cexpr_prim { t with loc_ }
  | Cexpr_let t -> Cexpr_let { t with loc_ }
  | Cexpr_letfn t -> Cexpr_letfn { t with loc_ }
  | Cexpr_function t -> Cexpr_function { t with loc_ }
  | Cexpr_apply t -> Cexpr_apply { t with loc_ }
  | Cexpr_letrec t -> Cexpr_letrec { t with loc_ }
  | Cexpr_constr t -> Cexpr_constr { t with loc_ }
  | Cexpr_tuple t -> Cexpr_tuple { t with loc_ }
  | Cexpr_record t -> Cexpr_record { t with loc_ }
  | Cexpr_record_update t -> Cexpr_record_update { t with loc_ }
  | Cexpr_field t -> Cexpr_field { t with loc_ }
  | Cexpr_mutate t -> Cexpr_mutate { t with loc_ }
  | Cexpr_array t -> Cexpr_array { t with loc_ }
  | Cexpr_assign t -> Cexpr_assign { t with loc_ }
  | Cexpr_sequence t -> Cexpr_sequence { t with loc_ }
  | Cexpr_if t -> Cexpr_if { t with loc_ }
  | Cexpr_switch_constr t -> Cexpr_switch_constr { t with loc_ }
  | Cexpr_handle_error t -> Cexpr_handle_error { t with loc_ }
  | Cexpr_switch_constant t -> Cexpr_switch_constant { t with loc_ }
  | Cexpr_loop t -> Cexpr_loop { t with loc_ }
  | Cexpr_break t -> Cexpr_break { t with loc_ }
  | Cexpr_continue t -> Cexpr_continue { t with loc_ }
  | Cexpr_return t -> Cexpr_return { t with loc_ }
  | Cexpr_and t -> Cexpr_and { t with loc_ }
  | Cexpr_or t -> Cexpr_or { t with loc_ }

let rec expr_equal e1 e2 =
  Basic_prelude.phys_equal e1 e2
  ||
  match (e1, e2) with
  | Cexpr_const { c = c1; _ }, Cexpr_const { c = c2; _ } -> Constant.equal c1 c2
  | Cexpr_unit _, Cexpr_unit _ -> true
  | Cexpr_var { id = id1; _ }, Cexpr_var { id = id2; _ } -> Ident.equal id1 id2
  | ( Cexpr_prim { prim = prim1; args = args1; _ },
      Cexpr_prim { prim = prim2; args = args2; _ } ) ->
      Primitive.equal_prim prim1 prim2
      && Lst.for_all2_no_exn args1 args2 expr_equal
  | Cexpr_as { expr = e1; trait = t1; _ }, Cexpr_as { expr = e2; trait = t2; _ }
    ->
      expr_equal e1 e2 && Type_path.equal t1 t2
  | ( Cexpr_apply { func = func1; args = args1; _ },
      Cexpr_apply { func = func2; args = args2; _ } ) ->
      Ident.equal func1 func2 && Lst.for_all2_no_exn args1 args2 expr_equal
  | ( Cexpr_constr { tag = tag1; args = args1; _ },
      Cexpr_constr { tag = tag2; args = args2; _ } ) ->
      Constr_info.equal_constr_tag tag1 tag2
      && Lst.for_all2_no_exn args1 args2 expr_equal
  | Cexpr_tuple { exprs = exprs1; _ }, Cexpr_tuple { exprs = exprs2; _ } ->
      Lst.for_all2_no_exn exprs1 exprs2 expr_equal
  | _ -> false

let ghost_loc_ = Rloc.no_location

let const ?(loc = ghost_loc_) (c : constant) =
  (let ty =
     match c with
     | C_int _ -> Stype.int
     | C_byte _ -> Stype.byte
     | C_int64 _ -> Stype.int64
     | C_uint _ -> Stype.uint
     | C_uint64 _ -> Stype.uint64
     | C_float _ -> Stype.float
     | C_double _ -> Stype.double
     | C_char _ -> Stype.char
     | C_bool _ -> Stype.bool
     | C_string _ -> Stype.string
     | C_bytes _ -> Stype.bytes
     | C_bigint _ -> Stype.bigint
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
   | Parith { operand_type = _; operator = Neg }, Cexpr_const { c; _ } :: []
     -> (
       match Constant.eval_negation c with
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
   | _ -> Cexpr_prim { prim; args; ty; loc_ = loc }
    : expr)

let and_ ~loc lhs rhs =
  (match lhs with
   | Cexpr_const { c = C_bool b; _ } -> if b then rhs else lhs
   | _ -> Cexpr_and { lhs; rhs; loc_ = loc }
    : expr)

let or_ ~loc lhs rhs =
  (match lhs with
   | Cexpr_const { c = C_bool b; _ } -> if b then lhs else rhs
   | _ -> Cexpr_or { lhs; rhs; loc_ = loc }
    : expr)

let unsaturated_prim ?(loc = ghost_loc_) ~ty prim =
  (match Stype.type_repr ty with
   | Tarrow { params_ty; ret_ty; is_async; err_ty = _ } as ty ->
       let params, args =
         Lst.map_split params_ty (fun ty ->
             (let id = Ident.fresh "*x" in
              ( { binder = id; ty; loc_ = ghost_loc_ },
                Cexpr_var
                  { id; ty; ty_args_ = [||]; prim = None; loc_ = ghost_loc_ } )
               : param * expr))
       in
       let body : expr =
         Cexpr_prim { prim; args; ty = ret_ty; loc_ = ghost_loc_ }
       in
       Cexpr_function
         { func = { params; body; is_async }; ty; is_raw_ = false; loc_ = loc }
   | _ -> assert false
    : expr)

let join_apply ?(loc = ghost_loc_) ~ty join args =
  (Cexpr_apply
     {
       func = join;
       args;
       kind = Join;
       ty;
       ty_args_ = [||];
       prim = None;
       loc_ = loc;
     }
    : expr)

let rec let_ ?(loc = ghost_loc_) (name : Ident.t) (rhs : expr) (body : expr) =
  (let ty = type_of_expr body in
   match (rhs, name) with
   | Cexpr_function { func; is_raw_ = false; _ }, Pident _ ->
       Cexpr_letfn { name; fn = func; kind = Nonrec; body; ty; loc_ = loc }
   | Cexpr_let { name = id2; rhs = rhs2; body = body2; ty = _; loc_ }, _ ->
       if !Basic_config.no_opt then
         Cexpr_let { name; rhs; body; ty; loc_ = loc }
       else
         let body = let_ name body2 body in
         let_ id2 rhs2 body ~loc:loc_
   | _ -> Cexpr_let { name; rhs; body; ty; loc_ = loc }
    : expr)

let letfn ?(loc = ghost_loc_) ~kind name fn body =
  let ty = type_of_expr body in
  Cexpr_letfn { name; fn; kind; body; ty; loc_ = loc }

let bind ?(loc = ghost_loc_) (rhs : expr) (cont : Ident.t -> expr) =
  match rhs with
  | Cexpr_var { id = (Pident _ | Pdot _ | Plocal_method _) as id; _ } -> cont id
  | _ ->
      let id = Ident.fresh "*bind" in
      let_ ~loc id rhs (cont id)

let field ?(loc = ghost_loc_) ~ty ~pos record accessor =
  (match record with
   | Cexpr_record { fields; _ } -> (List.nth fields pos).expr
   | Cexpr_tuple { exprs; _ } -> List.nth exprs pos
   | _ -> Cexpr_field { record; accessor; pos; ty; loc_ = loc }
    : expr)

let panic ?(loc = ghost_loc_) ty =
  (Cexpr_prim { prim = Ppanic; args = []; ty; loc_ = loc } : expr)

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
           prim ~loc ~ty:Stype.bool Pnot [ cond ]
       | _, Some ifnot when expr_equal ifso ifnot ->
           Cexpr_sequence
             {
               exprs =
                 [
                   Cexpr_prim
                     {
                       prim = Pignore;
                       args = [ cond ];
                       ty = Stype.unit;
                       loc_ = loc;
                     };
                 ];
               last_expr = ifso;
               ty = type_of_expr ifso;
               loc_ = loc;
             }
       | _ ->
           let ty = type_of_expr ifso in
           Cexpr_if { cond; ifso; ifnot; ty; loc_ = loc })
    : expr)

let letrec ~loc (bindings : (Ident.t * fn) list) body =
  (let ty = type_of_expr body in
   Cexpr_letrec { bindings; body; ty; loc_ = loc }
    : expr)

let switch_constr ?(loc = ghost_loc_) ~default obj cases =
  (match obj with
   | Cexpr_constr { tag; args; ty = _ } ->
       let rec select_case tag arg cases default =
         match cases with
         | [] -> (
             match default with Some default -> default | None -> assert false)
         | (tag', param, action) :: rest ->
             if Constr_info.equal tag' tag then
               match param with
               | None -> action
               | Some param ->
                   let_ ~loc param
                     (prim ~loc
                        ~ty:(Type.make_constr_type (type_of_expr obj) ~tag)
                        (Pcast { kind = Enum_to_constr })
                        [ obj ])
                     action
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

let handle_error ?(loc = ghost_loc_) obj handle_kind ~ty =
  (Cexpr_handle_error { obj; handle_kind; ty; loc_ = loc } : expr)

let return ?(loc = ghost_loc_) expr ~return_kind ~ty =
  (match expr with
   | Cexpr_continue { args; ty = _; label; loc_ } ->
       Cexpr_continue { args; ty; label; loc_ }
   | _ -> Cexpr_return { expr; ty; return_kind; loc_ = loc }
    : expr)

let switch_constant ?(loc = ghost_loc_) ~default obj cases =
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

let var ?(loc = ghost_loc_) ?(ty_args_ = [||]) ~ty ?(prim = None) (id : Ident.t)
    =
  (Cexpr_var { id; ty; ty_args_; prim; loc_ = loc } : expr)

let as_ ?(loc = ghost_loc_) ~trait ~obj_type (expr : expr) =
  (let ty = type_of_expr expr in
   match ty with
   | T_trait inner_trait ->
       if Type_path.equal inner_trait trait then expr
       else Cexpr_as { expr; trait; obj_type; loc_ = loc }
   | _ -> Cexpr_as { expr; trait; obj_type; loc_ = loc }
    : expr)

let function_ ?(loc = ghost_loc_) ~ty ~is_async (params : param list)
    (body : expr) =
  (Cexpr_function
     { func = { params; body; is_async }; ty; is_raw_ = false; loc_ = loc }
    : expr)

let raw_function ?(loc = ghost_loc_) ~ty ~is_async (params : param list)
    (body : expr) =
  (Cexpr_function
     { func = { params; body; is_async }; ty; is_raw_ = true; loc_ = loc }
    : expr)

let record ?(loc = ghost_loc_) ~ty (fields : field_def list) =
  (Cexpr_record { fields; ty; loc_ = loc } : expr)

let record_update ?(loc = ghost_loc_) record fields fields_num =
  (Cexpr_record_update
     { record; fields; fields_num; ty = type_of_expr record; loc_ = loc }
    : expr)

let mutate ?(loc = ghost_loc_) ~pos record label field =
  (Cexpr_mutate { record; label; field; pos; ty = Stype.unit; loc_ = loc }
    : expr)

let fn params body = ({ params; body; is_async = false } : fn)

exception Exit

let rec never_return (e : expr) (ty : Stype.t) =
  (let go e = never_return e ty [@@inline] in
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
   | Cexpr_if t -> (
       match t.ifnot with
       | None -> raise_notrace Exit
       | Some ifnot ->
           if_ ~loc:t.loc_ ~ifso:(go t.ifso) ~ifnot:(go ifnot) t.cond)
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
   | Cexpr_loop _ | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ | Cexpr_prim _
   | Cexpr_and _ | Cexpr_or _ | Cexpr_function _ | Cexpr_apply _
   | Cexpr_constr _ | Cexpr_tuple _ | Cexpr_record _ | Cexpr_record_update _
   | Cexpr_field _ | Cexpr_mutate _ | Cexpr_array _ | Cexpr_assign _
   | Cexpr_as _ | Cexpr_handle_error _ ->
       raise_notrace Exit
   | Cexpr_continue { args; ty = _; label; loc_ } ->
       Cexpr_continue { args; ty; label; loc_ }
   | Cexpr_break { arg; ty = _; label; loc_ } ->
       Cexpr_break { arg; ty; label; loc_ }
   | Cexpr_return { expr; ty = _; loc_; return_kind } ->
       Cexpr_return { expr; loc_; ty; return_kind }
    : expr)

and sequence2 ?(loc = ghost_loc_) (expr1 : expr) (expr2 : expr) =
  (let ty2 = type_of_expr expr2 in
   try never_return expr1 ty2
   with Exit -> (
     match expr1 with
     | Cexpr_unit _ -> expr2
     | _ -> (
         match expr2 with
         | Cexpr_sequence { exprs; last_expr; ty; loc_ } ->
             Cexpr_sequence { exprs = expr1 :: exprs; last_expr; ty; loc_ }
         | _ ->
             Cexpr_sequence
               { exprs = [ expr1 ]; last_expr = expr2; ty = ty2; loc_ = loc }))
    : expr)

and sequence ?(loc = ghost_loc_) (exprs : expr list) (last_expr : expr) =
  (let ty2 = type_of_expr last_expr in
   let make_sequence rev_exprs last_expr =
     if rev_exprs = [] then last_expr
     else
       match last_expr with
       | Cexpr_sequence { exprs = exprs2; last_expr; ty; loc_ = loc2 } ->
           let loc_ = Rloc.merge loc loc2 in
           Cexpr_sequence
             { exprs = List.rev_append rev_exprs exprs2; last_expr; ty; loc_ }
       | _ ->
           Cexpr_sequence
             { exprs = List.rev rev_exprs; last_expr; ty = ty2; loc_ = loc }
       [@@inline]
   in
   let rec loop last_expr rev_exprs = function
     | expr1 :: remain -> (
         match never_return expr1 ty2 with
         | exception Exit -> (
             match expr1 with
             | Cexpr_unit _ -> loop last_expr rev_exprs remain
             | _ -> loop last_expr (expr1 :: rev_exprs) remain)
         | expr1 -> make_sequence rev_exprs expr1)
     | [] -> make_sequence rev_exprs last_expr
   in
   loop last_expr [] exprs
    : expr)

let constr ?(loc = ghost_loc_) ~ty (tag : constr_tag) (args : expr list) =
  (Cexpr_constr { tag; args; ty; loc_ = loc } : expr)

let tuple ?(loc = ghost_loc_) ~ty (exprs : expr list) =
  (Cexpr_tuple { exprs; ty; loc_ = loc } : expr)

let array ?(loc = ghost_loc_) ~ty (exprs : expr list) =
  (Cexpr_array { exprs; ty; loc_ = loc } : expr)

let assign ?(loc = ghost_loc_) (var : Ident.t) (expr : expr) =
  (Cexpr_assign { var; expr; ty = Stype.unit; loc_ = loc } : expr)

let loop ~loc (params : param list) (body : expr) (args : expr list) label =
  (Cexpr_loop { params; body; args; ty = type_of_expr body; label; loc_ = loc }
    : expr)

let break ~loc_ arg label ty = (Cexpr_break { arg; ty; label; loc_ } : expr)

let continue ?(loc = ghost_loc_) args label ty =
  (Cexpr_continue { args; ty; label; loc_ = loc } : expr)

let apply ?(loc = ghost_loc_) ~ty ?(ty_args_ = [||]) ?(prim = None) ~kind
    (func : Ident.t) (args : expr list) =
  (Cexpr_apply { func; args; kind; ty; ty_args_; prim; loc_ = loc } : expr)

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
   | Cexpr_loop t -> loop ~loc:t.loc_ t.params (go t.body) t.args t.label
   | Cexpr_and { lhs; rhs; loc_ } -> and_ ~loc:loc_ lhs (go rhs)
   | Cexpr_or { lhs; rhs; loc_ } -> or_ ~loc:loc_ lhs (go rhs)
   | Cexpr_return _ | Cexpr_break _ | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _
   | Cexpr_prim _ | Cexpr_function _ | Cexpr_apply _ | Cexpr_constr _
   | Cexpr_tuple _ | Cexpr_record _ | Cexpr_record_update _ | Cexpr_field _
   | Cexpr_mutate _ | Cexpr_array _ | Cexpr_assign _ | Cexpr_continue _
   | Cexpr_as _ | Cexpr_handle_error _ ->
       f e
    : expr)

exception Not_all_tuple_tail

let rec tuple_map (e : expr) ~ty ~join =
  (let go e = tuple_map e ~ty ~join [@@inline] in
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
   | Cexpr_loop t -> loop ~loc:t.loc_ t.params (go t.body) t.args t.label
   | Cexpr_tuple { exprs; ty = _; loc_ = loc } -> join_apply ~loc ~ty join exprs
   | Cexpr_return t -> go t.expr
   | Cexpr_break _ | Cexpr_const _ | Cexpr_unit _ | Cexpr_var _ | Cexpr_prim _
   | Cexpr_and _ | Cexpr_or _ | Cexpr_function _ | Cexpr_apply _
   | Cexpr_constr _ | Cexpr_record _ | Cexpr_record_update _ | Cexpr_field _
   | Cexpr_mutate _ | Cexpr_array _ | Cexpr_assign _ | Cexpr_continue _
   | Cexpr_as _ | Cexpr_handle_error _ ->
       raise_notrace Not_all_tuple_tail
    : expr)

let tuple_map (e : expr) ~join_ty ~join ~ok ~err =
  try ok (tuple_map e ~ty:join_ty ~join) with Not_all_tuple_tail -> err ()

let joinlet_tail ?(loc = ghost_loc_) (name : Ident.t) (params : param list)
    (join_body : expr) (body : expr) =
  (let default () =
     (letfn ~loc name (fn params join_body) ~kind:Tail_join body : expr)
       [@@inline]
   in
   match (body : expr) with
   | Cexpr_apply { kind = Join; func; args; ty = _; prim = _ }
     when Ident.equal name func ->
       Lst.fold_right2 params args join_body (fun param ->
           fun arg -> fun body -> let_ param.binder arg body)
   | _ -> (
       match (params, join_body) with
       | [], (Cexpr_const _ | Cexpr_unit _) ->
           tail_map body (fun tail_expr ->
               match tail_expr with
               | Cexpr_apply { func; _ } when Ident.equal func name -> join_body
               | _ -> tail_expr)
       | [], _ -> (
           let num_shots = ref 0 in
           try
             tail_map body (fun tail_expr ->
                 match tail_expr with
                 | Cexpr_apply { func; _ } when Ident.equal func name ->
                     incr num_shots;
                     if !num_shots = 1 then join_body else raise_notrace Exit
                 | _ -> tail_expr)
           with Exit -> default ())
       | _, _ -> default ())
    : expr)

let rec if_with_commute ?loc ~ifso ?ifnot cond =
  (match cond with
   | Cexpr_if { cond = cond1; ifso = ifso1; ifnot = ifnot1; ty = _ } -> (
       let exception Exit in
       let tail_map_branch expr =
         tail_map expr (fun tail_expr ->
             match tail_expr with
             | Cexpr_const { c = C_bool true; _ } -> ifso
             | Cexpr_const { c = C_bool false; _ } -> (
                 match ifnot with Some ifnot -> ifnot | None -> unit ?loc ())
             | _ -> raise_notrace Exit)
       in
       try
         let ifso2 = tail_map_branch ifso1 in
         let ifnot2 =
           match ifnot1 with
           | Some ifnot1 -> Some (tail_map_branch ifnot1)
           | None -> None
         in
         if_ cond1 ~ifso:ifso2 ?ifnot:ifnot2 ?loc
       with Exit -> if_ cond ~ifso ?ifnot ?loc)
   | Cexpr_let { name; rhs; body; ty = _; loc_ } ->
       let body = if_with_commute ~ifso ?ifnot body in
       let_ name rhs body ~loc:loc_
   | Cexpr_sequence { exprs; last_expr; loc_; _ } ->
       sequence ~loc:loc_ exprs (if_with_commute last_expr ?loc ~ifso ?ifnot)
   | _ -> if_ cond ~ifso ?ifnot ?loc
    : expr)

let if_ ?loc ~ifso ?ifnot cond = if_with_commute ?loc ~ifso ?ifnot cond

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

      method visit_type_path : 'a -> type_path -> type_path =
        fun _ -> fun e -> e

      method visit_tvar_env : 'a -> tvar_env -> tvar_env = fun _ -> fun e -> e

      method visit_func_stubs : 'a -> func_stubs -> func_stubs =
        fun _ -> fun e -> e

      method visit_return_kind : 'a -> return_kind -> return_kind =
        fun _ -> fun e -> e
    end

  type _unused

  include struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN]

    class virtual ['self] map =
      object (self : 'self)
        inherit [_] mapbase

        method visit_Ctop_expr : _ -> expr -> bool -> absolute_loc -> top_item =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_fis_main ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_fexpr in
               let _visitors_r1 =
                 (fun _visitors_this -> _visitors_this) _visitors_fis_main
               in
               let _visitors_r2 = self#visit_absolute_loc env _visitors_floc_ in
               Ctop_expr
                 {
                   expr = _visitors_r0;
                   is_main = _visitors_r1;
                   loc_ = _visitors_r2;
                 }

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
            bool ->
            absolute_loc ->
            top_item =
          fun env ->
            fun _visitors_fbinder ->
             fun _visitors_ffunc_stubs ->
              fun _visitors_fparams_ty ->
               fun _visitors_freturn_ty ->
                fun _visitors_fis_pub_ ->
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
                    (fun _visitors_this -> _visitors_this) _visitors_fis_pub_
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
                      is_pub_ = _visitors_r4;
                      loc_ = _visitors_r5;
                    }

        method visit_top_item : _ -> top_item -> top_item =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Ctop_expr
                 {
                   expr = _visitors_fexpr;
                   is_main = _visitors_fis_main;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_expr env _visitors_fexpr _visitors_fis_main
                   _visitors_floc_
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
                   is_pub_ = _visitors_fis_pub_;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Ctop_stub env _visitors_fbinder
                   _visitors_ffunc_stubs _visitors_fparams_ty
                   _visitors_freturn_ty _visitors_fis_pub_ _visitors_floc_

        method visit_subtop_fun_decl : _ -> subtop_fun_decl -> subtop_fun_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.fn in
             { binder = _visitors_r0; fn = _visitors_r1 }

        method visit_top_fun_decl : _ -> top_fun_decl -> top_fun_decl =
          fun env ->
            fun _visitors_this ->
             let _visitors_r0 = self#visit_binder env _visitors_this.binder in
             let _visitors_r1 = self#visit_fn env _visitors_this.func in
             let _visitors_r2 =
               (fun _visitors_this ->
                 Basic_lst.map _visitors_this (self#visit_subtop_fun_decl env))
                 _visitors_this.subtops
             in
             let _visitors_r3 =
               self#visit_tvar_env env _visitors_this.ty_params_
             in
             let _visitors_r4 =
               (fun _visitors_this -> _visitors_this) _visitors_this.is_pub_
             in
             let _visitors_r5 =
               self#visit_absolute_loc env _visitors_this.loc_
             in
             {
               binder = _visitors_r0;
               func = _visitors_r1;
               subtops = _visitors_r2;
               ty_params_ = _visitors_r3;
               is_pub_ = _visitors_r4;
               loc_ = _visitors_r5;
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
            _ -> var -> typ -> typ array -> prim option -> location -> expr =
          fun env ->
            fun _visitors_fid ->
             fun _visitors_fty ->
              fun _visitors_fty_args_ ->
               fun _visitors_fprim ->
                fun _visitors_floc_ ->
                 let _visitors_r0 = self#visit_var env _visitors_fid in
                 let _visitors_r1 = self#visit_typ env _visitors_fty in
                 let _visitors_r2 =
                   (fun _visitors_this ->
                     Basic_arr.map _visitors_this (self#visit_typ env))
                     _visitors_fty_args_
                 in
                 let _visitors_r3 =
                   (fun _visitors_this ->
                     match _visitors_this with
                     | Some t -> Some ((self#visit_prim env) t)
                     | None -> None)
                     _visitors_fprim
                 in
                 let _visitors_r4 = self#visit_location env _visitors_floc_ in
                 Cexpr_var
                   {
                     id = _visitors_r0;
                     ty = _visitors_r1;
                     ty_args_ = _visitors_r2;
                     prim = _visitors_r3;
                     loc_ = _visitors_r4;
                   }

        method visit_Cexpr_as :
            _ -> expr -> type_path -> typ -> location -> expr =
          fun env ->
            fun _visitors_fexpr ->
             fun _visitors_ftrait ->
              fun _visitors_fobj_type ->
               fun _visitors_floc_ ->
                let _visitors_r0 = self#visit_expr env _visitors_fexpr in
                let _visitors_r1 = self#visit_type_path env _visitors_ftrait in
                let _visitors_r2 = self#visit_typ env _visitors_fobj_type in
                let _visitors_r3 = self#visit_location env _visitors_floc_ in
                Cexpr_as
                  {
                    expr = _visitors_r0;
                    trait = _visitors_r1;
                    obj_type = _visitors_r2;
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

        method visit_Cexpr_apply :
            _ ->
            var ->
            expr list ->
            apply_kind ->
            typ ->
            typ array ->
            prim option ->
            location ->
            expr =
          fun env ->
            fun _visitors_ffunc ->
             fun _visitors_fargs ->
              fun _visitors_fkind ->
               fun _visitors_fty ->
                fun _visitors_fty_args_ ->
                 fun _visitors_fprim ->
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
                   let _visitors_r3 = self#visit_typ env _visitors_fty in
                   let _visitors_r4 =
                     (fun _visitors_this ->
                       Basic_arr.map _visitors_this (self#visit_typ env))
                       _visitors_fty_args_
                   in
                   let _visitors_r5 =
                     (fun _visitors_this ->
                       match _visitors_this with
                       | Some t -> Some ((self#visit_prim env) t)
                       | None -> None)
                       _visitors_fprim
                   in
                   let _visitors_r6 = self#visit_location env _visitors_floc_ in
                   Cexpr_apply
                     {
                       func = _visitors_r0;
                       args = _visitors_r1;
                       kind = _visitors_r2;
                       ty = _visitors_r3;
                       ty_args_ = _visitors_r4;
                       prim = _visitors_r5;
                       loc_ = _visitors_r6;
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
                 (fun record ->
                   fun accessor ->
                    fun pos ->
                     fun ty -> fun loc -> field ~loc record accessor ~pos ~ty)
                   _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3
                   _visitors_r4

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
                (fun expr1 ->
                  fun expr2 ->
                   fun ty -> fun loc_ -> sequence ~loc:loc_ expr1 expr2)
                  _visitors_r0 _visitors_r1 _visitors_r2 _visitors_r3

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
               (fun lhs -> fun rhs -> fun loc -> and_ ~loc lhs rhs)
                 _visitors_r0 _visitors_r1 _visitors_r2

        method visit_Cexpr_or : _ -> expr -> expr -> location -> expr =
          fun env ->
            fun _visitors_flhs ->
             fun _visitors_frhs ->
              fun _visitors_floc_ ->
               let _visitors_r0 = self#visit_expr env _visitors_flhs in
               let _visitors_r1 = self#visit_expr env _visitors_frhs in
               let _visitors_r2 = self#visit_location env _visitors_floc_ in
               (fun lhs -> fun rhs -> fun loc -> or_ ~loc lhs rhs)
                 _visitors_r0 _visitors_r1 _visitors_r2

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
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_var env _visitors_fid _visitors_fty
                   _visitors_fty_args_ _visitors_fprim _visitors_floc_
             | Cexpr_as
                 {
                   expr = _visitors_fexpr;
                   trait = _visitors_ftrait;
                   obj_type = _visitors_fobj_type;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_as env _visitors_fexpr _visitors_ftrait
                   _visitors_fobj_type _visitors_floc_
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
             | Cexpr_letrec
                 {
                   bindings = _visitors_fbindings;
                   body = _visitors_fbody;
                   ty = _visitors_fty;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_letrec env _visitors_fbindings _visitors_fbody
                   _visitors_fty _visitors_floc_
             | Cexpr_apply
                 {
                   func = _visitors_ffunc;
                   args = _visitors_fargs;
                   kind = _visitors_fkind;
                   ty = _visitors_fty;
                   ty_args_ = _visitors_fty_args_;
                   prim = _visitors_fprim;
                   loc_ = _visitors_floc_;
                 } ->
                 self#visit_Cexpr_apply env _visitors_ffunc _visitors_fargs
                   _visitors_fkind _visitors_fty _visitors_fty_args_
                   _visitors_fprim _visitors_floc_
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
             let _visitors_r2 =
               (fun _visitors_this -> _visitors_this) _visitors_this.is_async
             in
             {
               params = _visitors_r0;
               body = _visitors_r1;
               is_async = _visitors_r2;
             }

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

        method visit_Async : _ -> typ -> apply_kind =
          fun env ->
            fun _visitors_ffunc_ty ->
             let _visitors_r0 = self#visit_typ env _visitors_ffunc_ty in
             Async { func_ty = _visitors_r0 }

        method visit_Join : _ -> apply_kind = fun env -> Join

        method visit_apply_kind : _ -> apply_kind -> apply_kind =
          fun env ->
            fun _visitors_this ->
             match _visitors_this with
             | Normal { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Normal env _visitors_ffunc_ty
             | Async { func_ty = _visitors_ffunc_ty } ->
                 self#visit_Async env _visitors_ffunc_ty
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
