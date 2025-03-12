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

type printer_ctx = {
  mutable type_var_names : (Stype.t * string) list;
  mutable type_var_ctr : int;
  type_param_names : string Array.t option;
}

let make_ctx type_param_names =
  { type_var_names = []; type_var_ctr = 0; type_param_names }

let type_to_string_aux (ctx : printer_ctx) (ty : Stype.t) =
  let name_of_type_var (var : Stype.t) =
    match List.assq_opt var ctx.type_var_names with
    | Some name -> name
    | None ->
        let name = "_/" ^ Int.to_string ctx.type_var_ctr in
        ctx.type_var_ctr <- ctx.type_var_ctr + 1;
        ctx.type_var_names <- (var, name) :: ctx.type_var_names;
        name
  in
  let buf = Buffer.create 17 in
  let append s = Buffer.add_string buf s in
  let rec go (ty : Stype.t) =
    match ty with
    | Tarrow { params_ty; ret_ty; err_ty; is_async } -> (
        if is_async then append "async ";
        append "(";
        (match params_ty with
        | [] -> ()
        | ty :: [] -> go ty
        | tys -> gos ", " tys);
        append ")";
        append " -> ";
        go ret_ty;
        match err_ty with
        | None -> ()
        | Some ty ->
            append "!";
            go ty)
    | T_constr { type_constructor = Tuple _; tys } ->
        append "(";
        gos ", " tys;
        append ")"
    | T_constr { type_constructor = T_local { toplevel_id = _; name }; tys = _ }
      ->
        append name
    | T_constr { type_constructor = c; tys = args } -> (
        append (Type_path_util.name c);
        match args with
        | [] -> ()
        | tys ->
            append "[";
            gos ", " tys;
            append "]")
    | Tvar { contents = Tnolink _ } -> append (name_of_type_var ty)
    | Tvar { contents = Tlink ty' } -> go ty'
    | Tparam { index; name_ } -> (
        match ctx.type_param_names with
        | None -> append name_
        | Some names -> append names.(index))
    | T_trait trait ->
        append "&";
        append (Type_path_util.name trait)
    | T_builtin T_unit -> append "Unit"
    | T_builtin T_bool -> append "Bool"
    | T_builtin T_byte -> append "Byte"
    | T_builtin T_int16 -> append "Int16"
    | T_builtin T_uint16 -> append "UInt16"
    | T_builtin T_char -> append "Char"
    | T_builtin T_int -> append "Int"
    | T_builtin T_int64 -> append "Int64"
    | T_builtin T_uint -> append "UInt"
    | T_builtin T_uint64 -> append "UInt64"
    | T_builtin T_float -> append "Float"
    | T_builtin T_double -> append "Double"
    | T_builtin T_string -> append "String"
    | T_builtin T_bytes -> append "Bytes"
    | T_blackhole -> append "?"
  and gos sep = function
    | [] -> ()
    | ty :: [] -> go ty
    | ty :: tys ->
        go ty;
        append sep;
        gos sep tys
  in
  go ty;
  Buffer.contents buf

let type_to_string ?type_param_names typ =
  let ctx = make_ctx type_param_names in
  type_to_string_aux ctx typ

let type_pair_to_string typ1 typ2 =
  let ctx = make_ctx None in
  (type_to_string_aux ctx typ1, type_to_string_aux ctx typ2)

let toplevel_function_type_to_string ~arity typ =
  let ctx = make_ctx None in
  match (typ : Stype.t) with
  | Tarrow { params_ty; ret_ty; err_ty; is_async } ->
      let buf = Buffer.create 17 in
      let is_first = ref true in
      if is_async then Buffer.add_string buf "async ";
      Buffer.add_char buf '(';
      Fn_arity.iter2 arity params_ty (fun kind ->
          fun ty ->
           if !is_first then is_first := false else Buffer.add_string buf ", ";
           match kind with
           | Positional _ -> Buffer.add_string buf (type_to_string_aux ctx ty)
           | Labelled { label; _ } ->
               Buffer.add_string buf
                 ((label ^ "~ : " : Stdlib.String.t) [@merlin.hide]);
               Buffer.add_string buf (type_to_string_aux ctx ty)
           | Optional { label; _ } ->
               Buffer.add_string buf
                 ((label ^ "~ : " : Stdlib.String.t) [@merlin.hide]);
               Buffer.add_string buf (type_to_string_aux ctx ty);
               Buffer.add_string buf " = .."
           | Autofill { label } ->
               Buffer.add_string buf
                 ((label ^ "~ : " : Stdlib.String.t) [@merlin.hide]);
               Buffer.add_string buf (type_to_string_aux ctx ty);
               Buffer.add_string buf " = _"
           | Question_optional { label } ->
               Buffer.add_string buf
                 ((label ^ "? : " : Stdlib.String.t) [@merlin.hide]);
               Buffer.add_string buf (type_to_string_aux ctx ty));
      Buffer.add_string buf ") -> ";
      Buffer.add_string buf (type_to_string_aux ctx ret_ty);
      (match err_ty with
      | None -> ()
      | Some ty ->
          Buffer.add_string buf "!";
          Buffer.add_string buf (type_to_string_aux ctx ty));
      Buffer.contents buf
  | typ -> type_to_string_aux ctx typ

let tvar_env_to_string (env : Tvar_env.t) =
  if Tvar_env.is_empty env then ""
  else
    let buf = Buffer.create 17 in
    Buffer.add_char buf '[';
    Tvar_env.iteri env (fun i ->
        fun tvar_info ->
         if i > 0 then Buffer.add_string buf ", ";
         Buffer.add_string buf tvar_info.name;
         Lst.iteri tvar_info.constraints ~f:(fun i ->
             fun c ->
              if i = 0 then Buffer.add_string buf " : "
              else Buffer.add_string buf " + ";
              Buffer.add_string buf (Type_path_util.name c.trait)));
    Buffer.add_char buf ']';
    Buffer.contents buf
