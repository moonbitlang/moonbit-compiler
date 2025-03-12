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


module StringMap = Basic_map_string
module Syntax = Parsing_syntax
module Lst = Basic_lst

let mk_diag_emitter ~(host_type : string) ~(trait_name : Basic_longident.t)
    (diag : Local_diagnostics.t) (msg : string) (loc : Rloc.t) =
  let report =
    Errors.cannot_derive ~tycon:host_type ~trait:trait_name ~reason:msg ~loc
  in
  Local_diagnostics.add_error diag report

let deny_all_args ~host_type ~trait_name diag
    (directive : Syntax.deriving_directive) =
  if directive.args <> [] then (
    mk_diag_emitter ~host_type ~trait_name diag
      (Basic_longident.to_string trait_name ^ " does not accept any arguments."
        : Stdlib.String.t)
      directive.loc_;
    true)
  else false

let extract_string (expr : Syntax.expr) =
  match expr with
  | Syntax.Pexpr_constant { c = Syntax.Const_string s; _ } -> Ok s.string_val
  | Syntax.Pexpr_multiline_string { elems; _ } ->
      if
        List.for_all
          (fun elem ->
            match elem with Syntax.Multiline_string _ -> true | _ -> false)
          elems
      then
        Ok
          (String.concat "\n"
             (Lst.map elems (fun elem ->
                  match elem with Syntax.Multiline_string s -> s | _ -> "")))
      else Error "Interpolation is not allowed in this position"
  | Syntax.Pexpr_interp _ ->
      Error "Interpolation is not allowed in this position"
  | _ -> Error "Expected a string"

let extract_constant (expr : Syntax.expr) =
  match expr with
  | Syntax.Pexpr_constant { c; _ } -> Ok c
  | _ -> (
      match extract_string expr with
      | Ok s -> Ok (Syntax.Const_string { string_val = s; string_repr = s })
      | Error _ -> Error "Not a constant value")

let extract_string_interp (expr : Syntax.expr) =
  match expr with
  | Syntax.Pexpr_constant { c = Syntax.Const_string s; loc_ } ->
      Ok
        [ Syntax.Interp_lit { str = s.string_val; repr = s.string_repr; loc_ } ]
  | Syntax.Pexpr_interp { elems; _ } -> Ok elems
  | Syntax.Pexpr_multiline_string { elems; _ } ->
      Ok
        (Lst.concat
           (Lst.map elems (fun elem ->
                match elem with
                | Syntax.Multiline_string s ->
                    [
                      Syntax.Interp_lit
                        { str = s; repr = s; loc_ = Rloc.no_location };
                    ]
                | Syntax.Multiline_interp interp -> interp)))
  | _ -> Error "Expected a string or an interpolated string"

type meta_item_value =
  | Nothing
  | Value of Syntax.expr
  | Arguments of Syntax.argument list

type meta_item = {
  label : string option;
  value : meta_item_value;
  loc_ : Rloc.t;
}

exception MetaParseError of string * Rloc.t
exception MetaParseErrorNoEmit

let item_label (item : meta_item) = item.label

let item_label_is (label : string) (item : meta_item) =
  match item.label with Some lbl -> lbl = label | None -> false

let item_is_positional (item : meta_item) = item.label = None

let item_no_value_exn (item : meta_item) =
  match item.value with
  | Nothing -> ()
  | _ -> raise (MetaParseError ("Expected no value", item.loc_))

let item_value_expr_exn (item : meta_item) =
  match item.value with
  | Value expr -> expr
  | _ -> raise (MetaParseError ("Expected an expression", item.loc_))

let item_value_args_exn (item : meta_item) =
  match item.value with
  | Arguments args -> args
  | _ -> raise (MetaParseError ("Expected arguments", item.loc_))

let item_string_exn (item : meta_item) =
  match extract_string (item_value_expr_exn item) with
  | Ok s -> s
  | Error msg -> raise (MetaParseError (msg, item.loc_))

let item_string_interp_exn (item : meta_item) =
  match extract_string_interp (item_value_expr_exn item) with
  | Ok interp -> interp
  | Error msg -> raise (MetaParseError (msg, item.loc_))

let item_constant_exn (item : meta_item) =
  match extract_constant (item_value_expr_exn item) with
  | Ok c -> c
  | Error msg -> raise (MetaParseError (msg, item.loc_))

let fail_item (item : meta_item) msg = raise (MetaParseError (msg, item.loc_))

let parse_single (arg : Syntax.argument) =
  match arg.arg_kind with
  | Syntax.Positional -> (
      match arg.arg_value with
      | Syntax.Pexpr_ident
          { id = { var_name = Basic_longident.Lident id; _ }; loc_ }
      | Syntax.Pexpr_constr
          { constr = { constr_name = { name = id; _ }; _ }; loc_ } ->
          { label = Some id; value = Nothing; loc_ }
      | Syntax.Pexpr_ident { id = { var_name = id; _ }; loc_ } ->
          raise
            (MetaParseError
               ( (("Non-local identifier "
                   ^ Basic_longident.to_string id
                   ^ " not supported"
                   : Stdlib.String.t)
                   [@merlin.hide]),
                 loc_ ))
      | Syntax.Pexpr_apply
          {
            func =
              Syntax.Pexpr_ident
                { id = { var_name = Basic_longident.Lident id; _ }; _ };
            args;
            loc_;
            _;
          }
      | Syntax.Pexpr_apply
          {
            func =
              Syntax.Pexpr_constr
                { constr = { constr_name = { name = id; _ }; _ }; _ };
            args;
            loc_;
            _;
          } ->
          { label = Some id; value = Arguments args; loc_ }
      | Syntax.Pexpr_apply { loc_; _ } ->
          raise
            (MetaParseError
               ( "Unexpected left-hand side of labelled meta arguments, should \
                  be an identifier",
                 loc_ ))
      | expr ->
          {
            label = None;
            value = Value expr;
            loc_ = Syntax.loc_of_expression expr;
          })
  | Syntax.Labelled lbl | Syntax.Labelled_option { label = lbl; _ } ->
      {
        label = Some lbl.label_name;
        value = Value arg.arg_value;
        loc_ = Syntax.loc_of_expression arg.arg_value;
      }
  | Syntax.Labelled_pun _ | Syntax.Labelled_option_pun _ ->
      raise
        (MetaParseError
           ( "Labelled pun arguments are not allowed",
             Syntax.loc_of_expression arg.arg_value ))

let parse (args : Syntax.argument list) (emit_diag : string -> Rloc.t -> unit)
    (on_item : meta_item -> unit) =
  (let has_err =
     List.fold_left
       (fun has_err ->
         fun it ->
          try
            let item = parse_single it in
            on_item item;
            has_err
          with
          | MetaParseErrorNoEmit -> true
          | MetaParseError (msg, loc) ->
              emit_diag msg loc;
              true)
       false args
   in
   if has_err then raise MetaParseErrorNoEmit
    : unit)

let parse_fold (args : Syntax.argument list)
    (emit_diag : string -> Rloc.t -> unit) (init : 'a)
    (f : 'a -> meta_item -> 'a) =
  (let rec loop acc = function
     | [] -> acc
     | arg :: rest -> (
         try
           let item = parse_single arg in
           loop (f acc item) rest
         with
         | MetaParseErrorNoEmit -> loop acc rest
         | MetaParseError (msg, loc) ->
             emit_diag msg loc;
             loop acc rest)
   in
   loop init args
    : 'a)

let wrap_parse (f : unit -> 'a) =
  (try Some (f ()) with
   | MetaParseErrorNoEmit -> None
   | MetaParseError _ ->
       failwith
         "Unexpected error: MetaParseError should not appear outside of \
          parsing functions"
    : 'a option)
