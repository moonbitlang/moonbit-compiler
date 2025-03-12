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


module Syntax = Parsing_syntax
module Partial_info = Parsing_partial_info

type t =
  | Import of { module_name : string; func_name : string }
  | Internal of { func_name : string }
  | Inline_code_sexp of {
      language : string; [@sexp_drop_if function "wasm" -> true | _ -> false]
      func_body : W.t; [@ceh.ignore]
    }
  | Inline_code_text of {
      language : string; [@sexp_drop_if function "wasm" -> true | _ -> false]
      func_body : string; [@ceh.ignore]
    }

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (let (drop_if__011_ : string -> Stdlib.Bool.t) = function
       | "wasm" -> true
       | _ -> false
     and (drop_if__019_ : string -> Stdlib.Bool.t) = function
       | "wasm" -> true
       | _ -> false
     in
     function
     | Import { module_name = module_name__002_; func_name = func_name__004_ }
       ->
         let bnds__001_ = ([] : _ Stdlib.List.t) in
         let bnds__001_ =
           let arg__005_ = Moon_sexp_conv.sexp_of_string func_name__004_ in
           (S.List [ S.Atom "func_name"; arg__005_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         let bnds__001_ =
           let arg__003_ = Moon_sexp_conv.sexp_of_string module_name__002_ in
           (S.List [ S.Atom "module_name"; arg__003_ ] :: bnds__001_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Import" :: bnds__001_)
     | Internal { func_name = func_name__007_ } ->
         let bnds__006_ = ([] : _ Stdlib.List.t) in
         let bnds__006_ =
           let arg__008_ = Moon_sexp_conv.sexp_of_string func_name__007_ in
           (S.List [ S.Atom "func_name"; arg__008_ ] :: bnds__006_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Internal" :: bnds__006_)
     | Inline_code_sexp
         { language = language__012_; func_body = func_body__015_ } ->
         let bnds__009_ = ([] : _ Stdlib.List.t) in
         let bnds__009_ =
           let arg__016_ = W.sexp_of_t func_body__015_ in
           (S.List [ S.Atom "func_body"; arg__016_ ] :: bnds__009_
             : _ Stdlib.List.t)
         in
         let bnds__009_ =
           if drop_if__011_ language__012_ then bnds__009_
           else
             let arg__014_ = Moon_sexp_conv.sexp_of_string language__012_ in
             let bnd__013_ = S.List [ S.Atom "language"; arg__014_ ] in
             (bnd__013_ :: bnds__009_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Inline_code_sexp" :: bnds__009_)
     | Inline_code_text
         { language = language__020_; func_body = func_body__023_ } ->
         let bnds__017_ = ([] : _ Stdlib.List.t) in
         let bnds__017_ =
           let arg__024_ = Moon_sexp_conv.sexp_of_string func_body__023_ in
           (S.List [ S.Atom "func_body"; arg__024_ ] :: bnds__017_
             : _ Stdlib.List.t)
         in
         let bnds__017_ =
           if drop_if__019_ language__020_ then bnds__017_
           else
             let arg__022_ = Moon_sexp_conv.sexp_of_string language__020_ in
             let bnd__021_ = S.List [ S.Atom "language"; arg__022_ ] in
             (bnd__021_ :: bnds__017_ : _ Stdlib.List.t)
         in
         S.List (S.Atom "Inline_code_text" :: bnds__017_)
      : t -> S.t)

  let _ = sexp_of_t

  let (hash_fold_t : Ppx_base.state -> t -> Ppx_base.state) =
    (fun hsv ->
       fun arg ->
        match arg with
        | Import _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 0 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_string hsv _ir.module_name
            in
            Ppx_base.hash_fold_string hsv _ir.func_name
        | Internal _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 1 in
            let hsv = hsv in
            Ppx_base.hash_fold_string hsv _ir.func_name
        | Inline_code_sexp _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 2 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_string hsv _ir.language
            in
            hsv
        | Inline_code_text _ir ->
            let hsv = Ppx_base.hash_fold_int hsv 3 in
            let hsv =
              let hsv = hsv in
              Ppx_base.hash_fold_string hsv _ir.language
            in
            hsv
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

  let equal =
    (fun a__025_ ->
       fun b__026_ ->
        if Stdlib.( == ) a__025_ b__026_ then true
        else
          match (a__025_, b__026_) with
          | Import _a__027_, Import _b__028_ ->
              Stdlib.( && )
                (Stdlib.( = )
                   (_a__027_.module_name : string)
                   _b__028_.module_name)
                (Stdlib.( = ) (_a__027_.func_name : string) _b__028_.func_name)
          | Import _, _ -> false
          | _, Import _ -> false
          | Internal _a__029_, Internal _b__030_ ->
              Stdlib.( = ) (_a__029_.func_name : string) _b__030_.func_name
          | Internal _, _ -> false
          | _, Internal _ -> false
          | Inline_code_sexp _a__031_, Inline_code_sexp _b__032_ ->
              Stdlib.( = ) (_a__031_.language : string) _b__032_.language
          | Inline_code_sexp _, _ -> false
          | _, Inline_code_sexp _ -> false
          | Inline_code_text _a__033_, Inline_code_text _b__034_ ->
              Stdlib.( = ) (_a__033_.language : string) _b__034_.language
      : t -> t -> bool)

  let _ = equal
end

exception Invalid_wasm_function_call of string

let wasm_linear_whitelist =
  [ "$moonbit.malloc"; "$moonbit.gc.malloc"; "$moonbit.decref" ]

let wasm_gc_whitelist = []

let check_inlinewasm (func : Dwarfsm_ast.func) =
  let whitelist =
    match !Basic_config.target with Wasm_gc -> wasm_gc_whitelist
  in
  Dwarfsm_instr_utils.iter func.code (fun instr ->
      match instr with
      | Call { var = Unresolve name } | Ref_func { var = Unresolve name } ->
          if not (Basic_lst.mem_string whitelist name) then
            raise (Invalid_wasm_function_call name)
      | _ -> ())

let from_syntax ~loc ~diagnostics (stub : Syntax.func_stubs) =
  (match stub with
   | Import { module_name; func_name } ->
       Ok
         (Import
            {
              module_name = module_name.string_val;
              func_name = func_name.string_val;
            })
   | Embedded { language; code } -> (
       let aux s =
         (if
            (match language with None -> true | _ -> false)
            && String.length s >= 1
            && s.[0] = '$'
          then Ok (Internal { func_name = s })
          else
            let language =
              match language with Some s -> s.string_val | None -> "wasm"
            in
            match language with
            | "wasm" -> (
                match Dwarfsm_sexp_parse.parse s with
                | ((List (Atom "func" :: _) : W.t) as func_body) :: [] -> (
                    match !Basic_config.target with
                    | Wasm_gc -> (
                        match Dwarfsm_parse.modulefield func_body with
                        | Dwarfsm_ast.Func func :: [] -> (
                            match check_inlinewasm func with
                            | () ->
                                Ok
                                  (Inline_code_sexp
                                     { language = "wasm"; func_body })
                            | exception Invalid_wasm_function_call name ->
                                let backend =
                                  match !Basic_config.target with
                                  | Wasm_gc -> "wasm_gc"
                                in
                                let warning_msg =
                                  (Stdlib.String.concat ""
                                     [
                                       "Inline wasm invalid function call: \
                                        function ";
                                       name;
                                       " is not on the whitelist of the ";
                                       backend;
                                       " backend";
                                     ] [@merlin.hide])
                                in
                                Diagnostics.add_warning diagnostics
                                  {
                                    loc;
                                    kind = Invalid_inline_wasm warning_msg;
                                  };
                                Ok
                                  (Inline_code_sexp
                                     { language = "wasm"; func_body }))
                        | exception exn ->
                            let warning_msg =
                              (("Invalid inline wasm: " ^ Printexc.to_string exn
                                : Stdlib.String.t)
                                [@merlin.hide])
                            in
                            Diagnostics.add_warning diagnostics
                              { loc; kind = Invalid_inline_wasm warning_msg };
                            Ok
                              (Inline_code_sexp { language = "wasm"; func_body })
                        | _ -> assert false))
                | _ -> Ok (Internal { func_name = s })
                | exception Dwarfsm_sexp_parse_error.Unmatched_parenthesis (l, r)
                  ->
                    let loc_inside_wasm = Loc.of_menhir (l, r) in
                    let message = "unmatched parenthesis" in
                    Partial
                      ( Internal { func_name = s },
                        [
                          Errors.inline_wasm_syntax_error ~loc_inside_wasm ~loc
                            ~message;
                        ] )
                | exception Wasm_lex.Syntax ({ left; right }, message) ->
                    let loc_inside_wasm =
                      Loc.of_menhir
                        ( Lexing.of_stdlib_position left,
                          Lexing.of_stdlib_position right )
                    in
                    Partial
                      ( Internal { func_name = s },
                        [
                          Errors.inline_wasm_syntax_error ~loc_inside_wasm ~loc
                            ~message;
                        ] ))
            | "C" | "c" ->
                let all_chars_valid =
                  String.for_all
                    (fun c ->
                      ('0' <= c && c <= '9')
                      || ('a' <= c && c <= 'z')
                      || ('A' <= c && c <= 'Z')
                      || c = '_' || c = '$')
                    s
                in
                let stub = Inline_code_text { language; func_body = s } in
                if
                  all_chars_valid
                  && String.length s > 1
                  && not ('0' <= s.[0] && s.[0] <= '9')
                then Ok stub
                else Partial (stub, [ Errors.c_stub_invalid_function_name loc ])
            | language -> Ok (Inline_code_text { language; func_body = s })
           : _ Partial_info.t)
       in
       match code with
       | Code_string s -> aux s.string_val
       | Code_multiline_string xs -> aux (String.concat " " xs))
    : t Partial_info.t)
