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


module Ast = Dwarfsm_ast
module String_table = Basic_hash_string
module Lst = Basic_lst
module Encode_context = Dwarfsm_encode_context

let resolve_localidx_and_labelidx (fn : Ast.func) =
  let local_space = Encode_context.make_space () in
  let bind_local (id : Ast.binder) =
    let index = local_space.next_index in
    local_space.next_index <- local_space.next_index + 1;
    let () =
      match id.id with
      | Some name -> String_table.add local_space.map name index
      | None -> ()
    in
    id.index <- index
  in
  let curr_labels : Ast.label list ref = ref [] in
  let enter_label l f =
    let prev_labels = !curr_labels in
    curr_labels := l :: prev_labels;
    let () = f () in
    curr_labels := prev_labels
  in
  let localidx (i : Ast.localidx) =
    match i.var with
    | Unresolve var_name ->
        i.var <-
          Ast.Resolved
            { var_name; index = String_table.find_exn local_space.map var_name }
    | Resolved _ -> ()
  in
  let labelidx (i : Ast.labelidx) =
    match i.var with
    | Resolved _ -> ()
    | Unresolve name ->
        let rec aux i (l : Ast.label list) =
          match l with
          | Some name' :: _ when name = name' -> i
          | [] -> -1
          | _ :: l -> aux (i + 1) l
        in
        i.var <- Resolved { var_name = name; index = aux 0 !curr_labels }
  in
  let rec aux (instrs : Ast.instr list) =
    match instrs with
    | [] -> ()
    | instr :: instrs ->
        (match instr with
        | Br l -> labelidx l
        | Br_if l -> labelidx l
        | Br_table (ls, l) ->
            labelidx l;
            Lst.iter ls ~f:labelidx
        | Local_get x -> localidx x
        | Local_set x -> localidx x
        | Local_tee x -> localidx x
        | Loop { label; instrs; _ } -> enter_label label (fun () -> aux instrs)
        | If { label; then_; else_; _ } ->
            enter_label label (fun () ->
                aux then_;
                aux else_)
        | Block { label; instrs; _ } -> enter_label label (fun () -> aux instrs)
        | Try_table { label; catchs; instrs; _ } ->
            Lst.iter catchs ~f:(fun (catch : Ast.catch) ->
                match catch with Catch (_, i) -> labelidx i);
            enter_label label (fun () -> aux instrs)
        | _ -> ());
        aux instrs
  in
  let pts =
    match fn.type_ with Use (_, pts, _) -> pts | Inline (pts, _) -> pts
  in
  let param ({ id; _ } : Ast.param) = bind_local id in
  let local ({ id; _ } : Ast.local) = bind_local id in
  Lst.iter pts ~f:param;
  Lst.iter fn.locals ~f:local;
  aux fn.code

let resolve (module_ : Ast.module_) =
  let rec aux (mfs : Ast.modulefield list) =
    match mfs with
    | [] -> ()
    | mf :: mfs ->
        (match mf with
        | Func func -> resolve_localidx_and_labelidx func
        | _ -> ());
        aux mfs
  in
  aux module_.fields
