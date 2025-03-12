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


module Ident = Clam_ident
module Ident_set = Ident.Set
module Ident_map = Ident.Map

type usage_map = int ref Ident_map.t

let clean_usage_count (usage : usage_map) =
  (Ident_map.iter usage (fun _name -> fun count -> count := 0) : unit)

let get_impure_bindings (prog : Clam.prog) =
  (let impure_names = ref Ident_set.empty in
   let obj =
     object
       inherit [_] Clam.iter as super

       method! visit_Llet () name e body =
         if not (Check_purity.check e) then
           impure_names := Ident_set.add !impure_names name;
         super#visit_Llet () name e body
     end
   in
   obj#visit_prog () prog;
   !impure_names
    : Ident_set.t)

let clean_and_build_usage_count (prog : Clam.prog) (dead_names : Ident_set.t)
    (usage_map : usage_map ref) =
  (clean_usage_count !usage_map;
   let incr_usage var =
     match Ident_map.find_opt !usage_map var with
     | Some count -> incr count
     | None -> usage_map := Ident_map.add !usage_map var (ref 1)
   in
   let register_name var =
     match Ident_map.find_opt !usage_map var with
     | None -> usage_map := Ident_map.add !usage_map var (ref 0)
     | _ -> ()
   in
   let obj =
     object (self)
       inherit [_] Clam.iter
       method! visit_var () var = incr_usage var
       method! visit_binder () name = register_name name

       method! visit_Llet () name e body =
         if Ident_set.mem dead_names name then ()
         else (
           self#visit_binder () name;
           self#visit_lambda () e);
         self#visit_lambda () body

       method! visit_Lletrec () names fns body =
         List.iter2
           (fun name ->
             fun fn ->
              if Ident_set.mem dead_names name then ()
              else (
                self#visit_binder () name;
                self#visit_closure () fn))
           names fns;
         self#visit_lambda () body
     end
   in
   obj#visit_prog () prog
    : unit)

let update_dead_names (dead_names : Ident_set.t ref)
    (impure_names : Ident_set.t) (usage : usage_map) =
  (let old_dead_names = !dead_names in
   let update_bit = ref false in
   Ident_map.iter usage (fun name ->
       fun count ->
        if
          !count = 0
          && (not (Ident_set.mem impure_names name))
          && not (Ident_set.mem old_dead_names name)
        then (
          dead_names := Ident_set.add !dead_names name;
          update_bit := true));
   !update_bit
    : bool)

let get_unused_names (usage : usage_map) =
  (Ident_map.fold usage Ident_set.empty (fun name ->
       fun count ->
        fun acc -> if !count = 0 then Ident_set.add acc name else acc)
    : Ident_set.t)

let clean_up (prog : Clam.prog) ~(dead_names : Ident_set.t)
    ~(unused_names : Ident_set.t) =
  (let obj =
     object (self)
       inherit [_] Clam.map

       method! visit_Llet () name e body =
         let body = self#visit_lambda () body in
         if Ident_set.mem dead_names name then body
         else if Ident_set.mem unused_names name then
           Lsequence { exprs = [ self#visit_lambda () e ]; last_expr = body }
         else Llet { name; e = self#visit_lambda () e; body }

       method! visit_Lletrec () names fns body =
         let body = self#visit_lambda () body in
         let names, fns =
           Basic_lst.fold_right2 names fns ([], []) (fun name ->
               fun fn ->
                fun (acc_names, acc_fns) ->
                 if Ident_set.mem dead_names name then (acc_names, acc_fns)
                 else (name :: acc_names, fn :: acc_fns))
         in
         match (names, fns) with
         | [], [] -> body
         | _ -> Lletrec { names; fns; body }
     end
   in
   obj#visit_prog () prog
    : Clam.prog)

let print_map map =
  Ident_map.iter map (fun name ->
      fun count -> Printf.printf "%s -> %d\n" (Ident.to_string name) !count)
[@@warning "-unused-value-declaration"]

let unused_let_opt (prog : Clam.prog) =
  (let impure_names = get_impure_bindings prog in
   let usage = ref Ident_map.empty in
   let dead_names = ref Ident_set.empty in
   let update_bit = ref true in
   while !update_bit do
     clean_and_build_usage_count prog !dead_names usage;
     update_bit := update_dead_names dead_names impure_names !usage
   done;
   clean_up prog ~dead_names:!dead_names ~unused_names:(get_unused_names !usage)
    : Clam.prog)
