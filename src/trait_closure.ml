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


module H = Basic_type_path.Hashset
module Lst = Basic_lst
module Vec = Basic_vec

let compute_closure ~(types : Global_env.All_types.t)
    (constraints : Tvar_env.type_constraint list) =
  let visited = H.create (List.length constraints) in
  let closure = Vec.empty () in
  Lst.iter constraints ~f:(fun c ->
      H.add visited c.trait;
      Vec.push closure c);
  let rec add_super_traits ~loc_ ~supers trait =
    let supers = trait :: supers in
    match Global_env.All_types.find_trait_by_path types trait with
    | None -> ()
    | Some trait_decl ->
        Lst.iter trait_decl.supers ~f:(fun new_trait ->
            if not (H.mem visited new_trait) then
              let src_ : Tvar_env.constraint_src =
                match supers with [] -> Direct | _ :: _ -> Super_traits supers
              in
              let new_c : Tvar_env.type_constraint =
                { trait = new_trait; loc_; src_ }
              in
              let () = H.add visited new_trait in
              let () = Vec.push closure new_c in
              add_super_traits ~loc_ ~supers new_trait)
  in
  Lst.iter constraints ~f:(fun c ->
      add_super_traits c.trait ~loc_:c.loc_ ~supers:[]);
  Vec.to_list closure
