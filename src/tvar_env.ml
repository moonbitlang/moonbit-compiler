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


open Basic_unsafe_external
module Type_path = Basic_type_path
module Loc = Rloc

type constraint_src =
  | Super_traits of Type_path.t list
  | Static_assert of string
  | Direct

type type_constraint = {
  trait : Type_path.t;
  loc_ : Loc.t;
  src_ : constraint_src;
}

let sexp_of_type_constraint { trait } = Type_path.sexp_of_t trait

type constraints = type_constraint list

include struct
  let _ = fun (_ : constraints) -> ()

  let sexp_of_constraints =
    (fun x__001_ -> Moon_sexp_conv.sexp_of_list sexp_of_type_constraint x__001_
      : constraints -> S.t)

  let _ = sexp_of_constraints
end

type tparam_info = {
  name : string;
  typ : Stype.t;
  constraints : constraints;
  loc_ : Loc.t;
}

type t = tparam_info array

module Arr = Basic_arr
module Lst = Basic_lst

let tparam_info ~name ~typ ~constraints ~loc =
  { name; typ; constraints; loc_ = loc }

let empty : t = [||]
let find_by_index_exn (env : t) (index : int) = (env.(index) : tparam_info)

let find_by_name (env : t) (name : string) =
  (Arr.find env (fun tvar_info -> name = tvar_info.name) : tparam_info option)

let is_empty (x : t) = Arr.is_empty x [@@inline]

let make_type_subst (tvar_env : t) =
  (let len = Array.length tvar_env in
   match len with
   | 0 -> [||]
   | 1 -> [| Stype.new_type_var Tvar_normal |]
   | 2 -> [| Stype.new_type_var Tvar_normal; Stype.new_type_var Tvar_normal |]
   | 3 ->
       [|
         Stype.new_type_var Tvar_normal;
         Stype.new_type_var Tvar_normal;
         Stype.new_type_var Tvar_normal;
       |]
   | 4 ->
       [|
         Stype.new_type_var Tvar_normal;
         Stype.new_type_var Tvar_normal;
         Stype.new_type_var Tvar_normal;
         Stype.new_type_var Tvar_normal;
       |]
   | _ ->
       let res : Stype.t array =
         Array.make len (Stype.new_type_var Tvar_normal)
       in
       for i = 1 to len - 1 do
         res.!(i) <- Stype.new_type_var Tvar_normal
       done;
       res
    : Stype.t array)
[@@inline]

let to_list_map (env : t) (f : tparam_info -> 'a) =
  (Arr.to_list_f env f : 'a list)
[@@inline]

let get_types (env : t) =
  (to_list_map env (fun tvar_info -> tvar_info.typ) : Stype.t list)

let iter (env : t) (f : tparam_info -> unit) = (Arr.iter env f : unit)

let iteri (env : t) (f : int -> tparam_info -> unit) =
  (Array.iteri f env : unit)

let map (env : t) (f : tparam_info -> tparam_info) = (Arr.map env f : t)

let sexp_of_t env =
  S.List
    (to_list_map env (fun { name; typ = _; constraints } ->
         let constraints =
           match constraints with
           | [] -> []
           | _ -> [ sexp_of_constraints constraints ]
         in
         (List (List.cons (Atom name : S.t) (constraints : S.t list)) : S.t)))

let of_list_mapi (type a) (items : a list) (f : int -> a -> tparam_info) =
  (Arr.of_list_mapi items f : t)

let tvar_env_1 : t =
  [|
    { name = "A"; typ = Stype.param0; constraints = []; loc_ = Loc.no_location };
  |]

let tvar_env_2 =
  [|
    { name = "A"; typ = Stype.param0; constraints = []; loc_ = Loc.no_location };
    { name = "B"; typ = Stype.param1; constraints = []; loc_ = Loc.no_location };
  |]

let tvar_env_self : t =
  [|
    {
      name = "Self";
      typ = Stype.param_self;
      constraints = [];
      loc_ = Loc.no_location;
    };
  |]

let equal env1 env2 =
  let constraint_equal c1 c2 = Type_path.equal c1.trait c2.trait [@@inline] in
  let constraints_equal (tv1_constraints : constraints)
      (tv2_constraints : constraints) =
    Lst.for_all tv1_constraints (fun c1 ->
        Lst.exists tv2_constraints (constraint_equal c1))
    && Lst.for_all tv2_constraints (fun c2 ->
           Lst.exists tv1_constraints (constraint_equal c2))
      [@@inline]
  in
  Arr.for_all2_no_exn env1 env2 (fun tv1 ->
      fun tv2 -> constraints_equal tv1.constraints tv2.constraints)
