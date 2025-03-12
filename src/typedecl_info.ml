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


module Type_path = Basic_type_path
module Constr_info = Basic_constr_info
module Lst = Basic_lst

type typ = Stype.t

include struct
  let _ = fun (_ : typ) -> ()
  let sexp_of_typ = (Stype.sexp_of_t : typ -> S.t)
  let _ = sexp_of_typ
end

type constr_tag = Constr_info.constr_tag

include struct
  let _ = fun (_ : constr_tag) -> ()
  let sexp_of_constr_tag = (Constr_info.sexp_of_constr_tag : constr_tag -> S.t)
  let _ = sexp_of_constr_tag
end

type path = Type_path.t

include struct
  let _ = fun (_ : path) -> ()
  let sexp_of_path = (Type_path.sexp_of_t : path -> S.t)
  let _ = sexp_of_path
end

type location = Loc.t

include struct
  let _ = fun (_ : location) -> ()
  let sexp_of_location = (Loc.sexp_of_t : location -> S.t)
  let _ = sexp_of_location
end

type fn_arity = Fn_arity.t

include struct
  let _ = fun (_ : fn_arity) -> ()
  let sexp_of_fn_arity = (Fn_arity.sexp_of_t : fn_arity -> S.t)
  let _ = sexp_of_fn_arity
end

type tvar_env = Tvar_env.t

include struct
  let _ = fun (_ : tvar_env) -> ()
  let sexp_of_tvar_env = (Tvar_env.sexp_of_t : tvar_env -> S.t)
  let _ = sexp_of_tvar_env
end

type docstring = Docstring.t

include struct
  let _ = fun (_ : docstring) -> ()
  let sexp_of_docstring = (Docstring.sexp_of_t : docstring -> S.t)
  let _ = sexp_of_docstring
end

type attributes = Checked_attributes.t

include struct
  let _ = fun (_ : attributes) -> ()
  let sexp_of_attributes = (Checked_attributes.sexp_of_t : attributes -> S.t)
  let _ = sexp_of_attributes
end

type visibility = Vis_priv | Vis_default | Vis_readonly | Vis_fully_pub

include struct
  let _ = fun (_ : visibility) -> ()

  let sexp_of_visibility =
    (function
     | Vis_priv -> S.Atom "Vis_priv"
     | Vis_default -> S.Atom "Vis_default"
     | Vis_readonly -> S.Atom "Vis_readonly"
     | Vis_fully_pub -> S.Atom "Vis_fully_pub"
      : visibility -> S.t)

  let _ = sexp_of_visibility
end

let vis_is_pub vis =
  match vis with
  | Vis_priv | Vis_default -> false
  | Vis_readonly | Vis_fully_pub -> true

let hide_loc _ = not !Basic_config.show_loc

class ['a] iterbase =
  object (self)
    method visit_typ : 'a -> typ -> unit = fun _ -> fun _ -> ()

    method private visit_type_constraint :
        'a -> Tvar_env.type_constraint -> unit =
      fun _ -> fun _ -> ()

    method private visit_constr_tag : 'a -> Constr_info.constr_tag -> unit =
      fun _ -> fun _ -> ()

    method private visit_tvar_env : 'a -> tvar_env -> unit =
      fun ctx ->
        fun env ->
         Tvar_env.iter env (fun tvar_info ->
             self#visit_typ ctx tvar_info.typ;
             Lst.iter tvar_info.constraints ~f:(self#visit_type_constraint ctx))
  end

type t = {
  ty_constr : path;
  ty_arity : int;
  ty_desc : type_components;
  ty_vis : visibility; [@sexp_drop_if fun vis -> vis = Vis_default]
  ty_params_ : tvar_env;
  ty_loc_ : location;
  ty_doc_ : docstring; [@sexp_drop_if Docstring.is_empty]
  ty_attrs : attributes;
  ty_is_only_tag_enum_ : bool; [@sexp_drop_if fun x -> x = false]
  ty_is_suberror_ : bool; [@sexp_drop_if fun x -> x = false]
}

and constructors = constructor list
and fields = field list

and newtype_info = {
  newtype_constr : constructor;
  underlying_typ : typ;
  recursive : bool; [@sexp_drop_if fun recur -> not recur]
}

and type_components =
  | Extern_type
  | Abstract_type
  | Error_type of constructor
  | ErrorEnum_type of constructor list
  | New_type of newtype_info
  | Variant_type of constructor list
  | Record_type of { fields : field list; has_private_field_ : bool }

and type_component_visibility = Invisible | Readable | Read_write

and constructor = {
  constr_name : string;
  cs_args : typ list;
  cs_res : typ;
  cs_tag : constr_tag;
  cs_vis : type_component_visibility; [@sexp_drop_if fun vis -> vis = Invisible]
  cs_ty_params_ : tvar_env;
  cs_arity_ : fn_arity; [@sexp_drop_if Fn_arity.is_simple]
  cs_constr_loc_ : location; [@sexp_drop_if hide_loc]
  cs_loc_ : location; [@sexp_drop_if hide_loc]
}

and field = {
  field_name : string;
  pos : int;
  ty_field : typ;
  ty_record : typ;
  mut : bool;
  vis : type_component_visibility; [@sexp_drop_if fun vis -> vis = Invisible]
  all_labels : string list;
  ty_params_ : tvar_env;
  label_loc_ : location; [@sexp_drop_if hide_loc]
  loc_ : location; [@sexp_drop_if hide_loc]
}

include struct
  [@@@ocaml.warning "-4-26-27"]
  [@@@VISITORS.BEGIN]

  class virtual ['self] iter =
    object (self : 'self)
      inherit [_] iterbase

      method visit_t : _ -> t -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this -> ()) _visitors_this.ty_constr
           in
           let _visitors_r1 =
             (fun _visitors_this -> ()) _visitors_this.ty_arity
           in
           let _visitors_r2 =
             self#visit_type_components env _visitors_this.ty_desc
           in
           let _visitors_r3 =
             (fun _visitors_this -> ()) _visitors_this.ty_vis
           in
           let _visitors_r4 =
             self#visit_tvar_env env _visitors_this.ty_params_
           in
           let _visitors_r5 =
             (fun _visitors_this -> ()) _visitors_this.ty_loc_
           in
           let _visitors_r6 =
             (fun _visitors_this -> ()) _visitors_this.ty_doc_
           in
           let _visitors_r7 =
             (fun _visitors_this -> ()) _visitors_this.ty_attrs
           in
           let _visitors_r8 =
             (fun _visitors_this -> ()) _visitors_this.ty_is_only_tag_enum_
           in
           let _visitors_r9 =
             (fun _visitors_this -> ()) _visitors_this.ty_is_suberror_
           in
           ()

      method private visit_constructors : _ -> constructors -> unit =
        fun env ->
          fun _visitors_this ->
           Basic_lst.iter _visitors_this ~f:(self#visit_constructor env)

      method private visit_fields : _ -> fields -> unit =
        fun env ->
          fun _visitors_this ->
           Basic_lst.iter _visitors_this ~f:(self#visit_field env)

      method private visit_newtype_info : _ -> newtype_info -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             self#visit_constructor env _visitors_this.newtype_constr
           in
           let _visitors_r1 =
             self#visit_typ env _visitors_this.underlying_typ
           in
           let _visitors_r2 =
             (fun _visitors_this -> ()) _visitors_this.recursive
           in
           ()

      method private visit_Extern_type : _ -> unit = fun env -> ()
      method private visit_Abstract_type : _ -> unit = fun env -> ()

      method private visit_Error_type : _ -> constructor -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_constructor env _visitors_c0 in
           ()

      method private visit_ErrorEnum_type : _ -> constructor list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_constructor env))
               _visitors_c0
           in
           ()

      method private visit_New_type : _ -> newtype_info -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 = self#visit_newtype_info env _visitors_c0 in
           ()

      method private visit_Variant_type : _ -> constructor list -> unit =
        fun env ->
          fun _visitors_c0 ->
           let _visitors_r0 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_constructor env))
               _visitors_c0
           in
           ()

      method private visit_Record_type : _ -> field list -> bool -> unit =
        fun env ->
          fun _visitors_ffields ->
           fun _visitors_fhas_private_field_ ->
            let _visitors_r0 =
              (fun _visitors_this ->
                Basic_lst.iter _visitors_this ~f:(self#visit_field env))
                _visitors_ffields
            in
            let _visitors_r1 =
              (fun _visitors_this -> ()) _visitors_fhas_private_field_
            in
            ()

      method private visit_type_components : _ -> type_components -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Extern_type -> self#visit_Extern_type env
           | Abstract_type -> self#visit_Abstract_type env
           | Error_type _visitors_c0 -> self#visit_Error_type env _visitors_c0
           | ErrorEnum_type _visitors_c0 ->
               self#visit_ErrorEnum_type env _visitors_c0
           | New_type _visitors_c0 -> self#visit_New_type env _visitors_c0
           | Variant_type _visitors_c0 ->
               self#visit_Variant_type env _visitors_c0
           | Record_type
               {
                 fields = _visitors_ffields;
                 has_private_field_ = _visitors_fhas_private_field_;
               } ->
               self#visit_Record_type env _visitors_ffields
                 _visitors_fhas_private_field_

      method private visit_Invisible : _ -> unit = fun env -> ()
      method private visit_Readable : _ -> unit = fun env -> ()
      method private visit_Read_write : _ -> unit = fun env -> ()

      method private visit_type_component_visibility :
          _ -> type_component_visibility -> unit =
        fun env ->
          fun _visitors_this ->
           match _visitors_this with
           | Invisible -> self#visit_Invisible env
           | Readable -> self#visit_Readable env
           | Read_write -> self#visit_Read_write env

      method private visit_constructor : _ -> constructor -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this -> ()) _visitors_this.constr_name
           in
           let _visitors_r1 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(self#visit_typ env))
               _visitors_this.cs_args
           in
           let _visitors_r2 = self#visit_typ env _visitors_this.cs_res in
           let _visitors_r3 = self#visit_constr_tag env _visitors_this.cs_tag in
           let _visitors_r4 =
             self#visit_type_component_visibility env _visitors_this.cs_vis
           in
           let _visitors_r5 =
             self#visit_tvar_env env _visitors_this.cs_ty_params_
           in
           let _visitors_r6 =
             (fun _visitors_this -> ()) _visitors_this.cs_arity_
           in
           let _visitors_r7 =
             (fun _visitors_this -> ()) _visitors_this.cs_constr_loc_
           in
           let _visitors_r8 =
             (fun _visitors_this -> ()) _visitors_this.cs_loc_
           in
           ()

      method private visit_field : _ -> field -> unit =
        fun env ->
          fun _visitors_this ->
           let _visitors_r0 =
             (fun _visitors_this -> ()) _visitors_this.field_name
           in
           let _visitors_r1 = (fun _visitors_this -> ()) _visitors_this.pos in
           let _visitors_r2 = self#visit_typ env _visitors_this.ty_field in
           let _visitors_r3 = self#visit_typ env _visitors_this.ty_record in
           let _visitors_r4 = (fun _visitors_this -> ()) _visitors_this.mut in
           let _visitors_r5 =
             self#visit_type_component_visibility env _visitors_this.vis
           in
           let _visitors_r6 =
             (fun _visitors_this ->
               Basic_lst.iter _visitors_this ~f:(fun _visitors_this -> ()))
               _visitors_this.all_labels
           in
           let _visitors_r7 =
             self#visit_tvar_env env _visitors_this.ty_params_
           in
           let _visitors_r8 =
             (fun _visitors_this -> ()) _visitors_this.label_loc_
           in
           let _visitors_r9 = (fun _visitors_this -> ()) _visitors_this.loc_ in
           ()
    end

  [@@@VISITORS.END]
end

include struct
  let _ = fun (_ : t) -> ()
  let _ = fun (_ : constructors) -> ()
  let _ = fun (_ : fields) -> ()
  let _ = fun (_ : newtype_info) -> ()
  let _ = fun (_ : type_components) -> ()
  let _ = fun (_ : type_component_visibility) -> ()
  let _ = fun (_ : constructor) -> ()
  let _ = fun (_ : field) -> ()

  let rec sexp_of_t =
    (let (drop_if__009_ : visibility -> Stdlib.Bool.t) =
      fun vis -> vis = Vis_default
     and (drop_if__018_ : docstring -> Stdlib.Bool.t) = Docstring.is_empty
     and (drop_if__025_ : bool -> Stdlib.Bool.t) = fun x -> x = false
     and (drop_if__030_ : bool -> Stdlib.Bool.t) = fun x -> x = false in
     fun {
           ty_constr = ty_constr__002_;
           ty_arity = ty_arity__004_;
           ty_desc = ty_desc__006_;
           ty_vis = ty_vis__010_;
           ty_params_ = ty_params___013_;
           ty_loc_ = ty_loc___015_;
           ty_doc_ = ty_doc___019_;
           ty_attrs = ty_attrs__022_;
           ty_is_only_tag_enum_ = ty_is_only_tag_enum___026_;
           ty_is_suberror_ = ty_is_suberror___031_;
         } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         if drop_if__030_ ty_is_suberror___031_ then bnds__001_
         else
           let arg__033_ = Moon_sexp_conv.sexp_of_bool ty_is_suberror___031_ in
           let bnd__032_ = S.List [ S.Atom "ty_is_suberror_"; arg__033_ ] in
           (bnd__032_ :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         if drop_if__025_ ty_is_only_tag_enum___026_ then bnds__001_
         else
           let arg__028_ =
             Moon_sexp_conv.sexp_of_bool ty_is_only_tag_enum___026_
           in
           let bnd__027_ =
             S.List [ S.Atom "ty_is_only_tag_enum_"; arg__028_ ]
           in
           (bnd__027_ :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__023_ = sexp_of_attributes ty_attrs__022_ in
         (S.List [ S.Atom "ty_attrs"; arg__023_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         if drop_if__018_ ty_doc___019_ then bnds__001_
         else
           let arg__021_ = sexp_of_docstring ty_doc___019_ in
           let bnd__020_ = S.List [ S.Atom "ty_doc_"; arg__021_ ] in
           (bnd__020_ :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__016_ = sexp_of_location ty_loc___015_ in
         (S.List [ S.Atom "ty_loc_"; arg__016_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__014_ = sexp_of_tvar_env ty_params___013_ in
         (S.List [ S.Atom "ty_params_"; arg__014_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         if drop_if__009_ ty_vis__010_ then bnds__001_
         else
           let arg__012_ = sexp_of_visibility ty_vis__010_ in
           let bnd__011_ = S.List [ S.Atom "ty_vis"; arg__012_ ] in
           (bnd__011_ :: bnds__001_ : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__007_ = sexp_of_type_components ty_desc__006_ in
         (S.List [ S.Atom "ty_desc"; arg__007_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = Moon_sexp_conv.sexp_of_int ty_arity__004_ in
         (S.List [ S.Atom "ty_arity"; arg__005_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = sexp_of_path ty_constr__002_ in
         (S.List [ S.Atom "ty_constr"; arg__003_ ] :: bnds__001_
           : _ Stdlib.List.t)
       in
       S.List bnds__001_
      : t -> S.t)

  and sexp_of_constructors =
    (fun x__034_ -> Moon_sexp_conv.sexp_of_list sexp_of_constructor x__034_
      : constructors -> S.t)

  and sexp_of_fields =
    (fun x__035_ -> Moon_sexp_conv.sexp_of_list sexp_of_field x__035_
      : fields -> S.t)

  and sexp_of_newtype_info =
    (let (drop_if__042_ : bool -> Stdlib.Bool.t) = fun recur -> not recur in
     fun {
           newtype_constr = newtype_constr__037_;
           underlying_typ = underlying_typ__039_;
           recursive = recursive__043_;
         } ->
       let bnds__036_ = ([] : _ Stdlib.List.t) in
       let bnds__036_ =
         if drop_if__042_ recursive__043_ then bnds__036_
         else
           let arg__045_ = Moon_sexp_conv.sexp_of_bool recursive__043_ in
           let bnd__044_ = S.List [ S.Atom "recursive"; arg__045_ ] in
           (bnd__044_ :: bnds__036_ : _ Stdlib.List.t)
       in
       let bnds__036_ =
         let arg__040_ = sexp_of_typ underlying_typ__039_ in
         (S.List [ S.Atom "underlying_typ"; arg__040_ ] :: bnds__036_
           : _ Stdlib.List.t)
       in
       let bnds__036_ =
         let arg__038_ = sexp_of_constructor newtype_constr__037_ in
         (S.List [ S.Atom "newtype_constr"; arg__038_ ] :: bnds__036_
           : _ Stdlib.List.t)
       in
       S.List bnds__036_
      : newtype_info -> S.t)

  and sexp_of_type_components =
    (function
     | Extern_type -> S.Atom "Extern_type"
     | Abstract_type -> S.Atom "Abstract_type"
     | Error_type arg0__046_ ->
         let res0__047_ = sexp_of_constructor arg0__046_ in
         S.List [ S.Atom "Error_type"; res0__047_ ]
     | ErrorEnum_type arg0__048_ ->
         let res0__049_ =
           Moon_sexp_conv.sexp_of_list sexp_of_constructor arg0__048_
         in
         S.List [ S.Atom "ErrorEnum_type"; res0__049_ ]
     | New_type arg0__050_ ->
         let res0__051_ = sexp_of_newtype_info arg0__050_ in
         S.List [ S.Atom "New_type"; res0__051_ ]
     | Variant_type arg0__052_ ->
         let res0__053_ =
           Moon_sexp_conv.sexp_of_list sexp_of_constructor arg0__052_
         in
         S.List [ S.Atom "Variant_type"; res0__053_ ]
     | Record_type
         {
           fields = fields__055_;
           has_private_field_ = has_private_field___057_;
         } ->
         let bnds__054_ = ([] : _ Stdlib.List.t) in
         let bnds__054_ =
           let arg__058_ =
             Moon_sexp_conv.sexp_of_bool has_private_field___057_
           in
           (S.List [ S.Atom "has_private_field_"; arg__058_ ] :: bnds__054_
             : _ Stdlib.List.t)
         in
         let bnds__054_ =
           let arg__056_ =
             Moon_sexp_conv.sexp_of_list sexp_of_field fields__055_
           in
           (S.List [ S.Atom "fields"; arg__056_ ] :: bnds__054_
             : _ Stdlib.List.t)
         in
         S.List (S.Atom "Record_type" :: bnds__054_)
      : type_components -> S.t)

  and sexp_of_type_component_visibility =
    (function
     | Invisible -> S.Atom "Invisible"
     | Readable -> S.Atom "Readable"
     | Read_write -> S.Atom "Read_write"
      : type_component_visibility -> S.t)

  and sexp_of_constructor =
    (let (drop_if__069_ : type_component_visibility -> Stdlib.Bool.t) =
      fun vis -> vis = Invisible
     and (drop_if__076_ : fn_arity -> Stdlib.Bool.t) = Fn_arity.is_simple
     and (drop_if__081_ : location -> Stdlib.Bool.t) = hide_loc
     and (drop_if__086_ : location -> Stdlib.Bool.t) = hide_loc in
     fun {
           constr_name = constr_name__060_;
           cs_args = cs_args__062_;
           cs_res = cs_res__064_;
           cs_tag = cs_tag__066_;
           cs_vis = cs_vis__070_;
           cs_ty_params_ = cs_ty_params___073_;
           cs_arity_ = cs_arity___077_;
           cs_constr_loc_ = cs_constr_loc___082_;
           cs_loc_ = cs_loc___087_;
         } ->
       let bnds__059_ = ([] : _ Stdlib.List.t) in
       let bnds__059_ =
         if drop_if__086_ cs_loc___087_ then bnds__059_
         else
           let arg__089_ = sexp_of_location cs_loc___087_ in
           let bnd__088_ = S.List [ S.Atom "cs_loc_"; arg__089_ ] in
           (bnd__088_ :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         if drop_if__081_ cs_constr_loc___082_ then bnds__059_
         else
           let arg__084_ = sexp_of_location cs_constr_loc___082_ in
           let bnd__083_ = S.List [ S.Atom "cs_constr_loc_"; arg__084_ ] in
           (bnd__083_ :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         if drop_if__076_ cs_arity___077_ then bnds__059_
         else
           let arg__079_ = sexp_of_fn_arity cs_arity___077_ in
           let bnd__078_ = S.List [ S.Atom "cs_arity_"; arg__079_ ] in
           (bnd__078_ :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         let arg__074_ = sexp_of_tvar_env cs_ty_params___073_ in
         (S.List [ S.Atom "cs_ty_params_"; arg__074_ ] :: bnds__059_
           : _ Stdlib.List.t)
       in
       let bnds__059_ =
         if drop_if__069_ cs_vis__070_ then bnds__059_
         else
           let arg__072_ = sexp_of_type_component_visibility cs_vis__070_ in
           let bnd__071_ = S.List [ S.Atom "cs_vis"; arg__072_ ] in
           (bnd__071_ :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         let arg__067_ = sexp_of_constr_tag cs_tag__066_ in
         (S.List [ S.Atom "cs_tag"; arg__067_ ] :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         let arg__065_ = sexp_of_typ cs_res__064_ in
         (S.List [ S.Atom "cs_res"; arg__065_ ] :: bnds__059_ : _ Stdlib.List.t)
       in
       let bnds__059_ =
         let arg__063_ =
           Moon_sexp_conv.sexp_of_list sexp_of_typ cs_args__062_
         in
         (S.List [ S.Atom "cs_args"; arg__063_ ] :: bnds__059_
           : _ Stdlib.List.t)
       in
       let bnds__059_ =
         let arg__061_ = Moon_sexp_conv.sexp_of_string constr_name__060_ in
         (S.List [ S.Atom "constr_name"; arg__061_ ] :: bnds__059_
           : _ Stdlib.List.t)
       in
       S.List bnds__059_
      : constructor -> S.t)

  and sexp_of_field =
    (let (drop_if__102_ : type_component_visibility -> Stdlib.Bool.t) =
      fun vis -> vis = Invisible
     and (drop_if__111_ : location -> Stdlib.Bool.t) = hide_loc
     and (drop_if__116_ : location -> Stdlib.Bool.t) = hide_loc in
     fun {
           field_name = field_name__091_;
           pos = pos__093_;
           ty_field = ty_field__095_;
           ty_record = ty_record__097_;
           mut = mut__099_;
           vis = vis__103_;
           all_labels = all_labels__106_;
           ty_params_ = ty_params___108_;
           label_loc_ = label_loc___112_;
           loc_ = loc___117_;
         } ->
       let bnds__090_ = ([] : _ Stdlib.List.t) in
       let bnds__090_ =
         if drop_if__116_ loc___117_ then bnds__090_
         else
           let arg__119_ = sexp_of_location loc___117_ in
           let bnd__118_ = S.List [ S.Atom "loc_"; arg__119_ ] in
           (bnd__118_ :: bnds__090_ : _ Stdlib.List.t)
       in
       let bnds__090_ =
         if drop_if__111_ label_loc___112_ then bnds__090_
         else
           let arg__114_ = sexp_of_location label_loc___112_ in
           let bnd__113_ = S.List [ S.Atom "label_loc_"; arg__114_ ] in
           (bnd__113_ :: bnds__090_ : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__109_ = sexp_of_tvar_env ty_params___108_ in
         (S.List [ S.Atom "ty_params_"; arg__109_ ] :: bnds__090_
           : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__107_ =
           Moon_sexp_conv.sexp_of_list Moon_sexp_conv.sexp_of_string
             all_labels__106_
         in
         (S.List [ S.Atom "all_labels"; arg__107_ ] :: bnds__090_
           : _ Stdlib.List.t)
       in
       let bnds__090_ =
         if drop_if__102_ vis__103_ then bnds__090_
         else
           let arg__105_ = sexp_of_type_component_visibility vis__103_ in
           let bnd__104_ = S.List [ S.Atom "vis"; arg__105_ ] in
           (bnd__104_ :: bnds__090_ : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__100_ = Moon_sexp_conv.sexp_of_bool mut__099_ in
         (S.List [ S.Atom "mut"; arg__100_ ] :: bnds__090_ : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__098_ = sexp_of_typ ty_record__097_ in
         (S.List [ S.Atom "ty_record"; arg__098_ ] :: bnds__090_
           : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__096_ = sexp_of_typ ty_field__095_ in
         (S.List [ S.Atom "ty_field"; arg__096_ ] :: bnds__090_
           : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__094_ = Moon_sexp_conv.sexp_of_int pos__093_ in
         (S.List [ S.Atom "pos"; arg__094_ ] :: bnds__090_ : _ Stdlib.List.t)
       in
       let bnds__090_ =
         let arg__092_ = Moon_sexp_conv.sexp_of_string field_name__091_ in
         (S.List [ S.Atom "field_name"; arg__092_ ] :: bnds__090_
           : _ Stdlib.List.t)
       in
       S.List bnds__090_
      : field -> S.t)

  let _ = sexp_of_t
  and _ = sexp_of_constructors
  and _ = sexp_of_fields
  and _ = sexp_of_newtype_info
  and _ = sexp_of_type_components
  and _ = sexp_of_type_component_visibility
  and _ = sexp_of_constructor
  and _ = sexp_of_field
end
