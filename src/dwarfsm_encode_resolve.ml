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
module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Hash_int = Basic_hash_int
module Ast = Dwarfsm_ast
module Encode_context = Dwarfsm_encode_context

type binder = Ast.binder
type modulefield = Ast.modulefield
type field = Ast.field
type structtype = Ast.structtype
type comptype = Ast.comptype
type subtype = Ast.subtype
type typedef = Ast.typedef
type param = Ast.param
type local = Ast.local
type module_ = Ast.module_

module Implicits (Arg : sig
  val ctx : Encode_context.context
end) =
struct
  let ctx = Arg.ctx

  let bind (space : Encode_context.space) (id : binder) =
    let index = space.next_index in
    space.next_index <- space.next_index + 1;
    let () =
      match id.id with
      | Some name -> Hash_string.add space.map name index
      | None -> ()
    in
    id.index <- index;
    index

  let bind_type = bind ctx.spaces.types

  let bind_field sti id =
    let space =
      Hash_int.find_or_update ctx.spaces.fields sti ~update:(fun _ ->
          Encode_context.make_space ())
    in
    bind space id

  let bind_local fi id =
    let space =
      Hash_int.find_or_update ctx.spaces.locals fi ~update:(fun _ ->
          Encode_context.make_space ())
    in
    bind space id

  let bind_func id = bind ctx.spaces.funcs id
  let bind_table id = bind ctx.spaces.tables id
  let bind_mem id = bind ctx.spaces.mems id
  let bind_global id = bind ctx.spaces.globals id
  let bind_elem id = bind ctx.spaces.elems id
  let bind_data id = bind ctx.spaces.datas id
  let bind_tag id = bind ctx.spaces.tags id

  let import_field (mf : modulefield) =
    match mf with
    | Import ip -> (
        Vec.push ctx.imports ip;
        match ip.desc with
        | Func (id, _) -> ignore (bind_func id)
        | Table (id, _) -> ignore (bind_table id)
        | Memory (id, _) -> ignore (bind_mem id)
        | Global (id, _) -> ignore (bind_global id)
        | Tag (id, _) -> ignore (bind_tag id))
    | _ -> ()

  let non_import_field (mf : modulefield) =
    match mf with
    | Import _ -> ()
    | Rectype rt ->
        Vec.push ctx.types rt;
        let field ~struct_type_index (field : field) =
          ignore (bind_field struct_type_index field.id)
        in
        let structtype ~index (Struct ft : structtype) =
          Basic_lst.iter ft ~f:(field ~struct_type_index:index)
        in
        let comptype ~index (ct : comptype) =
          match ct with Structtype t -> structtype ~index t | _ -> ()
        in
        let subtype ~index (st : subtype) = comptype ~index st.type_ in
        let typedef ((id, st) : typedef) =
          let index = bind_type id in
          subtype ~index st
        in
        Basic_lst.iter rt ~f:typedef
    | Func fn ->
        Vec.push ctx.funcs fn;
        let index = bind_func fn.id in
        let pts =
          match fn.type_ with Use (_, pts, _) -> pts | Inline (pts, _) -> pts
        in
        let param ~func_index ({ id; _ } : param) =
          ignore (bind_local func_index id)
        in
        let local ~func_index ({ id; _ } : local) =
          ignore (bind_local func_index id)
        in
        Basic_lst.iter pts ~f:(param ~func_index:index);
        Basic_lst.iter fn.locals ~f:(local ~func_index:index)
    | Table t ->
        Vec.push ctx.tables t;
        ignore (bind_table t.id)
    | Mem m ->
        Vec.push ctx.mems m;
        ignore (bind_mem m.id)
    | Global g ->
        Vec.push ctx.globals g;
        ignore (bind_global g.id)
    | Export ep -> Vec.push ctx.exports ep
    | Start s ->
        assert (match ctx.start with None -> true | _ -> false);
        ctx.start <- Some s
    | Elem e ->
        Vec.push ctx.elems e;
        ignore (bind_elem e.id)
    | Data d ->
        Vec.push ctx.datas d;
        ignore (bind_data d.id)
    | Tag t ->
        Vec.push ctx.tags t;
        ignore (bind_tag t.id)

  let module_ (m : module_) =
    Lst.iter m.fields ~f:import_field;
    Lst.iter m.fields ~f:non_import_field
end

let resolve ctx module_ =
  let module I = Implicits (struct
    let ctx = ctx
  end) in
  I.module_ module_
