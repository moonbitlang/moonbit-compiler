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


module Vec = Basic_vec
module Hash_string = Basic_hash_string
module Hash_int = Basic_hash_int

type rectype = Dwarfsm_ast.rectype
type import = Dwarfsm_ast.import
type func = Dwarfsm_ast.func
type table = Dwarfsm_ast.table
type mem = Dwarfsm_ast.mem
type global = Dwarfsm_ast.global
type export = Dwarfsm_ast.export
type start = Dwarfsm_ast.start
type elem = Dwarfsm_ast.elem
type data = Dwarfsm_ast.data
type tag = Dwarfsm_ast.tag
type space = { mutable next_index : int; map : int Hash_string.t }

type spaces = {
  types : space;
  fields : space Hash_int.t;
  locals : space Hash_int.t;
  funcs : space;
  tables : space;
  mems : space;
  globals : space;
  elems : space;
  datas : space;
  tags : space;
}

type context = {
  spaces : spaces;
  types : rectype Vec.t;
  imports : import Vec.t;
  funcs : func Vec.t;
  tables : table Vec.t;
  mems : mem Vec.t;
  globals : global Vec.t;
  exports : export Vec.t;
  mutable start : start option;
  elems : elem Vec.t;
  datas : data Vec.t;
  tags : tag Vec.t;
}

let make_space () = { next_index = 0; map = Hash_string.create 0 }

let make_context () =
  {
    spaces =
      {
        types = make_space ();
        fields = Hash_int.create 0;
        locals = Hash_int.create 0;
        funcs = make_space ();
        tables = make_space ();
        mems = make_space ();
        globals = make_space ();
        elems = make_space ();
        datas = make_space ();
        tags = make_space ();
      };
    types = Vec.empty ();
    imports = Vec.empty ();
    funcs = Vec.empty ();
    tables = Vec.empty ();
    mems = Vec.empty ();
    globals = Vec.empty ();
    exports = Vec.empty ();
    start = None;
    elems = Vec.empty ();
    datas = Vec.empty ();
    tags = Vec.empty ();
  }
