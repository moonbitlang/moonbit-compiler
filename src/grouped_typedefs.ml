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


module VI = Basic_vec_int
module Vec = Basic_vec
module Lst = Basic_lst
module Ltype = Ltype_gc
module Tid = Basic_ty_ident
module Tid_hash = Tid.Hash
module Tid_hashset = Tid.Hashset

type ltype_def = Ltype.def

include struct
  let _ = fun (_ : ltype_def) -> ()
  let sexp_of_ltype_def = (Ltype.sexp_of_def : ltype_def -> S.t)
  let _ = sexp_of_ltype_def
end

type def = Rec of (Tid.t * ltype_def) list | Nonrec of (Tid.t * ltype_def)

include struct
  let _ = fun (_ : def) -> ()

  let sexp_of_def =
    (function
     | Rec arg0__005_ ->
         let res0__006_ =
           Moon_sexp_conv.sexp_of_list
             (fun (arg0__001_, arg1__002_) ->
               let res0__003_ = Tid.sexp_of_t arg0__001_
               and res1__004_ = sexp_of_ltype_def arg1__002_ in
               S.List [ res0__003_; res1__004_ ])
             arg0__005_
         in
         S.List [ S.Atom "Rec"; res0__006_ ]
     | Nonrec arg0__011_ ->
         let res0__012_ =
           let arg0__007_, arg1__008_ = arg0__011_ in
           let res0__009_ = Tid.sexp_of_t arg0__007_
           and res1__010_ = sexp_of_ltype_def arg1__008_ in
           S.List [ res0__009_; res1__010_ ]
         in
         S.List [ S.Atom "Nonrec"; res0__012_ ]
      : def -> S.t)

  let _ = sexp_of_def
end

type t = def list

include struct
  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun x__013_ -> Moon_sexp_conv.sexp_of_list sexp_of_def x__013_ : t -> S.t)

  let _ = sexp_of_t
end

let collect_ref (def : Ltype.def) =
  (let tid_set = Tid_hashset.create 17 in
   let go (ltype : Ltype.t) =
     match ltype with
     | Ref_lazy_init { tid } | Ref { tid } | Ref_nullable { tid } ->
         if
           not
             (Tid.equal tid Ltype.tid_enum
             || Tid.equal tid Ltype.tid_bytes
             || Tid.equal tid Ltype.tid_string)
         then Tid_hashset.add tid_set tid
     | I32 _ | I64 | F32 | F64 | Ref_extern | Ref_string | Ref_bytes | Ref_func
     | Ref_any ->
         ()
   in
   (match def with
   | Ref_array { elem } -> go elem
   | Ref_struct { fields } -> Lst.iter fields ~f:(fun (t, _) -> go t)
   | Ref_late_init_struct { fields } -> Lst.iter fields ~f:go
   | Ref_constructor { args } -> Lst.iter args ~f:(fun (t, _) -> go t)
   | Ref_closure_abstract { fn_sig = { params; ret } } ->
       Lst.iter params ~f:go;
       Lst.iter ret ~f:go
   | Ref_object { methods } ->
       Lst.iter methods ~f:(fun { params; ret } ->
           Lst.iter params ~f:go;
           Lst.iter ret ~f:go)
   | Ref_concrete_object { abstract_obj_tid; self } ->
       Tid_hashset.add tid_set abstract_obj_tid;
       go self
   | Ref_closure { fn_sig_tid = fn_tid; captures } ->
       Tid_hashset.add tid_set fn_tid;
       Lst.iter captures ~f:go);
   tid_set
    : Tid_hashset.t)

let group_typedefs (td : Ltype.type_defs) =
  (let td_arr = Tid_hash.to_array td in
   let index_tbl = Tid_hash.create 17 in
   let self_rec_set = Tid_hashset.create 17 in
   Array.iteri (fun i -> fun (tid, _def) -> Tid_hash.add index_tbl tid i) td_arr;
   let adjacency_array =
     Array.init (Array.length td_arr) (fun _ -> VI.empty ())
   in
   let add_edge (def : Tid.t) (ref_ : Tid.t) =
     let def_index = Tid_hash.find_exn index_tbl def in
     let ref_index = Tid_hash.find_exn index_tbl ref_ in
     VI.push adjacency_array.(def_index) ref_index
   in
   Array.iter
     (fun (tid, def) ->
       let ref_set = collect_ref def in
       if Tid_hashset.mem ref_set tid then Tid_hashset.add self_rec_set tid;
       Tid_hashset.iter ref_set (fun ref_ -> add_edge tid ref_))
     td_arr;
   let scc = Basic_scc.graph adjacency_array in
   let res =
     Vec.map_into_list scc ~unorder:(fun component ->
         if VI.length component = 1 then
           let tid_index = VI.get component 0 in
           let tid, def = td_arr.(tid_index) in
           if Tid_hashset.mem self_rec_set tid then Rec [ (tid, def) ]
           else Nonrec (tid, def)
         else
           Rec
             (VI.map_into_list component ~unorder:(fun tid_index ->
                  td_arr.(tid_index))))
   in
   res
    : t)
