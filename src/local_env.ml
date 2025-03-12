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


module Ident = Basic_ident

type t = Empty | Node of t * data * t * int
and data = { key : string; v : Value_info.t; previous : data option }

include struct
  let _ = fun (_ : t) -> ()
  let _ = fun (_ : data) -> ()

  let rec sexp_of_t =
    (function
     | Empty -> S.Atom "Empty"
     | Node (arg0__001_, arg1__002_, arg2__003_, arg3__004_) ->
         let res0__005_ = sexp_of_t arg0__001_
         and res1__006_ = sexp_of_data arg1__002_
         and res2__007_ = sexp_of_t arg2__003_
         and res3__008_ = Moon_sexp_conv.sexp_of_int arg3__004_ in
         S.List
           [ S.Atom "Node"; res0__005_; res1__006_; res2__007_; res3__008_ ]
      : t -> S.t)

  and sexp_of_data =
    (fun { key = key__010_; v = v__012_; previous = previous__014_ } ->
       let bnds__009_ = ([] : _ Stdlib.List.t) in
       let bnds__009_ =
         let arg__015_ =
           Moon_sexp_conv.sexp_of_option sexp_of_data previous__014_
         in
         (S.List [ S.Atom "previous"; arg__015_ ] :: bnds__009_
           : _ Stdlib.List.t)
       in
       let bnds__009_ =
         let arg__013_ = Value_info.sexp_of_t v__012_ in
         (S.List [ S.Atom "v"; arg__013_ ] :: bnds__009_ : _ Stdlib.List.t)
       in
       let bnds__009_ =
         let arg__011_ = Moon_sexp_conv.sexp_of_string key__010_ in
         (S.List [ S.Atom "key"; arg__011_ ] :: bnds__009_ : _ Stdlib.List.t)
       in
       S.List bnds__009_
      : data -> S.t)

  let _ = sexp_of_t
  and _ = sexp_of_data
end

let empty = Empty

open struct
  let height = function Empty -> 0 | Node (_, _, _, h) -> h [@@inline]

  let mknode l d r =
    let hl = height l in
    let hr = height r in
    Node (l, d, r, if hl >= hr then hl + 1 else hr + 1)

  let balance l d r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 1 then
      match[@warning "-fragile-match"] l with
      | Node (ll, ld, lr, _) -> (
          if height ll >= height lr then mknode ll ld (mknode lr d r)
          else
            match[@warning "-fragile-match"] lr with
            | Node (lrl, lrd, lrr, _) ->
                mknode (mknode ll ld lrl) lrd (mknode lrr d r)
            | _ -> assert false)
      | _ -> assert false
    else if hr > hl + 1 then
      match[@warning "-fragile-match"] r with
      | Node (rl, rd, rr, _) -> (
          if height rr >= height rl then mknode (mknode l d rl) rd rr
          else
            match[@warning "-fragile-match"] rl with
            | Node (rll, rld, rlr, _) ->
                mknode (mknode l d rll) rld (mknode rlr rd rr)
            | _ -> assert false)
      | _ -> assert false
    else mknode l d r
end

let rec add (t : t) (id : string) v =
  match t with
  | Empty -> Node (Empty, { key = id; v; previous = None }, Empty, 1)
  | Node (l, k, r, h) ->
      let c = String.compare id k.key in
      if c = 0 then Node (l, { key = id; v; previous = Some k }, r, h)
      else if c < 0 then balance (add l id v) k r
      else balance l k (add r id v)

let add t id ~typ ~mut ~loc =
  add t (Ident.base_name id)
    (if mut then Local_mut { id; typ; loc_ = loc }
     else Local_imm { id; typ; loc_ = loc })

let rec find_by_name_opt (env : t) (name : string) =
  (match env with
   | Empty -> None
   | Node (l, k, r, _) ->
       let c = String.compare name k.key in
       if c = 0 then Some k.v
       else find_by_name_opt (if c < 0 then l else r) name
    : Value_info.t option)
