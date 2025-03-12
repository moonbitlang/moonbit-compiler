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


type 'a t = ('a -> unit) -> unit

external make : 'a t -> 'a t = "%identity"

let iter ~f seq = seq f [@@inline]
let map (seq : _ t) ~f = (fun k -> seq (fun x -> k (f x)) : _ t) [@@inline]

let filter_map (seq : _ t) ~f =
  (fun k -> seq (fun x -> match f x with None -> () | Some y -> k y) : _ t)
[@@inline]

exception ExitHead

let head (seq : _ t) =
  let r = ref None in
  try
    seq (fun x ->
        r := Some x;
        raise_notrace ExitHead);
    None
  with ExitHead -> !r

exception ExitTake

let take n (seq : _ t) =
  (fun k ->
     let count = ref 0 in
     try
       seq (fun x ->
           if !count = n then raise_notrace ExitTake;
           incr count;
           k x)
     with ExitTake -> ()
    : _ t)

let to_rev_list seq =
  let r = ref [] in
  seq (fun elt -> r := elt :: !r);
  !r
[@@inline]
