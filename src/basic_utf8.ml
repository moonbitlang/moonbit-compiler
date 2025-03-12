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


module Unsafe_external = Basic_unsafe_external
module Vec_int = Basic_vec_int
open Unsafe_external

exception MalFormed

let sub (s : string) ofs len =
  let next_spos spos =
    spos
    +
    match s.[spos] with
    | '\000' .. '\127' -> 1
    | '\192' .. '\223' -> 2
    | '\224' .. '\239' -> 3
    | '\240' .. '\247' -> 4
    | _ -> raise MalFormed
  in
  let rec get_index spos i =
    if i = 0 then spos else get_index (next_spos spos) (i - 1)
  in
  let spos = get_index 0 ofs in
  let epos = get_index spos len in
  String.sub s spos (epos - spos)

let unsafe_utf8_of_string ~offset ~len (s : string) (a : int array) =
  (let spos = ref offset in
   let apos = ref 0 in
   let end_ = offset + len in
   while !spos < end_ do
     let spos_code = s.![!spos] in
     (match spos_code with
     | '\000' .. '\127' as c ->
         a.!(!apos) <- Char.code c;
         incr spos
     | '\192' .. '\223' as c ->
         let n1 = Char.code c in
         let n2 = Char.code s.![!spos + 1] in
         if n2 lsr 6 <> 0b10 then raise MalFormed;
         a.!(!apos) <- ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f);
         spos := !spos + 2
     | '\224' .. '\239' as c ->
         let n1 = Char.code c in
         let n2 = Char.code s.![!spos + 1] in
         let n3 = Char.code s.![!spos + 2] in
         let p =
           ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
         in
         if
           (n2 lsr 6 <> 0b10 || n3 lsr 6 <> 0b10) || (p >= 0xd800 && p <= 0xdfff)
         then raise MalFormed;
         a.!(!apos) <- p;
         spos := !spos + 3
     | '\240' .. '\247' as c ->
         let n1 = Char.code c in
         let n2 = Char.code s.![!spos + 1] in
         let n3 = Char.code s.![!spos + 2] in
         let n4 = Char.code s.![!spos + 3] in
         if n2 lsr 6 <> 0b10 || n3 lsr 6 <> 0b10 || n4 lsr 6 <> 0b10 then
           raise MalFormed;
         let p =
           ((n1 land 0x07) lsl 18)
           lor ((n2 land 0x3f) lsl 12)
           lor ((n3 land 0x3f) lsl 6)
           lor (n4 land 0x3f)
         in
         if p > 0x10ffff then raise MalFormed;
         a.!(!apos) <- p;
         spos := !spos + 4
     | _ -> raise MalFormed);
     incr apos
   done;
   !apos
    : int)

let from_sub_string s ~offset ~len =
  if offset < 0 || len < 0 || offset + len > String.length s then
    invalid_arg __FUNCTION__;
  let a = Array.make len 0 in
  let len = unsafe_utf8_of_string ~offset ~len s a in
  Vec_int.of_sub_array a 0 len

let from_string s =
  let slen = String.length s in
  let a = Array.make slen 0 in
  let len = unsafe_utf8_of_string ~offset:0 ~len:slen s a in
  Vec_int.of_sub_array a 0 len

let unsafe_string_of_utf8 (a : int array) ~(offset : int) ~(len : int)
    (b : bytes) =
  (let apos = ref offset in
   let len = ref len in
   let i = ref 0 in
   while !len > 0 do
     let u = a.!(!apos) in
     if u < 0 then raise MalFormed
     else if u <= 0x007F then (
       b.![!i] <- Char.unsafe_chr u;
       incr i)
     else if u <= 0x07FF then (
       b.![!i] <- Char.unsafe_chr (0xC0 lor (u lsr 6));
       b.![!i + 1] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
       i := !i + 2)
     else if u <= 0xFFFF then (
       b.![!i] <- Char.unsafe_chr (0xE0 lor (u lsr 12));
       b.![!i + 1] <- Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F));
       b.![!i + 2] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
       i := !i + 3)
     else if u <= 0x10FFFF then (
       b.![!i] <- Char.unsafe_chr (0xF0 lor (u lsr 18));
       b.![!i + 1] <- Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F));
       b.![!i + 2] <- Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F));
       b.![!i + 3] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
       i := !i + 4)
     else raise MalFormed;
     incr apos;
     decr len
   done;
   !i
    : int)

let string_of_utf8 (lexbuf : int array) ~offset ~len =
  (let b = Bytes.create (len * 4) in
   let i = unsafe_string_of_utf8 lexbuf ~offset ~len b in
   Bytes.sub_string b 0 i
    : string)

let string_of_vec (v : Vec_int.t) ~offset ~len =
  if offset < 0 || len < 0 || offset + len > v.len then invalid_arg __FUNCTION__
  else string_of_utf8 v.arr ~offset ~len

let length s =
  let rec length_aux s c i =
    if i >= String.length s then c
    else
      let n = Char.code s.![i] in
      let k =
        if n < 0x80 then 1
        else if n < 0xe0 then 2
        else if n < 0xf0 then 3
        else 4
      in
      length_aux s (c + 1) (i + k)
  in
  length_aux s 0 0
