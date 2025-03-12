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
module Utf8 = Basic_utf8
module Vec_int = Basic_vec_int
module Base64 = Basic_base64
open Unsafe_external

let trim s =
  let i = ref 0 in
  let j = String.length s in
  while
    !i < j
    &&
    let u = s.![!i] in
    u = '\t' || u = '\n' || u = ' '
  do
    incr i
  done;
  let k = ref (j - 1) in
  while
    !k >= !i
    &&
    let u = s.![!k] in
    u = '\t' || u = '\n' || u = ' '
  do
    decr k
  done;
  String.sub s !i (!k - !i + 1)

let rec ends_aux s end_ j k =
  if k < 0 then j + 1
  else if s.![j] = end_.![k] then ends_aux s end_ (j - 1) (k - 1)
  else -1

let ends_with_index s end_ =
  (let s_finish = String.length s - 1 in
   let s_beg = String.length end_ - 1 in
   if s_beg > s_finish then -1 else ends_aux s end_ s_finish s_beg
    : int)

let ends_with_then_chop s beg =
  let i = ends_with_index s beg in
  if i >= 0 then Some (String.sub s 0 i) else None

let rec unsafe_for_all_range s ~start ~finish p =
  start > finish
  || (p s.![start] && unsafe_for_all_range s ~start:(start + 1) ~finish p)

let for_all_from s start p =
  let len = String.length s in
  if start < 0 then invalid_arg __FUNCTION__
  else unsafe_for_all_range s ~start ~finish:(len - 1) p

let for_all s (p : char -> bool) =
  unsafe_for_all_range s ~start:0 ~finish:(String.length s - 1) p

let is_empty s = String.length s = 0

let repeat n s =
  let len = String.length s in
  let res = Bytes.create (n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len then true else sub.![i + k] = s.![j + k] && check (k + 1)
  in
  j + len <= String.length s && check 0

let find ?(start = 0) ~sub s =
  let exception Local_exit in
  let n = String.length sub in
  let s_len = String.length s in
  let i = ref start in
  try
    while !i + n <= s_len do
      if unsafe_is_sub ~sub 0 s !i ~len:n then raise_notrace Local_exit;
      incr i
    done;
    -1
  with Local_exit -> !i

let contain_substring s sub = find s ~sub >= 0

let rfind ~sub s =
  let exception Local_exit in
  let n = String.length sub in
  let i = ref (String.length s - n) in
  try
    while !i >= 0 do
      if unsafe_is_sub ~sub 0 s !i ~len:n then raise_notrace Local_exit;
      decr i
    done;
    -1
  with Local_exit -> !i

let rec unsafe_no_char x ch i last_idx =
  i > last_idx || (x.![i] <> ch && unsafe_no_char x ch (i + 1) last_idx)

let no_char x ch i len =
  (let str_len = String.length x in
   if i < 0 || i >= str_len || len >= str_len then invalid_arg __FUNCTION__
   else unsafe_no_char x ch i len
    : bool)

let no_slash x = unsafe_no_char x '/' 0 (String.length x - 1)

let replace_slash_backward (x : string) =
  let len = String.length x in
  if unsafe_no_char x '/' 0 (len - 1) then x
  else String.map (function '/' -> '\\' | x -> x) x

let replace_backward_slash (x : string) =
  let len = String.length x in
  if unsafe_no_char x '\\' 0 (len - 1) then x
  else String.map (function '\\' -> '/' | x -> x) x

let empty = ""

let concat_array sep (s : string array) =
  let s_len = Array.length s in
  match s_len with
  | 0 -> empty
  | 1 -> s.!(0)
  | _ ->
      let sep_len = String.length sep in
      let len = ref 0 in
      for i = 0 to s_len - 1 do
        len := !len + String.length s.!(i)
      done;
      let target = Bytes.create (!len + ((s_len - 1) * sep_len)) in
      let hd = s.!(0) in
      let hd_len = String.length hd in
      String.unsafe_blit hd 0 target 0 hd_len;
      let current_offset = ref hd_len in
      for i = 1 to s_len - 1 do
        String.unsafe_blit sep 0 target !current_offset sep_len;
        let cur = s.!(i) in
        let cur_len = String.length cur in
        let new_off_set = !current_offset + sep_len in
        String.unsafe_blit cur 0 target new_off_set cur_len;
        current_offset := new_off_set + cur_len
      done;
      Bytes.unsafe_to_string target

let collapse_spaces (s : string) =
  let slen = String.length s in
  let buf = Bytes.create slen in
  let bi = ref 0 in
  let in_spaces = ref false in
  for i = 0 to slen - 1 do
    let c = s.![i] in
    match c with
    | '\n' | ' ' ->
        if not !in_spaces then (
          in_spaces := true;
          Bytes.unsafe_set buf !bi ' ';
          incr bi)
    | c ->
        in_spaces := false;
        Bytes.unsafe_set buf !bi c;
        incr bi
  done;
  Bytes.sub_string buf 0 !bi

let escaped_string ?(char_literal = false) str =
  let buf = Buffer.create (String.length str) in
  let add_escaped c =
    Buffer.add_char buf '\\';
    Buffer.add_char buf c
  in
  String.iter
    (fun c ->
      match c with
      | '\\' -> add_escaped '\\'
      | '\'' when char_literal -> add_escaped '\''
      | '"' when not char_literal -> add_escaped '"'
      | '\n' -> add_escaped 'n'
      | '\t' -> add_escaped 't'
      | '\b' -> add_escaped 'b'
      | '\r' -> add_escaped 'r'
      | _ -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let split_on_first c s =
  let rec go i s =
    if i >= String.length s then (s, "")
    else if s.[i] = c then
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - (i + 1)))
    else go (i + 1) s
  in
  go 0 s

let split_on_last c s =
  let rec go i s =
    if i < 0 then ("", s)
    else if s.[i] = c then
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - (i + 1)))
    else go (i - 1) s
  in
  go (String.length s - 1) s

let no_need_wasm_mangled s =
  let rec go i len =
    if i >= len then true
    else
      match s.![i] with
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9'
      | '_' | '.' | '*' | '|' | '<' | '>' | '+' | '-' | '@' | ':' | '$' | '!'
      | '=' | '/' ->
          go (i + 1) len
      | _ -> false
  in
  go 0 (String.length s)

let mangle_wasm_name s = if no_need_wasm_mangled s then s else Base64.encode s

let drop_while p s =
  let rec go i s =
    if i >= String.length s then ""
    else if p s.[i] then go (i + 1) s
    else String.sub s i (String.length s - i)
  in
  go 0 s

let starts_with_lower_case c = c = '_' || (c >= 'a' && c <= 'z')
[@@inline always]

let esc_quote s = "\"" ^ String.escaped s ^ "\""

let string_utf16_of_utf8 s =
  let vec = Utf8.from_string s in
  let buf = Buffer.create (Vec_int.length vec * 2) in
  Vec_int.iter vec (fun c ->
      let uc = Uchar.of_int c in
      Buffer.add_utf_16le_uchar buf uc);
  Buffer.contents buf

let first_char_is s c = String.length s > 0 && s.![0] = c
