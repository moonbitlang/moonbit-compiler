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


type t = Atom of string | List of t list

let rec equal (x : t) (y : t) =
  match x with
  | Atom x -> ( match y with Atom y -> x = y | _ -> false)
  | List ls -> (
      match y with
      | List ly -> ( try List.for_all2 equal ls ly with _ -> false)
      | _ -> false)

let rec map l f =
  match l with
  | [] -> []
  | x1 :: [] ->
      let y1 = f x1 in
      [ y1 ]
  | [ x1; x2 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      [ y1; y2 ]
  | [ x1; x2; x3 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      [ y1; y2; y3 ]
  | [ x1; x2; x3; x4 ] ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      [ y1; y2; y3; y4 ]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      let y5 = f x5 in
      y1 :: y2 :: y3 :: y4 :: y5 :: map tail f
[@@tail_mod_cons]

let rec sexp_of_t = function
  | Atom s -> S.Atom s
  | List l -> S.List (map l sexp_of_t)

let char_hex n =
  Char.unsafe_chr (n + if n < 10 then Char.code '0' else Char.code 'A' - 10)

let bytes_escaped s =
  let unsafe_set = Bytes.unsafe_set in
  let unsafe_get = Bytes.unsafe_get in
  let create = Bytes.create in
  let copy = Bytes.copy in
  let char_code = Char.code in
  let length = Bytes.length in
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n :=
      !n
      +
      match unsafe_get s i with
      | '"' | '\\' | '\000' .. '\031' | '\127' .. '\255' -> 3
      | ' ' .. '~' -> 1
  done;
  if !n = length s then copy s
  else
    let s' = create !n in
    n := 0;
    for i = 0 to length s - 1 do
      let c = unsafe_get s i in
      (match c with
      | '"' | '\\' | '\000' .. '\031' | '\127' .. '\255' ->
          let a = char_code c in
          unsafe_set s' !n '\\';
          incr n;
          unsafe_set s' !n (char_hex (a lsr 4));
          incr n;
          unsafe_set s' !n (char_hex (a land 0x0f))
      | ' ' .. '~' -> unsafe_set s' !n c);
      incr n
    done;
    s'

let escaped s =
  let rec escape_if_needed s n i =
    if i >= n then s
    else
      match String.unsafe_get s i with
      | '"' | '\\' | '\000' .. '\031' | '\127' .. '\255' ->
          Bytes.unsafe_to_string (bytes_escaped (Bytes.unsafe_of_string s))
      | _ -> escape_if_needed s n (i + 1)
  in
  escape_if_needed s (String.length s) 0

open struct
  let pp_print_string = Format.pp_print_string
  and pp_open_box = Format.pp_open_box
  and pp_print_space = Format.pp_print_space
  and pp_close_box = Format.pp_close_box

  let default_indent = ref 1

  let must_escape str =
    let len = String.length str in
    len = 0
    ||
    let rec loop str ix start ~in_quot =
      let c = str.[ix] in
      match c with
      | '\\' | '"' | '\000' .. ' ' | '\127' .. '\255' -> true
      | ('(' | ')' | ';') when not in_quot -> true
      | _ -> ix > start && loop str (ix - 1) start ~in_quot
    in
    let in_quot = len >= 2 && str.[0] = '"' && str.[len - 1] = '"' in
    if in_quot then false else loop str (len - 1) 0 ~in_quot

  let esc_str str =
    let estr = escaped str in
    let elen = String.length estr in
    let res = Bytes.create (elen + 2) in
    Bytes.blit_string estr 0 res 1 elen;
    Bytes.unsafe_set res 0 '"';
    Bytes.unsafe_set res (elen + 1) '"';
    Bytes.unsafe_to_string res

  let pp_hum_maybe_esc_str ppf (str : string) =
    if not (must_escape str) then pp_print_string ppf str
    else pp_print_string ppf (esc_str str)

  let rec pp_hum_indent indent ppf = function
    | Atom str -> pp_hum_maybe_esc_str ppf str
    | List (h :: t) ->
        pp_open_box ppf indent;
        pp_print_string ppf "(";
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
    | List [] -> pp_print_string ppf "()"

  and pp_hum_rest indent ppf = function
    | h :: t ->
        pp_print_space ppf ();
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
    | [] ->
        pp_print_string ppf ")";
        pp_close_box ppf ()

  let to_buffer_hum ~buf sexp =
    let indent = !default_indent in
    let ppf = Format.formatter_of_buffer buf in
    Format.fprintf ppf "%a@?" (pp_hum_indent indent) sexp

  let buffer () = Buffer.create 1024
end

let to_string = function
  | sexp ->
      let buf = buffer () in
      to_buffer_hum sexp ~buf;
      Buffer.contents buf

let print s =
  Format.fprintf Format.std_formatter "@[%a@]@."
    (pp_hum_indent !default_indent)
    s
