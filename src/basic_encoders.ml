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


module Byteseq = Basic_byteseq
module Vlq64 = Basic_vlq64
module Json_utils = Basic_json_utils

let ( ^^ ) = Byteseq.O.( ^^ )

let of_buffer_add add_fn x =
  let b = Buffer.create 0 in
  add_fn b x;
  Byteseq.of_string (Buffer.contents b)

let byte x = Byteseq.of_string (String.make 1 (Char.chr x))
let int_16_le x = of_buffer_add Buffer.add_int16_le x
let int32_le x = of_buffer_add Buffer.add_int32_le x
let int_32_le x = int32_le (Int32.of_int x)
let float_le x = of_buffer_add Buffer.add_int64_le (Int64.bits_of_float x)
let float32_le x = of_buffer_add Buffer.add_int32_le (Int32.bits_of_float x)

let int_sleb128 x =
  of_buffer_add
    (fun b ->
      fun x ->
       let rec aux x =
         let y = x land 0x7f in
         if -64 <= x && x < 64 then Buffer.add_uint8 b y
         else (
           Buffer.add_uint8 b (y lor 0x80);
           aux (x asr 7))
       in
       aux x)
    x

let int_uleb128 x =
  of_buffer_add
    (fun b ->
      fun x ->
       let rec aux x =
         let y = x land 0x7f in
         if 0 <= x && x < 128 then Buffer.add_uint8 b y
         else (
           Buffer.add_uint8 b (y lor 0x80);
           aux (x lsr 7))
       in
       aux x)
    x

let int32_sleb128 x =
  of_buffer_add
    (fun b ->
      fun x ->
       let rec aux x =
         let y =
           let open Int32 in
           to_int (logand x 0x7fl)
         in
         if -64l <= x && x < 64l then Buffer.add_uint8 b y
         else (
           Buffer.add_uint8 b (y lor 0x80);
           aux (Int32.shift_right x 7))
       in
       aux x)
    x

let int32_uleb128 x =
  of_buffer_add
    (fun b ->
      fun x ->
       let rec aux x =
         let y =
           let open Int32 in
           to_int (logand x 0x7fl)
         in
         if 0l <= x && x < 128l then Buffer.add_uint8 b y
         else (
           Buffer.add_uint8 b (y lor 0x80);
           aux (Int32.shift_right_logical x 7))
       in
       aux x)
    x

let int64_sleb128 x =
  of_buffer_add
    (fun b ->
      fun x ->
       let rec aux x =
         let y =
           let open Int64 in
           to_int (logand x 0x7fL)
         in
         if -64L <= x && x < 64L then Buffer.add_uint8 b y
         else (
           Buffer.add_uint8 b (y lor 0x80);
           aux (Int64.shift_right x 7))
       in
       aux x)
    x

let int_vlq64 = of_buffer_add Vlq64.buffer_add_vlq64
let string = Byteseq.of_string
let json_string = of_buffer_add Json_utils.buffer_add_json_string
let with_length_preceded ~f x = f (Byteseq.length x) ^^ x
