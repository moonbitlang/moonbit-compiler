/*
 * Copyright (C) 2024 International Digital Economy Academy.
 * This program is licensed under the MoonBit Public Source
 * License as published by the International Digital Economy Academy,
 * either version 1 of the License, or (at your option) any later
 * version. This program is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the MoonBit
 * Public Source License for more details. You should have received a
 * copy of the MoonBit Public Source License along with this program. If
 * not, see
 * <https://www.moonbitlang.com/licenses/moonbit-public-source-license-v1>.
 */

#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/hash.h>


/* This pretends that the state of the OCaml internal hash function, which is an
   int32, is actually stored in an OCaml int. */

CAMLprim value Ppx_fold_int32(value st, value i)
{
  return Val_long(caml_hash_mix_uint32(Long_val(st), Int32_val(i)));
}

CAMLprim value Ppx_fold_nativeint(value st, value i)
{
  return Val_long(caml_hash_mix_intnat(Long_val(st), Nativeint_val(i)));
}

CAMLprim value Ppx_fold_int64(value st, value i)
{
  return Val_long(caml_hash_mix_int64(Long_val(st), Int64_val(i)));
}

CAMLprim value Ppx_fold_int(value st, value i)
{
  return Val_long(caml_hash_mix_intnat(Long_val(st), Long_val(i)));
}

CAMLprim value Ppx_fold_float(value st, value i)
{
  return Val_long(caml_hash_mix_double(Long_val(st), Double_val(i)));
}

/* This code mimics what hashtbl.hash does in OCaml's hash.c */
#define FINAL_MIX(h)                            \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

CAMLprim value Ppx_get_hash_value(value st)
{
  uint32_t h = Int_val(st);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU); /*30 bits*/
}

/* Macros copied from hash.c in ocaml distribution */
#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d)   \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

/* Version of [caml_hash_mix_string] from hash.c - adapted for arbitrary char arrays */
CAMLexport uint32_t Ppx_fold_blob(uint32_t h, mlsize_t len, uint8_t *s)
{
  mlsize_t i;
  uint32_t w;

  /* Mix by 32-bit blocks (little-endian) */
  for (i = 0; i + 4 <= len; i += 4) {
#ifdef ARCH_BIG_ENDIAN
    w = s[i]
      | (s[i+1] << 8)
      | (s[i+2] << 16)
      | (s[i+3] << 24);
#else
    w = *((uint32_t *) &(s[i]));
#endif
    MIX(h, w);
  }
  /* Finish with up to 3 bytes */
  w = 0;
  switch (len & 3) {
  case 3: w  = s[i+2] << 16;   /* fallthrough */
  case 2: w |= s[i+1] << 8;    /* fallthrough */
  case 1: w |= s[i];
          MIX(h, w);
  default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
  }
  /* Finally, mix in the length. Ignore the upper 32 bits, generally 0. */
  h ^= (uint32_t) len;
  return h;
}

CAMLprim value Ppx_fold_string(value st, value v_str)
{
  uint32_t h = Long_val(st);
  mlsize_t len = caml_string_length(v_str);
  uint8_t *s = (uint8_t *) String_val(v_str);

  h = Ppx_fold_blob(h, len, s);

  return Val_long(h);
}



/* Final mix and return from the hash.c implementation from INRIA */
#define FINAL_MIX_AND_RETURN(h)                                                \
  h ^= h >> 16;                                                                \
  h *= 0x85ebca6b;                                                             \
  h ^= h >> 13;                                                                \
  h *= 0xc2b2ae35;                                                             \
  h ^= h >> 16;                                                                \
  return Val_int(h & 0x3FFFFFFFU);

CAMLprim value Ppx_hash_string(value string) {
  uint32_t h;
  h = caml_hash_mix_string(0, string);
  FINAL_MIX_AND_RETURN(h)
}

CAMLprim value Ppx_hash_double(value d) {
  uint32_t h;
  h = caml_hash_mix_double(0, Double_val(d));
  FINAL_MIX_AND_RETURN(h);
}
