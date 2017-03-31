open Bigarray

type buf = (char, int8_unsigned_elt, c_layout) Array1.t

external unsafe_get   : buf -> int -> char          = "%caml_ba_unsafe_ref_1"
external unsafe_set   : buf -> int -> char  -> unit = "%caml_ba_unsafe_set_1"
external unsafe_get8  : buf -> int -> int           = "%caml_ba_unsafe_ref_1"
external unsafe_set8  : buf -> int -> int   -> unit = "%caml_ba_unsafe_set_1"
external unsafe_get16 : buf -> int -> int           = "%caml_bigstring_get16u"
external unsafe_set16 : buf -> int -> int   -> unit = "%caml_bigstring_set16u"
external unsafe_get32 : buf -> int -> int32         = "%caml_bigstring_get32u"
external unsafe_set32 : buf -> int -> int32 -> unit = "%caml_bigstring_set32u"

let exchange packet =
  assert (Array1.dim packet >= 42);
  let a = unsafe_get16 packet 0 in
  let b = unsafe_get32 packet 2 in
  unsafe_set16 packet 0 (unsafe_get16 packet 6);
  unsafe_set32 packet 2 (unsafe_get32 packet 8);
  unsafe_set16 packet 6 a;
  unsafe_set32 packet 8 b;
  unsafe_set16 packet 24 0;
  let a = unsafe_get32 packet 26 in
  unsafe_set32 packet 26 (unsafe_get32 packet 30);
  unsafe_set32 packet 30 a;
  let a = unsafe_get16 packet 34 in
  unsafe_set16 packet 34 (unsafe_get16 packet 36);
  unsafe_set16 packet 36 a;
  unsafe_set16 packet 40 0;
;;

let buf : buf = Array1.create char c_layout 42

let%bench "bigarray" = exchange buf
