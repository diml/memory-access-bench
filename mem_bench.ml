open Bigarray

module With_bigarray = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  external unsafe_get   : t -> int -> char          = "%caml_ba_unsafe_ref_1"
  external unsafe_set   : t -> int -> char  -> unit = "%caml_ba_unsafe_set_1"
  external unsafe_get8  : t -> int -> int           = "%caml_ba_unsafe_ref_1"
  external unsafe_set8  : t -> int -> int   -> unit = "%caml_ba_unsafe_set_1"
  external unsafe_get16 : t -> int -> int           = "%caml_bigstring_get16u"
  external unsafe_set16 : t -> int -> int   -> unit = "%caml_bigstring_set16u"
  external unsafe_get32 : t -> int -> int32         = "%caml_bigstring_get32u"
  external unsafe_set32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"

  (*$*)
  let exchange packet =
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
    unsafe_set16 packet 40 0
  (*$ let bench = _last_text_block $*)

  let buf : t = Array1.create char c_layout 42
  let%bench "bigarray" = exchange buf
end

module With_bytes = struct
  type t = string

  external unsafe_get   : t -> int -> char          = "%bytes_unsafe_get"
  external unsafe_set   : t -> int -> char  -> unit = "%bytes_unsafe_set"
  external unsafe_get8  : t -> int -> int           = "%bytes_unsafe_get"
  external unsafe_set8  : t -> int -> int   -> unit = "%bytes_unsafe_set"
  external unsafe_get16 : t -> int -> int           = "%caml_string_get16u"
  external unsafe_set16 : t -> int -> int   -> unit = "%caml_string_set16u"
  external unsafe_get32 : t -> int -> int32         = "%caml_string_get32u"
  external unsafe_set32 : t -> int -> int32 -> unit = "%caml_string_set32u"

  (*$print_string bench*)
  let exchange packet =
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
    unsafe_set16 packet 40 0
  (*$*)

  let buf = Bytes.create 42
  let%bench "bytes" = exchange buf
end
