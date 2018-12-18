open Bigarray

external malloc : (int [@untagged]) -> (nativeint [@unboxed]) = "" "malloc" [@@noalloc]

let malloc n =
  let ptr = malloc n in
  if ptr = 0n then
    raise Out_of_memory;
  ptr

module With_bigarray = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t

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
  let%bench "" = exchange buf
end

module With_bytes = struct
  type t = bytes

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
  let%bench "" = exchange buf
end

module Raw_pointers_compiler_prims = struct
  external unsafe_get8  : nativeint -> int           = "%load8"
  external unsafe_set8  : nativeint -> int   -> unit = "%store8"
  external unsafe_get16 : nativeint -> int           = "%load16"
  external unsafe_set16 : nativeint -> int   -> unit = "%store16"
  external unsafe_get32 : nativeint -> int32         = "%load32"
  external unsafe_set32 : nativeint -> int32 -> unit = "%store32"
end

module With_raw_pointers = struct
  type t = nativeint

  open Raw_pointers_compiler_prims

  let shift ptr ofs =
    Nativeint.add ptr (Nativeint.of_int ofs)

  let unsafe_get8  t ofs = unsafe_get8  (shift t ofs)
  let unsafe_get16 t ofs = unsafe_get16 (shift t ofs)
  let unsafe_get32 t ofs = unsafe_get32 (shift t ofs)

  let unsafe_set8  t ofs x = unsafe_set8  (shift t ofs) x
  let unsafe_set16 t ofs x = unsafe_set16 (shift t ofs) x
  let unsafe_set32 t ofs x = unsafe_set32 (shift t ofs) x

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

  let buf = malloc 42
  let%bench "" = exchange buf
end

module With_2aligned_raw_pointers_as_int = struct
  type t = int (* 2-aligned pointer *)

  open Raw_pointers_compiler_prims

  let wrap ptr =
    assert (Nativeint.logand ptr 1n = 0n);
    Nativeint.to_int (Nativeint.shift_right_logical ptr 1)
  let unwrap_shift ptr ofs =
    Nativeint.add
      (Nativeint.shift_left (Nativeint.of_int ptr) 1)
      (Nativeint.of_int ofs)

  let unsafe_get8  t ofs = unsafe_get8  (unwrap_shift t ofs)
  let unsafe_get16 t ofs = unsafe_get16 (unwrap_shift t ofs)
  let unsafe_get32 t ofs = unsafe_get32 (unwrap_shift t ofs)

  let unsafe_set8  t ofs x = unsafe_set8  (unwrap_shift t ofs) x
  let unsafe_set16 t ofs x = unsafe_set16 (unwrap_shift t ofs) x
  let unsafe_set32 t ofs x = unsafe_set32 (unwrap_shift t ofs) x

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

  let buf = wrap (malloc 42)
  let%bench "" = exchange buf
end

module Raw_pointers_c_stubs = struct
  external unsafe_get8  : (nativeint [@unboxed]) -> (int   [@untagged])         = "" "caml_load_int8"   [@@noalloc]
  external unsafe_set8  : (nativeint [@unboxed]) -> (int   [@untagged]) -> unit = "" "caml_store_int8"  [@@noalloc]
  external unsafe_get16 : (nativeint [@unboxed]) -> (int   [@untagged])         = "" "caml_load_int16"  [@@noalloc]
  external unsafe_set16 : (nativeint [@unboxed]) -> (int   [@untagged]) -> unit = "" "caml_store_int16" [@@noalloc]
  external unsafe_get32 : (nativeint [@unboxed]) -> (int32 [@unboxed] )         = "" "caml_load_int32"  [@@noalloc]
  external unsafe_set32 : (nativeint [@unboxed]) -> (int32 [@unboxed] ) -> unit = "" "caml_store_int32" [@@noalloc]
end

module With_raw_pointers_c = struct
  type t = nativeint

  open Raw_pointers_c_stubs

  let shift ptr ofs =
    Nativeint.add ptr (Nativeint.of_int ofs)

  let unsafe_get8  t ofs = unsafe_get8  (shift t ofs)
  let unsafe_get16 t ofs = unsafe_get16 (shift t ofs)
  let unsafe_get32 t ofs = unsafe_get32 (shift t ofs)

  let unsafe_set8  t ofs x = unsafe_set8  (shift t ofs) x
  let unsafe_set16 t ofs x = unsafe_set16 (shift t ofs) x
  let unsafe_set32 t ofs x = unsafe_set32 (shift t ofs) x

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

  let buf = malloc 42
  let%bench "" = exchange buf
end

module With_2aligned_raw_pointers_c = struct
  type t = int (* 2-aligned pointer *)

  open Raw_pointers_c_stubs

  let wrap ptr =
    assert (Nativeint.logand ptr 1n = 0n);
    Nativeint.to_int (Nativeint.shift_right_logical ptr 1)
  let unwrap_shift ptr ofs =
    Nativeint.add
      (Nativeint.shift_left (Nativeint.of_int ptr) 1)
      (Nativeint.of_int ofs)

  let unsafe_get8  t ofs = unsafe_get8  (unwrap_shift t ofs)
  let unsafe_get16 t ofs = unsafe_get16 (unwrap_shift t ofs)
  let unsafe_get32 t ofs = unsafe_get32 (unwrap_shift t ofs)

  let unsafe_set8  t ofs x = unsafe_set8  (unwrap_shift t ofs) x
  let unsafe_set16 t ofs x = unsafe_set16 (unwrap_shift t ofs) x
  let unsafe_set32 t ofs x = unsafe_set32 (unwrap_shift t ofs) x

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

  let buf = wrap (malloc 42)
  let%bench "" = exchange buf
end
