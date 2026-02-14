(* Interop test: write .npy files from OCaml, verify with numpy,
   then write from numpy and read back in OCaml.
   Covers all 14 dtypes, big-endian, Fortran order, byte strings,
   unicode strings, datetime64, and timedelta64. *)

let tmp = Filename.get_temp_dir_name ()

let ocaml_to_numpy () =
  (* Write files from OCaml *)
  Npy.save (tmp ^ "/ocaml_bool.npy")
    (Npy.of_bool_array [| 4 |] [| true; false; true; false |]);
  Npy.save (tmp ^ "/ocaml_f32.npy")
    (Npy.of_float_array Float32 [| 2; 3 |]
       [| 1.0; 2.5; -3.0; 0.0; 100.0; -0.125 |]);
  Npy.save (tmp ^ "/ocaml_f64.npy")
    (Npy.of_float_array Float64 [| 3 |]
       [| 3.141592653589793; -2.718281828459045; 0.0 |]);
  Npy.save (tmp ^ "/ocaml_f16.npy")
    (Npy.of_float_array Float16 [| 4 |]
       [| 0.0; 1.0; -1.0; 0.5 |]);
  Npy.save (tmp ^ "/ocaml_i8.npy")
    (Npy.of_int_array Int8 [| 5 |] [| -128; -1; 0; 1; 127 |]);
  Npy.save (tmp ^ "/ocaml_u8.npy")
    (Npy.of_int_array Uint8 [| 4 |] [| 0; 1; 128; 255 |]);
  Npy.save (tmp ^ "/ocaml_i16.npy")
    (Npy.of_int_array Int16 [| 3 |] [| -32768; 0; 32767 |]);
  Npy.save (tmp ^ "/ocaml_u16.npy")
    (Npy.of_int_array Uint16 [| 3 |] [| 0; 1; 65535 |]);
  Npy.save (tmp ^ "/ocaml_i32.npy")
    (Npy.of_int_array Int32 [| 3 |] [| -100000; 0; 100000 |]);
  Npy.save (tmp ^ "/ocaml_u32.npy")
    (Npy.of_int_array Uint32 [| 3 |] [| 0; 1; 3000000000 |]);
  Npy.save (tmp ^ "/ocaml_i64.npy")
    (Npy.of_int64_array Int64 [| 3 |]
       [| -1000000000000L; 0L; 1000000000000L |]);
  Npy.save (tmp ^ "/ocaml_u64.npy")
    (Npy.of_int64_array Uint64 [| 3 |]
       [| 0L; 1L; 1000000000000L |]);
  Npy.save (tmp ^ "/ocaml_c64.npy")
    (Npy.of_complex_array Complex64 [| 2 |]
       [| Complex.{ re = 1.0; im = -2.0 }; Complex.{ re = 0.0; im = 3.0 } |]);
  Npy.save (tmp ^ "/ocaml_c128.npy")
    (Npy.of_complex_array Complex128 [| 2 |]
       [| Complex.{ re = 1.0; im = -2.0 }; Complex.{ re = 0.0; im = 3.0 } |]);
  Npy.save (tmp ^ "/ocaml_4d.npy")
    (Npy.of_float_array Float32 [| 2; 3; 4; 5 |]
       (Array.init 120 (fun i -> Float.of_int i *. 0.1)));
  (* Fortran-ordered file *)
  Npy.save (tmp ^ "/ocaml_fortran.npy")
    (Npy.of_float_array ~order:Fortran Float64 [| 2; 3 |]
       [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]);
  (* Byte strings *)
  Npy.save (tmp ^ "/ocaml_bytes.npy")
    (Npy.of_string_array (Bytes 10) [| 3 |]
       [| "hello"; "world"; "" |]);
  (* Unicode strings *)
  Npy.save (tmp ^ "/ocaml_unicode.npy")
    (Npy.of_string_array (Unicode 10) [| 3 |]
       [| "hello"; "\xc3\xa9l\xc3\xa8ve"; "\xf0\x9f\x98\x80" |]);
  (* Datetime64 *)
  Npy.save (tmp ^ "/ocaml_dt64.npy")
    (Npy.of_int64_array (Datetime64 Ns) [| 3 |]
       [| 0L; 1_000_000_000L; Npy.nat |]);
  (* Timedelta64 *)
  Npy.save (tmp ^ "/ocaml_td64.npy")
    (Npy.of_int64_array (Timedelta64 Sec) [| 3 |]
       [| 0L; 3600L; -86400L |]);
  (* Verify with numpy *)
  let py_script = {|
import numpy as np
import sys

d = sys.argv[1]

# bool
a = np.load(d + "/ocaml_bool.npy")
assert a.dtype == np.bool_, f"bool dtype: {a.dtype}"
assert list(a) == [True, False, True, False], f"bool values: {list(a)}"
print("PASS ocaml_bool.npy")

# float32 2x3
a = np.load(d + "/ocaml_f32.npy")
assert a.dtype == np.float32, f"f32 dtype: {a.dtype}"
assert a.shape == (2, 3), f"f32 shape: {a.shape}"
expected = [1.0, 2.5, -3.0, 0.0, 100.0, -0.125]
for i, v in enumerate(a.flat):
    assert abs(v - expected[i]) < 1e-6, f"f32[{i}]: {v} != {expected[i]}"
print("PASS ocaml_f32.npy")

# float64
a = np.load(d + "/ocaml_f64.npy")
assert a.dtype == np.float64, f"f64 dtype: {a.dtype}"
expected = [3.141592653589793, -2.718281828459045, 0.0]
for i, v in enumerate(a.flat):
    assert abs(v - expected[i]) < 1e-15, f"f64[{i}]: {v} != {expected[i]}"
print("PASS ocaml_f64.npy")

# float16
a = np.load(d + "/ocaml_f16.npy")
assert a.dtype == np.float16, f"f16 dtype: {a.dtype}"
expected = [0.0, 1.0, -1.0, 0.5]
for i, v in enumerate(a.flat):
    assert abs(float(v) - expected[i]) < 1e-3, f"f16[{i}]: {v} != {expected[i]}"
print("PASS ocaml_f16.npy")

# int8
a = np.load(d + "/ocaml_i8.npy")
assert a.dtype == np.int8, f"i8 dtype: {a.dtype}"
assert list(a) == [-128, -1, 0, 1, 127], f"i8 values: {list(a)}"
print("PASS ocaml_i8.npy")

# uint8
a = np.load(d + "/ocaml_u8.npy")
assert a.dtype == np.uint8, f"u8 dtype: {a.dtype}"
assert list(a) == [0, 1, 128, 255], f"u8 values: {list(a)}"
print("PASS ocaml_u8.npy")

# int16
a = np.load(d + "/ocaml_i16.npy")
assert a.dtype == np.int16, f"i16 dtype: {a.dtype}"
assert list(a) == [-32768, 0, 32767], f"i16 values: {list(a)}"
print("PASS ocaml_i16.npy")

# uint16
a = np.load(d + "/ocaml_u16.npy")
assert a.dtype == np.uint16, f"u16 dtype: {a.dtype}"
assert list(a) == [0, 1, 65535], f"u16 values: {list(a)}"
print("PASS ocaml_u16.npy")

# int32
a = np.load(d + "/ocaml_i32.npy")
assert a.dtype == np.int32, f"i32 dtype: {a.dtype}"
assert list(a) == [-100000, 0, 100000], f"i32 values: {list(a)}"
print("PASS ocaml_i32.npy")

# uint32
a = np.load(d + "/ocaml_u32.npy")
assert a.dtype == np.uint32, f"u32 dtype: {a.dtype}"
assert list(a) == [0, 1, 3000000000], f"u32 values: {list(a)}"
print("PASS ocaml_u32.npy")

# int64
a = np.load(d + "/ocaml_i64.npy")
assert a.dtype == np.int64, f"i64 dtype: {a.dtype}"
assert list(a) == [-1000000000000, 0, 1000000000000], f"i64 values: {list(a)}"
print("PASS ocaml_i64.npy")

# uint64
a = np.load(d + "/ocaml_u64.npy")
assert a.dtype == np.uint64, f"u64 dtype: {a.dtype}"
assert list(a) == [0, 1, 1000000000000], f"u64 values: {list(a)}"
print("PASS ocaml_u64.npy")

# complex64
a = np.load(d + "/ocaml_c64.npy")
assert a.dtype == np.complex64, f"c64 dtype: {a.dtype}"
assert abs(a[0].real - 1.0) < 1e-6, f"c64[0].re: {a[0].real}"
assert abs(a[0].imag - (-2.0)) < 1e-6, f"c64[0].im: {a[0].imag}"
assert abs(a[1].real - 0.0) < 1e-6, f"c64[1].re: {a[1].real}"
assert abs(a[1].imag - 3.0) < 1e-6, f"c64[1].im: {a[1].imag}"
print("PASS ocaml_c64.npy")

# complex128
a = np.load(d + "/ocaml_c128.npy")
assert a.dtype == np.complex128, f"c128 dtype: {a.dtype}"
assert abs(a[0].real - 1.0) < 1e-15, f"c128[0].re: {a[0].real}"
assert abs(a[0].imag - (-2.0)) < 1e-15, f"c128[0].im: {a[0].imag}"
assert abs(a[1].real - 0.0) < 1e-15, f"c128[1].re: {a[1].real}"
assert abs(a[1].imag - 3.0) < 1e-15, f"c128[1].im: {a[1].imag}"
print("PASS ocaml_c128.npy")

# 4d float32
a = np.load(d + "/ocaml_4d.npy")
assert a.dtype == np.float32, f"4d dtype: {a.dtype}"
assert a.shape == (2, 3, 4, 5), f"4d shape: {a.shape}"
for i in range(120):
    assert abs(a.flat[i] - i * 0.1) < 1e-6, f"4d[{i}]: {a.flat[i]}"
print("PASS ocaml_4d.npy")

# Fortran-ordered
a = np.load(d + "/ocaml_fortran.npy")
assert a.dtype == np.float64, f"fortran dtype: {a.dtype}"
assert a.shape == (2, 3), f"fortran shape: {a.shape}"
print("PASS ocaml_fortran.npy")

# Byte strings
a = np.load(d + "/ocaml_bytes.npy")
assert a.dtype == np.dtype('S10'), f"bytes dtype: {a.dtype}"
assert list(a) == [b"hello", b"world", b""], f"bytes values: {list(a)}"
print("PASS ocaml_bytes.npy")

# Unicode strings
a = np.load(d + "/ocaml_unicode.npy")
assert a.dtype.kind == 'U', f"unicode dtype kind: {a.dtype.kind}"
assert a[0] == "hello", f"unicode[0]: {a[0]}"
assert a[1] == "\u00e9l\u00e8ve", f"unicode[1]: {a[1]}"
assert a[2] == "\U0001f600", f"unicode[2]: {a[2]}"
print("PASS ocaml_unicode.npy")

# Datetime64
a = np.load(d + "/ocaml_dt64.npy")
assert a.dtype == np.dtype('<M8[ns]'), f"dt64 dtype: {a.dtype}"
assert a[0] == np.datetime64(0, 'ns'), f"dt64[0]: {a[0]}"
assert a[1] == np.datetime64(1000000000, 'ns'), f"dt64[1]: {a[1]}"
assert np.isnat(a[2]), f"dt64[2] should be NaT: {a[2]}"
print("PASS ocaml_dt64.npy")

# Timedelta64
a = np.load(d + "/ocaml_td64.npy")
assert a.dtype == np.dtype('<m8[s]'), f"td64 dtype: {a.dtype}"
assert a[0] == np.timedelta64(0, 's'), f"td64[0]: {a[0]}"
assert a[1] == np.timedelta64(3600, 's'), f"td64[1]: {a[1]}"
assert a[2] == np.timedelta64(-86400, 's'), f"td64[2]: {a[2]}"
print("PASS ocaml_td64.npy")

print("ALL OCaml->NumPy checks passed")
|} in
  let rc =
    Unix.system
      (Printf.sprintf "python3 -c %s %s"
         (Filename.quote py_script) (Filename.quote tmp))
  in
  match rc with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "numpy verification of OCaml-written files failed"

let numpy_to_ocaml () =
  (* Write files from numpy, including big-endian and Fortran order *)
  let py_script = {|
import numpy as np
import sys
d = sys.argv[1]
np.save(d + "/numpy_bool.npy", np.array([True, False, True, False], dtype=np.bool_))
np.save(d + "/numpy_f16.npy", np.array([0.0, 1.0, -1.0, 0.5], dtype=np.float16))
np.save(d + "/numpy_f32.npy", np.array([[1.5, -2.5, 3.0], [0.0, 42.0, -1.0]], dtype=np.float32))
np.save(d + "/numpy_f64.npy", np.array([3.141592653589793, -2.718281828459045, 0.0], dtype=np.float64))
np.save(d + "/numpy_i8.npy", np.array([-128, -1, 0, 1, 127], dtype=np.int8))
np.save(d + "/numpy_u8.npy", np.array([0, 1, 200, 255], dtype=np.uint8))
np.save(d + "/numpy_i16.npy", np.array([-32768, 0, 32767], dtype=np.int16))
np.save(d + "/numpy_u16.npy", np.array([0, 1000, 65535], dtype=np.uint16))
np.save(d + "/numpy_i32.npy", np.array([-100000, 0, 100000], dtype=np.int32))
np.save(d + "/numpy_u32.npy", np.array([0, 1, 3000000000], dtype=np.uint32))
np.save(d + "/numpy_i64.npy", np.array([-1000000000000, 0, 1000000000000], dtype=np.int64))
np.save(d + "/numpy_u64.npy", np.array([0, 1, 1000000000000], dtype=np.uint64))
np.save(d + "/numpy_c64.npy", np.array([1-2j, 0+3j], dtype=np.complex64))
np.save(d + "/numpy_c128.npy", np.array([1-2j, 0+3j], dtype=np.complex128))
np.save(d + "/numpy_3d.npy", np.arange(24, dtype=np.float32).reshape(2, 3, 4))

# Big-endian files
a = np.array([1.0, 2.0, 3.0], dtype=np.float32).byteswap().view(np.dtype('>f4'))
np.save(d + "/numpy_be_f32.npy", a)
a = np.array([-100, 0, 100], dtype=np.int32).byteswap().view(np.dtype('>i4'))
np.save(d + "/numpy_be_i32.npy", a)
a = np.array([1.0, 2.0, 3.0], dtype=np.float64).byteswap().view(np.dtype('>f8'))
np.save(d + "/numpy_be_f64.npy", a)

# Fortran-ordered file
a = np.asfortranarray(np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], dtype=np.float64))
np.save(d + "/numpy_fortran.npy", a)

# Byte strings
np.save(d + "/numpy_bytes.npy", np.array([b"hello", b"world", b""], dtype='S10'))

# Unicode strings (ASCII, multibyte, emoji)
np.save(d + "/numpy_unicode.npy", np.array(["hello", "\u00e9l\u00e8ve", "\U0001f600"], dtype='<U10'))

# Big-endian unicode
a = np.array(["abc", "xyz"], dtype='>U5')
np.save(d + "/numpy_be_unicode.npy", a)

# Datetime64
np.save(d + "/numpy_dt64.npy", np.array([0, 1000000000, 'NaT'], dtype='datetime64[ns]'))

# Timedelta64
np.save(d + "/numpy_td64.npy", np.array([0, 3600, -86400], dtype='timedelta64[s]'))

# Datetime64 with NaT
np.save(d + "/numpy_dt64_nat.npy", np.array(['NaT', 'NaT'], dtype='datetime64[ns]'))

print("NumPy files written")
|} in
  let rc =
    Unix.system
      (Printf.sprintf "python3 -c %s %s"
         (Filename.quote py_script) (Filename.quote tmp))
  in
  (match rc with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "failed to write numpy files");
  let check_floats ?(eps = 1e-6) name expected t =
    let actual = Npy.to_float_array t in
    Array.iteri
      (fun i e ->
        if Float.abs (actual.(i) -. e) > eps then
          failwith
            (Printf.sprintf "%s[%d]: expected %f, got %f" name i e actual.(i)))
      expected
  in
  let check_ints name expected t =
    let actual = Npy.to_int_array t in
    Array.iteri
      (fun i e ->
        if actual.(i) <> e then
          failwith
            (Printf.sprintf "%s[%d]: expected %d, got %d" name i e actual.(i)))
      expected
  in
  let check_int64s name expected t =
    let actual = Npy.to_int64_array t in
    Array.iteri
      (fun i e ->
        if Int64.compare actual.(i) e <> 0 then
          failwith
            (Printf.sprintf "%s[%d]: expected %Ld, got %Ld" name i e actual.(i)))
      expected
  in
  let check_bools name expected t =
    let actual = Npy.to_bool_array t in
    Array.iteri
      (fun i e ->
        if actual.(i) <> e then
          failwith
            (Printf.sprintf "%s[%d]: expected %b, got %b" name i e actual.(i)))
      expected
  in
  let check_complex ?(eps = 1e-6) name expected t =
    let actual = Npy.to_complex_array t in
    Array.iteri
      (fun i e ->
        if Float.abs (actual.(i).Complex.re -. e.Complex.re) > eps
           || Float.abs (actual.(i).Complex.im -. e.Complex.im) > eps then
          failwith
            (Printf.sprintf "%s[%d]: expected (%f,%f), got (%f,%f)" name i
               e.Complex.re e.Complex.im actual.(i).Complex.re actual.(i).Complex.im))
      expected
  in
  let check_strings name expected t =
    let actual = Npy.to_string_array t in
    Array.iteri
      (fun i e ->
        if actual.(i) <> e then
          failwith
            (Printf.sprintf "%s[%d]: expected %S, got %S" name i e actual.(i)))
      expected
  in
  let load_ok path =
    match Npy.load path with
    | Ok t -> t
    | Error e -> failwith (Printf.sprintf "failed to load %s: %s" path e)
  in
  (* Bool *)
  let t = load_ok (tmp ^ "/numpy_bool.npy") in
  assert (t.dtype = Bool);
  check_bools "bool" [| true; false; true; false |] t;
  Printf.printf "PASS numpy_bool.npy\n";
  (* Float16 *)
  let t = load_ok (tmp ^ "/numpy_f16.npy") in
  assert (t.dtype = Float16);
  check_floats ~eps:1e-3 "f16" [| 0.0; 1.0; -1.0; 0.5 |] t;
  Printf.printf "PASS numpy_f16.npy\n";
  (* Float32 *)
  let t = load_ok (tmp ^ "/numpy_f32.npy") in
  assert (t.dtype = Float32);
  assert (t.shape = [| 2; 3 |]);
  check_floats "f32" [| 1.5; -2.5; 3.0; 0.0; 42.0; -1.0 |] t;
  Printf.printf "PASS numpy_f32.npy\n";
  (* Float64 *)
  let t = load_ok (tmp ^ "/numpy_f64.npy") in
  assert (t.dtype = Float64);
  check_floats ~eps:1e-15 "f64"
    [| 3.141592653589793; -2.718281828459045; 0.0 |] t;
  Printf.printf "PASS numpy_f64.npy\n";
  (* Int8 *)
  let t = load_ok (tmp ^ "/numpy_i8.npy") in
  assert (t.dtype = Int8);
  check_ints "i8" [| -128; -1; 0; 1; 127 |] t;
  Printf.printf "PASS numpy_i8.npy\n";
  (* Uint8 *)
  let t = load_ok (tmp ^ "/numpy_u8.npy") in
  assert (t.dtype = Uint8);
  check_ints "u8" [| 0; 1; 200; 255 |] t;
  Printf.printf "PASS numpy_u8.npy\n";
  (* Int16 *)
  let t = load_ok (tmp ^ "/numpy_i16.npy") in
  assert (t.dtype = Int16);
  check_ints "i16" [| -32768; 0; 32767 |] t;
  Printf.printf "PASS numpy_i16.npy\n";
  (* Uint16 *)
  let t = load_ok (tmp ^ "/numpy_u16.npy") in
  assert (t.dtype = Uint16);
  check_ints "u16" [| 0; 1000; 65535 |] t;
  Printf.printf "PASS numpy_u16.npy\n";
  (* Int32 *)
  let t = load_ok (tmp ^ "/numpy_i32.npy") in
  assert (t.dtype = Int32);
  check_ints "i32" [| -100000; 0; 100000 |] t;
  Printf.printf "PASS numpy_i32.npy\n";
  (* Uint32 *)
  let t = load_ok (tmp ^ "/numpy_u32.npy") in
  assert (t.dtype = Uint32);
  check_ints "u32" [| 0; 1; 3000000000 |] t;
  Printf.printf "PASS numpy_u32.npy\n";
  (* Int64 *)
  let t = load_ok (tmp ^ "/numpy_i64.npy") in
  assert (t.dtype = Int64);
  check_int64s "i64" [| -1000000000000L; 0L; 1000000000000L |] t;
  Printf.printf "PASS numpy_i64.npy\n";
  (* Uint64 *)
  let t = load_ok (tmp ^ "/numpy_u64.npy") in
  assert (t.dtype = Uint64);
  check_int64s "u64" [| 0L; 1L; 1000000000000L |] t;
  Printf.printf "PASS numpy_u64.npy\n";
  (* Complex64 *)
  let t = load_ok (tmp ^ "/numpy_c64.npy") in
  assert (t.dtype = Complex64);
  check_complex "c64"
    [| Complex.{ re = 1.0; im = -2.0 }; Complex.{ re = 0.0; im = 3.0 } |] t;
  Printf.printf "PASS numpy_c64.npy\n";
  (* Complex128 *)
  let t = load_ok (tmp ^ "/numpy_c128.npy") in
  assert (t.dtype = Complex128);
  check_complex ~eps:1e-15 "c128"
    [| Complex.{ re = 1.0; im = -2.0 }; Complex.{ re = 0.0; im = 3.0 } |] t;
  Printf.printf "PASS numpy_c128.npy\n";
  (* 3d float32 *)
  let t = load_ok (tmp ^ "/numpy_3d.npy") in
  assert (t.dtype = Float32);
  assert (t.shape = [| 2; 3; 4 |]);
  check_floats "3d" (Array.init 24 Float.of_int) t;
  Printf.printf "PASS numpy_3d.npy\n";
  (* Big-endian float32 *)
  let t = load_ok (tmp ^ "/numpy_be_f32.npy") in
  assert (t.dtype = Float32);
  check_floats "be_f32" [| 1.0; 2.0; 3.0 |] t;
  Printf.printf "PASS numpy_be_f32.npy\n";
  (* Big-endian int32 *)
  let t = load_ok (tmp ^ "/numpy_be_i32.npy") in
  assert (t.dtype = Int32);
  check_ints "be_i32" [| -100; 0; 100 |] t;
  Printf.printf "PASS numpy_be_i32.npy\n";
  (* Big-endian float64 *)
  let t = load_ok (tmp ^ "/numpy_be_f64.npy") in
  assert (t.dtype = Float64);
  check_floats ~eps:1e-15 "be_f64" [| 1.0; 2.0; 3.0 |] t;
  Printf.printf "PASS numpy_be_f64.npy\n";
  (* Fortran-ordered *)
  let t = load_ok (tmp ^ "/numpy_fortran.npy") in
  assert (t.dtype = Float64);
  assert (t.order = Fortran);
  assert (t.shape = [| 2; 3 |]);
  Printf.printf "PASS numpy_fortran.npy\n";
  (* Byte strings *)
  let t = load_ok (tmp ^ "/numpy_bytes.npy") in
  (match t.dtype with Npy.Bytes _ -> () | _ -> failwith "expected Bytes dtype");
  check_strings "bytes" [| "hello"; "world"; "" |] t;
  Printf.printf "PASS numpy_bytes.npy\n";
  (* Unicode strings *)
  let t = load_ok (tmp ^ "/numpy_unicode.npy") in
  (match t.dtype with Npy.Unicode _ -> () | _ -> failwith "expected Unicode dtype");
  check_strings "unicode"
    [| "hello"; "\xc3\xa9l\xc3\xa8ve"; "\xf0\x9f\x98\x80" |] t;
  Printf.printf "PASS numpy_unicode.npy\n";
  (* Big-endian unicode *)
  let t = load_ok (tmp ^ "/numpy_be_unicode.npy") in
  (match t.dtype with Npy.Unicode _ -> () | _ -> failwith "expected Unicode dtype");
  check_strings "be_unicode" [| "abc"; "xyz" |] t;
  Printf.printf "PASS numpy_be_unicode.npy\n";
  (* Datetime64 *)
  let t = load_ok (tmp ^ "/numpy_dt64.npy") in
  (match t.dtype with Npy.Datetime64 Ns -> () | _ -> failwith "expected Datetime64 Ns dtype");
  check_int64s "dt64" [| 0L; 1_000_000_000L; Npy.nat |] t;
  assert (Npy.is_nat t 2);
  Printf.printf "PASS numpy_dt64.npy\n";
  (* Timedelta64 *)
  let t = load_ok (tmp ^ "/numpy_td64.npy") in
  (match t.dtype with Npy.Timedelta64 Sec -> () | _ -> failwith "expected Timedelta64 Sec dtype");
  check_int64s "td64" [| 0L; 3600L; -86400L |] t;
  Printf.printf "PASS numpy_td64.npy\n";
  (* Datetime64 with NaT *)
  let t = load_ok (tmp ^ "/numpy_dt64_nat.npy") in
  assert (Npy.is_nat t 0);
  assert (Npy.is_nat t 1);
  Printf.printf "PASS numpy_dt64_nat.npy\n";
  Printf.printf "ALL NumPy->OCaml checks passed\n"

let cleanup () =
  List.iter
    (fun f ->
      let path = tmp ^ "/" ^ f in
      if Sys.file_exists path then Sys.remove path)
    [
      "ocaml_bool.npy"; "ocaml_f16.npy"; "ocaml_f32.npy"; "ocaml_f64.npy";
      "ocaml_i8.npy"; "ocaml_u8.npy"; "ocaml_i16.npy";
      "ocaml_u16.npy"; "ocaml_i32.npy"; "ocaml_u32.npy";
      "ocaml_i64.npy"; "ocaml_u64.npy";
      "ocaml_c64.npy"; "ocaml_c128.npy";
      "ocaml_4d.npy"; "ocaml_fortran.npy";
      "ocaml_bytes.npy"; "ocaml_unicode.npy";
      "ocaml_dt64.npy"; "ocaml_td64.npy";
      "numpy_bool.npy"; "numpy_f16.npy"; "numpy_f32.npy"; "numpy_f64.npy";
      "numpy_i8.npy"; "numpy_u8.npy"; "numpy_i16.npy";
      "numpy_u16.npy"; "numpy_i32.npy"; "numpy_u32.npy";
      "numpy_i64.npy"; "numpy_u64.npy";
      "numpy_c64.npy"; "numpy_c128.npy";
      "numpy_3d.npy";
      "numpy_be_f32.npy"; "numpy_be_i32.npy"; "numpy_be_f64.npy";
      "numpy_fortran.npy";
      "numpy_bytes.npy"; "numpy_unicode.npy"; "numpy_be_unicode.npy";
      "numpy_dt64.npy"; "numpy_td64.npy"; "numpy_dt64_nat.npy";
    ]

let has_python_numpy () =
  match
    Unix.system "python3 -c 'import numpy' >/dev/null 2>&1"
  with
  | Unix.WEXITED 0 -> true
  | _ -> false

let () =
  if not (has_python_numpy ()) then (
    Printf.printf "SKIP: python3 with numpy not available\n";
    exit 0)
  else
    Fun.protect ~finally:cleanup (fun () ->
      Printf.printf "=== OCaml -> NumPy ===\n";
      ocaml_to_numpy ();
      Printf.printf "\n=== NumPy -> OCaml ===\n";
      numpy_to_ocaml ();
      Printf.printf "\nAll interop tests passed.\n")
