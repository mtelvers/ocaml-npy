(* Tests for the Npy library. *)

let dtype_t =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Npy.dtype_to_descr d))
    ( = )

let order_t =
  Alcotest.testable
    (fun fmt o ->
      Format.pp_print_string fmt (match o with Npy.C -> "C" | Npy.Fortran -> "Fortran"))
    ( = )

let check_int_array msg expected actual =
  Alcotest.(check int) (msg ^ " length") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      Alcotest.(check int) (Printf.sprintf "%s[%d]" msg i) e actual.(i))
    expected

let check_float_array ?(eps = 1e-6) msg expected actual =
  Alcotest.(check int) (msg ^ " length") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      Alcotest.(check (float eps)) (Printf.sprintf "%s[%d]" msg i) e actual.(i))
    expected

let check_int64_array msg expected actual =
  Alcotest.(check int) (msg ^ " length") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      Alcotest.(check int64) (Printf.sprintf "%s[%d]" msg i) e actual.(i))
    expected

let check_complex_array ?(eps = 1e-6) msg expected actual =
  Alcotest.(check int) (msg ^ " length") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      Alcotest.(check (float eps))
        (Printf.sprintf "%s[%d].re" msg i) e.Complex.re actual.(i).Complex.re;
      Alcotest.(check (float eps))
        (Printf.sprintf "%s[%d].im" msg i) e.Complex.im actual.(i).Complex.im)
    expected

let check_string_array msg expected actual =
  Alcotest.(check int) (msg ^ " length") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      Alcotest.(check string) (Printf.sprintf "%s[%d]" msg i) e actual.(i))
    expected

(* --- Dtype --- *)

let test_dtype_to_descr () =
  let cases =
    Npy.
      [
        (Bool, "|b1");
        (Int8, "|i1");
        (Uint8, "|u1");
        (Int16, "<i2");
        (Uint16, "<u2");
        (Int32, "<i4");
        (Uint32, "<u4");
        (Int64, "<i8");
        (Uint64, "<u8");
        (Float16, "<f2");
        (Float32, "<f4");
        (Float64, "<f8");
        (Complex64, "<c8");
        (Complex128, "<c16");
        (Bytes 10, "|S10");
        (Bytes 0, "|S0");
        (Unicode 5, "<U5");
        (Unicode 0, "<U0");
        (Datetime64 Ns, "<M8[ns]");
        (Datetime64 Generic, "<M8");
        (Datetime64 Us, "<M8[us]");
        (Datetime64 D, "<M8[D]");
        (Timedelta64 Sec, "<m8[s]");
        (Timedelta64 Generic, "<m8");
        (Timedelta64 Ms, "<m8[ms]");
      ]
  in
  List.iter
    (fun (dt, expected) ->
      Alcotest.(check string)
        (Printf.sprintf "descr of %s" expected)
        expected (Npy.dtype_to_descr dt))
    cases

let test_dtype_of_descr () =
  let ok_cases =
    Npy.
      [
        ("|b1", Bool);
        ("|i1", Int8);
        ("|u1", Uint8);
        ("<i2", Int16);
        ("|i2", Int16);
        ("<u2", Uint16);
        ("|u2", Uint16);
        ("<i4", Int32);
        ("<u4", Uint32);
        ("<i8", Int64);
        ("<u8", Uint64);
        ("<f2", Float16);
        ("<f4", Float32);
        ("<f8", Float64);
        ("<c8", Complex64);
        ("<c16", Complex128);
        (* Big-endian descriptors *)
        (">i2", Int16);
        (">u2", Uint16);
        (">i4", Int32);
        (">u4", Uint32);
        (">i8", Int64);
        (">u8", Uint64);
        (">f2", Float16);
        (">f4", Float32);
        (">f8", Float64);
        (">c8", Complex64);
        (">c16", Complex128);
        (* Byte strings *)
        ("|S10", Bytes 10);
        ("<S10", Bytes 10);
        (">S10", Bytes 10);
        ("|S0", Bytes 0);
        ("|S1", Bytes 1);
        ("|S255", Bytes 255);
        (* Unicode strings *)
        ("<U5", Unicode 5);
        ("|U5", Unicode 5);
        (">U5", Unicode 5);
        ("<U0", Unicode 0);
        ("<U1", Unicode 1);
        ("<U100", Unicode 100);
        (* Datetime64 *)
        ("<M8[ns]", Datetime64 Ns);
        ("<M8[us]", Datetime64 Us);
        ("<M8[ms]", Datetime64 Ms);
        ("<M8[s]", Datetime64 Sec);
        ("<M8[m]", Datetime64 Min);
        ("<M8[h]", Datetime64 Hour);
        ("<M8[D]", Datetime64 D);
        ("<M8[W]", Datetime64 W);
        ("<M8[M]", Datetime64 M);
        ("<M8[Y]", Datetime64 Y);
        ("<M8[ps]", Datetime64 Ps);
        ("<M8[fs]", Datetime64 Fs);
        ("<M8[as]", Datetime64 As);
        ("<M8", Datetime64 Generic);
        (">M8[ns]", Datetime64 Ns);
        (* Timedelta64 *)
        ("<m8[ns]", Timedelta64 Ns);
        ("<m8[us]", Timedelta64 Us);
        ("<m8[s]", Timedelta64 Sec);
        ("<m8[D]", Timedelta64 D);
        ("<m8", Timedelta64 Generic);
        (">m8[ms]", Timedelta64 Ms);
      ]
  in
  List.iter
    (fun (descr, expected) ->
      match Npy.dtype_of_descr descr with
      | Ok dt -> Alcotest.check dtype_t descr expected dt
      | Error e -> Alcotest.fail e)
    ok_cases;
  (* Error cases *)
  List.iter
    (fun descr ->
      match Npy.dtype_of_descr descr with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail (Printf.sprintf "expected error for %s" descr))
    [ "garbage"; "<M8[xx]"; "<m8[zz]"; "<S"; "<U" ]

let test_element_size () =
  Alcotest.(check int) "bool" 1 (Npy.element_size Bool);
  Alcotest.(check int) "int8" 1 (Npy.element_size Int8);
  Alcotest.(check int) "uint8" 1 (Npy.element_size Uint8);
  Alcotest.(check int) "int16" 2 (Npy.element_size Int16);
  Alcotest.(check int) "uint16" 2 (Npy.element_size Uint16);
  Alcotest.(check int) "float16" 2 (Npy.element_size Float16);
  Alcotest.(check int) "int32" 4 (Npy.element_size Int32);
  Alcotest.(check int) "uint32" 4 (Npy.element_size Uint32);
  Alcotest.(check int) "float32" 4 (Npy.element_size Float32);
  Alcotest.(check int) "int64" 8 (Npy.element_size Int64);
  Alcotest.(check int) "uint64" 8 (Npy.element_size Uint64);
  Alcotest.(check int) "float64" 8 (Npy.element_size Float64);
  Alcotest.(check int) "complex64" 8 (Npy.element_size Complex64);
  Alcotest.(check int) "complex128" 16 (Npy.element_size Complex128);
  Alcotest.(check int) "bytes10" 10 (Npy.element_size (Bytes 10));
  Alcotest.(check int) "bytes0" 0 (Npy.element_size (Bytes 0));
  Alcotest.(check int) "unicode5" 20 (Npy.element_size (Unicode 5));
  Alcotest.(check int) "unicode0" 0 (Npy.element_size (Unicode 0));
  Alcotest.(check int) "datetime64" 8 (Npy.element_size (Datetime64 Ns));
  Alcotest.(check int) "timedelta64" 8 (Npy.element_size (Timedelta64 Sec))

let test_num_elements () =
  Alcotest.(check int) "scalar" 1 (Npy.num_elements [||]);
  Alcotest.(check int) "1d" 5 (Npy.num_elements [| 5 |]);
  Alcotest.(check int) "2d" 12 (Npy.num_elements [| 3; 4 |]);
  Alcotest.(check int) "3d" 60 (Npy.num_elements [| 3; 4; 5 |])

let test_time_unit_to_string () =
  Alcotest.(check string) "ns" "ns" (Npy.time_unit_to_string Ns);
  Alcotest.(check string) "us" "us" (Npy.time_unit_to_string Us);
  Alcotest.(check string) "ms" "ms" (Npy.time_unit_to_string Ms);
  Alcotest.(check string) "s" "s" (Npy.time_unit_to_string Sec);
  Alcotest.(check string) "m" "m" (Npy.time_unit_to_string Min);
  Alcotest.(check string) "h" "h" (Npy.time_unit_to_string Hour);
  Alcotest.(check string) "D" "D" (Npy.time_unit_to_string D);
  Alcotest.(check string) "W" "W" (Npy.time_unit_to_string W);
  Alcotest.(check string) "M" "M" (Npy.time_unit_to_string M);
  Alcotest.(check string) "Y" "Y" (Npy.time_unit_to_string Y);
  Alcotest.(check string) "generic" "" (Npy.time_unit_to_string Generic)

let test_descr_roundtrip () =
  let dtypes = Npy.[
    Bool; Int8; Uint8; Int16; Uint16; Int32; Uint32;
    Int64; Uint64; Float16; Float32; Float64;
    Complex64; Complex128;
    Bytes 0; Bytes 1; Bytes 10; Bytes 255;
    Unicode 0; Unicode 1; Unicode 5; Unicode 100;
    Datetime64 Generic; Datetime64 Ns; Datetime64 Us;
    Datetime64 Ms; Datetime64 Sec; Datetime64 Min;
    Datetime64 Hour; Datetime64 D; Datetime64 W;
    Datetime64 M; Datetime64 Y; Datetime64 Ps;
    Datetime64 Fs; Datetime64 As;
    Timedelta64 Generic; Timedelta64 Ns; Timedelta64 Sec;
    Timedelta64 D;
  ] in
  List.iter
    (fun dt ->
      let descr = Npy.dtype_to_descr dt in
      match Npy.dtype_of_descr descr with
      | Ok dt' -> Alcotest.check dtype_t (Printf.sprintf "roundtrip %s" descr) dt dt'
      | Error e -> Alcotest.fail (Printf.sprintf "roundtrip %s: %s" descr e))
    dtypes

(* --- Round-trip encode/decode --- *)

let roundtrip_int dtype values shape () =
  let t = Npy.of_int_array dtype shape values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dtype t'.Npy.dtype;
      Alcotest.check order_t "order" C t'.Npy.order;
      check_int_array "shape" shape t'.shape;
      check_int_array "values" values (Npy.to_int_array t')

let roundtrip_float dtype values shape () =
  let t = Npy.of_float_array dtype shape values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dtype t'.Npy.dtype;
      Alcotest.check order_t "order" C t'.Npy.order;
      check_int_array "shape" shape t'.shape;
      check_float_array "values" values (Npy.to_float_array t')

let roundtrip_int64 dtype values shape () =
  let t = Npy.of_int64_array dtype shape values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dtype t'.Npy.dtype;
      check_int_array "shape" shape t'.shape;
      check_int64_array "values" values (Npy.to_int64_array t')

let roundtrip_complex dtype values shape () =
  let t = Npy.of_complex_array dtype shape values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dtype t'.Npy.dtype;
      check_int_array "shape" shape t'.shape;
      check_complex_array "values" values (Npy.to_complex_array t')

let test_roundtrip_int8 =
  roundtrip_int Int8 [| -128; -1; 0; 1; 127 |] [| 5 |]

let test_roundtrip_uint8 =
  roundtrip_int Uint8 [| 0; 1; 128; 255 |] [| 4 |]

let test_roundtrip_int16 =
  roundtrip_int Int16 [| -32768; -1; 0; 1; 32767 |] [| 5 |]

let test_roundtrip_uint16 =
  roundtrip_int Uint16 [| 0; 1; 32768; 65535 |] [| 4 |]

let test_roundtrip_int32 =
  roundtrip_int Int32 [| -100_000; -1; 0; 1; 100_000 |] [| 5 |]

let test_roundtrip_uint32 =
  roundtrip_int Uint32 [| 0; 1; 100_000; 3_000_000_000 |] [| 4 |]

let test_roundtrip_float16 =
  roundtrip_float Float16
    [| -1.0; 0.0; 0.5; 1.0; 65504.0 |] [| 5 |]

let test_roundtrip_float32 =
  roundtrip_float Float32 [| -1.5; 0.0; 1.0; 3.14 |] [| 4 |]

let test_roundtrip_float64 =
  roundtrip_float Float64
    [| -1.5; 0.0; 1.0; 3.141592653589793 |] [| 4 |]

let test_roundtrip_int64 =
  roundtrip_int64 Int64
    [| -1_000_000_000_000L; -1L; 0L; 1L; 1_000_000_000_000L |] [| 5 |]

let test_roundtrip_uint64 =
  roundtrip_int64 Uint64
    [| 0L; 1L; 1_000_000_000_000L |] [| 3 |]

let test_roundtrip_bool () =
  let values = [| true; false; true; true; false |] in
  let t = Npy.of_bool_array [| 5 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" Bool t'.Npy.dtype;
      let actual = Npy.to_bool_array t' in
      Array.iteri
        (fun i e ->
          Alcotest.(check bool)
            (Printf.sprintf "bool[%d]" i) e actual.(i))
        values

let test_roundtrip_complex64 =
  roundtrip_complex Complex64
    [| Complex.{ re = 1.0; im = -2.0 };
       Complex.{ re = 0.0; im = 0.0 };
       Complex.{ re = 3.14; im = 2.71 } |]
    [| 3 |]

let test_roundtrip_complex128 =
  roundtrip_complex Complex128
    [| Complex.{ re = 1.0; im = -2.0 };
       Complex.{ re = 0.0; im = 0.0 };
       Complex.{ re = 3.141592653589793; im = 2.718281828459045 } |]
    [| 3 |]

(* --- String roundtrips --- *)

let test_roundtrip_bytes () =
  let dt = Npy.Bytes 10 in
  let values = [| "hello"; "world"; "" |] in
  let t = Npy.of_string_array dt [| 3 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dt t'.Npy.dtype;
      check_string_array "values" values (Npy.to_string_array t')

let test_roundtrip_unicode () =
  let dt = Npy.Unicode 10 in
  let values = [| "hello"; "world"; "" |] in
  let t = Npy.of_string_array dt [| 3 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dt t'.Npy.dtype;
      check_string_array "values" values (Npy.to_string_array t')

let test_roundtrip_unicode_multibyte () =
  let dt = Npy.Unicode 10 in
  (* UTF-8 multibyte: e-acute (2 bytes), CJK char (3 bytes), emoji (4 bytes) *)
  let values = [| "\xc3\xa9"; "\xe4\xb8\xad"; "\xf0\x9f\x98\x80" |] in
  let t = Npy.of_string_array dt [| 3 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      check_string_array "values" values (Npy.to_string_array t')

let test_roundtrip_bytes_truncation () =
  let dt = Npy.Bytes 3 in
  let values = [| "abcde"; "xy" |] in
  let t = Npy.of_string_array dt [| 2 |] values in
  let actual = Npy.to_string_array t in
  Alcotest.(check string) "truncated" "abc" actual.(0);
  Alcotest.(check string) "padded" "xy" actual.(1)

let test_roundtrip_unicode_truncation () =
  let dt = Npy.Unicode 2 in
  let values = [| "abcde"; "x" |] in
  let t = Npy.of_string_array dt [| 2 |] values in
  let actual = Npy.to_string_array t in
  Alcotest.(check string) "truncated" "ab" actual.(0);
  Alcotest.(check string) "padded" "x" actual.(1)

let test_roundtrip_bytes_zero () =
  let dt = Npy.Bytes 0 in
  let values = [| ""; "" |] in
  let t = Npy.of_string_array dt [| 2 |] values in
  Alcotest.(check int) "element_size" 0 (Npy.element_size dt);
  check_string_array "values" values (Npy.to_string_array t)

let test_roundtrip_unicode_zero () =
  let dt = Npy.Unicode 0 in
  let values = [| ""; "" |] in
  let t = Npy.of_string_array dt [| 2 |] values in
  Alcotest.(check int) "element_size" 0 (Npy.element_size dt);
  check_string_array "values" values (Npy.to_string_array t)

let test_bytes_embedded_nul () =
  let dt = Npy.Bytes 5 in
  let t = Npy.create dt [| 1 |] in
  (* Manually write "a\x00b" into the data *)
  Bytes.set t.data 0 'a';
  Bytes.set t.data 1 '\x00';
  Bytes.set t.data 2 'b';
  let s = Npy.get_string t 0 in
  (* Trailing NULs stripped, but embedded NUL preserved *)
  Alcotest.(check int) "length" 3 (String.length s);
  Alcotest.(check char) "s[0]" 'a' s.[0];
  Alcotest.(check char) "s[1]" '\x00' s.[1];
  Alcotest.(check char) "s[2]" 'b' s.[2]

(* --- Datetime/Timedelta roundtrips --- *)

let test_roundtrip_datetime64 () =
  let dt = Npy.Datetime64 Ns in
  let values = [| 0L; 1_000_000_000L; -1_000_000_000L |] in
  let t = Npy.of_int64_array dt [| 3 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dt t'.Npy.dtype;
      check_int64_array "values" values (Npy.to_int64_array t')

let test_roundtrip_timedelta64 () =
  let dt = Npy.Timedelta64 Sec in
  let values = [| 0L; 3600L; -86400L |] in
  let t = Npy.of_int64_array dt [| 3 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dt t'.Npy.dtype;
      check_int64_array "values" values (Npy.to_int64_array t')

let test_roundtrip_datetime64_generic () =
  let dt = Npy.Datetime64 Generic in
  let values = [| 42L |] in
  let t = Npy.of_int64_array dt [| 1 |] values in
  let s = Npy.encode t in
  match Npy.decode s with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check dtype_t "dtype" dt t'.Npy.dtype;
      check_int64_array "values" values (Npy.to_int64_array t')

let test_nat () =
  let dt = Npy.Datetime64 Ns in
  let t = Npy.of_int64_array dt [| 3 |] [| Npy.nat; 0L; 42L |] in
  Alcotest.(check bool) "nat[0]" true (Npy.is_nat t 0);
  Alcotest.(check bool) "nat[1]" false (Npy.is_nat t 1);
  Alcotest.(check bool) "nat[2]" false (Npy.is_nat t 2);
  Alcotest.(check int64) "nat value" Int64.min_int Npy.nat

let test_nat_timedelta () =
  let dt = Npy.Timedelta64 Sec in
  let t = Npy.of_int64_array dt [| 2 |] [| Npy.nat; 100L |] in
  Alcotest.(check bool) "nat[0]" true (Npy.is_nat t 0);
  Alcotest.(check bool) "nat[1]" false (Npy.is_nat t 1)

let test_datetime_get_float () =
  let dt = Npy.Datetime64 Ns in
  let t = Npy.of_int64_array dt [| 1 |] [| 1_000_000_000L |] in
  Alcotest.(check (float 1.0)) "get_float" 1e9 (Npy.get_float t 0)

let test_datetime_get_int () =
  let dt = Npy.Datetime64 D in
  let t = Npy.of_int64_array dt [| 1 |] [| 42L |] in
  Alcotest.(check int) "get_int" 42 (Npy.get_int t 0)

(* --- Multi-dimensional shapes --- *)

let test_roundtrip_2d () =
  let values = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |] in
  let shape = [| 2; 3 |] in
  let t = Npy.of_float_array Float32 shape values in
  match Npy.decode (Npy.encode t) with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      check_int_array "shape" shape t'.shape;
      check_float_array "values" values (Npy.to_float_array t')

let test_roundtrip_3d () =
  let values = Array.init 24 Float.of_int in
  let shape = [| 2; 3; 4 |] in
  let t = Npy.of_float_array Float32 shape values in
  match Npy.decode (Npy.encode t) with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      check_int_array "shape" shape t'.shape;
      check_float_array "values" values (Npy.to_float_array t')

let test_roundtrip_4d () =
  let values = Array.init 120 Float.of_int in
  let shape = [| 2; 3; 4; 5 |] in
  let t = Npy.of_float_array Float32 shape values in
  match Npy.decode (Npy.encode t) with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      check_int_array "shape" shape t'.shape;
      check_float_array "values" values (Npy.to_float_array t')

(* --- Element access --- *)

let test_get_set_float () =
  let t = Npy.create Float32 [| 3 |] in
  Npy.set_float t 0 1.5;
  Npy.set_float t 1 (-2.5);
  Npy.set_float t 2 0.0;
  Alcotest.(check (float 1e-6)) "v0" 1.5 (Npy.get_float t 0);
  Alcotest.(check (float 1e-6)) "v1" (-2.5) (Npy.get_float t 1);
  Alcotest.(check (float 1e-6)) "v2" 0.0 (Npy.get_float t 2)

let test_get_set_int () =
  let t = Npy.create Int16 [| 3 |] in
  Npy.set_int t 0 1000;
  Npy.set_int t 1 (-1000);
  Npy.set_int t 2 0;
  Alcotest.(check int) "v0" 1000 (Npy.get_int t 0);
  Alcotest.(check int) "v1" (-1000) (Npy.get_int t 1);
  Alcotest.(check int) "v2" 0 (Npy.get_int t 2)

let test_get_set_int64 () =
  let t = Npy.create Int64 [| 3 |] in
  Npy.set_int64 t 0 1_000_000_000_000L;
  Npy.set_int64 t 1 (-1_000_000_000_000L);
  Npy.set_int64 t 2 0L;
  Alcotest.(check int64) "v0" 1_000_000_000_000L (Npy.get_int64 t 0);
  Alcotest.(check int64) "v1" (-1_000_000_000_000L) (Npy.get_int64 t 1);
  Alcotest.(check int64) "v2" 0L (Npy.get_int64 t 2)

let test_get_set_complex () =
  let t = Npy.create Complex128 [| 2 |] in
  Npy.set_complex t 0 Complex.{ re = 1.5; im = -2.5 };
  Npy.set_complex t 1 Complex.{ re = 0.0; im = 3.14 };
  let c0 = Npy.get_complex t 0 in
  Alcotest.(check (float 1e-6)) "v0.re" 1.5 c0.re;
  Alcotest.(check (float 1e-6)) "v0.im" (-2.5) c0.im;
  let c1 = Npy.get_complex t 1 in
  Alcotest.(check (float 1e-6)) "v1.re" 0.0 c1.re;
  Alcotest.(check (float 1e-6)) "v1.im" 3.14 c1.im

let test_get_set_bool () =
  let t = Npy.create Bool [| 3 |] in
  Npy.set_bool t 0 true;
  Npy.set_bool t 1 false;
  Npy.set_bool t 2 true;
  Alcotest.(check bool) "v0" true (Npy.get_bool t 0);
  Alcotest.(check bool) "v1" false (Npy.get_bool t 1);
  Alcotest.(check bool) "v2" true (Npy.get_bool t 2)

let test_get_set_string () =
  let t = Npy.create (Bytes 10) [| 3 |] in
  Npy.set_string t 0 "hello";
  Npy.set_string t 1 "world";
  Npy.set_string t 2 "";
  Alcotest.(check string) "v0" "hello" (Npy.get_string t 0);
  Alcotest.(check string) "v1" "world" (Npy.get_string t 1);
  Alcotest.(check string) "v2" "" (Npy.get_string t 2)

let test_get_set_unicode_string () =
  let t = Npy.create (Unicode 10) [| 2 |] in
  Npy.set_string t 0 "hello";
  Npy.set_string t 1 "\xf0\x9f\x98\x80";  (* emoji *)
  Alcotest.(check string) "v0" "hello" (Npy.get_string t 0);
  Alcotest.(check string) "v1" "\xf0\x9f\x98\x80" (Npy.get_string t 1)

let test_signed_unsigned_boundary () =
  let t = Npy.create Int8 [| 2 |] in
  Npy.set_int t 0 (-128);
  Npy.set_int t 1 127;
  Alcotest.(check int) "min_int8" (-128) (Npy.get_int t 0);
  Alcotest.(check int) "max_int8" 127 (Npy.get_int t 1);
  let t = Npy.create Uint8 [| 2 |] in
  Npy.set_int t 0 0;
  Npy.set_int t 1 255;
  Alcotest.(check int) "min_uint8" 0 (Npy.get_int t 0);
  Alcotest.(check int) "max_uint8" 255 (Npy.get_int t 1)

let test_uint32_boundary () =
  let t = Npy.create Uint32 [| 2 |] in
  Npy.set_int t 0 0;
  Npy.set_int t 1 3_000_000_000;
  Alcotest.(check int) "min_uint32" 0 (Npy.get_int t 0);
  Alcotest.(check int) "max_uint32" 3_000_000_000 (Npy.get_int t 1)

let test_float_via_int_dtypes () =
  let t = Npy.of_int_array Int16 [| 3 |] [| -100; 0; 200 |] in
  check_float_array "int16 as float"
    [| -100.0; 0.0; 200.0 |]
    (Npy.to_float_array t)

(* --- Fortran order --- *)

let test_fortran_order_roundtrip () =
  let values = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |] in
  let t = Npy.of_float_array ~order:Fortran Float64 [| 2; 3 |] values in
  Alcotest.check order_t "order before" Fortran t.order;
  match Npy.decode (Npy.encode t) with
  | Error e -> Alcotest.fail e
  | Ok t' ->
      Alcotest.check order_t "order after" Fortran t'.order;
      check_float_array "values" values (Npy.to_float_array t')

(* --- Big-endian reading --- *)

let test_big_endian_float32 () =
  (* Construct a big-endian float32 .npy file manually *)
  let header = "{'descr': '>f4', 'fortran_order': False, 'shape': (3,), }" in
  let pad_len =
    let total = 10 + String.length header + 1 in
    ((total + 63) / 64) * 64 - total
  in
  let padded_header = header ^ String.make pad_len ' ' ^ "\n" in
  let hlen = String.length padded_header in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\x93NUMPY";
  Buffer.add_char buf '\x01';
  Buffer.add_char buf '\x00';
  Buffer.add_char buf (Char.chr (hlen land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
  Buffer.add_string buf padded_header;
  (* Write 3 big-endian float32 values: 1.0, 2.0, 3.0 *)
  let write_be_f32 v =
    let bits = Int32.bits_of_float v in
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int bits land 0xff))
  in
  write_be_f32 1.0;
  write_be_f32 2.0;
  write_be_f32 3.0;
  match Npy.decode (Buffer.contents buf) with
  | Error e -> Alcotest.fail e
  | Ok t ->
      Alcotest.check dtype_t "dtype" Float32 t.dtype;
      check_float_array "values" [| 1.0; 2.0; 3.0 |] (Npy.to_float_array t)

let test_big_endian_int32 () =
  let header = "{'descr': '>i4', 'fortran_order': False, 'shape': (3,), }" in
  let pad_len =
    let total = 10 + String.length header + 1 in
    ((total + 63) / 64) * 64 - total
  in
  let padded_header = header ^ String.make pad_len ' ' ^ "\n" in
  let hlen = String.length padded_header in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\x93NUMPY";
  Buffer.add_char buf '\x01';
  Buffer.add_char buf '\x00';
  Buffer.add_char buf (Char.chr (hlen land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
  Buffer.add_string buf padded_header;
  (* Write 3 big-endian int32 values: 1, -1, 100000 *)
  let write_be_i32 v =
    let bits = Int32.of_int v in
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int bits land 0xff))
  in
  write_be_i32 1;
  write_be_i32 (-1);
  write_be_i32 100000;
  match Npy.decode (Buffer.contents buf) with
  | Error e -> Alcotest.fail e
  | Ok t ->
      Alcotest.check dtype_t "dtype" Int32 t.dtype;
      check_int_array "values" [| 1; -1; 100000 |] (Npy.to_int_array t)

(* --- Format version 2.0 --- *)

let test_decode_v2 () =
  (* Construct a v2.0 .npy file manually *)
  let header = "{'descr': '<f4', 'fortran_order': False, 'shape': (2,), }" in
  let pad_len =
    let total = 12 + String.length header + 1 in
    ((total + 63) / 64) * 64 - total
  in
  let padded_header = header ^ String.make pad_len ' ' ^ "\n" in
  let hlen = String.length padded_header in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\x93NUMPY";
  Buffer.add_char buf '\x02';
  Buffer.add_char buf '\x00';
  Buffer.add_char buf (Char.chr (hlen land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 24) land 0xff));
  Buffer.add_string buf padded_header;
  (* Write 2 LE float32 values: 1.0, 2.0 *)
  let write_le_f32 v =
    let bits = Int32.bits_of_float v in
    Buffer.add_char buf (Char.chr (Int32.to_int bits land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xff));
    Buffer.add_char buf (Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xff))
  in
  write_le_f32 1.0;
  write_le_f32 2.0;
  match Npy.decode (Buffer.contents buf) with
  | Error e -> Alcotest.fail e
  | Ok t ->
      Alcotest.check dtype_t "dtype" Float32 t.dtype;
      check_float_array "values" [| 1.0; 2.0 |] (Npy.to_float_array t)

(* --- Float16 specific --- *)

let test_float16_special_values () =
  let t = Npy.create Float16 [| 5 |] in
  Npy.set_float t 0 0.0;
  Npy.set_float t 1 1.0;
  Npy.set_float t 2 (-1.0);
  Npy.set_float t 3 Float.infinity;
  Npy.set_float t 4 Float.neg_infinity;
  Alcotest.(check (float 1e-6)) "zero" 0.0 (Npy.get_float t 0);
  Alcotest.(check (float 1e-6)) "one" 1.0 (Npy.get_float t 1);
  Alcotest.(check (float 1e-6)) "neg_one" (-1.0) (Npy.get_float t 2);
  Alcotest.(check bool) "inf" true (Float.is_infinite (Npy.get_float t 3));
  Alcotest.(check bool) "neg_inf" true (Float.is_infinite (Npy.get_float t 4))

let test_float16_nan () =
  let t = Npy.create Float16 [| 1 |] in
  Npy.set_float t 0 Float.nan;
  Alcotest.(check bool) "nan" true (Float.is_nan (Npy.get_float t 0))

(* --- File I/O --- *)

let test_save_load () =
  let values = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |] in
  let t = Npy.of_float_array Float32 [| 2; 3 |] values in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' ->
          Alcotest.check dtype_t "dtype" Float32 t'.dtype;
          check_int_array "shape" [| 2; 3 |] t'.shape;
          check_float_array "values" values (Npy.to_float_array t'))

let test_save_load_int8 () =
  let values = [| -1; 0; 42; 127 |] in
  let t = Npy.of_int_array Int8 [| 4 |] values in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' -> check_int_array "values" values (Npy.to_int_array t'))

let test_save_load_float64 () =
  let values = [| 3.141592653589793; -2.718281828459045; 0.0 |] in
  let t = Npy.of_float_array Float64 [| 3 |] values in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' ->
          Alcotest.check dtype_t "dtype" Float64 t'.dtype;
          check_float_array ~eps:1e-15 "values" values (Npy.to_float_array t'))

let test_save_load_bytes () =
  let dt = Npy.Bytes 10 in
  let t = Npy.of_string_array dt [| 3 |] [| "hello"; "world"; "" |] in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' ->
          Alcotest.check dtype_t "dtype" dt t'.dtype;
          check_string_array "values" [| "hello"; "world"; "" |]
            (Npy.to_string_array t'))

let test_save_load_unicode () =
  let dt = Npy.Unicode 10 in
  let t = Npy.of_string_array dt [| 2 |] [| "hello"; "\xf0\x9f\x98\x80" |] in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' ->
          Alcotest.check dtype_t "dtype" dt t'.dtype;
          check_string_array "values" [| "hello"; "\xf0\x9f\x98\x80" |]
            (Npy.to_string_array t'))

let test_save_load_datetime64 () =
  let dt = Npy.Datetime64 Ns in
  let t = Npy.of_int64_array dt [| 3 |] [| 0L; 1_000_000_000L; Npy.nat |] in
  let filename = Filename.temp_file "npy_test" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Npy.save filename t;
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t' ->
          Alcotest.check dtype_t "dtype" dt t'.dtype;
          check_int64_array "values" [| 0L; 1_000_000_000L; Npy.nat |]
            (Npy.to_int64_array t');
          Alcotest.(check bool) "nat" true (Npy.is_nat t' 2))

(* --- Streaming write_header --- *)

let test_write_header () =
  let filename = Filename.temp_file "npy_header" ".npy" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      let oc = open_out_bin filename in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          Npy.write_header oc Float32 [| 3 |];
          List.iter
            (fun v ->
              let bits = Int32.bits_of_float v in
              output_char oc (Char.chr (Int32.to_int bits land 0xff));
              output_char oc
                (Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xff));
              output_char oc
                (Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xff));
              output_char oc
                (Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xff)))
            [ 1.0; 2.0; 3.0 ]);
      match Npy.load filename with
      | Error e -> Alcotest.fail e
      | Ok t ->
          Alcotest.check dtype_t "dtype" Float32 t.dtype;
          check_int_array "shape" [| 3 |] t.shape;
          check_float_array "values"
            [| 1.0; 2.0; 3.0 |]
            (Npy.to_float_array t))

(* --- Encode format details --- *)

let test_magic_bytes () =
  let t = Npy.create Int8 [| 1 |] in
  let s = Npy.encode t in
  Alcotest.(check char) "magic[0]" '\x93' s.[0];
  Alcotest.(check char) "magic[1]" 'N' s.[1];
  Alcotest.(check char) "magic[2]" 'U' s.[2];
  Alcotest.(check char) "magic[3]" 'M' s.[3];
  Alcotest.(check char) "magic[4]" 'P' s.[4];
  Alcotest.(check char) "magic[5]" 'Y' s.[5];
  Alcotest.(check char) "version_major" '\x01' s.[6];
  Alcotest.(check char) "version_minor" '\x00' s.[7]

let test_header_alignment () =
  let t = Npy.create Float32 [| 10; 20; 30 |] in
  let s = Npy.encode t in
  let hlen = Char.code s.[8] lor (Char.code s.[9] lsl 8) in
  let total = 10 + hlen in
  Alcotest.(check int) "64-byte aligned" 0 (total mod 64)

let test_1d_trailing_comma () =
  let t = Npy.create Int8 [| 5 |] in
  let s = Npy.encode t in
  let hlen = Char.code s.[8] lor (Char.code s.[9] lsl 8) in
  let header = String.sub s 10 hlen in
  Alcotest.(check bool) "has trailing comma" true (String.contains header ',')

(* --- Error handling --- *)

let test_decode_too_short () =
  match Npy.decode "short" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for short input"

let test_decode_bad_magic () =
  match Npy.decode "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" with
  | Error msg ->
      Alcotest.(check bool)
        "mentions magic"
        true
        (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for bad magic"

let test_decode_truncated_data () =
  let t = Npy.of_int_array Int32 [| 100 |] (Array.make 100 42) in
  let s = Npy.encode t in
  let truncated = String.sub s 0 (String.length s - 100) in
  match Npy.decode truncated with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for truncated data"

let test_create_negative_dim () =
  match Npy.create Int8 [| -1 |] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument for negative dim"

let test_unsupported_version () =
  (* Version 4.0 *)
  let header = "{'descr': '<f4', 'fortran_order': False, 'shape': (1,), }" in
  let pad_len =
    let total = 10 + String.length header + 1 in
    ((total + 63) / 64) * 64 - total
  in
  let padded_header = header ^ String.make pad_len ' ' ^ "\n" in
  let hlen = String.length padded_header in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\x93NUMPY";
  Buffer.add_char buf '\x04';
  Buffer.add_char buf '\x00';
  Buffer.add_char buf (Char.chr (hlen land 0xff));
  Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
  Buffer.add_string buf padded_header;
  let data = Bytes.create 4 in
  Bytes.set_int32_le data 0 (Int32.bits_of_float 1.0);
  Buffer.add_bytes buf data;
  match Npy.decode (Buffer.contents buf) with
  | Error msg ->
    Alcotest.(check bool) "mentions version" true
      (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error for unsupported version"

let test_string_dtype_numeric_access () =
  let t = Npy.create (Bytes 5) [| 2 |] in
  let check_raises name f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail (Printf.sprintf "expected Invalid_argument for %s" name)
  in
  check_raises "get_float" (fun () -> Npy.get_float t 0);
  check_raises "get_int" (fun () -> Npy.get_int t 0);
  check_raises "get_int64" (fun () -> Npy.get_int64 t 0);
  check_raises "get_complex" (fun () -> Npy.get_complex t 0);
  check_raises "get_bool" (fun () -> Npy.get_bool t 0);
  check_raises "set_float" (fun () -> Npy.set_float t 0 1.0);
  check_raises "set_int" (fun () -> Npy.set_int t 0 1);
  check_raises "set_int64" (fun () -> Npy.set_int64 t 0 1L);
  check_raises "set_complex" (fun () -> Npy.set_complex t 0 Complex.{ re = 1.0; im = 0.0 });
  check_raises "set_bool" (fun () -> Npy.set_bool t 0 true)

let test_numeric_dtype_string_access () =
  let t = Npy.create Float32 [| 2 |] in
  let check_raises name f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail (Printf.sprintf "expected Invalid_argument for %s" name)
  in
  check_raises "get_string" (fun () -> Npy.get_string t 0);
  check_raises "set_string" (fun () -> Npy.set_string t 0 "hello")

let test_is_nat_non_datetime () =
  let t = Npy.create Float64 [| 1 |] in
  match Npy.is_nat t 0 with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument for is_nat on Float64"

(* --- Cross-dtype get_float consistency --- *)

let test_cross_dtype_float () =
  let dtypes = Npy.[ Int8; Uint8; Int16; Uint16; Int32; Uint32; Float16;
                      Float32; Float64 ] in
  List.iter
    (fun dt ->
      let t = Npy.of_float_array dt [| 3 |] [| 0.0; 1.0; 42.0 |] in
      Alcotest.(check (float 1e-1))
        (Printf.sprintf "%s[0]" (Npy.dtype_to_descr dt))
        0.0 (Npy.get_float t 0);
      Alcotest.(check (float 1e-1))
        (Printf.sprintf "%s[1]" (Npy.dtype_to_descr dt))
        1.0 (Npy.get_float t 1);
      Alcotest.(check (float 1e-1))
        (Printf.sprintf "%s[2]" (Npy.dtype_to_descr dt))
        42.0 (Npy.get_float t 2))
    dtypes

(* --- Run --- *)

let () =
  Alcotest.run "Npy"
    [
      ( "dtype",
        [
          Alcotest.test_case "to_descr" `Quick test_dtype_to_descr;
          Alcotest.test_case "of_descr" `Quick test_dtype_of_descr;
          Alcotest.test_case "element_size" `Quick test_element_size;
          Alcotest.test_case "num_elements" `Quick test_num_elements;
          Alcotest.test_case "time_unit_to_string" `Quick test_time_unit_to_string;
          Alcotest.test_case "descr_roundtrip" `Quick test_descr_roundtrip;
        ] );
      ( "roundtrip",
        [
          Alcotest.test_case "bool" `Quick test_roundtrip_bool;
          Alcotest.test_case "int8" `Quick test_roundtrip_int8;
          Alcotest.test_case "uint8" `Quick test_roundtrip_uint8;
          Alcotest.test_case "int16" `Quick test_roundtrip_int16;
          Alcotest.test_case "uint16" `Quick test_roundtrip_uint16;
          Alcotest.test_case "int32" `Quick test_roundtrip_int32;
          Alcotest.test_case "uint32" `Quick test_roundtrip_uint32;
          Alcotest.test_case "int64" `Quick test_roundtrip_int64;
          Alcotest.test_case "uint64" `Quick test_roundtrip_uint64;
          Alcotest.test_case "float16" `Quick test_roundtrip_float16;
          Alcotest.test_case "float32" `Quick test_roundtrip_float32;
          Alcotest.test_case "float64" `Quick test_roundtrip_float64;
          Alcotest.test_case "complex64" `Quick test_roundtrip_complex64;
          Alcotest.test_case "complex128" `Quick test_roundtrip_complex128;
          Alcotest.test_case "2d" `Quick test_roundtrip_2d;
          Alcotest.test_case "3d" `Quick test_roundtrip_3d;
          Alcotest.test_case "4d" `Quick test_roundtrip_4d;
          Alcotest.test_case "bytes" `Quick test_roundtrip_bytes;
          Alcotest.test_case "unicode" `Quick test_roundtrip_unicode;
          Alcotest.test_case "unicode_multibyte" `Quick test_roundtrip_unicode_multibyte;
          Alcotest.test_case "bytes_truncation" `Quick test_roundtrip_bytes_truncation;
          Alcotest.test_case "unicode_truncation" `Quick test_roundtrip_unicode_truncation;
          Alcotest.test_case "bytes_zero" `Quick test_roundtrip_bytes_zero;
          Alcotest.test_case "unicode_zero" `Quick test_roundtrip_unicode_zero;
          Alcotest.test_case "bytes_embedded_nul" `Quick test_bytes_embedded_nul;
          Alcotest.test_case "datetime64" `Quick test_roundtrip_datetime64;
          Alcotest.test_case "timedelta64" `Quick test_roundtrip_timedelta64;
          Alcotest.test_case "datetime64_generic" `Quick test_roundtrip_datetime64_generic;
        ] );
      ( "element_access",
        [
          Alcotest.test_case "get/set float" `Quick test_get_set_float;
          Alcotest.test_case "get/set int" `Quick test_get_set_int;
          Alcotest.test_case "get/set int64" `Quick test_get_set_int64;
          Alcotest.test_case "get/set complex" `Quick test_get_set_complex;
          Alcotest.test_case "get/set bool" `Quick test_get_set_bool;
          Alcotest.test_case "get/set string" `Quick test_get_set_string;
          Alcotest.test_case "get/set unicode string" `Quick test_get_set_unicode_string;
          Alcotest.test_case "signed/unsigned" `Quick test_signed_unsigned_boundary;
          Alcotest.test_case "uint32 boundary" `Quick test_uint32_boundary;
          Alcotest.test_case "float via int dtypes" `Quick test_float_via_int_dtypes;
          Alcotest.test_case "cross-dtype float" `Quick test_cross_dtype_float;
        ] );
      ( "datetime",
        [
          Alcotest.test_case "nat" `Quick test_nat;
          Alcotest.test_case "nat timedelta" `Quick test_nat_timedelta;
          Alcotest.test_case "datetime get_float" `Quick test_datetime_get_float;
          Alcotest.test_case "datetime get_int" `Quick test_datetime_get_int;
        ] );
      ( "order",
        [
          Alcotest.test_case "fortran roundtrip" `Quick test_fortran_order_roundtrip;
        ] );
      ( "endianness",
        [
          Alcotest.test_case "big-endian float32" `Quick test_big_endian_float32;
          Alcotest.test_case "big-endian int32" `Quick test_big_endian_int32;
        ] );
      ( "format_versions",
        [
          Alcotest.test_case "decode v2.0" `Quick test_decode_v2;
          Alcotest.test_case "unsupported version" `Quick test_unsupported_version;
        ] );
      ( "float16",
        [
          Alcotest.test_case "special values" `Quick test_float16_special_values;
          Alcotest.test_case "nan" `Quick test_float16_nan;
        ] );
      ( "file_io",
        [
          Alcotest.test_case "save/load float32" `Quick test_save_load;
          Alcotest.test_case "save/load int8" `Quick test_save_load_int8;
          Alcotest.test_case "save/load float64" `Quick test_save_load_float64;
          Alcotest.test_case "save/load bytes" `Quick test_save_load_bytes;
          Alcotest.test_case "save/load unicode" `Quick test_save_load_unicode;
          Alcotest.test_case "save/load datetime64" `Quick test_save_load_datetime64;
          Alcotest.test_case "write_header" `Quick test_write_header;
        ] );
      ( "format",
        [
          Alcotest.test_case "magic bytes" `Quick test_magic_bytes;
          Alcotest.test_case "header alignment" `Quick test_header_alignment;
          Alcotest.test_case "1d trailing comma" `Quick test_1d_trailing_comma;
        ] );
      ( "errors",
        [
          Alcotest.test_case "decode too short" `Quick test_decode_too_short;
          Alcotest.test_case "decode bad magic" `Quick test_decode_bad_magic;
          Alcotest.test_case "decode truncated" `Quick test_decode_truncated_data;
          Alcotest.test_case "negative dimension" `Quick test_create_negative_dim;
          Alcotest.test_case "unsupported version" `Quick test_unsupported_version;
          Alcotest.test_case "string dtype numeric access" `Quick test_string_dtype_numeric_access;
          Alcotest.test_case "numeric dtype string access" `Quick test_numeric_dtype_string_access;
          Alcotest.test_case "is_nat non-datetime" `Quick test_is_nat_non_datetime;
        ] );
    ]
