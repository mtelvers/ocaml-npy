# ocaml-npy

A pure OCaml library for reading and writing NumPy `.npy` files (format versions 1.0, 2.0, and 3.0).

## Features

- All 14 standard numeric dtypes: `bool`, `int8`, `uint8`, `int16`, `uint16`, `int32`, `uint32`, `int64`, `uint64`, `float16`, `float32`, `float64`, `complex64`, `complex128`
- Byte strings (`S`) and Unicode strings (`U`) with UTF-8 conversion
- Temporal types: `datetime64` (`M8`) and `timedelta64` (`m8`) with all standard time units
- C (row-major) and Fortran (column-major) memory order
- Big-endian and little-endian data (big-endian is byte-swapped on read)
- Streaming writes via `write_header`

## Installation

```
opam install npy
```

## Usage

### Numeric arrays

```ocaml
(* Write a 2x3 float32 array *)
let arr = Npy.of_float_array Float32 [| 2; 3 |]
  [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]
in
Npy.save "output.npy" arr;

(* Read it back *)
match Npy.load "output.npy" with
| Ok t ->
  Printf.printf "shape: %dx%d\n" t.shape.(0) t.shape.(1);
  let values = Npy.to_float_array t in
  Printf.printf "first element: %f\n" values.(0)
| Error msg -> failwith msg
```

### String arrays

```ocaml
(* Byte strings — each element holds up to 10 bytes *)
let arr = Npy.of_string_array (Bytes 10) [| 3 |]
  [| "hello"; "world"; "" |]
in
Npy.save "strings.npy" arr;

(* Unicode strings — each element holds up to 10 UCS-4 codepoints *)
let arr = Npy.of_string_array (Unicode 10) [| 2 |]
  [| "hello"; "\xf0\x9f\x98\x80" |]  (* "hello" and a smiley emoji *)
in
Npy.save "unicode.npy" arr
```

### Datetime and timedelta

```ocaml
(* Datetime64 with nanosecond resolution *)
let arr = Npy.of_int64_array (Datetime64 Ns) [| 3 |]
  [| 0L; 1_000_000_000L; Npy.nat |]  (* epoch, 1s later, NaT *)
in
Npy.save "timestamps.npy" arr;

(* Check for NaT *)
match Npy.load "timestamps.npy" with
| Ok t -> Printf.printf "is NaT: %b\n" (Npy.is_nat t 2)
| Error msg -> failwith msg
```

### Element access

```ocaml
let t = Npy.create Float64 [| 5 |] in
Npy.set_float t 0 3.14;
Printf.printf "%f\n" (Npy.get_float t 0);

(* Int64 access for large values *)
let t = Npy.create Int64 [| 1 |] in
Npy.set_int64 t 0 1_000_000_000_000L;

(* Complex access *)
let t = Npy.create Complex128 [| 1 |] in
Npy.set_complex t 0 Complex.{ re = 1.0; im = -2.0 };

(* String access *)
let t = Npy.create (Unicode 10) [| 1 |] in
Npy.set_string t 0 "hello";
Printf.printf "%s\n" (Npy.get_string t 0)
```

## License

MIT
