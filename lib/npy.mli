(** NumPy .npy file format reader and writer.

    Read and write NumPy .npy files (format versions 1.0, 2.0, and 3.0) with
    all standard numeric element types, byte strings, Unicode strings, and
    temporal types. The format stores a single dense array with its dtype,
    memory order, and shape in a self-describing binary header.

    Data is stored internally in little-endian layout. Big-endian files are
    automatically byte-swapped on read. Writing always produces
    little-endian output. *)

(** {1 Types} *)

(** Time unit for datetime64 and timedelta64 dtypes. *)
type time_unit =
  | Y
  | M
  | W
  | D
  | Hour
  | Min
  | Sec
  | Ms
  | Us
  | Ns
  | Ps
  | Fs
  | As
  | Generic

(** Supported element types. *)
type dtype =
  | Bool
  | Int8
  | Uint8
  | Int16
  | Uint16
  | Int32
  | Uint32
  | Int64
  | Uint64
  | Float16
  | Float32
  | Float64
  | Complex64
  | Complex128
  | Bytes of int
  | Unicode of int
  | Datetime64 of time_unit
  | Timedelta64 of time_unit

(** Memory layout order. *)
type order = C | Fortran

(** An array stored in NumPy .npy format. The [data] field holds raw bytes
    in little-endian layout matching the [dtype]. The [order] field indicates
    whether elements are stored in C (row-major) or Fortran (column-major)
    order. *)
type t = {
  dtype : dtype;
  order : order;
  shape : int array;
  data : bytes;
}

(** {1 Dtype operations} *)

val dtype_to_descr : dtype -> string
(** [dtype_to_descr dt] is the NumPy descriptor string for [dt].
    For example, [dtype_to_descr Float32] is ["<f4"]. Always returns
    little-endian descriptors. Byte strings use ["|S{n}"], Unicode strings
    use ["<U{n}"], datetime64 uses ["<M8[unit]"], timedelta64 uses
    ["<m8[unit]"]. *)

val dtype_of_descr : string -> (dtype, string) result
(** [dtype_of_descr s] parses a NumPy descriptor string such as ["<f4"],
    ["|i1"], [">f8"], ["|S10"], ["<U5"], ["<M8[ns]"], or ["<m8[s]"].
    Accepts both little-endian and big-endian prefixes.
    Returns [Error] for unsupported descriptors. *)

val element_size : dtype -> int
(** [element_size dt] is the byte width of a single element of type [dt].
    For [Bytes n] this is [n], for [Unicode n] this is [4*n], and for
    [Datetime64 _] and [Timedelta64 _] this is [8]. *)

val num_elements : int array -> int
(** [num_elements shape] is the product of all dimensions in [shape].
    Returns [1] for an empty (scalar) shape. *)

val time_unit_to_string : time_unit -> string
(** [time_unit_to_string u] converts a time unit to its NumPy string
    representation. For example, [time_unit_to_string Ns] is ["ns"] and
    [time_unit_to_string Generic] is [""]. *)

(** {1 Construction} *)

val create : ?order:order -> dtype -> int array -> t
(** [create ?order dtype shape] allocates a zero-filled array with the given
    dtype, order (default [C]), and shape. *)

val of_float_array : ?order:order -> dtype -> int array -> float array -> t
(** [of_float_array ?order dtype shape values] creates an array from float
    values, converting each to the target dtype. *)

val of_int_array : ?order:order -> dtype -> int array -> int array -> t
(** [of_int_array ?order dtype shape values] creates an array from integer
    values, converting each to the target dtype. *)

val of_bool_array : ?order:order -> int array -> bool array -> t
(** [of_bool_array ?order shape values] creates a [Bool] array from boolean
    values. The dtype is always [Bool]. *)

val of_int64_array : ?order:order -> dtype -> int array -> int64 array -> t
(** [of_int64_array ?order dtype shape values] creates an array from [int64]
    values, converting each to the target dtype. *)

val of_complex_array : ?order:order -> dtype -> int array -> Complex.t array -> t
(** [of_complex_array ?order dtype shape values] creates an array from
    [Complex.t] values. For non-complex dtypes, only the real part is stored. *)

val of_string_array : ?order:order -> dtype -> int array -> string array -> t
(** [of_string_array ?order dtype shape values] creates a string array.
    The dtype must be [Bytes n] or [Unicode n]. Each string is
    truncated/zero-padded to fit the field width. *)

(** {1 Element access} *)

val get_float : t -> int -> float
(** [get_float t i] reads the [i]-th element as a float. For complex dtypes,
    returns only the real part. The flat index follows the array's order.
    Raises [Invalid_argument] for string dtypes ([Bytes _] and [Unicode _]). *)

val get_int : t -> int -> int
(** [get_int t i] reads the [i]-th element as an integer. For [Uint32],
    returns the correct unsigned value on 64-bit OCaml.
    Raises [Invalid_argument] for string dtypes. *)

val get_int64 : t -> int -> int64
(** [get_int64 t i] reads the [i]-th element as an [int64]. Use this for
    [Int64] and [Uint64] dtypes to avoid truncation. For [Uint64], values
    >= 2^63 appear as negative [int64] (raw bits preserved).
    Raises [Invalid_argument] for string dtypes. *)

val get_complex : t -> int -> Complex.t
(** [get_complex t i] reads the [i]-th element as a [Complex.t]. For
    non-complex dtypes, the imaginary part is [0.0].
    Raises [Invalid_argument] for string dtypes. *)

val get_bool : t -> int -> bool
(** [get_bool t i] reads the [i]-th element as a boolean.
    Raises [Invalid_argument] for string dtypes. *)

val get_string : t -> int -> string
(** [get_string t i] reads the [i]-th element as a string. For [Bytes n],
    returns raw bytes with trailing NUL bytes stripped. For [Unicode n],
    decodes UCS-4 codepoints to UTF-8 with trailing NUL codepoints stripped.
    Raises [Invalid_argument] for non-string dtypes. *)

val set_float : t -> int -> float -> unit
(** [set_float t i v] stores [v] into the [i]-th element. For complex dtypes,
    sets the real part and zeroes the imaginary part.
    Raises [Invalid_argument] for string dtypes. *)

val set_int : t -> int -> int -> unit
(** [set_int t i v] stores [v] into the [i]-th element.
    Raises [Invalid_argument] for string dtypes. *)

val set_int64 : t -> int -> int64 -> unit
(** [set_int64 t i v] stores [v] into the [i]-th element using [int64] to
    avoid truncation for 64-bit types.
    Raises [Invalid_argument] for string dtypes. *)

val set_complex : t -> int -> Complex.t -> unit
(** [set_complex t i v] stores [v] into the [i]-th element. For non-complex
    dtypes, stores only the real part.
    Raises [Invalid_argument] for string dtypes. *)

val set_bool : t -> int -> bool -> unit
(** [set_bool t i v] stores [v] into the [i]-th element.
    Raises [Invalid_argument] for string dtypes. *)

val set_string : t -> int -> string -> unit
(** [set_string t i s] stores [s] into the [i]-th element. For [Bytes n],
    writes raw bytes truncated/zero-padded to [n]. For [Unicode n], encodes
    UTF-8 to UCS-4 codepoints truncated/zero-padded to [n] codepoints.
    Raises [Invalid_argument] for non-string dtypes. *)

(** {1 Array conversion} *)

val to_float_array : t -> float array
(** [to_float_array t] reads every element as a float. *)

val to_int_array : t -> int array
(** [to_int_array t] reads every element as an integer. *)

val to_bool_array : t -> bool array
(** [to_bool_array t] reads every element as a boolean. *)

val to_int64_array : t -> int64 array
(** [to_int64_array t] reads every element as an [int64]. *)

val to_complex_array : t -> Complex.t array
(** [to_complex_array t] reads every element as a [Complex.t]. *)

val to_string_array : t -> string array
(** [to_string_array t] reads every element as a string. The dtype must be
    [Bytes _] or [Unicode _]. *)

(** {1 Datetime/Timedelta} *)

val nat : int64
(** [nat] is the NaT (Not a Time) sentinel value, equal to [Int64.min_int]. *)

val is_nat : t -> int -> bool
(** [is_nat t i] returns [true] if the [i]-th element is NaT.
    Raises [Invalid_argument] for non-datetime/timedelta dtypes. *)

(** {1 Serialization} *)

val encode : t -> string
(** [encode t] serializes [t] to a complete .npy format string. Uses format
    version 1.0 when the header fits in 65535 bytes, otherwise version 2.0. *)

val decode : string -> (t, string) result
(** [decode s] parses a .npy format string. Supports format versions 1.0,
    2.0, and 3.0. Big-endian data is automatically byte-swapped. *)

(** {1 File I/O} *)

val save : string -> t -> unit
(** [save filename t] writes [t] to a .npy file. *)

val load : string -> (t, string) result
(** [load filename] reads and parses a .npy file. *)

(** {1 Streaming} *)

val write_header : out_channel -> ?order:order -> dtype -> int array -> unit
(** [write_header oc ?order dtype shape] writes the .npy preamble and header
    to [oc]. The caller is then responsible for writing exactly
    [num_elements shape * element_size dtype] bytes of raw data. *)
