(* Npy — pure OCaml reader/writer for NumPy .npy files. *)

(* {1 Types} *)

type time_unit =
  | Y | M | W | D | Hour | Min | Sec
  | Ms | Us | Ns | Ps | Fs | As | Generic

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

type order = C | Fortran

type t = { dtype : dtype; order : order; shape : int array; data : bytes }

(* {1 Time unit conversion} *)

let time_unit_to_string = function
  | Y -> "Y" | M -> "M" | W -> "W" | D -> "D"
  | Hour -> "h" | Min -> "m" | Sec -> "s"
  | Ms -> "ms" | Us -> "us" | Ns -> "ns"
  | Ps -> "ps" | Fs -> "fs" | As -> "as"
  | Generic -> ""

let time_unit_of_string = function
  | "Y" -> Some Y | "M" -> Some M | "W" -> Some W | "D" -> Some D
  | "h" -> Some Hour | "m" -> Some Min | "s" -> Some Sec
  | "ms" -> Some Ms | "us" -> Some Us | "ns" -> Some Ns
  | "ps" -> Some Ps | "fs" -> Some Fs | "as" -> Some As
  | "" -> Some Generic
  | _ -> None

(* {1 Dtype operations} *)

let dtype_to_descr = function
  | Bool -> "|b1"
  | Int8 -> "|i1"
  | Uint8 -> "|u1"
  | Int16 -> "<i2"
  | Uint16 -> "<u2"
  | Int32 -> "<i4"
  | Uint32 -> "<u4"
  | Int64 -> "<i8"
  | Uint64 -> "<u8"
  | Float16 -> "<f2"
  | Float32 -> "<f4"
  | Float64 -> "<f8"
  | Complex64 -> "<c8"
  | Complex128 -> "<c16"
  | Bytes n -> Printf.sprintf "|S%d" n
  | Unicode n -> Printf.sprintf "<U%d" n
  | Datetime64 Generic -> "<M8"
  | Datetime64 u -> Printf.sprintf "<M8[%s]" (time_unit_to_string u)
  | Timedelta64 Generic -> "<m8"
  | Timedelta64 u -> Printf.sprintf "<m8[%s]" (time_unit_to_string u)

let try_parse_bytes rest =
  if String.length rest >= 2 && rest.[0] = 'S' then
    let num_str = String.sub rest 1 (String.length rest - 1) in
    match int_of_string_opt num_str with
    | Some n when n >= 0 -> Some (Bytes n, false)
    | _ -> None
  else None

let try_parse_unicode rest big_endian =
  if String.length rest >= 2 && rest.[0] = 'U' then
    let num_str = String.sub rest 1 (String.length rest - 1) in
    match int_of_string_opt num_str with
    | Some n when n >= 0 -> Some (Unicode n, big_endian)
    | _ -> None
  else None

let try_parse_temporal rest big_endian prefix_char =
  let make_dt u = if prefix_char = 'M' then Datetime64 u else Timedelta64 u in
  let expected = Printf.sprintf "%c8" prefix_char in
  let elen = String.length expected in
  let rlen = String.length rest in
  if rlen >= elen && String.sub rest 0 elen = expected then begin
    if rlen = elen then
      Some (make_dt Generic, big_endian)
    else if rest.[elen] = '[' then begin
      match String.index_from_opt rest elen ']' with
      | Some close_idx ->
        let unit_str = String.sub rest (elen + 1) (close_idx - elen - 1) in
        (match time_unit_of_string unit_str with
         | Some u -> Some (make_dt u, big_endian)
         | None -> None)
      | None -> None
    end else
      None
  end else
    None

let dtype_of_descr_internal s =
  let len = String.length s in
  if len < 2 then Error (Printf.sprintf "unsupported dtype: %s" s)
  else
    let prefix = s.[0] in
    let rest = String.sub s 1 (len - 1) in
    let big_endian = prefix = '>' in
    match rest with
    | "b1" -> Ok (Bool, false)
    | "i1" -> Ok (Int8, false)
    | "u1" -> Ok (Uint8, false)
    | "i2" -> Ok (Int16, big_endian)
    | "u2" -> Ok (Uint16, big_endian)
    | "i4" -> Ok (Int32, big_endian)
    | "u4" -> Ok (Uint32, big_endian)
    | "i8" -> Ok (Int64, big_endian)
    | "u8" -> Ok (Uint64, big_endian)
    | "f2" -> Ok (Float16, big_endian)
    | "f4" -> Ok (Float32, big_endian)
    | "f8" -> Ok (Float64, big_endian)
    | "c8" -> Ok (Complex64, big_endian)
    | "c16" -> Ok (Complex128, big_endian)
    | _ ->
      let parsers = [
        (fun () -> try_parse_bytes rest);
        (fun () -> try_parse_unicode rest big_endian);
        (fun () -> try_parse_temporal rest big_endian 'M');
        (fun () -> try_parse_temporal rest big_endian 'm');
      ] in
      match List.find_map (fun f -> f ()) parsers with
      | Some (dt, be) -> Ok (dt, be)
      | None -> Error (Printf.sprintf "unsupported dtype: %s" s)

let dtype_of_descr s =
  match dtype_of_descr_internal s with
  | Ok (dtype, _) -> Ok dtype
  | Error _ as e -> e

let element_size = function
  | Bool | Int8 | Uint8 -> 1
  | Int16 | Uint16 | Float16 -> 2
  | Int32 | Uint32 | Float32 -> 4
  | Int64 | Uint64 | Float64 | Complex64 -> 8
  | Complex128 -> 16
  | Bytes n -> n
  | Unicode n -> 4 * n
  | Datetime64 _ | Timedelta64 _ -> 8

let swap_unit_size = function
  | Complex64 -> 4
  | Complex128 -> 8
  | Bytes _ -> 1
  | Unicode _ -> 4
  | Datetime64 _ | Timedelta64 _ -> 8
  | dt -> element_size dt

let num_elements shape = Array.fold_left ( * ) 1 shape

(* {1 Internal helpers} *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

let find_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec search i =
    if i > hlen - nlen then None
    else if String.sub haystack i nlen = needle then Some (i + nlen)
    else search (i + 1)
  in
  search 0

let magic = "\x93NUMPY"

(* {1 IEEE 754 half-precision (float16) conversion} *)

let float16_bits_to_float bits =
  let sign = (bits lsr 15) land 1 in
  let exp = (bits lsr 10) land 0x1f in
  let mant = bits land 0x3ff in
  let f =
    if exp = 0 then
      if mant = 0 then 0.0
      else Float.ldexp (Float.of_int mant) (-24)
    else if exp = 0x1f then
      if mant = 0 then Float.infinity
      else Float.nan
    else
      Float.ldexp (Float.of_int (mant lor 0x400)) (exp - 25)
  in
  if sign = 1 then Float.neg f else f

let float_to_float16_bits v =
  if Float.is_nan v then 0x7e00
  else
    let bits = Int64.bits_of_float v in
    let sign = Int64.to_int (Int64.shift_right_logical bits 63) land 1 in
    let exp64 = Int64.to_int (Int64.shift_right_logical bits 52) land 0x7ff in
    let mant64 = Int64.logand bits 0xfffffffffffffL in
    if exp64 = 0 then
      sign lsl 15
    else if exp64 = 0x7ff then
      (sign lsl 15) lor 0x7c00
    else
      let exp = exp64 - 1023 in
      if exp > 15 then
        (sign lsl 15) lor 0x7c00
      else if exp >= -14 then
        let mant16 = Int64.to_int (Int64.shift_right_logical mant64 42) in
        (sign lsl 15) lor ((exp + 15) lsl 10) lor mant16
      else if exp >= -24 then
        let full_mant = Int64.logor mant64 (Int64.shift_left 1L 52) in
        let shift = 28 - exp in
        let mant16 = Int64.to_int (Int64.shift_right_logical full_mant shift) in
        (sign lsl 15) lor mant16
      else
        sign lsl 15

(* {1 Byte swapping} *)

let byte_swap_buffer data unit_size =
  let len = Bytes.length data in
  let n = len / unit_size in
  for i = 0 to n - 1 do
    let off = i * unit_size in
    for j = 0 to unit_size / 2 - 1 do
      let a = Bytes.get_uint8 data (off + j) in
      let b = Bytes.get_uint8 data (off + unit_size - 1 - j) in
      Bytes.set_uint8 data (off + j) b;
      Bytes.set_uint8 data (off + unit_size - 1 - j) a
    done
  done

(* {1 UTF-8 / UCS-4 helpers} *)

let utf8_decode_codepoint s i =
  let len = String.length s in
  let b0 = Char.code s.[i] in
  if b0 land 0x80 = 0 then
    (b0, i + 1)
  else if b0 land 0xe0 = 0xc0 then begin
    if i + 1 >= len then (0xFFFD, i + 1)
    else
      let b1 = Char.code s.[i + 1] in
      let cp = ((b0 land 0x1f) lsl 6) lor (b1 land 0x3f) in
      (cp, i + 2)
  end else if b0 land 0xf0 = 0xe0 then begin
    if i + 2 >= len then (0xFFFD, i + 1)
    else
      let b1 = Char.code s.[i + 1] in
      let b2 = Char.code s.[i + 2] in
      let cp = ((b0 land 0x0f) lsl 12)
               lor ((b1 land 0x3f) lsl 6)
               lor (b2 land 0x3f) in
      (cp, i + 3)
  end else if b0 land 0xf8 = 0xf0 then begin
    if i + 3 >= len then (0xFFFD, i + 1)
    else
      let b1 = Char.code s.[i + 1] in
      let b2 = Char.code s.[i + 2] in
      let b3 = Char.code s.[i + 3] in
      let cp = ((b0 land 0x07) lsl 18)
               lor ((b1 land 0x3f) lsl 12)
               lor ((b2 land 0x3f) lsl 6)
               lor (b3 land 0x3f) in
      (cp, i + 4)
  end else
    (0xFFFD, i + 1)

let utf8_encode_codepoint buf cp =
  if cp < 0x80 then
    Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xc0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3f)))
  end else if cp < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xe0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3f)))
  end else begin
    Buffer.add_char buf (Char.chr (0xf0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3f)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3f)))
  end

let read_le_int32_unsigned data off =
  let b0 = Bytes.get_uint8 data off in
  let b1 = Bytes.get_uint8 data (off + 1) in
  let b2 = Bytes.get_uint8 data (off + 2) in
  let b3 = Bytes.get_uint8 data (off + 3) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let write_le_int32_unsigned data off v =
  Bytes.set_uint8 data off (v land 0xff);
  Bytes.set_uint8 data (off + 1) ((v lsr 8) land 0xff);
  Bytes.set_uint8 data (off + 2) ((v lsr 16) land 0xff);
  Bytes.set_uint8 data (off + 3) ((v lsr 24) land 0xff)

let read_unicode_string data off n =
  let rec find_last i =
    if i < 0 then -1
    else if read_le_int32_unsigned data (off + i * 4) <> 0 then i
    else find_last (i - 1)
  in
  let last = find_last (n - 1) in
  let buf = Buffer.create ((last + 1) * 4) in
  for i = 0 to last do
    utf8_encode_codepoint buf (read_le_int32_unsigned data (off + i * 4))
  done;
  Buffer.contents buf

let write_unicode_string data off n s =
  Bytes.fill data off (n * 4) '\x00';
  let slen = String.length s in
  let rec loop si ci =
    if si >= slen || ci >= n then ()
    else
      let cp, next = utf8_decode_codepoint s si in
      write_le_int32_unsigned data (off + ci * 4) cp;
      loop next (ci + 1)
  in
  loop 0 0

let read_byte_string data off n =
  let rec find_last i =
    if i < 0 then 0
    else if Bytes.get_uint8 data (off + i) <> 0 then i + 1
    else find_last (i - 1)
  in
  Bytes.sub_string data off (find_last (n - 1))

let write_byte_string data off n s =
  Bytes.fill data off n '\x00';
  let copy_len = min n (String.length s) in
  Bytes.blit_string s 0 data off copy_len

(* {1 Header parsing} *)

let parse_string_field header prefix =
  let* start =
    find_substring header prefix
    |> Option.to_result ~none:(Printf.sprintf "missing %s in header" prefix)
  in
  String.index_from_opt header start '\''
  |> Option.to_result ~none:(Printf.sprintf "malformed field after %s" prefix)
  |> Result.map (fun end_idx -> String.sub header start (end_idx - start))

let parse_descr header = parse_string_field header "'descr': '"

let parse_fortran_order header =
  match find_substring header "'fortran_order': " with
  | None -> Ok C
  | Some pos ->
    let remaining = String.length header - pos in
    if remaining >= 4 && String.sub header pos 4 = "True" then Ok Fortran
    else if remaining >= 5 && String.sub header pos 5 = "False" then Ok C
    else Error "invalid fortran_order value"

let parse_shape header =
  let* start =
    find_substring header "'shape': ("
    |> Option.to_result ~none:"missing 'shape' field in header"
  in
  let* end_idx =
    String.index_from_opt header start ')'
    |> Option.to_result ~none:"malformed 'shape' field"
  in
  let shape_str = String.sub header start (end_idx - start) in
  let parts =
    String.split_on_char ',' shape_str
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let parsed = List.filter_map int_of_string_opt parts in
  if List.length parsed = List.length parts then Ok (Array.of_list parsed)
  else Error "invalid dimension in shape"

(* {1 Header encoding} *)

let encode_header dtype order shape =
  let shape_str =
    let parts = Array.to_list (Array.map string_of_int shape) in
    let s = String.concat ", " parts in
    if Array.length shape = 1 then s ^ "," else s
  in
  let order_str = match order with C -> "False" | Fortran -> "True" in
  Printf.sprintf "{'descr': '%s', 'fortran_order': %s, 'shape': (%s), }"
    (dtype_to_descr dtype) order_str shape_str

let pad_header ~preamble_len header =
  let total = preamble_len + String.length header + 1 in
  let padded = ((total + 63) / 64) * 64 in
  header ^ String.make (padded - total) ' ' ^ "\n"

(* {1 String dtype check} *)

let check_not_string_dtype fname = function
  | Bytes _ | Unicode _ ->
    invalid_arg (Printf.sprintf "Npy.%s: not supported for string dtypes" fname)
  | _ -> ()

let check_string_dtype fname = function
  | Bytes _ | Unicode _ -> ()
  | _ ->
    invalid_arg (Printf.sprintf "Npy.%s: requires Bytes or Unicode dtype" fname)

let check_datetime_dtype fname = function
  | Datetime64 _ | Timedelta64 _ -> ()
  | _ ->
    invalid_arg (Printf.sprintf "Npy.%s: requires Datetime64 or Timedelta64 dtype" fname)

(* {1 Element access} *)

let get_float t idx =
  check_not_string_dtype "get_float" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> if Bytes.get_uint8 t.data off <> 0 then 1.0 else 0.0
  | Int8 -> Bytes.get_int8 t.data off |> Float.of_int
  | Uint8 -> Bytes.get_uint8 t.data off |> Float.of_int
  | Int16 -> Bytes.get_int16_le t.data off |> Float.of_int
  | Uint16 -> Bytes.get_uint16_le t.data off |> Float.of_int
  | Int32 -> Bytes.get_int32_le t.data off |> Int32.to_float
  | Uint32 ->
    let v = Bytes.get_int32_le t.data off in
    Int64.to_float (Int64.logand (Int64.of_int32 v) 0xffffffffL)
  | Int64 -> Bytes.get_int64_le t.data off |> Int64.to_float
  | Uint64 ->
    let v = Bytes.get_int64_le t.data off in
    if Int64.compare v 0L >= 0 then Int64.to_float v
    else Int64.to_float v +. 18446744073709551616.0
  | Float16 -> float16_bits_to_float (Bytes.get_uint16_le t.data off)
  | Float32 -> Bytes.get_int32_le t.data off |> Int32.float_of_bits
  | Float64 -> Bytes.get_int64_le t.data off |> Int64.float_of_bits
  | Complex64 -> Bytes.get_int32_le t.data off |> Int32.float_of_bits
  | Complex128 -> Bytes.get_int64_le t.data off |> Int64.float_of_bits
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.get_int64_le t.data off |> Int64.to_float
  | Bytes _ | Unicode _ -> assert false

let get_int t idx =
  check_not_string_dtype "get_int" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> if Bytes.get_uint8 t.data off <> 0 then 1 else 0
  | Int8 -> Bytes.get_int8 t.data off
  | Uint8 -> Bytes.get_uint8 t.data off
  | Int16 -> Bytes.get_int16_le t.data off
  | Uint16 -> Bytes.get_uint16_le t.data off
  | Int32 -> Bytes.get_int32_le t.data off |> Int32.to_int
  | Uint32 ->
    let v = Bytes.get_int32_le t.data off in
    Int64.to_int (Int64.logand (Int64.of_int32 v) 0xffffffffL)
  | Int64 -> Bytes.get_int64_le t.data off |> Int64.to_int
  | Uint64 -> Bytes.get_int64_le t.data off |> Int64.to_int
  | Float16 ->
    float16_bits_to_float (Bytes.get_uint16_le t.data off) |> Float.to_int
  | Float32 ->
    Bytes.get_int32_le t.data off |> Int32.float_of_bits |> Float.to_int
  | Float64 ->
    Bytes.get_int64_le t.data off |> Int64.float_of_bits |> Float.to_int
  | Complex64 ->
    Bytes.get_int32_le t.data off |> Int32.float_of_bits |> Float.to_int
  | Complex128 ->
    Bytes.get_int64_le t.data off |> Int64.float_of_bits |> Float.to_int
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.get_int64_le t.data off |> Int64.to_int
  | Bytes _ | Unicode _ -> assert false

let get_int64 t idx =
  check_not_string_dtype "get_int64" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> if Bytes.get_uint8 t.data off <> 0 then 1L else 0L
  | Int8 -> Bytes.get_int8 t.data off |> Int64.of_int
  | Uint8 -> Bytes.get_uint8 t.data off |> Int64.of_int
  | Int16 -> Bytes.get_int16_le t.data off |> Int64.of_int
  | Uint16 -> Bytes.get_uint16_le t.data off |> Int64.of_int
  | Int32 -> Bytes.get_int32_le t.data off |> Int64.of_int32
  | Uint32 ->
    let v = Bytes.get_int32_le t.data off in
    Int64.logand (Int64.of_int32 v) 0xffffffffL
  | Int64 -> Bytes.get_int64_le t.data off
  | Uint64 -> Bytes.get_int64_le t.data off
  | Float16 ->
    float16_bits_to_float (Bytes.get_uint16_le t.data off) |> Int64.of_float
  | Float32 ->
    Bytes.get_int32_le t.data off |> Int32.float_of_bits |> Int64.of_float
  | Float64 ->
    Bytes.get_int64_le t.data off |> Int64.float_of_bits |> Int64.of_float
  | Complex64 ->
    Bytes.get_int32_le t.data off |> Int32.float_of_bits |> Int64.of_float
  | Complex128 ->
    Bytes.get_int64_le t.data off |> Int64.float_of_bits |> Int64.of_float
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.get_int64_le t.data off
  | Bytes _ | Unicode _ -> assert false

let get_complex t idx =
  check_not_string_dtype "get_complex" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Complex64 ->
    let re = Int32.float_of_bits (Bytes.get_int32_le t.data off) in
    let im = Int32.float_of_bits (Bytes.get_int32_le t.data (off + 4)) in
    Complex.{ re; im }
  | Complex128 ->
    let re = Int64.float_of_bits (Bytes.get_int64_le t.data off) in
    let im = Int64.float_of_bits (Bytes.get_int64_le t.data (off + 8)) in
    Complex.{ re; im }
  | _ ->
    let v = get_float t idx in
    Complex.{ re = v; im = 0.0 }

let get_bool t idx =
  check_not_string_dtype "get_bool" t.dtype;
  match t.dtype with
  | Bool ->
    let off = idx * element_size t.dtype in
    Bytes.get_uint8 t.data off <> 0
  | _ -> get_int t idx <> 0

let get_string t idx =
  check_string_dtype "get_string" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bytes n -> read_byte_string t.data off n
  | Unicode n -> read_unicode_string t.data off n
  | _ -> assert false

let set_float t idx v =
  check_not_string_dtype "set_float" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> Bytes.set_uint8 t.data off (if v <> 0.0 then 1 else 0)
  | Int8 -> Bytes.set_int8 t.data off (Float.to_int v)
  | Uint8 -> Bytes.set_uint8 t.data off (Float.to_int v)
  | Int16 -> Bytes.set_int16_le t.data off (Float.to_int v)
  | Uint16 -> Bytes.set_uint16_le t.data off (Float.to_int v)
  | Int32 -> Bytes.set_int32_le t.data off (Int32.of_float v)
  | Uint32 ->
    Bytes.set_int32_le t.data off (Int64.to_int32 (Int64.of_float v))
  | Int64 -> Bytes.set_int64_le t.data off (Int64.of_float v)
  | Uint64 -> Bytes.set_int64_le t.data off (Int64.of_float v)
  | Float16 -> Bytes.set_uint16_le t.data off (float_to_float16_bits v)
  | Float32 -> Bytes.set_int32_le t.data off (Int32.bits_of_float v)
  | Float64 -> Bytes.set_int64_le t.data off (Int64.bits_of_float v)
  | Complex64 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float v);
    Bytes.set_int32_le t.data (off + 4) 0l
  | Complex128 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float v);
    Bytes.set_int64_le t.data (off + 8) 0L
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.set_int64_le t.data off (Int64.of_float v)
  | Bytes _ | Unicode _ -> assert false

let set_int t idx v =
  check_not_string_dtype "set_int" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> Bytes.set_uint8 t.data off (if v <> 0 then 1 else 0)
  | Int8 -> Bytes.set_int8 t.data off v
  | Uint8 -> Bytes.set_uint8 t.data off v
  | Int16 -> Bytes.set_int16_le t.data off v
  | Uint16 -> Bytes.set_uint16_le t.data off v
  | Int32 -> Bytes.set_int32_le t.data off (Int32.of_int v)
  | Uint32 -> Bytes.set_int32_le t.data off (Int32.of_int v)
  | Int64 -> Bytes.set_int64_le t.data off (Int64.of_int v)
  | Uint64 -> Bytes.set_int64_le t.data off (Int64.of_int v)
  | Float16 ->
    Bytes.set_uint16_le t.data off (float_to_float16_bits (Float.of_int v))
  | Float32 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float (Float.of_int v))
  | Float64 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float (Float.of_int v))
  | Complex64 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float (Float.of_int v));
    Bytes.set_int32_le t.data (off + 4) 0l
  | Complex128 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float (Float.of_int v));
    Bytes.set_int64_le t.data (off + 8) 0L
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.set_int64_le t.data off (Int64.of_int v)
  | Bytes _ | Unicode _ -> assert false

let set_int64 t idx v =
  check_not_string_dtype "set_int64" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> Bytes.set_uint8 t.data off (if Int64.compare v 0L <> 0 then 1 else 0)
  | Int8 -> Bytes.set_int8 t.data off (Int64.to_int v)
  | Uint8 -> Bytes.set_uint8 t.data off (Int64.to_int v)
  | Int16 -> Bytes.set_int16_le t.data off (Int64.to_int v)
  | Uint16 -> Bytes.set_uint16_le t.data off (Int64.to_int v)
  | Int32 -> Bytes.set_int32_le t.data off (Int64.to_int32 v)
  | Uint32 -> Bytes.set_int32_le t.data off (Int64.to_int32 v)
  | Int64 -> Bytes.set_int64_le t.data off v
  | Uint64 -> Bytes.set_int64_le t.data off v
  | Float16 ->
    Bytes.set_uint16_le t.data off (float_to_float16_bits (Int64.to_float v))
  | Float32 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float (Int64.to_float v))
  | Float64 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float (Int64.to_float v))
  | Complex64 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float (Int64.to_float v));
    Bytes.set_int32_le t.data (off + 4) 0l
  | Complex128 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float (Int64.to_float v));
    Bytes.set_int64_le t.data (off + 8) 0L
  | Datetime64 _ | Timedelta64 _ ->
    Bytes.set_int64_le t.data off v
  | Bytes _ | Unicode _ -> assert false

let set_complex t idx v =
  check_not_string_dtype "set_complex" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Complex64 ->
    Bytes.set_int32_le t.data off (Int32.bits_of_float v.Complex.re);
    Bytes.set_int32_le t.data (off + 4) (Int32.bits_of_float v.Complex.im)
  | Complex128 ->
    Bytes.set_int64_le t.data off (Int64.bits_of_float v.Complex.re);
    Bytes.set_int64_le t.data (off + 8) (Int64.bits_of_float v.Complex.im)
  | _ -> set_float t idx v.Complex.re

let set_bool t idx v =
  check_not_string_dtype "set_bool" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bool -> Bytes.set_uint8 t.data off (if v then 1 else 0)
  | _ -> set_int t idx (if v then 1 else 0)

let set_string t idx s =
  check_string_dtype "set_string" t.dtype;
  let off = idx * element_size t.dtype in
  match t.dtype with
  | Bytes n -> write_byte_string t.data off n s
  | Unicode n -> write_unicode_string t.data off n s
  | _ -> assert false

(* {1 Datetime/Timedelta} *)

let nat = Int64.min_int

let is_nat t idx =
  check_datetime_dtype "is_nat" t.dtype;
  let off = idx * element_size t.dtype in
  Int64.compare (Bytes.get_int64_le t.data off) nat = 0

(* {1 Construction} *)

let create ?(order = C) dtype shape =
  if Array.exists (fun d -> d < 0) shape then
    invalid_arg "Npy.create: negative dimension";
  let n = num_elements shape in
  { dtype; order; shape; data = Bytes.make (n * element_size dtype) '\x00' }

let of_float_array ?(order = C) dtype shape values =
  let t = create ~order dtype shape in
  Array.iteri (set_float t) values;
  t

let of_int_array ?(order = C) dtype shape values =
  let t = create ~order dtype shape in
  Array.iteri (set_int t) values;
  t

let of_bool_array ?(order = C) shape values =
  let t = create ~order Bool shape in
  Array.iteri (set_bool t) values;
  t

let of_int64_array ?(order = C) dtype shape values =
  let t = create ~order dtype shape in
  Array.iteri (set_int64 t) values;
  t

let of_complex_array ?(order = C) dtype shape values =
  let t = create ~order dtype shape in
  Array.iteri (set_complex t) values;
  t

let of_string_array ?(order = C) dtype shape values =
  check_string_dtype "of_string_array" dtype;
  let t = create ~order dtype shape in
  Array.iteri (set_string t) values;
  t

(* {1 Serialization} *)

let encode_preamble dtype order shape =
  let raw_header = encode_header dtype order shape in
  let header_v1 = pad_header ~preamble_len:10 raw_header in
  if String.length header_v1 <= 65535 then begin
    let hlen = String.length header_v1 in
    let buf = Buffer.create (10 + hlen) in
    Buffer.add_string buf magic;
    Buffer.add_char buf '\x01';
    Buffer.add_char buf '\x00';
    Buffer.add_char buf (Char.chr (hlen land 0xff));
    Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
    Buffer.add_string buf header_v1;
    Buffer.contents buf
  end else begin
    let header_v2 = pad_header ~preamble_len:12 raw_header in
    let hlen = String.length header_v2 in
    let buf = Buffer.create (12 + hlen) in
    Buffer.add_string buf magic;
    Buffer.add_char buf '\x02';
    Buffer.add_char buf '\x00';
    Buffer.add_char buf (Char.chr (hlen land 0xff));
    Buffer.add_char buf (Char.chr ((hlen lsr 8) land 0xff));
    Buffer.add_char buf (Char.chr ((hlen lsr 16) land 0xff));
    Buffer.add_char buf (Char.chr ((hlen lsr 24) land 0xff));
    Buffer.add_string buf header_v2;
    Buffer.contents buf
  end

let encode t =
  let preamble = encode_preamble t.dtype t.order t.shape in
  let buf = Buffer.create (String.length preamble + Bytes.length t.data) in
  Buffer.add_string buf preamble;
  Buffer.add_bytes buf t.data;
  Buffer.contents buf

let decode s =
  let len = String.length s in
  if len < 10 then Error "file too short"
  else if String.sub s 0 6 <> magic then Error "invalid magic number"
  else
    let major = Char.code s.[6] in
    let* (hlen, preamble_len) =
      match major with
      | 1 ->
        if len < 10 then Error "file too short for v1.0 header"
        else Ok (Char.code s.[8] lor (Char.code s.[9] lsl 8), 10)
      | 2 | 3 ->
        if len < 12 then Error "file too short for v2.0/3.0 header"
        else
          let hlen =
            Char.code s.[8]
            lor (Char.code s.[9] lsl 8)
            lor (Char.code s.[10] lsl 16)
            lor (Char.code s.[11] lsl 24)
          in
          Ok (hlen, 12)
      | v -> Error (Printf.sprintf "unsupported format version %d" v)
    in
    if len < preamble_len + hlen then Error "file truncated in header"
    else
      let header = String.sub s preamble_len hlen in
      let* descr_str = parse_descr header in
      let* (dtype, needs_swap) = dtype_of_descr_internal descr_str in
      let* order = parse_fortran_order header in
      let* shape = parse_shape header in
      let data_offset = preamble_len + hlen in
      let data_len = len - data_offset in
      let expected = num_elements shape * element_size dtype in
      if data_len < expected then
        Error
          (Printf.sprintf "data too short: expected %d bytes, got %d" expected
             data_len)
      else
        let data = Bytes.of_string (String.sub s data_offset expected) in
        let su = swap_unit_size dtype in
        if needs_swap && su > 1 then byte_swap_buffer data su;
        Ok { dtype; order; shape; data }

(* Read just the preamble + header from a channel, leaving it positioned at the
   start of the data.  Returns the dtype (with its byte-swap flag), order, shape
   and the absolute data offset. *)
let read_header_ic ic =
  let preamble = really_input_string ic 8 in
  if String.sub preamble 0 6 <> magic then Error "invalid magic number"
  else
    let major = Char.code preamble.[6] in
    let* (hlen, preamble_len) =
      match major with
      | 1 ->
        let b = really_input_string ic 2 in
        Ok (Char.code b.[0] lor (Char.code b.[1] lsl 8), 10)
      | 2 | 3 ->
        let b = really_input_string ic 4 in
        Ok ( Char.code b.[0]
             lor (Char.code b.[1] lsl 8)
             lor (Char.code b.[2] lsl 16)
             lor (Char.code b.[3] lsl 24),
             12 )
      | v -> Error (Printf.sprintf "unsupported format version %d" v)
    in
    let header = really_input_string ic hlen in
    let* descr_str = parse_descr header in
    let* (dtype, needs_swap) = dtype_of_descr_internal descr_str in
    let* order = parse_fortran_order header in
    let* shape = parse_shape header in
    Ok (dtype, needs_swap, order, shape, preamble_len + hlen)

(* {1 File I/O} *)

let read_shape filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let* (dtype, _, order, shape, _) = read_header_ic ic in
      Ok (dtype, order, shape))

let load_strided filename ~stride =
  if stride < 1 then Error "load_strided: stride must be >= 1"
  else
    let ic = open_in_bin filename in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let* (dtype, needs_swap, order, shape, data_offset) =
          read_header_ic ic
        in
        match order with
        | Fortran -> Error "load_strided: Fortran order not supported"
        | C ->
          if Array.length shape = 0 || shape.(0) = 0 then Ok { dtype; order = C; shape; data = Bytes.create 0 }
          else begin
            let d0 = shape.(0) in
            let inner = num_elements shape / d0 in
            let esz = element_size dtype in
            let row_bytes = inner * esz in
            let n_rows = (d0 + stride - 1) / stride in
            let out = Bytes.create (n_rows * row_bytes) in
            for i = 0 to n_rows - 1 do
              seek_in ic (data_offset + (i * stride) * row_bytes);
              really_input ic out (i * row_bytes) row_bytes
            done;
            let su = swap_unit_size dtype in
            if needs_swap && su > 1 then byte_swap_buffer out su;
            let new_shape = Array.copy shape in
            new_shape.(0) <- n_rows;
            Ok { dtype; order = C; shape = new_shape; data = out }
          end)

let save filename t =
  let oc = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (encode t))

let load filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      decode s)

(* {1 Array conversion} *)

let to_float_array t = Array.init (num_elements t.shape) (get_float t)
let to_int_array t = Array.init (num_elements t.shape) (get_int t)
let to_bool_array t = Array.init (num_elements t.shape) (get_bool t)
let to_int64_array t = Array.init (num_elements t.shape) (get_int64 t)
let to_complex_array t = Array.init (num_elements t.shape) (get_complex t)

let to_string_array t =
  check_string_dtype "to_string_array" t.dtype;
  Array.init (num_elements t.shape) (get_string t)

(* {1 Streaming} *)

let write_header oc ?(order = C) dtype shape =
  output_string oc (encode_preamble dtype order shape)
