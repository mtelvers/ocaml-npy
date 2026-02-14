(lang dune 3.0)

(name npy)

(source (github mtelvers/ocaml-npy))

(generate_opam_files true)

(license MIT)

(authors "Mark Elvers <mark.elvers@tunbury.org>")

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(package
 (name npy)
 (synopsis "Read and write NumPy .npy files")
 (description "A pure OCaml library for reading and writing NumPy .npy files (format versions 1.0, 2.0, and 3.0). Supports all 14 standard numeric dtypes, byte strings (S), Unicode strings (U), datetime64 (M8), and timedelta64 (m8). Handles both C and Fortran memory order, and both little-endian and big-endian data.")
 (depends
  (ocaml (>= 4.14))
  (alcotest (and (>= 1.0) :with-test))))
