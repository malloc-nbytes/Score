(lang dune 3.14)

(name Score)

(generate_opam_files true)

(source
 (github malloc-nbytes/Score))

(authors "Zachary Haskins")

(maintainers "Zachary Haskins")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name Score)
 (synopsis "A small C-like language written in OCaml")
 (description "This is currrently in its infant stages of development. Eventually this language will be written in itself (self hosted).")
 (depends ocaml dune llvm)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
