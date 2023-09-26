(lang dune 3.10)

(name rocambole)
(generate_opam_files true)

(source
 (github appositum/rocambole))

(authors "appositum")

(maintainers "appositum")

(package
 (name rocambole)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune angstrom)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
