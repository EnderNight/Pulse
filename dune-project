(lang dune 3.16)

(name Pulse)

(generate_opam_files true)

(source
 (github EnderNight/Pulse))

(authors "Ender")

(maintainers "Ender")

(license MIT)

(documentation https://github.com/EnderNight/Pulse)

(package
 (name Pulse)
 (synopsis "A simple programming language")
 (description "A small and simple programming language")
 (depends ocaml dune)
 (tags
  (topics programming language)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
