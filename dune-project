(lang dune 3.19)

(name pulse)

(version 0.1)

(generate_opam_files true)

(source
 (github EnderNight/Pulse))

(authors "Ender <ndxendernight@gmail.com>")

(maintainers "Ender <ndxendernight@gmail.com")

(license MIT)

(documentation https://github.com/EnderNight/Pulse/wiki)

(package
 (name pulse)
 (synopsis "Simple programming language")
 (description "Simple programming language")
 (depends
  (ocaml
   (>= 5.3))
  (cmdliner
   (>= 1.3)))
 (tags
  (programming language)))
