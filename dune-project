(lang dune 3.24)

(name aoc)

(generate_opam_files true)

(source
 (github alinespereira/ocaml-aoc))

(authors "Aline")

(maintainers "Aline")

(package
 (name aoc)
 (synopsis "Advent of Code solutions in OCaml")
 (description
  "Solutions to Advent of Code puzzles, one library per day with inline expect tests.")
 (depends
  ocaml
  base
  core
  angstrom
  ppx_deriving
  ppx_enumerate
  ppx_expect)
 (tags
  ("advent of code" aoc puzzles)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
