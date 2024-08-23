(lang dune 2.8)

(package
 (name dune-expand)
 (synopsis "Tool to view ppx-expanded OCaml source files")
 (description
  "dune-expand is a tool that allows users to view the expanded output of PPXs on their source files.")
 (depends
  (sexplib
   (>= ~))
  (ppx_sexp_conv
   (>= ~))
  (ppx_deriving
   (>= ~))
  (core
   (>= ~))
  (cmdliner
   (>= 1.0.0))
  (ocaml
   (>= 4.11.0))))

(generate_opam_files true)

(license GPL-3.0+)

(source
 (uri git+https://github.com/jevinskie/dune-expand.git))

(bug_reports https://github.com/jevinskie/dune-expand/issues)

(homepage https://github.com/jevinskie/dune-expand)

(name dune-expand)

(authors "Kiran Gopinathan" "Jevin Sweval")

(maintainers "kirang@comp.nus.edu.sg" "jevin.sweval@gmail.com")
