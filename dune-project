(lang dune 3.12)

(package
 (name dune-expand)
 (synopsis "Tool to view ppx-expanded OCaml source files")
 (description
  "dune-expand is a tool that allows users to view the expanded output of PPXs on their source files.")
 (depends
  (sexplib
   (>= 0.15.0))
  (ppx_sexp_conv
   (>= 0.15.0))
  (ppx_deriving
   (>= 5.0))
  (core
   (>= 0.15.0))
  (core_unix
   (>= 0.15.0))
  (cmdliner
   (>= 1.1.0))
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
