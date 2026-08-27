(lang dune 3.20)

(name siesta)

(generate_opam_files true)

(source
 (github enetsee/siesta))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

(documentation https://enetsee.github.io/siesta/)

(package
 (name siesta)
 (synopsis "A language-independent concrete-syntax-tree library")
 (description
  "siesta provides an immutable, lossless, hash-consed concrete-syntax-tree substrate (the red-green tree design) for building parsers, editors, and refactoring tools. Node and token kinds are plain integers, so the same core serves any grammar.")
 (depends
  (ocaml
   (>= 5.2))
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test))
 (tags
  ("cst" "syntax-tree" "hash-consing" "incremental-parsing" "red-green-tree")))
