(lang dune 3.17)

(name ppx_deriving_jsont)

(generate_opam_files)

(source
 (github voodoos/ppx_deriving_jsont))

(license MIT)

(authors "Ulysse Gérard")

(maintainers "Ulysse Gérard")

(package
 (allow_empty)
 (name ppx_deriving_jsont)
 (synopsis "A ppx deriver for Jsont")
 (description "A convenient tool to generate Jsont descriptions for various OCaml types.")
 (depends
  (ppxlib (>= 0.36.0))
  jsont
  (bytesrw :with-test)))
