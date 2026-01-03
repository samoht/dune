The ocaml compiler needs to be marked inside the lock dir:

  $ mkrepo

To mark it, we use `conflict-class: "ocaml-core-compiler"`

  $ mkpkg foocaml <<EOF
  > conflict-class: "ocaml-core-compiler"
  > EOF

  $ solve foocaml
  Solution for dune.lock (1 package):
  opam:
  - foocaml.0.0.1

  $ grep ocaml ${default_lock_dir}/lock.dune
  (ocaml foocaml)
