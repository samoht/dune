With a faulty version of OCamlFormat, "dune fmt" is supposed to stop with the
build error of "ocamlformat".

  $ mkrepo

Make a fake ocamlformat with a missing ocamlformat.ml file:
  $ make_fake_ocamlformat "0.26.4" "no-ml-file"
  $ make_ocamlformat_opam_pkg "0.26.4"

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

It fails during the build because of missing OCamlFormat module.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.4
  File "ocamlformat.pkg", line 4, characters 6-10:
  Error: Logs for package ocamlformat
  File "dune", line 2, characters 14-25:
  2 |  (public_name ocamlformat))
                    ^^^^^^^^^^^
  Error: Module "Ocamlformat" doesn't exist.
  
  [1]
