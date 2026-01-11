Test the (vendor ...) stanza for selective library exposure from vendored directories.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a duniverse-style vendored directory structure:

  $ mkdir -p duniverse/fmt.0.9.0
  $ cat >duniverse/fmt.0.9.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name fmt))
  > EOF

  $ cat >duniverse/fmt.0.9.0/dune <<EOF
  > (library
  >  (name fmt)
  >  (modules fmt)
  >  (public_name fmt))
  > (library
  >  (name fmt_tty)
  >  (modules fmt_tty)
  >  (public_name fmt.tty))
  > (library
  >  (name fmt_cli)
  >  (modules fmt_cli)
  >  (public_name fmt.cli))
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt.ml <<EOF
  > let greeting = "Hello from fmt"
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt_tty.ml <<EOF
  > let tty_msg = "TTY output"
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt_cli.ml <<EOF
  > let cli_msg = "CLI output"
  > EOF

Use vendored_dirs with vendor stanza to expose only specific libraries:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0 (libraries fmt fmt.tty))
  > EOF

Create main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries fmt))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Fmt.greeting
  > EOF

Build should succeed with allowed library:

  $ dune build main.exe

Test that vendor stanza is parsed with packages field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt fmt.tty)
  >  (packages fmt))
  > EOF

  $ dune build main.exe

Test duplicate vendor stanza error:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0 (libraries fmt))
  > (vendor fmt.0.9.0 (libraries fmt.tty))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "duniverse/dune", line 3, characters 0-38:
  3 | (vendor fmt.0.9.0 (libraries fmt.tty))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: vendor stanza for directory "fmt.0.9.0" already defined

Test vendor stanza with mode field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt)
  >  (mode dune))
  > EOF

  $ dune build main.exe

Test vendor stanza with install field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt)
  >  (install true))
  > EOF

  $ dune build main.exe

Test vendor stanza with toolchain field (native compiler):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt)
  >  (toolchain native))
  > EOF

  $ dune build main.exe

Test vendor stanza with toolchain field (cross-compilation):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt)
  >  (toolchain windows))
  > EOF

  $ dune build main.exe

Test vendor stanza with all fields:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt fmt.tty)
  >  (packages fmt)
  >  (mode dune)
  >  (install true)
  >  (toolchain native))
  > EOF

  $ dune build main.exe
