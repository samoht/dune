Test the installation of toolchains package by building and installing
a mock compiler package using dune's toolchain mechanism.

  $ make_lockdir
  $ add_mock_repo_if_needed

We create a fake compiler by creating a configure file.

  $ mkdir fake-compiler
  $ cat > fake-compiler/configure << 'EOF'
  > #!/bin/sh
  > PREFIX=$1
  > echo $PREFIX > prefix.txt
  > EOF
  $ chmod a+x fake-compiler/configure

We add a shell script to be installed as a fake compiler

  $ mkdir -p fake-compiler/target/share/bin
  $ cat > fake-compiler/target/share/bin/ocamlc << EOF
  > #!/bin/sh
  > echo "Hello from fake ocamlc!"
  > EOF
  $ chmod a+x fake-compiler/target/share/bin/ocamlc

We make sure the installed script is installing the script at the correct
location

  $ cat > fake-compiler/make << 'EOF'
  > #!/bin/sh
  > prefix=$(cat prefix.txt)
  > target=${DESTDIR}${prefix}
  > install() {
  >   mkdir -p "${target}"
  >   cp -r target/* "${target}"
  > }
  > install
  > EOF
  $ chmod +x fake-compiler/make

We generate the lockfile for the fake compiler

  $ make_lockpkg ocaml-base-compiler << EOF
  > (version 1)
  > (build
  >  (run ./configure %{prefix}))
  > (install
  >  (run ./make install))
  > (source
  >  (copy $PWD/fake-compiler))
  > EOF

We generate the lock file for the package to demonstrate the variable is
replaced with the path to the sandbox inside of the path to the non-relocatable
location.

  $ make_lockpkg baz << EOF
  > (version 1)
  > (build
  >  (run sh -exc "echo %{pkg:ocaml-base-compiler:share}"))
  > (depends ocaml-base-compiler)
  > EOF

We generate a fake package to use it

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > (package
  >  (name foo)
  >  (depends ocaml-base-compiler baz))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > foo.ml << EOF
  > print_endline "Hello, World!"
  > EOF

We try to build the dependency to show that it echoes the correct path. For
toolchain packages, the path points to the toolchain cache directory.

  $ XDG_CACHE_HOME=$PWD/fake-cache dune build @pkg-install 2>&1 \
  > | dune_cmd subst '[[:alnum:]]{32}' '<hash>' \
  > | dune_cmd subst '[^ ]*_build' '$TESTCASE_ROOT/_build'
  Internal error, please report upstream including the contents of $TESTCASE_ROOT/_build/log.
  Description:
    $TESTCASE_ROOT/_build_dir_exn] called on something not in build dir",
     { t =
         External
           $TESTCASE_ROOT/_build/.sandbox/<hash>/default/test/blackbox-tests/test-cases/pkg/fake-cache/dune/toolchains/ocaml-base-compiler.1-<hash>/target/lib/ocaml-base-compiler"
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_rules__Pkg_rules.build_only_rule.(fun) in file
    "src/dune_rules/pkg_rules.ml", line 2249, characters 6-73
  Called from Stdlib__List.rev_map.rmap_f in file "list.ml", line 107,
    characters 22-25
  Called from Dune_rules__Pkg_rules.build_only_rule in file
    "src/dune_rules/pkg_rules.ml", lines 2247-2249, characters 4-90
  Called from Fiber__Core.O.(>>|).(fun) in file "src/fiber/src/core.ml", line
    257, characters 36-41
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by
     ("load-dir", $TESTCASE_ROOT/_build_dir ".pkgs/default/ocaml-base-compiler.1")
  -> required by
     ("build-file", $TESTCASE_ROOT/_build_dir ".pkgs/default/ocaml-base-compiler.1/cookie")
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", $TESTCASE_ROOT/_build_dir ".pkgs/default/baz.1")
  -> required by ("build-file", $TESTCASE_ROOT/_build_dir ".pkgs/default/baz.1/cookie")
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = $TESTCASE_ROOT/_build_dir "default"; name = "pkg-install" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.

