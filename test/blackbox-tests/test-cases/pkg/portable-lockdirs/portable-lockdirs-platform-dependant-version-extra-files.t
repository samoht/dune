Test that extra files associated with a package are handled correctly when
multiple different versions of the package are present in the lockdir.

  $ mkrepo
  $ add_mock_repo_if_needed

Define 2 versions of the package foo that write their version number to a file
during their build so we can validate which version was built.

  $ VERSION1_FILE=mock-opam-repository/packages/foo/foo.1/files/version.txt
  $ VERSION2_FILE=mock-opam-repository/packages/foo/foo.2/files/version.txt

  $ mkdir -p $(dirname $VERSION1_FILE)
  $ echo version_1 > $VERSION1_FILE

  $ mkdir -p $(dirname $VERSION2_FILE)
  $ echo version_2 > $VERSION2_FILE

  $ mkpkg foo 1 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION1_FILE | cut -f1 -d' ')"]
  > ]
  > EOF
  $ mkpkg foo 2 <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > extra-files: [
  >   ["version.txt" "md5=$(md5sum $VERSION2_FILE | cut -f1 -d' ')"]
  > ]
  > EOF

Define a package bar which conditionally depends on different versions of foo:

  $ mkpkg bar <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > depends: [
  >   "foo" {= "1" & os = "linux"}
  >   "foo" {= "2" & os = "macos"}
  > ]
  > EOF

Define a project with a package depending on bar:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends bar))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Solve the project. The solution will contain extra files for both versions of foo:
  $ dune pkg lock --format=directory
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.of_list_exn", { key = "2" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Stdlib__Map.Make.map in file "map.ml", line 312, characters 19-22
  Called from Dune_pkg__Lock.merge_conditionals in file "src/dune_pkg/lock.ml",
    lines 1519-1524, characters 4-27
  Called from Dune_pkg__Opam_solver.Solver_result.merge in file
    "src/dune_pkg/opam_solver.ml", line 1611, characters 19-64
  Called from Stdlib__List.fold_left in file "list.ml", line 125, characters
    24-34
  Called from Dune__exe__Pkg__Lock.solve_multiple_platforms in file
    "bin/pkg/lock.ml", line 221, characters 6-75
  Called from Fiber__Core.O.(>>|).(fun) in file "src/fiber/src/core.ml", line
    257, characters 36-41
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

Verify the contents of the extra files for each version of foo:
  $ cat ${default_lock_dir}/foo.1.files/version.txt
  cat: dune.lock/foo.1.files/version.txt: No such file or directory
  [1]
  $ cat ${default_lock_dir}/foo.2.files/version.txt
  cat: dune.lock/foo.2.files/version.txt: No such file or directory
  [1]

Build as if we're on linux and verify that the appropriate extra file was copied into _build:
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat ${default_lock_dir}/foo.1.files/version.txt
  cat: dune.lock/foo.1.files/version.txt: No such file or directory
  [1]

  $ dune clean

Build as if we're on macos and verify that the appropriate extra file was copied into _build:
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat ${default_lock_dir}/foo.2.files/version.txt
  cat: dune.lock/foo.2.files/version.txt: No such file or directory
  [1]
