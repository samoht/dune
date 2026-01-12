Test the behavior of source pinning with different URL schemes.

This test documents known limitations:
- First build after lock fails because duniverse is created during build but
  memo cache was already populated without it
- Second build works because duniverse already exists

  $ mkrepo
  $ add_mock_repo_if_needed

Create a dependency package:

  $ mkdir _dependency
  $ cat > _dependency/dune-project <<EOF
  > (lang dune 3.21)
  > (package (name dependency))
  > EOF
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "initial"
  > EOF
  $ cat > _dependency/dune <<EOF
  > (library (public_name dependency))
  > EOF

Initialize as git repo:

  $ git -C _dependency init --quiet
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Initial" --quiet

Create main executable:

  $ cat > main.ml <<EOF
  > print_endline Dependency.version
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dependency))
  > EOF

Test 1: Pin with git+file:// URL (recommended for git repos)

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "git+file://$PWD/_dependency")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF

  $ dune pkg lock --format=directory 2> /dev/null
  $ dune exec ./main.exe
  File "dune", line 3, characters 12-22:
  3 |  (libraries dependency))
                  ^^^^^^^^^^
  Error: Library "dependency" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

TODO: First build should work. Currently fails because duniverse is created
during build but memo cache was already populated.

Second build works:
  $ dune exec ./main.exe
  initial

Update dependency and commit:
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "updated"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update" --quiet

Without re-locking, we still get the locked version:
  $ dune exec ./main.exe
  initial

Re-lock and rebuild to get updated version:
  $ rm -rf dune.lock _build
  $ dune pkg lock --format=directory 2> /dev/null
  $ dune exec ./main.exe
  File "dune", line 3, characters 12-22:
  3 |  (libraries dependency))
                  ^^^^^^^^^^
  Error: Library "dependency" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]
  $ dune exec ./main.exe
  updated
