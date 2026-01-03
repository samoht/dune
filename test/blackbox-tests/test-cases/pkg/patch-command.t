Test the dune pkg patch command

  $ mkrepo
  $ add_mock_repo_if_needed

Create a simple package:
  $ mkpkg foo << 'EOF'
  > build: ["echo" "building foo"]
  > EOF

Create a project that depends on foo:
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Lock the project:
  $ dune pkg lock 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


Test --list with no patches directory:
  $ dune pkg patch --list
  No patches directory found.

Create patches directory:
  $ mkdir patches

Test --list with empty patches directory:
  $ dune pkg patch --list
  No patches found.

Test error when no action specified:
  $ dune pkg patch 2>&1
  Error: No action specified.
  Usage: dune pkg patch <PKG> | --list | --commit <PKG> | --remove <PKG>
  [1]

Test error when package not found:
  $ dune pkg patch nonexistent 2>&1
  Error: Package nonexistent not found in lock directory
  [1]

Test prepare action (package not fetched):
  $ dune pkg patch foo 2>&1
  Error: Package directory duniverse/foo.0.0.1 does not exist.
  Run 'dune pkg fetch' first to download package sources.
  [1]

Test --commit with package not fetched:
  $ dune pkg patch --commit foo 2>&1
  Error: Package directory duniverse/foo.0.0.1 does not exist. Run 'dune pkg
  fetch' first.
  [1]

Test --remove with no patch:
  $ dune pkg patch --remove foo 2>&1
  Error: Patch patches/foo@0.0.1.patch does not exist
  [1]

Test conflicting options:
  $ dune pkg patch --list --commit foo 2>&1
  Error: Conflicting options. Specify only one action at a time.
  [1]
