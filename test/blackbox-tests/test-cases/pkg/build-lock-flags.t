Test the --auto-lock flag for dune build.

  $ mkrepo

Create a simple package in the mock repository.

  $ mkpkg foo <<EOF
  > build: [ "echo" "building foo" ]
  > EOF

  $ mkpkg bar <<EOF
  > build: [ "echo" "building bar" ]
  > depends: [ "foo" ]
  > EOF

Setup a project with dependencies.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name myproject)
  >  (depends bar))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name myproject))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline "Hello"
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.16)
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock))
  > EOF

First, create the initial lock file.

  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

Verify the lock file exists.

  $ cat dune.lock/lock.dune
  (lang package 0.1)

  (repositories
   (complete true)
   (used))

  $ ls dune.lock/*.pkg
  dune.lock/bar.pkg
  dune.lock/foo.pkg

Test --auto-lock=disabled (default): should use system packages, ignoring the lock file.
This will fail if the packages aren't available in the system/opam switch.

  $ dune build --auto-lock=disabled 2>&1 | head -5
  Error: Library "bar" not found.
  [1]

The error is expected - bar isn't installed in the system. This confirms
--auto-lock=disabled is ignoring the lock file and using system packages.

Test --auto-lock=always: should re-solve and update the lock file.
First, add a new version of foo to the repository.

  $ mkpkg foo 1.0.0 <<EOF
  > build: [ "echo" "building foo 1.0.0" ]
  > EOF

Now --auto-lock=always should pick up the new version.

  $ dune build --auto-lock=always 2>&1 | grep -E "^Solution|^-"
  Solution for dune.lock:
  - bar.0.0.1
  - foo.1.0.0

Verify the lock file was updated.

  $ grep version dune.lock/foo.pkg
  (version 1.0.0)

Test that default (disabled) doesn't use the lock file.

  $ dune build 2>&1 | head -5
  Error: Library "bar" not found.
  [1]

Test that --auto-lock=always can be enabled via environment variable.

  $ mkpkg foo 2.0.0 <<EOF
  > build: [ "echo" "building foo 2.0.0" ]
  > EOF

  $ DUNE_CONFIG__AUTO_LOCK=always dune build 2>&1 | grep -E "^Solution|^-"
  Solution for dune.lock:
  - bar.0.0.1
  - foo.2.0.0

  $ grep version dune.lock/foo.pkg
  (version 2.0.0)

Test that CLI flag takes precedence over environment variable.
Set always via env, but override with disabled on CLI.

  $ DUNE_CONFIG__AUTO_LOCK=always dune build --auto-lock=disabled 2>&1 | head -5
  Error: Library "bar" not found.
  [1]

Test --auto-lock=enabled: should auto-lock only if lock is missing.
First remove the lock file.

  $ rm -rf dune.lock

  $ mkpkg foo 3.0.0 <<EOF
  > build: [ "echo" "building foo 3.0.0" ]
  > EOF

  $ dune build --auto-lock=enabled 2>&1 | grep -E "^Solution|^-"
  Solution for dune.lock:
  - bar.0.0.1
  - foo.3.0.0

Lock file should now exist.

  $ ls dune.lock/*.pkg
  dune.lock/bar.pkg
  dune.lock/foo.pkg

Add a newer version - with enabled mode, it should NOT re-solve since lock exists.

  $ mkpkg foo 4.0.0 <<EOF
  > build: [ "echo" "building foo 4.0.0" ]
  > EOF

  $ dune build --auto-lock=enabled 2>&1 | grep -E "^Solution" || echo "No re-solving (lock exists)"
  No re-solving (lock exists)

  $ grep version dune.lock/foo.pkg
  (version 3.0.0)
