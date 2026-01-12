Test the --lock and --pkg flags for dune build.

The --lock flag controls whether package management is used:
- auto: Default behavior (use workspace config, then check for lock file)
- disabled: Ignore lock file, use system packages only
- enabled: Enable package management, auto-lock if missing
- always: Always re-solve before building

The --pkg flag is a high-level convenience flag:
- enabled: Auto-lock + auto-fetch
- portable: Auto-lock for all platforms + auto-fetch
- disabled: No automatic locking or fetching

  $ mkrepo
  $ add_mock_repo_if_needed

Create a simple project that depends on an external package:

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name bar)
  >  (name bar)
  >  (libraries foo))
  > EOF

  $ cat > bar.ml << EOF
  > let () = print_endline "Hello"
  > EOF

Test --lock=disabled: should NOT use package management.
Even without a lock file, should fail because foo is not in the system:

  $ dune build --lock=disabled 2>&1 | grep -E "(Error|not found)" | head -2
  Error: Library "foo" not found.

Now enable package management in workspace:

  $ enable_pkg

Test --lock=disabled should override workspace (pkg enabled):

  $ dune build --lock=disabled 2>&1 | grep -E "(Error|not found)" | head -2
  Error: Library "foo" not found.

Test that the --pkg help text is available (grep needs special handling for man page formatting):

  $ dune build --help 2>&1 | grep -i 'high-level package management control' | head -1
             High-level package management control (one of eennaabblleedd, ppoorrttaabbllee or

Test that --pkg=disabled environment variable works (disables both lock and fetch):

  $ rm -rf dune.lock duniverse _build
  $ DUNE_CONFIG__PKG=disabled dune build 2>&1 | grep -E "(Error|not found)" | head -2
  Error: Library "foo" not found.
