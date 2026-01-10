Solving with an unknown variable on depexts:

  $ mkrepo
  $ add_mock_repo_if_needed

The "foobar" variable is not defined:
  $ mkpkg foo <<EOF
  > depexts: [[ "unzip" ] {foobar}]
  > EOF

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Locking should succeed and not include the "unzip" package.
With single-file format (the default), depexts are derived from opam repo:
  $ dune pkg lock 2>&1 | head -n 1
  Solution for dune.lock (1 package)
  $ cat dune.lock | strip_sandbox | dune_cmd subst '[a-f0-9]{40}' 'COMMIT_HASH' | grep -v "^$" | head -10
  (lang package 0.1)
  (repos
   $SANDBOX/default/test/blackbox-tests/test-cases/pkg/depexts/mock-opam-repository
    COMMIT_HASH))
  (packages foo.0.0.1)
  (platforms
   (linux
    (arch arm64 x86_64))
   (macos
    (arch arm64 x86_64)))




With directory format, the unknown variable is preserved in the pkg file:
  $ rm dune.lock
  $ dune pkg lock --format=directory 2>&1 | head -n 1
  Solution for dune.lock (1 package)
  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   ((unzip) %{pkg-self:foobar}))
  
  (build_id 3b9ce106061cfffc9b56249364ebda84)

