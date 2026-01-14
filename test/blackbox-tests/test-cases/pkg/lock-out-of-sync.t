Trying to build a package after updating the dependencies in dune-project but
without running `dune_pkg_lock_normalized` must raise an error in the context of Dune
Package Managemenet. 

Create a fake project and lock it:

  $ mkrepo
  $ mkpkg foo <<EOF
  > build: [ "echo" "foo" ]
  > EOF
  $ mkpkg bar <<EOF
  > build: [ "echo" "bar" ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo))
  > EOF
  $ add_mock_repo_if_needed
  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1

As the lock file is syncronised with `dune-pkg`, the build succeeds:
  $ build_pkg foo
  foo

We add the bar dependency to the test package
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo bar))
  > EOF

It fails as we have not regenerated the lock:
  $ dune build
  File "dune.lock/lock.dune", line 1, characters 0-0:
  Error: Lock dir out of sync with dune-project
  Dependencies not in lock file: bar
  Current dependencies in dune-project: bar, foo
  Hint: Run 'dune pkg lock' to regenerate
  Hint: Or use '--lock=always' or set '(lock always)' in ~/.config/dune/config
  for automatic re-locking
  [1]

We fix it and the build succeeds again:
  $ dune_pkg_lock_normalized
  Solution for dune.lock (2 packages):
  opam:
  - bar.0.0.1
  - foo.0.0.1
  $ build_pkg foo
  $ build_pkg bar
  bar

Now test out-of-sync detection with single-file lock format.
Single-file locks are derived to directory format in _build/.locks/ with
dependency_hash computed from local packages, enabling out-of-sync detection.

  $ rm -rf dune.lock _build
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo))
  > EOF
  $ dune pkg lock --format=file 2>&1 | head -6
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1



  $ test -f dune.lock && echo "dune.lock is a file"
  dune.lock is a file

Build works with single-file lock:
  $ build_pkg foo
  foo

Add a new dependency without re-locking:
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo bar))
  > EOF

Out-of-sync should be detected for single-file lock too:
(Note: error points to derived lock.dune in _build/.locks/, not source dune.lock)
  $ dune build
  File "dune.lock/pkgs/lock.dune", line 1, characters 0-0:
  Error: Lock dir out of sync with dune-project
  Dependencies not in lock file: bar
  Current dependencies in dune-project: bar, foo
  Hint: Run 'dune pkg lock' to regenerate
  Hint: Or use '--lock=always' or set '(lock always)' in ~/.config/dune/config
  for automatic re-locking
  [1]

Re-lock to fix it:
  $ dune pkg lock --format=file 2>&1 | head -6
  Solution for dune.lock (2 packages)
  
  Dependencies common to all supported platforms:
  opam:
  - bar.0.0.1
  - foo.0.0.1


  $ build_pkg foo
  $ build_pkg bar
  bar
