Test out-of-sync detection for both directory and single-file lock formats.

This test verifies the workflow matrix:
| Source lock changed? | dune-project changed? | Expected |
|---------------------|----------------------|----------|
| No                  | No                   | in sync  |
| No                  | Yes                  | OUT OF SYNC |
| Yes                 | No                   | in sync  |
| Yes                 | Yes                  | in sync  |

Setup:

  $ mkrepo
  $ mkpkg foo <<EOF
  > build: [ "echo" "foo" ]
  > EOF
  $ mkpkg bar <<EOF
  > build: [ "echo" "bar" ]
  > EOF
  $ add_mock_repo_if_needed

=============================================================================
PART 1: Default lock format (single-file)
=============================================================================

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ dune pkg lock 2>&1 | head -1
  Solution for dune.lock (1 package)

  $ test -f dune.lock && echo "single-file format"
  single-file format

Scenario 1.1: No changes - should be in sync
  $ dune build 2>&1 | grep -E "Error:" || echo "in sync"
  in sync

Scenario 1.2: Edit dune-project (add dep) without re-locking - OUT OF SYNC
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo bar))
  > EOF

  $ dune build 2>&1 | grep -E "Error:"
  Error: Lock dir out of sync with dune-project

Scenario 1.3: Re-lock after edit - back in sync
  $ dune pkg lock 2>&1 | head -1
  Solution for dune.lock (2 packages)

  $ dune build 2>&1 | grep -E "Error:" || echo "in sync"
  in sync

Scenario 1.4: Edit dune-project (remove dep) without re-locking - OUT OF SYNC
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ dune build 2>&1 | grep -E "Error:"
  Error: Lock dir out of sync with dune-project

=============================================================================
PART 2: Single-file format lock
=============================================================================

  $ rm -rf dune.lock _build

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ dune pkg lock --format=file 2>&1 | head -1
  Solution for dune.lock (1 package)

  $ test -f dune.lock && echo "single-file format"
  single-file format

Scenario 2.1: No changes - should be in sync
  $ dune build 2>&1 | grep -E "Error:" || echo "in sync"
  in sync

Scenario 2.2: Edit dune-project (add dep) without re-locking - OUT OF SYNC
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo bar))
  > EOF

  $ dune build 2>&1 | grep -E "Error:"
  Error: Lock dir out of sync with dune-project

Scenario 2.3: Re-lock after edit - back in sync
  $ dune pkg lock --format=file 2>&1 | head -1
  Solution for dune.lock (2 packages)

  $ dune build 2>&1 | grep -E "Error:" || echo "in sync"
  in sync

Scenario 2.4: Edit dune-project (remove dep) without re-locking - OUT OF SYNC
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ dune build 2>&1 | grep -E "Error:"
  Error: Lock dir out of sync with dune-project

=============================================================================
PART 3: Verify derivation caching behavior for single-file locks
=============================================================================

This is the critical test: when dune-project changes but source lock doesn't,
does the derived lock retain the OLD hash (enabling out-of-sync detection)?

  $ rm -rf dune.lock _build

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ dune pkg lock --format=file 2>&1 | head -1
  Solution for dune.lock (1 package)

Build once to populate the derived lock cache:
  $ dune build 2>&1 | grep -E "Error:" || echo "in sync"
  in sync

Check derived lock exists and has dependency_hash:
  $ test -f _build/.locks/default/dune.lock/lock.dune && echo "derived lock exists"
  derived lock exists
  $ grep dependency_hash _build/.locks/default/dune.lock/lock.dune | wc -l | tr -d ' '
  1

Now change dune-project WITHOUT touching the source lock:
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo bar))
  > EOF

The source lock file is unchanged (still has only foo):
  $ grep packages dune.lock
  (packages foo.0.0.1)

Build should detect out-of-sync (derived lock should have OLD hash):
  $ dune build 2>&1 | grep -E "Error:"
  Error: Lock dir out of sync with dune-project
