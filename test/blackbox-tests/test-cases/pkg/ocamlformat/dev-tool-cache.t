Test dev tool caching as specified in doc/dev/tool-caching.md

This test verifies the UX for compiler-independent tools (ocamlformat):

1. Global cache at ~/.cache/dune/tools/
2. Cache key: {package_name}.{version} for compiler-independent tools
3. Fast reinstall after dune clean (cache survives clean)
4. dune tools which shows cache path
5. No lock files written outside _build/

See also: dev-tool-cache.t in pkg/odoc/ for compiler-dependent tools

  $ mkrepo

Create fake ocamlformat package (compiler-independent tool).

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"

Set up workspace.

  $ make_project_with_dev_tool_lockdir

  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

Set up a custom cache directory for testing.

  $ export XDG_CACHE_HOME=$PWD/fake-cache

=== Test 1: Global cache is populated ===

Install ocamlformat. The binary should be cached globally.

  $ dune tools install ocamlformat
  Solution for _build/lock/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2

Check that the global cache directory was created with the expected structure.
Cache key for compiler-independent tools: {package_name}.{version}

  $ find fake-cache/dune/tools -type f 2>/dev/null | sort
  fake-cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat

=== Test 2: Symlink in _build/install/bin/ ===

A symlink should be created in _build/install/default/bin/ pointing to the cache.

  $ readlink _build/install/default/bin/ocamlformat
  $TESTCASE_ROOT/fake-cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat

=== Test 3: Running tool from cache ===

After using the tool, it should run from the cache.

  $ dune tools exec ocamlformat
       Running 'ocamlformat'
  formatted with version 0.26.2

=== Test 4: Fast reinstall after dune clean ===

The global cache should survive dune clean.

  $ dune clean

Cache should still exist.

  $ test -f fake-cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat && echo "Cache exists"
  Cache exists

Running the tool again should NOT rebuild (fast reinstall from cache).
No "Solution for" message should appear.

  $ dune tools exec ocamlformat 2>&1 | grep -v "Running"
  formatted with version 0.26.2

=== Test 5: dune tools which shows cache path ===

  $ dune tools which ocamlformat
  $TESTCASE_ROOT/fake-cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat

=== Test 6: No lock files written outside _build ===

Per design doc: "No lock file written. No files outside `_build/` in user's repo."
The dev tool should use the global cache without creating lock directories.

  $ ls -d .dune* 2>/dev/null || echo "No .dune files in repo root"
  No .dune files in repo root

  $ find . -maxdepth 1 -name "*.lock" -o -name "*lock*" -type d 2>/dev/null | grep -v _build || echo "No lock files outside _build"
  No lock files outside _build

=== Test 7: Install specific version with tool.version syntax ===

Create another version of ocamlformat.

  $ make_fake_ocamlformat "0.27.0"
  $ make_ocamlformat_opam_pkg "0.27.0"

Install a specific version using the tool.version syntax.

  $ dune tools install ocamlformat.0.27.0
  Solution for _build/lock/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.27.0

Both versions should now be in the cache.

  $ find fake-cache/dune/tools -name ocamlformat -type f | sort
  fake-cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat
  fake-cache/dune/tools/ocamlformat.0.27.0/bin/ocamlformat
