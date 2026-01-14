Test lazy dev tool loading via dune fmt.

When dune fmt is called with DUNE_CONFIG__LOCK_DEV_TOOL=enabled,
it should lazily install ocamlformat only when needed.

  $ mkrepo

Create fake ocamlformat package.

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"

Set up workspace.

  $ make_project_with_dev_tool_lockdir

  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

Set up a custom cache directory for testing.

  $ export XDG_CACHE_HOME=$PWD/fake-cache

=== Test 1: dune fmt lazily installs ocamlformat ===

No lock directory exists yet:

  $ test -d _build/.locks/tools-ocamlformat && echo "Lock exists" || echo "No lock dir"
  No lock dir

Running dune fmt with lazy dev tool enabled should trigger installation:

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview 2>&1 | head -10
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2
     Vendoring ocamlformat.0.26.2
  File "ocamlformat.pkg", line 4, characters 6-10:
  4 |  (run dune build -p %{pkg-self:name} @install))
            ^^^^
  Error: Unknown action run

Lock directory should now exist (but currently doesn't due to error above):

  $ test -d _build/.locks/tools-ocamlformat && echo "Lock exists" || echo "No lock dir"
  No lock dir

=== Test 2: Subsequent dune fmt does not re-solve ===

Running dune fmt again should NOT show "Solution for" message:

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview 2>&1 | grep -c "Solution for" || echo "No re-solving"
  0
  No re-solving
