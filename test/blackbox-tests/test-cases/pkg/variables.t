Test that we can set variables

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| opam-version: "2.0"
  >          "\| variables {
  >          "\|   abool: true
  >          "\|   astring: "foobar"
  >          "\|   somestrings: ["foo" "bar"]
  >          "\|   version: "1.2.3"
  >          "\| }
  >          "\| EOF
  >  ))
  > EOF

  $ make_lockpkg usetest <<EOF
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (progn
  >   (system "\| echo abool: %{pkg:test:abool}
  >           "\| echo astring: %{pkg:test:astring}
  >           "\| echo somestrings: %{pkg:test:somestrings}
  >           "\| echo share path: %{pkg:test:share}
  >           "\| echo version: %{pkg:test:version}
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  File "_build/.locks/default/dune.lock/usetest.pkg", line 5, characters 26-43:
  5 |   (system "\| echo abool: %{pkg:test:abool}
                                ^^^^^^^^^^^^^^^^^
  Error: Undefined package variable: abool
  [1]

  $ show_pkg_cookie test
  { files = []; variables = [] }

Now we demonstrate we get a proper error from invalid .config files:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| this is dummy text
  >          "\| EOF
  >  ))
  > EOF

  $ build_pkg test 2>&1 | dune_cmd subst 'File .*:' 'File $REDACTED:'
  
