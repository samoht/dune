Setting environment variables in actions

  $ make_lockdir
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (withenv
  >   ((= FOO myfoo)
  >    (= XYZ 000)
  >    (+= XYZ 111)
  >    (= BAR xxx)
  >    (+= BAR yyy)
  >    (:= BAR "")
  >    (+= BAR "")
  >    (:= BAZ baz)
  >    (=: QUX qux))
  >   (system "echo XYZ=$XYZ; echo FOO=$FOO; echo BAR=$BAR; echo BAZ=$BAZ; echo QUX=$QUX")))
  > EOF
  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Note that the value so of BAZ and QUX above should be "baz:" and ":qux" respectively.
See https://github.com/ocaml/dune/issues/10440
