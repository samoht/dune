We test how opam files with substs fields are translated into the dune.lock file.

  $ mkrepo

Make a package with a substs field 
  $ mkpkg with-substs <<EOF
  > substs: ["foo.ml"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ solve with-substs
  Solution for dune.lock (1 package):
  opam:
  - with-substs.0.0.1
  $ append_to_lockpkg with-substs.0.0.1 <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the substitute action.
  $ cat ${default_lock_dir}/with-substs.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (substitute foo.ml.in foo.ml)
       (run sh -c "[ -e foo.ml ] && cat foo.ml"))))))
  
  (build_id ec5f6b065e4f1d9fa1e8a38e0ff9ea07)
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source
  $ cat > source/foo.ml.in <<EOF
  > I have been substituted.
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs
  Error: Don't know how to build _build/.pkgs/default/with-substs/installed
  [1]
