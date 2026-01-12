In this test we test the translation of a package with a build-env field and no build or
install step into a dune lock file.

  $ mkrepo

Make a package with a build-env field and no build or install step
  $ mkpkg with-build-env <<'EOF'
  > build-env: [ [ MY_ENV_VAR = "Hello from env var!" ] ]
  > EOF

  $ solve with-build-env
  Solution for dune.lock (1 package):
  opam:
  - with-build-env.0.0.1

When there is no build or install step the build environment does not appear in the lock
file.

  $ cat ${default_lock_dir}/with-build-env.0.0.1.pkg
  (version 0.0.1)
  
  (build_id 7936eaf0734c289a55c052301a07b846)
