Fetch from more than one source

  $ make_lockdir
  $ mkdir foo
  $ cat >foo/bar <<EOF
  > this is bar
  > EOF

  $ cat >baz <<EOF
  > this is baz
  > EOF

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/foo))
  > (extra_sources (mybaz (copy $PWD/baz)))
  > (build
  >  (system "find . | sort -u"))
  > EOF

  $ build_pkg test
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("fetch_local: unpack is not set",
     { url =
         "file:///Users/samoht/git/dune/_build/.sandbox/bc15a8b80683882dea8c263e09b54346/default/test/blackbox-tests/test-cases/pkg/baz"
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Fetch.fetch.(fun) in file "src/dune_pkg/fetch.ml", line
    276, characters 11-90
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

Make sure extra source patches are downloaded, checksum-verified and applied
when building.

First we need a project that will have the patch applied:

  $ mkdir needs-patch
  $ cd needs-patch
  $ git init --quiet
  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (name needs-patch)
  > EOF
  $ cat > needs-patch.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ cat > dune <<EOF
  > (library (public_name needs-patch) (name needs_patch))
  > EOF
  $ cat > needs_patch.ml <<EOF
  > let msg = "Needs to be patched"
  > EOF
  $ cd ..
  $ tar cf needs-patch.tar needs-patch
  $ SRC_MD5=$(md5sum needs-patch.tar | cut -f1 -d' ')
  $ cd needs-patch
  $ git add -A
  $ git commit -m "Initial" --quiet
  $ cat > needs_patch.ml <<EOF
  > let msg = "Patch successfully applied"
  > EOF
  $ git diff > ../required.patch
  $ git add needs_patch.ml
  $ git commit -m "First patch" --quiet
  $ cat > needs_patch.ml <<EOF
  > let msg = "Patch successfully applied, multiple times"
  > EOF
  $ git diff > ../additional.patch
  $ cd ..
  $ rm -rf needs-patch
  $ REQUIRED_PATCH_MD5=$(md5sum required.patch | cut -f1 -d' ')
  $ ADDITIONAL_PATCH_MD5=$(md5sum additional.patch | cut -f1 -d' ')

Then we start the oneshot server for both the source and the patch.

  $ echo needs-patch.tar > fake-curls
  $ SRC_PORT=1
  $ echo required.patch >> fake-curls
  $ REQUIRED_PATCH_PORT=2

We now have the checksums as well as the port numbers, so we can define the
package.

  $ mkrepo
  $ mkpkg needs-patch 0.0.1 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > patches: ["required.patch"]
  > url {
  >   src: "http://localhost:$SRC_PORT"
  >   checksum: "md5=$SRC_MD5"
  > }
  > extra-source "required.patch" {
  >   src: "http://localhost:$REQUIRED_PATCH_PORT"
  >   checksum: "md5=$REQUIRED_PATCH_MD5"
  > }
  > EOF

Now let's depend on that project with a binary that will display the message
that is supposed to get patched.

  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (package (name my) (depends needs-patch))
  > EOF
  $ cat > dune <<EOF
  > (executable (public_name display) (libraries needs-patch))
  > EOF
  $ cat > display.ml <<EOF
  > let () = print_endline Needs_patch.msg
  > EOF

Lock the dependency, it should generate an a lock dir that references both the
url and the extra source.

  $ add_mock_repo_if_needed
  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  dune:
  - needs-patch.0.0.1

  $ dune_cmd subst 'md5=[0-9a-f]+' 'md5=$HASH'< "${default_lock_dir}/needs-patch.0.0.1.pkg"
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (patch required.patch)
       (run dune build -p %{pkg-self:name} -j %{jobs}))))))
  
  (source
   (fetch
    (url http://localhost:1)
    (checksum md5=$HASH)))
  
  (extra_sources
   (required.patch
    (fetch
     (url http://localhost:2)
     (checksum md5=$HASH))))
  
  (build_id 3939fc28893b257311c39743f333dd8f)

Running the binary should download the tarball & patch, build them and show the
correct, patched, message:

  $ dune exec ./display.exe
  File "dune", line 1, characters 45-56:
  1 | (executable (public_name display) (libraries needs-patch))
                                                   ^^^^^^^^^^^
  Error: Library "needs-patch" not found.
  -> required by _build/default/.display.eobjs/native/dune__exe__Display.cmx
  -> required by _build/default/display.exe
  [1]

Set up a new version of the package which has multiple `extra-sources`, the
application order of them mattering:

  $ echo needs-patch.tar >> fake-curls
  $ SRC_PORT=3
  $ echo required.patch >> fake-curls
  $ REQUIRED_PATCH_PORT=4
  $ echo additional.patch >> fake-curls
  $ ADDITIONAL_PATCH_PORT=5

  $ mkpkg needs-patch 0.0.2 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > patches: ["required.patch" "additional.patch"]
  > url {
  >   src: "http://localhost:$SRC_PORT"
  >   checksum: "md5=$SRC_MD5"
  > }
  > extra-source "required.patch" {
  >   src: "http://localhost:$REQUIRED_PATCH_PORT"
  >   checksum: "md5=$REQUIRED_PATCH_MD5"
  > }
  > extra-source "additional.patch" {
  >   src: "http://localhost:$ADDITIONAL_PATCH_PORT"
  >   checksum: "md5=$ADDITIONAL_PATCH_MD5"
  > }
  > EOF

Lock the project to use that new package

  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  dune:
  - needs-patch.0.0.2

Running the binary should work and output the double patched message:

  $ dune exec ./display.exe
  File "dune", line 1, characters 45-56:
  1 | (executable (public_name display) (libraries needs-patch))
                                                   ^^^^^^^^^^^
  Error: Library "needs-patch" not found.
  -> required by _build/default/.display.eobjs/native/dune__exe__Display.cmx
  -> required by _build/default/display.exe
  [1]
