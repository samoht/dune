Test conversion of opam sources into lock dir package specifications

  $ mkrepo

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.1.tar.gz"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1

  $ showpkg() {
  >   local f="${default_lock_dir}"/testpkg.0.0.1.pkg
  >   [ -e $f ] && cat $f
  > }
 
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.1.tar.gz)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
  
  (build_id e67cd9c0260beac3b87f9c85f606560a)

  $ mkdir testpkgsources

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "file://$PWD/testpkgsources"
  > }
  > EOF

  $ rm -rf ${default_lock_dir}

  $ solve testpkg
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1

  $ showpkg | dune_cmd subst "$PWD" '<pwd>'
  (version 0.0.1)
  
  (source
   (fetch
    (url
     file://<pwd>/testpkgsources)))
  
  (dev)
  
  (build_id 1af41672d4be4b743cea530c234cc58e)

Unsupported backends:

  $ rm -rf ${default_lock_dir}

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "hg+http://no-support.com/foo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg 2>&1
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url hg+http://no-support.com/foo)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
  
  (build_id dc3ac740e3cda1d181fbe2fd55b81b2f)

git+http

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+http://github.com/foo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg 2>&1
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+http://github.com/foo)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
  
  (build_id ac314a52df84899794d5408b3994af80)

git+file

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+file://here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+file://here)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
  
  (build_id 0a0964fbba711eaf2af2e9be5ea45501)

git+foobar

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+foobar://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+foobar://random-thing-here)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
  
  (build_id d8ab95162ea2e70ea61fb9152ca0390c)

file+git

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "file+git://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (build_id f2e2d9265a868e78d52f59a855bddbb5)
