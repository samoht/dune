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
  
  (build_id f45dc1c89820073e4124dafd4c108ea2)

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
  
  (build_id f2b8b00854423dfb2e7fcdc0203aaafa)

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
  
  (build_id dd7625f5580d9b891ac642747989057f)

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
  
  (build_id f5cc8ac24c29238bb29b2df136ea345f)

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
  
  (build_id c576a0724329959551e5d70add6cd2b6)

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
  
  (build_id d7489b5cd714a884f85e6b8913b02032)

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
  
  (build_id 2ebc29d2332ed3b471308f24ff693de1)
