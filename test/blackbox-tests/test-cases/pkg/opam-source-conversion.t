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
  
  (build_id ef610f7272995a1ce246c59da07960c6)

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
  
  (build_id 4b35c36bf23cc8313124bc9dc8666ac2)

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
  
  (build_id ef610f7272995a1ce246c59da07960c6)

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
  
  (build_id ef610f7272995a1ce246c59da07960c6)

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
  
  (build_id ef610f7272995a1ce246c59da07960c6)

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
  
  (build_id ef610f7272995a1ce246c59da07960c6)

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
