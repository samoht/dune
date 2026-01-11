Demonstrate a cycle from package sources:

CR-rgrinberg: the cycle checking is disabled for now because it's not clear if
it's even worth checking for. What matters is that there are no cycles at the
package level, the sources can contain a cycle, we just need to make sure we
detect it and not descend into an infinite loop.

  $ mkdir a b

  $ cat >a/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/b")
  >  (package (name b)))
  > (package
  >  (name a)
  >  (depends b))
  > EOF

  $ cat >b/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/a")
  >  (package (name a)))
  > (package
  >  (name b)
  >  (depends a))
  > EOF

  $ runtest() {
  > local res;
  > res=$(cd $1 && mkrepo && add_mock_repo_if_needed && dune pkg lock 2>&1)
  > local code=$?
  > printf "$res" $res
  > return $code
  > }

  $ runtest a
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  dune:
  - b.dev
  $ runtest b
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  dune:
  - a.dev
