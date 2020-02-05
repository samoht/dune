  $ dune build ./.mirage/foo.libs
  Info: Creating file dune-project with this contents:
  | (lang dune 2.2)
  Info: Appending this line to dune-project: (using mirage 0.1)
  $ dune exec -- cat _build/default/.mirage/foo.libs
  $ dune build @depends
  $ dune build ./foo.exe
