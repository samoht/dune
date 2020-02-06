  $ dune build ./.mirage/foo.libs
  Info: Creating file dune-project with this contents:
  | (lang dune 2.2)
  Info: Appending this line to dune-project: (using mirage 0.1)
  $ cat _build/default/.mirage/foo.libs
  $ cat _build/default/.mirage/foo.packages
  $ dune build ./.mirage/mirage.all-packages
  $ cat _build/default/.mirage/mirage.all-packages
