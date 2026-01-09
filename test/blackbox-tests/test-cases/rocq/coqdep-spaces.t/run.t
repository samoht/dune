Testing a simple composition of theories. We have two theories A and B and B
depends on A.

  $ dune build
  File "lib B/dune", lines 1-4, characters 0-55:
  1 | (rocq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a1.vo"
  File "lib B/dune", lines 1-4, characters 0-55:
  1 | (rocq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a2.vo"
  File "lib B/dune", lines 1-4, characters 0-55:
  1 | (rocq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib B/A/a3.vo"
  File "lib B/dune", lines 1-4, characters 0-55:
  1 | (rocq.theory
  2 |  (name B)
  3 |  (package Simple)
  4 |  (theories A))
  Error: No rule found for "lib\\"
  [1]

We inspect the contents of the build directory.

  $ ls _build/install/default/lib/coq/user-contrib/A/a1.vo
  _build/install/default/lib/coq/user-contrib/A/a1.vo
  $ ls "_build/default/lib A/a1.vo"
  _build/default/lib A/a1.vo
  $ test -f _build/install/default/lib/coq/user-contrib/B/b.vo || echo "B/b.vo not installed (expected)"
  B/b.vo not installed (expected)
  $ test -f "_build/default/lib B/b.vo" || echo "lib B/b.vo not built (expected)"
  lib B/b.vo not built (expected)
