
  $ make_lockdir

Helper function to create a lockfile with a given install action and then build it, running the action.
  $ test_action() {
  >   dune clean || true
  >   make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install $1)
  > EOF
  >   build_pkg test
  > }

Tests for concat:
  $ test_action '(run echo (concat foo))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (concat foo bar))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (concat foo (concat bar (concat baz (concat)))))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (concat %{pkg-self:name} . %{pkg-self:version}))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (concat e c h o) foo)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo foo (concat) bar (concat) baz)' # two spaces between each word because (concat) is the empty string
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (= (concat foo bar) (concat f o o b a r)))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Tests for when:
  $ test_action '(run echo (when true foo) bar (when false baz) qux)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (when (= a b) xxx) (when (and (< 41 42) (<> foo bar)) echo) foo)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (when invalid-condition echo) foo)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (when (and (< 1 2) (or (concat t r u e) (concat f a l s e))) foo) bar)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (when (or true invalid-condition-ignored-due-to-laziness) foo))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (when (not (and false invalid-condition-ignored-due-to-laziness)) foo))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (concat (when true foo) (when false bar) baz))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (when (or %{pkg-self:not_a_variable} true) foo))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (when (or true %{pkg-self:not_a_variable}) foo))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Tests for if:
  $ test_action '(run echo (if (= %{pkg-self:version} 0.0.1) foo bar) (if (<> %{pkg-self:name} test) baz qux))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (if (if true true false) (concat foo bar) (concat baz qux)))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (if invalid-condition foo bar))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Tests for has_undefined_var:
  $ test_action '(run echo (if (has_undefined_var %{pkg-self:name}) foo bar))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (if (has_undefined_var %{pkg-self:not_a_variable}) foo bar))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (if (has_undefined_var (when %{pkg-self:not_a_variable} foo)) foo bar))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Test conversion from blang to string:
  $ test_action '(run echo (and (= (concat foo bar) foobar)))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run echo (and true false))'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Test the error message when the program doesn't exist:
  $ test_action '(run madeup)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (concat e c h o) hello)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (concat m a d e u p) hello)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
  $ test_action '(run (if true madeup echo) hello)'
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
