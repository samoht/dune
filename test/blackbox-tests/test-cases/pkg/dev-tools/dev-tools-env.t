Test `dune tools env` command for both POSIX and fish shells.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

The POSIX shell output should export PATH with the default install bin path.
All dev tools are promoted to _build/install/default/bin/.

  $ dune tools env
  export PATH=$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:$TESTCASE_ROOT/_build/install/default/bin:/Users/samoht/git/dune/_build/default/test/blackbox-tests/test-cases/.bin:/Users/samoht/git/dune/_build/install/default/bin:/Users/samoht/git/dune/_opam/bin:/Users/samoht/.local/bin:/Users/samoht/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/Library/TeX/texbin:/Users/samoht/.cargo/bin:/Applications/iTerm.app/Contents/Resources/utilities

The fish shell output should use fish_add_path.

  $ dune tools env --fish
  fish_add_path --prepend _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin _build/install/default/bin
