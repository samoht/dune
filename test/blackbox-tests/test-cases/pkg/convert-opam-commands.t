
  $ mkrepo

  $ mkpkg standard-dune <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >     "-j"
  >     jobs
  >     "@install"
  >     "@runtest" {with-test}
  >     "@doc" {with-doc}
  >   ]
  > ]
  > install: [ make "install" ]
  > EOF

  $ mkpkg with-interpolation <<EOF
  > build: [
  >   [
  >     "./configure"
  >     "--prefix=%{prefix}%"
  >     "--docdir=%{doc}%/ocaml"
  >   ]
  >   [make "-j%{jobs}%"]
  > ]
  > install: [make "install"]
  > EOF

Make sure we don't mess up percent signs that aren't part of variable interpolation syntax:
  $ mkpkg with-percent-sign <<EOF
  > build: [ "printf" "%d" "42" ]
  > EOF

  $ mkpkg with-malformed-interpolation <<EOF
  > build: [ "./configure" "--prefix=%{prefix" ]
  > EOF

  $ mkpkg variable-types <<EOF
  > build: [
  >   ["echo" local_var]
  >   ["echo" _:explicit_local_var]
  >   ["echo" foo:package_var]
  >   ["echo" os-family]
  > ]
  > EOF

Package for exercising opam filters on commands:
  $ mkpkg exercise-filters <<EOF
  > build: [
  >   [ "echo" "a" ] { foo  }
  >   [ "echo" "b" ] { foo & bar }
  >   [ "echo" "c" ] { foo & bar & baz }
  >   [ "echo" "d" ] { foo | bar }
  >   [ "echo" "e" ] { foo | bar & baz }
  >   [ "echo" "f" ] { (foo | bar) & baz }
  >   [ "echo" "b" ] { foo = bar }
  >   [ "echo" "g" ] { version < "1.0" }
  >   [ "echo" "h" ] { with-test & ocaml:version < "5.0.0" }
  >   [ "echo" "i" ] { true }
  >   [ "echo" "j" ] { ! false }
  >   [ "echo" "k" ] { foo:installed }
  >   [ "echo" "l" ] { foo:version < "0.4" }
  >   [ "echo" "m" ] { foo+bar+baz:installed }
  >   [ "echo" "n" ] { ? madeup }
  >   [ "echo" "o" ] { ? installed }
  > ]
  > EOF

Package for exercising opam filters on terms:
  $ mkpkg exercise-term-filters <<EOF
  > build: [
  >   [ "echo" "a" "b" { foo } "c" { bar & baz } ]
  > ]
  > EOF

Package which has boolean where string was expected. This should be caught while parsing:
  $ mkpkg filter-error-bool-where-string-expected <<EOF
  > build: [
  >   [ "echo" "a" ] { foo:version < (foo = bar) }
  > ]
  > EOF

  $ solve --format=directory standard-dune with-interpolation with-percent-sign variable-types
  Solution for dune.lock (4 packages):
  dune:
  - standard-dune.0.0.1
  
  opam:
  - variable-types.0.0.1
  - with-interpolation.0.0.1
  - with-percent-sign.0.0.1



  $ cat ${default_lock_dir}/standard-dune.0.0.1.pkg
  (version 0.0.1)
  
  (install
   (all_platforms
    (run %{make} install)))
  
  (build
   (all_platforms
    ((action
      (progn
       (when %{pkg-self:dev} (run dune subst))
       (run dune build -p %{pkg-self:name} -j %{jobs} @install))))))
  
  (build_id bc6647f9aa5e02e45f87bf11d774ffbc)

  $ cat ${default_lock_dir}/with-interpolation.0.0.1.pkg
  (version 0.0.1)
  
  (install
   (all_platforms
    (run %{make} install)))
  
  (build
   (all_platforms
    ((action
      (progn
       (run ./configure --prefix=%{prefix} --docdir=%{doc}/ocaml)
       (run %{make} -j%{jobs}))))))
  
  (build_id f188145da1ab58087a839931602d6b59)

  $ cat ${default_lock_dir}/with-percent-sign.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run printf %d 42)))))
  
  (build_id 22c2fa0587dd36756bb6d1d6dbdcda1b)

  $ cat ${default_lock_dir}/variable-types.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{pkg-self:local_var})
       (run echo %{pkg-self:explicit_local_var})
       (run echo %{pkg:foo:package_var})
       (run echo %{os_family}))))))
  
  (build_id a3e3383fde978f6e5dd9132c1daa38c0)

  $ solve with-malformed-interpolation
  File "$TESTCASE_ROOT/mock-opam-repository/packages/with-malformed-interpolation/with-malformed-interpolation.0.0.1/opam", line 1, characters 0-0:
  Error: Encountered malformed variable interpolation while processing commands
  for package with-malformed-interpolation.0.0.1.
  The variable interpolation:
  %{prefix
  [1]

  $ solve --format=directory exercise-filters
  Solution for dune.lock (1 package):
  opam:
  - exercise-filters.0.0.1

  $ cat ${default_lock_dir}/exercise-filters.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (when %{pkg-self:foo} (run echo a))
       (when
        (and_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
        (run echo b))
       (when
        (and_absorb_undefined_var
         %{pkg-self:foo}
         %{pkg-self:bar}
         %{pkg-self:baz})
        (run echo c))
       (when
        (or_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
        (run echo d))
       (when
        (or_absorb_undefined_var
         %{pkg-self:foo}
         (and_absorb_undefined_var %{pkg-self:bar} %{pkg-self:baz}))
        (run echo e))
       (when
        (and_absorb_undefined_var
         (or_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
         %{pkg-self:baz})
        (run echo f))
       (when (= %{pkg-self:foo} %{pkg-self:bar}) (run echo b))
       (when (< %{pkg-self:version} 1.0) (run echo g))
       (run echo i)
       (run echo j)
       (when %{pkg:foo:installed} (run echo k))
       (when (< %{pkg:foo:version} 0.4) (run echo l))
       (when
        (and %{pkg:foo:installed} %{pkg:bar:installed} %{pkg:baz:installed})
        (run echo m)))))))
  
  (build_id 386160b9e006131d467bd64bcd9ad32b)



Test that if opam filter translation is disabled the output doesn't contain any translated filters:
  $ solve --format=directory exercise-filters
  Solution for dune.lock (1 package):
  opam:
  - exercise-filters.0.0.1
  $ cat ${default_lock_dir}/exercise-filters.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (when %{pkg-self:foo} (run echo a))
       (when
        (and_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
        (run echo b))
       (when
        (and_absorb_undefined_var
         %{pkg-self:foo}
         %{pkg-self:bar}
         %{pkg-self:baz})
        (run echo c))
       (when
        (or_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
        (run echo d))
       (when
        (or_absorb_undefined_var
         %{pkg-self:foo}
         (and_absorb_undefined_var %{pkg-self:bar} %{pkg-self:baz}))
        (run echo e))
       (when
        (and_absorb_undefined_var
         (or_absorb_undefined_var %{pkg-self:foo} %{pkg-self:bar})
         %{pkg-self:baz})
        (run echo f))
       (when (= %{pkg-self:foo} %{pkg-self:bar}) (run echo b))
       (when (< %{pkg-self:version} 1.0) (run echo g))
       (run echo i)
       (run echo j)
       (when %{pkg:foo:installed} (run echo k))
       (when (< %{pkg:foo:version} 0.4) (run echo l))
       (when
        (and %{pkg:foo:installed} %{pkg:bar:installed} %{pkg:baz:installed})
        (run echo m)))))))
  
  (build_id 386160b9e006131d467bd64bcd9ad32b)

  $ solve --format=directory exercise-term-filters
  Solution for dune.lock (1 package):
  opam:
  - exercise-term-filters.0.0.1
  $ cat ${default_lock_dir}/exercise-term-filters.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (run
       echo
       a
       (when (catch_undefined_var %{pkg-self:foo} false) b)
       (when
        (catch_undefined_var
         (and_absorb_undefined_var %{pkg-self:bar} %{pkg-self:baz})
         false)
        c))))))
  
  (build_id 2062a794bb674c206602689d759d96d8)

  $ solve filter-error-bool-where-string-expected
  File "$TESTCASE_ROOT/mock-opam-repository/packages/filter-error-bool-where-string-expected/filter-error-bool-where-string-expected.0.0.1/opam", line 3, characters 33-34:
  3 |   [ "echo" "a" ] { foo:version < (foo = bar) }
                                       ^
  Error: unable to parse opam file
  Parse error
  [1]

Package with package conjunction and string selections inside variable interpolations:
  $ mkpkg package-conjunction-and-string-selection <<EOF
  > build: [
  >   [ "echo" "a %{installed}% b" ]
  >   [ "echo" "c %{installed?x:y}% d" ]
  >   [ "echo" "e %{foo:installed?x:y}% f" ]
  >   [ "echo" "g %{foo+bar+_:installed?x:y}% h" ]
  >   # The "enable" variable is syntactic sugar around "installed" in some (but not all) cases.
  >   # Its intention appears to be for use with ./configure scripts that take --enable-<feature> or
  >   # --disable-<feature> as arguments.
  >   [ "echo" "--%{enable}%-feature" ]
  >   [ "echo" "--%{_:enable}%-feature" ]
  >   [ "echo" "--%{foo+bar:enable}%-feature" ]
  >   [ "echo" "--%{foo+bar:enable?x:y}%-feature" ]
  > ]
  > EOF

  $ solve_project --format=directory <<EOF
  > (lang dune 3.8)
  > (package (name x) (depends package-conjunction-and-string-selection))
  > EOF
  Solution for dune.lock (1 package):
  opam:
  - package-conjunction-and-string-selection.0.0.1
Note that "enable" is not a true opam variable. Opam desugars occurrences of
"pkg:enable" into "pkg:enable?enable:disable" but if the explicit package scope
is omitted then it's treated like a regular variable. That explains why the
opam syntax `"--%{enable}%-feature"` is converted to
`--%{pkg-self:enable}-feature`. Also if the "enable" pseudo-variable is used
with an explicit string conversion it is treated as a regular variable which
explains why the opam syntax `"--%{foo+bar:enable?x:y}%-feature"` is converted
to a dune if-statement that checks the "enable" variable (rather than the
"installed" variable as in other cases).

It's probably an error for opam packages to use the "enable" in ways that opam
doesn't desugar but these tests are included so we can check that behaviour is
preserved between opam and dune.
  $ cat ${default_lock_dir}/package-conjunction-and-string-selection.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo "a %{pkg-self:installed} b")
       (run
        echo
        (concat
         "c "
         (if (catch_undefined_var %{pkg-self:installed} false) x y)
         " d"))
       (run
        echo
        (concat
         "e "
         (if (catch_undefined_var %{pkg:foo:installed} false) x y)
         " f"))
       (run
        echo
        (concat
         "g "
         (if
          (catch_undefined_var
           (and %{pkg:foo:installed} %{pkg:bar:installed} %{pkg-self:installed})
           false)
          x
          y)
         " h"))
       (run echo --%{pkg-self:enable}-feature)
       (run
        echo
        (concat
         --
         (if (catch_undefined_var %{pkg-self:installed} false) enable disable)
         -feature))
       (run
        echo
        (concat
         --
         (if
          (catch_undefined_var
           (and %{pkg:foo:installed} %{pkg:bar:installed})
           false)
          enable
          disable)
         -feature))
       (run
        echo
        (concat
         --
         (if
          (catch_undefined_var (and %{pkg:foo:enable} %{pkg:bar:enable}) false)
          x
          y)
         -feature)))))))
  
  (build_id 600cc2a3c69680439d88ca9dac36dc77)
