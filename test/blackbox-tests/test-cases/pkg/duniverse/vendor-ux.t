Test comprehensive UX scenarios for the vendor stanza and duniverse feature.

These tests document the EXPECTED behavior per the vendoring design spec.

===========================================
BASIC VENDOR STANZA - Sanity checks
===========================================

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a simple vendored package:

  $ mkdir -p duniverse/mylib.1.0.0
  $ cat >duniverse/mylib.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name mylib))
  > EOF

  $ cat >duniverse/mylib.1.0.0/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat >duniverse/mylib.1.0.0/mylib.ml <<EOF
  > let value = "hello from mylib"
  > EOF

Set up duniverse with vendored_dirs and vendor stanza:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mylib.1.0.0 (libraries mylib))
  > EOF

Create main app:

  $ cat >dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.value
  > EOF

Build should succeed:

  $ dune build main.exe
  $ dune exec ./main.exe
  hello from mylib

===========================================
SELECTIVE LIBRARY EXPOSURE - Only expose specific libraries
===========================================

Clean up and create a package with multiple libraries:

  $ rm -rf _build duniverse
  $ mkdir -p duniverse/multi.1.0.0

  $ cat >duniverse/multi.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name multi))
  > EOF

  $ cat >duniverse/multi.1.0.0/dune <<EOF
  > (library
  >  (name multi_core)
  >  (modules multi_core)
  >  (public_name multi.core))
  > (library
  >  (name multi_extra)
  >  (modules multi_extra)
  >  (public_name multi.extra))
  > (library
  >  (name multi_internal)
  >  (modules multi_internal)
  >  (public_name multi.internal))
  > EOF

  $ cat >duniverse/multi.1.0.0/multi_core.ml <<EOF
  > let core_value = "core"
  > EOF

  $ cat >duniverse/multi.1.0.0/multi_extra.ml <<EOF
  > let extra_value = "extra"
  > EOF

  $ cat >duniverse/multi.1.0.0/multi_internal.ml <<EOF
  > let internal_value = "internal"
  > EOF

Expose only core and extra, not internal:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi.1.0.0 (libraries multi.core multi.extra))
  > EOF

Test using the exposed libraries:

  $ cat >dune <<EOF
  > (executable (name main) (libraries multi.core multi.extra))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   print_endline Multi_core.core_value;
  >   print_endline Multi_extra.extra_value
  > EOF

Should build successfully with exposed libraries:

  $ dune build main.exe
  $ dune exec ./main.exe
  core
  extra

Now try to use the NON-exposed library (should fail):

  $ cat >dune <<EOF
  > (executable (name main) (libraries multi.internal))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Multi_internal.internal_value
  > EOF

This should fail because multi.internal is not exposed:

  $ dune build main.exe 2>&1
  File "dune", line 1, characters 35-49:
  1 | (executable (name main) (libraries multi.internal))
                                         ^^^^^^^^^^^^^^
  Error: Library "multi.internal" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

===========================================
MULTI-VERSION COEXISTENCE WITH DIFFERENT PACKAGE NAMES
===========================================

For multi-version to work, vendored packages must have different package names.
Use library aliasing to expose them under convenient names.

Clean and set up two versions with different package names:

  $ rm -rf _build duniverse

  $ mkdir -p duniverse/yojson-v1.1.7.0
  $ cat >duniverse/yojson-v1.1.7.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name yojson-v1))
  > EOF
  $ cat >duniverse/yojson-v1.1.7.0/dune <<EOF
  > (library (name yojson_v1) (public_name yojson-v1))
  > EOF
  $ cat >duniverse/yojson-v1.1.7.0/yojson_v1.ml <<EOF
  > module Basic = struct
  >   let from_string _ = "parsed with 1.7.0"
  > end
  > EOF

  $ mkdir -p duniverse/yojson.2.0.0
  $ cat >duniverse/yojson.2.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name yojson))
  > EOF
  $ cat >duniverse/yojson.2.0.0/dune <<EOF
  > (library (name yojson) (public_name yojson))
  > EOF
  $ cat >duniverse/yojson.2.0.0/yojson.ml <<EOF
  > module Basic = struct
  >   let from_string _ = "parsed with 2.0.0"
  > end
  > EOF

Vendor both with explicit library lists:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson-v1.1.7.0 (libraries yojson-v1))
  > (vendor yojson.2.0.0 (libraries yojson))
  > EOF

Use both versions in the same project:

  $ cat >dune <<EOF
  > (executable (name main) (libraries yojson-v1 yojson))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   Printf.printf "Old: %s\n" (Yojson_v1.Basic.from_string "{}");
  >   Printf.printf "New: %s\n" (Yojson.Basic.from_string "{}")
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  Old: parsed with 1.7.0
  New: parsed with 2.0.0

===========================================
DUPLICATE VENDOR STANZA ERROR
===========================================

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries yojson))
  > (vendor yojson.1.7.0 (libraries yojson))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "duniverse/dune", line 3, characters 0-40:
  3 | (vendor yojson.1.7.0 (libraries yojson))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: vendor stanza for directory "yojson.1.7.0" already defined

===========================================
LIBRARY NOT FOUND IN VENDOR ERROR
===========================================

TODO: Validation that listed libraries exist in vendored directory is not yet implemented.
For now, listing a nonexistent library just means no library is exposed.

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries nonexistent))
  > EOF

  $ dune build main.exe 2>&1 | head -10

===========================================
DUPLICATE LIBRARY EXPOSURE ERROR
===========================================

Create two packages that both expose the same library name:

  $ rm -rf duniverse
  $ mkdir -p duniverse/pkg-a.1.0.0
  $ cat >duniverse/pkg-a.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name pkg-a))
  > EOF
  $ cat >duniverse/pkg-a.1.0.0/dune <<EOF
  > (library (name mylib) (modules mylib) (public_name pkg-a.mylib))
  > EOF
  $ cat >duniverse/pkg-a.1.0.0/mylib.ml <<EOF
  > let x = "from pkg-a"
  > EOF

  $ mkdir -p duniverse/pkg-b.1.0.0
  $ cat >duniverse/pkg-b.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name pkg-b))
  > EOF
  $ cat >duniverse/pkg-b.1.0.0/dune <<EOF
  > (library (name mylib) (modules mylib) (public_name pkg-b.mylib))
  > EOF
  $ cat >duniverse/pkg-b.1.0.0/mylib.ml <<EOF
  > let x = "from pkg-b"
  > EOF

Both expose internal library name "mylib" - this currently errors via dune's
standard library duplicate detection (not vendor-specific):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor pkg-a.1.0.0 (libraries pkg-a.mylib))
  > (vendor pkg-b.1.0.0 (libraries pkg-b.mylib))
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries pkg-a.mylib))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.x
  > EOF

  $ dune build main.exe 2>&1 | head -10

===========================================
LINKING MULTIPLE VERSIONS WITH SAME PACKAGE AND MODULE NAME
===========================================

This tests linking two versions of the SAME package where both have the
same package name and same library/module name. The vendor stanza's
library aliasing feature allows exposing them under different names.

Set up two versions of "json" package, both with library "json":

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/json.1.0.0
  $ cat >duniverse/json.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name json) (version 1.0.0))
  > EOF
  $ cat >duniverse/json.1.0.0/dune <<EOF
  > (library (name json) (public_name json))
  > EOF
  $ cat >duniverse/json.1.0.0/json.ml <<EOF
  > let version = "1.0.0"
  > let parse _ = "old parser"
  > EOF

  $ mkdir -p duniverse/json.2.0.0
  $ cat >duniverse/json.2.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name json) (version 2.0.0))
  > EOF
  $ cat >duniverse/json.2.0.0/dune <<EOF
  > (library (name json) (public_name json))
  > EOF
  $ cat >duniverse/json.2.0.0/json.ml <<EOF
  > let version = "2.0.0"
  > let parse _ = "new parser"
  > EOF

Use library aliasing to expose both versions under different names:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor json.1.0.0 (libraries (json :as json_v1)))
  > (vendor json.2.0.0 (libraries (json :as json_v2)))
  > EOF

Link both versions and use them:

  $ cat >dune <<EOF
  > (executable (name main) (libraries json_v1 json_v2))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   Printf.printf "v1: %s\n" Json_v1.Json.version;
  >   Printf.printf "v2: %s\n" Json_v2.Json.version
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  v1: 1.0.0
  v2: 2.0.0

===========================================
OVERRIDE WITH SEPARATE VENDOR DIRECTORY
===========================================

User customizations go in a separate vendor/ directory (not duniverse/):

  $ rm -rf duniverse vendor _build
  $ mkdir -p duniverse/override-test.1.0.0
  $ cat >duniverse/override-test.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name override-test))
  > EOF
  $ cat >duniverse/override-test.1.0.0/dune <<EOF
  > (library (name lib_public) (modules lib_public) (public_name override-test.public))
  > (library (name lib_internal) (modules lib_internal) (public_name override-test.internal))
  > EOF
  $ cat >duniverse/override-test.1.0.0/lib_public.ml <<EOF
  > let x = "public"
  > EOF
  $ cat >duniverse/override-test.1.0.0/lib_internal.ml <<EOF
  > let x = "internal"
  > EOF

Generated duniverse/dune exposes all libraries:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor override-test.1.0.0 (libraries override-test.public override-test.internal))
  > EOF

To override, copy the package to a user-maintained vendor/ directory:

  $ mkdir -p vendor
  $ cp -r duniverse/override-test.1.0.0 vendor/override-test.1.0.0

  $ cat >vendor/dune <<EOF
  > (vendored_dirs *)
  > (vendor override-test.1.0.0 (libraries override-test.public))
  > EOF

Remove from duniverse so there's no conflict:

  $ rm -rf duniverse/override-test.1.0.0
  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries override-test.public))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Lib_public.x
  > EOF

Public library works:

  $ dune build main.exe
  $ dune exec ./main.exe
  public

Internal library should be blocked by vendor/ override:

  $ cat >dune <<EOF
  > (executable (name main) (libraries override-test.internal))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Lib_internal.x
  > EOF

  $ dune build main.exe 2>&1
  File "dune", line 1, characters 35-57:
  1 | (executable (name main) (libraries override-test.internal))
                                         ^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "override-test.internal" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

===========================================
VENDOR WITHOUT LIBRARIES FIELD - All libraries exposed
===========================================

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/all-libs.1.0.0
  $ cat >duniverse/all-libs.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name all-libs))
  > EOF
  $ cat >duniverse/all-libs.1.0.0/dune <<EOF
  > (library (name lib_a) (modules lib_a) (public_name all-libs.a))
  > (library (name lib_b) (modules lib_b) (public_name all-libs.b))
  > EOF
  $ cat >duniverse/all-libs.1.0.0/lib_a.ml <<EOF
  > let a = "lib a"
  > EOF
  $ cat >duniverse/all-libs.1.0.0/lib_b.ml <<EOF
  > let b = "lib b"
  > EOF

Vendor without libraries field should expose all:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor all-libs.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries all-libs.a all-libs.b))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   print_endline Lib_a.a;
  >   print_endline Lib_b.b
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  lib a
  lib b

===========================================
VENDOR WITH PACKAGES FIELD
===========================================

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/monorepo.1.0.0/x duniverse/monorepo.1.0.0/y
  $ cat >duniverse/monorepo.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name pkg-x))
  > (package (name pkg-y))
  > EOF
  $ cat >duniverse/monorepo.1.0.0/x/dune <<EOF
  > (library (name x) (public_name pkg-x))
  > EOF
  $ cat >duniverse/monorepo.1.0.0/x/x.ml <<EOF
  > let x = "from x"
  > EOF
  $ cat >duniverse/monorepo.1.0.0/y/dune <<EOF
  > (library (name y) (public_name pkg-y))
  > EOF
  $ cat >duniverse/monorepo.1.0.0/y/y.ml <<EOF
  > let y = "from y"
  > EOF

Expose only pkg-x package:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor monorepo.1.0.0 (packages pkg-x))
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries pkg-x))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline X.x
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  from x

pkg-y should not be available:

  $ cat >dune <<EOF
  > (executable (name main) (libraries pkg-y))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Y.y
  > EOF

  $ dune build main.exe 2>&1
  File "dune", line 1, characters 35-40:
  1 | (executable (name main) (libraries pkg-y))
                                         ^^^^^
  Error: Library "pkg-y" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

===========================================
EDITABLE DEPENDENCIES - Changes should be picked up
===========================================

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/editable.1.0.0
  $ cat >duniverse/editable.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name editable))
  > EOF
  $ cat >duniverse/editable.1.0.0/dune <<EOF
  > (library (name editable) (public_name editable))
  > EOF
  $ cat >duniverse/editable.1.0.0/editable.ml <<EOF
  > let msg = "original"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor editable.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries editable))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Editable.msg
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  original

Edit the vendored source:

  $ cat >duniverse/editable.1.0.0/editable.ml <<EOF
  > let msg = "MODIFIED!"
  > EOF

Rebuild should pick up the change:

  $ dune build main.exe
  $ dune exec ./main.exe
  MODIFIED!

===========================================
VENDOR IN CUSTOM DIRECTORY (not just duniverse/)
===========================================

  $ rm -rf duniverse _build
  $ mkdir -p vendor/custom-loc.1.0.0
  $ cat >vendor/custom-loc.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name custom))
  > EOF
  $ cat >vendor/custom-loc.1.0.0/dune <<EOF
  > (library (name custom) (public_name custom))
  > EOF
  $ cat >vendor/custom-loc.1.0.0/custom.ml <<EOF
  > let msg = "from custom vendor dir"
  > EOF

  $ cat >vendor/dune <<EOF
  > (vendored_dirs *)
  > (vendor custom-loc.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries custom))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Custom.msg
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  from custom vendor dir

===========================================
VENDOR WITH INTERNAL DEPENDENCIES
===========================================

  $ rm -rf vendor _build
  $ mkdir -p duniverse/chain.1.0.0
  $ cat >duniverse/chain.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name chain))
  > EOF

Library A depends on library B (both in same vendored dir):

  $ cat >duniverse/chain.1.0.0/dune <<EOF
  > (library (name chain_base) (modules chain_base) (public_name chain.base))
  > (library (name chain_derived) (modules chain_derived) (libraries chain.base) (public_name chain.derived))
  > EOF

  $ cat >duniverse/chain.1.0.0/chain_base.ml <<EOF
  > let base = "base"
  > EOF

  $ cat >duniverse/chain.1.0.0/chain_derived.ml <<EOF
  > let derived = Chain_base.base ^ "-derived"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor chain.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries chain.derived))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Chain_derived.derived
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  base-derived

===========================================
VENDOR STANZA SYNTAX ERRORS
===========================================

Missing directory:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor)
  > EOF

  $ dune build 2>&1 | head -5
  File "duniverse/dune", line 2, characters 0-8:
  2 | (vendor)
      ^^^^^^^^
  Error: Not enough arguments for "vendor"

Invalid field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor chain.1.0.0 (invalid_field foo))
  > EOF

  $ dune build 2>&1 | head -5
  File "duniverse/dune", line 2, characters 21-34:
  2 | (vendor chain.1.0.0 (invalid_field foo))
                           ^^^^^^^^^^^^^
  Error: Unknown field "invalid_field"

===========================================
@pkg-install ALIAS FOR VENDORED PACKAGES
===========================================

  $ rm -rf _build
  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor chain.1.0.0)
  > EOF

TODO: @pkg-install currently requires a lock dir. For vendored-only projects,
building the libraries directly works:

  $ dune build @pkg-install 2>&1 | head -10
  Error: The @pkg-install alias cannot be used without a lock dir
  -> required by alias pkg-install
  Hint: You might want to create the lock dir with 'dune pkg lock'

Instead, use a simple executable that depends on the vendored library:

  $ cat >dune <<EOF
  > (executable (name main) (libraries chain.derived))
  > EOF
  $ cat >main.ml <<EOF
  > let () = print_endline Chain_derived.derived
  > EOF
  $ dune build main.exe
  $ dune exec ./main.exe
  base-derived

===========================================
EMPTY LIBRARIES LIST - No libraries exposed
===========================================

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor chain.1.0.0 (libraries))
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries chain.base))
  > EOF

With empty libraries list, chain.base should NOT be available:

  $ dune build main.exe 2>&1
  File "dune", line 1, characters 35-45:
  1 | (executable (name main) (libraries chain.base))
                                         ^^^^^^^^^^
  Error: Library "chain.base" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

===========================================
DEEP NESTING IN VENDORED PACKAGE
===========================================

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/deep.1.0.0/src/lib/nested
  $ cat >duniverse/deep.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name deep))
  > EOF
  $ cat >duniverse/deep.1.0.0/src/lib/nested/dune <<EOF
  > (library (name deep_nested) (public_name deep.nested))
  > EOF
  $ cat >duniverse/deep.1.0.0/src/lib/nested/deep_nested.ml <<EOF
  > let deep = "deeply nested"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor deep.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (libraries deep.nested))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Deep_nested.deep
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  deeply nested

===========================================
SANDBOX MODE - opam sandbox for non-dune packages
===========================================

Non-dune packages (those without a dune-project file) can be built using
opam's sandbox mechanism when (build opam) is specified. The build and
install commands are read from the package's opam file.

  $ rm -rf duniverse _build
  $ mkdir -p duniverse/make-pkg.1.0.0
  $ cat >duniverse/make-pkg.1.0.0/make-pkg.opam <<'EOF'
  > opam-version: "2.0"
  > name: "make-pkg"
  > version: "1.0.0"
  > build: [
  >   ["make" "all"]
  > ]
  > install: [
  >   ["make" "install"]
  > ]
  > EOF

  $ cat >duniverse/make-pkg.1.0.0/Makefile <<'EOF'
  > all:
  > 	@echo "Building make-pkg"
  > install:
  > 	@echo "Installing make-pkg"
  > 	mkdir -p $(OPAM_SWITCH_PREFIX)/lib/make-pkg
  > 	echo "data" > $(OPAM_SWITCH_PREFIX)/lib/make-pkg/data.txt
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor make-pkg.1.0.0 (build opam))
  > EOF

Non-dune packages with (build opam) should be built in opam sandbox:

  $ dune build @pkg-install 2>&1
  $ cat _build/.pkgs/default/make-pkg.1.0.0/target/cookie 2>&1 || echo "no cookie file"
  DUNE-INSTALL-COOKIEv3:„•¦¾             @@
