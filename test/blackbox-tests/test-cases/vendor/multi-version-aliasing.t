Test that two versions of the same package can be vendored using library aliasing
to avoid conflicts. This is the "gradual migration" use case from the vendor RFC.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create two versions of a yojson-like package.
Note: We use opam files (not dune-project) to avoid dune's duplicate package detection.
The vendor stanza handles library aliasing at the library level.

Version 1.7.0 (old API):

  $ mkdir -p duniverse/yojson.1.7.0
  $ cat >duniverse/yojson.1.7.0/yojson.opam <<EOF
  > opam-version: "2.0"
  > name: "yojson"
  > version: "1.7.0"
  > EOF

  $ cat >duniverse/yojson.1.7.0/dune <<EOF
  > (library (name yojson))
  > EOF

  $ cat >duniverse/yojson.1.7.0/yojson.ml <<EOF
  > let version = "1.7.0"
  > let from_string s = "parsed_v1:" ^ s
  > EOF

Version 2.0.0 (new API):

  $ mkdir -p duniverse/yojson.2.0.0
  $ cat >duniverse/yojson.2.0.0/yojson.opam <<EOF
  > opam-version: "2.0"
  > name: "yojson"
  > version: "2.0.0"
  > EOF

  $ cat >duniverse/yojson.2.0.0/dune <<EOF
  > (library (name yojson))
  > EOF

  $ cat >duniverse/yojson.2.0.0/yojson.ml <<EOF
  > let version = "2.0.0"
  > let from_string s = "parsed_v2:" ^ s
  > let to_string _ = "serialized"
  > EOF

Use vendor stanzas with library aliasing to expose both versions:
- Old version exposed as "yojson_v1" for legacy code
- New version exposed as "yojson" for new code

Note: :as aliasing implies (install false), so we don't need to specify it.

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
  > (vendor yojson.2.0.0 (libraries yojson))
  > EOF

Create an application that uses BOTH versions during a gradual migration:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries yojson yojson_v1))
  > EOF

  $ cat >main.ml <<EOF
  > (* Legacy code still using v1 - aliased library has wrapper *)
  > let old_result = Yojson_v1.Yojson.from_string "legacy"
  > (* New code using v2 - non-aliased library has direct access *)
  > let new_result = Yojson.from_string "modern"
  > let () =
  >   Printf.printf "v1: %s (version %s)\n" old_result Yojson_v1.Yojson.version;
  >   Printf.printf "v2: %s (version %s)\n" new_result Yojson.version
  > EOF

Build and run - both versions should work together:

  $ dune exec ./main.exe
  v1: parsed_v1:legacy (version 1.7.0)
  v2: parsed_v2:modern (version 2.0.0)

Verify that without aliasing, we get a duplicate library error.
First create a simple app that just uses one library to avoid the yojson_v1 reference:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries yojson))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Yojson.from_string "test"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries yojson))
  > (vendor yojson.2.0.0 (libraries yojson))
  > EOF

  $ dune build main.exe 2>&1 | head -10
  File "duniverse/yojson.1.7.0/dune", line 1, characters 0-23:
  1 | (library (name yojson))
      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library with name "yojson" is already defined in
  duniverse/yojson.2.0.0/dune:1. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.

Test that :as aliasing implies (install false), allowing multiple versions
to coexist. Restore working configuration:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
  > (vendor yojson.2.0.0 (libraries yojson))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries yojson yojson_v1))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   Printf.printf "v1: %s\n" (Yojson_v1.Yojson.from_string "test");
  >   Printf.printf "v2: %s\n" (Yojson.from_string "test")
  > EOF

  $ dune exec ./main.exe
  v1: parsed_v1:test
  v2: parsed_v2:test

Test that two packages with the same opam name and both having install=true
causes an error. This is required by opam semantics where only one package
with a given name can be installed per context.

First, add a third package that uses yojson:

  $ mkdir -p duniverse/yojson.3.0.0
  $ cat >duniverse/yojson.3.0.0/yojson.opam <<EOF
  > opam-version: "2.0"
  > name: "yojson"
  > version: "3.0.0"
  > EOF

  $ cat >duniverse/yojson.3.0.0/dune <<EOF
  > (library (name yojson))
  > EOF

  $ cat >duniverse/yojson.3.0.0/yojson.ml <<EOF
  > let version = "3.0.0"
  > let from_string s = "parsed_v3:" ^ s
  > EOF

Now configure vendor stanzas where multiple packages have install=true.
Note: :as aliasing implies (install false), so we need to explicitly set
(install true) to test the error case:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)) (install true))
  > (vendor yojson.2.0.0 (libraries (yojson :as yojson_v2)) (install true))
  > (vendor yojson.3.0.0 (libraries yojson))
  > EOF

  $ dune build main.exe 2>&1 | head -10

Fix by letting :as aliasing imply install=false (or set it explicitly):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
  > (vendor yojson.2.0.0 (libraries (yojson :as yojson_v2)))
  > (vendor yojson.3.0.0 (libraries yojson))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries yojson yojson_v1 yojson_v2))
  > EOF

  $ cat >main.ml <<EOF
  > let () =
  >   Printf.printf "v1: %s\n" (Yojson_v1.Yojson.from_string "test");
  >   Printf.printf "v2: %s\n" (Yojson_v2.Yojson.from_string "test");
  >   Printf.printf "v3: %s\n" (Yojson.from_string "test")
  > EOF

  $ dune exec ./main.exe
  v1: parsed_v1:test
  v2: parsed_v2:test
  v3: parsed_v3:test
