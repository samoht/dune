# RFC: Opam Build Mode

## Summary

The `(mode opam)` build mode enables dune to build non-dune packages using their
opam build instructions. This provides full compatibility with opam's build semantics.

## Background

Many OCaml packages don't use dune as their build system. They use:
- `./configure && make`
- topkg
- oasis
- Custom build scripts

To vendor or lock these packages, dune needs to understand opam's build instructions.

## When Opam Mode is Used

Opam mode is used when:
1. `(vendor dir (mode opam))` is explicitly specified
2. A vendored directory contains an opam file but no dune files (auto-detected)
3. A locked package has non-dune build commands

## Opam Compatibility

### Variable Expansion

All opam variables are supported:

**Global variables:**
- `%{make}%`, `%{jobs}%` - build tools and parallelism
- `%{arch}%`, `%{os}%`, `%{os-family}%`, `%{os-distribution}%`, `%{os-version}%` - platform

**Directory variables:**
- `%{prefix}%`, `%{lib}%`, `%{bin}%`, `%{share}%`, `%{etc}%`, `%{doc}%`, `%{man}%`, `%{stublibs}%`

**Package variables:**
- `%{name}%`, `%{version}%`, `%{build}%`, `%{build-id}%`

**Cross-package variables:**
- `%{pkg:var}%`, `%{pkg:installed}%`, `%{pkg:enable}%`

### Filters

Conditional commands are evaluated based on platform:

```
[make] {os = "linux"}
[nmake] {os = "win32"}
["./configure" "--prefix=%{prefix}%"] {os != "win32"}
```

### Substs

Files listed in `substs:` are processed. For example, `config.ml.in` becomes
`config.ml` with variables expanded.

### Patches

Patches listed in `patches:` are applied, including conditional patches:

```
patches: [
  "fix.patch"
  "linux-only.patch" {os = "linux"}
]
```

### Environment

- `build-env:` sets environment variables for build commands
- `setenv:` exports variables to dependent packages

## Build Location

Opam-mode packages are built in `_build/.pkgs/<context>/<name>/`:

```
_build/.pkgs/default/zarith/
  source/     # Linked/copied from duniverse/ or fetched
  target/     # Build artifacts
```

When `(install false)`, the build-id is included in the path to allow multiple
versions: `_build/.pkgs/<context>/<name>-<build-id>/`

## Build-id

The `%{build-id}%` variable is a content-addressable hash for deterministic caching.
For locked packages, it's computed as a Merkle tree over dependencies:

```
build_id(pkg) = hash(
  pkg_content_hash,       # hash of package definition
  sorted(deps_build_ids), # build_ids of all dependencies
  platforms_hash          # platforms the package is enabled on
)
```

For manually vendored packages (no lock file), build-id is computed from:
- Hash of the opam file contents
- Hash of source directory contents
- Build-ids of resolved dependencies (same Merkle tree approach)

## Cross-Compilation

When building for cross-compilation targets, dune sets up:
- `OCAMLFIND_TOOLCHAIN=<target>`
- Appropriate sysroot paths
- Cross-compiler variables

This enables non-dune packages to "just work" with cross-compilation.

## Installation

Artifacts are installed to the shared prefix `_build/install/<context>/`:
- Binaries to `bin/`
- Libraries to `lib/<name>/`
- Stubs to `lib/stublibs/`

Dune sets `PATH`, `OCAMLPATH`, and `CAML_LD_LIBRARY_PATH` so dependent packages
can find installed artifacts.

## References

- [RFC: Extended Vendor Stanza](vendor.md)
- [RFC: Lock Files](lock.md)
- [dune#8652](https://github.com/ocaml/dune/issues/8652) - Package management: build non-dune packages
