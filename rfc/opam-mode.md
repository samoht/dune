# RFC: Opam Build Mode

## Overview

The `(mode opam)` build mode enables dune to build non-dune packages using their
opam build instructions.

Many OCaml packages use `./configure && make`, topkg, oasis, or custom scripts.
Dune runs these build commands in a sandboxed environment with opam variables
expanded.

## When Used

1. `(vendor dir (mode opam))` explicitly specified
2. Vendored directory has opam file but no dune files (auto-detected)
3. Locked package has non-dune build commands

## Variable Expansion

All opam variables are supported:

| Category | Variables |
|----------|-----------|
| Global | `%{make}%`, `%{jobs}%`, `%{arch}%`, `%{os}%`, `%{os-family}%`, `%{os-distribution}%`, `%{os-version}%` |
| Directories | `%{prefix}%`, `%{lib}%`, `%{bin}%`, `%{share}%`, `%{etc}%`, `%{doc}%`, `%{man}%`, `%{stublibs}%` |
| Package | `%{name}%`, `%{version}%`, `%{build}%`, `%{build-id}%` |
| Cross-package | `%{pkg:var}%`, `%{pkg:installed}%`, `%{pkg:enable}%` |

## Filters

Conditional commands are evaluated based on platform:

```
build: [
  [make] {os = "linux"}
  [nmake] {os = "win32"}
  ["./configure" "--prefix=%{prefix}%"] {os != "win32"}
]
```

## Substs and Patches

- `substs:` files are processed (`config.ml.in` → `config.ml` with variables expanded)
- `patches:` are applied, including conditional patches

```
patches: [
  "fix.patch"
  "linux-only.patch" {os = "linux"}
]
```

## Environment

- `build-env:` sets environment for build commands
- `setenv:` exports variables to dependent packages

## Build Location

```
_build/.pkgs/<context>/<name>/
  source/     # Linked from duniverse/ or fetched to _build/.pkgs/
  target/     # Build artifacts
```

With `(install false)`, the build-id is included:
`_build/.pkgs/<context>/<name>-<build-id>/`

## Build-id

Content-addressable hash for deterministic caching. Same mechanism for locked
and vendored packages:

```
build_id(pkg) = hash(
  opam_file_hash,         # includes url checksum for released packages
  sorted(deps_build_ids), # Merkle tree over dependencies
  platforms_hash
)
```

For vendored packages without an opam file url field, dune computes a checksum
from the source directory contents.

## Installation

Artifacts install to `_build/install/<context>/`:
- Binaries → `bin/`
- Libraries → `lib/<name>/`
- Stubs → `lib/stublibs/`

Dune sets `PATH`, `OCAMLPATH`, `CAML_LD_LIBRARY_PATH` for dependent packages.

## Cross-Compilation

Dune sets up:
- `OCAMLFIND_TOOLCHAIN=<target>`
- Sysroot paths
- Cross-compiler variables

Non-dune packages work with cross-compilation without modification.

## References

- [RFC: Extended Vendor Stanza](vendor.md)
- [RFC: Lock Files](lock.md)
- [dune#8652](https://github.com/ocaml/dune/issues/8652)
