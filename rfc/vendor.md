# [RFC] Extended Vendor Stanza

## Summary

Add a `(vendor ...)` stanza that extends `(vendored_dirs)` with selective library
exposure, library aliasing, and leverages existing package management infrastructure
to support non-dune packages.

## Workflow

The workflow is simple: copy source code into a directory, add a vendor stanza,
and run `dune build`.

```
my-project/
  src/
  vendor/
    fmt.0.9.0/       # copied from somewhere
    yojson.2.0.0/    # copied from somewhere else
    dune             # contains (vendor ...) stanzas
  dune-project
```

The `(vendor ...)` stanza goes in a `dune` file (like the existing `(vendored_dirs)`).
Dune doesn't care where the source came from or how it got there. It just builds
what's in the directory.

## Design Principles

### The Vendoring Model

Vendored code is external source you import into your workspace to:

1. Modify: fix bugs, add features, adapt to your needs
2. Upstream: eventually contribute those changes back
3. Rebase: easily reapply your patches when upstream releases a new version

This model requires that build system configuration (library visibility, naming) stays
outside the vendored sources. Otherwise your patches are more difficult to upstream,
and rebasing becomes tedious.

### Why These Features Need Dune Support

**Library aliasing** cannot be accomplished by external tools without modifying vendored
`dune` files. To rename `yojson` to `yojson_v1`, you would need to edit the library's
`(public_name ...)` field, which breaks updates.

**Selective library exposure** could theoretically be done by deleting unwanted source
files, but this is destructive and error-prone. Dune can simply ignore libraries at
the dependency resolution level without touching sources.

**Non-dune package building** leverages existing package management infrastructure
([dune#8652](https://github.com/ocaml/dune/issues/8652)). The `(mode opam)` functionality
is not new—it extends what dune already does for locked packages to manually vendored
sources.

### Composable Features

Each capability (aliasing, filtering, opam build mode) is independent. Users can adopt
only what they need. The `(vendor ...)` stanza bundles these for convenience, but they
solve orthogonal problems.

## Background

### Current State

Dune supports vendoring via `(vendored_dirs <dirs>)`. This marks directories as
vendored (disables warnings-as-errors) and includes them in the build.

However, `(vendored_dirs)` exposes all libraries in a directory. There is no way to:
1. Expose only a subset of libraries
2. Vendor multiple versions of the same library
3. Build non-dune packages

### Prior Art: opam-monorepo

[opam-monorepo](https://github.com/tarides/opam-monorepo) established a vendoring
workflow for OCaml, fetching dependencies into a `duniverse/` directory. It has
known limitations:

- [Issue #145](https://github.com/tarides/opam-monorepo/issues/145): Sources from
  the same git repository but with different versions cause invalid solutions.

- All sources must build with dune. Non-dune sources require
  [opam-overlays](https://github.com/dune-universe/opam-overlays).

### The Problem

**Multiple versions conflict:**
```
vendor/
  yojson.1.7.0/    # contains library "yojson"
  yojson.2.0.0/    # contains library "yojson" → conflict
```

**Monorepo version skew:**
```
vendor/
  core.0.16.0/    # contains core, core_kernel, core_unix, etc.
```

Libraries from a monorepo may have different release schedules. You might need
`core` from one commit and `core_unix` from another. With `(vendored_dirs)`,
you cannot pick libraries from different directories.

**Non-dune source:**
```
vendor/
  zarith.1.14/   # uses ./configure && make
```

## Proposed Solution

### Syntax

```dune
(vendor <directory>
 (package <name>)             ; optional: inferred from opam file
 (version <version>)          ; optional: inferred from opam file
 (libraries <lib-spec>...)    ; optional: inferred by scanning directory
 (mode <build-mode>)          ; optional: dune | opam (inferred from contents)
 (install <bool>)             ; optional: promote artifacts to shared install path
 (toolchain <name>))          ; optional: marks this as providing a compiler/toolchain
```

Both `<lib-spec>` and `<pkg-spec>` use the standard ordered set language (like `(modules ...)`) with:
- `:standard` — all libraries/packages found in the directory
- `<name>` — expose with its original name
- `(<name> :as <alias>)` — expose under a different name

All fields are optional. When omitted:
- `(package ...)` and `(version ...)` are inferred from the opam file in the directory
- `(libraries ...)` is inferred by scanning the directory
- `(mode ...)` is inferred: `dune` if dune files exist, `opam` otherwise
- `(install ...)` defaults to `true`, except when `:as` aliasing is used (then `false`)
- `(toolchain ...)` defaults to none

Explicit fields override inferred values.

### Examples

**Expose all libraries (equivalent to vendored_dirs):**
```dune
(vendor vendor/fmt.0.9.0)
```

**Selective exposure:**
```dune
(vendor vendor/fmt.0.9.0
 (libraries fmt fmt.tty))
```

**Multi-version with aliasing:**
```dune
(vendor vendor/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

(vendor vendor/yojson.2.0.0
 (libraries yojson))
```

**Non-dune source:**
```dune
(vendor vendor/zarith.1.14)
```
Dune detects the opam file and runs its build commands.

**Force opam mode:**
```dune
(vendor vendor/foo.1.0.0
 (mode opam))
```

**Native compiler:**
```dune
(vendor vendor/ocaml.5.2.0
 (toolchain native))
```
Provides the native OCaml compiler for the default context.

**Cross-compilation toolchain:**
```dune
(vendor vendor/ocaml-windows.5.2.0
 (toolchain windows))
```
Provides the `windows` toolchain for `(targets native windows)`.

### Semantics

**Library filtering:**
1. Scan directory for libraries (existing behavior)
2. If `(libraries ...)` specified, expose only those listed
3. Unlisted libraries should not be built and cannot be used as dependencies

**Library aliasing:**
- `(<name> :as <alias>)` should change the public name used in `(libraries ...)` stanzas
- OCaml module names inside the library should remain unchanged

**Install promotion:**
- When `(install true)` (default), artifacts are promoted to `_build/install/<context>/`
- When `(install false)`, artifacts remain in the vendor build directory only (but are
  still available within the workspace)
- Using `:as` aliasing implies `(install false)` — aliased names are workspace-local and
  external packages cannot reference them

Opam semantics require that only one package with a given name can be installed per context.
This only becomes an issue when a vendored opam package depends on a package that has
multiple versions in the workspace. In this case, dune should error if two packages with
the same name both have `(install true)` in the same context, with a message suggesting
to set `(install false)` on one of them.

**Dependency resolution:**

Dune resolves dependencies at the library level, not the package level. When anything
in the workspace needs library "foo" — a dune library, executable, or vendored package —
dune looks for "foo" in:
1. Vendored dune packages in the workspace
2. Vendored opam packages (installed to shared prefix)
3. Locked packages from dune.lock
4. System OCAMLPATH (for libraries with C bindings)

This is the same resolution mechanism for all consumers. Dune sets up OCAMLPATH and
other findlib variables so that build commands can discover dependencies via ocamlfind.

Importantly, dune does not require a closure of opam packages. If a vendored opam
package's `depends:` field lists package "foo", but the required libraries are available
from a different source (e.g., a dune package), that works. Errors occur at build time
if ocamlfind cannot locate a required library.

There is no automatic library-to-package resolution. If a missing library requires an
opam package, the user must explicitly add it to `dune-project` so it appears in the
lock file.

**Toolchain declaration:**
- `(toolchain <name>)` marks the package as providing a compiler or cross-compilation toolchain
- `(toolchain native)` provides the native OCaml compiler (`ocamlc`, `ocamlopt`, etc.)
- Other toolchain names (e.g., `windows`) provide cross-compilers for `(targets ...)`
- Dune sets `OCAMLFIND_TOOLCHAIN=<name>` for the target context
- Dune builds toolchain packages first before any code that depends on them

**Build ordering:**

No solver, no version constraints. Dependencies must be in the workspace or ambient
system (PATH, OCAMLPATH, and related environment variables).

When a target needs a library from an opam-mode package:
1. Parse the package's opam `depends:` field, evaluating filters (`{os = "linux"}`, etc.)
2. For each dependency package name, look for a package with that name in workspace
   (vendored, locked, or `(package ...)` stanza). If found, build it first.
   If not found, assume the ambient system provides it.
3. Build the opam package with OCAMLPATH = workspace libraries + ambient system
4. If ocamlfind can't find a library, error: "Library X not found (required by package Y)"

Example: vendor fmt (a topkg package) to patch it:
```
my-project/
  src/
    cmdliner.ml
    dune              # (library (name cmdliner) (public_name cmdliner))
  vendor/
    fmt/              # cloned from git, with local patches
      opam            # depends: ["ocaml" "ocamlfind" "cmdliner" ...]
    dune              # (vendor fmt)
  dune-project        # (package (name cmdliner))
```

When building something that uses fmt:
1. Parse fmt's `depends:` → `ocaml`, `ocamlfind`, `cmdliner`
2. `ocaml`, `ocamlfind` → no package in workspace → assume system
3. `cmdliner` → package defined in dune-project → build that package first
4. Build fmt with OCAMLPATH pointing to workspace packages + system
5. If fmt's build can't find a library → clear error

No auto-fetching, no solving — dune builds what's in the workspace.

**Error handling:**
- If `(libraries foo)` lists a library not found in the directory → error at library resolution time
- If two `(vendor)` stanzas expose the same library name → error at library resolution time
- If `(mode dune)` but directory has no dune files → error (library not found)
- If `(mode opam)` but directory has no opam file → error with clear message
- If `(<name> :as <alias>)` has invalid syntax → error at parse time
- If `(<name> :as <alias>)` where alias conflicts with another library → error at library resolution time
- Windows paths and paths with spaces should be supported

**Build mode:**

| Directory Contents | Default Mode |
|--------------------|--------------|
| Contains `dune-project` or `dune` files | `dune` |
| Contains only `*.opam` file | `opam` |

In `dune` mode, the vendored directory is built like normal dune code, with library
filtering and aliasing applied. This is equivalent to `(vendored_dirs)` plus the
ability to hide or rename libraries.

In `opam` mode, dune should provide full compatibility with opam's build semantics:

**Variable expansion:**
All opam variables should be supported, including:
- Global: `%{make}%`, `%{jobs}%`, `%{arch}%`, `%{os}%`, `%{os-family}%`, `%{os-distribution}%`, `%{os-version}%`
- Directories: `%{prefix}%`, `%{lib}%`, `%{bin}%`, `%{share}%`, `%{etc}%`, `%{doc}%`, `%{man}%`, `%{stublibs}%`
- Package: `%{name}%`, `%{version}%`, `%{build}%`, `%{build-id}%`
- Cross-package: `%{pkg:var}%`, `%{pkg:installed}%`, `%{pkg:enable}%`

**Filters:**
Conditional commands should be evaluated: `[make] {os = "linux"}`, `[nmake] {os = "win32"}`

**Substs:**
Files listed in `substs:` should be processed (e.g., `config.ml.in` → `config.ml`)

**Patches:**
Patches listed in `patches:` should be applied, including conditional patches

**Environment:**
- `build-env:` should set environment for build commands
- `setenv:` should export variables to dependent packages

**Build location:**
- When `(install true)`: build in `_build/.pkgs/<context>/<name>/`
- When `(install false)`: build in `_build/.pkgs/<context>/<name>-<build-id>/`
- Should install to `_build/install/<context>/`

When `install=true`, the path uses `<name>` (without version or build-id) to support clean
uninstall and upgrade: when a package version changes, the same directory is reused rather
than accumulating stale directories. When `install=false`, the build-id is included to
allow multiple versions of the same package to coexist.

**Build-id computation:**
The `build-id` is a content-addressable hash computed at lock time for deterministic caching.
It forms a Merkle tree over the dependency graph:

```
build_id(pkg) = hash(
  pkg_content_hash,      # hash of package definition (version, build commands, etc.)
  sorted(deps_build_ids), # build_ids of all dependencies
  platforms_hash         # set of platforms the package is enabled on
)
```

Any change to a package or its transitive dependencies produces a new build-id, enabling:
- Deterministic toolchain caching (same inputs → same build-id → cache hit)
- Dev tool caching across projects
- Precise cache invalidation when dependencies change

**Cross-compilation:**
When building for cross-compilation targets (`-x windows`, MirageOS, etc.), the opam
build-mode context should set up proper `OCAMLFIND_TOOLCHAIN`, sysroot paths, and
cross-compiler variables so that non-dune packages "just work".

## Use Cases

### Gradual Migration

Keep two versions of a library during migration:

```dune
(vendor vendor/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

(vendor vendor/yojson.2.0.0
 (libraries yojson))
```

### Cherry-Picking from Monorepos

Pick specific libraries from different copies of a monorepo:

```dune
(vendor vendor/core.0.16.1
 (libraries core core_kernel))

(vendor vendor/core_unix.0.16.0
 (libraries core_unix))
```

### Non-Dune Source

```dune
(vendor vendor/zarith.1.14)
```

### Hiding Internal Libraries

Expose only public libraries:

```dune
(vendor vendor/lwt.5.7.0
 (libraries lwt lwt.unix))

(vendor vendor/cohttp.6.0.0
 (libraries :standard \ cohttp-async))
```

## Compatibility

`(vendored_dirs)` continues to work. These are equivalent:

```dune
(vendored_dirs vendor/foo.1.0.0)
(vendor vendor/foo.1.0.0)
```

No migration required.

**Composition with opam:**
Vendored libraries appear in the build graph like any other library. A project can
mix vendored dependencies with opam-installed dependencies — this is no different
from existing `(vendored_dirs)` behavior.

## Alternatives Considered

**External tool modifies sources:**
A source management tool could rename libraries by editing `dune` files, or hide
libraries by deleting source files.
Rejected: violates the unmodified sources principle. Updates become complex merge
operations instead of simple directory replacements.

**Extend `(vendored_dirs)` syntax:**
```dune
(vendored_dirs
 (dir vendor/fmt.0.9.0 (libraries fmt)))
```
Rejected: mixes listing with configuration, harder to read.

**Per-directory config file:**
```
vendor/fmt.0.9.0/.dune-vendor
```
Rejected: splits configuration, requires modifying vendored sources.

**Library-level aliasing:**
```dune
(library
 (name yojson)
 (vendor_alias yojson_v1))
```
Rejected: requires modifying vendored dune files.

## Non-goals

The following are explicitly out of scope for this RFC:

- **Source fetching**: How sources get into the vendor directory is not dune's concern
- **Version resolution**: Dune builds what's there, it doesn't resolve dependencies
- **Checksums/reproducibility**: Ensuring vendored sources are correct is the user's responsibility
- **Depexts**: Dune already has a depext system (`dune show depexts`); improvements
  should be addressed in a separate RFC

## Future Work

The `(vendor)` stanza provides build-time infrastructure that higher-level tools can
target. Lock file generators, source fetchers, or dependency managers can generate
`(vendor)` stanzas as their output format, enabling a clean separation between
resolution and building.

## Open Questions

**Nix-style derivations vs shared prefix?**

There are two models for how vendored opam packages expose their artifacts:

**Shared prefix (opam-style):**
All packages install to `_build/install/<context>/`. Dune sets:
- `PATH` → `_build/install/<context>/bin/`
- `OCAMLPATH` → `_build/install/<context>/lib/`
- `CAML_LD_LIBRARY_PATH` → `_build/install/<context>/lib/stublibs/`

Executable dependencies (`%{bin:menhir}`) resolve from the shared bin directory.

Pros:
- Matches opam semantics exactly
- Simple PATH/OCAMLPATH setup
- `%{bin:...}` resolution is straightforward

Cons:
- Requires install step (copying artifacts)
- Only one version of a package can be installed per context
- Cleanup on upgrade requires tracking what was installed

**Isolated derivations (nix-style):**
Each package builds in `_build/.pkgs/<context>/<name>-<build-id>/` and artifacts
are used directly from there. Dune sets:
- `PATH` → union of all `<pkg>/target/bin/` directories
- `OCAMLPATH` → union of all `<pkg>/target/lib/` directories

Executable dependencies resolve by scanning package build directories.

Pros:
- No install/copy step
- Multiple versions can coexist naturally
- Consistent with how dune handles dune-native packages
- Build-id in path enables precise caching

Cons:
- More complex PATH/OCAMLPATH setup
- `%{bin:...}` resolution needs to search multiple directories
- Diverges from opam's model

**Impact on executable resolution:**

In dune rules, executables can be referenced via `%{bin:menhir}` or as dependencies.
The choice above affects how these are resolved:
- Shared prefix: look in `_build/install/<context>/bin/`
- Isolated: search through `_build/.pkgs/<context>/*/target/bin/`

For dune-native vendored packages, executables are part of the normal build graph
and dune resolves them directly. The question is specific to opam-mode packages.

## References

- [dune#8652](https://github.com/ocaml/dune/issues/8652) — Package management: build non-dune packages
- [opam-monorepo](https://github.com/tarides/opam-monorepo)
- [opam-monorepo #145](https://github.com/tarides/opam-monorepo/issues/145)
- [dune-universe/opam-overlays](https://github.com/dune-universe/opam-overlays)
