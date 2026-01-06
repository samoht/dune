# Vendoring Design

**Related documents:**
- [patching.md](patching.md) - Patch workflow for modifying vendored dependencies
- [lock-file-format.md](lock-file-format.md) - Lock file format and derivation
- [pkg-ux-design.md](pkg-ux-design.md) - CLI UX design for `dune pkg` commands

This document describes dune's vendoring system for building dependencies as part
of the main project, with selective library exposure and sandbox modes.

## Overview

**All locked dependencies are fetched to `duniverse/`** - both dune-buildable and
non-dune packages. The `_build/.pkg/` directory is used only as a **sandbox for
building** non-dune packages, not for storing sources.

```
project/
  dune.lock                   # Lock file (single-file or directory format)
  duniverse/                  # ALL package sources
    ppxlib.0.33.0/            # Dune package → built in main context
    zarith.1.14/              # Non-dune package → built in .pkg sandbox
    dune                      # Generated vendor stanzas
  _build/
    .pkg/                     # Sandbox for building non-dune packages
      <digest>/               # Build artifacts (NOT sources)
```

This enables:

- **Editable dependencies**: All sources in duniverse/, edit and rebuild
- **Full editor tooling**: Merlin/LSP works into dependency code
- **Unified view**: One place for all dependency sources
- **Appropriate build method**: Dune packages in main context, others sandboxed

## The Vendor Stanza

The `vendor` stanza declares a vendored dependency directory:

```lisp
(vendor fmt.0.9.0
 (libraries fmt fmt.tty fmt.cli))
```

### Full Syntax

```lisp
(vendor <directory>           ; required: path to vendored source (relative to dune file)
 (libraries <lib-name> ...)   ; optional: libraries to expose (all if omitted)
 (packages <pkg-name> ...)    ; optional: packages to expose (all if omitted)
 (sandbox <mode>))            ; optional: sandbox mode (default: none)
```

## Selective Library Exposure

### Problem

When vendoring dependencies, dune scans the entire vendored directory for libraries.
This creates problems when:

1. You want to vendor multiple versions of the same project (e.g., for gradual migration)
2. A vendored project contains many libraries but you only need a few
3. Two vendored directories contain libraries with the same name

### Solution

The `libraries` field specifies which libraries to expose:

```lisp
(vendor duniverse/fmt.0.9.0
 (libraries fmt fmt.tty))

(vendor duniverse/fmt.0.10.0
 (libraries fmt.cli))
```

When `(libraries ...)` is specified:
- Only the listed libraries are visible to the dependency resolver
- Other libraries in the directory are ignored (not built, not available as deps)
- This allows multiple versions of the same project to coexist

### Library List

The `libraries` field lists the libraries to expose:

```lisp
; Expose only specific libraries
(libraries fmt fmt.tty)
```

If omitted, all libraries in the vendored directory are exposed.

### Use Cases

#### Multi-Version Migration

Gradual migration from one version of a library to another:

```lisp
; Old version for legacy code
(vendor duniverse/yojson.1.7.0
 (libraries yojson_1_7))  ; renamed to avoid conflict

; New version for new code
(vendor duniverse/yojson.2.0.0
 (libraries yojson))
```

#### Cherry-Picking from Large Projects

Vendor only what you need from a monorepo:

```lisp
(vendor duniverse/core.0.16.0
 (libraries core core_kernel))
; Ignores core_unix, core_thread, etc.
```

#### Conflict Resolution

Two packages provide the same library name:

```lisp
(vendor duniverse/lwt.5.6.0
 (libraries lwt lwt.unix))

(vendor duniverse/async.0.16.0
 (libraries async async_kernel))
; Both might have internal 'scheduler' library - no conflict
```

### Implementation Notes

#### Library Discovery

1. Scan vendored directory to discover all libraries
2. Filter the discovered libraries against the `(libraries ...)` list
3. Only register matching libraries in the dependency graph
4. Mark excluded libraries as "unavailable" (not "missing")

#### Error Handling

- **Library not found**: Error if a library listed in `(libraries ...)` doesn't exist
  in the vendored directory
- **Duplicate libraries**: Error if two vendor stanzas expose the same library name

#### Build Behavior

Excluded libraries:
- Are not built (no `.cma`, `.cmxa`, etc.)
- Cannot be used as dependencies
- Their `dune` files are not parsed
- Their source files are ignored

## Sandbox Modes

The `sandbox` field controls how vendored code is built:

| Mode | Description |
|------|-------------|
| `none` | No sandboxing (default). Vendored code builds like regular project code. |
| `opam` | Opam-style sandbox. Simulates opam's build environment for packages that expect it. |

The `opam` sandbox mode is useful for packages that:
- Use opam-specific variables (e.g., `%{lib}%`, `%{prefix}%`)
- Have install steps that expect opam directory layout
- Include opam build scripts or substitutions

Example:

```lisp
(vendor dune-release.2.0.0
 (sandbox opam))  ; needs opam environment simulation
```

## Generated Vendor File

When running `dune pkg fetch`, dune generates `duniverse/dune` with vendor stanzas
for each fetched package:

```lisp
; duniverse/dune (auto-generated by dune pkg fetch)
(vendored_dirs *)

(vendor fmt.0.9.0
 (libraries fmt fmt.tty fmt.cli))

(vendor cmdliner.1.3.0
 (libraries cmdliner))

(vendor opam-state.2.2.0
 (sandbox opam))
```

### Generation Logic

For each locked package, `dune pkg fetch`:

1. Fetches source to `duniverse/<name>.<version>/`
2. Scans for libraries defined in the package
3. Determines sandbox mode:
   - `opam` if package has opam-specific build commands or substitutions
   - `none` otherwise
4. Writes vendor stanza to `duniverse/dune`

### Sandbox Mode Detection

A package needs `(sandbox opam)` if its lock file entry contains:
- `%{...}%` variable substitutions (opam variables)
- Non-dune build commands (`make`, `./configure`, etc.)
- Install commands that reference opam paths

Example lock entry triggering opam sandbox:

```lisp
; dune.lock/opam-state.2.2.0.pkg
(build
 (run dune build -p %{name}% -j %{jobs}%))
(install
 (run dune install --prefix %{prefix}%))
```

### Regeneration and Overrides

The `duniverse/dune` file is regenerated on each `dune pkg fetch`. User customizations
(e.g., library filtering) should be done in the project's `dune-project` or a separate
`dune` file, not in the generated file.

To override generated settings, add vendor stanzas in `dune-project`:

```lisp
; dune-project - overrides take precedence
(vendor duniverse/fmt.0.9.0
 (libraries fmt))  ; expose only fmt, not fmt.tty or fmt.cli
```

## The @pkg-install Alias

### Current Behavior

Currently `@pkg-install` builds and installs locked packages into `_build/_private/default/.pkg`.
This is used to prebuild dependencies before building the main project.

### New Behavior

With vendored dependencies, `@pkg-install` becomes an alias for building all vendored directories:

```bash
dune build @pkg-install
# Equivalent to: dune build @duniverse/all @other-vendor-dir/all ...
```

This builds all libraries in all vendor stanzas, ensuring dependencies are ready.

### Rationale for Keeping

Keep `@pkg-install` rather than removing it because:

1. **Familiar name** - users already know it means "prepare dependencies"
2. **CI caching** - common pattern to `dune build @pkg-install` as a separate cache step
3. **Prebuilding** - useful to build deps once, then iterate on main project
4. **Uniform interface** - same command works whether using lock-based or vendor-based deps

### Behavior Summary

| Scenario | `@pkg-install` builds |
|----------|----------------------|
| Lock dir only (no duniverse) | Packages in `_build/_private/.pkg` |
| Duniverse only | All libraries in `duniverse/` |
| Both | Both (lock packages + duniverse) |
| Additional vendor dirs | Includes all vendor stanza directories |

### Example

```bash
# Prebuild all dependencies (vendored + locked)
dune build @pkg-install

# Then iterate on main project (deps already built)
dune build @check
dune build @check  # fast - deps cached
```

## Gitignore Generation

On `dune pkg fetch`, create `duniverse/.gitignore`:

```
# Auto-generated by dune pkg
# User patches are stored in patches/, not here
*
!.gitignore
!dune
```

Users who want to vendor can delete this file or add `duniverse/` to their
top-level `.gitignore` exclusions.

## Future Extensions

### Aliasing Libraries

Allow renaming libraries for conflict resolution:

```lisp
(vendor duniverse/yojson.1.7.0
 (libraries
  (yojson :as yojson_old)))
```

### Virtual Library Providers

Specify which implementation a vendor provides for virtual libraries:

```lisp
(vendor duniverse/mirage-crypto-rng-lwt
 (libraries mirage-crypto-rng-lwt)
 (provides random))  ; implements virtual lib 'random'
```

### Build System Auto-Detection

Automatically detect the appropriate sandbox mode from the opam file:

| Detected | Sandbox Mode |
|----------|--------------|
| `dune` in build-deps, or `dune build ...` in build | `none` (native dune build) |
| Anything else | `opam` (needs opam environment) |

The detected mode is recorded in `duniverse/dune`:

```lisp
; duniverse/dune (auto-generated)
(vendor fmt.0.9.0)              ; build: [dune build ...] → no sandbox needed

(vendor old-lib.1.0.0
 (sandbox opam))                ; build: [make] → explicit sandbox
```

For regular packages, sandbox mode is determined from the lock file (which
contains the opam build commands). For pins, we need to fetch the source first
to read the opam file, then record the detected mode in `duniverse/dune`.

For overrides, explicit `(sandbox ...)` in `dune-project` takes precedence over
the generated `duniverse/dune`.
