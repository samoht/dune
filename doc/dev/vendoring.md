# Vendoring Design

**Related documents:**
- [patching.md](patching.md) - Patch workflow for modifying vendored dependencies
- [lock-file-format.md](lock-file-format.md) - Lock file format and derivation
- [pkg-ux-design.md](pkg-ux-design.md) - CLI UX design for `dune pkg` commands

This document describes dune's vendoring system for building dependencies as part
of the main project, with selective library exposure and sandbox modes.

## Overview

**All locked dependencies are fetched to `duniverse/`** - both dune-buildable and
non-dune packages. Non-dune packages are built in a sandbox under `_build/.pkgs/`.

```
project/
  dune.lock                   # Lock file (single-file or directory format)
  duniverse/                  # ALL package sources (editable)
    ppxlib.0.33.0/            # Dune package → built in main context
    zarith.1.14/              # Non-dune package → built in pkg sandbox
    dune                      # Generated vendor stanzas
  _build/
    .pkgs/default/            # Package builds
      zarith/                 # <name> when install=true (supports upgrade/uninstall)
        source/               # Linked/copied from duniverse/ (rule inputs)
        target/               # Build artifacts (rule outputs)
      yojson-<build-id>/      # <name>-<build-id> when install=false (multi-version)
        source/
        target/
    install/default/          # Shared install prefix
```

Build paths depend on the `(install ...)` setting:
- `(install true)`: `_build/.pkgs/<ctx>/<name>/` - supports clean upgrade/uninstall
- `(install false)`: `_build/.pkgs/<ctx>/<name>-<build-id>/` - allows multiple versions

For vendored packages, source is linked from `duniverse/`.

### Library Cache

The `_build/.pkgs/lib-cache` file maps library names to their source directories
in `duniverse/`. This is the key mechanism for triggering automatic package fetching:

```
# _build/.pkgs/lib-cache (auto-generated)
fmt:fmt.0.9.0
fmt.tty:fmt.0.9.0
cmdliner:cmdliner.1.3.0
zarith:zarith.1.14
```

**Purpose:**
- Triggers eager fetching: all locked packages are fetched when lib-cache is generated
- Enables lazy building: packages are only built when their libraries are actually needed
- Provides fast library→directory lookup without scanning duniverse/ on every build
- Acts as the dependency edge between lock file and library resolution

**Implementation:**

The lib-cache rule depends on `dune.lock` and produces `_build/.pkgs/lib-cache`:

```
Rule: _build/.pkgs/lib-cache
  Depends: dune.lock
  Action:
    1. Read package list from dune.lock
    2. For each package, fetch source to duniverse/<name>.<version>/
    3. Scan each directory for libraries (dune files, META, opam files)
    4. Write library:directory mappings to lib-cache
```

Library resolution (findlib paths, library lookup) depends on lib-cache:

```
dune exec ./main.exe
  → needs library "fmt"
  → library resolution depends on lib-cache
  → lib-cache rule runs (fetches packages, scans libraries)
  → library resolution finds fmt in duniverse/fmt.0.9.0/
  → build proceeds
```

**Cache invalidation:**
- lib-cache is regenerated when `dune.lock` changes (via dune's dependency tracking)
- Manual deletion triggers regeneration on next build

This enables:

- **Editable dependencies**: All sources in duniverse/, edit and rebuild
- **Full editor tooling**: Merlin/LSP works into dependency code
- **Unified view**: One place for all dependency sources
- **Appropriate build method**: Dune packages in main context, others sandboxed

## Architecture

The vendor infrastructure is the **primitive foundation** for building external
packages. Lock files are a higher-level abstraction that **compiles to** the
vendor infrastructure:

```
Lock file → compiles to → Vendor stanzas → builds packages
                              ↑
         Manual vendor stanzas also work directly
```

This layered design means:

1. **Vendor infrastructure is the foundation**
   - Can be anywhere in the workspace (not just `duniverse/`)
   - Scans directories for opam files (packages) and dune files (libraries)
   - Handles variable expansion (`%{prefix}%`, `%{lib}%`, etc.)
   - Builds packages using opam-style or dune-style methods

2. **Lock files compile to vendor infrastructure**
   - `dune pkg lock` resolves dependencies and generates lock file
   - `dune pkg fetch` fetches sources to `duniverse/` and generates vendor stanzas
   - The generated stanzas use the same vendor infrastructure as manual stanzas
   - Lock files just provide metadata; the vendor layer does the actual building

3. **Single code path for variable expansion**
   (see https://opam.ocaml.org/doc/Manual.html#Variables)
   - Both lock file packages and manual vendor stanzas use the same expansion logic
   - Global: `%{make}%`, `%{jobs}%`, `%{arch}%`, `%{os}%`, `%{os-family}%`,
     `%{os-distribution}%`, `%{os-version}%`
   - Switch: `%{prefix}%`, `%{lib}%`, `%{bin}%`, `%{sbin}%`, `%{share}%`,
     `%{doc}%`, `%{etc}%`, `%{man}%`, `%{toplevel}%`, `%{stublibs}%`
   - Package: `%{name}%`, `%{version}%`, `%{pkg:installed}%`, `%{pkg:enable}%`,
     `%{pkg:lib}%`, `%{pkg:share}%`, `%{pkg:etc}%`, `%{pkg:doc}%`
   - Build: `%{_:name}%`, `%{_:lib}%`, `%{_:share}%`, `%{_:etc}%` for current package

This means you can:
- Use vendor stanzas directly without a lock file (for manual dependency management)
- Let `dune pkg` generate vendor stanzas from lock files (for automated management)
- Mix both approaches in the same workspace

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
 (mode <method>)              ; optional: build mode (dune or opam, default: dune)
 (install <bool>)             ; optional: promote to shared install path (default: true)
 (toolchain <name>))          ; optional: marks this as providing a toolchain
```

Both `<lib-name>` and `<pkg-name>` use the standard ordered set language (like `(modules ...)`) with:
- `:standard` — all libraries/packages found in the directory
- `<name>` — expose with its original name
- `(<name> :as <alias>)` — expose under a different name

All fields are optional. When omitted:
- `(libraries ...)` and `(packages ...)` should be auto-detected by scanning the directory
- `(mode ...)` should be auto-detected: `dune` if dune files exist, `opam` otherwise
- `(install ...)` defaults to `true`, except when `:as` aliasing is used (then `false`)
- `(toolchain ...)` defaults to none

### Toolchain Declaration

The `(toolchain ...)` field marks vendored packages as providing toolchains (compilers).
A toolchain is a complete set of OCaml build tools: `ocamlc`, `ocamlopt`, standard library, etc.

- `(toolchain native)` or `(toolchain default)` — provides the native compiler
- `(toolchain <name>)` — provides a cross-compilation toolchain for the named target

```lisp
; Native compiler - provides ocamlc, ocamlopt, etc. for the default context
(vendor ocaml.5.2.0
 (toolchain native))

; Another native compiler version
(vendor ocaml.4.14.2
 (toolchain default))

; Cross-compilation toolchain for Windows
(vendor ocaml-windows.5.2.0
 (toolchain windows))
```

These can then be used via workspace targets:

```dune
; Build for native and Windows targets
; - native context uses (toolchain native) vendor package
; - windows context uses (toolchain windows) vendor package
(context (default
  (targets native windows)))
```

**`(toolchain ...)`**:
- Declares the package as providing a toolchain (native or cross-compilation)
- `native` or `default` = native compiler for the host platform
- Other names = cross-compilation toolchains matching `(targets ...)` in dune-workspace
- Dune builds this package first to get `ocamlc`, `ocamlopt`, etc.
- For cross-compilation, sets `OCAMLFIND_TOOLCHAIN=<name>` for the target context

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

### Library Aliasing

Libraries can be renamed to avoid conflicts when vendoring multiple versions:

```lisp
(vendor duniverse/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

(vendor duniverse/yojson.2.0.0
 (libraries yojson))
```

The alias creates a new public name for the library while keeping the original
module structure. In the example above, code can use both versions:

```ocaml
(* Uses yojson 1.7.0 *)
let old_result = Yojson_v1.Basic.from_string json

(* Uses yojson 2.0.0 *)
let new_result = Yojson.Basic.from_string json
```

Aliasing only affects the public library name used in `(libraries ...)` stanzas.
The OCaml module names inside the library remain unchanged.

Using `:as` aliasing implies `(install false)` — aliased names are workspace-local
and external packages cannot reference them. This can be overridden with an explicit
`(install true)` if needed.

### Use Cases

#### Multi-Version Migration

Gradual migration from one version of a library to another:

```lisp
; Old version for legacy code - alias to avoid conflict
(vendor duniverse/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

; New version for new code
(vendor duniverse/yojson.2.0.0
 (libraries yojson))
```

This allows gradual migration where legacy code uses `yojson_v1` and new code
uses `yojson`, both coexisting in the same project.

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

## Dependency Resolution

Dune resolves dependencies at the library level, not the package level. When anything
in the workspace needs library "foo" — a dune library, executable, or vendored package —
dune looks for "foo" in:

1. Vendored dune packages in the workspace
2. Vendored opam packages (installed to shared prefix)
3. Locked packages from dune.lock
4. System OCAMLPATH (for libraries with C bindings)

This is the same resolution mechanism for all consumers. For opam-mode packages, dune
sets up:
- `OCAMLPATH` → all dependency lib directories
- `CAML_LD_LIBRARY_PATH` → stublibs
- Other findlib variables

Build commands use `ocamlfind` to discover libraries.

Importantly, dune does not require a closure of opam packages. If a vendored opam
package's `depends:` field lists package "foo", but the required libraries are available
from a different source (e.g., a dune package), that works. Errors occur at build time
if ocamlfind cannot locate a required library.

There is no automatic library-to-package resolution. If a missing library requires an
opam package, the user must explicitly add it to `dune-project` so it appears in the
lock file.

## Build Methods

The `mode` field controls how vendored code is built:

| Method | Description |
|--------|-------------|
| `dune` | Native dune build (default). Vendored code builds like regular project code. |
| `opam` | Opam-style build. Uses opam build/install commands from lock file or opam file. |

The `opam` build method is useful for packages that:
- Use opam-specific variables (e.g., `%{lib}%`, `%{prefix}%`)
- Have install steps that expect opam directory layout
- Include opam build scripts or substitutions
- Don't use dune as their build system

Example:

```lisp
(vendor dune-release.2.0.0
 (mode opam))  ; needs opam environment simulation
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
 (mode opam))
```

### Generation Logic

For each locked package, `dune pkg fetch`:

1. Fetches source to `duniverse/<name>.<version>/`
2. Scans for libraries defined in the package
3. Determines sandbox mode:
   - `opam` if package has opam-specific build commands or substitutions
   - `none` otherwise
4. Writes vendor stanza to `duniverse/dune`

### Mode Detection

A package needs `(mode opam)` if its lock file entry contains:
- `%{...}%` variable substitutions (opam variables)
- Non-dune build commands (`make`, `./configure`, etc.)
- Install commands that reference opam paths

Example lock entry triggering opam build:

```lisp
; dune.lock/opam-state.2.2.0.pkg
(build
 (run dune build -p %{name}% -j %{jobs}%))
(install
 (run dune install --prefix %{prefix}%))
```

### Regeneration and Overrides

The `duniverse/dune` file is **fully regenerated** on each `dune pkg fetch`.

For customizations, create your own vendor directory:

```lisp
; vendor/dune (user-maintained)
(vendored_dirs *)

; Custom vendored package with specific library selection
(vendor my-fork.1.0.0
 (libraries my-fork.core))

; Override a duniverse package by copying it here
(vendor fmt.0.9.0
 (libraries fmt))  ; expose only fmt, not fmt.tty or fmt.cli
```

This approach:
- Keeps `duniverse/` fully managed by dune (can be gitignored)
- User customizations live in `vendor/` (committed to git)
- No conflict between generated and user-maintained configuration

## The @pkg-install Alias

### Current Behavior

Currently `@pkg-install` builds and installs locked packages into `_build/.pkgs/default/`.
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
| Lock dir only (no duniverse) | Packages in `_build/.pkgs/<ctx>/` |
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
 (mode opam))                   ; build: [make] → explicit mode
```

For regular packages, mode is determined from the lock file (which contains
the opam build commands). For pins, we need to fetch the source first to read
the opam file, then record the detected mode in `duniverse/dune`.

For overrides, explicit `(mode ...)` in `dune-project` takes precedence over
the generated `duniverse/dune`.

## Opam File Generation

**Status**: Implemented

When `dune pkg fetch` fetches packages to duniverse, it also writes opam files
for each package. This enables `(mode opam)` packages to be built standalone
without needing the lock directory.

### Implementation

For single-file lock format (`dune.lock` file):
1. `dune pkg fetch` derives package metadata from the opam repository
2. The opam file content is extracted from the resolved package
3. An `opam` file is written to each package's duniverse directory

```
duniverse/
  zarith.1.14/
    opam               # Auto-generated from opam-repository
    src/               # Source code
    configure          # Build scripts
```

For directory lock format (`dune.lock/` directory):
- Opam files are not generated (metadata is already in .pkg files)
- The directory lock format is being deprecated in favor of single-file

### Build Behavior

When building `(mode opam)` packages, dune reads build/install commands
directly from the local opam file in duniverse. This means:

- **No lock directory needed at build time** - all information is in duniverse
- **Editable build commands** - users can modify the local opam file
- **Standalone builds** - the duniverse directory is self-contained

### Opam File Handling

- Many packages use a single `opam` file (not `<name>.opam`); dune writes to `opam`
- On re-fetch, the opam file is overwritten (user modifications are lost)
- Users who want custom build commands can commit their modified opam file

### Future: Build Command Overrides

A future enhancement may allow specifying build commands directly in the vendor
stanza:

```lisp
(vendor zarith.1.14
 (mode opam)
 (build_command (run ./configure) (run make))
 (install_command (run make install PREFIX=%{prefix}%)))
```

This would allow customization without modifying the vendored source.
