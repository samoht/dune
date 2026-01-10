# Lock File Format Redesign

**Related documents:**
- [vendoring.md](vendoring.md) - Vendor stanza syntax, selective libraries, sandbox modes
- [patching.md](patching.md) - Patch workflow for modifying dependencies
- [pkg-ux-design.md](pkg-ux-design.md) - CLI UX design for `dune pkg` commands

## Philosophy

Package resolution is a deterministic process with three levels of reproducibility:

1. **opam-repo hash + solver + formula → reproducible solution** (auto-lock)
2. **opam-repo hash + version list → reproducible** (minimal lock file) ← **TARGET**
3. **duniverse/ directory → fully reproducible** (vendored sources)

The current `dune.lock/` directory sits between 2 and 3 - it stores full package
specs that duplicate info from the opam repo. We should move to pure approach 2.

## Auto-Lock on Build

When `dune build` runs and no lock file exists (or dependencies changed), dune can
automatically invoke the solver for the **current OS/platform only**. This produces
a single-platform lock file optimized for the developer's machine.

### Configuration

Add `auto_lock` option to `~/.config/dune/config`:

```lisp
(auto_lock enabled)   ; or: disabled (default), prompt
```

**Behavior:**
- `enabled`: Automatically lock when needed, single platform (current host)
- `disabled`: Error if lock file missing/outdated (current behavior)
- `prompt`: Ask user before locking

### Implementation

1. **Add config field** (`src/dune_config_file/dune_config_file.ml`):
   ```ocaml
   type auto_lock = Enabled | Disabled | Prompt
   ```

2. **Check in build flow** (`src/dune_rules/pkg_rules.ml`):
   - Before loading lock dir, check if it exists/is fresh
   - If missing and `auto_lock = Enabled`, invoke solver with current platform
   - Solver uses `Sys` to detect current os/arch

3. **Single-platform solver call**:
   ```ocaml
   let current_platform = Solver_env.of_current_system () in
   Solver.solve ~platforms:[current_platform] ...
   ```

**Files:**
- `src/dune_config_file/dune_config_file.ml` - add `auto_lock` field
- `src/dune_rules/pkg_rules.ml` - check lock freshness, trigger auto-lock
- `src/dune_pkg/solver.ml` - `of_current_system` helper
- `src/dune_pkg/solver_env.ml` - detect current os/arch

## Updating Dependencies

Like other package managers (`cargo update`, `bundle update`, `poetry update`), dune
provides commands to update locked dependencies to newer versions.

### Commands

```bash
dune pkg update                    # Update all packages to latest versions
dune pkg update fmt cmdliner       # Update specific packages only
dune pkg lock                      # Re-lock (same as update if lock exists)
```

### CI Mode: Test with Latest Dependencies

For CI pipelines that want to test compatibility with the latest opam packages:

```bash
dune pkg update && dune build
```

### The `--lock` Flag

A single flag controls locking behavior:

```bash
dune build --lock=disabled   # No pkg management, use system/opam (default)
dune build --lock=enabled    # Auto-lock if missing, use existing if present
dune build --lock=always     # Always re-solve with latest opam repo
```

| Mode | Lock file | Dependencies from | Use case |
|------|-----------|-------------------|----------|
| `disabled` | Ignored | System findlib/opam | Traditional workflow (default) |
| `enabled` | Create if missing | Lock or fresh solve | Dev with pkg management |
| `always` | Overwritten | Fresh solve | CI compat testing |

### CI Workflow Example

```yaml
# .github/workflows/compat.yml
name: Compatibility Check
on:
  schedule:
    - cron: '0 0 * * *'  # nightly

jobs:
  test-latest-deps:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: dune build --lock=always
```

### Global Configuration

Set default in `~/.config/dune/config`:

```lisp
(auto_lock disabled)   ; No pkg management, use system/opam (current default)
(auto_lock enabled)    ; Auto-lock if missing, use existing if present
(auto_lock always)     ; Always re-solve (CI mode)
```

**Environment variable** (useful for CI):

```bash
DUNE_CONFIG__AUTO_LOCK=always dune build
```

**Precedence:** CLI flag > config file > environment variable > default (`disabled`)

## Workflow

**Simple case (most users):**
1. User has dune-project with dependencies
2. `dune pkg lock` → creates dune.lock for current platform/compiler (or auto-lock on build)
3. `dune build` works
4. No dune-workspace needed

**Multiple compilers or platforms:**
1. User specifies via CLI or dune-workspace
2. `dune pkg lock` solves for all specified compilers/platforms
3. dune.lock contains packages with filters
4. `dune build` builds all contexts

**CLI options** (for quick setup without workspace):
```bash
dune pkg lock                                    # current platform, default compiler
dune pkg lock --compiler ocaml.4.14.2            # specific compiler
dune pkg lock --compiler ocaml.5.2.0,ocaml.4.14.2   # multiple (comma-separated)
dune pkg lock --platform linux-x86_64,macos-arm64   # multiple platforms
dune pkg lock --repo opam,relocatable            # multiple repos (comma-separated)
dune pkg lock --repo opam --repo relocatable     # multiple repos (repeated option)
dune pkg lock --repo myrepo:https://github.com/user/repo.git  # custom repo
```

**Built-in repositories** (used by default, in priority order):
1. `relocatable` → relocatable compiler overlay (first priority)
2. `overlay` → dune-specific package patches
3. `opam` → `https://github.com/ocaml/opam-repository.git`

Most users need no configuration - defaults just work.

**dune-workspace** (only if customizing repos):
```dune
; Add custom repo while keeping defaults
(repositories :standard myrepo)

; Remove a default repo
(repositories (:standard \ relocatable))

; Explicit list (no defaults)
(repositories myrepo opam)
```

**dune-workspace** (for persistence):
```dune
(toolchain windows
  (repository https://github.com/ocaml-cross/opam-cross-windows.git)
  (package ocaml-windows))

(context default)
(context (workspace (compiler ocaml.4.14) (name ocaml414)))
(context (workspace (compiler ocaml.5.4) (targets native windows)))
```

If dune-workspace exists, solver reads it. CLI options override/extend.

The lock file records what was solved for - it's output, not input. The workspace
(or CLI) is the source of truth for what compilers/platforms to solve for.

## Lock File Formats

### Single-File Format (Canonical)

The single-file format is the canonical lock format. It's human-readable,
VCS-friendly, and contains only the essential information needed to reproduce
the dependency resolution.

```lisp
; dune.lock (single file in source tree)
(lang package 0.2)

; Pin the repository state for reproducibility
(repos
 (opam-repository 1234abcd))  ; git commit hash

; Just the versions - everything else derived from repo
(packages
 fmt.0.9.0
 cmdliner.1.3.0
 base.v0.17.0
 conf-gmp.4
 zarith.1.14)

; User patches (optional)
(patches
 (fmt patches/fmt@0.9.0.patch))
```

### Directory Format (Derived)

The directory format contains expanded package specs and is derived from
the single-file lock. It's stored in `_build/.locks/<ctx>/pkgs/` and used
internally for building packages.

```
_build/.locks/default/pkgs/
  lock.dune           # metadata
  foo.0.9.0.pkg       # package spec (build, install, source, deps...)
  bar.1.2.3.pkg
  bar.1.2.3.files/    # patches and extra files
```

**Characteristics:**
- One file per package
- Full package specs (build commands, source URLs, deps)
- Derived from single-file + opam repo lookup
- Not committed to version control

## Build Directory Structure

When building with package management enabled:

```
project/
  dune.lock                     # single-file lock (source of truth, committed to VCS)

_build/
  .locks/
    <ctx>/                      # context name (e.g., "default")
      <lock_name>/              # lock directory name (e.g., "dune.lock")
        lock                    # copy of dune.lock file from source tree
        pkgs/                   # derived directory (platform-specific .pkg files)
          lock.dune             # metadata
          foo.0.9.0.pkg         # package spec (build, install, deps...)
          bar.1.2.3.pkg
          ...
```

### Key Paths

| Path | Type | Description |
|------|------|-------------|
| `dune.lock` | file | Source tree, committed to VCS |
| `_build/.locks/<ctx>/<lock>/lock` | file | Copy of dune.lock for this context |
| `_build/.locks/<ctx>/<lock>/pkgs/` | dir | Derived .pkg files (platform-specific) |

### Flow

1. **Solver runs** → generates single-file `_build/.locks/<ctx>/<lock>/lock`
2. **Promote to source** → copy to `dune.lock` in project root
3. **Derive pkgs directory** → expand to `_build/.locks/<ctx>/<lock>/pkgs/`
   - Variables resolved for this context's os/platform (no opam variables in .pkg files)
4. **Build packages** → uses derived `pkgs/` directory

### Derivation

When deriving the `pkgs/` directory from the single-file lock, dune:

1. **Fetches opam-repo at pinned hash** (cached in ~/.cache/dune)
2. **Looks up each package** → gets source URL, checksum, build commands, deps
3. **Resolves opam variables** → evaluates for context's os/arch/platform
   - `%{os}%`, `%{arch}%`, `%{os-family}%` resolved to concrete values
   - Conditional dependencies filtered based on platform
   - No opam variables remain in derived .pkg files
4. **Classifies** → duniverse (dune-built) or opam
5. **Records patches** → references to patches from opam repo + user patches
   (patches are applied to package sources at build time, not during derivation)
6. **Writes .pkg files** → to `_build/.locks/<ctx>/<lock>/pkgs/`

The derived `pkgs/` directory is **platform-specific**: each context gets its own
derivation with variables resolved for that context's os and platform. This means
the same `dune.lock` can produce different `pkgs/` directories for different
build contexts (e.g., linux vs macos, or native vs cross-compilation).

This is the same as what happens with auto-lock, just with pinned repo + versions.

## Lock File Equivalence

All lock formats are **semantically equivalent** - they represent the same
dependency solution. The formats differ only in efficiency for different use cases:

| Format | Optimized For |
|--------|---------------|
| Single-file (`dune.lock`) | Human review, VCS, source tree |
| Directory (`pkgs/`) | Machine processing, incremental updates, network resilience |

More expanded formats contain more information locally, making them more isolated
from network errors and external dependencies. The single-file format is the
**canonical** representation that users interact with. The directory format is
**derived** for efficient and resilient package building.

## What About Offline Builds?

Two modes:

### Default: Minimal Lock (Approach 2)
- Lock file pins repo hash + versions
- Opam-repo cached in `~/.cache/dune/` (works offline after first fetch)
- Sources fetched on demand to `_build/.pkgs/<context>/<name>.<version>-<digest>/source/`

### Optional: Full Vendor (Approach 3)
- Run `dune pkg fetch` to download sources to `duniverse/`
- Commit `duniverse/` to version control
- Works fully offline, no network needed
- **Requires**: patching workflow from `duniverse-patching.md`

Most users should use approach 2. Approach 3 is for those who want to:
- Commit dependencies to their repo
- Work fully offline without any cache
- Patch dependencies and contribute upstream

## What About Patches?

Patches from opam-repo are already stored in the repo (by hash).
User patches go in `patches/` directory and are referenced in lock file.

No need to store in `dune.lock.d/` - that would bring back the directory problem.

## Implementation

### Architecture

```
                                    dune.lock (source tree)
                                         ↑
                                    [promote]
                                         │
Solver  ──→  _build/.locks/<ctx>/<lock>/lock (single-file, canonical)
                                         │
                                    [derive for os/platform]
                                         ↓
             _build/.locks/<ctx>/<lock>/pkgs/  (platform-specific .pkg files)
                      │
                      ↓
               Lock_dir.t (in memory)
                      │
                      ↓
              Package builds (see pkg-rules documentation)
```

**Key paths:**
- `dune.lock` - source tree, promoted, committed to VCS
- `_build/.locks/<ctx>/<lock>/lock` - copy of single-file lock
- `_build/.locks/<ctx>/<lock>/pkgs/` - derived .pkg files (platform-specific)

### Changes Required

**src/dune_pkg/lock.ml:** (renamed from lock_dir.ml)
```ocaml
(* Single-file lock format *)
module File : sig
  module Repo : sig
    type t = { source : string; hash : string }
  end
  module Package_entry : sig
    type t = { name : Package_name.t; version : Package_version.t }
  end
  module Patch_entry : sig
    type t = { package : Package_name.t; path : Path.Local.t }
  end
  type t = {
    repos : Repo.t list;
    packages : Package_entry.t list;
    patches : Patch_entry.t list;
  }
  val encode : t -> Dune_sexp.t list
  val decode : t Decoder.t
end

(* Derive full Lock.t from File.t + repo *)
val derive
  :  File.t
  -> repos:Opam_repo.t list
  -> Lock.t Fiber.t
```

**src/dune_pkg/opam_repo.ml:**
```ocaml
(* New: lookup package at specific repo hash *)
val load_package_at_hash
  :  t
  -> hash:string
  -> Package_name.t
  -> Package_version.t
  -> Resolved_package.t option Fiber.t
```

### Phase 1: Add File Encoder/Decoder ✓
- Added `Lock.File` module with encoder/decoder
- Renamed `Lock_dir` to `Lock` for cleaner naming

### Phase 2: Generate Single-File Lock
- Solver outputs to `_build/.locks/<ctx>/dune.lock` (single-file)
- Modify `lock_rules.ml` to write single-file format

### Phase 3: Add Promotion
- After generating `_build/.locks/<ctx>/dune.lock`, promote to source tree
- Respect `--no-promote` flag if specified
- Promotion creates/updates `dune.lock` in project root

### Phase 4: Add Derivation
- Derive `_build/.locks/<ctx>/pkgs/` from single-file lock
- Reuse existing `setup_single_file_derive_rules` logic
- Update `Lock_dir.get_exn` to read from `pkgs/` directory

### Phase 5: Update Path Constants
- Update `lock_dir.ml` paths for new structure
- `_build/.locks/<ctx>/dune.lock` (file)
- `_build/.locks/<ctx>/pkgs/` (directory)

### Phase 6: Cleanup
- Deprecation warning for directory format in source tree
- Eventually remove support for `dune.lock/` directory in source

## Virtual Packages

Some opam packages have no source - they exist only to:
- Check for system dependencies (`conf-*` packages)
- **Set opam variables** that other packages depend on
- Bundle other packages (meta-packages)

### Why Virtual Packages Matter

Virtual packages are **necessary** to resolve opam variables properly. Many packages
use `%{pkg:installed}%` guards or depend on variables set by virtual packages:

```
# Example: conditional dependency on base-unix
depends: [ "base-unix" {os != "win32"} ]
build: [ ... ] { %{base-unix:installed}% }
```

Without `base-unix` in the lock, `%{base-unix:installed}%` cannot be resolved.

### Examples

```
conf-gmp.4        # sets conf-gmp:lib, conf-gmp:installed
conf-pkg-config.3 # sets conf-pkg-config:installed
conf-libffi.2.0   # sets libffi paths for ctypes
base-unix.base    # sets base-unix:installed (stdlib component)
base-threads.base # sets base-threads:installed
```

### Representation in Lock File

Virtual packages are listed like regular packages:

```lisp
(packages
 fmt.0.9.0
 zarith.1.14
 conf-gmp.4           ; virtual - required for zarith's %{conf-gmp:lib}%
 conf-pkg-config.3)   ; virtual - required for pkg-config detection
```

### Build Behavior

During `dune pkg fetch` / build:

1. **Has source URL** → fetch to duniverse, build normally
2. **No source URL** → skip fetch, run build commands to set variables

Virtual packages may have:
- `depexts` field → system package requirements
- `build` commands → detect paths, set variables
- `setenv` / exported variables → consumed by dependent packages
- No `install` → nothing to install

### Depexts Integration

For `conf-*` packages, dune should:

1. Parse `depexts` from opam file
2. Check if system package is installed
3. Suggest installation command if missing

```bash
$ dune build
Error: System dependency missing: libgmp-dev
Hint: apt install libgmp-dev
```

See `dune show depexts` for listing all system dependencies.

### Special Cases

| Package Type | Source | Build | Notes |
|--------------|--------|-------|-------|
| Regular | Yes | Yes | Normal package |
| Virtual/conf | No | Maybe | System dep check |
| Meta-package | No | No | Just deps |
| Compiler | Special | Special | Uses toolchain cache |

## Post Dependencies

### Semantics

Post dependencies (`{post}` in opam) are installed *with* a package but not
required to *build* it. They break apparent cycles:

```
A depends on B (regular)
B post-depends on A
```

This is **not** a cycle:
1. Build B first (post dep on A doesn't block B's build)
2. Build A (needs B, which exists)
3. Both installed together

### Current Implementation Gaps

**Gap 1: Post deps discarded**

Dune currently **discards** post deps rather than handling them properly:

```ocaml
(* opam_solver.ml *)
| Ok { regular; post = _ (* discard post deps *) } ->

(* lock_pkg.ml *)
(* CR-someday rgrinberg: think about post deps *)
```

This causes dune to reject valid dependency graphs as "cycles" when post deps
would break the cycle in opam.

**Note: `:installed` variable resolution**

opam checks if package is **actually installed** at runtime:
```ocaml
(* OpamPackageVar.ml *)
| "installed", Some _ ->
  Some (bool (OpamPackage.has_name st.installed name))
```

dune checks if package is **in the lock file**:
```ocaml
(* pkg_rules.ml *)
| "installed" ->
  let in_lock = Package.Name.Map.mem all_versions package_name in
  ...
```

**Simple rule:** Error if a post dependency's `:installed` is referenced.

If B post-depends on A, and B's build checks `%{A:installed}%`, dune should
fail with an error. This is unusual and likely a mistake - if you post-depend
on A, you don't need A at build time, so why check `:installed`?

This avoids the semantic mismatch (opam returns `false`, dune would return `true`)
by rejecting the problematic pattern entirely.

### Variable Resolution

For `B post-depends on A`:

| When | `%{A:installed}%` | `%{B:installed}%` |
|------|-------------------|-------------------|
| Building B | `false` | N/A |
| Building A | `true` | `true` |
| After both | `true` | `true` |

Post deps are **not installed** when the depending package builds, so
`:installed` must be `false` for them during that build.

**Note:** This semantics is confusing but dune must follow opam exactly for compatibility.

### Required Fix

Post deps should:
1. Be included in the lock file / solution
2. **Not** be required for build order (don't block the package's build)
3. Have `:installed` = `false` when building the post-depending package
4. Follow opam semantics exactly

Build order for `B post-depends on A`:
```
B builds first (%{A:installed}% = false)
A builds second (%{B:installed}% = true)
```

## Filters (Platform and OCaml Version)

Package entries can have filters for platform and/or OCaml version:

```lisp
(packages
 ; Common to all platforms and OCaml versions
 fmt.0.9.0
 cmdliner.1.3.0

 ; Platform-specific (system library detection)
 (conf-libffi.2.0.0 (os linux))
 (conf-libffi.2.1.0 (os macos))

 ; OCaml version-specific (API compatibility)
 (ppxlib.0.32.0 (ocaml < 5))
 (ppxlib.0.33.0 (ocaml >= 5))

 ; Compilers - no filters, selected by context's (compiler ...) field
 ocaml.5.2.0
 ocaml.4.14.2
 ocaml-windows.5.2.0)  ; cross-compiler, runs on host
```

### Filter Syntax

```
package-entry ::= name.version                              ; always included
               |  (name.version filter+)                    ; conditional

filter ::= (os <name>)                            ; linux, macos, windows
        |  (arch <name>)                          ; x86_64, arm64
        |  (ocaml <op> <version>)                 ; version constraint

op ::= < | <= | = | >= | >
```

### Workspace to Lock Mapping

The workspace specifies contexts with their compilers. The solver reads this
and solves for all specified compilers/platforms:

```dune
; dune-workspace
(toolchain windows
  (repository https://github.com/ocaml-cross/opam-cross-windows.git)
  (package ocaml-windows))

(context default)  ; uses system compiler

(context (workspace
  (compiler ocaml.4.14)
  (name ocaml414)))

(context (workspace
  (compiler ocaml.5.4)
  (targets native windows)))
```

At solve time: `dune pkg lock` reads workspace, solves for each unique
(platform, compiler) combination.

At build time: dune evaluates package filters against each context's platform
and compiler to determine which packages apply.

### Solver Behavior

When locking with multiple contexts configured:

1. Solver runs once per unique (platform, ocaml-version) combination
2. Results merged into single lock file with appropriate filters
3. Common packages (same version across all) have no filter
4. Differing packages get filters for when they apply
