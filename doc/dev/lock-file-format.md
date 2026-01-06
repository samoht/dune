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

## Current Format (Directory)

```
dune.lock/
  lock.dune           # metadata
  foo.0.9.0.pkg       # package spec (build, install, source, deps...)
  bar.1.2.3.pkg
  bar.1.2.3.files/    # patches and extra files
```

**Characteristics:**
- One file per package
- Full package specs stored (build commands, source URLs, deps)
- Info is duplicated from opam-repo (can be re-derived from repo + version)

## New Format (Single File) - Minimal

```lisp
; dune.lock
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

That's it. ~10 lines vs hundreds of lines in current format.

## Derivation on Build

When building, dune:

1. **Fetches opam-repo at pinned hash** (cached in ~/.cache/dune)
2. **Looks up each package** → gets source URL, checksum, build commands, deps
3. **Classifies** → duniverse (dune-built) or opam sandbox
4. **Applies patches** → from opam repo + user patches
5. **Builds** → as today

This is the same as what happens with auto-lock, just with pinned repo + versions.

## Benefits

| Aspect | Current (Directory) | New (Single File) |
|--------|---------------------|-------------------|
| Size | ~100 lines/package | 1 line/package |
| Files | N+1 files | 1 file |
| Merge conflicts | Common | Rare |
| Review | Hard (noise) | Easy (just versions) |
| Redundancy | High | None |

## What About Offline Builds?

Two modes:

### Default: Minimal Lock (Approach 2)
- Lock file pins repo hash + versions
- Opam-repo cached in `~/.cache/dune/` (works offline after first fetch)
- Sources fetched on demand to `_build/.pkg/`

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

### Current Architecture

```
Lock_dir.t  ←──  Lock_dir.read_disk()  ←──  dune.lock/ directory
     │                                           │
     │                                      lock.dune (metadata)
     │                                      foo.0.9.0.pkg (full spec)
     │                                      bar.1.2.3.pkg (full spec)
     │
     ↓
  file_contents_by_path()  ──→  Write_disk.commit()  ──→  dune.lock/
```

Key insight: `Lock_dir.t` already contains `Pkg.t` with full specs.
The directory format stores these verbatim. The minimal format stores just versions.

### New Architecture

```
Lock_dir.t  ←──  derive_from_repo()  ←──  Opam_repo + versions
     ↑                                        │
     │                                   dune.lock (minimal file)
     │                                   - repo hash
     │                                   - package versions only
     │
encode_minimal()  ──→  Io.write_file()  ──→  dune.lock
```

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
val derive_from_file
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

### Phase 2: Add Derivation from Repo
- Modify `Opam_repo` to support lookup by hash
- Add `derive_from_file` function
- Cache derived Lock.t in `_build/.pkg/lock-cache/`

### Phase 3: Integration
- Modify `Write_disk.prepare` to use file format
- Modify `Make_load` to detect and read both formats
- Add migration path: read directory → write file

### Phase 4: Cleanup
- Deprecation warning for directory format
- Eventually remove directory format support

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

## Platforms

For portable lock dirs (multi-platform), the version list may differ per platform:

```lisp
(packages
 ; Common to all platforms
 fmt.0.9.0
 cmdliner.1.3.0

 ; Platform-specific
 (linux conf-libffi.2.0.0)
 (macos conf-libffi.2.1.0))
```

Or simpler - just list all versions, let build pick what applies.
