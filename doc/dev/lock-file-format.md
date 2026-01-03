# Lock File Format Redesign

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
