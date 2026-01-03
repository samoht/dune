# Duniverse Patching Design

This document describes the design for user patches in the duniverse workflow,
enabling local modifications to dependencies and easier upstream contribution.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Directory lock format (`dune.lock/`) | ✅ Stable | Current default |
| Portable lockdirs | ✅ Enabled | Default since setup.defaults.ml |
| Single-file format types | ✅ Done | `Lock.File`, `Lock.format` |
| Single-file encode/decode | ✅ Done | `Lock.File.encode/decode` |
| Single-file derive | ✅ Done | `Lock.File.derive` |
| Single-file conversion | ✅ Done | `Lock.File.of_lock`, `Lock_pkg.file_to_lock` |
| Format detection | ✅ Done | `Lock.detect_format` |
| CLI integration (write) | ✅ Done | `dune pkg lock --format=single-file` |
| CLI integration (read) | ⏸️ Blocked | Needs async loading architecture |
| Single-file as default | ❌ Pending | Needs read support first |
| Tests for single-file | ✅ Done | `test/blackbox-tests/test-cases/pkg/single-file-lock.t` |
| `dune pkg patch` command | ✅ Done | `bin/pkg/patch.ml`, tests in `test/blackbox-tests/test-cases/pkg/patch-command.t` |
| Patch application on fetch | ✅ Done | Applied in `bin/pkg/fetch.ml` after opam patches |
| Git-based cache for patches | ❌ Not started | Low priority |

## Goals

1. Enable local modifications to dependencies that persist across `dune pkg fetch`
2. Keep repository size small (don't force-vendor all dependencies)
3. Support contributing patches upstream
4. Be explicit about what's modified vs original
5. Work seamlessly with both lock file formats (single-file and directory)

## Lock File Formats

Dune supports two lock file formats:

### Single-File Format (`dune.lock`)

A minimal lock file storing only essential information:

```lisp
(lang package 0.2)
(repos
 (https://github.com/ocaml/opam-repository.git abc123def456))
(packages
 fmt.0.9.0
 cmdliner.1.3.0
 re.1.11.0)
(patches
 (fmt patches/fmt@0.9.0.patch)
 (cmdliner patches/cmdliner@1.3.0.patch))
```

Benefits:
- Small, human-readable, easy to review in PRs
- Fast to parse - no need to read many files
- Contains patch references directly
- Full package metadata derived from opam repos at pinned commits

### Directory Format (`dune.lock/`)

The traditional format with per-package `.pkg` files:

```
dune.lock/
  lock.dune           # metadata
  fmt.0.9.0.pkg       # full package spec
  cmdliner.1.3.0.pkg
```

Benefits:
- Self-contained (works offline after lock)
- Supports portable lockdirs across platforms
- No need to fetch opam repos to read lock

### Format Detection

`Lock.detect_format : Path.t -> Lock.format option` determines the format:
- `Directory` if path is a directory containing `lock.dune`
- `Single_file` if path is a file starting with `(lang package ...)`
- `None` if neither

## Directory Structure

```
project/
  dune.lock                     # single-file format (OR dune.lock/ directory)
  patches/                      # version controlled - user patches
    fmt@0.9.0.patch
    cmdliner@1.3.0.patch
  duniverse/                    # gitignored - regenerable
    .gitignore                  # auto-generated
    .dune-duniverse             # marker file
    fmt.0.9.0/
    cmdliner.1.3.0/
```

## Patch Storage

Following [Bun's approach](https://bun.sh/docs/pm/cli/patch):

- Patches stored in `patches/` directory (configurable via dune-workspace)
- Filename format: `<name>@<version>.patch` (version-specific)
- Standard unified diff format with optional metadata header
- Recorded in lock file (both formats)

### Patch Format

Standard unified diff with optional metadata header:

```diff
# dune-patch-version: 1
# package: fmt
# version: 0.9.0
# upstream: https://github.com/dbuenzli/fmt
# description: Fix buffer overflow in Format.pp_print_string
--- a/src/fmt.ml
+++ b/src/fmt.ml
@@ -42,7 +42,7 @@
...
```

The metadata header is optional but recommended for upstream contribution workflow.

## Commands

### `dune pkg lock`

When locking, patches are discovered and recorded:

1. Solve dependencies
2. Scan `patches/` directory for matching patches
3. Record patches in lock file:
   - Single-file: `(patches (fmt patches/fmt@0.9.0.patch) ...)`
   - Directory: Add `(user_patches ...)` to `lock.dune`

### `dune pkg fetch`

Enhanced behavior:

1. Create `duniverse/.gitignore` automatically
2. Fetch sources to `duniverse/<name>.<version>/`
3. Apply opam patches (from package metadata)
4. Apply user patches (from `patches/` directory)

### `dune pkg patch <pkg>` (new)

Prepare a package for patching:

```bash
$ dune pkg patch fmt
Preparing fmt.0.9.0 for patching...
Edit files in duniverse/fmt.0.9.0/ then run:
  dune pkg patch --commit fmt
```

This:
- Ensures the package is fetched
- Stores a snapshot of the original source (for diffing)
- Marks it as "being patched" (to detect uncommitted changes)

### `dune pkg patch --commit <pkg>` (new)

Generate patch from modifications:

```bash
$ dune pkg patch --commit fmt
Generated patches/fmt@0.9.0.patch
Updated dune.lock
```

This:
1. Diffs current `duniverse/<pkg>.<version>/` against original source
2. Generates unified diff to `patches/<name>@<version>.patch`
3. Updates lock file with patch reference

### `dune pkg patch --remove <pkg>` (new)

Explicitly remove a patch:

```bash
$ dune pkg patch --remove fmt
Removed patches/fmt@0.9.0.patch
Updated dune.lock
Re-fetch with: dune pkg fetch
```

No automatic pruning - patches must be explicitly removed.

### `dune pkg patch --list` (new)

Show patch status:

```bash
$ dune pkg patch --list
fmt@0.9.0       patches/fmt@0.9.0.patch      (applied)
cmdliner@1.2.0  patches/cmdliner@1.2.0.patch (stale: locked version is 1.3.0)
re@1.11.0       -                            (no patch)
```

Warns about version mismatches but doesn't auto-remove.

## Version Changes

When a patched package's version changes (e.g., `fmt` 0.9.0 → 0.10.0):

1. **Old patch remains** - it's version-specific, won't be applied
2. **Warning on fetch** - "Patch for fmt@0.9.0 exists but locked version is 0.10.0"
3. **User decides**:
   - Try applying old patch to new version: `dune pkg patch --rebase fmt`
   - Create fresh patch: `dune pkg patch fmt` + edit + `--commit`
   - Remove stale patch: `dune pkg patch --remove fmt@0.9.0`

No automatic removal - patches might still be needed for history or multi-version.

## Upstream Contribution Workflow

Optional helper for contributing patches upstream:

```bash
$ dune pkg patch --upstream fmt
Patch: patches/fmt@0.9.0.patch
Upstream: https://github.com/dbuenzli/fmt

Options:
  1. Copy patch to clipboard (for manual PR)
  2. Show git format-patch style output
  3. Open upstream repo in browser
```

This is informational - actual PR creation is manual. The upstream URL can be
stored in the patch file header or derived from the opam file's `dev-repo` field.

## Implementation Notes

### Gitignore Generation

On `dune pkg fetch`, create `duniverse/.gitignore`:

```
# Auto-generated by dune pkg
# User patches are stored in patches/, not here
*
!.gitignore
```

Users who want to vendor can delete this file or add `duniverse/` to their
top-level `.gitignore` exclusions.

### Patch Application Order

1. Opam patches (from package's `patches` field in opam file)
2. User patches (from `patches/` directory)

User patches are applied last, so they can fix issues introduced by opam patches
or override them.

### Detecting Uncommitted Changes

When running `dune pkg fetch` on an already-fetched package:

1. Check if `duniverse/<pkg>/` has modifications vs last fetch
2. If yes, warn: "fmt.0.9.0 has local changes. Commit patch first?"
3. User can: `--force` to discard, or `dune pkg patch --commit fmt` first

### Original Source Storage

To generate diffs, we need the original (unpatched) source. Options:

1. **Git-based**: Initialize `duniverse/<pkg>/` as git repo, commit original
   - Pros: Standard tooling, easy diffs
   - Cons: Many `.git` directories

2. **Cache-based**: Store original in `~/.cache/dune/pkg-sources/<checksum>/`
   - Pros: Shared across projects, no repo pollution
   - Cons: Requires cache management

3. **Re-fetch**: Re-download original source when generating diff
   - Pros: Simple, no storage
   - Cons: Slow, requires network

Recommended: Cache-based with git as optional enhancement for complex patches.

### Lock File Updates

When patches change, the lock file must be updated:

**Single-file format**: Direct update of `(patches ...)` stanza

**Directory format**: Update `lock.dune` with `(user_patches ...)` field

Both formats use `Lock.File.Patch_entry`:
```ocaml
type t = { package : Package_name.t; path : Path.Local.t }
```

## Configuration

In `dune-workspace`:

```lisp
(lang dune 3.18)
(lock_dirs
 (default
  (patch_directory patches)         ; default: patches/
  (duniverse_directory duniverse))) ; default: duniverse/
```

## Next Steps

### Phase 1: CLI Integration for Single-File Format

1. **Add `--format` flag to `dune pkg lock`**
   ```bash
   dune pkg lock --format=single-file  # Write dune.lock file
   dune pkg lock --format=directory    # Write dune.lock/ directory (current default)
   ```

2. **Modify `bin/pkg/lock.ml`**:
   - Add `format` argument to lock command
   - Call `Lock.File.of_lock` + `Lock.File.write_to_disk` for single-file
   - Keep existing `Write_disk.prepare/commit` for directory format

3. **Modify lock loading** in `src/dune_rules/lock_rules.ml`:
   - Use `Lock.detect_format` to determine format
   - For single-file: parse with `Lock.File.decode`, convert with `Lock_pkg.file_to_lock`
   - For directory: use existing `Make_load` functor

### Phase 2: Tests

Add blackbox tests in `test/blackbox-tests/test-cases/pkg/single-file-lock/`:

```bash
# Test single-file lock generation
$ dune pkg lock --format=single-file
$ cat dune.lock
(lang package 0.2)
(repos ...)
(packages ...)

# Test reading single-file lock
$ dune build
```

### Phase 3: Make Single-File Default

Once stable:
1. Change default in `bin/pkg/lock.ml`
2. Update documentation
3. Add migration path for existing projects

## Alternatives Considered

### Vendor everything (like Go modules)

- Pros: Simple, fully reproducible
- Cons: Large repos, noisy diffs, doesn't match OCaml ecosystem norms

### Patches in lock directory

Store patches in `dune.lock/<pkg>.patch` instead of separate `patches/` dir.
- Pros: Everything in one place
- Cons: Lock dir is auto-generated, mixing generated + manual feels wrong

### Automatic patch pruning

`dune pkg patch --prune` to remove stale patches.
- Rejected: Too error-prone. Patches might be intentionally kept for history,
  multi-version support, or the version change might be temporary.

### Patches stored by content hash

Store patches as `patches/<sha256>.patch` with references in lock file.
- Rejected: Less human-friendly, harder to review in PRs

## References

- [Bun patch documentation](https://bun.sh/docs/pm/cli/patch)
- [pnpm patch documentation](https://pnpm.io/cli/patch)
- [patch-package npm](https://www.npmjs.com/package/patch-package)
- [Nix overlays](https://wiki.nixos.org/wiki/Nixpkgs/Modifying_Packages)
- [opam pin](https://opam.ocaml.org/doc/Usage.html)
