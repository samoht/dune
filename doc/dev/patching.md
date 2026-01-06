# Dependency Patching Design

**Related documents:**
- [vendoring.md](vendoring.md) - Vendor stanza syntax, selective libraries, sandbox modes
- [lock-file-format.md](lock-file-format.md) - Lock file format and derivation
- [pkg-ux-design.md](pkg-ux-design.md) - CLI UX design for `dune pkg` commands

This document describes the patching workflow for modifying vendored dependencies.

## Overview

Patching enables local modifications to dependencies that:
- Persist across `dune pkg fetch`
- Can be contributed upstream
- Are explicit and version-controlled

## Directory Structure

```
project/
  dune.lock                     # lock file (references patches)
  patches/                      # version controlled - user patches
    fmt@0.9.0.patch
    cmdliner@1.3.0.patch
  duniverse/                    # gitignored - regenerable
    fmt.0.9.0/
    cmdliner.1.3.0/
```

## Patch Storage

Following [Bun's approach](https://bun.sh/docs/pm/cli/patch):

- Patches stored in `patches/` directory (configurable via dune-workspace)
- Filename format: `<name>@<version>.patch` (version-specific)
- Standard unified diff format with optional metadata header
- Recorded in lock file

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

### `dune pkg patch list`

Show patch status:

```bash
$ dune pkg patch list
fmt@0.9.0       patches/fmt@0.9.0.patch      (applied)
cmdliner@1.2.0  patches/cmdliner@1.2.0.patch (stale: locked version is 1.3.0)
re@1.11.0       -                            (no patch)
```

Warns about version mismatches but doesn't auto-remove.

### `dune pkg patch diff <pkg>`

Show local changes to a package:

```bash
$ dune pkg patch diff fmt
--- a/src/fmt.ml
+++ b/src/fmt.ml
@@ -42,7 +42,7 @@
...
```

### `dune pkg patch commit <pkg>`

Generate patch from modifications:

```bash
$ dune pkg patch commit fmt
Generated patches/fmt@0.9.0.patch
Updated dune.lock
```

This:
1. Diffs current `duniverse/<pkg>.<version>/` against original source
2. Generates unified diff to `patches/<name>@<version>.patch`
3. Updates lock file with patch reference

### `dune pkg patch remove <pkg>`

Explicitly remove a patch:

```bash
$ dune pkg patch remove fmt
Removed patches/fmt@0.9.0.patch
Updated dune.lock
Re-fetch with: dune pkg fetch
```

No automatic pruning - patches must be explicitly removed.

## Workflow

### Creating a Patch

1. Edit files directly in `duniverse/<pkg>.<version>/`
2. Preview changes: `dune pkg patch diff <pkg>`
3. Save patch: `dune pkg patch commit <pkg>`
4. Commit `patches/<pkg>@<version>.patch` to version control

### Applying Patches

On `dune pkg fetch`:

1. Fetch sources to `duniverse/<name>.<version>/`
2. Apply opam patches (from package metadata)
3. Apply user patches (from `patches/` directory)

User patches are applied last, so they can fix issues introduced by opam patches
or override them.

## Version Changes

When a patched package's version changes (e.g., `fmt` 0.9.0 → 0.10.0):

1. **Old patch remains** - it's version-specific, won't be applied
2. **Warning on fetch** - "Patch for fmt@0.9.0 exists but locked version is 0.10.0"
3. **User decides**:
   - Try applying old patch to new version: `dune pkg patch --rebase fmt`
   - Create fresh patch: edit files + `dune pkg patch commit fmt`
   - Remove stale patch: `dune pkg patch remove fmt@0.9.0`

No automatic removal - patches might still be needed for history or multi-version.

## Upstream Contribution

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

## Original Source Storage

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

## Detecting Uncommitted Changes

When running `dune pkg fetch` on an already-fetched package:

1. Check if `duniverse/<pkg>/` has modifications vs last fetch
2. If yes, warn: "fmt.0.9.0 has local changes. Commit patch first?"
3. User can: `--force` to discard, or `dune pkg patch commit fmt` first

## Configuration

In `dune-workspace`:

```lisp
(lang dune 3.18)
(lock_dirs
 (default
  (patch_directory patches)         ; default: patches/
  (duniverse_directory duniverse))) ; default: duniverse/
```

## Lock File Integration

Patches are recorded in the lock file:

**Single-file format:**
```lisp
(patches
 (fmt patches/fmt@0.9.0.patch)
 (cmdliner patches/cmdliner@1.3.0.patch))
```

**Directory format:**
Add `(user_patches ...)` to `lock.dune`.

## References

- [Bun patch documentation](https://bun.sh/docs/pm/cli/patch)
- [pnpm patch documentation](https://pnpm.io/cli/patch)
- [patch-package npm](https://www.npmjs.com/package/patch-package)
- [Nix overlays](https://wiki.nixos.org/wiki/Nixpkgs/Modifying_Packages)
