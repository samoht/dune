# RFC: Lock Files

## Mental Model

**Lock file = snapshot of the package universe.**

Commit one file (`dune.lock`), and dune reconstructs your exact dependencies
on any machine.

```
dune.lock    =    repo commit  +  package versions
            ↓
         derived
            ↓
       full metadata (URLs, checksums, build commands)
            ↓
         fetched
            ↓
       _build/.pkgs/ (default) or duniverse/ (with --vendor)
```

## User Experience

### Getting Started

```bash
# New project
echo '(depends fmt cmdliner)' >> dune-project
dune pkg lock
git add dune.lock
git commit -m "Lock dependencies"
```

### Daily Workflow

```bash
git clone project
dune build --pkg=enabled    # Auto-lock + auto-fetch, just works™
```

Dune handles everything: fetches the repo, derives metadata, downloads sources,
generates vendor stanzas, builds.

### Updating Dependencies

```bash
dune pkg update             # Update all
dune pkg update fmt         # Update one package
git diff dune.lock          # Review changes
git commit -m "Update fmt"
```

### Editing a Dependency (with vendoring)

```bash
dune pkg fetch --vendor     # Vendor sources to duniverse/
vim duniverse/fmt.0.9.0/src/fmt.ml
dune build                  # Builds with your changes
```

Your edits are protected — `dune pkg fetch --vendor` won't overwrite them
without `--force`.

## The Lock File

```lisp
(lang package 0.1)

(repos
 (opam-repository abc123def456))

(packages
 ocaml.5.2.0
 fmt.0.9.0
 cmdliner.1.3.0)
```

That's a complete lock file. No URLs, no checksums, no build commands.

**Why so minimal?** Because `(opam-repository, abc123, fmt, 0.9.0)` uniquely
identifies the package in the opam-repository history. Dune derives everything
else.

### Portable Locks

For projects that need to work across platforms:

```lisp
(packages
 fmt.0.9.0
 (conf-libffi.2.0.0 (os linux))
 (conf-libffi.2.1.0 (os macos)))
```

Use `dune pkg lock --portable` or `dune build --pkg=portable` to solve for all
platforms at once.

### Git Pins

```lisp
(pins
 (my-lib (git "https://github.com/me/my-lib")
         (commit abc123)))
```

## What Gets Generated

By default, `dune pkg fetch` downloads sources to `_build/.pkgs/<context>/<name>/source/`.
These are transient and not version controlled.

With `--vendor`, sources are copied to `duniverse/` for version control:

```
duniverse/
  fmt.0.9.0/           # Vendored source
  cmdliner.1.3.0/      # Vendored source
  dune                 # Generated vendor stanzas
```

The `dune` file contains:

```lisp
(vendor fmt.0.9.0)
(vendor cmdliner.1.3.0)
(vendor zarith.1.14 (mode opam))   ; Non-dune package
```

Dune infers `(package ...)`, `(version ...)`, and `(libraries ...)` from the opam
file and directory contents. See [vendor RFC](vendor.md) for explicit overrides.

## Why Git?

The single-file format requires git-based repositories.

Git commits are immutable and content-addressed. One commit hash pins thousands
of packages with cryptographic guarantees. The opam-repository is fetched once
and cached — subsequent operations are local and fast.

Dune caches repos in `~/.cache/dune/`. After first fetch, everything works
offline.

For HTTP sources or local pins, use `--format=directory`.

## Commands Reference

| Command | What it does |
|---------|--------------|
| `dune pkg lock` | Solve dependencies, write `dune.lock` |
| `dune pkg lock --portable` | Solve for all platforms at once |
| `dune pkg fetch` | Download sources to `_build/.pkgs/` |
| `dune pkg fetch --vendor` | Download sources and copy to `duniverse/` |
| `dune pkg update` | Re-solve with latest versions |
| `dune pkg update PKG` | Update specific package |

### Build Flags

| Flag | Effect |
|------|--------|
| `--pkg=enabled` | Auto-lock if missing + auto-fetch |
| `--pkg=portable` | Auto-lock for all platforms + auto-fetch |
| `--pkg=disabled` | No package management (default) |
| `--lock=enabled` | Auto-lock if missing |
| `--lock=always` | Re-solve even if lock exists |
| `--lock=disabled` | Don't auto-lock |
| `--fetch=enabled` | Auto-fetch sources to `_build/.pkgs/` |
| `--fetch=disabled` | Don't auto-fetch |
| `--vendor=enabled` | Copy fetched sources to `duniverse/` |
| `--vendor=disabled` | Keep sources in `_build/.pkgs/` only (default) |

`--pkg=enabled` is shorthand for `--lock=enabled --fetch=enabled`.

## CI Setup

```yaml
- uses: actions/cache@v3
  with:
    path: ~/.cache/dune/
    key: ${{ runner.os }}-${{ hashFiles('dune.lock') }}

- run: dune build --pkg=enabled
```

Or with vendoring (if `duniverse/` is committed):

```yaml
- run: dune build  # Uses committed duniverse/
```

## Migration

Existing `dune.lock/` directories keep working. No action required.

To convert: `dune pkg lock --format=single-file`

## Error Messages

```
Error: duniverse/fmt.0.9.0/ has local modifications
Hint: Use --force to discard, or save your changes first.

Error: Package fmt.0.9.0 not found at commit abc123
Hint: Run 'dune pkg lock' to update.

Error: Checksum mismatch for fmt.0.9.0
This may indicate a corrupted download.
```

## References

- [RFC: Extended Vendor Stanza](vendor.md)
- [RFC: Opam Build Mode](opam-mode.md)
- [dune#8652](https://github.com/ocaml/dune/issues/8652)
