# RFC: Unified Package Rules

## Principle

**One build flow, one cache, one secondary index.**

All packages use the same pkg_rules build flow. The only variations are:
1. **Cache key formula** - What identifies a cached build
2. **Context** - Where the package builds
3. **OCAMLPATH** - Set only for opam-mode packages

## Package Types

| Type | Source | Context | Cached | Cache Key |
|------|--------|---------|--------|-----------|
| **Workspace** | Local dune project | `default` | No | — |
| **Vendored (dune)** | `duniverse/` with dune files | `default` | No | — |
| **Vendored (opam)** | `duniverse/` with opam file | `default` | No | — |
| **Locked** | `dune.lock` → `_build/.pkgs/` | `default` | Yes | `<name>.<ver>-<bid>` |
| **Dev tool** | Auto-solved | `tools-<name>` | Yes | `<name>.<ver>[-<ocaml>-<bid>]` |
| **Toolchain** | `dune.lock` (compiler pkg) | `default` | Yes | `<name>.<ver>-<bid>` |

Where:
- `<bid>` = 8-char build_id (Merkle hash of opam + deps' build_ids + platforms)
- `<ocaml>` = compiler version (only for compiler-dependent dev tools)

**Note:** Vendored packages are never cached because users may edit them.

## Cache Architecture

Reuse the existing content-addressed build cache. Add a secondary index for
cross-project sharing:

```
~/.cache/dune/
├── files/v5/<hash>/              # Content-addressed cache (existing)
│   └── ...
└── index/                       # Secondary index (new)
    ├── fmt.0.9.0-a1b2c3d4 → ../files/v5/deadbeef
    ├── ocamlformat.0.26.2 → ../files/v5/cafebabe
    └── odoc.2.4.0-5.2.0-b2c3 → ../files/v5/12345678
```

### Why a Secondary Index?

The content-addressed cache keys on rule hash (all inputs). This prevents
sharing in two important cases:

**1. Compiler-independent dev tools**

```
Project A (OCaml 5.2) + ocamlformat 0.26.2 → rule hash X
Project B (OCaml 4.14) + ocamlformat 0.26.2 → rule hash Y
```

Different rule hashes, but the binary is identical. The secondary index
`ocamlformat.0.26.2` enables sharing.

**2. Relocatable compilers**

The relocatable-compiler package produces binaries that work at any path.
Without a secondary index, each project would rebuild the compiler even
though the output is shareable.

### Why Symlinks?

1. **No duplication** - One copy in content-addressed store, multiple lookup paths
2. **Atomic** - `symlink()` is atomic on POSIX
3. **Simple check** - Symlink exists = installed
4. **Self-validating** - Symlink target hash ensures correctness

### Cache Key Formula

```ocaml
let cache_key ~pkg ~build_id ~context =
  let name = Package.Name.to_string pkg.info.name in
  let version = Package_version.to_string pkg.info.version in
  let bid = String.sub (Dune_digest.to_string build_id) ~pos:0 ~len:8 in

  match pkg_type pkg ~context with
  | Workspace | Vendored ->
      (* Never cached - users may edit sources *)
      None

  | Locked | Toolchain ->
      (* build_id already includes platform info *)
      Some (sprintf "%s.%s-%s" name version bid)

  | Dev_tool { compiler_dependent = false } ->
      (* Compiler-independent: same binary for any OCaml version *)
      Some (sprintf "%s.%s" name version)

  | Dev_tool { compiler_dependent = true; ocaml_version; ocaml_build_id } ->
      (* Compiler-dependent: must match project's compiler *)
      let ocaml_bid = String.sub (Dune_digest.to_string ocaml_build_id) ~pos:0 ~len:8 in
      Some (sprintf "%s.%s-%s-%s" name version ocaml_version ocaml_bid)
```

## Build Flow

```
                    ┌─────────────────┐
                    │  Compute        │
                    │  cache_key      │
                    └────────┬────────┘
                             │
              ┌──────────────┴──────────────┐
              │                             │
              ▼                             ▼
     cache_key = None              cache_key = Some key
     (workspace/vendored)          (locked/tool/toolchain)
              │                             │
              │                    ┌────────┴────────┐
              │                    │                 │
              │                    ▼                 ▼
              │              symlink exists?    symlink missing
              │                    │                 │
              │                    ▼                 │
              │              restore from           │
              │              cache (follow          │
              │              symlink)               │
              │                    │                 │
              │                    │                 ▼
              └────────────────────┴──────► build normally
                                                    │
                                                    ▼
                                           install to target_dir
                                                    │
                                           ┌────────┴────────┐
                                           │                 │
                                           ▼                 ▼
                                    cache_key = None   cache_key = Some
                                           │                 │
                                           │                 ▼
                                           │          store in files/v5/
                                           │          create symlink
                                           │                 │
                                           └────────┬────────┘
                                                    ▼
                                           promote to shared prefix
                                           _build/install/<context>/
```

## Environment Setup

### For all packages

```
PATH                ← deps' bin/ directories (prepended)
OPAM_SWITCH_PREFIX  ← target_dir
OPAM_PACKAGE_NAME   ← package name
OPAM_PACKAGE_VERSION ← package version
```

### For opam-mode packages only

```
OCAMLPATH           ← deps' lib/ directories
OCAMLFIND_DESTDIR   ← install lib root
```

This matches vendor.md which says "Dune sets OCAMLPATH so build commands
discover dependencies via ocamlfind" for opam-mode packages.

**Note:** Workspace and vendored dune packages don't need OCAMLPATH because
dune handles library resolution directly.

## Concurrency

Multiple dune processes may try to cache the same package simultaneously.
The flow handles this safely:

1. Build to temporary directory
2. Store in content-addressed cache (atomic via rename)
3. Create symlink (atomic, idempotent - if exists, same target)

If two processes race:
- Both build (wasted work, but correct)
- First to finish creates symlink
- Second sees symlink exists, skips creation

## Cache Invalidation

Cache entries are never automatically invalidated. The content-addressed
nature ensures correctness: same inputs → same hash → same cache entry.

For space management:
```bash
dune cache trim              # Remove least-recently-used entries
dune cache trim --size 10G   # Keep only 10GB
dune cache clear             # Remove everything
```

## What This Simplifies

### Before (current)

```
src/dune_rules/
├── pkg_toolchain.ml      # Separate toolchain cache
├── dev_tool_cache.ml     # Separate dev tool cache (dead code!)
└── pkg_rules.ml          # Special-case handling for toolchains

~/.cache/dune/
├── toolchains/           # Toolchain cache (full copies)
├── tools/                # Dev tool cache (unused)
└── files/                # Build cache
```

### After (unified)

```
src/dune_rules/
├── pkg_cache.ml          # Unified cache (symlinks to files/)
└── pkg_rules.ml          # One flow, no special cases

~/.cache/dune/
├── files/v5/             # Content-addressed cache (unchanged)
└── index/              # Secondary index (symlinks only)
```

## Compatibility

### Migration from old cache directories

The unified `index/` replaces the old separate directories:

| Old | New | Purpose |
|-----|-----|---------|
| `~/.cache/dune/toolchains/` | `~/.cache/dune/index/` | Relocatable compilers |
| `~/.cache/dune/tools/` | `~/.cache/dune/index/` | Dev tools |

Old directories are not migrated. On first use after upgrade:
- Relocatable compilers will be rebuilt and cached in new location
- Dev tools will be rebuilt and cached in new location

Users can delete old directories to reclaim space:
```bash
rm -rf ~/.cache/dune/toolchains ~/.cache/dune/tools
```

### Lock file format

No changes to lock file format. The cache is purely an optimization.

## Open Questions

### Should compiler-independent dev tools include build_id?

Currently: `ocamlformat.0.26.2` (no build_id)

This assumes all builds of ocamlformat 0.26.2 produce identical binaries.
This is mostly true but could fail if:
- opam-repository is updated with a patched version
- Different platforms produce different binaries

Alternative: `ocamlformat.0.26.2-<bid>` for all cached packages.

Trade-off: More cache misses vs. more correctness.

## References

- [RFC: Lock Files](lock.md) - build_id computation
- [RFC: Developer Tools](tools.md) - dev tool caching design
- [RFC: Extended Vendor Stanza](vendor.md) - vendored packages and OCAMLPATH
- [RFC: Opam Build Mode](opam-mode.md) - opam-mode package builds
